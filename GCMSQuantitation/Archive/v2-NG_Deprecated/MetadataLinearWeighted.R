
# =========================================================
# GC-MS PETN/RDX Quantification Script (Per-Analyte Calibration)
# =========================================================
#
# This script performs quantitative GC-MS analysis of multiple
# analytes (e.g. PETN, RDX) using external calibration with
# the following features:
#
#  - Data-driven: analytes defined in GlobalCode.R config
#  - Per-analyte calibration model type:
#       * PETN: 1/x weighted quadratic (with linear fallback)
#       * RDX:  1/x weighted linear
#       * Controlled by cal_model field in analyte definitions
#  - Multiple calibration sets (bracketed in a sequence run)
#  - Back-calculation of calibration standards
#  - Out-of-range flagging (extrapolated values reported, flagged)
#  - Full plotting per analyte:
#       * One plot per calibration (curve + standards + samples)
#       * One combined plot of all calibrations with curves
#  - Calculation of adjusted concentrations and %bias
#  - QC sample accuracy assessment with pass/fail flagging
#  - QC results export to long-term monitoring file
#  - Per-analyte internal standards:
#       * PETN uses NG (m/z 76) as IS -- ratio = PETN PA / PETN IS PA
#       * RDX uses 15N-RDX (m/z 122) as IS -- ratio = RDX PA / RDX IS PA
#
# This script is intended to be open-source, readable,
# auditable, and defensible for analytical chemistry workflows.
#
# Author: <add name>
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)     # data manipulation
  library(ggplot2)   # plotting
  library(stringr)   # regex-based string parsing
})

# -----------------------------
# Required user-defined globals
# -----------------------------
# These must be defined before running the script
required_globals <- c(
  "DataFolder",          # folder containing sequence log
  "GcData.dir",          # folder containing *Summary.csv files
  "Results.dir",         # output folder
  "ParentFolder",        # name used for output CSV
  "Dilution",            # dilution factor applied to standards/samples
  "SampleVol",           # injected or extracted sample volume
  "DepositMass",         # reference mass for recovery calculation
  "rdx_is_config",       # list(mz, rt) for RDX internal standard (15N-RDX)
  "petn_is_config",      # list(mz, rt) for PETN internal standard (NG)
  "analytes",            # named list of analyte definitions
  "qc_bias_limit",       # max allowable |%bias| for QC pass/fail
  "qc_monitoring_file",  # path to long-term QC monitoring CSV
  "snr_detect",          # minimum SNR for peak detection (LOD)
  "snr_quant",           # minimum SNR for quantitation (LOQ)
  "apply_petn_drift_correction"  # TRUE/FALSE: enable QC-based PETN PA drift correction
)

missing_globals <- required_globals[!sapply(required_globals, exists, where = .GlobalEnv, inherits = FALSE)]
if (length(missing_globals) > 0) {
  stop("Missing required globals: ",
       paste(missing_globals, collapse = ", "))
}

# =========================================================
# Concentration solver (quadratic with linear fallback)
# =========================================================
# Inverts a calibration curve to obtain concentration from the
# observed response (peak area or PA/IS ratio).
#
# Out-of-range responses are still quantified (extrapolated)
# but clearly flagged via the range_flag return value.
#
# When quad_model is provided, quadratic inversion is attempted
# first. If the discriminant is negative, the quadratic
# coefficient is degenerate, or no valid (non-negative) roots
# exist, it falls back to linear inversion.
#
# Args:
#   y            : observed response value
#   lin_model    : linear lm model (always required as fallback)
#   cal_y_range  : valid response range of the calibration
#   quad_model   : quadratic lm model (NULL for linear-only analytes)
#
# Returns:
#   list(value, method, range_flag)
#     value      : numeric concentration (NA if unquantifiable)
#     method     : "Quadratic", "Linear", "No_peak", "Unquantifiable"
#     range_flag : "Within_range", "Below_range", "Above_range", or NA
# =========================================================
solve_concentration <- function(y, lin_model, cal_y_range, quad_model = NULL) {

  # Missing or absent peak
  if (is.na(y)) {
    return(list(value = NA_real_, method = "No_peak", range_flag = NA_character_))
  }

  # Determine range status (used for flagging, no longer blocks quantification)
  if (y < cal_y_range[1]) {
    range_flag <- "Below_range"
  } else if (y > cal_y_range[2]) {
    range_flag <- "Above_range"
  } else {
    range_flag <- "Within_range"
  }

  # --- Quadratic inversion (when quad_model is provided) ---
  if (!is.null(quad_model)) {
    coefs <- coef(quad_model)
    a <- coefs["I(CalLevelAdj^2)"]
    b <- coefs["CalLevelAdj"]
    c_int <- coefs["(Intercept)"]

    # Quadratic discriminant: solve a*x^2 + b*x + (c - y) = 0
    disc <- b^2 - 4 * a * (c_int - y)

    if (is.finite(disc) && disc >= 0 && abs(a) > .Machine$double.eps) {
      roots <- c(
        (-b + sqrt(disc)) / (2 * a),
        (-b - sqrt(disc)) / (2 * a)
      )

      # Keep only physically meaningful roots (non-negative, finite)
      roots <- roots[is.finite(roots) & roots >= 0]

      if (length(roots) > 0) {
        return(list(value = min(roots), method = "Quadratic", range_flag = range_flag))
      }
    }
    # If quadratic fails, fall through to linear
  }

  # --- Linear inversion (primary for linear analytes, fallback for quadratic) ---
  lin <- coef(lin_model)
  slope <- lin[2]
  intercept <- lin[1]

  if (is.na(slope) || abs(slope) < .Machine$double.eps * 100) {
    return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
  }

  x <- (y - intercept) / slope
  if (x < 0) {
    return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
  }

  list(value = x, method = "Linear", range_flag = range_flag)
}

# =========================================================
# Load GC-MS summary files
# =========================================================
summary_files <- list.files(
  GcData.dir,
  pattern = "Summary.csv$",
  full.names = TRUE
)

if (length(summary_files) == 0) {
  stop("No *Summary.csv files found in GcData.dir: ", GcData.dir)
}

GcResults <- bind_rows(lapply(summary_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$File <- basename(f)
  df
}))

# =========================================================
# Parse sequence metadata
# =========================================================
log_files <- list.files(DataFolder, pattern = "Sequence.*\\.LOG$",
                        full.names = TRUE, ignore.case = TRUE)

if (length(log_files) == 0) {
  stop("No Sequence Log (.LOG) files found in DataFolder: ", DataFolder)
}

log_lines <- unlist(lapply(log_files, readLines))

# Extract run date
date_line <- log_lines[str_detect(log_lines, "Starting sequence")]

if (length(date_line) == 0) {
  stop("No 'Starting sequence' line found in sequence logs: ",
       paste(log_files, collapse = ", "))
}
if (length(date_line) > 1) {
  warning("Multiple 'Starting sequence' lines found; using the first.")
  date_line <- date_line[1]
}
date_str <- str_extract(
  date_line,
  "\\w+ \\w+ \\d+ \\d{2}:\\d{2}:\\d{2} \\d{4}"
)
run_date <- format(
  as.Date(as.POSIXct(date_str, format = "%a %b %d %H:%M:%S %Y")),
  "%Y%m%d"
)

# Extract sequence table lines
metadata_lines <- log_lines[str_detect(log_lines, "^\\s*\\d+\\)")]

if (length(metadata_lines) == 0) {
  stop("No numbered sequence lines (e.g. '1) ...') found in: ",
       paste(log_files, collapse = ", "))
}

# Parse a single sequence line
parse_line <- function(line) {

  m <- str_match(
    line,
    "^\\s*(\\d+)\\)\\s+(\\w+)\\s+(\\d+)\\s+(\\d+)\\s+(.*)$"
  )
  if (any(is.na(m))) return(NULL)

  cal_level <- NA_real_
  if (m[3] %in% c("Cal", "QC")) {
    cm <- str_match(m[6], "(\\d+(\\.\\d+)?)ng")
    if (!is.na(cm[1])) cal_level <- as.numeric(cm[2])
  }

  data.frame(
    Date = run_date,
    Line = as.integer(m[2]),
    Type = m[3],
    Vial = as.integer(m[4]),
    DataFile = paste0(m[5], "Summary.csv"),
    SampleName = m[6],
    CalLevel = cal_level,
    stringsAsFactors = FALSE
  )
}

Metadata <- bind_rows(lapply(metadata_lines, parse_line))

# Reassign Line numbers sequentially across all sequence logs
Metadata$Line <- seq_len(nrow(Metadata))

# =========================================================
# Combine metadata and GC-MS results
# =========================================================
Combined <- Metadata %>%
  left_join(GcResults, by = c("DataFile" = "File")) %>%
  mutate(Row = row_number())

# =========================================================
# RDX IS (15N-RDX) Peak Area QC Chart
# =========================================================
rdx_is_plot_data <- Combined %>%
  filter(!is.na(rdx_is_pa), Type != "Blank") %>%
  mutate(Group = case_when(
    Type == "Cal" ~ "Standard",
    Type == "QC"  ~ "QC",
    TRUE          ~ "Sample"
  ))

if (nrow(rdx_is_plot_data) > 0) {
  p_rdx_is <- ggplot(rdx_is_plot_data, aes(x = Row, y = rdx_is_pa, colour = Group)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    theme_bw() +
    labs(
      title = "RDX IS (15N-RDX) Peak Area Across Sequence",
      x = "Injection Number",
      y = "RDX IS Peak Area (m/z 122)",
      colour = "Type"
    )

  ggsave(
    "RDX_IS_PeakArea_QC.png", p_rdx_is,
    path = Results.dir,
    width = 8, height = 5, dpi = 300
  )
} else {
  warning("No RDX IS peak area data available -- skipping RDX IS QC chart.")
}

# =========================================================
# PETN IS (NG) Peak Area QC Chart
# =========================================================
petn_is_plot_data <- Combined %>%
  filter(!is.na(petn_is_pa), Type != "Blank") %>%
  mutate(Group = case_when(
    Type == "Cal" ~ "Standard",
    Type == "QC"  ~ "QC",
    TRUE          ~ "Sample"
  ))

if (nrow(petn_is_plot_data) > 0) {
  p_petn_is <- ggplot(petn_is_plot_data, aes(x = Row, y = petn_is_pa, colour = Group)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    theme_bw() +
    labs(
      title = "PETN IS (NG) Peak Area Across Sequence",
      x = "Injection Number",
      y = "PETN IS Peak Area (m/z 76)",
      colour = "Type"
    )

  ggsave(
    "PETN_IS_PeakArea_QC.png", p_petn_is,
    path = Results.dir,
    width = 8, height = 5, dpi = 300
  )
} else {
  warning("No PETN IS peak area data available -- skipping PETN IS QC chart.")
}

# =========================================================
# Compute analyte/IS ratio columns
# =========================================================
# Each analyte uses its own IS:
#   PETN ratio = PETN PA / PETN IS PA (NG, m/z 76)
#   RDX ratio  = RDX PA / RDX IS PA (15N-RDX, m/z 122)

# Map each analyte to its IS peak area column
analyte_is_map <- list(
  PETN = "petn_is_pa",
  RDX  = "rdx_is_pa"
)

for (name in names(analytes)) {
  prefix <- tolower(name)
  pa_col <- paste0(prefix, "_pa")
  ratio_col <- paste0(prefix, "_ratio")
  is_pa_col <- analyte_is_map[[name]]
  Combined[[ratio_col]] <- Combined[[pa_col]] / Combined[[is_pa_col]]
}

# =========================================================
# Build calibration sets
# =========================================================
CalData <- Combined %>%
  filter(Type == "Cal", !is.na(CalLevel)) %>%
  arrange(Row) %>%
  mutate(CalLevelAdj = CalLevel * Dilution)

# Assume equal number of calibration levels per set
n_levels <- n_distinct(CalData$CalLevelAdj)

single_level_cal <- nrow(CalData) > 0 && n_levels == 1
has_calibration  <- nrow(CalData) > 0 && n_levels >= 2

if (single_level_cal) {
  warning("Only one calibration level found (",
          unique(CalData$CalLevelAdj), " ng). ",
          "Cannot fit a calibration curve. ",
          "Results will contain peak areas only (uncalibrated).")
}

if (!has_calibration && !single_level_cal) {
  warning("No calibration standards found. Results will contain peak areas only (uncalibrated).")
}

if (has_calibration && nrow(CalData) %% n_levels != 0) {
  stop(
    "Number of calibration rows (", nrow(CalData),
    ") is not evenly divisible by the number of distinct levels (", n_levels, ").",
    " Cannot form equal-sized calibration sets."
  )
}

if (has_calibration) {
  CalSets <- split(
    CalData,
    rep(seq_len(nrow(CalData) / n_levels), each = n_levels)
  )
}

Combined$CalibrationSet <- NA_integer_

# Initialise per-analyte result columns for enabled calibration methods
for (name in names(analytes)) {
  prefix <- tolower(name)
  enabled <- analytes[[name]]$cal_methods
  if (is.null(enabled)) enabled <- c("pa", "ratio")
  for (suffix in paste0("_", enabled)) {
    Combined[[paste0(prefix, "_concentration", suffix)]] <- NA_real_
    Combined[[paste0(prefix, "_quant_method", suffix)]]  <- NA_character_
    Combined[[paste0(prefix, "_range_flag", suffix)]]    <- NA_character_
  }
}

# =========================================================
# Apply calibration models and generate plots (per analyte)
# =========================================================
# Two calibration methods per analyte:
#   "pa"    -- calibrate directly on peak area
#   "ratio" -- calibrate on (analyte PA / analyte's IS PA)
#              PETN: PA / PETN IS (NG) PA
#              RDX:  PA / RDX IS (15N-RDX) PA

# Map analyte to IS label for plot y-axis
analyte_is_label <- list(
  PETN = "PA / PETN IS Ratio",
  RDX  = "PA / RDX IS Ratio"
)

cal_stats <- list()  # accumulates R2/AdjR2 per analyte/method/set

# Storage for PETN PA calibration models, needed for drift-corrected
# re-quantification after the calibration loop.  Keyed by cal set index.
stored_petn_pa_models <- list()

if (has_calibration) {

for (analyte_name in names(analytes)) {

  prefix    <- tolower(analyte_name)
  pa_col    <- paste0(prefix, "_pa")
  ratio_col <- paste0(prefix, "_ratio")

  # Skip if the PA column is all NA (e.g., analyte RT not yet configured)
  if (!(pa_col %in% names(Combined)) || all(is.na(Combined[[pa_col]]))) {
    warning("All ", pa_col, " values are NA -- skipping ", analyte_name, " calibration.")
    next
  }

  # Define calibration methods, filtered by the analyte's cal_methods config.
  # If cal_methods is not defined, default to both PA and ratio (backward compatible).
  all_cal_methods <- list(
    pa    = list(response_col = pa_col,    y_label = "Peak Area"),
    ratio = list(response_col = ratio_col, y_label = analyte_is_label[[analyte_name]])
  )
  enabled_methods <- analytes[[analyte_name]]$cal_methods
  if (is.null(enabled_methods)) enabled_methods <- c("pa", "ratio")
  cal_methods <- all_cal_methods[enabled_methods]

  # Storage for curves and per-set plot data, keyed by method
  all_curve_store <- setNames(
    lapply(names(cal_methods), function(x) list()),
    names(cal_methods)
  )
  all_set_plots   <- list()

  for (i in seq_along(CalSets)) {

    cal <- CalSets[[i]]

    # Rows quantified by this calibration set (shared across methods)
    rows_std <- Combined$Row %in% cal$Row
    rows_smp <- Combined$Row > max(cal$Row)
    if (i < length(CalSets)) {
      rows_smp <- rows_smp & Combined$Row < min(CalSets[[i + 1]]$Row)
    }
    rows <- which(rows_std | rows_smp)

    # Assign CalibrationSet (once, not per method)
    Combined$CalibrationSet[rows] <- i

    # Per-set facet data collectors
    set_plot_data  <- list()
    set_curve_data <- list()

    for (method_name in names(cal_methods)) {

      method   <- cal_methods[[method_name]]
      resp_col <- method$response_col
      conc_col   <- paste0(prefix, "_concentration_", method_name)
      method_col <- paste0(prefix, "_quant_method_", method_name)
      range_col  <- paste0(prefix, "_range_flag_", method_name)

      # Determine which calibration rows to use for model fitting.
      # Exclude Below_LOD peaks (noise) from the calibration model.
      # The snr_flag column may not exist in older data; treat missing
      # flag as "include" (no filtering).
      snr_flag_col <- paste0(prefix, "_snr_flag")
      if (snr_flag_col %in% names(cal)) {
        cal_fit <- cal %>%
          dplyr::filter(is.na(.data[[snr_flag_col]]) | .data[[snr_flag_col]] != "Below_LOD")
      } else {
        cal_fit <- cal
      }

      # Skip method if all usable calibration responses are NA
      if (nrow(cal_fit) == 0 || all(is.na(cal_fit[[resp_col]]))) {
        warning(analyte_name, " ", method_name, " calibration set ", i,
                ": all response values NA (or Below_LOD) -- skipping.")
        next
      }

      # Determine calibration model type for this analyte
      # Default to "linear" if cal_model is not defined (backward compatible)
      cal_model_type <- analytes[[analyte_name]]$cal_model
      if (is.null(cal_model_type)) cal_model_type <- "linear"

      # Build 1/x weighted models dynamically
      # Weighting by 1/concentration gives more importance to lower
      # calibration levels, improving accuracy at the low end of the range.
      cal_weights  <- 1 / cal_fit$CalLevelAdj

      # Linear model (always fitted -- primary for linear analytes, fallback for quadratic)
      lin_formula  <- reformulate("CalLevelAdj", response = resp_col)
      lin_model    <- lm(lin_formula, data = cal_fit, weights = cal_weights)

      # Quadratic model (fitted only for quadratic analytes)
      quad_model <- NULL
      if (cal_model_type == "quadratic") {
        quad_formula <- reformulate(c("CalLevelAdj", "I(CalLevelAdj^2)"), response = resp_col)
        quad_model   <- lm(quad_formula, data = cal_fit, weights = cal_weights)
      }

      # Select the primary model for statistics and plotting
      primary_model <- if (!is.null(quad_model)) quad_model else lin_model

      # Capture model fit statistics from the primary model
      primary_smry <- summary(primary_model)

      cal_stats[[length(cal_stats) + 1]] <- data.frame(
        Analyte        = analyte_name,
        CalibrationSet = i,
        Method         = method$y_label,
        ModelType      = if (!is.null(quad_model)) "Quadratic" else "Linear",
        N_Standards    = nrow(cal_fit),
        N_Excluded_LOD = nrow(cal) - nrow(cal_fit),
        R2             = primary_smry$r.squared,
        AdjR2          = primary_smry$adj.r.squared,
        stringsAsFactors = FALSE
      )

      cal_y_range <- range(cal_fit[[resp_col]], na.rm = TRUE)

      # Store PETN PA models for drift-corrected re-quantification
      if (analyte_name == "PETN" && method_name == "pa") {
        stored_petn_pa_models[[i]] <- list(
          lin_model   = lin_model,
          quad_model  = quad_model,
          cal_y_range = cal_y_range,
          rows        = rows,
          snr_flag_col = snr_flag_col
        )
      }

      # Quantify rows, applying SNR-based acceptance criteria
      for (j in rows) {

        # Check the SNR flag for this row's analyte
        row_snr_flag <- NA_character_
        if (snr_flag_col %in% names(Combined)) {
          row_snr_flag <- Combined[[snr_flag_col]][j]
        }

        # Below_LOD: peak is noise -- do not quantify
        if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
          Combined[[conc_col]][j]   <- NA_real_
          Combined[[method_col]][j] <- "Below_LOD"
          Combined[[range_col]][j]  <- NA_character_
          next
        }

        # Below_LOQ: peak is real but too imprecise -- do not quantify
        if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOQ") {
          Combined[[conc_col]][j]   <- NA_real_
          Combined[[method_col]][j] <- "Below_LOQ"
          Combined[[range_col]][j]  <- NA_character_
          next
        }

        # Quantifiable or no SNR data: proceed with calibration
        res <- solve_concentration(
          Combined[[resp_col]][j],
          lin_model,
          cal_y_range,
          quad_model
        )
        Combined[[conc_col]][j]   <- res$value
        Combined[[method_col]][j] <- res$method
        Combined[[range_col]][j]  <- res$range_flag
      }

      # Curve for plotting (uses the primary model)
      curve_x <- seq(min(cal$CalLevelAdj), max(cal$CalLevelAdj), length.out = 200)
      curve_df <- data.frame(
        CalibrationSet = i,
        CalLevelAdj = curve_x,
        Fitted = predict(primary_model, newdata = data.frame(CalLevelAdj = curve_x)),
        Method = method$y_label
      )
      all_curve_store[[method_name]][[i]] <- curve_df
      set_curve_data[[method_name]] <- curve_df

      # Plot data for this set and method
      pd <- Combined %>%
        filter(CalibrationSet == i & !is.na(.data[[resp_col]])) %>%
        mutate(
          Xplot  = ifelse(Type %in% c("Cal", "QC"), CalLevel * Dilution, .data[[conc_col]]),
          y_val  = .data[[resp_col]],
          Group  = case_when(
            Type == "Cal" ~ "Standard",
            Type == "QC"  ~ "QC",
            TRUE          ~ "Sample"
          ),
          SNR_Flag = if (snr_flag_col %in% names(.)) .data[[snr_flag_col]] else NA_character_,
          Method = method$y_label
        )
      set_plot_data[[method_name]] <- pd
    }

    # -------- Faceted per-calibration plot (PA + Ratio side by side) --------
    combined_plot_data  <- bind_rows(set_plot_data)
    combined_curve_data <- bind_rows(set_curve_data)

    if (nrow(combined_plot_data) > 0 && nrow(combined_curve_data) > 0) {
      p <- ggplot() +
        geom_point(
          data = combined_plot_data,
          aes(Xplot, y_val, color = Group, shape = SNR_Flag),
          size = 3
        ) +
        scale_shape_manual(
          values = c("Quantifiable" = 16, "Below_LOQ" = 17, "Below_LOD" = 4),
          na.value = 1
        ) +
        geom_line(
          data = combined_curve_data,
          aes(CalLevelAdj, Fitted),
          color = "black"
        ) +
        facet_wrap(~Method, scales = "free_y") +
        theme_bw() +
        labs(
          title = paste(analyte_name, "- Calibration", i),
          x = paste(analyte_name, "concentration"),
          y = "Response",
          shape = "SNR Flag"
        )

      ggsave(
        paste0(analyte_name, "_Calibration_", i, ".png"),
        p,
        path = Results.dir,
        width = 12,
        height = 5,
        dpi = 300
      )
    }
  }

  # =========================================================
  # Combined calibration plot (all sets, faceted by method)
  # =========================================================
  all_cals <- Combined %>%
    filter(Type %in% c("Cal", "QC")) %>%
    mutate(Xplot = CalLevel * Dilution)

  # Build faceted data for the "All Calibrations" plot
  all_plot_methods  <- list()
  all_curve_methods <- list()

  for (method_name in names(cal_methods)) {
    method   <- cal_methods[[method_name]]
    resp_col <- method$response_col

    if (all(is.na(all_cals[[resp_col]]))) next

    all_plot_methods[[method_name]] <- all_cals %>%
      mutate(y_val = .data[[resp_col]], Method = method$y_label)

    curves <- bind_rows(all_curve_store[[method_name]])
    if (nrow(curves) > 0) {
      all_curve_methods[[method_name]] <- curves
    }
  }

  all_plot_combined  <- bind_rows(all_plot_methods)
  all_curve_combined <- bind_rows(all_curve_methods)

  if (nrow(all_plot_combined) > 0 && nrow(all_curve_combined) > 0) {
    p_all <- ggplot() +
      geom_point(
        data = all_plot_combined,
        aes(Xplot, y_val, color = factor(CalibrationSet), shape = Type),
        size = 3
      ) +
      scale_shape_manual(values = c("Cal" = 16, "QC" = 17)) +
      geom_line(
        data = all_curve_combined,
        aes(CalLevelAdj, Fitted, color = factor(CalibrationSet)),
        linewidth = 1
      ) +
      facet_wrap(~Method, scales = "free_y") +
      theme_bw() +
      labs(
        title = paste(analyte_name, "- All Calibration Curves"),
        x = paste(analyte_name, "concentration"),
        y = "Response",
        color = "Calibration Set"
      )

    ggsave(
      paste0(analyte_name, "_All_Calibrations.png"),
      p_all,
      path = Results.dir,
      width = 12,
      height = 6,
      dpi = 300
    )
  }
}

}  # end if (has_calibration)

# =========================================================
# Single-level standard reference plots (no calibration)
# =========================================================
# When standards were run at only one concentration, plot
# peak areas and ratios as a visual reference (no fitted
# curve).  Standards are shown at their known concentration
# while samples are plotted by injection order on a
# secondary panel.

if (single_level_cal) {

  std_conc <- unique(CalData$CalLevelAdj)  # single value

  for (analyte_name in names(analytes)) {

    prefix    <- tolower(analyte_name)
    pa_col    <- paste0(prefix, "_pa")
    ratio_col <- paste0(prefix, "_ratio")

    # Skip if the PA column is all NA
    if (!(pa_col %in% names(Combined)) || all(is.na(Combined[[pa_col]]))) next

    # Collect data for enabled methods (PA and/or Ratio)
    all_ref_methods <- list(
      pa    = list(resp_col = pa_col,    y_label = "Peak Area"),
      ratio = list(resp_col = ratio_col, y_label = analyte_is_label[[analyte_name]])
    )
    enabled_methods <- analytes[[analyte_name]]$cal_methods
    if (is.null(enabled_methods)) enabled_methods <- c("pa", "ratio")
    ref_methods <- all_ref_methods[enabled_methods]

    ref_plot_rows <- list()

    for (method_name in names(ref_methods)) {
      method   <- ref_methods[[method_name]]
      resp_col <- method$resp_col

      if (all(is.na(Combined[[resp_col]]))) next

      snr_flag_col <- paste0(prefix, "_snr_flag")

      method_df <- Combined %>%
        filter(!is.na(.data[[resp_col]])) %>%
        mutate(
          Response = .data[[resp_col]],
          Group = case_when(
            Type == "Cal" ~ "Standard",
            Type == "QC"  ~ "QC",
            TRUE          ~ "Sample"
          ),
          SNR_Flag = if (snr_flag_col %in% names(.)) .data[[snr_flag_col]] else NA_character_,
          Method = method$y_label
        )
      ref_plot_rows[[method_name]] <- method_df
    }

    ref_plot_data <- bind_rows(ref_plot_rows)
    if (nrow(ref_plot_data) == 0) next

    p_ref <- ggplot(ref_plot_data,
                    aes(x = Row, y = Response, colour = Group, shape = SNR_Flag)) +
      geom_point(size = 3) +
      scale_shape_manual(
        values = c("Quantifiable" = 16, "Below_LOQ" = 17, "Below_LOD" = 4),
        na.value = 1
      ) +
      geom_hline(
        data = ref_plot_data %>%
          filter(Group == "Standard") %>%
          group_by(Method) %>%
          summarise(MeanResp = mean(Response), .groups = "drop"),
        aes(yintercept = MeanResp),
        linetype = "dashed", colour = "grey40"
      ) +
      facet_wrap(~Method, scales = "free_y") +
      theme_bw() +
      labs(
        title = paste0(analyte_name,
                       " - Single-Level Standard Reference (",
                       std_conc, " ng)"),
        x = "Injection Number",
        y = "Response",
        colour = "Type",
        shape  = "SNR Flag"
      )

    ggsave(
      paste0(analyte_name, "_SingleLevel_Reference.png"),
      p_ref,
      path = Results.dir,
      width = 12, height = 5, dpi = 300
    )
  }
}

# =========================================================
# Calibration Statistics Excel Export
# =========================================================

# Build calibration stats data frame
cal_stats_df <- bind_rows(cal_stats)

if (nrow(cal_stats_df) > 0) {

  # Pivot to wide format: one row per calibration set
  cal_stats_wide <- cal_stats_df %>%
    tidyr::pivot_wider(
      id_cols = CalibrationSet,
      names_from = c(Analyte, Method),
      values_from = c(ModelType, N_Standards, N_Excluded_LOD, R2, AdjR2),
      names_sep = "_"
    )

  # RDX IS %RSD per calibration set
  rdx_is_rsd_per_set <- Combined %>%
    filter(!is.na(rdx_is_pa), !is.na(CalibrationSet)) %>%
    group_by(CalibrationSet) %>%
    summarise(
      RDX_IS_RSD_Percent = (sd(rdx_is_pa) / mean(rdx_is_pa)) * 100,
      .groups = "drop"
    )

  # RDX IS %RSD overall (all injections)
  rdx_is_pa_all <- Combined$rdx_is_pa[!is.na(Combined$rdx_is_pa)]
  if (length(rdx_is_pa_all) >= 2) {
    rdx_is_rsd_overall <- (sd(rdx_is_pa_all) / mean(rdx_is_pa_all)) * 100
  } else {
    rdx_is_rsd_overall <- NA_real_
  }

  # PETN IS %RSD per calibration set
  petn_is_rsd_per_set <- Combined %>%
    filter(!is.na(petn_is_pa), !is.na(CalibrationSet)) %>%
    group_by(CalibrationSet) %>%
    summarise(
      PETN_IS_RSD_Percent = (sd(petn_is_pa) / mean(petn_is_pa)) * 100,
      .groups = "drop"
    )

  # PETN IS %RSD overall (all injections)
  petn_is_pa_all <- Combined$petn_is_pa[!is.na(Combined$petn_is_pa)]
  if (length(petn_is_pa_all) >= 2) {
    petn_is_rsd_overall <- (sd(petn_is_pa_all) / mean(petn_is_pa_all)) * 100
  } else {
    petn_is_rsd_overall <- NA_real_
  }

  # Merge per-set IS %RSD and add overall
  cal_stats_wide <- cal_stats_wide %>%
    left_join(rdx_is_rsd_per_set, by = "CalibrationSet") %>%
    left_join(petn_is_rsd_per_set, by = "CalibrationSet") %>%
    mutate(RDX_IS_RSD_Overall = rdx_is_rsd_overall,
           PETN_IS_RSD_Overall = petn_is_rsd_overall)

  # Write Excel
  writexl::write_xlsx(
    cal_stats_wide,
    path = file.path(Results.dir, paste0(ParentFolder, "_CalibrationStats.xlsx"))
  )

} else {
  warning("No calibration statistics to export -- skipping CalibrationStats.xlsx.")
}

# =========================================================
# PETN PA-Level Drift Correction (QC-based)
# =========================================================
# Corrects raw PETN peak areas for progressive signal loss
# BEFORE calibration inversion, so the quadratic calibration
# curve handles non-linear mapping correctly at all
# concentration levels.
#
# The correction factor is computed from the ratio of the
# reference Cal standard PA to the QC PA at each bracket
# position, then modelled as a quadratic (or linear fallback)
# function of injection position.
#
# References:
#   Yu et al. (2025) Sci. Rep. 15:24794
#   Dunn et al. (2011) Nat. Protoc. 6:1060-1083
# ---------------------------------------------------------

# Initialise drift-correction columns to uncorrected defaults.
# These are always present in the output regardless of whether
# correction is enabled, so downstream scripts can rely on them.
Combined$petn_drift_correction_factor <- 1.0
Combined$petn_drift_model              <- NA_character_
Combined$petn_pa_dc                    <- Combined$petn_pa
Combined$petn_concentration_pa_dc      <- Combined$petn_concentration_pa
Combined$petn_quant_method_pa_dc       <- Combined$petn_quant_method_pa
Combined$petn_range_flag_pa_dc         <- Combined$petn_range_flag_pa

if (apply_petn_drift_correction && has_calibration &&
    "petn_pa" %in% names(Combined) &&
    length(stored_petn_pa_models) > 0) {

  # --- Step 1: Identify the highest QC level and reference Cal PA ---
  qc_candidates <- Combined %>%
    filter(Type == "QC", !is.na(petn_pa), !is.na(CalLevel))

  if (nrow(qc_candidates) > 0) {
    max_qc_cal_level <- max(qc_candidates$CalLevel, na.rm = TRUE)

    # Reference PA: mean PA of Cal standards at the QC level from the
    # first calibration set (earliest injections, minimal drift).
    ref_pa <- Combined %>%
      filter(Type == "Cal",
             CalLevel == max_qc_cal_level,
             !is.na(petn_pa),
             CalibrationSet == 1) %>%
      summarise(ref_pa = mean(petn_pa, na.rm = TRUE)) %>%
      pull(ref_pa)

    if (is.finite(ref_pa) && ref_pa > 0) {

      # --- Step 2: Compute PA correction factors at each QC position ---
      qc_drift <- Combined %>%
        filter(Type == "QC",
               CalLevel == max_qc_cal_level,
               !is.na(petn_pa)) %>%
        mutate(PA_CorrFactor = ref_pa / petn_pa) %>%
        filter(is.finite(PA_CorrFactor), PA_CorrFactor > 0)

      n_levels <- length(unique(qc_candidates$CalLevel))
      if (n_levels > 1) {
        message("  Drift correction: using QC level ", max_qc_cal_level,
                " ng (", nrow(qc_drift), " PA points; ",
                n_levels - 1, " lower QC level(s) excluded)")
      } else {
        message("  Drift correction: using ", nrow(qc_drift),
                " QC PA points at ", max_qc_cal_level, " ng")
      }
      message("  Reference Cal PA (", max_qc_cal_level, " ng): ",
              round(ref_pa, 0))

      qc_drift <- qc_drift %>%
        select(Row, petn_pa, PA_CorrFactor)

      n_qc <- nrow(qc_drift)

      # --- Step 3: Fit the drift model ---
      drift_model  <- NULL
      drift_method <- "None"

      if (n_qc >= 4) {
        drift_model <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_drift)
        drift_method <- "Quadratic"

        r2 <- summary(drift_model)$r.squared
        if (is.na(r2) || r2 < 0.5) {
          message("  Drift correction: quadratic R2 = ", round(r2, 3),
                  " (poor fit); falling back to linear.")
          drift_model  <- lm(PA_CorrFactor ~ Row, data = qc_drift)
          drift_method <- "Linear"
        }

      } else if (n_qc == 3) {
        drift_model  <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_drift)
        drift_method <- "Quadratic"
        message("  Drift correction: only 3 QC points -- quadratic fit is exact (0 residual df).")

      } else if (n_qc == 2) {
        drift_model  <- lm(PA_CorrFactor ~ Row, data = qc_drift)
        drift_method <- "Linear"
        message("  Drift correction: only 2 QC points -- using linear interpolation.")

      } else {
        message("  Drift correction: fewer than 2 usable QC points -- skipping correction.")
      }

      # --- Step 4: Apply PA correction and re-quantify ---
      if (!is.null(drift_model)) {
        predicted_cf <- predict(drift_model, newdata = data.frame(Row = Combined$Row))

        # Post-hoc anchor normalisation: force CF = 1.0 at the
        # calibration point.  The drift model is fitted to QC points
        # only (capturing the drift shape), then normalised so that
        # no correction is applied at the calibrant injection
        # positions.  This separates two distinct effects:
        #   - Progressive instrument degradation (corrected by model)
        #   - Systematic Cal-vs-QC offset (left uncorrected; not drift)
        # Follows the QC-RLSC principle (Dunn et al. 2011 Nat. Protoc.)
        # where the drift model is fitted exclusively to QC samples
        # and the calibration is treated as a separate step.
        cal_rows <- Combined$Row[Combined$Type == "Cal" & Combined$CalibrationSet == 1]
        if (length(cal_rows) > 0) {
          anchor_row <- mean(cal_rows)
          anchor_cf  <- predict(drift_model, newdata = data.frame(Row = anchor_row))
          if (is.finite(anchor_cf) && anchor_cf > 0) {
            predicted_cf <- predicted_cf / anchor_cf
            message("  Drift correction: anchored to Cal set 1 (mean Row ",
                    round(anchor_row, 1), ", raw CF = ",
                    round(anchor_cf, 4), " -> normalised to 1.0)")
          }
        }

        # Sanity: clamp correction factors
        if (any(predicted_cf < 0, na.rm = TRUE)) {
          warning("  Drift correction: some predicted PA correction factors were negative -- clamping to 1.0.")
          predicted_cf[predicted_cf < 0] <- 1.0
        }
        if (any(predicted_cf > 3.0, na.rm = TRUE)) {
          warning("  Drift correction: some PA correction factors exceed 3.0 -- severe drift detected.")
        }

        Combined$petn_drift_correction_factor <- predicted_cf
        Combined$petn_drift_model             <- drift_method
        Combined$petn_pa_dc                   <- Combined$petn_pa * predicted_cf

        # Re-quantify drift-corrected PA using stored calibration models
        for (model_idx in seq_along(stored_petn_pa_models)) {
          m <- stored_petn_pa_models[[model_idx]]

          for (j in m$rows) {
            # Apply same SNR-based acceptance criteria as original quantification
            row_snr_flag <- NA_character_
            if (m$snr_flag_col %in% names(Combined)) {
              row_snr_flag <- Combined[[m$snr_flag_col]][j]
            }

            if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
              Combined$petn_concentration_pa_dc[j] <- NA_real_
              Combined$petn_quant_method_pa_dc[j]  <- "Below_LOD"
              Combined$petn_range_flag_pa_dc[j]    <- NA_character_
              next
            }
            if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOQ") {
              Combined$petn_concentration_pa_dc[j] <- NA_real_
              Combined$petn_quant_method_pa_dc[j]  <- "Below_LOQ"
              Combined$petn_range_flag_pa_dc[j]    <- NA_character_
              next
            }

            res <- solve_concentration(
              Combined$petn_pa_dc[j],
              m$lin_model,
              m$cal_y_range,
              m$quad_model
            )
            Combined$petn_concentration_pa_dc[j] <- res$value
            Combined$petn_quant_method_pa_dc[j]  <- res$method
            Combined$petn_range_flag_pa_dc[j]    <- res$range_flag
          }
        }

        # --- Console summary ---
        r2_val <- round(summary(drift_model)$r.squared, 4)

        message("  PETN PA-level drift correction applied:")
        message("    Model: ", drift_method)
        message("    QC points used: ", n_qc)
        message("    R-squared: ", r2_val)
        message("    PA correction factor range: ",
                round(min(predicted_cf, na.rm = TRUE), 4), " - ",
                round(max(predicted_cf, na.rm = TRUE), 4))

        # --- Diagnostic plot ---
        pred_line <- data.frame(
          Row = seq(min(Combined$Row), max(Combined$Row), length.out = 200)
        )
        pred_line$PA_CorrFactor <- predict(drift_model, newdata = pred_line)

        sample_points <- data.frame(
          Row = Combined$Row,
          PA_CorrFactor = predicted_cf,
          Type = Combined$Type
        )

        p_drift <- ggplot() +
          geom_line(data = pred_line,
                    aes(x = Row, y = PA_CorrFactor),
                    colour = "steelblue", linewidth = 1) +
          geom_point(data = qc_drift,
                     aes(x = Row, y = PA_CorrFactor),
                     colour = "red", size = 4, shape = 17) +
          geom_point(data = sample_points %>% filter(Type == "Sample"),
                     aes(x = Row, y = PA_CorrFactor),
                     colour = "grey50", size = 2, alpha = 0.6) +
          geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
          theme_bw() +
          labs(
            title = paste0("PETN PA Drift Correction (", drift_method,
                           ", R\u00B2 = ", r2_val, ")"),
            subtitle = paste0("QC PA correction factors (red triangles), ",
                              "fitted model (blue line), sample positions (grey dots)"),
            x = "Injection Number",
            y = "PA Correction Factor"
          )

        ggsave(
          "PETN_DriftCorrection.png",
          p_drift,
          path = Results.dir,
          width = 10, height = 5, dpi = 300
        )

      } else {
        Combined$petn_drift_model <- "None"
      }

    } else {
      message("  Drift correction: reference Cal PA is invalid -- skipping correction.")
      Combined$petn_drift_model <- "None"
    }
  } else {
    message("  Drift correction: no usable QC rows -- skipping correction.")
    Combined$petn_drift_model <- "None"
  }

} else if (!apply_petn_drift_correction) {
  Combined$petn_drift_model <- "Disabled"
  message("  PETN drift correction: disabled (apply_petn_drift_correction = FALSE).")
}

# =========================================================
# Final QC calculations (per analyte, per method)
# =========================================================
if (has_calibration) {

Combined <- Combined %>%
  mutate(TrueCalConcAdj = ifelse(Type %in% c("Cal", "QC"), CalLevel * Dilution, NA_real_))

for (name in names(analytes)) {
  prefix <- tolower(name)

  enabled <- analytes[[name]]$cal_methods
  if (is.null(enabled)) enabled <- c("pa", "ratio")
  for (suffix in paste0("_", enabled)) {
    conc_col <- paste0(prefix, "_concentration", suffix)

    # Skip if the concentration column doesn't exist or is all NA
    if (!(conc_col %in% names(Combined)) || all(is.na(Combined[[conc_col]]))) next

    Combined[[paste0(prefix, "_percent_bias", suffix)]] <- ifelse(
      Combined$Type %in% c("Cal", "QC") & !is.na(Combined[[conc_col]]) & !is.na(Combined$TrueCalConcAdj),
      (Combined[[conc_col]] - Combined$TrueCalConcAdj) / Combined$TrueCalConcAdj * 100,
      NA_real_
    )
    Combined[[paste0(prefix, "_sample_conc", suffix)]]     <- Combined[[conc_col]] / Dilution
    Combined[[paste0(prefix, "_mass_in_sample", suffix)]]   <- Combined[[paste0(prefix, "_sample_conc", suffix)]] * SampleVol
    Combined[[paste0(prefix, "_percent_recovery", suffix)]] <- Combined[[paste0(prefix, "_mass_in_sample", suffix)]] / DepositMass * 100
  }
}

# --- Drift-corrected derived columns for PETN PA ---
# These mirror the uncorrected PETN PA columns but use the
# drift-corrected concentration.  Always present in the output
# (when correction is disabled, they equal the uncorrected values).
Combined$petn_sample_conc_pa_dc     <- Combined$petn_concentration_pa_dc / Dilution
Combined$petn_mass_in_sample_pa_dc  <- Combined$petn_sample_conc_pa_dc * SampleVol
Combined$petn_percent_recovery_pa_dc <- Combined$petn_mass_in_sample_pa_dc / DepositMass * 100

# --- Drift-corrected %bias and QC flag for PETN PA ---
# Back-calculate QC accuracy using drift-corrected concentrations.
# Provides an independent validation: 6ng QCs (used to build the model)
# should show near-zero bias; 0.2ng QCs (excluded from the model) will
# show residual bias, quantifying the non-proportional losses at trace levels.
Combined$petn_percent_bias_pa_dc <- ifelse(
  Combined$Type %in% c("Cal", "QC") &
    !is.na(Combined$petn_concentration_pa_dc) &
    !is.na(Combined$TrueCalConcAdj),
  (Combined$petn_concentration_pa_dc - Combined$TrueCalConcAdj) / Combined$TrueCalConcAdj * 100,
  NA_real_
)

Combined$petn_qc_flag_pa_dc <- ifelse(
  Combined$Type == "QC" & !is.na(Combined$petn_percent_bias_pa_dc),
  ifelse(abs(Combined$petn_percent_bias_pa_dc) <= qc_bias_limit, "PASS", "FAIL"),
  NA_character_
)

# =========================================================
# QC pass/fail assessment
# =========================================================
for (name in names(analytes)) {
  prefix <- tolower(name)
  enabled <- analytes[[name]]$cal_methods
  if (is.null(enabled)) enabled <- c("pa", "ratio")
  for (suffix in paste0("_", enabled)) {
    bias_col <- paste0(prefix, "_percent_bias", suffix)
    flag_col <- paste0(prefix, "_qc_flag", suffix)

    # Only flag QC rows that have a computed percent bias
    Combined[[flag_col]] <- ifelse(
      Combined$Type == "QC" & !is.na(Combined[[bias_col]]),
      ifelse(abs(Combined[[bias_col]]) <= qc_bias_limit, "PASS", "FAIL"),
      NA_character_
    )
  }
}

}  # end if (has_calibration)

Combined <- Combined %>% arrange(SampleName)

# =========================================================
# QC Accuracy Plots (per analyte)
# =========================================================
# Generates a faceted plot showing QC percent bias across the
# run with acceptance limits (+/- qc_bias_limit) for each
# calibration method (Peak Area and PA/IS Ratio).

qc_plot_data <- Combined %>% filter(Type == "QC")

if (nrow(qc_plot_data) > 0 && has_calibration) {

  for (analyte_name in names(analytes)) {
    prefix <- tolower(analyte_name)

    # Gather bias columns into long format for faceting
    bias_pa_col    <- paste0(prefix, "_percent_bias_pa")
    bias_ratio_col <- paste0(prefix, "_percent_bias_ratio")
    flag_pa_col    <- paste0(prefix, "_qc_flag_pa")
    flag_ratio_col <- paste0(prefix, "_qc_flag_ratio")

    # Skip if neither bias column exists
    if (!(bias_pa_col %in% names(qc_plot_data)) &&
        !(bias_ratio_col %in% names(qc_plot_data))) next

    # Build long-form data for each method
    plot_rows <- list()

    if (bias_pa_col %in% names(qc_plot_data)) {
      pa_df <- qc_plot_data %>%
        select(Row, CalLevel,
               PercentBias = !!sym(bias_pa_col),
               QCFlag      = !!sym(flag_pa_col)) %>%
        mutate(Method = "Peak Area")
      plot_rows[["pa"]] <- pa_df
    }

    if (bias_ratio_col %in% names(qc_plot_data)) {
      ratio_df <- qc_plot_data %>%
        select(Row, CalLevel,
               PercentBias = !!sym(bias_ratio_col),
               QCFlag      = !!sym(flag_ratio_col)) %>%
        mutate(Method = analyte_is_label[[analyte_name]])
      plot_rows[["ratio"]] <- ratio_df
    }

    # Drift-corrected PA facet (PETN only)
    if (prefix == "petn" &&
        "petn_percent_bias_pa_dc" %in% names(qc_plot_data) &&
        !all(is.na(qc_plot_data$petn_percent_bias_pa_dc))) {
      dc_df <- qc_plot_data %>%
        select(Row, CalLevel,
               PercentBias = petn_percent_bias_pa_dc,
               QCFlag      = petn_qc_flag_pa_dc) %>%
        mutate(Method = "Peak Area (Drift-Corrected)")
      plot_rows[["pa_dc"]] <- dc_df
    }

    qc_long <- bind_rows(plot_rows) %>%
      filter(!is.na(PercentBias))

    if (nrow(qc_long) == 0) next

    # Set explicit facet order: Peak Area, then Ratio (if present),
    # then Drift-Corrected (if present).  Without this, ggplot2 sorts
    # alphabetically, which puts "PA / PETN IS Ratio" before "Peak Area".
    method_order <- c("Peak Area")
    if ("ratio" %in% names(plot_rows)) {
      method_order <- c(method_order, analyte_is_label[[analyte_name]])
    }
    if ("pa_dc" %in% names(plot_rows)) {
      method_order <- c(method_order, "Peak Area (Drift-Corrected)")
    }
    qc_long$Method <- factor(qc_long$Method, levels = method_order)

    p_qc <- ggplot(qc_long, aes(x = Row, y = PercentBias,
                                 colour = factor(CalLevel),
                                 shape = QCFlag)) +
      geom_point(size = 3) +
      geom_line(aes(group = factor(CalLevel)), alpha = 0.4) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_hline(yintercept = c(-qc_bias_limit, qc_bias_limit),
                 linetype = "dashed", colour = "red") +
      facet_wrap(~Method, scales = "free_y") +
      scale_shape_manual(
        values = c("PASS" = 16, "FAIL" = 4),
        na.value = 1
      ) +
      theme_bw() +
      labs(
        title = paste(analyte_name, "- QC Accuracy Across Sequence"),
        x = "Injection Number",
        y = "% Bias",
        colour = "QC Level (ng)",
        shape = "QC Flag"
      )

    ggsave(
      paste0(analyte_name, "_QC_Accuracy.png"),
      p_qc,
      path = Results.dir,
      width = 12, height = 5, dpi = 300
    )
  }
}

# =========================================================
# Export final results
# =========================================================
outfile <- file.path(
  Results.dir,
  paste0(ParentFolder, "_GCMSResults.csv")
)
write.csv(Combined, outfile, row.names = FALSE)

# =========================================================
# QC Results Export to Monitoring File
# =========================================================
# Extracts QC rows and appends them to a long-term monitoring
# CSV for tracking instrument performance across runs.

if ("QC" %in% Combined$Type && has_calibration) {

  # Build QC export columns: core identifiers + per-analyte results
  qc_export_cols <- c("Date", "DataFile", "SampleName", "CalLevel",
                       "CalibrationSet",
                       "rdx_is_pa", "rdx_is_snr", "rdx_is_snr_flag",
                       "petn_is_pa", "petn_is_snr", "petn_is_snr_flag")

  for (name in names(analytes)) {
    prefix <- tolower(name)
    qc_export_cols <- c(qc_export_cols,
      paste0(prefix, "_pa"),
      paste0(prefix, "_snr"),
      paste0(prefix, "_snr_flag"),
      paste0(prefix, "_ratio"),
      paste0(prefix, "_concentration_pa"),
      paste0(prefix, "_range_flag_pa"),
      paste0(prefix, "_percent_bias_pa"),
      paste0(prefix, "_qc_flag_pa"),
      paste0(prefix, "_concentration_ratio"),
      paste0(prefix, "_range_flag_ratio"),
      paste0(prefix, "_percent_bias_ratio"),
      paste0(prefix, "_qc_flag_ratio")
    )
  }

  # Keep only columns that actually exist in Combined
  qc_export_cols <- intersect(qc_export_cols, names(Combined))

  QCResults <- Combined %>%
    filter(Type == "QC") %>%
    select(all_of(qc_export_cols))

  if (nrow(QCResults) > 0) {
    if (file.exists(qc_monitoring_file)) {
      ExistingQC <- read.csv(qc_monitoring_file, stringsAsFactors = FALSE)

      # Ensure Date column types match before combining
      ExistingQC$Date <- as.character(ExistingQC$Date)
      QCResults$Date  <- as.character(QCResults$Date)

      # Combine and remove duplicates by DataFile
      CombinedQC <- bind_rows(ExistingQC, QCResults) %>%
        distinct(DataFile, .keep_all = TRUE)

      # Backup old file before overwriting
      backup_dir <- dirname(qc_monitoring_file)
      qc_backup_name <- paste0(
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        "_SystemMonitoring_backup.csv"
      )
      write.csv(ExistingQC,
                file = file.path(backup_dir, qc_backup_name),
                row.names = FALSE)

      # Write updated monitoring file
      write.csv(CombinedQC, file = qc_monitoring_file, row.names = FALSE)

    } else {
      # Create new monitoring file
      # Ensure the directory exists
      dir.create(dirname(qc_monitoring_file), recursive = TRUE, showWarnings = FALSE)
      write.csv(QCResults, file = qc_monitoring_file, row.names = FALSE)
    }
  }
}

# ======================= END OF SCRIPT ====================
