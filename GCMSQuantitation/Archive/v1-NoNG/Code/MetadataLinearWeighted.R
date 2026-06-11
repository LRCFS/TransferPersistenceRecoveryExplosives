
# =========================================================
# GC-MS PETN/RDX Quantification Script (1/x Weighted Linear Calibration)
# =========================================================
#
# This script performs quantitative GC-MS analysis of multiple
# analytes (e.g. PETN, RDX) using external calibration with
# the following features:
#
#  - Data-driven: analytes defined in GlobalCode.R config
#  - Multiple calibration sets (bracketed in a sequence run)
#  - 1/x weighted linear calibration (analyte/IS ratio vs concentration)
#  - Back-calculation of calibration standards
#  - Protection against extrapolation
#  - Full plotting per analyte:
#       * One plot per calibration (curve + standards + samples)
#       * One combined plot of all calibrations with curves
#  - Calculation of adjusted concentrations and %bias
#  - QC sample accuracy assessment with pass/fail flagging
#  - QC results export to long-term monitoring file
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
  "DataFolder",         # folder containing sequence log
  "GcData.dir",         # folder containing *Summary.csv files
  "Results.dir",        # output folder
  "ParentFolder",       # name used for output CSV
  "Dilution",           # dilution factor applied to standards/samples
  "SampleVol",          # injected or extracted sample volume
  "DepositMass",        # reference mass for recovery calculation
  "is_config",          # list(mz, rt) for internal standard
  "analytes",           # named list of analyte definitions
  "qc_bias_limit",      # max allowable |%bias| for QC pass/fail
  "qc_monitoring_file", # path to long-term QC monitoring CSV
  "snr_detect",         # minimum SNR for peak detection (LOD)
  "snr_quant"           # minimum SNR for quantitation (LOQ)
)

missing_globals <- required_globals[!sapply(required_globals, exists, where = .GlobalEnv, inherits = FALSE)]
if (length(missing_globals) > 0) {
  stop("Missing required globals: ",
       paste(missing_globals, collapse = ", "))
}

# =========================================================
# Concentration solver (linear)
# =========================================================
# Inverts a linear calibration curve to obtain concentration
# from the observed response (peak area or PA/IS ratio).
# Extrapolation is explicitly blocked.
#
# Args:
#   y            : observed response value
#   lin_model    : linear lm model
#   cal_y_range  : valid response range of the calibration
#
# Returns:
#   list(value, method)
# =========================================================
solve_concentration <- function(y, lin_model, cal_y_range) {

  # Missing or absent peak
  if (is.na(y)) {
    return(list(value = NA_real_, method = "No_peak"))
  }

  # Block extrapolation outside calibration domain
  if (y < cal_y_range[1] || y > cal_y_range[2]) {
    return(list(value = NA_real_, method = "Out_of_range"))
  }

  # Invert linear model: y = intercept + slope * x  =>  x = (y - intercept) / slope
  lin <- coef(lin_model)
  slope <- lin[2]
  intercept <- lin[1]

  if (is.na(slope) || abs(slope) < .Machine$double.eps * 100) {
    return(list(value = NA_real_, method = "Unquantifiable"))
  }

  x <- (y - intercept) / slope
  if (x < 0) {
    return(list(value = NA_real_, method = "Unquantifiable"))
  }

  list(value = x, method = "Linear")
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
# IS Peak Area QC Chart
# =========================================================
is_plot_data <- Combined %>% filter(!is.na(is_pa))

if (nrow(is_plot_data) > 0) {
  p_is <- ggplot(is_plot_data, aes(x = Row, y = is_pa)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    theme_bw() +
    labs(
      title = "Internal Standard Peak Area Across Sequence",
      x = "Injection Number",
      y = "IS Peak Area"
    )

  ggsave(
    "IS_PeakArea_QC.png", p_is,
    path = Results.dir,
    width = 8, height = 5, dpi = 300
  )
} else {
  warning("No IS peak area data available -- skipping IS QC chart.")
}

# =========================================================
# Compute analyte/IS ratio columns
# =========================================================
for (name in names(analytes)) {
  prefix <- tolower(name)
  pa_col <- paste0(prefix, "_pa")
  ratio_col <- paste0(prefix, "_ratio")
  Combined[[ratio_col]] <- Combined[[pa_col]] / Combined[["is_pa"]]
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

# Initialise per-analyte result columns for both calibration methods
for (name in names(analytes)) {
  prefix <- tolower(name)
  for (suffix in c("_pa", "_ratio")) {
    Combined[[paste0(prefix, "_concentration", suffix)]] <- NA_real_
    Combined[[paste0(prefix, "_quant_method", suffix)]]  <- NA_character_
  }
}

# =========================================================
# Apply calibration models and generate plots (per analyte)
# =========================================================
# Two calibration methods per analyte:
#   "pa"    -- calibrate directly on peak area
#   "ratio" -- calibrate on (analyte PA / IS PA)

cal_stats <- list()  # accumulates R2/AdjR2 per analyte/method/set

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

  # Define the two calibration methods
  cal_methods <- list(
    pa    = list(response_col = pa_col,    y_label = "Peak Area"),
    ratio = list(response_col = ratio_col, y_label = "PA / IS Ratio")
  )

  # Storage for curves and per-set plot data, keyed by method
  all_curve_store <- list(pa = list(), ratio = list())
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

      # Build 1/x weighted linear model dynamically
      # Weighting by 1/concentration gives more importance to lower
      # calibration levels, improving accuracy at the low end of the range.
      lin_formula  <- reformulate("CalLevelAdj", response = resp_col)
      cal_weights  <- 1 / cal_fit$CalLevelAdj

      lin_model  <- lm(lin_formula,  data = cal_fit, weights = cal_weights)

      # Capture model fit statistics
      lin_smry  <- summary(lin_model)

      cal_stats[[length(cal_stats) + 1]] <- data.frame(
        Analyte        = analyte_name,
        CalibrationSet = i,
        Method         = method$y_label,
        N_Standards    = nrow(cal_fit),
        N_Excluded_LOD = nrow(cal) - nrow(cal_fit),
        R2             = lin_smry$r.squared,
        AdjR2          = lin_smry$adj.r.squared,
        stringsAsFactors = FALSE
      )

      cal_y_range <- range(cal_fit[[resp_col]], na.rm = TRUE)

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
          next
        }

        # Below_LOQ: peak is real but too imprecise -- do not quantify
        if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOQ") {
          Combined[[conc_col]][j]   <- NA_real_
          Combined[[method_col]][j] <- "Below_LOQ"
          next
        }

        # Quantifiable or no SNR data: proceed with calibration
        res <- solve_concentration(
          Combined[[resp_col]][j],
          lin_model,
          cal_y_range
        )
        Combined[[conc_col]][j]   <- res$value
        Combined[[method_col]][j] <- res$method
      }

      # Curve for plotting
      curve_x <- seq(min(cal$CalLevelAdj), max(cal$CalLevelAdj), length.out = 200)
      curve_df <- data.frame(
        CalibrationSet = i,
        CalLevelAdj = curve_x,
        Fitted = predict(lin_model, newdata = data.frame(CalLevelAdj = curve_x)),
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

    # Collect data for the two methods (PA and Ratio)
    ref_methods <- list(
      pa    = list(resp_col = pa_col,    y_label = "Peak Area"),
      ratio = list(resp_col = ratio_col, y_label = "PA / IS Ratio")
    )

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
      values_from = c(N_Standards, N_Excluded_LOD, R2, AdjR2),
      names_sep = "_"
    )

  # IS %RSD per calibration set
  is_rsd_per_set <- Combined %>%
    filter(!is.na(is_pa), !is.na(CalibrationSet)) %>%
    group_by(CalibrationSet) %>%
    summarise(
      IS_RSD_Percent = (sd(is_pa) / mean(is_pa)) * 100,
      .groups = "drop"
    )

  # IS %RSD overall (all injections)
  is_pa_all <- Combined$is_pa[!is.na(Combined$is_pa)]
  if (length(is_pa_all) >= 2) {
    is_rsd_overall <- (sd(is_pa_all) / mean(is_pa_all)) * 100
  } else {
    is_rsd_overall <- NA_real_
  }

  # Merge per-set IS %RSD and add overall
  cal_stats_wide <- cal_stats_wide %>%
    left_join(is_rsd_per_set, by = "CalibrationSet") %>%
    mutate(IS_RSD_Overall = is_rsd_overall)

  # Write Excel
  writexl::write_xlsx(
    cal_stats_wide,
    path = file.path(Results.dir, paste0(ParentFolder, "_CalibrationStats.xlsx"))
  )

} else {
  warning("No calibration statistics to export -- skipping CalibrationStats.xlsx.")
}

# =========================================================
# Final QC calculations (per analyte, per method)
# =========================================================
if (has_calibration) {

Combined <- Combined %>%
  mutate(TrueCalConcAdj = ifelse(Type %in% c("Cal", "QC"), CalLevel * Dilution, NA_real_))

for (name in names(analytes)) {
  prefix <- tolower(name)

  for (suffix in c("_pa", "_ratio")) {
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

# =========================================================
# QC pass/fail assessment
# =========================================================
for (name in names(analytes)) {
  prefix <- tolower(name)
  for (suffix in c("_pa", "_ratio")) {
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

    # Build long-form data for PA method
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
        mutate(Method = "PA / IS Ratio")
      plot_rows[["ratio"]] <- ratio_df
    }

    qc_long <- bind_rows(plot_rows) %>%
      filter(!is.na(PercentBias))

    if (nrow(qc_long) == 0) next

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
                       "CalibrationSet", "is_pa", "is_snr", "is_snr_flag")

  for (name in names(analytes)) {
    prefix <- tolower(name)
    qc_export_cols <- c(qc_export_cols,
      paste0(prefix, "_pa"),
      paste0(prefix, "_snr"),
      paste0(prefix, "_snr_flag"),
      paste0(prefix, "_concentration_pa"),
      paste0(prefix, "_percent_bias_pa"),
      paste0(prefix, "_qc_flag_pa"),
      paste0(prefix, "_concentration_ratio"),
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
