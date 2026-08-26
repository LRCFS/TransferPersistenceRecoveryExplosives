
# =========================================================
# GC-MS PETN/RDX Quantification Script (Simplified)
# =========================================================
#
# This script performs quantitative GC-MS analysis of PETN
# and RDX using 1/x weighted quadratic calibration.
#
# Key features:
#   - Data-driven: analytes defined in GlobalCode.R config
#   - Quadratic calibration only (no linear fallback)
#   - Single calibration method per analyte:
#       * PETN: ratio (petn_pa/rdx_is_pa) if use_15nrdx_for_petn=TRUE
#               else PA (petn_pa)
#       * RDX:  ratio (rdx_pa/rdx_is_pa)
#   - PETN drift correction (QC-based, optional)
#   - QC accuracy assessment with pass/fail flagging
#
# Author: <add name>
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

# -----------------------------
# Configuration toggle: use_15nrdx_for_petn
# -----------------------------
# NOTE: deliberately NOT `where = .GlobalEnv, inherits = FALSE` -- this script is
# sometimes sourced into an isolated environment (e.g. RunAllDatasets.R's
# dataset_env), and a literal .GlobalEnv check would miss config set there.
# Plain exists() (inherits = TRUE, default envir = current evaluation frame)
# correctly resolves whether this script is run at top level or sourced with
# local = <env>.
if (!exists("use_15nrdx_for_petn")) {
  use_15nrdx_for_petn <- TRUE
}

# -----------------------------
# Configuration toggle: use_is_for_rdx
# -----------------------------
if (!exists("use_is_for_rdx")) {
  use_is_for_rdx <- TRUE
}

# -----------------------------
# Configuration toggle: drift_correction_min_qc
# -----------------------------
# Minimum same-level QC points required to attempt a PowerLaw fit. Default
# (3) matches the general FINEX validation; GlobalCode.R relaxes this to 2
# for the ASTRA Swabbing pilot study. Fallback here only for standalone runs
# of this script without GlobalCode.R having set it.
if (!exists("drift_correction_min_qc")) {
  drift_correction_min_qc <- 3
}

# -----------------------------
# Required user-defined globals
# -----------------------------
required_globals <- c(
  "DataFolder",
  "GcData.dir",
  "Results.dir",
  "ParentFolder",
  "Dilution",
  "rdx_is_config",
  "analytes",
  "qc_bias_limit",
  "qc_monitoring_file",
  "snr_detect",
  "snr_quant",
  "apply_petn_drift_correction",
  "MinPeakArea_IS"
)

# NOTE: explicit envir = <this script's own execution frame> (captured below),
# NOT `where = .GlobalEnv, inherits = FALSE` and NOT bare exists() inside sapply().
# Bare exists() called via sapply()/lapply() does NOT reliably resolve the
# calling frame (a well-known R gotcha -- exists()'s default frame lookup sees
# sapply's internal frame, not this script's), so it would silently report every
# global as missing when this script is sourced into an isolated environment
# (e.g. RunAllDatasets.R's dataset_env). Capturing environment() explicitly and
# passing it as envir (with inherits = TRUE) works correctly whether this script
# runs at top level (.GlobalEnv) or is sourced with local = <env>.
.this_env <- environment()
missing_globals <- required_globals[!sapply(required_globals, exists, envir = .this_env, inherits = TRUE)]
rm(.this_env)
if (length(missing_globals) > 0) {
  stop("Missing required globals: ",
       paste(missing_globals, collapse = ", "))
}

# =========================================================
# Concentration solver (quadratic only)
# =========================================================
solve_concentration <- function(y, quad_model, cal_y_range) {

  if (is.na(y)) {
    return(list(value = NA_real_, method = "No_peak", extrapolated = NA))
  }

  coefs <- coef(quad_model)
  a <- coefs["I(CalLevelAdj^2)"]
  b <- coefs["CalLevelAdj"]
  c_int <- coefs["(Intercept)"]

  disc <- b^2 - 4 * a * (c_int - y)

  if (!is.finite(disc) || disc < 0 || abs(a) < .Machine$double.eps) {
    return(list(value = NA_real_, method = "Unquantifiable", extrapolated = NA))
  }

  roots <- c(
    (-b + sqrt(disc)) / (2 * a),
    (-b - sqrt(disc)) / (2 * a)
  )

  roots <- roots[is.finite(roots)]

  if (length(roots) == 0) {
    return(list(value = NA_real_, method = "Unquantifiable", extrapolated = NA))
  }

  # Select smallest positive root (physically meaningful concentration)
  # If no positive root exists, floor at zero (below-range extrapolation)
  positive_roots <- roots[roots >= 0]
  if (length(positive_roots) > 0) {
    concentration <- min(positive_roots)
  } else {
    concentration <- 0
  }

  # Flag responses outside the fitted calibration range (cal_y_range is the
  # min/max of the calibrator response values actually used to fit
  # quad_model). Because the calibration is quadratic (non-monotonic in
  # general), a response outside this range means `concentration` above is
  # an unvalidated extrapolation, not an interpolated result -- flag it so
  # downstream reporting/QC can distinguish the two rather than treating
  # every "Quadratic" result as equally defensible.
  is_extrapolated <- length(cal_y_range) == 2 &&
    all(is.finite(cal_y_range)) &&
    (y < cal_y_range[1] || y > cal_y_range[2])

  list(value = concentration, method = "Quadratic", extrapolated = is_extrapolated)
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

metadata_lines <- log_lines[str_detect(log_lines, "^\\s*\\d+\\)")]

if (length(metadata_lines) == 0) {
  stop("No numbered sequence lines (e.g. '1) ...') found in: ",
       paste(log_files, collapse = ", "))
}

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
# Compute ratio columns (with division-by-zero protection)
# =========================================================
# If rdx_is_pa is NA, zero, or negative, ratio is set to NA
# and a warning is issued indicating the affected rows.
invalid_is <- is.na(Combined$rdx_is_pa) | Combined$rdx_is_pa <= 0
if (any(invalid_is)) {
  warning(sum(invalid_is), " injection(s) have invalid IS peak area (NA or <= 0). ",
          "Ratios set to NA for these rows.")
}
Combined$petn_ratio <- ifelse(invalid_is, NA_real_, Combined$petn_pa / Combined$rdx_is_pa)
Combined$rdx_ratio  <- ifelse(invalid_is, NA_real_, Combined$rdx_pa / Combined$rdx_is_pa)

# =========================================================
# IS (15N-RDX) injection-validity flag
# =========================================================
# Confirms the internal standard was actually present at a reliable level in
# this injection, independent of what the analyte peaks show. A peak can be
# "detected" (SNR/PH above the peak-detection gate) yet still sit at
# blank-channel noise level, which does not confirm the IS was genuinely
# injected -- see MinPeakArea_IS in GlobalCode.R for the evidence behind the
# threshold. Not computed for Blanks (no IS is spiked into blank vials, so a
# low/absent IS signal there is expected, not a failure).
Combined$rdx_is_pa_flag <- ifelse(
  Combined$Type == "Blank", NA_character_,
  ifelse(is.na(Combined$rdx_is_pa), "NOT_DETECTED",
         ifelse(Combined$rdx_is_pa < MinPeakArea_IS, "LOW", "OK"))
)

n_is_not_detected <- sum(Combined$rdx_is_pa_flag == "NOT_DETECTED", na.rm = TRUE)
n_is_low <- sum(Combined$rdx_is_pa_flag == "LOW", na.rm = TRUE)
if (n_is_not_detected > 0 || n_is_low > 0) {
  message("  IS injection-validity check: ", n_is_not_detected,
          " injection(s) with IS not detected, ", n_is_low,
          " injection(s) with IS below ", MinPeakArea_IS, " PA (LOW).")
}

# =========================================================
# Build calibration sets
# =========================================================
CalData <- Combined %>%
  filter(Type == "Cal", !is.na(CalLevel)) %>%
  arrange(Row) %>%
  mutate(CalLevelAdj = CalLevel * Dilution)

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

# Initialise per-analyte result columns
for (name in names(analytes)) {
  prefix <- tolower(name)

  Combined[[paste0(prefix, "_concentration")]] <- NA_real_
  # TRUE when the response used for quantification fell outside the fitted
  # calibration curve's response range (cal_y_range) -- i.e. the reported
  # concentration is an unvalidated extrapolation from a non-monotonic
  # quadratic, not an interpolated result. NA where no concentration was
  # attempted (Below_LOD / no peak).
  Combined[[paste0(prefix, "_extrapolated")]] <- NA
}

# Add petn_quant_method column
Combined$petn_quant_method <- if (use_15nrdx_for_petn) "15N-RDX_Ratio" else "Peak_Area"

# Add rdx_quant_method column
Combined$rdx_quant_method <- if (use_is_for_rdx) "15N-RDX_Ratio" else "Peak_Area"

# =========================================================
# Apply calibration models and generate plots
# =========================================================
cal_stats <- list()

stored_petn_models <- list()
stored_rdx_models <- list()

if (has_calibration) {

for (analyte_name in names(analytes)) {

  prefix <- tolower(analyte_name)
  pa_col <- paste0(prefix, "_pa")

  if (!(pa_col %in% names(Combined)) || all(is.na(Combined[[pa_col]]))) {
    warning("All ", pa_col, " values are NA -- skipping ", analyte_name, " calibration.")
    next
  }

  if (analyte_name == "PETN") {
    if (use_15nrdx_for_petn) {
      resp_col <- "petn_ratio"
      y_label <- "PA / 15N-RDX IS Ratio"
    } else {
      resp_col <- "petn_pa"
      y_label <- "Peak Area"
    }
  } else if (analyte_name == "RDX") {
    if (use_is_for_rdx) {
      resp_col <- "rdx_ratio"
      y_label <- "PA / 15N-RDX IS Ratio"
    } else {
      resp_col <- "rdx_pa"
      y_label <- "Peak Area"
    }
  } else {
    next
  }

  all_curve_store <- list()
  all_set_plots   <- list()

  for (i in seq_along(CalSets)) {

    cal <- CalSets[[i]]

    rows_std <- Combined$Row %in% cal$Row
    rows_qc_smp <- Combined$Row > max(cal$Row)
    if (i < length(CalSets)) {
      rows_qc_smp <- rows_qc_smp & Combined$Row < min(CalSets[[i + 1]]$Row)
    }
    # Explicitly include QC rows that belong to this calibration set
    if (i == length(CalSets)) {
      # Last cal set: include all QCs after this cal
      rows_qc <- Combined$Type == "QC" & Combined$Row > max(cal$Row)
    } else {
      # Not last cal set: include QCs after this cal but before next cal
      rows_qc <- Combined$Type == "QC" & 
                 Combined$Row > max(cal$Row) & 
                 Combined$Row < min(CalSets[[i + 1]]$Row)
    }
    rows <- which(rows_std | rows_qc_smp | rows_qc)

    Combined$CalibrationSet[rows] <- i

    snr_flag_col <- paste0(prefix, "_snr_flag")
    if (snr_flag_col %in% names(cal)) {
      cal_fit <- cal %>%
        dplyr::filter(is.na(.data[[snr_flag_col]]) | .data[[snr_flag_col]] != "Below_LOD")
    } else {
      cal_fit <- cal
    }

    if (nrow(cal_fit) == 0 || all(is.na(cal_fit[[resp_col]]))) {
      warning(analyte_name, " calibration set ", i,
              ": all response values NA (or Below_LOD) -- skipping.")
      next
    }

    cal_weights  <- 1 / cal_fit$CalLevelAdj

    quad_formula <- reformulate(c("CalLevelAdj", "I(CalLevelAdj^2)"), response = resp_col)
    quad_model   <- lm(quad_formula, data = cal_fit, weights = cal_weights)

    quad_smry <- summary(quad_model)

    cal_stats[[length(cal_stats) + 1]] <- data.frame(
      Analyte        = analyte_name,
      CalibrationSet = i,
      Method         = y_label,
      N_Standards    = nrow(cal_fit),
      N_Excluded_LOD = nrow(cal) - nrow(cal_fit),
      R2             = quad_smry$r.squared,
      AdjR2          = quad_smry$adj.r.squared,
      stringsAsFactors = FALSE
    )

    cal_y_range <- range(cal_fit[[resp_col]], na.rm = TRUE)

    if (analyte_name == "PETN") {
      stored_petn_models[[i]] <- list(
        quad_model = quad_model,
        cal_y_range = cal_y_range,
        rows = rows,
        snr_flag_col = snr_flag_col
      )
    }

    if (analyte_name == "RDX") {
      stored_rdx_models[[i]] <- list(
        quad_model = quad_model,
        cal_y_range = cal_y_range,
        rows = rows,
        snr_flag_col = snr_flag_col
      )
    }

    conc_col  <- paste0(prefix, "_concentration")
    extrap_col <- paste0(prefix, "_extrapolated")

    for (j in rows) {

      row_snr_flag <- NA_character_
      if (snr_flag_col %in% names(Combined)) {
        row_snr_flag <- Combined[[snr_flag_col]][j]
      }

      if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
        Combined[[conc_col]][j]   <- NA_real_
        next
      }

      # Below_LOQ samples still get quantified (signal is present, just low precision)
      # Skip quantification only for Below_LOD (no reliable signal)
      if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
        Combined[[conc_col]][j]   <- NA_real_
        next
      }

      res <- solve_concentration(
        Combined[[resp_col]][j],
        quad_model,
        cal_y_range
      )
      Combined[[conc_col]][j]   <- res$value / Dilution
      Combined[[extrap_col]][j] <- res$extrapolated
    }

    curve_x <- seq(min(cal$CalLevelAdj), max(cal$CalLevelAdj), length.out = 200)
    curve_df <- data.frame(
      CalibrationSet = i,
      Xplot = curve_x / Dilution,
      Fitted = predict(quad_model, newdata = data.frame(CalLevelAdj = curve_x)),
      Method = y_label
    )
    all_curve_store[[i]] <- curve_df

    pd <- Combined %>%
      filter(CalibrationSet == i & !is.na(.data[[resp_col]])) %>%
      mutate(
        Xplot  = ifelse(Type %in% c("Cal", "QC"), CalLevel, .data[[conc_col]]),
        y_val  = .data[[resp_col]],
        Group  = case_when(
          Type == "Cal" ~ "Standard",
          Type == "QC"  ~ "QC",
          TRUE          ~ "Sample"
        ),
        SNR_Flag = if (snr_flag_col %in% names(.)) .data[[snr_flag_col]] else NA_character_,
        Method = y_label
      )
    all_set_plots[[i]] <- pd

    if (nrow(pd) > 0 && nrow(curve_df) > 0) {
      p <- ggplot() +
        geom_point(
          data = pd,
          aes(Xplot, y_val, color = Group, shape = SNR_Flag),
          size = 3
        ) +
        scale_shape_manual(
          values = c("Quantifiable" = 16, "Below_LOQ" = 17, "Below_LOD" = 4),
          na.value = 1
        ) +
        geom_line(
          data = curve_df,
          aes(Xplot, Fitted),
          color = "black"
        ) +
        theme_bw() +
        labs(
          title = paste(analyte_name, "- Calibration", i),
          x = paste(analyte_name, "concentration"),
          y = y_label,
          shape = "SNR Flag"
        )

      ggsave(
        paste0(analyte_name, "_Calibration_", i, ".png"),
        p,
        path = Results.dir,
        width = 8,
        height = 5,
        dpi = 300
      )
    }
  }

  all_cals <- Combined %>%
    filter(Type %in% c("Cal", "QC")) %>%
    mutate(Xplot = CalLevel)

  if (!all(is.na(all_cals[[resp_col]]))) {
    all_cals$y_val <- all_cals[[resp_col]]

    all_curves <- bind_rows(all_curve_store)

    if (nrow(all_curves) > 0) {
      p_all <- ggplot() +
        geom_point(
          data = all_cals,
          aes(Xplot, y_val, color = factor(CalibrationSet), shape = Type),
          size = 3
        ) +
        scale_shape_manual(values = c("Cal" = 16, "QC" = 17)) +
        geom_line(
          data = all_curves,
          aes(Xplot, Fitted, color = factor(CalibrationSet)),
          linewidth = 1
        ) +
        theme_bw() +
        labs(
          title = paste(analyte_name, "- All Calibration Curves"),
          x = paste(analyte_name, "concentration"),
          y = y_label,
          color = "Calibration Set"
        )

      ggsave(
        paste0(analyte_name, "_All_Calibrations.png"),
        p_all,
        path = Results.dir,
        width = 10,
        height = 6,
        dpi = 300
      )
    }
  }
}

}  # end if (has_calibration)

# =========================================================
# Single-level standard reference plots (no calibration)
# =========================================================
if (single_level_cal) {

  std_conc <- unique(CalData$CalLevelAdj)

  for (analyte_name in names(analytes)) {

    prefix <- tolower(analyte_name)
    pa_col <- paste0(prefix, "_pa")

    if (!(pa_col %in% names(Combined)) || all(is.na(Combined[[pa_col]]))) next

    if (analyte_name == "PETN") {
      if (use_15nrdx_for_petn) {
        resp_col <- "petn_ratio"
        y_label <- "PA / 15N-RDX IS Ratio"
      } else {
        resp_col <- "petn_pa"
        y_label <- "Peak Area"
      }
    } else if (analyte_name == "RDX") {
      if (use_is_for_rdx) {
        resp_col <- "rdx_ratio"
        y_label <- "PA / 15N-RDX IS Ratio"
      } else {
        resp_col <- "rdx_pa"
        y_label <- "Peak Area"
      }
    } else {
      next
    }

    if (all(is.na(Combined[[resp_col]]))) next

    snr_flag_col <- paste0(prefix, "_snr_flag")

    ref_plot_data <- Combined %>%
      filter(!is.na(.data[[resp_col]])) %>%
      mutate(
        Response = .data[[resp_col]],
        Group = case_when(
          Type == "Cal" ~ "Standard",
          Type == "QC"  ~ "QC",
          TRUE          ~ "Sample"
        ),
        SNR_Flag = if (snr_flag_col %in% names(.)) .data[[snr_flag_col]] else NA_character_,
        Method = y_label
      )

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
          summarise(MeanResp = mean(Response)),
        aes(yintercept = MeanResp),
        linetype = "dashed", colour = "grey40"
      ) +
      theme_bw() +
      labs(
        title = paste0(analyte_name,
                       " - Single-Level Standard Reference (",
                       std_conc, " ng)"),
        x = "Injection Number",
        y = y_label,
        colour = "Type",
        shape  = "SNR Flag"
      )

    ggsave(
      paste0(analyte_name, "_SingleLevel_Reference.png"),
      p_ref,
      path = Results.dir,
      width = 10, height = 5, dpi = 300
    )
  }
}

# =========================================================
# Calibration Statistics Excel Export
# =========================================================
cal_stats_df <- bind_rows(cal_stats)

if (nrow(cal_stats_df) > 0) {

  cal_stats_wide <- cal_stats_df %>%
    tidyr::pivot_wider(
      id_cols = CalibrationSet,
      names_from = c(Analyte, Method),
      values_from = c(N_Standards, N_Excluded_LOD, R2, AdjR2),
      names_sep = "_"
    )

  rdx_is_rsd_per_set <- Combined %>%
    filter(!is.na(rdx_is_pa), !is.na(CalibrationSet)) %>%
    group_by(CalibrationSet) %>%
    summarise(
      RDX_IS_RSD_Percent = (sd(rdx_is_pa) / mean(rdx_is_pa)) * 100,
      .groups = "drop"
    )

  rdx_is_pa_all <- Combined$rdx_is_pa[!is.na(Combined$rdx_is_pa)]
  if (length(rdx_is_pa_all) >= 2) {
    rdx_is_rsd_overall <- (sd(rdx_is_pa_all) / mean(rdx_is_pa_all)) * 100
  } else {
    rdx_is_rsd_overall <- NA_real_
  }

  cal_stats_wide <- cal_stats_wide %>%
    left_join(rdx_is_rsd_per_set, by = "CalibrationSet") %>%
    mutate(RDX_IS_RSD_Overall = rdx_is_rsd_overall)

  writexl::write_xlsx(
    cal_stats_wide,
    path = file.path(Results.dir, paste0(ParentFolder, "_CalibrationStats.xlsx"))
  )

} else {
  warning("No calibration statistics to export -- skipping CalibrationStats.xlsx.")
}

# =========================================================
# PETN Drift Correction (QC-based: Power Law or Polynomial)
# =========================================================
Combined$petn_drift_correction_factor <- 1.0
Combined$petn_drift_model             <- NA_character_
Combined$petn_concentration_dc        <- Combined$petn_concentration
Combined$petn_extrapolated_dc         <- Combined$petn_extrapolated

# Uncertainty propagated from the drift-correction step itself (item 19 --
# previously the correction factor was applied as if exact). Deliberately
# scoped to ONLY the drift-correction step's own contribution (via the
# delta method through the fitted drift curve, then through the local
# slope of the quadratic calibration curve) -- NOT a full measurement
# uncertainty budget (peak-area/integration uncertainty itself is a
# separate, unaddressed problem, out of scope here). NA when no drift
# correction is applied at all, or when the fit is an exact/degenerate
# interpolation with zero residual degrees of freedom (uncertainty is
# mathematically not estimable from such a fit, so NA rather than a
# fabricated number -- see the "zero residual df" messages below).
Combined$petn_dc_cf_se                <- 0
Combined$petn_concentration_dc_se     <- NA_real_


if (apply_petn_drift_correction && has_calibration && length(stored_petn_models) > 0) {

  if (use_15nrdx_for_petn) {
    resp_col_for_drift <- "petn_ratio"
  } else {
    resp_col_for_drift <- "petn_pa"
  }

  qc_candidates <- Combined %>%
    filter(Type == "QC", !is.na(.data[[resp_col_for_drift]]), !is.na(CalLevel))

  if (nrow(qc_candidates) > 0) {
    
    if (!is.na(drift_correction_qc_level) && 
        drift_correction_qc_level %in% qc_candidates$CalLevel) {
      selected_qc_level <- drift_correction_qc_level
    } else if (!is.na(drift_correction_qc_level)) {
      selected_qc_level <- max(qc_candidates$CalLevel, na.rm = TRUE)
      warning("PETN drift correction: specified QC level ", drift_correction_qc_level,
              " ng not found; using ", selected_qc_level, " ng")
    } else {
      selected_qc_level <- max(qc_candidates$CalLevel, na.rm = TRUE)
    }

    qc_drift_data_all <- Combined %>%
      filter(Type == "QC",
             CalLevel == selected_qc_level,
             !is.na(.data[[resp_col_for_drift]])) %>%
      arrange(Row) %>%
      select(Row, Line, CalibrationSet, Response = !!sym(resp_col_for_drift))

    # Optionally exclude specific QC injections (by sequence Line number,
    # drift_correction_exclude_qc_lines in GlobalCode.R) from the drift
    # MODEL FIT itself -- e.g. an anomalous QC whose own shape doesn't
    # match the rest of the run. An excluded QC is NOT removed from
    # Combined or from QC evaluation: it still receives a correction
    # factor from the resulting fit below (extrapolated, since it's no
    # longer one of the fitted points) and is still evaluated for its own
    # bias/PASS-FAIL exactly like any other QC further down this script --
    # this only controls which points the curve is fit through.
    qc_drift_data <- qc_drift_data_all %>%
      filter(!(Line %in% drift_correction_exclude_qc_lines))

    n_excluded_qc <- nrow(qc_drift_data_all) - nrow(qc_drift_data)
    if (n_excluded_qc > 0) {
      excluded_lines_present <- intersect(drift_correction_exclude_qc_lines, qc_drift_data_all$Line)
      message("  PETN drift correction: excluding ", n_excluded_qc,
              " QC point(s) from the fit (Line ", paste(excluded_lines_present, collapse = ", "),
              ") per drift_correction_exclude_qc_lines -- still evaluated for its own bias/PASS-FAIL using the resulting fit.")
    }

    n_qc <- nrow(qc_drift_data)
    
    # Continuous power law/polynomial models generally need at least 3
    # points to fit a meaningful curve; drift_correction_min_qc (set in
    # GlobalCode.R) relaxes this to 2 for the ASTRA Swabbing pilot study,
    # deliberately allowing a degenerate/exact 2-point fit rather than
    # skipping correction outright -- see the comment on
    # drift_correction_min_qc in GlobalCode.R.
    min_qc_required <- drift_correction_min_qc
    
    if (n_qc >= min_qc_required) {
      message("  PETN drift correction: using ", n_qc,
              " QC points at ", selected_qc_level, " ng")
      
      drift_model  <- NULL
      drift_method <- "None"
      predicted_cf <- rep(1.0, nrow(Combined))
      r2_val <- NA_real_
      power_a <- NA_real_
      power_b <- NA_real_
      
      # ===== POWER LAW METHOD =====
      # Fit whatever QC data exists for this run with no per-dataset shape
      # exclusions (see drift_correction_method comment in GlobalCode.R).
      # n_qc >= 2 is the absolute minimum for any fit at all; the actual
      # policy decision (whether to attempt a fit with as few as 2 points)
      # is already made by min_qc_required above.
      if (drift_correction_method == "power_law" && n_qc >= 2) {
        
        message("  Fitting power law: Response ~ a * Row^b (weighted 1/Response^2,")
        message("  consistent with the 1/x-weighted calibration curve elsewhere in ",
                "this script -- see item 19 notes: an unweighted fit here would ",
                "under-report the drift-correction uncertainty added below if QC ",
                "peak-area noise is proportional/relative, as calibration weighting ",
                "elsewhere in this script already assumes it is).")
        
        power_model <- tryCatch({
          nls(Response ~ a * Row^b,
              data = qc_drift_data,
              start = list(a = max(qc_drift_data$Response), b = -1.0),
              weights = 1 / qc_drift_data$Response^2,
              control = nls.control(maxiter = 200, warnOnly = TRUE))
        }, error = function(e) {
          warning("  Power law fit failed: ", e$message, ". Falling back to polynomial.")
          return(NULL)
        })
        
        if (!is.null(power_model)) {
          coefs <- coef(power_model)
          power_a <- coefs["a"]
          power_b <- coefs["b"]
          
          residuals_power <- residuals(power_model)
          ss_res <- sum(residuals_power^2)
          ss_tot <- sum((qc_drift_data$Response - mean(qc_drift_data$Response))^2)
          r2_val <- if (ss_tot > 0) 1 - (ss_res / ss_tot) else NA_real_
          
          if (n_qc == 2) {
            message("  Note: PowerLaw fit with only 2 QC points has zero residual ",
                    "degrees of freedom -- this is an exact/degenerate fit, not a ",
                    "meaningful curve fit; R\u00B2 is not informative here.")
          } else if (!is.na(r2_val) && r2_val < 0.90) {
            warning("  Power law R\u00B2 = ", round(r2_val, 3), " (< 0.90) -- fit may be poor.")
          }
          
          first_qc_row <- qc_drift_data$Row[1]
          reference_response <- predict(power_model, newdata = data.frame(Row = first_qc_row))
          predicted_response <- predict(power_model, newdata = data.frame(Row = Combined$Row))
          
          predicted_cf <- reference_response / predicted_response
          predicted_cf[Combined$Row <= first_qc_row] <- 1.0
          
          # Delta-method SE of the correction factor (item 19). The "a"
          # parameter cancels out of the ratio entirely --
          # cf(Row) = (first_qc_row/Row)^b -- so cf depends only on b,
          # which makes the delta method exact and simple:
          # d(cf)/db = cf * ln(first_qc_row/Row), so
          # SE(cf) = |cf * ln(first_qc_row/Row)| * SE(b).
          # Only defined when the fit has residual df > 0 -- an
          # exact/degenerate fit (e.g. n_qc == 2) has no estimable
          # residual variance, so SE(b) is not meaningful there.
          df_resid_power <- n_qc - length(coefs)
          if (df_resid_power > 0) {
            var_b <- tryCatch(vcov(power_model)["b", "b"], error = function(e) NA_real_)
            if (!is.na(var_b) && is.finite(var_b) && var_b >= 0) {
              log_ratio <- log(first_qc_row / Combined$Row)
              cf_se <- abs(predicted_cf * log_ratio) * sqrt(var_b)
              cf_se[Combined$Row <= first_qc_row] <- 0
            } else {
              cf_se <- rep(NA_real_, nrow(Combined))
            }
          } else {
            message("  Note: PowerLaw fit has zero residual degrees of freedom ",
                     "(n_qc = ", n_qc, ") -- drift-correction uncertainty is not ",
                     "estimable from this fit (petn_dc_cf_se will be NA for this dataset).")
            cf_se <- rep(NA_real_, nrow(Combined))
          }
          
          drift_model <- power_model
          drift_method <- "PowerLaw"
          
          message("  Power law fit: a = ", format(power_a, scientific = FALSE),
                  ", b = ", round(power_b, 4))
          message("  R\u00B2 = ", if (is.na(r2_val)) "N/A" else round(r2_val, 4))
        }
      }
      
      # ===== POLYNOMIAL METHOD (FALLBACK OR EXPLICIT) =====
      if (is.null(drift_model)) {
        
        if (drift_correction_method == "power_law") {
          message("  Falling back to polynomial method...")
        }
        
        ref_val <- qc_drift_data$Response[1]
        
        if (is.finite(ref_val) && ref_val > 0) {
          qc_drift <- qc_drift_data %>%
            mutate(CorrFactor = ref_val / Response) %>%
            filter(is.finite(CorrFactor), CorrFactor > 0)
          
          if (nrow(qc_drift) >= 2) {
            
            if (nrow(qc_drift) >= 4) {
              drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift, weights = 1 / qc_drift$CorrFactor^2)
              drift_method <- "Quadratic"
              
              r2 <- summary(drift_model)$r.squared
              if (is.na(r2) || r2 < 0.5) {
                message("  Quadratic R\u00B2 = ", round(r2, 3), " (poor); falling back to linear.")
                drift_model <- lm(CorrFactor ~ Row, data = qc_drift, weights = 1 / qc_drift$CorrFactor^2)
                drift_method <- "Linear"
              }
              
            } else if (nrow(qc_drift) == 3) {
              drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift, weights = 1 / qc_drift$CorrFactor^2)
              drift_method <- "Quadratic"
              
            } else if (nrow(qc_drift) == 2) {
              drift_model <- lm(CorrFactor ~ Row, data = qc_drift, weights = 1 / qc_drift$CorrFactor^2)
              drift_method <- "Linear"
            }
            
            if (!is.null(drift_model)) {
              pred_result <- predict(drift_model, newdata = data.frame(Row = Combined$Row), se.fit = TRUE)
              predicted_cf <- pred_result$fit
              first_qc_row <- qc_drift$Row[1]
              predicted_cf[Combined$Row <= first_qc_row] <- 1.0
              
              # SE of the fitted correction factor (item 19) -- predict.lm()
              # gives this natively here since CorrFactor (the correction
              # factor itself) is the response variable in this branch, no
              # delta method needed. Only defined when residual df > 0 --
              # an exact/degenerate fit (e.g. Linear through 2 points, or
              # Quadratic through exactly 3) has no estimable residual
              # variance.
              df_resid_poly <- drift_model$df.residual
              if (df_resid_poly > 0) {
                cf_se <- pred_result$se.fit
                cf_se[Combined$Row <= first_qc_row] <- 0
              } else {
                message("  Note: ", drift_method, " fit has zero residual degrees ",
                        "of freedom (n_qc = ", nrow(qc_drift), ") -- drift-correction ",
                        "uncertainty is not estimable from this fit (petn_dc_cf_se ",
                        "will be NA for this dataset).")
                cf_se <- rep(NA_real_, nrow(Combined))
              }
              
              r2_val <- summary(drift_model)$r.squared
            }
          }
        }
      }
      
      # ===== APPLY CORRECTION =====
      if (!is.null(drift_model)) {
        
        if (any(predicted_cf < 0, na.rm = TRUE)) {
          warning("  Some correction factors negative -- clamping to 1.0.")
          cf_se[predicted_cf < 0] <- NA_real_
          predicted_cf[predicted_cf < 0] <- 1.0
        }
        if (any(predicted_cf > 10.0, na.rm = TRUE)) {
          warning("  Some correction factors exceed 10.0 -- severe drift.")
        }
        
        Combined$petn_drift_correction_factor <- predicted_cf
        Combined$petn_drift_model             <- drift_method
        Combined$petn_dc_cf_se                <- cf_se
        
        corrected_resp <- Combined[[resp_col_for_drift]] * predicted_cf
        
        # SE of the drift-corrected response, propagated from the
        # correction factor's own SE only (item 19) -- the raw response
        # itself is treated as fixed/known here; its own measurement
        # uncertainty is a separate, unaddressed problem, out of scope.
        corrected_resp_se <- abs(Combined[[resp_col_for_drift]]) * cf_se
        
        if (use_15nrdx_for_petn) {
          Combined$petn_ratio_dc <- corrected_resp
        } else {
          Combined$petn_pa_dc <- corrected_resp
        }
        
        for (model_idx in seq_along(stored_petn_models)) {
          m <- stored_petn_models[[model_idx]]
          
          for (j in m$rows) {
            row_snr_flag <- NA_character_
            if (m$snr_flag_col %in% names(Combined)) {
              row_snr_flag <- Combined[[m$snr_flag_col]][j]
            }
            
            # Only skip drift correction for Below_LOD (no signal)
            # Below_LOQ samples get drift correction applied
            if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
              Combined$petn_concentration_dc[j] <- NA_real_
              next
            }
            
            res <- solve_concentration(
              corrected_resp[j],
              m$quad_model,
              m$cal_y_range
            )
            
            Combined$petn_concentration_dc[j] <- res$value / Dilution
            Combined$petn_extrapolated_dc[j]  <- res$extrapolated
            
            # Propagate the drift-correction uncertainty one step further,
            # through the local slope of the (quadratic) calibration curve
            # at this solved concentration, via the delta method: since
            # y = a*x^2 + b*x + c, dy/dx = 2*a*x + b at the solution
            # x = concentration, so Var(concentration) ~=
            # [1/(2*a*x+b)]^2 * Var(corrected_response).
            if (!is.na(res$value) && !is.na(corrected_resp_se[j]) &&
                res$method == "Quadratic") {
              qcoefs <- coef(m$quad_model)
              qa <- qcoefs["I(CalLevelAdj^2)"]
              qb <- qcoefs["CalLevelAdj"]
              local_slope <- 2 * qa * res$value + qb
              if (is.finite(local_slope) && abs(local_slope) > .Machine$double.eps) {
                Combined$petn_concentration_dc_se[j] <- abs(corrected_resp_se[j] / local_slope) / Dilution
              } else {
                Combined$petn_concentration_dc_se[j] <- NA_real_
              }
            } else {
              Combined$petn_concentration_dc_se[j] <- NA_real_
            }
          }
        }
        
        message("  PETN drift correction applied:")
        message("    Model: ", drift_method)
        message("    QC points used: ", n_qc)
        if (is.na(r2_val)) {
          message("    R\u00B2: N/A (constant correction, no regression fit)")
        } else {
          message("    R\u00B2: ", round(r2_val, 4))
        }
        if (drift_method == "PowerLaw") {
          message("    Exponent (b): ", round(power_b, 4))
        }
        message("    Correction factor range: ",
                round(min(predicted_cf, na.rm = TRUE), 4), " - ",
                round(max(predicted_cf, na.rm = TRUE), 4))
        message("    Correction factor SE range (item 19): ",
                if (all(is.na(cf_se))) "N/A (zero residual df -- see note above)" else
                  paste0(round(min(cf_se, na.rm = TRUE), 4), " - ", round(max(cf_se, na.rm = TRUE), 4)))
        
        # Generate diagnostic plot
        first_qc_row <- qc_drift_data$Row[1]
        
        if (drift_method == "PowerLaw") {
          # Power law plot: show PA decay and fitted curve.
          # Curve is drawn only within the observed Row range of ALL same-
          # level QCs (qc_drift_data_all), not just the ones actually used
          # to fit the model -- if a QC was excluded via
          # drift_correction_exclude_qc_lines, the curve still needs to
          # extend out to its Row position so the plot doesn't look
          # truncated relative to where that point is marked. Otherwise
          # unchanged from before: outside this range the fit is
          # unvalidated extrapolation, and for degenerate low-n fits (e.g.
          # a 2-QC run) the extrapolated curve can be orders of magnitude
          # larger than the actual data, making the plot unreadable. The
          # correction factors applied to real Sample/QC rows are NOT
          # affected by this -- this only restricts what's drawn.
          pred_line <- data.frame(
            Row = seq(min(qc_drift_data_all$Row), max(qc_drift_data_all$Row), length.out = 200)
          )
          pred_line$Response <- predict(power_model, newdata = pred_line)
          
          p_drift <- ggplot() +
            geom_line(data = pred_line,
                      aes(x = Row, y = Response),
                      colour = "steelblue", linewidth = 1) +
            geom_point(data = qc_drift_data,
                       aes(x = Row, y = Response),
                       colour = "red", size = 4, shape = 17)
          
          # Excluded QC(s) (drift_correction_exclude_qc_lines): plotted
          # distinctly (hollow orange X) so it's visible at a glance which
          # points were left out of the fit -- these still show their real
          # measured Response and still get a correction factor/bias from
          # the resulting curve, they just weren't used to fit it.
          if (n_excluded_qc > 0) {
            excluded_points <- qc_drift_data_all %>% filter(Line %in% drift_correction_exclude_qc_lines)
            p_drift <- p_drift +
              geom_point(data = excluded_points,
                         aes(x = Row, y = Response),
                         colour = "darkorange", size = 5, shape = 4, stroke = 1.5)
          }
          
          p_drift <- p_drift +
            theme_bw() +
            labs(
              title = paste0("PETN Power Law Drift (R\u00B2 = ", round(r2_val, 4), ")"),
              subtitle = paste0("PA ~ ", format(power_a, digits = 4, scientific = FALSE),
                               " \u00d7 Row^", round(power_b, 3),
                               if (n_excluded_qc > 0) " | orange X = excluded from fit" else ""),
              x = "Injection Number",
              y = ifelse(use_15nrdx_for_petn, "PETN Ratio", "PETN Peak Area")
            )
          
        } else {
          # Polynomial plot: show correction factors
          qc_cf <- data.frame(
            Row = qc_drift_data$Row,
            CorrFactor = predicted_cf[match(qc_drift_data$Row, Combined$Row)]
          )
          
          pred_line <- data.frame(
            Row = seq(min(Combined$Row), max(Combined$Row), length.out = 200)
          )
          pred_line$CorrFactor <- predict(drift_model, newdata = pred_line)
          
          sample_points <- data.frame(
            Row = Combined$Row,
            CorrFactor = predicted_cf,
            Type = Combined$Type
          )
          
          p_drift <- ggplot() +
            geom_line(data = pred_line,
                      aes(x = Row, y = CorrFactor),
                      colour = "steelblue", linewidth = 1) +
            geom_point(data = qc_cf,
                       aes(x = Row, y = CorrFactor),
                       colour = "red", size = 4, shape = 17) +
            geom_point(data = sample_points %>% filter(Type == "Sample"),
                       aes(x = Row, y = CorrFactor),
                       colour = "grey50", size = 2, alpha = 0.6) +
            geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
            theme_bw() +
            labs(
              title = paste0("PETN Drift Correction (", drift_method,
                             ", R\u00B2 = ", round(r2_val, 4), ")"),
              subtitle = "QC correction factors (red), fitted model (blue), samples (grey)",
              x = "Injection Number",
              y = "Correction Factor"
            )
        }
        
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
      message("  PETN drift correction: fewer than ", min_qc_required,
              " usable QC points (need \u2265", min_qc_required,
              " for power law fit) -- skipping.")
      Combined$petn_drift_model <- "None"
    }
    
  } else {
    message("  PETN drift correction: no usable QC rows -- skipping.")
    Combined$petn_drift_model <- "None"
  }

} else if (!apply_petn_drift_correction) {
  Combined$petn_drift_model <- "Disabled"
  message("  PETN drift correction: disabled (apply_petn_drift_correction = FALSE).")
}

# =========================================================
# PETN Drift-Corrected Calibration Plots
# =========================================================

if (apply_petn_drift_correction && 
    !is.na(Combined$petn_drift_model[1]) &&
    Combined$petn_drift_model[1] %in% c("PowerLaw", "Quadratic", "Linear")) {
  
  message("  Generating PETN drift-corrected calibration plots...")
  
  drift_col <- ifelse(use_15nrdx_for_petn, "petn_ratio_dc", "petn_pa_dc")
  y_label_dc <- ifelse(use_15nrdx_for_petn, "Corrected Ratio", "Corrected Peak Area")
  petn_resp_col <- ifelse(use_15nrdx_for_petn, "petn_ratio", "petn_pa")
  
  for (model_idx in seq_along(stored_petn_models)) {
    
    m <- stored_petn_models[[model_idx]]
    
    cal_data <- Combined %>%
      filter(Type == "Cal", CalibrationSet == model_idx, !is.na(.data[[petn_resp_col]])) %>%
      mutate(CalLevelAdj = CalLevel * Dilution)
    
    if (nrow(cal_data) > 0) {
      curve_x <- seq(min(cal_data$CalLevelAdj, na.rm = TRUE),
                     max(cal_data$CalLevelAdj, na.rm = TRUE),
                     length.out = 200)
      curve_df <- data.frame(
        Xplot = curve_x / Dilution,
        Fitted = predict(m$quad_model, newdata = data.frame(CalLevelAdj = curve_x))
      )
      
      pd_dc <- Combined %>%
        filter(CalibrationSet == model_idx) %>%
        mutate(
          Xplot = case_when(
            Type %in% c("Cal", "QC") ~ CalLevel,
            TRUE                     ~ petn_concentration_dc
          ),
          y_val = .data[[drift_col]],
          Group = case_when(
            Type == "Cal" ~ "Standard",
            Type == "QC"  ~ "QC",
            TRUE          ~ "Sample"
          ),
          SNR_Flag = if ("petn_snr_flag" %in% names(.)) petn_snr_flag else NA_character_
        ) %>%
        filter(!is.na(y_val), is.finite(Xplot))
      
      if (nrow(pd_dc) > 0 && nrow(curve_df) > 0) {
        p_dc <- ggplot() +
          geom_line(data = curve_df, aes(Xplot, Fitted), color = "black") +
          geom_point(data = pd_dc, aes(Xplot, y_val, color = Group, shape = SNR_Flag), size = 3) +
          scale_shape_manual(
            values = c("Quantifiable" = 16, "Below_LOQ" = 17, "Below_LOD" = 4),
            na.value = 1
          ) +
          theme_bw() +
          labs(
            title = paste("PETN - Calibration", model_idx, "(Drift-Corrected)"),
            x = "PETN concentration (ng)",
            y = y_label_dc,
            shape = "SNR Flag"
          )
        
        ggsave(
          paste0("PETN_Calibration_", model_idx, "_DriftCorrected.png"),
          p_dc,
          path = Results.dir,
          width = 8,
          height = 5,
          dpi = 300
        )
      }
    }
  }
}

# =========================================================
# RDX Drift Correction - Not Applied
# =========================================================
# RDX uses IS ratio correction only (when use_is_for_rdx = TRUE)
# No QC-based drift correction is applied to RDX PA
# No RDX drift-corrected columns (_dc suffix) are created

if (use_is_for_rdx) {
  message("  RDX drift correction: not needed (IS ratio correction sufficient).")
} else {
  message("  RDX drift correction: not applied (PA-only mode uses uncorrected concentrations).")
}

# =========================================================
# Final QC calculations
# =========================================================
if (has_calibration) {

Combined <- Combined %>%
  mutate(TrueCalConcAdj = ifelse(Type %in% c("Cal", "QC"), CalLevel, NA_real_))

for (name in names(analytes)) {
  prefix <- tolower(name)

  conc_col <- paste0(prefix, "_concentration")

  if (!(conc_col %in% names(Combined)) || all(is.na(Combined[[conc_col]]))) next

  Combined[[paste0(prefix, "_percent_bias")]] <- ifelse(
    Combined$Type %in% c("Cal", "QC") & !is.na(Combined[[conc_col]]) & !is.na(Combined$TrueCalConcAdj),
    (Combined[[conc_col]] - Combined$TrueCalConcAdj) / Combined$TrueCalConcAdj * 100,
    NA_real_
  )
}

# =========================================================
# 10. QC Evaluation (6ng QCs: bias-based; 0.2ng QCs: SNR-based)
# =========================================================

Combined$petn_percent_bias_dc <- ifelse(
  Combined$Type %in% c("Cal", "QC") &
    !is.na(Combined$petn_concentration_dc) &
    !is.na(Combined$TrueCalConcAdj),
  (Combined$petn_concentration_dc - Combined$TrueCalConcAdj) / Combined$TrueCalConcAdj * 100,
  NA_real_
)

# PETN drift-corrected QC flags (apply same logic as uncorrected)
Combined$petn_qc_flag_dc <- NA_character_
for (i in which(Combined$Type == "QC")) {
  cal_level <- Combined$CalLevel[i]
  if (is.na(cal_level)) next
  
  if (cal_level == 0.2) {
    # 0.2ng: Three-tier evaluation (concentration first, then SNR thresholds)
    # Priority: conc <= 0 → FAIL (not quantifiable)
    #           SNR < 3 → FAIL (below LOD)
    #           SNR < 10 → WARN (detected but marginal)
    #           SNR >= 10 AND conc > 0 → PASS (good sensitivity)
    snr_val <- if ("petn_snr" %in% names(Combined)) Combined$petn_snr[i] else NA_real_
    conc_dc_val <- Combined$petn_concentration_dc[i]
    
    if (is.na(snr_val) || is.na(conc_dc_val)) {
      # No peak detected at all (non-detection) or concentration could not be
      # computed -- per spec this is a sensitivity failure, not an unevaluated
      # QC. Previously this silently produced NA, masking non-detection.
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
    } else if (conc_dc_val <= 0) {
      # Concentration check FIRST: not quantifiable regardless of SNR
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
    } else if (snr_val < 3) {
      # Below detection limit
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
    } else if (snr_val < 10) {
      # Marginal: detected and quantifiable but SNR < 10
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_WARN"
    } else {
      # SNR >= 10 AND conc > 0
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_PASS"
    }
  } else {
    # Higher QCs: Bias + SNR
    bias_val_dc <- Combined$petn_percent_bias_dc[i]
    snr_val <- if ("petn_snr" %in% names(Combined)) Combined$petn_snr[i] else NA_real_
    if (is.na(bias_val_dc)) {
      # No concentration available (typically non-detection, SNR/PA both NA) --
      # general failure per spec, not an unevaluated QC.
      Combined$petn_qc_flag_dc[i] <- "FAIL"
    } else if (abs(bias_val_dc) <= qc_bias_limit && (!is.na(snr_val) && snr_val >= 10)) {
      Combined$petn_qc_flag_dc[i] <- "PASS"
    } else if (abs(bias_val_dc) > qc_bias_limit) {
      Combined$petn_qc_flag_dc[i] <- "FAIL_BIAS"
    } else if (!is.na(snr_val) && snr_val < 10) {
      Combined$petn_qc_flag_dc[i] <- "FAIL_SNR"
    } else {
      Combined$petn_qc_flag_dc[i] <- "FAIL"
    }
  }
}

for (name in names(analytes)) {
  prefix <- tolower(name)
  bias_col <- paste0(prefix, "_percent_bias")
  flag_col <- paste0(prefix, "_qc_flag")
  qc_type_col <- paste0(prefix, "_qc_type")
  snr_col <- paste0(prefix, "_snr")

  # Initialize QC type and flag columns
  Combined[[qc_type_col]] <- NA_character_
  Combined[[flag_col]] <- NA_character_

  # Evaluate QCs
  for (i in which(Combined$Type == "QC")) {
    cal_level <- Combined$CalLevel[i]
    
    if (is.na(cal_level)) next
    
    if (cal_level == 0.2) {
      # 0.2ng QCs: SNR-based evaluation only (sensitivity check)
      # PASS if SNR >= 3 (detectable), FAIL only if SNR < 3 (below LOD)
      Combined[[qc_type_col]][i] <- "SENSITIVITY"
      
      snr_val <- if (snr_col %in% names(Combined)) Combined[[snr_col]][i] else NA_real_
      
      if (is.na(snr_val)) {
        # No peak detected at all (non-detection) -- below LOD, worse than
        # SNR < 3. Previously silently produced NA, masking non-detection.
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_FAIL"
      } else if (snr_val >= 3) {
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_PASS"
      } else {
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_FAIL"
      }
      
    } else {
      # Higher QCs (6ng): Bias + SNR evaluation (quantitative QCs)
      Combined[[qc_type_col]][i] <- "QUANTITATIVE"
      
      bias_val <- if (bias_col %in% names(Combined)) Combined[[bias_col]][i] else NA_real_
      snr_val <- if (snr_col %in% names(Combined)) Combined[[snr_col]][i] else NA_real_
      
      if (is.na(bias_val)) {
        # No concentration available (typically non-detection) -- general
        # failure per spec, not an unevaluated QC.
        Combined[[flag_col]][i] <- "FAIL"
      } else if (abs(bias_val) <= qc_bias_limit && (!is.na(snr_val) && snr_val >= 10)) {
        Combined[[flag_col]][i] <- "PASS"
      } else if (abs(bias_val) > qc_bias_limit) {
        Combined[[flag_col]][i] <- "FAIL_BIAS"
      } else if (!is.na(snr_val) && snr_val < 10) {
        Combined[[flag_col]][i] <- "FAIL_SNR"
      } else {
        Combined[[flag_col]][i] <- "FAIL"
      }
    }
  }
}

}  # end if (has_calibration)

Combined <- Combined %>% arrange(Line)

# =========================================================
# QC Plots: Separate 6ng Bias and 0.2ng SNR Plots
# =========================================================
qc_plot_data <- Combined %>% filter(Type == "QC")

if (nrow(qc_plot_data) > 0 && has_calibration) {

  for (analyte_name in names(analytes)) {
    prefix <- tolower(analyte_name)

    bias_col <- paste0(prefix, "_percent_bias")
    flag_col <- paste0(prefix, "_qc_flag")
    snr_col  <- paste0(prefix, "_snr")

    if (analyte_name == "PETN") {
      method_label <- if (use_15nrdx_for_petn) "PA / 15N-RDX IS Ratio" else "Peak Area"
    } else if (analyte_name == "RDX") {
      method_label <- if (use_is_for_rdx) "PA / 15N-RDX IS Ratio" else "Peak Area"
    } else {
      next
    }

    # ===== 6ng QC Accuracy Plot (Bias) =====
    qc_6ng <- qc_plot_data %>%
      filter(CalLevel == 6, !is.na(.data[[bias_col]])) %>%
      select(Row, PercentBias = !!sym(bias_col), QCFlag = !!sym(flag_col))

    if (nrow(qc_6ng) > 0) {
      p_6ng_bias <- ggplot(qc_6ng, aes(x = Row, y = PercentBias, shape = QCFlag)) +
        geom_point(size = 4, colour = "darkblue") +
        geom_line(colour = "darkblue") +
        geom_hline(yintercept = 0, colour = "grey50") +
        geom_hline(yintercept = c(-qc_bias_limit, qc_bias_limit),
                   linetype = "dashed", colour = "red", linewidth = 1) +
        theme_bw() +
        labs(
          title = paste(analyte_name, "- 6ng QC Accuracy"),
          subtitle = paste("Red lines = ±", qc_bias_limit, "% acceptance limits | Method:", method_label),
          x = "Injection Number",
          y = "% Bias vs True (6ng)",
          shape = "QC Flag"
        ) +
        scale_shape_manual(
          values = c("PASS" = 16, "FAIL_BIAS" = 4, "FAIL_SNR" = 17, "FAIL" = 4),
          na.value = 1
        )

      ggsave(
        paste0(analyte_name, "_QC_6ng_Accuracy.png"),
        p_6ng_bias,
        path = Results.dir,
        width = 10, height = 5, dpi = 300
      )
    }

    # ===== 0.2ng QC SNR Trend Plot =====
    qc_0p2ng <- qc_plot_data %>%
      filter(CalLevel == 0.2, !is.na(.data[[snr_col]])) %>%
      select(Row, SNR = !!sym(snr_col), QCFlag = !!sym(flag_col))

    if (nrow(qc_0p2ng) > 0) {
      p_0p2ng_snr <- ggplot(qc_0p2ng, aes(x = Row, y = SNR, shape = QCFlag)) +
        geom_point(size = 4, colour = "blue") +
        geom_line(colour = "blue") +
        geom_hline(yintercept = 10, linetype = "dashed", colour = "green", 
                   linewidth = 1) +
        geom_hline(yintercept = 3, linetype = "dashed", colour = "orange",
                   linewidth = 1) +
        annotate("text", x = max(qc_0p2ng$Row) * 0.9, y = 10.5, 
                 label = "SNR ≥ 10 (Pass)", color = "green", hjust = 1) +
        annotate("text", x = max(qc_0p2ng$Row) * 0.9, y = 3.5,
                 label = "SNR ≥ 3 (Warn)", color = "orange", hjust = 1) +
        theme_bw() +
        labs(
          title = paste(analyte_name, "- 0.2ng QC Sensitivity Monitor"),
          subtitle = "Green line = Pass threshold | Orange line = LOD",
          x = "Injection Number",
          y = "Signal-to-Noise Ratio (SNR)",
          shape = "QC Flag"
        ) +
        scale_shape_manual(
          values = c("SENSITIVITY_CHECK_PASS" = 16, "SENSITIVITY_CHECK_WARN" = 17, 
                     "SENSITIVITY_CHECK_FAIL" = 4),
          na.value = 1
        )

      ggsave(
        paste0(analyte_name, "_QC_0p2ng_SNR_Trend.png"),
        p_0p2ng_snr,
        path = Results.dir,
        width = 10, height = 5, dpi = 300
      )
    }

    # ===== 6ng QC Accuracy Plot - Drift Corrected (PETN only) =====
    if (analyte_name == "PETN" &&
        "petn_percent_bias_dc" %in% names(qc_plot_data) &&
        !all(is.na(qc_plot_data$petn_percent_bias_dc))) {

      qc_6ng_dc <- qc_plot_data %>%
        filter(CalLevel == 6, !is.na(petn_percent_bias_dc)) %>%
        select(Row, PercentBias = petn_percent_bias_dc, QCFlag = petn_qc_flag_dc)

      if (nrow(qc_6ng_dc) > 0) {
        p_6ng_bias_dc <- ggplot(qc_6ng_dc, aes(x = Row, y = PercentBias, shape = QCFlag)) +
          geom_point(size = 4, colour = "darkgreen") +
          geom_line(colour = "darkgreen") +
          geom_hline(yintercept = 0, colour = "grey50") +
          geom_hline(yintercept = c(-qc_bias_limit, qc_bias_limit),
                     linetype = "dashed", colour = "red", linewidth = 1) +
          theme_bw() +
          labs(
            title = "PETN - 6ng QC Accuracy (Drift-Corrected)",
            subtitle = paste("Red lines = ±", qc_bias_limit, "% acceptance limits"),
            x = "Injection Number",
            y = "% Bias vs True (6ng)",
            shape = "QC Flag"
          ) +
          scale_shape_manual(
            values = c("PASS" = 16, "FAIL_BIAS" = 4, "FAIL_SNR" = 17, "FAIL" = 4),
            na.value = 1
          )

        ggsave(
          "PETN_QC_6ng_Accuracy_DriftCorrected.png",
          p_6ng_bias_dc,
          path = Results.dir,
          width = 10, height = 5, dpi = 300
        )
      }
    }

  }
}

# =========================================================
# QC Raw Peak Area Trend Plots (Pre-Drift Correction)
# =========================================================
# Purpose: Visualize signal drift in QCs BEFORE correction is applied
# Shows raw PA trends for each QC level separately for optimal y-axis scaling
# Helps identify drift patterns (linear, quadratic, step changes, outliers)

if (nrow(qc_plot_data) > 0 && has_calibration) {
  
  for (analyte_name in names(analytes)) {
    prefix <- tolower(analyte_name)
    pa_col <- paste0(prefix, "_pa")
    
    if (!(pa_col %in% names(qc_plot_data))) next
    
    # Get unique QC levels for this analyte (with valid PA data)
    qc_levels <- qc_plot_data %>%
      filter(!is.na(.data[[pa_col]]), .data[[pa_col]] > 0) %>%
      pull(CalLevel) %>%
      unique() %>%
      sort()
    
    if (length(qc_levels) == 0) next
    
    # Generate separate plot for each QC level
    for (qc_level in qc_levels) {
      
      # Filter to this analyte and QC level
      plot_data <- qc_plot_data %>%
        filter(CalLevel == qc_level, !is.na(.data[[pa_col]]), .data[[pa_col]] > 0)
      
      if (nrow(plot_data) < 2) next  # Need at least 2 points for a trend plot
      
      # Calculate mean PA for reference line
      mean_pa <- mean(plot_data[[pa_col]], na.rm = TRUE)
      
      # Create plot
      p_raw_trend <- ggplot(plot_data, aes(x = Row, y = .data[[pa_col]])) +
        geom_point(size = 3, colour = "blue") +
        geom_line(colour = "blue", alpha = 0.6) +
        geom_hline(yintercept = mean_pa, 
                   linetype = "dashed", 
                   colour = "darkblue", 
                   linewidth = 0.8) +
        theme_bw() +
        labs(
          title = paste0(analyte_name, " - ", qc_level, "ng QC Raw Peak Area Trend"),
          subtitle = paste0("Mean PA: ", format(round(mean_pa, 0), big.mark = ","),
                           " | n = ", nrow(plot_data)),
          x = "Injection Number (Row)",
          y = "Raw Peak Area (counts)"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10, colour = "grey30")
        )
      
      # Format QC level for filename (handle decimal levels like 0.2)
      qc_level_str <- gsub("\\.", "p", as.character(qc_level))
      
      # Save plot
      ggsave(
        paste0(analyte_name, "_QC_", qc_level_str, "ng_RawPA_Trends.png"),
        p_raw_trend,
        path = Results.dir,
        width = 8, height = 6, dpi = 300
      )
    }
  }
  
  message("QC raw PA trend plots generated.")
}

# =========================================================
# Export final results
# =========================================================
outfile <- file.path(
  Results.dir,
  paste0(ParentFolder, "_GCMSResults.csv")
)

# Backup existing results before overwriting
if (file.exists(outfile)) {
  backup_name <- paste0(
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    "_", ParentFolder, "_GCMSResults.csv"
  )
  file.copy(outfile, file.path(Backup.dir, backup_name))
  message("  Previous results backed up to: ", file.path(Backup.dir, backup_name))
}

# Remove RDX drift correction columns from output (not applicable - RDX uses IS ratio only)
Combined_output <- Combined %>%
  select(-any_of(c("rdx_drift_correction_factor", "rdx_concentration_dc", 
                   "rdx_percent_bias_dc", "rdx_qc_flag_dc")))

write.csv(Combined_output, outfile, row.names = FALSE)

# =========================================================
# Export XLSX workbook with separate sheets
# =========================================================
xlsx_outfile <- file.path(
  Results.dir,
  paste0(ParentFolder, "_GCMSResults.xlsx")
)

# Confirmatory (qualifier) ion columns (added Aug 2026 -- see CONTEXT.md
# "Confirmatory Ion Detection" session). Not used for quantification;
# included here purely for audit visibility alongside each analyte's own
# raw measurements. `any_of()` in the sheet-building select() calls below
# means these are silently omitted for any dataset processed before this
# feature existed (no error).
petn_qual_cols <- c("petn_qual76_rt", "petn_qual76_pa", "petn_qual76_ph", "petn_qual76_snr", "petn_qual76_snr_flag")
rdx_qual_cols  <- c("rdx_qual46_rt", "rdx_qual46_pa", "rdx_qual46_ph", "rdx_qual46_snr", "rdx_qual46_snr_flag",
                    "rdx_qual75_rt", "rdx_qual75_pa", "rdx_qual75_ph", "rdx_qual75_snr", "rdx_qual75_snr_flag")

# Tailored column sets per sheet type

# "Other significant peak" TIC scan columns (added Aug 2026, extended to
# ALL sheet types Aug 2026 Stage 5 -- see CONTEXT.md "TIC Deinterleaving
# Fix" session). find_other_tic_peaks() (Code/ModPeaks.R) runs identically
# for every injection type (Cal/QC/Sample/Blank) in Code/02_PeakDetection.R
# -- Stage 4's study-wide re-analysis (Code/04_CollateStudyResults.R
# Section 13a) showed "other" peaks are actually now MORE prevalent in QC
# (94%) and Sample (90.5%) rows than in Blanks (44.8%), so restricting
# per-lab XLSX visibility to only the Blanks sheet (the original,
# Blanks-only conception of this feature) was a real gap once the
# consumption broadened to all types -- fixed by adding these columns to
# every sheet's column list below, not just blk_cols. Instrument-
# performance-only, non-gating -- see Code/InjectionAcceptance.R
# evaluate_blanks() for how Blank rows specifically feed into the (still
# unaffected) blank_status/petn_blank_bracket/rdx_blank_bracket chain.
# Reports up to tic_other_peak_max_report individual peaks (largest first)
# so a recurring-but-secondary peak isn't masked by a larger one-off peak
# in the same injection -- see the study-level Recurring Peak clustering
# in Code/04_CollateStudyResults.R.
tic_other_peak_cols <- c(
  "tic_other_peak_count",
  paste0("tic_other_peak_rt", seq_len(tic_other_peak_max_report)),
  paste0("tic_other_peak_height", seq_len(tic_other_peak_max_report))
)

cal_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
              # Internal Standard (15N-RDX)
              "rdx_is_rt", "rdx_is_pa", "rdx_is_pa_flag", "rdx_is_snr", "rdx_is_snr_flag",
              # === ALL PETN COLUMNS ===
              # PETN raw measurements
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
              # PETN confirmatory ions
              petn_qual_cols,
              # PETN quantitation
              "petn_concentration", "petn_quant_method", "petn_extrapolated",
              # PETN accuracy
              "petn_percent_bias",
              # === ALL RDX COLUMNS ===
              # RDX raw measurements
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
              # RDX confirmatory ions
              rdx_qual_cols,
              # RDX quantitation
              "rdx_concentration", "rdx_quant_method", "rdx_extrapolated",
              # RDX accuracy
              "rdx_percent_bias",
              # True concentration (for both analytes)
              "TrueCalConcAdj",
              # "Other significant peak" TIC scan (see comment above)
              tic_other_peak_cols)

qc_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
             # Internal Standard (15N-RDX)
             "rdx_is_rt", "rdx_is_pa", "rdx_is_pa_flag",
             # === ALL PETN COLUMNS ===
             # PETN raw measurements
             "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
             # PETN confirmatory ions
             petn_qual_cols,
             # PETN quantitation (uncorrected)
             "petn_concentration", "petn_extrapolated",
             # PETN drift correction
             "petn_drift_correction_factor", "petn_dc_cf_se",
             "petn_concentration_dc", "petn_concentration_dc_se", "petn_extrapolated_dc",
             # PETN QC evaluation (drift-corrected only)
             "petn_percent_bias_dc", "petn_qc_flag_dc",
             # === ALL RDX COLUMNS ===
             # RDX raw measurements
             "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
             # RDX confirmatory ions
             rdx_qual_cols,
             # RDX quantitation (uncorrected)
             "rdx_concentration", "rdx_extrapolated",
             # RDX accuracy (uncorrected)
             "rdx_percent_bias",
             # RDX QC evaluation
             "rdx_qc_type", "rdx_qc_flag",
             # True concentration (for both analytes)
             "TrueCalConcAdj",
             # "Other significant peak" TIC scan (see comment above)
             tic_other_peak_cols)

smp_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial",
              # Internal Standard (15N-RDX)
              "rdx_is_rt", "rdx_is_pa", "rdx_is_pa_flag", "rdx_is_snr", "rdx_is_snr_flag",
              # === ALL PETN COLUMNS ===
              # PETN raw measurements
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
              # PETN confirmatory ions
              petn_qual_cols,
              # PETN quantitation (uncorrected)
              "petn_concentration", "petn_quant_method", "petn_extrapolated",
              # PETN drift correction
              "petn_drift_correction_factor", "petn_dc_cf_se",
              "petn_concentration_dc", "petn_concentration_dc_se", "petn_extrapolated_dc",
              # === ALL RDX COLUMNS ===
              # RDX raw measurements
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
              # RDX confirmatory ions
              rdx_qual_cols,
              # RDX quantitation (uncorrected only - no drift correction for RDX)
              "rdx_concentration", "rdx_quant_method", "rdx_extrapolated",
              # "Other significant peak" TIC scan (see comment above)
              tic_other_peak_cols)

blk_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial",
              # === ALL PETN COLUMNS ===
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag",
              # PETN confirmatory ions
              petn_qual_cols,
              # === ALL RDX COLUMNS ===
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag",
              # RDX confirmatory ions
              rdx_qual_cols,
              # "Other significant peak" TIC scan (added Aug 2026 -- see
              # CONTEXT.md "Blank Other-Significant-Peak Detection" and
              # find_other_tic_peaks() in Code/ModPeaks.R). Instrument-
              # performance-only, non-gating -- see Code/InjectionAcceptance.R
              # evaluate_blanks() for how this is consumed downstream.
              # Reports up to tic_other_peak_max_report individual peaks
              # (largest first) so recurring-but-secondary peaks aren't
              # masked by a larger one-off peak in the same injection --
              # see the study-level Recurring Peak clustering in
              # Code/04_CollateStudyResults.R.
              tic_other_peak_cols)

# Build sheet list (using any_of to handle missing columns gracefully)
sheet_list <- list(
  "Calibration" = Combined %>% filter(Type == "Cal")    %>% select(any_of(cal_cols)),
  "QC"          = Combined %>% filter(Type == "QC")     %>% arrange(CalLevel, Line) %>% select(any_of(qc_cols)),
  "Samples"     = Combined %>% filter(Type == "Sample") %>% select(any_of(smp_cols)),
  "Blanks"      = Combined %>% filter(Type == "Blank")  %>% select(any_of(blk_cols))
)

# Remove empty sheets (e.g. if no blanks in sequence)
sheet_list <- sheet_list[sapply(sheet_list, nrow) > 0]

# Fill NA values with contextual labels for readability
# ND = Not Detected (peak absent), BQL = Below Quantification Limit (detected but not quantifiable)
nd_columns <- c("petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_ratio",
                "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_ratio",
                "rdx_is_rt", "rdx_is_pa", "rdx_is_snr",
                # Confirmatory (qualifier) ion columns (added Aug 2026; PETN
                # m/z57 removed Aug 2026 -- see CONTEXT.md)
                "petn_qual76_rt", "petn_qual76_pa", "petn_qual76_ph", "petn_qual76_snr",
                "rdx_qual46_rt", "rdx_qual46_pa", "rdx_qual46_ph", "rdx_qual46_snr",
                "rdx_qual75_rt", "rdx_qual75_pa", "rdx_qual75_ph", "rdx_qual75_snr")
bql_columns <- c("petn_concentration", "rdx_concentration",
                 "petn_concentration_dc")

fill_na_labels <- function(df) {
  for (col in names(df)) {
    if (col %in% nd_columns && any(is.na(df[[col]]))) {
      df[[col]] <- ifelse(is.na(df[[col]]), "ND", as.character(df[[col]]))
    } else if (col %in% bql_columns && any(is.na(df[[col]]))) {
      # Determine if ND or BQL based on corresponding SNR flag
      prefix <- sub("_concentration.*", "", col)
      snr_flag_col <- paste0(prefix, "_snr_flag")
      if (snr_flag_col %in% names(df)) {
        df[[col]] <- ifelse(
          is.na(df[[col]]),
          ifelse(df[[snr_flag_col]] == "Not_Detected", "ND", "BQL"),
          as.character(df[[col]])
        )
      } else {
        df[[col]] <- ifelse(is.na(df[[col]]), "BQL", as.character(df[[col]]))
      }
    }
  }
  df
}

sheet_list <- lapply(sheet_list, fill_na_labels)

write_xlsx(sheet_list, xlsx_outfile)
message("  XLSX results exported to: ", xlsx_outfile)

# =========================================================
# QC Results Export to Monitoring File
# =========================================================
if ("QC" %in% Combined$Type && has_calibration) {

  # QC export columns - streamlined for monitoring (DataPath added at end)
  # Note: RDX drift correction columns excluded (no DC applied to RDX)
  # Note: IS SNR, PETN concentration, and qc_type removed per user request
  qc_export_cols <- c(
    "Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
    # IS (15N-RDX) - PA only
    "rdx_is_rt", "rdx_is_pa", "rdx_is_pa_flag",
    # PETN - drift-corrected columns only
    "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
    "petn_drift_correction_factor", "petn_dc_cf_se",
    "petn_concentration_dc", "petn_concentration_dc_se",
    "petn_percent_bias_dc", "petn_qc_flag_dc",
    # RDX (no drift correction columns)
    "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
    "rdx_concentration", "rdx_percent_bias", "rdx_qc_type", "rdx_qc_flag"
  )

  # Only keep columns that actually exist in Combined dataframe
  qc_export_cols <- intersect(qc_export_cols, names(Combined))

  QCResults <- Combined %>%
    filter(Type == "QC") %>%
    select(all_of(qc_export_cols))
  
  # Add DataPath (full folder path) as FINAL column
  QCResults <- QCResults %>%
    mutate(DataPath = DataFolder)
  
  # Split into separate dataframes for 0.2ng and 6ng QCs
  QCResults_02ng <- QCResults %>% filter(CalLevel == 0.2)
  QCResults_6ng <- QCResults %>% filter(CalLevel == 6)
  
  # For 0.2ng QCs: simplify drift-corrected flags to PASS/FAIL (sensitivity check only)
  if (nrow(QCResults_02ng) > 0) {
    QCResults_02ng <- QCResults_02ng %>%
      mutate(
        petn_qc_flag_dc = case_when(
          grepl("PASS", petn_qc_flag_dc, ignore.case = TRUE) ~ "PASS",
          grepl("FAIL", petn_qc_flag_dc, ignore.case = TRUE) ~ "FAIL",
          grepl("WARN", petn_qc_flag_dc, ignore.case = TRUE) ~ "FAIL",
          TRUE ~ petn_qc_flag_dc
        ),
        rdx_qc_flag = case_when(
          grepl("PASS", rdx_qc_flag, ignore.case = TRUE) ~ "PASS",
          grepl("FAIL", rdx_qc_flag, ignore.case = TRUE) ~ "FAIL",
          grepl("WARN", rdx_qc_flag, ignore.case = TRUE) ~ "FAIL",
          TRUE ~ rdx_qc_flag
        )
      )
  }

  if (nrow(QCResults) > 0) {
    # Fresh start: save directly as XLSX with separate sheets for 0.2ng and 6ng
    if (file.exists(qc_monitoring_file)) {
      # Read existing XLSX sheets
      sheet_names <- readxl::excel_sheets(qc_monitoring_file)
      
      # Read existing sheets if they exist
      if ("QC_0.2ng" %in% sheet_names) {
        Existing_02ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_0.2ng")
        Existing_02ng$Date <- as.character(Existing_02ng$Date)
      } else {
        Existing_02ng <- NULL
      }
      
      if ("QC_6ng" %in% sheet_names) {
        Existing_6ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_6ng")
        Existing_6ng$Date <- as.character(Existing_6ng$Date)
      } else {
        Existing_6ng <- NULL
      }
      
      # Convert new QC dates to character
      if (nrow(QCResults_02ng) > 0) QCResults_02ng$Date <- as.character(QCResults_02ng$Date)
      if (nrow(QCResults_6ng) > 0) QCResults_6ng$Date <- as.character(QCResults_6ng$Date)
      
      # Combine and remove duplicates based on (DataPath, DataFile)
      if (!is.null(Existing_02ng) && nrow(QCResults_02ng) > 0) {
        Combined_02ng <- bind_rows(Existing_02ng, QCResults_02ng) %>%
          distinct(DataPath, DataFile, .keep_all = TRUE)
      } else if (nrow(QCResults_02ng) > 0) {
        Combined_02ng <- QCResults_02ng
      } else {
        Combined_02ng <- Existing_02ng
      }
      
      if (!is.null(Existing_6ng) && nrow(QCResults_6ng) > 0) {
        Combined_6ng <- bind_rows(Existing_6ng, QCResults_6ng) %>%
          distinct(DataPath, DataFile, .keep_all = TRUE)
      } else if (nrow(QCResults_6ng) > 0) {
        Combined_6ng <- QCResults_6ng
      } else {
        Combined_6ng <- Existing_6ng
      }
      
      # Backup existing file
      qc_backup_dir <- file.path(dirname(qc_monitoring_file), "Backup")
      dir.create(qc_backup_dir, recursive = TRUE, showWarnings = FALSE)
      qc_backup_name <- paste0(
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        "_SystemMonitoring_backup.xlsx"
      )
      file.copy(qc_monitoring_file, file.path(qc_backup_dir, qc_backup_name))
      message("  QC monitoring file backed up to: ", file.path(qc_backup_dir, qc_backup_name))
      
      # Write combined data with separate sheets
      sheets_to_write <- list()
      if (!is.null(Combined_02ng) && nrow(Combined_02ng) > 0) {
        sheets_to_write[["QC_0.2ng"]] <- Combined_02ng
      }
      if (!is.null(Combined_6ng) && nrow(Combined_6ng) > 0) {
        sheets_to_write[["QC_6ng"]] <- Combined_6ng
      }
      
      if (length(sheets_to_write) > 0) {
        writexl::write_xlsx(sheets_to_write, qc_monitoring_file)
      }
      
    } else {
      # Create new file with separate sheets
      dir.create(dirname(qc_monitoring_file), recursive = TRUE, showWarnings = FALSE)
      
      sheets_to_write <- list()
      if (nrow(QCResults_02ng) > 0) {
        sheets_to_write[["QC_0.2ng"]] <- QCResults_02ng
      }
      if (nrow(QCResults_6ng) > 0) {
        sheets_to_write[["QC_6ng"]] <- QCResults_6ng
      }
      
      if (length(sheets_to_write) > 0) {
        writexl::write_xlsx(sheets_to_write, qc_monitoring_file)
      }
    }
    message("  QC monitoring updated: ", qc_monitoring_file)
    
    # ============================================================
    # Generate QC Monitoring Trend Plots
    # ============================================================
    # Generate PNG plots showing PETN and RDX peak area trends over time
    # for 0.2ng and 6ng QC levels. Uses sequential dataset number as x-axis
    # and shows mean PA per dataset.
    
    if (file.exists(qc_monitoring_file)) {
      message("  Generating QC trend plots...")
      
      tryCatch({
        # Read both sheets
        sheet_names <- readxl::excel_sheets(qc_monitoring_file)
        
        # Process 0.2ng QCs if sheet exists
        if ("QC_0.2ng" %in% sheet_names) {
          qc_02ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_0.2ng")
          
          if (nrow(qc_02ng) > 0) {
            # Prepare data: use individual QC injections (not aggregated)
            qc_02ng_plot <- qc_02ng %>%
              mutate(
                Dataset = basename(DataPath),
                Date = as.Date(as.character(Date), format = "%Y%m%d")
              ) %>%
              arrange(Date, Line) %>%
              mutate(SequenceNumber = row_number())
            
            # Identify first QC injection of each new dataset (sequence)
            qc_02ng_plot <- qc_02ng_plot %>%
              group_by(Dataset) %>%
              mutate(IsFirstOfSequence = row_number() == 1) %>%
              ungroup()
            
            # Skip plot if only one QC injection (not enough for trend)
            if (nrow(qc_02ng_plot) < 2) {
              message("    -> Skipping QC_0.2ng_Trends.png (need ≥2 QC injections for trend)")
            } else {
            
            # Reshape to long format for faceting
            qc_02ng_long <- qc_02ng_plot %>%
              pivot_longer(
                cols = c(petn_pa, rdx_pa),
                names_to = "Analyte",
                values_to = "PA"
              ) %>%
              mutate(Analyte = ifelse(grepl("petn", Analyte), "PETN", "RDX")) %>%
              filter(!is.na(PA))  # Remove NA values for proper line connections
            
            # Calculate overall mean per analyte for reference lines
            mean_lines_02ng <- qc_02ng_long %>%
              group_by(Analyte) %>%
              summarise(MeanPA = mean(PA, na.rm = TRUE))
            
            # Generate 0.2ng PA plot
            p_02ng <- ggplot(qc_02ng_long, aes(x = SequenceNumber, y = PA)) +
              geom_line(color = "black", size = 0.8) +
              geom_point(aes(color = IsFirstOfSequence), size = 3) +
              scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                                 labels = c("FALSE" = "Within sequence", "TRUE" = "First of new sequence"),
                                 name = NULL) +
              geom_hline(data = mean_lines_02ng, 
                         aes(yintercept = MeanPA),
                         linetype = "dashed", 
                         color = "gray40") +
              facet_wrap(~ Analyte, ncol = 1, scales = "free_y") +
              labs(
                title = "QC 0.2ng Peak Area Trends",
                subtitle = sprintf("Individual QC injections (n = %d)", 
                                   max(qc_02ng_long$SequenceNumber, na.rm = TRUE)),
                x = "Sequential QC Injection Number",
                y = "Peak Area (AU)"
              ) +
              theme_bw(base_size = 12) +
              theme(
                strip.background = element_rect(fill = "gray90"),
                strip.text = element_text(face = "bold", size = 12),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
              )
            
            plot_path_02ng <- file.path(dirname(qc_monitoring_file), "QC_0.2ng_Trends.png")
            ggsave(plot_path_02ng, plot = p_02ng, width = 10, height = 8, dpi = 300, units = "in")
            message("    -> QC_0.2ng_Trends.png")
            }
          }
        }
        
        # Process 6ng QCs if sheet exists
        if ("QC_6ng" %in% sheet_names) {
          qc_6ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_6ng")
          
          if (nrow(qc_6ng) > 0) {
            # Prepare data: use individual QC injections (not aggregated)
            qc_6ng_plot <- qc_6ng %>%
              mutate(
                Dataset = basename(DataPath),
                Date = as.Date(as.character(Date), format = "%Y%m%d")
              ) %>%
              arrange(Date, Line) %>%
              mutate(SequenceNumber = row_number())
            
            # Identify first QC injection of each new dataset (sequence)
            qc_6ng_plot <- qc_6ng_plot %>%
              group_by(Dataset) %>%
              mutate(IsFirstOfSequence = row_number() == 1) %>%
              ungroup()
            
            # Skip plot if only one QC injection (not enough for trend)
            if (nrow(qc_6ng_plot) < 2) {
              message("    -> Skipping QC_6ng_Trends.png (need ≥2 QC injections for trend)")
            } else {
            
            # Reshape to long format for faceting
            qc_6ng_long <- qc_6ng_plot %>%
              pivot_longer(
                cols = c(petn_pa, rdx_pa),
                names_to = "Analyte",
                values_to = "PA"
              ) %>%
              mutate(Analyte = ifelse(grepl("petn", Analyte), "PETN", "RDX")) %>%
              filter(!is.na(PA))  # Remove NA values for proper line connections
            
            # Calculate overall mean per analyte for reference lines
            mean_lines_6ng <- qc_6ng_long %>%
              group_by(Analyte) %>%
              summarise(MeanPA = mean(PA, na.rm = TRUE))
            
            # Generate 6ng PA plot
            p_6ng <- ggplot(qc_6ng_long, aes(x = SequenceNumber, y = PA)) +
              geom_line(color = "black", size = 0.8) +
              geom_point(aes(color = IsFirstOfSequence), size = 3) +
              scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                                 labels = c("FALSE" = "Within sequence", "TRUE" = "First of new sequence"),
                                 name = NULL) +
              geom_hline(data = mean_lines_6ng, 
                         aes(yintercept = MeanPA),
                         linetype = "dashed", 
                         color = "gray40") +
              facet_wrap(~ Analyte, ncol = 1, scales = "free_y") +
              labs(
                title = "QC 6ng Peak Area Trends",
                subtitle = sprintf("Individual QC injections (n = %d)", 
                                   max(qc_6ng_long$SequenceNumber, na.rm = TRUE)),
                x = "Sequential QC Injection Number",
                y = "Peak Area (AU)"
              ) +
              theme_bw(base_size = 12) +
              theme(
                strip.background = element_rect(fill = "gray90"),
                strip.text = element_text(face = "bold", size = 12),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
              )
            
            plot_path_6ng <- file.path(dirname(qc_monitoring_file), "QC_6ng_Trends.png")
            ggsave(plot_path_6ng, plot = p_6ng, width = 10, height = 8, dpi = 300, units = "in")
            message("    -> QC_6ng_Trends.png")
            }
          }
        }
        
        # ============================================================
        # Generate Drift-Corrected Concentration Trend Plots
        # ============================================================
        
        # Process 0.2ng drift-corrected concentration trends
        if ("QC_0.2ng" %in% sheet_names) {
          qc_02ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_0.2ng")
          
          if (nrow(qc_02ng) > 0) {
            # Prepare data: use individual QC injections
            qc_02ng_conc_plot <- qc_02ng %>%
              mutate(
                Dataset = basename(DataPath),
                Date = as.Date(as.character(Date), format = "%Y%m%d")
              ) %>%
              arrange(Date, Line) %>%
              mutate(SequenceNumber = row_number())
            
            # Identify first QC injection of each new dataset (sequence)
            qc_02ng_conc_plot <- qc_02ng_conc_plot %>%
              group_by(Dataset) %>%
              mutate(IsFirstOfSequence = row_number() == 1) %>%
              ungroup()
            
            # Skip plot if only one QC injection
            if (nrow(qc_02ng_conc_plot) < 2) {
              message("    -> Skipping QC_0.2ng_DriftCorrected_Trends.png (need ≥2 QC injections)")
            } else {
            
            # Reshape to long format for faceting
            qc_02ng_conc_long <- qc_02ng_conc_plot %>%
              pivot_longer(
                cols = c(petn_concentration_dc, rdx_concentration),
                names_to = "Analyte",
                values_to = "Concentration"
              ) %>%
              mutate(Analyte = ifelse(grepl("petn", Analyte), "PETN", "RDX")) %>%
              filter(!is.na(Concentration))  # Remove NA values
            
            # Calculate overall mean per analyte for reference lines
            mean_lines_02ng_conc <- qc_02ng_conc_long %>%
              group_by(Analyte) %>%
              summarise(MeanConc = mean(Concentration, na.rm = TRUE))
            
            # Generate 0.2ng drift-corrected concentration plot
            # Adjusted target: 0.18 ng (90% of 0.2ng nominal)
            # Acceptance criteria: ±20% of adjusted = 0.144 to 0.216 ng
            # Additional reference lines: -50% (0.09ng) and +50% (0.27ng)
            # First QC of each sequence highlighted in red
            p_02ng_conc <- ggplot(qc_02ng_conc_long, aes(x = SequenceNumber, y = Concentration)) +
              geom_ribbon(aes(ymin = 0.144, ymax = 0.216), fill = "lightgreen", alpha = 0.2) +
              geom_hline(yintercept = 0.18, linetype = "solid", color = "darkgreen", size = 0.8) +
              geom_hline(yintercept = 0.144, linetype = "dashed", color = "red", size = 0.5) +
              geom_hline(yintercept = 0.216, linetype = "dashed", color = "red", size = 0.5) +
              geom_hline(yintercept = 0.09, linetype = "dotdash", color = "darkblue", size = 0.5) +  # -50% line
              geom_hline(yintercept = 0.27, linetype = "dotdash", color = "darkblue", size = 0.5) +  # +50% line
              geom_line(color = "black", size = 0.8) +
              geom_point(aes(color = IsFirstOfSequence), size = 3) +
              scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                                 labels = c("FALSE" = "Within sequence", "TRUE" = "First of new sequence"),
                                 name = NULL) +
              geom_hline(data = mean_lines_02ng_conc, 
                         aes(yintercept = MeanConc),
                         linetype = "dotted", 
                         color = "blue") +
              facet_wrap(~ Analyte, ncol = 1, scales = "free_y") +
              labs(
                title = "QC 0.2ng Drift-Corrected Concentration Trends",
                subtitle = sprintf("Individual QC injections (n = %d), Target: 0.18 ng, Acceptance: ±20%%, Reference: ±50%%", 
                                   max(qc_02ng_conc_long$SequenceNumber, na.rm = TRUE)),
                x = "Sequential QC Injection Number",
                y = "Concentration (ng)"
              ) +
              theme_bw(base_size = 12) +
              theme(
                strip.background = element_rect(fill = "gray90"),
                strip.text = element_text(face = "bold", size = 12),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
              )
            
            plot_path_02ng_conc <- file.path(dirname(qc_monitoring_file), "QC_0.2ng_DriftCorrected_Trends.png")
            ggsave(plot_path_02ng_conc, plot = p_02ng_conc, width = 10, height = 8, dpi = 300, units = "in")
            message("    -> QC_0.2ng_DriftCorrected_Trends.png")
            }
          }
        }
        
        # Process 6ng drift-corrected concentration trends
        if ("QC_6ng" %in% sheet_names) {
          qc_6ng <- readxl::read_excel(qc_monitoring_file, sheet = "QC_6ng")
          
          if (nrow(qc_6ng) > 0) {
            # Prepare data: use individual QC injections
            qc_6ng_conc_plot <- qc_6ng %>%
              mutate(
                Dataset = basename(DataPath),
                Date = as.Date(as.character(Date), format = "%Y%m%d")
              ) %>%
              arrange(Date, Line) %>%
              mutate(SequenceNumber = row_number())
            
            # Identify first QC injection of each new dataset (sequence)
            qc_6ng_conc_plot <- qc_6ng_conc_plot %>%
              group_by(Dataset) %>%
              mutate(IsFirstOfSequence = row_number() == 1) %>%
              ungroup()
            
            # Skip plot if only one QC injection
            if (nrow(qc_6ng_conc_plot) < 2) {
              message("    -> Skipping QC_6ng_DriftCorrected_Trends.png (need ≥2 QC injections)")
            } else {
            
            # Reshape to long format for faceting
            qc_6ng_conc_long <- qc_6ng_conc_plot %>%
              pivot_longer(
                cols = c(petn_concentration_dc, rdx_concentration),
                names_to = "Analyte",
                values_to = "Concentration"
              ) %>%
              mutate(Analyte = ifelse(grepl("petn", Analyte), "PETN", "RDX")) %>%
              filter(!is.na(Concentration))  # Remove NA values
            
            # Calculate overall mean per analyte for reference lines
            mean_lines_6ng_conc <- qc_6ng_conc_long %>%
              group_by(Analyte) %>%
              summarise(MeanConc = mean(Concentration, na.rm = TRUE))
            
            # Generate 6ng drift-corrected concentration plot
            # Adjusted target: 5.4 ng (90% of 6ng nominal)
            # Acceptance criteria: ±20% of adjusted = 4.32 to 6.48 ng
            # Additional reference lines: -50% (2.7ng) and +50% (8.1ng)
            # First QC of each sequence highlighted in red
            p_6ng_conc <- ggplot(qc_6ng_conc_long, aes(x = SequenceNumber, y = Concentration)) +
              geom_ribbon(aes(ymin = 4.32, ymax = 6.48), fill = "lightgreen", alpha = 0.2) +
              geom_hline(yintercept = 5.4, linetype = "solid", color = "darkgreen", size = 0.8) +
              geom_hline(yintercept = 4.32, linetype = "dashed", color = "red", size = 0.5) +
              geom_hline(yintercept = 6.48, linetype = "dashed", color = "red", size = 0.5) +
              geom_hline(yintercept = 2.7, linetype = "dotdash", color = "darkblue", size = 0.5) +  # -50% line
              geom_hline(yintercept = 8.1, linetype = "dotdash", color = "darkblue", size = 0.5) +  # +50% line
              geom_line(color = "black", size = 0.8) +
              geom_point(aes(color = IsFirstOfSequence), size = 3) +
              scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                                 labels = c("FALSE" = "Within sequence", "TRUE" = "First of new sequence"),
                                 name = NULL) +
              geom_hline(data = mean_lines_6ng_conc, 
                         aes(yintercept = MeanConc),
                         linetype = "dotted", 
                         color = "blue") +
              facet_wrap(~ Analyte, ncol = 1, scales = "free_y") +
              labs(
                title = "QC 6ng Drift-Corrected Concentration Trends",
                subtitle = sprintf("Individual QC injections (n = %d), Target: 5.4 ng, Acceptance: ±20%%, Reference: ±50%%", 
                                   max(qc_6ng_conc_long$SequenceNumber, na.rm = TRUE)),
                x = "Sequential QC Injection Number",
                y = "Concentration (ng)"
              ) +
              theme_bw(base_size = 12) +
              theme(
                strip.background = element_rect(fill = "gray90"),
                strip.text = element_text(face = "bold", size = 12),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
              )
            
            plot_path_6ng_conc <- file.path(dirname(qc_monitoring_file), "QC_6ng_DriftCorrected_Trends.png")
            ggsave(plot_path_6ng_conc, plot = p_6ng_conc, width = 10, height = 8, dpi = 300, units = "in")
            message("    -> QC_6ng_DriftCorrected_Trends.png")
            }
          }
        }
        
        message("  QC trend plots saved to: ", dirname(qc_monitoring_file))
        
      }, error = function(e) {
        warning("  Failed to generate QC trend plots: ", e$message)
      })
    }
  }

}

message("Quantification complete. Results exported to: ", outfile)

# ======================= END OF SCRIPT ====================
