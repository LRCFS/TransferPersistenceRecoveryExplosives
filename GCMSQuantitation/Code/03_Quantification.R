
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
if (!exists("use_15nrdx_for_petn", where = .GlobalEnv, inherits = FALSE)) {
  use_15nrdx_for_petn <- TRUE
}

# -----------------------------
# Configuration toggle: use_is_for_rdx
# -----------------------------
if (!exists("use_is_for_rdx", where = .GlobalEnv, inherits = FALSE)) {
  use_is_for_rdx <- TRUE
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
  "apply_petn_drift_correction"
)

missing_globals <- required_globals[!sapply(required_globals, exists, where = .GlobalEnv, inherits = FALSE)]
if (length(missing_globals) > 0) {
  stop("Missing required globals: ",
       paste(missing_globals, collapse = ", "))
}

# =========================================================
# Concentration solver (quadratic only)
# =========================================================
solve_concentration <- function(y, quad_model, cal_y_range) {

  if (is.na(y)) {
    return(list(value = NA_real_, method = "No_peak", range_flag = NA_character_))
  }

  coefs <- coef(quad_model)
  a <- coefs["I(CalLevelAdj^2)"]
  b <- coefs["CalLevelAdj"]
  c_int <- coefs["(Intercept)"]

  disc <- b^2 - 4 * a * (c_int - y)

  if (!is.finite(disc) || disc < 0 || abs(a) < .Machine$double.eps) {
    return(list(value = NA_real_, method = "Unquantifiable", range_flag = NA_character_))
  }

  roots <- c(
    (-b + sqrt(disc)) / (2 * a),
    (-b - sqrt(disc)) / (2 * a)
  )

  roots <- roots[is.finite(roots)]

  if (length(roots) == 0) {
    return(list(value = NA_real_, method = "Unquantifiable", range_flag = NA_character_))
  }

  # Select smallest positive root (physically meaningful concentration)
  # If no positive root exists, floor at zero (below-range extrapolation)
  positive_roots <- roots[roots >= 0]
  if (length(positive_roots) > 0) {
    concentration <- min(positive_roots)
  } else {
    concentration <- 0
  }
  
  # Set range flag based on concentration (ng/µL), not calibration response
  # Note: range_flag will be assigned later based on row type (Samples only)
  
  list(value = concentration, method = "Quadratic", range_flag = NA_character_)
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
  Combined[[paste0(prefix, "_range_flag")]]    <- NA_character_
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
    range_col <- paste0(prefix, "_range_flag")

    for (j in rows) {

      row_snr_flag <- NA_character_
      if (snr_flag_col %in% names(Combined)) {
        row_snr_flag <- Combined[[snr_flag_col]][j]
      }

      if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOD") {
        Combined[[conc_col]][j]   <- NA_real_
        Combined[[range_col]][j]  <- NA_character_
        next
      }

      if (!is.na(row_snr_flag) && row_snr_flag == "Below_LOQ") {
        Combined[[conc_col]][j]   <- NA_real_
        Combined[[range_col]][j]  <- NA_character_
        next
      }

      res <- solve_concentration(
        Combined[[resp_col]][j],
        quad_model,
        cal_y_range
      )
      Combined[[conc_col]][j]   <- res$value
      Combined[[range_col]][j]  <- res$range_flag
    }

    curve_x <- seq(min(cal$CalLevelAdj), max(cal$CalLevelAdj), length.out = 200)
    curve_df <- data.frame(
      CalibrationSet = i,
      CalLevelAdj = curve_x,
      Fitted = predict(quad_model, newdata = data.frame(CalLevelAdj = curve_x)),
      Method = y_label
    )
    all_curve_store[[i]] <- curve_df

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
          aes(CalLevelAdj, Fitted),
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
    mutate(Xplot = CalLevel * Dilution)

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
          aes(CalLevelAdj, Fitted, color = factor(CalibrationSet)),
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

    qc_drift_data <- Combined %>%
      filter(Type == "QC",
             CalLevel == selected_qc_level,
             !is.na(.data[[resp_col_for_drift]])) %>%
      arrange(Row) %>%
      select(Row, Response = !!sym(resp_col_for_drift))

    n_qc <- nrow(qc_drift_data)
    
    if (n_qc >= 2) {
      message("  PETN drift correction: using ", n_qc,
              " QC points at ", selected_qc_level, " ng")
      
      drift_model  <- NULL
      drift_method <- "None"
      predicted_cf <- rep(1.0, nrow(Combined))
      r2_val <- NA_real_
      power_a <- NA_real_
      power_b <- NA_real_
      
      # ===== POWER LAW METHOD =====
      if (drift_correction_method == "power_law" && n_qc >= 3) {
        
        message("  Fitting power law: Response ~ a * Row^b")
        
        power_model <- tryCatch({
          nls(Response ~ a * Row^b,
              data = qc_drift_data,
              start = list(a = max(qc_drift_data$Response), b = -1.0),
              control = nls.control(maxiter = 100))
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
          r2_val <- 1 - (ss_res / ss_tot)
          
          if (r2_val < 0.90) {
            warning("  Power law R² = ", round(r2_val, 3), " (< 0.90) -- fit may be poor.")
          }
          
          first_qc_row <- qc_drift_data$Row[1]
          reference_response <- predict(power_model, newdata = data.frame(Row = first_qc_row))
          predicted_response <- predict(power_model, newdata = data.frame(Row = Combined$Row))
          
          predicted_cf <- reference_response / predicted_response
          predicted_cf[Combined$Row <= first_qc_row] <- 1.0
          
          drift_model <- power_model
          drift_method <- "PowerLaw"
          
          message("  Power law fit: a = ", format(power_a, scientific = FALSE),
                  ", b = ", round(power_b, 4))
          message("  R² = ", round(r2_val, 4))
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
              drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
              drift_method <- "Quadratic"
              
              r2 <- summary(drift_model)$r.squared
              if (is.na(r2) || r2 < 0.5) {
                message("  Quadratic R² = ", round(r2, 3), " (poor); falling back to linear.")
                drift_model <- lm(CorrFactor ~ Row, data = qc_drift)
                drift_method <- "Linear"
              }
              
            } else if (nrow(qc_drift) == 3) {
              drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
              drift_method <- "Quadratic"
              
            } else if (nrow(qc_drift) == 2) {
              drift_model <- lm(CorrFactor ~ Row, data = qc_drift)
              drift_method <- "Linear"
            }
            
            if (!is.null(drift_model)) {
              predicted_cf <- predict(drift_model, newdata = data.frame(Row = Combined$Row))
              first_qc_row <- qc_drift$Row[1]
              predicted_cf[Combined$Row <= first_qc_row] <- 1.0
              
              r2_val <- summary(drift_model)$r.squared
            }
          }
        }
      }
      
      # ===== APPLY CORRECTION =====
      if (!is.null(drift_model)) {
        
        if (any(predicted_cf < 0, na.rm = TRUE)) {
          warning("  Some correction factors negative -- clamping to 1.0.")
          predicted_cf[predicted_cf < 0] <- 1.0
        }
        if (any(predicted_cf > 10.0, na.rm = TRUE)) {
          warning("  Some correction factors exceed 10.0 -- severe drift.")
        }
        
        Combined$petn_drift_correction_factor <- predicted_cf
        Combined$petn_drift_model             <- drift_method
        
        corrected_resp <- Combined[[resp_col_for_drift]] * predicted_cf
        
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
            
            if (!is.na(row_snr_flag) && row_snr_flag %in% c("Below_LOD", "Below_LOQ")) {
              Combined$petn_concentration_dc[j] <- NA_real_
              next
            }
            
            res <- solve_concentration(
              corrected_resp[j],
              m$quad_model,
              m$cal_y_range
            )
            Combined$petn_concentration_dc[j] <- res$value
          }
        }
        
        message("  PETN drift correction applied:")
        message("    Model: ", drift_method)
        message("    QC points used: ", n_qc)
        message("    R²: ", round(r2_val, 4))
        if (drift_method == "PowerLaw") {
          message("    Exponent (b): ", round(power_b, 4))
        }
        message("    Correction factor range: ",
                round(min(predicted_cf, na.rm = TRUE), 4), " - ",
                round(max(predicted_cf, na.rm = TRUE), 4))
        
        # Generate diagnostic plot
        first_qc_row <- qc_drift_data$Row[1]
        
        if (drift_method == "PowerLaw") {
          # Power law plot: show PA decay and fitted curve
          pred_line <- data.frame(
            Row = seq(min(Combined$Row), max(Combined$Row), length.out = 200)
          )
          pred_line$Response <- predict(power_model, newdata = pred_line)
          
          p_drift <- ggplot() +
            geom_line(data = pred_line,
                      aes(x = Row, y = Response),
                      colour = "steelblue", linewidth = 1) +
            geom_point(data = qc_drift_data,
                       aes(x = Row, y = Response),
                       colour = "red", size = 4, shape = 17) +
            theme_bw() +
            labs(
              title = paste0("PETN Power Law Drift (R\u00B2 = ", round(r2_val, 4), ")"),
              subtitle = paste0("PA ~ ", format(power_a, digits = 4, scientific = FALSE),
                               " × Row^", round(power_b, 3)),
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
      message("  PETN drift correction: fewer than 2 usable QC points -- skipping.")
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
    Combined$petn_drift_model[1] %in% c("Quadratic", "Linear")) {
  
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
        CalLevelAdj = curve_x,
        Fitted = predict(m$quad_model, newdata = data.frame(CalLevelAdj = curve_x))
      )
      
      pd_dc <- Combined %>%
        filter(CalibrationSet == model_idx) %>%
        mutate(
          Xplot = case_when(
            Type %in% c("Cal", "QC") ~ CalLevel * Dilution,
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
          geom_line(data = curve_df, aes(CalLevelAdj, Fitted), color = "black") +
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
# RDX Drift Correction (QC-based) - PA only when use_is_for_rdx = FALSE
# =========================================================
Combined$rdx_drift_correction_factor <- 1.0
Combined$rdx_drift_model             <- NA_character_
Combined$rdx_concentration_dc         <- Combined$rdx_concentration

if (apply_rdx_drift_correction && !use_is_for_rdx && has_calibration && length(stored_rdx_models) > 0) {

  resp_col_for_drift <- "rdx_pa"

  qc_candidates <- Combined %>%
    filter(Type == "QC", !is.na(.data[[resp_col_for_drift]]), !is.na(CalLevel))

  if (nrow(qc_candidates) > 0) {
    
    if (!is.na(drift_correction_qc_level) && 
        drift_correction_qc_level %in% qc_candidates$CalLevel) {
      selected_qc_level <- drift_correction_qc_level
    } else if (!is.na(drift_correction_qc_level)) {
      selected_qc_level <- max(qc_candidates$CalLevel, na.rm = TRUE)
      warning("RDX drift correction: specified QC level ", drift_correction_qc_level,
              " ng not found; using ", selected_qc_level, " ng")
    } else {
      selected_qc_level <- max(qc_candidates$CalLevel, na.rm = TRUE)
    }

    ref_val <- Combined %>%
      filter(Type == "QC",
             CalLevel == selected_qc_level,
             !is.na(.data[[resp_col_for_drift]])) %>%
      arrange(Row) %>%
      slice(1) %>%
      pull(.data[[resp_col_for_drift]])

    if (is.finite(ref_val) && ref_val > 0) {

      qc_drift <- Combined %>%
        filter(Type == "QC",
               CalLevel == selected_qc_level,
               !is.na(.data[[resp_col_for_drift]])) %>%
        mutate(CorrFactor = ref_val / .data[[resp_col_for_drift]]) %>%
        filter(is.finite(CorrFactor), CorrFactor > 0)

      n_qc <- nrow(qc_drift)

      if (n_qc >= 2) {
        message("  RDX drift correction: using ", n_qc,
                " QC points at ", selected_qc_level, " ng (reference = first QC)")

        drift_model  <- NULL
        drift_method <- "None"

        if (n_qc >= 4) {
          drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
          drift_method <- "Quadratic"

          r2 <- summary(drift_model)$r.squared
          if (is.na(r2) || r2 < 0.5) {
            message("  RDX drift correction: quadratic R2 = ", round(r2, 3),
                    " (poor fit); falling back to linear.")
            drift_model  <- lm(CorrFactor ~ Row, data = qc_drift)
            drift_method <- "Linear"
          }

        } else if (n_qc == 3) {
          drift_model  <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
          drift_method <- "Quadratic"
          message("  RDX drift correction: only 3 QC points -- quadratic fit is exact.")

        } else if (n_qc == 2) {
          drift_model  <- lm(CorrFactor ~ Row, data = qc_drift)
          drift_method <- "Linear"
          message("  RDX drift correction: only 2 QC points -- using linear interpolation.")
        }

        if (!is.null(drift_model)) {
          predicted_cf <- predict(drift_model, newdata = data.frame(Row = Combined$Row))

          first_qc_row <- qc_drift %>%
            arrange(Row) %>%
            slice(1) %>%
            pull(Row)

          predicted_cf[Combined$Row <= first_qc_row] <- 1.0

          message("  RDX drift correction: first QC at row ", first_qc_row,
                  ". Correcting rows > ", first_qc_row, " only.")

          if (any(predicted_cf < 0, na.rm = TRUE)) {
            warning("  RDX drift correction: some correction factors negative -- clamping to 1.0.")
            predicted_cf[predicted_cf < 0] <- 1.0
          }
          if (any(predicted_cf > 10.0, na.rm = TRUE)) {
            warning("  RDX drift correction: some correction factors exceed 10.0 -- severe drift.")
          }

          Combined$rdx_drift_correction_factor <- predicted_cf
          Combined$rdx_drift_model             <- drift_method

          corrected_resp <- Combined[[resp_col_for_drift]] * predicted_cf
          
          Combined$rdx_pa_dc <- corrected_resp

          for (model_idx in seq_along(stored_rdx_models)) {
            m <- stored_rdx_models[[model_idx]]

            for (j in m$rows) {
              row_snr_flag <- NA_character_
              if (m$snr_flag_col %in% names(Combined)) {
                row_snr_flag <- Combined[[m$snr_flag_col]][j]
              }

              if (!is.na(row_snr_flag) && row_snr_flag %in% c("Below_LOD", "Below_LOQ")) {
                Combined$rdx_concentration_dc[j] <- NA_real_
                next
              }

              res <- solve_concentration(
                corrected_resp[j],
                m$quad_model,
                m$cal_y_range
              )
              Combined$rdx_concentration_dc[j] <- res$value
            }
          }

          r2_val <- round(summary(drift_model)$r.squared, 4)
          message("  RDX drift correction applied:")
          message("    Model: ", drift_method)
          message("    QC points used: ", n_qc)
          message("    R-squared: ", r2_val)
          message("    Correction factor range: ",
                  round(min(predicted_cf, na.rm = TRUE), 4), " - ",
                  round(max(predicted_cf, na.rm = TRUE), 4))

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
            geom_point(data = qc_drift,
                       aes(x = Row, y = CorrFactor),
                       colour = "red", size = 4, shape = 17) +
            geom_point(data = sample_points %>% filter(Type == "Sample"),
                       aes(x = Row, y = CorrFactor),
                       colour = "grey50", size = 2, alpha = 0.6) +
            geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
            theme_bw() +
            labs(
              title = paste0("RDX Drift Correction (", drift_method,
                             ", R\\u00B2 = ", r2_val, ")"),
              subtitle = "QC correction factors (red), fitted model (blue), samples (grey)",
              x = "Injection Number",
              y = "Correction Factor"
            )

          ggsave(
            "RDX_DriftCorrection.png",
            p_drift,
            path = Results.dir,
            width = 10, height = 5, dpi = 300
          )

        } else {
          Combined$rdx_drift_model <- "None"
        }

      } else {
        message("  RDX drift correction: fewer than 2 usable QC points -- skipping.")
        Combined$rdx_drift_model <- "None"
      }

    } else {
      message("  RDX drift correction: reference value invalid -- skipping.")
      Combined$rdx_drift_model <- "None"
    }
  } else {
    message("  RDX drift correction: no usable QC rows -- skipping.")
    Combined$rdx_drift_model <- "None"
  }

} else if (!apply_rdx_drift_correction) {
  Combined$rdx_drift_model <- "Disabled"
  message("  RDX drift correction: disabled (apply_rdx_drift_correction = FALSE).")
} else if (use_is_for_rdx) {
  Combined$rdx_drift_model <- "Not_needed"
  message("  RDX drift correction: not needed (use_is_for_rdx = TRUE, IS ratio correction sufficient).")
}

# =========================================================
# RDX Drift-Corrected Calibration Plots
# =========================================================

if (apply_rdx_drift_correction && 
    !use_is_for_rdx &&
    !is.na(Combined$rdx_drift_model[1]) &&
    Combined$rdx_drift_model[1] %in% c("Quadratic", "Linear")) {
  
  message("  Generating RDX drift-corrected calibration plots...")
  
  for (model_idx in seq_along(stored_rdx_models)) {
    
    m <- stored_rdx_models[[model_idx]]
    
    cal_data <- Combined %>%
      filter(Type == "Cal", CalibrationSet == model_idx, !is.na(rdx_pa)) %>%
      mutate(CalLevelAdj = CalLevel * Dilution)
    
    if (nrow(cal_data) > 0) {
      curve_x <- seq(min(cal_data$CalLevelAdj, na.rm = TRUE),
                     max(cal_data$CalLevelAdj, na.rm = TRUE),
                     length.out = 200)
      curve_df <- data.frame(
        CalLevelAdj = curve_x,
        Fitted = predict(m$quad_model, newdata = data.frame(CalLevelAdj = curve_x))
      )
      
      pd_dc <- Combined %>%
        filter(CalibrationSet == model_idx) %>%
        mutate(
          Xplot = case_when(
            Type %in% c("Cal", "QC") ~ CalLevel * Dilution,
            TRUE                     ~ rdx_concentration_dc
          ),
          y_val = rdx_pa_dc,
          Group = case_when(
            Type == "Cal" ~ "Standard",
            Type == "QC"  ~ "QC",
            TRUE          ~ "Sample"
          ),
          SNR_Flag = if ("rdx_snr_flag" %in% names(.)) rdx_snr_flag else NA_character_
        ) %>%
        filter(!is.na(y_val), is.finite(Xplot))
      
      if (nrow(pd_dc) > 0 && nrow(curve_df) > 0) {
        p_dc <- ggplot() +
          geom_line(data = curve_df, aes(CalLevelAdj, Fitted), color = "black") +
          geom_point(data = pd_dc, aes(Xplot, y_val, color = Group, shape = SNR_Flag), size = 3) +
          scale_shape_manual(
            values = c("Quantifiable" = 16, "Below_LOQ" = 17, "Below_LOD" = 4),
            na.value = 1
          ) +
          theme_bw() +
          labs(
            title = paste("RDX - Calibration", model_idx, "(Drift-Corrected)"),
            x = "RDX concentration (ng)",
            y = "Corrected Peak Area",
            shape = "SNR Flag"
          )
        
        ggsave(
          paste0("RDX_Calibration_", model_idx, "_DriftCorrected.png"),
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
# Final QC calculations
# =========================================================
if (has_calibration) {

Combined <- Combined %>%
  mutate(TrueCalConcAdj = ifelse(Type %in% c("Cal", "QC"), CalLevel * Dilution, NA_real_))

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
# Apply Fixed Concentration Threshold Range Flags (Samples Only)
# =========================================================
# Range flags evaluate sample concentrations against fixed thresholds:
#   < 0.1 ng/µL → "Below_range"
#   0.1-0.2 ng/µL → "Warning"  
#   ≥ 0.2 ng/µL → "Within_range"
#   NA concentration → NA flag
# Applied to Sample rows only (not QCs, Cals, or Blanks)

# Initialize drift-corrected range flag columns if drift correction was applied
if ("petn_concentration_dc" %in% names(Combined)) {
  Combined$petn_range_flag_dc <- NA_character_
}
if ("rdx_concentration_dc" %in% names(Combined)) {
  Combined$rdx_range_flag_dc <- NA_character_
}

for (name in names(analytes)) {
  prefix <- tolower(name)
  
  # Uncorrected concentration range flags
  conc_col <- paste0(prefix, "_concentration")
  range_col <- paste0(prefix, "_range_flag")
  
  if (conc_col %in% names(Combined)) {
    for (i in which(Combined$Type == "Sample")) {
      conc <- Combined[[conc_col]][i]
      
      if (is.na(conc)) {
        Combined[[range_col]][i] <- NA_character_
      } else if (conc < 0.1) {
        Combined[[range_col]][i] <- "Below_range"
      } else if (conc >= 0.1 && conc < 0.2) {
        Combined[[range_col]][i] <- "Warning"
      } else {
        Combined[[range_col]][i] <- "Within_range"
      }
    }
  }
  
  # Drift-corrected concentration range flags (PETN only, when applicable)
  if (name == "PETN" && "petn_concentration_dc" %in% names(Combined)) {
    for (i in which(Combined$Type == "Sample")) {
      conc_dc <- Combined$petn_concentration_dc[i]
      
      if (is.na(conc_dc)) {
        Combined$petn_range_flag_dc[i] <- NA_character_
      } else if (conc_dc < 0.1) {
        Combined$petn_range_flag_dc[i] <- "Below_range"
      } else if (conc_dc >= 0.1 && conc_dc < 0.2) {
        Combined$petn_range_flag_dc[i] <- "Warning"
      } else {
        Combined$petn_range_flag_dc[i] <- "Within_range"
      }
    }
  }
  
  # RDX drift-corrected range flags (when PA-only drift correction applied)
  if (name == "RDX" && "rdx_concentration_dc" %in% names(Combined)) {
    for (i in which(Combined$Type == "Sample")) {
      conc_dc <- Combined$rdx_concentration_dc[i]
      
      if (is.na(conc_dc)) {
        Combined$rdx_range_flag_dc[i] <- NA_character_
      } else if (conc_dc < 0.1) {
        Combined$rdx_range_flag_dc[i] <- "Below_range"
      } else if (conc_dc >= 0.1 && conc_dc < 0.2) {
        Combined$rdx_range_flag_dc[i] <- "Warning"
      } else {
        Combined$rdx_range_flag_dc[i] <- "Within_range"
      }
    }
  }
}

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
    # 0.2ng: SNR-based (use uncorrected SNR since drift correction doesn't change SNR)
    snr_val <- if ("petn_snr" %in% names(Combined)) Combined$petn_snr[i] else NA_real_
    if (is.na(snr_val)) {
      Combined$petn_qc_flag_dc[i] <- NA_character_
    } else if (snr_val >= 10) {
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_PASS"
    } else if (snr_val >= 3 && snr_val < 10) {
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_WARN"
    } else {
      Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
    }
  } else {
    # Higher QCs: Bias + SNR
    bias_val_dc <- Combined$petn_percent_bias_dc[i]
    snr_val <- if ("petn_snr" %in% names(Combined)) Combined$petn_snr[i] else NA_real_
    if (is.na(bias_val_dc)) {
      Combined$petn_qc_flag_dc[i] <- NA_character_
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

Combined$rdx_percent_bias_dc <- ifelse(
  Combined$Type %in% c("Cal", "QC") &
    !is.na(Combined$rdx_concentration_dc) &
    !is.na(Combined$TrueCalConcAdj),
  (Combined$rdx_concentration_dc - Combined$TrueCalConcAdj) / Combined$TrueCalConcAdj * 100,
  NA_real_
)

# RDX drift-corrected QC flags (apply same logic as uncorrected)
Combined$rdx_qc_flag_dc <- NA_character_
for (i in which(Combined$Type == "QC")) {
  cal_level <- Combined$CalLevel[i]
  if (is.na(cal_level)) next
  
  if (cal_level == 0.2) {
    # 0.2ng: SNR-based (use uncorrected SNR since drift correction doesn't change SNR)
    snr_val <- if ("rdx_snr" %in% names(Combined)) Combined$rdx_snr[i] else NA_real_
    if (is.na(snr_val)) {
      Combined$rdx_qc_flag_dc[i] <- NA_character_
    } else if (snr_val >= 10) {
      Combined$rdx_qc_flag_dc[i] <- "SENSITIVITY_CHECK_PASS"
    } else if (snr_val >= 3 && snr_val < 10) {
      Combined$rdx_qc_flag_dc[i] <- "SENSITIVITY_CHECK_WARN"
    } else {
      Combined$rdx_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
    }
  } else {
    # Higher QCs: Bias + SNR
    bias_val_dc <- Combined$rdx_percent_bias_dc[i]
    snr_val <- if ("rdx_snr" %in% names(Combined)) Combined$rdx_snr[i] else NA_real_
    if (is.na(bias_val_dc)) {
      Combined$rdx_qc_flag_dc[i] <- NA_character_
    } else if (abs(bias_val_dc) <= qc_bias_limit && (!is.na(snr_val) && snr_val >= 10)) {
      Combined$rdx_qc_flag_dc[i] <- "PASS"
    } else if (abs(bias_val_dc) > qc_bias_limit) {
      Combined$rdx_qc_flag_dc[i] <- "FAIL_BIAS"
    } else if (!is.na(snr_val) && snr_val < 10) {
      Combined$rdx_qc_flag_dc[i] <- "FAIL_SNR"
    } else {
      Combined$rdx_qc_flag_dc[i] <- "FAIL"
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
      Combined[[qc_type_col]][i] <- "SENSITIVITY"
      
      snr_val <- if (snr_col %in% names(Combined)) Combined[[snr_col]][i] else NA_real_
      
      if (is.na(snr_val)) {
        Combined[[flag_col]][i] <- NA_character_
      } else if (snr_val >= 10) {
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_PASS"
      } else if (snr_val >= 3 && snr_val < 10) {
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_WARN"
      } else {
        Combined[[flag_col]][i] <- "SENSITIVITY_CHECK_FAIL"
      }
      
    } else {
      # Higher QCs (6ng): Bias + SNR evaluation (quantitative QCs)
      Combined[[qc_type_col]][i] <- "QUANTITATIVE"
      
      bias_val <- if (bias_col %in% names(Combined)) Combined[[bias_col]][i] else NA_real_
      snr_val <- if (snr_col %in% names(Combined)) Combined[[snr_col]][i] else NA_real_
      
      if (is.na(bias_val)) {
        Combined[[flag_col]][i] <- NA_character_
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

    # ===== 6ng QC Accuracy Plot - Drift Corrected (RDX PA-only mode) =====
    if (analyte_name == "RDX" &&
        !use_is_for_rdx &&
        "rdx_percent_bias_dc" %in% names(qc_plot_data) &&
        !all(is.na(qc_plot_data$rdx_percent_bias_dc))) {

      qc_6ng_dc <- qc_plot_data %>%
        filter(CalLevel == 6, !is.na(rdx_percent_bias_dc)) %>%
        select(Row, PercentBias = rdx_percent_bias_dc, QCFlag = rdx_qc_flag_dc)

      if (nrow(qc_6ng_dc) > 0) {
        p_6ng_bias_dc <- ggplot(qc_6ng_dc, aes(x = Row, y = PercentBias, shape = QCFlag)) +
          geom_point(size = 4, colour = "darkgreen") +
          geom_line(colour = "darkgreen") +
          geom_hline(yintercept = 0, colour = "grey50") +
          geom_hline(yintercept = c(-qc_bias_limit, qc_bias_limit),
                     linetype = "dashed", colour = "red", linewidth = 1) +
          theme_bw() +
          labs(
            title = "RDX - 6ng QC Accuracy (Drift-Corrected)",
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
          "RDX_QC_6ng_Accuracy_DriftCorrected.png",
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

write.csv(Combined, outfile, row.names = FALSE)

# =========================================================
# Export XLSX workbook with separate sheets
# =========================================================
xlsx_outfile <- file.path(
  Results.dir,
  paste0(ParentFolder, "_GCMSResults.xlsx")
)

# Tailored column sets per sheet type
cal_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
              # Internal Standard (15N-RDX)
              "rdx_is_rt", "rdx_is_pa", "rdx_is_snr", "rdx_is_snr_flag",
              # === ALL PETN COLUMNS ===
              # PETN raw measurements
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
              # PETN quantitation
              "petn_concentration", "petn_range_flag", "petn_quant_method",
              # PETN accuracy
              "petn_percent_bias",
              # === ALL RDX COLUMNS ===
              # RDX raw measurements
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
              # RDX quantitation
              "rdx_concentration", "rdx_range_flag", "rdx_quant_method",
              # RDX accuracy
              "rdx_percent_bias",
              # True concentration (for both analytes)
              "TrueCalConcAdj")

qc_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
             # Internal Standard (15N-RDX)
             "rdx_is_rt", "rdx_is_pa", "rdx_is_snr", "rdx_is_snr_flag",
             # === ALL PETN COLUMNS ===
             # PETN raw measurements
             "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
             # PETN quantitation (uncorrected)
             "petn_concentration", "petn_range_flag",
             # PETN drift correction
             "petn_drift_correction_factor", "petn_concentration_dc",
             # PETN accuracy (uncorrected)
             "petn_percent_bias",
             # PETN QC evaluation
             "petn_qc_type", "petn_qc_flag",
             # PETN accuracy (drift-corrected)
             "petn_percent_bias_dc", "petn_qc_flag_dc",
             # === ALL RDX COLUMNS ===
             # RDX raw measurements
             "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
             # RDX quantitation (uncorrected)
             "rdx_concentration", "rdx_range_flag",
             # RDX drift correction
             "rdx_drift_correction_factor", "rdx_concentration_dc",
             # RDX accuracy (uncorrected)
             "rdx_percent_bias",
             # RDX QC evaluation
             "rdx_qc_type", "rdx_qc_flag",
             # RDX accuracy (drift-corrected)
             "rdx_percent_bias_dc", "rdx_qc_flag_dc",
             # True concentration (for both analytes)
             "TrueCalConcAdj")

smp_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial",
              # Internal Standard (15N-RDX)
              "rdx_is_rt", "rdx_is_pa", "rdx_is_snr", "rdx_is_snr_flag",
              # === ALL PETN COLUMNS ===
              # PETN raw measurements
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
              # PETN quantitation (uncorrected)
              "petn_concentration", "petn_range_flag", "petn_quant_method",
              # PETN drift correction
              "petn_drift_correction_factor", "petn_concentration_dc",
              # === ALL RDX COLUMNS ===
              # RDX raw measurements
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
              # RDX quantitation (uncorrected)
              "rdx_concentration", "rdx_range_flag", "rdx_quant_method",
              # RDX drift correction
              "rdx_drift_correction_factor", "rdx_concentration_dc")

blk_cols <- c("Date", "Line", "SampleName", "DataFile", "Vial",
              # === ALL PETN COLUMNS ===
              "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag",
              # === ALL RDX COLUMNS ===
              "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag")

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
                "rdx_is_rt", "rdx_is_pa", "rdx_is_snr")
bql_columns <- c("petn_concentration", "rdx_concentration",
                 "petn_concentration_dc", "rdx_concentration_dc")

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

  qc_export_cols <- c("Date", "DataFile", "SampleName", "CalLevel",
                       "CalibrationSet",
                       "rdx_is_pa", "rdx_is_snr", "rdx_is_snr_flag",
                       "petn_quant_method")

  for (name in names(analytes)) {
    prefix <- tolower(name)

    qc_export_cols <- c(qc_export_cols,
      paste0(prefix, "_pa"),
      paste0(prefix, "_snr"),
      paste0(prefix, "_snr_flag"),
      paste0(prefix, "_ratio"),
      paste0(prefix, "_concentration"),
      paste0(prefix, "_range_flag"),
      paste0(prefix, "_percent_bias"),
      paste0(prefix, "_qc_flag"))
  }

  qc_export_cols <- c(qc_export_cols,
    "petn_drift_correction_factor", "petn_drift_model",
    "petn_concentration_dc",
    "petn_percent_bias_dc", "petn_qc_flag_dc",
    "rdx_drift_correction_factor", "rdx_drift_model",
    "rdx_concentration_dc",
    "rdx_percent_bias_dc", "rdx_qc_flag_dc")

  qc_export_cols <- intersect(qc_export_cols, names(Combined))

  QCResults <- Combined %>%
    filter(Type == "QC") %>%
    select(all_of(qc_export_cols))

  if (nrow(QCResults) > 0) {
    if (file.exists(qc_monitoring_file)) {
      ExistingQC <- read.csv(qc_monitoring_file, stringsAsFactors = FALSE)

      ExistingQC$Date <- as.character(ExistingQC$Date)
      QCResults$Date  <- as.character(QCResults$Date)

      CombinedQC <- bind_rows(ExistingQC, QCResults) %>%
        distinct(DataFile, .keep_all = TRUE)

      qc_backup_dir <- file.path(dirname(qc_monitoring_file), "Backup")
      dir.create(qc_backup_dir, recursive = TRUE, showWarnings = FALSE)
      qc_backup_name <- paste0(
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        "_SystemMonitoring_backup.csv"
      )
      write.csv(ExistingQC,
                file = file.path(qc_backup_dir, qc_backup_name),
                row.names = FALSE)
      message("  QC monitoring file backed up to: ", file.path(qc_backup_dir, qc_backup_name))

      write.csv(CombinedQC, file = qc_monitoring_file, row.names = FALSE)

    } else {
      dir.create(dirname(qc_monitoring_file), recursive = TRUE, showWarnings = FALSE)
      write.csv(QCResults, file = qc_monitoring_file, row.names = FALSE)
    }
  }
}

message("Quantification complete. Results exported to: ", outfile)

# ======================= END OF SCRIPT ====================
