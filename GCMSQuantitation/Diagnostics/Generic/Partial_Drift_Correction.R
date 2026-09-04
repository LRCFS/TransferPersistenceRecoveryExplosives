#############################################################
# Partial_Drift_Correction.R
#############################################################
# Standalone script to apply drift correction to a specific
# region of a GC-MS sequence where drift is present, while
# leaving earlier injections unchanged.
#
# Use case: 20260602 Filter Test data where:
#   - Lines 1-46 (filtered samples): No drift, QCs stable
#   - Lines 47-75 (unfiltered samples): Drift present
#
# This script:
#   1. Loads existing _GCMSResults.csv
#   2. Builds drift model from QCs in drifted region
#   3. Applies correction only to that region
#   4. Re-quantifies concentrations
#   5. Outputs corrected file with diagnostics
#
# Developed for: PETN drift correction (PA-only method)
# RDX uses IS ratio and is left unchanged.
#############################################################

# =========================================================
# Configuration
# =========================================================

# User: Set the path to your data folder
DataFolder <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260602 Filter Test"

# User: Define the line number where drift region starts and ends
drift_start_line <- 47
drift_end_line   <- 75

# User: Which QC level to use for drift model (typically highest level)
qc_level_for_drift <- 6

# User: Which analyte to correct (this script is designed for single-analyte correction)
analyte_prefix <- "petn"  # Uses petn_pa, petn_concentration, etc.

# User: Calibration level for reference value (should match qc_level_for_drift)
cal_ref_level <- 6

# =========================================================
# Load Packages
# =========================================================

required_packages <- c("dplyr", "ggplot2", "gridExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
  }
}

library(dplyr)
library(ggplot2)
library(gridExtra)

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# =========================================================
# Helper Functions
# =========================================================

# Solve quadratic equation: y = a*x^2 + b*x + c
# Returns smallest positive root, or NA if no valid solution
solve_quadratic_conc <- function(y, a, b, c, cal_y_range = NULL) {
  
  # Handle edge cases
  if (is.na(y) || !is.finite(y) || y <= 0) {
    return(list(value = NA_real_, range_flag = "Invalid_input"))
  }
  
  # Quadratic formula: x = (-b + sqrt(b^2 - 4*a*(c-y))) / (2*a)
  disc <- b^2 - 4*a*(c - y)
  
  if (disc < 0) {
    return(list(value = NA_real_, range_flag = "No_real_roots"))
  }
  
  x1 <- (-b + sqrt(disc)) / (2*a)
  x2 <- (-b - sqrt(disc)) / (2*a)
  
  valid_roots <- c(x1, x2)[c(x1, x2) > 0]
  
  if (length(valid_roots) == 0) {
    return(list(value = NA_real_, range_flag = "No_positive_roots"))
  }
  
  conc <- min(valid_roots)
  
  # Determine range flag
  range_flag <- "Within_range"
  if (!is.null(cal_y_range)) {
    if (y < min(cal_y_range)) {
      range_flag <- "Below_range"
    } else if (y > max(cal_y_range)) {
      range_flag <- "Above_range"
    }
  }
  
  list(value = conc, range_flag = range_flag)
}

# =========================================================
# Discover Files
# =========================================================

Results.dir <- file.path(DataFolder, "Results")

# Find _GCMSResults.csv
results_files <- list.files(Results.dir, pattern = "_GCMSResults\\.csv$", full.names = TRUE)

if (length(results_files) == 0) {
  stop("No *_GCMSResults.csv file found in: ", Results.dir)
}

if (length(results_files) > 1) {
  warning("Multiple results files found. Using the first one: ", basename(results_files[1]))
}

ResultsFile <- results_files[1]
message("Loading: ", basename(ResultsFile))

# =========================================================
# Load Data
# =========================================================

Combined <- read.csv(ResultsFile, stringsAsFactors = FALSE)

# Ensure Line column exists
if (!"Line" %in% names(Combined)) {
  stop("Results file must have a 'Line' column (sequence line numbers)")
}

# Ensure required columns exist
pa_col <- paste0(analyte_prefix, "_pa")
conc_col <- paste0(analyte_prefix, "_concentration")

if (!pa_col %in% names(Combined)) {
  stop("Column '", pa_col, "' not found in results file")
}

message("Loaded ", nrow(Combined), " rows")
message("Applying drift correction to Lines ", drift_start_line, "-", drift_end_line)

# =========================================================
# Define Regions
# =========================================================

Combined <- Combined %>%
  mutate(
    Region = case_when(
      Line < drift_start_line ~ "Filtered",
      Line >= drift_start_line & Line <= drift_end_line ~ "Unfiltered",
      TRUE ~ "Post_Drift"
    ),
    is_drifted = Line >= drift_start_line & Line <= drift_end_line
  )

# =========================================================
# Build Drift Model
# =========================================================

# Get reference value: mean PA from calibration standards at ref level
ref_val <- Combined %>%
  filter(Type == "Cal",
         CalLevel == cal_ref_level,
         !is.na(.data[[pa_col]])) %>%
  summarise(ref_pa = mean(.data[[pa_col]], na.rm = TRUE)) %>%
  pull(ref_pa)

if (is.na(ref_val) || ref_val <= 0) {
  stop("Could not compute reference value from Cal level ", cal_ref_level)
}

message("Reference value (Cal ", cal_ref_level, " ng mean PA): ", round(ref_val, 1))

# Get QC points in drifted region
qc_drift <- Combined %>%
  filter(Type == "QC",
         CalLevel == qc_level_for_drift,
         Line >= drift_start_line,
         Line <= drift_end_line,
         !is.na(.data[[pa_col]])) %>%
  mutate(CorrFactor = ref_val / .data[[pa_col]]) %>%
  filter(is.finite(CorrFactor), CorrFactor > 0)

n_qc <- nrow(qc_drift)

if (n_qc < 3) {
  stop("Need at least 3 QC points for drift correction (required for power law fit). Found: ", n_qc)
}

message("Found ", n_qc, " QC points at ", qc_level_for_drift, " ng in drifted region")

# Fit drift model
if (n_qc == 3) {
  drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
  drift_method <- "Quadratic"
  message("  Using quadratic fit (3 QC points - exact fit)")
} else {
  drift_model <- lm(CorrFactor ~ Row + I(Row^2), data = qc_drift)
  drift_method <- "Quadratic"
  
  r2 <- summary(drift_model)$r.squared
  if (is.na(r2) || r2 < 0.5) {
    message("  Quadratic R2 = ", round(r2, 3), " (poor fit); falling back to linear")
    drift_model <- lm(CorrFactor ~ Row, data = qc_drift)
    drift_method <- "Linear"
  }
}

r2_val <- round(summary(drift_model)$r.squared, 4)
message("  Model: ", drift_method, ", R2 = ", r2_val)

# =========================================================
# Predict Correction Factors
# =========================================================

# All rows get default CF = 1.0
Combined$petn_drift_cf_partial <- 1.0

# Drifted region gets predicted CF
drifted_indices <- Combined$is_drifted

predicted_cf <- predict(drift_model, newdata = data.frame(Row = Combined$Row[drifted_indices]))

# Clamp negative predictions to 1.0
predicted_cf[predicted_cf < 0] <- 1.0

Combined$petn_drift_cf_partial[drifted_indices] <- predicted_cf

message("Correction factor range in drifted region: ",
        round(min(predicted_cf), 4), " - ",
        round(max(predicted_cf), 4))

# =========================================================
# Apply Correction
# =========================================================

Combined$petn_pa_corrected <- Combined[[pa_col]] * Combined$petn_drift_cf_partial

# =========================================================
# Re-fit Calibration
# =========================================================

cal_data <- Combined %>%
  filter(Type == "Cal",
         !is.na(petn_pa_corrected),
         !is.na(CalLevel))

if (nrow(cal_data) < 3) {
  stop("Need at least 3 calibration points to fit quadratic model")
}

# Fit weighted quadratic calibration
cal_model <- lm(petn_pa_corrected ~ CalLevel + I(CalLevel^2),
                data = cal_data,
                weights = 1/CalLevel)

cal_coefs <- coef(cal_model)
cal_a <- cal_coefs["I(CalLevel^2)"]
cal_b <- cal_coefs["CalLevel"]
cal_c <- cal_coefs["(Intercept)"]

# Get calibration y-range for range detection
cal_y_range <- range(cal_data$petn_pa_corrected, na.rm = TRUE)

# Calibrate dilution factor from existing data
Dilution <- 95 / 100  # Default, could be extracted from existing conc column

message("Calibration model fitted:")
message("  y = ", round(cal_a, 2), "*x^2 + ", round(cal_b, 2), "*x + ", round(cal_c, 0))

# =========================================================
# Re-quantify Concentrations
# =========================================================

# Create new columns for corrected concentrations
Combined$petn_concentration_corrected <- NA_real_
Combined$petn_range_flag_corrected <- NA_character_

for (i in 1:nrow(Combined)) {
  
  # Skip if PA is missing
  if (is.na(Combined$petn_pa_corrected[i])) {
    next
  }
  
  # Skip if SNR flag indicates Below_LOD or Below_LOQ
  snr_flag_col <- paste0(analyte_prefix, "_snr_flag")
  if (snr_flag_col %in% names(Combined)) {
    snr_flag <- Combined[[snr_flag_col]][i]
    if (!is.na(snr_flag) && snr_flag %in% c("Below_LOD", "Below_LOQ")) {
      Combined$petn_concentration_corrected[i] <- NA_real_
      Combined$petn_range_flag_corrected[i] <- snr_flag
      next
    }
  }
  
  # Solve quadratic for concentration
  res <- solve_quadratic_conc(
    Combined$petn_pa_corrected[i],
    cal_a, cal_b, cal_c,
    cal_y_range
  )
  
  Combined$petn_concentration_corrected[i] <- res$value
  Combined$petn_range_flag_corrected[i] <- res$range_flag
}

# =========================================================
# Re-calculate %Bias for QCs and Calibrants
# =========================================================

Combined <- Combined %>%
  mutate(
    TrueConc = ifelse(Type %in% c("Cal", "QC"), CalLevel * Dilution, NA_real_),
    petn_percent_bias_corrected = ifelse(
      !is.na(TrueConc) & !is.na(petn_concentration_corrected),
      ((petn_concentration_corrected - TrueConc) / TrueConc) * 100,
      NA_real_
    )
  )

# =========================================================
# Generate Diagnostic Plot
# =========================================================

# Plot 1: Drift model
pred_line <- data.frame(
  Row = seq(min(Combined$Row), max(Combined$Row), length.out = 200)
)
pred_line$CorrFactor <- predict(drift_model, newdata = pred_line)
pred_line$CorrFactor[pred_line$CorrFactor < 0] <- NA

sample_points <- data.frame(
  Row = Combined$Row,
  CorrFactor = Combined$petn_drift_cf_partial,
  Region = Combined$Region,
  Type = Combined$Type
)

# Add region shading
region_shade <- data.frame(
  xmin = min(Combined$Row[Combined$Region == "Unfiltered"]),
  xmax = max(Combined$Row[Combined$Region == "Unfiltered"]),
  ymin = 0.95,
  ymax = max(pred_line$CorrFactor, na.rm = TRUE) * 1.05
)

p1 <- ggplot() +
  geom_rect(data = region_shade, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "lightyellow", alpha = 0.5) +
  geom_line(data = pred_line, aes(x = Row, y = CorrFactor),
            colour = "steelblue", linewidth = 1) +
  geom_point(data = qc_drift,
             aes(x = Row, y = CorrFactor),
             colour = "red", size = 4, shape = 17) +
  geom_point(data = sample_points %>% filter(Type == "Sample"),
             aes(x = Row, y = CorrFactor),
             colour = "grey50", size = 2, alpha = 0.6) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
  annotate("text", x = region_shade$xmin + 1, y = region_shade$ymax - 0.02,
           label = "Unfiltered\nRegion", hjust = 0, vjust = 1, size = 3, colour = "grey40") +
  theme_bw() +
  labs(
    title = paste0("PETN Partial Drift Correction (", drift_method, ", R\U00B2 = ", r2_val, ")"),
    subtitle = "Yellow shading = drifted region; Red triangles = QC correction factors",
    x = "Injection Number (Row)",
    y = "Correction Factor"
  )

# Plot 2: QC accuracy before/after
qc_comparison <- Combined %>%
  filter(Type == "QC",
         Line >= drift_start_line,
         Line <= drift_end_line,
         CalLevel == qc_level_for_drift) %>%
  select(Line, Row, CalLevel, petn_percent_bias, petn_percent_bias_corrected) %>%
  tidyr::pivot_longer(
    cols = c(petn_percent_bias, petn_percent_bias_corrected),
    names_to = "Method",
    values_to = "PercentBias"
  ) %>%
  mutate(
    Method = ifelse(Method == "petn_percent_bias", "Before Correction", "After Correction"),
    Method = factor(Method, levels = c("Before Correction", "After Correction"))
  )

p2 <- ggplot(qc_comparison, aes(x = Method, y = PercentBias, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "black") +
  geom_hline(yintercept = c(-15, 15), linetype = "dashed", colour = "red", alpha = 0.5) +
  facet_wrap(~ paste0("QC at Line ", Line)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "QC Accuracy Before vs After Drift Correction",
    subtitle = paste0("6ng QC level in unfiltered region (Lines ", drift_start_line, "-", drift_end_line, ")"),
    x = "",
    y = "%Bias"
  ) +
  scale_fill_manual(values = pal_method)

# Combine plots
p_combined <- grid.arrange(p1, p2, nrow = 2, heights = c(1.2, 0.8))

# Save diagnostic plot
diag_plot_file <- file.path(Results.dir, "Partial_DriftCorrection_Diagnostic.png")
ggsave(diag_plot_file, p_combined, width = 10, height = 8, dpi = 300)
message("Diagnostic plot saved: ", basename(diag_plot_file))

# =========================================================
# Output Corrected Results
# =========================================================

# Remove temporary columns before saving
Combined_out <- Combined %>%
  select(-Region, -is_drifted, -TrueConc)

output_file <- file.path(Results.dir,
                         paste0(gsub("_GCMSResults\\.csv$", "", basename(ResultsFile)),
                                "_DriftCorrected.csv"))

write.csv(Combined_out, output_file, row.names = FALSE)
message("Corrected results saved: ", basename(output_file))

# =========================================================
# Summary Report
# =========================================================

message("\n==================================================")
message("PARTIAL DRIFT CORRECTION SUMMARY")
message("==================================================")
message("Data folder: ", DataFolder)
message("Input file:  ", basename(ResultsFile))
message("Output file: ", basename(output_file))
message("\nRegions:")
message("  Lines 1-", drift_start_line - 1, ": No correction (CF = 1.0)")
message("  Lines ", drift_start_line, "-", drift_end_line, ": Drift correction applied")
message("\nDrift model:")
message("  Method: ", drift_method)
message("  R-squared: ", r2_val)
message("  QC points used: ", n_qc, " at ", qc_level_for_drift, " ng level")
message("  Reference value: ", round(ref_val, 1), " (Cal ", cal_ref_level, " ng mean)")
message("\nCorrection factor range in drifted region:")
message("  Min: ", round(min(predicted_cf), 4))
message("  Max: ", round(max(predicted_cf), 4))
message("\nQC accuracy improvement (6ng QCs in drifted region):")

qc_before <- Combined %>%
  filter(Type == "QC",
         Line >= drift_start_line,
         Line <= drift_end_line,
         CalLevel == qc_level_for_drift) %>%
  pull(petn_percent_bias)

qc_after <- Combined %>%
  filter(Type == "QC",
         Line >= drift_start_line,
         Line <= drift_end_line,
         CalLevel == qc_level_for_drift) %>%
  pull(petn_percent_bias_corrected)

if (length(qc_before) > 0) {
  message("  Before: ", paste(round(qc_before, 1), collapse = ", "), " %")
  message("  After:  ", paste(round(qc_after, 1), collapse = ", "), " %")
}

message("\n==================================================")
message("Drift correction complete.")
message("==================================================")
