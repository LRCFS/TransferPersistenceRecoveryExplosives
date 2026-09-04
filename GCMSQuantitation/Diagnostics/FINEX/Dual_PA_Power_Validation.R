# Phase 4: Dual PA Power Law Validation
# Purpose: Validate power law correction on BOTH PETN PA and IS PA independently,
#          then calculate ratio from corrected PAs for quantification
# This tests the hypothesis that correcting drift at PA level before ratio calculation
# is better than correcting ratio drift directly

library(ggplot2)
library(dplyr)

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# Define datasets
base_path <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"
datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1")

# Initialize results storage
results_summary <- data.frame(
  Dataset = character(),
  PETN_Power_R2 = numeric(),
  PETN_Power_b = numeric(),
  IS_Power_R2 = numeric(),
  IS_Power_b = numeric(),
  Current_Mean_Abs_Bias = numeric(),
  PA_Power_Mean_Abs_Bias = numeric(),
  Dual_Power_Mean_Abs_Bias = numeric(),
  Current_Pass_Rate = numeric(),
  PA_Power_Pass_Rate = numeric(),
  Dual_Power_Pass_Rate = numeric(),
  stringsAsFactors = FALSE
)

all_qc_data <- list()
detailed_results <- list()

# Function to apply dual PA power law correction
apply_dual_power_correction <- function(data, petn_power_model, is_power_model, cal_model) {
  
  # Predict PA at each injection for BOTH analytes
  petn_predicted_pa <- predict(petn_power_model, newdata = data.frame(Row = data$Row))
  is_predicted_pa <- predict(is_power_model, newdata = data.frame(Row = data$Row))
  
  # Calculate correction factors (normalize to first QC)
  first_qc_row <- min(data$Row[data$Type == "QC" & data$CalLevel == 6 & 
                                !is.na(data$petn_pa) & !is.na(data$rdx_is_pa)])
  
  reference_petn_pa <- predict(petn_power_model, newdata = data.frame(Row = first_qc_row))
  reference_is_pa <- predict(is_power_model, newdata = data.frame(Row = first_qc_row))
  
  petn_cf <- reference_petn_pa / petn_predicted_pa
  is_cf <- reference_is_pa / is_predicted_pa
  
  # Only correct rows after first QC
  petn_cf[data$Row <= first_qc_row] <- 1.0
  is_cf[data$Row <= first_qc_row] <- 1.0
  
  # Apply corrections to raw PAs
  data$petn_pa_corrected_dual <- data$petn_pa * petn_cf
  data$is_pa_corrected_dual <- data$rdx_is_pa * is_cf
  
  # Calculate corrected ratio from corrected PAs
  data$petn_ratio_corrected_dual <- data$petn_pa_corrected_dual / data$is_pa_corrected_dual
  
  # Re-quantify using ratio calibration model
  cal_coefs <- coef(cal_model)
  
  if (length(cal_coefs) == 3) {
    # Quadratic: a*x^2 + b*x + c = y
    a <- cal_coefs[3]
    b <- cal_coefs[2]
    c <- cal_coefs[1]
    
    data$petn_conc_dual <- sapply(data$petn_ratio_corrected_dual, function(y) {
      if (is.na(y) || y <= 0) return(NA_real_)
      
      discriminant <- b^2 - 4*a*(c - y)
      if (discriminant < 0) return(0)
      
      roots <- c(
        (-b + sqrt(discriminant)) / (2*a),
        (-b - sqrt(discriminant)) / (2*a)
      )
      
      positive_roots <- roots[roots >= 0]
      if (length(positive_roots) == 0) return(0)
      return(min(positive_roots))
    })
    
  } else {
    # Linear
    slope <- cal_coefs[2]
    intercept <- cal_coefs[1]
    data$petn_conc_dual <- (data$petn_ratio_corrected_dual - intercept) / slope
    data$petn_conc_dual[data$petn_conc_dual < 0] <- 0
  }
  
  return(data)
}

# Process each dataset
for (i in seq_along(datasets)) {
  dataset_name <- datasets[i]
  cat("\n========================================\n")
  cat("Processing:", dataset_name, "\n")
  cat("========================================\n")
  
  # Read results file
  results_file <- file.path(base_path, dataset_name, "Results", 
                           paste0(dataset_name, "_GCMSResults.csv"))
  
  if (!file.exists(results_file)) {
    cat("WARNING: File not found:", results_file, "\n")
    next
  }
  
  data <- read.csv(results_file, stringsAsFactors = FALSE)
  
  # Check required columns exist
  required_cols <- c("petn_pa", "rdx_is_pa", "petn_ratio")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    cat("WARNING: Missing columns:", paste(missing_cols, collapse=", "), "\n")
    next
  }
  
  # Extract PETN 6ng QC data
  qc_6ng <- data %>%
    filter(Type == "QC", CalLevel == 6, 
           !is.na(petn_pa), petn_pa > 0,
           !is.na(rdx_is_pa), rdx_is_pa > 0) %>%
    select(Row, petn_pa, rdx_is_pa, petn_ratio,
           petn_concentration, petn_percent_bias,
           petn_concentration_dc, petn_percent_bias_dc) %>%
    arrange(Row)
  
  if (nrow(qc_6ng) < 3) {
    cat("WARNING: Insufficient QC data (n =", nrow(qc_6ng), ")\n")
    next
  }
  
  cat("Found", nrow(qc_6ng), "PETN 6ng QCs at rows:", paste(qc_6ng$Row, collapse=", "), "\n")
  
  qc_6ng$Dataset <- dataset_name
  all_qc_data[[dataset_name]] <- qc_6ng
  
  # ====================================================
  # Fit power curve to PETN PA
  # ====================================================
  cat("\n--- Fitting Power Curves ---\n")
  cat("PETN PA: PA ~ a * Row^b\n")
  
  petn_power_model <- tryCatch({
    nls(petn_pa ~ a * Row^b, 
        data = qc_6ng,
        start = list(a = max(qc_6ng$petn_pa), b = -1.0),
        control = nls.control(maxiter = 100))
  }, error = function(e) {
    cat("ERROR fitting PETN power curve:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(petn_power_model)) {
    petn_coef <- coef(petn_power_model)
    petn_a <- petn_coef["a"]
    petn_b <- petn_coef["b"]
    
    petn_residuals <- residuals(petn_power_model)
    petn_ss_res <- sum(petn_residuals^2)
    petn_ss_tot <- sum((qc_6ng$petn_pa - mean(qc_6ng$petn_pa))^2)
    petn_r2 <- 1 - (petn_ss_res / petn_ss_tot)
    
    cat("  PETN: a =", format(petn_a, scientific=FALSE), ", b =", round(petn_b, 4), 
        ", R² =", round(petn_r2, 4), "\n")
    
    qc_6ng$predicted_petn_pa <- predict(petn_power_model, newdata = qc_6ng)
  } else {
    petn_r2 <- NA
    petn_b <- NA
  }
  
  # ====================================================
  # Fit power curve to IS PA (15N-RDX)
  # ====================================================
  cat("IS PA: PA ~ a * Row^b\n")
  
  is_power_model <- tryCatch({
    nls(rdx_is_pa ~ a * Row^b, 
        data = qc_6ng,
        start = list(a = max(qc_6ng$rdx_is_pa), b = -1.0),
        control = nls.control(maxiter = 100))
  }, error = function(e) {
    cat("ERROR fitting IS power curve:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(is_power_model)) {
    is_coef <- coef(is_power_model)
    is_a <- is_coef["a"]
    is_b <- is_coef["b"]
    
    is_residuals <- residuals(is_power_model)
    is_ss_res <- sum(is_residuals^2)
    is_ss_tot <- sum((qc_6ng$rdx_is_pa - mean(qc_6ng$rdx_is_pa))^2)
    is_r2 <- 1 - (is_ss_res / is_ss_tot)
    
    cat("  IS:   a =", format(is_a, scientific=FALSE), ", b =", round(is_b, 4), 
        ", R² =", round(is_r2, 4), "\n")
    
    qc_6ng$predicted_is_pa <- predict(is_power_model, newdata = qc_6ng)
  } else {
    is_r2 <- NA
    is_b <- NA
  }
  
  # Check if both models fit well
  if (is.null(petn_power_model) || is.null(is_power_model)) {
    cat("WARNING: Cannot proceed with dual correction - one or both power models failed\n")
    next
  }
  
  # ====================================================
  # Extract calibration curve (RATIO-BASED)
  # ====================================================
  cal_data <- data %>%
    filter(Type == "Cal", !is.na(petn_ratio), petn_ratio > 0, 
           !is.na(CalLevel), CalLevel > 0) %>%
    select(Row, CalLevel, petn_ratio) %>%
    arrange(CalLevel)
  
  cat("\nCalibration (ratio-based): n =", nrow(cal_data), "standards\n")
  
  cal_model <- lm(petn_ratio ~ poly(CalLevel, 2, raw = TRUE), 
                  data = cal_data, 
                  weights = 1/CalLevel)
  
  cal_r2 <- summary(cal_model)$r.squared
  cat("  Calibration R² =", round(cal_r2, 4), "\n")
  
  # ====================================================
  # Apply dual PA power law correction
  # ====================================================
  cat("\n--- Applying Dual PA Power Law Correction ---\n")
  
  data_corrected_dual <- apply_dual_power_correction(data, petn_power_model, 
                                                      is_power_model, cal_model)
  
  # Extract QC results with dual correction
  qc_results <- data_corrected_dual %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa)) %>%
    mutate(
      petn_percent_bias_dual = ((petn_conc_dual - 6) / 6) * 100
    ) %>%
    select(Row, 
           petn_concentration, petn_percent_bias,           # Uncorrected
           petn_concentration_dc, petn_percent_bias_dc,     # Current (ratio polynomial)
           petn_conc_dual, petn_percent_bias_dual)          # Dual PA power law
  
  # Also get PA-only power law results from previous analysis
  # For fair comparison, read from PA-based results
  pa_results_file <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX/PowerCurve_Comparison_Summary.csv"
  if (file.exists(pa_results_file)) {
    pa_summary <- read.csv(pa_results_file)
    pa_bias_row <- pa_summary[pa_summary$Dataset == dataset_name, ]
    if (nrow(pa_bias_row) > 0) {
      pa_power_bias <- pa_bias_row$Power_Row_Mean_Abs_Bias
    } else {
      pa_power_bias <- NA
    }
  } else {
    pa_power_bias <- NA
  }
  
  # Calculate summary statistics
  cat("\n--- QC Performance Comparison (6ng level) ---\n")
  
  # Current method (ratio-based polynomial drift correction)
  current_mean_bias <- mean(abs(qc_results$petn_percent_bias_dc), na.rm = TRUE)
  current_pass_rate <- mean(abs(qc_results$petn_percent_bias_dc) <= 15, na.rm = TRUE) * 100
  
  # Dual PA power law
  dual_mean_bias <- mean(abs(qc_results$petn_percent_bias_dual), na.rm = TRUE)
  dual_pass_rate <- mean(abs(qc_results$petn_percent_bias_dual) <= 15, na.rm = TRUE) * 100
  
  cat("\n1. Current Method (Ratio Polynomial Drift Correction):\n")
  cat("   Mean |%Bias|:", round(current_mean_bias, 2), "%\n")
  cat("   QC Pass Rate:", round(current_pass_rate, 1), "%\n")
  
  cat("\n2. PA-Only Power Law (from previous analysis):\n")
  cat("   Mean |%Bias|:", round(pa_power_bias, 2), "%\n")
  
  cat("\n3. Dual PA Power Law (PETN PA + IS PA correction → ratio quant):\n")
  cat("   Mean |%Bias|:", round(dual_mean_bias, 2), "%\n")
  cat("   QC Pass Rate:", round(dual_pass_rate, 1), "%\n")
  cat("   Improvement vs Current:", round(current_mean_bias - dual_mean_bias, 2), "% points\n")
  cat("   Improvement vs PA-Only:", round(pa_power_bias - dual_mean_bias, 2), "% points\n")
  
  # Store detailed results
  detailed_results[[dataset_name]] <- list(
    qc_results = qc_results,
    current_mean_bias = current_mean_bias,
    current_pass_rate = current_pass_rate,
    pa_power_mean_bias = pa_power_bias,
    dual_mean_bias = dual_mean_bias,
    dual_pass_rate = dual_pass_rate
  )
  
  # Update results summary
  results_summary <- rbind(results_summary, data.frame(
    Dataset = dataset_name,
    PETN_Power_R2 = petn_r2,
    PETN_Power_b = petn_b,
    IS_Power_R2 = is_r2,
    IS_Power_b = is_b,
    Current_Mean_Abs_Bias = current_mean_bias,
    PA_Power_Mean_Abs_Bias = pa_power_bias,
    Dual_Power_Mean_Abs_Bias = dual_mean_bias,
    Current_Pass_Rate = current_pass_rate,
    PA_Power_Pass_Rate = NA,  # From previous analysis
    Dual_Power_Pass_Rate = dual_pass_rate,
    stringsAsFactors = FALSE
  ))
}

# Save results
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX"
write.csv(results_summary, file.path(output_dir, "Dual_PA_Power_Comparison_Summary.csv"), 
          row.names = FALSE)

cat("\n========================================\n")
cat("SUMMARY: Power Law Fit Quality\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "PETN_Power_R2", "PETN_Power_b", 
                          "IS_Power_R2", "IS_Power_b")])

cat("\n========================================\n")
cat("SUMMARY: QC Bias Comparison\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Current_Mean_Abs_Bias", 
                          "PA_Power_Mean_Abs_Bias", "Dual_Power_Mean_Abs_Bias")])

# Calculate improvement metrics
results_summary$Dual_vs_Current <- results_summary$Current_Mean_Abs_Bias - results_summary$Dual_Power_Mean_Abs_Bias
results_summary$Dual_vs_PA <- results_summary$PA_Power_Mean_Abs_Bias - results_summary$Dual_Power_Mean_Abs_Bias

cat("\n========================================\n")
cat("Improvement with Dual PA Power Law\n")
cat("========================================\n")
cat("(Positive = Dual method is better)\n\n")
print(results_summary[, c("Dataset", "Dual_vs_Current", "Dual_vs_PA")])

# Generate diagnostic plots
all_qc_combined <- do.call(rbind, all_qc_data)

# Plot 1: PETN PA vs IS PA power law fits
if (nrow(all_qc_combined) > 0) {
  # Reshape for faceted plot
  pa_long <- all_qc_combined %>%
    select(Dataset, Row, PETN_PA = petn_pa, IS_PA = rdx_is_pa,
           PETN_Predicted = predicted_petn_pa, IS_Predicted = predicted_is_pa) %>%
    tidyr::pivot_longer(cols = c(PETN_PA, IS_PA), 
                       names_to = "Analyte", values_to = "Measured_PA") %>%
    tidyr::pivot_longer(cols = c(PETN_Predicted, IS_Predicted),
                       names_to = "Pred_Type", values_to = "Predicted_PA") %>%
    filter((Analyte == "PETN_PA" & Pred_Type == "PETN_Predicted") |
           (Analyte == "IS_PA" & Pred_Type == "IS_Predicted")) %>%
    select(-Pred_Type)
  
  p1 <- ggplot(pa_long, aes(x = Row)) +
    geom_point(aes(y = Measured_PA, color = Analyte), size = 3) +
    geom_line(aes(y = Predicted_PA, color = Analyte), linewidth = 1, alpha = 0.7) +
    facet_wrap(~Dataset, scales = "free_y") +
    theme_bw() +
    scale_color_manual(values = pal_analyte_vs_is,
                      labels = c("PETN PA", "15N-RDX IS PA")) +
    labs(
      title = "Dual Power Curve Fits: PETN PA and IS PA",
      subtitle = "Points = Measured PA | Lines = Power curve fits (PA ~ a × Row^b)",
      x = "Injection Number (Row)",
      y = "Peak Area (counts)",
      color = "Analyte"
    ) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "Dual_PA_Power_Fits.png"),
         p1, width = 12, height = 8, dpi = 300)
  cat("\nPlot saved: Dual_PA_Power_Fits.png\n")
}

# Plot 2: Exponent comparison (PETN vs IS decay rates)
p2 <- ggplot(results_summary, aes(x = Dataset)) +
  geom_point(aes(y = PETN_Power_b, color = "PETN"), size = 4) +
  geom_point(aes(y = IS_Power_b, color = "IS"), size = 4) +
  geom_hline(yintercept = -1.0, linetype = "dashed", color = "grey50") +
  theme_bw() +
  scale_color_manual(values = pal_analyte_vs_is) +
  labs(
    title = "Power Law Decay Exponents: PETN vs IS",
    subtitle = "More negative = faster signal loss | Dashed line = -1.0 (inverse proportional)",
    x = "Dataset",
    y = "Power Law Exponent (b)",
    color = "Analyte"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Exponent_Comparison.png"),
       p2, width = 8, height = 6, dpi = 300)
cat("Plot saved: Exponent_Comparison.png\n")

# Plot 3: Three-way bias comparison
bias_comparison <- results_summary %>%
  select(Dataset, Current_Mean_Abs_Bias, PA_Power_Mean_Abs_Bias, Dual_Power_Mean_Abs_Bias) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "Mean_Abs_Bias") %>%
  mutate(Method = factor(Method,
                        levels = c("Current_Mean_Abs_Bias", "PA_Power_Mean_Abs_Bias", "Dual_Power_Mean_Abs_Bias"),
                        labels = c("Current (Ratio Poly)", "PA Power (No IS)", "Dual PA Power")))

p3 <- ggplot(bias_comparison, aes(x = Dataset, y = Mean_Abs_Bias, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 15, linetype = "dashed", colour = "red") +
  theme_bw() +
  labs(
    title = "QC Mean Absolute Bias: Three-Method Comparison",
    subtitle = "Red line = ±15% QC acceptance limit | Lower is better",
    x = "Dataset",
    y = "Mean Absolute % Bias",
    fill = "Drift Correction Method"
  ) +
  scale_fill_manual(values = pal_method) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Three_Method_Bias_Comparison.png"),
       p3, width = 10, height = 6, dpi = 300)
cat("Plot saved: Three_Method_Bias_Comparison.png\n")

# Plot 4: Individual QC bias trajectories for dual method
for (dataset_name in names(detailed_results)) {
  qc_data <- detailed_results[[dataset_name]]$qc_results
  
  qc_long <- qc_data %>%
    select(Row, 
           `Current (Ratio Poly)` = petn_percent_bias_dc,
           `Dual PA Power` = petn_percent_bias_dual) %>%
    tidyr::pivot_longer(cols = -Row, names_to = "Method", values_to = "Percent_Bias")
  
  p_traj <- ggplot(qc_long, aes(x = Row, y = Percent_Bias, color = Method, shape = Method)) +
    geom_hline(yintercept = c(-15, 15), linetype = "dashed", colour = "red", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(size = 4) +
    geom_line(alpha = 0.6, linewidth = 1) +
    theme_bw() +
    scale_color_manual(values = pal_method) +
    labs(
      title = paste(dataset_name, "- QC Bias Trajectories: Current vs Dual PA Power"),
      subtitle = "Red lines = ±15% acceptance limits",
      x = "Injection Number (Row)",
      y = "% Bias vs True (6ng)",
      color = "Method",
      shape = "Method"
    ) +
    scale_y_continuous(breaks = seq(-60, 60, 10)) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0(dataset_name, "_Dual_vs_Current_Trajectories.png")),
         p_traj, width = 10, height = 6, dpi = 300)
}

cat("QC bias trajectory plots saved for all datasets.\n")

# Final recommendation
cat("\n========================================\n")
cat("FINAL RECOMMENDATION\n")
cat("========================================\n")

avg_improvement_vs_current <- mean(results_summary$Dual_vs_Current, na.rm = TRUE)
avg_improvement_vs_pa <- mean(results_summary$Dual_vs_PA, na.rm = TRUE)

cat("\nDual PA Power Law Performance:\n")
cat("  Average bias reduction vs Current:", round(avg_improvement_vs_current, 2), "% points\n")
cat("  Average bias reduction vs PA-only:", round(avg_improvement_vs_pa, 2), "% points\n")

# Count how many datasets improved
improved_vs_current <- sum(results_summary$Dual_vs_Current > 0, na.rm = TRUE)
improved_vs_pa <- sum(results_summary$Dual_vs_PA > 0, na.rm = TRUE)

cat("  Datasets improved vs Current:", improved_vs_current, "out of", nrow(results_summary), "\n")
cat("  Datasets improved vs PA-only:", improved_vs_pa, "out of", nrow(results_summary), "\n")

# Check if both PETN and IS follow power law well
good_fits <- sum(results_summary$PETN_Power_R2 > 0.95 & results_summary$IS_Power_R2 > 0.95, na.rm = TRUE)
cat("  Datasets with good power law fits (R² > 0.95) for BOTH analytes:", good_fits, "out of", nrow(results_summary), "\n")

if (avg_improvement_vs_current > 2 && avg_improvement_vs_pa > 0 && good_fits >= 3) {
  cat("\n✅ IMPLEMENT DUAL PA POWER LAW DRIFT CORRECTION\n\n")
  cat("Evidence:\n")
  cat("  - Both PETN PA and IS PA follow power law (R² > 0.95)\n")
  cat("  - Outperforms current ratio-based polynomial correction\n")
  cat("  - Outperforms PA-only power law (maintains IS normalization benefits)\n")
  cat("  - Best of both worlds: power law accuracy + IS normalization\n")
} else if (avg_improvement_vs_current > 0) {
  cat("\n⚠️ MIXED RESULTS: Dual PA power law shows promise but not universal improvement\n")
  cat("Consider dataset-specific selection or further investigation.\n")
} else {
  cat("\n❌ Dual PA power law does NOT improve results\n")
  cat("Stick with current method or PA-only power law.\n")
}

cat("\n========================================\n")
cat("Phase 4 Complete: Dual PA Power Validation\n")
cat("========================================\n")
