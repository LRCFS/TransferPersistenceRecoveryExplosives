# Phase 5: PA Power Law with Uncorrected IS → Ratio Quantification
# Purpose: Test the hypothesis that correcting PETN PA drift with power law,
#          then using the UNCORRECTED IS PA for ratio calculation, produces
#          better results than either PA-only or dual correction approaches
#
# This is what "PA-only power law" validation actually tested - let's verify
# it explicitly and compare to all other methods comprehensively.

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
  Method1_Current_Bias = numeric(),
  Method2_PA_Power_Bias = numeric(),
  Method3_Dual_Power_Bias = numeric(),
  Method4_PETN_Power_UncorrectedIS_Bias = numeric(),
  stringsAsFactors = FALSE
)

detailed_results <- list()

# Function to apply PETN PA power law correction + ratio with uncorrected IS
apply_petn_power_uncorrected_is <- function(data, petn_power_model, cal_model) {
  
  # Predict PETN PA at each injection
  petn_predicted_pa <- predict(petn_power_model, newdata = data.frame(Row = data$Row))
  
  # Calculate PETN correction factors (normalize to first QC)
  first_qc_row <- min(data$Row[data$Type == "QC" & data$CalLevel == 6 & 
                                !is.na(data$petn_pa) & !is.na(data$rdx_is_pa)])
  
  reference_petn_pa <- predict(petn_power_model, newdata = data.frame(Row = first_qc_row))
  
  petn_cf <- reference_petn_pa / petn_predicted_pa
  
  # Only correct rows after first QC
  petn_cf[data$Row <= first_qc_row] <- 1.0
  
  # Apply correction to PETN PA only (IS PA stays raw)
  data$petn_pa_corrected <- data$petn_pa * petn_cf
  
  # Calculate ratio from corrected PETN PA and UNCORRECTED IS PA
  data$petn_ratio_petn_corrected <- data$petn_pa_corrected / data$rdx_is_pa
  
  # Re-quantify using ratio calibration model
  cal_coefs <- coef(cal_model)
  
  if (length(cal_coefs) == 3) {
    # Quadratic
    a <- cal_coefs[3]
    b <- cal_coefs[2]
    c <- cal_coefs[1]
    
    data$petn_conc_method4 <- sapply(data$petn_ratio_petn_corrected, function(y) {
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
    data$petn_conc_method4 <- (data$petn_ratio_petn_corrected - intercept) / slope
    data$petn_conc_method4[data$petn_conc_method4 < 0] <- 0
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
  
  # Fit power curves
  petn_power_model <- nls(petn_pa ~ a * Row^b, 
                          data = qc_6ng,
                          start = list(a = max(qc_6ng$petn_pa), b = -1.0))
  
  is_power_model <- nls(rdx_is_pa ~ a * Row^b,
                        data = qc_6ng,
                        start = list(a = max(qc_6ng$rdx_is_pa), b = -1.0))
  
  # Calculate R²
  petn_r2 <- 1 - sum(residuals(petn_power_model)^2) / sum((qc_6ng$petn_pa - mean(qc_6ng$petn_pa))^2)
  is_r2 <- 1 - sum(residuals(is_power_model)^2) / sum((qc_6ng$rdx_is_pa - mean(qc_6ng$rdx_is_pa))^2)
  
  petn_b <- coef(petn_power_model)["b"]
  is_b <- coef(is_power_model)["b"]
  
  cat("\nPETN PA power law: R² =", round(petn_r2, 4), ", b =", round(petn_b, 4), "\n")
  cat("IS PA power law:   R² =", round(is_r2, 4), ", b =", round(is_b, 4), "\n")
  
  # Extract calibration curve (ratio-based)
  cal_data <- data %>%
    filter(Type == "Cal", !is.na(petn_ratio), petn_ratio > 0, 
           !is.na(CalLevel), CalLevel > 0) %>%
    select(Row, CalLevel, petn_ratio) %>%
    arrange(CalLevel)
  
  cal_model <- lm(petn_ratio ~ poly(CalLevel, 2, raw = TRUE), 
                  data = cal_data, 
                  weights = 1/CalLevel)
  
  # Apply Method 4: PETN PA power correction + uncorrected IS
  data_corrected <- apply_petn_power_uncorrected_is(data, petn_power_model, cal_model)
  
  # Extract QC results
  qc_results <- data_corrected %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa)) %>%
    mutate(
      petn_percent_bias_method4 = ((petn_conc_method4 - 6) / 6) * 100
    ) %>%
    select(Row, 
           petn_concentration, petn_percent_bias,           # Method 1: Current (ratio poly)
           petn_concentration_dc, petn_percent_bias_dc,     # Already in CSV
           petn_conc_method4, petn_percent_bias_method4)    # Method 4: PETN power + raw IS
  
  # Get Method 2 (PA-only power) and Method 3 (Dual power) from previous analyses
  pa_summary <- read.csv("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX/PowerCurve_Comparison_Summary.csv")
  dual_summary <- read.csv("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX/Dual_PA_Power_Comparison_Summary.csv")
  
  method2_bias <- pa_summary$Power_Row_Mean_Abs_Bias[pa_summary$Dataset == dataset_name]
  method3_bias <- dual_summary$Dual_Power_Mean_Abs_Bias[dual_summary$Dataset == dataset_name]
  
  # Calculate summary statistics
  method1_bias <- mean(abs(qc_results$petn_percent_bias_dc), na.rm = TRUE)
  method4_bias <- mean(abs(qc_results$petn_percent_bias_method4), na.rm = TRUE)
  
  cat("\n--- QC Performance Comparison (6ng PETN) ---\n")
  cat("Method 1 (Current: Ratio Poly Drift):       ", round(method1_bias, 2), "%\n")
  cat("Method 2 (PA Power, NO IS):                 ", round(method2_bias, 2), "%\n")
  cat("Method 3 (Dual Power: PETN + IS):           ", round(method3_bias, 2), "%\n")
  cat("Method 4 (PETN Power + Uncorrected IS):     ", round(method4_bias, 2), "%\n")
  
  # Store results
  detailed_results[[dataset_name]] <- qc_results
  
  results_summary <- rbind(results_summary, data.frame(
    Dataset = dataset_name,
    PETN_Power_R2 = petn_r2,
    PETN_Power_b = petn_b,
    IS_Power_R2 = is_r2,
    IS_Power_b = is_b,
    Method1_Current_Bias = method1_bias,
    Method2_PA_Power_Bias = method2_bias,
    Method3_Dual_Power_Bias = method3_bias,
    Method4_PETN_Power_UncorrectedIS_Bias = method4_bias,
    stringsAsFactors = FALSE
  ))
}

# Save results
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX"
write.csv(results_summary, file.path(output_dir, "AllMethods_Comparison_Summary.csv"), 
          row.names = FALSE)

cat("\n========================================\n")
cat("FINAL COMPARISON: All 4 Methods\n")
cat("========================================\n")

print(results_summary[, c("Dataset", "Method1_Current_Bias", "Method2_PA_Power_Bias",
                          "Method3_Dual_Power_Bias", "Method4_PETN_Power_UncorrectedIS_Bias")])

# Determine best method per dataset
results_summary$Best_Method <- apply(
  results_summary[, c("Method1_Current_Bias", "Method2_PA_Power_Bias",
                      "Method3_Dual_Power_Bias", "Method4_PETN_Power_UncorrectedIS_Bias")],
  1,
  function(row) {
    methods <- c("Method1_Current", "Method2_PA_Power", "Method3_Dual_Power", "Method4_PETN_Power_UncorrIS")
    methods[which.min(row)]
  }
)

cat("\n========================================\n")
cat("Best Method per Dataset\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Best_Method")])

# Overall winner (lowest mean bias across all datasets)
overall_means <- colMeans(results_summary[, c("Method1_Current_Bias", "Method2_PA_Power_Bias",
                                               "Method3_Dual_Power_Bias", "Method4_PETN_Power_UncorrectedIS_Bias")],
                          na.rm = TRUE)

cat("\n========================================\n")
cat("Overall Performance (Mean |%Bias| across all datasets)\n")
cat("========================================\n")
cat("Method 1 (Current):                  ", round(overall_means[1], 2), "%\n")
cat("Method 2 (PA Power, NO IS):          ", round(overall_means[2], 2), "%\n")
cat("Method 3 (Dual Power):               ", round(overall_means[3], 2), "%\n")
cat("Method 4 (PETN Power + Raw IS):      ", round(overall_means[4], 2), "%\n")

winner_idx <- which.min(overall_means)
winner_names <- c("Current (Ratio Poly)", "PA Power (No IS)", "Dual Power", "PETN Power + Raw IS")

cat("\n✅ WINNER:", winner_names[winner_idx], "\n")
cat("   Average bias:", round(overall_means[winner_idx], 2), "%\n")

# Generate comparison plot
bias_long <- results_summary %>%
  select(Dataset, Method1_Current_Bias, Method2_PA_Power_Bias,
         Method3_Dual_Power_Bias, Method4_PETN_Power_UncorrectedIS_Bias) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "Mean_Abs_Bias") %>%
  mutate(Method = factor(Method,
                        levels = c("Method1_Current_Bias", "Method2_PA_Power_Bias",
                                   "Method3_Dual_Power_Bias", "Method4_PETN_Power_UncorrectedIS_Bias"),
                        labels = c("Current\n(Ratio Poly)", "PA Power\n(No IS)",
                                   "Dual Power\n(PETN+IS)", "PETN Power +\nRaw IS")))

p <- ggplot(bias_long, aes(x = Dataset, y = Mean_Abs_Bias, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 15, linetype = "dashed", colour = "red") +
  theme_bw() +
  labs(
    title = "QC Mean Absolute Bias: Four-Method Comparison",
    subtitle = "Red line = ±15% QC acceptance limit | Lower is better",
    x = "Dataset",
    y = "Mean Absolute % Bias",
    fill = "Drift Correction Method"
  ) +
  scale_fill_manual(values = pal_method) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Four_Method_Comparison.png"),
       p, width = 10, height = 6, dpi = 300)
cat("\nPlot saved: Four_Method_Comparison.png\n")

cat("\n========================================\n")
cat("Phase 5 Complete\n")
cat("========================================\n")
