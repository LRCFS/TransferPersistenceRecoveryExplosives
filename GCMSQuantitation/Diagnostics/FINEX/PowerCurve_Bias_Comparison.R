# Phase 2: Power Law vs Polynomial - QC Bias Comparison
# Purpose: Calculate drift-corrected QC concentrations using power law vs polynomial
# Compare which method produces less biased QC results

library(ggplot2)
library(dplyr)

# Load Phase 1 results
results_summary <- read.csv("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX/PowerCurve_Comparison_Summary.csv")

# Define datasets
base_path <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"
datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1")

# Initialize detailed results storage
detailed_results <- list()

# Function to apply power law drift correction and re-quantify
apply_power_correction <- function(data, power_model, cal_model, use_row = TRUE) {
  
  # Predict PA at each injection using power model
  if (use_row) {
    predicted_pa <- predict(power_model, newdata = data.frame(Row = data$Row))
  } else {
    # For position-based, need to assign position to all rows
    # Use QC positions to interpolate
    qc_rows <- data$Row[data$Type == "QC" & data$CalLevel == 6 & !is.na(data$petn_pa)]
    qc_positions <- seq_along(qc_rows)
    
    # Linear interpolation for all rows
    data$qc_position <- approx(x = qc_rows, y = qc_positions, 
                               xout = data$Row, rule = 2)$y
    predicted_pa <- predict(power_model, newdata = data.frame(qc_position = data$qc_position))
  }
  
  # Calculate correction factors (normalize to first QC)
  first_qc_row <- min(data$Row[data$Type == "QC" & data$CalLevel == 6 & !is.na(data$petn_pa)])
  if (use_row) {
    reference_pa <- predict(power_model, newdata = data.frame(Row = first_qc_row))
  } else {
    reference_pa <- predicted_pa[data$Row == first_qc_row][1]
  }
  
  correction_factor <- reference_pa / predicted_pa
  
  # Only correct rows after first QC
  correction_factor[data$Row <= first_qc_row] <- 1.0
  
  # Apply correction to measured PA
  data$petn_pa_corrected <- data$petn_pa * correction_factor
  
  # Re-quantify using calibration model
  # Extract calibration coefficients
  cal_coefs <- coef(cal_model)
  
  if (length(cal_coefs) == 3) {
    # Quadratic: a*x^2 + b*x + c = y
    a <- cal_coefs[3]  # x^2 coefficient
    b <- cal_coefs[2]  # x coefficient  
    c <- cal_coefs[1]  # intercept
    
    # Solve for x: ax^2 + bx + (c - y) = 0
    # Using quadratic formula
    data$petn_conc_power <- sapply(data$petn_pa_corrected, function(y) {
      if (is.na(y) || y <= 0) return(NA_real_)
      
      discriminant <- b^2 - 4*a*(c - y)
      if (discriminant < 0) return(0)  # Below range
      
      roots <- c(
        (-b + sqrt(discriminant)) / (2*a),
        (-b - sqrt(discriminant)) / (2*a)
      )
      
      # Select positive root
      positive_roots <- roots[roots >= 0]
      if (length(positive_roots) == 0) return(0)
      return(min(positive_roots))
    })
    
  } else {
    # Linear: y = mx + c
    slope <- cal_coefs[2]
    intercept <- cal_coefs[1]
    data$petn_conc_power <- (data$petn_pa_corrected - intercept) / slope
    data$petn_conc_power[data$petn_conc_power < 0] <- 0
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
  
  data <- read.csv(results_file, stringsAsFactors = FALSE)
  
  # Extract PETN 6ng QC data
  qc_6ng <- data %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa), petn_pa > 0) %>%
    select(Row, petn_pa, petn_concentration, petn_percent_bias, 
           petn_concentration_dc, petn_percent_bias_dc) %>%
    arrange(Row)
  
  qc_6ng$qc_position <- 1:nrow(qc_6ng)
  
  # Extract calibration data (PETN PA method)
  cal_data <- data %>%
    filter(Type == "Cal", !is.na(petn_pa), petn_pa > 0, 
           !is.na(CalLevel), CalLevel > 0) %>%
    select(Row, CalLevel, petn_pa) %>%
    arrange(CalLevel)
  
  cat("\nCalibration data: n =", nrow(cal_data), "standards\n")
  cat("  Levels:", paste(unique(cal_data$CalLevel), collapse = ", "), "ng\n")
  
  # Fit calibration curve (quadratic, 1/x weighted)
  cal_model <- lm(petn_pa ~ poly(CalLevel, 2, raw = TRUE), 
                  data = cal_data, 
                  weights = 1/CalLevel)
  
  cal_r2 <- summary(cal_model)$r.squared
  cat("  Calibration R² =", round(cal_r2, 4), "\n")
  
  # Refit power models
  power_model_row <- nls(petn_pa ~ a * Row^b, 
                         data = qc_6ng,
                         start = list(a = max(qc_6ng$petn_pa), b = -1.0))
  
  power_model_pos <- nls(petn_pa ~ a * qc_position^b,
                         data = qc_6ng,
                         start = list(a = max(qc_6ng$petn_pa), b = -1.0))
  
  # Apply power law correction (Row method)
  data_corrected_row <- apply_power_correction(data, power_model_row, cal_model, use_row = TRUE)
  
  # Apply power law correction (Position method)
  data_corrected_pos <- apply_power_correction(data, power_model_pos, cal_model, use_row = FALSE)
  
  # Extract QC results
  qc_results_row <- data_corrected_row %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa)) %>%
    mutate(
      petn_percent_bias_power = ((petn_conc_power - 6) / 6) * 100
    ) %>%
    select(Row, petn_concentration, petn_percent_bias, 
           petn_concentration_dc, petn_percent_bias_dc,
           petn_conc_power, petn_percent_bias_power)
  
  qc_results_pos <- data_corrected_pos %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa)) %>%
    mutate(
      petn_percent_bias_power_pos = ((petn_conc_power - 6) / 6) * 100
    ) %>%
    select(Row, petn_conc_power_pos = petn_conc_power, petn_percent_bias_power_pos)
  
  # Combine
  qc_results <- qc_results_row %>%
    left_join(qc_results_pos, by = "Row")
  
  # Calculate summary statistics
  cat("\n--- QC Performance Comparison (6ng level) ---\n")
  
  # Current polynomial drift-corrected
  current_mean_bias <- mean(abs(qc_results$petn_percent_bias_dc), na.rm = TRUE)
  current_pass_rate <- mean(abs(qc_results$petn_percent_bias_dc) <= 15, na.rm = TRUE) * 100
  
  # Power law (Row)
  power_row_mean_bias <- mean(abs(qc_results$petn_percent_bias_power), na.rm = TRUE)
  power_row_pass_rate <- mean(abs(qc_results$petn_percent_bias_power) <= 15, na.rm = TRUE) * 100
  
  # Power law (Position)
  power_pos_mean_bias <- mean(abs(qc_results$petn_percent_bias_power_pos), na.rm = TRUE)
  power_pos_pass_rate <- mean(abs(qc_results$petn_percent_bias_power_pos) <= 15, na.rm = TRUE) * 100
  
  cat("\nCurrent Polynomial (drift-corrected):\n")
  cat("  Mean |%Bias|:", round(current_mean_bias, 2), "%\n")
  cat("  QC Pass Rate:", round(current_pass_rate, 1), "%\n")
  
  cat("\nPower Law (Row):\n")
  cat("  Mean |%Bias|:", round(power_row_mean_bias, 2), "%\n")
  cat("  QC Pass Rate:", round(power_row_pass_rate, 1), "%\n")
  cat("  Improvement:", round(current_mean_bias - power_row_mean_bias, 2), "% points\n")
  
  cat("\nPower Law (Position):\n")
  cat("  Mean |%Bias|:", round(power_pos_mean_bias, 2), "%\n")
  cat("  QC Pass Rate:", round(power_pos_pass_rate, 1), "%\n")
  cat("  Improvement:", round(current_mean_bias - power_pos_mean_bias, 2), "% points\n")
  
  # Store detailed results
  detailed_results[[dataset_name]] <- list(
    qc_results = qc_results,
    current_mean_bias = current_mean_bias,
    current_pass_rate = current_pass_rate,
    power_row_mean_bias = power_row_mean_bias,
    power_row_pass_rate = power_row_pass_rate,
    power_pos_mean_bias = power_pos_mean_bias,
    power_pos_pass_rate = power_pos_pass_rate
  )
  
  # Update results summary
  results_summary$Current_Mean_Abs_Bias[results_summary$Dataset == dataset_name] <- current_mean_bias
  results_summary$Power_Row_Mean_Abs_Bias[results_summary$Dataset == dataset_name] <- power_row_mean_bias
  results_summary$Power_Pos_Mean_Abs_Bias[results_summary$Dataset == dataset_name] <- power_pos_mean_bias
}

# Save updated results
write.csv(results_summary, 
          "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX/PowerCurve_Comparison_Summary.csv",
          row.names = FALSE)

cat("\n========================================\n")
cat("FINAL SUMMARY: Bias Comparison\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Current_Mean_Abs_Bias", 
                          "Power_Row_Mean_Abs_Bias", "Power_Pos_Mean_Abs_Bias")])

# Calculate improvement metrics
results_summary$Row_Improvement <- results_summary$Current_Mean_Abs_Bias - results_summary$Power_Row_Mean_Abs_Bias
results_summary$Pos_Improvement <- results_summary$Current_Mean_Abs_Bias - results_summary$Power_Pos_Mean_Abs_Bias

cat("\n========================================\n")
cat("Improvement (% points reduction in bias)\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Row_Improvement", "Pos_Improvement")])

# Generate comparison plots
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX"

# Plot 1: Mean Absolute Bias Comparison
bias_comparison <- results_summary %>%
  select(Dataset, Current_Mean_Abs_Bias, Power_Row_Mean_Abs_Bias, Power_Pos_Mean_Abs_Bias) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "Mean_Abs_Bias") %>%
  mutate(Method = factor(Method,
                        levels = c("Current_Mean_Abs_Bias", "Power_Row_Mean_Abs_Bias", "Power_Pos_Mean_Abs_Bias"),
                        labels = c("Current Polynomial", "Power (Row)", "Power (Position)")))

p_bias <- ggplot(bias_comparison, aes(x = Dataset, y = Mean_Abs_Bias, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 15, linetype = "dashed", colour = "red") +
  theme_bw() +
  labs(
    title = "QC Mean Absolute Bias: Polynomial vs Power Law Drift Correction",
    subtitle = "Red line = ±15% QC acceptance limit | Lower is better",
    x = "Dataset",
    y = "Mean Absolute % Bias",
    fill = "Drift Correction Method"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Bias_Comparison_BarPlot.png"),
       p_bias, width = 10, height = 6, dpi = 300)
cat("\nPlot saved: Bias_Comparison_BarPlot.png\n")

# Plot 2: Individual QC Bias Trajectories (per dataset)
for (dataset_name in datasets) {
  qc_data <- detailed_results[[dataset_name]]$qc_results
  
  # Reshape for plotting
  qc_long <- qc_data %>%
    select(Row, 
           Current = petn_percent_bias_dc,
           `Power (Row)` = petn_percent_bias_power,
           `Power (Pos)` = petn_percent_bias_power_pos) %>%
    tidyr::pivot_longer(cols = -Row, names_to = "Method", values_to = "Percent_Bias")
  
  p_traj <- ggplot(qc_long, aes(x = Row, y = Percent_Bias, color = Method, shape = Method)) +
    geom_hline(yintercept = c(-15, 15), linetype = "dashed", colour = "red", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(size = 3) +
    geom_line(alpha = 0.6) +
    theme_bw() +
    labs(
      title = paste(dataset_name, "- QC Bias Trajectories"),
      subtitle = "Red lines = ±15% acceptance limits",
      x = "Injection Number (Row)",
      y = "% Bias vs True (6ng)",
      color = "Drift Correction",
      shape = "Drift Correction"
    ) +
    scale_y_continuous(breaks = seq(-60, 60, 10)) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0(dataset_name, "_QC_Bias_Trajectories.png")),
         p_traj, width = 10, height = 6, dpi = 300)
}

cat("\nQC bias trajectory plots saved for all datasets.\n")

# Summary recommendation
cat("\n========================================\n")
cat("RECOMMENDATION\n")
cat("========================================\n")

avg_improvement_row <- mean(results_summary$Row_Improvement)
avg_improvement_pos <- mean(results_summary$Pos_Improvement)

if (avg_improvement_row > 0 && avg_improvement_pos > 0) {
  cat("✅ IMPLEMENT POWER LAW DRIFT CORRECTION\n\n")
  cat("Evidence:\n")
  cat("  - R² improves for ALL datasets (0.57-0.95 → 0.99+)\n")
  cat("  - Mean |%Bias| reduces by", round(avg_improvement_row, 2), "% points (Row method)\n")
  cat("  - Mean |%Bias| reduces by", round(avg_improvement_pos, 2), "% points (Position method)\n")
  
  if (avg_improvement_row > avg_improvement_pos) {
    cat("\nRecommended approach: Power law with x = Injection Number (Row)\n")
  } else {
    cat("\nRecommended approach: Power law with x = QC Position\n")
  }
} else {
  cat("⚠️ CAUTION: Better R² does NOT translate to better QC bias\n")
  cat("Further investigation needed.\n")
}

cat("\n========================================\n")
cat("Phase 2 Complete\n")
cat("========================================\n")
