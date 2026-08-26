# Phase 3: Power Law vs Polynomial - RATIO-BASED Drift Correction Validation
# Purpose: Validate power law on petn_ratio (matching actual pipeline configuration)
# Compare ratio-based polynomial vs ratio-based power law

library(ggplot2)
library(dplyr)

# Define datasets
base_path <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"
datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1")

# Known R² values from current polynomial drift correction (PETN ratio-based)
# These should be the actual R² values from ratio drift correction
current_r2 <- c(0.9492, 0.6724, 0.5712, 0.8537)

# Initialize results storage
results_summary <- data.frame(
  Dataset = character(),
  Current_Poly_R2 = numeric(),
  Power_Row_R2 = numeric(),
  Power_Row_a = numeric(),
  Power_Row_b = numeric(),
  Power_Pos_R2 = numeric(),
  Power_Pos_a = numeric(),
  Power_Pos_b = numeric(),
  Current_Mean_Abs_Bias = numeric(),
  Power_Row_Mean_Abs_Bias = numeric(),
  Power_Pos_Mean_Abs_Bias = numeric(),
  stringsAsFactors = FALSE
)

all_qc_data <- list()
detailed_results <- list()

# Function to apply power law drift correction to RATIO and re-quantify
apply_power_correction_ratio <- function(data, power_model, cal_model, use_row = TRUE) {
  
  # Predict ratio at each injection using power model
  if (use_row) {
    predicted_ratio <- predict(power_model, newdata = data.frame(Row = data$Row))
  } else {
    # For position-based, need to assign position to all rows
    qc_rows <- data$Row[data$Type == "QC" & data$CalLevel == 6 & !is.na(data$petn_ratio)]
    qc_positions <- seq_along(qc_rows)
    
    # Linear interpolation for all rows
    data$qc_position <- approx(x = qc_rows, y = qc_positions, 
                               xout = data$Row, rule = 2)$y
    predicted_ratio <- predict(power_model, newdata = data.frame(qc_position = data$qc_position))
  }
  
  # Calculate correction factors (normalize to first QC)
  first_qc_row <- min(data$Row[data$Type == "QC" & data$CalLevel == 6 & !is.na(data$petn_ratio)])
  if (use_row) {
    reference_ratio <- predict(power_model, newdata = data.frame(Row = first_qc_row))
  } else {
    reference_ratio <- predicted_ratio[data$Row == first_qc_row][1]
  }
  
  correction_factor <- reference_ratio / predicted_ratio
  
  # Only correct rows after first QC
  correction_factor[data$Row <= first_qc_row] <- 1.0
  
  # Apply correction to measured ratio
  data$petn_ratio_corrected <- data$petn_ratio * correction_factor
  
  # Re-quantify using calibration model (ratio-based)
  cal_coefs <- coef(cal_model)
  
  if (length(cal_coefs) == 3) {
    # Quadratic: a*x^2 + b*x + c = y
    a <- cal_coefs[3]  # x^2 coefficient
    b <- cal_coefs[2]  # x coefficient  
    c <- cal_coefs[1]  # intercept
    
    # Solve for x: ax^2 + bx + (c - y) = 0
    data$petn_conc_power <- sapply(data$petn_ratio_corrected, function(y) {
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
    # Linear: y = mx + c
    slope <- cal_coefs[2]
    intercept <- cal_coefs[1]
    data$petn_conc_power <- (data$petn_ratio_corrected - intercept) / slope
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
  
  if (!file.exists(results_file)) {
    cat("WARNING: File not found:", results_file, "\n")
    next
  }
  
  data <- read.csv(results_file, stringsAsFactors = FALSE)
  
  # Check if petn_ratio column exists
  if (!"petn_ratio" %in% names(data)) {
    cat("WARNING: petn_ratio column not found in", dataset_name, "\n")
    next
  }
  
  # Extract PETN 6ng QC ratio data
  qc_6ng <- data %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_ratio), petn_ratio > 0) %>%
    select(Row, petn_ratio, petn_concentration, petn_percent_bias, 
           petn_concentration_dc, petn_percent_bias_dc) %>%
    arrange(Row)
  
  if (nrow(qc_6ng) < 3) {
    cat("WARNING: Insufficient QC ratio data (n =", nrow(qc_6ng), ")\n")
    next
  }
  
  cat("Found", nrow(qc_6ng), "PETN 6ng QCs at rows:", paste(qc_6ng$Row, collapse=", "), "\n")
  
  # Add QC position (1, 2, 3, 4, 5)
  qc_6ng$qc_position <- 1:nrow(qc_6ng)
  
  # Store for later
  qc_6ng$Dataset <- dataset_name
  all_qc_data[[dataset_name]] <- qc_6ng
  
  # ====================================================
  # Approach A: Power curve with x = Injection Number (Row)
  # ====================================================
  cat("\nFitting power curve to RATIO: ratio ~ a * Row^b\n")
  
  start_a_row <- max(qc_6ng$petn_ratio)
  start_b_row <- -1.0
  
  power_model_row <- tryCatch({
    nls(petn_ratio ~ a * Row^b, 
        data = qc_6ng,
        start = list(a = start_a_row, b = start_b_row),
        control = nls.control(maxiter = 100))
  }, error = function(e) {
    cat("ERROR fitting power curve (Row):", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(power_model_row)) {
    coef_row <- coef(power_model_row)
    a_row <- coef_row["a"]
    b_row <- coef_row["b"]
    
    residuals_row <- residuals(power_model_row)
    ss_res_row <- sum(residuals_row^2)
    ss_tot_row <- sum((qc_6ng$petn_ratio - mean(qc_6ng$petn_ratio))^2)
    r2_row <- 1 - (ss_res_row / ss_tot_row)
    
    cat("  Coefficients: a =", format(a_row, scientific=FALSE), ", b =", round(b_row, 4), "\n")
    cat("  R² =", round(r2_row, 4), "\n")
    
    qc_6ng$predicted_ratio_row <- predict(power_model_row, newdata = qc_6ng)
    
  } else {
    r2_row <- NA
    a_row <- NA
    b_row <- NA
    qc_6ng$predicted_ratio_row <- NA
  }
  
  # ====================================================
  # Approach B: Power curve with x = QC Position (1-5)
  # ====================================================
  cat("\nFitting power curve to RATIO: ratio ~ a * position^b\n")
  
  start_a_pos <- max(qc_6ng$petn_ratio)
  start_b_pos <- -1.0
  
  power_model_pos <- tryCatch({
    nls(petn_ratio ~ a * qc_position^b, 
        data = qc_6ng,
        start = list(a = start_a_pos, b = start_b_pos),
        control = nls.control(maxiter = 100))
  }, error = function(e) {
    cat("ERROR fitting power curve (Position):", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(power_model_pos)) {
    coef_pos <- coef(power_model_pos)
    a_pos <- coef_pos["a"]
    b_pos <- coef_pos["b"]
    
    residuals_pos <- residuals(power_model_pos)
    ss_res_pos <- sum(residuals_pos^2)
    ss_tot_pos <- sum((qc_6ng$petn_ratio - mean(qc_6ng$petn_ratio))^2)
    r2_pos <- 1 - (ss_res_pos / ss_tot_pos)
    
    cat("  Coefficients: a =", format(a_pos, scientific=FALSE), ", b =", round(b_pos, 4), "\n")
    cat("  R² =", round(r2_pos, 4), "\n")
    
    qc_6ng$predicted_ratio_pos <- predict(power_model_pos, newdata = qc_6ng)
    
  } else {
    r2_pos <- NA
    a_pos <- NA
    b_pos <- NA
    qc_6ng$predicted_ratio_pos <- NA
  }
  
  # ====================================================
  # Extract calibration curve (RATIO-BASED)
  # ====================================================
  cal_data <- data %>%
    filter(Type == "Cal", !is.na(petn_ratio), petn_ratio > 0, 
           !is.na(CalLevel), CalLevel > 0) %>%
    select(Row, CalLevel, petn_ratio) %>%
    arrange(CalLevel)
  
  cat("\nCalibration data (RATIO): n =", nrow(cal_data), "standards\n")
  cat("  Levels:", paste(unique(cal_data$CalLevel), collapse = ", "), "ng\n")
  
  # Fit calibration curve (quadratic, 1/x weighted)
  cal_model <- lm(petn_ratio ~ poly(CalLevel, 2, raw = TRUE), 
                  data = cal_data, 
                  weights = 1/CalLevel)
  
  cal_r2 <- summary(cal_model)$r.squared
  cat("  Calibration R² =", round(cal_r2, 4), "\n")
  
  # ====================================================
  # Apply power law correction (Row method)
  # ====================================================
  if (!is.null(power_model_row)) {
    data_corrected_row <- apply_power_correction_ratio(data, power_model_row, cal_model, use_row = TRUE)
  } else {
    data_corrected_row <- data
    data_corrected_row$petn_conc_power <- NA
  }
  
  # Apply power law correction (Position method)
  if (!is.null(power_model_pos)) {
    data_corrected_pos <- apply_power_correction_ratio(data, power_model_pos, cal_model, use_row = FALSE)
  } else {
    data_corrected_pos <- data
    data_corrected_pos$petn_conc_power <- NA
  }
  
  # Extract QC results
  qc_results_row <- data_corrected_row %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_ratio)) %>%
    mutate(
      petn_percent_bias_power = ((petn_conc_power - 6) / 6) * 100
    ) %>%
    select(Row, petn_concentration, petn_percent_bias, 
           petn_concentration_dc, petn_percent_bias_dc,
           petn_conc_power, petn_percent_bias_power)
  
  qc_results_pos <- data_corrected_pos %>%
    filter(Type == "QC", CalLevel == 6, !is.na(petn_ratio)) %>%
    mutate(
      petn_percent_bias_power_pos = ((petn_conc_power - 6) / 6) * 100
    ) %>%
    select(Row, petn_conc_power_pos = petn_conc_power, petn_percent_bias_power_pos)
  
  # Combine
  qc_results <- qc_results_row %>%
    left_join(qc_results_pos, by = "Row")
  
  # Calculate summary statistics
  cat("\n--- QC Performance Comparison (6ng level, RATIO-BASED) ---\n")
  
  # Current polynomial drift-corrected (ratio-based)
  current_mean_bias <- mean(abs(qc_results$petn_percent_bias_dc), na.rm = TRUE)
  current_pass_rate <- mean(abs(qc_results$petn_percent_bias_dc) <= 15, na.rm = TRUE) * 100
  
  # Power law (Row)
  power_row_mean_bias <- mean(abs(qc_results$petn_percent_bias_power), na.rm = TRUE)
  power_row_pass_rate <- mean(abs(qc_results$petn_percent_bias_power) <= 15, na.rm = TRUE) * 100
  
  # Power law (Position)
  power_pos_mean_bias <- mean(abs(qc_results$petn_percent_bias_power_pos), na.rm = TRUE)
  power_pos_pass_rate <- mean(abs(qc_results$petn_percent_bias_power_pos) <= 15, na.rm = TRUE) * 100
  
  cat("\nCurrent Polynomial (ratio drift-corrected):\n")
  cat("  Mean |%Bias|:", round(current_mean_bias, 2), "%\n")
  cat("  QC Pass Rate:", round(current_pass_rate, 1), "%\n")
  
  cat("\nPower Law on Ratio (Row):\n")
  cat("  Mean |%Bias|:", round(power_row_mean_bias, 2), "%\n")
  cat("  QC Pass Rate:", round(power_row_pass_rate, 1), "%\n")
  cat("  Improvement:", round(current_mean_bias - power_row_mean_bias, 2), "% points\n")
  
  cat("\nPower Law on Ratio (Position):\n")
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
  results_summary <- rbind(results_summary, data.frame(
    Dataset = dataset_name,
    Current_Poly_R2 = current_r2[i],
    Power_Row_R2 = r2_row,
    Power_Row_a = a_row,
    Power_Row_b = b_row,
    Power_Pos_R2 = r2_pos,
    Power_Pos_a = a_pos,
    Power_Pos_b = b_pos,
    Current_Mean_Abs_Bias = current_mean_bias,
    Power_Row_Mean_Abs_Bias = power_row_mean_bias,
    Power_Pos_Mean_Abs_Bias = power_pos_mean_bias,
    stringsAsFactors = FALSE
  ))
  
  # Update stored QC data
  all_qc_data[[dataset_name]] <- qc_6ng
}

# Save results
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX"
write.csv(results_summary, file.path(output_dir, "PowerCurve_Ratio_Comparison_Summary.csv"), 
          row.names = FALSE)

cat("\n========================================\n")
cat("FINAL SUMMARY: R² Comparison (RATIO-BASED)\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Current_Poly_R2", "Power_Row_R2", "Power_Pos_R2")])

cat("\n========================================\n")
cat("FINAL SUMMARY: Bias Comparison (RATIO-BASED)\n")
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
# Plot 1: Raw Ratio trends with power curve fits (x = Row)
all_qc_combined <- do.call(rbind, all_qc_data)

if (any(!is.na(all_qc_combined$predicted_ratio_row))) {
  p1 <- ggplot(all_qc_combined, aes(x = Row, y = petn_ratio)) +
    geom_point(size = 3, colour = "blue") +
    geom_line(aes(y = predicted_ratio_row), colour = "red", linewidth = 1) +
    facet_wrap(~Dataset, scales = "free") +
    theme_bw() +
    labs(
      title = "Power Curve Fit to RATIO: ratio ~ a * Row^b",
      subtitle = "Blue points = Measured ratio | Red line = Power curve fit",
      x = "Injection Number (Row)",
      y = "PETN Ratio (petn_pa / rdx_is_pa)"
    )
  
  ggsave(file.path(output_dir, "PowerCurve_Ratio_Fit_by_Row.png"),
         p1, width = 10, height = 8, dpi = 300)
  cat("\nPlot saved: PowerCurve_Ratio_Fit_by_Row.png\n")
}

# Plot 2: R² comparison
r2_comparison <- results_summary %>%
  select(Dataset, Current_Poly_R2, Power_Row_R2, Power_Pos_R2) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "R2") %>%
  mutate(Method = factor(Method, 
                        levels = c("Current_Poly_R2", "Power_Row_R2", "Power_Pos_R2"),
                        labels = c("Current Polynomial", "Power (Row)", "Power (Position)")))

p2 <- ggplot(r2_comparison, aes(x = Dataset, y = R2, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "darkgreen") +
  theme_bw() +
  labs(
    title = "Drift Correction Model Fit Quality: R² Comparison (RATIO-BASED)",
    subtitle = "Green line = R² = 0.95 (excellent fit threshold)",
    x = "Dataset",
    y = "R² (Coefficient of Determination)",
    fill = "Method"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "R2_Ratio_Comparison_BarPlot.png"),
       p2, width = 10, height = 6, dpi = 300)
cat("Plot saved: R2_Ratio_Comparison_BarPlot.png\n")

# Plot 3: Mean Absolute Bias Comparison
bias_comparison <- results_summary %>%
  select(Dataset, Current_Mean_Abs_Bias, Power_Row_Mean_Abs_Bias, Power_Pos_Mean_Abs_Bias) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "Mean_Abs_Bias") %>%
  mutate(Method = factor(Method,
                        levels = c("Current_Mean_Abs_Bias", "Power_Row_Mean_Abs_Bias", "Power_Pos_Mean_Abs_Bias"),
                        labels = c("Current Polynomial", "Power (Row)", "Power (Position)")))

p3 <- ggplot(bias_comparison, aes(x = Dataset, y = Mean_Abs_Bias, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 15, linetype = "dashed", colour = "red") +
  theme_bw() +
  labs(
    title = "QC Mean Absolute Bias: Polynomial vs Power Law (RATIO-BASED)",
    subtitle = "Red line = ±15% QC acceptance limit | Lower is better",
    x = "Dataset",
    y = "Mean Absolute % Bias",
    fill = "Drift Correction Method"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Bias_Ratio_Comparison_BarPlot.png"),
       p3, width = 10, height = 6, dpi = 300)
cat("Plot saved: Bias_Ratio_Comparison_BarPlot.png\n")

# Plot 4: Individual QC Bias Trajectories (per dataset)
for (dataset_name in datasets) {
  if (!(dataset_name %in% names(detailed_results))) next
  
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
      title = paste(dataset_name, "- QC Bias Trajectories (RATIO-BASED)"),
      subtitle = "Red lines = ±15% acceptance limits",
      x = "Injection Number (Row)",
      y = "% Bias vs True (6ng)",
      color = "Drift Correction",
      shape = "Drift Correction"
    ) +
    scale_y_continuous(breaks = seq(-60, 60, 10)) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0(dataset_name, "_QC_Bias_Ratio_Trajectories.png")),
         p_traj, width = 10, height = 6, dpi = 300)
}

cat("\nQC bias trajectory plots saved for all datasets.\n")

# Summary recommendation
cat("\n========================================\n")
cat("RECOMMENDATION (RATIO-BASED VALIDATION)\n")
cat("========================================\n")

avg_improvement_row <- mean(results_summary$Row_Improvement, na.rm = TRUE)
avg_improvement_pos <- mean(results_summary$Pos_Improvement, na.rm = TRUE)

if (avg_improvement_row > 0 && avg_improvement_pos > 0) {
  cat("✅ IMPLEMENT POWER LAW DRIFT CORRECTION (RATIO-BASED)\n\n")
  cat("Evidence:\n")
  cat("  - R² improves for ratio-based drift correction\n")
  cat("  - Mean |%Bias| reduces by", round(avg_improvement_row, 2), "% points (Row method)\n")
  cat("  - Mean |%Bias| reduces by", round(avg_improvement_pos, 2), "% points (Position method)\n")
  
  if (avg_improvement_row > avg_improvement_pos) {
    cat("\nRecommended approach: Power law with x = Injection Number (Row)\n")
  } else {
    cat("\nRecommended approach: Power law with x = QC Position\n")
  }
} else if (avg_improvement_row > 0 || avg_improvement_pos > 0) {
  cat("⚠️ MIXED RESULTS: Power law improves some datasets but not all\n")
  cat("Average improvement (Row):", round(avg_improvement_row, 2), "% points\n")
  cat("Average improvement (Pos):", round(avg_improvement_pos, 2), "% points\n")
  cat("\nConsider hybrid approach or dataset-specific selection.\n")
} else {
  cat("❌ Power law does NOT improve ratio-based drift correction\n")
  cat("Keep current polynomial method.\n")
}

cat("\n========================================\n")
cat("Phase 3 Complete: Ratio-Based Validation\n")
cat("========================================\n")
