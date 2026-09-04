# Power Curve vs Polynomial Drift Correction Validation
# Purpose: Compare power law fit (y = a * x^b) vs current quadratic polynomial
# Test whether power curve produces less biased QC results

library(ggplot2)
library(dplyr)

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# Define datasets
base_path <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1")

# Known R² values from current polynomial drift correction (PETN 6ng QCs)
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

# Loop through each dataset
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
    filter(Type == "QC", CalLevel == 6, !is.na(petn_pa), petn_pa > 0) %>%
    select(Row, petn_pa, petn_concentration, petn_percent_bias, 
           petn_concentration_dc, petn_percent_bias_dc) %>%
    arrange(Row)
  
  if (nrow(qc_6ng) < 3) {
    cat("WARNING: Insufficient QC data (n =", nrow(qc_6ng), ")\n")
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
  cat("\nFitting power curve: PA ~ a * Row^b\n")
  
  # Starting values for nls
  start_a_row <- max(qc_6ng$petn_pa)
  start_b_row <- -1.0
  
  power_model_row <- tryCatch({
    nls(petn_pa ~ a * Row^b, 
        data = qc_6ng,
        start = list(a = start_a_row, b = start_b_row),
        control = nls.control(maxiter = 100))
  }, error = function(e) {
    cat("ERROR fitting power curve (Row):", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(power_model_row)) {
    # Extract coefficients
    coef_row <- coef(power_model_row)
    a_row <- coef_row["a"]
    b_row <- coef_row["b"]
    
    # Calculate R²
    residuals_row <- residuals(power_model_row)
    ss_res_row <- sum(residuals_row^2)
    ss_tot_row <- sum((qc_6ng$petn_pa - mean(qc_6ng$petn_pa))^2)
    r2_row <- 1 - (ss_res_row / ss_tot_row)
    
    cat("  Coefficients: a =", format(a_row, scientific=FALSE), ", b =", round(b_row, 4), "\n")
    cat("  R² =", round(r2_row, 4), "\n")
    
    # Predict PA at each QC
    qc_6ng$predicted_pa_row <- predict(power_model_row, newdata = qc_6ng)
    
  } else {
    r2_row <- NA
    a_row <- NA
    b_row <- NA
    qc_6ng$predicted_pa_row <- NA
  }
  
  # ====================================================
  # Approach B: Power curve with x = QC Position (1-5)
  # ====================================================
  cat("\nFitting power curve: PA ~ a * position^b\n")
  
  start_a_pos <- max(qc_6ng$petn_pa)
  start_b_pos <- -1.0
  
  power_model_pos <- tryCatch({
    nls(petn_pa ~ a * qc_position^b, 
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
    ss_tot_pos <- sum((qc_6ng$petn_pa - mean(qc_6ng$petn_pa))^2)
    r2_pos <- 1 - (ss_res_pos / ss_tot_pos)
    
    cat("  Coefficients: a =", format(a_pos, scientific=FALSE), ", b =", round(b_pos, 4), "\n")
    cat("  R² =", round(r2_pos, 4), "\n")
    
    qc_6ng$predicted_pa_pos <- predict(power_model_pos, newdata = qc_6ng)
    
  } else {
    r2_pos <- NA
    a_pos <- NA
    b_pos <- NA
    qc_6ng$predicted_pa_pos <- NA
  }
  
  # ====================================================
  # Calculate mean absolute bias for comparison
  # ====================================================
  
  # Current method (already in data as petn_percent_bias_dc)
  current_abs_bias <- mean(abs(qc_6ng$petn_percent_bias_dc), na.rm = TRUE)
  cat("\nCurrent method mean absolute bias:", round(current_abs_bias, 2), "%\n")
  
  # For power law methods, we need to:
  # 1. Calculate correction factors from predicted PAs
  # 2. Apply corrections to measured PAs
  # 3. Re-quantify using calibration curve
  # 4. Calculate new %Bias
  
  # This requires the calibration curve, which we'll extract next
  
  # Store results
  results_summary <- rbind(results_summary, data.frame(
    Dataset = dataset_name,
    Current_Poly_R2 = current_r2[i],
    Power_Row_R2 = r2_row,
    Power_Row_a = a_row,
    Power_Row_b = b_row,
    Power_Pos_R2 = r2_pos,
    Power_Pos_a = a_pos,
    Power_Pos_b = b_pos,
    Current_Mean_Abs_Bias = current_abs_bias,
    Power_Row_Mean_Abs_Bias = NA,  # Will calculate below
    Power_Pos_Mean_Abs_Bias = NA,
    stringsAsFactors = FALSE
  ))
  
  # Update stored QC data
  all_qc_data[[dataset_name]] <- qc_6ng
}

cat("\n========================================\n")
cat("SUMMARY: R² Comparison\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Current_Poly_R2", "Power_Row_R2", "Power_Pos_R2")])

cat("\n========================================\n")
cat("SUMMARY: Power Law Exponents\n")
cat("========================================\n")
print(results_summary[, c("Dataset", "Power_Row_b", "Power_Pos_b")])

# Save results
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/FINEX"
write.csv(results_summary, file.path(output_dir, "PowerCurve_Comparison_Summary.csv"), 
          row.names = FALSE)

cat("\nResults saved to:", file.path(output_dir, "PowerCurve_Comparison_Summary.csv"), "\n")

# ====================================================
# Generate comparison plots
# ====================================================

# Combine all QC data
all_qc_combined <- do.call(rbind, all_qc_data)

# Plot 1: Raw PA trends with power curve fits (x = Row)
if (any(!is.na(all_qc_combined$predicted_pa_row))) {
  p1 <- ggplot(all_qc_combined, aes(x = Row, y = petn_pa)) +
    geom_point(size = 3, colour = "blue") +
    geom_line(aes(y = predicted_pa_row), colour = "red", linewidth = 1) +
    facet_wrap(~Dataset, scales = "free") +
    theme_bw() +
    labs(
      title = "Power Curve Fit: PA ~ a * Row^b",
      subtitle = "Blue points = Measured PA | Red line = Power curve fit",
      x = "Injection Number (Row)",
      y = "PETN Peak Area (counts)"
    )
  
  ggsave(file.path(output_dir, "PowerCurve_Fit_by_Row.png"),
         p1, width = 10, height = 8, dpi = 300)
  cat("\nPlot saved: PowerCurve_Fit_by_Row.png\n")
}

# Plot 2: Raw PA trends with power curve fits (x = Position)
if (any(!is.na(all_qc_combined$predicted_pa_pos))) {
  p2 <- ggplot(all_qc_combined, aes(x = qc_position, y = petn_pa)) +
    geom_point(size = 3, colour = "blue") +
    geom_line(aes(y = predicted_pa_pos), colour = "red", linewidth = 1) +
    facet_wrap(~Dataset, scales = "free_y") +
    theme_bw() +
    labs(
      title = "Power Curve Fit: PA ~ a * Position^b",
      subtitle = "Blue points = Measured PA | Red line = Power curve fit",
      x = "QC Position (1-5)",
      y = "PETN Peak Area (counts)"
    )
  
  ggsave(file.path(output_dir, "PowerCurve_Fit_by_Position.png"),
         p2, width = 10, height = 8, dpi = 300)
  cat("Plot saved: PowerCurve_Fit_by_Position.png\n")
}

# Plot 3: R² comparison
r2_comparison <- results_summary %>%
  select(Dataset, Current_Poly_R2, Power_Row_R2, Power_Pos_R2) %>%
  tidyr::pivot_longer(cols = -Dataset, names_to = "Method", values_to = "R2") %>%
  mutate(Method = factor(Method, 
                        levels = c("Current_Poly_R2", "Power_Row_R2", "Power_Pos_R2"),
                        labels = c("Current Polynomial", "Power (Row)", "Power (Position)")))

p3 <- ggplot(r2_comparison, aes(x = Dataset, y = R2, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "darkgreen") +
  scale_fill_manual(values = pal_method) +
  theme_bw() +
  labs(
    title = "Drift Correction Model Fit Quality: R² Comparison",
    subtitle = "Green line = R² = 0.95 (excellent fit threshold)",
    x = "Dataset",
    y = "R² (Coefficient of Determination)",
    fill = "Method"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "R2_Comparison_BarPlot.png"),
       p3, width = 10, height = 6, dpi = 300)
cat("Plot saved: R2_Comparison_BarPlot.png\n")

cat("\n========================================\n")
cat("Phase 1 Complete: Power Curve Fitting\n")
cat("========================================\n")
cat("\nNext step: Calculate corrected concentrations and %Bias\n")
cat("This requires extracting calibration curves from each dataset.\n")
