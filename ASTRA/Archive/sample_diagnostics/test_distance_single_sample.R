# Test Distance-Based Analysis on Single Sample
# Quick test to validate distance calculation

# Set sample to test
TEST_SAMPLE <- "PILOT_001"

cat("\n========================================\n")
cat("DISTANCE CALCULATION TEST\n")
cat(sprintf("Sample: %s\n", TEST_SAMPLE))
cat("========================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(writexl)
})

# Source configuration and functions from main script
# (This loads all functions but doesn't run main_batch due to sys.nframe check)
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/time_based_batch_process_adaptive_DISTANCE.R")

# Check if sample exists in configuration
if (!(TEST_SAMPLE %in% names(SAMPLE_CONFIG))) {
  cat(sprintf("ERROR: Sample '%s' not found in SAMPLE_CONFIG\n", TEST_SAMPLE))
  cat("Available samples:\n")
  print(names(SAMPLE_CONFIG))
  stop("Sample not found")
}

# Get configuration for test sample
config <- SAMPLE_CONFIG[[TEST_SAMPLE]]

cat(sprintf("Target pressure: %dg\n", config$target))
cat(sprintf("Stable range: %.1f - %.1fg\n\n", config$stable_low, config$stable_high))

# Process the sample
result <- process_sample(TEST_SAMPLE, config, PARAMS)

# Display results
if (!is.null(result) && result$success) {
  cat("\n========================================\n")
  cat("TEST SUCCESSFUL!\n")
  cat("========================================\n\n")
  
  cat("TIME-BASED METRICS:\n")
  cat(sprintf("  Cycles detected: %d / %d\n", result$stats$n_cycles_detected, result$stats$n_cycles_total))
  cat(sprintf("  Stable time: %.2f%%\n", result$stats$pct_stable))
  cat(sprintf("  Total time: %.1f s\n", result$stats$total_pass_time))
  cat(sprintf("  Stable duration: %.1f s\n\n", result$stats$total_stable_time))
  
  cat("DISTANCE-BASED METRICS:\n")
  if (!is.na(result$stats$total_distance_mm)) {
    cat(sprintf("  Total swabbing distance: %.1f mm\n", result$stats$total_distance_mm))
    cat(sprintf("  Theoretical maximum: %.0f mm\n", result$stats$theoretical_max_distance))
    cat(sprintf("  Expected from cycles: %.0f mm\n", result$stats$expected_distance_from_cycles))
    cat(sprintf("  Distance efficiency (actual): %.1f%%\n", result$stats$distance_efficiency_actual))
    cat(sprintf("  Mean distance per cycle: %.1f mm\n", result$stats$mean_distance_per_cycle))
    cat(sprintf("  Stable distance: %.1f%%\n\n", result$stats$pct_stable_distance))
  } else {
    cat("  No distance data calculated\n\n")
  }
  
  cat("PRESSURE STATISTICS:\n")
  cat(sprintf("  Mean pressure: %.1f g\n", result$stats$mean_pressure_stable))
  cat(sprintf("  RSD: %.2f%%\n\n", result$stats$rsd_pressure_stable))
  
  # Check output files
  output_dir <- file.path(PARAMS$output_dir, TEST_SAMPLE)
  cat("OUTPUT FILES:\n")
  
  files_to_check <- c(
    paste0(TEST_SAMPLE, "_full_trace_time.png"),
    paste0(TEST_SAMPLE, "_full_trace_distance.png"),
    paste0(TEST_SAMPLE, "_zoomed_distance.png"),
    paste0(TEST_SAMPLE, "_detected_cycles.csv"),
    paste0(TEST_SAMPLE, "_summary_statistics.txt")
  )
  
  for (fname in files_to_check) {
    fpath <- file.path(output_dir, fname)
    exists_msg <- if (file.exists(fpath)) "✓" else "✗"
    cat(sprintf("  %s %s\n", exists_msg, fname))
  }
  
  cat(sprintf("\nOutput directory: %s\n", output_dir))
  
} else {
  cat("\n========================================\n")
  cat("TEST FAILED!\n")
  cat("========================================\n")
  if (!is.null(result)) {
    cat("Result returned but success=FALSE\n")
  } else {
    cat("Result is NULL\n")
  }
}
