# Test Distance-Based Analysis on PILOT_001 Only
# Quick test script to validate distance calculation before full batch

# Source the main script functions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/time_based_batch_process_adaptive_DISTANCE.R")

# Override SAMPLE_CONFIG to only include PILOT_001
SAMPLE_CONFIG <- list(
  "PILOT_001" = SAMPLE_CONFIG[["PILOT_001"]]
)

cat("\n========================================\n")
cat("TESTING DISTANCE CALCULATION\n")
cat("Sample: PILOT_001 only\n")
cat("========================================\n\n")

# Run processing
result <- process_sample("PILOT_001", SAMPLE_CONFIG[["PILOT_001"]], PARAMS)

if (!is.null(result) && result$success) {
  cat("\n========================================\n")
  cat("TEST SUCCESSFUL!\n")
  cat("========================================\n\n")
  
  cat("RESULTS SUMMARY:\n")
  cat(sprintf("  Cycles detected: %d\n", result$stats$n_cycles_detected))
  cat(sprintf("  Stable time %%: %.2f%%\n", result$stats$pct_stable))
  cat(sprintf("  Total distance: %.1f mm\n", result$stats$total_distance_mm))
  cat(sprintf("  Stable distance %%: %.2f%%\n", result$stats$pct_stable_distance))
  cat(sprintf("  Distance efficiency: %.1f%%\n", result$stats$distance_efficiency_actual))
  cat(sprintf("  Mean pressure: %.1f g\n", result$stats$mean_pressure_stable))
  
  cat("\nOUTPUT FILES:\n")
  cat(sprintf("  %s/PILOT_001_full_trace_time.png\n", PARAMS$output_dir))
  cat(sprintf("  %s/PILOT_001_full_trace_distance.png\n", PARAMS$output_dir))
  cat(sprintf("  %s/PILOT_001_zoomed_distance.png\n", PARAMS$output_dir))
  cat(sprintf("  %s/PILOT_001_summary_statistics.txt\n", PARAMS$output_dir))
  
} else {
  cat("\n========================================\n")
  cat("TEST FAILED!\n")
  cat("========================================\n")
}
