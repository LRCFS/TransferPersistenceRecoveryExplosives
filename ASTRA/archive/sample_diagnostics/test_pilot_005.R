# Test PILOT_005 with corrected 5s period
# This is a quick test before running full batch

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

# Process just PILOT_005
config <- SAMPLE_CONFIG[["PILOT_005"]]
result <- process_sample("PILOT_005", config, PARAMS)

if (!is.null(result) && result$success) {
  cat("\n\n=== PILOT_005 TEST RESULTS ===\n")
  cat(sprintf("Total cycles detected: %d\n", result$stats$n_cycles_total))
  cat(sprintf("Cycles with data: %d\n", result$stats$n_cycles_detected))
  cat(sprintf("Estimated period: %.3f s\n", result$period_stats$period))
  cat(sprintf("Stable time percentage: %.2f%%\n", result$stats$pct_stable))
  
  # Show first 10 intervals
  cat("\nFirst 10 inter-cycle intervals:\n")
  intervals <- diff(result$cycles$start_time[1:min(11, nrow(result$cycles))])
  for (i in 1:length(intervals)) {
    cat(sprintf("  Interval %d: %.3f s\n", i, intervals[i]))
  }
} else {
  cat("\n\n=== PILOT_005 TEST FAILED ===\n")
  if (!is.null(result)) {
    cat(sprintf("Error: %s\n", result$error))
  }
}
