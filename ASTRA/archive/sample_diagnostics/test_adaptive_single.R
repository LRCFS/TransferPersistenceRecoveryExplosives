# Test adaptive thresholds on a single sample

cat("Testing ADAPTIVE THRESHOLD script\n")
cat("==================================\n\n")

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process_adaptive.R")

# Test on PILOT_013 (the problematic one with spikes)
cat("Testing PILOT_013 (known spike issue)...\n\n")

config <- SAMPLE_CONFIG[["PILOT_013"]]
result <- process_sample("PILOT_013", config, PARAMS)

if (!is.null(result) && result$success) {
  cat("\n\n=== PILOT_013 RESULTS ===\n")
  cat(sprintf("Cycles: %d (%d detected, %d forced)\n",
              result$stats$n_cycles_total,
              result$stats$n_cycles_detected,
              result$stats$n_cycles_forced))
  cat(sprintf("Stable time: %.2f%% (%.2fs total)\n",
              result$stats$pct_stable,
              result$stats$total_stable_time))
  cat(sprintf("Mean cycle duration: %.3fs\n", result$stats$mean_duration))
  cat(sprintf("Mean pressure: %.1fg (RSD: %.2f%%)\n",
              result$stats$mean_pressure_stable,
              result$stats$rsd_pressure_stable))
  cat(sprintf("Pressure range: %.1f - %.1fg\n",
              result$stats$min_pressure_stable,
              result$stats$max_pressure_stable))
} else {
  cat("\n✗ PILOT_013 FAILED\n")
}
