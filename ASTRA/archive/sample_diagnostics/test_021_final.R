# Test PILOT_021 with max_normal_duration = 1.5s

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

config <- SAMPLE_CONFIG[["PILOT_021"]]
result <- process_sample("PILOT_021", config, PARAMS)

if (!is.null(result) && result$success) {
  cat("\n✓ PILOT_021 SUCCESS!\n")
  cat(sprintf("  Cycles: %d (%d detected, %d forced)\n",
              result$stats$n_cycles_total,
              result$stats$n_cycles_detected,
              result$stats$n_cycles_forced))
  cat(sprintf("  Stable: %.2f%%\n", result$stats$pct_stable))
  cat(sprintf("  Mean pressure: %.1fg (RSD: %.2f%%)\n", 
              result$stats$mean_pressure_stable,
              result$stats$rsd_pressure_stable))
  cat(sprintf("  Period: %.3fs (CV: %.2f%%)\n",
              result$period_stats$period,
              result$period_stats$cv_period * 100))
} else {
  cat("\n✗ PILOT_021 FAILED\n")
}
