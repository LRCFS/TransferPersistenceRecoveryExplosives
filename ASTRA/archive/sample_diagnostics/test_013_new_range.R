# Test PILOT_013 with new 175-210g range

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

cat("Testing PILOT_013 with 175-210g range\n")
cat("======================================\n\n")

config <- SAMPLE_CONFIG[["PILOT_013"]]
cat(sprintf("Stable range: %d - %dg\n\n", config$stable_low, config$stable_high))

result <- process_sample("PILOT_013", config, PARAMS)

if (!is.null(result) && result$success) {
  cat("\n✓ PILOT_013 SUCCESS!\n")
  cat(sprintf("  Cycles: %d (%d detected, %d forced)\n",
              result$stats$n_cycles_total,
              result$stats$n_cycles_detected,
              result$stats$n_cycles_forced))
  cat(sprintf("  Stable time: %.2f%% (%.2fs total)\n", 
              result$stats$pct_stable,
              result$stats$total_stable_time))
  cat(sprintf("  Mean cycle duration: %.3fs\n", result$stats$mean_duration))
  cat(sprintf("  Mean pressure: %.1fg (RSD: %.2f%%)\n", 
              result$stats$mean_pressure_stable,
              result$stats$rsd_pressure_stable))
  cat(sprintf("  Period: %.3fs (CV: %.2f%%)\n",
              result$period_stats$period,
              result$period_stats$cv_period * 100))
} else {
  cat("\n✗ PILOT_013 FAILED\n")
}
