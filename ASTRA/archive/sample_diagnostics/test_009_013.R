# Test just PILOT_009 and PILOT_013 with widened range

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

# Test PILOT_009
cat("\n\n===========================================\n")
cat("TESTING PILOT_009 with 185-210g range\n")
cat("===========================================\n")
config_009 <- SAMPLE_CONFIG[["PILOT_009"]]
result_009 <- process_sample("PILOT_009", config_009, PARAMS)

if (!is.null(result_009) && result_009$success) {
  cat("\n✓ PILOT_009 SUCCESS!\n")
  cat(sprintf("  Cycles: %d (%d detected, %d forced)\n",
              result_009$stats$n_cycles_total,
              result_009$stats$n_cycles_detected,
              result_009$stats$n_cycles_forced))
  cat(sprintf("  Stable: %.2f%%\n", result_009$stats$pct_stable))
  cat(sprintf("  Period: %.3fs (CV: %.2f%%)\n",
              result_009$period_stats$period, result_009$period_stats$cv_period * 100))
} else {
  cat("\n✗ PILOT_009 FAILED\n")
}

# Test PILOT_013
cat("\n\n===========================================\n")
cat("TESTING PILOT_013 with 185-210g range\n")
cat("===========================================\n")
config_013 <- SAMPLE_CONFIG[["PILOT_013"]]
result_013 <- process_sample("PILOT_013", config_013, PARAMS)

if (!is.null(result_013) && result_013$success) {
  cat("\n✓ PILOT_013 SUCCESS!\n")
  cat(sprintf("  Cycles: %d (%d detected, %d forced)\n",
              result_013$stats$n_cycles_total,
              result_013$stats$n_cycles_detected,
              result_013$stats$n_cycles_forced))
  cat(sprintf("  Stable: %.2f%%\n", result_013$stats$pct_stable))
  cat(sprintf("  Period: %.3fs (CV: %.2f%%)\n",
              result_013$period_stats$period, result_013$period_stats$cv_period * 100))
} else {
  cat("\n✗ PILOT_013 FAILED\n")
}
