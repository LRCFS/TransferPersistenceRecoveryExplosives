# Test PILOT_021 detection with cleaned data

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

# Process PILOT_021
cat("Testing PILOT_021 with cleaned data and 45-55g range\n")
cat("=====================================================\n\n")

config <- SAMPLE_CONFIG[["PILOT_021"]]
cat(sprintf("Target: %dg\n", config$target))
cat(sprintf("Stable range: %d - %dg\n\n", config$stable_low, config$stable_high))

result <- process_sample("PILOT_021", config, PARAMS)

if (!is.null(result) && result$success) {
  cat("\n✓ PILOT_021 SUCCESS!\n")
  cat(sprintf("  Cycles: %d (%d detected, %d forced)\n",
              result$stats$n_cycles_total,
              result$stats$n_cycles_detected,
              result$stats$n_cycles_forced))
  cat(sprintf("  Stable: %.2f%%\n", result$stats$pct_stable))
} else {
  cat("\n✗ PILOT_021 FAILED - checking why...\n\n")
  
  # Load and analyze the data
  data_file <- file.path(PARAMS$data_dir, "PILOT_021.csv")
  data <- clean_and_read_csv(data_file)
  
  cat(sprintf("Data loaded: %d rows, %.2f - %.2fs\n", 
              nrow(data), min(data$elapsed_sec), max(data$elapsed_sec)))
  
  # Find ALL stable regions manually
  stable_data <- data[data$load >= config$stable_low & data$load <= config$stable_high, ]
  
  if (nrow(stable_data) > 0) {
    # Mark continuous segments
    stable_data$segment_id <- cumsum(c(1, diff(stable_data$elapsed_sec) > 0.5))
    
    segments <- aggregate(
      elapsed_sec ~ segment_id,
      data = stable_data,
      FUN = function(x) c(start = min(x), end = max(x), duration = max(x) - min(x), n = length(x))
    )
    
    # Filter segments >= 0.3s
    long_segments <- segments[segments$elapsed_sec[, "duration"] >= 0.3, ]
    
    cat(sprintf("\nFound %d stable regions (>= 0.3s duration) in %d - %dg range:\n",
                nrow(long_segments), config$stable_low, config$stable_high))
    
    if (nrow(long_segments) > 0) {
      for (i in 1:min(10, nrow(long_segments))) {
        start <- long_segments$elapsed_sec[i, "start"]
        end <- long_segments$elapsed_sec[i, "end"]
        dur <- long_segments$elapsed_sec[i, "duration"]
        cat(sprintf("  %2d. %.3f - %.3fs (duration: %.3fs)\n", i, start, end, dur))
      }
      
      if (nrow(long_segments) >= 2) {
        # Calculate intervals
        starts <- long_segments$elapsed_sec[, "start"]
        intervals <- diff(starts)
        cat(sprintf("\nFirst %d inter-region intervals:\n", min(5, length(intervals))))
        for (i in 1:min(5, length(intervals))) {
          cat(sprintf("  Interval %d: %.3fs\n", i, intervals[i]))
        }
      }
    }
  } else {
    cat("\nNo stable regions found in the specified range!\n")
  }
  
  # Check overall data distribution
  cat(sprintf("\nOverall load statistics:\n"))
  cat(sprintf("  Min: %.2fg\n", min(data$load, na.rm=TRUE)))
  cat(sprintf("  Max: %.2fg\n", max(data$load, na.rm=TRUE)))
  cat(sprintf("  Mean: %.2fg\n", mean(data$load, na.rm=TRUE)))
  cat(sprintf("  Median: %.2fg\n", median(data$load, na.rm=TRUE)))
  
  # Check how much time is in passes (load > 0)
  pass_data <- data[data$load > 0, ]
  cat(sprintf("\nTime with load > 0: %.2fs (%.1f%% of total)\n",
              nrow(pass_data) * median(diff(data$elapsed_sec)),
              100 * nrow(pass_data) / nrow(data)))
}
