# Analyze PILOT_013 stable regions

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_013.csv"
data <- clean_and_read_csv(data_file)

cat("PILOT_013 Pressure Range Analysis\n")
cat("==================================\n\n")

# Look at data around 10-25s where we expect periodic cycles
window <- data[data$elapsed_sec >= 10 & data$elapsed_sec <= 25, ]
cat("Data from 10-25s (expected periodic region):\n")
cat(sprintf("  Load range: %.2f - %.2fg\n", min(window$load), max(window$load)))
cat(sprintf("  Mean load: %.2fg\n", mean(window$load)))
cat(sprintf("  Median load: %.2fg\n\n", median(window$load)))

# Test different ranges
ranges <- list(
  "185-210g (current)" = c(185, 210),
  "180-210g" = c(180, 210),
  "175-210g" = c(175, 210),
  "170-210g" = c(170, 210)
)

for (range_name in names(ranges)) {
  range_vals <- ranges[[range_name]]
  low <- range_vals[1]
  high <- range_vals[2]
  
  stable_data <- data[data$load >= low & data$load <= high, ]
  
  if (nrow(stable_data) > 0) {
    # Mark continuous segments
    stable_data$segment_id <- cumsum(c(1, diff(stable_data$elapsed_sec) > 0.5))
    
    segments <- aggregate(
      elapsed_sec ~ segment_id,
      data = stable_data,
      FUN = function(x) c(start = min(x), end = max(x), duration = max(x) - min(x), n = length(x))
    )
    
    # Filter segments >= 0.3s AND after 8s startup
    long_segments <- segments[segments$elapsed_sec[, "start"] >= 8 & 
                              segments$elapsed_sec[, "duration"] >= 0.3, ]
    
    cat(sprintf("%s: Found %d regions (>= 0.3s, after 8s startup)\n", 
                range_name, nrow(long_segments)))
    
    if (nrow(long_segments) >= 1) {
      # Show first 5
      n_show <- min(5, nrow(long_segments))
      for (i in 1:n_show) {
        start <- long_segments$elapsed_sec[i, "start"]
        end <- long_segments$elapsed_sec[i, "end"]
        dur <- long_segments$elapsed_sec[i, "duration"]
        
        # Get actual load values in this segment
        seg_data <- data[data$elapsed_sec >= start & data$elapsed_sec <= end, ]
        mean_load <- mean(seg_data$load)
        
        cat(sprintf("  %2d. %.3f-%.3fs (%.3fs, mean: %.1fg)\n", 
                    i, start, end, dur, mean_load))
      }
      
      # Show intervals for first few
      if (nrow(long_segments) >= 3) {
        starts <- long_segments$elapsed_sec[, "start"]
        intervals <- diff(starts[1:min(5, nrow(long_segments))])
        cat("  First intervals:")
        for (int in intervals) {
          cat(sprintf(" %.2fs", int))
        }
        cat("\n")
      }
    }
    cat("\n")
  }
}

# Look at specific region around 12s
cat("\nDetailed look at 12-13s region:\n")
detail <- data[data$elapsed_sec >= 12 & data$elapsed_sec <= 13, ]
cat(sprintf("  Min: %.2fg, Max: %.2fg, Mean: %.2fg\n", 
            min(detail$load), max(detail$load), mean(detail$load)))
