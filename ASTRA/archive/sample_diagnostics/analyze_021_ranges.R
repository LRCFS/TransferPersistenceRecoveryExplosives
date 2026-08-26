# Analyze PILOT_021 with wider range

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_021.csv"
data <- clean_and_read_csv(data_file)

cat("PILOT_021 Analysis\n")
cat("==================\n\n")

# Check what's around 8-9s where you say the first stable region is
window <- data[data$elapsed_sec >= 7 & data$elapsed_sec <= 10, ]
cat("Data from 7-10s:\n")
cat(sprintf("  Load range: %.2f - %.2fg\n", min(window$load), max(window$load)))
cat(sprintf("  Mean load: %.2fg\n", mean(window$load)))

# Find regions in DIFFERENT ranges
ranges <- list(
  "45-55g (current)" = c(45, 55),
  "50-60g" = c(50, 60),
  "55-65g" = c(55, 65),
  "50-65g" = c(50, 65),
  "45-65g (wide)" = c(45, 65)
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
    
    # Filter segments >= 0.3s
    long_segments <- segments[segments$elapsed_sec[, "duration"] >= 0.3, ]
    
    cat(sprintf("\n%s: Found %d regions (>= 0.3s)\n", range_name, nrow(long_segments)))
    
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
        cat("  Intervals:")
        for (int in intervals) {
          cat(sprintf(" %.2fs", int))
        }
        cat("\n")
      }
    }
  }
}
