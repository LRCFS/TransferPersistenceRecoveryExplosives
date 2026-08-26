# Quick diagnostic for PILOT_009 around 10-13s
library(dplyr)

# Read the file
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_009.csv"
data <- clean_and_read_csv(data_file)

# Look at data around cycle 2 expected time
window_data <- data[data$elapsed_sec >= 10 & data$elapsed_sec <= 13.5, ]

cat("PILOT_009 data from 10-13.5s:\n")
cat("Time range:", range(window_data$elapsed_sec), "\n")
cat("Load range:", range(window_data$load, na.rm = TRUE), "\n\n")

# Check if any data is in 185-200g range
stable_data <- window_data[window_data$load >= 185 & window_data$load <= 200, ]
cat(sprintf("Points in 185-200g range: %d\n", nrow(stable_data)))

if (nrow(stable_data) > 0) {
  cat("\nFirst few points in stable range:\n")
  print(head(stable_data[, c("elapsed_sec", "load")], 10))
  
  # Check for continuous segments
  stable_data$is_stable <- TRUE
  stable_data$stable_id <- cumsum(c(0, diff(stable_data$elapsed_sec) > 0.2))
  
  segments <- aggregate(
    elapsed_sec ~ stable_id,
    data = stable_data,
    FUN = function(x) c(start = min(x), end = max(x), duration = max(x) - min(x), n = length(x))
  )
  
  cat("\nStable segments:\n")
  for (i in 1:nrow(segments)) {
    start <- segments$elapsed_sec[i, "start"]
    end <- segments$elapsed_sec[i, "end"]
    dur <- segments$elapsed_sec[i, "duration"]
    n <- segments$elapsed_sec[i, "n"]
    cat(sprintf("  Segment %d: %.3f - %.3fs (duration: %.3fs, %d points)\n", i, start, end, dur, n))
  }
}

# Check what happens if we widen the range
cat("\n\nPoints in 185-210g range: %d\n", sum(window_data$load >= 185 & window_data$load <= 210))
wide_stable <- window_data[window_data$load >= 185 & window_data$load <= 210, ]
if (nrow(wide_stable) > 0) {
  cat(sprintf("Time range: %.3f - %.3fs\n", min(wide_stable$elapsed_sec), max(wide_stable$elapsed_sec)))
  cat(sprintf("Duration: %.3fs\n", max(wide_stable$elapsed_sec) - min(wide_stable$elapsed_sec)))
}
