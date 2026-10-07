# Diagnostic script to find first stable regions in failed samples
# This will help us determine appropriate startup skip times

library(dplyr)

# Configuration
SAMPLE_CONFIG <- list(
  "PILOT_009" = list(target = 200, stable_low = 185, stable_high = 200),
  "PILOT_013" = list(target = 200, stable_low = 185, stable_high = 200),
  "PILOT_005" = list(target = 50, stable_low = 45, stable_high = 55),
  "PILOT_021" = list(target = 50, stable_low = 45, stable_high = 55)
)

data_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces"

# Function to clean and read CSV
clean_and_read_csv <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  proper_header <- "timestamp,elapsed_sec,analogValue,load,distance"
  header_line <- NULL
  
  for (i in 1:min(20, length(lines))) {
    line <- lines[i]
    if (grepl("analogValue.*load.*distance", line)) {
      if (i > 1 && (grepl("^[0-9]", lines[i-1]) || grepl("timestamp.*elapsed_sec", lines[i-1]))) {
        header_line <- i + 1
        lines <- c(proper_header, lines[header_line:length(lines)])
        header_line <- 1
        break
      } else {
        header_line <- i
        break
      }
    }
    if (line == proper_header) {
      header_line <- i
      break
    }
  }
  
  if (is.null(header_line)) {
    first_line <- lines[1]
    if (grepl("timestamp,elapsed_sec", first_line) && grepl("[0-9]", first_line)) {
      lines[1] <- proper_header
      header_line <- 1
    }
  }
  
  if (!is.null(header_line) && header_line > 1) {
    lines <- lines[header_line:length(lines)]
  }
  
  if (is.null(header_line)) {
    return(NULL)
  }
  
  temp_file <- tempfile(fileext = ".csv")
  writeLines(lines, temp_file)
  
  data <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    NULL
  })
  
  unlink(temp_file)
  return(data)
}

# Function to find first stable regions
find_first_stable_regions <- function(data, low_thresh, high_thresh, min_duration = 0.3) {
  
  # Mark points within stable range
  data$is_stable <- data$load >= low_thresh & data$load <= high_thresh
  
  # Find continuous stable segments
  data$stable_id <- cumsum(c(0, diff(data$is_stable) != 0))
  
  # Keep only stable segments
  stable_segments <- data[data$is_stable, ]
  
  if (nrow(stable_segments) == 0) {
    return(NULL)
  }
  
  # Aggregate by segment
  results <- aggregate(
    cbind(elapsed_sec, load) ~ stable_id,
    data = stable_segments,
    FUN = function(x) c(start = min(x), end = max(x), mean = mean(x), n = length(x))
  )
  
  # Extract values
  regions <- data.frame(
    start_time = results$elapsed_sec[, "start"],
    end_time = results$elapsed_sec[, "end"],
    mean_load = results$load[, "mean"],
    n_points = results$load[, "n"]
  )
  
  # Calculate durations
  regions$duration <- regions$end_time - regions$start_time
  
  # Filter by minimum duration
  regions <- regions[regions$duration >= min_duration, ]
  
  return(regions)
}

# Process each failed sample
cat("========================================\n")
cat("DIAGNOSTIC: Finding First Stable Regions\n")
cat("========================================\n\n")

for (sample_name in names(SAMPLE_CONFIG)) {
  config <- SAMPLE_CONFIG[[sample_name]]
  
  cat(sprintf("Sample: %s (%dg target)\n", sample_name, config$target))
  cat(sprintf("Stable range: %d - %d g\n", config$stable_low, config$stable_high))
  
  # Load data
  data_file <- file.path(data_dir, paste0(sample_name, ".csv"))
  data <- clean_and_read_csv(data_file)
  
  if (is.null(data)) {
    cat("  ERROR: Could not read file\n\n")
    next
  }
  
  cat(sprintf("Time range: %.2f - %.2f s\n", min(data$elapsed_sec), max(data$elapsed_sec)))
  
  # Find all stable regions
  regions <- find_first_stable_regions(data, config$stable_low, config$stable_high, min_duration = 0.3)
  
  if (is.null(regions) || nrow(regions) == 0) {
    cat("  No stable regions found!\n\n")
    next
  }
  
  cat(sprintf("Found %d stable regions (duration >= 0.3s)\n", nrow(regions)))
  cat("\nFirst 10 stable regions:\n")
  
  n_show <- min(10, nrow(regions))
  for (i in 1:n_show) {
    region <- regions[i, ]
    cat(sprintf("  %2d. %.3f - %.3fs (duration: %.3fs, mean: %.1fg)\n",
                i, region$start_time, region$end_time, region$duration, region$mean_load))
  }
  
  # Calculate intervals between first few regions
  if (nrow(regions) >= 5) {
    intervals <- diff(regions$start_time[1:5])
    cat("\nFirst 4 inter-region intervals:\n")
    for (i in 1:length(intervals)) {
      cat(sprintf("  Interval %d: %.3f s\n", i, intervals[i]))
    }
  }
  
  cat("\n")
  cat("========================================\n\n")
}
