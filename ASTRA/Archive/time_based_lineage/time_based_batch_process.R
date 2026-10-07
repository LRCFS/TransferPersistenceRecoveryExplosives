# Time-Based Periodic Stable Contact Detection - Batch Processing
# Standalone script - processes all PILOT samples
# Author: OpenCode
# Date: 2026-07-17

# ===============================================================================
# CONFIGURATION
# ===============================================================================

# Metadata file path
METADATA_FILE <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/pilot_data_nested.csv"

# Pressure-level configurations (unified defaults)
PRESSURE_CONFIGS <- list(
  "200" = list(  # 200g high pressure samples
    target = 200,
    stable_low = 175,
    stable_high = 210,
    expected_period = 10
  ),
  "50" = list(   # 50g low pressure samples
    target = 50,
    stable_low = 45,
    stable_high = 65,
    expected_period = 5
  )
)

# # Sample-specific overrides (only for exceptions with different stable ranges)
# SAMPLE_OVERRIDES <- list(
#   "PILOT_009" = list(stable_high = 210),              # Higher stable pressure
#   "PILOT_013" = list(stable_low = 178, stable_high = 195),  # Lower/tighter range
#   "PILOT_029" = list(stable_high = 195)               # Lower upper limit
# )

SAMPLE_OVERRIDES <- list()

# ===============================================================================
# GLOBAL PARAMETERS
# ===============================================================================

PARAMS <- list(
  # File paths
  data_dir = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces",
  output_dir = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/ProcessedData",
  
  # Detection parameters (common to all samples)
  min_stable_duration = 0.3,       # Minimum duration to consider stable (s)
  max_normal_duration = 2.0,       # Maximum duration for normal cycle (longer = startup artifact)
  skip_startup_sec = 8,            # Skip first 8 seconds (startup period for all samples)
  
  # Period estimation
  n_cycles_for_period = 5,         # Number of initial cycles to estimate period
  period_tolerance_factor = 2,     # Tolerance = factor × period_sd
  min_period_tolerance = 2.0,      # Minimum tolerance window (s)
  use_median_period = TRUE,        # Use median instead of mean for robustness
  
  # Initial detection
  initial_search_window = 5,       # Window size for searching each cycle
  max_time_for_initial = 120       # Maximum time to search for initial cycles
)

# ===============================================================================
# FUNCTION: build_sample_config
# ===============================================================================
# Automatically builds sample configuration by:
# 1. Reading metadata CSV to get pressure levels
# 2. Finding available pressure trace CSV files
# 3. Applying pressure-level defaults
# 4. Applying sample-specific overrides

build_sample_config <- function(metadata_file, data_dir, pressure_configs, overrides) {
  
  cat("Building sample configuration...\n")
  
  # 1. Read metadata CSV
  if (!file.exists(metadata_file)) {
    stop(sprintf("Metadata file not found: %s", metadata_file))
  }
  
  metadata <- read.csv(metadata_file, stringsAsFactors = FALSE)
  
  # Check required columns
  if (!all(c("RunID", "Pressure_g") %in% colnames(metadata))) {
    stop("Metadata CSV must contain 'RunID' and 'Pressure_g' columns")
  }
  
  cat(sprintf("  Loaded metadata for %d samples\n", nrow(metadata)))
  
  # 2. Find available pressure trace CSV files
  csv_files <- list.files(data_dir, pattern = "^PILOT_[0-9]+\\.csv$", full.names = FALSE)
  available_samples <- sub("\\.csv$", "", csv_files)
  
  cat(sprintf("  Found %d pressure trace CSV files\n", length(available_samples)))
  
  # 3. Build configuration for each available sample
  sample_config <- list()
  skipped_count <- 0
  
  for (sample_id in available_samples) {
    # Get pressure level from metadata
    sample_meta <- metadata[metadata$RunID == sample_id, ]
    
    if (nrow(sample_meta) == 0) {
      cat(sprintf("  WARNING: %s not found in metadata, skipping\n", sample_id))
      skipped_count <- skipped_count + 1
      next
    }
    
    pressure_g <- as.character(sample_meta$Pressure_g[1])
    
    # Get default config for this pressure level
    if (!(pressure_g %in% names(pressure_configs))) {
      cat(sprintf("  WARNING: Unknown pressure level %sg for %s, skipping\n", 
                  pressure_g, sample_id))
      skipped_count <- skipped_count + 1
      next
    }
    
    config <- pressure_configs[[pressure_g]]
    
    # Apply sample-specific overrides if they exist
    if (sample_id %in% names(overrides)) {
      override_vals <- overrides[[sample_id]]
      for (param in names(override_vals)) {
        config[[param]] <- override_vals[[param]]
      }
    }
    
    sample_config[[sample_id]] <- config
  }
  
  if (skipped_count > 0) {
    cat(sprintf("  Skipped %d sample(s) due to warnings\n", skipped_count))
  }
  
  cat(sprintf("  Configured %d samples for processing\n\n", length(sample_config)))
  
  return(sample_config)
}

# ===============================================================================
# AUTO-GENERATE SAMPLE CONFIGURATION
# ===============================================================================

SAMPLE_CONFIG <- build_sample_config(
  metadata_file = METADATA_FILE,
  data_dir = PARAMS$data_dir,
  pressure_configs = PRESSURE_CONFIGS,
  overrides = SAMPLE_OVERRIDES
)

# Exit if no samples configured
if (length(SAMPLE_CONFIG) == 0) {
  stop("No samples configured for processing. Check metadata and CSV files.")
}

# ===============================================================================
# FUNCTION: clean_and_read_csv
# ===============================================================================
# Handles CSV files with malformed headers by finding and keeping only rows
# from the proper header row onwards

clean_and_read_csv <- function(filepath) {
  
  # Read all lines
  lines <- readLines(filepath, warn = FALSE)
  
  # The proper header should be: "timestamp,elapsed_sec,analogValue,load,distance"
  proper_header <- "timestamp,elapsed_sec,analogValue,load,distance"
  
  header_line <- NULL
  
  # Strategy: Find the line that contains "analogValue,load,distance"
  # This is the most reliable signature of the actual header
  for (i in 1:min(20, length(lines))) {
    line <- lines[i]
    
    # Check if this line contains the key header elements
    if (grepl("analogValue.*load.*distance", line)) {
      # Check if this line is JUST the column names (header row as data)
      if (i > 1 && (grepl("^[0-9]", lines[i-1]) || grepl("timestamp.*elapsed_sec", lines[i-1]))) {
        # Previous line had timestamp/elapsed_sec, this line has the rest
        # Remove both and start from next line
        header_line <- i + 1
        # But we need to prepend the proper header
        lines <- c(proper_header, lines[header_line:length(lines)])
        header_line <- 1
        cat(sprintf("  Cleaning CSV: Reconstructed header from split rows\n"))
        break
      } else {
        # This line has the full proper header
        header_line <- i
        break
      }
    }
    
    # Check if this is already a proper full header
    if (line == proper_header) {
      header_line <- i
      break
    }
  }
  
  # If we didn't find proper header, check for the case where first row 
  # has "timestamp,elapsed_sec,<numbers>" (PILOT_005, PILOT_021 case)
  if (is.null(header_line)) {
    first_line <- lines[1]
    if (grepl("timestamp,elapsed_sec", first_line) && grepl("[0-9]", first_line)) {
      # First row has header + data mixed - replace it with proper header
      cat(sprintf("  Cleaning CSV: Replacing malformed first row with proper header\n"))
      lines[1] <- proper_header
      header_line <- 1
    }
  }
  
  # If we found a proper header line and it's not the first, remove lines before it
  if (!is.null(header_line) && header_line > 1) {
    cat(sprintf("  Cleaning CSV: Removing %d line(s) before header\n", header_line - 1))
    lines <- lines[header_line:length(lines)]
  }
  
  # If we still haven't found it, the file might be too corrupted
  if (is.null(header_line)) {
    cat("  ERROR: Could not find valid header in CSV file\n")
    return(NULL)
  }
  
  # Write cleaned lines to temporary file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(lines, temp_file)
  
  # Read the cleaned CSV
  data <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat(sprintf("  Error reading cleaned CSV: %s\n", e$message))
    NULL
  })
  
  # Clean up temp file
  unlink(temp_file)
  
  # Validate that we have the expected columns
  if (!is.null(data)) {
    required_cols <- c("timestamp", "elapsed_sec", "analogValue", "load", "distance")
    if (!all(required_cols %in% colnames(data))) {
      cat(sprintf("  ERROR: Missing required columns. Found: %s\n", 
                  paste(colnames(data), collapse=", ")))
      return(NULL)
    }
    
    # Check if elapsed_sec is actually numeric
    if (!is.numeric(data$elapsed_sec)) {
      cat("  ERROR: elapsed_sec column is not numeric\n")
      cat(sprintf("  First few values: %s\n", paste(head(data$elapsed_sec), collapse=", ")))
      return(NULL)
    }
  }
  
  return(data)
}

# ===============================================================================
# FUNCTION: detect_stable_by_threshold
# ===============================================================================

detect_stable_by_threshold <- function(data, start_time, end_time, 
                                       low_thresh, high_thresh, min_duration,
                                       return_all = FALSE) {
  
  # Filter data to search window
  window_data <- data[data$elapsed_sec >= start_time & data$elapsed_sec <= end_time, ]
  
  if (nrow(window_data) == 0) {
    return(NULL)
  }
  
  # Mark points within stable range
  window_data$is_stable <- window_data$load >= low_thresh & window_data$load <= high_thresh
  
  # Find continuous stable segments
  window_data$stable_id <- cumsum(c(0, diff(window_data$is_stable) != 0))
  
  # Keep only stable segments
  stable_segments <- window_data[window_data$is_stable, ]
  
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
  
  if (nrow(regions) == 0) {
    return(NULL)
  }
  
  # Return all regions or just the longest
  if (return_all) {
    # Sort by start time
    regions <- regions[order(regions$start_time), ]
    return(regions)
  } else {
    # Keep the longest segment in this window
    regions <- regions[which.max(regions$duration), ]
    return(regions)
  }
}

# ===============================================================================
# FUNCTION: detect_initial_cycles
# ===============================================================================

detect_initial_cycles <- function(data, n_cycles, params) {
  
  cat(sprintf("PHASE 1: Detecting first %d cycles (skipping first %.1fs)...\n", 
              n_cycles, params$skip_startup_sec))
  cat("  Strategy: Search consecutive windows with expected period\n")
  cat(sprintf("  Note: Skipping startup regions longer than %.1fs (startup artifacts)\n", 
              params$max_normal_duration))
  
  all_cycles <- data.frame()
  
  # Find ALL stable regions in the initial search window
  # We'll filter out startup artifacts (too long) and use the first normal one
  all_regions <- detect_stable_by_threshold(
    data = data,
    start_time = params$skip_startup_sec,
    end_time = params$skip_startup_sec + params$max_time_for_initial,
    low_thresh = params$stable_range_low,
    high_thresh = params$stable_range_high,
    min_duration = params$min_stable_duration,
    return_all = TRUE
  )
  
  if (is.null(all_regions)) {
    stop("Could not find any stable region after startup. Check parameters.")
  }
  
  # If we only got one region, use it even if it's long
  if (nrow(all_regions) == 1) {
    first_region <- all_regions[1, ]
  } else {
    # Filter out startup artifacts (regions that are too long)
    # These are typically 2-4+ seconds and represent startup phase
    normal_regions <- all_regions[all_regions$duration <= params$max_normal_duration, ]
    
    if (nrow(normal_regions) == 0) {
      # All regions are too long - this might be a problem
      # Use the shortest one as it's most likely to be real
      cat("  Warning: All initial regions are longer than expected. Using shortest.\n")
      first_region <- all_regions[which.min(all_regions$duration), ]
    } else {
      # Use the first normal-length region
      first_region <- normal_regions[1, ]
      if (nrow(all_regions) > nrow(normal_regions)) {
        n_skipped <- sum(all_regions$start_time < first_region$start_time)
        cat(sprintf("  Skipped %d startup artifact(s) (too long)\n", n_skipped))
      }
    }
  }
  
  first_region$cycle_num <- 1
  all_cycles <- rbind(all_cycles, first_region)
  
  cat(sprintf("  Cycle %d: %.3fs - %.3fs (duration: %.3fs, mean load: %.1fg)\n",
              1, first_region$start_time, first_region$end_time, 
              first_region$duration, first_region$mean_load))
  
  # Search for subsequent cycles
  crude_period <- params$expected_period
  search_window_size <- params$initial_search_window
  
  last_start <- first_region$start_time
  
  for (i in 2:n_cycles) {
    # Predict next cycle
    expected_start <- last_start + crude_period
    window_start <- expected_start - search_window_size/2
    window_end <- expected_start + search_window_size/2
    
    # Search in window
    region <- detect_stable_by_threshold(
      data = data,
      start_time = window_start,
      end_time = window_end,
      low_thresh = params$stable_range_low,
      high_thresh = params$stable_range_high,
      min_duration = params$min_stable_duration
    )
    
    if (is.null(region)) {
      cat(sprintf("  Warning: Could not find cycle %d in window %.1f-%.1fs\n", 
                  i, window_start, window_end))
      break
    }
    
    region$cycle_num <- i
    all_cycles <- rbind(all_cycles, region)
    
    cat(sprintf("  Cycle %d: %.3fs - %.3fs (duration: %.3fs, mean load: %.1fg)\n",
                i, region$start_time, region$end_time, region$duration, region$mean_load))
    
    # Update last start
    last_start <- region$start_time
    
    # Refine period estimate after 3 cycles
    if (i == 3) {
      intervals <- diff(all_cycles$start_time)
      crude_period <- median(intervals)
      cat(sprintf("  > Refined period estimate to %.3fs based on first %d cycles\n", 
                  crude_period, i))
    }
  }
  
  return(all_cycles)
}

# ===============================================================================
# FUNCTION: estimate_period
# ===============================================================================

estimate_period <- function(cycles, params) {
  
  if (nrow(cycles) < 2) {
    stop("Need at least 2 cycles to estimate period")
  }
  
  cat("\nPHASE 2: Estimating period...\n")
  
  # Calculate inter-cycle intervals
  intervals <- diff(cycles$start_time)
  
  # Use median if specified
  if (params$use_median_period) {
    central_period <- median(intervals)
    cat("  Using median for robust estimation\n")
  } else {
    central_period <- mean(intervals)
    cat("  Using mean for estimation\n")
  }
  
  sd_period <- sd(intervals)
  cv_period <- sd_period / central_period
  
  cat(sprintf("  Number of intervals: %d\n", length(intervals)))
  cat(sprintf("  Median period: %.3f s\n", median(intervals)))
  cat(sprintf("  Mean period: %.3f s\n", mean(intervals)))
  cat(sprintf("  SD period: %.3f s\n", sd_period))
  cat(sprintf("  CV period: %.2f%%\n", cv_period * 100))
  cat("  Individual intervals:\n")
  for (i in 1:length(intervals)) {
    cat(sprintf("    Interval %d: %.3f s\n", i, intervals[i]))
  }
  
  return(list(
    period = central_period,
    mean_period = mean(intervals),
    median_period = median(intervals),
    sd_period = sd_period,
    cv_period = cv_period,
    intervals = intervals
  ))
}

# ===============================================================================
# FUNCTION: predict_and_detect_remaining
# ===============================================================================

predict_and_detect_remaining <- function(data, initial_cycles, period_stats, params) {
  
  cat("\nPHASE 3: Predicting and detecting remaining cycles with STRICT periodicity...\n")
  
  # Calculate search tolerance
  tolerance <- max(
    params$period_tolerance_factor * period_stats$sd_period,
    params$min_period_tolerance
  )
  
  cat(sprintf("  Using period: %.3f s\n", period_stats$period))
  cat(sprintf("  Search tolerance: ±%.3f s\n", tolerance))
  
  # Get last detected cycle
  last_cycle <- initial_cycles[nrow(initial_cycles), ]
  last_start <- last_cycle$start_time
  cycle_count <- last_cycle$cycle_num
  
  max_time <- max(data$elapsed_sec)
  cat(sprintf("  Data extends to: %.1f s\n", max_time))
  cat(sprintf("  Expected number of additional cycles: ~%.0f\n", 
              (max_time - last_start) / period_stats$period))
  
  all_remaining_cycles <- data.frame()
  missed_count <- 0
  
  # Predict cycles until end of data
  while (TRUE) {
    cycle_count <- cycle_count + 1
    
    # Predict next cycle start
    predicted_start <- last_start + period_stats$period
    
    # Check if we've reached end of data
    if (predicted_start > max_time) {
      cat(sprintf("\n  Reached end of data at cycle %d\n", cycle_count - 1))
      break
    }
    
    # Define search window
    search_start <- predicted_start - tolerance
    search_end <- predicted_start + tolerance
    
    # Detect in window
    region <- detect_stable_by_threshold(
      data = data,
      start_time = search_start,
      end_time = search_end,
      low_thresh = params$stable_range_low,
      high_thresh = params$stable_range_high,
      min_duration = params$min_stable_duration
    )
    
    if (is.null(region)) {
      # STRICT MODE: Force placeholder
      cat(sprintf("  Cycle %d: FORCED at predicted %.3fs (not detected in data)\n",
                  cycle_count, predicted_start))
      
      region <- data.frame(
        start_time = predicted_start,
        end_time = predicted_start,
        mean_load = NA,
        n_points = 0,
        duration = 0,
        cycle_num = cycle_count
      )
      
      missed_count <- missed_count + 1
      last_start <- predicted_start
    } else {
      region$cycle_num <- cycle_count
      last_start <- region$start_time
      
      if (cycle_count %% 10 == 0) {
        cat(sprintf("  Cycle %d: %.3fs - %.3fs (predicted: %.3fs, error: %.3fs)\n",
                    cycle_count, region$start_time, region$end_time, 
                    predicted_start, region$start_time - predicted_start))
      }
    }
    
    all_remaining_cycles <- rbind(all_remaining_cycles, region)
  }
  
  cat(sprintf("  Total cycles predicted: %d\n", nrow(all_remaining_cycles)))
  cat(sprintf("  Successfully detected: %d\n", nrow(all_remaining_cycles) - missed_count))
  cat(sprintf("  Forced (not found): %d (%.1f%%)\n", 
              missed_count, 100 * missed_count / nrow(all_remaining_cycles)))
  
  return(all_remaining_cycles)
}

# ===============================================================================
# FUNCTION: calculate_statistics
# ===============================================================================

calculate_statistics <- function(all_cycles, data, params) {
  
  cat("\nPHASE 4: Calculating statistics...\n")
  
  # Separate real detections from forced placeholders
  real_cycles <- all_cycles[all_cycles$duration > 0, ]
  forced_cycles <- all_cycles[all_cycles$duration == 0, ]
  
  # Total stable time
  total_stable_time <- sum(real_cycles$duration, na.rm = TRUE)
  
  # Total time in passes
  total_pass_time <- sum(data$load > 0) * median(diff(data$elapsed_sec), na.rm = TRUE)
  
  # Percentage
  pct_stable <- (total_stable_time / total_pass_time) * 100
  
  # Cycle statistics
  n_cycles_total <- nrow(all_cycles)
  n_cycles_detected <- nrow(real_cycles)
  n_cycles_forced <- nrow(forced_cycles)
  mean_duration <- mean(real_cycles$duration, na.rm = TRUE)
  sd_duration <- sd(real_cycles$duration, na.rm = TRUE)
  
  # PRESSURE STATISTICS
  max_pressure_all <- max(data$load, na.rm = TRUE)
  
  # Get all individual pressure points in stable regions
  stable_pressures <- c()
  for (i in 1:nrow(real_cycles)) {
    cycle <- real_cycles[i, ]
    cycle_data <- data[data$elapsed_sec >= cycle$start_time & 
                       data$elapsed_sec <= cycle$end_time, ]
    stable_pressures <- c(stable_pressures, cycle_data$load)
  }
  
  # Statistics across stable regions
  mean_pressure_stable <- mean(stable_pressures, na.rm = TRUE)
  median_pressure_stable <- median(stable_pressures, na.rm = TRUE)
  sd_pressure_stable <- sd(stable_pressures, na.rm = TRUE)
  min_pressure_stable <- min(stable_pressures, na.rm = TRUE)
  max_pressure_stable <- max(stable_pressures, na.rm = TRUE)
  
  # RSD (Relative Standard Deviation)
  rsd_pressure_stable <- (sd_pressure_stable / mean_pressure_stable) * 100
  
  cat(sprintf("  Total cycles (strict periodicity): %d\n", n_cycles_total))
  cat(sprintf("  Cycles with data detected: %d (%.1f%%)\n", 
              n_cycles_detected, 100 * n_cycles_detected / n_cycles_total))
  cat(sprintf("  Cycles forced (no data): %d (%.1f%%)\n", 
              n_cycles_forced, 100 * n_cycles_forced / n_cycles_total))
  cat(sprintf("  Mean cycle duration: %.3f ± %.3f s\n", mean_duration, sd_duration))
  cat(sprintf("  Total stable time: %.2f s\n", total_stable_time))
  cat(sprintf("  Total pass time: %.2f s\n", total_pass_time))
  cat(sprintf("  Percentage stable: %.2f%%\n", pct_stable))
  cat("\n  PRESSURE STATISTICS:\n")
  cat(sprintf("    Maximum pressure (all data): %.1f g\n", max_pressure_all))
  cat(sprintf("    Mean pressure (stable regions): %.1f g\n", mean_pressure_stable))
  cat(sprintf("    Median pressure (stable regions): %.1f g\n", median_pressure_stable))
  cat(sprintf("    SD pressure (stable regions): %.1f g\n", sd_pressure_stable))
  cat(sprintf("    RSD pressure (stable regions): %.2f%%\n", rsd_pressure_stable))
  cat(sprintf("    Range (stable regions): %.1f - %.1f g\n", 
              min_pressure_stable, max_pressure_stable))
  
  return(list(
    n_cycles_total = n_cycles_total,
    n_cycles_detected = n_cycles_detected,
    n_cycles_forced = n_cycles_forced,
    total_stable_time = total_stable_time,
    total_pass_time = total_pass_time,
    pct_stable = pct_stable,
    mean_duration = mean_duration,
    sd_duration = sd_duration,
    max_pressure_all = max_pressure_all,
    mean_pressure_stable = mean_pressure_stable,
    median_pressure_stable = median_pressure_stable,
    sd_pressure_stable = sd_pressure_stable,
    rsd_pressure_stable = rsd_pressure_stable,
    min_pressure_stable = min_pressure_stable,
    max_pressure_stable = max_pressure_stable
  ))
}

# ===============================================================================
# FUNCTION: create_25s_plots
# ===============================================================================

create_25s_plots <- function(data, all_cycles, params, sample_name) {
  
  cat("\nCreating 25s window plots...\n")
  
  # Output directory for 25s plots
  plot_dir <- file.path(params$output_dir, "25s_windows")
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Get time range
  max_time <- max(data$elapsed_sec, na.rm = TRUE)
  
  # Create plots in 25s windows
  window_size <- 25
  window_starts <- seq(0, max_time, by = window_size)
  
  cat(sprintf("  Creating %d plots (25s windows)...\n", length(window_starts)))
  
  for (i in 1:length(window_starts)) {
    window_start <- window_starts[i]
    window_end <- window_start + window_size
    
    # Filter data for this window
    window_data <- data[data$elapsed_sec >= window_start & data$elapsed_sec < window_end, ]
    
    if (nrow(window_data) == 0) {
      next
    }
    
    # Filter cycles for this window
    window_cycles <- all_cycles[all_cycles$start_time >= window_start & all_cycles$start_time < window_end, ]
    # Only real cycles
    window_cycles_real <- window_cycles[window_cycles$duration > 0, ]
    
    # Create filename
    filename <- sprintf("%s_%03d_%.0f-%.0fs.png", sample_name, i, window_start, window_end)
    filepath <- file.path(plot_dir, filename)
    
    # Create plot
    png(filepath, width = 1200, height = 600, res = 100)
    
    par(mar = c(4, 4, 3, 1))
    
    plot(window_data$elapsed_sec, window_data$load, type = "l", col = "gray40", lwd = 1.5,
         xlab = "Time (s)", ylab = "Load (g)",
         main = sprintf("%s: %.0f - %.0fs (%d cycles)", 
                        sample_name, window_start, window_end, nrow(window_cycles_real)),
         ylim = c(0, max(window_data$load, na.rm = TRUE) * 1.05))
    
    # Add grid
    grid(col = "gray90", lty = 1)
    
    # Draw threshold lines
    abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 2)
    abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 2)
    
    # Highlight detected stable regions
    if (nrow(window_cycles_real) > 0) {
      for (j in 1:nrow(window_cycles_real)) {
        cycle <- window_cycles_real[j, ]
        rect(cycle$start_time, 0, cycle$end_time, max(window_data$load, na.rm = TRUE),
             col = rgb(0, 1, 0, 0.25), border = NA)
        
        # Add cycle number label
        text(cycle$start_time + cycle$duration/2, max(window_data$load, na.rm = TRUE) * 0.95,
             labels = sprintf("#%d", cycle$cycle_num), cex = 0.8, col = "darkgreen", font = 2)
      }
    }
    
    # Add legend
    legend("topright", 
           legend = c("Load", sprintf("Stable thresholds (%d-%dg)", 
                                     params$stable_range_low, params$stable_range_high), 
                     "Detected stable"),
           col = c("gray40", "blue", rgb(0, 1, 0, 0.5)),
           lty = c(1, 2, 1), lwd = c(1.5, 2, 8), bty = "n", cex = 0.9)
    
    dev.off()
    
    if (i %% 5 == 0) {
      cat(sprintf("    Created %d/%d plots\n", i, length(window_starts)))
    }
  }
  
  cat(sprintf("  25s plots saved to: %s\n", plot_dir))
}

# ===============================================================================
# FUNCTION: save_results
# ===============================================================================

save_results <- function(all_cycles, stats, period_stats, params, sample_name) {
  
  cat("\nSaving results...\n")
  
  # Save detected cycles
  cycles_file <- file.path(params$output_dir, paste0(sample_name, "_detected_cycles.csv"))
  write.csv(all_cycles, cycles_file, row.names = FALSE)
  cat(sprintf("  Cycles saved to: %s\n", cycles_file))
  
  # Save summary statistics
  summary_file <- file.path(params$output_dir, paste0(sample_name, "_summary_statistics.txt"))
  sink(summary_file)
  cat(sprintf("%s - Time-Based Detection Summary\n", sample_name))
  cat("==========================================\n\n")
  cat("PARAMETERS:\n")
  cat(sprintf("  Target pressure: %d g\n", params$target_pressure))
  cat(sprintf("  Stable range: %d - %d g\n", params$stable_range_low, params$stable_range_high))
  cat(sprintf("  Minimum duration: %.2f s\n", params$min_stable_duration))
  cat(sprintf("  Startup skip: %.1f s\n", params$skip_startup_sec))
  cat("\nPERIOD ESTIMATION:\n")
  cat(sprintf("  Median period: %.3f s\n", period_stats$median_period))
  cat(sprintf("  Mean period: %.3f s\n", period_stats$mean_period))
  cat(sprintf("  SD period: %.3f s\n", period_stats$sd_period))
  cat(sprintf("  CV period: %.2f%%\n", period_stats$cv_period * 100))
  cat("\nDETECTION RESULTS:\n")
  cat(sprintf("  Total cycles detected: %d\n", stats$n_cycles_total))
  cat(sprintf("  Cycles with data: %d (%.1f%%)\n", stats$n_cycles_detected, 
              100 * stats$n_cycles_detected / stats$n_cycles_total))
  cat(sprintf("  Cycles forced: %d (%.1f%%)\n", stats$n_cycles_forced,
              100 * stats$n_cycles_forced / stats$n_cycles_total))
  cat(sprintf("  Mean cycle duration: %.3f ± %.3f s\n", stats$mean_duration, stats$sd_duration))
  cat(sprintf("  Total stable time: %.2f s\n", stats$total_stable_time))
  cat(sprintf("  Total pass time: %.2f s\n", stats$total_pass_time))
  cat(sprintf("  Percentage stable: %.2f%%\n", stats$pct_stable))
  cat("\nPRESSURE STATISTICS:\n")
  cat(sprintf("  Maximum pressure (all data): %.1f g\n", stats$max_pressure_all))
  cat(sprintf("  Mean pressure (stable regions): %.1f g\n", stats$mean_pressure_stable))
  cat(sprintf("  Median pressure (stable regions): %.1f g\n", stats$median_pressure_stable))
  cat(sprintf("  SD pressure (stable regions): %.1f g\n", stats$sd_pressure_stable))
  cat(sprintf("  RSD pressure (stable regions): %.2f%%\n", stats$rsd_pressure_stable))
  cat(sprintf("  Range (stable regions): %.1f - %.1f g\n", 
              stats$min_pressure_stable, stats$max_pressure_stable))
  sink()
  cat(sprintf("  Summary saved to: %s\n", summary_file))
}

# ===============================================================================
# FUNCTION: process_sample
# ===============================================================================

process_sample <- function(sample_name, config, params) {
  
  cat("\n")
  cat("========================================\n")
  cat(sprintf("Processing: %s (%dg target)\n", sample_name, config$target))
  cat("========================================\n\n")
  
  # Load data
  data_file <- file.path(params$data_dir, paste0(sample_name, ".csv"))
  
  if (!file.exists(data_file)) {
    cat(sprintf("ERROR: File not found: %s\n", data_file))
    return(NULL)
  }
  
  cat("Loading data...\n")
  data <- clean_and_read_csv(data_file)
  
  if (is.null(data)) {
    cat(sprintf("ERROR: Could not read file: %s\n", data_file))
    return(NULL)
  }
  
  cat(sprintf("  Loaded %d rows\n", nrow(data)))
  cat(sprintf("  Time range: %.2f - %.2f s\n", 
              min(data$elapsed_sec, na.rm = TRUE), 
              max(data$elapsed_sec, na.rm = TRUE)))
  
  # Update parameters for this sample
  sample_params <- params
  sample_params$target_pressure <- config$target
  sample_params$stable_range_low <- config$stable_low
  sample_params$stable_range_high <- config$stable_high
  sample_params$output_dir <- file.path(params$output_dir, sample_name)
  
  # Expected period already set in config from PRESSURE_CONFIGS
  sample_params$expected_period <- config$expected_period
  
  # Create output directory
  dir.create(sample_params$output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Run detection
  tryCatch({
    # Detect initial cycles
    initial_cycles <- detect_initial_cycles(data, sample_params$n_cycles_for_period, sample_params)
    
    if (nrow(initial_cycles) < 2) {
      cat(sprintf("ERROR: Could not detect enough initial cycles for %s\n", sample_name))
      return(NULL)
    }
    
    # Estimate period
    period_stats <- estimate_period(initial_cycles, sample_params)
    
    # Detect remaining cycles
    remaining_cycles <- predict_and_detect_remaining(data, initial_cycles, period_stats, sample_params)
    
    # Combine all cycles
    all_cycles <- rbind(initial_cycles, remaining_cycles)
    all_cycles <- all_cycles[order(all_cycles$start_time), ]
    
    # Calculate statistics
    stats <- calculate_statistics(all_cycles, data, sample_params)
    
    # Create 25s plots
    create_25s_plots(data, all_cycles, sample_params, sample_name)
    
    # Save results
    save_results(all_cycles, stats, period_stats, sample_params, sample_name)
    
    cat("\n========================================\n")
    cat(sprintf("%s COMPLETE!\n", sample_name))
    cat("========================================\n")
    
    return(list(
      sample = sample_name,
      target = config$target,
      cycles = all_cycles,
      stats = stats,
      period_stats = period_stats,
      success = TRUE
    ))
    
  }, error = function(e) {
    cat(sprintf("\nERROR processing %s: %s\n", sample_name, e$message))
    return(list(
      sample = sample_name,
      target = config$target,
      success = FALSE,
      error = e$message
    ))
  })
}

# ===============================================================================
# MAIN BATCH EXECUTION
# ===============================================================================

main_batch <- function() {
  
  cat("========================================\n")
  cat("Time-Based Batch Processing\n")
  cat("========================================\n")
  cat(sprintf("Samples to process: %d\n", length(SAMPLE_CONFIG)))
  cat(sprintf("Output directory: %s\n\n", PARAMS$output_dir))
  
  # Process all samples
  results <- list()
  
  for (sample_name in names(SAMPLE_CONFIG)) {
    config <- SAMPLE_CONFIG[[sample_name]]
    result <- process_sample(sample_name, config, PARAMS)
    results[[sample_name]] <- result
  }
  
  # Create summary table
  cat("\n\n")
  cat("========================================\n")
  cat("BATCH PROCESSING SUMMARY\n")
  cat("========================================\n\n")
  
  summary_data <- data.frame(
    Sample = character(),
    Target = numeric(),
    Success = logical(),
    Cycles = numeric(),
    Detected = numeric(),
    Stable_Pct = numeric(),
    Mean_Pressure = numeric(),
    RSD_Pressure = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (sample_name in names(results)) {
    result <- results[[sample_name]]
    if (!is.null(result) && result$success) {
      summary_data <- rbind(summary_data, data.frame(
        Sample = sample_name,
        Target = result$target,
        Success = TRUE,
        Cycles = result$stats$n_cycles_total,
        Detected = result$stats$n_cycles_detected,
        Stable_Pct = result$stats$pct_stable,
        Mean_Pressure = result$stats$mean_pressure_stable,
        RSD_Pressure = result$stats$rsd_pressure_stable,
        stringsAsFactors = FALSE
      ))
    } else {
      summary_data <- rbind(summary_data, data.frame(
        Sample = sample_name,
        Target = if(!is.null(result)) result$target else NA,
        Success = FALSE,
        Cycles = NA,
        Detected = NA,
        Stable_Pct = NA,
        Mean_Pressure = NA,
        RSD_Pressure = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  print(summary_data)
  
  # Save summary table
  summary_file <- file.path(PARAMS$output_dir, "batch_summary.csv")
  write.csv(summary_data, summary_file, row.names = FALSE)
  cat(sprintf("\nBatch summary saved to: %s\n", summary_file))
  
  cat("\n========================================\n")
  cat("BATCH PROCESSING COMPLETE!\n")
  cat("========================================\n")
  
  return(results)
}

# Run
results <- main_batch()
