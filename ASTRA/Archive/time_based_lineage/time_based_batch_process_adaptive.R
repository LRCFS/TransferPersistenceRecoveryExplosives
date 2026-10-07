# Time-Based Periodic Stable Contact Detection - Batch Processing (ADAPTIVE VERSION)
# This version automatically calculates sample-specific pressure ranges
# Standalone script - processes all PILOT samples
# Author: OpenCode
# Date: 2026-07-17
# Version: Adaptive Thresholds

# ===============================================================================
# LOAD REQUIRED LIBRARIES
# ===============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(writexl)
})

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
  max_time_for_initial = 120,      # Maximum time to search for initial cycles
  
  # Adaptive threshold parameters
  use_adaptive_thresholds = TRUE,  # Enable automatic threshold calculation
  adaptive_discovery_duration = 60, # Use first 60s for pressure discovery
  adaptive_tolerance_200g = 10,    # ±10g tolerance for 200g samples
  adaptive_tolerance_50g = 8,      # ±8g tolerance for 50g samples
  adaptive_min_regions = 3,        # Need at least 3 regions for reliable stats
  adaptive_fallback_to_defaults = TRUE  # Use config defaults if discovery fails
)

# ===============================================================================
# PLOTTING CONFIGURATION
# ===============================================================================

PLOTTING_PARAMS <- list(
  enabled = TRUE,
  output_dir = file.path(PARAMS$output_dir, "Figures"),
  dpi = 300,
  
  # Full trace comparison plots
  trace_width = 10,
  trace_height = 6,
  trace_line_width = 0.7,
  trace_alpha = 0.8,
  
  # Stable region overlay plots
  overlay_width = 8,
  overlay_height = 6,
  overlay_line_width = 0.6,
  overlay_alpha = 0.6,
  
  # Color palettes
  colors_wet = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#c6dbef"),  # Dark to light blues
  colors_dry = c("#006d2c", "#31a354", "#74c476", "#a1d99b", "#c7e9c0")   # Dark to light greens
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
# FUNCTION: discover_stable_pressure_range
# ===============================================================================
# Analyzes initial data to find the actual stable pressure range for this sample
# Uses wide initial thresholds to capture all potential stable regions

discover_stable_pressure_range <- function(data, config, params) {
  
  if (!params$use_adaptive_thresholds) {
    return(NULL)  # Adaptive thresholds disabled
  }
  
  # Use wide discovery range (85% to 110% of target pressure)
  discovery_low <- config$target * 0.85
  discovery_high <- config$target * 1.10
  
  # Calculate discovery window
  discovery_start <- params$skip_startup_sec
  discovery_end <- params$skip_startup_sec + params$adaptive_discovery_duration
  
  # Make sure we don't exceed available data
  discovery_end <- min(discovery_end, max(data$elapsed_sec))
  
  if (discovery_end - discovery_start < 20) {
    # Not enough data for discovery
    return(NULL)
  }
  
  # Find all stable regions in discovery window
  regions <- detect_stable_by_threshold(
    data = data,
    start_time = discovery_start,
    end_time = discovery_end,
    low_thresh = discovery_low,
    high_thresh = discovery_high,
    min_duration = params$min_stable_duration,
    return_all = TRUE
  )
  
  if (is.null(regions) || nrow(regions) < params$adaptive_min_regions) {
    # Not enough regions found for reliable statistics
    return(NULL)
  }
  
  # Calculate pressure statistics from discovered regions
  mean_pressure <- mean(regions$mean_load)
  median_pressure <- median(regions$mean_load)
  sd_pressure <- sd(regions$mean_load)
  min_pressure <- min(regions$mean_load)
  max_pressure <- max(regions$mean_load)
  
  return(list(
    mean = mean_pressure,
    median = median_pressure,
    sd = sd_pressure,
    min = min_pressure,
    max = max_pressure,
    n_regions = nrow(regions)
  ))
}

# ===============================================================================
# FUNCTION: calculate_adaptive_thresholds
# ===============================================================================
# Calculates adaptive pressure thresholds based on discovered pressure statistics
# Uses dynamic tolerance based on the standard deviation of discovered pressures

calculate_adaptive_thresholds <- function(pressure_stats, config, params) {
  
  if (is.null(pressure_stats)) {
    return(NULL)  # Fall back to defaults
  }
  
  # METHOD: Dynamic tolerance based on SD
  # Use mean ± (2 * SD) to capture ~95% of stable regions
  # This adapts to each sample's natural variability
  
  tolerance <- 2 * pressure_stats$sd
  
  # Apply minimum and maximum tolerance bounds
  # Minimum: 5g (prevent too narrow ranges)
  # Maximum: 20g (prevent too wide ranges that capture transients)
  min_tolerance <- 5
  max_tolerance <- 20
  
  if (tolerance < min_tolerance) {
    tolerance <- min_tolerance
  } else if (tolerance > max_tolerance) {
    tolerance <- max_tolerance
  }
  
  # Calculate adaptive thresholds: mean ± tolerance
  adaptive_low <- pressure_stats$mean - tolerance
  adaptive_high <- pressure_stats$mean + tolerance
  
  return(list(
    low = adaptive_low,
    high = adaptive_high,
    tolerance_used = tolerance,
    sd_based = TRUE
  ))
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
           legend = c("Load", sprintf("Stable thresholds (%.0f-%.0fg)", 
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
# FUNCTION: create_full_trace_plot
# ===============================================================================

create_full_trace_plot <- function(data, all_cycles, params, sample_name) {
  
  cat("\nCreating full trace plot...\n")
  
  # Create filename
  filepath <- file.path(params$output_dir, paste0(sample_name, "_full_trace.png"))
  
  # Get max time and pressure
  max_time <- max(data$elapsed_sec, na.rm = TRUE)
  max_pressure <- max(data$load, na.rm = TRUE)
  
  # Count real cycles (duration > 0)
  cycles_real <- all_cycles[all_cycles$duration > 0, ]
  n_cycles <- nrow(cycles_real)
  
  # Create plot
  png(filepath, width = 1800, height = 600, res = 100)
  
  par(mar = c(4, 4, 3, 1))
  
  # Main plot
  plot(data$elapsed_sec, data$load, type = "l", col = "gray40", lwd = 1.5,
       xlab = "Time (s)", ylab = "Load (g)",
       main = sprintf("%s: Full Pressure Trace (%d cycles detected)", 
                      sample_name, n_cycles),
       ylim = c(0, max_pressure * 1.05))
  
  # Add grid
  grid(col = "gray90", lty = 1)
  
  # Draw threshold lines
  abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 2)
  abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 2)
  
  # Highlight detected stable regions
  if (n_cycles > 0) {
    for (j in 1:n_cycles) {
      cycle <- cycles_real[j, ]
      rect(cycle$start_time, 0, cycle$end_time, max_pressure,
           col = rgb(0, 1, 0, 0.25), border = NA)
    }
  }
  
  # Add legend
  legend("topright", 
         legend = c("Load", sprintf("Stable thresholds (%.0f-%.0fg)", 
                                   params$stable_range_low, params$stable_range_high), 
                   "Detected stable"),
         col = c("gray40", "blue", rgb(0, 1, 0, 0.5)),
         lty = c(1, 2, 1), lwd = c(1.5, 2, 8), bty = "n", cex = 0.9)
  
  dev.off()
  
  cat(sprintf("  Full trace plot saved to: %s\n", filepath))
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
  cat(sprintf("  Stable range: %.1f - %.1f g\n", params$stable_range_low, params$stable_range_high))
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
  sample_params$output_dir <- file.path(params$output_dir, sample_name)
  
  # Expected period already set in config from PRESSURE_CONFIGS
  sample_params$expected_period <- config$expected_period
  
  # ADAPTIVE THRESHOLD DISCOVERY
  if (params$use_adaptive_thresholds) {
    cat("\nDiscovering sample-specific pressure range...\n")
    
    # Discover actual stable pressure range
    pressure_stats <- discover_stable_pressure_range(data, config, params)
    
    if (!is.null(pressure_stats)) {
      # Calculate adaptive thresholds
      adaptive_thresh <- calculate_adaptive_thresholds(pressure_stats, config, params)
      
      if (!is.null(adaptive_thresh)) {
        # Use adaptive thresholds
        sample_params$stable_range_low <- adaptive_thresh$low
        sample_params$stable_range_high <- adaptive_thresh$high
        
        cat(sprintf("  Discovered: mean=%.1fg, SD=%.1fg (n=%d regions)\n",
                    pressure_stats$mean, pressure_stats$sd, pressure_stats$n_regions))
        cat(sprintf("  Adaptive range: %.1f - %.1fg (mean ± %.1fg)\n",
                    adaptive_thresh$low, adaptive_thresh$high,
                    adaptive_thresh$tolerance_used))
      } else {
        # Fall back to config defaults
        sample_params$stable_range_low <- config$stable_low
        sample_params$stable_range_high <- config$stable_high
        cat("  Using default ranges (adaptive calculation failed)\n")
      }
    } else {
      # Fall back to config defaults
      sample_params$stable_range_low <- config$stable_low
      sample_params$stable_range_high <- config$stable_high
      cat(sprintf("  Using default ranges: %.1f - %.1fg (insufficient data for adaptation)\n",
                  config$stable_low, config$stable_high))
    }
  } else {
    # Adaptive thresholds disabled, use config defaults
    sample_params$stable_range_low <- config$stable_low
    sample_params$stable_range_high <- config$stable_high
    cat(sprintf("  Using default ranges: %.1f - %.1fg\n",
                config$stable_low, config$stable_high))
  }
  
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
    
    # Create full trace plot
    create_full_trace_plot(data, all_cycles, sample_params, sample_name)
    
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
# PLOTTING FUNCTIONS
# ===============================================================================

# ===============================================================================
# FUNCTION: load_raw_trace_data
# ===============================================================================

load_raw_trace_data <- function(sample_id, data_dir, align_time = TRUE, align_to_first_zero = FALSE, align_to_first_max = FALSE) {
  # Load and optionally align raw pressure trace data for plotting
  # align_time: if TRUE, shift time to start at 0; if FALSE, keep original elapsed_sec values
  # align_to_first_zero: if TRUE, align to first zero pressure reading (overrides align_time)
  # align_to_first_max: if TRUE, align to first local maximum after skip_startup
  
  file_path <- file.path(data_dir, paste0(sample_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(sprintf("Raw data file not found: %s", file_path))
    return(NULL)
  }
  
  # Use existing clean_and_read_csv function
  data <- clean_and_read_csv(file_path)
  
  # Extract relevant columns
  data <- data %>%
    select(elapsed_sec, load) %>%
    rename(time_sec = elapsed_sec, pressure_g = load)
  
  # Align based on first local maximum after startup
  if (align_to_first_max) {
    skip_startup_sec <- 8
    # Find SECOND local maximum after skip_startup (skip the first one)
    after_startup <- data[data$time_sec >= skip_startup_sec, ]
    
    if (nrow(after_startup) > 2) {
      # Calculate 75th percentile to identify significant peaks
      threshold_high <- quantile(after_startup$pressure_g, 0.75, na.rm = TRUE)
      
      # Find local maxima (points higher than neighbors AND above 75th percentile)
      max_count <- 0
      for (i in 2:(nrow(after_startup) - 1)) {
        if (after_startup$pressure_g[i] > after_startup$pressure_g[i-1] &&
            after_startup$pressure_g[i] > after_startup$pressure_g[i+1] &&
            after_startup$pressure_g[i] > threshold_high) {  # Must be significant peak
          max_count <- max_count + 1
          if (max_count == 2) {  # Use the SECOND maximum
            max_time <- after_startup$time_sec[i]
            data$time_sec <- data$time_sec - max_time
            break
          }
        }
      }
    }
  } else if (align_to_first_zero) {
    # Align based on first zero pressure reading
    # Find first point where pressure is at or near zero (< 5g threshold)
    zero_threshold <- 5
    zero_indices <- which(data$pressure_g < zero_threshold)
    
    if (length(zero_indices) > 0) {
      # Find the first zero reading
      first_zero_idx <- zero_indices[1]
      zero_time <- data$time_sec[first_zero_idx]
      
      # Shift time so first zero is at t=0
      data$time_sec <- data$time_sec - zero_time
    } else {
      # No zero found, fall back to regular alignment
      data$time_sec <- data$time_sec - min(data$time_sec, na.rm = TRUE)
    }
  } else if (align_time) {
    # Regular alignment to start at 0
    data$time_sec <- data$time_sec - min(data$time_sec, na.rm = TRUE)
  }
  
  # Add sample ID
  data$sample_id <- sample_id
  
  return(data)
}

# ===============================================================================
# FUNCTION: extract_stable_regions_for_overlay
# ===============================================================================

extract_stable_regions_for_overlay <- function(sample_id, data_dir, output_dir) {
  # Extract all stable regions with normalized time for overlay plotting
  
  # Read detected cycles
  cycles_file <- file.path(output_dir, sample_id, paste0(sample_id, "_detected_cycles.csv"))
  
  if (!file.exists(cycles_file)) {
    warning(sprintf("Detected cycles file not found: %s", cycles_file))
    return(NULL)
  }
  
  cycles <- read.csv(cycles_file, stringsAsFactors = FALSE)
  
  # Filter out forced cycles (NA mean_load)
  cycles <- cycles %>% filter(!is.na(mean_load))
  
  if (nrow(cycles) == 0) {
    return(NULL)
  }
  
  # Load raw data (DO NOT align time - use original elapsed_sec to match cycle start/end times)
  raw_data <- load_raw_trace_data(sample_id, data_dir, align_time = FALSE)
  
  if (is.null(raw_data)) {
    return(NULL)
  }
  
  # Extract each stable region
  all_regions <- list()
  
  for (i in 1:nrow(cycles)) {
    cycle <- cycles[i, ]
    
    # Filter data within this stable region
    region_data <- raw_data %>%
      filter(time_sec >= cycle$start_time & time_sec <= cycle$end_time) %>%
      mutate(
        time_in_region = time_sec - cycle$start_time,
        cycle_num = cycle$cycle_num,
        region_id = i
      ) %>%
      select(sample_id, cycle_num, region_id, time_in_region, pressure_g)
    
    if (nrow(region_data) > 0) {
      all_regions[[i]] <- region_data
    }
  }
  
  if (length(all_regions) == 0) {
    return(NULL)
  }
  
  # Combine all regions
  combined <- bind_rows(all_regions)
  
  return(combined)
}

# ===============================================================================
# FUNCTION: plot_full_traces_comparison
# ===============================================================================

plot_full_traces_comparison <- function(target_pressure, sample_results, metadata_df, data_dir, params) {
  # Create full trace comparison plot for all samples at target pressure
  
  cat(sprintf("  Loading %dg samples: ", target_pressure))
  
  # Filter samples by target pressure
  target_samples <- names(sample_results)[sapply(sample_results, function(r) r$target == target_pressure)]
  cat(paste(target_samples, collapse = ", "), "\n")
  
  # Load raw traces
  all_traces <- list()
  for (sample_id in target_samples) {
    if (target_pressure >= 150) {
      # 200g: align to first zero (works better)
      trace <- load_raw_trace_data(sample_id, data_dir, align_time = TRUE, align_to_first_zero = TRUE)
    } else {
      # 50g: align to start of first detected stable region
      trace <- load_raw_trace_data(sample_id, data_dir, align_time = FALSE)
      
      if (!is.null(trace)) {
        # Get first stable region start time from results
        result <- sample_results[[sample_id]]
        if (!is.null(result) && result$success) {
          cycles_with_data <- result$cycles %>% filter(!is.na(mean_load))
          if (nrow(cycles_with_data) > 0) {
            first_stable_start <- cycles_with_data$start_time[1]
            trace$time_sec <- trace$time_sec - first_stable_start
          }
        }
      }
    }
    
    if (!is.null(trace)) {
      # Get wet/dry status from metadata
      meta_row <- metadata_df[metadata_df$RunID == sample_id, ]
      if (nrow(meta_row) > 0) {
        trace$wet_dry <- ifelse(meta_row$Solvent_level == "present", "wet", "dry")
      } else {
        trace$wet_dry <- "unknown"
      }
      all_traces[[sample_id]] <- trace
      cat(sprintf("    %s: %d data points (%s)\n", sample_id, nrow(trace), trace$wet_dry[1]))
    }
  }
  
  if (length(all_traces) == 0) {
    warning("No traces loaded for plotting")
    return(NULL)
  }
  
  # Combine all traces
  plot_data <- bind_rows(all_traces)
  
  # Assign colors based on wet/dry and chronological order within each group
  wet_samples <- unique(plot_data[plot_data$wet_dry == "wet", "sample_id"])
  dry_samples <- unique(plot_data[plot_data$wet_dry == "dry", "sample_id"])
  
  color_map <- c()
  
  if (length(wet_samples) > 0) {
    wet_colors <- params$colors_wet[1:min(length(wet_samples), length(params$colors_wet))]
    names(wet_colors) <- sort(wet_samples)
    color_map <- c(color_map, wet_colors)
  }
  
  if (length(dry_samples) > 0) {
    dry_colors <- params$colors_dry[1:min(length(dry_samples), length(params$colors_dry))]
    names(dry_colors) <- sort(dry_samples)
    color_map <- c(color_map, dry_colors)
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = time_sec, y = pressure_g, color = sample_id)) +
    geom_line(linewidth = params$trace_line_width, alpha = params$trace_alpha) +
    scale_color_manual(values = color_map, name = "Sample ID") +
    scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    labs(
      title = sprintf("%dg Samples - Full Pressure Traces", target_pressure),
      subtitle = "Blue = Wet | Green = Dry",
      x = "Normalized time (s)",
      y = "Pressure (g)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "right"
    )
  
  # Save plot
  filename <- file.path(params$output_dir, sprintf("%dg_samples_full_traces.png", target_pressure))
  ggsave(filename, p, width = params$trace_width, height = params$trace_height, dpi = params$dpi)
  
  cat(sprintf("  Saved: %s\n\n", basename(filename)))
  
  return(filename)
}

# ===============================================================================
# FUNCTION: plot_stable_regions_overlay
# ===============================================================================

plot_stable_regions_overlay <- function(sample_id, sample_result, data_dir, output_dir, params) {
  # Create overlay plot of all stable regions for a single sample
  
  # Extract stable regions
  regions_data <- extract_stable_regions_for_overlay(sample_id, data_dir, output_dir)
  
  if (is.null(regions_data) || nrow(regions_data) == 0) {
    warning(sprintf("No stable regions data for %s", sample_id))
    return(NULL)
  }
  
  n_regions <- length(unique(regions_data$cycle_num))
  
  # Get statistics from results
  mean_pressure <- sample_result$stats$mean_pressure_stable
  sd_pressure <- sample_result$stats$sd_pressure_stable
  rsd_pressure <- sample_result$stats$rsd_pressure_stable
  target_pressure <- sample_result$target
  
  # Set y-axis limits based on target pressure (zoomed out view)
  if (target_pressure >= 150) {
    # 200g samples: show 150-250g range
    y_limits <- c(150, 250)
  } else {
    # 50g samples: show 30-70g range
    y_limits <- c(30, 70)
  }
  
  # Create plot
  p <- ggplot(regions_data, aes(x = time_in_region, y = pressure_g, group = cycle_num, color = cycle_num)) +
    geom_line(linewidth = params$overlay_line_width, alpha = params$overlay_alpha) +
    scale_color_gradient(low = "#c6dbef", high = "#08519c", name = "Cycle #") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title = sprintf("%s - Stable Region Overlay", sample_id),
      subtitle = "Color intensity: Light (early) → Dark (late)",
      x = "Time in stable region (s)",
      y = "Pressure (g)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    ) +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
             label = sprintf("Mean: %.1f ± %.1f g\nRSD: %.2f%%\nRegions: %d",
                             mean_pressure, sd_pressure, rsd_pressure, n_regions),
             size = 3.5, fontface = "bold")
  
  # Save plot
  filename <- file.path(params$output_dir, sprintf("%s_stable_overlay.png", sample_id))
  ggsave(filename, p, width = params$overlay_width, height = params$overlay_height, dpi = params$dpi)
  
  return(filename)
}

# ===============================================================================
# FUNCTION: create_all_comparison_plots
# ===============================================================================

create_all_comparison_plots <- function(results, metadata_df, data_dir, output_dir, params) {
  # Orchestrate generation of all plots
  
  # Create output directory
  dir.create(params$output_dir, showWarnings = FALSE, recursive = TRUE)
  
  generated_files <- list()
  
  # Generate full trace comparison plots
  cat("Creating full trace comparison plots...\n")
  
  # 50g samples
  file_50g <- plot_full_traces_comparison(50, results, metadata_df, data_dir, params)
  if (!is.null(file_50g)) {
    generated_files <- c(generated_files, file_50g)
  }
  
  # 200g samples
  file_200g <- plot_full_traces_comparison(200, results, metadata_df, data_dir, params)
  if (!is.null(file_200g)) {
    generated_files <- c(generated_files, file_200g)
  }
  
  # Generate stable region overlay plots
  cat("Creating stable region overlay plots...\n")
  
  for (sample_id in names(results)) {
    result <- results[[sample_id]]
    
    if (result$success) {
      file_overlay <- plot_stable_regions_overlay(sample_id, result, data_dir, output_dir, params)
      
      if (!is.null(file_overlay)) {
        n_regions <- result$stats$n_cycles_detected
        cat(sprintf("  %s: %d stable regions\n", sample_id, n_regions))
        generated_files <- c(generated_files, file_overlay)
      }
    }
  }
  
  return(list(files = generated_files))
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
  
  # Load metadata for plotting
  metadata_df <- read.csv(METADATA_FILE, stringsAsFactors = FALSE)
  
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
    First_Cycle_Time = numeric(),
    Last_Cycle_Time = numeric(),
    Stable_Pct = numeric(),
    Mean_Pressure = numeric(),
    RSD_Pressure = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (sample_name in names(results)) {
    result <- results[[sample_name]]
    if (!is.null(result) && result$success) {
      # Get first and last cycle times from cycles with data
      cycles_with_data <- result$cycles %>% filter(!is.na(mean_load))
      first_cycle_time <- if(nrow(cycles_with_data) > 0) cycles_with_data$start_time[1] else NA
      last_cycle_time <- if(nrow(cycles_with_data) > 0) cycles_with_data$end_time[nrow(cycles_with_data)] else NA
      
      summary_data <- rbind(summary_data, data.frame(
        Sample = sample_name,
        Target = result$target,
        Success = TRUE,
        Cycles = result$stats$n_cycles_total,
        Detected = result$stats$n_cycles_detected,
        First_Cycle_Time = first_cycle_time,
        Last_Cycle_Time = last_cycle_time,
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
        First_Cycle_Time = NA,
        Last_Cycle_Time = NA,
        Stable_Pct = NA,
        Mean_Pressure = NA,
        RSD_Pressure = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  print(summary_data)
  
  # Save summary table as CSV
  summary_file <- file.path(PARAMS$output_dir, "batch_summary.csv")
  write.csv(summary_data, summary_file, row.names = FALSE)
  cat(sprintf("\nBatch summary saved to: %s\n", summary_file))
  
  # Merge summary data with metadata to get solvent information
  summary_with_metadata <- summary_data %>%
    left_join(metadata_df %>% select(RunID, Solvent_detail), 
              by = c("Sample" = "RunID"))
  
  # Calculate overall statistics by pressure level AND condition (wet/dry)
  overall_stats <- data.frame(
    Pressure_Level = character(),
    Condition = character(),
    Mean_Pressure = numeric(),
    SD_Pressure = numeric(),
    RSD_Pressure = numeric(),
    Min_Pressure = numeric(),
    Max_Pressure = numeric(),
    N_Samples = integer(),
    stringsAsFactors = FALSE
  )
  
  # 50g Overall
  data_50g <- summary_with_metadata %>% filter(Target == 50 & Success == TRUE)
  if (nrow(data_50g) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "50g",
      Condition = "Overall",
      Mean_Pressure = mean(data_50g$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_50g$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_50g$Mean_Pressure, na.rm = TRUE) / mean(data_50g$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_50g$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_50g$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_50g),
      stringsAsFactors = FALSE
    ))
  }
  
  # 50g Wet (100uL_EtOH)
  data_50g_wet <- summary_with_metadata %>% filter(Target == 50 & Success == TRUE & Solvent_detail == "100uL_EtOH")
  if (nrow(data_50g_wet) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "50g",
      Condition = "Wet",
      Mean_Pressure = mean(data_50g_wet$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_50g_wet$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_50g_wet$Mean_Pressure, na.rm = TRUE) / mean(data_50g_wet$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_50g_wet$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_50g_wet$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_50g_wet),
      stringsAsFactors = FALSE
    ))
  }
  
  # 50g Dry
  data_50g_dry <- summary_with_metadata %>% filter(Target == 50 & Success == TRUE & Solvent_detail == "dry")
  if (nrow(data_50g_dry) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "50g",
      Condition = "Dry",
      Mean_Pressure = mean(data_50g_dry$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_50g_dry$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_50g_dry$Mean_Pressure, na.rm = TRUE) / mean(data_50g_dry$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_50g_dry$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_50g_dry$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_50g_dry),
      stringsAsFactors = FALSE
    ))
  }
  
  # 200g Overall
  data_200g <- summary_with_metadata %>% filter(Target == 200 & Success == TRUE)
  if (nrow(data_200g) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "200g",
      Condition = "Overall",
      Mean_Pressure = mean(data_200g$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_200g$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_200g$Mean_Pressure, na.rm = TRUE) / mean(data_200g$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_200g$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_200g$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_200g),
      stringsAsFactors = FALSE
    ))
  }
  
  # 200g Wet (100uL_EtOH)
  data_200g_wet <- summary_with_metadata %>% filter(Target == 200 & Success == TRUE & Solvent_detail == "100uL_EtOH")
  if (nrow(data_200g_wet) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "200g",
      Condition = "Wet",
      Mean_Pressure = mean(data_200g_wet$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_200g_wet$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_200g_wet$Mean_Pressure, na.rm = TRUE) / mean(data_200g_wet$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_200g_wet$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_200g_wet$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_200g_wet),
      stringsAsFactors = FALSE
    ))
  }
  
  # 200g Dry
  data_200g_dry <- summary_with_metadata %>% filter(Target == 200 & Success == TRUE & Solvent_detail == "dry")
  if (nrow(data_200g_dry) > 0) {
    overall_stats <- rbind(overall_stats, data.frame(
      Pressure_Level = "200g",
      Condition = "Dry",
      Mean_Pressure = mean(data_200g_dry$Mean_Pressure, na.rm = TRUE),
      SD_Pressure = sd(data_200g_dry$Mean_Pressure, na.rm = TRUE),
      RSD_Pressure = (sd(data_200g_dry$Mean_Pressure, na.rm = TRUE) / mean(data_200g_dry$Mean_Pressure, na.rm = TRUE)) * 100,
      Min_Pressure = min(data_200g_dry$Mean_Pressure, na.rm = TRUE),
      Max_Pressure = max(data_200g_dry$Mean_Pressure, na.rm = TRUE),
      N_Samples = nrow(data_200g_dry),
      stringsAsFactors = FALSE
    ))
  }
  
  # Save as Excel file with two sheets
  excel_file <- file.path(PARAMS$output_dir, "batch_summary.xlsx")
  tryCatch({
    write_xlsx(list(
      "Sample_Summary" = summary_data,
      "Overall_Statistics" = overall_stats
    ), excel_file)
    cat(sprintf("Batch summary Excel saved to: %s\n", excel_file))
  }, error = function(e) {
    cat(sprintf("Warning: Could not save Excel file: %s\n", e$message))
  })
  
  # ===============================================================================
  # CREATE COMPARISON PLOTS
  # ===============================================================================
  
  if (PLOTTING_PARAMS$enabled) {
    cat("\n========================================\n")
    cat("CREATING COMPARISON PLOTS\n")
    cat("========================================\n\n")
    
    tryCatch({
      plot_summary <- create_all_comparison_plots(
        results = results,
        metadata_df = metadata_df,
        data_dir = PARAMS$data_dir,
        output_dir = PARAMS$output_dir,
        params = PLOTTING_PARAMS
      )
      
      cat("\nGenerated plots:\n")
      for (plot_file in plot_summary$files) {
        cat(sprintf("  %s\n", basename(plot_file)))
      }
      
      cat(sprintf("\nAll plots saved to: %s\n", PLOTTING_PARAMS$output_dir))
      
    }, error = function(e) {
      cat(sprintf("\nWARNING: Plotting failed with error: %s\n", e$message))
      cat("Batch processing completed successfully despite plotting error.\n")
    })
    
    cat("\n========================================\n")
    cat("PLOTTING COMPLETE!\n")
    cat("========================================\n")
  }
  
  cat("\n========================================\n")
  cat("BATCH PROCESSING COMPLETE!\n")
  cat("========================================\n")
  
  return(results)
}

# Run
results <- main_batch()
