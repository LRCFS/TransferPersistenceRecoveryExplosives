# Time-Based Periodic Stable Contact Detection - Batch Processing (ADAPTIVE + DISTANCE VERSION)
# This version automatically calculates sample-specific pressure ranges
# AND includes distance-based analysis alongside time-based analysis
#
# SINGLE-SCRIPT DESIGN: this file is fully self-contained. It does not
# source() any other script, and nothing should source() it either -
# run it directly with Rscript (or via RUN_BATCH_ANALYSIS.bat). Set
# RUN_MODE below to "batch" (process every configured sample) or
# "single" (quick test of one sample, replaces the old separate
# test_distance_*.R scripts).
#
# Author: OpenCode
# Date: 2026-07-30
# Version: v1.3.0-CONSOLIDATED (Adaptive Thresholds + Distance Tracking, single-file)

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
# RUN MODE
# ===============================================================================
# "batch"  - process every sample in SAMPLE_CONFIG and generate the full
#            batch summary + comparison plots (default production use)
# "single" - quickly process just SINGLE_TEST_SAMPLE and print a short
#            report; useful for testing parameter changes before a full
#            batch run. Replaces the old standalone test_distance_*.R
#            scripts, which have been folded into this single file.

RUN_MODE <- "batch"
SINGLE_TEST_SAMPLE <- "PILOT_001"

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

# Sample-specific overrides (only for exceptions with different stable ranges)
#
# PILOT_014 and PILOT_018 (both 50g protocol) never reached the nominal
# 45-65g stable band, nor the adaptive discovery band (42.5-55g, i.e.
# target*0.85 to target*1.10) - both previously failed with "Could not
# find any stable region after startup". Direct inspection of the raw
# traces shows a clean, highly periodic, non-noisy signal that simply
# plateaus lower than target:
#   PILOT_014: sustained plateau ~36-37g (~73% of the 50g target)
#   PILOT_018: sustained plateau ~39-41g (~80% of the 50g target)
# This is not sensor noise (region durations/values are highly
# repeatable cycle-to-cycle) and not a script bug (the same detection
# logic works correctly for 22 other samples). Both runs happen to have
# been executed back-to-back, 10 minutes apart, same day, same
# swab_mount config, but that timing coincidence does not by itself
# establish a cause - no physical/mechanical root cause has been
# independently confirmed (no logged calibration event or operator-
# observed anomaly during collection). Treat the low pressure as a
# confirmed, measured characteristic of these two runs; *why* it
# occurred is not known. Rather than leave this real data
# unprocessed, the overrides below recover it using sample-specific
# stable bands matched to each sample's *actual* achieved pressure.
# `target` is intentionally left at 50 (the nominal/intended protocol) -
# only the achieved stable band is overridden. As a result, Mean_Pressure
# in the batch summary for these two samples will legitimately read well
# below 50g; see README Troubleshooting section before including them in
# any "at-target" aggregate statistics.
SAMPLE_OVERRIDES <- list(
  "PILOT_014" = list(stable_low = 30, stable_high = 42),
  "PILOT_018" = list(stable_low = 34, stable_high = 48)
)

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
  adaptive_fallback_to_defaults = TRUE, # Use config defaults if discovery fails

  # Batch summary QC flag: flags any sample whose Mean_Pressure deviates
  # from its own Target by more than this fraction (absolute threshold,
  # not group-relative - see main_batch()/Flag_15Pct_Outlier). 0.15 = 15%.
  mean_pressure_outlier_pct = 0.15,

  # Side-by-side comparison folders for distance-based plots, generated
  # with vs. without non-contact transients suppressed (see
  # mask_noncontact_data(), and the suppress_transients/output_dir_override
  # parameters on create_full_trace_plot_distance(), create_zoomed_plot_distance(),
  # and plot_full_traces_comparison()).
  # Non-contact points (is_contact == FALSE, e.g. lift-off between passes)
  # collapse to a near-flat distance_mm, so on a distance x-axis they
  # render as thin near-vertical excursions toward 0g at every pass
  # boundary - expected, not a bug, but visually noisy. These two folders
  # let both versions be inspected side by side to evaluate which is
  # preferable, without altering the standard per-sample output location.
  transient_comparison_dir = file.path(
    "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/ProcessedData",
    "TransientComparison"
  )
)

# ===============================================================================
# G-CODE MOVEMENT PARAMETERS
# ===============================================================================
# Based on Ratchet9x9 swabbing patterns
# Source: Ratchet9x9-50g.gcode.txt and Ratchet9x9-200g.gcode.txt
# These parameters define the physical swabbing motion used to generate pressure traces

GCODE_PARAMS <- list(
  # Movement distances (from G-code)
  # USED (calculate_cumulative_distance, plotting):
  swab_distance_mm = 90,           # Horizontal swab distance per pass (X90 command)
  # REFERENCE ONLY - documents the pattern but not read by any calculation:
  pass_spacing_mm = 2,             # Y-axis spacing between passes (Y2 command)
  total_passes_nominal = 45,       # Nominal number of swab passes in pattern

  # Movement speeds (from G-code F parameters)
  # USED (calculate_cumulative_distance):
  swab_speed_mm_s = 83.33,         # F5000 mm/min = 83.33 mm/s (contact phase)
  # REFERENCE ONLY - documents theoretical cycle timing, not read by any calculation:
  lift_speed_mm_s = 5.0,           # F300 mm/min = 5.0 mm/s (Z-axis vertical)
  return_speed_mm_s = 300.0,       # F18000 mm/min = 300 mm/s (air return)

  # Pressure-specific parameters
  # REFERENCE ONLY - kept to document expected cycle timing per protocol;
  # actual cycle detection uses adaptive/period-based logic instead (see
  # PRESSURE_CONFIGS$expected_period and the adaptive threshold functions).
  pressure_params = list(
    "50" = list(
      lift_height_mm = 11,         # Z11 in 50g G-code
      lift_time_s = 2.2,           # 11mm ÷ 5 mm/s
      expected_cycle_time_s = 5.78, # Theoretical: 1.08s contact + 2.2s lift + 0.3s return + 2.2s lower
      contact_threshold_g = 35      # Load > 35g considered contact for distance calculation
    ),
    "200" = list(
      lift_height_mm = 22,         # Z22 in 200g G-code
      lift_time_s = 4.4,           # 22mm ÷ 5 mm/s
      expected_cycle_time_s = 10.18, # Theoretical: 1.08s contact + 4.4s lift + 0.3s return + 4.4s lower
      contact_threshold_g = 150     # Load > 150g considered contact for distance calculation
    )
  ),

  # Calculated constants
  # REFERENCE ONLY (documentation of theoretical timing):
  contact_time_s = 1.08,           # 90mm ÷ 83.33 mm/s (theoretical contact duration)
  return_time_s = 0.30,            # 90mm ÷ 300 mm/s (air return duration)
  # USED (calculate_statistics distance efficiency):
  total_distance_theoretical_mm = 4050  # 45 passes × 90mm (theoretical maximum)
)

# ===============================================================================
# PLOTTING CONFIGURATION
# ===============================================================================

PLOTTING_PARAMS <- list(
  enabled = TRUE,
  output_dir = file.path(PARAMS$output_dir, "Figures"),
  dpi = 300,
  
  # Full trace comparison plots (distance-based). Faceted as small multiples
  # (one panel per sample) rather than overlaid on a single panel - with
  # ~10-13 samples, a single overlaid panel becomes an illegible "spaghetti"
  # of overlapping sawtooth lines. Panels share a fixed y-axis scale within
  # each figure (50g or 200g) so that a sample sitting at a different
  # absolute pressure level than its peers is visually obvious.
  trace_line_width = 0.7,
  trace_alpha = 0.9,
  comparison_distance_limit_mm = 1000,
  comparison_ncol = 5,          # Panels per row
  comparison_panel_width_in = 3.0,   # Approx width per panel, inches
  comparison_panel_height_in = 2.2,  # Approx height per panel, inches
  comparison_extra_height_in = 1.2,  # Extra height for title/subtitle/legend
  
  # Stable region overlay plots (distance-based)
  overlay_width = 8,
  overlay_height = 6,
  overlay_line_width = 0.6,
  overlay_alpha = 0.6,
  overlay_distance_limit_mm = 90,  # One nominal pass (90mm); regions rarely exceed this
  
  # Batch QC overview plot (Mean_Pressure vs Target, all samples)
  qc_overview_width = 10,
  qc_overview_height_per_row_in = 0.4,  # Extra height per sample row
  qc_overview_base_height_in = 2.5,     # Fixed overhead (title/axis/etc.)
  
  # Fixed, non-recycled colors for wet/dry condition. Small-multiples faceting
  # (one panel per sample) means color no longer needs to distinguish
  # individual samples, so a flat 2-color scheme is sufficient and cannot
  # collide the way the old per-sample recycled 5-color palettes could (a
  # group with >5 dry or >5 wet samples - e.g. the 200g group currently has
  # 7 dry and 6 wet - would silently reuse the same hex color for two
  # different samples, previously making them visually indistinguishable).
  # Values match the thesis-wide shared palette's pal_solvent$wet/dry
  # (repo-root thesis_palette.R, Okabe-Ito colourblind-safe) -- inlined literally
  # rather than source()'d, since this script is deliberately single-file/
  # self-contained (see header).
  color_wet = "#0072B2",
  color_dry = "#009E73"
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
# FUNCTION: calculate_cumulative_distance
# ===============================================================================
# Calculates cumulative swabbing distance based on contact phases
# Distance only accumulates during horizontal contact (load > threshold)
# This represents actual swabbing distance, not total motion distance

calculate_cumulative_distance <- function(data, target_pressure, stable_range_low, stable_range_high, gcode_params, verbose = FALSE) {
  
  # Use the adaptive stable range for contact threshold
  # This ensures distance only accumulates during stable swabbing
  contact_threshold_low <- stable_range_low
  contact_threshold_high <- stable_range_high
  
  if (verbose) {
    cat(sprintf("\n=== DISTANCE CALCULATION ===\n"))
    cat(sprintf("  Target pressure: %dg\n", target_pressure))
    cat(sprintf("  Contact range: %.1f - %.1fg (stable swabbing range)\n", 
                contact_threshold_low, contact_threshold_high))
    cat(sprintf("  Swab speed: %.2f mm/s\n", gcode_params$swab_speed_mm_s))
  }
  
  # Initialize columns
  data$is_contact <- FALSE
  data$distance_mm <- 0.0
  cumulative_distance <- 0.0
  
  # Validation tracking
  total_contact_points <- 0
  total_contact_time <- 0.0
  
  # Calculate cumulative distance
  for (i in 1:nrow(data)) {
    # Determine if in stable contact range
    is_contact <- data$load[i] >= contact_threshold_low & data$load[i] <= contact_threshold_high
    data$is_contact[i] <- is_contact
    
    # Accumulate distance only during stable contact
    if (i > 1 && is_contact) {
      time_delta <- data$elapsed_sec[i] - data$elapsed_sec[i-1]
      
      # Only accumulate if time delta is reasonable (< 1 second to avoid gaps)
      if (time_delta > 0 && time_delta < 1.0) {
        distance_delta <- time_delta * gcode_params$swab_speed_mm_s
        cumulative_distance <- cumulative_distance + distance_delta
        
        # Validation tracking
        total_contact_time <- total_contact_time + time_delta
      }
    }
    
    # Store cumulative distance
    data$distance_mm[i] <- cumulative_distance
    
    if (is_contact) {
      total_contact_points <- total_contact_points + 1
    }
  }
  
  # Validation diagnostics
  if (verbose) {
    cat(sprintf("\n=== VALIDATION DIAGNOSTICS ===\n"))
    cat(sprintf("  Total data points: %d\n", nrow(data)))
    cat(sprintf("  Contact points: %d (%.1f%%)\n", 
                total_contact_points, 
                100 * total_contact_points / nrow(data)))
    cat(sprintf("  Total contact time: %.2f s\n", total_contact_time))
    cat(sprintf("  Total swabbing distance: %.1f mm\n", cumulative_distance))
    
    # Calculate implied speed and compare to expected
    if (total_contact_time > 0) {
      implied_speed <- cumulative_distance / total_contact_time
      cat(sprintf("  Implied swab speed: %.2f mm/s\n", implied_speed))
      cat(sprintf("  Expected swab speed: %.2f mm/s\n", gcode_params$swab_speed_mm_s))
      speed_error_pct <- 100 * abs(implied_speed - gcode_params$swab_speed_mm_s) / gcode_params$swab_speed_mm_s
      cat(sprintf("  Speed error: %.1f%%\n", speed_error_pct))
      
      if (speed_error_pct > 10) {
        cat("  WARNING: Speed error > 10%, check contact threshold or data quality\n")
      }
    }
    
    # Estimate number of passes based on distance
    if (cumulative_distance > 0) {
      estimated_passes <- cumulative_distance / gcode_params$swab_distance_mm
      cat(sprintf("  Estimated passes: %.1f (%.1f mm per pass)\n", 
                  estimated_passes, 
                  cumulative_distance / estimated_passes))
    } else {
      cat("  Estimated passes: 0 (no distance accumulated - check contact range)\n")
    }
    
    # Calculate theoretical maximum
    theoretical_max <- gcode_params$total_distance_theoretical_mm
    distance_efficiency <- 100 * cumulative_distance / theoretical_max
    cat(sprintf("  Theoretical maximum: %.0f mm (45 passes)\n", theoretical_max))
    cat(sprintf("  Distance efficiency: %.1f%%\n", distance_efficiency))
    cat("===========================\n\n")
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
  # Minimum is pressure-level specific (PARAMS$adaptive_tolerance_200g /
  # adaptive_tolerance_50g), preventing too-narrow ranges for whichever
  # protocol this sample belongs to.
  # Maximum: 20g (prevent too wide ranges that capture transients)
  min_tolerance <- if (config$target >= 150) {
    params$adaptive_tolerance_200g
  } else {
    params$adaptive_tolerance_50g
  }
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
  
  # Aggregate by segment (check if distance_mm exists first)
  has_distance <- "distance_mm" %in% colnames(stable_segments)
  
  if (has_distance) {
    results <- aggregate(
      cbind(elapsed_sec, load, distance_mm) ~ stable_id,
      data = stable_segments,
      FUN = function(x) c(start = min(x), end = max(x), mean = mean(x), n = length(x))
    )
  } else {
    results <- aggregate(
      cbind(elapsed_sec, load) ~ stable_id,
      data = stable_segments,
      FUN = function(x) c(start = min(x), end = max(x), mean = mean(x), n = length(x))
    )
  }
  
  # Extract values
  regions <- data.frame(
    start_time = results$elapsed_sec[, "start"],
    end_time = results$elapsed_sec[, "end"],
    mean_load = results$load[, "mean"],
    n_points = results$load[, "n"]
  )
  
  # Add distance tracking if distance_mm column exists
  if (has_distance) {
    regions$start_distance_mm = results$distance_mm[, "start"]
    regions$end_distance_mm = results$distance_mm[, "end"]
    regions$distance_span_mm = regions$end_distance_mm - regions$start_distance_mm
  }
  
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
        start_distance_mm = NA,
        end_distance_mm = NA,
        distance_span_mm = NA,
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

calculate_statistics <- function(all_cycles, data, params, gcode_params) {
  
  cat("\nPHASE 4: Calculating statistics...\n")
  
  # Separate real detections from forced placeholders
  real_cycles <- all_cycles[all_cycles$duration > 0, ]
  forced_cycles <- all_cycles[all_cycles$duration == 0, ]
  
  # Total stable time
  total_stable_time <- sum(real_cycles$duration, na.rm = TRUE)
  
  # Total time in passes
  total_pass_time <- sum(data$load > 0) * median(diff(data$elapsed_sec), na.rm = TRUE)
  
  # Percentage (guard against zero/NA denominator)
  pct_stable <- if (!is.na(total_pass_time) && total_pass_time > 0) {
    (total_stable_time / total_pass_time) * 100
  } else {
    cat("  WARNING: total_pass_time is 0 or NA; pct_stable set to NA\n")
    NA
  }
  
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
  
  # DISTANCE-BASED STATISTICS (if distance_mm column exists)
  has_distance <- "distance_mm" %in% colnames(data)
  
  if (has_distance) {
    # Calculate total swabbing distance
    total_distance_mm <- max(data$distance_mm, na.rm = TRUE)
    
    # Calculate stable distance from cycles
    total_stable_distance_mm <- 0
    if ("distance_span_mm" %in% colnames(real_cycles)) {
      total_stable_distance_mm <- sum(real_cycles$distance_span_mm, na.rm = TRUE)
    }
    
    # Calculate distance percentage (guard against zero total distance)
    pct_stable_distance <- if (!is.na(total_distance_mm) && total_distance_mm > 0) {
      (total_stable_distance_mm / total_distance_mm) * 100
    } else {
      cat("  WARNING: total_distance_mm is 0 or NA; pct_stable_distance set to NA\n")
      NA
    }
    
    # Calculate efficiency metrics
    theoretical_max_distance <- gcode_params$total_distance_theoretical_mm
    
    if (n_cycles_detected > 0) {
      expected_distance_from_cycles <- n_cycles_detected * gcode_params$swab_distance_mm
      distance_efficiency_actual <- (total_distance_mm / expected_distance_from_cycles) * 100
      mean_distance_per_cycle <- total_distance_mm / n_cycles_detected
    } else {
      cat("  WARNING: n_cycles_detected is 0; distance-per-cycle metrics set to NA\n")
      expected_distance_from_cycles <- NA
      distance_efficiency_actual <- NA
      mean_distance_per_cycle <- NA
    }
    
    distance_efficiency_theoretical <- (total_distance_mm / theoretical_max_distance) * 100
  } else {
    total_distance_mm <- NA
    total_stable_distance_mm <- NA
    pct_stable_distance <- NA
    theoretical_max_distance <- NA
    expected_distance_from_cycles <- NA
    distance_efficiency_theoretical <- NA
    distance_efficiency_actual <- NA
    mean_distance_per_cycle <- NA
  }
  
  cat(sprintf("  Total cycles (strict periodicity): %d\n", n_cycles_total))
  cat(sprintf("  Cycles with data detected: %d (%.1f%%)\n", 
              n_cycles_detected, 100 * n_cycles_detected / n_cycles_total))
  cat(sprintf("  Cycles forced (no data): %d (%.1f%%)\n", 
              n_cycles_forced, 100 * n_cycles_forced / n_cycles_total))
  cat(sprintf("  Mean cycle duration: %.3f ± %.3f s\n", mean_duration, sd_duration))
  cat(sprintf("  Total stable time: %.2f s\n", total_stable_time))
  cat(sprintf("  Total pass time: %.2f s\n", total_pass_time))
  cat(sprintf("  Percentage stable (time): %.2f%%\n", pct_stable))
  
  if (has_distance) {
    cat("\n  DISTANCE-BASED STATISTICS:\n")
    cat(sprintf("    Total swabbing distance: %.1f mm\n", total_distance_mm))
    cat(sprintf("    Theoretical maximum: %.0f mm (45 passes × 90mm)\n", theoretical_max_distance))
    cat(sprintf("    Expected from cycles: %.0f mm (%d cycles × 90mm)\n", 
                expected_distance_from_cycles, n_cycles_detected))
    cat(sprintf("    Distance efficiency (theoretical): %.1f%%\n", distance_efficiency_theoretical))
    cat(sprintf("    Distance efficiency (actual): %.1f%%\n", distance_efficiency_actual))
    cat(sprintf("    Mean distance per cycle: %.1f mm\n", mean_distance_per_cycle))
    cat(sprintf("    Total stable distance: %.1f mm\n", total_stable_distance_mm))
    cat(sprintf("    Percentage stable (distance): %.2f%%\n", pct_stable_distance))
  }
  
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
    max_pressure_stable = max_pressure_stable,
    # Distance-based metrics
    total_distance_mm = total_distance_mm,
    total_stable_distance_mm = total_stable_distance_mm,
    pct_stable_distance = pct_stable_distance,
    theoretical_max_distance = theoretical_max_distance,
    expected_distance_from_cycles = expected_distance_from_cycles,
    distance_efficiency_theoretical = distance_efficiency_theoretical,
    distance_efficiency_actual = distance_efficiency_actual,
    mean_distance_per_cycle = mean_distance_per_cycle
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
  
  cat("\nCreating full trace plot (time-based)...\n")
  
  # Create filename with _time suffix
  filepath <- file.path(params$output_dir, paste0(sample_name, "_full_trace_time.png"))
  
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
       main = sprintf("%s: Full Applied Mass Trace (%d cycles detected)", 
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
# FUNCTION: mask_noncontact_data
# ===============================================================================
# Returns a copy of a trace data frame with the pressure column set to NA
# wherever is_contact is FALSE (or NA). On a distance x-axis, non-contact
# points (lift-off between passes) collapse to a near-flat distance_mm, so
# they render as thin near-vertical excursions toward 0g at every pass
# boundary. Setting them to NA causes geom_line()/base plot(type="l") to
# break the line there instead of drawing a connecting segment down to 0
# and back - decluttering the plot to show only the plateau shape, at the
# cost of no longer being able to see the ramp-up/ramp-down transition
# shape in that particular plot (still fully visible in the time-based and
# 25s-window plots, which are unaffected by this function).
# Works on either "load" (used by the base-R per-sample distance plots) or
# "pressure_g" (used by the ggplot2 comparison plot), whichever is present.

mask_noncontact_data <- function(data) {
  if (!"is_contact" %in% colnames(data)) {
    return(data)
  }
  keep <- !is.na(data$is_contact) & data$is_contact
  if ("load" %in% colnames(data)) {
    data$load[!keep] <- NA
  }
  if ("pressure_g" %in% colnames(data)) {
    data$pressure_g[!keep] <- NA
  }
  return(data)
}

# ===============================================================================
# FUNCTION: create_full_trace_plot_distance
# ===============================================================================

create_full_trace_plot_distance <- function(data, all_cycles, params, sample_name, output_dir_override = NULL) {
  
  # Check if distance data exists
  if (!"distance_mm" %in% colnames(data)) {
    cat("  Skipping distance plot (no distance_mm column)\n")
    return()
  }
  
  cat("\nCreating full trace plot (distance-based)...\n")
  
  # Create filename with _distance suffix
  out_dir <- if (!is.null(output_dir_override)) output_dir_override else params$output_dir
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(out_dir, paste0(sample_name, "_full_trace_distance.png"))
  
  # Get max distance and pressure
  max_distance <- max(data$distance_mm, na.rm = TRUE)
  max_pressure <- max(data$load, na.rm = TRUE)
  
  # Count real cycles (duration > 0)
  cycles_real <- all_cycles[all_cycles$duration > 0, ]
  n_cycles <- nrow(cycles_real)
  
  # Create plot
  png(filepath, width = 1800, height = 600, res = 100)
  
  par(mar = c(4, 4, 3, 1))
  
  # Main plot
  plot(data$distance_mm, data$load, type = "l", col = "gray40", lwd = 1.5,
       xlab = "Distance (mm)", ylab = "Load (g)",
       main = sprintf("%s: Full Applied Mass Trace - Distance Based (%d cycles, %.0fmm total)", 
                      sample_name, n_cycles, max_distance),
       ylim = c(0, max_pressure * 1.05))
  
  # Add grid
  grid(col = "gray90", lty = 1)
  
  # Draw threshold lines
  abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 2)
  abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 2)
  
  # Highlight detected stable regions (if distance columns exist)
  if (n_cycles > 0 && "start_distance_mm" %in% colnames(cycles_real)) {
    for (j in 1:n_cycles) {
      cycle <- cycles_real[j, ]
      if (!is.na(cycle$start_distance_mm) && !is.na(cycle$end_distance_mm)) {
        rect(cycle$start_distance_mm, 0, cycle$end_distance_mm, max_pressure,
             col = rgb(0, 1, 0, 0.25), border = NA)
      }
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
  
  cat(sprintf("  Full trace plot (distance) saved to: %s\n", filepath))
}

# ===============================================================================
# FUNCTION: create_zoomed_plot_distance
# ===============================================================================

create_zoomed_plot_distance <- function(data, all_cycles, params, sample_name, output_dir_override = NULL) {
  
  # Check if distance data exists
  if (!"distance_mm" %in% colnames(data)) {
    cat("  Skipping zoomed distance plot (no distance_mm column)\n")
    return()
  }
  
  cat("\nCreating zoomed plot for passes 1-5 (distance-based)...\n")
  
  # Create filename
  out_dir <- if (!is.null(output_dir_override)) output_dir_override else params$output_dir
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(out_dir, paste0(sample_name, "_zoomed_distance.png"))
  
  # Zoom to passes 1-5: 0-450mm (5 passes × 90mm)
  zoom_start <- 0
  zoom_end <- 450
  
  # Filter data to zoom window
  zoom_data <- data[data$distance_mm >= zoom_start & data$distance_mm <= zoom_end, ]
  
  if (nrow(zoom_data) == 0) {
    cat("  Warning: No data in zoom window (0-450mm)\n")
    return()
  }
  
  # Filter cycles to zoom window
  cycles_real <- all_cycles[all_cycles$duration > 0, ]
  if ("start_distance_mm" %in% colnames(cycles_real)) {
    zoom_cycles <- cycles_real[
      !is.na(cycles_real$start_distance_mm) & 
      !is.na(cycles_real$end_distance_mm) &
      cycles_real$start_distance_mm < zoom_end & 
      cycles_real$end_distance_mm > zoom_start, 
    ]
  } else {
    zoom_cycles <- data.frame()
  }
  
  max_pressure <- max(zoom_data$load, na.rm = TRUE)
  
  # Create plot
  png(filepath, width = 1800, height = 600, res = 100)
  
  par(mar = c(4, 4, 3, 1))
  
  # Main plot
  plot(zoom_data$distance_mm, zoom_data$load, type = "l", col = "gray40", lwd = 1.5,
       xlab = "Distance (mm)", ylab = "Load (g)",
       main = sprintf("%s: Zoomed View - Passes 1-5 (0-450mm)", sample_name),
       ylim = c(0, max_pressure * 1.05))
  
  # Add grid
  grid(col = "gray90", lty = 1)
  
  # Draw threshold lines
  abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 2)
  abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 2)
  
  # Add pass boundaries (every 90mm)
  for (pass_num in 1:5) {
    pass_distance <- pass_num * 90
    abline(v = pass_distance, col = "gray70", lty = 3, lwd = 1)
    text(pass_distance - 45, max_pressure * 0.95, sprintf("Pass %d", pass_num), 
         cex = 0.8, col = "gray50")
  }
  
  # Highlight detected stable regions
  if (nrow(zoom_cycles) > 0) {
    for (j in 1:nrow(zoom_cycles)) {
      cycle <- zoom_cycles[j, ]
      rect(cycle$start_distance_mm, 0, cycle$end_distance_mm, max_pressure,
           col = rgb(0, 1, 0, 0.25), border = NA)
    }
  }
  
  # Add legend
  legend("topright", 
         legend = c("Load", sprintf("Stable thresholds (%.0f-%.0fg)", 
                                   params$stable_range_low, params$stable_range_high), 
                   "Detected stable", "Pass boundaries"),
         col = c("gray40", "blue", rgb(0, 1, 0, 0.5), "gray70"),
         lty = c(1, 2, 1, 3), lwd = c(1.5, 2, 8, 1), bty = "n", cex = 0.9)
  
  dev.off()
  
  cat(sprintf("  Zoomed plot (distance) saved to: %s\n", filepath))
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
  cat(sprintf("  Percentage stable (time): %.2f%%\n", stats$pct_stable))
  
  # Add distance-based statistics if available
  if (!is.na(stats$total_distance_mm)) {
    cat("\nDISTANCE-BASED STATISTICS:\n")
    cat(sprintf("  Total swabbing distance: %.1f mm\n", stats$total_distance_mm))
    cat(sprintf("  Theoretical maximum: %.0f mm (45 passes × 90mm)\n", 
                stats$theoretical_max_distance))
    cat(sprintf("  Expected from cycles: %.0f mm (%d cycles × 90mm)\n", 
                stats$expected_distance_from_cycles, stats$n_cycles_detected))
    cat(sprintf("  Distance efficiency (theoretical): %.1f%%\n", 
                stats$distance_efficiency_theoretical))
    cat(sprintf("  Distance efficiency (actual): %.1f%%\n", 
                stats$distance_efficiency_actual))
    cat(sprintf("  Mean distance per cycle: %.1f mm\n", stats$mean_distance_per_cycle))
    cat(sprintf("  Total stable distance: %.1f mm\n", stats$total_stable_distance_mm))
    cat(sprintf("  Percentage stable (distance): %.2f%%\n", stats$pct_stable_distance))
  }
  
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
  
  # ADAPTIVE THRESHOLD DISCOVERY (do this BEFORE distance calculation)
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
        # Threshold calculation failed (pressure_stats existed but produced
        # no usable thresholds). Respect adaptive_fallback_to_defaults.
        if (isTRUE(params$adaptive_fallback_to_defaults)) {
          sample_params$stable_range_low <- config$stable_low
          sample_params$stable_range_high <- config$stable_high
          cat("  Using default ranges (adaptive calculation failed)\n")
        } else {
          cat("  ERROR: Adaptive threshold calculation failed and adaptive_fallback_to_defaults is FALSE\n")
          return(list(
            sample = sample_name,
            target = config$target,
            success = FALSE,
            error = "Adaptive threshold calculation failed and fallback to defaults is disabled"
          ))
        }
      }
    } else {
      # Discovery failed (insufficient data for reliable stats). Respect
      # adaptive_fallback_to_defaults.
      if (isTRUE(params$adaptive_fallback_to_defaults)) {
        sample_params$stable_range_low <- config$stable_low
        sample_params$stable_range_high <- config$stable_high
        cat(sprintf("  Using default ranges: %.1f - %.1fg (insufficient data for adaptation)\n",
                    config$stable_low, config$stable_high))
      } else {
        cat("  ERROR: Adaptive threshold discovery failed and adaptive_fallback_to_defaults is FALSE\n")
        return(list(
          sample = sample_name,
          target = config$target,
          success = FALSE,
          error = "Adaptive threshold discovery failed and fallback to defaults is disabled"
        ))
      }
    }
  } else {
    # Adaptive thresholds disabled, use config defaults
    sample_params$stable_range_low <- config$stable_low
    sample_params$stable_range_high <- config$stable_high
    cat(sprintf("  Using default ranges: %.1f - %.1fg\n",
                config$stable_low, config$stable_high))
  }
  
  # NOW calculate cumulative distance using the stable range
  cat("\nCalculating cumulative swabbing distance...\n")
  data <- calculate_cumulative_distance(
    data = data,
    target_pressure = config$target,
    stable_range_low = sample_params$stable_range_low,
    stable_range_high = sample_params$stable_range_high,
    gcode_params = GCODE_PARAMS,
    verbose = TRUE  # Show validation diagnostics
  )
  
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
    
    # Combine all cycles (handle potential column mismatches)
    # Get all unique column names
    all_cols <- unique(c(colnames(initial_cycles), colnames(remaining_cycles)))
    
    # Add missing columns to each dataframe
    for (col in all_cols) {
      if (!(col %in% colnames(initial_cycles))) {
        initial_cycles[[col]] <- NA
      }
      if (!(col %in% colnames(remaining_cycles))) {
        remaining_cycles[[col]] <- NA
      }
    }
    
    # Now rbind with matching columns
    all_cycles <- rbind(initial_cycles, remaining_cycles)
    all_cycles <- all_cycles[order(all_cycles$start_time), ]
    
    # Calculate statistics
    stats <- calculate_statistics(all_cycles, data, sample_params, GCODE_PARAMS)
    
    # Create time-based full trace plot
    create_full_trace_plot(data, all_cycles, sample_params, sample_name)
    
    # Create distance-based full trace plot
    create_full_trace_plot_distance(data, all_cycles, sample_params, sample_name)
    
    # Create distance-based zoomed plot (passes 1-5)
    create_zoomed_plot_distance(data, all_cycles, sample_params, sample_name)
    
    # Side-by-side comparison folders: same distance-based plots duplicated
    # with vs. without non-contact transients suppressed (see
    # mask_noncontact_data()), so both versions can be inspected side by
    # side without altering the standard per-sample output above.
    with_transients_dir <- file.path(PARAMS$transient_comparison_dir, "WithTransients", sample_name)
    without_transients_dir <- file.path(PARAMS$transient_comparison_dir, "NoTransients", sample_name)
    
    create_full_trace_plot_distance(data, all_cycles, sample_params, sample_name,
                                     output_dir_override = with_transients_dir)
    create_zoomed_plot_distance(data, all_cycles, sample_params, sample_name,
                                 output_dir_override = with_transients_dir)
    
    data_masked <- mask_noncontact_data(data)
    create_full_trace_plot_distance(data_masked, all_cycles, sample_params, sample_name,
                                     output_dir_override = without_transients_dir)
    create_zoomed_plot_distance(data_masked, all_cycles, sample_params, sample_name,
                                 output_dir_override = without_transients_dir)
    
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
      stable_range_low = sample_params$stable_range_low,
      stable_range_high = sample_params$stable_range_high,
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

load_raw_trace_data <- function(sample_id, data_dir, align_time = TRUE, align_to_first_zero = FALSE, align_to_first_max = FALSE,
                                 compute_distance = FALSE, stable_range_low = NULL, stable_range_high = NULL,
                                 target_pressure = NA, gcode_params = NULL) {
  # Load and optionally align raw pressure trace data for plotting
  # align_time: if TRUE, shift time to start at 0; if FALSE, keep original elapsed_sec values
  # align_to_first_zero: if TRUE, align to first zero pressure reading (overrides align_time)
  # align_to_first_max: if TRUE, align to first local maximum after skip_startup
  # compute_distance: if TRUE, recompute cumulative distance_mm using the sample's adaptive
  #   stable_range_low/high (as originally discovered for that sample in process_sample()).
  #   Distance naturally starts at 0 for every sample's trace, so no time-alignment is
  #   needed when plotting against distance instead of time.
  
  file_path <- file.path(data_dir, paste0(sample_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(sprintf("Raw data file not found: %s", file_path))
    return(NULL)
  }
  
  # Use existing clean_and_read_csv function
  data <- clean_and_read_csv(file_path)
  
  if (compute_distance) {
    if (is.null(stable_range_low) || is.null(stable_range_high) || is.null(gcode_params)) {
      warning(sprintf("compute_distance requested for %s but stable_range_low/high or gcode_params missing; skipping distance calculation", sample_id))
    } else {
      data <- calculate_cumulative_distance(
        data = data,
        target_pressure = target_pressure,
        stable_range_low = stable_range_low,
        stable_range_high = stable_range_high,
        gcode_params = gcode_params,
        verbose = FALSE
      )
    }
  }
  
  # Extract relevant columns (keep distance_mm/is_contact if they were computed)
  data <- data %>%
    select(elapsed_sec, load, dplyr::any_of(c("distance_mm", "is_contact"))) %>%
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

extract_stable_regions_for_overlay <- function(sample_id, data_dir, output_dir,
                                                stable_range_low, stable_range_high,
                                                target_pressure, gcode_params) {
  # Extract all stable regions with normalized distance for overlay plotting
  
  # Read detected cycles
  cycles_file <- file.path(output_dir, sample_id, paste0(sample_id, "_detected_cycles.csv"))
  
  if (!file.exists(cycles_file)) {
    warning(sprintf("Detected cycles file not found: %s", cycles_file))
    return(NULL)
  }
  
  cycles <- read.csv(cycles_file, stringsAsFactors = FALSE)
  
  # Filter out forced cycles (NA mean_load) and cycles missing distance info
  cycles <- cycles %>% filter(!is.na(mean_load), !is.na(start_distance_mm))
  
  if (nrow(cycles) == 0) {
    return(NULL)
  }
  
  # Load raw data (DO NOT align time - use original elapsed_sec to match cycle start/end
  # times) and recompute distance_mm using this sample's adaptive stable range.
  raw_data <- load_raw_trace_data(
    sample_id, data_dir, align_time = FALSE,
    compute_distance = TRUE,
    stable_range_low = stable_range_low,
    stable_range_high = stable_range_high,
    target_pressure = target_pressure,
    gcode_params = gcode_params
  )
  
  if (is.null(raw_data) || !("distance_mm" %in% colnames(raw_data))) {
    return(NULL)
  }
  
  # Extract each stable region
  all_regions <- list()
  
  for (i in 1:nrow(cycles)) {
    cycle <- cycles[i, ]
    
    # Filter data within this stable region (time bounds still identify the correct rows;
    # distance_in_region is what gets plotted)
    region_data <- raw_data %>%
      filter(time_sec >= cycle$start_time & time_sec <= cycle$end_time) %>%
      mutate(
        distance_in_region = distance_mm - cycle$start_distance_mm,
        cycle_num = cycle$cycle_num,
        region_id = i
      ) %>%
      select(sample_id, cycle_num, region_id, distance_in_region, pressure_g)
    
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

plot_full_traces_comparison <- function(target_pressure, sample_results, metadata_df, data_dir, params, gcode_params,
                                          suppress_transients = FALSE, output_dir_override = NULL) {
  # Create full trace comparison plot (distance-based) for all samples at target pressure
  
  cat(sprintf("  Loading %dg samples: ", target_pressure))
  
  # Filter samples by target pressure - only successful samples have a discovered
  # stable range available to recompute distance with
  target_samples <- names(sample_results)[sapply(sample_results, function(r) {
    !is.null(r) && isTRUE(r$success) && r$target == target_pressure
  })]
  cat(paste(target_samples, collapse = ", "), "\n")
  
  # Load raw traces with recomputed cumulative distance, then re-zero each sample to
  # its own detected Cycle 1 start. Each ASTRA run is started manually (both the data
  # logger and the swab motion), so raw distance_mm = 0 (the first CSV row) corresponds
  # to a slightly different point in the swab cycle for every sample - without this,
  # pass peaks don't line up across samples even though the underlying cycles are the
  # same nominal 90mm apart.
  all_traces <- list()
  for (sample_id in target_samples) {
    result <- sample_results[[sample_id]]
    
    trace <- load_raw_trace_data(
      sample_id, data_dir, align_time = FALSE,
      compute_distance = TRUE,
      stable_range_low = result$stable_range_low,
      stable_range_high = result$stable_range_high,
      target_pressure = result$target,
      gcode_params = gcode_params
    )
    
    if (!is.null(trace) && "distance_mm" %in% colnames(trace)) {
      # Align to Cycle 1's PEAK pressure rather than the point it enters the (narrower)
      # adaptive stable band. The ramp-up overshoot typically exceeds stable_range_high
      # and only re-enters the band on the way back down, so "start_distance_mm" marks
      # a point strictly after the peak; the peak itself is a sharper, more consistently
      # identifiable physical event (the swab head impact) to align samples on. Search a
      # window from just before Cycle 1's recognized start through its end to find it.
      cycle1 <- result$cycles[result$cycles$cycle_num == 1 & !is.na(result$cycles$start_distance_mm), ]
      distance_offset <- 0
      if (nrow(cycle1) > 0) {
        search_start <- cycle1$start_time[1] - 5
        search_end <- cycle1$end_time[1] + 1
        peak_window <- trace[trace$time_sec >= search_start & trace$time_sec <= search_end, ]
        if (nrow(peak_window) > 0) {
          distance_offset <- peak_window$distance_mm[which.max(peak_window$pressure_g)]
        } else {
          distance_offset <- cycle1$start_distance_mm[1]
        }
      }
      trace$distance_mm <- trace$distance_mm - distance_offset
      
      # Use the complete raw trace (matching create_full_trace_plot_distance() for a
      # single sample) so overlaying multiple samples here looks like stacking those
      # per-sample plots on top of each other. This does include brief drops toward 0g
      # at each pass boundary (lift-off/return collapses to a nearly flat distance, so
      # those points render as thin near-vertical excursions) - this is expected and
      # matches the individual per-sample distance plots, not a plotting bug.
      meta_row <- metadata_df[metadata_df$RunID == sample_id, ]
      if (nrow(meta_row) > 0) {
        trace$wet_dry <- ifelse(meta_row$Solvent_level == "present", "wet", "dry")
      } else {
        trace$wet_dry <- "unknown"
      }
      all_traces[[sample_id]] <- trace
      cat(sprintf("    %s: %d data points (%s, cycle-1 offset %.1fmm)\n",
                  sample_id, nrow(trace), trace$wet_dry[1], distance_offset))
    } else {
      cat(sprintf("    %s: skipped (no distance data)\n", sample_id))
    }
  }
  
  if (length(all_traces) == 0) {
    warning("No traces loaded for plotting")
    return(NULL)
  }
  
  # Combine all traces, then trim to the display window up front (rather than letting
  # scale_x_continuous() silently drop out-of-range rows later, which is wasteful for
  # multi-thousand-mm traces and spams the console with "Removed N rows" warnings).
  dist_limit <- params$comparison_distance_limit_mm
  plot_data <- bind_rows(all_traces) %>%
    filter(distance_mm >= 0, distance_mm <= dist_limit)
  
  # Suppress non-contact transients for display only, AFTER the Cycle-1-peak alignment
  # above (which needs the true, unmasked peak - the peak typically exceeds
  # stable_range_high, i.e. is_contact == FALSE at that exact point, so masking before
  # alignment would break which.max() into finding the wrong, lower local peak).
  if (isTRUE(suppress_transients)) {
    plot_data <- mask_noncontact_data(plot_data)
  }
  
  n_samples <- length(unique(plot_data$sample_id))
  ncol_facet <- min(params$comparison_ncol, n_samples)
  n_rows <- ceiling(n_samples / ncol_facet)
  
  # Small multiples: one panel per sample (facet_wrap), rather than overlaying every
  # sample's trace on a single panel. With ~10-13 samples, a single overlaid panel
  # becomes an illegible "spaghetti" of overlapping sawtooth lines where individual
  # samples cannot be visually distinguished even with distinct colors. Faceting
  # instead makes each sample's trace independently legible, and - critically - using
  # the *default fixed* y-axis scale across panels (not scales = "free") means a
  # sample sitting at a different absolute pressure level than its peers (e.g. a
  # 50g-target sample that only reached ~35g) is immediately, visually obvious by
  # comparing panel heights, without needing to read any numbers.
  #
  # Color now only needs to distinguish wet vs dry condition (the facet panel itself
  # identifies the sample), so a flat 2-color scheme is used instead of the old
  # per-sample recycled palette (see PLOTTING_PARAMS comment for the color-collision
  # bug this replaces).
  p <- ggplot(plot_data, aes(x = distance_mm, y = pressure_g, color = wet_dry, group = sample_id)) +
    geom_line(linewidth = params$trace_line_width, alpha = params$trace_alpha) +
    scale_color_manual(
      values = c(wet = params$color_wet, dry = params$color_dry),
      name = "Condition"
    ) +
    scale_x_continuous(breaks = seq(0, dist_limit, dist_limit / 5)) +
    facet_wrap(~ sample_id, ncol = ncol_facet) +
    labs(
      title = sprintf("%dg Samples - Full Applied Mass Traces (Distance-Based)", target_pressure),
      subtitle = sprintf(
        "Blue = Wet | Green = Dry  |  First %.0fmm, aligned to each sample's Cycle 1 start  |  Shared y-axis scale across panels%s",
        dist_limit,
        if (isTRUE(suppress_transients)) "  |  Non-contact transients suppressed" else ""
      ),
      x = "Cumulative swabbing distance (mm)",
      y = "Applied Mass (g)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "top",
      strip.text = element_text(face = "bold")
    )
  
  # Save plot - dimensions scale with the number of panels so each stays legible
  # regardless of whether this is the 10-sample 50g group or the 13-sample 200g group.
  plot_width <- ncol_facet * params$comparison_panel_width_in
  plot_height <- n_rows * params$comparison_panel_height_in + params$comparison_extra_height_in
  out_dir <- if (!is.null(output_dir_override)) output_dir_override else params$output_dir
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  filename <- file.path(out_dir, sprintf("%dg_samples_full_traces.png", target_pressure))
  if (isTRUE(suppress_transients)) {
    # mask_noncontact_data() intentionally introduces NA gaps in the line data;
    # ggplot2 reports these as "Removed N rows containing missing values" on
    # every such save, which is expected here (not a real problem) and would
    # otherwise spam the console once per pressure level.
    suppressWarnings(
      ggsave(filename, p, width = plot_width, height = plot_height, dpi = params$dpi, limitsize = FALSE)
    )
  } else {
    ggsave(filename, p, width = plot_width, height = plot_height, dpi = params$dpi, limitsize = FALSE)
  }
  
  cat(sprintf("  Saved: %s\n\n", basename(filename)))
  
  return(filename)
}

# ===============================================================================
# FUNCTION: plot_stable_regions_overlay
# ===============================================================================

plot_stable_regions_overlay <- function(sample_id, sample_result, data_dir, output_dir, params, gcode_params) {
  # Create overlay plot (distance-based) of all stable regions for a single sample
  
  # Extract stable regions
  regions_data <- extract_stable_regions_for_overlay(
    sample_id, data_dir, output_dir,
    stable_range_low = sample_result$stable_range_low,
    stable_range_high = sample_result$stable_range_high,
    target_pressure = sample_result$target,
    gcode_params = gcode_params
  )
  
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
  dist_limit <- params$overlay_distance_limit_mm
  p <- ggplot(regions_data, aes(x = distance_in_region, y = pressure_g, group = cycle_num, color = cycle_num)) +
    geom_line(linewidth = params$overlay_line_width, alpha = params$overlay_alpha) +
    # direction = -1: viridis's default is dark(low)->light(high), but the
    # plot's own subtitle says "Light (early) -> Dark (late)" (Cycle # low =
    # early) -- reversed here so the actual color direction matches that
    # caption instead of contradicting it.
    scale_color_viridis_c(name = "Cycle #", direction = -1) +
    scale_x_continuous(limits = c(0, dist_limit), breaks = seq(0, dist_limit, dist_limit / 6)) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title = sprintf("%s - Stable Region Overlay (Distance-Based)", sample_id),
      subtitle = "Color intensity: Light (early) -> Dark (late)",
      x = "Distance in stable region (mm)",
      y = "Applied Mass (g)"
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
# FUNCTION: plot_batch_qc_overview
# ===============================================================================
# Batch-level QC visualization: one row per successful sample, showing
# Mean_Pressure against its own Target, faceted by Target level (50g/200g
# evaluated separately since their absolute pressures aren't comparable).
# A shaded band marks the +/-mean_pressure_outlier_pct acceptance range (the
# same threshold used for Flag_15Pct_Outlier in batch_summary.csv) so it's
# visually obvious *why* a point is flagged, and points are colored by
# Flag_15Pct_Outlier. This is the visual counterpart to that CSV column -
# previously the only way to see how samples compare to target was reading
# the table; this makes outliers (as of this writing: PILOT_014, PILOT_018,
# PILOT_011, PILOT_030) visible at a glance.

plot_batch_qc_overview <- function(summary_data, params, outlier_pct) {
  
  plot_df <- summary_data[summary_data$Success == TRUE & !is.na(summary_data$Mean_Pressure), ]
  
  if (nrow(plot_df) == 0) {
    warning("No successful samples available for QC overview plot")
    return(NULL)
  }
  
  plot_df <- plot_df[order(plot_df$Target, plot_df$Mean_Pressure), ]
  plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample)
  plot_df$Lower <- plot_df$Target * (1 - outlier_pct)
  plot_df$Upper <- plot_df$Target * (1 + outlier_pct)
  plot_df$Flag_Label <- factor(
    ifelse(isTRUE(plot_df$Flag_15Pct_Outlier) | plot_df$Flag_15Pct_Outlier == TRUE,
           sprintf("Outside %.0f%% of Target", outlier_pct * 100),
           sprintf("Within %.0f%% of Target", outlier_pct * 100)),
    levels = c(sprintf("Within %.0f%% of Target", outlier_pct * 100),
               sprintf("Outside %.0f%% of Target", outlier_pct * 100))
  )
  
  band_df <- unique(plot_df[, c("Target", "Lower", "Upper")])
  target_df <- unique(plot_df[, "Target", drop = FALSE])
  
  n_max_rows <- max(table(plot_df$Target))
  plot_height <- params$qc_overview_base_height_in + n_max_rows * params$qc_overview_height_per_row_in
  
  p <- ggplot(plot_df, aes(x = Mean_Pressure, y = Sample)) +
    geom_rect(data = band_df, aes(xmin = Lower, xmax = Upper), ymin = -Inf, ymax = Inf,
              fill = "gray85", alpha = 0.6, inherit.aes = FALSE) +
    geom_vline(data = target_df, aes(xintercept = Target), linetype = "dashed", color = "gray30") +
    geom_segment(aes(x = Target, xend = Mean_Pressure, y = Sample, yend = Sample), color = "gray50") +
    geom_point(aes(color = Flag_Label), size = 3) +
    scale_color_manual(
      values = setNames(
        # Bluish Green/Vermillion match the thesis-wide shared palette's
        # pal_qc_status$PASS/FAIL (repo-root thesis_palette.R, Okabe-Ito) --
        # inlined literally rather than source()'d, since this script is
        # deliberately single-file/self-contained (see header).
        c("#009E73", "#D55E00"),
        c(sprintf("Within %.0f%% of Target", outlier_pct * 100),
          sprintf("Outside %.0f%% of Target", outlier_pct * 100))
      ),
      name = NULL
    ) +
    facet_wrap(~ Target, scales = "free", labeller = labeller(Target = function(x) paste0(x, "g target"))) +
    labs(
      title = "Batch QC Overview - Mean Applied Mass vs Target",
      subtitle = sprintf(
        "Dashed line = Target  |  Shaded band = within %.0f%% of Target  |  Red = flagged (Flag_15Pct_Outlier)",
        outlier_pct * 100
      ),
      x = "Mean Applied Mass (stable regions, g)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  filename <- file.path(params$output_dir, "batch_qc_overview.png")
  ggsave(filename, p, width = params$qc_overview_width, height = plot_height, dpi = params$dpi, limitsize = FALSE)
  
  cat(sprintf("  Saved: %s\n\n", basename(filename)))
  
  return(filename)
}

# ===============================================================================
# FUNCTION: create_all_comparison_plots
# ===============================================================================

create_all_comparison_plots <- function(results, metadata_df, data_dir, output_dir, params, gcode_params,
                                          summary_data = NULL, outlier_pct = 0.15) {
  # Orchestrate generation of all plots
  
  # Create output directory
  dir.create(params$output_dir, showWarnings = FALSE, recursive = TRUE)
  
  generated_files <- list()
  
  # Generate full trace comparison plots (distance-based)
  cat("Creating full trace comparison plots (distance-based)...\n")
  
  # 50g samples
  file_50g <- plot_full_traces_comparison(50, results, metadata_df, data_dir, params, gcode_params)
  if (!is.null(file_50g)) {
    generated_files <- c(generated_files, file_50g)
  }
  
  # 200g samples
  file_200g <- plot_full_traces_comparison(200, results, metadata_df, data_dir, params, gcode_params)
  if (!is.null(file_200g)) {
    generated_files <- c(generated_files, file_200g)
  }
  
  # Side-by-side comparison folders: same small-multiples comparison plots
  # duplicated with vs. without non-contact transients suppressed (see
  # mask_noncontact_data() / PARAMS$transient_comparison_dir), so both
  # versions can be inspected side by side without altering the standard
  # Figures/ output above.
  cat("Creating with/without-transients comparison plots...\n")
  
  with_figs_dir <- file.path(PARAMS$transient_comparison_dir, "WithTransients", "Figures")
  without_figs_dir <- file.path(PARAMS$transient_comparison_dir, "NoTransients", "Figures")
  
  for (target_pressure in c(50, 200)) {
    file_with <- plot_full_traces_comparison(target_pressure, results, metadata_df, data_dir, params, gcode_params,
                                              suppress_transients = FALSE, output_dir_override = with_figs_dir)
    if (!is.null(file_with)) generated_files <- c(generated_files, file_with)
    
    file_without <- plot_full_traces_comparison(target_pressure, results, metadata_df, data_dir, params, gcode_params,
                                                 suppress_transients = TRUE, output_dir_override = without_figs_dir)
    if (!is.null(file_without)) generated_files <- c(generated_files, file_without)
  }
  
  # Generate stable region overlay plots (distance-based)
  cat("Creating stable region overlay plots (distance-based)...\n")
  
  for (sample_id in names(results)) {
    result <- results[[sample_id]]
    
    if (isTRUE(result$success)) {
      file_overlay <- plot_stable_regions_overlay(sample_id, result, data_dir, output_dir, params, gcode_params)
      
      if (!is.null(file_overlay)) {
        n_regions <- result$stats$n_cycles_detected
        cat(sprintf("  %s: %d stable regions\n", sample_id, n_regions))
        generated_files <- c(generated_files, file_overlay)
      }
    }
  }
  
  # Generate batch-level QC overview plot (Mean_Pressure vs Target, all samples)
  if (!is.null(summary_data)) {
    cat("Creating batch QC overview plot...\n")
    file_qc <- plot_batch_qc_overview(summary_data, params, outlier_pct)
    if (!is.null(file_qc)) {
      generated_files <- c(generated_files, file_qc)
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
    Stable_Time_Pct = numeric(),
    Total_Distance_mm = numeric(),
    Stable_Distance_Pct = numeric(),
    Distance_Efficiency_Pct = numeric(),
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
        Stable_Time_Pct = result$stats$pct_stable,
        Total_Distance_mm = result$stats$total_distance_mm,
        Stable_Distance_Pct = result$stats$pct_stable_distance,
        Distance_Efficiency_Pct = result$stats$distance_efficiency_actual,
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
        Stable_Time_Pct = NA,
        Total_Distance_mm = NA,
        Stable_Distance_Pct = NA,
        Distance_Efficiency_Pct = NA,
        Mean_Pressure = NA,
        RSD_Pressure = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Flag samples whose Mean_Pressure deviates from its own Target by more
  # than PARAMS$mean_pressure_outlier_pct (default 15%), evaluated per
  # sample against its own Target directly (an absolute threshold, not a
  # group-relative statistic, so it is not subject to outlier masking).
  # At the default 15% threshold it flags PILOT_014 (-30.0%), PILOT_018
  # (-18.6%), PILOT_011 (+15.7%), and PILOT_030 (-15.2%). No
  # physical/mechanical cause has been independently confirmed for any
  # of these four - this is a measured statistical deviation only, not a
  # diagnosis. Failed samples get NA.
  summary_data$Flag_15Pct_Outlier <- ifelse(
    summary_data$Success,
    abs(summary_data$Mean_Pressure - summary_data$Target) / summary_data$Target >
      PARAMS$mean_pressure_outlier_pct,
    NA
  )
  
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
        params = PLOTTING_PARAMS,
        gcode_params = GCODE_PARAMS,
        summary_data = summary_data,
        outlier_pct = PARAMS$mean_pressure_outlier_pct
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

# ===============================================================================
# FUNCTION: test_single_sample
# ===============================================================================
# Quick single-sample smoke test, replacing the old standalone
# test_distance_single_sample.R / test_distance_PILOT_001.R scripts.
# Runs process_sample() for one sample and prints a short report,
# without generating the full batch summary/comparison plots.

test_single_sample <- function(sample_id, sample_config, params) {
  
  cat("\n========================================\n")
  cat("SINGLE-SAMPLE TEST\n")
  cat(sprintf("Sample: %s\n", sample_id))
  cat("========================================\n\n")
  
  if (!(sample_id %in% names(sample_config))) {
    cat(sprintf("ERROR: Sample '%s' not found in SAMPLE_CONFIG\n", sample_id))
    cat("Available samples:\n")
    print(names(sample_config))
    stop("Sample not found")
  }
  
  config <- sample_config[[sample_id]]
  
  cat(sprintf("Target pressure: %dg\n", config$target))
  cat(sprintf("Default stable range: %.1f - %.1fg\n\n", config$stable_low, config$stable_high))
  
  result <- process_sample(sample_id, config, params)
  
  if (!is.null(result) && isTRUE(result$success)) {
    cat("\n========================================\n")
    cat("TEST SUCCESSFUL!\n")
    cat("========================================\n\n")
    
    cat("TIME-BASED METRICS:\n")
    cat(sprintf("  Cycles detected: %d / %d\n", result$stats$n_cycles_detected, result$stats$n_cycles_total))
    cat(sprintf("  Stable time: %.2f%%\n", result$stats$pct_stable))
    cat(sprintf("  Total time: %.1f s\n", result$stats$total_pass_time))
    cat(sprintf("  Stable duration: %.1f s\n\n", result$stats$total_stable_time))
    
    cat("DISTANCE-BASED METRICS:\n")
    if (!is.na(result$stats$total_distance_mm)) {
      cat(sprintf("  Total swabbing distance: %.1f mm\n", result$stats$total_distance_mm))
      cat(sprintf("  Theoretical maximum: %.0f mm\n", result$stats$theoretical_max_distance))
      cat(sprintf("  Expected from cycles: %.0f mm\n", result$stats$expected_distance_from_cycles))
      cat(sprintf("  Distance efficiency (actual): %.1f%%\n", result$stats$distance_efficiency_actual))
      cat(sprintf("  Mean distance per cycle: %.1f mm\n", result$stats$mean_distance_per_cycle))
      cat(sprintf("  Stable distance: %.1f%%\n\n", result$stats$pct_stable_distance))
    } else {
      cat("  No distance data calculated\n\n")
    }
    
    cat("PRESSURE STATISTICS:\n")
    cat(sprintf("  Mean pressure: %.1f g\n", result$stats$mean_pressure_stable))
    cat(sprintf("  RSD: %.2f%%\n\n", result$stats$rsd_pressure_stable))
    
    # Check output files
    output_dir <- file.path(params$output_dir, sample_id)
    cat("OUTPUT FILES:\n")
    
    files_to_check <- c(
      paste0(sample_id, "_full_trace_time.png"),
      paste0(sample_id, "_full_trace_distance.png"),
      paste0(sample_id, "_zoomed_distance.png"),
      paste0(sample_id, "_detected_cycles.csv"),
      paste0(sample_id, "_summary_statistics.txt")
    )
    
    for (fname in files_to_check) {
      fpath <- file.path(output_dir, fname)
      exists_msg <- if (file.exists(fpath)) "OK" else "MISSING"
      cat(sprintf("  [%s] %s\n", exists_msg, fname))
    }
    
    cat(sprintf("\nOutput directory: %s\n", output_dir))
    
  } else {
    cat("\n========================================\n")
    cat("TEST FAILED!\n")
    cat("========================================\n")
    if (!is.null(result)) {
      cat(sprintf("Result returned but success=FALSE (error: %s)\n",
                  if (!is.null(result$error)) result$error else "unknown"))
    } else {
      cat("Result is NULL\n")
    }
  }
  
  return(result)
}

# ===============================================================================
# ENTRY POINT (only runs if script is executed directly, not sourced)
# ===============================================================================

if (sys.nframe() == 0) {
  if (identical(RUN_MODE, "single")) {
    single_result <- test_single_sample(SINGLE_TEST_SAMPLE, SAMPLE_CONFIG, PARAMS)
  } else {
    results <- main_batch()
  }
}
