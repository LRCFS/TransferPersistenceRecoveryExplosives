# Time-Based Periodic Stable Contact Detection - PILOT_001 (200g)
# Author: OpenCode
# Date: 2026-07-17
# 
# This script implements a time-based approach to detect stable contact regions
# by leveraging the periodic nature of the swabbing motion.

# ===============================================================================
# PARAMETERS
# ===============================================================================

PARAMS <- list(
  # File paths
  data_file = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_001.csv",
  output_dir = "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs/time_based",
  
  # Detection parameters
  target_pressure = 200,           # Target pressure in grams
  stable_range_low = 185,          # Lower bound for stable region (g)
  stable_range_high = 200,         # Upper bound for stable region (g)
  min_stable_duration = 0.3,       # Minimum duration to consider stable (s)
  
  # Period estimation
  skip_startup_sec = 3,            # Skip first N seconds (startup period)
  n_cycles_for_period = 5,         # Number of initial cycles to estimate period
  period_tolerance_factor = 2,     # Tolerance = factor × period_sd
  min_period_tolerance = 2.0,      # Minimum tolerance window (s) - wide enough to capture full cycle
  use_median_period = TRUE,        # Use median instead of mean for robustness
  
  # Analysis
  max_time_for_initial_detection = 60  # Search first 60s for initial cycles (after startup)
)

# ===============================================================================
# FUNCTION: detect_stable_by_threshold
# ===============================================================================
# Detects stable regions within a time window based on load thresholds
#
# Args:
#   data: data frame with columns 'elapsed_sec' and 'load'
#   start_time: start of search window (seconds)
#   end_time: end of search window (seconds)
#   low_thresh: lower threshold for stable load (g)
#   high_thresh: upper threshold for stable load (g)
#   min_duration: minimum duration to be considered stable (s)
#
# Returns:
#   data frame with columns: cycle_num, start_time, end_time, duration, mean_load

detect_stable_by_threshold <- function(data, start_time, end_time, 
                                       low_thresh, high_thresh, min_duration) {
  
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
  
  # Keep the longest segment in this window (most likely to be the true stable period)
  regions <- regions[which.max(regions$duration), ]
  
  return(regions)
}

# ===============================================================================
# FUNCTION: detect_initial_cycles
# ===============================================================================
# Detects the first N cycles to establish the period
#
# Args:
#   data: data frame with columns 'elapsed_sec' and 'load'
#   n_cycles: number of cycles to detect
#   params: parameter list
#
# Returns:
#   data frame with detected cycles

detect_initial_cycles <- function(data, n_cycles, params) {
  
  cat(sprintf("PHASE 1: Detecting first %d cycles (skipping first %.1fs)...\n", 
              n_cycles, params$skip_startup_sec))
  cat("  Strategy: Search consecutive windows with expected period\n")
  
  all_cycles <- data.frame()
  
  # First, find the VERY FIRST cycle to establish starting point
  first_region <- detect_stable_by_threshold(
    data = data,
    start_time = params$skip_startup_sec,
    end_time = params$skip_startup_sec + params$max_time_for_initial_detection,
    low_thresh = params$stable_range_low,
    high_thresh = params$stable_range_high,
    min_duration = params$min_stable_duration
  )
  
  if (is.null(first_region)) {
    stop("Could not find any stable region after startup. Check parameters.")
  }
  
  first_region$cycle_num <- 1
  all_cycles <- rbind(all_cycles, first_region)
  
  cat(sprintf("  Cycle %d: %.3fs - %.3fs (duration: %.3fs, mean load: %.1fg)\n",
              1, first_region$start_time, first_region$end_time, 
              first_region$duration, first_region$mean_load))
  
  # Now search for subsequent cycles using a crude period estimate
  # We expect ~10s period based on observations
  crude_period <- 10  # seconds - we'll refine this after finding a few cycles
  search_window_size <- 3  # seconds - how wide to search around expected time
  
  # For the very first cycle, search just 5s ahead (not 10s) to avoid overshooting
  first_search_end <- first_region$start_time + 5
  
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
    
    # Update last start for next iteration
    last_start <- region$start_time
    
    # After finding a few cycles, refine the period estimate
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
# Estimates the period from detected cycles
#
# Args:
#   cycles: data frame with detected cycles (must have start_time column)
#
# Returns:
#   list with mean_period, sd_period, cv_period

estimate_period <- function(cycles, params) {
  
  if (nrow(cycles) < 2) {
    stop("Need at least 2 cycles to estimate period")
  }
  
  cat("\nPHASE 2: Estimating period...\n")
  
  # Calculate inter-cycle intervals (start to start)
  intervals <- diff(cycles$start_time)
  
  # Use median if specified (more robust to outliers)
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
    period = central_period,  # Either median or mean
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
# Predicts and detects remaining cycles based on established period
#
# Args:
#   data: data frame with columns 'elapsed_sec' and 'load'
#   initial_cycles: data frame with initial detected cycles
#   period_stats: list with period statistics
#   params: parameter list
#
# Returns:
#   data frame with all remaining detected cycles

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
    
    # Predict next cycle start using STRICT periodicity
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
      # STRICT MODE: If we can't find it, use the predicted time with zero duration
      # This flags it for attention but maintains strict periodicity
      cat(sprintf("  Cycle %d: FORCED at predicted %.3fs (not detected in data)\n",
                  cycle_count, predicted_start))
      
      # Create a placeholder entry
      region <- data.frame(
        start_time = predicted_start,
        end_time = predicted_start,
        mean_load = NA,
        n_points = 0,
        duration = 0,
        cycle_num = cycle_count
      )
      
      missed_count <- missed_count + 1
      
      # Update for next iteration using predicted position
      last_start <- predicted_start
    } else {
      # Found a real region
      region$cycle_num <- cycle_count
      
      # Update for next iteration using actual detected position
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
# Calculates statistics for detected stable regions
#
# Args:
#   all_cycles: data frame with all detected cycles
#   data: original data frame
#   params: parameter list
#
# Returns:
#   list with statistics

calculate_statistics <- function(all_cycles, data, params) {
  
  cat("\nPHASE 4: Calculating statistics...\n")
  
  # Separate real detections from forced placeholders
  real_cycles <- all_cycles[all_cycles$duration > 0, ]
  forced_cycles <- all_cycles[all_cycles$duration == 0, ]
  
  # Total stable time (only from real detections)
  total_stable_time <- sum(real_cycles$duration, na.rm = TRUE)
  
  # Total time in passes (where load > 0)
  total_pass_time <- sum(data$load > 0) * median(diff(data$elapsed_sec), na.rm = TRUE)
  
  # Percentage
  pct_stable <- (total_stable_time / total_pass_time) * 100
  
  # Cycle statistics (only real cycles)
  n_cycles_total <- nrow(all_cycles)
  n_cycles_detected <- nrow(real_cycles)
  n_cycles_forced <- nrow(forced_cycles)
  mean_duration <- mean(real_cycles$duration, na.rm = TRUE)
  sd_duration <- sd(real_cycles$duration, na.rm = TRUE)
  
  # PRESSURE STATISTICS
  # Maximum across all data
  max_pressure_all <- max(data$load, na.rm = TRUE)
  
  # For stable regions, get all individual pressure points
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
  
  # RSD / CV (Relative Standard Deviation / Coefficient of Variation)
  rsd_pressure_stable <- (sd_pressure_stable / mean_pressure_stable) * 100
  cv_pressure_stable <- rsd_pressure_stable  # CV and RSD are the same
  
  # Mean of cycle means (for comparison)
  mean_of_cycle_means <- mean(real_cycles$mean_load, na.rm = TRUE)
  
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
    cv_pressure_stable = cv_pressure_stable,
    min_pressure_stable = min_pressure_stable,
    max_pressure_stable = max_pressure_stable,
    mean_of_cycle_means = mean_of_cycle_means
  ))
}

# ===============================================================================
# FUNCTION: create_plots
# ===============================================================================
# Creates visualization plots
#
# Args:
#   data: original data frame
#   all_cycles: data frame with all detected cycles
#   params: parameter list
#   stats: statistics list

create_plots <- function(data, all_cycles, params, stats) {
  
  cat("\nPHASE 5: Creating plots...\n")
  
  # Create output directory
  dir.create(params$output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # --- Plot 1: Full time series ---
  cat("  Creating full time series plot...\n")
  png(file.path(params$output_dir, "PILOT_001_time_based_full.png"), 
      width = 1400, height = 800, res = 100)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
  
  plot(data$elapsed_sec, data$load, type = "l", col = "gray50", lwd = 1,
       xlab = "Time (s)", ylab = "Load (g)",
       main = sprintf("PILOT_001: Time-Based Detection (200g)\n%d cycles (%d detected), %.1f%% stable time",
                      stats$n_cycles_total, stats$n_cycles_detected, stats$pct_stable))
  
  # Draw threshold lines
  abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 1)
  abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 1)
  
  # Highlight detected stable regions (skip forced cycles with duration=0)
  real_cycles <- all_cycles[all_cycles$duration > 0, ]
  for (i in 1:nrow(real_cycles)) {
    cycle <- real_cycles[i, ]
    rect(cycle$start_time, 0, cycle$end_time, max(data$load, na.rm = TRUE),
         col = rgb(0, 1, 0, 0.2), border = NA)
  }
  
  legend("topright", 
         legend = c("Load", "Stable thresholds", "Detected stable"),
         col = c("gray50", "blue", rgb(0, 1, 0, 0.5)),
         lty = c(1, 2, 1), lwd = c(1, 1, 5), bty = "n")
  
  dev.off()
  
  # --- Plot 2: Zoomed view (400-425s window) ---
  cat("  Creating zoomed plot (400-425s)...\n")
  png(file.path(params$output_dir, "PILOT_001_time_based_zoomed.png"), 
      width = 1400, height = 800, res = 100)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
  
  zoom_data <- data[data$elapsed_sec >= 400 & data$elapsed_sec <= 425, ]
  zoom_cycles <- all_cycles[all_cycles$start_time >= 400 & all_cycles$start_time <= 425, ]
  # Only plot cycles with real data
  zoom_cycles_real <- zoom_cycles[zoom_cycles$duration > 0, ]
  
  plot(zoom_data$elapsed_sec, zoom_data$load, type = "l", col = "gray50", lwd = 2,
       xlab = "Time (s)", ylab = "Load (g)",
       main = "PILOT_001: Time-Based Detection - Zoomed (400-425s)")
  
  abline(h = params$stable_range_low, col = "blue", lty = 2, lwd = 1.5)
  abline(h = params$stable_range_high, col = "blue", lty = 2, lwd = 1.5)
  
  # Only draw rectangles if there are cycles in this window
  if (nrow(zoom_cycles_real) > 0) {
    for (i in 1:nrow(zoom_cycles_real)) {
      cycle <- zoom_cycles_real[i, ]
      rect(cycle$start_time, 0, cycle$end_time, max(zoom_data$load, na.rm = TRUE),
           col = rgb(0, 1, 0, 0.3), border = NA)
    }
  }
  
  legend("topright", 
         legend = c("Load", "Stable thresholds", "Detected stable"),
         col = c("gray50", "blue", rgb(0, 1, 0, 0.5)),
         lty = c(1, 2, 1), lwd = c(2, 1.5, 5), bty = "n")
  
  dev.off()
  
  # --- Plot 3: Period distribution ---
  cat("  Creating period distribution plot...\n")
  if (nrow(all_cycles) >= 2) {
    intervals <- diff(all_cycles$start_time)
    
    png(file.path(params$output_dir, "PILOT_001_period_distribution.png"), 
        width = 1000, height = 700, res = 100)
    
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
    
    hist(intervals, breaks = 20, col = "lightblue", border = "darkblue",
         xlab = "Inter-cycle interval (s)", ylab = "Frequency",
         main = sprintf("Period Distribution\nMean: %.3f s, SD: %.3f s",
                        mean(intervals), sd(intervals)))
    abline(v = mean(intervals), col = "red", lwd = 2, lty = 2)
    
    dev.off()
  }
  
  cat(sprintf("  Plots saved to: %s\n", params$output_dir))
}

# ===============================================================================
# FUNCTION: save_results
# ===============================================================================
# Saves results to CSV
#
# Args:
#   all_cycles: data frame with all detected cycles
#   stats: statistics list
#   period_stats: period statistics
#   params: parameter list

save_results <- function(all_cycles, stats, period_stats, params) {
  
  cat("\nSaving results...\n")
  
  # Save detected cycles
  cycles_file <- file.path(params$output_dir, "PILOT_001_detected_cycles.csv")
  write.csv(all_cycles, cycles_file, row.names = FALSE)
  cat(sprintf("  Cycles saved to: %s\n", cycles_file))
  
  # Save summary statistics
  summary_file <- file.path(params$output_dir, "PILOT_001_summary_statistics.txt")
  sink(summary_file)
  cat("PILOT_001 - Time-Based Detection Summary\n")
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
# MAIN EXECUTION
# ===============================================================================

main <- function() {
  
  cat("========================================\n")
  cat("Time-Based Periodic Stable Detection\n")
  cat("Sample: PILOT_001 (200g)\n")
  cat("========================================\n\n")
  
  # Load data
  cat("Loading data...\n")
  data <- read.csv(PARAMS$data_file)
  cat(sprintf("  Loaded %d rows\n", nrow(data)))
  cat(sprintf("  Time range: %.2f - %.2f s\n", min(data$elapsed_sec, na.rm = TRUE), 
              max(data$elapsed_sec, na.rm = TRUE)))
  
  # Detect initial cycles
  initial_cycles <- detect_initial_cycles(data, PARAMS$n_cycles_for_period, PARAMS)
  
  if (nrow(initial_cycles) < 2) {
    stop("Could not detect enough initial cycles. Check parameters.")
  }
  
  # Estimate period
  period_stats <- estimate_period(initial_cycles, PARAMS)
  
  # Detect remaining cycles
  remaining_cycles <- predict_and_detect_remaining(data, initial_cycles, period_stats, PARAMS)
  
  # Combine all cycles
  all_cycles <- rbind(initial_cycles, remaining_cycles)
  all_cycles <- all_cycles[order(all_cycles$start_time), ]
  
  # Calculate statistics
  stats <- calculate_statistics(all_cycles, data, PARAMS)
  
  # Create plots
  create_plots(data, all_cycles, PARAMS, stats)
  
  # Save results
  save_results(all_cycles, stats, period_stats, PARAMS)
  
  cat("\n========================================\n")
  cat("COMPLETE!\n")
  cat("========================================\n")
  cat(sprintf("\nSUMMARY: %d total cycles (%d detected, %d forced) with %.2f%% stable time\n", 
              stats$n_cycles_total, stats$n_cycles_detected, stats$n_cycles_forced, stats$pct_stable))
  
  return(list(
    cycles = all_cycles,
    stats = stats,
    period_stats = period_stats
  ))
}

# Run main function
if (!interactive()) {
  result <- main()
}
