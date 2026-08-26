# ==============================================================================
# Flat Plateau Detection Functions
# ==============================================================================
# 
# This file contains functions for detecting flat plateau periods in pressure
# trace data using derivative-based flatness detection.
#
# Flat plateaus are defined as regions where:
# - Pressure is relatively constant (low derivative)
# - Local variance is low
# - Pressure is above minimum threshold (contact with surface)
# - Duration is reasonable (1-15 seconds)
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-plateau
# ==============================================================================

#' Identify flat plateau periods in pressure trace data
#' 
#' This function detects periods where pressure is flat (stable plateau)
#' using derivative-based analysis. It finds regions where the rate of
#' pressure change is minimal, indicating stable contact with the surface.
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams (for reference only)
#' @param max_derivative_g_per_sec Maximum rate of change for flat region (default: 5)
#' @param max_local_sd_pct Maximum local SD as percentage of target (default: 0.01 = 1%)
#' @param min_pressure_pct Minimum pressure as percentage of target (default: 0.70 = 70%)
#' @param max_pressure_pct Maximum pressure as percentage of target (default: 1.20 = 120%)
#' @param min_duration_sec Minimum duration for plateau (default: 1.0)
#' @param max_duration_sec Maximum duration for plateau (default: 15.0)
#' @param merge_gap_sec Maximum gap to merge plateaus (default: 0.2)
#' @return Data frame with plateau flags and pass IDs
#' @export
identify_flat_plateaus <- function(data, 
                                    target_pressure_g,
                                    max_derivative_g_per_sec = 5.0,
                                    max_local_sd_pct = 0.01,
                                    min_pressure_pct = 0.70,
                                    max_pressure_pct = 1.20,
                                    min_duration_sec = 1.0,
                                    max_duration_sec = 15.0,
                                    merge_gap_sec = 0.2) {
  
  # Load required package
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required. Install with: install.packages('zoo')")
  }
  library(zoo)
  
  # Sort by time
  data <- data %>% arrange(elapsed_sec)
  
  # Calculate thresholds
  max_local_sd <- target_pressure_g * max_local_sd_pct
  min_pressure <- target_pressure_g * min_pressure_pct
  max_pressure <- target_pressure_g * max_pressure_pct
  
  # Estimate sampling rate
  time_diffs <- diff(data$elapsed_sec)
  median_dt <- median(time_diffs, na.rm = TRUE)
  sampling_rate <- 1 / median_dt
  
  # Calculate window sizes
  local_sd_window_sec <- 0.5  # 0.5 second window for local SD
  local_sd_window_points <- max(5, round(local_sd_window_sec / median_dt))
  if (local_sd_window_points %% 2 == 0) local_sd_window_points <- local_sd_window_points + 1
  
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, sampling_rate))
  cat(sprintf("  Derivative threshold: %.2f g/s\n", max_derivative_g_per_sec))
  cat(sprintf("  Local SD threshold: %.2fg (%.1f%% of target)\n", 
              max_local_sd, max_local_sd_pct * 100))
  cat(sprintf("  Pressure range: %.1f - %.1fg (%.0f%%-%.0f%% of target)\n", 
              min_pressure, max_pressure, 
              min_pressure_pct * 100, max_pressure_pct * 100))
  cat(sprintf("  Duration range: %.1f - %.1f seconds\n", 
              min_duration_sec, max_duration_sec))
  cat(sprintf("  Local SD window: %.1fs (%d points)\n", 
              local_sd_window_sec, local_sd_window_points))
  
  # ============================================================================
  # STEP 1: Calculate Derivatives (Rate of Change)
  # ============================================================================
  
  cat("  Calculating derivatives...\n")
  
  # Use 5-point derivative for smoothness
  # This calculates slope over 4 data points (2 forward, 2 backward)
  data <- data %>%
    mutate(
      derivative = (lead(load_g, 2) - lag(load_g, 2)) / 
                   (lead(elapsed_sec, 2) - lag(elapsed_sec, 2)),
      abs_derivative = abs(derivative)
    )
  
  # ============================================================================
  # STEP 2: Calculate Local Standard Deviation
  # ============================================================================
  
  cat("  Calculating local variance...\n")
  
  data <- data %>%
    mutate(
      local_sd = rollapply(
        load_g,
        width = local_sd_window_points,
        FUN = function(x) sd(x, na.rm = TRUE),
        fill = NA,
        align = "center",
        partial = TRUE
      )
    )
  
  # ============================================================================
  # STEP 3: Mark Flat Points (All Criteria)
  # ============================================================================
  
  cat("  Identifying flat regions...\n")
  
  data <- data %>%
    mutate(
      # Check each criterion
      has_low_derivative = !is.na(derivative) & abs_derivative < max_derivative_g_per_sec,
      has_low_variance = !is.na(local_sd) & local_sd < max_local_sd,
      in_pressure_range = load_g >= min_pressure & load_g <= max_pressure,
      
      # Combined: all criteria must be met
      is_flat = has_low_derivative & has_low_variance & in_pressure_range,
      
      # Store thresholds for reference
      derivative_threshold = max_derivative_g_per_sec,
      sd_threshold = max_local_sd,
      pressure_min = min_pressure,
      pressure_max = max_pressure
    )
  
  # Count initial flat points
  n_flat_raw <- sum(data$is_flat, na.rm = TRUE)
  pct_flat_raw <- (n_flat_raw / nrow(data)) * 100
  cat(sprintf("  Raw flat points: %d / %d (%.1f%%)\n", 
              n_flat_raw, nrow(data), pct_flat_raw))
  
  # ============================================================================
  # STEP 4: Segment Consecutive Flat Points
  # ============================================================================
  
  cat("  Segmenting consecutive flat regions...\n")
  
  data <- data %>%
    mutate(
      flat_change = is_flat != lag(is_flat, default = FALSE),
      segment_id = cumsum(flat_change)
    ) %>%
    mutate(segment_id = ifelse(is_flat, segment_id, NA))
  
  # Get segment boundaries
  segments <- data %>%
    filter(!is.na(segment_id)) %>%
    group_by(segment_id) %>%
    summarize(
      start_time = min(elapsed_sec),
      end_time = max(elapsed_sec),
      duration = end_time - start_time,
      n_points = n(),
      mean_pressure = mean(load_g, na.rm = TRUE),
      sd_pressure = sd(load_g, na.rm = TRUE),
      cv_pressure = (sd(load_g, na.rm = TRUE) / mean(load_g, na.rm = TRUE)) * 100,
      mean_derivative = mean(abs_derivative, na.rm = TRUE),
      max_derivative = max(abs_derivative, na.rm = TRUE),
      mean_local_sd = mean(local_sd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d raw segments\n", nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No flat segments found with current parameters!")
    data <- data %>%
      mutate(
        is_plateau = FALSE,
        pass_id = NA_integer_
      ) %>%
      select(-flat_change, -segment_id)
    return(data)
  }
  
  # ============================================================================
  # STEP 5: Filter by Duration
  # ============================================================================
  
  cat(sprintf("  Filtering by duration (%.1fs - %.1fs)...\n", 
              min_duration_sec, max_duration_sec))
  
  segments <- segments %>%
    filter(duration >= min_duration_sec, duration <= max_duration_sec)
  
  cat(sprintf("  After duration filter: %d segments\n", nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No segments met duration criteria!")
    data <- data %>%
      mutate(
        is_plateau = FALSE,
        pass_id = NA_integer_
      ) %>%
      select(-flat_change, -segment_id)
    return(data)
  }
  
  # ============================================================================
  # STEP 6: Merge Close Segments
  # ============================================================================
  
  cat(sprintf("  Merging segments with gaps < %.1fs...\n", merge_gap_sec))
  
  if (nrow(segments) > 1) {
    segments <- segments %>%
      mutate(
        gap_to_next = lead(start_time) - end_time,
        merge_with_next = !is.na(gap_to_next) & gap_to_next < merge_gap_sec
      )
    
    # Build merged segment groups
    segments$merged_group <- NA
    current_group <- 1
    segments$merged_group[1] <- current_group
    
    for (i in 2:nrow(segments)) {
      if (segments$merge_with_next[i-1]) {
        segments$merged_group[i] <- current_group
      } else {
        current_group <- current_group + 1
        segments$merged_group[i] <- current_group
      }
    }
    
    # Create merged ranges
    merged_ranges <- segments %>%
      group_by(merged_group) %>%
      summarize(
        merge_start = min(start_time),
        merge_end = max(end_time),
        merge_duration = merge_end - merge_start,
        mean_pressure = mean(mean_pressure),
        mean_cv = mean(cv_pressure),
        n_sub_segments = n(),
        .groups = "drop"
      )
    
    cat(sprintf("  After merging: %d plateau passes\n", nrow(merged_ranges)))
    
  } else {
    # Only one segment
    merged_ranges <- segments %>%
      mutate(
        merged_group = 1,
        merge_start = start_time,
        merge_end = end_time,
        merge_duration = duration,
        mean_cv = cv_pressure,
        n_sub_segments = 1
      ) %>%
      select(merged_group, merge_start, merge_end, merge_duration, 
             mean_pressure, mean_cv, n_sub_segments)
    
    cat("  Only 1 plateau pass found\n")
  }
  
  # ============================================================================
  # STEP 7: Apply Merged Groups Back to Data
  # ============================================================================
  
  data <- data %>%
    mutate(
      pass_id = NA_integer_,
      is_plateau = FALSE
    )
  
  for (i in 1:nrow(merged_ranges)) {
    data <- data %>%
      mutate(
        pass_id = ifelse(
          elapsed_sec >= merged_ranges$merge_start[i] & 
          elapsed_sec <= merged_ranges$merge_end[i],
          i,
          pass_id
        ),
        is_plateau = ifelse(
          elapsed_sec >= merged_ranges$merge_start[i] & 
          elapsed_sec <= merged_ranges$merge_end[i],
          TRUE,
          is_plateau
        )
      )
  }
  
  # Clean up temporary columns
  data <- data %>%
    select(-flat_change, -segment_id, -has_low_derivative, 
           -has_low_variance, -in_pressure_range, -is_flat)
  
  # Calculate summary stats
  n_plateau <- sum(data$is_plateau)
  pct_plateau <- (n_plateau / nrow(data)) * 100
  
  cat(sprintf("  Result: %d/%d points in plateaus (%.1f%%)\n", 
              n_plateau, nrow(data), pct_plateau))
  
  # Print plateau summary
  cat("\n  Detected Plateaus:\n")
  for (i in 1:min(5, nrow(merged_ranges))) {
    cat(sprintf("    Pass %d: %.1fs - %.1fs (%.1fs duration, %.1fg mean)\n",
                i,
                merged_ranges$merge_start[i],
                merged_ranges$merge_end[i],
                merged_ranges$merge_duration[i],
                merged_ranges$mean_pressure[i]))
  }
  if (nrow(merged_ranges) > 5) {
    cat(sprintf("    ... and %d more passes\n", nrow(merged_ranges) - 5))
  }
  cat("\n")
  
  return(data)
}


#' Calculate statistics for plateau periods only
#' 
#' @param data Data frame with pressure trace and plateau flags
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with plateau-based statistics
#' @export
calculate_plateau_statistics <- function(data, target_pressure_g) {
  
  # Ensure plateau detection has been run
  if (!"is_plateau" %in% colnames(data)) {
    stop("Plateau detection must be run first. Call identify_flat_plateaus() first.")
  }
  
  # Filter to plateau periods only
  plateau_data <- data %>% filter(is_plateau == TRUE)
  
  if (nrow(plateau_data) == 0) {
    warning("No plateau periods found!")
    return(NULL)
  }
  
  # Calculate time metrics
  total_duration <- max(data$elapsed_sec, na.rm = TRUE) - 
                   min(data$elapsed_sec, na.rm = TRUE)
  
  plateau_duration <- plateau_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total_plateau = sum(time_diff, na.rm = TRUE)) %>%
    pull(total_plateau)
  
  # Count passes
  n_passes <- max(plateau_data$pass_id, na.rm = TRUE)
  
  # Calculate statistics
  stats <- data.frame(
    run_id = first(data$run_id),
    mean_plateau_pressure_g = mean(plateau_data$load_g, na.rm = TRUE),
    sd_plateau_pressure_g = sd(plateau_data$load_g, na.rm = TRUE),
    cv_plateau_percent = (sd(plateau_data$load_g, na.rm = TRUE) / 
                         mean(plateau_data$load_g, na.rm = TRUE)) * 100,
    min_plateau_pressure_g = min(plateau_data$load_g, na.rm = TRUE),
    max_plateau_pressure_g = max(plateau_data$load_g, na.rm = TRUE),
    median_plateau_pressure_g = median(plateau_data$load_g, na.rm = TRUE),
    n_plateau_observations = nrow(plateau_data),
    total_plateau_time_sec = plateau_duration,
    total_trace_duration_sec = total_duration,
    percent_time_plateau = (plateau_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = plateau_duration / n_passes,
    mean_derivative_plateau = mean(abs(plateau_data$abs_derivative), na.rm = TRUE),
    max_derivative_plateau = max(abs(plateau_data$abs_derivative), na.rm = TRUE),
    mean_local_sd_plateau = mean(plateau_data$local_sd, na.rm = TRUE)
  )
  
  return(stats)
}


#' Read a single pressure trace CSV file
#' 
#' @param run_id Sample identifier
#' @param traces_path Path to traces directory
#' @return Data frame with pressure trace data
#' @export
read_pressure_trace_file <- function(run_id, traces_path) {
  file_path <- file.path(traces_path, paste0(run_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read the entire file as text first
  lines <- readLines(file_path, warn = FALSE)
  
  # Find data rows (skip headers and text rows)
  data_lines <- lines[2:length(lines)]
  
  # Parse each line
  data_list <- list()
  for (i in seq_along(data_lines)) {
    line <- data_lines[i]
    
    # Skip lines that contain text headers
    if (grepl("analog|load|distance|timestamp", line, ignore.case = TRUE)) {
      next
    }
    
    # Try to parse the line
    parts <- strsplit(line, ",")[[1]]
    
    if (length(parts) >= 5) {
      # Check if columns 3-5 are numeric
      if (!is.na(suppressWarnings(as.numeric(parts[3]))) &&
          !is.na(suppressWarnings(as.numeric(parts[4])))) {
        data_list[[length(data_list) + 1]] <- data.frame(
          timestamp = parts[1],
          elapsed_sec = as.numeric(parts[2]),
          analog_value = as.numeric(parts[3]),
          load_g = as.numeric(parts[4]),
          distance_mm = as.numeric(parts[5]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(data_list) == 0) {
    warning(paste("No valid data rows found in", run_id))
    return(NULL)
  }
  
  # Combine all rows
  df <- bind_rows(data_list)
  
  # Parse timestamp
  df <- df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g))
  
  return(df)
}
