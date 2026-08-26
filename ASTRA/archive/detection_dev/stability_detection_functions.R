# ==============================================================================
# Stability-Based Contact Detection Functions
# ==============================================================================
# 
# This file contains functions for detecting stable contact periods in pressure
# trace data using rolling standard deviation instead of tolerance bands.
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-prototype
# ==============================================================================

#' Identify stable periods in pressure trace data using rolling SD
#' 
#' This function detects periods where pressure is stable (low variance) 
#' rather than simply within a tolerance band. It uses a rolling standard
#' deviation approach to identify true swabbing plateaus.
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams
#' @param window_sec Width of rolling window in seconds (default: 1.5)
#' @param max_sd_pct Maximum SD as percentage of target (default: 0.025 = 2.5%)
#' @param broad_tolerance Broad range filter as percentage (default: 0.20 = ±20%)
#' @param min_duration_sec Minimum duration for stable period (default: 2.0)
#' @param merge_gap_sec Maximum gap to merge stable periods (default: 0.3)
#' @return Data frame with stability flags and pass IDs
#' @export
identify_stable_periods <- function(data, 
                                     target_pressure_g,
                                     window_sec = 1.5,
                                     max_sd_pct = 0.025,
                                     broad_tolerance = 0.20,
                                     min_duration_sec = 2.0,
                                     merge_gap_sec = 0.3) {
  
  # Load required package - use zoo as fallback
  use_zoo <- FALSE
  if (!requireNamespace("slider", quietly = TRUE)) {
    message("Package 'slider' not found, using 'zoo' instead")
    if (!requireNamespace("zoo", quietly = TRUE)) {
      stop("Either 'slider' or 'zoo' package is required. Install with: install.packages('zoo')")
    }
    use_zoo <- TRUE
    library(zoo)
  }
  
  # Sort by time
  data <- data %>% arrange(elapsed_sec)
  
  # Calculate actual SD threshold
  max_sd_g <- target_pressure_g * max_sd_pct
  
  # Calculate broad range filter (coarse pressure filter)
  lower_broad <- target_pressure_g * (1 - broad_tolerance)
  upper_broad <- target_pressure_g * (1 + broad_tolerance)
  
  # Estimate sampling rate and calculate window size in points
  time_diffs <- diff(data$elapsed_sec)
  median_dt <- median(time_diffs, na.rm = TRUE)
  window_points <- round(window_sec / median_dt)
  
  # Ensure odd number for symmetric window
  if (window_points %% 2 == 0) window_points <- window_points + 1
  
  # Ensure minimum window size
  if (window_points < 5) window_points <- 5
  
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, 1/median_dt))
  cat(sprintf("  Window size: %.1f seconds = %d points\n", window_sec, window_points))
  cat(sprintf("  SD threshold: %.2fg (%.1f%% of target)\n", 
              max_sd_g, max_sd_pct * 100))
  cat(sprintf("  Broad range: %.1f - %.1fg (±%.0f%%)\n", 
              lower_broad, upper_broad, broad_tolerance * 100))
  
  # Calculate rolling SD using zoo or slider
  if (use_zoo) {
    data <- data %>%
      mutate(
        rolling_sd = rollapply(
          load_g,
          width = window_points,
          FUN = function(x) sd(x, na.rm = TRUE),
          fill = NA,
          align = "center",
          partial = TRUE
        ),
        rolling_mean = rollapply(
          load_g,
          width = window_points,
          FUN = function(x) mean(x, na.rm = TRUE),
          fill = NA,
          align = "center",
          partial = TRUE
        )
      )
  } else {
    library(slider)
    data <- data %>%
      mutate(
        rolling_sd = slide_dbl(
          load_g, 
          ~sd(.x, na.rm = TRUE),
          .before = floor(window_points / 2),
          .after = floor(window_points / 2),
          .complete = FALSE
        ),
        rolling_mean = slide_dbl(
          load_g,
          ~mean(.x, na.rm = TRUE),
          .before = floor(window_points / 2),
          .after = floor(window_points / 2),
          .complete = FALSE
        )
      )
  }
  
  # Flag stable points
  # Criteria: low SD AND within broad pressure range
  data <- data %>%
    mutate(
      is_stable_raw = !is.na(rolling_sd) & 
                      rolling_sd <= max_sd_g &
                      load_g >= lower_broad & 
                      load_g <= upper_broad,
      stability_threshold = max_sd_g,
      broad_lower = lower_broad,
      broad_upper = upper_broad
    )
  
  # Identify consecutive stable segments
  data <- data %>%
    mutate(
      stability_change = is_stable_raw != lag(is_stable_raw, default = FALSE),
      segment_id = cumsum(stability_change)
    ) %>%
    mutate(segment_id = ifelse(is_stable_raw, segment_id, NA))
  
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
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d raw stable segments\n", nrow(segments)))
  
  # Filter by minimum duration
  segments <- segments %>%
    filter(duration >= min_duration_sec)
  
  cat(sprintf("  After duration filter (>%.1fs): %d segments\n", 
              min_duration_sec, nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No stable segments found with current parameters!")
    data <- data %>%
      mutate(
        is_stable = FALSE,
        pass_id = NA_integer_
      ) %>%
      select(-stability_change, -segment_id, -is_stable_raw)
    return(data)
  }
  
  # Merge segments with small gaps
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
        .groups = "drop"
      )
    
    cat(sprintf("  After merging gaps (<%.1fs): %d stable passes\n", 
                merge_gap_sec, nrow(merged_ranges)))
    
  } else {
    # Only one segment
    merged_ranges <- segments %>%
      mutate(
        merged_group = 1,
        merge_start = start_time,
        merge_end = end_time,
        merge_duration = duration,
        mean_pressure = mean_pressure
      ) %>%
      select(merged_group, merge_start, merge_end, merge_duration, mean_pressure)
    
    cat("  Only 1 stable pass found\n")
  }
  
  # Apply merged groups back to data
  data <- data %>%
    mutate(
      pass_id = NA_integer_,
      is_stable = FALSE
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
        is_stable = ifelse(
          elapsed_sec >= merged_ranges$merge_start[i] & 
          elapsed_sec <= merged_ranges$merge_end[i],
          TRUE,
          is_stable
        )
      )
  }
  
  # Clean up temporary columns
  data <- data %>%
    select(-stability_change, -segment_id, -is_stable_raw)
  
  # Calculate summary stats
  n_stable <- sum(data$is_stable)
  pct_stable <- (n_stable / nrow(data)) * 100
  cat(sprintf("  Result: %d/%d points stable (%.1f%%)\n", 
              n_stable, nrow(data), pct_stable))
  
  return(data)
}


#' Calculate statistics for stable periods only
#' 
#' @param data Data frame with pressure trace and stability flags
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with stability-based statistics
#' @export
calculate_stability_statistics <- function(data, target_pressure_g) {
  
  # Ensure stability detection has been run
  if (!"is_stable" %in% colnames(data)) {
    stop("Stability detection must be run first. Call identify_stable_periods() first.")
  }
  
  # Filter to stable periods only
  stable_data <- data %>% filter(is_stable == TRUE)
  
  if (nrow(stable_data) == 0) {
    warning("No stable periods found!")
    return(NULL)
  }
  
  # Calculate time metrics
  total_duration <- max(data$elapsed_sec, na.rm = TRUE) - 
                   min(data$elapsed_sec, na.rm = TRUE)
  
  stable_duration <- stable_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total_stable = sum(time_diff, na.rm = TRUE)) %>%
    pull(total_stable)
  
  # Count passes
  n_passes <- max(stable_data$pass_id, na.rm = TRUE)
  
  # Calculate statistics
  stats <- data.frame(
    run_id = first(data$run_id),
    mean_stable_pressure_g = mean(stable_data$load_g, na.rm = TRUE),
    sd_stable_pressure_g = sd(stable_data$load_g, na.rm = TRUE),
    cv_stable_percent = (sd(stable_data$load_g, na.rm = TRUE) / 
                         mean(stable_data$load_g, na.rm = TRUE)) * 100,
    min_stable_pressure_g = min(stable_data$load_g, na.rm = TRUE),
    max_stable_pressure_g = max(stable_data$load_g, na.rm = TRUE),
    median_stable_pressure_g = median(stable_data$load_g, na.rm = TRUE),
    n_stable_observations = nrow(stable_data),
    total_stable_time_sec = stable_duration,
    total_trace_duration_sec = total_duration,
    percent_time_stable = (stable_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = stable_duration / n_passes,
    mean_rolling_sd_stable = mean(stable_data$rolling_sd, na.rm = TRUE),
    max_rolling_sd_stable = max(stable_data$rolling_sd, na.rm = TRUE)
  )
  
  return(stats)
}


#' Read a single pressure trace CSV file (same as v2 but exported)
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
