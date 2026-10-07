# ==============================================================================
# Sustained Pressure Detection Functions
# ==============================================================================
# 
# This file contains functions for detecting sustained pressure contact periods
# in pressure trace data. Unlike strict plateau detection, this approach
# identifies regions where pressure is maintained at a reasonable level with
# natural fluctuation, excluding major ramp-up/ramp-down transitions.
#
# Sustained contact is defined as:
# - Pressure above minimum threshold (contact with surface)
# - NOT in a major ramp-up or ramp-down phase
# - Pressure sustained for reasonable duration (1-15 seconds)
# - Natural fluctuations are acceptable
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-sustained
# ==============================================================================

#' Identify sustained pressure contact periods in pressure trace data
#' 
#' This function detects periods where pressure is sustained at a contact level
#' with natural fluctuation, excluding major transition phases (ramp-up/down).
#' It uses a combination of pressure thresholds and transition detection.
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams (for reference)
#' @param min_pressure_pct Minimum pressure as percentage of target (default: 0.60 = 60%)
#' @param max_pressure_pct Maximum pressure as percentage of target (default: 1.30 = 130%)
#' @param transition_window_sec Window for detecting transitions (default: 2.0)
#' @param transition_threshold_g Pressure change indicating transition (default: 30g for 200g, 10g for 50g)
#' @param min_duration_sec Minimum duration for sustained contact (default: 1.0)
#' @param max_duration_sec Maximum duration for sustained contact (default: 15.0)
#' @param merge_gap_sec Maximum gap to merge contact periods (default: 0.2)
#' @return Data frame with sustained contact flags and pass IDs
#' @export
identify_sustained_contact <- function(data, 
                                        target_pressure_g,
                                        min_pressure_pct = 0.60,
                                        max_pressure_pct = 1.30,
                                        transition_window_sec = 2.0,
                                        transition_threshold_g = NULL,
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
  
  # Auto-calculate transition threshold if not provided
  if (is.null(transition_threshold_g)) {
    if (target_pressure_g >= 100) {
      transition_threshold_g <- 30  # 30g for 200g samples
    } else {
      transition_threshold_g <- 10  # 10g for 50g samples
    }
  }
  
  # Calculate pressure thresholds
  min_pressure <- target_pressure_g * min_pressure_pct
  max_pressure <- target_pressure_g * max_pressure_pct
  
  # Estimate sampling rate
  time_diffs <- diff(data$elapsed_sec)
  median_dt <- median(time_diffs, na.rm = TRUE)
  sampling_rate <- 1 / median_dt
  
  # Calculate window sizes
  transition_window_points <- max(5, round(transition_window_sec / median_dt))
  if (transition_window_points %% 2 == 0) transition_window_points <- transition_window_points + 1
  
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, sampling_rate))
  cat(sprintf("  Pressure range: %.1f - %.1fg (%.0f%%-%.0f%% of target)\n", 
              min_pressure, max_pressure, 
              min_pressure_pct * 100, max_pressure_pct * 100))
  cat(sprintf("  Transition window: %.1fs (%d points)\n", 
              transition_window_sec, transition_window_points))
  cat(sprintf("  Transition threshold: %.1fg change over %.1fs window\n", 
              transition_threshold_g, transition_window_sec))
  cat(sprintf("  Duration range: %.1f - %.1f seconds\n", 
              min_duration_sec, max_duration_sec))
  
  # ============================================================================
  # STEP 1: Calculate Rolling Pressure Change (Transition Detection)
  # ============================================================================
  
  cat("  Calculating rolling pressure changes...\n")
  
  # Calculate rolling min and max over transition window
  data <- data %>%
    mutate(
      rolling_min = rollapply(
        load_g,
        width = transition_window_points,
        FUN = function(x) min(x, na.rm = TRUE),
        fill = NA,
        align = "center",
        partial = TRUE
      ),
      rolling_max = rollapply(
        load_g,
        width = transition_window_points,
        FUN = function(x) max(x, na.rm = TRUE),
        fill = NA,
        align = "center",
        partial = TRUE
      ),
      rolling_range = rolling_max - rolling_min
    )
  
  # ============================================================================
  # STEP 2: Mark Sustained Contact Points
  # ============================================================================
  
  cat("  Identifying sustained contact regions...\n")
  
  data <- data %>%
    mutate(
      # Criteria for sustained contact:
      # 1. Pressure in reasonable range (not zero, not excessive)
      in_pressure_range = load_g >= min_pressure & load_g <= max_pressure,
      
      # 2. NOT in a major transition (rolling range < threshold)
      not_in_transition = !is.na(rolling_range) & rolling_range < transition_threshold_g,
      
      # Combined: sustained contact when both criteria met
      is_sustained = in_pressure_range & not_in_transition,
      
      # Store thresholds for reference
      pressure_min = min_pressure,
      pressure_max = max_pressure,
      transition_threshold = transition_threshold_g
    )
  
  # Count initial sustained points
  n_sustained_raw <- sum(data$is_sustained, na.rm = TRUE)
  pct_sustained_raw <- (n_sustained_raw / nrow(data)) * 100
  cat(sprintf("  Raw sustained points: %d / %d (%.1f%%)\n", 
              n_sustained_raw, nrow(data), pct_sustained_raw))
  
  # ============================================================================
  # STEP 3: Segment Consecutive Sustained Points
  # ============================================================================
  
  cat("  Segmenting consecutive sustained regions...\n")
  
  data <- data %>%
    mutate(
      sustained_change = is_sustained != lag(is_sustained, default = FALSE),
      segment_id = cumsum(sustained_change)
    ) %>%
    mutate(segment_id = ifelse(is_sustained, segment_id, NA))
  
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
      min_pressure_val = min(load_g, na.rm = TRUE),
      max_pressure_val = max(load_g, na.rm = TRUE),
      pressure_range = max_pressure_val - min_pressure_val,
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d raw segments\n", nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No sustained contact segments found!")
    data <- data %>%
      mutate(
        is_contact = FALSE,
        pass_id = NA_integer_
      ) %>%
      select(-sustained_change, -segment_id)
    return(data)
  }
  
  # ============================================================================
  # STEP 4: Filter by Duration
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
        is_contact = FALSE,
        pass_id = NA_integer_
      ) %>%
      select(-sustained_change, -segment_id)
    return(data)
  }
  
  # ============================================================================
  # STEP 5: Merge Close Segments
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
    
    cat(sprintf("  After merging: %d sustained contact passes\n", nrow(merged_ranges)))
    
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
    
    cat("  Only 1 sustained contact pass found\n")
  }
  
  # ============================================================================
  # STEP 6: Apply Merged Groups Back to Data
  # ============================================================================
  
  data <- data %>%
    mutate(
      pass_id = NA_integer_,
      is_contact = FALSE
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
        is_contact = ifelse(
          elapsed_sec >= merged_ranges$merge_start[i] & 
          elapsed_sec <= merged_ranges$merge_end[i],
          TRUE,
          is_contact
        )
      )
  }
  
  # Clean up temporary columns
  data <- data %>%
    select(-sustained_change, -segment_id, -in_pressure_range, 
           -not_in_transition, -is_sustained)
  
  # Calculate summary stats
  n_contact <- sum(data$is_contact)
  pct_contact <- (n_contact / nrow(data)) * 100
  
  cat(sprintf("  Result: %d/%d points in sustained contact (%.1f%%)\n", 
              n_contact, nrow(data), pct_contact))
  
  # Print contact summary
  cat("\n  Detected Sustained Contact Periods:\n")
  for (i in 1:min(10, nrow(merged_ranges))) {
    cat(sprintf("    Pass %d: %.1fs - %.1fs (%.1fs duration, %.1fg mean)\n",
                i,
                merged_ranges$merge_start[i],
                merged_ranges$merge_end[i],
                merged_ranges$merge_duration[i],
                merged_ranges$mean_pressure[i]))
  }
  if (nrow(merged_ranges) > 10) {
    cat(sprintf("    ... and %d more passes\n", nrow(merged_ranges) - 10))
  }
  cat("\n")
  
  return(data)
}


#' Calculate statistics for sustained contact periods only
#' 
#' @param data Data frame with pressure trace and contact flags
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with sustained contact statistics
#' @export
calculate_sustained_statistics <- function(data, target_pressure_g) {
  
  # Ensure detection has been run
  if (!"is_contact" %in% colnames(data)) {
    stop("Sustained contact detection must be run first. Call identify_sustained_contact() first.")
  }
  
  # Filter to contact periods only
  contact_data <- data %>% filter(is_contact == TRUE)
  
  if (nrow(contact_data) == 0) {
    warning("No sustained contact periods found!")
    return(NULL)
  }
  
  # Calculate time metrics
  total_duration <- max(data$elapsed_sec, na.rm = TRUE) - 
                   min(data$elapsed_sec, na.rm = TRUE)
  
  contact_duration <- contact_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total_contact = sum(time_diff, na.rm = TRUE)) %>%
    pull(total_contact)
  
  # Count passes
  n_passes <- max(contact_data$pass_id, na.rm = TRUE)
  
  # Calculate statistics
  stats <- data.frame(
    run_id = first(data$run_id),
    mean_contact_pressure_g = mean(contact_data$load_g, na.rm = TRUE),
    sd_contact_pressure_g = sd(contact_data$load_g, na.rm = TRUE),
    cv_contact_percent = (sd(contact_data$load_g, na.rm = TRUE) / 
                         mean(contact_data$load_g, na.rm = TRUE)) * 100,
    min_contact_pressure_g = min(contact_data$load_g, na.rm = TRUE),
    max_contact_pressure_g = max(contact_data$load_g, na.rm = TRUE),
    median_contact_pressure_g = median(contact_data$load_g, na.rm = TRUE),
    n_contact_observations = nrow(contact_data),
    total_contact_time_sec = contact_duration,
    total_trace_duration_sec = total_duration,
    percent_time_in_contact = (contact_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = contact_duration / n_passes,
    mean_rolling_range = mean(contact_data$rolling_range, na.rm = TRUE)
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
