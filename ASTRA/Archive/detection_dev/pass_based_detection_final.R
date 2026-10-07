# ==============================================================================
# Stable Contact Region Detection v4.0 - Rate-of-Change Based
# ==============================================================================
# 
# Physical Model of Swab Pass:
# [Zero] → [Ramp-Up] → [Stable Contact] → [Ramp-Down] → [Zero]
#   <2g   rate>+20g/s     rate≈0, Δ<5g      rate<-20g/s    <2g
#
# Detection Strategy:
# 1. Separate passes by zero-pressure regions (< 2g)
# 2. Within each pass, detect ramps using rate-of-change (±20 g/s)
# 3. Identify stable middle section (verified by ±5g point-to-point variation)
#
# Key Improvements over v3.0:
# - Stricter zero threshold (2g vs 5g)
# - Explicit rate-based ramp detection (not just absolute change)
# - Larger stable variation tolerance (±5g vs ±2g)
# - Clear physical phase labeling (zero/ramp_up/stable/ramp_down)
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 4.0
# ==============================================================================

library(tidyverse)
library(zoo)  # For rollmean()


#' Calculate rate of change with smoothing
#' 
#' @param load_g Vector of pressure values in grams
#' @param elapsed_sec Vector of elapsed time in seconds
#' @param smooth_window Number of points for rolling mean smoothing (default: 5)
#' @return Data frame with rate_raw and rate_smooth columns
#' @export
calculate_rate_of_change <- function(load_g, elapsed_sec, smooth_window = 5) {
  n <- length(load_g)
  
  # Initialize vectors
  rate_raw <- rep(0, n)
  
  # Calculate instantaneous rate: dP/dt
  for (i in 2:n) {
    dt <- elapsed_sec[i] - elapsed_sec[i-1]
    if (dt > 0) {
      rate_raw[i] <- (load_g[i] - load_g[i-1]) / dt
    } else {
      rate_raw[i] <- 0  # Avoid division by zero
    }
  }
  
  # Apply smoothing (rolling mean)
  # Use partial windows at edges to avoid losing data
  if (n >= smooth_window) {
    rate_smooth <- rollmean(rate_raw, k = smooth_window, fill = "extend", align = "center")
  } else {
    rate_smooth <- rate_raw
  }
  
  return(data.frame(
    rate_raw = rate_raw,
    rate_smooth = rate_smooth
  ))
}


#' Detect ramp boundaries within a pass using rate thresholds
#' 
#' BALANCED APPROACH: Scan from edges until rate stabilizes, but use
#' minimal fallback (5%) if no ramps detected. This leaves maximum room
#' for stable validation to do its work.
#' 
#' @param pass_data Data frame for a single pass with rate_smooth column
#' @param ramp_rate_threshold Absolute rate threshold for ramp detection (g/s)
#' @return List with ramp_up_end (index) and ramp_down_start (index)
#' @export
detect_ramp_boundaries <- function(pass_data, ramp_rate_threshold = 20) {
  n <- nrow(pass_data)
  
  if (n < 3) {
    # Pass too short, mark entire pass as stable
    return(list(ramp_up_end = 1, ramp_down_start = n))
  }
  
  # Find end of ramp-up: scan from start until rate drops below positive threshold
  ramp_up_end <- 1
  for (i in 2:n) {
    if (pass_data$rate_smooth[i] < ramp_rate_threshold) {
      ramp_up_end <- i - 1
      break
    }
  }
  
  # If no clear ramp-up found (broke out at n-1), use first 5% of pass (minimal)
  if (ramp_up_end >= n - 1) {
    ramp_up_end <- max(1, floor(n * 0.05))
  }
  
  # Find start of ramp-down: scan backward from end until rate rises above negative threshold
  ramp_down_start <- n
  for (i in (n-1):1) {
    if (pass_data$rate_smooth[i] > -ramp_rate_threshold) {
      ramp_down_start <- i + 1
      break
    }
  }
  
  # If no clear ramp-down found (broke out at 2), use last 5% of pass (minimal)
  if (ramp_down_start <= 2) {
    ramp_down_start <- min(n, ceiling(n * 0.95))
  }
  
  # Ensure stable region exists (ramp_down_start > ramp_up_end)
  if (ramp_down_start <= ramp_up_end) {
    # If ramps overlap, split pass in middle
    mid <- ceiling(n / 2)
    ramp_up_end <- max(1, mid - 1)
    ramp_down_start <- min(n, mid + 1)
  }
  
  return(list(
    ramp_up_end = ramp_up_end,
    ramp_down_start = ramp_down_start
  ))
}


#' Identify stable contact regions using rate-of-change analysis (v4.0)
#' 
#' This function implements a three-phase algorithm:
#' 1. Separate passes by zero-pressure regions
#' 2. Detect ramp-up/ramp-down using rate thresholds
#' 3. Validate stable middle section by point-to-point variation
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams (for reference)
#' @param zero_threshold Pressure below this = zero/no contact (default: 2g)
#' @param max_zero_gap_sec Maximum zero gap to merge passes (default: 0.5s)
#' @param ramp_rate_threshold Absolute rate for ramp detection (default: 20 g/s)
#' @param stable_max_change_g Max point-to-point change for stable (default: 5g)
#' @param stable_max_sd_g Max rolling standard deviation for stable (default: 8g)
#' @param stable_sd_window_sec Rolling window for SD calculation (default: 0.5s)
#' @param smooth_window Points for rate smoothing (default: 5)
#' @param min_stable_duration_sec Minimum stable duration (default: 0.3s)
#' @return Data frame with added columns: pass_id, region, is_stable, rate_raw, rate_smooth
#' @export
identify_stable_contact_regions_v4 <- function(data,
                                                target_pressure_g,
                                                zero_threshold = 2,
                                                max_zero_gap_sec = 0.5,
                                                ramp_rate_threshold = 20,
                                                stable_max_change_g = 5.0,
                                                stable_max_sd_g = 8.0,
                                                stable_sd_window_sec = 0.5,
                                                smooth_window = 5,
                                                min_stable_duration_sec = 0.3) {
  
  # Ensure data is sorted
  data <- data %>% arrange(elapsed_sec)
  
  # Calculate sampling rate
  median_dt <- median(diff(data$elapsed_sec), na.rm = TRUE)
  sampling_freq <- 1 / median_dt
  
  cat("\n==============================================================================\n")
  cat("STABLE CONTACT REGION DETECTION v4.0 (Rate-of-Change Based)\n")
  cat("==============================================================================\n\n")
  cat(sprintf("  Run ID: %s\n", first(data$run_id)))
  cat(sprintf("  Target pressure: %.1fg\n", target_pressure_g))
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, sampling_freq))
  cat(sprintf("  Total observations: %d\n\n", nrow(data)))
  
  cat("PARAMETERS:\n")
  cat(sprintf("  Zero threshold: %.1fg\n", zero_threshold))
  cat(sprintf("  Max zero gap: %.2fs\n", max_zero_gap_sec))
  cat(sprintf("  Ramp rate threshold: %.1f g/s\n", ramp_rate_threshold))
  cat(sprintf("  Stable max change: %.1fg (point-to-point)\n", stable_max_change_g))
  cat(sprintf("  Stable max SD: %.1fg (rolling, %.1fs window)\n", stable_max_sd_g, stable_sd_window_sec))
  cat(sprintf("  Smooth window: %d points\n", smooth_window))
  cat(sprintf("  Min stable duration: %.2fs\n\n", min_stable_duration_sec))
  
  # ===========================================================================
  # PHASE 1: Separate Passes by Zero-Pressure Regions
  # ===========================================================================
  
  cat("PHASE 1: Separating passes by zero-pressure regions...\n")
  
  data <- data %>%
    mutate(
      is_nonzero = load_g > zero_threshold,
      zero_change = is_nonzero != lag(is_nonzero, default = FALSE),
      pass_segment_id = cumsum(zero_change)
    ) %>%
    mutate(pass_segment_id = ifelse(is_nonzero, pass_segment_id, NA))
  
  # Get pass segments
  pass_segments <- data %>%
    filter(!is.na(pass_segment_id)) %>%
    group_by(pass_segment_id) %>%
    summarize(
      start_time = min(elapsed_sec),
      end_time = max(elapsed_sec),
      duration = end_time - start_time,
      n_points = n(),
      mean_pressure = mean(load_g),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d non-zero segments\n", nrow(pass_segments)))
  
  # Merge segments separated by short zero gaps
  if (nrow(pass_segments) > 1) {
    pass_segments <- pass_segments %>%
      mutate(
        gap_to_next = lead(start_time) - end_time,
        merge_with_next = !is.na(gap_to_next) & gap_to_next < max_zero_gap_sec
      )
    
    # Build merged groups
    pass_segments$merged_pass_id <- NA
    current_pass <- 1
    pass_segments$merged_pass_id[1] <- current_pass
    
    for (i in 2:nrow(pass_segments)) {
      if (pass_segments$merge_with_next[i-1]) {
        pass_segments$merged_pass_id[i] <- current_pass
      } else {
        current_pass <- current_pass + 1
        pass_segments$merged_pass_id[i] <- current_pass
      }
    }
    
    passes <- pass_segments %>%
      group_by(merged_pass_id) %>%
      summarize(
        pass_start = min(start_time),
        pass_end = max(end_time),
        pass_duration = pass_end - pass_start,
        mean_pressure = mean(mean_pressure),
        .groups = "drop"
      )
    
    cat(sprintf("  After merging gaps < %.2fs: %d passes\n", max_zero_gap_sec, nrow(passes)))
  } else {
    passes <- pass_segments %>%
      mutate(
        merged_pass_id = 1,
        pass_start = start_time,
        pass_end = end_time,
        pass_duration = duration
      ) %>%
      select(merged_pass_id, pass_start, pass_end, pass_duration, mean_pressure)
  }
  
  # Assign pass IDs to data
  data <- data %>%
    mutate(pass_id = NA_integer_)
  
  for (i in 1:nrow(passes)) {
    data <- data %>%
      mutate(
        pass_id = ifelse(
          elapsed_sec >= passes$pass_start[i] & elapsed_sec <= passes$pass_end[i],
          i,
          pass_id
        )
      )
  }
  
  n_in_passes <- sum(!is.na(data$pass_id))
  pct_in_passes <- (n_in_passes / nrow(data)) * 100
  cat(sprintf("  Result: %d/%d points in passes (%.1f%%)\n\n", 
              n_in_passes, nrow(data), pct_in_passes))
  
  # ===========================================================================
  # PHASE 2: Calculate Rate of Change for All Data
  # ===========================================================================
  
  cat("PHASE 2: Calculating rate of change...\n")
  
  rate_data <- calculate_rate_of_change(data$load_g, data$elapsed_sec, smooth_window)
  data$rate_raw <- rate_data$rate_raw
  data$rate_smooth <- rate_data$rate_smooth
  
  cat(sprintf("  Rate range: %.1f to %.1f g/s\n", 
              min(data$rate_smooth, na.rm = TRUE),
              max(data$rate_smooth, na.rm = TRUE)))
  cat(sprintf("  Rate median: %.1f g/s\n\n", median(data$rate_smooth, na.rm = TRUE)))
  
  # ===========================================================================
  # PHASE 3: Detect Ramps and Stable Regions Within Each Pass
  # ===========================================================================
  
  cat("PHASE 3: Detecting ramps and stable regions within passes...\n")
  
  # Initialize region column
  data <- data %>%
    mutate(
      region = case_when(
        is.na(pass_id) ~ "zero",
        TRUE ~ "unknown"
      ),
      point_change = abs(load_g - lag(load_g, default = first(load_g)))
    )
  
  # Process each pass
  for (pass in unique(passes$merged_pass_id)) {
    pass_indices <- which(data$pass_id == pass)
    pass_data <- data[pass_indices, ]
    
    # Detect ramp boundaries
    boundaries <- detect_ramp_boundaries(pass_data, ramp_rate_threshold)
    
    # Mark regions
    for (i in seq_along(pass_indices)) {
      idx <- pass_indices[i]
      if (i <= boundaries$ramp_up_end) {
        data$region[idx] <- "ramp_up"
      } else if (i >= boundaries$ramp_down_start) {
        data$region[idx] <- "ramp_down"
      } else {
        data$region[idx] <- "stable"
      }
    }
  }
  
  # Count regions
  region_counts <- data %>%
    group_by(region) %>%
    summarize(n = n(), .groups = "drop")
  
  cat("  Region distribution:\n")
  for (i in 1:nrow(region_counts)) {
    pct <- (region_counts$n[i] / nrow(data)) * 100
    cat(sprintf("    %s: %d points (%.1f%%)\n", 
                region_counts$region[i], region_counts$n[i], pct))
  }
  cat("\n")
  
  # ===========================================================================
  # PHASE 4: Validate Stable Regions by Variation Criteria
  # ===========================================================================
  
  cat("PHASE 4: Validating stable regions with multiple criteria...\n")
  cat(sprintf("  Criterion 1: Point-to-point change < %.1fg (REQUIRED)\n", stable_max_change_g))
  cat(sprintf("  Criterion 2: Rolling SD < %.1fg (when available, %.1fs window)\n", 
              stable_max_sd_g, stable_sd_window_sec))
  
  # Calculate rolling standard deviation for regions marked as "stable"
  # Determine window size in observations
  sd_window_points <- round(stable_sd_window_sec / median_dt)
  sd_window_points <- max(3, min(sd_window_points, 50))  # Constrain to reasonable range
  
  cat(sprintf("  Rolling SD window: %d points (~%.2fs)\n", 
              sd_window_points, sd_window_points * median_dt))
  
  # Initialize rolling SD column
  data$rolling_sd <- NA
  
  # Calculate rolling SD only within candidate stable regions (not across ramps)
  # This prevents ramps from inflating the SD in stable regions
  for (pass in unique(passes$merged_pass_id)) {
    pass_indices <- which(data$pass_id == pass)
    stable_indices <- which(data$pass_id == pass & data$region == "stable")
    
    if (length(stable_indices) < sd_window_points) next
    
    # Only calculate SD for stable region points
    stable_load <- data$load_g[stable_indices]
    
    # Calculate rolling SD using zoo::rollapply
    rolling_sd_vals <- rollapply(
      stable_load,
      width = sd_window_points,
      FUN = sd,
      fill = NA,
      align = "center"
    )
    
    data$rolling_sd[stable_indices] <- rolling_sd_vals
  }
  
  # Mark points as truly stable with SMART criteria:
  # - If rolling_sd available: use both point_change AND rolling_sd criteria
  # - If rolling_sd is NA (edge points, short segments): use only point_change criterion
  # This captures edge points that would otherwise be rejected
  data <- data %>%
    mutate(
      is_stable = (region == "stable") & 
                  (point_change < stable_max_change_g) &
                  (is.na(rolling_sd) | rolling_sd < stable_max_sd_g)
    )
  
  # Identify stable segments
  data <- data %>%
    mutate(
      stable_change = is_stable != lag(is_stable, default = FALSE),
      stable_segment_id = cumsum(stable_change)
    ) %>%
    mutate(stable_segment_id = ifelse(is_stable, stable_segment_id, NA))
  
  stable_segments <- data %>%
    filter(!is.na(stable_segment_id)) %>%
    group_by(stable_segment_id, pass_id) %>%
    summarize(
      start_time = min(elapsed_sec),
      end_time = max(elapsed_sec),
      duration = end_time - start_time,
      n_points = n(),
      mean_pressure = mean(load_g),
      mean_change = mean(point_change),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d stable segments before duration filter\n", nrow(stable_segments)))
  
  # Filter by minimum duration
  stable_segments_filtered <- stable_segments %>%
    filter(duration >= min_stable_duration_sec)
  
  cat(sprintf("  After duration filter (>= %.2fs): %d stable segments\n", 
              min_stable_duration_sec, nrow(stable_segments_filtered)))
  
  # Report how many points passed each criterion
  stable_candidate_count <- sum(data$region == "stable", na.rm = TRUE)
  criterion1_pass <- sum(data$region == "stable" & data$point_change < stable_max_change_g, na.rm = TRUE)
  has_rolling_sd <- sum(data$region == "stable" & !is.na(data$rolling_sd), na.rm = TRUE)
  criterion2_pass <- sum(data$region == "stable" & !is.na(data$rolling_sd) & 
                    data$rolling_sd < stable_max_sd_g, na.rm = TRUE)
  na_rolling_sd <- sum(data$region == "stable" & is.na(data$rolling_sd), na.rm = TRUE)
  both_criteria <- sum(data$region == "stable" & data$point_change < stable_max_change_g &
                      !is.na(data$rolling_sd) & data$rolling_sd < stable_max_sd_g, na.rm = TRUE)
  
  cat(sprintf("  Criterion breakdown (from %d candidate points):\n", stable_candidate_count))
  cat(sprintf("    Point change < %.1fg: %d points (%.1f%%)\n", 
              stable_max_change_g, criterion1_pass, 100*criterion1_pass/stable_candidate_count))
  cat(sprintf("    Rolling SD available: %d points (%.1f%%)\n", 
              has_rolling_sd, 100*has_rolling_sd/stable_candidate_count))
  cat(sprintf("    Rolling SD < %.1fg: %d of %d with SD (%.1f%%)\n", 
              stable_max_sd_g, criterion2_pass, has_rolling_sd, 
              100*criterion2_pass/has_rolling_sd))
  cat(sprintf("    Rolling SD NA (edges/short): %d points (%.1f%%)\n",
              na_rolling_sd, 100*na_rolling_sd/stable_candidate_count))
  cat(sprintf("    Both criteria (when SD available): %d points\n", both_criteria))
  
  # Update is_stable based on filtered segments
  data$is_stable <- FALSE
  if (nrow(stable_segments_filtered) > 0) {
    for (i in 1:nrow(stable_segments_filtered)) {
      data <- data %>%
        mutate(
          is_stable = ifelse(
            elapsed_sec >= stable_segments_filtered$start_time[i] & 
            elapsed_sec <= stable_segments_filtered$end_time[i],
            TRUE,
            is_stable
          )
        )
    }
  }
  
  n_stable <- sum(data$is_stable)
  pct_stable <- (n_stable / nrow(data)) * 100
  
  cat(sprintf("  Final result: %d/%d points in stable regions (%.1f%%)\n\n", 
              n_stable, nrow(data), pct_stable))
  
  # ===========================================================================
  # SUMMARY
  # ===========================================================================
  
  cat("==============================================================================\n")
  cat("PASS SUMMARY\n")
  cat("==============================================================================\n\n")
  
  for (i in 1:min(10, nrow(passes))) {
    pass_stable <- stable_segments_filtered %>% filter(pass_id == i)
    stable_time <- sum(pass_stable$duration)
    stable_pct <- (stable_time / passes$pass_duration[i]) * 100
    
    cat(sprintf("Pass %d: %.2fs - %.2fs (duration: %.2fs, mean: %.1fg)\n",
                i, passes$pass_start[i], passes$pass_end[i],
                passes$pass_duration[i], passes$mean_pressure[i]))
    cat(sprintf("  Stable segments: %d (total: %.2fs, %.1f%% of pass)\n\n",
                nrow(pass_stable), stable_time, stable_pct))
  }
  if (nrow(passes) > 10) {
    cat(sprintf("... and %d more passes\n\n", nrow(passes) - 10))
  }
  
  cat("==============================================================================\n\n")
  
  # Clean up temporary columns
  data <- data %>%
    select(-is_nonzero, -zero_change, -pass_segment_id, -stable_change, -stable_segment_id)
  
  return(data)
}


#' Calculate statistics for stable contact regions only
#' 
#' @param data Data frame with pass_id, region, is_stable columns
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with comprehensive statistics
#' @export
calculate_stable_contact_statistics <- function(data, target_pressure_g) {
  
  if (!"is_stable" %in% colnames(data)) {
    stop("Stable region detection must be run first (identify_stable_contact_regions_v4).")
  }
  
  # Filter to different regions
  pass_data <- data %>% filter(!is.na(pass_id))
  stable_data <- data %>% filter(is_stable == TRUE)
  ramp_up_data <- data %>% filter(region == "ramp_up")
  ramp_down_data <- data %>% filter(region == "ramp_down")
  
  if (nrow(pass_data) == 0) {
    warning("No passes detected!")
    return(NULL)
  }
  
  # Calculate time durations
  # Use observation count × median sampling interval for accurate duration
  median_dt <- median(diff(data$elapsed_sec), na.rm = TRUE)
  total_duration <- max(data$elapsed_sec) - min(data$elapsed_sec)
  
  pass_duration <- nrow(pass_data) * median_dt
  stable_duration <- nrow(stable_data) * median_dt
  ramp_up_duration <- nrow(ramp_up_data) * median_dt
  ramp_down_duration <- nrow(ramp_down_data) * median_dt
  
  n_passes <- max(pass_data$pass_id, na.rm = TRUE)
  
  # Count stable segments
  data_temp <- data %>%
    mutate(
      stable_change = is_stable != lag(is_stable, default = FALSE),
      segment_id = cumsum(stable_change)
    )
  n_stable_segments <- data_temp %>%
    filter(is_stable == TRUE) %>%
    pull(segment_id) %>%
    n_distinct()
  
  # Compile statistics
  stats <- data.frame(
    run_id = first(data$run_id),
    target_pressure_g = target_pressure_g,
    
    # Pass metrics
    n_passes = n_passes,
    n_stable_segments = n_stable_segments,
    mean_stable_segments_per_pass = n_stable_segments / n_passes,
    
    # Time metrics
    total_trace_duration_sec = total_duration,
    total_pass_time_sec = pass_duration,
    total_stable_time_sec = stable_duration,
    total_ramp_up_time_sec = ramp_up_duration,
    total_ramp_down_time_sec = ramp_down_duration,
    
    percent_time_in_passes = (pass_duration / total_duration) * 100,
    percent_time_stable = (stable_duration / total_duration) * 100,
    percent_time_ramp_up = (ramp_up_duration / total_duration) * 100,
    percent_time_ramp_down = (ramp_down_duration / total_duration) * 100,
    
    # Duration metrics
    mean_pass_duration_sec = pass_duration / n_passes,
    mean_stable_segment_duration_sec = if (n_stable_segments > 0) stable_duration / n_stable_segments else NA,
    mean_ramp_up_duration_sec = ramp_up_duration / n_passes,
    mean_ramp_down_duration_sec = ramp_down_duration / n_passes,
    
    # Pressure metrics (stable regions only)
    mean_stable_pressure_g = if (nrow(stable_data) > 0) mean(stable_data$load_g) else NA,
    sd_stable_pressure_g = if (nrow(stable_data) > 0) sd(stable_data$load_g) else NA,
    cv_stable_percent = if (nrow(stable_data) > 0) (sd(stable_data$load_g) / mean(stable_data$load_g)) * 100 else NA,
    min_stable_pressure_g = if (nrow(stable_data) > 0) min(stable_data$load_g) else NA,
    max_stable_pressure_g = if (nrow(stable_data) > 0) max(stable_data$load_g) else NA,
    median_stable_pressure_g = if (nrow(stable_data) > 0) median(stable_data$load_g) else NA,
    
    # Observation counts
    n_total_observations = nrow(data),
    n_pass_observations = nrow(pass_data),
    n_stable_observations = nrow(stable_data),
    n_ramp_up_observations = nrow(ramp_up_data),
    n_ramp_down_observations = nrow(ramp_down_data)
  )
  
  return(stats)
}


#' Read pressure trace CSV file
#' 
#' @param run_id Sample identifier (e.g., "PILOT_001")
#' @param traces_path Path to directory containing CSV files
#' @return Data frame with pressure trace data
#' @export
read_pressure_trace_file <- function(run_id, traces_path) {
  file_path <- file.path(traces_path, paste0(run_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read file as text lines
  lines <- readLines(file_path, warn = FALSE)
  data_lines <- lines[2:length(lines)]  # Skip header
  
  # Parse each line
  data_list <- list()
  for (line in data_lines) {
    # Skip lines with text headers
    if (grepl("analog|load|distance|timestamp", line, ignore.case = TRUE)) {
      next
    }
    
    parts <- strsplit(line, ",")[[1]]
    
    if (length(parts) >= 5) {
      # Check if numeric columns are valid
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
  
  # Combine and process
  df <- bind_rows(data_list)
  df <- df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g))
  
  return(df)
}
