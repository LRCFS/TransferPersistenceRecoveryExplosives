# ==============================================================================
# Pass-Based Stable Region Detection with Ramp Exclusion
# ==============================================================================
# 
# Algorithm:
# 1. Identify passes (separated by zero-pressure periods)
# 2. Within each pass, find stable regions (low point-to-point change)
# 3. Exclude ramp-up (rapid increase) and ramp-down (rapid decrease)
# 4. Keep stable middle sections
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-final
# ==============================================================================

#' Identify swabbing passes and stable regions with ramp exclusion
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams
#' @param zero_threshold Pressure below this = not in contact (default: 5g)
#' @param max_zero_gap_sec Maximum zero gap to still be same pass (default: 0.5s)
#' @param max_point_change_g Max point-to-point change for stable (default: 2.0g)
#' @param min_stable_duration_sec Minimum stable region duration (default: 0.5s)
#' @return Data frame with pass and stable region flags
#' @export
identify_passes_with_stable_regions <- function(data,
                                                 target_pressure_g,
                                                 zero_threshold = 5,
                                                 max_zero_gap_sec = 0.5,
                                                 max_point_change_g = 2.0,
                                                 min_stable_duration_sec = 0.5) {
  
  library(tidyverse)
  
  data <- data %>% arrange(elapsed_sec)
  
  median_dt <- median(diff(data$elapsed_sec), na.rm = TRUE)
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, 1/median_dt))
  cat(sprintf("  Zero threshold: %.1fg\n", zero_threshold))
  cat(sprintf("  Max zero gap: %.1fs\n", max_zero_gap_sec))
  cat(sprintf("  Max point-to-point change: %.1fg\n", max_point_change_g))
  cat(sprintf("  Min stable duration: %.1fs\n", min_stable_duration_sec))
  
  # ============================================================================
  # STEP 1: Identify Non-Zero Contact Regions (Passes)
  # ============================================================================
  
  cat("  Identifying passes (non-zero regions)...\n")
  
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
    
    cat(sprintf("  After merging: %d passes\n", nrow(passes)))
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
  
  # ============================================================================
  # STEP 2: Assign Pass IDs to Data
  # ============================================================================
  
  data <- data %>%
    mutate(pass_id = NA_integer_)
  
  for (i in 1:nrow(passes)) {
    data <- data %>%
      mutate(
        pass_id = ifelse(
          elapsed_sec >= passes$pass_start[i] & elapsed_sec <= passes$pass_end[i],
          i, pass_id
        )
      )
  }
  
  # ============================================================================
  # STEP 3: Within Each Pass, Find Stable Regions
  # ============================================================================
  
  cat("  Finding stable regions within passes...\n")
  
  # Calculate point-to-point change
  data <- data %>%
    mutate(
      point_change = abs(load_g - lag(load_g, default = first(load_g))),
      is_stable_point = !is.na(pass_id) & point_change < max_point_change_g,
      change_threshold = max_point_change_g
    )
  
  n_stable <- sum(data$is_stable_point, na.rm = TRUE)
  pct_stable <- (n_stable / nrow(data)) * 100
  cat(sprintf("  Stable points (low change): %d / %d (%.1f%%)\n",
              n_stable, nrow(data), pct_stable))
  
  # Segment stable points
  data <- data %>%
    mutate(
      stable_change = is_stable_point != lag(is_stable_point, default = FALSE),
      stable_segment_id = cumsum(stable_change)
    ) %>%
    mutate(stable_segment_id = ifelse(is_stable_point, stable_segment_id, NA))
  
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
  
  cat(sprintf("  Found %d raw stable segments\n", nrow(stable_segments)))
  
  # Filter by duration
  stable_segments <- stable_segments %>%
    filter(duration >= min_stable_duration_sec)
  
  cat(sprintf("  After duration filter: %d stable segments\n", nrow(stable_segments)))
  
  # ============================================================================
  # STEP 4: Mark Stable Regions in Data
  # ============================================================================
  
  data <- data %>%
    mutate(is_stable = FALSE)
  
  if (nrow(stable_segments) > 0) {
    for (i in 1:nrow(stable_segments)) {
      data <- data %>%
        mutate(
          is_stable = ifelse(
            elapsed_sec >= stable_segments$start_time[i] & 
            elapsed_sec <= stable_segments$end_time[i],
            TRUE, is_stable
          )
        )
    }
  }
  
  # Clean up
  data <- data %>%
    select(-is_nonzero, -zero_change, -pass_segment_id, -is_stable_point,
           -stable_change, -stable_segment_id)
  
  # Summary
  n_in_passes <- sum(!is.na(data$pass_id))
  pct_in_passes <- (n_in_passes / nrow(data)) * 100
  n_stable_final <- sum(data$is_stable)
  pct_stable_final <- (n_stable_final / nrow(data)) * 100
  
  cat(sprintf("  Result: %d/%d points in passes (%.1f%%)\n",
              n_in_passes, nrow(data), pct_in_passes))
  cat(sprintf("          %d/%d points in stable regions (%.1f%%)\n",
              n_stable_final, nrow(data), pct_stable_final))
  
  # Print pass summary
  cat("\n  Detected Passes:\n")
  for (i in 1:min(10, nrow(passes))) {
    pass_stable <- stable_segments %>% filter(pass_id == i)
    cat(sprintf("    Pass %d: %.1fs - %.1fs (%.1fs, %.1fg mean, %d stable segments)\n",
                i, passes$pass_start[i], passes$pass_end[i],
                passes$pass_duration[i], passes$mean_pressure[i],
                nrow(pass_stable)))
  }
  if (nrow(passes) > 10) {
    cat(sprintf("    ... and %d more passes\n", nrow(passes) - 10))
  }
  cat("\n")
  
  return(data)
}


#' Calculate statistics
#' @export
calculate_pass_statistics <- function(data, target_pressure_g) {
  if (!"pass_id" %in% colnames(data)) {
    stop("Pass detection must be run first.")
  }
  
  pass_data <- data %>% filter(!is.na(pass_id))
  stable_data <- data %>% filter(is_stable == TRUE)
  
  if (nrow(pass_data) == 0) return(NULL)
  
  total_duration <- max(data$elapsed_sec) - min(data$elapsed_sec)
  
  pass_duration <- pass_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total = sum(time_diff)) %>% pull(total)
  
  stable_duration <- if(nrow(stable_data) > 0) {
    stable_data %>%
      arrange(elapsed_sec) %>%
      mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
      summarize(total = sum(time_diff)) %>% pull(total)
  } else { 0 }
  
  n_passes <- max(pass_data$pass_id, na.rm = TRUE)
  
  data.frame(
    run_id = first(data$run_id),
    mean_pass_pressure_g = mean(pass_data$load_g),
    cv_pass_percent = (sd(pass_data$load_g) / mean(pass_data$load_g)) * 100,
    mean_stable_pressure_g = if(nrow(stable_data) > 0) mean(stable_data$load_g) else NA,
    cv_stable_percent = if(nrow(stable_data) > 0) (sd(stable_data$load_g) / mean(stable_data$load_g)) * 100 else NA,
    percent_time_in_passes = (pass_duration / total_duration) * 100,
    percent_time_stable = (stable_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = pass_duration / n_passes
  )
}


#' Read pressure trace
#' @export
read_pressure_trace_file <- function(run_id, traces_path) {
  file_path <- file.path(traces_path, paste0(run_id, ".csv"))
  if (!file.exists(file_path)) return(NULL)
  
  lines <- readLines(file_path, warn = FALSE)
  data_lines <- lines[2:length(lines)]
  
  data_list <- list()
  for (line in data_lines) {
    if (grepl("analog|load|distance|timestamp", line, ignore.case = TRUE)) next
    parts <- strsplit(line, ",")[[1]]
    if (length(parts) >= 5) {
      if (!is.na(suppressWarnings(as.numeric(parts[3]))) &&
          !is.na(suppressWarnings(as.numeric(parts[4])))) {
        data_list[[length(data_list) + 1]] <- data.frame(
          timestamp = parts[1], elapsed_sec = as.numeric(parts[2]),
          analog_value = as.numeric(parts[3]), load_g = as.numeric(parts[4]),
          distance_mm = as.numeric(parts[5]), stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(data_list) == 0) return(NULL)
  
  df <- bind_rows(data_list)
  df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g))
}
