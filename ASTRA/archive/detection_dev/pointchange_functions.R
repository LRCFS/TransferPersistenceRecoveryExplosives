# ==============================================================================
# Point-to-Point Change Detection Functions
# ==============================================================================
# 
# Detects stable swabbing regions based on small point-to-point pressure changes.
# Stable = consecutive points where |ΔP| < threshold (e.g., < 1.5g)
#
# Author: ASTRA Analysis System  
# Date: 2026-07-16
# Version: 3.0-pointchange
# ==============================================================================

#' Identify stable regions using point-to-point change detection
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams (for reference)
#' @param max_point_change_g Maximum point-to-point change for stability (default: 2.0g)
#' @param min_pressure_g Minimum pressure for contact (default: 30g for 50g, 100g for 200g)
#' @param min_stable_duration_sec Minimum stable segment duration (default: 0.8)
#' @param max_stable_duration_sec Maximum stable segment duration (default: 3.0)
#' @param cluster_gap_sec Maximum gap to cluster segments into passes (default: 15.0)
#' @return Data frame with pass assignments
#' @export
identify_pointchange_passes <- function(data,
                                         target_pressure_g,
                                         max_point_change_g = 2.0,
                                         min_pressure_g = NULL,
                                         min_stable_duration_sec = 0.8,
                                         max_stable_duration_sec = 3.0,
                                         cluster_gap_sec = 15.0) {
  
  library(tidyverse)
  
  # Auto-calculate min pressure
  if (is.null(min_pressure_g)) {
    min_pressure_g <- ifelse(target_pressure_g >= 100, 100, 30)
  }
  
  # Sort by time
  data <- data %>% arrange(elapsed_sec)
  
  median_dt <- median(diff(data$elapsed_sec), na.rm = TRUE)
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, 1/median_dt))
  cat(sprintf("  Max point-to-point change: %.2fg\n", max_point_change_g))
  cat(sprintf("  Min pressure: %.1fg\n", min_pressure_g))
  cat(sprintf("  Stable segment duration: %.1f - %.1f seconds\n",
              min_stable_duration_sec, max_stable_duration_sec))
  cat(sprintf("  Clustering gap: %.1f seconds\n", cluster_gap_sec))
  
  # ============================================================================
  # STEP 1: Calculate Point-to-Point Changes
  # ============================================================================
  
  cat("  Calculating point-to-point changes...\n")
  
  data <- data %>%
    mutate(
      point_change = abs(load_g - lag(load_g, default = first(load_g))),
      is_stable_point = point_change < max_point_change_g & load_g >= min_pressure_g,
      change_threshold = max_point_change_g,
      pressure_min = min_pressure_g
    )
  
  n_stable <- sum(data$is_stable_point, na.rm = TRUE)
  pct_stable <- (n_stable / nrow(data)) * 100
  cat(sprintf("  Stable points: %d / %d (%.1f%%)\n", n_stable, nrow(data), pct_stable))
  
  # ============================================================================
  # STEP 2: Segment Consecutive Stable Points
  # ============================================================================
  
  cat("  Segmenting consecutive stable points...\n")
  
  data <- data %>%
    mutate(
      stable_change = is_stable_point != lag(is_stable_point, default = FALSE),
      segment_id = cumsum(stable_change)
    ) %>%
    mutate(segment_id = ifelse(is_stable_point, segment_id, NA))
  
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
      mean_point_change = mean(point_change, na.rm = TRUE),
      max_point_change = max(point_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d raw segments\n", nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No stable segments found!")
    data <- data %>%
      mutate(is_contact = FALSE, pass_id = NA_integer_) %>%
      select(-stable_change, -segment_id, -is_stable_point)
    return(data)
  }
  
  # ============================================================================
  # STEP 3: Filter by Duration
  # ============================================================================
  
  cat(sprintf("  Filtering segments (%.1fs - %.1fs)...\n",
              min_stable_duration_sec, max_stable_duration_sec))
  
  segments <- segments %>%
    filter(duration >= min_stable_duration_sec,
           duration <= max_stable_duration_sec)
  
  cat(sprintf("  Valid segments: %d\n", nrow(segments)))
  
  if (nrow(segments) == 0) {
    warning("No segments met duration criteria!")
    data <- data %>%
      mutate(is_contact = FALSE, pass_id = NA_integer_) %>%
      select(-stable_change, -segment_id, -is_stable_point)
    return(data)
  }
  
  # ============================================================================
  # STEP 4: Cluster Segments Into Passes
  # ============================================================================
  
  cat(sprintf("  Clustering segments (gap < %.1fs)...\n", cluster_gap_sec))
  
  segments <- segments %>%
    mutate(
      gap_to_next = lead(start_time) - end_time,
      cluster_with_next = !is.na(gap_to_next) & gap_to_next < cluster_gap_sec
    )
  
  # Build clusters
  segments$cluster_id <- NA
  current_cluster <- 1
  segments$cluster_id[1] <- current_cluster
  
  for (i in 2:nrow(segments)) {
    if (segments$cluster_with_next[i-1]) {
      segments$cluster_id[i] <- current_cluster
    } else {
      current_cluster <- current_cluster + 1
      segments$cluster_id[i] <- current_cluster
    }
  }
  
  # Summarize passes
  passes <- segments %>%
    group_by(cluster_id) %>%
    summarize(
      pass_start = min(start_time),
      pass_end = max(end_time),
      pass_duration = pass_end - pass_start,
      n_segments = n(),
      mean_pressure = mean(mean_pressure),
      total_stable_time = sum(duration),
      .groups = "drop"
    )
  
  cat(sprintf("  Created %d passes\n", nrow(passes)))
  
  # ============================================================================
  # STEP 5: Map Back to Data
  # ============================================================================
  
  segments <- segments %>%
    inner_join(passes %>% select(cluster_id), by = "cluster_id")
  
  data <- data %>%
    mutate(
      pass_id = NA_integer_,
      is_contact = FALSE,
      is_stable_segment = FALSE
    )
  
  # Mark pass ranges
  for (i in 1:nrow(passes)) {
    data <- data %>%
      mutate(
        pass_id = ifelse(
          elapsed_sec >= passes$pass_start[i] & elapsed_sec <= passes$pass_end[i],
          i, pass_id
        ),
        is_contact = ifelse(
          elapsed_sec >= passes$pass_start[i] & elapsed_sec <= passes$pass_end[i],
          TRUE, is_contact
        )
      )
  }
  
  # Mark actual stable segments
  for (i in 1:nrow(segments)) {
    data <- data %>%
      mutate(
        is_stable_segment = ifelse(
          elapsed_sec >= segments$start_time[i] & elapsed_sec <= segments$end_time[i],
          TRUE, is_stable_segment
        )
      )
  }
  
  data <- data %>%
    select(-stable_change, -segment_id, -is_stable_point)
  
  n_contact <- sum(data$is_contact)
  pct_contact <- (n_contact / nrow(data)) * 100
  n_seg <- sum(data$is_stable_segment)
  pct_seg <- (n_seg / nrow(data)) * 100
  
  cat(sprintf("  Result: %d/%d points in passes (%.1f%%)\n",
              n_contact, nrow(data), pct_contact))
  cat(sprintf("  Of which %d in stable segments (%.1f%%)\n",
              n_seg, nrow(data), pct_seg))
  
  cat("\n  Detected Passes:\n")
  for (i in 1:min(10, nrow(passes))) {
    cat(sprintf("    Pass %d: %.1fs - %.1fs (%.1fs span, %d segments, %.1fg mean)\n",
                i, passes$pass_start[i], passes$pass_end[i],
                passes$pass_duration[i], passes$n_segments[i], passes$mean_pressure[i]))
  }
  if (nrow(passes) > 10) {
    cat(sprintf("    ... and %d more\n", nrow(passes) - 10))
  }
  cat("\n")
  
  return(data)
}


#' Calculate statistics
#' @export
calculate_pointchange_statistics <- function(data, target_pressure_g) {
  if (!"is_contact" %in% colnames(data)) {
    stop("Detection must be run first.")
  }
  
  contact_data <- data %>% filter(is_contact == TRUE)
  segment_data <- data %>% filter(is_stable_segment == TRUE)
  
  if (nrow(contact_data) == 0) return(NULL)
  
  total_duration <- max(data$elapsed_sec) - min(data$elapsed_sec)
  
  contact_duration <- contact_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total = sum(time_diff)) %>% pull(total)
  
  segment_duration <- segment_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total = sum(time_diff)) %>% pull(total)
  
  n_passes <- max(contact_data$pass_id, na.rm = TRUE)
  
  data.frame(
    run_id = first(data$run_id),
    mean_contact_pressure_g = mean(contact_data$load_g),
    cv_contact_percent = (sd(contact_data$load_g) / mean(contact_data$load_g)) * 100,
    mean_segment_pressure_g = mean(segment_data$load_g),
    cv_segment_percent = (sd(segment_data$load_g) / mean(segment_data$load_g)) * 100,
    percent_time_in_passes = (contact_duration / total_duration) * 100,
    percent_time_in_segments = (segment_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = contact_duration / n_passes
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
