# ==============================================================================
# Micro-Plateau Clustering Detection Functions
# ==============================================================================
# 
# This approach detects short stable micro-plateaus (0.5-2 seconds) where
# pressure is relatively flat, then clusters them into swabbing "passes".
# 
# Key concept: A swabbing pass consists of multiple brief contacts with the
# surface, each showing stable pressure. These micro-plateaus occur within
# a larger time window and collectively represent one swabbing action.
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-microplateau
# ==============================================================================

#' Identify micro-plateaus and cluster them into swabbing passes
#' 
#' @param data Data frame with columns: elapsed_sec, load_g, run_id
#' @param target_pressure_g Target pressure in grams (for reference)
#' @param min_pressure_g Minimum pressure for contact (default: 30g for 50g, 100g for 200g)
#' @param max_derivative_g_per_sec Maximum derivative for "flat" (default: 10)
#' @param micro_min_duration_sec Minimum micro-plateau duration (default: 0.5)
#' @param micro_max_duration_sec Maximum micro-plateau duration (default: 3.0)
#' @param cluster_gap_sec Maximum gap between micro-plateaus to group into pass (default: 5.0)
#' @param pass_min_duration_sec Minimum total pass duration (default: 3.0)
#' @return Data frame with pass assignments
#' @export
identify_microplateau_passes <- function(data, 
                                          target_pressure_g,
                                          min_pressure_g = NULL,
                                          max_derivative_g_per_sec = 10,
                                          micro_min_duration_sec = 0.5,
                                          micro_max_duration_sec = 3.0,
                                          cluster_gap_sec = 5.0,
                                          pass_min_duration_sec = 3.0) {
  
  # Load required package
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required. Install with: install.packages('zoo')")
  }
  library(zoo)
  
  # Auto-calculate min pressure if not provided
  if (is.null(min_pressure_g)) {
    if (target_pressure_g >= 100) {
      min_pressure_g <- 100  # 100g for 200g samples
    } else {
      min_pressure_g <- 30   # 30g for 50g samples
    }
  }
  
  # Sort by time
  data <- data %>% arrange(elapsed_sec)
  
  # Estimate sampling rate
  time_diffs <- diff(data$elapsed_sec)
  median_dt <- median(time_diffs, na.rm = TRUE)
  sampling_rate <- 1 / median_dt
  
  cat(sprintf("  Sampling rate: %.3f seconds (%.1f Hz)\n", median_dt, sampling_rate))
  cat(sprintf("  Min pressure: %.1fg\n", min_pressure_g))
  cat(sprintf("  Max derivative: %.1f g/s (for flatness)\n", max_derivative_g_per_sec))
  cat(sprintf("  Micro-plateau duration: %.1f - %.1f seconds\n", 
              micro_min_duration_sec, micro_max_duration_sec))
  cat(sprintf("  Clustering gap: %.1f seconds\n", cluster_gap_sec))
  cat(sprintf("  Pass min duration: %.1f seconds\n", pass_min_duration_sec))
  
  # ============================================================================
  # STEP 1: Calculate Derivatives
  # ============================================================================
  
  cat("  Calculating derivatives...\n")
  
  # Use 5-point derivative for smoothness
  data <- data %>%
    mutate(
      derivative = (lead(load_g, 2) - lag(load_g, 2)) / 
                   (lead(elapsed_sec, 2) - lag(elapsed_sec, 2)),
      abs_derivative = abs(derivative)
    )
  
  # ============================================================================
  # STEP 2: Mark Potential Micro-Plateau Points
  # ============================================================================
  
  cat("  Identifying flat regions...\n")
  
  data <- data %>%
    mutate(
      is_flat = !is.na(derivative) &
                abs_derivative < max_derivative_g_per_sec &
                load_g >= min_pressure_g,
      derivative_threshold = max_derivative_g_per_sec,
      pressure_min = min_pressure_g
    )
  
  n_flat <- sum(data$is_flat, na.rm = TRUE)
  pct_flat <- (n_flat / nrow(data)) * 100
  cat(sprintf("  Flat points: %d / %d (%.1f%%)\n", n_flat, nrow(data), pct_flat))
  
  # ============================================================================
  # STEP 3: Segment Into Micro-Plateaus
  # ============================================================================
  
  cat("  Segmenting into micro-plateaus...\n")
  
  data <- data %>%
    mutate(
      flat_change = is_flat != lag(is_flat, default = FALSE),
      segment_id = cumsum(flat_change)
    ) %>%
    mutate(segment_id = ifelse(is_flat, segment_id, NA))
  
  # Get micro-plateau segments
  micro_segments <- data %>%
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
      max_derivative = max(abs_derivative, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  cat(sprintf("  Found %d raw segments\n", nrow(micro_segments)))
  
  if (nrow(micro_segments) == 0) {
    warning("No flat segments found!")
    data <- data %>%
      mutate(is_contact = FALSE, pass_id = NA_integer_) %>%
      select(-flat_change, -segment_id, -is_flat)
    return(data)
  }
  
  # ============================================================================
  # STEP 4: Filter Micro-Plateaus by Duration
  # ============================================================================
  
  cat(sprintf("  Filtering micro-plateaus (%.1fs - %.1fs)...\n", 
              micro_min_duration_sec, micro_max_duration_sec))
  
  micro_segments <- micro_segments %>%
    filter(duration >= micro_min_duration_sec, 
           duration <= micro_max_duration_sec)
  
  cat(sprintf("  Valid micro-plateaus: %d\n", nrow(micro_segments)))
  
  if (nrow(micro_segments) == 0) {
    warning("No micro-plateaus met duration criteria!")
    data <- data %>%
      mutate(is_contact = FALSE, pass_id = NA_integer_) %>%
      select(-flat_change, -segment_id, -is_flat)
    return(data)
  }
  
  # ============================================================================
  # STEP 5: Cluster Micro-Plateaus Into Passes
  # ============================================================================
  
  cat(sprintf("  Clustering micro-plateaus (gap < %.1fs)...\n", cluster_gap_sec))
  
  # Calculate gaps between consecutive micro-plateaus
  micro_segments <- micro_segments %>%
    mutate(
      gap_to_next = lead(start_time) - end_time,
      cluster_with_next = !is.na(gap_to_next) & gap_to_next < cluster_gap_sec
    )
  
  # Build cluster groups
  micro_segments$cluster_id <- NA
  current_cluster <- 1
  micro_segments$cluster_id[1] <- current_cluster
  
  for (i in 2:nrow(micro_segments)) {
    if (micro_segments$cluster_with_next[i-1]) {
      micro_segments$cluster_id[i] <- current_cluster
    } else {
      current_cluster <- current_cluster + 1
      micro_segments$cluster_id[i] <- current_cluster
    }
  }
  
  # Summarize clusters (passes)
  pass_clusters <- micro_segments %>%
    group_by(cluster_id) %>%
    summarize(
      pass_start = min(start_time),
      pass_end = max(end_time),
      pass_duration = pass_end - pass_start,
      n_micro_plateaus = n(),
      mean_pressure = mean(mean_pressure),
      total_plateau_time = sum(duration),
      .groups = "drop"
    )
  
  cat(sprintf("  Created %d pass clusters\n", nrow(pass_clusters)))
  
  # ============================================================================
  # STEP 6: Filter Passes by Duration
  # ============================================================================
  
  cat(sprintf("  Filtering passes (min %.1fs total duration)...\n", pass_min_duration_sec))
  
  pass_clusters <- pass_clusters %>%
    filter(pass_duration >= pass_min_duration_sec)
  
  cat(sprintf("  Final passes: %d\n", nrow(pass_clusters)))
  
  if (nrow(pass_clusters) == 0) {
    warning("No passes met duration criteria!")
    data <- data %>%
      mutate(is_contact = FALSE, pass_id = NA_integer_) %>%
      select(-flat_change, -segment_id, -is_flat)
    return(data)
  }
  
  # ============================================================================
  # STEP 7: Map Passes Back to Original Data
  # ============================================================================
  
  # Map each micro-plateau to its pass
  micro_segments <- micro_segments %>%
    inner_join(pass_clusters %>% select(cluster_id), by = "cluster_id")
  
  # Mark all points within pass time ranges as contact
  data <- data %>%
    mutate(
      pass_id = NA_integer_,
      is_contact = FALSE,
      is_micro_plateau = FALSE
    )
  
  for (i in 1:nrow(pass_clusters)) {
    data <- data %>%
      mutate(
        pass_id = ifelse(
          elapsed_sec >= pass_clusters$pass_start[i] & 
          elapsed_sec <= pass_clusters$pass_end[i],
          i,
          pass_id
        ),
        is_contact = ifelse(
          elapsed_sec >= pass_clusters$pass_start[i] & 
          elapsed_sec <= pass_clusters$pass_end[i],
          TRUE,
          is_contact
        )
      )
  }
  
  # Also mark which points are actually in micro-plateaus
  for (i in 1:nrow(micro_segments)) {
    data <- data %>%
      mutate(
        is_micro_plateau = ifelse(
          elapsed_sec >= micro_segments$start_time[i] & 
          elapsed_sec <= micro_segments$end_time[i],
          TRUE,
          is_micro_plateau
        )
      )
  }
  
  # Clean up
  data <- data %>%
    select(-flat_change, -segment_id, -is_flat)
  
  # Calculate summary
  n_contact <- sum(data$is_contact)
  pct_contact <- (n_contact / nrow(data)) * 100
  n_plateau <- sum(data$is_micro_plateau)
  pct_plateau <- (n_plateau / nrow(data)) * 100
  
  cat(sprintf("  Result: %d/%d points in passes (%.1f%%)\n", 
              n_contact, nrow(data), pct_contact))
  cat(sprintf("  Of which %d points in micro-plateaus (%.1f%%)\n", 
              n_plateau, nrow(data), pct_plateau))
  
  # Print pass summary
  cat("\n  Detected Passes:\n")
  for (i in 1:min(10, nrow(pass_clusters))) {
    cat(sprintf("    Pass %d: %.1fs - %.1fs (%.1fs span, %d micro-plateaus, %.1fg mean)\n",
                i,
                pass_clusters$pass_start[i],
                pass_clusters$pass_end[i],
                pass_clusters$pass_duration[i],
                pass_clusters$n_micro_plateaus[i],
                pass_clusters$mean_pressure[i]))
  }
  if (nrow(pass_clusters) > 10) {
    cat(sprintf("    ... and %d more passes\n", nrow(pass_clusters) - 10))
  }
  cat("\n")
  
  return(data)
}


#' Calculate statistics for micro-plateau passes
#' 
#' @param data Data frame with pass assignments
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with statistics
#' @export
calculate_microplateau_statistics <- function(data, target_pressure_g) {
  
  if (!"is_contact" %in% colnames(data)) {
    stop("Micro-plateau detection must be run first.")
  }
  
  contact_data <- data %>% filter(is_contact == TRUE)
  plateau_data <- data %>% filter(is_micro_plateau == TRUE)
  
  if (nrow(contact_data) == 0) {
    warning("No contact periods found!")
    return(NULL)
  }
  
  # Calculate time metrics
  total_duration <- max(data$elapsed_sec, na.rm = TRUE) - 
                   min(data$elapsed_sec, na.rm = TRUE)
  
  contact_duration <- contact_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total = sum(time_diff, na.rm = TRUE)) %>%
    pull(total)
  
  plateau_duration <- plateau_data %>%
    arrange(elapsed_sec) %>%
    mutate(time_diff = elapsed_sec - lag(elapsed_sec, default = first(elapsed_sec))) %>%
    summarize(total = sum(time_diff, na.rm = TRUE)) %>%
    pull(total)
  
  n_passes <- max(contact_data$pass_id, na.rm = TRUE)
  
  stats <- data.frame(
    run_id = first(data$run_id),
    mean_contact_pressure_g = mean(contact_data$load_g, na.rm = TRUE),
    sd_contact_pressure_g = sd(contact_data$load_g, na.rm = TRUE),
    cv_contact_percent = (sd(contact_data$load_g, na.rm = TRUE) / 
                         mean(contact_data$load_g, na.rm = TRUE)) * 100,
    mean_plateau_pressure_g = mean(plateau_data$load_g, na.rm = TRUE),
    cv_plateau_percent = (sd(plateau_data$load_g, na.rm = TRUE) / 
                         mean(plateau_data$load_g, na.rm = TRUE)) * 100,
    n_contact_observations = nrow(contact_data),
    n_plateau_observations = nrow(plateau_data),
    total_pass_time_sec = contact_duration,
    total_plateau_time_sec = plateau_duration,
    total_trace_duration_sec = total_duration,
    percent_time_in_passes = (contact_duration / total_duration) * 100,
    percent_time_in_plateaus = (plateau_duration / total_duration) * 100,
    number_of_passes = n_passes,
    mean_pass_duration_sec = contact_duration / n_passes
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
  
  lines <- readLines(file_path, warn = FALSE)
  data_lines <- lines[2:length(lines)]
  
  data_list <- list()
  for (i in seq_along(data_lines)) {
    line <- data_lines[i]
    if (grepl("analog|load|distance|timestamp", line, ignore.case = TRUE)) next
    
    parts <- strsplit(line, ",")[[1]]
    if (length(parts) >= 5) {
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
  
  df <- bind_rows(data_list)
  df <- df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g))
  
  return(df)
}
