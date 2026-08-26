# ==============================================================================
# ASTRA Swabbing Pilot Study - Pressure Trace Analysis (UPDATED)
# ==============================================================================
# 
# This script analyzes pressure trace data from the ASTRA swabbing pilot study.
# It creates individual plots for each sample, grouped plots by pressure level 
# and surface type, and performs statistical analysis.
#
# KEY FEATURES:
# - Contact detection: Identifies periods within ±10% of target pressure
# - Merges contact periods separated by < 0.2 seconds
# - Generates full trace and contact-only visualizations
# - Calculates statistics based on contact periods only
# - Includes zoomed views (400-500 seconds)
#
# Author: Generated for ASTRA Swabbing Pilot Study
# Date: 2026-07-16
# Version: 2.2 - Tolerance band implementation (target ±10%)
# ==============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(scales)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Define paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
METADATA_FILE <- file.path(BASE_PATH, "pilot_data_nested.csv")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs"

# Create output directories
dir.create(file.path(OUTPUT_PATH, "individual_plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "grouped_plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "statistics"), recursive = TRUE, showWarnings = FALSE)

# Define samples with pressure traces
SAMPLES_WITH_TRACES <- c("PILOT_001", "PILOT_005", "PILOT_009", "PILOT_013", 
                         "PILOT_017", "PILOT_021", "PILOT_025", "PILOT_029")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Identify contact periods in pressure trace data
#' @param data Data frame with pressure trace
#' @param target_pressure_g Target pressure in grams
#' @param tolerance_pct Tolerance around target (default 0.10 for ±10%)
#' @param merge_gap_sec Maximum gap in seconds to merge contact periods (default 0.2)
#' @return Data frame with contact periods marked
identify_contact_periods <- function(data, target_pressure_g, 
                                    tolerance_pct = 0.10, 
                                    merge_gap_sec = 0.2) {
  # Calculate tolerance band
  lower_threshold <- target_pressure_g * (1 - tolerance_pct)
  upper_threshold <- target_pressure_g * (1 + tolerance_pct)
  
  # Mark initial contact points (within tolerance band)
  data <- data %>%
    arrange(elapsed_sec) %>%
    mutate(
      is_contact = (load_g >= lower_threshold) & (load_g <= upper_threshold),
      contact_threshold_lower = lower_threshold,
      contact_threshold_upper = upper_threshold
    )
  
  # Identify contact segments (consecutive TRUE values)
  data <- data %>%
    mutate(
      contact_change = is_contact != lag(is_contact, default = FALSE),
      segment_id = cumsum(contact_change)
    ) %>%
    mutate(segment_id = ifelse(is_contact, segment_id, NA))
  
  # Get segment boundaries for merging
  segments <- data %>%
    filter(!is.na(segment_id)) %>%
    group_by(segment_id) %>%
    summarize(
      start_time = min(elapsed_sec),
      end_time = max(elapsed_sec),
      .groups = "drop"
    ) %>%
    arrange(start_time)
  
  # Merge segments with gaps < merge_gap_sec
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
    
    # Apply merged groups back to data
    # For each merged group, mark all points in time range as contact
    merged_ranges <- segments %>%
      group_by(merged_group) %>%
      summarize(
        merge_start = min(start_time),
        merge_end = max(end_time),
        .groups = "drop"
      )
    
    # Update is_contact based on merged ranges
    data <- data %>%
      mutate(
        pass_id = NA_integer_
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
  } else if (nrow(segments) == 1) {
    # Only one segment, assign pass_id = 1
    data <- data %>%
      mutate(pass_id = ifelse(is_contact, 1, NA_integer_))
  } else {
    # No segments found
    data <- data %>%
      mutate(pass_id = NA_integer_)
  }
  
  # Clean up temporary columns
  data <- data %>%
    select(-contact_change, -segment_id)
  
  return(data)
}

#' Calculate statistics for contact periods only
#' @param data Data frame with pressure trace
#' @param target_pressure_g Target pressure in grams
#' @return Data frame with contact statistics
calculate_contact_statistics <- function(data, target_pressure_g) {
  # Ensure contact detection has been run
  if (!"is_contact" %in% colnames(data)) {
    data <- identify_contact_periods(data, target_pressure_g)
  }
  
  # Filter to contact periods only
  contact_data <- data %>% filter(is_contact == TRUE)
  
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
    contact_threshold_lower_g = first(contact_data$contact_threshold_lower),
    contact_threshold_upper_g = first(contact_data$contact_threshold_upper)
  )
  
  return(stats)
}

#' Read a single pressure trace CSV file with robust error handling
#' @param run_id Sample identifier (e.g., "PILOT_001")
#' @return Data frame with pressure trace data
read_pressure_trace <- function(run_id) {
  file_path <- file.path(TRACES_PATH, paste0(run_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read the entire file as text first
  lines <- readLines(file_path, warn = FALSE)
  
  # Find data rows (skip headers and text rows)
  # Data rows should have numeric values in specific positions
  data_lines <- lines[2:length(lines)]  # Skip first line (header)
  
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

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("================================================================================\n")
cat("ASTRA SWABBING PILOT STUDY - PRESSURE TRACE ANALYSIS\n")
cat("================================================================================\n\n")

cat("Loading data...\n")

# Load experimental metadata
exp_metadata <- read_csv(METADATA_FILE, col_types = cols(), show_col_types = FALSE)

# Filter to samples with traces
exp_metadata_filtered <- exp_metadata %>%
  filter(RunID %in% SAMPLES_WITH_TRACES)

cat("\nExpected samples:\n")
print(exp_metadata_filtered %>% select(RunID, Pressure_g, Surface_type))

# Load all pressure traces
all_traces <- list()
successful_loads <- 0
failed_loads <- 0

cat("\nReading pressure trace files...\n")
for (sample_id in SAMPLES_WITH_TRACES) {
  cat(paste("  Reading", sample_id, "..."))
  trace <- tryCatch({
    read_pressure_trace(sample_id)
  }, error = function(e) {
    cat(" ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(trace) && nrow(trace) > 0) {
    # Get target pressure for this sample
    target_pressure <- exp_metadata_filtered %>% 
      filter(RunID == sample_id) %>% 
      pull(Pressure_g)
    
    # Apply contact detection
    trace_with_contacts <- identify_contact_periods(
      trace, 
      target_pressure_g = target_pressure[1],
      tolerance_pct = 0.10,
      merge_gap_sec = 0.2
    )
    
    # Calculate diagnostic info
    n_contact <- sum(trace_with_contacts$is_contact)
    pct_contact <- (n_contact / nrow(trace)) * 100
    n_passes <- max(trace_with_contacts$pass_id, na.rm = TRUE)
    
    cat(paste(" OK (", nrow(trace), " rows)\n", sep=""))
    cat(paste("    Contact: ", n_contact, " points (", 
              round(pct_contact, 1), "%), ",
              n_passes, " passes\n", sep=""))
    
    all_traces[[sample_id]] <- trace_with_contacts
    successful_loads <- successful_loads + 1
  } else {
    cat(" FAILED\n")
    failed_loads <- failed_loads + 1
  }
}

cat(paste("\nLoading complete:", successful_loads, "successful,", failed_loads, "failed\n"))

if (length(all_traces) == 0) {
  stop("No data loaded! Cannot continue.")
}

# Combine all traces into one data frame
combined_traces <- bind_rows(all_traces)

# Merge with experimental metadata
combined_data <- combined_traces %>%
  left_join(exp_metadata_filtered, by = c("run_id" = "RunID"))

cat(paste("Total observations:", nrow(combined_data), "\n"))

# Show what we have by group
cat("\nSamples by pressure and surface:\n")
sample_groups <- combined_data %>%
  select(run_id, Pressure_g, Surface_type) %>%
  distinct() %>%
  arrange(Pressure_g, Surface_type)
print(sample_groups)

cat("\n")

# ==============================================================================
# INDIVIDUAL PLOTS
# ==============================================================================

cat("Creating individual plots...\n")

for (sample_id in names(all_traces)) {
  sample_data <- combined_data %>% filter(run_id == sample_id)
  
  if (nrow(sample_data) == 0) next
  
  # Get sample metadata
  sample_info <- exp_metadata_filtered %>% filter(RunID == sample_id)
  pressure_level <- sample_info$Pressure_g[1]
  surface_type <- toupper(sample_info$Surface_type[1])
  
  # Create plot
  p <- ggplot(sample_data, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = "#2C3E50", linewidth = 0.5, alpha = 0.8) +
    labs(
      title = paste(sample_id, "- Pressure Trace"),
      subtitle = paste0(pressure_level, "g target pressure | ", surface_type, " surface"),
      x = "Time (seconds)",
      y = "Load (g)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "#7F8C8D", size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
      axis.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Add horizontal line for target pressure
  p <- p + geom_hline(yintercept = pressure_level, 
                      linetype = "dashed", 
                      color = "#E74C3C", 
                      alpha = 0.5,
                      linewidth = 0.8)
  
  # Save plot
  output_file <- file.path(OUTPUT_PATH, "individual_plots", 
                           paste0(sample_id, "_pressure_trace.png"))
  ggsave(output_file, p, width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("  Saved:", sample_id, "\n"))
}

cat("\n")

# ==============================================================================
# INDIVIDUAL PLOTS - CONTACT ONLY
# ==============================================================================

cat("Creating individual contact-only plots...\n")

for (sample_id in names(all_traces)) {
  sample_data <- combined_data %>% filter(run_id == sample_id)
  contact_data <- sample_data %>% filter(is_contact == TRUE)
  
  if (nrow(contact_data) == 0) next
  
  # Get sample metadata
  sample_info <- exp_metadata_filtered %>% filter(RunID == sample_id)
  pressure_level <- sample_info$Pressure_g[1]
  surface_type <- toupper(sample_info$Surface_type[1])
  n_passes <- max(contact_data$pass_id, na.rm = TRUE)
  threshold_lower <- pressure_level * 0.90
  threshold_upper <- pressure_level * 1.10
  
  # Create contact-only plot with points and lines
  p <- ggplot(contact_data, aes(x = elapsed_sec, y = load_g)) +
    geom_point(color = "#2C3E50", size = 0.8, alpha = 0.7) +
    geom_line(aes(group = pass_id), color = "#2C3E50", 
              linewidth = 0.4, alpha = 0.6) +
    labs(
      title = paste(sample_id, "- Pressure Trace (Contact Periods Only)"),
      subtitle = paste0(pressure_level, "g target | ", surface_type, 
                       " surface | ", n_passes, " passes detected"),
      x = "Time (seconds)",
      y = "Load (g)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "#7F8C8D", size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
      axis.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Add threshold reference lines (upper and lower bounds)
  p <- p + 
    geom_hline(yintercept = threshold_lower, 
               linetype = "dotted", 
               color = "#E74C3C", 
               alpha = 0.5,
               linewidth = 0.6) +
    geom_hline(yintercept = threshold_upper, 
               linetype = "dotted", 
               color = "#E74C3C", 
               alpha = 0.5,
               linewidth = 0.6)
  
  # Save plot
  output_file <- file.path(OUTPUT_PATH, "individual_plots", 
                           paste0(sample_id, "_contact_only.png"))
  ggsave(output_file, p, width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("  Saved:", sample_id, "contact-only\n"))
}

cat("\n")

# ==============================================================================
# GROUPED PLOTS
# ==============================================================================

cat("Creating grouped plots...\n")

# Define color palette for samples
color_palette <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", 
                   "#9B59B6", "#1ABC9C", "#34495E", "#E67E22")

# Create plots for each combination of pressure level and surface type
pressure_levels <- c(50, 200)
surface_types <- c("steel", "abs")

for (pressure in pressure_levels) {
  for (surface in surface_types) {
    
    # Filter data - use tolower for case-insensitive comparison
    plot_data <- combined_data %>%
      filter(Pressure_g == pressure, tolower(Surface_type) == surface)
    
    if (nrow(plot_data) == 0) {
      cat(paste("  No data for", pressure, "g", toupper(surface), "\n"))
      next
    }
    
    # Get unique samples
    samples <- unique(plot_data$run_id)
    n_samples <- length(samples)
    
    cat(paste("  Creating", pressure, "g", toupper(surface), "plot with", n_samples, "sample(s):", 
              paste(samples, collapse = ", "), "\n"))
    
    # Filter to 400-500 second window for zoomed view
    plot_data_zoomed <- plot_data %>%
      filter(elapsed_sec >= 400, elapsed_sec <= 500)
    
    # Create full plot
    p_full <- ggplot(plot_data, aes(x = elapsed_sec, y = load_g, color = run_id, group = run_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      labs(
        title = paste0(pressure, "g Pressure Traces - ", toupper(surface), " Surface (Full)"),
        subtitle = paste("Combined view of", n_samples, "sample(s)"),
        x = "Time (seconds)",
        y = "Load (g)",
        color = "Sample ID"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "#7F8C8D", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", color = "#BDC3C7", linewidth = 0.5),
        legend.title = element_text(face = "bold", size = 10)
      ) +
      scale_color_manual(values = color_palette[1:n_samples]) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
    
    # Add horizontal line for target pressure
    p_full <- p_full + geom_hline(yintercept = pressure, 
                        linetype = "dashed", 
                        color = "#95A5A6", 
                        alpha = 0.5,
                        linewidth = 0.8)
    
    # Save full plot
    output_file_full <- file.path(OUTPUT_PATH, "grouped_plots", 
                             paste0(pressure, "g_", surface, "_full.png"))
    ggsave(output_file_full, p_full, width = 12, height = 7, dpi = 300, bg = "white")
    
    # ==============================================================================
    # GROUPED PLOT - CONTACT ONLY
    # ==============================================================================
    
    # Filter to contact periods only
    contact_data <- plot_data %>% filter(is_contact == TRUE)
    
    if (nrow(contact_data) > 0) {
      # Create contact-only grouped plot
      p_contact <- ggplot(contact_data, aes(x = elapsed_sec, y = load_g, color = run_id)) +
        geom_point(size = 0.6, alpha = 0.6) +
        geom_line(aes(group = interaction(run_id, pass_id)), 
                  linewidth = 0.5, alpha = 0.7) +
        labs(
          title = paste0(pressure, "g Traces - ", toupper(surface), 
                        " Surface (Contact Periods Only)"),
          subtitle = paste(n_samples, "sample(s) | Contact periods within ±10% of target pressure"),
          x = "Time (seconds)",
          y = "Load (g)",
          color = "Sample ID"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "#7F8C8D", size = 11),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
          axis.title = element_text(face = "bold"),
          legend.position = "right",
          legend.background = element_rect(fill = "white", color = "#BDC3C7", 
                                          linewidth = 0.5),
          legend.title = element_text(face = "bold", size = 10)
        ) +
        scale_color_manual(values = color_palette[1:n_samples]) +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma)
      
      # Add threshold reference lines
      p_contact <- p_contact + 
        geom_hline(yintercept = pressure * 0.90, 
                   linetype = "dotted", 
                   color = "#95A5A6", 
                   alpha = 0.5,
                   linewidth = 0.6) +
        geom_hline(yintercept = pressure * 1.10, 
                   linetype = "dotted", 
                   color = "#95A5A6", 
                   alpha = 0.5,
                   linewidth = 0.6)
      
      # Save contact-only plot
      output_file_contact <- file.path(OUTPUT_PATH, "grouped_plots", 
                              paste0(pressure, "g_", surface, "_contact_only.png"))
      ggsave(output_file_contact, p_contact, width = 12, height = 7, dpi = 300, bg = "white")
    }
    
    # Create zoomed plot (400-500 seconds)
    if (nrow(plot_data_zoomed) > 0) {
      p_zoom <- ggplot(plot_data_zoomed, aes(x = elapsed_sec, y = load_g, color = run_id, group = run_id)) +
        geom_line(linewidth = 0.8, alpha = 0.8) +
        labs(
          title = paste0(pressure, "g Pressure Traces - ", toupper(surface), " Surface (Zoomed: 400-500s)"),
          subtitle = paste("Combined view of", n_samples, "sample(s) | Time window: 400-500 seconds"),
          x = "Time (seconds)",
          y = "Load (g)",
          color = "Sample ID"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "#7F8C8D", size = 11),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
          axis.title = element_text(face = "bold"),
          legend.position = "right",
          legend.background = element_rect(fill = "white", color = "#BDC3C7", linewidth = 0.5),
          legend.title = element_text(face = "bold", size = 10)
        ) +
        scale_color_manual(values = color_palette[1:n_samples]) +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(limits = c(400, 500), labels = comma)
      
      # Add horizontal line for target pressure
      p_zoom <- p_zoom + geom_hline(yintercept = pressure, 
                          linetype = "dashed", 
                          color = "#95A5A6", 
                          alpha = 0.5,
                          linewidth = 0.8)
      
      # Save zoomed full plot
      output_file_zoom <- file.path(OUTPUT_PATH, "grouped_plots", 
                               paste0(pressure, "g_", surface, "_zoomed_full.png"))
      ggsave(output_file_zoom, p_zoom, width = 12, height = 7, dpi = 300, bg = "white")
      
      # ==============================================================================
      # ZOOMED PLOT - CONTACT ONLY
      # ==============================================================================
      
      # Create zoomed contact-only plot (400-500 seconds)
      plot_data_zoomed_contact <- plot_data_zoomed %>% filter(is_contact == TRUE)
      
      if (nrow(plot_data_zoomed_contact) > 0) {
        p_zoom_contact <- ggplot(plot_data_zoomed_contact, 
                                 aes(x = elapsed_sec, y = load_g, color = run_id)) +
          geom_point(size = 0.8, alpha = 0.7) +
          geom_line(aes(group = interaction(run_id, pass_id)), 
                    linewidth = 0.6, alpha = 0.8) +
          labs(
            title = paste0(pressure, "g Traces - ", toupper(surface), 
                          " (Contact Only, Zoomed: 400-500s)"),
            subtitle = paste(n_samples, "sample(s) | Contact periods within ±10% target"),
            x = "Time (seconds)",
            y = "Load (g)",
            color = "Sample ID"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(color = "#7F8C8D", size = 11),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
            axis.title = element_text(face = "bold"),
            legend.position = "right",
            legend.background = element_rect(fill = "white", color = "#BDC3C7", 
                                            linewidth = 0.5),
            legend.title = element_text(face = "bold", size = 10)
          ) +
          scale_color_manual(values = color_palette[1:n_samples]) +
          scale_y_continuous(labels = comma) +
          scale_x_continuous(limits = c(400, 500), labels = comma)
        
        # Add threshold lines
        p_zoom_contact <- p_zoom_contact + 
          geom_hline(yintercept = pressure * 0.90, 
                     linetype = "dotted", 
                     color = "#95A5A6", 
                     alpha = 0.5,
                     linewidth = 0.6) +
          geom_hline(yintercept = pressure * 1.10, 
                     linetype = "dotted", 
                     color = "#95A5A6", 
                     alpha = 0.5,
                     linewidth = 0.6)
        
        # Save zoomed contact-only plot
        output_file_zoom_contact <- file.path(OUTPUT_PATH, "grouped_plots", 
                                             paste0(pressure, "g_", surface, 
                                                   "_zoomed_contact.png"))
        ggsave(output_file_zoom_contact, p_zoom_contact, 
               width = 12, height = 7, dpi = 300, bg = "white")
      }
      
      cat(paste("  Saved:", pressure, "g", toupper(surface), "plots (full + contact, including zoomed)\n"))
    } else {
      cat(paste("  Saved:", pressure, "g", toupper(surface), "plots (full + contact only, no data in 400-500s window)\n"))
    }
  }
}

cat("\n")

# ==============================================================================
# STATISTICAL ANALYSIS - CONTACT PERIODS ONLY
# ==============================================================================

cat("Performing statistical analysis (contact periods only)...\n")

# Calculate per-sample contact statistics
sample_stats <- combined_data %>%
  group_by(run_id) %>%
  do({
    target_p <- first(.$Pressure_g)
    calculate_contact_statistics(., target_p)
  }) %>%
  ungroup() %>%
  left_join(exp_metadata_filtered %>% 
              select(RunID, Pressure_level, Pressure_g, Surface_type, 
                     Solvent_level, PETN_Recovery_pct, RDX_Recovery_pct),
            by = c("run_id" = "RunID"))

# Summary by pressure level
pressure_summary <- sample_stats %>%
  group_by(Pressure_g) %>%
  summarize(
    n_samples = n(),
    mean_contact_pressure = mean(mean_contact_pressure_g, na.rm = TRUE),
    sd_contact_pressure = sd(mean_contact_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_contact_percent, na.rm = TRUE),
    mean_percent_time_contact = mean(percent_time_in_contact, na.rm = TRUE),
    mean_num_passes = mean(number_of_passes, na.rm = TRUE),
    mean_pass_duration = mean(mean_pass_duration_sec, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by surface type
surface_summary <- sample_stats %>%
  group_by(Surface_type) %>%
  summarize(
    n_samples = n(),
    mean_contact_pressure = mean(mean_contact_pressure_g, na.rm = TRUE),
    sd_contact_pressure = sd(mean_contact_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_contact_percent, na.rm = TRUE),
    mean_percent_time_contact = mean(percent_time_in_contact, na.rm = TRUE),
    mean_num_passes = mean(number_of_passes, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by pressure AND surface
combined_summary <- sample_stats %>%
  group_by(Pressure_g, Surface_type) %>%
  summarize(
    n_samples = n(),
    mean_contact_pressure = mean(mean_contact_pressure_g, na.rm = TRUE),
    sd_contact_pressure = sd(mean_contact_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_contact_percent, na.rm = TRUE),
    mean_percent_time_contact = mean(percent_time_in_contact, na.rm = TRUE),
    mean_num_passes = mean(number_of_passes, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# EXPORT STATISTICS
# ==============================================================================

cat("Exporting statistical summaries...\n")

# Export per-sample statistics
write_csv(sample_stats, 
          file.path(OUTPUT_PATH, "statistics", "sample_statistics_contact_only.csv"))

# Export pressure level summary
write_csv(pressure_summary, 
          file.path(OUTPUT_PATH, "statistics", "pressure_level_summary_contact.csv"))

# Export surface type summary
write_csv(surface_summary, 
          file.path(OUTPUT_PATH, "statistics", "surface_type_summary_contact.csv"))

# Export combined summary
write_csv(combined_summary, 
          file.path(OUTPUT_PATH, "statistics", "pressure_surface_summary_contact.csv"))

# ==============================================================================
# PRINT SUMMARY TO CONSOLE
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("                     STATISTICAL ANALYSIS SUMMARY\n")
cat("                          (CONTACT PERIODS ONLY)\n")
cat("                    Tolerance Band: Target ±10%\n")
cat("================================================================================\n\n")

cat("PER-SAMPLE CONTACT STATISTICS:\n")
cat("-------------------------------\n")
print(sample_stats %>% 
        select(run_id, Pressure_g, Surface_type, 
               mean_contact_pressure_g, cv_contact_percent,
               percent_time_in_contact, number_of_passes) %>%
        arrange(Pressure_g, Surface_type), 
      n = Inf)

cat("\n\nSUMMARY BY PRESSURE LEVEL:\n")
cat("--------------------------\n")
print(pressure_summary, n = Inf)

cat("\n\nSUMMARY BY SURFACE TYPE:\n")
cat("------------------------\n")
print(surface_summary, n = Inf)

cat("\n\nSUMMARY BY PRESSURE AND SURFACE:\n")
cat("--------------------------------\n")
print(combined_summary, n = Inf)

cat("\n")
cat("================================================================================\n")
cat("                            ANALYSIS COMPLETE\n")
cat("================================================================================\n")
cat("\nOutputs saved to:", OUTPUT_PATH, "\n")
cat("\nGenerated files:\n")
cat("  - Individual plots: ", length(all_traces) * 2, " PNG files\n", sep="")
cat("      * ", length(all_traces), " full trace plots\n", sep="")
cat("      * ", length(all_traces), " contact-only plots\n", sep="")
cat("  - Grouped plots: 16 PNG files\n")
cat("      * 4 full trace plots\n")
cat("      * 4 contact-only plots\n")
cat("      * 4 zoomed full trace plots (400-500s)\n")
cat("      * 4 zoomed contact-only plots (400-500s)\n")
cat("  - Statistics: 4 CSV files (all contact-period based)\n")
cat("\n")
cat("Contact detection parameters:\n")
cat("  - Tolerance: ±10% of target pressure\n")
cat("  - Merge gap: < 0.2 seconds\n")
cat("  - 200g samples: contact when 180g ≤ load ≤ 220g\n")
cat("  - 50g samples: contact when 45g ≤ load ≤ 55g\n")
cat("\n")
