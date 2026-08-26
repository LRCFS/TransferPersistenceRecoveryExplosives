# ==============================================================================
# ASTRA Stability Detection Prototype Test
# ==============================================================================
# 
# This script tests the new stability-based contact detection approach using
# rolling standard deviation instead of fixed tolerance bands.
#
# Tests on: PILOT_001 (200g) and PILOT_005 (50g)
# Compares: Old (±10% tolerance) vs New (rolling SD stability)
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-prototype
# ==============================================================================

# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)  # For combining plots
  # Note: zoo or slider will be loaded by stability_detection_functions.R
})

# Source stability detection functions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/stability_detection_functions.R")

# Also need old method for comparison
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/analyze_pressure_traces_final.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Define paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

# Create output directories
dir.create(file.path(OUTPUT_PATH, "comparison_plots"), recursive = TRUE, showWarnings = FALSE)

# Test samples
TEST_SAMPLES <- c("PILOT_001", "PILOT_005")

# Stability detection parameters (BALANCED - Finding sweet spot)
PARAMS <- list(
  window_sec = 1.5,         # Smaller window to capture more detail
  max_sd_pct = 0.020,       # 2.0% of target (relaxed from 1.5%)
  broad_tolerance = 0.10,   # ±10% for coarse filter
  min_duration_sec = 2.0,   # Standard minimum duration
  merge_gap_sec = 0.2       # Standard merging
)

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("================================================================================\n")
cat("STABILITY DETECTION PROTOTYPE TEST\n")
cat("================================================================================\n\n")

cat("Loading test samples...\n")

# Load metadata for target pressures
metadata <- data.frame(
  run_id = c("PILOT_001", "PILOT_005"),
  target_pressure_g = c(200, 50),
  surface_type = c("steel", "steel")
)

# Load pressure traces
all_data <- list()

for (sample_id in TEST_SAMPLES) {
  cat(paste("  Loading", sample_id, "..."))
  
  trace <- read_pressure_trace_file(sample_id, TRACES_PATH)
  
  if (!is.null(trace) && nrow(trace) > 0) {
    cat(paste(" OK (", nrow(trace), " rows)\n", sep=""))
    all_data[[sample_id]] <- trace
  } else {
    cat(" FAILED\n")
  }
}

if (length(all_data) == 0) {
  stop("No data loaded! Cannot continue.")
}

cat("\n")

# ==============================================================================
# APPLY OLD METHOD (±10% TOLERANCE)
# ==============================================================================

cat("================================================================================\n")
cat("APPLYING OLD METHOD (±10% Tolerance Band)\n")
cat("================================================================================\n\n")

old_results <- list()

for (sample_id in names(all_data)) {
  cat(paste("Processing", sample_id, "with OLD method...\n"))
  
  trace <- all_data[[sample_id]]
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  # Apply old tolerance band method
  trace_old <- identify_contact_periods(
    trace,
    target_pressure_g = target_pressure,
    tolerance_pct = 0.10,
    merge_gap_sec = 0.2
  )
  
  # Calculate old statistics
  stats_old <- calculate_contact_statistics(trace_old, target_pressure)
  
  old_results[[sample_id]] <- list(
    data = trace_old,
    stats = stats_old
  )
  
  cat("\n")
}

# ==============================================================================
# APPLY NEW METHOD (ROLLING SD STABILITY)
# ==============================================================================

cat("================================================================================\n")
cat("APPLYING NEW METHOD (Rolling SD Stability)\n")
cat("================================================================================\n\n")

cat("Parameters (BALANCED):\n")
cat(sprintf("  Window: %.1f seconds\n", PARAMS$window_sec))
cat(sprintf("  Max SD: %.1f%% of target\n", PARAMS$max_sd_pct * 100))
cat(sprintf("  Broad range: ±%.0f%%\n", PARAMS$broad_tolerance * 100))
cat(sprintf("  Min duration: %.1f seconds\n", PARAMS$min_duration_sec))
cat(sprintf("  Merge gap: %.1f seconds\n\n", PARAMS$merge_gap_sec))

new_results <- list()

for (sample_id in names(all_data)) {
  cat(paste("Processing", sample_id, "with NEW method...\n"))
  
  trace <- all_data[[sample_id]]
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  # Apply new stability detection
  trace_new <- identify_stable_periods(
    trace,
    target_pressure_g = target_pressure,
    window_sec = PARAMS$window_sec,
    max_sd_pct = PARAMS$max_sd_pct,
    broad_tolerance = PARAMS$broad_tolerance,
    min_duration_sec = PARAMS$min_duration_sec,
    merge_gap_sec = PARAMS$merge_gap_sec
  )
  
  # Calculate new statistics
  stats_new <- tryCatch({
    calculate_stability_statistics(trace_new, target_pressure)
  }, error = function(e) {
    # Create empty stats if no stable periods found
    warning(paste("No stable periods found for", sample_id))
    data.frame(
      run_id = sample_id,
      mean_stable_pressure_g = NA,
      sd_stable_pressure_g = NA,
      cv_stable_percent = NA,
      min_stable_pressure_g = NA,
      max_stable_pressure_g = NA,
      median_stable_pressure_g = NA,
      n_stable_observations = 0,
      total_stable_time_sec = 0,
      total_trace_duration_sec = max(trace$elapsed_sec) - min(trace$elapsed_sec),
      percent_time_stable = 0,
      number_of_passes = 0,
      mean_pass_duration_sec = NA,
      mean_rolling_sd_stable = NA,
      max_rolling_sd_stable = NA
    )
  })
  
  new_results[[sample_id]] <- list(
    data = trace_new,
    stats = stats_new
  )
  
  cat("\n")
}

# ==============================================================================
# COMPARISON STATISTICS
# ==============================================================================

cat("================================================================================\n")
cat("COMPARISON STATISTICS\n")
cat("================================================================================\n\n")

comparison_list <- list()

for (sample_id in names(all_data)) {
  cat(paste0("=== ", sample_id, " ===\n"))
  
  old_stats <- old_results[[sample_id]]$stats
  new_stats <- new_results[[sample_id]]$stats
  
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  # Build comparison
  comp <- data.frame(
    sample_id = sample_id,
    target_pressure_g = target_pressure,
    
    # Old method
    old_pct_time = old_stats$percent_time_in_contact,
    old_n_passes = old_stats$number_of_passes,
    old_mean_pressure = old_stats$mean_contact_pressure_g,
    old_cv_pct = old_stats$cv_contact_percent,
    old_pass_duration = old_stats$mean_pass_duration_sec,
    
    # New method
    new_pct_time = ifelse(is.null(new_stats) || is.na(new_stats$percent_time_stable), 0, new_stats$percent_time_stable),
    new_n_passes = ifelse(is.null(new_stats) || is.na(new_stats$number_of_passes), 0, new_stats$number_of_passes),
    new_mean_pressure = ifelse(is.null(new_stats) || is.na(new_stats$mean_stable_pressure_g), NA, new_stats$mean_stable_pressure_g),
    new_cv_pct = ifelse(is.null(new_stats) || is.na(new_stats$cv_stable_percent), NA, new_stats$cv_stable_percent),
    new_pass_duration = ifelse(is.null(new_stats) || is.na(new_stats$mean_pass_duration_sec), NA, new_stats$mean_pass_duration_sec),
    
    # Changes
    change_pct_time = ifelse(is.null(new_stats) || is.na(new_stats$percent_time_stable), -old_stats$percent_time_in_contact, new_stats$percent_time_stable - old_stats$percent_time_in_contact),
    change_n_passes = ifelse(is.null(new_stats) || is.na(new_stats$number_of_passes), -old_stats$number_of_passes, new_stats$number_of_passes - old_stats$number_of_passes),
    change_mean_pressure = ifelse(is.null(new_stats) || is.na(new_stats$mean_stable_pressure_g), NA, new_stats$mean_stable_pressure_g - old_stats$mean_contact_pressure_g),
    change_cv_pct = ifelse(is.null(new_stats) || is.na(new_stats$cv_stable_percent), NA, new_stats$cv_stable_percent - old_stats$cv_contact_percent)
  )
  
  comparison_list[[sample_id]] <- comp
  
  # Print formatted comparison
  cat(sprintf("Target Pressure: %dg\n\n", target_pressure))
  
  cat("OLD METHOD (±10% Tolerance):\n")
  cat(sprintf("  %% Time in Contact: %.1f%%\n", old_stats$percent_time_in_contact))
  cat(sprintf("  Number of Passes: %d\n", old_stats$number_of_passes))
  cat(sprintf("  Mean Pressure: %.2fg\n", old_stats$mean_contact_pressure_g))
  cat(sprintf("  CV%%: %.2f%%\n", old_stats$cv_contact_percent))
  cat(sprintf("  Mean Pass Duration: %.2fs\n\n", old_stats$mean_pass_duration_sec))
  
  cat("NEW METHOD (Rolling SD Stability):\n")
  cat(sprintf("  %% Time Stable: %.1f%%\n", new_stats$percent_time_stable))
  cat(sprintf("  Number of Passes: %d\n", new_stats$number_of_passes))
  cat(sprintf("  Mean Pressure: %.2fg\n", new_stats$mean_stable_pressure_g))
  cat(sprintf("  CV%%: %.2f%%\n", new_stats$cv_stable_percent))
  cat(sprintf("  Mean Pass Duration: %.2fs\n", new_stats$mean_pass_duration_sec))
  cat(sprintf("  Mean Rolling SD: %.2fg\n\n", new_stats$mean_rolling_sd_stable))
  
  cat("CHANGES:\n")
  cat(sprintf("  %% Time: %.1f%% → %.1f%% (Δ %.1f%%)\n", 
              old_stats$percent_time_in_contact,
              new_stats$percent_time_stable,
              comp$change_pct_time))
  cat(sprintf("  Passes: %d → %d (Δ %d)\n",
              old_stats$number_of_passes,
              new_stats$number_of_passes,
              comp$change_n_passes))
  cat(sprintf("  Mean Pressure: %.2fg → %.2fg (Δ %.2fg)\n",
              old_stats$mean_contact_pressure_g,
              new_stats$mean_stable_pressure_g,
              comp$change_mean_pressure))
  cat(sprintf("  CV%%: %.2f%% → %.2f%% (Δ %.2f%%)\n\n",
              old_stats$cv_contact_percent,
              new_stats$cv_stable_percent,
              comp$change_cv_pct))
  
  cat("\n")
}

# Combine comparison data
comparison_df <- bind_rows(comparison_list)

# Save comparison table
write_csv(comparison_df, file.path(OUTPUT_PATH, "comparison_statistics.csv"))
cat("Saved: comparison_statistics.csv\n\n")

# ==============================================================================
# GENERATE COMPARISON PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("GENERATING COMPARISON PLOTS\n")
cat("================================================================================\n\n")

# Color palette
COLOR_OLD <- "#E74C3C"
COLOR_NEW <- "#2ECC71"
COLOR_TRACE <- "#2C3E50"

for (sample_id in names(all_data)) {
  cat(paste("Creating plots for", sample_id, "...\n"))
  
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  trace_old <- old_results[[sample_id]]$data
  trace_new <- new_results[[sample_id]]$data
  
  # ============================================================================
  # PLOT 1: Side-by-Side Comparison
  # ============================================================================
  
  # Prepare data for comparison plot
  plot_data_old <- trace_old %>%
    mutate(method = "Old (±10% Tolerance)",
           detected = is_contact)
  
  plot_data_new <- trace_new %>%
    mutate(method = "New (Rolling SD Stability)",
           detected = is_stable)
  
  plot_data_combined <- bind_rows(plot_data_old, plot_data_new)
  
  p1 <- ggplot(plot_data_combined, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = COLOR_TRACE, linewidth = 0.3, alpha = 0.6) +
    geom_point(data = plot_data_combined %>% filter(detected),
               aes(color = method), size = 0.4, alpha = 0.5) +
    facet_wrap(~method, ncol = 1) +
    scale_color_manual(values = c("Old (±10% Tolerance)" = COLOR_OLD,
                                   "New (Rolling SD Stability)" = COLOR_NEW)) +
    labs(
      title = paste(sample_id, "- Method Comparison"),
      subtitle = paste0(target_pressure, "g target | Red = Old method, Green = New method"),
      x = "Time (seconds)",
      y = "Load (g)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "#7F8C8D", size = 10),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "#ECF0F1", color = NA)
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Add target line
  p1 <- p1 + geom_hline(yintercept = target_pressure, 
                        linetype = "dashed", 
                        color = "#95A5A6", 
                        alpha = 0.5)
  
  ggsave(file.path(OUTPUT_PATH, "comparison_plots", 
                   paste0(sample_id, "_comparison.png")),
         p1, width = 12, height = 8, dpi = 300, bg = "white")
  
  cat("  Saved: comparison plot\n")
  
  # ============================================================================
  # PLOT 2: Stability Profile
  # ============================================================================
  
  p2_top <- ggplot(trace_new, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = COLOR_TRACE, linewidth = 0.4, alpha = 0.7) +
    geom_point(data = trace_new %>% filter(is_stable),
               color = COLOR_NEW, size = 0.5, alpha = 0.7) +
    labs(
      title = paste(sample_id, "- Stability Profile"),
      subtitle = paste0(target_pressure, "g target | Green = Stable regions detected"),
      y = "Load (g)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "#7F8C8D", size = 10),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    scale_y_continuous(labels = comma) +
    geom_hline(yintercept = target_pressure, linetype = "dashed", 
               color = "#95A5A6", alpha = 0.5)
  
  # Calculate SD threshold for plot
  sd_threshold <- target_pressure * PARAMS$max_sd_pct
  
  p2_bottom <- ggplot(trace_new, aes(x = elapsed_sec, y = rolling_sd)) +
    geom_line(color = "#3498DB", linewidth = 0.5) +
    geom_hline(yintercept = sd_threshold, 
               linetype = "dashed", color = COLOR_NEW, linewidth = 0.8) +
    annotate("text", x = max(trace_new$elapsed_sec) * 0.95, y = sd_threshold * 1.2,
             label = paste0("Threshold: ", round(sd_threshold, 2), "g"),
             color = COLOR_NEW, hjust = 1, fontface = "bold") +
    labs(
      x = "Time (seconds)",
      y = "Rolling SD (g)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Combine plots
  library(patchwork)
  p2 <- p2_top / p2_bottom + plot_layout(heights = c(2, 1))
  
  ggsave(file.path(OUTPUT_PATH, "comparison_plots", 
                   paste0(sample_id, "_stability_profile.png")),
         p2, width = 12, height = 8, dpi = 300, bg = "white")
  
  cat("  Saved: stability profile\n")
  
  # ============================================================================
  # PLOT 3: Zoomed Detail View
  # ============================================================================
  
  # Find a good region to zoom (first stable pass with context)
  first_stable <- trace_new %>% filter(is_stable) %>% slice(1)
  if (nrow(first_stable) > 0) {
    zoom_center <- first_stable$elapsed_sec[1]
    zoom_start <- max(0, zoom_center - 20)
    zoom_end <- zoom_center + 40
    
    zoom_data <- trace_new %>% 
      filter(elapsed_sec >= zoom_start, elapsed_sec <= zoom_end)
    
    p3 <- ggplot(zoom_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = COLOR_TRACE, linewidth = 0.6) +
      geom_point(data = zoom_data %>% filter(is_stable),
                 color = COLOR_NEW, size = 1.5, alpha = 0.7) +
      geom_ribbon(data = zoom_data,
                  aes(ymin = target_pressure * (1 - PARAMS$broad_tolerance),
                      ymax = target_pressure * (1 + PARAMS$broad_tolerance)),
                  fill = COLOR_NEW, alpha = 0.1) +
      labs(
        title = paste(sample_id, "- Detail View (First Stable Pass)"),
        subtitle = paste0("Green shaded = Broad range (±", PARAMS$broad_tolerance*100, "%) | ",
                         "Green dots = Detected stable regions"),
        x = "Time (seconds)",
        y = "Load (g)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "#7F8C8D", size = 10)
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      geom_hline(yintercept = target_pressure, linetype = "dashed", 
                 color = "#E74C3C", alpha = 0.7, linewidth = 0.8)
    
    ggsave(file.path(OUTPUT_PATH, "comparison_plots", 
                     paste0(sample_id, "_detail_zoom.png")),
           p3, width = 12, height = 6, dpi = 300, bg = "white")
    
    cat("  Saved: detail zoom\n")
  }
  
  cat("\n")
}

# ==============================================================================
# PLOT 4: Histogram Comparison (Combined for both samples)
# ==============================================================================

cat("Creating histogram comparison...\n")

# Prepare histogram data
hist_data_list <- list()

for (sample_id in names(all_data)) {
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  old_data <- old_results[[sample_id]]$data %>%
    filter(is_contact) %>%
    mutate(method = "Old (±10% Tolerance)",
           sample = sample_id,
           target = target_pressure)
  
  new_data <- new_results[[sample_id]]$data %>%
    filter(is_stable) %>%
    mutate(method = "New (Rolling SD Stability)",
           sample = sample_id,
           target = target_pressure)
  
  hist_data_list[[paste0(sample_id, "_old")]] <- old_data
  hist_data_list[[paste0(sample_id, "_new")]] <- new_data
}

hist_data <- bind_rows(hist_data_list)

p4 <- ggplot(hist_data, aes(x = load_g, fill = method)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  geom_vline(aes(xintercept = target), linetype = "dashed", 
             color = "#E74C3C", linewidth = 0.8) +
  facet_wrap(~sample, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("Old (±10% Tolerance)" = COLOR_OLD,
                               "New (Rolling SD Stability)" = COLOR_NEW)) +
  labs(
    title = "Pressure Distribution Comparison",
    subtitle = "Distribution of pressure values in detected contact/stable regions",
    x = "Load (g)",
    y = "Count",
    fill = "Method"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "#ECF0F1", color = NA)
  )

ggsave(file.path(OUTPUT_PATH, "comparison_plots", "histogram_comparison.png"),
       p4, width = 10, height = 8, dpi = 300, bg = "white")

cat("  Saved: histogram comparison\n\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("PROTOTYPE TEST COMPLETE\n")
cat("================================================================================\n\n")

cat("Outputs saved to:", OUTPUT_PATH, "\n\n")

cat("Summary:\n")
cat("  - Tested on", length(TEST_SAMPLES), "samples\n")
cat("  - Generated", length(TEST_SAMPLES) * 3 + 1, "comparison plots\n")
cat("  - Saved comparison statistics CSV\n\n")

cat("Key Findings:\n")
for (sample_id in names(all_data)) {
  comp <- comparison_list[[sample_id]]
  cat(sprintf("  %s: %.1f%% → %.1f%% contact time (Δ %.1f%%)\n",
              sample_id,
              comp$old_pct_time,
              comp$new_pct_time,
              comp$change_pct_time))
}

cat("\n")
cat("Next steps:\n")
cat("  1. Review plots in test_outputs/comparison_plots/\n")
cat("  2. Examine comparison_statistics.csv\n")
cat("  3. If results look good, integrate into main analysis script\n")
cat("  4. Apply to all 8 samples\n\n")
