# ==============================================================================
# ASTRA Flat Plateau Detection Test
# ==============================================================================
# 
# This script tests the new flat plateau detection approach using
# derivative-based flatness analysis.
#
# Tests on: PILOT_001 (200g) and PILOT_005 (50g)
# Compares: Old (±10% tolerance) vs New (flat plateau detection)
#
# User-identified regions for validation:
# - PILOT_005: 95.8-102.9s and 149.0-158.9s
# - PILOT_001: Flat portions within ranges
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-plateau
# ==============================================================================

# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)  # For combining plots
  library(zoo)        # For rolling calculations
})

# Source functions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/plateau_detection_functions.R")
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/analyze_pressure_traces_final.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Define paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

# Create output directories
dir.create(file.path(OUTPUT_PATH, "plateau_plots"), recursive = TRUE, showWarnings = FALSE)

# Test samples
TEST_SAMPLES <- c("PILOT_001", "PILOT_005")

# Plateau detection parameters
PARAMS <- list(
  max_derivative_g_per_sec = 5.0,    # Flatness threshold
  max_local_sd_pct = 0.01,           # 1% of target
  min_pressure_pct = 0.70,           # 70% of target minimum
  max_pressure_pct = 1.20,           # 120% of target maximum
  min_duration_sec = 1.0,            # Allow short plateaus
  max_duration_sec = 15.0,           # Prevent over-merged segments
  merge_gap_sec = 0.2                # Merge close plateaus
)

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("================================================================================\n")
cat("FLAT PLATEAU DETECTION TEST\n")
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
# APPLY NEW METHOD (FLAT PLATEAU DETECTION)
# ==============================================================================

cat("================================================================================\n")
cat("APPLYING NEW METHOD (Flat Plateau Detection)\n")
cat("================================================================================\n\n")

cat("Parameters:\n")
cat(sprintf("  Max derivative: %.1f g/s\n", PARAMS$max_derivative_g_per_sec))
cat(sprintf("  Max local SD: %.1f%% of target\n", PARAMS$max_local_sd_pct * 100))
cat(sprintf("  Pressure range: %.0f%% - %.0f%% of target\n", 
            PARAMS$min_pressure_pct * 100, PARAMS$max_pressure_pct * 100))
cat(sprintf("  Duration: %.1f - %.1f seconds\n", 
            PARAMS$min_duration_sec, PARAMS$max_duration_sec))
cat(sprintf("  Merge gap: %.1f seconds\n\n", PARAMS$merge_gap_sec))

new_results <- list()

for (sample_id in names(all_data)) {
  cat(paste("Processing", sample_id, "with NEW method...\n"))
  
  trace <- all_data[[sample_id]]
  target_pressure <- metadata %>% 
    filter(run_id == sample_id) %>% 
    pull(target_pressure_g)
  
  # Apply new plateau detection
  trace_new <- identify_flat_plateaus(
    trace,
    target_pressure_g = target_pressure,
    max_derivative_g_per_sec = PARAMS$max_derivative_g_per_sec,
    max_local_sd_pct = PARAMS$max_local_sd_pct,
    min_pressure_pct = PARAMS$min_pressure_pct,
    max_pressure_pct = PARAMS$max_pressure_pct,
    min_duration_sec = PARAMS$min_duration_sec,
    max_duration_sec = PARAMS$max_duration_sec,
    merge_gap_sec = PARAMS$merge_gap_sec
  )
  
  # Calculate new statistics
  stats_new <- tryCatch({
    calculate_plateau_statistics(trace_new, target_pressure)
  }, error = function(e) {
    warning(paste("No plateaus found for", sample_id))
    data.frame(
      run_id = sample_id,
      mean_plateau_pressure_g = NA,
      sd_plateau_pressure_g = NA,
      cv_plateau_percent = NA,
      min_plateau_pressure_g = NA,
      max_plateau_pressure_g = NA,
      median_plateau_pressure_g = NA,
      n_plateau_observations = 0,
      total_plateau_time_sec = 0,
      total_trace_duration_sec = max(trace$elapsed_sec) - min(trace$elapsed_sec),
      percent_time_plateau = 0,
      number_of_passes = 0,
      mean_pass_duration_sec = NA,
      mean_derivative_plateau = NA,
      max_derivative_plateau = NA,
      mean_local_sd_plateau = NA
    )
  })
  
  new_results[[sample_id]] <- list(
    data = trace_new,
    stats = stats_new
  )
  
  cat("\n")
}

# ==============================================================================
# VALIDATION: CHECK USER-IDENTIFIED REGIONS
# ==============================================================================

cat("================================================================================\n")
cat("VALIDATION: Checking User-Identified Regions\n")
cat("================================================================================\n\n")

# PILOT_005 validation regions
cat("PILOT_005 Expected Plateaus:\n")
cat("  Region 1: 95.8 - 102.9 seconds\n")
cat("  Region 2: 149.0 - 158.9 seconds\n\n")

pilot005_data <- new_results[["PILOT_005"]]$data
pilot005_region1 <- pilot005_data %>%
  filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9, is_plateau == TRUE)
pilot005_region2 <- pilot005_data %>%
  filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9, is_plateau == TRUE)

cat(sprintf("  Detected in Region 1: %d points (%.1f%% of region)\n",
            nrow(pilot005_region1),
            100 * nrow(pilot005_region1) / nrow(pilot005_data %>% 
                                                filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  Detected in Region 2: %d points (%.1f%% of region)\n\n",
            nrow(pilot005_region2),
            100 * nrow(pilot005_region2) / nrow(pilot005_data %>% 
                                                filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))

# PILOT_001 validation
cat("PILOT_001 Expected Flat Regions:\n")
cat("  Within 133-143 seconds (looking for flat plateaus)\n\n")

pilot001_data <- new_results[["PILOT_001"]]$data
pilot001_region <- pilot001_data %>%
  filter(elapsed_sec >= 133, elapsed_sec <= 143, is_plateau == TRUE)

cat(sprintf("  Detected in 133-143s: %d points (%.1f%% of region)\n\n",
            nrow(pilot001_region),
            100 * nrow(pilot001_region) / nrow(pilot001_data %>% 
                                                filter(elapsed_sec >= 133, elapsed_sec <= 143))))

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
    new_pct_time = ifelse(is.null(new_stats) || is.na(new_stats$percent_time_plateau), 0, new_stats$percent_time_plateau),
    new_n_passes = ifelse(is.null(new_stats) || is.na(new_stats$number_of_passes), 0, new_stats$number_of_passes),
    new_mean_pressure = ifelse(is.null(new_stats) || is.na(new_stats$mean_plateau_pressure_g), NA, new_stats$mean_plateau_pressure_g),
    new_cv_pct = ifelse(is.null(new_stats) || is.na(new_stats$cv_plateau_percent), NA, new_stats$cv_plateau_percent),
    new_pass_duration = ifelse(is.null(new_stats) || is.na(new_stats$mean_pass_duration_sec), NA, new_stats$mean_pass_duration_sec),
    
    # Changes
    change_pct_time = ifelse(is.null(new_stats) || is.na(new_stats$percent_time_plateau), -old_stats$percent_time_in_contact, new_stats$percent_time_plateau - old_stats$percent_time_in_contact),
    change_n_passes = ifelse(is.null(new_stats) || is.na(new_stats$number_of_passes), -old_stats$number_of_passes, new_stats$number_of_passes - old_stats$number_of_passes),
    change_mean_pressure = ifelse(is.null(new_stats) || is.na(new_stats$mean_plateau_pressure_g), NA, new_stats$mean_plateau_pressure_g - old_stats$mean_contact_pressure_g),
    change_cv_pct = ifelse(is.null(new_stats) || is.na(new_stats$cv_plateau_percent), NA, new_stats$cv_plateau_percent - old_stats$cv_contact_percent)
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
  
  cat("NEW METHOD (Flat Plateau Detection):\n")
  cat(sprintf("  %% Time in Plateaus: %.1f%%\n", comp$new_pct_time))
  cat(sprintf("  Number of Passes: %d\n", comp$new_n_passes))
  if (!is.na(comp$new_mean_pressure)) {
    cat(sprintf("  Mean Pressure: %.2fg\n", comp$new_mean_pressure))
    cat(sprintf("  CV%%: %.2f%%\n", comp$new_cv_pct))
    cat(sprintf("  Mean Pass Duration: %.2fs\n", comp$new_pass_duration))
    if (!is.na(new_stats$mean_derivative_plateau)) {
      cat(sprintf("  Mean Derivative: %.3f g/s\n", new_stats$mean_derivative_plateau))
    }
  } else {
    cat("  (No plateaus detected)\n")
  }
  cat("\n")
  
  cat("CHANGES:\n")
  cat(sprintf("  %% Time: %.1f%% → %.1f%% (Δ %.1f%%)\n", 
              old_stats$percent_time_in_contact,
              comp$new_pct_time,
              comp$change_pct_time))
  cat(sprintf("  Passes: %d → %d (Δ %d)\n",
              old_stats$number_of_passes,
              comp$new_n_passes,
              comp$change_n_passes))
  if (!is.na(comp$change_mean_pressure)) {
    cat(sprintf("  Mean Pressure: %.2fg → %.2fg (Δ %.2fg)\n",
                old_stats$mean_contact_pressure_g,
                comp$new_mean_pressure,
                comp$change_mean_pressure))
    cat(sprintf("  CV%%: %.2f%% → %.2f%% (Δ %.2f%%)\n",
                old_stats$cv_contact_percent,
                comp$new_cv_pct,
                comp$change_cv_pct))
  }
  cat("\n\n")
}

# Combine comparison data
comparison_df <- bind_rows(comparison_list)

# Save comparison table
write_csv(comparison_df, file.path(OUTPUT_PATH, "plateau_comparison_statistics.csv"))
cat("Saved: plateau_comparison_statistics.csv\n\n")

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
COLOR_DERIV <- "#3498DB"

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
  
  # Prepare data
  plot_data_old <- trace_old %>%
    mutate(method = "Old (±10% Tolerance)",
           detected = is_contact)
  
  plot_data_new <- trace_new %>%
    mutate(method = "New (Flat Plateau Detection)",
           detected = is_plateau)
  
  plot_data_combined <- bind_rows(plot_data_old, plot_data_new)
  
  p1 <- ggplot(plot_data_combined, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = COLOR_TRACE, linewidth = 0.3, alpha = 0.6) +
    geom_point(data = plot_data_combined %>% filter(detected),
               aes(color = method), size = 0.4, alpha = 0.5) +
    facet_wrap(~method, ncol = 1) +
    scale_color_manual(values = c("Old (±10% Tolerance)" = COLOR_OLD,
                                   "New (Flat Plateau Detection)" = COLOR_NEW)) +
    labs(
      title = paste(sample_id, "- Method Comparison"),
      subtitle = paste0(target_pressure, "g target | Red = Old method, Green = New method (flat plateaus)"),
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
  
  ggsave(file.path(OUTPUT_PATH, "plateau_plots", 
                   paste0(sample_id, "_plateau_comparison.png")),
         p1, width = 12, height = 8, dpi = 300, bg = "white")
  
  cat("  Saved: comparison plot\n")
  
  # ============================================================================
  # PLOT 2: Derivative Profile
  # ============================================================================
  
  p2_top <- ggplot(trace_new, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = COLOR_TRACE, linewidth = 0.4, alpha = 0.7) +
    geom_point(data = trace_new %>% filter(is_plateau),
               color = COLOR_NEW, size = 0.5, alpha = 0.7) +
    labs(
      title = paste(sample_id, "- Derivative-Based Plateau Detection"),
      subtitle = paste0(target_pressure, "g target | Green = Detected flat plateaus"),
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
  
  # Bottom: Derivative trace
  deriv_threshold <- PARAMS$max_derivative_g_per_sec
  
  p2_bottom <- ggplot(trace_new, aes(x = elapsed_sec, y = abs_derivative)) +
    geom_line(color = COLOR_DERIV, linewidth = 0.5) +
    geom_hline(yintercept = deriv_threshold, 
               linetype = "dashed", color = COLOR_NEW, linewidth = 0.8) +
    annotate("text", x = max(trace_new$elapsed_sec, na.rm = TRUE) * 0.95, 
             y = deriv_threshold * 1.3,
             label = paste0("Threshold: ", round(deriv_threshold, 1), " g/s"),
             color = COLOR_NEW, hjust = 1, fontface = "bold") +
    labs(
      x = "Time (seconds)",
      y = "|dP/dt| (g/s)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_text(face = "bold")
    ) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(ylim = c(0, min(50, max(trace_new$abs_derivative, na.rm = TRUE))))
  
  # Combine plots
  p2 <- p2_top / p2_bottom + plot_layout(heights = c(2, 1))
  
  ggsave(file.path(OUTPUT_PATH, "plateau_plots", 
                   paste0(sample_id, "_derivative_profile.png")),
         p2, width = 12, height = 8, dpi = 300, bg = "white")
  
  cat("  Saved: derivative profile\n")
  
  # ============================================================================
  # PLOT 3: Validation Regions (Sample-Specific)
  # ============================================================================
  
  if (sample_id == "PILOT_005") {
    # Show both validation regions
    validation_data <- trace_new %>%
      filter((elapsed_sec >= 93 & elapsed_sec <= 105) |
             (elapsed_sec >= 148 & elapsed_sec <= 160))
    
    p3 <- ggplot(validation_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = COLOR_TRACE, linewidth = 0.6) +
      geom_point(data = validation_data %>% filter(is_plateau),
                 color = COLOR_NEW, size = 2, alpha = 0.7) +
      geom_vline(xintercept = c(95.8, 102.9, 149.0, 158.9),
                 linetype = "dotted", color = "blue", alpha = 0.5) +
      annotate("rect", xmin = 95.8, xmax = 102.9, ymin = -Inf, ymax = Inf,
               fill = "blue", alpha = 0.1) +
      annotate("rect", xmin = 149.0, xmax = 158.9, ymin = -Inf, ymax = Inf,
               fill = "blue", alpha = 0.1) +
      labs(
        title = paste(sample_id, "- Validation: User-Identified Regions"),
        subtitle = "Blue boxes = Expected plateaus (95.8-102.9s, 149-159s) | Green dots = Detected",
        x = "Time (seconds)",
        y = "Load (g)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "#7F8C8D", size = 10)
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
    
  } else {
    # PILOT_001: Show 133-143s region
    validation_data <- trace_new %>%
      filter(elapsed_sec >= 130 & elapsed_sec <= 146)
    
    p3 <- ggplot(validation_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = COLOR_TRACE, linewidth = 0.6) +
      geom_point(data = validation_data %>% filter(is_plateau),
                 color = COLOR_NEW, size = 2, alpha = 0.7) +
      geom_vline(xintercept = c(133, 143),
                 linetype = "dotted", color = "blue", alpha = 0.5) +
      annotate("rect", xmin = 133, xmax = 143, ymin = -Inf, ymax = Inf,
               fill = "blue", alpha = 0.1) +
      labs(
        title = paste(sample_id, "- Validation: User-Identified Region"),
        subtitle = "Blue box = Expected region (133-143s) | Green dots = Detected flat plateaus",
        x = "Time (seconds)",
        y = "Load (g)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "#7F8C8D", size = 10)
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
  }
  
  ggsave(file.path(OUTPUT_PATH, "plateau_plots", 
                   paste0(sample_id, "_validation.png")),
         p3, width = 12, height = 6, dpi = 300, bg = "white")
  
  cat("  Saved: validation plot\n\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("PLATEAU DETECTION TEST COMPLETE\n")
cat("================================================================================\n\n")

cat("Outputs saved to:", OUTPUT_PATH, "\n\n")

cat("Summary:\n")
cat("  - Tested on", length(TEST_SAMPLES), "samples\n")
cat("  - Generated", length(TEST_SAMPLES) * 3, "comparison plots\n")
cat("  - Saved plateau comparison statistics CSV\n\n")

cat("Key Findings:\n")
for (sample_id in names(all_data)) {
  comp <- comparison_list[[sample_id]]
  cat(sprintf("  %s: %.1f%% → %.1f%% plateau time (Δ %.1f%%), %d → %d passes\n",
              sample_id,
              comp$old_pct_time,
              comp$new_pct_time,
              comp$change_pct_time,
              comp$old_n_passes,
              comp$new_n_passes))
}

cat("\n")
cat("Validation Results:\n")
cat("  PILOT_005:\n")
cat(sprintf("    Region 1 (95.8-102.9s): %.1f%% detected\n",
            100 * nrow(pilot005_region1) / nrow(pilot005_data %>% 
                                                filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("    Region 2 (149-159s): %.1f%% detected\n",
            100 * nrow(pilot005_region2) / nrow(pilot005_data %>% 
                                                filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))
cat("  PILOT_001:\n")
cat(sprintf("    Region (133-143s): %.1f%% detected\n",
            100 * nrow(pilot001_region) / nrow(pilot001_data %>% 
                                                filter(elapsed_sec >= 133, elapsed_sec <= 143))))

cat("\n")
cat("Next steps:\n")
cat("  1. Review plots in test_outputs/plateau_plots/\n")
cat("  2. Check validation plots against expected regions\n")
cat("  3. Examine plateau_comparison_statistics.csv\n")
cat("  4. If results look good, integrate into main analysis script\n")
cat("  5. Apply to all 8 samples\n\n")
