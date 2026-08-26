# ==============================================================================
# ASTRA Sustained Pressure Detection Test
# ==============================================================================
# 
# This script tests the sustained pressure contact detection approach.
# This method identifies regions where pressure is maintained at a contact
# level with natural fluctuation, excluding major ramp-up/down transitions.
#
# Tests on: PILOT_001 (200g) and PILOT_005 (50g)
# Validates against user-identified regions:
# - PILOT_005: 95.8-102.9s and 149.0-158.9s
# - PILOT_001: Flat portions within 133-143s
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-sustained
# ==============================================================================

# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(zoo)
})

# Source functions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/sustained_pressure_functions.R")
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/analyze_pressure_traces_final.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

dir.create(file.path(OUTPUT_PATH, "sustained_plots"), recursive = TRUE, showWarnings = FALSE)

TEST_SAMPLES <- c("PILOT_001", "PILOT_005")

# Sustained pressure detection parameters
PARAMS <- list(
  min_pressure_pct = 0.60,           # 60% of target minimum
  max_pressure_pct = 1.30,           # 130% of target maximum
  transition_window_sec = 2.0,       # 2-second window for transition detection
  transition_threshold_g = NULL,     # Auto: 30g for 200g, 10g for 50g
  min_duration_sec = 1.0,            # Minimum 1 second
  max_duration_sec = 15.0,           # Maximum 15 seconds
  merge_gap_sec = 0.2                # Merge gaps < 0.2s
)

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("================================================================================\n")
cat("SUSTAINED PRESSURE DETECTION TEST\n")
cat("================================================================================\n\n")

cat("Loading test samples...\n")

metadata <- data.frame(
  run_id = c("PILOT_001", "PILOT_005"),
  target_pressure_g = c(200, 50),
  surface_type = c("steel", "steel")
)

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

if (length(all_data) == 0) stop("No data loaded!")
cat("\n")

# ==============================================================================
# APPLY OLD METHOD
# ==============================================================================

cat("================================================================================\n")
cat("APPLYING OLD METHOD (±10% Tolerance Band)\n")
cat("================================================================================\n\n")

old_results <- list()

for (sample_id in names(all_data)) {
  cat(paste("Processing", sample_id, "with OLD method...\n"))
  trace <- all_data[[sample_id]]
  target_pressure <- metadata %>% filter(run_id == sample_id) %>% pull(target_pressure_g)
  
  trace_old <- identify_contact_periods(trace, target_pressure_g = target_pressure,
                                         tolerance_pct = 0.10, merge_gap_sec = 0.2)
  stats_old <- calculate_contact_statistics(trace_old, target_pressure)
  
  old_results[[sample_id]] <- list(data = trace_old, stats = stats_old)
  cat("\n")
}

# ==============================================================================
# APPLY NEW METHOD (SUSTAINED PRESSURE)
# ==============================================================================

cat("================================================================================\n")
cat("APPLYING NEW METHOD (Sustained Pressure Detection)\n")
cat("================================================================================\n\n")

cat("Parameters:\n")
cat(sprintf("  Pressure range: %.0f%% - %.0f%% of target\n", 
            PARAMS$min_pressure_pct * 100, PARAMS$max_pressure_pct * 100))
cat(sprintf("  Transition window: %.1f seconds\n", PARAMS$transition_window_sec))
cat(sprintf("  Transition threshold: Auto (30g for 200g, 10g for 50g)\n"))
cat(sprintf("  Duration: %.1f - %.1f seconds\n", 
            PARAMS$min_duration_sec, PARAMS$max_duration_sec))
cat(sprintf("  Merge gap: %.1f seconds\n\n", PARAMS$merge_gap_sec))

new_results <- list()

for (sample_id in names(all_data)) {
  cat(paste("Processing", sample_id, "with NEW method...\n"))
  trace <- all_data[[sample_id]]
  target_pressure <- metadata %>% filter(run_id == sample_id) %>% pull(target_pressure_g)
  
  trace_new <- identify_sustained_contact(
    trace,
    target_pressure_g = target_pressure,
    min_pressure_pct = PARAMS$min_pressure_pct,
    max_pressure_pct = PARAMS$max_pressure_pct,
    transition_window_sec = PARAMS$transition_window_sec,
    transition_threshold_g = PARAMS$transition_threshold_g,
    min_duration_sec = PARAMS$min_duration_sec,
    max_duration_sec = PARAMS$max_duration_sec,
    merge_gap_sec = PARAMS$merge_gap_sec
  )
  
  stats_new <- tryCatch({
    calculate_sustained_statistics(trace_new, target_pressure)
  }, error = function(e) {
    warning(paste("No sustained contact for", sample_id))
    NULL
  })
  
  new_results[[sample_id]] <- list(data = trace_new, stats = stats_new)
  cat("\n")
}

# ==============================================================================
# VALIDATION
# ==============================================================================

cat("================================================================================\n")
cat("VALIDATION: Checking User-Identified Regions\n")
cat("================================================================================\n\n")

cat("PILOT_005 Expected Regions:\n")
cat("  Region 1: 95.8 - 102.9 seconds\n")
cat("  Region 2: 149.0 - 158.9 seconds\n\n")

pilot005_data <- new_results[["PILOT_005"]]$data
pilot005_r1 <- pilot005_data %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9, is_contact == TRUE)
pilot005_r2 <- pilot005_data %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9, is_contact == TRUE)

cat(sprintf("  Detected in Region 1: %d points (%.1f%% of region)\n",
            nrow(pilot005_r1),
            100 * nrow(pilot005_r1) / nrow(pilot005_data %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  Detected in Region 2: %d points (%.1f%% of region)\n\n",
            nrow(pilot005_r2),
            100 * nrow(pilot005_r2) / nrow(pilot005_data %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))

cat("PILOT_001 Expected Region:\n")
cat("  Within 133-143 seconds\n\n")

pilot001_data <- new_results[["PILOT_001"]]$data
pilot001_r1 <- pilot001_data %>% filter(elapsed_sec >= 133, elapsed_sec <= 143, is_contact == TRUE)

cat(sprintf("  Detected in 133-143s: %d points (%.1f%% of region)\n\n",
            nrow(pilot001_r1),
            100 * nrow(pilot001_r1) / nrow(pilot001_data %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))

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
  target_pressure <- metadata %>% filter(run_id == sample_id) %>% pull(target_pressure_g)
  
  comp <- data.frame(
    sample_id = sample_id,
    target_pressure_g = target_pressure,
    old_pct_time = old_stats$percent_time_in_contact,
    old_n_passes = old_stats$number_of_passes,
    old_mean_pressure = old_stats$mean_contact_pressure_g,
    old_cv_pct = old_stats$cv_contact_percent,
    new_pct_time = ifelse(is.null(new_stats), 0, new_stats$percent_time_in_contact),
    new_n_passes = ifelse(is.null(new_stats), 0, new_stats$number_of_passes),
    new_mean_pressure = ifelse(is.null(new_stats), NA, new_stats$mean_contact_pressure_g),
    new_cv_pct = ifelse(is.null(new_stats), NA, new_stats$cv_contact_percent)
  )
  
  comparison_list[[sample_id]] <- comp
  
  cat(sprintf("Target: %dg\n\n", target_pressure))
  cat("OLD METHOD:\n")
  cat(sprintf("  %% Time: %.1f%%, Passes: %d, Mean: %.2fg, CV: %.2f%%\n\n", 
              old_stats$percent_time_in_contact, old_stats$number_of_passes,
              old_stats$mean_contact_pressure_g, old_stats$cv_contact_percent))
  
  cat("NEW METHOD (Sustained Pressure):\n")
  if (!is.null(new_stats)) {
    cat(sprintf("  %% Time: %.1f%%, Passes: %d, Mean: %.2fg, CV: %.2f%%\n\n",
                new_stats$percent_time_in_contact, new_stats$number_of_passes,
                new_stats$mean_contact_pressure_g, new_stats$cv_contact_percent))
  } else {
    cat("  No sustained contact detected\n\n")
  }
  
  cat("CHANGES:\n")
  cat(sprintf("  %% Time: %.1f%% → %.1f%% (Δ %.1f%%)\n",
              comp$old_pct_time, comp$new_pct_time, comp$new_pct_time - comp$old_pct_time))
  cat(sprintf("  Passes: %d → %d (Δ %d)\n\n\n",
              comp$old_n_passes, comp$new_n_passes, comp$new_n_passes - comp$old_n_passes))
}

comparison_df <- bind_rows(comparison_list)
write_csv(comparison_df, file.path(OUTPUT_PATH, "sustained_comparison_statistics.csv"))
cat("Saved: sustained_comparison_statistics.csv\n\n")

# ==============================================================================
# GENERATE PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("GENERATING COMPARISON PLOTS\n")
cat("================================================================================\n\n")

COLOR_OLD <- "#E74C3C"
COLOR_NEW <- "#2ECC71"
COLOR_TRACE <- "#2C3E50"

for (sample_id in names(all_data)) {
  cat(paste("Creating plots for", sample_id, "...\n"))
  
  target_pressure <- metadata %>% filter(run_id == sample_id) %>% pull(target_pressure_g)
  trace_old <- old_results[[sample_id]]$data
  trace_new <- new_results[[sample_id]]$data
  
  # Comparison plot
  plot_data <- bind_rows(
    trace_old %>% mutate(method = "Old (±10%)", detected = is_contact),
    trace_new %>% mutate(method = "New (Sustained)", detected = is_contact)
  )
  
  p1 <- ggplot(plot_data, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = COLOR_TRACE, linewidth = 0.3, alpha = 0.6) +
    geom_point(data = plot_data %>% filter(detected),
               aes(color = method), size = 0.4, alpha = 0.5) +
    facet_wrap(~method, ncol = 1) +
    scale_color_manual(values = c("Old (±10%)" = COLOR_OLD, "New (Sustained)" = COLOR_NEW)) +
    labs(title = paste(sample_id, "- Sustained Pressure Detection"),
         subtitle = paste0(target_pressure, "g target | Green = Sustained contact regions"),
         x = "Time (seconds)", y = "Load (g)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none",
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "#ECF0F1", color = NA)) +
    scale_y_continuous(labels = comma) +
    geom_hline(yintercept = target_pressure, linetype = "dashed", color = "#95A5A6", alpha = 0.5)
  
  ggsave(file.path(OUTPUT_PATH, "sustained_plots", paste0(sample_id, "_sustained_comparison.png")),
         p1, width = 12, height = 8, dpi = 300, bg = "white")
  
  # Validation plot
  if (sample_id == "PILOT_005") {
    val_data <- trace_new %>% filter((elapsed_sec >= 93 & elapsed_sec <= 105) |
                                      (elapsed_sec >= 148 & elapsed_sec <= 160))
    p2 <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = COLOR_TRACE, linewidth = 0.6) +
      geom_point(data = val_data %>% filter(is_contact), color = COLOR_NEW, size = 2, alpha = 0.7) +
      geom_vline(xintercept = c(95.8, 102.9, 149.0, 158.9), linetype = "dotted", color = "blue") +
      annotate("rect", xmin = 95.8, xmax = 102.9, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
      annotate("rect", xmin = 149.0, xmax = 158.9, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
      labs(title = paste(sample_id, "- Validation"),
           subtitle = "Blue = Expected | Green = Detected",
           x = "Time (seconds)", y = "Load (g)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  } else {
    val_data <- trace_new %>% filter(elapsed_sec >= 130 & elapsed_sec <= 146)
    p2 <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = COLOR_TRACE, linewidth = 0.6) +
      geom_point(data = val_data %>% filter(is_contact), color = COLOR_NEW, size = 2, alpha = 0.7) +
      geom_vline(xintercept = c(133, 143), linetype = "dotted", color = "blue") +
      annotate("rect", xmin = 133, xmax = 143, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
      labs(title = paste(sample_id, "- Validation"),
           subtitle = "Blue = Expected | Green = Detected",
           x = "Time (seconds)", y = "Load (g)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  }
  
  ggsave(file.path(OUTPUT_PATH, "sustained_plots", paste0(sample_id, "_validation.png")),
         p2, width = 12, height = 6, dpi = 300, bg = "white")
  
  cat("  Saved plots\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n================================================================================\n")
cat("SUSTAINED PRESSURE DETECTION TEST COMPLETE\n")
cat("================================================================================\n\n")

cat("Validation Results:\n")
cat(sprintf("  PILOT_005 Region 1 (95.8-102.9s): %.1f%% detected\n",
            100 * nrow(pilot005_r1) / nrow(pilot005_data %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  PILOT_005 Region 2 (149-159s): %.1f%% detected\n",
            100 * nrow(pilot005_r2) / nrow(pilot005_data %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))
cat(sprintf("  PILOT_001 Region (133-143s): %.1f%% detected\n\n",
            100 * nrow(pilot001_r1) / nrow(pilot001_data %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))

cat("Key Findings:\n")
for (sample_id in names(all_data)) {
  comp <- comparison_list[[sample_id]]
  cat(sprintf("  %s: %.1f%% → %.1f%% contact (Δ %.1f%%), %d → %d passes\n",
              sample_id, comp$old_pct_time, comp$new_pct_time,
              comp$new_pct_time - comp$old_pct_time,
              comp$old_n_passes, comp$new_n_passes))
}

cat("\nPlots saved to: test_outputs/sustained_plots/\n")
