# ==============================================================================
# ASTRA Micro-Plateau Clustering Test
# ==============================================================================
# 
# Tests micro-plateau detection with clustering approach.
# Detects short flat segments and groups them into swabbing passes.
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# Version: 3.0-microplateau
# ==============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(zoo)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/microplateau_functions.R")
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/analyze_pressure_traces_final.R")

# ==============================================================================
# CONFIG
# ==============================================================================

BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

dir.create(file.path(OUTPUT_PATH, "microplateau_plots"), recursive = TRUE, showWarnings = FALSE)

TEST_SAMPLES <- c("PILOT_001", "PILOT_005")

PARAMS <- list(
  min_pressure_g = NULL,              # Auto: 100g for 200g, 30g for 50g
  max_derivative_g_per_sec = 10,      # 10 g/s for flatness
  micro_min_duration_sec = 0.5,       # Micro-plateaus: 0.5-3s
  micro_max_duration_sec = 3.0,
  cluster_gap_sec = 5.0,              # Group if < 5s apart
  pass_min_duration_sec = 3.0         # Pass must span >= 3s
)

cat("================================================================================\n")
cat("MICRO-PLATEAU CLUSTERING TEST\n")
cat("================================================================================\n\n")

cat("Loading samples...\n")

metadata <- data.frame(
  run_id = c("PILOT_001", "PILOT_005"),
  target_pressure_g = c(200, 50)
)

all_data <- list()
for (sid in TEST_SAMPLES) {
  cat(paste("  Loading", sid, "..."))
  trace <- read_pressure_trace_file(sid, TRACES_PATH)
  if (!is.null(trace) && nrow(trace) > 0) {
    cat(paste(" OK (", nrow(trace), " rows)\n", sep=""))
    all_data[[sid]] <- trace
  } else {
    cat(" FAILED\n")
  }
}

cat("\n")

# ==============================================================================
# OLD METHOD
# ==============================================================================

cat("================================================================================\n")
cat("OLD METHOD (±10% Tolerance)\n")
cat("================================================================================\n\n")

old_results <- list()
for (sid in names(all_data)) {
  cat(paste("Processing", sid, "...\n"))
  trace <- all_data[[sid]]
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  
  trace_old <- identify_contact_periods(trace, target_pressure_g = target,
                                         tolerance_pct = 0.10, merge_gap_sec = 0.2)
  stats_old <- calculate_contact_statistics(trace_old, target)
  old_results[[sid]] <- list(data = trace_old, stats = stats_old)
  cat("\n")
}

# ==============================================================================
# NEW METHOD
# ==============================================================================

cat("================================================================================\n")
cat("NEW METHOD (Micro-Plateau Clustering)\n")
cat("================================================================================\n\n")

cat("Parameters:\n")
cat(sprintf("  Derivative threshold: %.1f g/s\n", PARAMS$max_derivative_g_per_sec))
cat(sprintf("  Micro-plateau duration: %.1f - %.1f seconds\n", 
            PARAMS$micro_min_duration_sec, PARAMS$micro_max_duration_sec))
cat(sprintf("  Clustering gap: %.1f seconds\n", PARAMS$cluster_gap_sec))
cat(sprintf("  Pass min duration: %.1f seconds\n\n", PARAMS$pass_min_duration_sec))

new_results <- list()
for (sid in names(all_data)) {
  cat(paste("Processing", sid, "...\n"))
  trace <- all_data[[sid]]
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  
  trace_new <- identify_microplateau_passes(
    trace,
    target_pressure_g = target,
    min_pressure_g = PARAMS$min_pressure_g,
    max_derivative_g_per_sec = PARAMS$max_derivative_g_per_sec,
    micro_min_duration_sec = PARAMS$micro_min_duration_sec,
    micro_max_duration_sec = PARAMS$micro_max_duration_sec,
    cluster_gap_sec = PARAMS$cluster_gap_sec,
    pass_min_duration_sec = PARAMS$pass_min_duration_sec
  )
  
  stats_new <- tryCatch({
    calculate_microplateau_statistics(trace_new, target)
  }, error = function(e) NULL)
  
  new_results[[sid]] <- list(data = trace_new, stats = stats_new)
  cat("\n")
}

# ==============================================================================
# VALIDATION
# ==============================================================================

cat("================================================================================\n")
cat("VALIDATION\n")
cat("================================================================================\n\n")

cat("PILOT_005 Expected: 95.8-102.9s and 149.0-158.9s\n")
pilot005 <- new_results[["PILOT_005"]]$data
p005_r1 <- pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9, is_contact == TRUE)
p005_r2 <- pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9, is_contact == TRUE)

cat(sprintf("  Region 1: %d/%d points (%.1f%%)\n",
            nrow(p005_r1),
            nrow(pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9)),
            100 * nrow(p005_r1) / nrow(pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  Region 2: %d/%d points (%.1f%%)\n\n",
            nrow(p005_r2),
            nrow(pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9)),
            100 * nrow(p005_r2) / nrow(pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))

cat("PILOT_001 Expected: 133-143s\n")
pilot001 <- new_results[["PILOT_001"]]$data
p001_r1 <- pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143, is_contact == TRUE)

cat(sprintf("  Region: %d/%d points (%.1f%%)\n\n",
            nrow(p001_r1),
            nrow(pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143)),
            100 * nrow(p001_r1) / nrow(pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))

# ==============================================================================
# COMPARISON
# ==============================================================================

cat("================================================================================\n")
cat("COMPARISON\n")
cat("================================================================================\n\n")

for (sid in names(all_data)) {
  cat(paste0("=== ", sid, " ===\n"))
  old_s <- old_results[[sid]]$stats
  new_s <- new_results[[sid]]$stats
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  
  cat(sprintf("Target: %dg\n\n", target))
  cat(sprintf("OLD: %.1f%% contact, %d passes, %.2fg mean, %.2f%% CV\n",
              old_s$percent_time_in_contact, old_s$number_of_passes,
              old_s$mean_contact_pressure_g, old_s$cv_contact_percent))
  
  if (!is.null(new_s)) {
    cat(sprintf("NEW: %.1f%% in passes (%.1f%% plateaus), %d passes, %.2fg mean\n",
                new_s$percent_time_in_passes, new_s$percent_time_in_plateaus,
                new_s$number_of_passes, new_s$mean_contact_pressure_g))
    cat(sprintf("     %.2f%% CV overall, %.2f%% CV in plateaus\n",
                new_s$cv_contact_percent, new_s$cv_plateau_percent))
  } else {
    cat("NEW: No passes detected\n")
  }
  cat("\n")
}

# ==============================================================================
# PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("GENERATING PLOTS\n")
cat("================================================================================\n\n")

for (sid in names(all_data)) {
  cat(paste("Plotting", sid, "...\n"))
  
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  trace_new <- new_results[[sid]]$data
  
  # Validation plot
  if (sid == "PILOT_005") {
    val_data <- trace_new %>% filter(elapsed_sec >= 90 & elapsed_sec <= 165)
    p <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = "#2C3E50", linewidth = 0.5, alpha = 0.7) +
      geom_point(data = val_data %>% filter(is_micro_plateau), 
                 color = "#2ECC71", size = 1.5, alpha = 0.7) +
      geom_rect(aes(xmin = 95.8, xmax = 102.9, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.1) +
      geom_rect(aes(xmin = 149, xmax = 159, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.1) +
      labs(title = paste(sid, "- Micro-Plateau Detection Validation"),
           subtitle = "Blue boxes = Expected regions | Green dots = Detected micro-plateaus",
           x = "Time (seconds)", y = "Load (g)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  } else {
    val_data <- trace_new %>% filter(elapsed_sec >= 128 & elapsed_sec <= 148)
    p <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = "#2C3E50", linewidth = 0.5, alpha = 0.7) +
      geom_point(data = val_data %>% filter(is_micro_plateau), 
                 color = "#2ECC71", size = 1.5, alpha = 0.7) +
      geom_rect(aes(xmin = 133, xmax = 143, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.1) +
      labs(title = paste(sid, "- Micro-Plateau Detection Validation"),
           subtitle = "Blue box = Expected region | Green dots = Detected micro-plateaus",
           x = "Time (seconds)", y = "Load (g)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  }
  
  ggsave(file.path(OUTPUT_PATH, "microplateau_plots", paste0(sid, "_validation.png")),
         p, width = 12, height = 6, dpi = 300, bg = "white")
  
  cat("  Saved validation plot\n")
}

cat("\n================================================================================\n")
cat("TEST COMPLETE\n")
cat("================================================================================\n\n")

cat("Validation Summary:\n")
cat(sprintf("  PILOT_005 Region 1 (95.8-102.9s): %.1f%% detected\n",
            100 * nrow(p005_r1) / nrow(pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  PILOT_005 Region 2 (149-159s): %.1f%% detected\n",
            100 * nrow(p005_r2) / nrow(pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))
cat(sprintf("  PILOT_001 Region (133-143s): %.1f%% detected\n",
            100 * nrow(p001_r1) / nrow(pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))

cat("\nPlots: test_outputs/microplateau_plots/\n")
