# ==============================================================================
# Point-to-Point Change Detection Test
# ==============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/pointchange_functions.R")
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/analyze_pressure_traces_final.R")

BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

dir.create(file.path(OUTPUT_PATH, "pointchange_plots"), recursive = TRUE, showWarnings = FALSE)

TEST_SAMPLES <- c("PILOT_001", "PILOT_005")

PARAMS <- list(
  max_point_change_g = 2.0,         # < 2g point-to-point change
  min_pressure_g = NULL,            # Auto
  min_stable_duration_sec = 0.8,    # Min 0.8s stable
  max_stable_duration_sec = 3.0,    # Max 3s stable
  cluster_gap_sec = 15.0            # Cluster within 15s
)

cat("================================================================================\n")
cat("POINT-TO-POINT CHANGE DETECTION TEST\n")
cat("================================================================================\n\n")

metadata <- data.frame(
  run_id = c("PILOT_001", "PILOT_005"),
  target_pressure_g = c(200, 50)
)

all_data <- list()
for (sid in TEST_SAMPLES) {
  cat(paste("Loading", sid, "..."))
  trace <- read_pressure_trace_file(sid, TRACES_PATH)
  if (!is.null(trace) && nrow(trace) > 0) {
    cat(" OK\n")
    all_data[[sid]] <- trace
  } else {
    cat(" FAILED\n")
  }
}
cat("\n")

# OLD METHOD
cat("OLD METHOD\n")
cat("==========\n")
old_results <- list()
for (sid in names(all_data)) {
  trace <- all_data[[sid]]
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  trace_old <- identify_contact_periods(trace, target, 0.10, 0.2)
  stats_old <- calculate_contact_statistics(trace_old, target)
  old_results[[sid]] <- list(data = trace_old, stats = stats_old)
}
cat("\n")

# NEW METHOD
cat("NEW METHOD (Point-to-Point Change)\n")
cat("==================================\n")
cat(sprintf("Max change: %.1fg\n", PARAMS$max_point_change_g))
cat(sprintf("Segment: %.1f-%.1fs\n", PARAMS$min_stable_duration_sec, PARAMS$max_stable_duration_sec))
cat(sprintf("Cluster gap: %.1fs\n\n", PARAMS$cluster_gap_sec))

new_results <- list()
for (sid in names(all_data)) {
  cat(paste("Processing", sid, "...\n"))
  trace <- all_data[[sid]]
  target <- metadata %>% filter(run_id == sid) %>% pull(target_pressure_g)
  
  trace_new <- identify_pointchange_passes(
    trace, target,
    max_point_change_g = PARAMS$max_point_change_g,
    min_pressure_g = PARAMS$min_pressure_g,
    min_stable_duration_sec = PARAMS$min_stable_duration_sec,
    max_stable_duration_sec = PARAMS$max_stable_duration_sec,
    cluster_gap_sec = PARAMS$cluster_gap_sec
  )
  
  stats_new <- tryCatch({
    calculate_pointchange_statistics(trace_new, target)
  }, error = function(e) NULL)
  
  new_results[[sid]] <- list(data = trace_new, stats = stats_new)
}

# VALIDATION
cat("================================================================================\n")
cat("VALIDATION\n")
cat("================================================================================\n\n")

pilot005 <- new_results[["PILOT_005"]]$data
p005_r1 <- pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9, is_contact == TRUE)
p005_r2 <- pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9, is_contact == TRUE)

cat("PILOT_005:\n")
cat(sprintf("  Region 1 (95.8-102.9s): %.1f%% detected\n",
            100 * nrow(p005_r1) / nrow(pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  Region 2 (149-159s): %.1f%% detected\n",
            100 * nrow(p005_r2) / nrow(pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))

pilot001 <- new_results[["PILOT_001"]]$data
p001_r1 <- pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143, is_contact == TRUE)

cat(sprintf("\nPILOT_001:\n  Region (133-143s): %.1f%% detected\n\n",
            100 * nrow(p001_r1) / nrow(pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))

# PLOTS
cat("Generating plots...\n")

for (sid in names(all_data)) {
  trace_new <- new_results[[sid]]$data
  
  if (sid == "PILOT_005") {
    val_data <- trace_new %>% filter(elapsed_sec >= 90 & elapsed_sec <= 165)
    p <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = "#2C3E50", linewidth = 0.4) +
      geom_point(data = val_data %>% filter(is_stable_segment), 
                 color = "#2ECC71", size = 1.2, alpha = 0.8) +
      geom_rect(aes(xmin = 95.8, xmax = 102.9, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.05) +
      geom_rect(aes(xmin = 149, xmax = 159, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.05) +
      labs(title = paste(sid, "- Point-to-Point Change Detection"),
           subtitle = "Blue = Expected | Green = Detected stable segments",
           x = "Time (s)", y = "Load (g)") +
      theme_minimal()
  } else {
    val_data <- trace_new %>% filter(elapsed_sec >= 128 & elapsed_sec <= 148)
    p <- ggplot(val_data, aes(x = elapsed_sec, y = load_g)) +
      geom_line(color = "#2C3E50", linewidth = 0.4) +
      geom_point(data = val_data %>% filter(is_stable_segment), 
                 color = "#2ECC71", size = 1.2, alpha = 0.8) +
      geom_rect(aes(xmin = 133, xmax = 143, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.05) +
      labs(title = paste(sid, "- Point-to-Point Change Detection"),
           subtitle = "Blue = Expected | Green = Detected stable segments",
           x = "Time (s)", y = "Load (g)") +
      theme_minimal()
  }
  
  ggsave(file.path(OUTPUT_PATH, "pointchange_plots", paste0(sid, "_validation.png")),
         p, width = 12, height = 6, dpi = 300, bg = "white")
}

cat("\n================================================================================\n")
cat("COMPLETE\n")
cat("================================================================================\n\n")

cat("Validation:\n")
cat(sprintf("  PILOT_005 R1: %.1f%%\n",
            100 * nrow(p005_r1) / nrow(pilot005 %>% filter(elapsed_sec >= 95.8, elapsed_sec <= 102.9))))
cat(sprintf("  PILOT_005 R2: %.1f%%\n",
            100 * nrow(p005_r2) / nrow(pilot005 %>% filter(elapsed_sec >= 149.0, elapsed_sec <= 158.9))))
cat(sprintf("  PILOT_001: %.1f%%\n",
            100 * nrow(p001_r1) / nrow(pilot001 %>% filter(elapsed_sec >= 133, elapsed_sec <= 143))))
