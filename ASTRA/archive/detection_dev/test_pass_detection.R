# ==============================================================================
# Pass-Based Detection Test
# ==============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/pass_based_detection.R")

BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/test_outputs"

dir.create(file.path(OUTPUT_PATH, "pass_plots"), recursive = TRUE, showWarnings = FALSE)

cat("================================================================================\n")
cat("PASS-BASED DETECTION WITH RAMP EXCLUSION\n")
cat("================================================================================\n\n")

metadata <- data.frame(
  run_id = c("PILOT_001", "PILOT_005"),
  target_pressure_g = c(200, 50)
)

# Test PILOT_005
cat("Testing PILOT_005...\n")
trace <- read_pressure_trace_file("PILOT_005", TRACES_PATH)
cat(sprintf("Loaded %d rows\n\n", nrow(trace)))

trace_new <- identify_passes_with_stable_regions(
  trace,
  target_pressure_g = 50,
  zero_threshold = 5,
  max_zero_gap_sec = 0.5,
  max_point_change_g = 2.0,
  min_stable_duration_sec = 0.5
)

# Validation - CORRECT time ranges
cat("================================================================================\n")
cat("VALIDATION (Correct Time Ranges)\n")
cat("================================================================================\n\n")

cat("Expected stable regions (based on row numbers):\n")
cat("  Region 1: Rows 95-103  = Time 10.2-11.1s\n")
cat("  Region 2: Rows 149-159 = Time 15.7-16.8s\n\n")

r1_stable <- trace_new %>% filter(elapsed_sec >= 10.2, elapsed_sec <= 11.1, is_stable == TRUE)
r1_total <- trace_new %>% filter(elapsed_sec >= 10.2, elapsed_sec <= 11.1)

r2_stable <- trace_new %>% filter(elapsed_sec >= 15.7, elapsed_sec <= 16.8, is_stable == TRUE)
r2_total <- trace_new %>% filter(elapsed_sec >= 15.7, elapsed_sec <= 16.8)

cat("Detected:\n")
cat(sprintf("  Region 1 (10.2-11.1s): %d/%d points = %.1f%%\n",
            nrow(r1_stable), nrow(r1_total),
            100 * nrow(r1_stable) / nrow(r1_total)))
cat(sprintf("  Region 2 (15.7-16.8s): %d/%d points = %.1f%%\n\n",
            nrow(r2_stable), nrow(r2_total),
            100 * nrow(r2_stable) / nrow(r2_total)))

# Show what passes these regions fall into
cat("Pass assignments in expected regions:\n")
r1_passes <- trace_new %>% filter(elapsed_sec >= 10.2, elapsed_sec <= 11.1) %>%
             group_by(pass_id) %>% summarize(n = n(), .groups = "drop")
cat("  Region 1 passes:\n")
print(r1_passes)

r2_passes <- trace_new %>% filter(elapsed_sec >= 15.7, elapsed_sec <= 16.8) %>%
             group_by(pass_id) %>% summarize(n = n(), .groups = "drop")
cat("\n  Region 2 passes:\n")
print(r2_passes)

# Plot
cat("\nGenerating validation plot...\n")

plot_data <- trace_new %>% filter(elapsed_sec >= 5 & elapsed_sec <= 25)

p <- ggplot(plot_data, aes(x = elapsed_sec, y = load_g)) +
  geom_line(color = "#2C3E50", linewidth = 0.4, alpha = 0.7) +
  geom_point(data = plot_data %>% filter(is_stable), 
             color = "#2ECC71", size = 1.5, alpha = 0.8) +
  annotate("rect", xmin = 10.2, xmax = 11.1, ymin = -Inf, ymax = Inf,
           fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 15.7, xmax = 16.8, ymin = -Inf, ymax = Inf,
           fill = "blue", alpha = 0.1) +
  annotate("text", x = 10.65, y = 65, label = "Region 1", color = "blue", fontface = "bold") +
  annotate("text", x = 16.25, y = 65, label = "Region 2", color = "blue", fontface = "bold") +
  labs(title = "PILOT_005 - Pass-Based Stable Region Detection",
       subtitle = "Blue boxes = Expected regions | Green dots = Detected stable points",
       x = "Time (seconds)", y = "Load (g)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_PATH, "pass_plots", "PILOT_005_validation.png"),
       p, width = 12, height = 6, dpi = 300, bg = "white")

cat("\n================================================================================\n")
cat("TEST COMPLETE\n")
cat("================================================================================\n\n")

if (nrow(r1_stable) > 0 && nrow(r2_stable) > 0) {
  cat("SUCCESS! Both regions detected!\n")
} else if (nrow(r1_stable) > 0) {
  cat("PARTIAL: Region 1 detected, Region 2 missed\n")
} else if (nrow(r2_stable) > 0) {
  cat("PARTIAL: Region 2 detected, Region 1 missed\n")
} else {
  cat("FAILURE: Neither region detected\n")
}

cat(sprintf("\nRegion 1: %.1f%% detected\n", 100 * nrow(r1_stable) / nrow(r1_total)))
cat(sprintf("Region 2: %.1f%% detected\n", 100 * nrow(r2_stable) / nrow(r2_total)))
cat(sprintf("\nPlot saved to: %s\n", file.path(OUTPUT_PATH, "pass_plots", "PILOT_005_validation.png")))
