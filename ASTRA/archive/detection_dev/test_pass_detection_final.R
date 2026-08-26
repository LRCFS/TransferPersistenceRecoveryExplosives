# ==============================================================================
# Test Script for Stable Contact Detection v4.0
# ==============================================================================
# 
# This script validates the v4.0 algorithm and compares it with v3.0
# 
# Outputs:
# 1. Diagnostic plots showing detected regions (color-coded)
# 2. Rate-of-change visualization
# 3. Comparison statistics (v3.0 vs v4.0)
# 4. Summary tables
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# ==============================================================================

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(gridExtra)
})

# Load both versions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/pass_based_detection.R")  # v3.0
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/pass_based_detection_final.R")  # v4.0

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs/v4_validation"

# Create output directory
dir.create(OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)

# Test sample
TEST_SAMPLE <- "PILOT_001"  # 200g target, steel surface
TARGET_PRESSURE <- 200

cat("==============================================================================\n")
cat("VALIDATION TEST: v3.0 vs v4.0 Algorithm Comparison\n")
cat("==============================================================================\n\n")
cat(sprintf("Test sample: %s\n", TEST_SAMPLE))
cat(sprintf("Target pressure: %dg\n\n", TARGET_PRESSURE))

# ==============================================================================
# LOAD TEST DATA
# ==============================================================================

cat("Loading test data...\n")
test_data <- read_pressure_trace_file(TEST_SAMPLE, TRACES_PATH)

if (is.null(test_data)) {
  stop("Failed to load test data!")
}

cat(sprintf("Loaded %d observations\n", nrow(test_data)))
cat(sprintf("Time range: %.1f to %.1f seconds (%.1fs total)\n\n",
            min(test_data$elapsed_sec), max(test_data$elapsed_sec),
            max(test_data$elapsed_sec) - min(test_data$elapsed_sec)))

# ==============================================================================
# RUN v3.0 ALGORITHM
# ==============================================================================

cat("Running v3.0 algorithm...\n")
data_v3 <- identify_passes_with_stable_regions(
  test_data,
  target_pressure_g = TARGET_PRESSURE,
  zero_threshold = 5,
  max_zero_gap_sec = 0.5,
  max_point_change_g = 2.0,
  min_stable_duration_sec = 0.5
)

stats_v3 <- calculate_pass_statistics(data_v3, TARGET_PRESSURE)
cat("\n")

# ==============================================================================
# RUN v4.0 ALGORITHM
# ==============================================================================

cat("Running v4.0 algorithm...\n")
data_v4 <- identify_stable_contact_regions_v4(
  test_data,
  target_pressure_g = TARGET_PRESSURE,
  zero_threshold = 2,
  max_zero_gap_sec = 0.5,
  ramp_rate_threshold = 20,
  stable_max_change_g = 5.0,
  smooth_window = 5,
  min_stable_duration_sec = 0.5
)

stats_v4 <- calculate_stable_contact_statistics(data_v4, TARGET_PRESSURE)
cat("\n")

# ==============================================================================
# COMPARISON STATISTICS
# ==============================================================================

cat("==============================================================================\n")
cat("COMPARISON: v3.0 vs v4.0\n")
cat("==============================================================================\n\n")

# Calculate v3.0 stable time
v3_stable_time <- if (!is.null(stats_v3) && "percent_time_stable" %in% names(stats_v3)) {
  round((stats_v3$percent_time_stable / 100) * (max(test_data$elapsed_sec) - min(test_data$elapsed_sec)), 2)
} else {
  NA
}

comparison <- data.frame(
  Metric = c(
    "Number of passes",
    "Stable time (seconds)",
    "Stable time (%)",
    "Mean stable pressure (g)",
    "CV stable (%)",
    "Stable segments",
    "Mean segment duration (s)"
  ),
  v3.0 = c(
    ifelse(!is.null(stats_v3), stats_v3$number_of_passes, NA),
    v3_stable_time,
    ifelse(!is.null(stats_v3), round(stats_v3$percent_time_stable, 1), NA),
    ifelse(!is.null(stats_v3), round(stats_v3$mean_stable_pressure_g, 1), NA),
    ifelse(!is.null(stats_v3), round(stats_v3$cv_stable_percent, 1), NA),
    NA,
    ifelse(!is.null(stats_v3), round(stats_v3$mean_pass_duration_sec, 2), NA)
  ),
  v4.0 = c(
    stats_v4$n_passes,
    round(stats_v4$total_stable_time_sec, 2),
    round(stats_v4$percent_time_stable, 1),
    round(stats_v4$mean_stable_pressure_g, 1),
    round(stats_v4$cv_stable_percent, 1),
    stats_v4$n_stable_segments,
    round(stats_v4$mean_stable_segment_duration_sec, 2)
  )
)

print(comparison)
cat("\n")

# ==============================================================================
# VISUALIZATION 1: Full Trace with Regions (v4.0)
# ==============================================================================

cat("Creating visualization 1: Full trace with regions...\n")

# Define region colors
region_colors <- c(
  "zero" = "#95A5A6",           # Gray
  "ramp_up" = "#E74C3C",        # Red
  "stable" = "#2ECC71",          # Green
  "ramp_down" = "#3498DB"        # Blue
)

p1 <- ggplot(data_v4, aes(x = elapsed_sec, y = load_g, color = region)) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  scale_color_manual(
    values = region_colors,
    labels = c("Zero (no contact)", "Ramp-up (applying)", "Stable (contact)", "Ramp-down (removing)")
  ) +
  labs(
    title = paste(TEST_SAMPLE, "- v4.0 Region Detection"),
    subtitle = sprintf("%dg target | Zero < 2g | Ramp rate: ±20 g/s | Stable: ±5g", TARGET_PRESSURE),
    x = "Time (seconds)",
    y = "Pressure (g)",
    color = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma)

ggsave(file.path(OUTPUT_PATH, "v4_full_trace_regions.png"), 
       p1, width = 12, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# VISUALIZATION 2: Zoomed View with Rate of Change
# ==============================================================================

cat("Creating visualization 2: Zoomed view with rate...\n")

# Select a representative window (e.g., 50-100 seconds)
zoom_start <- 50
zoom_end <- 100

data_zoom <- data_v4 %>% 
  filter(elapsed_sec >= zoom_start, elapsed_sec <= zoom_end)

# Create dual-axis plot
p2a <- ggplot(data_zoom, aes(x = elapsed_sec)) +
  geom_line(aes(y = load_g, color = region), linewidth = 0.8) +
  scale_color_manual(values = region_colors) +
  labs(
    title = paste(TEST_SAMPLE, "- Zoomed View with Rate of Change"),
    subtitle = sprintf("Time window: %d-%d seconds | Shows pressure and rate", zoom_start, zoom_end),
    x = "Time (seconds)",
    y = "Pressure (g)",
    color = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma)

p2b <- ggplot(data_zoom, aes(x = elapsed_sec)) +
  geom_line(aes(y = rate_smooth), color = "#9B59B6", linewidth = 0.6) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#E74C3C", alpha = 0.6) +
  geom_hline(yintercept = -20, linetype = "dashed", color = "#3498DB", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#34495E", alpha = 0.4) +
  labs(
    x = "Time (seconds)",
    y = "Rate of Change (g/s)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

p2_combined <- grid.arrange(p2a, p2b, nrow = 2, heights = c(2, 1))

ggsave(file.path(OUTPUT_PATH, "v4_zoomed_with_rate.png"), 
       p2_combined, width = 12, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# VISUALIZATION 3: v3 vs v4 Comparison
# ==============================================================================

cat("Creating visualization 3: v3 vs v4 comparison...\n")

# Select same zoom window for comparison
data_v3_zoom <- data_v3 %>% 
  filter(elapsed_sec >= zoom_start, elapsed_sec <= zoom_end) %>%
  mutate(version = "v3.0")

data_v4_zoom <- data_v4 %>% 
  filter(elapsed_sec >= zoom_start, elapsed_sec <= zoom_end) %>%
  mutate(
    version = "v4.0",
    is_stable_v4 = is_stable
  )

p3a <- ggplot(data_v3_zoom, aes(x = elapsed_sec, y = load_g)) +
  geom_line(color = "#95A5A6", linewidth = 0.4, alpha = 0.5) +
  geom_line(data = data_v3_zoom %>% filter(is_stable == TRUE),
            aes(x = elapsed_sec, y = load_g), 
            color = "#2ECC71", linewidth = 0.8) +
  labs(
    title = "v3.0: Point-to-point change < 2g",
    x = "Time (seconds)",
    y = "Pressure (g)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = comma)

p3b <- ggplot(data_v4_zoom, aes(x = elapsed_sec, y = load_g, color = region)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "v4.0: Rate-based ramp detection + stable validation",
    x = "Time (seconds)",
    y = "Pressure (g)",
    color = "Region"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  scale_y_continuous(labels = comma)

p3_combined <- grid.arrange(p3a, p3b, nrow = 2)

ggsave(file.path(OUTPUT_PATH, "v3_vs_v4_comparison.png"), 
       p3_combined, width = 12, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# VISUALIZATION 4: Stable Regions Only (Both Versions)
# ==============================================================================

cat("Creating visualization 4: Stable regions comparison...\n")

stable_v3 <- data_v3 %>% filter(is_stable == TRUE)
stable_v4 <- data_v4 %>% filter(is_stable == TRUE)

p4 <- ggplot() +
  geom_point(data = stable_v3, aes(x = elapsed_sec, y = load_g, color = "v3.0"),
             size = 0.5, alpha = 0.5) +
  geom_point(data = stable_v4, aes(x = elapsed_sec, y = load_g, color = "v4.0"),
             size = 0.5, alpha = 0.5) +
  scale_color_manual(
    values = c("v3.0" = "#E67E22", "v4.0" = "#2ECC71"),
    name = "Version"
  ) +
  labs(
    title = paste(TEST_SAMPLE, "- Stable Regions: v3.0 vs v4.0"),
    subtitle = sprintf("v3.0: %.1f%% stable | v4.0: %.1f%% stable",
                       stats_v3$percent_time_stable, stats_v4$percent_time_stable),
    x = "Time (seconds)",
    y = "Pressure (g)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma)

ggsave(file.path(OUTPUT_PATH, "stable_regions_comparison.png"), 
       p4, width = 12, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# SAVE STATISTICS
# ==============================================================================

cat("Saving statistics...\n")

write_csv(comparison, file.path(OUTPUT_PATH, "comparison_statistics.csv"))
write_csv(stats_v3, file.path(OUTPUT_PATH, "stats_v3.csv"))
write_csv(stats_v4, file.path(OUTPUT_PATH, "stats_v4.csv"))

# Save detailed data
write_csv(data_v4 %>% select(elapsed_sec, load_g, pass_id, region, is_stable, 
                              rate_smooth, point_change),
          file.path(OUTPUT_PATH, "detailed_data_v4.csv"))

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("VALIDATION COMPLETE\n")
cat("==============================================================================\n\n")
cat("Key Findings:\n")
if (!is.null(stats_v3)) {
  cat(sprintf("  - v3.0 detected %.1f%% stable time\n", stats_v3$percent_time_stable))
  cat(sprintf("  - v4.0 detected %.1f%% stable time\n", stats_v4$percent_time_stable))
  cat(sprintf("  - Difference: %.1f percentage points\n", 
              stats_v4$percent_time_stable - stats_v3$percent_time_stable))
} else {
  cat("  - v3.0 statistics unavailable\n")
  cat(sprintf("  - v4.0 detected %.1f%% stable time\n", stats_v4$percent_time_stable))
}
cat("\n")

cat("Outputs saved to:", OUTPUT_PATH, "\n")
cat("  - v4_full_trace_regions.png\n")
cat("  - v4_zoomed_with_rate.png\n")
cat("  - v3_vs_v4_comparison.png\n")
cat("  - stable_regions_comparison.png\n")
cat("  - comparison_statistics.csv\n")
cat("  - stats_v3.csv\n")
cat("  - stats_v4.csv\n")
cat("  - detailed_data_v4.csv\n")
cat("\n")

cat("==============================================================================\n")
