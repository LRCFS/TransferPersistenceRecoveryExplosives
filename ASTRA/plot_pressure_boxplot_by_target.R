# Actual Achieved Pressure by Nominal Target -- Box Plot -- Pilot + Main Study
# ===============================================================================
# Box plot of each sample's own ACTUAL achieved mass (Mean_Pressure, from the
# ASTRA pressure-trace pipeline's batch_summary.csv) grouped by its NOMINAL
# target level (10/50/100/200/300g) -- i.e. "how well did the ASTRA device
# actually hit each target, across the whole pooled dataset?" Companion to
# plot_combined_qc_overview.R's dumbbell-style QC overview (same underlying
# data/sample-selection), shown here as a box plot instead so the overall
# SPREAD at each nominal level is the primary read, rather than every
# individual sample's own deviation.
#
# Sample selection: identical to plot_combined_qc_overview.R/
# plot_combined_traces_50_200.R -- exactly the samples pooled into
# ASTRA/doe/main_study_analysis.R's own combined model:
#   - Main Study:  SampleType == "Main", analysis_accepted %in% c("PASS","PASS*")
#   - Pilot Study: SampleType == "Pilot", Solvent_level == "present" (wet-only),
#                  Pressure_g %in% c(50, 200), analysis_accepted %in% c("PASS","PASS*")
#
# Styling matches the other combined ASTRA figures' shared colour/marker
# language: colour = Surface_type (blue Steel/orange ABS), shape = Study
# (hollow = Pilot, solid = Main), mean diamond = pal_surface_type_mean
# (lighter) fill + black border, boxplot outline fixed black, outliers drawn
# as Surface-coloured squares.
#
# Faceted by nominal Target (one panel per 10/50/100/200/300g level, free
# y-scale per panel -- matches plot_combined_qc_overview.R's own
# facet_wrap(~Target, scales="free") convention) rather than a single shared
# continuous mass axis spanning the whole 10-300g range -- a shared axis
# compresses the low-target panels' real spread down to a few pixels and
# makes the actual per-level detail unreadable (updated 2026-09-10, per
# explicit feedback that the original continuous-axis version "isn't
# useful"). Surface_type is the x-axis WITHIN each panel instead. A dashed
# horizontal line + shaded band mark that panel's own Target +/-15%
# (matching plot_combined_qc_overview.R's acceptance-band convention).
#
# Does NOT modify main_study_batch_process.R/
# time_based_batch_process_adaptive_DISTANCE.R -- reads each study's own
# already-computed Pressure Traces/ProcessedData/batch_summary.csv directly.
#
# Output:
#   - Main Study/Pressure_Boxplot_ByTarget_PilotMain.png
#
# Author: OpenCode
# Date: 2026-09-10

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# ===============================================================================
# CONFIGURATION
# ===============================================================================

ASTRA_SWABBING_DIR <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing"
MAIN_DIR  <- file.path(ASTRA_SWABBING_DIR, "Main Study")
PILOT_DIR <- file.path(ASTRA_SWABBING_DIR, "Pilot Study")

MAIN_BATCH_SUMMARY_FILE  <- file.path(MAIN_DIR, "Pressure Traces/ProcessedData/batch_summary.csv")
MAIN_METADATA_FILE       <- file.path(MAIN_DIR, "main_study_data_nested.csv")

PILOT_BATCH_SUMMARY_FILE <- file.path(PILOT_DIR, "Pressure Traces/ProcessedData/batch_summary.csv")
PILOT_METADATA_FILE      <- file.path(PILOT_DIR, "pilot_data_nested.csv")

OUTPUT_FILE <- file.path(MAIN_DIR, "Pressure_Boxplot_ByTarget_PilotMain.png")

# Box width for the (categorical, Surface_type) x-axis within each panel.
BOX_WIDTH <- 0.5

# Jitter half-width, in categorical x-axis units, for individual points.
JITTER_WIDTH <- 0.15

# Acceptance band, matching plot_combined_qc_overview.R's own
# mean_pressure_outlier_pct.
OUTLIER_PCT <- 0.15

set.seed(20260910)  # reproducible jitter

# ===============================================================================
# LOAD + FILTER TO EXACTLY THE POOLED SAMPLE SET
# ===============================================================================

for (f in c(MAIN_BATCH_SUMMARY_FILE, MAIN_METADATA_FILE, PILOT_BATCH_SUMMARY_FILE, PILOT_METADATA_FILE)) {
  if (!file.exists(f)) stop(sprintf("Required input file not found: %s", f))
}

main_batch <- read.csv(MAIN_BATCH_SUMMARY_FILE, stringsAsFactors = FALSE)
main_meta  <- read.csv(MAIN_METADATA_FILE, stringsAsFactors = FALSE)

pilot_batch <- read.csv(PILOT_BATCH_SUMMARY_FILE, stringsAsFactors = FALSE)
pilot_meta  <- read.csv(PILOT_METADATA_FILE, stringsAsFactors = FALSE)

main_eligible <- main_meta %>%
  filter(SampleType == "Main", analysis_accepted %in% c("PASS", "PASS*")) %>%
  transmute(RunID, Surface_type, Study = "Main")

pilot_eligible <- pilot_meta %>%
  filter(SampleType == "Pilot", Solvent_level == "present", Pressure_g %in% c(50, 200),
         analysis_accepted %in% c("PASS", "PASS*")) %>%
  transmute(RunID, Surface_type, Study = "Pilot")

eligible <- bind_rows(main_eligible, pilot_eligible)

main_pooled <- main_batch %>%
  filter(Sample %in% eligible$RunID, Success == TRUE, !is.na(Mean_Pressure)) %>%
  transmute(RunID = Sample, Target, Mean_Pressure)

pilot_pooled <- pilot_batch %>%
  filter(Sample %in% eligible$RunID, Success == TRUE, !is.na(Mean_Pressure)) %>%
  transmute(RunID = Sample, Target, Mean_Pressure)

plot_data <- bind_rows(main_pooled, pilot_pooled) %>%
  left_join(eligible, by = "RunID") %>%
  mutate(
    Surface_type = factor(Surface_type, levels = c("steel", "abs")),
    Study = factor(Study, levels = c("Pilot", "Main"))
  )

cat(sprintf("Combined pooled samples with a valid Mean_Pressure: %d (%d Main, %d Pilot)\n",
            nrow(plot_data), sum(plot_data$Study == "Main"), sum(plot_data$Study == "Pilot")))
cat("Samples per nominal Target level:\n")
print(table(plot_data$Target))

target_levels <- sort(unique(plot_data$Target))

# ===============================================================================
# OUTLIER FLAG (1.5xIQR rule, per Target x Surface_type group -- matches
# plot_recovery_boxplot_actualpressure.R's own outlier convention) + jitter
# ===============================================================================

plot_data <- plot_data %>%
  group_by(Surface_type, Target) %>%
  mutate(
    q1 = quantile(Mean_Pressure, 0.25, type = 7),
    q3 = quantile(Mean_Pressure, 0.75, type = 7),
    iqr = q3 - q1,
    is_outlier = Mean_Pressure < (q1 - 1.5 * iqr) | Mean_Pressure > (q3 + 1.5 * iqr),
    n_group = n()
  ) %>%
  ungroup() %>%
  mutate(
    Surface_pos = as.numeric(Surface_type),  # 1 = steel, 2 = abs
    x_plot = Surface_pos + runif(n(), -JITTER_WIDTH, JITTER_WIDTH)
  )

n_outliers <- sum(plot_data$is_outlier)
cat(sprintf("Flagged %d of %d points as boxplot outliers (1.5xIQR rule, drawn as Surface-coloured squares)\n",
            n_outliers, nrow(plot_data)))

points_to_plot <- plot_data %>% filter(!is_outlier)
outlier_points <- plot_data %>% filter(is_outlier)

mean_points <- plot_data %>%
  group_by(Surface_type, Surface_pos, Target) %>%
  summarise(mean_pressure = mean(Mean_Pressure, na.rm = TRUE), .groups = "drop")

# Per-panel Target reference line + shaded +/-15% acceptance band (matches
# plot_combined_qc_overview.R's own band convention).
target_df <- data.frame(Target = target_levels)
band_df <- data.frame(
  Target = target_levels,
  Lower = target_levels * (1 - OUTLIER_PCT),
  Upper = target_levels * (1 + OUTLIER_PCT)
)

# ===============================================================================
# PLOT
# ===============================================================================

p <- ggplot() +
  geom_rect(data = band_df, aes(ymin = Lower, ymax = Upper), xmin = -Inf, xmax = Inf,
            fill = "gray85", alpha = 0.6, inherit.aes = FALSE) +
  geom_hline(data = target_df, aes(yintercept = Target), linetype = "dashed", color = "gray30") +
  geom_boxplot(
    data = plot_data,
    aes(x = Surface_pos, y = Mean_Pressure, group = Surface_type),
    width = BOX_WIDTH, outlier.shape = NA, fill = NA, color = "black", linewidth = 0.5
  ) +
  geom_point(
    data = points_to_plot,
    aes(x = x_plot, y = Mean_Pressure, color = Surface_type, shape = Study),
    size = 2.5, alpha = 0.85
  ) +
  { if (nrow(outlier_points) > 0) {
      geom_point(
        data = outlier_points,
        aes(x = x_plot, y = Mean_Pressure, color = Surface_type, shape = "Outlier (>1.5xIQR)"),
        size = 3, inherit.aes = FALSE
      )
    } else NULL } +
  geom_point(
    data = mean_points,
    aes(x = Surface_pos, y = mean_pressure, fill = Surface_type),
    shape = 23, color = "black", stroke = 0.6, size = 3.5
  ) +
  scale_color_manual(values = pal_surface_type, labels = c(steel = "Steel", abs = "ABS"), name = "Surface") +
  scale_fill_manual(values = pal_surface_type_mean, guide = "none") +
  scale_shape_manual(
    values = c(Pilot = 1, Main = 19, "Outlier (>1.5xIQR)" = 15),
    breaks = c("Pilot", "Main", "Outlier (>1.5xIQR)"),
    name = "Marker"
  ) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Steel", "ABS"), limits = c(0.5, 2.5)) +
  facet_wrap(~ Target, scales = "free_y", labeller = labeller(Target = function(x) paste0(x, "g target"))) +
  labs(
    title = sprintf("Actual Achieved Mass by Nominal Target -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = paste0(
      "Box (black) = IQR/median of actual achieved mass, per Surface within each nominal-target panel | Circles = individual samples (hollow = Pilot, solid = Main)\n",
      "Dashed line = Target | Shaded band = within 15% of Target | Diamonds (lighter fill) = group mean | Squares = boxplot outlier (1.5xIQR)\n",
      "Only samples pooled into main_study_analysis.R's combined model (Pilot restricted to QC-passed wet-only 50g/200g)"
    ),
    x = NULL,
    y = "Actual Achieved Mass (Mean_Pressure, stable regions, g)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 8.5),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

pre_write_mtime <- if (file.exists(OUTPUT_FILE)) file.info(OUTPUT_FILE)$mtime else as.POSIXct(-Inf)

ggsave(OUTPUT_FILE, p, width = 10, height = 8, dpi = 300)

if (file.exists(OUTPUT_FILE) && file.info(OUTPUT_FILE)$mtime > pre_write_mtime) {
  cat(sprintf("\nSaved: %s\n", OUTPUT_FILE))
} else {
  warning("ggsave() returned normally but ", OUTPUT_FILE,
          " does not appear to have just been written -- check for a file lock (e.g. OneDrive sync) and re-run.")
}

cat("\nDone.\n")
