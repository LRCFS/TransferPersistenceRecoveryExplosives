# Recovery Boxplot vs Nominal Pressure -- Points Positioned by Actual Pressure
# ===============================================================================
# Combines two prior visualisations into one figure:
#   1. A boxplot of recovery by NOMINAL pressure level (categorical x-axis:
#      10/50/100/200/300g), faceted by Analyte (columns: PETN, RDX) x Surface
#      (rows: Steel, ABS), pooling Pilot + Main Study samples per box - same
#      structure as the existing "Recovery by Pressure -- Pilot + Main Study"
#      preliminary figure.
#   2. The ACTUAL measured pressure (Mean_Pressure, from the ASTRA pressure-
#      trace pipeline) used to position each point's location *within* its
#      box's column, instead of a random jitter. Points are linearly spaced
#      between the box's left/right edges according to where their own real
#      achieved pressure falls within that group's real-pressure range - so
#      a point plotted further right genuinely achieved a higher pressure
#      than one plotted further left, even though both share the same
#      nominal/target pressure category.
#   3. Points that are statistical outliers in the underlying boxplot (using
#      the standard 1.5*IQR rule, computed per Surface x Analyte x nominal
#      Pressure group, pooling both studies) are drawn as hollow circles
#      (colored border, no fill) instead of solid, so they remain visually
#      distinct without needing a separate legend entry.
#
# Data sources: same as plot_main_study_recovery_vs_pressure.R (this script
# is self-contained and does not depend on it having been run first, per
# the ASTRA repo convention of single-file, non-sourcing scripts).
#
# Output:
#   - Main Study/Recovery_Boxplot_ActualPressure_PilotMain.png
#
# Author: OpenCode
# Date: 2026-08-25

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
})

# ===============================================================================
# CONFIGURATION
# ===============================================================================

ASTRA_SWABBING_DIR <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing"
BASE_DIR <- file.path(ASTRA_SWABBING_DIR, "Main Study")
PILOT_DIR <- file.path(ASTRA_SWABBING_DIR, "Pilot Study")

PRESSURE_SUMMARY_FILE <- file.path(BASE_DIR, "Pressure Traces/ProcessedData/batch_summary.csv")
RECOVERY_FILE <- file.path(BASE_DIR, "main_study_data_nested.csv")

PILOT_PRESSURE_SUMMARY_FILE <- file.path(PILOT_DIR, "Pressure Traces/ProcessedData/batch_summary.csv")
PILOT_RECOVERY_FILE <- file.path(PILOT_DIR, "pilot_data_nested.csv")

OUTPUT_PLOT_FILE <- file.path(BASE_DIR, "Recovery_Boxplot_ActualPressure_PilotMain.png")

# Half-width (in factor-position units) that points can spread across within
# their box's column, driven by their real Mean_Pressure value. The boxplot
# itself is drawn at width = 0.6 (below), so 0.3 keeps points within/at the
# box edges rather than overflowing into the neighbouring category.
POINT_SPREAD_HALFWIDTH <- 0.3

# ===============================================================================
# LOAD DATA
# ===============================================================================

for (f in c(PRESSURE_SUMMARY_FILE, RECOVERY_FILE, PILOT_PRESSURE_SUMMARY_FILE, PILOT_RECOVERY_FILE)) {
  if (!file.exists(f)) stop(sprintf("Required input file not found: %s", f))
}

pressure_data <- read.csv(PRESSURE_SUMMARY_FILE, stringsAsFactors = FALSE)
recovery_data <- read.csv(RECOVERY_FILE, stringsAsFactors = FALSE)
pilot_pressure_data <- read.csv(PILOT_PRESSURE_SUMMARY_FILE, stringsAsFactors = FALSE)
pilot_recovery_data <- read.csv(PILOT_RECOVERY_FILE, stringsAsFactors = FALSE)

cat(sprintf("Loaded %d Main pressure rows, %d Main recovery rows, %d Pilot pressure rows, %d Pilot recovery rows\n",
            nrow(pressure_data), nrow(recovery_data), nrow(pilot_pressure_data), nrow(pilot_recovery_data)))

# ===============================================================================
# FILTER + JOIN (same logic as plot_main_study_recovery_vs_pressure.R)
# ===============================================================================
# Main Study: drop NC rows (SampleType == "NC", no pressure trace exists).
# Pilot Study: drop NC rows AND restrict to wet (Solvent_level == "present")
# samples only, to match the Main Study's all-wet design - the Pilot Study
# also ran dry replicates at every level, not comparable here.

recovery_main <- recovery_data %>% filter(SampleType == "Main")
recovery_pilot <- pilot_recovery_data %>% filter(SampleType == "Pilot", Solvent_level == "present")

joined_main <- recovery_main %>%
  left_join(
    pressure_data %>% select(Sample, Target, Mean_Pressure),
    by = c("RunID" = "Sample")
  ) %>%
  mutate(Study = "Main")

joined_pilot <- recovery_pilot %>%
  left_join(
    pilot_pressure_data %>% select(Sample, Target, Mean_Pressure),
    by = c("RunID" = "Sample")
  ) %>%
  mutate(Study = "Pilot")

common_cols <- c("RunID", "Surface_type", "Study", "Target", "Mean_Pressure",
                  "PETN_Recovery_pct", "RDX_Recovery_pct")

joined <- bind_rows(
  joined_main %>% select(all_of(common_cols)),
  joined_pilot %>% select(all_of(common_cols))
)

cat(sprintf("Combined Main + Pilot joined dataset: %d rows (%d Main, %d Pilot)\n",
            nrow(joined), sum(joined$Study == "Main"), sum(joined$Study == "Pilot")))

# ===============================================================================
# RESHAPE TO LONG FORMAT AND DROP UNMATCHED / MISSING ROWS
# ===============================================================================

plot_data <- joined %>%
  select(RunID, Surface_type, Study, Target, Mean_Pressure, PETN_Recovery_pct, RDX_Recovery_pct) %>%
  pivot_longer(
    cols = c(PETN_Recovery_pct, RDX_Recovery_pct),
    names_to = "Analyte",
    values_to = "Recovery_pct"
  ) %>%
  mutate(Analyte = recode(Analyte, PETN_Recovery_pct = "PETN", RDX_Recovery_pct = "RDX")) %>%
  filter(!is.na(Mean_Pressure), !is.na(Recovery_pct))

cat(sprintf("Total sample x analyte points available for plotting: %d\n", nrow(plot_data)))

# ===============================================================================
# FACTOR ORDERING (Surface rows: Steel top, ABS bottom; Analyte cols: PETN, RDX;
# Pressure: ascending nominal levels, evenly spaced categorical positions)
# ===============================================================================

target_levels <- sort(unique(plot_data$Target))

plot_data <- plot_data %>%
  mutate(
    Surface_label = factor(ifelse(Surface_type == "steel", "Steel", "ABS"), levels = c("Steel", "ABS")),
    Analyte = factor(Analyte, levels = c("PETN", "RDX")),
    Target_factor = factor(Target, levels = target_levels),
    Target_pos = as.numeric(Target_factor)   # integer box-center position (1..5)
  )

# ===============================================================================
# PER-GROUP STATS: outlier flag (1.5*IQR rule) and actual-pressure-driven
# x-offset within each Surface x Analyte x nominal-Pressure group
# ===============================================================================
# Outlier rule matches ggplot2's own default boxplot outlier definition
# (below Q1 - 1.5*IQR or above Q3 + 1.5*IQR, using type-7 quantiles).
# x-offset: linearly rescales each point's own Mean_Pressure into
# [-POINT_SPREAD_HALFWIDTH, +POINT_SPREAD_HALFWIDTH] using the min/max
# Mean_Pressure actually observed *within that group* - so the point
# ordering left-to-right always reflects real achieved pressure ordering,
# not a random draw. Groups with only 1 point, or where every point in the
# group achieved identical pressure (range = 0), get offset 0 (centered).

plot_data <- plot_data %>%
  group_by(Surface_label, Analyte, Target_factor) %>%
  mutate(
    q1 = quantile(Recovery_pct, 0.25, type = 7),
    q3 = quantile(Recovery_pct, 0.75, type = 7),
    iqr = q3 - q1,
    is_outlier = Recovery_pct < (q1 - 1.5 * iqr) | Recovery_pct > (q3 + 1.5 * iqr),
    pressure_range = max(Mean_Pressure) - min(Mean_Pressure),
    x_offset = ifelse(
      n() > 1 & pressure_range > 0,
      as.numeric(scales::rescale(Mean_Pressure, to = c(-POINT_SPREAD_HALFWIDTH, POINT_SPREAD_HALFWIDTH))),
      0
    ),
    n_group = n()
  ) %>%
  ungroup() %>%
  mutate(
    x_plot = Target_pos + x_offset,
    # Hollow (hex NA) fill for outliers, solid Study-colored fill otherwise;
    # border color always shows Study, so outliers remain identifiable by
    # which study they came from even though they're unfilled.
    fill_value = ifelse(is_outlier, NA_character_, Study)
  )

n_outliers <- sum(plot_data$is_outlier)
cat(sprintf("Flagged %d of %d points as boxplot outliers (1.5xIQR rule, drawn as hollow circles)\n",
            n_outliers, nrow(plot_data)))

# ===============================================================================
# n= LABEL DATA (one per Surface x Analyte x Target group, pooled n)
# ===============================================================================

label_y <- max(plot_data$Recovery_pct, na.rm = TRUE) * 1.18

n_labels <- plot_data %>%
  distinct(Surface_label, Analyte, Target_factor, Target_pos, n_group) %>%
  mutate(label = sprintf("n=%d", n_group), y = label_y)

# ===============================================================================
# MEAN DIAMOND DATA (one per Surface x Analyte x Target group, pooled mean,
# plotted at the exact box center - no pressure-driven offset)
# ===============================================================================

mean_points <- plot_data %>%
  group_by(Surface_label, Analyte, Target_factor, Target_pos) %>%
  summarise(mean_recovery = mean(Recovery_pct, na.rm = TRUE), .groups = "drop")

# ===============================================================================
# PLOT
# ===============================================================================

y_max <- label_y * 1.05

p <- ggplot() +
  geom_boxplot(
    data = plot_data,
    aes(x = Target_factor, y = Recovery_pct),
    width = 0.6, outlier.shape = NA, fill = NA, color = "black", linewidth = 0.5
  ) +
  geom_point(
    data = plot_data,
    aes(x = x_plot, y = Recovery_pct, fill = fill_value, color = Study),
    shape = 21, size = 2.5, stroke = 0.9
  ) +
  geom_point(
    data = mean_points,
    aes(x = Target_pos, y = mean_recovery),
    shape = 23, fill = "red", color = "black", size = 3
  ) +
  geom_text(
    data = n_labels,
    aes(x = Target_pos, y = y, label = label),
    size = 3.2, color = "black"
  ) +
  scale_fill_manual(values = c("Main" = "#4477AA", "Pilot" = "#EE9944"), na.value = NA, guide = "none") +
  scale_color_manual(values = c("Main" = "#4477AA", "Pilot" = "#EE9944")) +
  scale_x_discrete(labels = as.character(target_levels)) +
  coord_cartesian(ylim = c(min(0, min(plot_data$Recovery_pct, na.rm = TRUE) * 1.05), y_max)) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery by Pressure -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = paste0(
      "Box = IQR/median across pooled Pilot+Main samples | Point x-position = actual achieved pressure within each nominal-pressure group (not jittered)\n",
      "Hollow circle = boxplot outlier (1.5xIQR) | Red diamond = mean"
    ),
    x = "Nominal Pressure (g)",
    y = "Recovery (%)",
    color = "Study"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 9.5),
    strip.background = element_rect(fill = "gray85", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(0.15, "lines"),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(fill = c("#4477AA", "#EE9944"), shape = 21, size = 3)))

ggsave(OUTPUT_PLOT_FILE, p, width = 13, height = 8.3, dpi = 300)
cat(sprintf("\nPlot saved to: %s\n", OUTPUT_PLOT_FILE))

# ===============================================================================
# CONSOLE SUMMARY: outlier points (for traceability)
# ===============================================================================

if (n_outliers > 0) {
  cat("\n=== Outlier points (hollow circles in plot) ===\n")
  outlier_summary <- plot_data %>%
    filter(is_outlier) %>%
    select(RunID, Study, Surface_label, Analyte, Target, Mean_Pressure, Recovery_pct) %>%
    arrange(Surface_label, Analyte, Target)
  print(as.data.frame(outlier_summary), row.names = FALSE)
}

cat("\nDone.\n")
