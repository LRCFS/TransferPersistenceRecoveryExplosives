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
#   4. Samples with a recovery value but NO matching pressure-trace record
#      (added 2026-09-02 -- previously these were silently DROPPED from this
#      plot entirely) are now INCLUDED, positioned at their nominal pressure's
#      box centre (no real-position information to place them by) and drawn
#      as a hollow TRIANGLE instead of a circle, so they remain visually
#      distinct from -- not confused with -- the real-position points.
#      Currently 2 such samples (both PILOT_ ABS at 200g) -- see the
#      console summary this script prints for the current, authoritative
#      list. The nominal pressure itself comes from Pressure_g in each
#      study's own recovery data file directly (always present), NOT from
#      the pressure-trace join's own Target column (which is NA exactly
#      when Mean_Pressure is NA, since both come from the same join).
#
# Data sources: same as the archived plot_main_study_recovery_vs_pressure.R
# (this script is self-contained and does not depend on it having been run
# first, per the ASTRA repo convention of single-file, non-sourcing scripts).
#
# Output:
#   - Main Study/Recovery_Boxplot_ActualPressure_PilotMain.png
#
# Author: OpenCode
# Date: 2026-08-25 (updated 2026-09-02 -- see item 4 above)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
})

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot in this repo. See thesis_palette.R's own
# header for the full rationale.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

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

common_cols <- c("RunID", "Surface_type", "Study", "Pressure_g", "Target", "Mean_Pressure",
                  "PETN_Recovery_pct", "RDX_Recovery_pct")

joined <- bind_rows(
  joined_main %>% select(all_of(common_cols)),
  joined_pilot %>% select(all_of(common_cols))
)

cat(sprintf("Combined Main + Pilot joined dataset: %d rows (%d Main, %d Pilot)\n",
            nrow(joined), sum(joined$Study == "Main"), sum(joined$Study == "Pilot")))

# ===============================================================================
# RESHAPE TO LONG FORMAT
# ===============================================================================
# NOTE (2026-09-02): previously also dropped !is.na(Mean_Pressure) here,
# silently excluding any sample lacking a pressure-trace match entirely.
# Now kept (see item 4 in the header comment) -- only a genuinely missing
# recovery value drops a row.

plot_data <- joined %>%
  select(RunID, Surface_type, Study, Pressure_g, Mean_Pressure, PETN_Recovery_pct, RDX_Recovery_pct) %>%
  pivot_longer(
    cols = c(PETN_Recovery_pct, RDX_Recovery_pct),
    names_to = "Analyte",
    values_to = "Recovery_pct"
  ) %>%
  mutate(
    Analyte = recode(Analyte, PETN_Recovery_pct = "PETN", RDX_Recovery_pct = "RDX"),
    has_actual_pressure = !is.na(Mean_Pressure)
  ) %>%
  filter(!is.na(Recovery_pct))

cat(sprintf("Total sample x analyte points available for plotting: %d\n", nrow(plot_data)))
n_missing_pressure <- sum(!plot_data$has_actual_pressure)
if (n_missing_pressure > 0) {
  cat(sprintf("  Of which %d point(s) have NO pressure-trace match -- ",
              n_missing_pressure))
  cat("plotted at nominal-pressure box centre, hollow TRIANGLE marker (see console summary below).\n")
}

# ===============================================================================
# FACTOR ORDERING (Surface rows: Steel top, ABS bottom; Analyte cols: PETN, RDX;
# Pressure: ascending nominal levels, evenly spaced categorical positions)
# ===============================================================================
# Uses Pressure_g (each study's own recovery-data column, always present)
# rather than Target (which comes from the pressure-trace join and is NA
# exactly when Mean_Pressure is NA -- i.e. useless for the samples this
# script now needs to still place on the x-axis).

target_levels <- sort(unique(plot_data$Pressure_g))

plot_data <- plot_data %>%
  mutate(
    Surface_label = factor(ifelse(Surface_type == "steel", "Steel", "ABS"), levels = c("Steel", "ABS")),
    Analyte = factor(Analyte, levels = c("PETN", "RDX")),
    Target_factor = factor(Pressure_g, levels = target_levels),
    Target_pos = as.numeric(Target_factor)   # integer box-center position (1..5)
  )

# ===============================================================================
# PER-GROUP STATS: outlier flag (1.5*IQR rule) and actual-pressure-driven
# x-offset within each Surface x Analyte x nominal-Pressure group
# ===============================================================================
# Outlier rule matches ggplot2's own default boxplot outlier definition
# (below Q1 - 1.5*IQR or above Q3 + 1.5*IQR, using type-7 quantiles) --
# computed over ALL points in the group (with or without an actual-pressure
# match; a missing pressure record says nothing about whether the recovery
# value itself is an outlier).
#
# x-offset: linearly rescales each point's own Mean_Pressure into
# [-POINT_SPREAD_HALFWIDTH, +POINT_SPREAD_HALFWIDTH] using the min/max
# Mean_Pressure actually observed *within that group* - so the point
# ordering left-to-right always reflects real achieved pressure ordering,
# not a random draw. Groups with only 1 point with a real pressure value,
# or where every such point achieved identical pressure (range = 0), get
# offset 0 (centered) for those points.
#
# Points with NO actual-pressure match (has_actual_pressure == FALSE) always
# get offset 0 (box centre) -- there is no real position to place them at.
# If more than one such point falls in the same group (rare -- currently 2,
# both PILOT_ ABS at 200g), a small fixed nudge (+/- 0.08, well inside the
# box) spreads them apart so they don't render as a single overlapping
# point; this nudge carries NO pressure information, it exists purely so
# both points remain visible.

plot_data <- plot_data %>%
  group_by(Surface_label, Analyte, Target_factor) %>%
  mutate(
    q1 = quantile(Recovery_pct, 0.25, type = 7),
    q3 = quantile(Recovery_pct, 0.75, type = 7),
    iqr = q3 - q1,
    is_outlier = Recovery_pct < (q1 - 1.5 * iqr) | Recovery_pct > (q3 + 1.5 * iqr),
    n_with_pressure = sum(has_actual_pressure),
    pressure_range = if (sum(has_actual_pressure) > 1) diff(range(Mean_Pressure, na.rm = TRUE)) else 0,
    rescaled_pressure = as.numeric(suppressWarnings(
      scales::rescale(Mean_Pressure, to = c(-POINT_SPREAD_HALFWIDTH, POINT_SPREAD_HALFWIDTH))
    )),
    # Deterministic, non-overlapping nudge for missing-pressure points only,
    # applied via a running count of missing rows seen so far within the
    # group (1, 2, 3... in whatever row order they appear) -- NOT row_number()
    # directly, which would give each missing row its absolute position in
    # the whole group (e.g. 3 and 5), not its rank among JUST the missing
    # ones (1 and 2), breaking the centering below.
    missing_nudge_rank = ifelse(has_actual_pressure, NA_integer_, cumsum(!has_actual_pressure)),
    n_missing_in_group = sum(!has_actual_pressure),
    x_offset = case_when(
      has_actual_pressure & pressure_range > 0 & is.finite(rescaled_pressure) ~ rescaled_pressure,
      has_actual_pressure ~ 0,
      n_missing_in_group <= 1 ~ 0,
      TRUE ~ (missing_nudge_rank - mean(seq_len(n_missing_in_group[1]))) * 0.08
    ),
    n_group = n()
  ) %>%
  ungroup() %>%
  mutate(
    x_plot = Target_pos + x_offset,
    # Hollow (hex NA) fill for outliers OR missing-pressure points; solid
    # Study-colored fill otherwise. Border color always shows Study, so
    # both remain identifiable by which study they came from even when
    # unfilled.
    fill_value = ifelse(is_outlier | !has_actual_pressure, NA_character_, Study),
    pressure_source = factor(
      ifelse(has_actual_pressure, "Actual mass recorded", "No mass record (nominal position)"),
      levels = c("Actual mass recorded", "No mass record (nominal position)")
    )
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
    aes(x = x_plot, y = Recovery_pct, fill = fill_value, color = Study, shape = pressure_source),
    size = 2.5, stroke = 0.9
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
  scale_fill_manual(values = pal_study, na.value = NA, guide = "none") +
  scale_color_manual(values = pal_study) +
  scale_shape_manual(values = c("Actual mass recorded" = 21, "No mass record (nominal position)" = 24)) +
  scale_x_discrete(labels = as.character(target_levels)) +
  coord_cartesian(ylim = c(min(0, min(plot_data$Recovery_pct, na.rm = TRUE) * 1.05), y_max)) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery by Applied Mass -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = paste0(
      "Box = IQR/median across pooled Pilot+Main samples | Circle x-position = actual achieved mass within each nominal-mass group (not jittered)\n",
      "Hollow circle = boxplot outlier (1.5xIQR) | Triangle = no mass-trace record, plotted at nominal box centre | Red diamond = mean"
    ),
    x = "Nominal Applied Mass (g)",
    y = "Recovery (%)",
    color = "Study",
    shape = "Mass position"
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
  guides(
    color = guide_legend(override.aes = list(fill = c(pal_study[["Main"]], pal_study[["Pilot"]]), shape = 21, size = 3)),
    shape = guide_legend(override.aes = list(fill = "gray70", color = "black", size = 3))
  )

ggsave(OUTPUT_PLOT_FILE, p, width = 13, height = 8.3, dpi = 300)
cat(sprintf("\nPlot saved to: %s\n", OUTPUT_PLOT_FILE))

# ===============================================================================
# CONSOLE SUMMARY: outlier points + missing-pressure points (for traceability)
# ===============================================================================

if (n_outliers > 0) {
  cat("\n=== Outlier points (hollow circles in plot) ===\n")
  outlier_summary <- plot_data %>%
    filter(is_outlier) %>%
    select(RunID, Study, Surface_label, Analyte, Pressure_g, Mean_Pressure, Recovery_pct) %>%
    arrange(Surface_label, Analyte, Pressure_g)
  print(as.data.frame(outlier_summary), row.names = FALSE)
}

if (n_missing_pressure > 0) {
  cat("\n=== Points with NO pressure-trace match (hollow triangles, plotted at nominal box centre) ===\n")
  missing_summary <- plot_data %>%
    filter(!has_actual_pressure) %>%
    select(RunID, Study, Surface_label, Analyte, Pressure_g, Recovery_pct) %>%
    arrange(Surface_label, Analyte, Pressure_g)
  print(as.data.frame(missing_summary), row.names = FALSE)
}

cat("\nDone.\n")
