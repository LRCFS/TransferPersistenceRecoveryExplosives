# Recovery Boxplot vs Nominal Pressure -- Points Positioned by Actual Pressure
# ===============================================================================
# Combines two prior visualisations into one figure:
#   1. A boxplot of recovery by NOMINAL pressure level (x-axis: 10/50/100/
#      200/300g), faceted by Surface (rows: Steel, ABS) x Analyte (columns:
#      PETN, RDX), pooling Pilot + Main Study samples per box.
#   2. The ACTUAL measured pressure (Mean_Pressure, from the ASTRA pressure-
#      trace pipeline) used to position each point's location *within* its
#      own box, instead of a random jitter. Points are linearly spaced
#      between the box's left/right edges according to where their own real
#      achieved pressure falls within that group's real-pressure range - so
#      a point plotted further right genuinely achieved a higher pressure
#      than one plotted further left, even though both share the same
#      nominal/target pressure category.
#   3. Points that are statistical outliers in the underlying boxplot (using
#      the standard 1.5*IQR rule, computed per Surface x Analyte x nominal
#      Pressure group, pooling both studies) are drawn as a SQUARE instead
#      of a circle, still coloured by Surface_type (blue Steel / orange
#      ABS), so they remain visually distinct without losing the Surface
#      colour encoding or needing a separate legend entry beyond the
#      "Marker" shape key added for this purpose (updated 2026-09-10 --
#      previously an extra black ring drawn on top of a normal circle
#      point, then a plain black square).
#   4. Samples with a recovery value but NO matching pressure-trace record
#      (2 currently, both PILOT_ ABS at 200g) are EXCLUDED from the point
#      layer entirely (updated 2026-09-10 -- previously drawn as a hollow
#      triangle at the box centre; now matches the categorical hinge plot's
#      own convention of excluding position-less points from its scatter
#      layer). They are NOT excluded from the box/whisker statistics
#      (quartiles are computed from every sample in the group regardless of
#      whether it has a real position to plot at) -- only the individual
#      dot for that sample is omitted. See the console summary this script
#      prints for the current, authoritative list of which samples these
#      are. The nominal pressure itself comes from Pressure_g in each
#      study's own recovery data file directly (always present), NOT from
#      the pressure-trace join's own Target column (which is NA exactly
#      when Mean_Pressure is NA, since both come from the same join).
#   5. Styling (updated 2026-09-10) shares the same colour/marker language
#      as the categorical hinge plot (ASTRA/doe/main_study_analysis.R),
#      adapted for a faceted box plot rather than a single-panel scatter:
#        - Colour = Surface_type (pal_surface_type: Steel blue / ABS
#          orange) for the individual points and mean diamond -- Surface is
#          ALSO shown via facet rows (item 1 above), so this is intentional
#          double-encoding for visual consistency with the hinge plot, not
#          redundant information the reader needs to cross-reference.
#        - Shape = Study (hollow circle = Pilot, solid circle = Main) for
#          non-outlier points, same as the hinge plot.
#        - Mean diamond: pal_surface_type_mean (lighter tint) fill + black
#          border, same as the hinge plot.
#        - Box outline is a fixed BLACK (not Surface-coloured) -- keeps the
#          box/whisker structure visually distinct from the coloured
#          points sitting on top of it.
#        - No trend/fit line (this is a box plot, not a scatter+model fit --
#          nothing analogous to the hinge plot's dashed line is added here).
#        - No n= labels (removed 2026-09-10 -- were previously drawn above
#          each box).
#
# Data sources: same as the archived plot_main_study_recovery_vs_pressure.R
# (this script is self-contained and does not depend on it having been run
# first, per the ASTRA repo convention of single-file, non-sourcing scripts).
#
# Output:
#   - Main Study/Recovery_Boxplot_ActualPressure_PilotMain.png
#
# Author: OpenCode
# Date: 2026-08-25 (updated 2026-09-02, 2026-09-10 -- see items 3-5 above)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
})

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot in this repo. See thesis_palette.R's own
# header for the full rationale. Also provides pal_surface_type_mean (the
# lighter diamond-fill tint used below), shared with the categorical hinge
# plot so both figures use the exact same hex values.
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

# Box width, in GRAMS (x is now a true continuous mass axis -- see FACTOR
# ORDERING below -- not evenly-spaced categorical positions). Chosen to fit
# safely within the narrowest real gap between adjacent nominal levels
# (10g -> 50g = 40g apart), leaving clear separation from the neighbouring
# box.
BOX_WIDTH_G <- 14

# Half-width, in GRAMS, that points can spread across within their own box,
# driven by their real Mean_Pressure value -- kept comfortably inside
# BOX_WIDTH_G/2 (7g) so points never overflow into the neighbouring box.
POINT_SPREAD_HALFWIDTH <- 6

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
# QC-ACCEPTANCE FILTER (added Oct 2026 red-team review): this script
# previously had NO filter on analysis_accepted at all, unlike the other
# companion recovery-vs-pressure plotting scripts (plot_petn_vs_rdx_recovery.R,
# plot_pressure_boxplot_by_target.R, plot_combined_qc_overview.R,
# plot_combined_traces_50_200.R), which all correctly restrict to
# analysis_accepted %in% c("PASS","PASS*") -- matching the authoritative
# model in main_study_analysis.R's build_pooled_dataset(). This gap let a
# QC-failed sample's recovery value silently appear in this boxplot/outlier
# analysis (confirmed: MAIN_013 carried FAIL status with materially
# different recovery values before its reanalysis, and would have been
# fully visible here with no filter in place).

recovery_main <- recovery_data %>%
  filter(SampleType == "Main", analysis_accepted %in% c("PASS", "PASS*"))
recovery_pilot <- pilot_recovery_data %>%
  filter(SampleType == "Pilot", Solvent_level == "present",
         analysis_accepted %in% c("PASS", "PASS*"))

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
# recovery value drops a row. Rows with no pressure-trace match still
# contribute to the box/whisker statistics; only the individual-point layer
# excludes them (see item 4 above).

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
  cat("still counted in the box/whisker statistics, but excluded from the individual-point layer (see console summary below).\n")
}

# ===============================================================================
# FACTOR ORDERING (Surface rows: Steel top, ABS bottom; Analyte cols: PETN,
# RDX; Pressure: TRUE numeric mass axis -- boxes sit at their real 10/50/
# 100/200/300g positions, not evenly-spaced categorical slots. Updated
# 2026-09-10 per explicit request for "correct numeric spacing" -- the gaps
# between adjacent levels are genuinely 40/50/100/100g, not visually equal.)
# ===============================================================================
# Uses Pressure_g (each study's own recovery-data column, always present)
# rather than Target (which comes from the pressure-trace join and is NA
# exactly when Mean_Pressure is NA -- i.e. useless for the samples this
# script now needs to still place on the x-axis).

target_levels <- sort(unique(plot_data$Pressure_g))

plot_data <- plot_data %>%
  mutate(
    Surface_label = factor(ifelse(Surface_type == "steel", "Steel", "ABS"), levels = c("Steel", "ABS")),
    Analyte = factor(Analyte, levels = c("PETN", "RDX"))
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
# x-offset (in GRAMS): linearly rescales each point's own Mean_Pressure into
# [-POINT_SPREAD_HALFWIDTH, +POINT_SPREAD_HALFWIDTH] using the min/max
# Mean_Pressure actually observed *within that group* - so the point
# ordering left-to-right always reflects real achieved pressure ordering,
# not a random draw. Groups with only 1 point with a real pressure value,
# or where every such point achieved identical pressure (range = 0), get
# offset 0 (centered).
#
# Points with NO actual-pressure match (has_actual_pressure == FALSE) are
# excluded from the point layer entirely (see header item 4) -- their
# x-offset is irrelevant and left at 0.

plot_data <- plot_data %>%
  group_by(Surface_label, Analyte, Pressure_g) %>%
  mutate(
    q1 = quantile(Recovery_pct, 0.25, type = 7),
    q3 = quantile(Recovery_pct, 0.75, type = 7),
    iqr = q3 - q1,
    is_outlier = Recovery_pct < (q1 - 1.5 * iqr) | Recovery_pct > (q3 + 1.5 * iqr),
    pressure_range = if (sum(has_actual_pressure) > 1) diff(range(Mean_Pressure, na.rm = TRUE)) else 0,
    rescaled_pressure = as.numeric(suppressWarnings(
      scales::rescale(Mean_Pressure, to = c(-POINT_SPREAD_HALFWIDTH, POINT_SPREAD_HALFWIDTH))
    )),
    x_offset = case_when(
      has_actual_pressure & pressure_range > 0 & is.finite(rescaled_pressure) ~ rescaled_pressure,
      TRUE ~ 0
    ),
    n_group = n()
  ) %>%
  ungroup() %>%
  mutate(x_plot = Pressure_g + x_offset)

n_outliers <- sum(plot_data$is_outlier)
cat(sprintf("Flagged %d of %d points as boxplot outliers (1.5xIQR rule, drawn as Surface-coloured squares)\n",
            n_outliers, nrow(plot_data)))

# ===============================================================================
# MEAN DIAMOND DATA (one per Surface x Analyte x Target group, pooled mean,
# plotted at the box centre - no pressure-driven offset)
# ===============================================================================

mean_points <- plot_data %>%
  group_by(Surface_label, Surface_type, Analyte, Pressure_g) %>%
  summarise(mean_recovery = mean(Recovery_pct, na.rm = TRUE), .groups = "drop")

# ===============================================================================
# PLOT
# ===============================================================================
# Styling shares the categorical hinge plot's colour/marker language
# (ASTRA/doe/main_study_analysis.R): colour = Surface_type, shape = Study
# (hollow = Pilot, solid = Main), mean diamond = pal_surface_type_mean
# (lighter) fill + black border. Surface is ALSO shown via facet rows here
# (unlike the hinge plot, which has no facet-by-surface) -- see header item
# 5 for the full rationale. Box outline is a fixed black, not
# Surface-coloured (per explicit request).

y_max <- max(plot_data$Recovery_pct, na.rm = TRUE) * 1.15

points_to_plot <- plot_data %>% filter(has_actual_pressure, !is_outlier)
outlier_points <- plot_data %>% filter(has_actual_pressure, is_outlier)

p <- ggplot() +
  geom_boxplot(
    data = plot_data,
    aes(x = Pressure_g, y = Recovery_pct, group = Pressure_g),
    width = BOX_WIDTH_G, outlier.shape = NA, fill = NA, color = "black", linewidth = 0.5
  ) +
  geom_point(
    data = points_to_plot,
    aes(x = x_plot, y = Recovery_pct, color = Surface_type, shape = Study),
    size = 2.5, alpha = 0.85
  ) +
  { if (nrow(outlier_points) > 0) {
      # Outliers drawn as a SQUARE instead of the normal circle, still
      # coloured by Surface_type (blue Steel / orange ABS) rather than a
      # fixed black -- per explicit request. `shape` is mapped to a
      # constant label (not left as a fixed parameter) specifically so it
      # gets its own key in the shared shape legend below, alongside
      # Pilot/Main.
      geom_point(
        data = outlier_points,
        aes(x = x_plot, y = Recovery_pct, color = Surface_type, shape = "Outlier (>1.5xIQR)"),
        size = 3, inherit.aes = FALSE
      )
    } else NULL } +
  geom_point(
    data = mean_points,
    aes(x = Pressure_g, y = mean_recovery, fill = Surface_type),
    shape = 23, color = "black", stroke = 0.6, size = 3.5
  ) +
  scale_color_manual(values = pal_surface_type, labels = c(steel = "Steel", abs = "ABS"), name = "Surface") +
  scale_fill_manual(values = pal_surface_type_mean, guide = "none") +
  scale_shape_manual(
    values = c(Pilot = 1, Main = 19, "Outlier (>1.5xIQR)" = 15),
    breaks = c("Pilot", "Main", "Outlier (>1.5xIQR)"),
    name = "Marker"
  ) +
  scale_x_continuous(breaks = target_levels) +
  coord_cartesian(ylim = c(min(0, min(plot_data$Recovery_pct, na.rm = TRUE) * 1.05), y_max)) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery by Applied Mass -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = paste0(
      "Box (black) = IQR/median across pooled Pilot+Main samples | Circles = individual samples (hollow = Pilot, solid = Main), x-position = actual achieved mass within each box (not jittered)\n",
      "Diamonds (lighter fill) = group mean | ", n_missing_pressure, " sample(s) with no mass-trace record are excluded from the point layer (still counted in the box statistics) | see legend for outlier marker"
    ),
    x = "Effective Applied Mass (g)",
    y = "Recovery (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 8.5),
    strip.background = element_rect(fill = "gray85", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(0.15, "lines"),
    legend.position = "right"
  )

# Record the pre-write mtime (or -Inf if the file doesn't exist yet) BEFORE
# calling ggsave, so the post-write check below compares against a genuine
# "did this write actually happen just now" baseline rather than merely
# "is the file recent" -- the latter can give a false pass if two runs
# happen close together in time (caught in a prior session: a failed
# second write was wrongly reported as successful because the STALE file
# from the first successful write was still "recent enough").
pre_write_mtime <- if (file.exists(OUTPUT_PLOT_FILE)) file.info(OUTPUT_PLOT_FILE)$mtime else as.POSIXct(-Inf)

ggsave(OUTPUT_PLOT_FILE, p, width = 13, height = 9, dpi = 300)

# Verify the write actually landed (ggsave/the underlying graphics device can
# silently fail -- e.g. a transient OneDrive sync lock -- while still
# returning normally; caught in a prior session, so checked explicitly here
# rather than trusting ggsave() unconditionally).
if (file.exists(OUTPUT_PLOT_FILE) &&
    file.info(OUTPUT_PLOT_FILE)$mtime > pre_write_mtime) {
  cat(sprintf("\nPlot saved to: %s\n", OUTPUT_PLOT_FILE))
} else {
  warning("ggsave() returned normally but ", OUTPUT_PLOT_FILE,
          " does not appear to have just been written -- check for a file lock (e.g. OneDrive sync) and re-run.")
}

# ===============================================================================
# CONSOLE SUMMARY: outlier points + missing-pressure points (for traceability)
# ===============================================================================

if (n_outliers > 0) {
  cat("\n=== Outlier points (Surface-coloured square in plot) ===\n")
  outlier_summary <- plot_data %>%
    filter(is_outlier) %>%
    select(RunID, Study, Surface_type, Analyte, Pressure_g, Mean_Pressure, Recovery_pct) %>%
    arrange(Surface_type, Analyte, Pressure_g)
  print(as.data.frame(outlier_summary), row.names = FALSE)
}

if (n_missing_pressure > 0) {
  cat("\n=== Points with NO pressure-trace match (excluded from the point layer, still counted in box stats) ===\n")
  missing_summary <- plot_data %>%
    filter(!has_actual_pressure) %>%
    select(RunID, Study, Surface_type, Analyte, Pressure_g, Recovery_pct) %>%
    arrange(Surface_type, Analyte, Pressure_g)
  print(as.data.frame(missing_summary), row.names = FALSE)
}

cat("\nDone.\n")
