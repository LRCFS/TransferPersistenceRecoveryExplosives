# Plot Main Study Recovery (PETN/RDX) vs Actual Swabbing Pressure
# ===============================================================================
# Joins the ASTRA-measured *actual* achieved pressure (Mean_Pressure, from
# batch_summary.csv, produced by main_study_batch_process.R) with the
# GCMSQuantitation recovery results (PETN_Recovery_pct/RDX_Recovery_pct,
# from main_study_data_nested.csv) on sample ID, and plots recovery against
# actual (not nominal/target) pressure for both analytes, faceted by
# Surface_type x nominal pressure level and shaped by QC acceptance status.
#
# MAIN_STUDY_ADAPTATION (2026-08-25): the Main Study design deliberately
# only collected 2 new replicates per surface at the 50g/200g levels,
# reusing the 4 replicates per surface already collected for those same
# two levels in the Pilot Study (the Pilot Study never tested 10/100/300g
# at all, so those three levels are Main-Study-only). To reflect the
# actual combined dataset the study design intends, this script now also
# loads the Pilot Study's recovery + actual-pressure data for the 50g/200g
# levels (wet/Solvent_level=="present" samples only, to match the Main
# Study's all-wet design - the Pilot Study also ran dry replicates at
# every level, which are deliberately excluded here as not comparable)
# and combines it with the Main Study data before plotting, tagging each
# point with a Study column (Pilot/Main) so the two sources remain visually
# distinguishable. The 10g/100g/300g panels are unaffected (Main Study only,
# since no Pilot Study data exists at those levels).
#
# Inputs:
#   - Main Study/Pressure Traces/ProcessedData/batch_summary.csv
#       (Sample, Target, Mean_Pressure, RSD_Pressure, Flag_15Pct_Outlier, ...)
#   - Main Study/main_study_data_nested.csv
#       (RunID, SampleType, Surface_type, PETN_Recovery_pct, RDX_Recovery_pct,
#        analysis_accepted, Pressure_g [nominal target, NOT used as the x-axis
#        here - Mean_Pressure from the pressure-trace pipeline is used instead])
#   - Pilot Study/Pressure Traces/ProcessedData/batch_summary.csv
#       (same columns as the Main Study version, keyed by PILOT_xxx)
#   - Pilot Study/pilot_data_nested.csv
#       (same shape as main_study_data_nested.csv, keyed by PILOT_xxx;
#        SampleType == "Pilot" for real samples (vs "NC"), Solvent_level
#        present/absent for wet/dry)
#
# Output:
#   - Main Study/Recovery_vs_ActualPressure_MainStudy_PETN.png
#   - Main Study/Recovery_vs_ActualPressure_MainStudy_RDX.png
#   - Main Study/Recovery_vs_ActualPressure_MainStudy_joined_data.csv
#     (the joined per-sample table used to make the plots, for traceability -
#      now includes both Pilot and Main rows, distinguished by the Study column)
#
# Author: OpenCode
# Date: 2026-08-25

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
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

OUTPUT_PLOT_FILE_PETN <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_MainStudy_PETN.png")
OUTPUT_PLOT_FILE_RDX <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_MainStudy_RDX.png")
OUTPUT_JOINED_CSV <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_MainStudy_joined_data.csv")

# ===============================================================================
# LOAD DATA
# ===============================================================================

for (f in c(PRESSURE_SUMMARY_FILE, RECOVERY_FILE, PILOT_PRESSURE_SUMMARY_FILE, PILOT_RECOVERY_FILE)) {
  if (!file.exists(f)) {
    stop(sprintf("Required input file not found: %s", f))
  }
}

pressure_data <- read.csv(PRESSURE_SUMMARY_FILE, stringsAsFactors = FALSE)
recovery_data <- read.csv(RECOVERY_FILE, stringsAsFactors = FALSE)
pilot_pressure_data <- read.csv(PILOT_PRESSURE_SUMMARY_FILE, stringsAsFactors = FALSE)
pilot_recovery_data <- read.csv(PILOT_RECOVERY_FILE, stringsAsFactors = FALSE)

cat(sprintf("Loaded %d rows from Main Study batch_summary.csv (pressure data)\n", nrow(pressure_data)))
cat(sprintf("Loaded %d rows from main_study_data_nested.csv (recovery data)\n", nrow(recovery_data)))
cat(sprintf("Loaded %d rows from Pilot Study batch_summary.csv (pressure data)\n", nrow(pilot_pressure_data)))
cat(sprintf("Loaded %d rows from pilot_data_nested.csv (recovery data)\n", nrow(pilot_recovery_data)))

# ===============================================================================
# FILTER RECOVERY DATA TO REAL SAMPLES ONLY (both studies)
# ===============================================================================
# main_study_data_nested.csv also contains 10 negative-control (NC) rows
# (SampleType == "NC") with no Pressure_g/recovery values and no matching
# pressure-trace file (they were excluded from batch processing entirely -
# see build_sample_config()'s file-name pattern in main_study_batch_process.R).
# pilot_data_nested.csv has the equivalent NC rows (SampleType == "NC").
# Drop both here so the joins below don't produce all-NA rows for them.
# Pilot Study is additionally filtered to Solvent_level == "present" (wet)
# only, to match the Main Study's all-wet design - see header comment.

recovery_main <- recovery_data %>%
  filter(SampleType == "Main")

cat(sprintf("Main Study: filtered to %d 'Main' SampleType rows (dropped %d NC rows)\n",
            nrow(recovery_main), nrow(recovery_data) - nrow(recovery_main)))

recovery_pilot <- pilot_recovery_data %>%
  filter(SampleType == "Pilot", Solvent_level == "present")

cat(sprintf("Pilot Study: filtered to %d wet 'Pilot' SampleType rows (dropped %d NC/dry rows)\n",
            nrow(recovery_pilot), nrow(pilot_recovery_data) - nrow(recovery_pilot)))

# ===============================================================================
# JOIN ON SAMPLE ID (each study against its own pressure summary)
# ===============================================================================
# Join key: RunID (e.g. "MAIN_001" / "PILOT_005") <-> Sample column in each
# study's own batch_summary.csv (same ID scheme within each study).

joined_main <- recovery_main %>%
  left_join(
    pressure_data %>%
      select(Sample, Target, Mean_Pressure, RSD_Pressure, Stable_Time_Pct, Flag_15Pct_Outlier, Success),
    by = c("RunID" = "Sample")
  ) %>%
  mutate(Study = "Main")

joined_pilot <- recovery_pilot %>%
  left_join(
    pilot_pressure_data %>%
      select(Sample, Target, Mean_Pressure, RSD_Pressure, Stable_Time_Pct, Flag_15Pct_Outlier, Success),
    by = c("RunID" = "Sample")
  ) %>%
  mutate(Study = "Pilot")

for (nm in c("Main", "Pilot")) {
  df <- if (nm == "Main") joined_main else joined_pilot
  n_missing <- sum(is.na(df$Mean_Pressure))
  if (n_missing > 0) {
    cat(sprintf(
      "WARNING: %d %s Study sample(s) have no matching pressure-trace result (missing Mean_Pressure): %s\n",
      n_missing, nm, paste(df$RunID[is.na(df$Mean_Pressure)], collapse = ", ")
    ))
  } else {
    cat(sprintf("All %s Study recovery rows matched to a pressure-trace result.\n", nm))
  }
}

# Keep only the columns needed downstream, common to both studies, before
# combining (Main/Pilot source files have different extra columns otherwise -
# e.g. Pilot has BatchNumber/Notes, Main has SurfaceProcessingOrder etc. -
# none of which are needed here).
common_cols <- c("RunID", "Surface_type", "Study", "Target", "Mean_Pressure",
                  "RSD_Pressure", "Stable_Time_Pct", "Flag_15Pct_Outlier", "Success",
                  "analysis_accepted", "PETN_Recovery_pct", "RDX_Recovery_pct")

joined <- bind_rows(
  joined_main %>% select(all_of(common_cols)),
  joined_pilot %>% select(all_of(common_cols))
)

cat(sprintf("\nCombined Main + Pilot joined dataset: %d rows (%d Main, %d Pilot)\n",
            nrow(joined), sum(joined$Study == "Main"), sum(joined$Study == "Pilot")))

# Simplify the QC-acceptance status (analysis_accepted has several distinct
# free-text FAIL reasons, e.g. "FAIL: PETN 6ng QC bracket FAILED or
# unavailable" / "FAIL: RDX 6ng QC bracket FAILED or unavailable") down to a
# binary PASS / QC Flagged for plotting purposes, without discarding any
# points - QC-flagged samples are shown (as a distinct point shape) rather
# than silently dropped, since the recovery numbers themselves are still
# present and may still be informative even if a QC bracket failed.
joined <- joined %>%
  mutate(
    QC_Status = case_when(
      is.na(analysis_accepted) ~ "Unknown",
      analysis_accepted == "PASS" ~ "PASS",
      TRUE ~ "QC Flagged"
    ),
    Pressure_Outlier = ifelse(is.na(Flag_15Pct_Outlier), FALSE, Flag_15Pct_Outlier)
  )

write.csv(joined, OUTPUT_JOINED_CSV, row.names = FALSE)
cat(sprintf("Joined data saved to: %s\n", OUTPUT_JOINED_CSV))

# ===============================================================================
# RESHAPE TO LONG FORMAT (one row per Sample x Analyte)
# ===============================================================================

plot_data <- joined %>%
  select(RunID, Surface_type, Study, Target, Mean_Pressure, RSD_Pressure, QC_Status,
         Pressure_Outlier, PETN_Recovery_pct, RDX_Recovery_pct) %>%
  pivot_longer(
    cols = c(PETN_Recovery_pct, RDX_Recovery_pct),
    names_to = "Analyte",
    values_to = "Recovery_pct"
  ) %>%
  mutate(
    Analyte = recode(Analyte, PETN_Recovery_pct = "PETN", RDX_Recovery_pct = "RDX")
  ) %>%
  filter(!is.na(Mean_Pressure), !is.na(Recovery_pct))

cat(sprintf("Plotting %d sample x analyte points (%d samples x 2 analytes, minus any missing pressure matches)\n",
            nrow(plot_data), length(unique(plot_data$RunID))))

# ===============================================================================
# PLOT: RECOVERY vs ACTUAL PRESSURE (faceted by Surface x Nominal Pressure)
# ===============================================================================
# One figure per analyte (PETN, RDX). Each figure is a 5 (nominal pressure
# level, ascending 10/50/100/200/300g) x 2 (Surface: steel on top, abs on
# bottom) grid of small panels. Each panel shows only the samples run at
# that one nominal pressure level, plotting their *actual* achieved
# pressure (Mean_Pressure) against recovery - with per-panel free x-axis
# scales, so the (fairly small, mostly single-digit-gram) within-level
# spread in actual pressure is visible instead of being compressed flat
# against a single shared 0-300g axis (which made the single combined
# plot from the first version of this script hard to read).
# No trend line is drawn (removed per feedback - with as few as 2 points
# in some panels a fitted line was not meaningful). Instead, each panel is
# annotated with the mean/min/max of the *actual* achieved pressure for
# that Surface x nominal-target combination, so the achieved spread around
# the nominal target is visible as text even without a line.
# Color now encodes Study (Pilot vs Main) rather than Surface_type, since
# Surface_type is already shown by the facet row - the 50g/200g panels mix
# both studies' points (see header comment); the 10g/100g/300g panels are
# Main-only (Pilot Study never tested those levels) and will show a single
# color throughout.

# Order Surface_type as a factor so facet_grid places "steel" in the top
# row and "abs" in the bottom row (facet_grid rows are drawn top-to-bottom
# in the row variable's factor level order).
plot_data <- plot_data %>%
  mutate(Surface_type = factor(Surface_type, levels = c("steel", "abs")))

make_recovery_facet_plot <- function(analyte_name, output_file) {
  sub <- plot_data %>% filter(Analyte == analyte_name)

  # Per-panel (Surface x Target) actual-pressure summary, used for both the
  # console log and the in-panel text annotation. n now reflects the
  # combined Pilot+Main sample count for that panel.
  panel_stats <- sub %>%
    group_by(Surface_type, Target) %>%
    summarise(
      n = n(),
      n_pilot = sum(Study == "Pilot"),
      n_main = sum(Study == "Main"),
      mean_p = mean(Mean_Pressure, na.rm = TRUE),
      min_p = min(Mean_Pressure, na.rm = TRUE),
      max_p = max(Mean_Pressure, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("Mean: %.1fg\nMin: %.1fg\nMax: %.1fg", mean_p, min_p, max_p)
    )

  cat(sprintf("\n%s: actual-pressure summary per panel (Surface x Target):\n", analyte_name))
  print(panel_stats %>% select(Surface_type, Target, n, n_pilot, n_main, mean_p, min_p, max_p))

  p <- ggplot(sub, aes(x = Mean_Pressure, y = Recovery_pct)) +
    geom_point(aes(shape = QC_Status, color = Study), size = 2.5, alpha = 0.9) +
    geom_text(
      data = panel_stats,
      aes(x = Inf, y = Inf, label = label),
      inherit.aes = FALSE,
      hjust = 1.05, vjust = 1.15,
      size = 2.5, color = "gray30", lineheight = 0.95
    ) +
    scale_shape_manual(values = c("PASS" = 16, "QC Flagged" = 4, "Unknown" = 1)) +
    scale_color_manual(values = c("Main" = "#2166ac", "Pilot" = "#b2182b")) +
    facet_grid(
      rows = vars(Surface_type),
      cols = vars(Target),
      scales = "free_x",
      labeller = labeller(Target = function(x) sprintf("%sg target", x))
    ) +
    labs(
      title = sprintf("Main + Pilot Study: %s Recovery vs Actual Swabbing Pressure", analyte_name),
      subtitle = "Rows = Surface (steel top, abs bottom) | Columns = nominal/target pressure level | Color = Study (10/100/300g are Main-only) | x-axis = actual achieved pressure (free scale per panel)",
      x = "Actual mean swabbing pressure (g, stable regions)",
      y = "Recovery (%)",
      color = "Study",
      shape = "QC Status"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 9),
      axis.text.x = element_text(size = 7),
      panel.spacing = unit(0.6, "lines")
    )

  ggsave(output_file, p, width = 13, height = 6, dpi = 300)
  cat(sprintf("%s plot saved to: %s\n", analyte_name, output_file))
}

make_recovery_facet_plot("PETN", OUTPUT_PLOT_FILE_PETN)
make_recovery_facet_plot("RDX", OUTPUT_PLOT_FILE_RDX)

# ===============================================================================
# QUICK NUMERIC SUMMARY (console)
# ===============================================================================

cat("\n=== Correlation (Pearson) between actual pressure and recovery, by analyte (combined Main+Pilot) ===\n")
for (analyte in c("PETN", "RDX")) {
  sub <- plot_data %>% filter(Analyte == analyte)
  if (nrow(sub) >= 3) {
    ct <- cor.test(sub$Mean_Pressure, sub$Recovery_pct)
    cat(sprintf("  %s: r = %.3f, p = %.4f, n = %d\n", analyte, ct$estimate, ct$p.value, nrow(sub)))
  }
}

cat("\n=== Correlation (Pearson) between actual pressure and recovery, by analyte x surface (combined Main+Pilot) ===\n")
for (analyte in c("PETN", "RDX")) {
  for (surf in unique(plot_data$Surface_type)) {
    sub <- plot_data %>% filter(Analyte == analyte, Surface_type == surf)
    if (nrow(sub) >= 3) {
      ct <- cor.test(sub$Mean_Pressure, sub$Recovery_pct)
      cat(sprintf("  %s / %s: r = %.3f, p = %.4f, n = %d\n", analyte, surf, ct$estimate, ct$p.value, nrow(sub)))
    }
  }
}

cat("\nDone.\n")
