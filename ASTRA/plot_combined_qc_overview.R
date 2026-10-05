# Batch QC Overview: Mean Applied Mass vs Target -- Pilot + Main Study
# ===============================================================================
# Extends main_study_batch_process.R's own "Batch QC Overview - Mean Applied
# Mass vs Target" figure (plot_batch_qc_overview(), Main Study only) to also
# include the Pilot Study samples that are actually pooled into
# ASTRA/doe/main_study_analysis.R's own combined statistical model
# (build_pooled_dataset()) -- i.e. exactly:
#   - Main Study:  SampleType == "Main", analysis_accepted %in% c("PASS","PASS*")
#   - Pilot Study: SampleType == "Pilot", Solvent_level == "present" (wet-only),
#                  Pressure_g %in% c(50, 200), analysis_accepted %in% c("PASS","PASS*")
# The Pilot Study only ever tested 50g/200g, so the 10g/100g/300g panels are
# unchanged (Main Study only, as before) -- only the 50g and 200g panels
# gain 8 additional Pilot points each (12 total per panel instead of 4).
#
# Does NOT modify main_study_batch_process.R (explicitly self-contained,
# single-study "SINGLE-SCRIPT DESIGN" per its own header) -- this is a new,
# standalone script that reads each study's own ALREADY-COMPUTED
# Pressure Traces/ProcessedData/batch_summary.csv directly (same file
# main_study_batch_process.R itself writes/reads -- Mean_Pressure,
# Flag_15Pct_Outlier etc. are already computed there, no reprocessing
# needed), joined against each study's own recovery/QC data file
# (main_study_data_nested.csv / pilot_data_nested.csv) purely to apply the
# pooling filter above.
#
# Styling/logic otherwise identical to plot_batch_qc_overview() (dashed
# Target line, shaded +/-15% band, red/green Flag_15Pct_Outlier colouring,
# one facet per Target level) -- see that function's own header comment in
# main_study_batch_process.R for the full rationale.
#
# Output:
#   - Main Study/batch_qc_overview_PilotMain.png
#
# Author: OpenCode
# Date: 2026-09-10

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

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

OUTPUT_FILE <- file.path(MAIN_DIR, "batch_qc_overview_PilotMain.png")

# Matches PARAMS$mean_pressure_outlier_pct in main_study_batch_process.R.
OUTLIER_PCT <- 0.15

QC_OVERVIEW_WIDTH <- 10
QC_OVERVIEW_BASE_HEIGHT_IN <- 2.5
QC_OVERVIEW_HEIGHT_PER_ROW_IN <- 0.4
PLOT_DPI <- 300

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

# Main: ALL pressure levels, restricted to QC-passed real samples (matches
# build_pooled_dataset()'s own main_data filter -- currently a no-op since
# all 44 Main samples are PASS, but kept explicit for correctness/future
# datasets).
main_eligible_ids <- main_meta %>%
  filter(SampleType == "Main", analysis_accepted %in% c("PASS", "PASS*")) %>%
  pull(RunID)

main_pooled <- main_batch %>%
  filter(Sample %in% main_eligible_ids, Success == TRUE, !is.na(Mean_Pressure))

# Pilot: ONLY the wet-only 50g/200g QC-passed subset that build_pooled_dataset()
# actually pools -- NOT every Pilot sample with a trace (that would include
# dry-swab samples and the 10g/100g/300g-only... actually Pilot never tested
# those, but it WOULD include Pilot's own dry-swab 50g/200g samples and any
# QC-FAIL sample, neither of which feed the combined statistical model).
pilot_eligible_ids <- pilot_meta %>%
  filter(SampleType == "Pilot", Solvent_level == "present", Pressure_g %in% c(50, 200),
         analysis_accepted %in% c("PASS", "PASS*")) %>%
  pull(RunID)

pilot_pooled <- pilot_batch %>%
  filter(Sample %in% pilot_eligible_ids, Success == TRUE, !is.na(Mean_Pressure))

cat(sprintf("Main pooled samples: %d\n", nrow(main_pooled)))
cat(sprintf("Pilot pooled samples (wet-only 50g/200g, QC-passed): %d (%s)\n",
            nrow(pilot_pooled), paste(pilot_pooled$Sample, collapse = ", ")))

plot_df <- bind_rows(main_pooled, pilot_pooled)

cat(sprintf("\nCombined samples per Target level:\n"))
print(table(plot_df$Target))

# ===============================================================================
# PLOT (logic identical to main_study_batch_process.R's plot_batch_qc_overview())
# ===============================================================================

plot_df <- plot_df[order(plot_df$Target, plot_df$Mean_Pressure), ]
plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample)
plot_df$Lower <- plot_df$Target * (1 - OUTLIER_PCT)
plot_df$Upper <- plot_df$Target * (1 + OUTLIER_PCT)
plot_df$Flag_Label <- factor(
  ifelse(isTRUE(plot_df$Flag_15Pct_Outlier) | plot_df$Flag_15Pct_Outlier == TRUE,
         sprintf("Outside %.0f%% of Target", OUTLIER_PCT * 100),
         sprintf("Within %.0f%% of Target", OUTLIER_PCT * 100)),
  levels = c(sprintf("Within %.0f%% of Target", OUTLIER_PCT * 100),
             sprintf("Outside %.0f%% of Target", OUTLIER_PCT * 100))
)

band_df <- unique(plot_df[, c("Target", "Lower", "Upper")])
target_df <- unique(plot_df[, "Target", drop = FALSE])

n_max_rows <- max(table(plot_df$Target))
plot_height <- QC_OVERVIEW_BASE_HEIGHT_IN + n_max_rows * QC_OVERVIEW_HEIGHT_PER_ROW_IN

p <- ggplot(plot_df, aes(x = Mean_Pressure, y = Sample)) +
  geom_rect(data = band_df, aes(xmin = Lower, xmax = Upper), ymin = -Inf, ymax = Inf,
            fill = "gray85", alpha = 0.6, inherit.aes = FALSE) +
  geom_vline(data = target_df, aes(xintercept = Target), linetype = "dashed", color = "gray30") +
  geom_segment(aes(x = Target, xend = Mean_Pressure, y = Sample, yend = Sample), color = "gray50") +
  geom_point(aes(color = Flag_Label), size = 3) +
  scale_color_manual(
    values = setNames(
      c("#009E73", "#D55E00"),
      c(sprintf("Within %.0f%% of Target", OUTLIER_PCT * 100),
        sprintf("Outside %.0f%% of Target", OUTLIER_PCT * 100))
    ),
    name = NULL
  ) +
  facet_wrap(~ Target, scales = "free", labeller = labeller(Target = function(x) paste0(x, "g target"))) +
  labs(
    title = "Batch QC Overview - Mean Applied Mass vs Target -- Pilot + Main Study",
    subtitle = sprintf(
      "Dashed line = Target | Shaded band = within %.0f%% of Target | Red = flagged (Flag_15Pct_Outlier)\nOnly samples pooled into main_study_analysis.R's combined model (50g/200g panels include the Pilot's QC-passed wet-only subset)",
      OUTLIER_PCT * 100
    ),
    x = "Mean Applied Mass (stable regions, g)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

pre_write_mtime <- if (file.exists(OUTPUT_FILE)) file.info(OUTPUT_FILE)$mtime else as.POSIXct(-Inf)

ggsave(OUTPUT_FILE, p, width = QC_OVERVIEW_WIDTH, height = plot_height, dpi = PLOT_DPI, limitsize = FALSE)

if (file.exists(OUTPUT_FILE) && file.info(OUTPUT_FILE)$mtime > pre_write_mtime) {
  cat(sprintf("\nSaved: %s\n", OUTPUT_FILE))
} else {
  warning("ggsave() returned normally but ", OUTPUT_FILE,
          " does not appear to have just been written -- check for a file lock (e.g. OneDrive sync) and re-run.")
}

cat("\nDone.\n")
