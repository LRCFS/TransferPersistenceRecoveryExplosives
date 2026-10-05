# Recovery Scatter: PETN vs RDX -- Pilot + Main Study
# ===============================================================================
# Plots each individual sample's own PETN recovery (%) against its own RDX
# recovery (%) on the same swab -- one point per sample. Answers "does a
# sample that recovers more PETN also tend to recover more RDX?" directly,
# at the individual-sample level (as opposed to the other ASTRA recovery
# figures, which compare recovery against pressure/mass -- this one compares
# the two analytes against EACH OTHER).
#
# Scope (updated 2026-09-10 per explicit request): shows ONLY the samples
# that actually feed ASTRA/doe/main_study_analysis.R's own combined
# Pilot+Main model/hinge plot (build_pooled_dataset()) -- i.e. exactly:
#   - Main Study: all real samples (SampleType != "NC") that pass QC
#     (analysis_accepted %in% c("PASS","PASS*"))
#   - Pilot Study: its wet-only 50g/200g subset (Solvent_level=="present",
#     Pressure_g %in% c(50,200)) that ALSO passes QC -- the Pilot's dry-swab
#     samples and its 10/100/300g-only rows are NOT part of this pooled
#     dataset and are correctly excluded here too.
# This replaces an earlier version of this script that used ALL Pilot +
# Main samples regardless of QC/pooling eligibility -- that version showed
# a cluster of Pilot dry-swab points near the origin that never actually
# fed into the main study's own combined analysis at all.
#
# Styling matches the other ASTRA recovery figures' shared colour/marker
# language (colour = Surface_type, shape = Study: hollow = Pilot, solid =
# Main). Per explicit request: points only, NO trend/fit line.
#
# Data sources: same main_study_data_nested.csv/pilot_data_nested.csv as
# plot_recovery_boxplot_actualpressure.R (self-contained, does not depend
# on any other script having been run first).
#
# Output:
#   - Main Study/Recovery_PETN_vs_RDX_PilotMain.png
#
# Author: OpenCode
# Date: 2026-09-10

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
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

RECOVERY_FILE <- file.path(BASE_DIR, "main_study_data_nested.csv")
PILOT_RECOVERY_FILE <- file.path(PILOT_DIR, "pilot_data_nested.csv")

OUTPUT_PLOT_FILE <- file.path(BASE_DIR, "Recovery_PETN_vs_RDX_PilotMain.png")

# ===============================================================================
# LOAD + COMBINE DATA
# ===============================================================================

for (f in c(RECOVERY_FILE, PILOT_RECOVERY_FILE)) {
  if (!file.exists(f)) stop(sprintf("Required input file not found: %s", f))
}

recovery_data <- read.csv(RECOVERY_FILE, stringsAsFactors = FALSE)
pilot_recovery_data <- read.csv(PILOT_RECOVERY_FILE, stringsAsFactors = FALSE)

cat(sprintf("Loaded %d Main recovery rows, %d Pilot recovery rows\n",
            nrow(recovery_data), nrow(pilot_recovery_data)))

# Restricted to exactly the rows that feed main_study_analysis.R's own
# build_pooled_dataset() -- see header for the full rationale:
#   Main:  SampleType != "NC", QC-passed (PASS/PASS*)
#   Pilot: SampleType != "NC", wet-only 50g/200g subset, QC-passed
recovery_main <- recovery_data %>%
  filter(SampleType == "Main", analysis_accepted %in% c("PASS", "PASS*")) %>%
  transmute(RunID, Surface_type, Study = "Main",
            PETN_Recovery_pct, RDX_Recovery_pct)

recovery_pilot <- pilot_recovery_data %>%
  filter(SampleType == "Pilot", Solvent_level == "present", Pressure_g %in% c(50, 200),
         analysis_accepted %in% c("PASS", "PASS*")) %>%
  transmute(RunID, Surface_type, Study = "Pilot",
            PETN_Recovery_pct, RDX_Recovery_pct)

plot_data <- bind_rows(recovery_main, recovery_pilot) %>%
  filter(!is.na(PETN_Recovery_pct), !is.na(RDX_Recovery_pct)) %>%
  mutate(Study = factor(Study, levels = c("Pilot", "Main")))

cat(sprintf("Combined Main + Pilot samples with both PETN and RDX recovery values: %d (%d Main, %d Pilot)\n",
            nrow(plot_data), sum(plot_data$Study == "Main"), sum(plot_data$Study == "Pilot")))

# Informational only (NOT drawn on the plot -- no trend line per explicit
# request) -- printed to console for reference.
corr_result <- suppressWarnings(cor.test(plot_data$PETN_Recovery_pct, plot_data$RDX_Recovery_pct))
cat(sprintf("Pearson correlation (PETN vs RDX recovery, all %d pooled points): r=%.3f, p=%s\n",
            nrow(plot_data), corr_result$estimate,
            if (corr_result$p.value < 0.001) "<0.001" else sprintf("%.3f", corr_result$p.value)))

# ===============================================================================
# PLOT
# ===============================================================================

axis_max <- max(c(plot_data$PETN_Recovery_pct, plot_data$RDX_Recovery_pct), na.rm = TRUE) * 1.05

p <- ggplot(plot_data, aes(x = PETN_Recovery_pct, y = RDX_Recovery_pct, color = Surface_type, shape = Study)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = pal_surface_type, labels = c(steel = "Steel", abs = "ABS"), name = "Surface") +
  scale_shape_manual(values = c(Pilot = 1, Main = 19), name = "Study") +
  coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
  labs(
    title = sprintf("PETN vs RDX Recovery -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = paste0(
      "Each point = one sample's own PETN recovery vs its own RDX recovery | No fit/trend line\n",
      "Only the samples pooled into main_study_analysis.R's own combined model: all QC-passed Main Study samples + ",
      "the Pilot Study's QC-passed wet-only 50g/200g subset"
    ),
    x = "PETN Recovery (%)",
    y = "RDX Recovery (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 8.5)
  )

# Record the pre-write mtime (or -Inf if the file doesn't exist yet) BEFORE
# calling ggsave, so the post-write check below compares against a genuine
# "did this write actually happen just now" baseline -- ggsave's underlying
# graphics device has been observed to silently fail (e.g. a transient
# OneDrive sync lock) while still returning normally, in prior sessions on
# the other ASTRA plotting scripts.
pre_write_mtime <- if (file.exists(OUTPUT_PLOT_FILE)) file.info(OUTPUT_PLOT_FILE)$mtime else as.POSIXct(-Inf)

ggsave(OUTPUT_PLOT_FILE, p, width = 11, height = 8.5, dpi = 300)

if (file.exists(OUTPUT_PLOT_FILE) && file.info(OUTPUT_PLOT_FILE)$mtime > pre_write_mtime) {
  cat(sprintf("\nPlot saved to: %s\n", OUTPUT_PLOT_FILE))
} else {
  warning("ggsave() returned normally but ", OUTPUT_PLOT_FILE,
          " does not appear to have just been written -- check for a file lock (e.g. OneDrive sync) and re-run.")
}

cat("\nDone.\n")
