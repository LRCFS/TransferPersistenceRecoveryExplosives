# Recovery vs Actual Pressure -- Unified Continuous Scale (4-panel grid)
# ===============================================================================
# A companion figure to plot_recovery_boxplot_actualpressure.R and
# plot_main_study_recovery_vs_pressure.R (both left untouched by this script -
# this is an additional, separate output, not a replacement).
#
# Unlike those two (which split the x-axis into discrete nominal-pressure
# categories, either as boxes or as free-scaled per-level facet columns),
# this figure shows every sample as a single continuous scatter of recovery
# vs the ASTRA-measured ACTUAL pressure, on one shared/unified x-axis scale
# across all four panels (Surface x Analyte: Steel/ABS rows, PETN/RDX
# columns) - so the full 10-300g range and the relative spacing between
# pressure levels is directly visible and comparable panel-to-panel, with a
# linear trend line + 95% confidence ribbon fitted per panel.
#
# Data: same combined Pilot (wet samples only, matching the Main Study's
# all-wet design) + Main Study dataset as the other two scripts in this
# folder, joined on sample ID with each study's own ASTRA pressure-trace
# batch_summary.csv.
#
# Output:
#   - Main Study/Recovery_vs_ActualPressure_Unified_PilotMain.png
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

OUTPUT_PLOT_FILE <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_Unified_PilotMain.png")

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
# FILTER + JOIN (same logic as the other two scripts in this folder)
# ===============================================================================

recovery_main <- recovery_data %>% filter(SampleType == "Main")
recovery_pilot <- pilot_recovery_data %>% filter(SampleType == "Pilot", Solvent_level == "present")

joined_main <- recovery_main %>%
  left_join(pressure_data %>% select(Sample, Target, Mean_Pressure), by = c("RunID" = "Sample")) %>%
  mutate(Study = "Main")

joined_pilot <- recovery_pilot %>%
  left_join(pilot_pressure_data %>% select(Sample, Target, Mean_Pressure), by = c("RunID" = "Sample")) %>%
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
# RESHAPE TO LONG FORMAT
# ===============================================================================

plot_data <- joined %>%
  select(RunID, Surface_type, Study, Target, Mean_Pressure, PETN_Recovery_pct, RDX_Recovery_pct) %>%
  pivot_longer(
    cols = c(PETN_Recovery_pct, RDX_Recovery_pct),
    names_to = "Analyte",
    values_to = "Recovery_pct"
  ) %>%
  mutate(
    Analyte = factor(recode(Analyte, PETN_Recovery_pct = "PETN", RDX_Recovery_pct = "RDX"), levels = c("PETN", "RDX")),
    Surface_label = factor(ifelse(Surface_type == "steel", "Steel", "ABS"), levels = c("Steel", "ABS"))
  ) %>%
  filter(!is.na(Mean_Pressure), !is.na(Recovery_pct))

cat(sprintf("Total sample x analyte points available for plotting: %d\n", nrow(plot_data)))

# ===============================================================================
# PER-PANEL CORRELATION STATS (for the top-right annotation)
# ===============================================================================

panel_stats <- plot_data %>%
  group_by(Surface_label, Analyte) %>%
  summarise(
    n = n(),
    r = if (n() >= 3 && sd(Mean_Pressure) > 0 && sd(Recovery_pct) > 0) cor(Mean_Pressure, Recovery_pct) else NA_real_,
    p_value = if (n() >= 3 && sd(Mean_Pressure) > 0 && sd(Recovery_pct) > 0) cor.test(Mean_Pressure, Recovery_pct)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("n=%d\nr=%.2f\np=%.3f", n, r, p_value)
  )

cat("\nPer-panel correlation (actual pressure vs recovery, pooled Pilot+Main, all nominal levels together):\n")
print(as.data.frame(panel_stats))

# ===============================================================================
# PLOT
# ===============================================================================
# Unified/shared x-axis scale across all 4 panels (default facet_grid
# behaviour - not free), so the full 10-300g range and relative spacing
# between pressure levels is directly comparable panel-to-panel. A single
# linear trend line + 95% CI ribbon is fitted per panel, pooling both
# studies (points themselves remain colored by Study).

label_x <- max(plot_data$Mean_Pressure, na.rm = TRUE) * 0.98
label_y <- max(plot_data$Recovery_pct, na.rm = TRUE) * 0.95

p <- ggplot(plot_data, aes(x = Mean_Pressure, y = Recovery_pct)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8, fill = "gray70", alpha = 0.35) +
  geom_point(aes(color = Study), size = 2.3, alpha = 0.85) +
  geom_text(
    data = panel_stats,
    aes(x = label_x, y = label_y, label = label),
    inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3, color = "gray20", lineheight = 0.95
  ) +
  scale_color_manual(values = c("Main" = "#4477AA", "Pilot" = "#EE9944")) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery vs Actual Swabbing Pressure -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = "Unified x-axis scale across all panels (actual pressure, all nominal levels pooled) | Linear trend line + 95% CI fitted per panel, pooling Pilot+Main",
    x = "Actual mean swabbing pressure (g, stable regions)",
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
  )

ggsave(OUTPUT_PLOT_FILE, p, width = 11, height = 8, dpi = 300)
cat(sprintf("\nPlot saved to: %s\n", OUTPUT_PLOT_FILE))

cat("\nDone.\n")
