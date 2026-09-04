# Recovery vs Actual Pressure -- Unified Continuous Scale (CONSOLIDATED)
# ===============================================================================
# Consolidated 02/09/2026 from what were previously 3 separate, near-identical
# scripts (plot_recovery_vs_pressure_unified.R / _quadratic.R / _logarithmic.R)
# -- each loaded and joined the exact same data, differing only in which curve
# shape was fit. This single script now does the data loading/joining ONCE,
# runs the model comparison ONCE, and produces all three curve-fit figures in
# one run. The two now-redundant standalone scripts have been deleted.
#
# Shows every sample as a single continuous scatter of recovery vs the
# ASTRA-measured ACTUAL pressure, on one shared/unified x-axis scale across
# all four panels (Surface x Analyte: Steel/ABS rows, PETN/RDX columns) -- so
# the full 10-300g range and the relative spacing between pressure levels is
# directly visible and comparable panel-to-panel.
#
# Data: combined Pilot (wet samples only, matching the Main Study's all-wet
# design) + Main Study dataset, joined on sample ID with each study's own
# ASTRA pressure-trace batch_summary.csv.
#
# Companion to plot_recovery_boxplot_actualpressure.R and
# plot_main_study_recovery_vs_pressure.R (both untouched, still separate --
# those split the x-axis into discrete nominal-pressure categories; this
# script's whole point is the unified continuous actual-pressure scale).
#
# Model comparison (AIC/BIC, not just R^2 -- R^2 alone always favours whichever
# model has more free parameters, i.e. quadratic, regardless of whether the
# extra curvature is actually justified by the data) across Linear / Quadratic
# / Logarithmic / Asymptotic-saturating candidate models per panel, printed to
# the console, to answer "is there a better fit" properly rather than just
# eyeballing R^2.
#
# Output (all in Main Study/):
#   - Recovery_vs_ActualPressure_Unified_PilotMain.png              (linear)
#   - Recovery_vs_ActualPressure_Unified_Quadratic_PilotMain.png    (quadratic)
#   - Recovery_vs_ActualPressure_Unified_Logarithmic_PilotMain.png  (logarithmic)
#
# Author: OpenCode
# Date: 2026-08-25 (consolidated 2026-09-02)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
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

OUTPUT_LINEAR      <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_Unified_PilotMain.png")
OUTPUT_QUADRATIC   <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_Unified_Quadratic_PilotMain.png")
OUTPUT_LOGARITHMIC <- file.path(BASE_DIR, "Recovery_vs_ActualPressure_Unified_Logarithmic_PilotMain.png")

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
# FILTER + JOIN
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

label_x <- max(plot_data$Mean_Pressure, na.rm = TRUE) * 0.98
label_y <- max(plot_data$Recovery_pct, na.rm = TRUE) * 0.95

base_theme <- theme_minimal(base_size = 12) +
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

# ===============================================================================
# MODEL COMPARISON: Linear / Quadratic / Logarithmic / Asymptotic-saturating
# ===============================================================================
# AIC/BIC penalise extra parameters, so they (not R^2) are the right basis for
# "which functional form genuinely fits better":
#   Linear:      Recovery ~ Pressure
#   Quadratic:   Recovery ~ Pressure + Pressure^2
#   Logarithmic: Recovery ~ log(Pressure)                    (diminishing returns, no turnover)
#   Asymptotic:  Recovery ~ a * (1 - exp(-b * Pressure))     (nls, saturating curve; may fail to
#                                                              converge on some panels -- handled
#                                                              gracefully with NA if so)

fit_and_compare_models <- function(df) {
  n <- nrow(df)

  fit_lin <- tryCatch(lm(Recovery_pct ~ Mean_Pressure, data = df), error = function(e) NULL)
  fit_quad <- tryCatch(lm(Recovery_pct ~ Mean_Pressure + I(Mean_Pressure^2), data = df), error = function(e) NULL)
  fit_log <- tryCatch(lm(Recovery_pct ~ log(Mean_Pressure), data = df), error = function(e) NULL)
  fit_asym <- tryCatch(
    nls(Recovery_pct ~ a * (1 - exp(-b * Mean_Pressure)),
        data = df,
        start = list(a = max(df$Recovery_pct, na.rm = TRUE), b = 0.01),
        control = nls.control(maxiter = 200, warnOnly = TRUE)),
    error = function(e) NULL
  )

  get_stats <- function(fit, label, k) {
    if (is.null(fit)) return(data.frame(Model = label, n = n, k = k, AIC = NA_real_, BIC = NA_real_, R2 = NA_real_))
    r2 <- if (inherits(fit, "lm")) summary(fit)$r.squared else {
      rss <- sum(residuals(fit)^2)
      tss <- sum((df$Recovery_pct - mean(df$Recovery_pct))^2)
      1 - rss / tss
    }
    data.frame(Model = label, n = n, k = k, AIC = AIC(fit), BIC = BIC(fit), R2 = r2)
  }

  bind_rows(
    get_stats(fit_lin, "Linear", 2),
    get_stats(fit_quad, "Quadratic", 3),
    get_stats(fit_log, "Logarithmic", 2),
    get_stats(fit_asym, "Asymptotic", 2)
  )
}

cat("\n=== Model comparison per panel (lower AIC/BIC = better; R2 is NOT comparable across models with different k) ===\n")
panels <- plot_data %>% distinct(Surface_label, Analyte)
for (i in seq_len(nrow(panels))) {
  surf <- panels$Surface_label[i]
  an <- panels$Analyte[i]
  df_panel <- plot_data %>% filter(Surface_label == surf, Analyte == an)
  cmp <- fit_and_compare_models(df_panel)
  cmp$Delta_AIC <- cmp$AIC - min(cmp$AIC, na.rm = TRUE)
  cmp <- cmp %>% arrange(AIC)
  cat(sprintf("\n--- %s / %s (n=%d) ---\n", surf, an, nrow(df_panel)))
  print(cmp %>% mutate(AIC = round(AIC, 1), BIC = round(BIC, 1), R2 = round(R2, 3), Delta_AIC = round(Delta_AIC, 1)),
        row.names = FALSE)
}

# ===============================================================================
# PLOT 1: LINEAR
# ===============================================================================

panel_stats_lin <- plot_data %>%
  group_by(Surface_label, Analyte) %>%
  summarise(
    n = n(),
    r = if (n() >= 3 && sd(Mean_Pressure) > 0 && sd(Recovery_pct) > 0) cor(Mean_Pressure, Recovery_pct) else NA_real_,
    p_value = if (n() >= 3 && sd(Mean_Pressure) > 0 && sd(Recovery_pct) > 0) cor.test(Mean_Pressure, Recovery_pct)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("n=%d\nr=%.2f\np=%.3f", n, r, p_value))

cat("\nPer-panel correlation (linear, actual pressure vs recovery, pooled Pilot+Main):\n")
print(as.data.frame(panel_stats_lin))

p_lin <- ggplot(plot_data, aes(x = Mean_Pressure, y = Recovery_pct)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8, fill = "gray70", alpha = 0.35) +
  geom_point(aes(color = Study), size = 2.3, alpha = 0.85) +
  geom_text(data = panel_stats_lin, aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3, color = "gray20", lineheight = 0.95) +
  scale_color_manual(values = pal_study) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery vs Actual Applied Mass -- Pilot + Main Study (n=%d)", nrow(plot_data)),
    subtitle = "Unified x-axis scale across all panels | Linear trend line + 95% CI fitted per panel, pooling Pilot+Main",
    x = "Actual mean applied mass (g, stable regions)", y = "Recovery (%)", color = "Study"
  ) +
  base_theme

ggsave(OUTPUT_LINEAR, p_lin, width = 11, height = 8, dpi = 300)
cat(sprintf("\nLinear plot saved to: %s\n", OUTPUT_LINEAR))

# ===============================================================================
# PLOT 2: QUADRATIC
# ===============================================================================

fit_quadratic_stats <- function(df) {
  if (nrow(df) < 5) return(data.frame(n = nrow(df), r_squared = NA_real_, p_value = NA_real_))
  fit <- tryCatch(lm(Recovery_pct ~ Mean_Pressure + I(Mean_Pressure^2), data = df), error = function(e) NULL)
  if (is.null(fit)) return(data.frame(n = nrow(df), r_squared = NA_real_, p_value = NA_real_))
  s <- summary(fit)
  fstat <- s$fstatistic
  p_val <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
  data.frame(n = nrow(df), r_squared = s$r.squared, p_value = as.numeric(p_val))
}

panel_stats_quad <- plot_data %>%
  group_by(Surface_label, Analyte) %>%
  do(fit_quadratic_stats(.)) %>%
  ungroup() %>%
  mutate(label = sprintf("n=%d\nR2=%.2f\np=%.3f", n, r_squared, p_value))

cat("\nPer-panel quadratic fit (Recovery ~ Pressure + Pressure^2, pooled Pilot+Main):\n")
print(as.data.frame(panel_stats_quad))

p_quad <- ggplot(plot_data, aes(x = Mean_Pressure, y = Recovery_pct)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE,
              color = "black", linewidth = 0.8, fill = "gray70", alpha = 0.35) +
  geom_point(aes(color = Study), size = 2.3, alpha = 0.85) +
  geom_text(data = panel_stats_quad, aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3, color = "gray20", lineheight = 0.95) +
  scale_color_manual(values = pal_study) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery vs Actual Applied Mass -- Pilot + Main Study (n=%d) -- Quadratic Fit", nrow(plot_data)),
    subtitle = "Unified x-axis scale across all panels | Quadratic trend (Recovery ~ Mass + Mass^2) + 95% CI fitted per panel, pooling Pilot+Main",
    x = "Actual mean applied mass (g, stable regions)", y = "Recovery (%)", color = "Study"
  ) +
  base_theme

ggsave(OUTPUT_QUADRATIC, p_quad, width = 11, height = 8, dpi = 300)
cat(sprintf("Quadratic plot saved to: %s\n", OUTPUT_QUADRATIC))

# ===============================================================================
# PLOT 3: LOGARITHMIC
# ===============================================================================

fit_log_stats <- function(df) {
  if (nrow(df) < 4) return(data.frame(n = nrow(df), r_squared = NA_real_, p_value = NA_real_))
  fit <- tryCatch(lm(Recovery_pct ~ log(Mean_Pressure), data = df), error = function(e) NULL)
  if (is.null(fit)) return(data.frame(n = nrow(df), r_squared = NA_real_, p_value = NA_real_))
  s <- summary(fit)
  fstat <- s$fstatistic
  p_val <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
  data.frame(n = nrow(df), r_squared = s$r.squared, p_value = as.numeric(p_val))
}

panel_stats_log <- plot_data %>%
  group_by(Surface_label, Analyte) %>%
  do(fit_log_stats(.)) %>%
  ungroup() %>%
  mutate(label = sprintf("n=%d\nR2=%.2f\np=%.3f", n, r_squared, p_value))

cat("\nPer-panel logarithmic fit (Recovery ~ log(Pressure)):\n")
print(as.data.frame(panel_stats_log))

p_log <- ggplot(plot_data, aes(x = Mean_Pressure, y = Recovery_pct)) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = TRUE,
              color = "black", linewidth = 0.8, fill = "gray70", alpha = 0.35) +
  geom_point(aes(color = Study), size = 2.3, alpha = 0.85) +
  geom_text(data = panel_stats_log, aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3, color = "gray20", lineheight = 0.95) +
  scale_color_manual(values = pal_study) +
  facet_grid(rows = vars(Surface_label), cols = vars(Analyte)) +
  labs(
    title = sprintf("Recovery vs Actual Applied Mass -- Pilot + Main Study (n=%d) -- Logarithmic Fit", nrow(plot_data)),
    subtitle = "Unified x-axis scale across all panels | Logarithmic trend (Recovery ~ log(Mass)) + 95% CI fitted per panel, pooling Pilot+Main",
    x = "Actual mean applied mass (g, stable regions)", y = "Recovery (%)", color = "Study"
  ) +
  base_theme

ggsave(OUTPUT_LOGARITHMIC, p_log, width = 11, height = 8, dpi = 300)
cat(sprintf("Logarithmic plot saved to: %s\n", OUTPUT_LOGARITHMIC))

cat("\nDone.\n")
