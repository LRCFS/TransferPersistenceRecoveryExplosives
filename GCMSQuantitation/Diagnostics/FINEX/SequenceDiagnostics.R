# ============================================================
# SequenceDiagnostics.R
# Diagnostic plots for a single FINEX sequence run:
#   1. QC Peak Area & Ratio trends across the sequence
#   2. Sample concentration box plots by surface (uncorrected)
#   3. Sample concentration box plots by surface (drift-corrected)
#
# Source AFTER running the main pipeline (Combined must exist)
# Requires in workspace:
#   Combined, Results.dir
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

message("=== FINEX Sequence Diagnostics ===")

# --- Configuration ---
# Set specific Line numbers (from sequence log) to include in QC plot, or NULL for all QCs
qc_lines_to_plot <- 1:52   # e.g. c(23, 41, 59, 77) or NULL for all


# --- 0. Parse Surface from SampleName for sample rows ---

sample_pattern <- "^Lab(\\d+)\\s+P(\\d+)\\s+(ABS-S|ABS-T|S|G)\\s+(NC|\\d+)$"

surface_map <- c(
 "S"     = "Steel",
 "G"     = "Glass",
 "ABS-S" = "ABS-Smooth",
 "ABS-T" = "ABS-Textured"
)

surface_order <- c("Glass", "Steel", "ABS-Smooth", "ABS-Textured")

samples <- Combined %>%
  filter(Type == "Sample") %>%
  mutate(
    SampleNameTrimmed = trimws(SampleName),
    parsed = regmatches(SampleNameTrimmed, regexec(sample_pattern, SampleNameTrimmed)),
    SurfaceCode = sapply(parsed, function(x) if (length(x) >= 4) x[4] else NA_character_),
    RepeatRaw   = sapply(parsed, function(x) if (length(x) >= 5) x[5] else NA_character_),
    SurfaceName = surface_map[SurfaceCode],
    IsNegativeControl = !is.na(RepeatRaw) & RepeatRaw == "NC"
  ) %>%
  select(-parsed, -SampleNameTrimmed)

# Exclude NCs and rows where surface couldn't be parsed
samples_valid <- samples %>%
  filter(!IsNegativeControl, !is.na(SurfaceName)) %>%
  mutate(SurfaceName = factor(SurfaceName, levels = surface_order))

message("  Samples parsed: ", nrow(samples_valid), " valid sample rows")
message("  Surfaces found: ", paste(unique(na.omit(samples_valid$SurfaceName)), collapse = ", "))

# ============================================================
# 1. QC Peak Area & Ratio Plot
# ============================================================

max_qc_level <- max(Combined$CalLevel[Combined$Type == "QC" & !is.na(Combined$CalLevel)], na.rm = TRUE)

qc_data <- Combined %>%
  filter(Type == "QC", CalLevel == max_qc_level)

if (!is.null(qc_lines_to_plot)) {
  qc_data <- qc_data %>% filter(Line %in% qc_lines_to_plot)
  message("  QC plot: filtered to ", nrow(qc_data), " rows (specified Line numbers)")
}

if (nrow(qc_data) > 0) {

  # PA facets
  qc_pa <- qc_data %>%
    select(Row, petn_pa, rdx_pa) %>%
    pivot_longer(cols = c(petn_pa, rdx_pa), names_to = "Metric", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    mutate(Metric = case_when(
      Metric == "petn_pa" ~ "PETN",
      Metric == "rdx_pa"  ~ "RDX"
    )) %>%
    mutate(Metric = factor(Metric, levels = c("PETN", "RDX")))

  p_pa <- ggplot(qc_pa, aes(x = Row, y = Value)) +
    geom_point(size = 2.5, colour = "steelblue") +
    geom_line(alpha = 0.4, colour = "steelblue") +
    facet_wrap(~Metric, ncol = 2, scales = "free_y") +
    expand_limits(y = 0) +
    theme_bw() +
    labs(x = "Injection Number", y = "Peak Area (Arb. Units)")

  # Ratio facets
  qc_ratio <- qc_data %>%
    select(Row, petn_ratio, rdx_ratio) %>%
    pivot_longer(cols = c(petn_ratio, rdx_ratio), names_to = "Metric", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    mutate(Metric = case_when(
      Metric == "petn_ratio" ~ "PETN / NG",
      Metric == "rdx_ratio"  ~ "RDX / 15N-RDX"
    )) %>%
    mutate(Metric = factor(Metric, levels = c("PETN / NG", "RDX / 15N-RDX")))

  p_ratio <- ggplot(qc_ratio, aes(x = Row, y = Value)) +
    geom_point(size = 2.5, colour = "steelblue") +
    geom_line(alpha = 0.4, colour = "steelblue") +
    facet_wrap(~Metric, ncol = 2, scales = "free_y") +
    expand_limits(y = 0) +
    theme_bw() +
    labs(x = "Injection Number", y = "Ratio")

  # Combine vertically
  p_qc <- gridExtra::arrangeGrob(
    p_pa, p_ratio, ncol = 1,
    top = grid::textGrob(
      paste0("QC Injection Trends Across Sequence (", max_qc_level, " ng)"),
      gp = grid::gpar(fontsize = 13, fontface = "bold"))
  )

  ggsave(file.path(Results.dir, "QC_PeakArea_Ratio.png"),
         p_qc, width = 10, height = 8, dpi = 300)
  message("  Saved: QC_PeakArea_Ratio.png")

} else {
  message("  No QC rows found -- skipping QC plot.")
}

# ============================================================
# 2. Sample Concentration Box Plots (Uncorrected)
# ============================================================

if (nrow(samples_valid) > 0) {

  box_data <- samples_valid %>%
    select(SurfaceName, petn_concentration_pa, rdx_concentration_ratio) %>%
    pivot_longer(
      cols = c(petn_concentration_pa, rdx_concentration_ratio),
      names_to = "Analyte",
      values_to = "Concentration"
    ) %>%
    filter(!is.na(Concentration)) %>%
    mutate(Analyte = case_when(
      Analyte == "petn_concentration_pa"    ~ "PETN",
      Analyte == "rdx_concentration_ratio"  ~ "RDX"
    )) %>%
    mutate(Analyte = factor(Analyte, levels = c("PETN", "RDX")))

  p_box <- ggplot(box_data, aes(x = SurfaceName, y = Concentration, fill = SurfaceName)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, colour = "black") +
    facet_wrap(~Analyte, scales = "free_y") +
    theme_bw() +
    guides(fill = "none") +
    labs(
      title = "Sample Concentration by Surface (Uncorrected)",
      subtitle = "Box = IQR, line = median, diamond = mean, points = outliers",
      x = "Surface",
      y = "Concentration (ng/\u00b5L)"
    )

  ggsave(file.path(Results.dir, "Sample_Concentration_BySurface.png"),
         p_box, width = 10, height = 5, dpi = 300)
  message("  Saved: Sample_Concentration_BySurface.png")

} else {
  message("  No valid sample rows -- skipping uncorrected box plot.")
}

# ============================================================
# 3. Sample Concentration Box Plots (Drift-Corrected)
# ============================================================

if (nrow(samples_valid) > 0 &&
    "petn_concentration_pa_dc" %in% names(samples_valid)) {

  box_data_dc <- samples_valid %>%
    select(SurfaceName, petn_concentration_pa_dc, rdx_concentration_ratio) %>%
    pivot_longer(
      cols = c(petn_concentration_pa_dc, rdx_concentration_ratio),
      names_to = "Analyte",
      values_to = "Concentration"
    ) %>%
    filter(!is.na(Concentration)) %>%
    mutate(Analyte = case_when(
      Analyte == "petn_concentration_pa_dc"   ~ "PETN (Drift-Corrected)",
      Analyte == "rdx_concentration_ratio"    ~ "RDX"
    )) %>%
    mutate(Analyte = factor(Analyte, levels = c("PETN (Drift-Corrected)", "RDX")))

  p_box_dc <- ggplot(box_data_dc, aes(x = SurfaceName, y = Concentration, fill = SurfaceName)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, colour = "black") +
    facet_wrap(~Analyte, scales = "free_y") +
    theme_bw() +
    guides(fill = "none") +
    labs(
      title = "Sample Concentration by Surface (Drift-Corrected)",
      subtitle = "Box = IQR, line = median, diamond = mean, points = outliers",
      x = "Surface",
      y = "Concentration (ng/\u00b5L)"
    )

  ggsave(file.path(Results.dir, "Sample_Concentration_BySurface_DC.png"),
         p_box_dc, width = 10, height = 5, dpi = 300)
  message("  Saved: Sample_Concentration_BySurface_DC.png")

} else {
  message("  No drift-corrected data available -- skipping DC box plot.")
}

message("=== Sequence Diagnostics Complete ===")
