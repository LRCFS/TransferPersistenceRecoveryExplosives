# ============================================================
# DriftCorrectionDiagram.R
# Explanatory diagram: illustrates how post-hoc anchor
# normalisation works for the PETN drift correction.
#
# Generates a three-panel figure:
#   Top: How correction factors are calculated (raw PA with annotation)
#   Middle: Raw drift model (fitted to 6ng QCs) showing CF values
#   Bottom: After anchor normalisation (CF forced to 1.0 at Cal)
#
# Source AFTER running the main pipeline (Combined must exist)
# Requires in workspace:
#   Combined, Results.dir
# ============================================================

library(dplyr)
library(ggplot2)
library(gridExtra)

message("=== Generating Drift Correction Anchor Diagram ===")

# --- 1. Setup ---

qc_all <- Combined %>%
  filter(Type == "QC", !is.na(petn_pa), !is.na(CalLevel))

max_qc_level <- max(qc_all$CalLevel)

# Reference PA: mean Cal Set 1 PA at max QC level
ref_pa <- Combined %>%
  filter(Type == "Cal", CalLevel == max_qc_level,
         !is.na(petn_pa), CalibrationSet == 1) %>%
  summarise(ref = mean(petn_pa, na.rm = TRUE)) %>%
  pull(ref)

# QC correction factors (6ng)
qc_drift <- Combined %>%
  filter(Type == "QC", CalLevel == max_qc_level, !is.na(petn_pa)) %>%
  mutate(PA_CorrFactor = ref_pa / petn_pa) %>%
  filter(is.finite(PA_CorrFactor), PA_CorrFactor > 0) %>%
  select(Row, petn_pa, PA_CorrFactor)

# Cal Set 1 position
cal_rows <- Combined$Row[Combined$Type == "Cal" & Combined$CalibrationSet == 1]
anchor_row <- mean(cal_rows)

# Cal point correction factor (should be ~1.0 since ref_pa is its own mean)
cal_cf <- Combined %>%
  filter(Type == "Cal", CalLevel == max_qc_level,
         !is.na(petn_pa), CalibrationSet == 1) %>%
  mutate(PA_CorrFactor = ref_pa / petn_pa) %>%
  summarise(Row = mean(Row), petn_pa = mean(petn_pa), PA_CorrFactor = mean(PA_CorrFactor))

# --- 2. Fit the drift model (QCs only) ---

if (nrow(qc_drift) >= 4) {
  drift_model <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_drift)
  if (summary(drift_model)$r.squared < 0.5) {
    drift_model <- lm(PA_CorrFactor ~ Row, data = qc_drift)
  }
} else if (nrow(qc_drift) == 3) {
  drift_model <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_drift)
} else {
  drift_model <- lm(PA_CorrFactor ~ Row, data = qc_drift)
}

# Prediction line across the full range
pred_line <- data.frame(
  Row = seq(min(c(anchor_row, min(qc_drift$Row))) - 2,
            max(qc_drift$Row) + 2, length.out = 200)
)
pred_line$CF_raw <- predict(drift_model, newdata = pred_line)

# Anchor CF: predicted value at Cal position
anchor_cf <- predict(drift_model, newdata = data.frame(Row = anchor_row))

# Normalised line
pred_line$CF_anchored <- pred_line$CF_raw / anchor_cf

# QC points (raw and anchored)
qc_drift$CF_anchored <- qc_drift$PA_CorrFactor / anchor_cf

# Cal point (raw and anchored)
cal_cf$CF_anchored <- cal_cf$PA_CorrFactor / anchor_cf

# --- 3. Panel 0: How correction factors are calculated ---

# Pick a mid-sequence QC point for the annotation
mid_idx <- which.min(abs(qc_drift$Row - median(qc_drift$Row)))
anno_point <- qc_drift[mid_idx, ]
anno_cf <- round(anno_point$PA_CorrFactor, 2)
anno_ref <- round(ref_pa, 0)
anno_meas <- round(anno_point$petn_pa, 0)

p_concept <- ggplot() +
  # QC PA points declining
  geom_point(data = qc_drift, aes(x = Row, y = petn_pa),
             colour = "red", size = 3.5, shape = 17) +
  # Cal PA point
  geom_point(data = cal_cf, aes(x = Row, y = petn_pa),
             colour = "forestgreen", size = 5, shape = 15) +
  # Reference PA line
  geom_hline(yintercept = ref_pa, linetype = "dashed", colour = "forestgreen", linewidth = 0.8) +
  # Label for reference line
  annotate("text", x = max(qc_drift$Row), y = ref_pa,
           label = paste0("Reference PA (Cal) = ", anno_ref),
           hjust = 1, vjust = -0.7, size = 3.5, colour = "forestgreen") +
  # Vertical arrow from annotated QC point to reference line
  annotate("segment", x = anno_point$Row, xend = anno_point$Row,
           y = anno_point$petn_pa, yend = ref_pa,
           arrow = arrow(length = unit(0.2, "cm"), ends = "both"),
           colour = "grey30", linewidth = 0.7) +
  # CF calculation label
  annotate("text", x = anno_point$Row + 2, 
           y = (anno_point$petn_pa + ref_pa) / 2,
           label = paste0("CF = ", anno_ref, " / ", anno_meas, " = ", anno_cf),
           hjust = 0, size = 3.5, colour = "grey30") +
  theme_bw() +
  labs(
    title = "Step 1: Calculate correction factor at each QC position",
    subtitle = paste0("CF = Reference Cal PA / Measured QC PA. ",
                      "As signal declines, CF increases to compensate."),
    x = "Injection Number",
    y = "PETN Peak Area"
  ) +
  theme(plot.title = element_text(size = 11, face = "bold"))

# --- 4. Panel A: Raw drift model ---

p_raw <- ggplot() +
  # Model curve
  geom_line(data = pred_line, aes(x = Row, y = CF_raw),
            colour = "steelblue", linewidth = 1.2) +
  # QC points
  geom_point(data = qc_drift, aes(x = Row, y = PA_CorrFactor),
             colour = "red", size = 3.5, shape = 17) +
  # Cal position
  geom_point(data = cal_cf, aes(x = Row, y = PA_CorrFactor),
             colour = "forestgreen", size = 5, shape = 15) +
  # Horizontal reference at 1.0
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
  # Vertical line at anchor position
  geom_vline(xintercept = anchor_row, linetype = "dotted", colour = "forestgreen", linewidth = 0.8) +
  # Annotation: anchor CF value
  annotate("text", x = anchor_row + 2, y = anchor_cf,
           label = paste0("CF at Cal = ", round(anchor_cf, 3)),
           hjust = 0, size = 3.5, colour = "forestgreen") +
  # Marker at anchor CF on the curve
  annotate("segment", x = anchor_row, xend = anchor_row,
           y = anchor_cf - 0.02, yend = anchor_cf + 0.02,
           colour = "forestgreen", linewidth = 1.5) +
  theme_bw() +
  labs(
    title = "Step 2: Fit drift model to QC correction factors",
    subtitle = paste0("Model fitted to ", max_qc_level,
                      " ng QC points (red triangles). Cal position shown (green square)."),
    x = "Injection Number",
    y = "PA Correction Factor (raw)"
  ) +
  theme(plot.title = element_text(size = 11, face = "bold"))

# --- 5. Panel B: After anchor normalisation ---

p_anchored <- ggplot() +
  # Normalised model curve
  geom_line(data = pred_line, aes(x = Row, y = CF_anchored),
            colour = "steelblue", linewidth = 1.2) +
  # Normalised QC points
  geom_point(data = qc_drift, aes(x = Row, y = CF_anchored),
             colour = "red", size = 3.5, shape = 17) +
  # Cal position (should now be at 1.0)
  geom_point(data = cal_cf, aes(x = Row, y = CF_anchored),
             colour = "forestgreen", size = 5, shape = 15) +
  # Horizontal reference at 1.0
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey40") +
  # Vertical line at anchor position
  geom_vline(xintercept = anchor_row, linetype = "dotted", colour = "forestgreen", linewidth = 0.8) +
  # Annotation
  annotate("text", x = anchor_row + 2, y = 1.0,
           label = "CF = 1.0 at Cal (no correction here)",
           hjust = 0, vjust = -0.7, size = 3.5, colour = "forestgreen") +
  theme_bw() +
  labs(
    title = "Step 3: Anchor normalisation (divide all CFs by predicted CF at Cal)",
    subtitle = paste0("CF forced to 1.0 at calibration position. ",
                      "Only progressive drift beyond the Cal is corrected."),
    x = "Injection Number",
    y = "PA Correction Factor (anchored)"
  ) +
  theme(plot.title = element_text(size = 11, face = "bold"))

# --- 6. Combine and save ---

p_combined <- arrangeGrob(p_concept, p_raw, p_anchored, ncol = 1,
                          top = grid::textGrob(
                            "PETN Drift Correction: Post-Hoc Anchor Normalisation",
                            gp = grid::gpar(fontsize = 13, fontface = "bold")))

out_path <- file.path(Results.dir, "PETN_DriftCorrection_AnchorDiagram.png")
ggsave(out_path, p_combined, width = 10, height = 12, dpi = 300)
message("  Diagram saved: ", out_path)

message("=== Anchor Diagram Complete ===")
