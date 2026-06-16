# ============================================================
# DriftCorrectionComparison.R
# One-off diagnostic: compare 3 PETN drift correction methods
# using 0.2ng QC level fitting data with different treatments:
#   A) 0.2ng QCs, no anchor
#   B) 0.2ng QCs + Cal point included, no anchor
#   C) 0.2ng QCs, post-hoc anchor to Cal
#
# Source AFTER running the main pipeline (Combined must exist)
# Requires in workspace:
#   Combined, stored_petn_pa_models, solve_concentration(),
#   Results.dir, qc_bias_limit
# ============================================================

library(dplyr)
library(ggplot2)

message("=== PETN Drift Correction Method Comparison (0.2ng QC) ===")

# --- 1. Setup: identify QC levels, reference PAs, fitting data ---

qc_all <- Combined %>%
  filter(Type == "QC", !is.na(petn_pa), !is.na(CalLevel))

if (nrow(qc_all) == 0) stop("No usable QC rows with PETN PA found in Combined.")

all_qc_levels <- sort(unique(qc_all$CalLevel))
min_qc_level  <- min(all_qc_levels)

message("  QC levels found: ", paste(all_qc_levels, collapse = ", "), " ng")
message("  Using low level for drift model: ", min_qc_level, " ng")

# Reference PA for low-level model: mean Cal Set 1 PA at min QC level
ref_pa_low <- Combined %>%
  filter(Type == "Cal", CalLevel == min_qc_level,
         !is.na(petn_pa), CalibrationSet == 1) %>%
  summarise(ref = mean(petn_pa, na.rm = TRUE)) %>%
  pull(ref)

message("  Reference Cal PA (", min_qc_level, " ng): ", round(ref_pa_low, 0))

# Cal Set 1 mean row (for anchor normalisation)
cal_rows <- Combined$Row[Combined$Type == "Cal" & Combined$CalibrationSet == 1]
anchor_row <- mean(cal_rows)

# --- 2. Helper: fit model, apply correction, compute QC %bias ---

compute_bias_for_method <- function(fitting_data, anchor = FALSE, method_label = "") {
  # fitting_data: data.frame with columns Row, PA_CorrFactor
  # anchor: if TRUE, normalise CFs so CF=1.0 at mean Cal Set 1 position
  # Returns: data.frame of ALL QC rows with corrected %bias

  if (nrow(fitting_data) < 2) {
    message("  [", method_label, "] Fewer than 2 fitting points -- skipping.")
    return(NULL)
  }

  # Fit model
  if (nrow(fitting_data) >= 4) {
    model <- lm(PA_CorrFactor ~ Row + I(Row^2), data = fitting_data)
    if (summary(model)$r.squared < 0.5) {
      model <- lm(PA_CorrFactor ~ Row, data = fitting_data)
    }
  } else if (nrow(fitting_data) == 3) {
    model <- lm(PA_CorrFactor ~ Row + I(Row^2), data = fitting_data)
  } else {
    model <- lm(PA_CorrFactor ~ Row, data = fitting_data)
  }

  # Predict CFs at all injection positions
  pred_cf <- predict(model, newdata = data.frame(Row = Combined$Row))

  # Anchor normalisation (if requested)
  if (anchor) {
    anchor_cf <- predict(model, newdata = data.frame(Row = anchor_row))
    if (is.finite(anchor_cf) && anchor_cf > 0) {
      pred_cf <- pred_cf / anchor_cf
    }
  }

  # Clamp negative CFs

  pred_cf[pred_cf < 0] <- 1.0

  # Apply correction to PETN PA
  corrected_pa <- Combined$petn_pa * pred_cf

  # Re-quantify through stored calibration models
  corrected_conc <- rep(NA_real_, nrow(Combined))

  for (model_idx in seq_along(stored_petn_pa_models)) {
    m <- stored_petn_pa_models[[model_idx]]
    for (j in m$rows) {
      # Skip Below_LOD / Below_LOQ
      row_snr_flag <- NA_character_
      if (m$snr_flag_col %in% names(Combined)) {
        row_snr_flag <- Combined[[m$snr_flag_col]][j]
      }
      if (!is.na(row_snr_flag) && row_snr_flag %in% c("Below_LOD", "Below_LOQ")) next
      if (is.na(corrected_pa[j])) next

      res <- solve_concentration(
        corrected_pa[j],
        m$lin_model,
        m$cal_y_range,
        m$quad_model
      )
      corrected_conc[j] <- res$value
    }
  }

  # Compute %bias for QC rows
  qc_result <- Combined %>%
    mutate(corrected_conc = corrected_conc,
           drift_cf = pred_cf) %>%
    filter(Type == "QC", !is.na(corrected_conc), !is.na(TrueCalConcAdj)) %>%
    mutate(
      PercentBias = (corrected_conc - TrueCalConcAdj) / TrueCalConcAdj * 100,
      Method = method_label
    ) %>%
    select(Row, CalLevel, PercentBias, Method, drift_cf)

  r2 <- round(summary(model)$r.squared, 3)
  message("  [", method_label, "] R2=", r2,
          ", ", nrow(fitting_data), " fitting points",
          ", QC bias range: ",
          round(min(qc_result$PercentBias, na.rm = TRUE), 1), "% to ",
          round(max(qc_result$PercentBias, na.rm = TRUE), 1), "%")

  return(qc_result)
}

# --- 3. Build fitting data for each method ---

# A) 0.2ng QCs, no anchor
fitting_A <- Combined %>%
  filter(Type == "QC", CalLevel == min_qc_level, !is.na(petn_pa)) %>%
  mutate(PA_CorrFactor = ref_pa_low / petn_pa) %>%
  filter(is.finite(PA_CorrFactor), PA_CorrFactor > 0) %>%
  select(Row, PA_CorrFactor)

# B) 0.2ng QCs + Cal point included, no anchor
cal_fitting_point <- Combined %>%
  filter(Type == "Cal", CalLevel == min_qc_level,
         !is.na(petn_pa), CalibrationSet == 1) %>%
  mutate(PA_CorrFactor = ref_pa_low / petn_pa) %>%
  filter(is.finite(PA_CorrFactor), PA_CorrFactor > 0) %>%
  select(Row, PA_CorrFactor)

fitting_B <- bind_rows(cal_fitting_point, fitting_A)

# C) 0.2ng QCs, post-hoc anchor to Cal (same fitting data as A, anchor=TRUE)
fitting_C <- fitting_A

message("\n  Fitting data sizes:")
message("    A (", min_qc_level, " ng QCs, no anchor): ", nrow(fitting_A), " points")
message("    B (", min_qc_level, " ng QCs + Cal, no anchor): ", nrow(fitting_B), " points")
message("    C (", min_qc_level, " ng QCs, anchored): ", nrow(fitting_C), " points")

# --- 4. Apply each method ---

message("\n  Computing corrected QC bias for each method...")

bias_A <- compute_bias_for_method(fitting_A, anchor = FALSE,
                                   method_label = paste0("A: ", min_qc_level, " ng QCs (no anchor)"))

bias_B <- compute_bias_for_method(fitting_B, anchor = FALSE,
                                   method_label = paste0("B: ", min_qc_level, " ng QCs + Cal (no anchor)"))

bias_C <- compute_bias_for_method(fitting_C, anchor = TRUE,
                                   method_label = paste0("C: ", min_qc_level, " ng QCs (anchored to Cal)"))

# --- 5. Combine and plot ---

all_bias <- bind_rows(bias_A, bias_B, bias_C) %>%
  filter(!is.na(PercentBias))

if (nrow(all_bias) == 0) {
  message("  No bias data to plot -- aborting comparison.")
} else {

  # Set facet order
  method_levels <- unique(c(
    if (!is.null(bias_A)) unique(bias_A$Method),
    if (!is.null(bias_B)) unique(bias_B$Method),
    if (!is.null(bias_C)) unique(bias_C$Method)
  ))
  all_bias$Method <- factor(all_bias$Method, levels = method_levels)

  p_compare <- ggplot(all_bias, aes(x = Row, y = PercentBias,
                                     colour = factor(CalLevel))) +
    geom_point(size = 2.5) +
    geom_line(aes(group = factor(CalLevel)), alpha = 0.4) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_hline(yintercept = c(-qc_bias_limit, qc_bias_limit),
               linetype = "dashed", colour = "red") +
    facet_wrap(~Method, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(
      title = "PETN Drift Correction: Method Comparison (0.2 ng QC)",
      subtitle = "QC % Bias after applying each drift correction strategy",
      x = "Injection Number",
      y = "% Bias",
      colour = "QC Level (ng)"
    )

  out_path <- file.path(Results.dir, "PETN_DriftCorrection_MethodComparison.png")
  ggsave(out_path, p_compare, width = 10, height = 9, dpi = 300)
  message("\n  Comparison plot saved: ", out_path)
}

message("=== Drift Correction Comparison Complete ===")

# ============================================================
# 6. Before/After PA Plot (6ng QCs, anchored drift correction)
# ============================================================

message("\n=== PETN Drift Correction: Before vs After (6 ng QC) ===")

# Identify highest QC level
all_qc_levels_high <- sort(unique(Combined$CalLevel[Combined$Type == "QC" & !is.na(Combined$CalLevel)]))
max_qc_level <- max(all_qc_levels_high)

# Reference PA for high-level: mean Cal Set 1 PA
ref_pa_high <- Combined %>%
  filter(Type == "Cal", CalLevel == max_qc_level,
         !is.na(petn_pa), CalibrationSet == 1) %>%
  summarise(ref = mean(petn_pa, na.rm = TRUE)) %>%
  pull(ref)

# 6ng QC data
qc_high <- Combined %>%
  filter(Type == "QC", CalLevel == max_qc_level, !is.na(petn_pa)) %>%
  mutate(PA_CorrFactor = ref_pa_high / petn_pa) %>%
  filter(is.finite(PA_CorrFactor), PA_CorrFactor > 0)

if (nrow(qc_high) >= 2) {

  # Fit drift model to 6ng QCs
  if (nrow(qc_high) >= 4) {
    model_high <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_high)
    if (summary(model_high)$r.squared < 0.5) {
      model_high <- lm(PA_CorrFactor ~ Row, data = qc_high)
    }
  } else if (nrow(qc_high) == 3) {
    model_high <- lm(PA_CorrFactor ~ Row + I(Row^2), data = qc_high)
  } else {
    model_high <- lm(PA_CorrFactor ~ Row, data = qc_high)
  }

  # Anchor normalisation
  anchor_cf_high <- predict(model_high, newdata = data.frame(Row = anchor_row))
  pred_cf_high <- predict(model_high, newdata = data.frame(Row = qc_high$Row))
  anchored_cf <- pred_cf_high / anchor_cf_high
  anchored_cf[anchored_cf < 0] <- 1.0

  # Corrected PA
  qc_high$petn_pa_corrected <- qc_high$petn_pa * anchored_cf

  # Build plot data (long format)
  plot_before <- qc_high %>%
    select(Row, Line, petn_pa) %>%
    mutate(Panel = "Before Drift Correction", PA = petn_pa)

  plot_after <- qc_high %>%
    select(Row, Line, petn_pa_corrected) %>%
    mutate(Panel = "After Drift Correction (Anchored)", PA = petn_pa_corrected)

  plot_ba <- bind_rows(plot_before, plot_after) %>%
    mutate(Panel = factor(Panel, levels = c("Before Drift Correction",
                                             "After Drift Correction (Anchored)")))

  p_before_after <- ggplot(plot_ba, aes(x = Row, y = PA)) +
    geom_point(size = 2.5, colour = "steelblue") +
    geom_line(alpha = 0.4, colour = "steelblue") +
    geom_hline(yintercept = ref_pa_high, linetype = "dashed", colour = "forestgreen", linewidth = 0.7) +
    facet_wrap(~Panel, ncol = 1, scales = "free_y") +
    expand_limits(y = 0) +
    theme_bw() +
    labs(
      title = paste0("PETN Drift Correction: Before vs After (", max_qc_level, " ng QC)"),
      subtitle = "Dashed green line = Cal Set 1 reference PA",
      x = "Injection Number",
      y = "PETN Peak Area (Arb. Units)"
    )

  out_path_ba <- file.path(Results.dir, "PETN_DriftCorrection_BeforeAfter.png")
  ggsave(out_path_ba, p_before_after, width = 10, height = 7, dpi = 300)
  message("  Before/After plot saved: ", out_path_ba)

} else {
  message("  Fewer than 2 high-level QCs -- skipping Before/After plot.")
}

message("=== All Drift Correction Diagnostics Complete ===")
