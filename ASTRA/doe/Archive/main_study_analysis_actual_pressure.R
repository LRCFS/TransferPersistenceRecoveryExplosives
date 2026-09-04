#===============================================================================
# MAIN STUDY ANALYSIS -- ACTUAL (ASTRA-measured) PRESSURE VARIANT
#
# Companion to main_study_analysis.R (LEFT COMPLETELY UNTOUCHED -- this is a
# separate, new script/output, not a replacement, specifically so the two
# can be compared side by side).
#
# main_study_analysis.R's Section 5 combined model uses NOMINAL/target
# Pressure_g (10/50/100/200/300) as the pressure predictor. This script
# fits the EXACT SAME model architecture (same formula shape, same Type III
# ANOVA, same effect-size/EMM/power-analysis sections, same pooling of the
# Pilot Study's existing 50g/200g wet-only samples with a Study covariate)
# but substitutes the ASTRA-measured ACTUAL achieved pressure
# (Mean_Pressure, from each study's own Pressure Traces/ProcessedData/
# batch_summary.csv, produced by ASTRA/main_study_batch_process.R and
# ASTRA's own pilot-study batch script respectively) in place of the
# nominal Pressure_g -- directly testing whether using the real achieved
# pressure sharpens (or weakens) the already-significant nominal-pressure
# result documented in main_study_analysis_summary.txt.
#
# Unlike main_study_analysis.R, this script does NOT re-run GC-MS
# collation (InjectionAcceptance.R etc.) -- it reads the ALREADY-COLLATED
# main_study_data_nested.csv / pilot_data_nested.csv directly (both
# produced by the existing pilot_analysis.R / main_study_analysis.R
# pipelines) and only adds the actual-pressure join + refit on top.
#
# Output:
#   - main_study_analysis_summary_ACTUAL_PRESSURE.txt (in the SAME Main
#     Study OneDrive folder as the existing main_study_analysis_summary.txt,
#     under a clearly different name so neither overwrites the other)
#
# Author: OpenCode
# Date: 2026-08-25
#===============================================================================

#===============================================================================
# USER CONFIGURATION (mirrors main_study_analysis.R's own config where
# applicable)
#===============================================================================

use_qc_filtered_data <- TRUE   # matches main_study_analysis.R's own default

alpha_level <- 0.05
meaningful_recovery_difference_pct <- 10   # matches main_study_analysis.R's own a priori target
target_power <- 0.80
n_target_per_level <- 6   # matches doe_nested_design.R's own n=6/level/surface-type design target

astra_swabbing_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing"
main_study_dir  <- file.path(astra_swabbing_dir, "Main Study")
pilot_study_dir <- file.path(astra_swabbing_dir, "Pilot Study")

main_data_file  <- file.path(main_study_dir, "main_study_data_nested.csv")
pilot_data_file <- file.path(pilot_study_dir, "pilot_data_nested.csv")

main_pressure_file  <- file.path(main_study_dir, "Pressure Traces/ProcessedData/batch_summary.csv")
pilot_pressure_file <- file.path(pilot_study_dir, "Pressure Traces/ProcessedData/batch_summary.csv")

stats_summary_file <- file.path(main_study_dir, "main_study_analysis_summary_ACTUAL_PRESSURE.txt")

#===============================================================================
# PACKAGE LOADING
#===============================================================================

suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(dplyr)
  library(car)
  library(effectsize)
  library(emmeans)
  library(pwr)
})

#===============================================================================
# SECTION 1: LOAD ALREADY-COLLATED DATA (no GC-MS recollation here)
#===============================================================================

for (f in c(main_data_file, pilot_data_file, main_pressure_file, pilot_pressure_file)) {
  if (!file.exists(f)) stop("Required input file not found: ", f)
}

main_data_all  <- read.csv(main_data_file, stringsAsFactors = FALSE)
pilot_data_all <- read.csv(pilot_data_file, stringsAsFactors = FALSE)
main_pressure  <- read.csv(main_pressure_file, stringsAsFactors = FALSE)
pilot_pressure <- read.csv(pilot_pressure_file, stringsAsFactors = FALSE)

cat("Loaded", nrow(main_data_all), "Main Study rows,", nrow(pilot_data_all), "Pilot Study rows,\n")
cat("       ", nrow(main_pressure), "Main pressure-trace rows,", nrow(pilot_pressure), "Pilot pressure-trace rows\n\n")

#===============================================================================
# SECTION 2: QC FILTER (mirrors main_study_analysis.R's use_qc_filtered_data
# toggle exactly) + JOIN ACTUAL PRESSURE
#===============================================================================

main_data <- main_data_all %>% filter(is.na(SampleType) | SampleType != "NC")
if (use_qc_filtered_data) {
  n_before <- nrow(main_data)
  main_data <- main_data %>% filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS", "PASS*"))
  cat("Main Study QC FILTER: retained", nrow(main_data), "of", n_before, "rows (PASS/PASS* only, NC excluded)\n")
} else {
  cat("Main Study QC FILTER: DISABLED\n")
}

main_data <- main_data %>%
  left_join(main_pressure %>% select(Sample, Mean_Pressure), by = c("RunID" = "Sample")) %>%
  mutate(Study = "Main")

n_missing_main <- sum(is.na(main_data$Mean_Pressure))
if (n_missing_main > 0) {
  cat("  [NOTE]", n_missing_main, "Main Study sample(s) have no matching actual-pressure result:",
      paste(main_data$RunID[is.na(main_data$Mean_Pressure)], collapse = ", "), "\n")
}

pilot_pool <- pilot_data_all %>%
  filter(Solvent_level == "present", Pressure_g %in% c(50, 200), is.na(SampleType) | SampleType != "NC")
if (use_qc_filtered_data) {
  n_before <- nrow(pilot_pool)
  pilot_pool <- pilot_pool %>% filter(analysis_accepted %in% c("PASS", "PASS*"))
  cat("Pilot Study QC FILTER: retained", nrow(pilot_pool), "of", n_before,
      "wet 50g/200g rows (PASS/PASS* only)\n")
} else {
  cat("Pilot Study QC FILTER: DISABLED (wet 50g/200g rows only)\n")
}

pilot_pool <- pilot_pool %>%
  left_join(pilot_pressure %>% select(Sample, Mean_Pressure), by = c("RunID" = "Sample")) %>%
  mutate(Study = "Pilot")

n_missing_pilot <- sum(is.na(pilot_pool$Mean_Pressure))
if (n_missing_pilot > 0) {
  cat("  [NOTE]", n_missing_pilot, "Pilot Study sample(s) have no matching actual-pressure result:",
      paste(pilot_pool$RunID[is.na(pilot_pool$Mean_Pressure)], collapse = ", "), "\n")
}

cat("\n")

#===============================================================================
# SECTION 3: POOL + BUILD SurfaceID_nested (mirrors main_study_analysis.R's
# build_pooled_dataset(), minus the CumulativeUseNumber approximation -- out
# of scope for this actual-pressure comparison script)
#===============================================================================

common_cols <- intersect(names(pilot_pool), names(main_data))
combined_data <- bind_rows(
  pilot_pool %>% select(all_of(common_cols)),
  main_data  %>% select(all_of(common_cols))
) %>%
  filter(!is.na(Mean_Pressure)) %>%   # drop samples with no matched actual-pressure result
  mutate(
    Surface_type = factor(Surface_type, levels = c("steel", "abs")),
    Study = factor(Study, levels = c("Pilot", "Main")),
    SurfaceID_nested = paste(Surface_type, SurfaceID, sep = ":")
  )

cat("Combined (pilot-pooled + main) dataset, actual-pressure variant:", nrow(combined_data), "rows",
    "(", sum(combined_data$Study == "Pilot"), "pooled pilot +", sum(combined_data$Study == "Main"), "main)\n\n")

#===============================================================================
# SECTION 4: COMBINED MIXED-EFFECTS MODEL, ACTUAL PRESSURE
#
# Recovery ~ Surface_type * (Mean_Pressure + I(Mean_Pressure^2)) + Study +
#            (1|SurfaceID_nested)
# -- identical formula shape to main_study_analysis.R's Section 5, with
# Mean_Pressure (actual) substituted for Pressure_g (nominal).
#===============================================================================

fit_combined_model_actual <- function(data, recovery_col, analyte_name) {

  data_clean <- data[!is.na(data[[recovery_col]]) & !is.na(data$Mean_Pressure), ]

  n_nominal_levels <- length(unique(data_clean$Pressure_g))
  n_surfaces       <- length(unique(data_clean$SurfaceID_nested))
  n_surface_types  <- length(unique(data_clean$Surface_type))

  cat("\n--- ", analyte_name, " combined model (ACTUAL pressure) ---\n", sep = "")
  cat("  n =", nrow(data_clean), " | distinct nominal Target levels =", n_nominal_levels,
      " | distinct physical surfaces =", n_surfaces, " | surface types =", n_surface_types, "\n")
  cat("  Actual Mean_Pressure range: ", round(min(data_clean$Mean_Pressure), 1), "-",
      round(max(data_clean$Mean_Pressure), 1), "g\n", sep = "")

  if (nrow(data_clean) < 10 || n_nominal_levels < 4 || n_surface_types < 2) {
    cat("  [NOT ENOUGH DATA YET] Need >=10 observations, >=4 distinct nominal levels,\n",
        "  and both surface types represented. Currently: n=", nrow(data_clean),
        ", levels=", n_nominal_levels, ", surface types=", n_surface_types, ".\n", sep = "")
    return(list(ok = FALSE, reason = "insufficient data"))
  }

  formula_base <- as.formula(paste0(
    recovery_col, " ~ Surface_type * (Mean_Pressure + I(Mean_Pressure^2)) + Study + (1|SurfaceID_nested)"
  ))

  model_base <- tryCatch(
    lmer(formula_base, data = data_clean, REML = TRUE),
    error = function(e) { cat("  [ERROR] Model failed to fit: ", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(model_base)) return(list(ok = FALSE, reason = "model fit failed"))

  if (isSingular(model_base)) {
    cat("  [WARNING] Boundary (singular) fit: between-surface variance estimated at ~0.\n")
  }

  cat("\nType III ANOVA (base model, ACTUAL pressure):\n")
  anova_base <- tryCatch(car::Anova(model_base, type = 3, test.statistic = "F"), error = function(e) NULL)
  if (!is.null(anova_base)) print(anova_base)

  eff <- tryCatch(effectsize::eta_squared(anova_base, partial = TRUE), error = function(e) NULL)
  if (!is.null(eff)) {
    cat("\nEffect sizes (partial eta^2):\n")
    print(eff)
  }

  # EMMs at the same round-number anchor points used by the nominal-pressure
  # analysis (10/50/100/200/300), for direct visual comparability -- these
  # are now query points on the continuous actual-pressure scale, not
  # discrete design levels.
  emm <- tryCatch(
    emmeans(model_base, ~ Surface_type | Mean_Pressure, at = list(Mean_Pressure = c(10, 50, 100, 200, 300))),
    error = function(e) { cat("  [NOTE] emmeans failed: ", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(emm)) {
    cat("\nEstimated marginal means at Mean_Pressure = 10/50/100/200/300 (anchor points):\n")
    print(emm)
  }

  shapiro_resid <- tryCatch(shapiro.test(residuals(model_base)), error = function(e) NULL)
  if (!is.null(shapiro_resid)) cat("\nShapiro-Wilk (residuals): p =", round(shapiro_resid$p.value, 4), "\n")

  ranef_vals <- tryCatch(unlist(ranef(model_base)$SurfaceID_nested), error = function(e) NULL)
  if (!is.null(ranef_vals) && length(unique(ranef_vals)) > 1) {
    shapiro_ranef <- tryCatch(shapiro.test(ranef_vals), error = function(e) NULL)
    if (!is.null(shapiro_ranef)) cat("Shapiro-Wilk (random effects): p =", round(shapiro_ranef$p.value, 4), "\n")
  } else {
    cat("Shapiro-Wilk (random effects): skipped -- BLUPs are constant (boundary/singular fit).\n")
  }

  list(ok = TRUE, model = model_base, anova = anova_base, effect_sizes = eff,
       emm = emm, data_clean = data_clean,
       n_pressure_levels = n_nominal_levels, n_surface_types = n_surface_types)
}

#===============================================================================
# SECTION 5: PER-EFFECT POWER ANALYSIS + A PRIORI/MDES CHECK
# (identical logic/formulas to main_study_analysis.R's Sections 6/6b -- see
# that script's own extensive comments for the full rationale; not
# reproduced here to keep this file focused on what's different: the
# pressure predictor itself)
#===============================================================================

run_power_analysis <- function(fit, analyte_name, n_target = n_target_per_level, alpha = alpha_level) {
  if (is.null(fit) || !isTRUE(fit$ok) || is.null(fit$effect_sizes)) {
    cat("\n[NOTE] ", analyte_name, ": no fitted model/effect sizes available -- skipping power analysis.\n", sep = "")
    return(invisible(NULL))
  }

  eff <- fit$effect_sizes
  n_pressure_levels <- fit$n_pressure_levels
  n_surface_types   <- fit$n_surface_types

  k_map <- c(
    "Surface_type"                            = n_surface_types,
    "Mean_Pressure"                           = n_pressure_levels,
    "I(Mean_Pressure^2)"                      = n_pressure_levels,
    "Study"                                   = 2,
    "Surface_type:Mean_Pressure"              = n_surface_types * n_pressure_levels,
    "Surface_type:I(Mean_Pressure^2)"         = n_surface_types * n_pressure_levels
  )

  eta2_to_f <- function(eta2) {
    eta2 <- max(eta2, 0)
    if (eta2 >= 1) return(Inf)
    sqrt(eta2 / (1 - eta2))
  }
  power_at_n <- function(k, n, f) {
    if (is.na(f) || is.infinite(f) || f <= 0 || k < 2) return(NA_real_)
    tryCatch(pwr::pwr.anova.test(k = k, n = n, f = f, sig.level = alpha)$power, error = function(e) NA_real_)
  }
  min_n_for_80pct <- function(k, f) {
    if (is.na(f) || is.infinite(f) || f <= 0 || k < 2) return(NA_real_)
    tryCatch(ceiling(pwr::pwr.anova.test(k = k, f = f, sig.level = alpha, power = 0.80)$n), error = function(e) NA_real_)
  }

  cat("\n--- ", analyte_name, ": per-effect power analysis, ACTUAL pressure (target n=", n_target,
      "/level/surface-type) ---\n", sep = "")
  cat("[IMPORTANT CAVEAT] Retrospective/observed power, computed from the same data being\n",
      "judged -- see main_study_analysis.R's own extensive caveat (Hoenig & Heisey 2001).\n",
      "Treat as a rough planning check only.\n\n", sep = "")

  results <- list()
  for (i in seq_len(nrow(eff))) {
    term <- eff$Parameter[i]
    if (term == "(Intercept)" || !(term %in% names(k_map))) next
    k <- unname(k_map[term])
    if (is.na(k) || k < 2) next

    eta2_opt <- eff$Eta2_partial[i]
    eta2_low <- max(eff$CI_low[i], 0)

    f_opt <- eta2_to_f(eta2_opt)
    f_con <- if (eta2_low <= 1e-6) NA_real_ else eta2_to_f(eta2_low)

    results[[length(results) + 1]] <- data.frame(
      Effect = term, k_groups = k,
      Eta2_partial = round(eta2_opt, 3), Eta2_CI_low = round(eta2_low, 3),
      Cohens_f_optimistic = round(f_opt, 3),
      Cohens_f_conservative = if (is.na(f_con)) NA_real_ else round(f_con, 3),
      Power_at_n6_optimistic = round(power_at_n(k, n_target, f_opt), 3),
      Power_at_n6_conservative = round(power_at_n(k, n_target, f_con), 3),
      MinN_for_80pct_optimistic = min_n_for_80pct(k, f_opt),
      MinN_for_80pct_conservative = min_n_for_80pct(k, f_con),
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) {
    cat("  No effects with a recognised k mapping -- nothing to report.\n")
    return(invisible(NULL))
  }

  power_table <- bind_rows(results)
  print(power_table, row.names = FALSE)
  invisible(power_table)
}

get_overall_sd <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  resid_var <- sum(vc$vcov[is.na(vc$grp) | vc$grp == "Residual"], na.rm = TRUE)
  ranef_var <- sum(vc$vcov[!is.na(vc$grp) & vc$grp != "Residual"], na.rm = TRUE)
  sqrt(resid_var + ranef_var)
}

run_apriori_power_check <- function(fit, analyte_name, target_range_pct = meaningful_recovery_difference_pct,
                                     n_target = n_target_per_level, alpha = alpha_level, power_goal = target_power) {
  if (is.null(fit) || !isTRUE(fit$ok)) {
    cat("\n[NOTE] ", analyte_name, ": no fitted model available -- skipping a priori/MDES power check.\n", sep = "")
    return(invisible(NULL))
  }

  sigma_total <- tryCatch(get_overall_sd(fit$model), error = function(e) NA_real_)
  k <- fit$n_pressure_levels

  if (is.na(sigma_total) || sigma_total <= 0 || k < 2) {
    cat("\n[NOTE] ", analyte_name, ": could not extract a usable variance estimate -- skipping.\n", sep = "")
    return(invisible(NULL))
  }

  f_target <- target_range_pct / (2 * sqrt(3) * sigma_total)
  power_at_target <- tryCatch(pwr::pwr.anova.test(k = k, n = n_target, f = f_target, sig.level = alpha)$power, error = function(e) NA_real_)
  min_n_for_target <- tryCatch(ceiling(pwr::pwr.anova.test(k = k, f = f_target, sig.level = alpha, power = power_goal)$n), error = function(e) NA_real_)
  f_mdes <- tryCatch(pwr::pwr.anova.test(k = k, n = n_target, sig.level = alpha, power = power_goal)$f, error = function(e) NA_real_)
  range_mdes <- if (!is.na(f_mdes)) f_mdes * 2 * sqrt(3) * sigma_total else NA_real_

  cat("\n--- ", analyte_name, ": A PRIORI / MDES power check, ACTUAL pressure (non-circular) ---\n", sep = "")
  cat("  Total noise SD (residual + between-surface, from the FITTED model): ", round(sigma_total, 2), " percentage points\n", sep = "")
  cat("  Target effect (decided independently of this study's own data): a range of ",
      target_range_pct, " percentage points across the ", k, " nominal pressure levels\n", sep = "")
  cat("  -> Cohen's f for this target: ", round(f_target, 3), "\n", sep = "")
  cat("  -> Power to detect this target at n=", n_target, "/level: ",
      ifelse(is.na(power_at_target), "n/a", paste0(round(100 * power_at_target), "%")), "\n", sep = "")
  cat("  -> Minimum n/level for ", round(100 * power_goal), "% power to detect this target: ",
      ifelse(is.na(min_n_for_target), "n/a", min_n_for_target), "\n", sep = "")
  cat("\n  Minimum Detectable Effect Size (MDES) at n=", n_target, "/level, ", round(100 * power_goal),
      "% power: a range of ", ifelse(is.na(range_mdes), "n/a", round(range_mdes, 1)),
      " percentage points across the ", k, " nominal pressure levels.\n", sep = "")

  invisible(list(sigma_total = sigma_total, f_target = f_target, power_at_target = power_at_target,
                 min_n_for_target = min_n_for_target, range_mdes = range_mdes))
}

#===============================================================================
# SECTION 6: RUN EVERYTHING, PERSIST TO FILE (mirrors main_study_analysis.R's
# sink() pattern exactly)
#===============================================================================

sink_con <- file(stats_summary_file, open = "wt", encoding = "UTF-8")
sink(sink_con, split = TRUE)

tryCatch({
  cat("========================================\n")
  cat("MAIN STUDY ANALYSIS SUMMARY -- ACTUAL (ASTRA-MEASURED) PRESSURE VARIANT\n")
  cat("Generated:", format(Sys.time()), "\n")
  cat("Companion to: main_study_analysis_summary.txt (nominal Pressure_g) -- NOT a replacement\n")
  cat("main_data_file:", main_data_file, "\n")
  cat("pilot_data_file:", pilot_data_file, "\n")
  cat("main_pressure_file (actual pressure):", main_pressure_file, "\n")
  cat("pilot_pressure_file (actual pressure):", pilot_pressure_file, "\n")
  cat("use_qc_filtered_data:", use_qc_filtered_data, "\n")
  cat("========================================\n\n")

  petn_fit <- fit_combined_model_actual(combined_data, "PETN_Recovery_pct", "PETN")
  rdx_fit  <- fit_combined_model_actual(combined_data, "RDX_Recovery_pct", "RDX")

  run_power_analysis(petn_fit, "PETN")
  run_power_analysis(rdx_fit, "RDX")

  run_apriori_power_check(petn_fit, "PETN")
  run_apriori_power_check(rdx_fit, "RDX")

  cat("\n========================================\n")
  cat("MAIN STUDY ANALYSIS COMPLETE (ACTUAL PRESSURE VARIANT)\n")
  cat("========================================\n")
}, finally = {
  sink()
  close(sink_con)
})

cat("\nStatistical summary saved to:", stats_summary_file, "\n")
cat("(Compare directly against main_study_analysis_summary.txt, which uses nominal Pressure_g)\n")

