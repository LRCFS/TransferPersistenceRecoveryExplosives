# RDX Batch/Date Dependency: Does Adding Main Study Data Extend the
# Pilot-Only Finding?
# ===============================================================================
# The Pilot Study's own investigation (Design of Experiments/CONTEXT.md,
# "RDX Batch-to-Batch Recovery Investigation" + follow-ups) found a
# significant RDX-specific decline over calendar time within the Pilot
# Study alone (p~0.02-0.03 by BatchNumber, or p=0.0046 for a continuous
# "days since prep" covariate), with NO equivalent effect for PETN at any
# point. This script tests whether that same date-dependency (a) persists
# when the Main Study's own later dates are added to the same continuous
# timeline, and (b) whether RDX's `Study` (Pilot vs Main) effect found in
# main_study_analysis_actual_pressure.R (still significant, p=0.0027,
# after controlling for actual pressure) reflects a CONTINUATION of the
# within-Pilot decline (Main sitting even lower, consistent with more
# elapsed time) or a DISCONTINUITY (e.g. a fresh spike-stock reset making
# Main higher than where the within-Pilot trend would extrapolate to).
#
# Self-contained (does not source or modify main_study_analysis.R,
# main_study_analysis_actual_pressure.R, or pilot_analysis.R) -- rebuilds
# the same combined wet-only-pilot + main dataset (with actual pressure)
# directly, per this repo's own single-script convention.
#
# Author: OpenCode
# Date: 2026-08-25

suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(dplyr)
})

astra_swabbing_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing"
main_study_dir  <- file.path(astra_swabbing_dir, "Main Study")
pilot_study_dir <- file.path(astra_swabbing_dir, "Pilot Study")

main_data_all  <- read.csv(file.path(main_study_dir, "main_study_data_nested.csv"), stringsAsFactors = FALSE)
pilot_data_all <- read.csv(file.path(pilot_study_dir, "pilot_data_nested.csv"), stringsAsFactors = FALSE)
main_pressure  <- read.csv(file.path(main_study_dir, "Pressure Traces/ProcessedData/batch_summary.csv"), stringsAsFactors = FALSE)
pilot_pressure <- read.csv(file.path(pilot_study_dir, "Pressure Traces/ProcessedData/batch_summary.csv"), stringsAsFactors = FALSE)

main_data <- main_data_all %>%
  filter(is.na(SampleType) | SampleType != "NC") %>%
  filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS", "PASS*")) %>%
  left_join(main_pressure %>% select(Sample, Mean_Pressure), by = c("RunID" = "Sample")) %>%
  mutate(Study = "Main")

pilot_pool <- pilot_data_all %>%
  filter(Solvent_level == "present", Pressure_g %in% c(50, 200), is.na(SampleType) | SampleType != "NC") %>%
  filter(analysis_accepted %in% c("PASS", "PASS*")) %>%
  left_join(pilot_pressure %>% select(Sample, Mean_Pressure), by = c("RunID" = "Sample")) %>%
  mutate(Study = "Pilot")

common_cols <- intersect(names(pilot_pool), names(main_data))
combined <- bind_rows(
  pilot_pool %>% select(all_of(common_cols)),
  main_data  %>% select(all_of(common_cols))
) %>%
  filter(!is.na(Mean_Pressure)) %>%
  mutate(
    Surface_type = factor(Surface_type, levels = c("steel", "abs")),
    Study = factor(Study, levels = c("Pilot", "Main")),
    SurfaceID_nested = paste(Surface_type, SurfaceID, sep = ":"),
    AnalysisDate = as.Date(as.character(GCMS_Analysis_Date), format = "%Y%m%d")
  )

cat("Combined dataset:", nrow(combined), "rows (", sum(combined$Study == "Pilot"), "Pilot +",
    sum(combined$Study == "Main"), "Main)\n")
cat("AnalysisDate range:", format(min(combined$AnalysisDate, na.rm = TRUE)), "to",
    format(max(combined$AnalysisDate, na.rm = TRUE)),
    "(", as.numeric(max(combined$AnalysisDate, na.rm = TRUE) - min(combined$AnalysisDate, na.rm = TRUE)), "days total span )\n\n")

combined <- combined %>%
  mutate(ElapsedDays = as.numeric(AnalysisDate - min(AnalysisDate, na.rm = TRUE)))

# ===============================================================================
# 1. Simple descriptive check: raw + pressure-adjusted mean recovery by Study
# ===============================================================================

cat("=== Raw mean recovery by Study (all pooled pressure levels, unadjusted) ===\n")
combined %>%
  group_by(Study) %>%
  summarise(
    n = n(),
    PETN_mean = round(mean(PETN_Recovery_pct, na.rm = TRUE), 2),
    RDX_mean  = round(mean(RDX_Recovery_pct, na.rm = TRUE), 2),
    mean_ElapsedDays = round(mean(ElapsedDays, na.rm = TRUE), 1),
    mean_AnalysisDate = format(mean(AnalysisDate, na.rm = TRUE))
  ) %>%
  print()

cat("\n=== Date groups (finer breakdown across the full combined timeline) ===\n")
combined %>%
  mutate(DateGroup = format(AnalysisDate, "%Y-%m-%d")) %>%
  group_by(DateGroup, Study) %>%
  summarise(n = n(), PETN_mean = round(mean(PETN_Recovery_pct, na.rm = TRUE), 2),
            RDX_mean = round(mean(RDX_Recovery_pct, na.rm = TRUE), 2), .groups = "drop") %>%
  arrange(DateGroup) %>%
  print(n = 30)

# ===============================================================================
# 2. Continuous ElapsedDays model across the FULL combined timeline (both
# studies), controlling for actual pressure (linear+quadratic) and Surface
# ===============================================================================

fit_date_model <- function(recovery_col, analyte_name) {
  d <- combined[!is.na(combined[[recovery_col]]) & !is.na(combined$ElapsedDays), ]
  cat("\n--- ", analyte_name, ": continuous ElapsedDays model (n=", nrow(d), ") ---\n", sep = "")

  formula_str <- paste0(recovery_col, " ~ ElapsedDays + Surface_type + Mean_Pressure + I(Mean_Pressure^2) + (1|SurfaceID_nested)")
  m <- tryCatch(lmer(as.formula(formula_str), data = d, REML = TRUE), error = function(e) { cat("  [ERROR] ", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(invisible(NULL))

  s <- summary(m)
  print(s$coefficients)

  coef_row <- s$coefficients["ElapsedDays", ]
  cat(sprintf("\n  ElapsedDays coefficient: %.4f pp/day  (SE=%.4f, t=%.2f, p=%.4f)\n",
              coef_row["Estimate"], coef_row["Std. Error"], coef_row["t value"], coef_row["Pr(>|t|)"]))
  cat(sprintf("  -> Over the full %d-day combined span, this implies a total change of %.1f percentage points\n",
              round(max(d$ElapsedDays) - min(d$ElapsedDays)),
              coef_row["Estimate"] * (max(d$ElapsedDays) - min(d$ElapsedDays))))

  invisible(m)
}

petn_date_model <- fit_date_model("PETN_Recovery_pct", "PETN")
rdx_date_model  <- fit_date_model("RDX_Recovery_pct", "RDX")

# ===============================================================================
# 3. Does a discrete Study step explain it better than continuous ElapsedDays,
# or are they redundant/complementary? Fit both together (if not aliased) to
# see whether ElapsedDays still carries information once Study is in the
# model, and vice versa.
# ===============================================================================

fit_combined_date_study_model <- function(recovery_col, analyte_name) {
  d <- combined[!is.na(combined[[recovery_col]]) & !is.na(combined$ElapsedDays), ]
  cat("\n--- ", analyte_name, ": ElapsedDays + Study together (n=", nrow(d), ") ---\n", sep = "")
  formula_str <- paste0(recovery_col, " ~ ElapsedDays + Study + Surface_type + Mean_Pressure + I(Mean_Pressure^2) + (1|SurfaceID_nested)")
  m <- tryCatch(lmer(as.formula(formula_str), data = d, REML = TRUE), error = function(e) { cat("  [ERROR] ", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(invisible(NULL))
  print(summary(m)$coefficients)
  invisible(m)
}

fit_combined_date_study_model("PETN_Recovery_pct", "PETN")
fit_combined_date_study_model("RDX_Recovery_pct", "RDX")

cat("\nDone.\n")
