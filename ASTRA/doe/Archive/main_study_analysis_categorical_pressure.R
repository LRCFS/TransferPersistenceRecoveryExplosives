#===============================================================================
# MAIN STUDY ANALYSIS: CATEGORICAL PRESSURE REFIT + PAIRWISE CONTRASTS
#
# Standalone companion to main_study_analysis.R -- mirrors its Section 3/5
# combined-model architecture (same pooled Pilot+Main dataset, same
# SurfaceID_nested random effect, same QC filter), but treats Pressure_g as
# a CATEGORICAL factor (5 levels: 10/50/100/200/300g) instead of a continuous
# quadratic (Pressure_g + I(Pressure_g^2)).
#
# Built to resolve two open questions flagged in the August 27, 2026 session
# (see ASTRA/doe/CONTEXT.md):
#   1. The pooled QUADRATIC model's EMM-based "peak at 200g" claim for Steel
#      does not match the raw data (which peaks at 100g) -- a symmetric
#      parabola forced through 5 points can't represent the real asymmetric
#      shape (sharp rise 10->100g, gentle decline 100->300g). A categorical
#      factor + pairwise contrasts formally tests 100g vs 200g (and every
#      other pair) directly from the group means, with no curve-shape
#      assumption at all.
#   2. Whether ABS shows any statistically detectable pressure effect ON ITS
#      OWN was never tested directly -- the combined model's significant
#      Surface_type:Pressure_g interaction only tells us Steel and ABS
#      respond DIFFERENTLY, not whether ABS has a real effect in either
#      direction. An ABS-only subset model answers this directly.
#
# Deliberately does NOT modify main_study_analysis.R, its outputs, or
# main_study_analysis_actual_pressure.R -- reads the already-collated
# main_study_data_nested.csv/pilot_data_nested.csv directly (no GC-MS
# re-collation), rebuilds the identical pooled dataset, and writes its own
# separate summary file + plot so all three analyses (quadratic/nominal,
# quadratic/actual-pressure, categorical/nominal) can be compared side by
# side without any of them overwriting each other.
#
# Output:
#   - main_study_analysis_summary_CATEGORICAL_PRESSURE.txt (in output_dir)
#   - Pressure_Categorical_Means_PilotMain.png (in output_dir)
#===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(lmerTest)
  library(car)
  library(effectsize)
  library(emmeans)
  library(ggplot2)
})

#===============================================================================
# USER CONFIGURATION -- identical values to main_study_analysis.R
#===============================================================================

use_qc_filtered_data <- TRUE

output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study"
data_file  <- file.path(output_dir, "main_study_data_nested.csv")
pilot_data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/pilot_data_nested.csv"

stats_summary_file <- file.path(output_dir, "main_study_analysis_summary_CATEGORICAL_PRESSURE.txt")
plot_file           <- file.path(output_dir, "Pressure_Categorical_Means_PilotMain.png")

alpha_level <- 0.05

#===============================================================================
# LOAD + POOL DATA (mirrors main_study_analysis.R Sections 2-3 exactly)
#===============================================================================

if (!file.exists(data_file)) {
  stop("ERROR: ", data_file, " not found. Run main_study_analysis.R first.")
}

main_data <- read.csv(data_file, stringsAsFactors = FALSE)
main_data$Surface_type <- factor(main_data$Surface_type, levels = c("steel", "abs"))

if (use_qc_filtered_data) {
  main_data <- main_data %>%
    filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS", "PASS*")) %>%
    filter(is.na(SampleType) | SampleType != "NC")
} else {
  main_data <- main_data %>% filter(is.na(SampleType) | SampleType != "NC")
}

pilot_pool <- data.frame()
if (file.exists(pilot_data_file)) {
  pilot_data <- read.csv(pilot_data_file, stringsAsFactors = FALSE)
  pilot_pool <- pilot_data %>%
    filter(Solvent_level == "present", Pressure_g %in% c(50, 200))
  if (use_qc_filtered_data) {
    pilot_pool <- pilot_pool %>% filter(analysis_accepted %in% c("PASS", "PASS*"))
  }
  pilot_pool <- pilot_pool %>% filter(is.na(SampleType) | SampleType != "NC")
  pilot_pool$Study <- "Pilot"
} else {
  cat("[NOTE] Pilot data file not found -- proceeding with main-study data only.\n")
}

main_pool <- main_data %>% mutate(Study = "Main")

common_cols <- intersect(names(pilot_pool), names(main_pool))
combined_data <- bind_rows(
  if (nrow(pilot_pool) > 0) pilot_pool %>% select(all_of(common_cols)) else pilot_pool,
  main_pool %>% select(all_of(common_cols))
)

combined_data$Surface_type <- factor(combined_data$Surface_type, levels = c("steel", "abs"))
combined_data$Study <- factor(combined_data$Study, levels = c("Pilot", "Main"))
combined_data$SurfaceID_nested <- paste(combined_data$Surface_type, combined_data$SurfaceID, sep = ":")

# Pressure_g as an ORDERED categorical factor -- ordered so pairwise contrast
# output lists levels in a sensible low-to-high order, but treated as a plain
# factor (not polynomial contrasts) in the model formula below.
combined_data$Pressure_f <- factor(combined_data$Pressure_g,
                                     levels = sort(unique(combined_data$Pressure_g)))

cat("Combined (pilot-pooled + main) dataset:", nrow(combined_data), "rows",
    "(", sum(combined_data$Study == "Pilot"), "pooled pilot +",
    sum(combined_data$Study == "Main"), "main)\n\n")

sink(stats_summary_file, split = TRUE)
cat("========================================\n")
cat("CATEGORICAL PRESSURE REFIT + PAIRWISE CONTRASTS\n")
cat("Generated:", format(Sys.time()), "\n")
cat("Companion to main_study_analysis.R -- Pressure_g modelled as a 5-level\n")
cat("categorical factor instead of a continuous quadratic. See header comment\n")
cat("in main_study_analysis_categorical_pressure.R for the full rationale.\n")
cat("========================================\n\n")

#===============================================================================
# PART A: COMBINED MODEL, CATEGORICAL PRESSURE, BOTH SURFACES
#===============================================================================

fit_categorical_model <- function(data, recovery_col, analyte_name, subset_label = NULL,
                                    include_surface_term = TRUE) {

  data_clean <- data[!is.na(data[[recovery_col]]) & !is.na(data$Pressure_f), ]

  label <- if (is.null(subset_label)) analyte_name else paste0(analyte_name, " (", subset_label, ")")
  cat("\n---", label, "---\n")
  cat("  n =", nrow(data_clean),
      " | distinct Pressure_g levels =", length(unique(data_clean$Pressure_f)),
      " | distinct physical surfaces =", length(unique(data_clean$SurfaceID_nested)), "\n")

  if (nrow(data_clean) < 10 || length(unique(data_clean$Pressure_f)) < 3) {
    cat("  [NOT ENOUGH DATA] Skipping.\n")
    return(NULL)
  }

  formula_str <- if (include_surface_term) {
    paste0(recovery_col, " ~ Surface_type * Pressure_f + Study + (1|SurfaceID_nested)")
  } else {
    paste0(recovery_col, " ~ Pressure_f + Study + (1|SurfaceID_nested)")
  }
  formula_obj <- as.formula(formula_str)

  model <- tryCatch(
    lmer(formula_obj, data = data_clean, REML = TRUE),
    error = function(e) { cat("  [ERROR] Model failed to fit:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(model)) return(NULL)

  if (isSingular(model)) {
    cat("  [WARNING] Boundary (singular) fit: between-surface variance estimated at ~0.\n")
  }

  cat("\nType III ANOVA:\n")
  anova_res <- tryCatch(car::Anova(model, type = 3, test.statistic = "F"), error = function(e) NULL)
  if (!is.null(anova_res)) print(anova_res)

  eff <- tryCatch(effectsize::eta_squared(anova_res, partial = TRUE), error = function(e) NULL)
  if (!is.null(eff)) {
    cat("\nEffect sizes (partial eta^2):\n")
    print(eff)
  }

  list(model = model, anova = anova_res, data_clean = data_clean)
}

for (analyte in list(list(col = "PETN_Recovery_pct", name = "PETN"),
                     list(col = "RDX_Recovery_pct",  name = "RDX"))) {

  cat("\n========================================\n")
  cat(analyte$name, ": COMBINED MODEL (Surface_type x Pressure_f)\n")
  cat("========================================\n")

  fit <- fit_categorical_model(combined_data, analyte$col, analyte$name)
  if (is.null(fit)) next

  # Pairwise contrasts WITHIN each Surface_type across Pressure_f levels --
  # this is the direct test of "is 100g different from 200g" (and every
  # other pair) that the quadratic model's smoothed EMM curve can't answer
  # cleanly on its own.
  emm_pairs <- tryCatch(
    emmeans(fit$model, ~ Pressure_f | Surface_type),
    error = function(e) { cat("  [NOTE] emmeans failed:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(emm_pairs)) {
    cat("\nEstimated marginal means by Surface_type x Pressure_g:\n")
    print(emm_pairs)

    cat("\nPairwise contrasts WITHIN each Surface_type (Tukey-adjusted):\n")
    contrasts <- tryCatch(pairs(emm_pairs, adjust = "tukey"), error = function(e) NULL)
    if (!is.null(contrasts)) print(contrasts)

    cat("\n  --> 100g vs 200g contrast specifically, both surfaces:\n")
    if (!is.null(contrasts)) {
      contrasts_df <- as.data.frame(contrasts)
      hit <- contrasts_df[grepl("100.*200|200.*100", contrasts_df$contrast), ]
      if (nrow(hit) > 0) print(hit) else cat("    (contrast not found in output -- check level labels above)\n")
    }
  }
}

#===============================================================================
# PART B: SURFACE-SPECIFIC SUBSET MODELS -- does ABS show ANY pressure effect
# on its own, independent of Steel's much larger effect dominating the
# pooled model's Surface_type:Pressure_g interaction term?
#===============================================================================

cat("\n\n========================================\n")
cat("PART B: SURFACE-SPECIFIC SUBSETS (does ABS show curvature on its own?)\n")
cat("========================================\n")

for (analyte in list(list(col = "PETN_Recovery_pct", name = "PETN"),
                     list(col = "RDX_Recovery_pct",  name = "RDX"))) {

  for (surf in c("steel", "abs")) {

    subset_data <- combined_data[combined_data$Surface_type == surf, ]

    cat("\n========================================\n")
    cat(analyte$name, "-", toupper(surf), "ONLY\n")
    cat("========================================\n")

    fit <- fit_categorical_model(subset_data, analyte$col, analyte$name,
                                   subset_label = paste0(surf, "-only"),
                                   include_surface_term = FALSE)
    if (is.null(fit)) next

    emm_surf <- tryCatch(emmeans(fit$model, ~ Pressure_f), error = function(e) NULL)
    if (!is.null(emm_surf)) {
      cat("\nEstimated marginal means (", surf, " only):\n", sep = "")
      print(emm_surf)
      cat("\nPairwise contrasts (Tukey-adjusted):\n")
      contrasts_surf <- tryCatch(pairs(emm_surf, adjust = "tukey"), error = function(e) NULL)
      if (!is.null(contrasts_surf)) print(contrasts_surf)
    }

    # Verdict line -- the direct answer to "does this surface show ANY
    # detectable pressure effect on its own?"
    if (!is.null(fit$anova) && "Pressure_f" %in% rownames(fit$anova)) {
      p_val <- fit$anova["Pressure_f", "Pr(>F)"]
      verdict <- if (is.na(p_val)) {
        "could not be tested"
      } else if (p_val < alpha_level) {
        sprintf("SIGNIFICANT pressure effect detected on its own (p = %.4g)", p_val)
      } else {
        sprintf("NO significant pressure effect detected on its own (p = %.4g)", p_val)
      }
      cat("\n>>> VERDICT (", analyte$name, ", ", surf, "-only): ", verdict, " <<<\n", sep = "")
    }
  }
}

cat("\n\n========================================\n")
cat("DONE\n")
cat("========================================\n")

sink()

cat("\nSummary written to:", stats_summary_file, "\n")

#===============================================================================
# PLOT: categorical means (with 95% CI) by Surface_type x Pressure_g, both
# analytes -- the direct visual companion to the pairwise contrasts above,
# with NO curve fit overlaid (unlike the quadratic model's own plots) so the
# real per-level shape is shown without any smoothing assumption.
#===============================================================================

plot_data <- combined_data %>%
  select(Surface_type, Pressure_g, PETN_Recovery_pct, RDX_Recovery_pct) %>%
  tidyr::pivot_longer(cols = c(PETN_Recovery_pct, RDX_Recovery_pct),
                       names_to = "Analyte", values_to = "Recovery") %>%
  mutate(Analyte = ifelse(Analyte == "PETN_Recovery_pct", "PETN", "RDX")) %>%
  filter(!is.na(Recovery)) %>%
  group_by(Surface_type, Pressure_g, Analyte) %>%
  summarise(
    mean_rec = mean(Recovery),
    se_rec   = sd(Recovery) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  )

p <- ggplot(plot_data, aes(x = factor(Pressure_g), y = mean_rec,
                             color = Surface_type, group = Surface_type)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = mean_rec - se_rec, ymax = mean_rec + se_rec),
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_text(aes(label = paste0("n=", n), y = mean_rec + se_rec + 1.5),
            position = position_dodge(width = 0.3), size = 2.8, show.legend = FALSE) +
  facet_wrap(~ Analyte, ncol = 1) +
  labs(
    title = "Recovery by Pressure (categorical) -- pooled Pilot + Main, no curve fit",
    subtitle = "Mean +/- SE per Surface_type x Pressure_g level. Companion to the pairwise contrasts\nin main_study_analysis_summary_CATEGORICAL_PRESSURE.txt",
    x = "Pressure (g, nominal)", y = "Recovery (%)", color = "Surface"
  ) +
  theme_bw()

ggsave(plot_file, p, width = 8, height = 8, dpi = 300)
cat("Plot written to:", plot_file, "\n")
