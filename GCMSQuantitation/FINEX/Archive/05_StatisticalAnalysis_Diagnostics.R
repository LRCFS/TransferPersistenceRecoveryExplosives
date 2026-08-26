# =========================================================
# FINEX Swabbing Study - Statistical Analysis Diagnostics
# =========================================================
#
# Diagnostic investigation script for PETN/RDX separation decision.
# Tests whether to analyze PETN and RDX separately or combined,
# and evaluates parametric vs non-parametric statistical approaches.
#
# This script generates:
# - Separate PETN and RDX diagnostic plots
# - Variance components by analyte
# - Non-parametric tests (Kruskal-Wallis + Dunn)
# - Method comparison table
#
# Run this BEFORE modifying 05_StatisticalAnalysis.R to decide
# which statistical approach to implement.
#
# Author: Diagnostic analysis module
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(effectsize)
  library(ggplot2)
  library(readxl)
  library(FSA)  # For Dunn's test (non-parametric post-hoc)
})

# =========================================================
# CONFIGURATION
# =========================================================

# Study root directory
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

# Input file
input_xlsx <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")

# Diagnostics output directory
diag_dir <- file.path(study_root_dir, "Plots", "Statistical_Diagnostics_PETN_RDX")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# Load and prepare data
# =========================================================

print("Loading FINEX_StudyResults.xlsx...")

if (!file.exists(input_xlsx)) {
  stop("FINEX_StudyResults.xlsx not found. Run 04_CollateStudyResults.R first.")
}

# Load All Data sheet (main dataset with all samples)
master <- read_excel(input_xlsx, sheet = "All Data")

print(paste0("Loaded ", nrow(master), " rows"))

# =========================================================
# Prepare data for analysis
# =========================================================

# Use drift-corrected recovery for PETN, standard recovery for RDX
# (RDX doesn't have drift correction applied in collated results)
analysis_data <- master %>%
  rename(
    petn_recovery = petn_recovery_dc
  ) %>%
  filter(analysis_accepted %in% c("PASS", "PASS*"))

# Convert to long format (one row per measurement)
long_data <- analysis_data %>%
  pivot_longer(
    cols = c(petn_recovery, rdx_recovery),
    names_to = "Analyte",
    values_to = "Recovery"
  ) %>%
  mutate(
    Analyte = toupper(gsub("_recovery", "", Analyte)),
    Lab = as.factor(Lab),
    Participant = as.factor(Participant),
    SurfaceName = as.factor(SurfaceName),
    Analyte = as.factor(Analyte)
  ) %>%
  filter(!is.na(Recovery))

# Study design info
n_labs <- n_distinct(long_data$Lab)
n_surfaces <- n_distinct(long_data$SurfaceName)

print("")
print("=== Data Structure ===")
print(paste0("Labs: ", n_labs))
print(paste0("Surfaces: ", n_surfaces))
print(paste0("Total observations: ", nrow(long_data)))
print(paste0("  PETN: ", sum(long_data$Analyte == "PETN")))
print(paste0("  RDX: ", sum(long_data$Analyte == "RDX")))
print("")

# Check for zeros
petn_zeros <- sum(long_data$Recovery == 0 & long_data$Analyte == "PETN")
rdx_zeros <- sum(long_data$Recovery == 0 & long_data$Analyte == "RDX")

print(paste0("Zero recoveries:"))
print(paste0("  PETN: ", petn_zeros, " (", round(100*petn_zeros/sum(long_data$Analyte == "PETN"), 1), "%)"))
print(paste0("  RDX: ", rdx_zeros, " (", round(100*rdx_zeros/sum(long_data$Analyte == "RDX"), 1), "%)"))
print("")

# =========================================================
# FIT SEPARATE MODELS FOR PETN AND RDX
# =========================================================

print("========================================")
print("=== FITTING SEPARATE MODELS ===")
print("========================================")
print("")

# Split data by analyte
petn_data <- long_data %>% filter(Analyte == "PETN")
rdx_data <- long_data %>% filter(Analyte == "RDX")

# Apply sum-to-zero contrasts for Lab
contrasts(petn_data$Lab) <- contr.sum(nlevels(petn_data$Lab))
contrasts(rdx_data$Lab) <- contr.sum(nlevels(rdx_data$Lab))

# =========================================================
# PETN Model
# =========================================================

print("--- Fitting PETN Model ---")

model_petn <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                   data = petn_data,
                   REML = TRUE)

print("")
print("PETN Model Summary:")
print(summary(model_petn))
print("")

# PETN ANOVA
anova_petn <- anova(model_petn)
print("PETN ANOVA:")
print(anova_petn)
print("")

# PETN Variance Components
vc_petn <- as.data.frame(VarCorr(model_petn))
var_participant_petn <- vc_petn$vcov[vc_petn$grp == "Lab:Participant"]
var_residual_petn <- vc_petn$vcov[vc_petn$grp == "Residual"]
total_var_petn <- var_participant_petn + var_residual_petn

variance_components_petn <- data.frame(
  Source = c("Between-Participant (within Lab)", "Residual (within Participant)"),
  Variance = c(var_participant_petn, var_residual_petn),
  SD = c(sqrt(var_participant_petn), sqrt(var_residual_petn)),
  Percent_Total = c(100 * var_participant_petn / total_var_petn,
                    100 * var_residual_petn / total_var_petn)
)

print("PETN Variance Components:")
print(variance_components_petn)
print("")

# =========================================================
# RDX Model
# =========================================================

print("--- Fitting RDX Model ---")

model_rdx <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                  data = rdx_data,
                  REML = TRUE)

print("")
print("RDX Model Summary:")
print(summary(model_rdx))
print("")

# RDX ANOVA
anova_rdx <- anova(model_rdx)
print("RDX ANOVA:")
print(anova_rdx)
print("")

# RDX Variance Components
vc_rdx <- as.data.frame(VarCorr(model_rdx))
var_participant_rdx <- vc_rdx$vcov[vc_rdx$grp == "Lab:Participant"]
var_residual_rdx <- vc_rdx$vcov[vc_rdx$grp == "Residual"]
total_var_rdx <- var_participant_rdx + var_residual_rdx

variance_components_rdx <- data.frame(
  Source = c("Between-Participant (within Lab)", "Residual (within Participant)"),
  Variance = c(var_participant_rdx, var_residual_rdx),
  SD = c(sqrt(var_participant_rdx), sqrt(var_residual_rdx)),
  Percent_Total = c(100 * var_participant_rdx / total_var_rdx,
                    100 * var_residual_rdx / total_var_rdx)
)

print("RDX Variance Components:")
print(variance_components_rdx)
print("")

# =========================================================
# DIAGNOSTIC PLOTS - PETN
# =========================================================

print("Generating PETN diagnostic plots...")

# PETN Residuals vs Fitted
resid_data_petn <- data.frame(
  fitted = fitted(model_petn),
  residuals = residuals(model_petn)
)

p_resid_petn <- ggplot(resid_data_petn, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "blue", method = "loess") +
  labs(title = "PETN: Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_bw()

ggsave(file.path(diag_dir, "PETN_Residuals_vs_Fitted.png"), p_resid_petn,
       width = 6, height = 5, dpi = 300)

# PETN QQ Plot
png(file.path(diag_dir, "PETN_QQ_Plot.png"), width = 6, height = 5, units = "in", res = 300)
qqnorm(residuals(model_petn), main = "PETN: Normal Q-Q Plot")
qqline(residuals(model_petn), col = "red")
dev.off()

# PETN Variance Components Bar Chart
if (!is.null(variance_components_petn) && !all(is.na(variance_components_petn$Percent_Total))) {
  vc_plot_data_petn <- variance_components_petn %>%
    filter(!is.na(Percent_Total)) %>%
    mutate(Source = factor(Source, levels = c("Between-Participant (within Lab)", 
                                               "Residual (within Participant)")))
  
  p_vc_petn <- ggplot(vc_plot_data_petn, aes(x = Source, y = Percent_Total)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = sprintf("%.1f%%", Percent_Total)), 
              vjust = -0.5, size = 4) +
    labs(title = "PETN: Variance Components",
         x = "Source", y = "Percent of Total Variance") +
    ylim(0, 100) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(diag_dir, "PETN_Variance_Components.png"), p_vc_petn,
         width = 7, height = 5, dpi = 300)
}

print("-> PETN diagnostic plots saved")

# =========================================================
# DIAGNOSTIC PLOTS - RDX
# =========================================================

print("Generating RDX diagnostic plots...")

# RDX Residuals vs Fitted
resid_data_rdx <- data.frame(
  fitted = fitted(model_rdx),
  residuals = residuals(model_rdx)
)

p_resid_rdx <- ggplot(resid_data_rdx, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "blue", method = "loess") +
  labs(title = "RDX: Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_bw()

ggsave(file.path(diag_dir, "RDX_Residuals_vs_Fitted.png"), p_resid_rdx,
       width = 6, height = 5, dpi = 300)

# RDX QQ Plot
png(file.path(diag_dir, "RDX_QQ_Plot.png"), width = 6, height = 5, units = "in", res = 300)
qqnorm(residuals(model_rdx), main = "RDX: Normal Q-Q Plot")
qqline(residuals(model_rdx), col = "red")
dev.off()

# RDX Variance Components Bar Chart
if (!is.null(variance_components_rdx) && !all(is.na(variance_components_rdx$Percent_Total))) {
  vc_plot_data_rdx <- variance_components_rdx %>%
    filter(!is.na(Percent_Total)) %>%
    mutate(Source = factor(Source, levels = c("Between-Participant (within Lab)", 
                                               "Residual (within Participant)")))
  
  p_vc_rdx <- ggplot(vc_plot_data_rdx, aes(x = Source, y = Percent_Total)) +
    geom_bar(stat = "identity", fill = "darkred") +
    geom_text(aes(label = sprintf("%.1f%%", Percent_Total)), 
              vjust = -0.5, size = 4) +
    labs(title = "RDX: Variance Components",
         x = "Source", y = "Percent of Total Variance") +
    ylim(0, 100) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(diag_dir, "RDX_Variance_Components.png"), p_vc_rdx,
         width = 7, height = 5, dpi = 300)
}

print("-> RDX diagnostic plots saved")

# =========================================================
# NON-PARAMETRIC TESTS
# =========================================================

print("")
print("========================================")
print("=== NON-PARAMETRIC TESTS ===")
print("========================================")
print("")

# Kruskal-Wallis test (analyte-specific)
print("--- Kruskal-Wallis Test (Surface Effect) ---")
print("")

kw_petn <- kruskal.test(Recovery ~ SurfaceName, data = petn_data)
print("PETN Kruskal-Wallis:")
print(kw_petn)
print("")

kw_rdx <- kruskal.test(Recovery ~ SurfaceName, data = rdx_data)
print("RDX Kruskal-Wallis:")
print(kw_rdx)
print("")

# Dunn's test for pairwise comparisons (if significant)
print("--- Dunn's Test (Pairwise Post-Hoc) ---")
print("")

if (kw_petn$p.value < 0.05) {
  print("PETN Dunn's Test:")
  dunn_petn <- dunnTest(Recovery ~ SurfaceName,
                        data = petn_data,
                        method = "bonferroni")
  print(dunn_petn)
  print("")
} else {
  print("PETN: Kruskal-Wallis not significant, skipping Dunn's test")
  print("")
}

if (kw_rdx$p.value < 0.05) {
  print("RDX Dunn's Test:")
  dunn_rdx <- dunnTest(Recovery ~ SurfaceName,
                       data = rdx_data,
                       method = "bonferroni")
  print(dunn_rdx)
  print("")
} else {
  print("RDX: Kruskal-Wallis not significant, skipping Dunn's test")
  print("")
}

# =========================================================
# PARAMETRIC POST-HOC COMPARISONS
# =========================================================

print("========================================")
print("=== PARAMETRIC POST-HOC (TUKEY HSD) ===")
print("========================================")
print("")

# PETN pairwise comparisons
print("PETN Pairwise Comparisons (Tukey HSD):")
emm_petn <- emmeans(model_petn, ~ SurfaceName)
pairs_petn <- pairs(emm_petn, adjust = "tukey")
print(pairs_petn)
print("")

# RDX pairwise comparisons
print("RDX Pairwise Comparisons (Tukey HSD):")
emm_rdx <- emmeans(model_rdx, ~ SurfaceName)
pairs_rdx <- pairs(emm_rdx, adjust = "tukey")
print(pairs_rdx)
print("")

# =========================================================
# METHOD COMPARISON TABLE
# =========================================================

print("========================================")
print("=== METHOD COMPARISON ===")
print("========================================")
print("")

# Extract surface rankings from parametric models
petn_means <- as.data.frame(emm_petn)
rdx_means <- as.data.frame(emm_rdx)

petn_ranking_param <- paste(petn_means$SurfaceName[order(-petn_means$emmean)], collapse = " > ")
rdx_ranking_param <- paste(rdx_means$SurfaceName[order(-rdx_means$emmean)], collapse = " > ")

# Extract surface rankings from non-parametric (median)
petn_medians <- petn_data %>%
  group_by(SurfaceName) %>%
  summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
  arrange(desc(median_recovery))

rdx_medians <- rdx_data %>%
  group_by(SurfaceName) %>%
  summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
  arrange(desc(median_recovery))

petn_ranking_nonparam <- paste(petn_medians$SurfaceName, collapse = " > ")
rdx_ranking_nonparam <- paste(rdx_medians$SurfaceName, collapse = " > ")

# Build comparison table
comparison_table <- data.frame(
  Analyte = c("PETN", "RDX"),
  Parametric_p = c(anova_petn["SurfaceName", "Pr(>F)"],
                   anova_rdx["SurfaceName", "Pr(>F)"]),
  Nonparametric_p = c(kw_petn$p.value, kw_rdx$p.value),
  Surface_Ranking_Parametric = c(petn_ranking_param, rdx_ranking_param),
  Surface_Ranking_Nonparametric = c(petn_ranking_nonparam, rdx_ranking_nonparam),
  Rankings_Match = c(petn_ranking_param == petn_ranking_nonparam,
                     rdx_ranking_param == rdx_ranking_nonparam)
)

print("Method Comparison Table:")
print(comparison_table)
print("")

# Agreement summary
if (all(comparison_table$Rankings_Match)) {
  print("✓ PARAMETRIC AND NON-PARAMETRIC RANKINGS AGREE FOR BOTH ANALYTES")
  print("  Recommendation: Use parametric mixed models with non-parametric validation")
} else {
  print("✗ RANKINGS DISAGREE - FURTHER INVESTIGATION NEEDED")
  print("  Recommendation: Use non-parametric tests or investigate violations")
}
print("")

# =========================================================
# SUMMARY REPORT
# =========================================================

print("")
print("========================================")
print("=== DIAGNOSTIC SUMMARY ===")
print("========================================")
print("")

print("1. SEPARATE MODELS:")
print(paste0("   - PETN diagnostics: ", ifelse(max(abs(resid_data_petn$residuals), na.rm = TRUE) < 10, "GOOD", "CHECK RESIDUALS")))
print(paste0("   - RDX diagnostics: ", ifelse(max(abs(resid_data_rdx$residuals), na.rm = TRUE) < 10, "GOOD", "CHECK RESIDUALS")))
print("")

print("2. VARIANCE COMPONENTS:")
print(paste0("   - PETN Participant variance: ", round(variance_components_petn$Percent_Total[1], 1), "%"))
print(paste0("   - RDX Participant variance: ", round(variance_components_rdx$Percent_Total[1], 1), "%"))
print("")

print("3. SURFACE EFFECT:")
print(paste0("   - PETN parametric p-value: ", format(anova_petn["SurfaceName", "Pr(>F)"], scientific = TRUE)))
print(paste0("   - PETN non-parametric p-value: ", format(kw_petn$p.value, scientific = TRUE)))
print(paste0("   - RDX parametric p-value: ", format(anova_rdx["SurfaceName", "Pr(>F)"], scientific = TRUE)))
print(paste0("   - RDX non-parametric p-value: ", format(kw_rdx$p.value, scientific = TRUE)))
print("")

print("4. METHOD AGREEMENT:")
print(paste0("   - PETN rankings match: ", comparison_table$Rankings_Match[1]))
print(paste0("   - RDX rankings match: ", comparison_table$Rankings_Match[2]))
print("")

print("All diagnostic plots saved to:")
print(diag_dir)
print("")
print("========================================")
print("=== DIAGNOSTIC SCRIPT COMPLETE ===")
print("========================================")
