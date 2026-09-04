### Compare recovery across swabbing patterns ###
# This script compares recovery percentages between the three swabbing patterns:
# - Snake
# - BaF (Back and Forth)
# - Ratchet
#
# PREREQUISITE:
#   1. Run 00-GlobalCode.R first
#   2. Run 01-OrganizeImages.R (creates ImageMapping.csv)
#   3. Run 06-RecoveryAnalysis.R (creates AverageRecovery.csv)

# === LOAD REQUIRED LIBRARIES ===
library(dplyr)
library(ggplot2)
library(tidyverse)

# === READ INPUT DATA ===

# Read recovery results (Pattern column already added by 06-RecoveryAnalysis.R)
recovery_data <- read.csv(paste0(AnalysisOutput.dir, "AverageRecovery.csv"))

# Check if Pattern column exists
if (!"Pattern" %in% names(recovery_data)) {
  stop("Pattern column not found in AverageRecovery.csv. Run updated 06-RecoveryAnalysis.R first.")
}

# Define custom Pattern order for consistent plotting
pattern_order <- c("50g_BackandForth", "50g_Snake", "50g_Ratchet")
recovery_data$Pattern <- factor(recovery_data$Pattern, levels = pattern_order)

# Filter to valid data (RSD <= 50, matching 06-RecoveryAnalysis.R threshold)
recovery_filtered <- recovery_data %>%
  filter(RSD >= 0 & RSD <= 50)

cat("=== DATA PREPARATION ===\n")
cat(sprintf("Total samples in recovery data: %d\n", nrow(recovery_data)))
cat(sprintf("Samples after filtering (RSD <= 50): %d\n", nrow(recovery_filtered)))
cat(sprintf("Samples excluded (RSD > 50): %d\n", nrow(recovery_data) - nrow(recovery_filtered)))

# Check for missing patterns
if (any(is.na(recovery_filtered$Pattern))) {
  cat("\nWARNING: Some samples have missing pattern assignments:\n")
  print(recovery_filtered %>% filter(is.na(Pattern)) %>% select(File, Pattern))
}

# === DESCRIPTIVE STATISTICS ===

cat("\n=== DESCRIPTIVE STATISTICS BY PATTERN ===\n")

pattern_stats <- recovery_filtered %>%
  group_by(Pattern) %>%
  summarise(
    N = n(),
    Mean = mean(mean, na.rm = TRUE),
    SD = sd(mean, na.rm = TRUE),
    SE = ifelse(n() > 1, sd(mean, na.rm = TRUE) / sqrt(n()), NA),
    Min = min(mean, na.rm = TRUE),
    Max = max(mean, na.rm = TRUE),
    Median = median(mean, na.rm = TRUE),
    .groups = "drop"
  )

print(pattern_stats)

# Save statistics table
write.csv(pattern_stats, 
          file = paste0(AnalysisOutput.dir, "PatternComparison_Statistics.csv"), 
          row.names = FALSE)
cat(sprintf("\nStatistics saved to: %s\n", paste0(AnalysisOutput.dir, "PatternComparison_Statistics.csv")))

# Check sample sizes and warn
cat("\n=== SAMPLE SIZE WARNINGS ===\n")
for (pattern_name in unique(recovery_filtered$Pattern)) {
  n <- sum(recovery_filtered$Pattern == pattern_name, na.rm = TRUE)
  if (n == 1) {
    cat(sprintf("WARNING: %s has only n=1 replicate - no variance estimate possible\n", pattern_name))
  } else if (n == 2) {
    cat(sprintf("NOTE: %s has n=2 replicates - SD estimate has low precision\n", pattern_name))
  } else if (n >= 3) {
    cat(sprintf("OK: %s has n=%d replicates\n", pattern_name, n))
  }
}

# === ONE-WAY ANOVA ===

cat("\n=== ONE-WAY ANOVA ===\n")

# Filter out patterns with n < 2 for ANOVA (can't estimate variance)
patterns_for_anova <- recovery_filtered %>%
  group_by(Pattern) %>%
  filter(n() >= 2) %>%
  ungroup()

n_patterns_for_anova <- length(unique(patterns_for_anova$Pattern))

if (n_patterns_for_anova >= 2) {
  
  cat(sprintf("Patterns included in ANOVA: %s\n", paste(unique(patterns_for_anova$Pattern), collapse = ", ")))
  cat(sprintf("Total observations: %d\n", nrow(patterns_for_anova)))
  
  # Run ANOVA
  anova_result <- aov(mean ~ Pattern, data = patterns_for_anova)
  
  # Print ANOVA summary
  cat("\nANOVA Table:\n")
  print(summary(anova_result))
  
  # Extract key statistics
  anova_summary <- summary(anova_result)[[1]]
  f_value <- anova_summary["Pattern", "F value"]
  p_value <- anova_summary["Pattern", "Pr(>F)"]
  df_between <- anova_summary["Pattern", "Df"]
  df_within <- anova_summary["Residuals", "Df"]
  
  cat(sprintf("\nF-statistic: %.3f\n", f_value))
  cat(sprintf("Degrees of freedom: (%d, %d)\n", df_between, df_within))
  cat(sprintf("p-value: %.4f\n", p_value))
  
  # Interpretation
  cat("\n=== INTERPRETATION ===\n")
  if (p_value < 0.05) {
    cat(sprintf("RESULT: Significant difference detected (p = %.4f < 0.05)\n", p_value))
    cat("The mean recovery differs significantly between at least two patterns.\n")
  } else if (p_value < 0.10) {
    cat(sprintf("RESULT: Marginally significant (p = %.4f, between 0.05 and 0.10)\n", p_value))
    cat("There may be a difference between patterns, but more replicates recommended.\n")
  } else {
    cat(sprintf("RESULT: No significant difference detected (p = %.4f >= 0.05)\n", p_value))
    cat("Cannot conclude that patterns differ in recovery.\n")
  }
  
  # Assumption checks
  cat("\n=== ASSUMPTION CHECKS ===\n")
  
  # Levene's test for homogeneity of variance
  if (requireNamespace("car", quietly = TRUE)) {
    library(car)
    levene_result <- leveneTest(mean ~ Pattern, data = patterns_for_anova)
    cat("\nLevene's Test for Homogeneity of Variance:\n")
    print(levene_result)
    
    levene_p <- levene_result["Pr(>F)"][1, 1]
    if (!is.na(levene_p) && levene_p < 0.05) {
      cat("WARNING: Variance may not be homogeneous (p < 0.05)\n")
    } else {
      cat("OK: Homogeneity of variance assumption met\n")
    }
  } else {
    cat("NOTE: 'car' package not installed - skipping Levene's test\n")
    cat("Install with: install.packages('car')\n")
  }
  
  # Shapiro-Wilk test for normality of residuals
  residuals_anova <- residuals(anova_result)
  if (length(residuals_anova) >= 3 && length(residuals_anova) <= 5000) {
    shapiro_result <- shapiro.test(residuals_anova)
    cat("\nShapiro-Wilk Test for Normality of Residuals:\n")
    cat(sprintf("W = %.4f, p = %.4f\n", shapiro_result$statistic, shapiro_result$p.value))
    
    if (shapiro_result$p.value < 0.05) {
      cat("WARNING: Residuals may not be normally distributed (p < 0.05)\n")
    } else {
      cat("OK: Normality assumption met\n")
    }
  }
  
} else {
  cat("WARNING: Insufficient patterns with n >= 2 replicates for ANOVA\n")
  cat("Need at least 2 patterns with 2+ replicates each.\n")
  
  if (n_patterns_for_anova == 1) {
    cat("\nOnly one pattern has sufficient replicates.\n")
    cat("Cannot compare groups with ANOVA.\n")
  }
}

# === VISUALIZATION ===

cat("\n=== CREATING VISUALIZATION ===\n")

# Define colors for patterns
pattern_colors <- pal_pattern

# Calculate mean recovery per pattern for diamond overlay
pattern_means <- recovery_filtered %>%
  group_by(Pattern) %>%
  summarise(Mean = mean(mean, na.rm = TRUE), .groups = "drop")

# Box plot with mean diamonds, only show outlier points
p <- ggplot(recovery_filtered, aes(x = Pattern, y = mean, fill = Pattern)) +
  geom_boxplot(alpha = 0.7) +
  # Add mean as diamond
  geom_point(data = pattern_means, 
             aes(x = Pattern, y = Mean), 
             shape = 18, size = 5, color = "black", fill = "black") +
  scale_fill_manual(values = pattern_colors) +
  labs(
    title = "Recovery Comparison by Swabbing Pattern",
    subtitle = "Diamonds show group means; points show outliers only",
    x = "Swabbing Pattern",
    y = "Recovery (%)",
    fill = "Pattern"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Add sample size annotations
label_data <- recovery_filtered %>%
  group_by(Pattern) %>%
  summarise(N = n(), .groups = "drop") %>%
  mutate(y_pos = min(recovery_filtered$mean) - 5)

p <- p + geom_text(data = label_data, aes(x = Pattern, y = y_pos, label = paste0("n=", N)), 
                   size = 3.5, color = "gray30")

show(p)

# Save plot
ggsave(
  filename = "PatternComparison_BoxPlot.png",
  plot = p,
  path = file.path(AnalysisOutput.dir),
  width = 8,
  height = 6,
  dpi = 300
)
cat(sprintf("Plot saved to: %s\n", paste0(AnalysisOutput.dir, "PatternComparison_BoxPlot.png")))

# === SUMMARY TABLE FOR REFERENCE ===

cat("\n=== COMPLETE DATA SUMMARY ===\n")

complete_summary <- recovery_filtered %>%
  select(File, Pattern, mean, StdDev, RSD) %>%
  rename(Recovery = mean, Recovery_SD = StdDev, Recovery_RSD = RSD) %>%
  arrange(Pattern, File)

print(complete_summary)

cat("\n=== SCRIPT COMPLETE ===\n")
cat(sprintf("Output files:\n"))
cat(sprintf("  - %s\n", paste0(AnalysisOutput.dir, "PatternComparison_Statistics.csv")))
cat(sprintf("  - %s\n", paste0(AnalysisOutput.dir, "PatternComparison_BoxPlot.png")))
