# Simple m/z 52 Analysis
library(ggplot2)
library(dplyr)

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConverterMs/"
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/TIC_Diagnostics/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read existing full results
full_results <- read.csv(file.path(output_dir, "TIC_AllIons_FullResults.csv"))

# Extract m/z 52 data
mz52 <- full_results %>% filter(mz == 52)
mz122 <- full_results %>% filter(mz == 122)
mz120 <- full_results %>% filter(mz == 120)

message("=== m/z 52 Analysis ===")
message("\nCalibrant intensities for m/z 52:")
message(sprintf("  0.2ng: %.0f", mz52$int_002))
message(sprintf("  2ng:   %.0f", mz52$int_003))
message(sprintf("  4ng:   %.0f", mz52$int_004))
message(sprintf("  6ng:   %.0f", mz52$int_005))
message(sprintf("  8ng:   %.0f", mz52$int_006))
message(sprintf("  10ng:  %.0f", mz52$int_007))

message("\nCalibrant intensities for m/z 122 (Current IS):")
message(sprintf("  0.2ng: %.0f", mz122$int_002))
message(sprintf("  2ng:   %.0f", mz122$int_003))
message(sprintf("  4ng:   %.0f", mz122$int_004))
message(sprintf("  6ng:   %.0f", mz122$int_005))
message(sprintf("  8ng:   %.0f", mz122$int_006))
message(sprintf("  10ng:  %.0f", mz122$int_007))

# Build comparison data frame
cal_conc <- c(0.2, 2, 4, 6, 8, 10)
mz52_cal <- c(mz52$int_002, mz52$int_003, mz52$int_004, mz52$int_005, mz52$int_006, mz52$int_007)
mz122_cal <- c(mz122$int_002, mz122$int_003, mz122$int_004, mz122$int_005, mz122$int_006, mz122$int_007)
mz120_cal <- c(mz120$int_002, mz120$int_003, mz120$int_004, mz120$int_005, mz120$int_006, mz120$int_007)

comparison <- data.frame(
  conc = cal_conc,
  mz52 = mz52_cal,
  mz122 = mz122_cal,
  mz120 = mz120_cal
)

# Calculate statistics
mz52_cv <- sd(mz52_cal) / mean(mz52_cal) * 100
mz122_cv <- sd(mz122_cal) / mean(mz122_cal) * 100
mz52_cor <- cor(mz52_cal, cal_conc)
mz122_cor <- cor(mz122_cal, cal_conc)

message("\n=== Statistics ===")
message(sprintf("m/z 52:  CV = %.1f%%, Cor vs Conc = %.3f", mz52_cv, mz52_cor))
message(sprintf("m/z 122: CV = %.1f%%, Cor vs Conc = %.3f", mz122_cv, mz122_cor))

# Calculate ratios
comparison$ratio_52 <- comparison$mz120 / comparison$mz52
comparison$ratio_122 <- comparison$mz120 / comparison$mz122

ratio_52_cv <- sd(comparison$ratio_52) / mean(comparison$ratio_52) * 100
ratio_122_cv <- sd(comparison$ratio_122) / mean(comparison$ratio_122) * 100

message("\n=== Ratio Statistics ===")
message(sprintf("m/z 120 / m/z 52 ratio:  CV = %.1f%%", ratio_52_cv))
message(sprintf("m/z 120 / m/z 122 ratio: CV = %.1f%%", ratio_122_cv))

# Calibration fit
fit_52 <- lm(ratio_52 ~ conc, data = comparison)
fit_122 <- lm(ratio_122 ~ conc, data = comparison)

r2_52 <- summary(fit_52)$r.squared
r2_122 <- summary(fit_122)$r.squared

message(sprintf("\nCalibration R-squared:"))
message(sprintf("  m/z 52 ratio:  R² = %.4f", r2_52))
message(sprintf("  m/z 122 ratio: R² = %.4f", r2_122))

# Plot
plot_data <- data.frame(
  conc = rep(cal_conc, 2),
  ion = rep(c("m/z 52", "m/z 122"), each = 6),
  intensity = c(mz52_cal, mz122_cal),
  norm_intensity = c(mz52_cal/mean(mz52_cal), mz122_cal/mean(mz122_cal))
)

p <- ggplot(plot_data, aes(x = conc, y = norm_intensity, color = ion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("m/z 52" = "darkgreen", "m/z 122" = "red")) +
  labs(
    title = "m/z 52 (Alternative IS Candidate) vs m/z 122 (Current IS)",
    subtitle = sprintf("m/z 52: CV=%.1f%%, r=%.2f | m/z 122: CV=%.1f%%, r=%.2f",
                       mz52_cv, mz52_cor, mz122_cv, mz122_cor),
    x = "RDX Concentration (ng)",
    y = "Normalized Intensity (mean = 1)"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_Normalized_Comparison.png"), p, width = 8, height = 6, dpi = 150)

message("\n=== CONCLUSION ===")
if (mz52_cv < mz122_cv && abs(mz52_cor) < abs(mz122_cor)) {
  message(">>> m/z 52 IS A VIABLE ALTERNATIVE IS CANDIDATE")
  message("    - Lower CV indicates more stable signal")
  message("    - Lower correlation indicates independence from RDX")
  message(sprintf("    - Improvement: %.0f%% lower CV, %.0f%% lower correlation",
                  (mz122_cv - mz52_cv)/mz122_cv*100,
                  (abs(mz122_cor) - abs(mz52_cor))/abs(mz122_cor)*100))
} else {
  message(">>> m/z 52 does not improve over m/z 122")
}

message("\nOutput: mz52_Normalized_Comparison.png")
