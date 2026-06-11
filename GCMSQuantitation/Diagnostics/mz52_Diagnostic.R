# ============================================================
# mz52_Diagnostic.R
# Detailed analysis of m/z 52 as potential alternative IS ion
# ============================================================

library(ggplot2)
library(dplyr)

message("=== m/z 52 Alternative IS Diagnostic ===")

# ============================================================
# Configuration
# ============================================================

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConverterMs/"
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/TIC_Diagnostics/"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rdx_rt_target <- 5.79
rdx_rt_window <- 0.15

# All files
calibrants <- c("002", "003", "004", "005", "006", "007")
cal_conc <- c(0.2, 2, 4, 6, 8, 10)
blanks <- c("001", "008", "010")
samples <- c("009", "011")
all_files <- c(calibrants, blanks, samples)

# ============================================================
# Functions
# ============================================================

parse_ms1_file <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  scans <- list()
  current_scan <- NULL
  current_rt <- NULL
  ions <- list()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    if (grepl("^S\\t", line)) {
      if (!is.null(current_scan) && length(ions) > 0) {
        ions_df <- do.call(rbind, ions)
        colnames(ions_df) <- c("mz", "intensity")
        ions_df <- as.data.frame(ions_df)
        ions_df$intensity <- as.numeric(ions_df$intensity)
        ions_df$mz <- as.numeric(ions_df$mz)
        scans[[current_scan]] <- list(rt = current_rt, ions = ions_df)
      }
      
      parts <- strsplit(line, "\t")[[1]]
      current_scan <- as.integer(parts[2])
      ions <- list()
      
    } else if (grepl("^I\\tRTime\\t", line)) {
      parts <- strsplit(line, "\t")[[1]]
      current_rt <- as.numeric(parts[3])
      
    } else if (grepl("^\\d", line) && !grepl("^I\\t", line)) {
      parts <- strsplit(line, "\\s+")[[1]]
      if (length(parts) >= 2) {
        ions[[length(ions) + 1]] <- c(parts[1], parts[2])
      }
    }
  }
  
  if (!is.null(current_scan) && length(ions) > 0) {
    ions_df <- do.call(rbind, ions)
    colnames(ions_df) <- c("mz", "intensity")
    ions_df <- as.data.frame(ions_df)
    ions_df$intensity <- as.numeric(ions_df$intensity)
    ions_df$mz <- as.numeric(ions_df$mz)
    scans[[current_scan]] <- list(rt = current_rt, ions = ions_df)
  }
  
  return(scans)
}

extract_ion_at_rt <- function(scans, target_mz, target_rt, window, mz_tol = 0.5) {
  intensities <- c()
  rts <- c()
  
  for (scan in scans) {
    if (is.null(scan$rt) || is.null(scan$ions)) next
    if (abs(scan$rt - target_rt) > window) next
    
    mz_match <- scan$ions$mz >= (target_mz - mz_tol) & scan$ions$mz <= (target_mz + mz_tol)
    if (any(mz_match)) {
      intensities <- c(intensities, max(scan$ions$intensity[mz_match], na.rm = TRUE))
      rts <- c(rts, scan$rt)
    }
  }
  
  if (length(intensities) == 0) return(list(max_intensity = NA_real_, rt = NA_real_, all_intensities = NA_real_))
  
  return(list(
    max_intensity = max(intensities, na.rm = TRUE),
    rt = rts[which.max(intensities)],
    all_intensities = intensities
  ))
}

# ============================================================
# Extract m/z 52 from ALL files
# ============================================================

message("\n--- Extracting m/z 52 from all files ---")

mz52_data <- data.frame()

for (file in all_files) {
  ms1_path <- file.path(ms1_dir, paste0(file, ".ms1"))
  
  if (!file.exists(ms1_path)) {
    message("File not found: ", ms1_path)
    next
  }
  
  message("Processing ", file, "...")
  
  scans <- parse_ms1_file(ms1_path)
  ion_data <- extract_ion_at_rt(scans, 52, rdx_rt_target, rdx_rt_window)
  
  type <- case_when(
    file %in% calibrants ~ "Calibrant",
    file %in% blanks ~ "Blank",
    file %in% samples ~ "QC Sample"
  )
  
  conc <- case_when(
    file %in% calibrants ~ cal_conc[calibrants == file],
    file %in% blanks ~ 0,
    file == "009" ~ 6,
    file == "011" ~ 0.2,
    TRUE ~ NA_real_
  )
  
  mz52_data <- rbind(mz52_data, data.frame(
    file = file,
    type = type,
    conc = ifelse(length(conc) == 0, NA, conc),
    intensity = ion_data$max_intensity,
    rt = ion_data$rt
  ))
  
  message(sprintf("  m/z 52: %.0f (RT: %.3f min)", ion_data$max_intensity, ion_data$rt))
}

# ============================================================
# Also extract m/z 122 (current IS) and m/z 120 (RDX) for comparison
# ============================================================

message("\n--- Extracting m/z 122 and m/z 120 for comparison ---")

comparison_data <- data.frame()

for (file in all_files) {
  ms1_path <- file.path(ms1_dir, paste0(file, ".ms1"))
  if (!file.exists(ms1_path)) next
  
  scans <- parse_ms1_file(ms1_path)
  
  mz120_data <- extract_ion_at_rt(scans, 120, rdx_rt_target, rdx_rt_window)
  mz122_data <- extract_ion_at_rt(scans, 122, rdx_rt_target, rdx_rt_window)
  mz52_data_file <- extract_ion_at_rt(scans, 52, rdx_rt_target, rdx_rt_window)
  
  type <- case_when(
    file %in% calibrants ~ "Calibrant",
    file %in% blanks ~ "Blank",
    file %in% samples ~ "QC Sample"
  )
  
  conc_val <- case_when(
    file %in% calibrants ~ cal_conc[calibrants == file],
    file %in% blanks ~ 0,
    file == "009" ~ 6,
    file == "011" ~ 0.2,
    TRUE ~ NA_real_
  )
  
  comparison_data <- rbind(comparison_data, data.frame(
    file = file,
    type = type,
    conc = conc_val,
    mz52 = mz52_data_file$max_intensity,
    mz120 = mz120_data$max_intensity,
    mz122 = mz122_data$max_intensity
  ))
}

# ============================================================
# Calculate Statistics
# ============================================================

message("\n--- Calculating statistics ---")

cal_data <- comparison_data %>% filter(type == "Calibrant")
blank_data <- comparison_data %>% filter(type == "Blank")
sample_data <- comparison_data %>% filter(type == "QC Sample")

# m/z 52 statistics
mz52_mean_cal <- mean(cal_data$mz52, na.rm = TRUE)
mz52_sd_cal <- sd(cal_data$mz52, na.rm = TRUE)
mz52_cv_cal <- mz52_sd_cal / mz52_mean_cal * 100
mz52_mean_blank <- mean(blank_data$mz52, na.rm = TRUE)
mz52_signal_blank <- mz52_mean_cal / (mz52_mean_blank + 1)
mz52_cor_vs_conc <- cor(cal_data$mz52, cal_data$conc, use = "complete.obs")
mz52_cor_vs_rdx <- cor(cal_data$mz52, cal_data$mz120, use = "complete.obs")

# m/z 122 statistics (for comparison)
mz122_mean_cal <- mean(cal_data$mz122, na.rm = TRUE)
mz122_sd_cal <- sd(cal_data$mz122, na.rm = TRUE)
mz122_cv_cal <- mz122_sd_cal / mz122_mean_cal * 100
mz122_mean_blank <- mean(blank_data$mz122, na.rm = TRUE)
mz122_signal_blank <- mz122_mean_cal / (mz122_mean_blank + 1)
mz122_cor_vs_conc <- cor(cal_data$mz122, cal_data$conc, use = "complete.obs")
mz122_cor_vs_rdx <- cor(cal_data$mz122, cal_data$mz120, use = "complete.obs")

message("\n=== m/z 52 Statistics ===")
message(sprintf("Mean calibrant intensity: %.0f", mz52_mean_cal))
message(sprintf("SD: %.0f", mz52_sd_cal))
message(sprintf("CV: %.1f%%", mz52_cv_cal))
message(sprintf("Mean blank intensity: %.1f", mz52_mean_blank))
message(sprintf("Signal/Blank: %.1f", mz52_signal_blank))
message(sprintf("Correlation with concentration: %.3f", mz52_cor_vs_conc))
message(sprintf("Correlation with m/z 120: %.3f", mz52_cor_vs_rdx))

message("\n=== m/z 122 Statistics (Current IS) ===")
message(sprintf("Mean calibrant intensity: %.0f", mz122_mean_cal))
message(sprintf("SD: %.0f", mz122_sd_cal))
message(sprintf("CV: %.1f%%", mz122_cv_cal))
message(sprintf("Mean blank intensity: %.1f", mz122_mean_blank))
message(sprintf("Signal/Blank: %.1f", mz122_signal_blank))
message(sprintf("Correlation with concentration: %.3f", mz122_cor_vs_conc))
message(sprintf("Correlation with m/z 120: %.3f", mz122_cor_vs_rdx))

# ============================================================
# Signal-to-Noise Calculation
# ============================================================

message("\n--- Signal-to-Noise Analysis ---")

message("\nm/z 52 Intensities by File:")
for (i in 1:nrow(comparison_data)) {
  row <- comparison_data[i, ]
  message(sprintf("  %s (%s, %.1f ng): %.0f", 
                  row$file, row$type, row$conc, row$mz52))
}

noise_estimate <- sd(blank_data$mz52, na.rm = TRUE)
if (is.na(noise_estimate) || noise_estimate == 0) {
  noise_estimate <- 100
}

sn_cal <- cal_data$mz52 / noise_estimate
message(sprintf("\nEstimated noise (SD of blank): %.1f", noise_estimate))
message(sprintf("m/z 52 S/N in calibrants: %.1f - %.1f", min(sn_cal), max(sn_cal)))

# ============================================================
# Fragment Identification Attempt
# ============================================================

message("\n--- Fragment Identification ---")
message("\nPossible identities for m/z 52:")
message("  - C4H4+ (mass: 52.0313) - butadiene fragment?")
message("  - C3H2N+ (mass: 52.0130) - requires N")
message("  - C2H2N2+ (mass: 52.0062) - 2 nitrogens")
message("  - C3N+ (mass: 50.0030) - close but not exact")
message("  - C2H6N2+ (mass: 58.053) - too heavy")

message("\nFor 15N-RDX (C3H6N6O6, MW with 6x 15N = 228):")
message("  If fragment contains 1x 15N: +1 amu shift from normal RDX fragment")
message("  Normal RDX m/z 51 fragment + 15N = m/z 52?")
message("  C2H3N2+ (m/z 51) from RDX -> C2H3N(15N)+ (m/z 52) from 15N-RDX?")

message("\nNeed to check:")
message("  - Does unlabeled RDX have m/z 51 fragment?")
message("  - Does 15N-RDX have both m/z 51 and 52?")

# ============================================================
# Generate Plots
# ============================================================

message("\n--- Generating plots ---")

# Plot 1: m/z 52 intensity vs concentration
p1 <- ggplot(comparison_data, aes(x = conc, y = mz52)) +
  geom_point(aes(color = type, shape = type), size = 3) +
  geom_hline(yintercept = mz52_mean_blank, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.5, y = mz52_mean_blank + 50, 
           label = sprintf("Mean blank: %.0f", mz52_mean_blank), color = "gray50") +
  geom_smooth(data = cal_data, method = "lm", se = FALSE, color = "blue", alpha = 0.5) +
  scale_color_manual(values = c("Calibrant" = "blue", "Blank" = "gray50", "QC Sample" = "orange")) +
  scale_shape_manual(values = c("Calibrant" = 16, "Blank" = 1, "QC Sample" = 17)) +
  labs(
    title = "m/z 52 as Alternative IS Candidate",
    subtitle = sprintf("CV = %.1f%% | Cor vs Conc = %.3f | S/B = %.0f",
                       mz52_cv_cal, mz52_cor_vs_conc, mz52_signal_blank),
    x = "RDX Concentration (ng)",
    y = "m/z 52 Intensity"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_Intensity_vs_Conc.png"), p1,
       width = 8, height = 6, dpi = 150)

# Plot 2: Comparison of m/z 52 vs m/z 122
comparison_long <- comparison_data %>%
  select(file, type, conc, mz52, mz122) %>%
  pivot_longer(cols = c(mz52, mz122), names_to = "ion", values_to = "intensity")

p2 <- ggplot(comparison_long, aes(x = conc, y = intensity)) +
  geom_point(aes(color = type, shape = type), size = 2.5) +
  geom_smooth(data = subset(comparison_long, type == "Calibrant"),
              method = "lm", se = FALSE, alpha = 0.5) +
  facet_wrap(~ ion, scales = "free_y") +
  scale_color_manual(values = c("Calibrant" = "blue", "Blank" = "gray50", "QC Sample" = "orange")) +
  scale_shape_manual(values = c("Calibrant" = 16, "Blank" = 1, "QC Sample" = 17)) +
  labs(
    title = "Comparison: m/z 52 (Potential Alternative) vs m/z 122 (Current IS)",
    subtitle = "m/z 52 shows flat response; m/z 122 increases with concentration",
    x = "RDX Concentration (ng)",
    y = "Intensity"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_vs_mz122_Comparison.png"), p2,
       width = 10, height = 5, dpi = 150)

# Plot 3: Intensity ratios
cal_data_norm <- cal_data %>%
  mutate(
    mz52_norm = mz52 / mean(mz52),
    mz122_norm = mz122 / mean(mz122),
    mz120_norm = mz120 / mean(mz120)
  )

ratios_long <- cal_data_norm %>%
  select(conc, mz52_norm, mz122_norm, mz120_norm) %>%
  pivot_longer(cols = c(mz52_norm, mz122_norm, mz120_norm),
               names_to = "ion", values_to = "normalized_intensity") %>%
  mutate(ion = gsub("_norm", "", ion))

p3 <- ggplot(ratios_long, aes(x = conc, y = normalized_intensity, color = ion)) +
  geom_point(size = 2.5) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("mz52" = "darkgreen", "mz122" = "red", "mz120" = "black")) +
  labs(
    title = "Normalized Ion Intensity vs Concentration",
    subtitle = "Flat line = independent of RDX (ideal IS behavior)",
    x = "RDX Concentration (ng)",
    y = "Normalized Intensity (mean = 1)"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_Normalized_Comparison.png"), p3,
       width = 8, height = 6, dpi = 150)

# ============================================================
# Calculate potential improvement in quantification
# ============================================================

message("\n--- Potential Quantification Improvement ---")

# Calculate ratios using m/z 52 vs m/z 122
cal_data_with_ratios <- cal_data %>%
  mutate(
    ratio_52 = mz120 / (mz52 + 1),
    ratio_122 = mz120 / (mz122 + 1)
  )

# Calculate CV of ratios
cv_ratio_52 <- sd(cal_data_with_ratios$ratio_52) / mean(cal_data_with_ratios$ratio_52) * 100
cv_ratio_122 <- sd(cal_data_with_ratios$ratio_122) / mean(cal_data_with_ratios$ratio_122) * 100

message(sprintf("\nm/z 120 / m/z 52 ratio CV: %.1f%%", cv_ratio_52))
message(sprintf("m/z 120 / m/z 122 ratio CV: %.1f%%", cv_ratio_122))

if (cv_ratio_52 < cv_ratio_122) {
  improvement <- (cv_ratio_122 - cv_ratio_52) / cv_ratio_122 * 100
  message(sprintf("\n>>> m/z 52 ratio CV is %.0f%% lower than m/z 122 ratio", improvement))
} else {
  message("\n>>> m/z 52 does not improve ratio CV")
}

# Fit calibration curves
fit_52 <- lm(ratio_52 ~ conc, data = cal_data_with_ratios)
fit_122 <- lm(ratio_122 ~ conc, data = cal_data_with_ratios)

r2_52 <- summary(fit_52)$r.squared
r2_122 <- summary(fit_122)$r.squared

message(sprintf("\nCalibration R-squared:"))
message(sprintf("  m/z 52 ratio: %.4f", r2_52))
message(sprintf("  m/z 122 ratio: %.4f", r2_122))

# ============================================================
# Summary
# ============================================================

message("\n============================================================")
message("SUMMARY")
message("============================================================")

message("\n1. DATA VERIFICATION:")
message(sprintf("   m/z 52 detected in %d/%d calibrants", sum(!is.na(cal_data$mz52)), nrow(cal_data)))
message(sprintf("   m/z 52 detected in %d/%d blanks", sum(!is.na(blank_data$mz52) & blank_data$mz52 > 0), nrow(blank_data)))
message(sprintf("   m/z 52 detected in %d/%d samples", sum(!is.na(sample_data$mz52)), nrow(sample_data)))

message("\n2. STABILITY:")
message(sprintf("   CV of m/z 52: %.1f%% (vs m/z 122: %.1f%%)", mz52_cv_cal, mz122_cv_cal))
message(sprintf("   Correlation with concentration: %.3f (vs m/z 122: %.3f)", mz52_cor_vs_conc, mz122_cor_vs_conc))

message("\n3. SIGNAL QUALITY:")
message(sprintf("   Mean intensity: %.0f", mz52_mean_cal))
message(sprintf("   Signal/Blank: %.0f", mz52_signal_blank))

message("\n4. QUANTIFICATION POTENTIAL:")
message(sprintf("   Ratio CV: %.1f%% (m/z 52) vs %.1f%% (m/z 122)", cv_ratio_52, cv_ratio_122))
message(sprintf("   Calibration R²: %.4f (m/z 52) vs %.4f (m/z 122)", r2_52, r2_122))

message("\n============================================================")
message("CONCLUSION")
message("============================================================")

if (mz52_cv_cal < 15 && mz52_signal_blank > 5 && abs(mz52_cor_vs_conc) < 0.7) {
  message("\n>>> m/z 52 IS A VIABLE ALTERNATIVE IS CANDIDATE")
  message("    - Low CV indicates stable signal")
  message("    - Low correlation indicates independence from RDX")
  message("    - Good signal above blank")
  message("\n    RECOMMENDED: Update SIM method to include m/z 52")
  message("    and test quantification performance")
} else {
  message("\n>>> m/z 52 has issues:")
  if (mz52_cv_cal >= 15) message("    - High CV")
  if (mz52_signal_blank <= 5) message("    - Low signal/blank")
  if (abs(mz52_cor_vs_conc) >= 0.7) message("    - Correlated with concentration")
}

message("\nOutput saved to: ", output_dir)
message("  - mz52_Intensity_vs_Conc.png")
message("  - mz52_vs_mz122_Comparison.png")
message("  - mz52_Normalized_Comparison.png")

write.csv(comparison_data, file.path(output_dir, "mz52_FullData.csv"), row.names = FALSE)

message("\n=== Done ===")
