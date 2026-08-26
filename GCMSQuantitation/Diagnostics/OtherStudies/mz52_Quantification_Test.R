# ============================================================
# mz52_Quantification_Test.R
# Standalone test script to evaluate m/z 52 as alternative IS
# for RDX quantification, comparing against current m/z 122 IS.
#
# Uses TIC data from Filter Test dataset only.
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

message("=== m/z 52 Quantification Test (RDX) ===")

# ============================================================
# Configuration
# ============================================================

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConverterMs/"
sim_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConvertedRcode/"
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/TIC_Diagnostics/"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rdx_rt_target <- 5.79
rdx_rt_window <- 0.15

# Calibration levels
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
  max_intensity <- 0
  max_rt <- NA
  
  for (scan in scans) {
    if (is.null(scan$rt) || is.null(scan$ions)) next
    if (abs(scan$rt - target_rt) > window) next
    
    mz_match <- scan$ions$mz >= (target_mz - mz_tol) & scan$ions$mz <= (target_mz + mz_tol)
    if (any(mz_match)) {
      intensity <- max(scan$ions$intensity[mz_match], na.rm = TRUE)
      if (intensity > max_intensity) {
        max_intensity <- intensity
        max_rt <- scan$rt
      }
    }
  }
  
  if (max_intensity == 0) return(list(intensity = NA_real_, rt = NA_real_))
  return(list(intensity = max_intensity, rt = max_rt))
}

fit_quadratic_calibration <- function(ratio, conc) {
  df <- data.frame(ratio = ratio, conc = conc)
  df <- df[!is.na(df$ratio) & !is.na(df$conc), ]
  
  if (nrow(df) < 3) return(NULL)
  
  model <- lm(ratio ~ conc + I(conc^2), data = df)
  
  coefs <- coef(model)
  
  return(list(
    a = coefs[3],
    b = coefs[2],
    c = coefs[1],
    r2 = summary(model)$r.squared,
    model = model
  ))
}

solve_quadratic <- function(ratio, a, b, c) {
  if (is.na(ratio)) return(NA_real_)
  if (is.na(a) || a == 0) return(NA_real_)
  
  disc <- b^2 - 4*a*(c - ratio)
  
  if (disc < 0) return(NA_real_)
  
  conc1 <- (-b + sqrt(disc)) / (2*a)
  conc2 <- (-b - sqrt(disc)) / (2*a)
  
  if (conc1 >= 0 && conc2 >= 0) {
    return(min(conc1, conc2))
  } else if (conc1 >= 0) {
    return(conc1)
  } else if (conc2 >= 0) {
    return(conc2)
  }
  
  return(NA_real_)
}

# ============================================================
# Extract Ion Data from All Files
# ============================================================

message("\n--- Extracting ion data from TIC files ---")

all_data <- data.frame()

for (file in all_files) {
  ms1_path <- file.path(ms1_dir, paste0(file, ".ms1"))
  
  if (!file.exists(ms1_path)) {
    message("File not found: ", ms1_path)
    next
  }
  
  message("Processing ", file, "...")
  
  scans <- parse_ms1_file(ms1_path)
  
  mz52 <- extract_ion_at_rt(scans, 52, rdx_rt_target, rdx_rt_window)
  mz120 <- extract_ion_at_rt(scans, 120, rdx_rt_target, rdx_rt_window)
  mz122 <- extract_ion_at_rt(scans, 122, rdx_rt_target, rdx_rt_window)
  
  type <- case_when(
    file %in% calibrants ~ "Calibrant",
    file %in% blanks ~ "Blank",
    file %in% samples ~ "Sample"
  )
  
  conc <- if (file %in% calibrants) {
    cal_conc[match(file, calibrants)]
  } else if (file %in% blanks) {
    0
  } else {
    NA_real_
  }
  
  mz52_int <- ifelse(is.null(mz52$intensity), NA_real_, mz52$intensity)
  mz120_int <- ifelse(is.null(mz120$intensity), NA_real_, mz120$intensity)
  mz122_int <- ifelse(is.null(mz122$intensity), NA_real_, mz122$intensity)
  
  all_data <- rbind(all_data, data.frame(
    file = file,
    type = type,
    conc_ng = conc,
    mz52 = mz52_int,
    mz120 = mz120_int,
    mz122 = mz122_int,
    stringsAsFactors = FALSE
  ))
}

message("\n--- Extracted Data Summary ---")
print(all_data)

# ============================================================
# Calculate Ratios
# ============================================================

all_data <- all_data %>%
  mutate(
    ratio_mz52 = mz120 / (mz52 + 1),
    ratio_mz122 = mz120 / (mz122 + 1)
  )

# ============================================================
# Build Calibration Data
# ============================================================

message("\n--- Building calibration curves ---")

cal_data <- all_data %>% filter(type == "Calibrant")

cal_mz52 <- fit_quadratic_calibration(cal_data$ratio_mz52, cal_data$conc_ng)
cal_mz122 <- fit_quadratic_calibration(cal_data$ratio_mz122, cal_data$conc_ng)

message("\nm/z 52 Calibration:")
message(sprintf("  Ratio = %.6f * conc^2 + %.4f * conc + %.2f", cal_mz52$a, cal_mz52$b, cal_mz52$c))
message(sprintf("  R² = %.4f", cal_mz52$r2))

message("\nm/z 122 Calibration:")
message(sprintf("  Ratio = %.6f * conc^2 + %.4f * conc + %.2f", cal_mz122$a, cal_mz122$b, cal_mz122$c))
message(sprintf("  R² = %.4f", cal_mz122$r2))

# ============================================================
# Quantify All Samples
# ============================================================

message("\n--- Quantifying samples ---")

all_data <- all_data %>%
  rowwise() %>%
  mutate(
    conc_calc_mz52 = solve_quadratic(ratio_mz52, cal_mz52$a, cal_mz52$b, cal_mz52$c),
    conc_calc_mz122 = solve_quadratic(ratio_mz122, cal_mz122$a, cal_mz122$b, cal_mz122$c)
  ) %>%
  ungroup()

# Calculate % difference for calibrants (accuracy check)
all_data <- all_data %>%
  mutate(
    accuracy_mz52 = ifelse(type == "Calibrant", 
                           (conc_calc_mz52 - conc_ng) / conc_ng * 100, NA_real_),
    accuracy_mz122 = ifelse(type == "Calibrant",
                           (conc_calc_mz122 - conc_ng) / conc_ng * 100, NA_real_)
  )

# ============================================================
# Results Summary
# ============================================================

message("\n============================================================")
message("QUANTIFICATION RESULTS")
message("============================================================")

message("\n=== Calibrant Recovery (Accuracy %) ===")
cal_results <- all_data %>% filter(type == "Calibrant")
message("\nm/z 52 IS:")
for (i in 1:nrow(cal_results)) {
  message(sprintf("  %s (%.1f ng): calc=%.2f ng, accuracy=%.1f%%",
                  cal_results$file[i], cal_results$conc_ng[i],
                  cal_results$conc_calc_mz52[i], cal_results$accuracy_mz52[i]))
}

message("\nm/z 122 IS (Current):")
for (i in 1:nrow(cal_results)) {
  message(sprintf("  %s (%.1f ng): calc=%.2f ng, accuracy=%.1f%%",
                  cal_results$file[i], cal_results$conc_ng[i],
                  cal_results$conc_calc_mz122[i], cal_results$accuracy_mz122[i]))
}

message("\n=== Sample Results ===")
sample_results <- all_data %>% filter(type == "Sample")
for (i in 1:nrow(sample_results)) {
  message(sprintf("\n%s:", sample_results$file[i]))
  message(sprintf("  m/z 52 IS:  %.2f ng", sample_results$conc_calc_mz52[i]))
  message(sprintf("  m/z 122 IS: %.2f ng", sample_results$conc_calc_mz122[i]))
  if (!is.na(sample_results$conc_calc_mz52[i]) && !is.na(sample_results$conc_calc_mz122[i])) {
    diff <- (sample_results$conc_calc_mz52[i] - sample_results$conc_calc_mz122[i]) / 
            sample_results$conc_calc_mz122[i] * 100
    message(sprintf("  Difference: %.1f%%", diff))
  }
}

message("\n=== Blank Results ===")
blank_results <- all_data %>% filter(type == "Blank")
for (i in 1:nrow(blank_results)) {
  message(sprintf("\n%s:", blank_results$file[i]))
  message(sprintf("  m/z 52 IS:  %.2f ng", blank_results$conc_calc_mz52[i]))
  message(sprintf("  m/z 122 IS: %.2f ng", blank_results$conc_calc_mz122[i]))
}

# ============================================================
# Statistics
# ============================================================

message("\n============================================================")
message("STATISTICAL COMPARISON")
message("============================================================")

cal_accuracy_mz52 <- mean(abs(cal_results$accuracy_mz52), na.rm = TRUE)
cal_accuracy_mz122 <- mean(abs(cal_results$accuracy_mz122), na.rm = TRUE)

message(sprintf("\nMean |Accuracy| (calibrants):"))
message(sprintf("  m/z 52:  %.1f%%", cal_accuracy_mz52))
message(sprintf("  m/z 122: %.1f%%", cal_accuracy_mz122))

cal_rsd_mz52 <- sd(cal_results$mz52, na.rm = TRUE) / mean(cal_results$mz52, na.rm = TRUE) * 100
cal_rsd_mz122 <- sd(cal_results$mz122, na.rm = TRUE) / mean(cal_results$mz122, na.rm = TRUE) * 100

message(sprintf("\nRSD of IS signal (calibrants):"))
message(sprintf("  m/z 52:  %.1f%%", cal_rsd_mz52))
message(sprintf("  m/z 122: %.1f%%", cal_rsd_mz122))

message(sprintf("\nCalibration R²:"))
message(sprintf("  m/z 52:  %.4f", cal_mz52$r2))
message(sprintf("  m/z 122: %.4f", cal_mz122$r2))

# ============================================================
# Plots
# ============================================================

message("\n--- Generating comparison plots ---")

# Plot 1: Calibration curves
cal_plot_data <- cal_data %>%
  select(file, conc_ng, ratio_mz52, ratio_mz122) %>%
  pivot_longer(cols = c(ratio_mz52, ratio_mz122),
               names_to = "IS", values_to = "ratio") %>%
  mutate(IS = ifelse(IS == "ratio_mz52", "m/z 52 IS", "m/z 122 IS"))

p1 <- ggplot(cal_plot_data, aes(x = conc_ng, y = ratio, color = IS)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  facet_wrap(~ IS, scales = "free_y") +
  labs(
    title = "Calibration Curves: m/z 52 vs m/z 122 as IS",
    subtitle = sprintf("m/z 52: R²=%.3f | m/z 122: R²=%.3f", cal_mz52$r2, cal_mz122$r2),
    x = "RDX Concentration (ng)",
    y = "Ratio (m/z 120 / IS)"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_Calibration_Comparison.png"), p1, width = 10, height = 5, dpi = 150)

# Plot 2: IS signal stability
is_stability <- cal_data %>%
  select(file, conc_ng, mz52, mz122) %>%
  pivot_longer(cols = c(mz52, mz122),
               names_to = "IS", values_to = "intensity") %>%
  mutate(IS = ifelse(IS == "mz52", "m/z 52 IS", "m/z 122 IS"))

p2 <- ggplot(is_stability, aes(x = conc_ng, y = intensity, color = IS)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  geom_hline(data = is_stability %>% group_by(IS) %>% summarise(mean_int = mean(intensity)),
             aes(yintercept = mean_int, color = IS), linetype = "dashed") +
  labs(
    title = "IS Signal Stability Across Calibration Range",
    subtitle = sprintf("m/z 52 RSD=%.1f%% | m/z 122 RSD=%.1f%%", cal_rsd_mz52, cal_rsd_mz122),
    x = "RDX Concentration (ng)",
    y = "IS Intensity"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "mz52_IS_Stability.png"), p2, width = 8, height = 5, dpi = 150)

# Plot 3: Calibrant accuracy
accuracy_plot <- cal_results %>%
  select(file, conc_ng, accuracy_mz52, accuracy_mz122) %>%
  pivot_longer(cols = c(accuracy_mz52, accuracy_mz122),
               names_to = "IS", values_to = "accuracy") %>%
  mutate(IS = ifelse(IS == "accuracy_mz52", "m/z 52 IS", "m/z 122 IS"))

p3 <- ggplot(accuracy_plot, aes(x = factor(conc_ng), y = accuracy, fill = IS)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_hline(yintercept = c(-15, 15), linetype = "dotted", color = "red") +
  labs(
    title = "Calibrant Accuracy: m/z 52 vs m/z 122 IS",
    subtitle = "Dotted red lines = ±15% acceptance limits",
    x = "Nominal Concentration (ng)",
    y = "Accuracy (%)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "mz52_Accuracy_Comparison.png"), p3, width = 10, height = 6, dpi = 150)

# ============================================================
# Save Results
# ============================================================

write.csv(all_data, file.path(output_dir, "mz52_Quantification_Results.csv"), row.names = FALSE)

# ============================================================
# Final Summary
# ============================================================

message("\n============================================================")
message("FINAL ASSESSMENT")
message("============================================================")

if (cal_rsd_mz52 < cal_rsd_mz122 && cal_accuracy_mz52 < cal_accuracy_mz122) {
  message("\n>>> m/z 52 performs BETTER than m/z 122")
  message(sprintf("    - %.0f%% lower RSD", (cal_rsd_mz122 - cal_rsd_mz52) / cal_rsd_mz122 * 100))
  message(sprintf("    - %.0f%% better accuracy", (cal_accuracy_mz122 - cal_accuracy_mz52) / cal_accuracy_mz122 * 100))
  message("\n    RECOMMENDATION: Consider implementing m/z 52 as alternative IS")
} else if (cal_rsd_mz52 < cal_rsd_mz122) {
  message("\n>>> m/z 52 has BETTER stability but similar accuracy")
  message(sprintf("    - %.0f%% lower RSD", (cal_rsd_mz122 - cal_rsd_mz52) / cal_rsd_mz122 * 100))
  message("\n    RECOMMENDATION: m/z 52 may be beneficial for precision")
} else if (cal_accuracy_mz52 < cal_accuracy_mz122) {
  message("\n>>> m/z 52 has BETTER accuracy but similar stability")
  message(sprintf("    - %.0f%% better accuracy", (cal_accuracy_mz122 - cal_accuracy_mz52) / cal_accuracy_mz122 * 100))
  message("\n    RECOMMENDATION: m/z 52 may be beneficial for accuracy")
} else {
  message("\n>>> m/z 52 does NOT show clear improvement over m/z 122")
  message("    Further investigation needed or mathematical correction preferred")
}

message("\n============================================================")
message("OUTPUT FILES")
message("============================================================")
message(sprintf("\nOutput directory: %s", output_dir))
message("  - mz52_Quantification_Results.csv")
message("  - mz52_Calibration_Comparison.png")
message("  - mz52_IS_Stability.png")
message("  - mz52_Accuracy_Comparison.png")

message("\n=== Done ===")
