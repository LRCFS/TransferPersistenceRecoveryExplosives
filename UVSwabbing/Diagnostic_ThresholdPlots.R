### Diagnostic Script - Plot Threshold Curves for One Sample ###
# This script creates comparison plots to troubleshoot recovery calculations
# Plots: 1) Uncorrected, 2) After 90% alignment, 3) Final corrected, 4) Recovery curve

# === CONFIGURATION ===
library(dplyr)
library(ggplot2)
library(tidyverse)

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# Set directories
ThresholdResults.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/UV Powder Swabbing/ASTRA Testing/ThresholdResults/"
Output.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/UV Powder Swabbing/ASTRA Testing/Diagnostic_Plots/"
dir.create(Output.dir, recursive = TRUE, showWarnings = FALSE)

# Select sample to analyze
sample_name <- "Surface1_Rep1"
summary_path <- file.path(ThresholdResults.dir, sample_name, "Summary.csv")

cat("=== DIAGNOSTIC THRESHOLD ANALYSIS ===\n")
cat(sprintf("Sample: %s\n", sample_name))
cat(sprintf("Reading: %s\n\n", summary_path))

# === READ DATA ===
raw_data <- read.csv(summary_path)
cat(sprintf("Raw data dimensions: %d rows, %d columns\n", nrow(raw_data), ncol(raw_data)))
cat(sprintf("Column names: %s\n\n", paste(names(raw_data), collapse = ", ")))

# Print first few and last few rows for inspection
cat("=== RAW DATA PREVIEW ===\n")
cat("First 5 rows:\n")
print(head(raw_data, 5))
cat("\nRows 99-102 (around boundary):\n")
print(raw_data[99:102, ])
cat("\nLast 5 rows:\n")
print(tail(raw_data, 5))

# === EXTRACT AREA VALUES ===
# The Summary.csv has 301 rows: header + 100 Blank + 100 Before + 100 After

cat("\n=== EXTRACTING BLANK/BEFORE/AFTER ===\n")

# Extract by position (rows 1-100, 101-200, 201-300)
Blank_area <- raw_data$X.Area[1:100]
Before_area <- raw_data$X.Area[101:200]
After_area <- raw_data$X.Area[201:300]

cat(sprintf("Blank area values (first 5): %s\n", paste(round(Blank_area[1:5], 3), collapse = ", ")))
cat(sprintf("Blank area values (last 5): %s\n", paste(round(Blank_area[96:100], 3), collapse = ", ")))
cat(sprintf("Before area values (first 5): %s\n", paste(round(Before_area[1:5], 3), collapse = ", ")))
cat(sprintf("Before area values (last 5): %s\n", paste(round(Before_area[96:100], 3), collapse = ", ")))
cat(sprintf("After area values (first 5): %s\n", paste(round(After_area[1:5], 3), collapse = ", ")))
cat(sprintf("After area values (last 5): %s\n", paste(round(After_area[96:100], 3), collapse = ", ")))

# === EXTRACT THRESHOLD FROM SLICE NAMES ===
cat("\n=== THRESHOLD INTERPRETATION ===\n")

# Extract the K number from Slice names (e.g., "Slice_K_100" -> 100)
slice_names <- raw_data$Slice[1:100]
Threshold <- as.numeric(gsub("Slice_K_", "", slice_names))

cat(sprintf("Threshold values (first 5): %s\n", paste(Threshold[1:5], collapse = ", ")))
cat(sprintf("Threshold values (last 5): %s\n", paste(Threshold[96:100], collapse = ", ")))

# === CREATE UNCORRECTED DATA FRAME ===
Uncorrected <- data.frame(
  Threshold = Threshold,
  Blank = Blank_area,
  Before = Before_area,
  After = After_area
)

cat("\n=== UNCORRECTED DATA SAMPLE ===\n")
cat("Thresholds 1-5:\n")
print(Uncorrected[1:5, ])
cat("\nThresholds 48-52 (around 50):\n")
print(Uncorrected[48:52, ])
cat("\nThresholds 96-100:\n")
print(Uncorrected[96:100, ])

# Check data order
cat("\n=== DATA ORDER CHECK (at threshold ~50) ===\n")
idx50_approx <- which.min(abs(Uncorrected$Threshold - 50))
cat(sprintf("At threshold %d: Blank=%.3f, Before=%.3f, After=%.3f\n",
            Uncorrected$Threshold[idx50_approx],
            Uncorrected$Blank[idx50_approx],
            Uncorrected$Before[idx50_approx],
            Uncorrected$After[idx50_approx]))

# === PLOT 1: UNCORRECTED CURVES ===
cat("\n=== CREATING PLOT 1: UNCORRECTED ===\n")

Uncorrected_long <- pivot_longer(Uncorrected,
                                  cols = c(Blank, Before, After),
                                  names_to = "Image",
                                  values_to = "Area")

p1 <- ggplot(Uncorrected_long, aes(x = Threshold, y = Area, color = Image)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal_image_stage) +
  labs(title = paste0(sample_name, " - UNCORRECTED Threshold Curves"),
       x = "Threshold",
       y = "% Area",
       color = "Image Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = paste0(sample_name, "_01_Uncorrected.png"),
  plot = p1,
  path = Output.dir,
  width = 10,
  height = 6,
  dpi = 300
)
cat(sprintf("Plot 1 saved: %s%s_01_Uncorrected.png\n", Output.dir, sample_name))

# === ALIGNMENT FUNCTION AT 90% ===
align_at_90 <- function(df) {
  # Find where Blank ≈ 90%
  Index90 <- which.min(abs(df$Blank - 90))
  Blank90 <- df$Blank[Index90]
  
  cat(sprintf("  Blank at threshold %d has area %.2f (target 90)\n", df$Threshold[Index90], Blank90))
  
  # Find where After ≈ 90%
  After90_idx <- which.min(abs(df$After - 90))
  cat(sprintf("  After at threshold %d has area %.2f\n", df$Threshold[After90_idx], df$After[After90_idx]))
  
  # Calculate offset (positive = need to shift right)
  After_offset <- Index90 - After90_idx
  
  cat(sprintf("  After offset: %d (positive = shift right)\n", After_offset))
  
  # Shift After RIGHT if offset > 0
  if (After_offset > 0) {
    df$After <- c(rep(100, After_offset), head(df$After, -After_offset))
    cat(sprintf("  After shifted RIGHT by %d\n", After_offset))
  } else {
    cat("  After not shifted (offset <= 0)\n")
  }
  
  # Get After's new value at Index90
  After90_aligned <- df$After[Index90]
  cat(sprintf("  After aligned value at Index90: %.2f\n", After90_aligned))
  
  # Find where Before ≈ After's aligned 90% value
  Before90_idx <- which.min(abs(df$Before - After90_aligned))
  cat(sprintf("  Before at threshold %d has area %.2f\n", df$Threshold[Before90_idx], df$Before[Before90_idx]))
  
  # Calculate offset for Before
  Before_offset <- Index90 - Before90_idx
  
  cat(sprintf("  Before offset: %d (positive = shift right)\n", Before_offset))
  
  # Shift Before RIGHT if offset > 0
  if (Before_offset > 0) {
    df$Before <- c(rep(100, Before_offset), head(df$Before, -Before_offset))
    cat(sprintf("  Before shifted RIGHT by %d\n", Before_offset))
  } else {
    cat("  Before not shifted (offset <= 0)\n")
  }
  
  return(list(
    data = df,
    Index90 = Index90,
    Blank90 = Blank90,
    After_offset = After_offset,
    Before_offset = Before_offset
  ))
}

# === APPLY 90% ALIGNMENT ===
cat("\n=== APPLYING 90% ALIGNMENT ===\n")
Aligned90 <- align_at_90(Uncorrected)

cat("\n90% Alignment Summary:\n")
cat(sprintf("  Threshold where Blank ≈ 90: %d (area = %.2f)\n", 
            Aligned90$data$Threshold[Aligned90$Index90], Aligned90$Blank90))
cat(sprintf("  After shifted right by: %d\n", Aligned90$After_offset))
cat(sprintf("  Before shifted right by: %d\n", Aligned90$Before_offset))

# === PLOT 2: AFTER 90% ALIGNMENT ===
cat("\n=== CREATING PLOT 2: AFTER 90% ALIGNMENT ===\n")

Aligned90_long <- pivot_longer(Aligned90$data,
                                cols = c(Blank, Before, After),
                                names_to = "Image",
                                values_to = "Area")

p2 <- ggplot(Aligned90_long, aes(x = Threshold, y = Area, color = Image)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal_image_stage) +
  labs(title = paste0(sample_name, " - After 90% Alignment"),
       x = "Threshold",
       y = "% Area",
       color = "Image Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = paste0(sample_name, "_02_After90Alignment.png"),
  plot = p2,
  path = Output.dir,
  width = 10,
  height = 6,
  dpi = 300
)
cat(sprintf("Plot 2 saved: %s%s_02_After90Alignment.png\n", Output.dir, sample_name))

# === ORDER CHECK FUNCTION AT 50% ===
check_order_at_50 <- function(df, original_df) {
  # Constants
  TOLERANCE <- 2
  MAX_SHIFTS <- 10
  
  # Find where Blank ≈ 50%
  Index50 <- which.min(abs(df$Blank - 50))
  
  # Get initial values
  Blank50 <- df$Blank[Index50]
  After50 <- df$After[Index50]
  Before50 <- df$Before[Index50]
  
  cat(sprintf("  At threshold %d: Blank=%.2f, After=%.2f, Before=%.2f\n",
              df$Threshold[Index50], Blank50, After50, Before50))
  
  # Initialize counters and flags
  before_shifts <- 0
  after_shifts <- 0
  after_flagged <- FALSE
  before_flagged <- FALSE
  
  # Storage for reversion
  after_values <- df$After
  before_values <- df$Before
  
  # Check: After > Blank?
  cat(sprintf("  Check: After (%.2f) > Blank (%.2f)? %s\n",
              After50, Blank50, ifelse(After50 > Blank50, "YES", "NO")))
  
  if (After50 <= Blank50) {
    After_Blank_diff <- Blank50 - After50
    
    if (After_Blank_diff > TOLERANCE) {
      # Difference significant - try to shift
      while (After50 <= Blank50 && after_shifts < MAX_SHIFTS) {
        after_shifts <- after_shifts + 1
        df$After <- c(100, head(df$After, -1))
        After50 <- df$After[Index50]
      }
      
      if (after_shifts >= MAX_SHIFTS) {
        cat(sprintf("  WARNING: After shift limit (%d) reached - reverting, no correction\n", MAX_SHIFTS))
        df$After <- original_df$After
        after_flagged <- TRUE
        after_shifts <- 0
      } else {
        cat(sprintf("  After shifted RIGHT by %d (now After50 = %.2f)\n", after_shifts, After50))
      }
    } else {
      cat(sprintf("  INFO: After <= Blank but within tolerance (%.2f) - no shift\n", TOLERANCE))
      after_flagged <- TRUE
    }
  }
  
  # Check: Before > After?
  Before50 <- df$Before[Index50]
  After50 <- df$After[Index50]
  
  cat(sprintf("  Check: Before (%.2f) > After (%.2f)? %s\n",
              Before50, After50, ifelse(Before50 > After50, "YES", "NO")))
  
  if (Before50 <= After50) {
    Before_After_diff <- After50 - Before50
    
    if (Before_After_diff > TOLERANCE) {
      # Difference significant - try to shift
      while (Before50 <= After50 && before_shifts < MAX_SHIFTS) {
        before_shifts <- before_shifts + 1
        df$Before <- c(100, head(df$Before, -1))
        Before50 <- df$Before[Index50]
      }
      
      if (before_shifts >= MAX_SHIFTS) {
        cat(sprintf("  WARNING: Before shift limit (%d) reached - reverting, no correction\n", MAX_SHIFTS))
        df$Before <- original_df$Before
        before_flagged <- TRUE
        before_shifts <- 0
      } else {
        cat(sprintf("  Before shifted RIGHT by %d (now Before50 = %.2f)\n", before_shifts, Before50))
      }
    } else {
      cat(sprintf("  INFO: Before <= After but within tolerance (%.2f) - no shift\n", TOLERANCE))
      before_flagged <- TRUE
    }
  }
  
  return(list(
    data = df,
    Index50 = Index50,
    Blank50 = Blank50,
    After50 = df$After[Index50],
    Before50 = df$Before[Index50],
    before_shifts = before_shifts,
    after_shifts = after_shifts,
    flagged = (after_flagged || before_flagged),
    after_flagged = after_flagged,
    before_flagged = before_flagged
  ))
}

# === APPLY 50% ORDER CHECK ===
cat("\n=== APPLYING 50% ORDER CHECK ===\n")
Corrected <- check_order_at_50(Aligned90$data, Uncorrected)

cat("\n50% Order Check Summary:\n")
cat(sprintf("  Threshold where Blank ≈ 50: %d\n", Corrected$data$Threshold[Corrected$Index50]))
cat(sprintf("  Final values: Blank=%.2f, After=%.2f, Before=%.2f\n",
            Corrected$Blank50, Corrected$After50, Corrected$Before50))
cat(sprintf("  Additional After shifts: %d\n", Corrected$after_shifts))
cat(sprintf("  Additional Before shifts: %d\n", Corrected$before_shifts))

if (Corrected$flagged) {
  cat("  *** FLAGGED: Curve order issues - no 50% correction applied ***\n")
}

# Verify final order
if (Corrected$Before50 > Corrected$After50 && Corrected$After50 > Corrected$Blank50) {
  cat("  Order check PASSED: Before > After > Blank\n")
} else {
  cat("  Order check FAILED: Check data!\n")
}

# === PLOT 3: FINAL CORRECTED ===
cat("\n=== CREATING PLOT 3: FINAL CORRECTED ===\n")

Corrected_long <- pivot_longer(Corrected$data,
                                cols = c(Blank, Before, After),
                                names_to = "Image",
                                values_to = "Area")

p3 <- ggplot(Corrected_long, aes(x = Threshold, y = Area, color = Image)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal_image_stage) +
  labs(title = paste0(sample_name, " - FINAL Corrected Threshold Curves"),
       x = "Threshold",
       y = "% Area",
       color = "Image Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = paste0(sample_name, "_03_FinalCorrected.png"),
  plot = p3,
  path = Output.dir,
  width = 10,
  height = 6,
  dpi = 300
)
cat(sprintf("Plot 3 saved: %s%s_03_FinalCorrected.png\n", Output.dir, sample_name))

# === RECOVERY CALCULATION ===
cat("\n=== CALCULATING RECOVERY ===\n")

# Add recovery calculations to corrected data
Corrected$data$BeforeAfter <- Corrected$data$Before - Corrected$data$After
Corrected$data$BeforeBlank <- Corrected$data$Before - Corrected$data$Blank
Corrected$data$Recovery <- Corrected$data$BeforeAfter / Corrected$data$BeforeBlank * 100

# Calculate statistics for all thresholds
cat("Recovery (all thresholds):\n")
cat(sprintf("  Min: %.2f%%\n", min(Corrected$data$Recovery, na.rm = TRUE)))
cat(sprintf("  Max: %.2f%%\n", max(Corrected$data$Recovery, na.rm = TRUE)))
cat(sprintf("  Mean: %.2f%%\n", mean(Corrected$data$Recovery, na.rm = TRUE)))

# Calculate statistics for threshold 35-65
recovery_zone <- Corrected$data[Corrected$data$Threshold >= 35 & Corrected$data$Threshold <= 65, ]

cat("\nRecovery (thresholds 35-65):\n")
cat(sprintf("  Min: %.2f%%\n", min(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  Max: %.2f%%\n", max(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  Mean: %.2f%%\n", mean(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  StdDev: %.2f%%\n", sd(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  Range: %.2f%%\n", max(recovery_zone$Recovery, na.rm = TRUE) - min(recovery_zone$Recovery, na.rm = TRUE)))

# Check for negative recoveries
negative_count <- sum(Corrected$data$Recovery < 0, na.rm = TRUE)
total_count <- sum(!is.na(Corrected$data$Recovery))
cat(sprintf("\nNegative recoveries: %d out of %d (%.1f%%)\n",
            negative_count, total_count, negative_count/total_count*100))

if (negative_count > 0) {
  cat("\nThresholds with negative recovery (first 10):\n")
  neg_recovery <- Corrected$data[Corrected$data$Recovery < 0, c("Threshold", "Blank", "Before", "After", "Recovery")]
  print(head(neg_recovery, 10))
}

# === PLOT 4: RECOVERY CURVE ===
cat("\n=== CREATING PLOT 4: RECOVERY CURVE ===\n")

p4 <- ggplot(Corrected$data, aes(x = Threshold, y = Recovery)) +
  geom_line(linewidth = 1, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  labs(title = paste0(sample_name, " - Recovery vs Threshold"),
       subtitle = "Recovery = (Before-After)/(Before-Blank) * 100",
       x = "Threshold",
       y = "Recovery (%)") +
  ylim(-50, 150) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = paste0(sample_name, "_04_RecoveryCurve.png"),
  plot = p4,
  path = Output.dir,
  width = 10,
  height = 6,
  dpi = 300
)
cat(sprintf("Plot 4 saved: %s%s_04_RecoveryCurve.png\n", Output.dir, sample_name))

# === SUMMARY TABLE ===
cat("\n=== SUMMARY TABLE (Thresholds 35-65) ===\n")
summary_table <- Corrected$data[Corrected$data$Threshold >= 35 & Corrected$data$Threshold <= 65,
                                  c("Threshold", "Blank", "Before", "After", "Recovery")]
print(summary_table)

# === FINAL SUMMARY ===
cat("\n")
cat(paste(rep("=", 60), collapse=""), "\n", sep="")
cat("=== DIAGNOSTIC COMPLETE ===\n")
cat(paste(rep("=", 60), collapse=""), "\n", sep="")
cat(sprintf("Plots saved to: %s\n\n", Output.dir))

cat("ALIGNMENT SUMMARY:\n")
cat(sprintf("  90%% Alignment:\n"))
cat(sprintf("    Blank at threshold %d (area = %.2f)\n", 
            Aligned90$data$Threshold[Aligned90$Index90], Aligned90$Blank90))
cat(sprintf("    After shift: %d\n", Aligned90$After_offset))
cat(sprintf("    Before shift: %d\n", Aligned90$Before_offset))
cat(sprintf("  50%% Order Check:\n"))
cat(sprintf("    Additional After shifts: %d\n", Corrected$after_shifts))
cat(sprintf("    Additional Before shifts: %d\n", Corrected$before_shifts))
cat(sprintf("\n  TOTAL SHIFTS:\n"))
cat(sprintf("    After: %d\n", Aligned90$After_offset + Corrected$after_shifts))
cat(sprintf("    Before: %d\n", Aligned90$Before_offset + Corrected$before_shifts))

cat(sprintf("\nRECOVERY (thresholds 35-65):\n"))
cat(sprintf("  Mean: %.2f%%\n", mean(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  StdDev: %.2f%%\n", sd(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  Range: %.2f%% to %.2f%%\n", 
            min(recovery_zone$Recovery, na.rm = TRUE),
            max(recovery_zone$Recovery, na.rm = TRUE)))
cat(sprintf("  Flagged: %s\n", ifelse(Corrected$flagged, "YES", "NO")))

if (mean(recovery_zone$Recovery, na.rm = TRUE) >= 0 && mean(recovery_zone$Recovery, na.rm = TRUE) <= 100) {
  cat("\nRESULT: Recovery values look REASONABLE (0-100% range)\n")
} else if (mean(recovery_zone$Recovery, na.rm = TRUE) < 0) {
  cat("\nWARNING: NEGATIVE recovery detected - check alignment!\n")
} else {
  cat("\nWARNING: Recovery > 100% - check alignment!\n")
}
