### UV Recovery vs Analyte Recovery Diagnostic Analysis ###
# This script tests multiple UV recovery metrics against analyte recovery
# to determine the best approach for correlation
#
# GOAL: Determine if UV powder recovery can predict analyte (explosive) recovery
#
# METRICS TESTED:
# 1. Current method (threshold 35-65 with alignment)
# 2. Fixed single thresholds (k=10, 15, 20, 25, 30)
# 3. AUC ratio method (various threshold ranges)
# 4. Before/After ratio method
# 5. Peak area difference method
# 6. Optimized threshold selection

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)

rm(list = ls())

###############################
### CONFIGURATION
###############################

Base.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/FEL Swabbing Study/"
UV.dir <- paste0(Base.dir, "UV Analysis/")
Analyte.dir <- paste0(Base.dir, "087 Analysis/DataProcessing/")

Output.dir <- paste0(Base.dir, "UV Analysis/Diagnostic_Output/")
dir.create(Output.dir, recursive = TRUE, showWarnings = FALSE)

###############################
### LOAD ANALYTE DATA
###############################

cat("=== LOADING ANALYTE RECOVERY DATA ===\n")

analyte_data <- read.csv(paste0(Analyte.dir, "AverageRecoveries.csv"))

cat(sprintf("Loaded %d participants with analyte data\n", nrow(analyte_data)))
print(analyte_data)

###############################
### LOAD UV THRESHOLD DATA
###############################

cat("\n=== LOADING UV THRESHOLD DATA ===\n")

Blank.dir <- paste0(UV.dir, "Blank Threshold Analysis/")
Before.dir <- paste0(UV.dir, "Unswabbed Threshold Analysis/")

# Get list of blank and before files
blank_files <- list.files(Blank.dir, pattern = "_Blank\\.csv$", full.names = TRUE)
before_files <- list.files(Before.dir, pattern = "_Before\\.csv$", full.names = TRUE)

cat(sprintf("Found %d blank files\n", length(blank_files)))
cat(sprintf("Found %d before files\n", length(before_files)))

# Function to extract participant and trial info from filename
parse_filename <- function(filepath) {
  basename <- tools::file_path_sans_ext(basename(filepath))
  parts <- strsplit(basename, "_")[[1]]
  list(
    participant = as.integer(parts[1]),
    trial = parts[2]
  )
}

# Load all blank data
blank_data <- list()
for (f in blank_files) {
  info <- parse_filename(f)
  df <- read.csv(f)
  df$Threshold <- as.numeric(df$Threshold)
  df$Area <- as.numeric(df$Area)
  df$Participant <- info$participant
  df$Trial <- info$trial
  blank_data[[paste(info$participant, info$trial, sep = "_")]] <- df
}

# Load all before data
before_data <- list()
for (f in before_files) {
  info <- parse_filename(f)
  df <- read.csv(f)
  df$Threshold <- as.numeric(df$Threshold)
  df$Area <- as.numeric(df$Area)
  df$Participant <- info$participant
  df$Trial <- info$trial
  before_data[[paste(info$participant, info$trial, sep = "_")]] <- df
}

###############################
### DIAGNOSTIC: EXAMINE BLANK VARIABILITY
###############################

cat("\n=== DIAGNOSTIC: BLANK CURVE VARIABILITY ===\n")

blank_summary <- data.frame()
for (key in names(blank_data)) {
  df <- blank_data[[key]]
  blank_summary <- rbind(blank_summary, data.frame(
    Sample = key,
    Area_k10 = df$Area[df$Threshold == 10],
    Area_k20 = df$Area[df$Threshold == 20],
    Area_k35 = df$Area[df$Threshold == 35],
    Area_k50 = df$Area[df$Threshold == 50],
    Area_k65 = df$Area[df$Threshold == 65],
    Area_k80 = df$Area[df$Threshold == 80]
  ))
}

cat("\nBlank %Area values at key thresholds:\n")
print(blank_summary)

cat(sprintf("\nBlank variability at k=50: Mean=%.3f, SD=%.3f, CV=%.1f%%\n",
    mean(blank_summary$Area_k50),
    sd(blank_summary$Area_k50),
    sd(blank_summary$Area_k50) / mean(blank_summary$Area_k50) * 100))

# Plot blank curves
blank_plot_data <- bind_rows(blank_data, .id = "Sample")
p <- ggplot(blank_plot_data, aes(x = Threshold, y = Area, color = Sample)) +
  geom_line() +
  labs(title = "Blank Curves - All Samples",
       subtitle = "Should be identical if camera/lighting controlled",
       x = "Threshold",
       y = "% Area") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 6))

ggsave(paste0(Output.dir, "01_Blank_Curves_All.png"), p, width = 10, height = 6, dpi = 300)

###############################
### DIAGNOSTIC: EXAMINE BEFORE VARIABILITY
###############################

cat("\n=== DIAGNOSTIC: BEFORE CURVE VARIABILITY ===\n")

before_summary <- data.frame()
for (key in names(before_data)) {
  df <- before_data[[key]]
  before_summary <- rbind(before_summary, data.frame(
    Sample = key,
    Area_k10 = df$Area[df$Threshold == 10],
    Area_k20 = df$Area[df$Threshold == 20],
    Area_k35 = df$Area[df$Threshold == 35],
    Area_k50 = df$Area[df$Threshold == 50],
    Area_k65 = df$Area[df$Threshold == 65],
    Area_k80 = df$Area[df$Threshold == 80]
  ))
}

cat("\nBefore %Area values at key thresholds:\n")
print(before_summary)

# Plot before curves
before_plot_data <- bind_rows(before_data, .id = "Sample")
p <- ggplot(before_plot_data, aes(x = Threshold, y = Area, color = Sample)) +
  geom_line() +
  labs(title = "Before Curves - All Samples",
       subtitle = "Should vary with powder deposition amount",
       x = "Threshold",
       y = "% Area") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 6))

ggsave(paste0(Output.dir, "02_Before_Curves_All.png"), p, width = 10, height = 6, dpi = 300)

###############################
### METRIC 1: CURRENT METHOD (for comparison)
###############################

cat("\n=== METRIC 1: CURRENT METHOD (Threshold 35-65) ===\n")

current_method <- read.csv(paste0(UV.dir, "AverageRecovery.csv"))
current_method$Sample <- current_method$File

cat("Current method results:\n")
print(current_method[, c("Sample", "mean", "StdDev", "RSD")])

###############################
### METRIC 2: FIXED SINGLE THRESHOLD METHOD
###############################

cat("\n=== METRIC 2: FIXED SINGLE THRESHOLD METHOD ===\n")
cat("Testing thresholds: 10, 15, 20, 25, 30, 35, 40, 50\n")

fixed_threshold_results <- data.frame()
thresholds_to_test <- c(10, 15, 20, 25, 30, 35, 40, 50)

for (key in names(blank_data)) {
  if (!key %in% names(before_data)) {
    cat(sprintf("  WARNING: No before data for %s\n", key))
    next
  }
  
  blank_df <- blank_data[[key]]
  before_df <- before_data[[key]]
  
  parts <- strsplit(key, "_")[[1]]
  participant <- as.integer(parts[1])
  trial <- parts[2]
  
  for (k in thresholds_to_test) {
    blank_area <- blank_df$Area[blank_df$Threshold == k]
    before_area <- before_df$Area[before_df$Threshold == k]
    
    # Calculate recovery using standard formula
    # Recovery = (Before - After) / (Before - Blank) * 100
    # But we don't have After data in these files
    # So we calculate "proportion recovered" as: (Before - Blank) as a proxy
    
    # Alternative metric: Just use Before area (assuming Blank is constant/baseline)
    # Or: Before/Blank ratio
    
    before_blank_ratio <- before_area / blank_area
    before_minus_blank <- before_area - blank_area
    
    fixed_threshold_results <- rbind(fixed_threshold_results, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Threshold = k,
      BlankArea = blank_area,
      BeforeArea = before_area,
      BeforeBlankRatio = before_blank_ratio,
      BeforeMinusBlank = before_minus_blank
    ))
  }
}

cat("\nFixed threshold results (sample):\n")
print(head(fixed_threshold_results, 20))

###############################
### METRIC 3: AUC METHOD
###############################

cat("\n=== METRIC 3: AREA UNDER CURVE (AUC) METHOD ===\n")

auc_results <- data.frame()
auc_ranges <- list(
  "Steep_10_20" = c(10, 20),
  "Steep_10_25" = c(10, 25),
  "Steep_10_30" = c(10, 30),
  "Mid_20_40" = c(20, 40),
  "Mid_30_50" = c(30, 50),
  "Current_35_65" = c(35, 65),
  "Wide_10_50" = c(10, 50),
  "Wide_10_100" = c(10, 100)
)

for (key in names(blank_data)) {
  if (!key %in% names(before_data)) next
  
  blank_df <- blank_data[[key]]
  before_df <- before_data[[key]]
  
  parts <- strsplit(key, "_")[[1]]
  participant <- as.integer(parts[1])
  trial <- parts[2]
  
  for (range_name in names(auc_ranges)) {
    range <- auc_ranges[[range_name]]
    k_min <- range[1]
    k_max <- range[2]
    
    # Calculate AUC (sum of areas) for threshold range
    blank_auc <- sum(blank_df$Area[blank_df$Threshold >= k_min & blank_df$Threshold <= k_max])
    before_auc <- sum(before_df$Area[before_df$Threshold >= k_min & before_df$Threshold <= k_max])
    
    auc_ratio <- before_auc / blank_auc
    auc_diff <- before_auc - blank_auc
    
    auc_results <- rbind(auc_results, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Range = range_name,
      BlankAUC = blank_auc,
      BeforeAUC = before_auc,
      AUC_Ratio = auc_ratio,
      AUC_Diff = auc_diff
    ))
  }
}

cat("\nAUC results (sample):\n")
print(head(auc_results, 20))

###############################
### LOAD AFTER DATA FROM SUMMARY FILES
###############################

cat("\n=== LOADING AFTER DATA FROM PARTICIPANT SUMMARY FILES ===\n")

# Need to find Summary.csv files that contain Blank, Before, After
participant_dirs <- list.dirs(paste0(UV.dir), recursive = TRUE)

summary_files <- list.files(UV.dir, pattern = "Summary\\.csv$", recursive = TRUE, full.names = TRUE)
xlsx_files <- list.files(UV.dir, pattern = "Summary\\.xlsx$", recursive = TRUE, full.names = TRUE)

cat(sprintf("Found %d Summary.csv files\n", length(summary_files)))
cat(sprintf("Found %d Summary.xlsx files\n", length(xlsx_files)))

# Read one summary file to understand structure
if (length(xlsx_files) > 0) {
  # Read Excel summary
  sample_summary <- read_excel(xlsx_files[2])  # Use second file ( Participant1)
  cat("\nSample summary file structure:\n")
  print(head(sample_summary, 10))
}

###############################
### LOAD EXISTING THRESHOLD RESULTS WITH AFTER DATA
###############################

cat("\n=== SEARCHING FOR COMPLETE DATA (BLANK, BEFORE, AFTER) ===\n")

# Look in Particle participant folders for complete data
participants <- list.dirs(paste0(UV.dir), recursive = FALSE, full.names = FALSE)
participants <- participants[grepl("^Participant", participants)]

cat(sprintf("Found %d participant directories\n", length(participants)))

# For each participant, look for threshold analysis results
complete_data <- list()

for (participant_dir in participants) {
  participant_path <- file.path(UV.dir, participant_dir)
  subdirs <- list.dirs(participant_path, recursive = FALSE, full.names = FALSE)
  
  for (trial in subdirs) {
    trial_path <- file.path(participant_path, trial)
    summary_csv <- file.path(trial_path, "Threshold Analysis", "Part1_Orig_Summary.csv")
    
    # Also check for alternative naming
    alt_files <- list.files(file.path(trial_path, "Threshold Analysis"), 
                            pattern = "Summary\\.csv$", full.names = TRUE)
    
    if (length(alt_files) > 0) {
      summary_path <- alt_files[1]
      
      # Extract participant number and trial
      part_num <- as.integer(gsub("Participant ", "", participant_dir))
      
      cat(sprintf("Processing: %s / %s\n", participant_dir, trial))
      
      # Read summary file
      summary_df <- read.csv(summary_path, stringsAsFactors = FALSE)
      
      # Extract columns
      # Based on earlier inspection, columns include: Threshold, Blank, Before, After
      
      # Store for later processing
      key <- paste(part_num, trial, sep = "_")
      complete_data[[key]] <- list(
        participant = part_num,
        trial = trial,
        data = summary_df
      )
    }
  }
}

cat(sprintf("\nLoaded complete data for %d samples\n", length(complete_data)))

###############################
### PROCESS COMPLETE DATA FILES
###############################

cat("\n=== PROCESSING COMPLETE DATA (BLANK, BEFORE, AFTER) ===\n")

all_metrics <- data.frame()

for (key in names(complete_data)) {
  item <- complete_data[[key]]
  df <- item$data
  
  # Extract Blank, Before, After from columns
  # The summary file has these in specific columns
  # From inspection: Column C = Threshold, D = Blank, E = Before, F = After
  
  # Clean column names
  colnames(df) <- make.unique(colnames(df))
  
  cat(sprintf("\nProcessing %s - Columns: %s\n", key, paste(colnames(df)[1:10], collapse = ", ")))
  
  # Try to extract relevant columns
  # Based on the structure seen earlier, we need to find the recovery calculation columns
  
  # Skip corrupted/problematic files
  if (ncol(df) < 5) {
    cat(sprintf("  SKIPPED: Insufficient columns\n"))
    next
  }
}

###############################
### ALTERNATIVE: USE PRE-CALCULATED RECOVERY DATA
###############################

cat("\n=== ALTERNATIVE: ANALYZE EXISTING RECOVERY RESULTS ===\n")

# Load existing UV recovery data
uv_recovery <- read.csv(paste0(UV.dir, "AverageRecovery.csv"))

# Also load comparison file
uv_comparison <- read.csv(paste0(UV.dir, "AverageRecoveryComparison.csv"))

cat("\nExisting UV recovery data:\n")
print(uv_recovery)

# Create participant mapping
# From Participants.csv: participant numbers map to IDs like 08809, 12811, etc.
participants_map <- read.csv(paste0(Analyte.dir, "Participants.csv"), header = FALSE)
colnames(participants_map) <- c("ParticipantNumber", "ParticipantID")

cat("\nParticipant mapping:\n")
print(participants_map)

###############################
### CORRELATION ANALYSIS
###############################

cat("\n=== CORRELATION ANALYSIS: UV VS ANALYTE ===\n")

# Prepare data for correlation
# UV data has Sample names like "1_Original", "2_Original", etc.
# Analyte data has ParticipantNumber (1-11) and Pressure levels (High, Normal, Low)

# Extract participant number from UV Sample
uv_recovery$Participant <- as.integer(sapply(strsplit(uv_recovery$File, "_"), `[`, 1))
uv_recovery$Trial <- sapply(strsplit(uv_recovery$File, "_"), `[`, 2)

# Filter to reasonable recoveries (0-100%)
uv_valid <- uv_recovery %>%
  filter(mean >= 0 & mean <= 100)

cat(sprintf("\n%d samples with valid recovery (0-100%%)\n", nrow(uv_valid)))
cat(sprintf("%d samples excluded (recovery <0 or >100%%)\n", nrow(uv_recovery) - nrow(uv_valid)))

# Map to analyte data
# Assuming "Normal" pressure samples are the primary comparison
# (Based on experimental design - check if correct)

correlation_data <- uv_valid %>%
  left_join(analyte_data, by = c("Participant" = "ParticipantNumber")) %>%
  filter(!is.na(High))

cat("\nCorrelation data (UV vs Analyte):\n")
print(correlation_data[, c("File", "mean", "High", "Normal", "Low")])

if (nrow(correlation_data) > 3) {
  # Calculate correlations
  cor_high <- cor(correlation_data$mean, correlation_data$High, use = "complete.obs")
  cor_normal <- cor(correlation_data$mean, correlation_data$Normal, use = "complete.obs")
  cor_low <- cor(correlation_data$mean, correlation_data$Low, use = "complete.obs")
  
  cat(sprintf("\nCorrelation (Current Method UV Recovery vs Analyte):\n"))
  cat(sprintf("  vs High Pressure:   r = %.3f\n", cor_high))
  cat(sprintf("  vs Normal Pressure: r = %.3f\n", cor_normal))
  cat(sprintf("  vs Low Pressure:    r = %.3f\n", cor_low))
  
  # Scatter plots
  p1 <- ggplot(correlation_data, aes(x = mean, y = High)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    labs(title = "UV Recovery vs Analyte Recovery (High Pressure)",
         x = "UV Recovery (%)",
         y = "Analyte Recovery (%)",
         subtitle = sprintf("r = %.3f", cor_high)) +
    theme_minimal()
  
  ggsave(paste0(Output.dir, "03_Correlation_High.png"), p1, width = 8, height = 6, dpi = 300)
  
  p2 <- ggplot(correlation_data, aes(x = mean, y = Normal)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    labs(title = "UV Recovery vs Analyte Recovery (Normal Pressure)",
         x = "UV Recovery (%)",
         y = "Analyte Recovery (%)",
         subtitle = sprintf("r = %.3f", cor_normal)) +
    theme_minimal()
  
  ggsave(paste0(Output.dir, "04_Correlation_Normal.png"), p2, width = 8, height = 6, dpi = 300)
  
  p3 <- ggplot(correlation_data, aes(x = mean, y = Low)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    labs(title = "UV Recovery vs Analyte Recovery (Low Pressure)",
         x = "UV Recovery (%)",
         y = "Analyte Recovery (%)",
         subtitle = sprintf("r = %.3f", cor_low)) +
    theme_minimal()
  
  ggsave(paste0(Output.dir, "05_Correlation_Low.png"), p3, width = 8, height = 6, dpi = 300)
  
  # Combined plot
  cor_long <- correlation_data %>%
    select(File, mean, High, Normal, Low) %>%
    pivot_longer(cols = c(High, Normal, Low), names_to = "Pressure", values_to = "Analyte")
  
  p_all <- ggplot(cor_long, aes(x = mean, y = Analyte, color = Pressure)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "UV Recovery vs Analyte Recovery (All Pressures)",
         x = "UV Recovery (%)",
         y = "Analyte Recovery (%)") +
    theme_minimal()
  
  ggsave(paste0(Output.dir, "06_Correlation_All.png"), p_all, width = 10, height = 6, dpi = 300)
}

###############################
### TEST ALTERNATIVE METRICS FROM RAW DATA
###############################

cat("\n=== TEST ALTERNATIVE METRICS ===\n")

# Test metrics using Blank and Before data only
# (Since complete After data is in different format)

alternative_metrics <- data.frame()

for (key in names(blank_data)) {
  if (!key %in% names(before_data)) next
  
  parts <- strsplit(key, "_")[[1]]
  participant <- as.integer(parts[1])
  trial <- parts[2]
  
  blank_df <- blank_data[[key]]
  before_df <- before_data[[key]]
  
  # Skip if entry doesn't have analyte data
  if (!participant %in% analyte_data$ParticipantNumber) {
    cat(sprintf("  Skipping %s - no analyte data for participant %d\n", key, participant))
    next
  }
  
  # Get analyte recovery for this participant (using Normal pressure)
  analyte_normal <- analyte_data$Normal[analyte_data$ParticipantNumber == participant]
  analyte_high <- analyte_data$High[analyte_data$ParticipantNumber == participant]
  analyte_low <- analyte_data$Low[analyte_data$ParticipantNumber == participant]
  
  # Calculate various metrics
  
  # 1. Single threshold metrics
  for (k in c(10, 15, 20, 25, 30, 35, 40, 50)) {
    blank_k <- blank_df$Area[blank_df$Threshold == k]
    before_k <- before_df$Area[before_df$Threshold == k]
    
    alternative_metrics <- rbind(alternative_metrics, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Metric = paste0("Single_k", k),
      Value = before_k - blank_k,  # Difference
      Analyte_High = analyte_high,
      Analyte_Normal = analyte_normal,
      Analyte_Low = analyte_low
    ))
    
    alternative_metrics <- rbind(alternative_metrics, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Metric = paste0("Ratio_k", k),
      Value = before_k / blank_k,  # Ratio
      Analyte_High = analyte_high,
      Analyte_Normal = analyte_normal,
      Analyte_Low = analyte_low
    ))
  }
  
  # 2. AUC metrics
  for (range_name in names(auc_ranges)) {
    range <- auc_ranges[[range_name]]
    k_min <- range[1]
    k_max <- range[2]
    
    blank_auc <- sum(blank_df$Area[blank_df$Threshold >= k_min & blank_df$Threshold <= k_max])
    before_auc <- sum(before_df$Area[before_df$Threshold >= k_min & before_df$Threshold <= k_max])
    
    alternative_metrics <- rbind(alternative_metrics, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Metric = paste0("AUC_Diff_", range_name),
      Value = before_auc - blank_auc,
      Analyte_High = analyte_high,
      Analyte_Normal = analyte_normal,
      Analyte_Low = analyte_low
    ))
    
    alternative_metrics <- rbind(alternative_metrics, data.frame(
      Sample = key,
      Participant = participant,
      Trial = trial,
      Metric = paste0("AUC_Ratio_", range_name),
      Value = before_auc / blank_auc,
      Analyte_High = analyte_high,
      Analyte_Normal = analyte_normal,
      Analyte_Low = analyte_low
    ))
  }
  
  # 3. Peak-based metrics
  # Find threshold where greatest difference occurs
  diff_curve <- before_df$Area - blank_df$Area
  max_diff <- max(diff_curve)
  max_diff_threshold <- before_df$Threshold[which.max(diff_curve)]
  
  alternative_metrics <- rbind(alternative_metrics, data.frame(
    Sample = key,
    Participant = participant,
    Trial = trial,
    Metric = "MaxDiff",
    Value = max_diff,
    Analyte_High = analyte_high,
    Analyte_Normal = analyte_normal,
    Analyte_Low = analyte_low
  ))
  
  alternative_metrics <- rbind(alternative_metrics, data.frame(
    Sample = key,
    Participant = participant,
    Trial = trial,
    Metric = "MaxDiff_Threshold",
    Value = max_diff_threshold,
    Analyte_High = analyte_high,
    Analyte_Normal = analyte_normal,
    Analyte_Low = analyte_low
  ))
}

cat(sprintf("\nCalculated %d metric values\n", nrow(alternative_metrics)))

###############################
### CORRELATION ANALYSIS: ALL METRICS
###############################

cat("\n=== CORRELATION ANALYSIS: ALL ALTERNATIVE METRICS ===\n")

# Calculate correlation for each metric
metric_correlations <- alternative_metrics %>%
  group_by(Metric) %>%
  summarise(
    N = sum(!is.na(Value) & !is.na(Analyte_Normal)),
    Cor_High = cor(Value, Analyte_High, use = "complete.obs"),
    Cor_Normal = cor(Value, Analyte_Normal, use = "complete.obs"),
    Cor_Low = cor(Value, Analyte_Low, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Cor_Normal)))

cat("\nCorrelation of UV metrics with Analyte Recovery (sorted by |Cor_Normal|):\n")
print(metric_correlations)

# Save to CSV
write.csv(metric_correlations, paste0(Output.dir, "07_Metric_Correlations.csv"), row.names = FALSE)

# Top 10 metrics visualization
top_metrics <- head(metric_correlations$Metric, 10)

for (metric_name in top_metrics) {
  metric_data <- alternative_metrics %>%
    filter(Metric == metric_name)
  
  if (nrow(metric_data) > 3) {
    p <- ggplot(metric_data, aes(x = Value, y = Analyte_Normal)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
      labs(title = paste0("UV Metric: ", metric_name, " vs Analyte (Normal)"),
           x = metric_name,
           y = "Analyte Recovery (%)",
           subtitle = sprintf("r = %.3f, n = %d", 
                   cor(metric_data$Value, metric_data$Analyte_Normal, use = "complete.obs"),
                   nrow(metric_data))) +
      theme_minimal()
    
    safe_name <- gsub("[^A-Za-z0-9]", "_", metric_name)
    ggsave(paste0(Output.dir, "08_Correlation_", safe_name, ".png"), p, width = 8, height = 6, dpi = 300)
  }
}

###############################
### SUMMARY AND RECOMMENDATIONS
###############################

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("=== SUMMARY AND RECOMMENDATIONS ===\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("1. BLANK VARIABILITY:\n")
cat(sprintf("   Blank curves vary %.1f-fold between samples at threshold 50\n",
    max(blank_summary$Area_k50) / min(blank_summary$Area_k50)))
cat("   This indicates lighting/background is NOT controlled\n")
cat("   RECOMMENDATION: Use ratio metrics (Before/Blank) to normalize\n\n")

cat("2. CURRENT METHOD ISSUES:\n")
cat(sprintf("   %d samples have recovery <0%% or >100%% (physically impossible)\n",
    sum(uv_recovery$mean < 0 | uv_recovery$mean > 100)))
cat("   Alignment procedure creates artifacts\n")
cat("   RECOMMENDATION: Abandon alignment, use simpler metrics\n\n")

cat("3. BEST CORRELATING METRICS:\n")
if (nrow(metric_correlations) > 0) {
  best <- metric_correlations[1, ]
  cat(sprintf("   Best metric: %s\n", best$Metric))
  cat(sprintf("   Correlation with Normal: r = %.3f\n", best$Cor_Normal))
  cat(sprintf("   Correlation with High:   r = %.3f\n", best$Cor_High))
}

cat("\n4. NEXT STEPS:\n")
cat("   a) Validate best metric on new experimental data\n")
cat("   b) Optimize threshold selection based on discrimination power\n")
cat("   c) Consider controlling powder deposition or using internal standard\n")
cat("   d) If correlation remains poor, UV powder may not be suitable surrogate\n")

cat("\n=== OUTPUT FILES SAVED TO: ===\n")
cat(paste0(Output.dir, "\n"))

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
