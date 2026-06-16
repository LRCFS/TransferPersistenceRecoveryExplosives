### UV Recovery Reprocessing - Alternative Methods ###
# This standalone script tests 5 alternative recovery calculation methods
# on existing UV threshold data (Blank, Before, After curves) WITHOUT
# any alignment or curve shifting, then correlates each with PETN
# analyte recovery.
#
# PURPOSE: Determine if a better processing approach can extract
# meaningful UV recovery values from the existing data.
#
# NO DEPENDENCIES on 00-GlobalCode.R - this script is self-contained.
#
# DATA SOURCES:
#   - Summary CSVs: UV Analysis/Participant {N}/{Trial}/Threshold Analysis/
#   - Analyte data: 087 Analysis/DataProcessing/AverageRecoveries.csv
#
# OUTPUT: UV Analysis/Reprocessing_Output/

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

###############################################################################
###                         CONFIGURATION                                   ###
###############################################################################

Base.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/FEL Swabbing Study/"
UV.dir <- paste0(Base.dir, "UV Analysis/")
Analyte.dir <- paste0(Base.dir, "087 Analysis/DataProcessing/")
Output.dir <- paste0(UV.dir, "Reprocessing_Output/")

dir.create(Output.dir, recursive = TRUE, showWarnings = FALSE)

# Division safety threshold: denominators smaller than this are treated as zero
DIV_SAFETY <- 0.01

###############################################################################
###                         DATA LOADING                                    ###
###############################################################################

cat("=== UV RECOVERY REPROCESSING ===\n")
cat("=== Loading data... ===\n\n")

# --- Load analyte recovery data ---
analyte_path <- paste0(Analyte.dir, "AverageRecoveries.csv")
if (!file.exists(analyte_path)) {
  stop("Analyte data not found: ", analyte_path)
}
analyte_data <- read.csv(analyte_path, stringsAsFactors = FALSE)
# Remove empty rows (trailing commas in CSV)
analyte_data <- analyte_data %>% filter(!is.na(ParticipantNumber) & ParticipantNumber != "")
analyte_data$ParticipantNumber <- as.integer(analyte_data$ParticipantNumber)
cat(sprintf("Loaded analyte data for %d participants\n", nrow(analyte_data)))

# --- Find and parse all Summary CSV files ---
# Search recursively for Summary CSVs in participant folders
summary_files <- list.files(UV.dir, pattern = "Summary\\.csv$", recursive = TRUE, full.names = TRUE)
# Filter to only those in "Threshold Analysis" folders
summary_files <- summary_files[grepl("Threshold Analysis", summary_files)]

cat(sprintf("Found %d Summary CSV files\n", length(summary_files)))

if (length(summary_files) == 0) {
  stop("No Summary CSV files found in: ", UV.dir)
}

# --- Parse each Summary CSV into standardised threshold data ---
# TWO FORMATS exist:
#   Format A (Excel-style, 4 files): Row 1-2 are headers, columns 4-7 = Threshold/Blank/Before/After
#   Format B (Raw ImageJ, 13 files): Header = Slice,Count,Total Area,Average Size,%Area,Mean
#     Rows 2-101 = Blank (image 1), Rows 102-201 = Before (image 2), Rows 202-301 = After (image 3)
#     %Area is in column 5

parse_summary_csv <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  if (length(lines) < 4) {
    warning("File too short: ", filepath)
    return(NULL)
  }
  
  # Detect format from first line
  first_line <- lines[1]
  
  if (grepl("^Slice,Count,Total Area", first_line)) {
    # === FORMAT B: Raw ImageJ output ===
    # Header on line 1, data on lines 2-301
    # Column 5 = %Area
    # Rows 1-100 = Blank, 101-200 = Before, 201-300 = After
    
    n_data_rows <- as.integer(length(lines) - 1)  # Exclude header
    if (n_data_rows < 200) {
      warning("Raw ImageJ format but only ", n_data_rows, " data rows (expected 300): ", filepath)
      return(NULL)
    }
    
    n_to_read <- min(300L, n_data_rows)
    
    # Parse %Area from column 5 for all data rows
    area_values <- numeric(n_to_read)
    for (i in 1:n_to_read) {
      fields <- strsplit(lines[i + 1], ",")[[1]]  # +1 to skip header
      if (length(fields) >= 5) {
        area_values[i] <- suppressWarnings(as.numeric(fields[5]))
      } else {
        area_values[i] <- NA
      }
    }
    
    # Split into Blank (1-100), Before (101-200), After (201-300)
    blank_vals <- area_values[1:100]
    before_vals <- area_values[101:200]
    after_vals <- if (n_to_read >= 300) area_values[201:300] else rep(NA_real_, 100)
    
    threshold_data <- data.frame(
      Threshold = 1:100,
      Blank = blank_vals,
      Before = before_vals,
      After = after_vals
    )
    
  } else if (grepl("Slice,%Area", first_line) || grepl("^Slice,", first_line)) {
    # === FORMAT A: Excel-style with pre-computed analysis ===
    # Rows 1-2 are headers, data starts at row 3
    # Columns: 1=Slice, 2=%Area, 3=empty, 4=Threshold, 5=Blank, 6=Before, 7=After
    
    n_data_rows <- as.integer(min(100L, length(lines) - 2L))
    if (n_data_rows < 1) {
      warning("Format A file too short: ", filepath)
      return(NULL)
    }
    
    threshold_vals <- numeric(n_data_rows)
    blank_vals <- numeric(n_data_rows)
    before_vals <- numeric(n_data_rows)
    after_vals <- numeric(n_data_rows)
    
    for (i in 1:n_data_rows) {
      line <- lines[i + 2]  # Skip 2 header rows
      fields <- strsplit(line, ",")[[1]]
      
      if (length(fields) >= 7) {
        threshold_vals[i] <- suppressWarnings(as.numeric(fields[4]))
        blank_vals[i] <- suppressWarnings(as.numeric(fields[5]))
        before_vals[i] <- suppressWarnings(as.numeric(fields[6]))
        after_vals[i] <- suppressWarnings(as.numeric(fields[7]))
      } else {
        threshold_vals[i] <- NA
        blank_vals[i] <- NA
        before_vals[i] <- NA
        after_vals[i] <- NA
      }
    }
    
    threshold_data <- data.frame(
      Threshold = threshold_vals,
      Blank = blank_vals,
      Before = before_vals,
      After = after_vals
    )
    
  } else {
    warning("Unknown file format: ", filepath, "\n  First line: ", first_line)
    return(NULL)
  }
  
  # Remove rows where all values are NA
  threshold_data <- threshold_data[!is.na(threshold_data$Threshold), ]
  
  return(threshold_data)
}

# Extract participant number and trial from file path
extract_sample_info <- function(filepath) {
  # Pattern: .../Participant {N}/{Trial}/Threshold Analysis/Part{N}_{suffix}_Summary.csv
  parts <- unlist(strsplit(filepath, "/|\\\\"))
  
  # Find "Participant X" element
  part_idx <- grep("^Participant", parts)
  if (length(part_idx) == 0) {
    return(list(participant = NA, trial = NA))
  }
  
  participant_str <- parts[part_idx[1]]
  participant <- as.integer(gsub("Participant ", "", participant_str))
  
  # Trial is the next folder after Participant X
  trial <- parts[part_idx[1] + 1]
  
  return(list(participant = participant, trial = trial))
}

# Process all summary files
all_samples <- list()

for (filepath in summary_files) {
  info <- extract_sample_info(filepath)
  
  if (is.na(info$participant)) {
    cat(sprintf("  WARNING: Could not parse participant from: %s\n", basename(filepath)))
    next
  }
  
  # Parse the CSV
  threshold_data <- parse_summary_csv(filepath)
  
  # Validate
  if (is.null(threshold_data) || nrow(threshold_data) == 0) {
    cat(sprintf("  WARNING: %s produced no valid data\n", basename(filepath)))
    next
  }
  if (nrow(threshold_data) < 100) {
    cat(sprintf("  WARNING: %s has only %d rows (expected 100)\n", basename(filepath), nrow(threshold_data)))
  }
  if (all(is.na(threshold_data$After))) {
    cat(sprintf("  WARNING: %s has no After data\n", basename(filepath)))
    next
  }
  
  sample_key <- paste0(info$participant, "_", info$trial)
  all_samples[[sample_key]] <- list(
    participant = info$participant,
    trial = info$trial,
    data = threshold_data,
    filepath = filepath
  )
  
  cat(sprintf("  Loaded: %s (%d rows, Threshold %d-%d)\n", 
              sample_key, nrow(threshold_data),
              min(threshold_data$Threshold, na.rm = TRUE),
              max(threshold_data$Threshold, na.rm = TRUE)))
}

cat(sprintf("\nSuccessfully loaded %d samples with complete data\n", length(all_samples)))
cat(sprintf("Samples: %s\n\n", paste(names(all_samples), collapse = ", ")))

# --- Input validation ---
if (length(all_samples) == 0) {
  stop("No valid samples loaded. Check Summary CSV file format.")
}

# Check for saturation (diagnostic)
cat("=== SATURATION CHECK ===\n")
for (key in names(all_samples)) {
  d <- all_samples[[key]]$data
  before_k10 <- d$Before[d$Threshold == 10]
  if (!is.na(before_k10) && before_k10 > 95) {
    cat(sprintf("  %s: Before_k10 = %.1f%% (SATURATED)\n", key, before_k10))
  } else if (!is.na(before_k10)) {
    cat(sprintf("  %s: Before_k10 = %.1f%%\n", key, before_k10))
  }
}

###############################################################################
###                  SAFE DIVISION HELPER                                    ###
###############################################################################

safe_divide <- function(numerator, denominator, threshold = DIV_SAFETY) {
  # Returns NA where denominator is too small (avoids Inf/NaN)
  ifelse(abs(denominator) < threshold, NA_real_, numerator / denominator)
}

###############################################################################
###          APPROACH 1: Direct Single-Threshold (No Alignment)             ###
###############################################################################

cat("\n=== APPROACH 1: Direct Single-Threshold ===\n")
# Recovery(k) = (Before_k - After_k) / (Before_k - Blank_k) * 100

thresholds_to_test <- c(5, 10, 15, 20, 25, 30, 35, 40, 50)

approach1_results <- data.frame()

for (key in names(all_samples)) {
  sample <- all_samples[[key]]
  d <- sample$data
  
  for (k in thresholds_to_test) {
    row_idx <- which(d$Threshold == k)
    if (length(row_idx) == 0) next
    
    blank_k <- d$Blank[row_idx]
    before_k <- d$Before[row_idx]
    after_k <- d$After[row_idx]
    
    # Skip if any value is NA
    if (any(is.na(c(blank_k, before_k, after_k)))) next
    
    numerator <- before_k - after_k
    denominator <- before_k - blank_k
    recovery <- safe_divide(numerator, denominator) * 100
    
    approach1_results <- rbind(approach1_results, data.frame(
      Sample = key,
      Participant = sample$participant,
      Trial = sample$trial,
      Method = "SingleThreshold",
      Parameter = paste0("k=", k),
      Recovery = recovery,
      Before_k = before_k,
      After_k = after_k,
      Blank_k = blank_k,
      stringsAsFactors = FALSE
    ))
  }
}

cat(sprintf("  Calculated %d recovery values across %d thresholds\n",
            nrow(approach1_results), length(thresholds_to_test)))

###############################################################################
###          APPROACH 2: Crossing-Point Shift (Horizontal Distance)         ###
###############################################################################

cat("\n=== APPROACH 2: Crossing-Point Shift ===\n")
# Find threshold where each curve crosses a target %Area level
# Recovery = (k_before - k_after) / (k_before - k_blank) * 100

crossing_levels <- c(30, 40, 50, 60, 70)

# Helper: find threshold where curve crosses target level
# Uses linear interpolation between adjacent points
find_crossing <- function(thresholds, values, target) {
  if (all(is.na(values))) return(NA_real_)
  
  # Find where values cross the target (decreasing curve)
  # Look for consecutive points where one is above and one is below target
  for (i in 1:(length(values) - 1)) {
    if (is.na(values[i]) || is.na(values[i + 1])) next
    
    if ((values[i] >= target & values[i + 1] < target) |
        (values[i] <= target & values[i + 1] > target)) {
      # Linear interpolation
      frac <- (target - values[i]) / (values[i + 1] - values[i])
      crossing_k <- thresholds[i] + frac * (thresholds[i + 1] - thresholds[i])
      return(crossing_k)
    }
  }
  
  # If no crossing found, use nearest point
  nearest_idx <- which.min(abs(values - target))
  if (length(nearest_idx) > 0) return(thresholds[nearest_idx])
  
  return(NA_real_)
}

approach2_results <- data.frame()

for (key in names(all_samples)) {
  sample <- all_samples[[key]]
  d <- sample$data
  
  for (level in crossing_levels) {
    k_blank <- find_crossing(d$Threshold, d$Blank, level)
    k_before <- find_crossing(d$Threshold, d$Before, level)
    k_after <- find_crossing(d$Threshold, d$After, level)
    
    if (any(is.na(c(k_blank, k_before, k_after)))) next
    
    numerator <- k_before - k_after
    denominator <- k_before - k_blank
    recovery <- safe_divide(numerator, denominator) * 100
    
    approach2_results <- rbind(approach2_results, data.frame(
      Sample = key,
      Participant = sample$participant,
      Trial = sample$trial,
      Method = "CrossingShift",
      Parameter = paste0(level, "%"),
      Recovery = recovery,
      k_blank = k_blank,
      k_before = k_before,
      k_after = k_after,
      stringsAsFactors = FALSE
    ))
  }
}

cat(sprintf("  Calculated %d recovery values across %d crossing levels\n",
            nrow(approach2_results), length(crossing_levels)))

###############################################################################
###          APPROACH 3: AUC with All Three Curves                          ###
###############################################################################

cat("\n=== APPROACH 3: AUC (Area Under Curve) ===\n")
# AUC_Recovery = (AUC_Before - AUC_After) / (AUC_Before - AUC_Blank) * 100

auc_ranges <- list(
  "5-15" = c(5, 15),
  "10-20" = c(10, 20),
  "10-25" = c(10, 25),
  "15-30" = c(15, 30),
  "20-40" = c(20, 40),
  "35-65" = c(35, 65)
)

approach3_results <- data.frame()

for (key in names(all_samples)) {
  sample <- all_samples[[key]]
  d <- sample$data
  
  for (range_name in names(auc_ranges)) {
    k_range <- auc_ranges[[range_name]]
    k_min <- k_range[1]
    k_max <- k_range[2]
    
    # Filter to threshold range
    in_range <- d$Threshold >= k_min & d$Threshold <= k_max
    
    if (sum(in_range) == 0) next
    
    blank_auc <- sum(d$Blank[in_range], na.rm = TRUE)
    before_auc <- sum(d$Before[in_range], na.rm = TRUE)
    after_auc <- sum(d$After[in_range], na.rm = TRUE)
    
    numerator <- before_auc - after_auc
    denominator <- before_auc - blank_auc
    recovery <- safe_divide(numerator, denominator) * 100
    
    approach3_results <- rbind(approach3_results, data.frame(
      Sample = key,
      Participant = sample$participant,
      Trial = sample$trial,
      Method = "AUC",
      Parameter = range_name,
      Recovery = recovery,
      AUC_Blank = blank_auc,
      AUC_Before = before_auc,
      AUC_After = after_auc,
      stringsAsFactors = FALSE
    ))
  }
}

cat(sprintf("  Calculated %d recovery values across %d AUC ranges\n",
            nrow(approach3_results), length(auc_ranges)))

###############################################################################
###          APPROACH 4: Per-Sample Optimal Threshold                       ###
###############################################################################

cat("\n=== APPROACH 4: Per-Sample Optimal Threshold ===\n")
# Find k where (Before - After) is maximised for each sample
# Calculate recovery at that k

approach4_results <- data.frame()

for (key in names(all_samples)) {
  sample <- all_samples[[key]]
  d <- sample$data
  
  # Calculate Before - After difference at each threshold
  diff_curve <- d$Before - d$After
  
  # Skip if all NA

  if (all(is.na(diff_curve))) next
  
  # Find threshold with maximum difference
  max_idx <- which.max(diff_curve)
  optimal_k <- d$Threshold[max_idx]
  max_diff <- diff_curve[max_idx]
  
  # Calculate recovery at optimal k
  blank_k <- d$Blank[max_idx]
  before_k <- d$Before[max_idx]
  after_k <- d$After[max_idx]
  
  numerator <- before_k - after_k
  denominator <- before_k - blank_k
  recovery <- safe_divide(numerator, denominator) * 100
  
  approach4_results <- rbind(approach4_results, data.frame(
    Sample = key,
    Participant = sample$participant,
    Trial = sample$trial,
    Method = "OptimalK",
    Parameter = "auto",
    Recovery = recovery,
    Optimal_k = optimal_k,
    MaxDiff = max_diff,
    Before_k = before_k,
    After_k = after_k,
    Blank_k = blank_k,
    stringsAsFactors = FALSE
  ))
}

cat(sprintf("  Calculated %d recovery values\n", nrow(approach4_results)))
cat("  Optimal k distribution:\n")
if (nrow(approach4_results) > 0) {
  cat(sprintf("    Range: k=%.0f to k=%.0f\n",
              min(approach4_results$Optimal_k), max(approach4_results$Optimal_k)))
  cat(sprintf("    Median: k=%.0f\n", median(approach4_results$Optimal_k)))
}

###############################################################################
###          APPROACH 5: Blank-Normalised Ratio                             ###
###############################################################################

cat("\n=== APPROACH 5: Blank-Normalised Ratio ===\n")
# NormBefore(k) = Before(k) / Blank(k)
# NormAfter(k)  = After(k) / Blank(k)
# Recovery(k) = (NormBefore - NormAfter) / (NormBefore - 1) * 100
# Averaged over a threshold range

norm_ranges <- list(
  "10-20" = c(10, 20),
  "15-25" = c(15, 25),
  "20-40" = c(20, 40),
  "10-30" = c(10, 30),
  "5-15" = c(5, 15)
)

approach5_results <- data.frame()

for (key in names(all_samples)) {
  sample <- all_samples[[key]]
  d <- sample$data
  
  for (range_name in names(norm_ranges)) {
    k_range <- norm_ranges[[range_name]]
    k_min <- k_range[1]
    k_max <- k_range[2]
    
    in_range <- d$Threshold >= k_min & d$Threshold <= k_max
    if (sum(in_range) == 0) next
    
    blank_vals <- d$Blank[in_range]
    before_vals <- d$Before[in_range]
    after_vals <- d$After[in_range]
    
    # Normalise by blank (with division safety)
    norm_before <- safe_divide(before_vals, blank_vals)
    norm_after <- safe_divide(after_vals, blank_vals)
    
    # Calculate recovery at each k in range
    numerator <- norm_before - norm_after
    denominator <- norm_before - 1
    recovery_per_k <- safe_divide(numerator, denominator) * 100
    
    # Average recovery across the range (excluding NA and extreme values)
    valid_recovery <- recovery_per_k[!is.na(recovery_per_k) & 
                                       is.finite(recovery_per_k) &
                                       recovery_per_k > -500 & recovery_per_k < 500]
    
    if (length(valid_recovery) == 0) next
    
    avg_recovery <- mean(valid_recovery)
    
    approach5_results <- rbind(approach5_results, data.frame(
      Sample = key,
      Participant = sample$participant,
      Trial = sample$trial,
      Method = "BlankNormalised",
      Parameter = range_name,
      Recovery = avg_recovery,
      N_valid = length(valid_recovery),
      N_total = sum(in_range),
      stringsAsFactors = FALSE
    ))
  }
}

cat(sprintf("  Calculated %d recovery values across %d ranges\n",
            nrow(approach5_results), length(norm_ranges)))

###############################################################################
###                    COMBINE ALL RESULTS                                   ###
###############################################################################

cat("\n=== COMBINING RESULTS ===\n")

# Standardise columns for combining
standardise_results <- function(df) {
  df %>% select(Sample, Participant, Trial, Method, Parameter, Recovery)
}

all_results <- bind_rows(
  standardise_results(approach1_results),
  standardise_results(approach2_results),
  standardise_results(approach3_results),
  standardise_results(approach4_results),
  standardise_results(approach5_results)
)

cat(sprintf("Total recovery calculations: %d\n", nrow(all_results)))
cat(sprintf("Methods: %s\n", paste(unique(all_results$Method), collapse = ", ")))

# Flag impossible recoveries
all_results$Flag <- case_when(
  is.na(all_results$Recovery) ~ "NA",
  all_results$Recovery < 0 ~ "Negative",
  all_results$Recovery > 100 ~ "Over100",
  TRUE ~ "Valid"
)

flag_summary <- all_results %>% 
  group_by(Method, Flag) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Flag, values_from = n, values_fill = 0)

cat("\nRecovery validity by method:\n")
print(as.data.frame(flag_summary))

###############################################################################
###                    CORRELATION ANALYSIS                                  ###
###############################################################################

cat("\n=== CORRELATION ANALYSIS ===\n")

# Create unique method identifier
all_results$MethodID <- paste(all_results$Method, all_results$Parameter, sep = "_")

# Join with analyte data
correlation_input <- all_results %>%
  left_join(analyte_data, by = c("Participant" = "ParticipantNumber")) %>%
  filter(!is.na(Recovery) & is.finite(Recovery))

# Calculate correlations for each MethodID
method_ids <- unique(correlation_input$MethodID)

correlation_results <- data.frame()

for (mid in method_ids) {
  subset <- correlation_input %>% filter(MethodID == mid)
  
  # Need at least 4 data points for meaningful correlation
  valid_high <- subset %>% filter(!is.na(High) & !is.na(Recovery))
  valid_normal <- subset %>% filter(!is.na(Normal) & !is.na(Recovery))
  valid_low <- subset %>% filter(!is.na(Low) & !is.na(Recovery))
  
  cor_high <- NA_real_; p_high <- NA_real_; n_high <- 0
  cor_normal <- NA_real_; p_normal <- NA_real_; n_normal <- 0
  cor_low <- NA_real_; p_low <- NA_real_; n_low <- 0
  
  if (nrow(valid_high) >= 4) {
    test <- cor.test(valid_high$Recovery, valid_high$High, method = "pearson")
    cor_high <- test$estimate
    p_high <- test$p.value
    n_high <- nrow(valid_high)
  }
  
  if (nrow(valid_normal) >= 4) {
    test <- cor.test(valid_normal$Recovery, valid_normal$Normal, method = "pearson")
    cor_normal <- test$estimate
    p_normal <- test$p.value
    n_normal <- nrow(valid_normal)
  }
  
  if (nrow(valid_low) >= 4) {
    test <- cor.test(valid_low$Recovery, valid_low$Low, method = "pearson")
    cor_low <- test$estimate
    p_low <- test$p.value
    n_low <- nrow(valid_low)
  }
  
  # Extract method and parameter
  parts <- strsplit(mid, "_", fixed = FALSE)[[1]]
  method_name <- parts[1]
  param_name <- paste(parts[-1], collapse = "_")
  
  # Count valid/invalid recoveries
  n_valid <- sum(subset$Flag == "Valid")
  n_impossible <- sum(subset$Flag %in% c("Negative", "Over100"))
  
  correlation_results <- rbind(correlation_results, data.frame(
    MethodID = mid,
    Method = method_name,
    Parameter = param_name,
    Cor_High = cor_high,
    P_High = p_high,
    N_High = n_high,
    Cor_Normal = cor_normal,
    P_Normal = p_normal,
    N_Normal = n_normal,
    Cor_Low = cor_low,
    P_Low = p_low,
    N_Low = n_low,
    N_Valid = n_valid,
    N_Impossible = n_impossible,
    Mean_Recovery = mean(subset$Recovery, na.rm = TRUE),
    SD_Recovery = sd(subset$Recovery, na.rm = TRUE),
    stringsAsFactors = FALSE
  ))
}

# Sort by Normal correlation (descending - looking for positive correlations)
correlation_results <- correlation_results %>% arrange(desc(Cor_Normal))

cat("\n--- TOP 10 METHODS BY CORRELATION WITH NORMAL PRESSURE ---\n")
print(head(correlation_results %>% 
             select(MethodID, Cor_Normal, P_Normal, N_Normal, Cor_High, Mean_Recovery, N_Impossible), 10))

cat("\n--- BOTTOM 5 (MOST NEGATIVE) ---\n")
print(tail(correlation_results %>% 
             select(MethodID, Cor_Normal, P_Normal, N_Normal, Mean_Recovery), 5))

# Flag potentially meaningful positive correlations
positive_hits <- correlation_results %>%
  filter(Cor_Normal > 0.3 & P_Normal < 0.1)

if (nrow(positive_hits) > 0) {
  cat(sprintf("\n*** POSITIVE CORRELATIONS FOUND: %d methods with r > 0.3, p < 0.1 ***\n",
              nrow(positive_hits)))
  print(positive_hits %>% select(MethodID, Cor_Normal, P_Normal, N_Normal, Mean_Recovery))
} else {
  cat("\n*** No methods achieved r > 0.3 with p < 0.1 for Normal pressure ***\n")
}

###############################################################################
###                    SAVE RESULTS                                          ###
###############################################################################

cat("\n=== SAVING RESULTS ===\n")

# Save full recovery results
write.csv(all_results, file.path(Output.dir, "Recovery_AllMethods.csv"), row.names = FALSE)
cat(sprintf("  Saved: Recovery_AllMethods.csv (%d rows)\n", nrow(all_results)))

# Save correlation summary
write.csv(correlation_results, file.path(Output.dir, "Correlation_Summary.csv"), row.names = FALSE)
cat(sprintf("  Saved: Correlation_Summary.csv (%d methods)\n", nrow(correlation_results)))

###############################################################################
###                    DIAGNOSTIC PLOTS                                      ###
###############################################################################

cat("\n=== GENERATING PLOTS ===\n")

# --- Plot 1: Raw threshold curves for all samples (faceted) ---
raw_plot_data <- data.frame()
for (key in names(all_samples)) {
  d <- all_samples[[key]]$data
  d$Sample <- key
  d_long <- d %>%
    pivot_longer(cols = c(Blank, Before, After), names_to = "Curve", values_to = "Area")
  raw_plot_data <- rbind(raw_plot_data, d_long)
}

p1 <- ggplot(raw_plot_data, aes(x = Threshold, y = Area, color = Curve)) +
  geom_line() +
  facet_wrap(~Sample, scales = "free_y") +
  scale_color_manual(values = c("Blank" = "blue", "Before" = "red", "After" = "green3")) +
  labs(title = "Raw Threshold Curves - All Samples (No Alignment)",
       subtitle = "Blue=Blank, Red=Before swab, Green=After swab",
       x = "Threshold (k)",
       y = "% Area") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7))

ggsave(file.path(Output.dir, "Diagnostic_RawCurves.png"), p1, 
       width = 14, height = 10, dpi = 300)
cat("  Saved: Diagnostic_RawCurves.png\n")

# --- Plot 2: Saturation diagnostic ---
saturation_data <- data.frame()
for (key in names(all_samples)) {
  d <- all_samples[[key]]$data
  before_k10 <- d$Before[d$Threshold == 10]
  before_k15 <- d$Before[d$Threshold == 15]
  before_k20 <- d$Before[d$Threshold == 20]
  saturation_data <- rbind(saturation_data, data.frame(
    Sample = key,
    Before_k10 = ifelse(length(before_k10) > 0, before_k10, NA),
    Before_k15 = ifelse(length(before_k15) > 0, before_k15, NA),
    Before_k20 = ifelse(length(before_k20) > 0, before_k20, NA)
  ))
}

sat_long <- saturation_data %>%
  pivot_longer(cols = starts_with("Before_"), names_to = "Threshold", values_to = "Area") %>%
  mutate(Threshold = gsub("Before_", "", Threshold))

p2 <- ggplot(sat_long, aes(x = reorder(Sample, -Area), y = Area, fill = Threshold)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 96, label = "Saturation threshold (95%)", 
           hjust = 0, color = "red", size = 3) +
  labs(title = "Powder Loading Diagnostic - Before Swab",
       subtitle = "Samples above red line are likely saturated at that threshold",
       x = "Sample",
       y = "% Area (Before image)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave(file.path(Output.dir, "Diagnostic_Saturation.png"), p2, 
       width = 12, height = 6, dpi = 300)
cat("  Saved: Diagnostic_Saturation.png\n")

# --- Plot 3: Optimal k distribution (Approach 4) ---
if (nrow(approach4_results) > 0) {
  p3 <- ggplot(approach4_results, aes(x = reorder(Sample, Optimal_k), y = Optimal_k)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = Optimal_k), vjust = -0.5, size = 3) +
    labs(title = "Approach 4: Per-Sample Optimal Threshold",
         subtitle = "Threshold k where (Before - After) difference is maximised",
         x = "Sample",
         y = "Optimal Threshold (k)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  ggsave(file.path(Output.dir, "Diagnostic_OptimalK.png"), p3, 
         width = 10, height = 6, dpi = 300)
  cat("  Saved: Diagnostic_OptimalK.png\n")
}

# --- Plot 4: Recovery comparison boxplot ---
# Select representative methods for comparison
representative_methods <- c(
  "SingleThreshold_k=15",
  "SingleThreshold_k=20",
  "SingleThreshold_k=35",
  "CrossingShift_50%",
  "AUC_10-20",
  "AUC_35-65",
  "OptimalK_auto",
  "BlankNormalised_10-20",
  "BlankNormalised_15-25"
)

# Match against MethodID
comparison_data <- all_results %>%
  mutate(MethodID = paste(Method, Parameter, sep = "_")) %>%
  filter(MethodID %in% representative_methods) %>%
  filter(!is.na(Recovery) & Recovery > -200 & Recovery < 300)  # Exclude extreme outliers for plotting

if (nrow(comparison_data) > 0) {
  p4 <- ggplot(comparison_data, aes(x = MethodID, y = Recovery, fill = Method)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 1) +
    geom_hline(yintercept = c(0, 100), linetype = "dashed", color = "grey50") +
    labs(title = "Recovery Distribution by Method",
         subtitle = "Dashed lines = physically valid range (0-100%)",
         x = "Method",
         y = "Recovery (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          legend.position = "none")
  
  ggsave(file.path(Output.dir, "Comparison_Boxplot.png"), p4, 
         width = 12, height = 6, dpi = 300)
  cat("  Saved: Comparison_Boxplot.png\n")
}

# --- Plot 5: Best correlation scatter plot ---
if (nrow(correlation_results) > 0) {
  best_method <- correlation_results$MethodID[1]
  best_cor <- correlation_results$Cor_Normal[1]
  
  best_data <- correlation_input %>%
    filter(MethodID == best_method & !is.na(Normal))
  
  if (nrow(best_data) >= 4) {
    p5 <- ggplot(best_data, aes(x = Recovery, y = Normal)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
      labs(title = paste0("Best Method: ", best_method),
           subtitle = sprintf("r = %.3f (Normal pressure), n = %d", best_cor, nrow(best_data)),
           x = "UV Recovery (%)",
           y = "Analyte Recovery (%)") +
      theme_minimal()
    
    ggsave(file.path(Output.dir, "BestCorrelation_Scatter.png"), p5, 
           width = 8, height = 6, dpi = 300)
    cat("  Saved: BestCorrelation_Scatter.png\n")
  }
}

###############################################################################
###                    SUMMARY REPORT                                        ###
###############################################################################

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("=== SUMMARY REPORT ===\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("SAMPLES PROCESSED: ", length(all_samples), "\n")
cat("TOTAL RECOVERY CALCULATIONS: ", nrow(all_results), "\n\n")

cat("METHODS TESTED:\n")
cat("  1. SingleThreshold: Direct (Before-After)/(Before-Blank) at fixed k\n")
cat("  2. CrossingShift: Horizontal distance between curves at fixed %Area\n")
cat("  3. AUC: Integrated area difference over threshold ranges\n")
cat("  4. OptimalK: Recovery at per-sample best threshold\n")
cat("  5. BlankNormalised: Ratio-based correction for lighting variation\n\n")

cat("TOP 5 CORRELATIONS WITH ANALYTE (NORMAL PRESSURE):\n")
top5 <- head(correlation_results, 5)
for (i in 1:nrow(top5)) {
  cat(sprintf("  %d. %s: r = %.3f (p = %.3f, n = %d)\n",
              i, top5$MethodID[i], top5$Cor_Normal[i], top5$P_Normal[i], top5$N_Normal[i]))
}

cat("\nCONCLUSION:\n")
best_r <- max(correlation_results$Cor_Normal, na.rm = TRUE)
if (best_r > 0.5) {
  cat(sprintf("  MODERATE-STRONG positive correlation found (r = %.3f)\n", best_r))
  cat("  This suggests the UV method CAN provide meaningful recovery data\n")
  cat("  with appropriate processing.\n")
} else if (best_r > 0.3) {
  cat(sprintf("  WEAK positive correlation found (r = %.3f)\n", best_r))
  cat("  Suggestive but not conclusive. May improve with better experimental control.\n")
} else if (best_r > 0) {
  cat(sprintf("  NEGLIGIBLE positive correlation (r = %.3f)\n", best_r))
  cat("  No evidence that any processing method can rescue these data.\n")
} else {
  cat(sprintf("  NO positive correlation found (best r = %.3f)\n", best_r))
  cat("  All methods show zero or negative correlation with analyte recovery.\n")
  cat("  This confirms the UV measurement is not predictive of PETN recovery\n")
  cat("  under the current experimental conditions.\n")
}

cat(sprintf("\nOUTPUT SAVED TO: %s\n", Output.dir))
cat("\n=== REPROCESSING COMPLETE ===\n")
