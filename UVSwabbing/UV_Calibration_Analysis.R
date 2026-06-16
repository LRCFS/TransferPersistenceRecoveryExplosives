### UV Calibration Experiment Analysis ###
# This script processes a powder loading calibration experiment to determine:
#   1. Does reduced powder loading reduce saturation?
#   2. Is UV recovery proportional to area swiped (number of swipes)?
#   3. What is the optimal threshold and powder concentration?
#
# EXPERIMENT DESIGN:
#   - 3 concentrations: 50%, 25%, 10% of original powder amount
#   - 3 surfaces (A, B, C) per concentration
#   - 5 conditions per surface: Blank, Before, 1 swipe, 2 swipes, 3 swipes
#   - Total: 45 images, each thresholded at k=1 to k=100
#
# INPUT: Raw ImageJ "Analyze Particles" CSV files (45 images x 100 thresholds = 4500 files)
#   Format: each CSV has columns (index), Area, Mean, Min, Max — one row per particle
#
# OUTPUT: Calibration_Output/ folder with results, plots, and recommendations
#
# NO DEPENDENCIES on 00-GlobalCode.R - this script is self-contained.

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

###############################################################################
###                         CONFIGURATION                                   ###
###############################################################################

Data.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Shared with Oliver/output/"
Output.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Shared with Oliver/Calibration_Output/"

dir.create(Output.dir, recursive = TRUE, showWarnings = FALSE)

# Total image area in pixels (4840 x 4824 crop from ImageJ macro)
TOTAL_IMAGE_AREA <- 4840 * 4824  # = 23,348,160

# Expected number of images
EXPECTED_IMAGES <- 45

# Division safety threshold
DIV_SAFETY <- 0.5  # %Area units — if Before-Blank < 0.5%, denominator too small

# Concentration labels (in sequence order)
CONCENTRATIONS <- c("50%", "25%", "10%")

# Conditions within each concentration block (in sequence order)
CONDITIONS <- c("Blank", "Before", "1_swipe", "2_swipes", "3_swipes")

# Surfaces (cycling within each group of 3)
SURFACES <- c("A", "B", "C")

###############################################################################
###          STAGE 1: Aggregate Raw Particle Data to %Area Curves           ###
###############################################################################

cat("=== UV CALIBRATION EXPERIMENT ANALYSIS ===\n\n")
cat("=== STAGE 1: Aggregating raw particle data ===\n")

# List all CSV files and extract unique image IDs
all_csv_files <- list.files(Data.dir, pattern = "\\[\\d+\\]\\.csv$", full.names = FALSE)

if (length(all_csv_files) == 0) {
  stop("No threshold CSV files found in: ", Data.dir)
}

# Extract image IDs (e.g., "_DSC2653" from "_DSC2653[1].csv")
image_ids <- unique(gsub("\\[\\d+\\]\\.csv$", "", all_csv_files))

# Sort numerically by the DSC number
dsc_numbers <- as.integer(gsub("^_DSC", "", image_ids))
image_ids <- image_ids[order(dsc_numbers)]
dsc_numbers <- sort(dsc_numbers)

cat(sprintf("Found %d unique images (expected %d)\n", length(image_ids), EXPECTED_IMAGES))
cat(sprintf("DSC range: %d to %d\n", min(dsc_numbers), max(dsc_numbers)))

if (length(image_ids) < EXPECTED_IMAGES) {
  cat(sprintf("WARNING: Only %d images found. Waiting for %d more.\n",
              length(image_ids), EXPECTED_IMAGES - length(image_ids)))
  cat("Script will process whatever is available.\n")
}

if (length(image_ids) > EXPECTED_IMAGES) {
  cat(sprintf("WARNING: Found %d images (more than expected %d). Using first %d.\n",
              length(image_ids), EXPECTED_IMAGES, EXPECTED_IMAGES))
  image_ids <- image_ids[1:EXPECTED_IMAGES]
}

# Aggregate each image: read 100 threshold CSVs, compute %Area at each threshold
aggregate_image <- function(image_id, data_dir) {
  percent_area <- numeric(100)
  
  for (k in 1:100) {
    csv_file <- file.path(data_dir, paste0(image_id, "[", k, "].csv"))
    
    if (!file.exists(csv_file)) {
      percent_area[k] <- NA
      next
    }
    
    # Check if file is empty (0 bytes or no data lines)
    file_size <- file.info(csv_file)$size
    if (is.na(file_size) || file_size == 0) {
      percent_area[k] <- 0
      next
    }
    
    # Try to read the particle data (handle empty/header-only files)
    particle_data <- tryCatch(
      read.csv(csv_file, stringsAsFactors = FALSE),
      error = function(e) NULL,
      warning = function(w) {
        suppressWarnings(read.csv(csv_file, stringsAsFactors = FALSE))
      }
    )
    
    # Sum all particle areas
    if (!is.null(particle_data) && "Area" %in% names(particle_data) && nrow(particle_data) > 0) {
      total_area <- sum(particle_data$Area, na.rm = TRUE)
      percent_area[k] <- (total_area / TOTAL_IMAGE_AREA) * 100
    } else {
      # Empty file or no particles detected — 0% area above threshold
      percent_area[k] <- 0
    }
  }
  
  return(percent_area)
}

# Process all images
cat("Processing images...\n")
threshold_data <- data.frame()

for (i in seq_along(image_ids)) {
  img_id <- image_ids[i]
  
  pct_area <- aggregate_image(img_id, Data.dir)
  
  img_df <- data.frame(
    Position = i,
    ImageID = img_id,
    Threshold = 1:100,
    PercentArea = pct_area,
    stringsAsFactors = FALSE
  )
  
  threshold_data <- rbind(threshold_data, img_df)
  
  if (i %% 5 == 0 || i == length(image_ids)) {
    cat(sprintf("  Processed %d/%d images\n", i, length(image_ids)))
  }
}

cat(sprintf("Stage 1 complete: %d data points from %d images\n\n",
            nrow(threshold_data), length(image_ids)))

###############################################################################
###          STAGE 2: Map Images to Experimental Conditions                 ###
###############################################################################

cat("=== STAGE 2: Mapping images to conditions ===\n")

# Create mapping based on position in sorted sequence
# Each concentration block = 15 images (5 conditions x 3 surfaces)
# Within each block: groups of 3 (A, B, C) for each condition

n_images <- length(image_ids)

mapping <- data.frame(
  Position = 1:n_images,
  ImageID = image_ids,
  stringsAsFactors = FALSE
)

# Assign concentration (blocks of 15)
mapping$Concentration <- CONCENTRATIONS[ceiling(mapping$Position / 15)]

# Position within concentration block (1-15)
mapping$BlockPosition <- ((mapping$Position - 1) %% 15) + 1

# Assign condition (groups of 3 within block)
mapping$Condition <- CONDITIONS[ceiling(mapping$BlockPosition / 3)]

# Assign surface (cycling A, B, C)
mapping$Surface <- SURFACES[((mapping$BlockPosition - 1) %% 3) + 1]

# Set concentration as ordered factor
mapping$Concentration <- factor(mapping$Concentration, levels = CONCENTRATIONS, ordered = TRUE)
mapping$Condition <- factor(mapping$Condition, levels = CONDITIONS, ordered = TRUE)

# Print mapping table
cat("\nImage mapping:\n")
print(mapping %>% select(Position, ImageID, Concentration, Condition, Surface))

# Join mapping to threshold data
threshold_data <- threshold_data %>%
  left_join(mapping %>% select(Position, Concentration, Condition, Surface),
            by = "Position")

cat(sprintf("\nStage 2 complete: %d images mapped to %d concentrations x %d conditions x %d surfaces\n\n",
            n_images, length(unique(mapping$Concentration)),
            length(unique(mapping$Condition)), length(unique(mapping$Surface))))

###############################################################################
###          STAGE 3: Compute Recovery                                      ###
###############################################################################

cat("=== STAGE 3: Computing recovery ===\n")

# Safe division helper
safe_divide <- function(numerator, denominator, threshold = DIV_SAFETY) {
  ifelse(abs(denominator) < threshold, NA_real_, numerator / denominator)
}

# For each (Concentration, Surface) combination, compute recovery at each threshold
# Recovery = (Before - After) / (Before - Blank) * 100

recovery_data <- data.frame()

for (conc in CONCENTRATIONS) {
  for (surf in SURFACES) {
    # Extract curves for this concentration/surface
    blank_curve <- threshold_data %>%
      filter(Concentration == conc, Condition == "Blank", Surface == surf) %>%
      arrange(Threshold) %>%
      pull(PercentArea)
    
    before_curve <- threshold_data %>%
      filter(Concentration == conc, Condition == "Before", Surface == surf) %>%
      arrange(Threshold) %>%
      pull(PercentArea)
    
    # Skip if blank or before data is missing
    if (length(blank_curve) != 100 || length(before_curve) != 100) {
      cat(sprintf("  WARNING: Missing data for %s Surface %s (Blank=%d, Before=%d points)\n",
                  conc, surf, length(blank_curve), length(before_curve)))
      next
    }
    
    # Process each swipe condition
    for (swipe_cond in c("1_swipe", "2_swipes", "3_swipes")) {
      after_curve <- threshold_data %>%
        filter(Concentration == conc, Condition == swipe_cond, Surface == surf) %>%
        arrange(Threshold) %>%
        pull(PercentArea)
      
      if (length(after_curve) != 100) {
        cat(sprintf("  WARNING: Missing data for %s Surface %s %s (%d points)\n",
                    conc, surf, swipe_cond, length(after_curve)))
        next
      }
      
      # Compute recovery at each threshold
      numerator <- before_curve - after_curve
      denominator <- before_curve - blank_curve
      recovery <- safe_divide(numerator, denominator) * 100
      
      # Number of swipes as integer
      n_swipes <- as.integer(gsub("_swipe.*", "", swipe_cond))
      
      recovery_df <- data.frame(
        Concentration = conc,
        Surface = surf,
        Swipes = n_swipes,
        Condition = swipe_cond,
        Threshold = 1:100,
        Recovery = recovery,
        Before = before_curve,
        After = after_curve,
        Blank = blank_curve,
        BeforeMinusBlank = before_curve - blank_curve,
        stringsAsFactors = FALSE
      )
      
      recovery_data <- rbind(recovery_data, recovery_df)
    }
  }
}

# Set concentration as ordered factor in recovery data
recovery_data$Concentration <- factor(recovery_data$Concentration, levels = CONCENTRATIONS, ordered = TRUE)

cat(sprintf("Stage 3 complete: %d recovery values computed\n", nrow(recovery_data)))
cat(sprintf("  Combinations: %d concentrations x %d surfaces x %d swipe conditions x 100 thresholds\n\n",
            length(unique(recovery_data$Concentration)),
            length(unique(recovery_data$Surface)),
            length(unique(recovery_data$Swipes))))

###############################################################################
###          STAGE 4: Analysis                                              ###
###############################################################################

cat("=== STAGE 4: Analysis ===\n")

# -------------------------------------------------------------------------
# Q1: SATURATION ASSESSMENT
# -------------------------------------------------------------------------

cat("\n--- Q1: Saturation Assessment ---\n")

# Extract Before %Area at key thresholds for each concentration/surface
saturation_data <- threshold_data %>%
  filter(Condition == "Before", Threshold %in% c(10, 15, 20, 25, 30, 40, 50)) %>%
  group_by(Concentration, Threshold) %>%
  summarise(
    Mean_PercentArea = mean(PercentArea, na.rm = TRUE),
    SD_PercentArea = sd(PercentArea, na.rm = TRUE),
    CV = sd(PercentArea, na.rm = TRUE) / mean(PercentArea, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nBefore %Area by concentration and threshold:\n")
saturation_wide <- saturation_data %>%
  select(Concentration, Threshold, Mean_PercentArea) %>%
  pivot_wider(names_from = Threshold, values_from = Mean_PercentArea, names_prefix = "k=")
print(as.data.frame(saturation_wide))

# Identify saturated conditions (Before > 95% at a given threshold)
cat("\nSaturation status at k=15 (target: 20-60% for optimal discrimination):\n")
for (conc in CONCENTRATIONS) {
  before_k15 <- saturation_data %>%
    filter(Concentration == conc, Threshold == 15) %>%
    pull(Mean_PercentArea)
  
  if (length(before_k15) > 0) {
    status <- case_when(
      before_k15 > 95 ~ "SATURATED",
      before_k15 > 60 ~ "HIGH (above optimal)",
      before_k15 >= 20 ~ "OPTIMAL RANGE",
      before_k15 > 5 ~ "LOW (below optimal)",
      TRUE ~ "VERY LOW / NO SIGNAL"
    )
    cat(sprintf("  %s: Before_k15 = %.1f%% → %s\n", conc, before_k15, status))
  }
}

# -------------------------------------------------------------------------
# Q2: LINEARITY (Recovery vs Swipes)
# -------------------------------------------------------------------------

cat("\n--- Q2: Linearity (Recovery vs Number of Swipes) ---\n")

# For each concentration and threshold, fit linear model: Recovery ~ Swipes
# Average across surfaces first for robustness

recovery_means <- recovery_data %>%
  group_by(Concentration, Swipes, Threshold) %>%
  summarise(
    Mean_Recovery = mean(Recovery, na.rm = TRUE),
    SD_Recovery = sd(Recovery, na.rm = TRUE),
    N = sum(!is.na(Recovery)),
    .groups = "drop"
  )

# Fit linearity at each threshold for each concentration
linearity_results <- data.frame()

for (conc in CONCENTRATIONS) {
  for (k in 1:100) {
    subset <- recovery_means %>%
      filter(Concentration == conc, Threshold == k, !is.na(Mean_Recovery) & is.finite(Mean_Recovery))
    
    if (nrow(subset) < 3) next  # Need at least 3 points (1, 2, 3 swipes)
    
    # Fit linear model
    model <- lm(Mean_Recovery ~ Swipes, data = subset)
    r_squared <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    # Check proportionality: does intercept ≈ 0 and slope > 0?
    proportional <- (intercept > -10 & intercept < 10 & slope > 0)
    
    linearity_results <- rbind(linearity_results, data.frame(
      Concentration = conc,
      Threshold = k,
      R_squared = r_squared,
      Slope = slope,
      Intercept = intercept,
      Proportional = proportional,
      Recovery_1swipe = subset$Mean_Recovery[subset$Swipes == 1],
      Recovery_2swipes = subset$Mean_Recovery[subset$Swipes == 2],
      Recovery_3swipes = subset$Mean_Recovery[subset$Swipes == 3],
      stringsAsFactors = FALSE
    ))
  }
}

linearity_results$Concentration <- factor(linearity_results$Concentration,
                                          levels = CONCENTRATIONS, ordered = TRUE)

cat("\nBest linearity (R²) per concentration:\n")
best_linearity <- linearity_results %>%
  group_by(Concentration) %>%
  slice_max(R_squared, n = 3) %>%
  select(Concentration, Threshold, R_squared, Slope, Intercept, Recovery_1swipe)
print(as.data.frame(best_linearity))

# -------------------------------------------------------------------------
# Q3: OPTIMAL THRESHOLD SELECTION
# -------------------------------------------------------------------------

cat("\n--- Q3: Optimal Threshold Selection ---\n")

# Score each threshold by:
# 1. Discrimination: Before - Blank difference (higher = better)
# 2. Linearity: R² (higher = better)
# 3. Precision: 1/CV across surfaces (lower CV = better)
# 4. Not saturated: Before < 95%
# 5. Above noise: Before - Blank > 2%

# Compute discrimination (Before - Blank) at each threshold
discrimination_data <- threshold_data %>%
  filter(Condition %in% c("Before", "Blank")) %>%
  select(Concentration, Condition, Surface, Threshold, PercentArea) %>%
  pivot_wider(names_from = Condition, values_from = PercentArea) %>%
  mutate(Discrimination = Before - Blank)

discrimination_summary <- discrimination_data %>%
  group_by(Concentration, Threshold) %>%
  summarise(
    Mean_Discrimination = mean(Discrimination, na.rm = TRUE),
    Mean_Before = mean(Before, na.rm = TRUE),
    CV_Discrimination = sd(Discrimination, na.rm = TRUE) / 
                         abs(mean(Discrimination, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

# Compute surface-to-surface CV of recovery at each threshold
surface_cv <- recovery_data %>%
  group_by(Concentration, Threshold, Swipes) %>%
  summarise(
    CV_Recovery = sd(Recovery, na.rm = TRUE) / abs(mean(Recovery, na.rm = TRUE)) * 100,
    .groups = "drop"
  ) %>%
  group_by(Concentration, Threshold) %>%
  summarise(
    Mean_CV_Recovery = mean(CV_Recovery, na.rm = TRUE),
    .groups = "drop"
  )

# Combine into scoring table
optimal_scores <- linearity_results %>%
  left_join(discrimination_summary, by = c("Concentration", "Threshold")) %>%
  left_join(surface_cv, by = c("Concentration", "Threshold")) %>%
  mutate(
    # Filters (must pass)
    Pass_Saturation = Mean_Before < 95,
    Pass_Signal = Mean_Discrimination > 2,
    
    # Normalise scores to 0-1 range within each concentration
    .by = Concentration
  ) %>%
  group_by(Concentration) %>%
  mutate(
    Norm_Discrimination = (Mean_Discrimination - min(Mean_Discrimination, na.rm = TRUE)) /
      (max(Mean_Discrimination, na.rm = TRUE) - min(Mean_Discrimination, na.rm = TRUE) + 0.001),
    Norm_Linearity = R_squared,
    Norm_Precision = 1 - pmin(Mean_CV_Recovery / 100, 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Composite score (only where filters pass)
    Composite_Score = ifelse(
      Pass_Saturation & Pass_Signal,
      Norm_Discrimination * Norm_Linearity * Norm_Precision,
      0
    )
  )

# Find optimal threshold for each concentration
optimal_per_conc <- optimal_scores %>%
  group_by(Concentration) %>%
  slice_max(Composite_Score, n = 1) %>%
  select(Concentration, Threshold, Composite_Score, R_squared, Slope,
         Mean_Discrimination, Mean_Before, Mean_CV_Recovery)

cat("\nOptimal threshold per concentration:\n")
print(as.data.frame(optimal_per_conc))

# -------------------------------------------------------------------------
# Q4: SURFACE VARIABILITY
# -------------------------------------------------------------------------

cat("\n--- Q4: Surface-to-Surface Variability ---\n")

surface_variability <- recovery_data %>%
  filter(Threshold %in% c(15, 20, 25, 30, 35, 40)) %>%
  group_by(Concentration, Condition, Threshold) %>%
  summarise(
    Mean_Recovery = mean(Recovery, na.rm = TRUE),
    SD_Recovery = sd(Recovery, na.rm = TRUE),
    CV_Recovery = sd(Recovery, na.rm = TRUE) / abs(mean(Recovery, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

cat("\nMean CV (%) across surfaces by concentration:\n")
cv_summary <- surface_variability %>%
  filter(!is.na(CV_Recovery) & is.finite(CV_Recovery)) %>%
  group_by(Concentration) %>%
  summarise(
    Mean_CV = mean(CV_Recovery, na.rm = TRUE),
    Median_CV = median(CV_Recovery, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(cv_summary))

###############################################################################
###          STAGE 5: RECOMMENDATION                                        ###
###############################################################################

cat("\n=== STAGE 5: Generating Recommendation ===\n")

# Find overall best configuration
overall_best <- optimal_scores %>%
  slice_max(Composite_Score, n = 1)

best_conc <- as.character(overall_best$Concentration[1])
best_k <- overall_best$Threshold[1]
best_r2 <- overall_best$R_squared[1]
best_slope <- overall_best$Slope[1]
best_before <- overall_best$Mean_Before[1]
best_cv <- overall_best$Mean_CV_Recovery[1]
best_discrim <- overall_best$Mean_Discrimination[1]

# Get recovery values at optimal configuration
opt_recovery <- recovery_means %>%
  filter(Concentration == best_conc, Threshold == best_k)

rec_1 <- opt_recovery$Mean_Recovery[opt_recovery$Swipes == 1]
rec_2 <- opt_recovery$Mean_Recovery[opt_recovery$Swipes == 2]
rec_3 <- opt_recovery$Mean_Recovery[opt_recovery$Swipes == 3]

# Proportionality check
prop_2 <- if (length(rec_1) > 0 && length(rec_2) > 0 && rec_1 != 0) rec_2 / rec_1 else NA
prop_3 <- if (length(rec_1) > 0 && length(rec_3) > 0 && rec_1 != 0) rec_3 / rec_1 else NA

proportionality_pass <- !is.na(prop_2) && !is.na(prop_3) &&
  prop_2 > 1.5 && prop_2 < 2.5 &&
  prop_3 > 2.0 && prop_3 < 4.0

# Build recommendation text
recommendation <- paste0(
  "========================================================================\n",
  "                    CALIBRATION RECOMMENDATION\n",
  "========================================================================\n\n",
  sprintf("RECOMMENDED CONFIGURATION:\n"),
  sprintf("  Powder concentration: %s\n", best_conc),
  sprintf("  Optimal threshold:    k = %d\n", best_k),
  sprintf("  Before %%Area at k=%d: %.1f%%\n", best_k, best_before),
  sprintf("  Before-Blank signal:  %.1f%% (discrimination)\n", best_discrim),
  sprintf("\nPERFORMANCE METRICS:\n"),
  sprintf("  Linearity (R²):      %.3f\n", best_r2),
  sprintf("  Slope:               %.2f%% recovery per swipe\n", best_slope),
  sprintf("  Surface CV:          %.1f%%\n", ifelse(is.na(best_cv), NA, best_cv)),
  sprintf("\nRECOVERY BY SWIPE COUNT:\n"),
  sprintf("  1 swipe:  %.1f%%\n", ifelse(length(rec_1) > 0, rec_1, NA)),
  sprintf("  2 swipes: %.1f%% (ratio to 1 swipe: %.2f, expected: 2.0)\n",
          ifelse(length(rec_2) > 0, rec_2, NA),
          ifelse(!is.na(prop_2), prop_2, NA)),
  sprintf("  3 swipes: %.1f%% (ratio to 1 swipe: %.2f, expected: 3.0)\n",
          ifelse(length(rec_3) > 0, rec_3, NA),
          ifelse(!is.na(prop_3), prop_3, NA)),
  sprintf("\nPROPORTIONALITY: %s\n", ifelse(proportionality_pass, "PASS", "FAIL")),
  sprintf("\n------------------------------------------------------------------------\n"),
  sprintf("INTERPRETATION:\n"),
  ifelse(proportionality_pass,
         "  Recovery is proportional to area swiped at this concentration.\n  UV powder is a valid semi-quantitative surrogate under these conditions.\n",
         "  Recovery is NOT proportional to area swiped.\n  This may indicate saturation, non-uniform deposition, or method limitations.\n"),
  sprintf("\nALTERNATIVE THRESHOLDS (top 3 per concentration):\n")
)

# Add alternatives
for (conc in CONCENTRATIONS) {
  top3 <- optimal_scores %>%
    filter(Concentration == conc, Composite_Score > 0) %>%
    slice_max(Composite_Score, n = 3)
  
  if (nrow(top3) > 0) {
    recommendation <- paste0(recommendation, sprintf("\n  %s:\n", conc))
    for (j in 1:nrow(top3)) {
      recommendation <- paste0(recommendation,
                               sprintf("    k=%d (R²=%.3f, Before=%.1f%%, CV=%.1f%%)\n",
                                       top3$Threshold[j], top3$R_squared[j],
                                       top3$Mean_Before[j],
                                       ifelse(is.na(top3$Mean_CV_Recovery[j]), 0, top3$Mean_CV_Recovery[j])))
    }
  }
}

recommendation <- paste0(recommendation, "\n========================================================================\n")

cat(recommendation)

# Save recommendation
writeLines(recommendation, file.path(Output.dir, "Recommendation.txt"))
cat("  Saved: Recommendation.txt\n")

###############################################################################
###          STAGE 6: PLOTS                                                 ###
###############################################################################

cat("\n=== STAGE 6: Generating Plots ===\n")

# --- Plot 1: Threshold curves by concentration ---
p1 <- ggplot(threshold_data, aes(x = Threshold, y = PercentArea,
                                  color = Condition, linetype = Surface)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~Concentration, ncol = 1) +
  scale_color_manual(values = c("Blank" = "grey50", "Before" = "red",
                                "1_swipe" = "orange", "2_swipes" = "green3",
                                "3_swipes" = "blue")) +
  labs(title = "Threshold Curves by Powder Concentration",
       subtitle = "Each concentration shows Blank, Before, and 1-3 swipe conditions",
       x = "Threshold (k)",
       y = "% Area") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(Output.dir, "ThresholdCurves_ByConcentration.png"), p1,
       width = 10, height = 12, dpi = 300)
cat("  Saved: ThresholdCurves_ByConcentration.png\n")

# --- Plot 2: Saturation comparison ---
sat_plot_data <- threshold_data %>%
  filter(Condition == "Before") %>%
  group_by(Concentration, Threshold) %>%
  summarise(Mean = mean(PercentArea, na.rm = TRUE),
            SD = sd(PercentArea, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(sat_plot_data, aes(x = Threshold, y = Mean, color = Concentration)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD, fill = Concentration),
              alpha = 0.15, colour = NA) +
  geom_hline(yintercept = c(20, 60), linetype = "dashed", color = "grey50") +
  annotate("rect", xmin = 0, xmax = 100, ymin = 20, ymax = 60,
           alpha = 0.05, fill = "green") +
  annotate("text", x = 80, y = 40, label = "Optimal range\n(20-60%)",
           color = "darkgreen", size = 3) +
  labs(title = "Saturation Comparison: Before Curves by Concentration",
       subtitle = "Green band = optimal operating range for threshold analysis",
       x = "Threshold (k)",
       y = "% Area (Before image)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(Output.dir, "Saturation_Comparison.png"), p2,
       width = 10, height = 6, dpi = 300)
cat("  Saved: Saturation_Comparison.png\n")

# --- Plot 3: Recovery vs Swipes ---
# Use recovery at the optimal threshold per concentration
recovery_at_optimal <- data.frame()
for (conc in CONCENTRATIONS) {
  opt_k <- optimal_per_conc$Threshold[optimal_per_conc$Concentration == conc]
  if (length(opt_k) == 0) next
  
  subset <- recovery_data %>%
    filter(Concentration == conc, Threshold == opt_k[1])
  
  if (nrow(subset) > 0) {
    subset$OptimalK <- opt_k[1]
    recovery_at_optimal <- rbind(recovery_at_optimal, subset)
  }
}

if (nrow(recovery_at_optimal) > 0) {
  p3 <- ggplot(recovery_at_optimal, aes(x = Swipes, y = Recovery, color = Surface)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black",
                linetype = "dashed", alpha = 0.3) +
    facet_wrap(~Concentration, ncol = 3) +
    labs(title = "Recovery vs Number of Swipes (at Optimal Threshold)",
         subtitle = sprintf("Threshold k chosen per concentration to maximise linearity and discrimination"),
         x = "Number of Swipes",
         y = "Recovery (%)") +
    scale_x_continuous(breaks = 1:3) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(Output.dir, "Recovery_vs_Swipes.png"), p3,
         width = 12, height = 5, dpi = 300)
  cat("  Saved: Recovery_vs_Swipes.png\n")
}

# --- Plot 4: Linearity R² across thresholds ---
if (nrow(linearity_results) > 0) {
  p4 <- ggplot(linearity_results, aes(x = Threshold, y = R_squared, color = Concentration)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey50") +
    annotate("text", x = 80, y = 0.92, label = "R² = 0.9", color = "grey40", size = 3) +
    labs(title = "Linearity of Recovery vs Swipes Across All Thresholds",
         subtitle = "Higher R² = better proportionality between swipes and recovery",
         x = "Threshold (k)",
         y = "R² (Recovery ~ Swipes)") +
    ylim(0, 1) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(Output.dir, "Linearity_Statistics.png"), p4,
         width = 10, height = 6, dpi = 300)
  cat("  Saved: Linearity_Statistics.png\n")
}

# --- Plot 5: Composite score (optimal threshold identification) ---
if (nrow(optimal_scores) > 0) {
  p5 <- ggplot(optimal_scores %>% filter(Composite_Score > 0),
               aes(x = Threshold, y = Composite_Score, color = Concentration)) +
    geom_line(linewidth = 0.8) +
    geom_point(data = optimal_per_conc, 
               aes(x = Threshold, y = Composite_Score),
               size = 4, shape = 18) +
    labs(title = "Composite Score: Optimal Threshold Identification",
         subtitle = "Score = Discrimination × Linearity × Precision (where not saturated and signal > 2%)",
         x = "Threshold (k)",
         y = "Composite Score") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(Output.dir, "Optimal_Threshold_Score.png"), p5,
         width = 10, height = 6, dpi = 300)
  cat("  Saved: Optimal_Threshold_Score.png\n")
}

# --- Plot 6: Surface variability ---
if (nrow(recovery_data) > 0) {
  cv_heatmap_data <- recovery_data %>%
    filter(Threshold %in% seq(10, 50, by = 5)) %>%
    group_by(Concentration, Threshold, Swipes) %>%
    summarise(
      CV = sd(Recovery, na.rm = TRUE) / abs(mean(Recovery, na.rm = TRUE)) * 100,
      .groups = "drop"
    ) %>%
    filter(is.finite(CV))
  
  if (nrow(cv_heatmap_data) > 0) {
    p6 <- ggplot(cv_heatmap_data,
                 aes(x = factor(Threshold), y = factor(Swipes), fill = CV)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.0f", CV)), size = 2.5) +
      scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                           midpoint = 30, limits = c(0, 100)) +
      facet_wrap(~Concentration, ncol = 3) +
      labs(title = "Surface-to-Surface Variability (CV %)",
           subtitle = "Green = low variability (good reproducibility), Red = high variability",
           x = "Threshold (k)",
           y = "Number of Swipes",
           fill = "CV (%)") +
      theme_minimal()
    
    ggsave(file.path(Output.dir, "Surface_Variability.png"), p6,
           width = 12, height = 5, dpi = 300)
    cat("  Saved: Surface_Variability.png\n")
  }
}

###############################################################################
###          SAVE DATA                                                       ###
###############################################################################

cat("\n=== Saving data files ===\n")

# Save full calibration results
write.csv(recovery_data, file.path(Output.dir, "Calibration_Results.csv"), row.names = FALSE)
cat(sprintf("  Saved: Calibration_Results.csv (%d rows)\n", nrow(recovery_data)))

# Save threshold curves
write.csv(threshold_data, file.path(Output.dir, "ThresholdCurves_All.csv"), row.names = FALSE)
cat(sprintf("  Saved: ThresholdCurves_All.csv (%d rows)\n", nrow(threshold_data)))

# Save linearity results
write.csv(linearity_results, file.path(Output.dir, "Linearity_Results.csv"), row.names = FALSE)
cat(sprintf("  Saved: Linearity_Results.csv (%d rows)\n", nrow(linearity_results)))

# Save optimal scores
write.csv(optimal_scores, file.path(Output.dir, "Optimal_Scores.csv"), row.names = FALSE)
cat(sprintf("  Saved: Optimal_Scores.csv (%d rows)\n", nrow(optimal_scores)))

# Save image mapping
write.csv(mapping, file.path(Output.dir, "Image_Mapping.csv"), row.names = FALSE)
cat(sprintf("  Saved: Image_Mapping.csv (%d rows)\n", nrow(mapping)))

cat(sprintf("\n=== ALL OUTPUT SAVED TO: %s ===\n", Output.dir))
cat("\n=== CALIBRATION ANALYSIS COMPLETE ===\n")
