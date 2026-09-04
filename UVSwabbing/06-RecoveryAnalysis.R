### Recovery Analysis - Calculate recovery percentages ###
# This script calculates recovery percentages by comparing
# Blank, Before, and After measurements
#
# PREREQUISITE:
#   1. Run 00-GlobalCode.R first
#   2. Run 02-CombineResults.R to create Summary.csv files
#   3. Run 03-SplitThresholdData.R (optional - for scripts 04-05)

# Get list of Surface/Rep folders
filenameFolders <- list.dirs(path = ThresholdResults.dir, full.names = TRUE, recursive = FALSE)

AverageRecoveryValues <- list()

for (i in filenameFolders) {
  surfacerep <- basename(i)
  summary_path <- file.path(i, "Summary.csv")
  
  if (!file.exists(summary_path)) {
    cat(sprintf("SKIPPED: No Summary.csv in %s\n", surfacerep))
    next
  }
  
  cat(sprintf("Processing: %s\n", surfacerep))
  
  results_name <- surfacerep
  Result.Dir <- i
  
  tempresults <- read.csv(summary_path) %>%
    rename(Area = X.Area) %>%
    select(Area)
  
  # Add Threshold values (100:1, repeated 3 times for Blank, Before, After)
  tempresults$Threshold <- rep(100:1, 3)
  tempresults <- tempresults[, c("Threshold", "Area")]
  
  # Split results by photograph (rows 1-100=Blank, 101-200=Before, 201-300=After)
  Blanktempresults <- tempresults[1:100, ]
  Beforetempresults <- tempresults[101:200, ]
  Aftertempresults <- tempresults[201:300, ]
  
  # Use cbind to combine - much faster and memory-efficient than reduce(full_join)
  UncorrectedThresholdResults <- data.frame(
    Threshold = 100:1,
    BlankArea = Blanktempresults$Area,
    BeforeArea = Beforetempresults$Area,
    AfterArea = Aftertempresults$Area
  )
  
  # Convert to long format
  UncorrectedResultsPlot <- pivot_longer(UncorrectedThresholdResults, 
                                    cols = c(BlankArea, AfterArea, BeforeArea), 
                                    names_to = "Source", 
                                    values_to = "Value")
    
  # Convert Threshold to numeric
  UncorrectedResultsPlot$Threshold <- as.numeric(UncorrectedResultsPlot$Threshold)
  
  # Sort by Threshold
  UncorrectedResultsPlot <- UncorrectedResultsPlot[order(UncorrectedResultsPlot$Threshold), ]
                  
  # Create the plot
  p <- ggplot(UncorrectedResultsPlot, aes(x = Threshold, y = Value, color = Source, group = Source)) +
    geom_line() +
    scale_colour_manual(values = ColourPalette2) +
    labs(title = "Uncorrected Threshold vs Area",
         x = "Threshold",
         y = "Value",
         color = "Type") +
    theme_minimal()
  
  ggsave(
    paste0("UncorrectedResults.png"),
    plot = p,
    path = file.path(Result.Dir),
    width = 7.5,
    height = 5.0,
    dpi = 300
  )
  
  # === 90% ALIGNMENT ===
  Index90 <- which.min(abs(UncorrectedThresholdResults$BlankArea - 90))
  Blank90 <- UncorrectedThresholdResults$BlankArea[Index90]
  
  # Align After at 90%
  After90_idx <- which.min(abs(UncorrectedThresholdResults$AfterArea - 90))
  After_offset <- Index90 - After90_idx
  if (After_offset > 0) {
    UncorrectedThresholdResults$AfterArea <- c(rep(100, After_offset),
                                                head(UncorrectedThresholdResults$AfterArea, -After_offset))
  }
  
  # Align Before at 90%
  After90_aligned <- UncorrectedThresholdResults$AfterArea[Index90]
  Before90_idx <- which.min(abs(UncorrectedThresholdResults$BeforeArea - After90_aligned))
  Before_offset <- Index90 - Before90_idx
  if (Before_offset > 0) {
    UncorrectedThresholdResults$BeforeArea <- c(rep(100, Before_offset),
                                                 head(UncorrectedThresholdResults$BeforeArea, -Before_offset))
  }
  
  # === 50% ORDER CHECK ===
  Index50 <- which.min(abs(UncorrectedThresholdResults$BlankArea - 50))
  Blank50 <- UncorrectedThresholdResults$BlankArea[Index50]
  
  # Store original values for potential reversion
  OriginalAfterArea <- UncorrectedThresholdResults$AfterArea
  OriginalBeforeArea <- UncorrectedThresholdResults$BeforeArea
  
  # Initialize
  after_shifts <- 0
  before_shifts <- 0
  flagged <- FALSE
  
  # Constants
  TOLERANCE <- 2
  MAX_SHIFTS <- 10
  
  # Ensure After > Blank
  After50 <- UncorrectedThresholdResults$AfterArea[Index50]
  if (After50 <= Blank50) {
    After_Blank_diff <- Blank50 - After50
    
    if (After_Blank_diff > TOLERANCE) {
      while (After50 <= Blank50 && after_shifts < MAX_SHIFTS) {
        after_shifts <- after_shifts + 1
        UncorrectedThresholdResults$AfterArea <- c(100, head(UncorrectedThresholdResults$AfterArea, -1))
        After50 <- UncorrectedThresholdResults$AfterArea[Index50]
      }
      
      if (after_shifts >= MAX_SHIFTS) {
        cat(sprintf("  WARNING: %s - After shift limit reached, reverting\n", surfacerep))
        UncorrectedThresholdResults$AfterArea <- OriginalAfterArea
        after_shifts <- 0
        flagged <- TRUE
      }
    } else {
      flagged <- TRUE
    }
  }
  
  # Ensure Before > After
  After50 <- UncorrectedThresholdResults$AfterArea[Index50]
  Before50 <- UncorrectedThresholdResults$BeforeArea[Index50]
  
  if (Before50 <= After50) {
    Before_After_diff <- After50 - Before50
    
    if (Before_After_diff > TOLERANCE) {
      while (Before50 <= After50 && before_shifts < MAX_SHIFTS) {
        before_shifts <- before_shifts + 1
        UncorrectedThresholdResults$BeforeArea <- c(100, head(UncorrectedThresholdResults$BeforeArea, -1))
        Before50 <- UncorrectedThresholdResults$BeforeArea[Index50]
      }
      
      if (before_shifts >= MAX_SHIFTS) {
        cat(sprintf("  WARNING: %s - Before shift limit reached, reverting\n", surfacerep))
        UncorrectedThresholdResults$BeforeArea <- OriginalBeforeArea
        before_shifts <- 0
        flagged <- TRUE
      }
    } else {
      flagged <- TRUE
    }
  }
  
  if (flagged) {
    cat(sprintf("  FLAGGED: %s - curve order issues, no 50%% correction applied\n", surfacerep))
  }
  
  # Rename for clarity (data is now corrected)
  CorrectedThresholdResults <- UncorrectedThresholdResults
  
  # Convert to long format for plotting corrected curves
  CorrectedResultsPlot <- pivot_longer(CorrectedThresholdResults,
                                       cols = c(BlankArea, BeforeArea, AfterArea),
                                       names_to = "Source",
                                       values_to = "Value")
  
  CorrectedResultsPlot$Threshold <- as.numeric(CorrectedResultsPlot$Threshold)
  CorrectedResultsPlot <- CorrectedResultsPlot[order(CorrectedResultsPlot$Threshold), ]
  
  # Plot corrected curves
  p <- ggplot(CorrectedResultsPlot, aes(x = Threshold, y = Value, color = Source, group = Source)) +
    geom_line() +
    scale_colour_manual(values = ColourPalette2) +
    labs(title = "Corrected Threshold vs Area for Each Image",
         x = "Threshold",
         y = "Value",
         color = "Source") +
    theme_minimal()
  
  ggsave(
    paste0("CorrectedResults.png"),
    plot = p,
    path = file.path(Result.Dir),
    width = 7.5,
    height = 5.0,
    dpi = 300
  )
  
  # === RECOVERY CALCULATION ===
  CorrectedThresholdResults$BeforeAfter <- CorrectedThresholdResults$BeforeArea - CorrectedThresholdResults$AfterArea
  CorrectedThresholdResults$BeforeBlank <- CorrectedThresholdResults$BeforeArea - CorrectedThresholdResults$BlankArea
  CorrectedThresholdResults$Recovery <- CorrectedThresholdResults$BeforeAfter / CorrectedThresholdResults$BeforeBlank * 100
  
  CorrectedThresholdResults$Threshold <- as.numeric(CorrectedThresholdResults$Threshold)
  CorrectedThresholdResults <- CorrectedThresholdResults[order(CorrectedThresholdResults$Threshold), ]
  
  # Filter to threshold range 35-65
  CorrectedThresholdResults <- CorrectedThresholdResults[35:65,]
  
  # Plot recovery curve
  p <- ggplot(CorrectedThresholdResults, aes(x = Threshold, y = Recovery)) +
    geom_line() +
    labs(title = "Corrected Threshold vs Recovery",
         x = "Threshold",
         y = "Recovery (%)") +
    ylim(0, 110) +
    theme_minimal()
  
  ggsave(
    paste0("CorrectedRecoveryResults.png"),
    plot = p,
    path = file.path(Result.Dir),
    width = 7.5,
    height = 5.0,
    dpi = 300
  )
  
  tempAverageRecoveryValue <- mean(CorrectedThresholdResults$Recovery)
  tempAverageRecoveryRange <- max(CorrectedThresholdResults$Recovery) - min(CorrectedThresholdResults$Recovery)
  tempAverageRecoveryStdDev <- sd(CorrectedThresholdResults$Recovery)
  tempAverageRecoveryRSD <- tempAverageRecoveryStdDev / tempAverageRecoveryValue * 100
  
  AverageRecoveryValues[[length(AverageRecoveryValues)+1]] <- data.frame(File = results_name, mean = tempAverageRecoveryValue, range = tempAverageRecoveryRange, StdDev = tempAverageRecoveryStdDev, RSD = tempAverageRecoveryRSD, CurveOrderCheck = flagged)
  
  print(paste0(results_name, " has been processed"))
}

AverageRecovery <- bind_rows(AverageRecoveryValues)

# Load pattern mapping from ImageMapping.csv
mapping_file <- paste0(OrganizedImages.dir, "ImageMapping.csv")

if (!file.exists(mapping_file)) {
  stop("ImageMapping.csv not found. Run 01-OrganizeImages.R first.")
}

mapping_data <- read.csv(mapping_file)

# Extract unique Surface/Rep/Pattern combinations
pattern_mapping <- mapping_data %>%
  group_by(NewFolder) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    File = NewFolder,
    Pattern = Pattern
  )

# Define custom order for Pattern x-axis
pattern_order <- c("50g_BackandForth", "50g_Snake", "50g_Ratchet")
pattern_mapping$Pattern <- factor(pattern_mapping$Pattern, levels = pattern_order)

# Add Pattern column to AverageRecovery before saving
AverageRecovery <- AverageRecovery %>%
  left_join(pattern_mapping, by = "File")

# Save full results
AnalysisOutput.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/UV Powder Swabbing/ASTRA Testing/Analysis/"
dir.create(file.path(AnalysisOutput.dir), recursive = TRUE, showWarnings = FALSE)

write.csv(AverageRecovery, file = paste0(AnalysisOutput.dir, "AverageRecovery.csv"), row.names = FALSE)

# Filter by RSD (keep existing threshold)
AverageRecovery_filtered <- AverageRecovery %>%
  filter(RSD >= 0 & RSD <= 50)

# Check for missing patterns
if (any(is.na(AverageRecovery_filtered$Pattern))) {
  missing_files <- AverageRecovery_filtered$File[is.na(AverageRecovery_filtered$Pattern)]
  warning(paste("Some files not found in mapping:", paste(missing_files, collapse = ", ")))
}

# Order by Pattern (custom order already set in factor)
AverageRecovery_filtered <- AverageRecovery_filtered %>%
  arrange(Pattern)

# Create unique replicate number within each Pattern
AverageRecovery_filtered <- AverageRecovery_filtered %>%
  group_by(Pattern) %>%
  mutate(
    rep_id = row_number()
  ) %>%
  ungroup()

AverageRecovery_removed <- AverageRecovery %>%
  filter(RSD > 10)

# Define pattern colors (3 patterns)
pattern_colors <- pal_pattern

# Create the plot with side-by-side bars for each replicate
p <- ggplot(AverageRecovery_filtered, 
            aes(x = Pattern, y = mean, fill = Pattern, group = rep_id)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9),
           alpha = 0.7, 
           width = 0.6) +
  geom_errorbar(aes(ymin = mean - StdDev, ymax = mean + StdDev),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_manual(values = pattern_colors) +
  labs(title = "Average Recovery by Swabbing Pattern",
       subtitle = "Individual bars show separate replicates with error bars (±SD)",
       y = "Average Recovery (%)",
       x = "Swabbing Pattern",
       fill = "Pattern") +
  ylim(0, 110) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray40"))

show(p)

ggsave(
  paste0("AverageRecoveryResults.png"),
  plot = p,
  path = file.path(AnalysisOutput.dir),
  width = 8,
  height = 5.0,
  dpi = 300)
