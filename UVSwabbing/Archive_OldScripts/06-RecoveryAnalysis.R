### Recovery Analysis - Calculate recovery percentages ###
# This script calculates recovery percentages by comparing
# Blank, Before, and After measurements
#
# PREREQUISITE:
#   1. Run 00-GlobalCode.R first
#   2. Run 02-CombineResults.R to create Summary.csv files

AverageRecoveryValues <- list()

# Get list of threshold result folders
filenameFolders <- list.dirs(path = ThresholdResults.dir, full.names = TRUE, recursive = FALSE)

for (i in filenameFolders) {
  surfacerep <- basename(i)
  tempfile <- list.files(i, pattern = "Summary.csv", recursive = TRUE)
  
  for (j in tempfile) {
    version <- sub("/.*", "", j)
    Result.Dir <- paste0(i, "/", version)
    results_name <- paste0(surfacerep, "_", version)
    tempresults <- read.csv(paste0(i, "/", j)) %>%
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
    
    # Correct by adjusting data so the curves overlap by aligning the values closest to 90#
    # Find threshold for value closest to 90 of blank
    # This will be the threshold to which all other values are aligned
    Index90 <- which.min(abs(UncorrectedThresholdResults[,2] - 90))
    Blank90Value <- UncorrectedThresholdResults[[2]][Index90]
    
    CorrectedThresholdResults <- UncorrectedThresholdResults
    CorrectedMidValues <- c()
    
    CorrectedMidValues$Blank90 <- Blank90Value
    
    # Adjust threshold of AfterArea results at the values closest to 90
    After90Index <- which.min(abs(CorrectedThresholdResults[, 3] - 90))
    Offset <- as.numeric(Index90 - After90Index)
    After90Value <- CorrectedThresholdResults[[3]][After90Index]
    
    if (After90Value < Blank90Value) {
      Offset <- Offset + 1
    }
    
    # Apply offset correction
    if (Offset > 0) {
      CorrectedThresholdResults[[3]] <- c(rep(100, Offset),
                                          head(CorrectedThresholdResults[[3]], -Offset))
    } else if (Offset < 0) {
      CorrectedThresholdResults[[3]] <- c(tail(CorrectedThresholdResults[[3]], -abs(Offset)),
                                          rep(0, abs(Offset)))
    }
    
    ### Correct again by ensuring the values closest to 50#
    # Find threshold for value closest to 50 of blank
    # This will be the threshold to which all other values are aligned
    Index50 <- which.min(abs(CorrectedThresholdResults[,2] - 50))
    Blank50Value <- CorrectedThresholdResults[[2]][Index50]
    
    CorrectedMidValues$Blank50 <- Blank50Value
    
    # Adjust threshold of AfterArea results for values closest to 50
    After50Value <- CorrectedThresholdResults[[3]][Index50]
    
    if (After50Value < Blank50Value) {
      Offset <- 1
    } else {
      Offset <- 0
    }
    
    # Apply offset correction
    if (Offset > 0) {
      CorrectedThresholdResults[[3]] <- c(rep(100, Offset),
                                          head(CorrectedThresholdResults[[3]], -Offset))
    } else if (Offset < 0) {
      CorrectedThresholdResults[[3]] <- c(tail(CorrectedThresholdResults[[3]], -abs(Offset)),
                                          rep(0, abs(Offset)))
    }
    
    After90Value <- CorrectedThresholdResults[[3]][[Index90]]
    After50Value <- CorrectedThresholdResults[[3]][[Index50]]
    
    # Store corrected mid value
    CorrectedMidValues$After90 <- After90Value
    CorrectedMidValues$After50 <- After50Value
    
    # Adjust threshold of BeforeArea results by aligning the values closest to 90
    Before90Index <- which.min(abs(CorrectedThresholdResults[, 4] - After90Value))
    Offset <- as.numeric(Index90 - Before90Index)
    Before90Value <- CorrectedThresholdResults[[4]][Before90Index]
    
    if (Before90Value < After90Value) {
      Offset <- Offset + 1
    } else {
    }
    
    # Apply offset correction
    if (Offset > 0) {
      CorrectedThresholdResults[[4]] <- c(rep(100, Offset),
                                          head(CorrectedThresholdResults[[4]], -Offset))
    } else if (Offset < 0) {
      CorrectedThresholdResults[[4]] <- c(tail(CorrectedThresholdResults[[4]], -abs(Offset)),
                                          rep(0, abs(Offset)))
    }
    
    # Adjust threshold of BeforeArea results for values closest to 50
    Before50Value <- CorrectedThresholdResults[[4]][Index50]
    Offset <- 0
    
    while (Before50Value <= After50Value && (Index50 - Offset) > 0) {
      Offset <- Offset + 1
      Before50Value <- CorrectedThresholdResults[[4]][Index50 - Offset]
    }
    
    
    # Apply offset correction
    if (Offset > 0) {
      CorrectedThresholdResults[[4]] <- c(rep(100, Offset),
                                          head(CorrectedThresholdResults[[4]], -Offset))
    } else if (Offset < 0) {
      CorrectedThresholdResults[[4]] <- c(tail(CorrectedThresholdResults[[4]], -abs(Offset)),
                                          rep(0, abs(Offset)))
    }
    
    Before90Value <- CorrectedThresholdResults[[4]][[Index90]]
    Before50Value <- CorrectedThresholdResults[[4]][[Index50]]
    
    # Store corrected mid value
    CorrectedMidValues$Before90 <- Before90Value
    CorrectedMidValues$Before50 <- Before50Value
    
    # Convert to data frame for plotting
    CorrectedMidValues_df <- data.frame(File = names(CorrectedMidValues), AreaValue = as.numeric(CorrectedMidValues))
    
    CorrectedMidValues_df$Row <- as.numeric(gsub("[^0-9]", "", CorrectedMidValues_df$File))
    CorrectedMidValues_df <- CorrectedMidValues_df[order(CorrectedMidValues_df$Row), ]
    CorrectedMidValues_df$File <- factor(CorrectedMidValues_df$File, levels = CorrectedMidValues_df$File)
    
    # Plot heatmap
    p <- ggplot(CorrectedMidValues_df, aes(x = factor(File), y = Row, fill = AreaValue)) +
      geom_tile() +
      geom_text(aes(label = round(AreaValue, 2)), color = "black", size = 3) +
      scale_fill_gradient(low = "pink", high = "red") +
      labs(title = "Area Values at ~50 and ~90 for Each Image",
           x = "File and Threshold",
           y = "",
           fill = "Area Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    show(p)
    
    ggsave(
      filename = "ResultMidValueHeatmap.png",
      plot = p,
      path = file.path(Result.Dir),
      width = 7.5,
      height = 2.5,
      dpi = 300
    )
    
    #Plot Corrected Data
    CorrectedResultsPlot <- pivot_longer(CorrectedThresholdResults, 
                                             cols = c(BlankArea, BeforeArea, AfterArea),
                                             names_to = "Source", 
                                             values_to = "Value")
    
    # Convert Threshold to numeric
    CorrectedResultsPlot$Threshold <- as.numeric(CorrectedResultsPlot$Threshold)
    
    # Sort by Threshold
    CorrectedResultsPlot <- CorrectedResultsPlot[order(CorrectedResultsPlot$Threshold), ]
    
    # Create the plot
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
    
    # Calculate recovery by taking the ratio of Before - After and Before - Blank
    
    CorrectedThresholdResults$BeforeAfter <- CorrectedThresholdResults$BeforeArea - CorrectedThresholdResults$AfterArea
    CorrectedThresholdResults$BeforeBlank <- CorrectedThresholdResults$BeforeArea - CorrectedThresholdResults$BlankArea
    CorrectedThresholdResults$Recovery <- CorrectedThresholdResults$BeforeAfter / CorrectedThresholdResults$BeforeBlank * 100
    
    # Convert Threshold to numeric
    CorrectedThresholdResults$Threshold <- as.numeric(CorrectedThresholdResults$Threshold)
    
    # Sort by Threshold
    CorrectedThresholdResults <- CorrectedThresholdResults[order(CorrectedThresholdResults$Threshold), ]
                                                    
    # Remove data at low thresholds
    CorrectedThresholdResults <- CorrectedThresholdResults[35:65,]
    
    # Create the plot
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
    
    AverageRecoveryValues[[length(AverageRecoveryValues)+1]] <- data.frame(File = results_name, mean = tempAverageRecoveryValue, range = tempAverageRecoveryRange, StdDev = tempAverageRecoveryStdDev, RSD = tempAverageRecoveryRSD)
    
    print(paste0(results_name, " has been processed"))
  }
  
}

AverageRecovery <- bind_rows(AverageRecoveryValues)

# Save full results
write.csv(AverageRecovery, file = paste0(AnalysisOutput.dir, "AverageRecovery.csv"), row.names = FALSE)


AverageRecovery_filtered <- AverageRecovery %>%
  filter(RSD >= 0 & RSD <= 10)

# Extract Surface and Rep from File column for ordering
AverageRecovery_filtered <- AverageRecovery_filtered %>%
  mutate(
    Surface = as.numeric(gsub("Surface(\\d+)_Rep.*", "\\1", File)),
    Rep = as.numeric(gsub(".*Rep(\\d+)_.*", "\\1", File))
  )

AverageRecovery_filtered <- AverageRecovery_filtered %>%
  arrange(Surface, Rep)

AverageRecovery_filtered$File <- factor(AverageRecovery_filtered$File, levels = AverageRecovery_filtered$File)

AverageRecovery_removed <- AverageRecovery %>%
  filter(RSD > 10)

# Create the plot
p <- ggplot(AverageRecovery_filtered, aes(x = File, y = mean, fill = factor(Surface))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - StdDev, ymax = mean + StdDev), width = 0.2) +
  scale_fill_manual(values = ColourPalette) +
  labs(title = "Average Recovery per Surface/Rep",
       y = "Average Recovery (%)",
       x = "",
       fill = "Surface")+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

show(p)

ggsave(
  paste0("AverageRecoveryResults.png"),
  plot = p,
  path = file.path(AnalysisOutput.dir),
  width = 8,
  height = 5.0,
  dpi = 300)

cat("\n=== COMPLETE ===\n")
cat(sprintf("Recovery analysis complete.\n"))
cat(sprintf("Results saved to: %s\n", paste0(AnalysisOutput.dir, "AverageRecovery.csv")))
cat(sprintf("Total samples analyzed: %d\n", nrow(AverageRecovery)))
cat(sprintf("Samples filtered (RSD <= 10): %d\n", nrow(AverageRecovery_filtered)))
cat(sprintf("Samples removed (RSD > 10): %d\n", nrow(AverageRecovery_removed)))
