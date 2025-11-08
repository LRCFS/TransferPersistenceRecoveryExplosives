##Extract Blank and Before Results from summary files

AverageRecoveryValues <- list()

for (i in filenameThresholdResults) {
  participant <- sub(".* ", "", i)
  tempfile <- list.files(i, pattern = "Summary.csv", recursive = TRUE)
  
for (j in tempfile) {
    version <- sub("/.*", "", j)
    Result.Dir <- paste0(i, "/", version)
    results_name <- paste0(participant, "_", version)
    tempresults <- read.csv(paste0(i, "/", j)) %>%
      rename(Area = X.Area) %>%
      rename(Threshold = Slice) %>%
      select(Threshold, Area)
    
    tempresults$Threshold <- str_sub(tempresults$Threshold, 10, -6)
    
    # Split results by photograph
    Blanktempresults <- tempresults[1:100, ]
    colnames(Blanktempresults)[2] <- "BlankArea"
    Beforetempresults <- tempresults[101:200, ]
    colnames(Beforetempresults)[2] <- "BeforeArea"
    Aftertempresults <- tempresults[201:300, ]
    colnames(Aftertempresults)[2] <- "AfterArea"
    
    results_list <- list(Blanktempresults, Aftertempresults, Beforetempresults)
    
    UncorrectedThresholdResults <- reduce(results_list, full_join, by = "Threshold")
  
# Convert to long format
UncorrectedResultsPlot <- pivot_longer(UncorrectedThresholdResults, 
                                  cols = c(BlankArea,AfterArea,BeforeArea), 
                                  names_to = "Source", 
                                  values_to = "Value")

# Convert Threshold to numeric
UncorrectedResultsPlot$Threshold <- as.numeric(UncorrectedResultsPlot$Threshold)

# Sort by Threshold
UncorrectedResultsPlot <- UncorrectedResultsPlot[order(UncorrectedResultsPlot$Threshold), ]
                   
# Create the plot
p <- ggplot(UncorrectedResultsPlot, aes(x = Threshold, y = Value, color = Source, group = Source)) +
  geom_line() +
  labs(title = "Uncorrected Threshold vs Area",
       x = "Threshold",
       y = "Value",
       color = "Type") +
  theme_minimal()

show(p)

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

CorrectedMidValues[names(CorrectedThresholdResults)[2]] <- Blank90Value

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

CorrectedMidValues[names(CorrectedThresholdResults)[2]] <- Blank50Value

# Adjust threshold of AfterArea results for values closest to 50
After50Value <- CorrectedThresholdResults[[3]][Index50]

if (After50Value < Blank50Value) {
  Offset <- 1
}else{
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

#After90Index<- which(CorrectedThresholdResults[,3] == After90Value)
#After50Index<- which(CorrectedThresholdResults[,3] == After50Value)

After90Value <- CorrectedThresholdResults[[3]][[Index90]]
After50Value <- CorrectedThresholdResults[[3]][[Index50]]

# Store corrected mid value
CorrectedMidValues[names(CorrectedThresholdResults)[3]] <- After50Value

#  Adjust threshold of BeforeArea results  by aligning the values closest to 90
Before90Index <- which.min(abs(CorrectedThresholdResults[, 4] - After90Value))
Offset <- as.numeric(Index90 - Before90Index)
Before90Value <- CorrectedThresholdResults[[4]][Before90Index]

if (Before90Value < After90Value) {
  Offset <- Offset + 1
}else {
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

#After90Index<- which(CorrectedThresholdResults[,3] == After90Value)
#After50Index<- which(CorrectedThresholdResults[,3] == After50Value)

Before90Value <- CorrectedThresholdResults[[4]][[Index90]]
Before50Value <- CorrectedThresholdResults[[4]][[Index50]]

# Store corrected mid value
CorrectedMidValues[names(CorrectedThresholdResults)[3]] <- Before50Value

# Convert to data frame for plotting
CorrectedMidValues_df <- data.frame(File = names(CorrectedMidValues), AreaValue = as.numeric(CorrectedMidValues))

CorrectedMidValues_df$Row <-"AreaValue"

# Plot heatmap
p <- ggplot(CorrectedMidValues_df, aes(x = factor(File), y = Row, fill = AreaValue)) +
  geom_tile() +
  geom_text(aes(label = round(AreaValue, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "pink", high = "red") +
  labs(title = "Area Values at ~89 for Each Image",
       x = "File",
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
                                           cols = c(BlankArea, BeforeArea,AfterArea),
                                           names_to = "Source", 
                                           values_to = "Value")

# Convert Threshold to numeric
CorrectedResultsPlot$Threshold <- as.numeric(CorrectedResultsPlot$Threshold)

# Sort by Threshold
CorrectedResultsPlot <- CorrectedResultsPlot[order(CorrectedResultsPlot$Threshold), ]

# Create the plot
p <- ggplot(CorrectedResultsPlot, aes(x = Threshold, y = Value, color = Source, group = Source)) +
  geom_line() +
  labs(title = "Corrected Threshold vs Area for Each Image",
       x = "Threshold",
       y = "Value",
       color = "Source") +
  theme_minimal()

show(p)

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
       y = "Recovery") +
  ylim(0, 110) +
  theme_minimal()

show(p)

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
tempAverageRecoveryRSD <- tempAverageRecoveryStdDev/tempAverageRecoveryValue*100

AverageRecoveryValues[[length(AverageRecoveryValues)+1]] <- data.frame(File = results_name, mean = tempAverageRecoveryValue, range = tempAverageRecoveryRange, StdDev = tempAverageRecoveryStdDev, RSD = tempAverageRecoveryRSD)
  
print(paste0(results_name, " has been processed"))
}

}

AverageRecovery <- bind_rows(AverageRecoveryValues)

write.csv(AverageRecovery, file = paste0(DataFolder.dir,"AverageRecovery.csv"), row.names = FALSE)
