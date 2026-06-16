###### For analysis of Befores ########
# This script analyzes the before (unswabbed) threshold curves
#
# PREREQUISITE:
#   1. Run 00-GlobalCode.R first
#   2. Run 03-SplitThresholdData.R to create Before files

# List all CSV files in the BeforeThresholdResults folder
BeforeThresholdResults_files <- list.files(path = BeforeThresholdResults.dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
BeforeThresholdResults_list <- list()

# Loop through each file
for (file in BeforeThresholdResults_files) {
  # Read the CSV
  tempresults <- read.csv(file)
  
  # Extract the filename without extension and remove last 7 characters
  results_name <- substr(tools::file_path_sans_ext(basename(file)), 1, nchar(tools::file_path_sans_ext(basename(file))) - 7)
  
  # Rename the non-threshold column(s) to the modified filename
  colnames(tempresults)[colnames(tempresults) != "Threshold"] <- results_name
  
  # Store in list
  BeforeThresholdResults_list[[results_name]] <- tempresults
}

# Merge all data frames by "threshold"
BeforeThresholdResults <- Reduce(function(x, y) merge(x, y, by = "Threshold", all = TRUE), BeforeThresholdResults_list)


# Extract numeric order from column names (excluding "Threshold")
file_names <- names(BeforeThresholdResults)[-1]
file_nums <- as.numeric(gsub("[^0-9]", "", file_names))
sorted_files <- file_names[order(file_nums)]

# Convert to long format
BeforeResultsPlot <- pivot_longer(BeforeThresholdResults, 
                                 cols = -Threshold, 
                                 names_to = "source", 
                                 values_to = "value")

# Create the plot
p <- ggplot(BeforeResultsPlot, aes(x = Threshold, y = value, color = factor(source, levels = sorted_files))) +
  geom_line() +
  labs(title = "Uncorrected Threshold vs Area for Each Before",
       x = "Threshold",
       y = "% Area",
       color = "Source") +
  theme_minimal()

show(p)

ggsave(
  paste0("UncorrectedBefores.png"),
  plot = p,
  path = file.path(BeforeThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

# Correct by adjusting data so the curves overlap by aligning the values closest to 50#
# Find threshold for value closest to 50 of first Before
# This will be the threshold to which all other Befores are aligned
Target_Threshold <- which.min(abs(BeforeThresholdResults[,2] - 50))

# Find value closest to 50 for all Befores
# Move column values up or down to align this value to the target threshold
BeforeThresholdResultsCorrected <- BeforeThresholdResults
CorrectedMidValues <- c()

for (i in 2:ncol(BeforeThresholdResults)) {
  tempMidIndex <- which.min(abs(BeforeThresholdResults[, i] - 50))
  Offset <- as.numeric(Target_Threshold - tempMidIndex)
  tempMidValue <- BeforeThresholdResultsCorrected[[i]][tempMidIndex]
  
  # Apply offset correction
  if (Offset > 0) {
    BeforeThresholdResultsCorrected[[i]] <- c(rep(100, Offset),
                                             head(BeforeThresholdResultsCorrected[[i]], -Offset))
  } else if (Offset < 0) {
    BeforeThresholdResultsCorrected[[i]] <- c(tail(BeforeThresholdResultsCorrected[[i]], -abs(Offset)),
                                             rep(0, abs(Offset)))
  }
  
  # Store corrected mid value
  CorrectedMidValues[names(BeforeThresholdResultsCorrected)[i]] <- BeforeThresholdResultsCorrected[[i]][Target_Threshold]
}

# Convert to data frame for plotting
CorrectedMidValues_df <- data.frame(File = names(CorrectedMidValues), AreaValue = as.numeric(CorrectedMidValues))

CorrectedMidValues_df$Row <-"AreaValue"

# Plot heatmap
p <- ggplot(CorrectedMidValues_df, aes(x = factor(File, levels = sorted_files), y = Row, fill = AreaValue)) +
  geom_tile() +
  geom_text(aes(label = round(AreaValue, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Area Values at ~50 for Each Before",
       x = "File",
       y = "",
       fill = "Area Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

show(p)

ggsave(
  filename = "BeforeMidValueHeatmap.png",
  plot = p,
  path = file.path(BeforeThresholdResults.dir),
  width = 7.5,
  height = 2.5,
  dpi = 300
)

# Plot Corrected Data
BeforeResultsCorrectedPlot <- pivot_longer(BeforeThresholdResultsCorrected, 
                                          cols = -Threshold, 
                                          names_to = "source", 
                                          values_to = "value")

# Create the plot
p <- ggplot(BeforeResultsCorrectedPlot, aes(x = Threshold, y = value, color = factor(source, levels = sorted_files))) +
  geom_line() +
  labs(title = "Corrected Threshold vs Area for Each Before",
       x = "Threshold",
       y = "% Area",
       color = "Source") +
  theme_minimal()

show(p)

ggsave(
  paste0("CorrectedBefores.png"),
  plot = p,
  path = file.path(BeforeThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

# Select measurement columns (skip Threshold)
threshold_cols <- names(BeforeThresholdResultsCorrected)[-1]

# Subset rows 35 to 65
BeforeDiff <- BeforeThresholdResultsCorrected[35:65, threshold_cols]

# Create a matrix for average differences
avg_matrix <- matrix(0, nrow = length(threshold_cols), ncol = length(threshold_cols),
                     dimnames = list(threshold_cols, threshold_cols))

# Compute average differences for each pair
for (i in 1:length(threshold_cols)) {
  for (j in 1:length(threshold_cols)) {
    if (i != j) {
      avg_matrix[i, j] <- mean(abs(BeforeDiff[[threshold_cols[i]]] - BeforeDiff[[threshold_cols[j]]]))
    }
  }
}

diag(avg_matrix) <- NA  # sets ALL diagonal cells to NA

# Melt the avg_matrix into long format
avg_long <- reshape2::melt(avg_matrix)

# Extract numeric parts from Var1 and Var2
avg_long$Var1_num <- as.numeric(gsub("[^0-9]", "", avg_long$Var1))
avg_long$Var2_num <- as.numeric(gsub("[^0-9]", "", avg_long$Var2))

# Keep only upper triangle (Var1 > Var2)
avg_long <- avg_long[avg_long$Var1_num >= avg_long$Var2_num, ]
avg_long <- na.omit(avg_long)
avg_long <- avg_long[!duplicated(avg_long$value), ]

# Plot heatmap with ordered axes
p <- ggplot(avg_long, aes(x = factor(Var2, levels = sorted_files),
                          y = factor(Var1, levels = sorted_files),
                          fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))),
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", na.value = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Differences Between Befores (Threshold 35-65)",
       x = "File 1",
       y = "File 2",
       fill = "Avg Difference")

show(p)

ggsave(
  paste0("CorrectedBeforesAvgDiff.png"),
  plot = p,
  path = file.path(BeforeThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

AverageDiff <- mean(avg_long$value)
StdDev <- sd(avg_long$value)

avg_long_reduced <- avg_long

# Plot heatmap with ordered axes
p <- ggplot(avg_long_reduced, aes(x = factor(Var2, levels = sorted_files),
                                  y = factor(Var1, levels = sorted_files),
                                  fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))),
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", na.value = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Differences Between Befores (Threshold 35-65)",
       x = "File 1",
       y = "File 2",
       fill = "Avg Difference")

show(p)

ggsave(
  paste0("CorrectedBeforesAvgDiffFiltered.png"),
  plot = p,
  path = file.path(BeforeThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

AverageDiffFiltered <- mean(avg_long_reduced$value)
StdDevFiltered <- sd(avg_long_reduced$value)

cat("\n=== COMPLETE ===\n")
cat(sprintf("Before analysis complete.\n"))
cat(sprintf("Average Difference: %.2f\n", AverageDiff))
cat(sprintf("Standard Deviation: %.2f\n", StdDev))
