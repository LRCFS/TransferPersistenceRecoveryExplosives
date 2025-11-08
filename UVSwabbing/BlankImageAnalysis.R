###### For analysis of blanks ########
# List all CSV files in the BlankThresholdResults folder
BlankThresholdResults_files <- list.files(path = BlankThresholdResults.dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
BlankThresholdResults_list <- list()

# Loop through each file
for (file in BlankThresholdResults_files) {
  # Read the CSV
  tempresults <- read.csv(file)
  
  # Extract the filename without extension and remove last 6 characters
  results_name <- substr(tools::file_path_sans_ext(basename(file)), 1, nchar(tools::file_path_sans_ext(basename(file))) - 6)
  
  # Rename the non-threshold column(s) to the modified filename
  colnames(tempresults)[colnames(tempresults) != "Threshold"] <- results_name
  
  # Store in list
  BlankThresholdResults_list[[results_name]] <- tempresults
}

# Merge all data frames by "threshold"
BlankThresholdResults <- Reduce(function(x, y) merge(x, y, by = "Threshold", all = TRUE), BlankThresholdResults_list)


# Extract numeric order from column names (excluding "Threshold")
file_names <- names(BlankThresholdResults)[-1]
file_nums <- as.numeric(gsub("[^0-9]", "", file_names))
sorted_files <- file_names[order(file_nums)]

# Convert to long format
BlankResultsPlot <- pivot_longer(BlankThresholdResults, 
                                 cols = -Threshold, 
                                 names_to = "source", 
                                 values_to = "value")

# Create the plot
p <- ggplot(BlankResultsPlot, aes(x = Threshold, y = value, color = factor(source, levels = sorted_files))) +
  geom_line() +
  labs(title = "Uncorrected Threshold vs Area for Each Blank",
       x = "Threshold",
       y = "Value",
       color = "Source") +
  theme_minimal()

show(p)

ggsave(
  paste0("UncorrectedBlanks.png"),
  plot = p,
  path = file.path(BlankThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

# Correct by adjusting data so the curves overlap by aligning the values closest to 50#
# Find threshold for value closest to 50 of first blank
# This will be the threshold to which all other blanks are aligned
Target_Threshold <- which.min(abs(BlankThresholdResults[,2] - 50))

# Find value closest to 50 for all blanks
# Move column values up or down to align this value to the target threshold
BlankThresholdResultsCorrected <- BlankThresholdResults
CorrectedMidValues <- c()

for (i in 2:ncol(BlankThresholdResults)) {
  tempMidIndex <- which.min(abs(BlankThresholdResults[, i] - 50))
  Offset <- as.numeric(Target_Threshold - tempMidIndex)
  tempMidValue <- BlankThresholdResultsCorrected[[i]][tempMidIndex]

  # Apply offset correction
  if (Offset > 0) {
    BlankThresholdResultsCorrected[[i]] <- c(rep(100, Offset),
                                             head(BlankThresholdResultsCorrected[[i]], -Offset))
  } else if (Offset < 0) {
    BlankThresholdResultsCorrected[[i]] <- c(tail(BlankThresholdResultsCorrected[[i]], -abs(Offset)),
                                             rep(0, abs(Offset)))
  }
  
  # Store corrected mid value
  CorrectedMidValues[names(BlankThresholdResultsCorrected)[i]] <- BlankThresholdResultsCorrected[[i]][Target_Threshold]
}

# Convert to data frame for plotting
CorrectedMidValues_df <- data.frame(File = names(CorrectedMidValues), AreaValue = as.numeric(CorrectedMidValues))

CorrectedMidValues_df$Row <-"AreaValue"

# Plot heatmap
p <- ggplot(CorrectedMidValues_df, aes(x = factor(File, levels = sorted_files), y = Row, fill = AreaValue)) +
  geom_tile() +
  geom_text(aes(label = round(AreaValue, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Area Values at ~50 for Each Blank",
       x = "File",
       y = "",
       fill = "Area Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

show(p)

ggsave(
  filename = "BlankMidValueHeatmap.png",
  plot = p,
  path = file.path(BlankThresholdResults.dir),
  width = 7.5,
  height = 2.5,
  dpi = 300
)

# Plot Corrected Data
BlankResultsCorrectedPlot <- pivot_longer(BlankThresholdResultsCorrected, 
                                          cols = -Threshold, 
                                          names_to = "source", 
                                          values_to = "value")

# Create the plot
p <- ggplot(BlankResultsCorrectedPlot, aes(x = Threshold, y = value, color = factor(source, levels = sorted_files))) +
  geom_line() +
  labs(title = "Corrected Threshold vs Area for Each Blank",
       x = "Threshold",
       y = "Value",
       color = "Source") +
  theme_minimal()

show(p)

ggsave(
  paste0("CorrectedBlanks.png"),
  plot = p,
  path = file.path(BlankThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)

# Select measurement columns (skip Threshold)
threshold_cols <- names(BlankThresholdResultsCorrected)[-1]

# Subset rows 30 to 100
BlankDiff <- BlankThresholdResultsCorrected[30:100, threshold_cols]

# Create a matrix for average differences
avg_matrix <- matrix(0, nrow = length(threshold_cols), ncol = length(threshold_cols),
                     dimnames = list(threshold_cols, threshold_cols))

# Compute average differences for each pair
for (i in 1:length(threshold_cols)) {
  for (j in 1:length(threshold_cols)) {
    if (i != j) {
      avg_matrix[i, j] <- mean(abs(BlankDiff[[threshold_cols[i]]] - BlankDiff[[threshold_cols[j]]]))
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
avg_long <- avg_long[avg_long$Var1_num > avg_long$Var2_num, ]

# Plot heatmap with ordered axes
p <- ggplot(avg_long, aes(x = factor(Var2, levels = sorted_files),
                          y = factor(Var1, levels = sorted_files),
                          fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))),
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", na.value = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Differences (Threshold 30-100)",
       x = "File 1",
       y = "File 2",
       fill = "Avg Difference")

show(p)

ggsave(
  paste0("CorrectedBlanksAvgDiff.png"),
  plot = p,
  path = file.path(BlankThresholdResults.dir),
  width = 7.5,
  height = 5.0,
  dpi = 300
)
