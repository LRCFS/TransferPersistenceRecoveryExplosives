BlankThresholdResultsCorrected <- BlankThresholdResults
CorrectedMidValues <- c()

for (i in 3:ncol(BlankThresholdResults)) {
  tempMidIndex <- which.min(abs(BlankThresholdResults[, i] - 50))
  Offset <- as.numeric(Target_Threshold - tempMidIndex)
  tempMidValue <- BlankThresholdResultsCorrected[[i]][tempMidIndex]
  
  # Adjust offset based on value range
  if (tempMidValue > 55){
    Offset <- Offset - 2
  } else if (tempMidValue < 45) {
    Offset <- Offset + 2
  }
  
  # Apply offset correction
  if (Offset > 0) {
    BlankThresholdResultsCorrected[[i]] <- c(rep(100, Offset),
                                             head(BlankThresholdResultsCorrected[[i]], -Offset))
  } else if (Offset < 0) {
    BlankThresholdResultsCorrected[[i]] <- c(tail(BlankThresholdResultsCorrected[[i]], -abs(Offset)),
                                             rep(0, abs(Offset)))
  }
  
  # Store corrected mid value
  CorrectedMidValues[names(BlankThresholdResultsCorrected)[i]] <- BlankThresholdResultsCorrected[[i]][tempMidIndex]
}

# Convert to data frame for plotting
CorrectedMidValues_df <- data.frame(File = names(CorrectedMidValues), AreaValue = as.numeric(CorrectedMidValues))

# Plot heatmap
p <- ggplot(CorrectedMidValues_df, aes(x = File, y = "AreaValue", fill = AreaValue)) +
  geom_tile() +
  geom_text(aes(label = round(AreaValue, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Area Values at tempMidIndex for Each Blank",
       x = "File",
       y = "",
       fill = "Area Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

show(p)

ggsave(
  filename = "tempMidIndexHeatmap.png",
  plot = p,
  path = file.path(BlankThresholdResults.dir),
  width = 7.5,
  height = 2.5,
  dpi = 300
)