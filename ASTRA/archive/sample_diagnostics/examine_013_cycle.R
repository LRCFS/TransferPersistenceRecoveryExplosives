# Examine PILOT_013 in detail around cycle 1 (11.8-13.7s)

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_013.csv"
data <- clean_and_read_csv(data_file)

cat("PILOT_013 - Detailed Analysis of First Cycle\n")
cat("==============================================\n\n")

# Look at the first detected cycle: 11.812 - 13.651s (1.84s duration)
cycle1_data <- data[data$elapsed_sec >= 11.5 & data$elapsed_sec <= 14, ]

cat("Data from 11.5-14s (around first cycle):\n\n")
cat("Time (s)  | Load (g) | In Range (175-210g)?\n")
cat("----------|----------|--------------------\n")

for (i in 1:nrow(cycle1_data)) {
  time <- cycle1_data$elapsed_sec[i]
  load <- cycle1_data$load[i]
  in_range <- load >= 175 & load <= 210
  marker <- ifelse(in_range, "  ✓", "   ")
  cat(sprintf("%8.3f  | %7.2f  | %s\n", time, load, marker))
}

cat("\n\nSummary:\n")
cat(sprintf("  Detected region: 11.812 - 13.651s (1.84s)\n"))
cat(sprintf("  Mean load in detected region: %.2fg\n", mean(cycle1_data[cycle1_data$elapsed_sec >= 11.812 & cycle1_data$elapsed_sec <= 13.651, ]$load)))

# Find where load is truly flat (small variation)
cycle1_in_range <- cycle1_data[cycle1_data$load >= 175 & cycle1_data$load <= 210, ]
cat(sprintf("  Time in 175-210g range: %.2fs\n", max(cycle1_in_range$elapsed_sec) - min(cycle1_in_range$elapsed_sec)))

# Check what portion is actually FLAT (e.g., within 180-190g - tighter range)
flat_data <- cycle1_data[cycle1_data$load >= 180 & cycle1_data$load <= 190, ]
if (nrow(flat_data) > 0) {
  cat(sprintf("  Time in 180-190g range (flatter): %.2fs\n", max(flat_data$elapsed_sec) - min(flat_data$elapsed_sec)))
}

# Even tighter - 182-188g
very_flat <- cycle1_data[cycle1_data$load >= 182 & cycle1_data$load <= 188, ]
if (nrow(very_flat) > 0) {
  cat(sprintf("  Time in 182-188g range (very flat): %.2fs\n", max(very_flat$elapsed_sec) - min(very_flat$elapsed_sec)))
}
