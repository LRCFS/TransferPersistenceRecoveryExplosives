# Generate 25s increment plots for PILOT_001
# This creates individual plots for every 25s window to review each cycle

library(png)

# Load data
cat("Loading data...\n")
data <- read.csv("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_001.csv")

# Load detected cycles
cat("Loading detected cycles...\n")
cycles <- read.csv("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs/time_based/PILOT_001_detected_cycles.csv")

# Parameters
stable_range_low <- 185
stable_range_high <- 200

# Output directory
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs/time_based/25s_windows"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Creating plots in: %s\n", output_dir))

# Get time range
max_time <- max(data$elapsed_sec, na.rm = TRUE)
cat(sprintf("Data time range: 0 - %.1fs\n", max_time))

# Create plots in 25s windows
window_size <- 25
window_starts <- seq(0, max_time, by = window_size)

cat(sprintf("Creating %d plots (25s windows)...\n", length(window_starts)))

for (i in 1:length(window_starts)) {
  window_start <- window_starts[i]
  window_end <- window_start + window_size
  
  # Filter data for this window
  window_data <- data[data$elapsed_sec >= window_start & data$elapsed_sec < window_end, ]
  
  if (nrow(window_data) == 0) {
    cat(sprintf("  Window %d (%.0f-%.0fs): No data, skipping\n", i, window_start, window_end))
    next
  }
  
  # Filter cycles for this window
  window_cycles <- cycles[cycles$start_time >= window_start & cycles$start_time < window_end, ]
  # Only real cycles (duration > 0)
  window_cycles_real <- window_cycles[window_cycles$duration > 0, ]
  
  # Create filename
  filename <- sprintf("PILOT_001_%03d_%.0f-%.0fs.png", i, window_start, window_end)
  filepath <- file.path(output_dir, filename)
  
  # Create plot
  png(filepath, width = 1200, height = 600, res = 100)
  
  par(mar = c(4, 4, 3, 1))
  
  plot(window_data$elapsed_sec, window_data$load, type = "l", col = "gray40", lwd = 1.5,
       xlab = "Time (s)", ylab = "Load (g)",
       main = sprintf("PILOT_001: %.0f - %.0fs (%d cycles)", 
                      window_start, window_end, nrow(window_cycles_real)),
       ylim = c(0, max(window_data$load, na.rm = TRUE) * 1.05))
  
  # Add grid
  grid(col = "gray90", lty = 1)
  
  # Draw threshold lines
  abline(h = stable_range_low, col = "blue", lty = 2, lwd = 2)
  abline(h = stable_range_high, col = "blue", lty = 2, lwd = 2)
  
  # Highlight detected stable regions
  if (nrow(window_cycles_real) > 0) {
    for (j in 1:nrow(window_cycles_real)) {
      cycle <- window_cycles_real[j, ]
      rect(cycle$start_time, 0, cycle$end_time, max(window_data$load, na.rm = TRUE),
           col = rgb(0, 1, 0, 0.25), border = NA)
      
      # Add cycle number label
      text(cycle$start_time + cycle$duration/2, max(window_data$load, na.rm = TRUE) * 0.95,
           labels = sprintf("#%d", cycle$cycle_num), cex = 0.8, col = "darkgreen", font = 2)
    }
  }
  
  # Add legend
  legend("topright", 
         legend = c("Load", "Stable thresholds (185-200g)", "Detected stable"),
         col = c("gray40", "blue", rgb(0, 1, 0, 0.5)),
         lty = c(1, 2, 1), lwd = c(1.5, 2, 8), bty = "n", cex = 0.9)
  
  dev.off()
  
  if (i %% 5 == 0) {
    cat(sprintf("  Created %d/%d plots\n", i, length(window_starts)))
  }
}

cat("\n========================================\n")
cat(sprintf("COMPLETE! Created plots in:\n%s\n", output_dir))
cat("========================================\n")
