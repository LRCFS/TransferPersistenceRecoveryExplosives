# Debug PILOT_021 CSV cleaning

# Read raw lines
filepath <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_021.csv"
lines <- readLines(filepath, warn = FALSE)

cat("First 15 lines of PILOT_021:\n")
for (i in 1:min(15, length(lines))) {
  cat(sprintf("Line %2d: %s\n", i, lines[i]))
}

cat("\n\nSearching for header components...\n")
for (i in 1:min(20, length(lines))) {
  line <- lines[i]
  
  if (grepl("analogValue.*load.*distance", line)) {
    cat(sprintf("Line %d contains 'analogValue,load,distance'\n", i))
  }
  
  if (grepl("timestamp.*elapsed_sec", line)) {
    cat(sprintf("Line %d contains 'timestamp,elapsed_sec'\n", i))
  }
}

cat("\n\nAttempting to clean with current function...\n")
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R")

data <- clean_and_read_csv(filepath)

if (!is.null(data)) {
  cat("✓ Successfully cleaned!\n")
  cat(sprintf("Loaded %d rows\n", nrow(data)))
  cat(sprintf("Columns: %s\n", paste(colnames(data), collapse=", ")))
  cat("\nFirst few rows:\n")
  print(head(data[, c("elapsed_sec", "load")], 10))
} else {
  cat("✗ Cleaning failed\n")
}
