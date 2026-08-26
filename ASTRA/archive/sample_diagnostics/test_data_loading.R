# Quick test script to check data loading
library(tidyverse)

BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")

read_pressure_trace <- function(run_id) {
  file_path <- file.path(TRACES_PATH, paste0(run_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read CSV as character first
  df <- read_csv(file_path, 
                 col_names = FALSE,
                 col_types = cols(.default = "c"),
                 show_col_types = FALSE)
  
  n_cols <- ncol(df)
  cat(paste("  File has", n_cols, "columns\n"))
  
  if (n_cols == 5) {
    colnames(df) <- c("timestamp", "elapsed_sec", "analog_value", "load_g", "distance_mm")
  } else {
    warning(paste("Unexpected column count:", n_cols))
    return(NULL)
  }
  
  # Remove header rows
  df <- df %>%
    filter(!grepl("analog|load|distance|timestamp", timestamp, ignore.case = TRUE),
           !grepl("analog|load|distance", analog_value, ignore.case = TRUE))
  
  cat(paste("  After filtering headers:", nrow(df), "rows\n"))
  
  # Convert types
  df <- df %>%
    mutate(
      elapsed_sec = as.numeric(elapsed_sec),
      load_g = as.numeric(load_g),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g), load_g >= 0)
  
  cat(paste("  After type conversion and filtering:", nrow(df), "rows\n"))
  
  return(df)
}

# Test all samples
SAMPLES <- c("PILOT_001", "PILOT_005", "PILOT_009", "PILOT_013", 
             "PILOT_017", "PILOT_021", "PILOT_025", "PILOT_029")

for (sample in SAMPLES) {
  cat(paste("\nTesting", sample, "...\n"))
  result <- tryCatch({
    read_pressure_trace(sample)
  }, error = function(e) {
    cat(paste("  ERROR:", conditionMessage(e), "\n"))
    return(NULL)
  })
  
  if (!is.null(result)) {
    cat(paste("  SUCCESS - Final row count:", nrow(result), "\n"))
  } else {
    cat("  FAILED\n")
  }
}
