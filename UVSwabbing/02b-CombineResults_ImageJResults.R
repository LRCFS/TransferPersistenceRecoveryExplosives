### Combine ImageJ Results from Flat Folder Structure ###
# For data where all CSVs are in one ImageJResults folder
# Uses ImageMapping.csv to match original filenames to Surface/Rep/State
#
# PREREQUISITE:
#   1. Run 01-OrganizeImages.R first (creates ImageMapping.csv)
#   2. Process images in ImageJ using the threshold macro
#   3. Save all output CSVs to the ImageJResults folder

# === CONFIGURATION ===
Base.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Shared with Oliver/"
ImageJResults.dir <- paste0(Base.dir, "ImageJResults/")
OrganizedImages.dir <- paste0(Base.dir, "OrganizedImages/")
ThresholdResults.dir <- paste0(Base.dir, "ThresholdResults/")

# Create output directory
dir.create(ThresholdResults.dir, recursive = TRUE, showWarnings = FALSE)

# === READ IMAGE MAPPING ===
mapping_file <- paste0(OrganizedImages.dir, "ImageMapping.csv")

if (!file.exists(mapping_file)) {
  stop("ImageMapping.csv not found. Run 01-OrganizeImages.R first.")
}

mapping <- read.csv(mapping_file)

cat("=== COMBINING IMAGEJ RESULTS ===\n")
cat(sprintf("ImageMapping.csv loaded: %d entries\n", nrow(mapping)))

# === HELPER FUNCTION TO READ %Area FROM CSVs ===
read_percent_area_from_folder <- function(original_basename, imagej_dir) {
  area_values <- numeric(100)
  files_found <- 0
  
  for (k in 1:100) {
    csv_pattern <- paste0("^", original_basename, "\\[", k, "\\]\\.csv$")
    csv_file <- list.files(path = imagej_dir, pattern = csv_pattern, full.names = TRUE)
    
    if (length(csv_file) >= 1) {
      files_found <- files_found + 1
      csv_data <- read.csv(csv_file[1])
      
      if ("%Area" %in% names(csv_data)) {
        area_values[k] <- csv_data[["%Area"]][1]
      } else if ("X..Area" %in% names(csv_data)) {
        area_values[k] <- csv_data[["X..Area"]][1]
      } else {
        area_cols <- grep("Area", names(csv_data), value = TRUE, ignore.case = TRUE)
        area_cols <- area_cols[!grepl("Total", area_cols, ignore.case = TRUE)]
        if (length(area_cols) > 0) {
          area_values[k] <- csv_data[[area_cols[1]]][1]
        } else {
          area_values[k] <- NA
        }
      }
    } else {
      area_values[k] <- NA
    }
  }
  
  return(list(values = area_values, files_found = files_found))
}

# === PROCESS EACH SURFACE/REP COMBINATION ===

surface_reps <- unique(mapping$NewFolder)

cat(sprintf("Found %d Surface/Rep combinations\n", length(surface_reps)))

processed_folders <- c()

for (sr in surface_reps) {
  cat(sprintf("\n=== Processing: %s ===\n", sr))
  
  sr_mapping <- mapping[mapping$NewFolder == sr, ]
  
  states_needed <- c("Blank", "Before", "After")
  
  if (!all(states_needed %in% sr_mapping$State)) {
    cat(sprintf("  SKIPPED: Missing states (need Blank, Before, After)\n"))
    cat(sprintf("  Found: %s\n", paste(unique(sr_mapping$State), collapse = ", ")))
    next
  }
  
  results <- data.frame(
    Slice = paste0("Slice_K_", 100:1),
    X.Area = numeric(300)
  )
  
  all_found <- TRUE
  
  for (state in states_needed) {
    original_file <- sr_mapping$OriginalFile[sr_mapping$State == state][1]
    original_basename <- tools::file_path_sans_ext(original_file)
    
    cat(sprintf("  %s: %s... ", state, original_basename))
    
    area_data <- read_percent_area_from_folder(original_basename, ImageJResults.dir)
    area_values <- area_data$values
    files_found <- area_data$files_found
    
    cat(sprintf("%d files found\n", files_found))
    
    if (files_found < 100) {
      all_found <- FALSE
      cat(sprintf("    WARNING: Only %d of 100 threshold files found\n", files_found))
    }
    
    if (state == "Blank") {
      row_start <- 1
      row_end <- 100
    } else if (state == "Before") {
      row_start <- 101
      row_end <- 200
    } else if (state == "After") {
      row_start <- 201
      row_end <- 300
    }
    
    results$X.Area[row_start:row_end] <- rev(area_values)
  }
  
  output_folder <- file.path(ThresholdResults.dir, sr)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  output_path <- file.path(output_folder, "Summary.csv")
  write.csv(results, file = output_path, row.names = FALSE)
  
  cat(sprintf("  Saved: %s\n", output_path))
  
  if (all_found) {
    processed_folders <- c(processed_folders, sr)
  }
}

# === SUMMARY ===
cat("\n=== COMBINATION SUMMARY ===\n")
cat(sprintf("Processed %d Surface/Rep combinations\n", length(processed_folders)))
cat(sprintf("Output saved to: %s\n", ThresholdResults.dir))

# List created files
cat("\nCreated Summary.csv files:\n")
summary_files <- list.files(ThresholdResults.dir, pattern = "Summary.csv", recursive = TRUE, full.names = TRUE)
for (f in summary_files) {
  folder <- basename(dirname(f))
  data <- read.csv(f)
  non_na <- sum(!is.na(data$X.Area))
  cat(sprintf("  %s: %d non-NA values\n", folder, non_na))
}

# === CLEANUP: DELETE INDIVIDUAL CSV FILES ===
cat("\n=== CLEANUP: DELETING INDIVIDUAL CSV FILES ===\n")

total_deleted <- 0

for (sr in processed_folders) {
  sr_mapping <- mapping[mapping$NewFolder == sr, ]
  
  summary_path <- file.path(ThresholdResults.dir, sr, "Summary.csv")
  
  if (file.exists(summary_path)) {
    for (state in c("Blank", "Before", "After")) {
      state_rows <- sr_mapping$State == state
      if (any(state_rows)) {
        original_file <- sr_mapping$OriginalFile[state_rows][1]
        original_basename <- tools::file_path_sans_ext(original_file)
        
        for (k in 1:100) {
          csv_pattern <- paste0("^", original_basename, "\\[", k, "\\]\\.csv$")
          csv_files <- list.files(ImageJResults.dir, pattern = csv_pattern, full.names = TRUE)
          
          if (length(csv_files) > 0) {
            file.remove(csv_files)
            total_deleted <- total_deleted + length(csv_files)
          }
        }
      }
    }
    cat(sprintf("  %s: CSVs deleted\n", sr))
  } else {
    cat(sprintf("  SKIPPED %s: Summary.csv not found\n", sr))
  }
}

cat(sprintf("\nTotal CSV files deleted: %d\n", total_deleted))
cat(sprintf("Remaining CSV files in ImageJResults: %d\n", length(list.files(ImageJResults.dir, pattern = "\\.csv$"))))

cat("\n=== SCRIPT COMPLETE ===\n")
