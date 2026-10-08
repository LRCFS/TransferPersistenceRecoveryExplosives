### Combine ImageJ Results from Flat Folder Structure ###
# For data where all CSVs are in one ImageJResults folder
# Uses ImageMapping.csv to match original filenames to Surface/Rep/State
#
# PREREQUISITE:
#   1. Run 01_OrganizeImages.R first (creates ImageMapping.csv)
#   2. Process images in ImageJ using the threshold macro
#   3. Save all output CSVs to the ImageJResults folder

# === CONFIGURATION ===
Base.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Shared with Oliver/"
ImageJResults.dir <- paste0(Base.dir, "ImageJResults/")
OrganizedImages.dir <- paste0(Base.dir, "OrganizedImages/")
ThresholdResults.dir <- paste0(Base.dir, "ThresholdResults/")

# === SAFETY TOGGLE (added Oct 2026 red-team fix) ===
# This script's cleanup step at the bottom permanently deletes the raw
# per-threshold ImageJ CSVs once Summary.csv is written -- see
# 02_CombineResults.R's own copy of this comment for the full Oct 2026
# rationale (a %Area-extraction bug there was found to silently pick up
# raw-pixel Total.Area instead of the percentage; this script shares the
# same two now-fixed broken checks, just happened to avoid the bug in
# practice via its own fallback's Total-exclusion). Default FALSE so a
# first re-run after this fix can be sanity-checked before trusting the
# destructive step. Set to TRUE once you're confident the output is correct.
DELETE_INDIVIDUAL_CSVS <- FALSE

# Create output directory
dir.create(ThresholdResults.dir, recursive = TRUE, showWarnings = FALSE)

# === READ IMAGE MAPPING ===
mapping_file <- paste0(OrganizedImages.dir, "ImageMapping.csv")

if (!file.exists(mapping_file)) {
  stop("ImageMapping.csv not found. Run 01_OrganizeImages.R first.")
}

mapping <- read.csv(mapping_file)

cat("=== COMBINING IMAGEJ RESULTS ===\n")
cat(sprintf("ImageMapping.csv loaded: %d entries\n", nrow(mapping)))

# === HELPER FUNCTION TO READ %Area FROM CSVs ===
read_percent_area_from_folder <- function(original_basename, imagej_dir) {
  area_values <- numeric(100)
  files_found <- 0
  any_implausible <- FALSE
  
  for (k in 1:100) {
    csv_pattern <- paste0("^", original_basename, "\\[", k, "\\]\\.csv$")
    csv_file <- list.files(path = imagej_dir, pattern = csv_pattern, full.names = TRUE)
    
    if (length(csv_file) >= 1) {
      files_found <- files_found + 1
      csv_data <- read.csv(csv_file[1])
      
      # Structural format guard (added Oct 2026, found via real-data
      # verification of the %Area fix -- see 02_CombineResults.R's
      # identical check for the full rationale): a genuine ImageJ
      # "Summarize" CSV always has exactly 1 data row. 4 real files in
      # this study's own data turned out to be per-particle "Analyze
      # Particles" dumps instead (thousands of rows, header
      # " ,Area,Mean,Min,Max") -- these have a column literally named
      # "Area" too, so column-name checks alone can't distinguish them,
      # and the first particle's pixel area can coincidentally fall
      # inside the 0-100 plausibility range by chance.
      if (nrow(csv_data) != 1) {
        warning(sprintf(
          "%s has %d data row(s), expected exactly 1 (Summarize format) -- likely a per-particle 'Analyze Particles' export saved under the wrong filename, treating as NA",
          basename(csv_file[1]), nrow(csv_data)
        ))
        area_values[k] <- NA
        any_implausible <- TRUE
        next
      }
      
      # BUG FIX (Oct 2026 red-team review): same wrong sanitized-column
      # literal as 02_CombineResults.R -- read.csv()/make.names() turns a
      # literal "%Area" header into "X.Area" (ONE dot), never "X..Area"
      # (two dots), so this explicit check could never match. This script
      # happened to still extract the right value in practice because its
      # fallback below already excludes "Total" from candidate columns
      # (02_CombineResults.R's own fallback lacked that exclusion, which IS
      # how the bug manifested there) -- but relying on that fallback
      # alone is fragile, so fixing the explicit check here too rather
      # than leaving it as permanently-dead-but-harmless code.
      if ("%Area" %in% names(csv_data)) {
        area_values[k] <- csv_data[["%Area"]][1]
      } else if ("X.Area" %in% names(csv_data)) {
        area_values[k] <- csv_data[["X.Area"]][1]
      } else {
        area_cols <- grep("Area", names(csv_data), value = TRUE, ignore.case = TRUE)
        area_cols <- area_cols[!grepl("Total", area_cols, ignore.case = TRUE)]
        if (length(area_cols) > 0) {
          pct_col <- grep("%Area|^X\\.Area$", area_cols, value = TRUE)
          if (length(pct_col) > 0) {
            area_values[k] <- csv_data[[pct_col[1]]][1]
          } else {
            area_values[k] <- csv_data[[area_cols[1]]][1]
          }
        } else {
          area_values[k] <- NA
        }
      }
      
      # Plausibility guard (added Oct 2026 red-team fix) -- see
      # 02_CombineResults.R's identical check for the full rationale.
      if (!is.na(area_values[k]) && (area_values[k] < 0 || area_values[k] > 100)) {
        warning(sprintf(
          "Implausible %%Area value (%.1f) for %s[%d] -- likely wrong column picked, treating as NA",
          area_values[k], original_basename, k
        ))
        area_values[k] <- NA
        any_implausible <- TRUE
      }
    } else {
      area_values[k] <- NA
    }
  }
  
  return(list(values = area_values, files_found = files_found, any_implausible = any_implausible))
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
  any_implausible_overall <- FALSE
  
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
    
    # Track plausibility across all states (added Oct 2026 red-team fix) --
    # feeds the cleanup gate below, mirroring 02_CombineResults.R's
    # equivalent fix.
    if (isTRUE(area_data$any_implausible)) {
      any_implausible_overall <- TRUE
      cat(sprintf("    WARNING: %s had implausible %%Area value(s) -- this folder's raw CSVs will NOT be deleted\n", state))
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
  
  # BUG FIX (Oct 2026 red-team review): previously gated on all_found alone
  # -- see 02_CombineResults.R's identical fix for the full rationale. Now
  # also requires !any_implausible_overall.
  if (all_found && !any_implausible_overall) {
    processed_folders <- c(processed_folders, sr)
  } else if (all_found && any_implausible_overall) {
    cat(sprintf("  NOTE: %s had complete file counts but implausible %%Area value(s) -- excluded from cleanup\n", sr))
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
# Gated on DELETE_INDIVIDUAL_CSVS (set near the top of this script) -- see
# 02_CombineResults.R's identical gate for the full Oct 2026 rationale.

if (!DELETE_INDIVIDUAL_CSVS) {
  cat("\n=== CLEANUP SKIPPED: DELETE_INDIVIDUAL_CSVS is FALSE ===\n")
  cat(sprintf("%d folder(s) were eligible for cleanup (complete file counts, no implausible values) but raw CSVs were left in place.\n", length(processed_folders)))
  cat("Inspect the regenerated Summary.csv files above, then set DELETE_INDIVIDUAL_CSVS <- TRUE and re-run this script to actually delete the raw per-threshold CSVs.\n")
} else {

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

}

cat("\n=== SCRIPT COMPLETE ===\n")
