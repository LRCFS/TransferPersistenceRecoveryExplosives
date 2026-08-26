###########################################################################
#
# RunAllDatasets.R - Batch Processing for FINEX Swabbing Study
#
# Detection and quantitation of PETN and RDX in swab samples - Copyright (C) 2025
# Leverhulme Research Centre for Forensic Science
# Website: https://github.com/LRCFS/GcMsDataProcess
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
###########################################################################
#
# USAGE:
#   1. Set 'accepted_analysis_dir' path below
#   2. Set working directory to GCMSQuantitation/ (or run from there)
#   3. Configure GlobalCode.R settings (RTs, thresholds, drift correction)
#   4. Configure reprocessing flags in Code/02_PeakDetection.R if needed
#   5. Run: source("FINEX/RunAllDatasets.R")
#
# OUTPUT:
#   - Individual Results/ folders per dataset (as usual)
#   - BatchProcessing_Log.csv in Accepted Analysis folder
#   - FINEX_StudyResults.xlsx (collated study results)
#
# BEHAVIOR:
#   - Processes ALL folders except "Plots" (case-insensitive)
#   - Continues on error (logs failures, reports at end)
#   - Always runs 04_CollateStudyResults.R after all batches
#   - Respects reprocess flags in 02_PeakDetection.R
#   - Uses same GlobalCode.R settings for all datasets
#
###########################################################################

###########################################################################
# Configuration
###########################################################################

# Path to Accepted Analysis folder
accepted_analysis_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

# Folders to exclude (case-insensitive match)
exclude_folders <- c("Plots")

# Set working directory to GCMSQuantitation folder
# (so relative paths to Code/, GlobalCode.R work)
setwd("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation")

# Verify required files exist
if (!file.exists("GlobalCode.R")) {
  stop("GlobalCode.R not found. Make sure working directory is set to GCMSQuantitation/")
}
if (!file.exists("Code/01_MsFilesReorganiser.R")) {
  stop("Code/01_MsFilesReorganiser.R not found.")
}
if (!file.exists("Code/02_PeakDetection.R")) {
  stop("Code/02_PeakDetection.R not found.")
}
if (!file.exists("Code/03_Quantification.R")) {
  stop("Code/03_Quantification.R not found.")
}
if (!file.exists("FINEX/04_CollateStudyResults.R")) {
  stop("FINEX/04_CollateStudyResults.R not found.")
}

###########################################################################
# Dataset Discovery
###########################################################################

message("\n========================================")
message("BATCH PROCESSING: FINEX Swabbing Study")
message("========================================\n")

# Verify accepted analysis directory exists
if (!dir.exists(accepted_analysis_dir)) {
  stop("Accepted Analysis directory not found: ", accepted_analysis_dir)
}

message("Discovering datasets in: ", accepted_analysis_dir)

# Discover all subdirectories
all_folders <- list.dirs(accepted_analysis_dir, full.names = TRUE, recursive = FALSE)

# Exclude specified folders (case-insensitive)
dataset_folders <- all_folders[
  !basename(all_folders) %in% toupper(exclude_folders) &
  !basename(all_folders) %in% tolower(exclude_folders) &
  !basename(all_folders) %in% exclude_folders
]

# Get folder names for reporting
dataset_names <- basename(dataset_folders)

message("Found ", length(dataset_folders), " datasets (excluding: ", 
        paste(exclude_folders, collapse = ", "), ")")
message("\nDatasets to process:")
for (name in dataset_names) {
  message("  - ", name)
}

###########################################################################
# Dataset Validation Function
###########################################################################

validate_dataset <- function(dataset_path, dataset_name) {
  # Check for sequence log file
  log_files <- list.files(dataset_path, 
                          pattern = "Sequence.*\\.LOG$", 
                          ignore.case = TRUE,
                          full.names = FALSE)
  
  if (length(log_files) == 0) {
    return(list(valid = FALSE, 
                reason = "No sequence .LOG file found"))
  }
  
  # Check for data directory (GcDataConverterMs/ or RawData/)
  has_converter_dir <- dir.exists(file.path(dataset_path, "GcDataConverterMs"))
  has_raw_dir <- dir.exists(file.path(dataset_path, "RawData"))
  
  if (!has_converter_dir && !has_raw_dir) {
    return(list(valid = FALSE,
                reason = "No GcDataConverterMs/ or RawData/ directory found"))
  }
  
  # Check for .ms1 files in GcDataConverterMs/
  if (has_converter_dir) {
    ms1_files <- list.files(file.path(dataset_path, "GcDataConverterMs"),
                           pattern = "\\.ms1$",
                           ignore.case = TRUE)
    if (length(ms1_files) == 0) {
      return(list(valid = FALSE,
                  reason = "No .ms1 files found in GcDataConverterMs/"))
    }
  }
  
  return(list(valid = TRUE, reason = ""))
}

###########################################################################
# Dataset Processing Function
###########################################################################

process_dataset <- function(dataset_path, dataset_name) {
  message("\n========================================")
  message("Processing: ", dataset_name)
  message("========================================\n")
  
  # Validate dataset
  validation <- validate_dataset(dataset_path, dataset_name)
  if (!validation$valid) {
    message("✗ SKIPPED: ", dataset_name)
    message("  Reason: ", validation$reason)
    return(list(success = FALSE, 
                error = paste0("Validation failed: ", validation$reason),
                skipped = TRUE))
  }
  
  # Create isolated environment (inherits from .GlobalEnv so functions/packages available)
  dataset_env <- new.env(parent = .GlobalEnv)
  
  # Override DataFolder in this environment. GlobalCode.R now preserves any
  # DataFolder/study_type already set here across its own rm(list=ls()) reset
  # (see the "Preserve caller-supplied DataFolder/study_type" comment at the
  # top of GlobalCode.R) -- previously this override was silently wiped out.
  dataset_env$DataFolder <- dataset_path
  dataset_env$study_type <- "FINEX"
  
  tryCatch({
    # Source GlobalCode.R in isolated environment. GlobalCode.R's own
    # bottom-of-file chain (01 -> 02 -> 03) now uses source(..., local = TRUE),
    # which resolves to "whatever environment is currently running GlobalCode.R"
    # -- i.e. this dataset_env, since we're sourcing GlobalCode.R itself with
    # local = dataset_env below. Previously that inner chain had no local=
    # argument, which defaults to source()'s local = FALSE (.GlobalEnv) --
    # meaning it ran 01/02/03 in the WRONG environment regardless of how
    # GlobalCode.R itself was sourced, causing "object '...' not found"
    # errors for anything GlobalCode.R had just set up in dataset_env. Do NOT
    # re-source 01/02/03 explicitly here as well -- GlobalCode.R's own chain
    # already does this correctly now; doing it again would just redundantly
    # reprocess every dataset twice.
    source("GlobalCode.R", local = dataset_env)
    
    message("\n✓ SUCCESS: ", dataset_name)
    return(list(success = TRUE, error = NULL, skipped = FALSE))
    
  }, error = function(e) {
    message("\n✗ FAILED: ", dataset_name)
    message("  Error: ", conditionMessage(e))
    return(list(success = FALSE, 
                error = conditionMessage(e),
                skipped = FALSE))
  })
}

###########################################################################
# Batch Processing Loop
###########################################################################

# Initialize tracking
results_log <- data.frame(
  Dataset = character(),
  Status = character(),
  Error = character(),
  stringsAsFactors = FALSE
)

# Start timer
start_time <- Sys.time()

# Process each dataset
for (i in seq_along(dataset_folders)) {
  folder <- dataset_folders[i]
  name <- dataset_names[i]
  
  # Progress indicator
  message("\n[Progress: ", i, "/", length(dataset_folders), "]")
  
  # Process dataset
  result <- process_dataset(folder, name)
  
  # Record result
  status <- if (result$skipped) {
    "SKIPPED"
  } else if (result$success) {
    "SUCCESS"
  } else {
    "FAILED"
  }
  
  results_log <- rbind(results_log, data.frame(
    Dataset = name,
    Status = status,
    Error = ifelse(is.null(result$error), "", result$error),
    stringsAsFactors = FALSE
  ))
  
  # Garbage collection between datasets (free memory)
  gc()
}

# End timer
end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")

###########################################################################
# Summary Report
###########################################################################

message("\n========================================")
message("BATCH PROCESSING COMPLETE")
message("========================================")
message("Total datasets: ", nrow(results_log))
message("Successful: ", sum(results_log$Status == "SUCCESS"))
message("Failed: ", sum(results_log$Status == "FAILED"))
message("Skipped: ", sum(results_log$Status == "SKIPPED"))
message("Elapsed time: ", round(elapsed_time, 1), " minutes")

# Report failures
if (any(results_log$Status == "FAILED")) {
  message("\n--- FAILED DATASETS ---")
  failed <- results_log[results_log$Status == "FAILED", ]
  for (i in 1:nrow(failed)) {
    message("  ", i, ". ", failed$Dataset[i])
    message("     Error: ", failed$Error[i])
  }
}

# Report skipped
if (any(results_log$Status == "SKIPPED")) {
  message("\n--- SKIPPED DATASETS ---")
  skipped <- results_log[results_log$Status == "SKIPPED", ]
  for (i in 1:nrow(skipped)) {
    message("  ", i, ". ", skipped$Dataset[i])
    message("     Reason: ", skipped$Error[i])
  }
}

# Save log to file
log_file <- file.path(accepted_analysis_dir, "BatchProcessing_Log.csv")
write.csv(results_log, log_file, row.names = FALSE)
message("\nProcessing log saved to: ", log_file)

###########################################################################
# Collation (Always Run)
###########################################################################

message("\n========================================")
message("Running CollateStudyResults...")
message("========================================\n")

collation_start <- Sys.time()

tryCatch({
  source("FINEX/04_CollateStudyResults.R")
  collation_end <- Sys.time()
  collation_time <- difftime(collation_end, collation_start, units = "mins")
  message("\n✓ Collation complete (", round(collation_time, 1), " minutes)")
}, error = function(e) {
  message("\n✗ Collation FAILED")
  message("  Error: ", conditionMessage(e))
})

###########################################################################
# Final Summary
###########################################################################

total_time <- difftime(Sys.time(), start_time, units = "mins")

message("\n========================================")
message("ALL PROCESSING COMPLETE")
message("========================================")
message("Total time: ", round(total_time, 1), " minutes")
message("Log file: ", log_file)
message("Study results: ", file.path(accepted_analysis_dir, "FINEX_StudyResults.xlsx"))
message("\nDone.")
