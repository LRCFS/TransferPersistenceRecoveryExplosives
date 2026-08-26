# =========================================================
# FINEX Swabbing Study - Collate Results Across All Runs
# =========================================================
#
# Run this script at any time to (re)build the study-level
# master dataset from all individually processed runs.
#
# This script is incremental: it discovers whatever
# *_GCMSResults.csv files exist at that moment and combines
# them. Safe to re-run at any stage of data collection.
#
# Only requires setting study_root_dir below.
#
# Outputs:
#   {study_root_dir}/FINEX_StudyResults.csv
#   {study_root_dir}/FINEX_StudyResults.xlsx
#
# Author: <add name>
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(writexl)
})

# =========================================================
# CONFIGURATION - set the study root folder
# =========================================================
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study"

# =========================================================
# Surface code mapping
# =========================================================
surface_map <- c(

  "S"     = "Steel",
  "G"     = "Glass",
  "ABS-S" = "ABS-Smooth",
  "ABS-T" = "ABS-Textured"
)

# =========================================================
# 1. Discover all *_GCMSResults.csv files recursively
# =========================================================
results_files <- list.files(

  study_root_dir,
  pattern = "_GCMSResults\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(results_files) == 0) {
  stop("No *_GCMSResults.csv files found under: ", study_root_dir)
}

print(paste0("Found ", length(results_files), " results file(s):"))
for (f in results_files) {
  print(paste0("  ", f))
}

# =========================================================
# 2. Read and combine all results files
# =========================================================
all_data <- bind_rows(lapply(results_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$SourceFile <- f
  df
}))

print(paste0("Total rows loaded: ", nrow(all_data)))

# =========================================================
# 2b. Handle column naming differences between script versions
# =========================================================
# Older results (v1-NoNG) use "is_pa" for a single IS.
# Newer results (v2-NG) use "rdx_is_pa" and "petn_is_pa" for dual IS.
# Fill rdx_is_pa from is_pa where the newer column is missing/NA.
if ("is_pa" %in% names(all_data)) {
  if (!("rdx_is_pa" %in% names(all_data))) {
    all_data$rdx_is_pa <- all_data$is_pa
  } else {
    all_data$rdx_is_pa <- ifelse(is.na(all_data$rdx_is_pa), all_data$is_pa, all_data$rdx_is_pa)
  }
}
# For rows from older files that lack petn_is_pa, it will naturally be NA
# (which is correct -- those runs didn't use NG as a PETN IS)

# =========================================================
# 3. Filter to Sample rows only (includes NCs)
# =========================================================
samples <- all_data %>%
  filter(Type == "Sample")

print(paste0("Sample rows (including NCs): ", nrow(samples)))

if (nrow(samples) == 0) {
  stop("No Sample rows found in the results files. Check that sequences have been processed.")
}

# =========================================================
# 4. Parse SampleName into structured columns
# =========================================================
# Expected patterns:
#   "Lab18 P1 S3"       -> Lab=18, Participant=1, Surface=S, Repeat=3
#   "Lab18 P1 S NC"     -> Lab=18, Participant=1, Surface=S, NC=TRUE
#   "Lab18 P1 ABS-S4"   -> Lab=18, Participant=1, Surface=ABS-S, Repeat=4
#   "Lab14 P1 G NC"     -> Lab=14, Participant=1, Surface=G, NC=TRUE

# Trim whitespace from SampleName before parsing
samples$SampleName_trimmed <- str_trim(samples$SampleName)

# Regex to capture Lab, Participant, Surface, and Repeat/NC
sample_pattern <- "^Lab(\\d+)\\s+P(\\d+)\\s+(ABS-S|ABS-T|S|G)\\s*(NC|\\d+)$"

parsed <- str_match(samples$SampleName_trimmed, sample_pattern)

samples$Lab         <- as.integer(parsed[, 2])
samples$Participant <- as.integer(parsed[, 3])
samples$Surface     <- parsed[, 4]
samples$RepeatRaw   <- parsed[, 5]

# Determine if negative control
samples$IsNegativeControl <- !is.na(samples$RepeatRaw) & samples$RepeatRaw == "NC"

# Repeat number (NA for NCs)
samples$Repeat <- ifelse(
  samples$IsNegativeControl,
  NA_integer_,
  as.integer(samples$RepeatRaw)
)

# Map surface code to human-readable name
samples$SurfaceName <- surface_map[samples$Surface]

# Report any rows that failed to parse
n_unparsed <- sum(is.na(samples$Lab))
if (n_unparsed > 0) {
  warning(n_unparsed, " sample row(s) could not be parsed from SampleName. ",
          "These will be included with NA identifiers.")
  unparsed_names <- unique(samples$SampleName_trimmed[is.na(samples$Lab)])
  for (nm in unparsed_names) {
    warning("  Unparsed: '", nm, "'")
  }
}

# =========================================================
# 5. Select curated columns
# =========================================================

# Define desired columns (keep only those that exist in the data)
desired_cols <- c(
  # Study identifiers
  "Lab", "Participant", "Surface", "SurfaceName", "Repeat", "IsNegativeControl",
  # Source tracing
  "Date", "SampleName", "DataFile", "SourceFile",
  # IS peak areas
  "petn_is_pa", "rdx_is_pa",
  # PETN results

  "petn_pa", "petn_ratio", "petn_snr", "petn_snr_flag",
  "petn_concentration_pa", "petn_quant_method_pa", "petn_range_flag_pa",
  "petn_concentration_ratio", "petn_quant_method_ratio", "petn_range_flag_ratio",
  "petn_mass_in_sample_pa", "petn_percent_recovery_pa",
  "petn_mass_in_sample_ratio", "petn_percent_recovery_ratio",
  # PETN drift-corrected (PA method only; columns present when apply_petn_drift_correction = TRUE)
  "petn_drift_correction_factor", "petn_drift_model",
  "petn_pa_dc", "petn_concentration_pa_dc",
  "petn_quant_method_pa_dc", "petn_range_flag_pa_dc",
  "petn_mass_in_sample_pa_dc", "petn_percent_recovery_pa_dc",
  # RDX results
  "rdx_pa", "rdx_ratio", "rdx_snr", "rdx_snr_flag",
  "rdx_concentration_pa", "rdx_quant_method_pa", "rdx_range_flag_pa",
  "rdx_concentration_ratio", "rdx_quant_method_ratio", "rdx_range_flag_ratio",
  "rdx_mass_in_sample_pa", "rdx_percent_recovery_pa",
  "rdx_mass_in_sample_ratio", "rdx_percent_recovery_ratio"
)

available_cols <- intersect(desired_cols, names(samples))
missing_cols <- setdiff(desired_cols, names(samples))
if (length(missing_cols) > 0) {
  message("Note: the following columns were not found in all results files and will be omitted: ",
          paste(missing_cols, collapse = ", "))
}

master <- samples %>%
  select(all_of(available_cols)) %>%
  arrange(Lab, Participant, Surface, Repeat)

# =========================================================
# 6. Export master CSV
# =========================================================
csv_path <- file.path(study_root_dir, "FINEX_StudyResults.csv")
write.csv(master, csv_path, row.names = FALSE)
print(paste0("Master CSV written: ", csv_path))

# =========================================================
# 7. Build per-surface sheets
# =========================================================
sheet_list <- list()
sheet_list[["All Data"]] <- master

for (surf_code in names(surface_map)) {
  surf_name <- surface_map[surf_code]
  surf_data <- master %>% filter(Surface == surf_code)
  if (nrow(surf_data) > 0) {
    sheet_list[[surf_name]] <- surf_data
  }
}

# =========================================================
# 8. Build Summary sheet
# =========================================================
# Helper function to compute group statistics
compute_stats <- function(df, group_vars) {
  df %>%
    filter(!IsNegativeControl, !is.na(Lab)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_PETN       = sum(!is.na(petn_mass_in_sample_ratio)),
      PETN_Mean_ng = mean(petn_mass_in_sample_ratio, na.rm = TRUE),
      PETN_SD_ng   = sd(petn_mass_in_sample_ratio, na.rm = TRUE),
      PETN_RSD_pct = ifelse(
        PETN_Mean_ng != 0 & !is.na(PETN_Mean_ng),
        (PETN_SD_ng / PETN_Mean_ng) * 100,
        NA_real_
      ),
      n_RDX        = sum(!is.na(rdx_mass_in_sample_ratio)),
      RDX_Mean_ng  = mean(rdx_mass_in_sample_ratio, na.rm = TRUE),
      RDX_SD_ng    = sd(rdx_mass_in_sample_ratio, na.rm = TRUE),
      RDX_RSD_pct  = ifelse(
        RDX_Mean_ng != 0 & !is.na(RDX_Mean_ng),
        (RDX_SD_ng / RDX_Mean_ng) * 100,
        NA_real_
      ),
      .groups = "drop"
    )
}

# Check if mass_in_sample columns exist before computing summaries
has_petn_mass <- "petn_mass_in_sample_ratio" %in% names(master)
has_rdx_mass  <- "rdx_mass_in_sample_ratio" %in% names(master)

if (has_petn_mass || has_rdx_mass) {

  # Ensure columns exist (fill with NA if one analyte is missing)
  if (!has_petn_mass) master$petn_mass_in_sample_ratio <- NA_real_
  if (!has_rdx_mass)  master$rdx_mass_in_sample_ratio  <- NA_real_

  # Level 1: Per Participant (Lab + Participant + Surface)
  summary_participant <- compute_stats(
    master,
    c("Lab", "Participant", "Surface", "SurfaceName")
  )

  # Level 2: Per Surface (Lab + Surface, pooling participants)
  summary_surface <- compute_stats(
    master,
    c("Lab", "Surface", "SurfaceName")
  )

  # Level 3: Per Lab (pooling all surfaces and participants)
  summary_lab <- compute_stats(
    master,
    c("Lab")
  )

  # Stack the three levels with separator rows
  # Use a blank-row + header approach via a common column structure
  # (writexl doesn't support merged cells, so we use a flat table with a Level column)
  summary_participant$Level <- "By Participant"
  summary_surface$Level     <- "By Surface"
  summary_lab$Level         <- "By Lab"

  # Ensure consistent columns for binding
  all_summary_cols <- c("Level", "Lab", "Participant", "Surface", "SurfaceName",
                        "n_PETN", "PETN_Mean_ng", "PETN_SD_ng", "PETN_RSD_pct",
                        "n_RDX", "RDX_Mean_ng", "RDX_SD_ng", "RDX_RSD_pct")

  # Add missing columns as NA to each summary level
  for (col in all_summary_cols) {
    if (!(col %in% names(summary_participant))) summary_participant[[col]] <- NA
    if (!(col %in% names(summary_surface)))     summary_surface[[col]]     <- NA
    if (!(col %in% names(summary_lab)))         summary_lab[[col]]         <- NA
  }

  summary_combined <- bind_rows(
    summary_participant %>% select(all_of(all_summary_cols)),
    summary_surface     %>% select(all_of(all_summary_cols)),
    summary_lab         %>% select(all_of(all_summary_cols))
  ) %>%
    arrange(Level, Lab, Participant, Surface)

  sheet_list[["Summary"]] <- summary_combined

} else {
  warning("Neither petn_mass_in_sample_ratio nor rdx_mass_in_sample_ratio found. ",
          "Summary sheet will not be generated. ",
          "Run MetadataLinearWeighted.R with calibration data first.")
}

# =========================================================
# 9. Export Excel workbook
# =========================================================
xlsx_path <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")
write_xlsx(sheet_list, path = xlsx_path)
print(paste0("Excel workbook written: ", xlsx_path))

# =========================================================
# 10. Final summary to console
# =========================================================
n_labs <- n_distinct(master$Lab, na.rm = TRUE)
n_participants <- master %>%
  filter(!is.na(Lab)) %>%
  distinct(Lab, Participant) %>%
  nrow()
n_surfaces <- n_distinct(master$Surface, na.rm = TRUE)
n_samples <- sum(!master$IsNegativeControl, na.rm = TRUE)
n_ncs <- sum(master$IsNegativeControl, na.rm = TRUE)

print(paste0(""))
print(paste0("=== FINEX Study Collation Complete ==="))
print(paste0("  Labs:                ", n_labs))
print(paste0("  Lab-Participant sets:", n_participants))
print(paste0("  Surface types:       ", n_surfaces))
print(paste0("  Sample repeats:      ", n_samples))
print(paste0("  Negative controls:   ", n_ncs))
print(paste0("  Output CSV:  ", csv_path))
print(paste0("  Output XLSX: ", xlsx_path))
print(paste0("======================================"))
