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
# Outputs:
#   {study_root_dir}/FINEX_StudyResults.csv
#   {study_root_dir}/FINEX_StudyResults.xlsx (with conditional formatting)
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
  library(openxlsx)
})

# =========================================================
# Load parameters from GlobalCode.R
# =========================================================
# Deliberately done FIRST, before any of this script's own config below --
# GlobalCode.R unconditionally does rm(list = setdiff(ls(), ...)) internally
# (to reset stale state between dataset runs), which would silently wipe out
# any of THIS script's own variables (study_root_dir, qc_bracket_exclude,
# expected_sample_totals, etc.) if they were defined first and this script
# is ever run in a genuinely fresh session (e.g. a standalone `Rscript
# 04_CollateStudyResults.R`, or as the final step of RunAllDatasets.R in a
# fresh Rscript process with no prior interactive state) rather than an
# already-warmed-up interactive session where SampleVol/DepositMass happen
# to already exist. Confirmed by direct testing: this ordering previously
# caused "object 'global_code_path' not found" (and would have caused the
# same for every other variable below) whenever GlobalCode.R's reset
# actually fired.
global_code_path <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/GlobalCode.R"

if (!exists("SampleVol") || !exists("DepositMass")) {
  source(global_code_path)
}

# Re-affirm global_code_path in case the source() call above actually fired:
# GlobalCode.R's own rm(list=ls()), which runs INSIDE that call, wipes out
# global_code_path itself (along with everything else in this script defined
# so far) the moment it fires -- global_code_path is used again below (to
# locate Code/InjectionAcceptance.R relative to GlobalCode.R), so it must be
# guaranteed to still exist at that point regardless of whether the source()
# call above ran or was skipped.
global_code_path <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/GlobalCode.R"

# =========================================================
# CONFIGURATION - set the study root folder
# =========================================================
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

# =========================================================
# QC Bracket Acceptance Thresholds
# =========================================================
qc_6ng_bias_limit  <- 20   # +/- 20% for 6ng QC (bias-based)
# 0.2ng QC uses flag-based criteria, computed fresh (from raw SNR +
# concentration) inside Code/InjectionAcceptance.R's assign_qc_brackets(),
# identically for BOTH PETN and RDX (previously RDX only had a 2-tier
# PASS/FAIL flag -- an incidental implementation gap, not a deliberate
# asymmetry -- see Diagnostics/Improved_QC_Flowchart.dot legend NOTE4):
#   SENSITIVITY_CHECK_PASS: SNR >= 10 AND concentration > 0
#   SENSITIVITY_CHECK_WARN: 3 <= SNR < 10 AND concentration > 0
#   SENSITIVITY_CHECK_FAIL: SNR < 3 OR concentration <= 0

# Manual override: exclude specific QC injections from being used as a
# bracket for ANY sample's PASS/FAIL evaluation, even though the automatic
# "failed to inject" check in assign_qc_brackets() (IS SNR-flag based) didn't
# catch them. Use this when a QC's own analyte-specific signal is obviously
# wrong despite the IS looking fine -- e.g. the known case below: a 6ng QC
# where PETN's PA read 0 but the IS wasn't flagged low, so it silently
# bracketed (and FAILed) every neighbouring sample on a bogus PETN reading
# until this override was added. The excluded QC is NOT removed from the
# dataset, its own PASS/FAIL evaluation, or any calibration/drift fit --
# ONLY from bracket duty for other samples (the next QC on each side is
# substituted automatically, same mechanism as a genuine non-injection).
# Format: list("<substring to match against SourceFile/dataset path>" =
# c(<sequence Line numbers>)). Matching is by substring, NOT bare Line
# number, since Line numbers restart at 1 per dataset and are not unique
# study-wide. See assign_qc_brackets()'s header comment in
# Code/InjectionAcceptance.R for the full mechanism.
qc_bracket_exclude <- list(
  "Outstanding samples 4" = c(39)  # 6ng QC, PETN PA = 0, IS not flagged low
)

# =========================================================
# Study-wide expected sample totals per surface (Added July 2026, corrected
# July 29, 2026 -- the original figures accidentally included NC counts
# folded into the surface totals. Samples and NCs now have separate
# expected totals.)
# =========================================================
# Used ONLY for the `Complete %` column in the SampleSummary sheet, which
# tracks study-wide completion progress (samples PASS+PASS* so far, out of
# the full study target for that surface) -- NOT the pass rate among
# samples processed so far. The `Total` column in that sheet still shows
# the actual number of samples currently collated/processed.
expected_sample_totals <- c(
  "Steel"        = 330,
  "ABS-Smooth"   = 336,
  "Glass"        = 141,
  "ABS-Textured" = 141
)

# Study-wide expected NC total (separate from the sample totals above).
expected_nc_total <- 161



# =========================================================
# Load shared injection-acceptance logic (Sections A-D of
# Diagnostics/Improved_QC_Flowchart.dot). This is the SINGLE
# implementation of blank/QC/sample/NC acceptance logic, also sourced by
# the ASTRA study's "Design of Experiments/pilot_analysis.R" -- do not
# re-duplicate any of this logic locally in either script.
# =========================================================
source(file.path(dirname(global_code_path), "Code", "InjectionAcceptance.R"))

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

# Exclude backup files (stored in Results/Backup/ folders)
results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]

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
# 2b. Discover and load calibration statistics files
# =========================================================
cal_stats_files <- list.files(
  study_root_dir,
  pattern = "_CalibrationStats\\.xlsx$",
  full.names = TRUE,
  recursive = TRUE
)

# Exclude backup files
cal_stats_files <- cal_stats_files[!grepl("/Backup/", cal_stats_files, fixed = TRUE)]

print(paste0("Found ", length(cal_stats_files), " calibration stats file(s)."))

if (length(cal_stats_files) > 0) {
  
  all_cal_stats <- bind_rows(lapply(cal_stats_files, function(f) {
    # Extract dataset name from file path
    # Path: .../Lab10-1/Results/Lab10-1_CalibrationStats.xlsx
    # Extract: "Lab10-1"
    dataset_name <- basename(dirname(dirname(f)))
    
    # Read the XLSX file
    df <- readxl::read_excel(f, sheet = 1)
    
    # Add identifier column
    df$Dataset <- dataset_name
    
    df
  }))
  
  print(paste0("Total calibration stats rows loaded: ", nrow(all_cal_stats)))
  
  # Add date information from results CSV
  if (nrow(all_data) > 0) {
    # Build a lookup: Dataset -> earliest Date
    date_lookup <- all_data %>%
      group_by(SourceFile) %>%
      summarise(Date = min(Date, na.rm = TRUE), .groups = "drop") %>%
      mutate(Dataset = basename(dirname(dirname(SourceFile)))) %>%
      select(Dataset, Date)
    
    # Join date info to cal_stats
    all_cal_stats <- all_cal_stats %>%
      left_join(date_lookup, by = "Dataset")
    
    # Sort by Date, then Dataset, then CalibrationSet
    all_cal_stats <- all_cal_stats %>%
      arrange(Date, Dataset, CalibrationSet)
    
    print(paste0("CalSummary: sorted by date (", 
                 min(all_cal_stats$Date, na.rm = TRUE), 
                 " to ",
                 max(all_cal_stats$Date, na.rm = TRUE), 
                 ")"))
  }
  
  # Reorganize columns: identifiers -> PETN -> RDX -> IS
  id_cols <- c("Dataset", "Date", "CalibrationSet")
  
  # All PETN columns (in logical order)
  petn_cols <- grep("^(N_Standards|N_Excluded_LOD|R2|AdjR2).*PETN", 
                    names(all_cal_stats), value = TRUE)
  
  # All RDX columns (same order)
  rdx_cols <- grep("^(N_Standards|N_Excluded_LOD|R2|AdjR2).*RDX", 
                   names(all_cal_stats), value = TRUE)
  
  # IS columns
  is_cols <- grep("^RDX_IS_RSD", names(all_cal_stats), value = TRUE)
  
  # Reorder
  all_cal_stats <- all_cal_stats %>%
    select(all_of(c(id_cols, petn_cols, rdx_cols, is_cols)))
  
  print(paste0("CalSummary: ", nrow(all_cal_stats), " rows, ", 
               ncol(all_cal_stats), " columns"))
  
} else {
  warning("No calibration statistics files found -- CalSummary sheet will be empty.")
  all_cal_stats <- data.frame()
}

# =========================================================
# 2c. Handle column naming differences between script versions
# =========================================================
# For backward compatibility with older results files:
if ("is_pa" %in% names(all_data) && !("rdx_is_pa" %in% names(all_data))) {
  all_data$rdx_is_pa <- all_data$is_pa
}

# Determine PETN quantification method from columns present
if (!("petn_quant_method" %in% names(all_data))) {
  all_data$petn_quant_method <- ifelse(
    "petn_concentration_ratio" %in% names(all_data) && 
    !is.na(all_data$petn_concentration_ratio),
    "15N-RDX_Ratio",
    ifelse("petn_concentration_pa" %in% names(all_data), "Peak_Area", "Unknown")
  )
}

# Map old suffixed columns to new unified names for PETN
if (!("petn_concentration" %in% names(all_data))) {
  all_data$petn_concentration <- ifelse(
    all_data$petn_quant_method == "15N-RDX_Ratio",
    all_data$petn_concentration_ratio,
    all_data$petn_concentration_pa
  )
}

# Drift-corrected PETN concentration
if (!("petn_concentration_dc" %in% names(all_data))) {
  if ("petn_concentration_pa_dc" %in% names(all_data)) {
    all_data$petn_concentration_dc <- all_data$petn_concentration_pa_dc
  } else if ("petn_concentration_ratio_dc" %in% names(all_data)) {
    all_data$petn_concentration_dc <- all_data$petn_concentration_ratio_dc
  }
}

# Map RDX columns (always uses ratio)
if (!("rdx_concentration" %in% names(all_data)) && "rdx_concentration_ratio" %in% names(all_data)) {
  all_data$rdx_concentration <- all_data$rdx_concentration_ratio
}

# =========================================================
# 3. Extract QC bracket information (BEFORE filtering)
# =========================================================
# Determine bias column names for PETN and RDX
# PETN: use drift-corrected bias if available, else uncorrected
petn_bias_col <- if ("petn_percent_bias_dc" %in% names(all_data)) {
  "petn_percent_bias_dc"
} else if ("petn_percent_bias_pa_dc" %in% names(all_data)) {
  "petn_percent_bias_pa_dc"
} else if ("petn_percent_bias" %in% names(all_data)) {
  "petn_percent_bias"
} else {
  NA_character_
}

# RDX: use uncorrected (IS ratio already handles drift)
rdx_bias_col <- if ("rdx_percent_bias" %in% names(all_data)) {
  "rdx_percent_bias"
} else if ("rdx_percent_bias_dc" %in% names(all_data)) {
  "rdx_percent_bias_dc"
} else {
  NA_character_
}

print(paste0("PETN bias column for QC brackets: ", petn_bias_col))
print(paste0("RDX bias column for QC brackets: ", rdx_bias_col))

# Build QC lookup per source file
# 6ng QCs: evaluated on bias (drift-corrected for PETN, uncorrected for RDX)
# 0.2ng QCs: evaluated by reading pre-computed flags from results files
#
# assign_qc_brackets() (and evaluate_blanks(), compute_injection_acceptance(),
# derive_nc_result()) now live in Code/InjectionAcceptance.R, shared with the
# ASTRA study's pilot_analysis.R -- see that file for the full implementation
# of Sections A-D of Diagnostics/Improved_QC_Flowchart.dot. Do not re-add a
# local copy of any of this logic here.

# Assign blank status (Section A) BEFORE QC brackets, since assign_qc_brackets()
# also attaches the nearest-bracketing-blank status to every sample row.
all_data <- evaluate_blanks(all_data)
print("Blank status assigned.")

n_blank_contam <- sum(all_data$blank_status == "Contaminated", na.rm = TRUE)
n_blank_trace  <- sum(all_data$blank_status == "Trace detected", na.rm = TRUE)
n_blank_clean  <- sum(all_data$blank_status == "Clean", na.rm = TRUE)
print(paste0("Blank evaluation: Contaminated=", n_blank_contam,
             " Trace detected=", n_blank_trace, " Clean=", n_blank_clean))

# Assign QC brackets (Section B)
all_data <- assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col,
                                qc_6ng_bias_limit,
                                qc_bracket_exclude = qc_bracket_exclude)

print("QC bracket flags assigned.")

# =========================================================
# 4. Filter to Sample rows only (includes NCs)
# =========================================================
samples <- all_data %>%
  filter(Type == "Sample")

print(paste0("Sample rows (including NCs): ", nrow(samples)))

if (nrow(samples) == 0) {
  stop("No Sample rows found in the results files. Check that sequences have been processed.")
}

# =========================================================
# 5. Parse SampleName into structured columns
# =========================================================
samples$SampleName_trimmed <- str_trim(samples$SampleName)

# Surface code accepts both the canonical hyphenated form ("ABS-S"/"ABS-T")
# and a bare-space variant ("ABS S"/"ABS T") -- found via UnparsedSampleNames.csv
# that several labs (10, 17, 20, 33) typed the space form, silently excluding
# ~21 real sample/NC rows from the whole study (see red-team review Section D /
# CONTEXT.md). The captured value is normalised back to the hyphenated form
# immediately below, since surface_map and later sheet-filtering both key on
# the literal strings "ABS-S"/"ABS-T".
sample_pattern <- "^Lab(\\d+)\\s*P(\\d+)\\s*(ABS[- ]S|ABS[- ]T|S|G)\\s*(NC|\\d+)$"

parsed <- str_match(samples$SampleName_trimmed, sample_pattern)

samples$Lab         <- as.integer(parsed[, 2])
samples$Participant <- as.integer(parsed[, 3])
samples$Surface     <- parsed[, 4]
samples$Surface     <- gsub("ABS T", "ABS-T", samples$Surface, fixed = TRUE)
samples$Surface     <- gsub("ABS S", "ABS-S", samples$Surface, fixed = TRUE)
samples$RepeatRaw   <- parsed[, 5]

samples$IsNegativeControl <- !is.na(samples$RepeatRaw) & samples$RepeatRaw == "NC"

samples$Repeat <- ifelse(
  samples$IsNegativeControl,
  NA_integer_,
  as.integer(samples$RepeatRaw)
)

# Identify unparsed rows BEFORE filtering them out below -- this check must
# run first: the filter on the next lines removes exactly the rows this is
# trying to detect, which previously made this check permanently dead code
# (n_unparsed was always 0, and the "included with NA identifiers" comment
# was inaccurate -- unparsed rows were actually being silently dropped with
# no warning at all).
n_unparsed <- sum(is.na(samples$Lab) | is.na(samples$Participant) | is.na(samples$Surface))
if (n_unparsed > 0) {
  unparsed_rows <- is.na(samples$Lab) | is.na(samples$Participant) | is.na(samples$Surface)
  unparsed_names <- unique(samples$SampleName_trimmed[unparsed_rows])
  warning(n_unparsed, " sample row(s) could not be parsed from SampleName and ",
          "will be EXCLUDED from the study results (Lab/Participant/Surface required). ",
          "See UnparsedSampleNames.csv in study_root_dir for the full list.")
  for (nm in unparsed_names) {
    warning("  Unparsed: '", nm, "'")
  }
  # Persist to a file as well as the console, so unparsed samples aren't only
  # visible in a scrollback warning log that's easy to miss in a long batch run.
  write.csv(
    data.frame(SampleName = unparsed_names),
    file.path(study_root_dir, "UnparsedSampleNames.csv"),
    row.names = FALSE
  )
}

# Filter out samples without Lab/Participant/Surface
samples <- samples %>%
  filter(!is.na(Lab) & !is.na(Participant) & !is.na(Surface))

samples$SurfaceName <- surface_map[samples$Surface]

# =========================================================
# 6. Calculate mass and recovery
# =========================================================
# Mass in sample (ng) = concentration (ng/uL) * SampleVol (uL)
# Note: concentration already accounts for IS dilution (divided by Dilution in 03_Quantification)
samples$petn_mass <- samples$petn_concentration * SampleVol
samples$rdx_mass  <- samples$rdx_concentration * SampleVol

# Drift-corrected mass
if ("petn_concentration_dc" %in% names(samples)) {
  samples$petn_mass_dc <- samples$petn_concentration_dc * SampleVol
}

# Recovery (%) = (mass / deposit) / (extraction * filtration) * 100
samples$petn_recovery <- (samples$petn_mass / DepositMass) / 
                          (petn_extraction_efficiency * petn_filtration_efficiency) * 100
samples$rdx_recovery  <- (samples$rdx_mass / DepositMass) / 
                          (rdx_extraction_efficiency * rdx_filtration_efficiency) * 100

# Drift-corrected recovery
if ("petn_mass_dc" %in% names(samples)) {
  samples$petn_recovery_dc <- (samples$petn_mass_dc / DepositMass) / 
                               (petn_extraction_efficiency * petn_filtration_efficiency) * 100
}

# Drift-correction uncertainty, propagated one final (linear) step from
# concentration to recovery (item 19). recovery_dc is a pure linear
# rescaling of concentration_dc (recovery_dc = concentration_dc * K for a
# constant K), so SE(recovery_dc) = SE(concentration_dc) * K -- the same
# scaling formula, applied to the SE column instead of the point estimate.
# Backward-compatible: NA/absent for any dataset processed before this
# column existed in 03_Quantification.R's output (old *_GCMSResults.xlsx
# files won't have it until reprocessed).
if ("petn_concentration_dc_se" %in% names(samples)) {
  samples$petn_recovery_dc_se <- (samples$petn_concentration_dc_se * SampleVol / DepositMass) /
                                   (petn_extraction_efficiency * petn_filtration_efficiency) * 100
}

# RDX uses uncorrected concentration only (no drift correction applied)
# Fallback to rdx_concentration_dc for backward compatibility with old datasets
rdx_conc_col <- if ("rdx_concentration" %in% names(samples)) {
  "rdx_concentration"
} else if ("rdx_concentration_dc" %in% names(samples)) {
  "rdx_concentration_dc"  # Old format fallback
} else {
  NA_character_
}

if (!is.na(rdx_conc_col)) {
  samples$rdx_mass <- samples[[rdx_conc_col]] * SampleVol
  samples$rdx_recovery <- (samples$rdx_mass / DepositMass) / 
                          (rdx_extraction_efficiency * rdx_filtration_efficiency) * 100
}

# =========================================================
# 7. Compute analysis_accepted column (Sections C+D)
# =========================================================
# compute_injection_acceptance() (Code/InjectionAcceptance.R) implements
# the unified Sample/NC acceptance logic -- per-analyte carryover/tier/
# bracket evaluation, first-failure-only row combination in fixed order
# IS -> PETN -> RDX. Samples and NCs are evaluated by this SAME call --
# there is no more structural difference between them (the old 0.2ng-
# bracket-FAIL override, the only thing that used to distinguish NC
# handling, has been removed entirely; see Diagnostics/Improved_QC_Flowchart.dot).
samples <- compute_injection_acceptance(samples)

# Print summary of acceptance. NCs are now excluded from these counts by
# checking IsNegativeControl directly (previously they carried a literal
# "NC" sentinel value in analysis_accepted -- they now get a real
# PASS/PASS*/FAIL like everyone else, since the acceptance logic no
# longer special-cases them).
n_pass <- sum(samples$analysis_accepted == "PASS" & !samples$IsNegativeControl, na.rm = TRUE)
n_pass_star <- sum(samples$analysis_accepted == "PASS*" & !samples$IsNegativeControl, na.rm = TRUE)
n_fail <- sum(grepl("^FAIL:", samples$analysis_accepted) & !samples$IsNegativeControl, na.rm = TRUE)
n_nc <- sum(samples$IsNegativeControl, na.rm = TRUE)
print(paste0("Analysis acceptance: PASS=", n_pass, " PASS*=", n_pass_star,
             " FAIL=", n_fail, " NC=", n_nc))

# =========================================================
# 7b. NC evaluation: derived from the SAME unified computation above
# =========================================================
# nc_analysis_accepted is simply samples$analysis_accepted for NC rows,
# aliased under its NC-sheet column name for backward compatibility.
# nc_result is derived from the per-analyte result tiers (petn_result_tier/
# rdx_result_tier) computed above, mapping Quantifiable/Trace/Negative to
# the NC contamination vocabulary (contaminated/trace detected/clean),
# forced to "Not evaluated (analysis failed)" whenever nc_analysis_accepted
# is a FAIL -- see derive_nc_result() in Code/InjectionAcceptance.R.
nc_rows <- samples %>% filter(IsNegativeControl)
if (nrow(nc_rows) > 0) {
  nc_rows$nc_analysis_accepted <- nc_rows$analysis_accepted
  nc_rows$nc_result <- derive_nc_result(nc_rows$petn_result_tier, nc_rows$rdx_result_tier,
                                         nc_rows$nc_analysis_accepted)
}

# =========================================================
# 7c. HANDLE REANALYSIS (same physical sample analysed in more than one
#     dataset folder)
#
# If a sample failed QC, it may be re-run in a later run (e.g. this study's
# "Steel 3"/"Steel 4"/"Outstanding samples" folders exist specifically to
# hold reanalysed samples). Since `all_data` is built from every
# */Results/*_GCMSResults.csv discovered under study_root_dir (Section 1),
# the exact same physical sample -- identified by its parsed (Lab,
# Participant, Surface, Repeat) identity -- can appear more than once.
# Confirmed as a REAL, currently-occurring scenario (not theoretical): e.g.
# "Lab6 P1 S4" appears in both "Steel 2" (20260730, FAIL) and "Outstanding
# samples" (20260811); "Lab17 P1 S4"/"Lab17 P2 S1" each have both a FAIL row
# (Steel-1, 20260626) and a genuine PASS* row (Outstanding samples,
# 20260811) sitting side by side with no prior deduplication.
#
# Without deduplication this silently: (a) leaves BOTH attempts in `master`,
# double-counting the sample in SampleSummary/per-surface sheets/plots/
# statistics, and (b) gives no indication of which row is the "real"
# result when the two attempts disagree.
#
# Resolution (ported from the ASTRA study's `select_best_attempt()` in
# "Design of Experiments/pilot_analysis.R", including that script's own
# August 11, 2026 tie-break fix -- see that repo's CONTEXT.md "select_best_
# attempt() Now Prefers the EARLIEST Accepted Attempt" session): keep
# exactly one row per physical sample -- preferring an accepted attempt
# (PASS/PASS* for samples, nc_analysis_accepted %in% c("PASS","PASS*") for
# NCs) over a failed one, and breaking ties by the EARLIEST GC-MS analysis
# Date among attempts of equal quality. "Earliest wins" (not "most recent
# wins" or file-discovery order) was deliberately chosen to match ASTRA's
# own corrected behaviour: a later reanalysis exists only because an
# earlier attempt failed for SOME reason (often analyte- or QC-vial-
# specific), not because later data is inherently more trustworthy -- and
# "most recent" was shown there to systematically bias RDX (which has no
# drift correction in this pipeline) toward more-drifted later runs, for
# reasons that only ever concerned PETN.
# `N_Attempts`/`MultipleAttempts` columns retain visibility that a re-run
# happened even though only the selected attempt is used downstream.
# =========================================================
select_best_attempt <- function(df, quality_ok, key_cols) {
  if (nrow(df) == 0) {
    df$N_Attempts <- integer(0)
    df$MultipleAttempts <- logical(0)
    return(df)
  }
  df$..quality_ok <- quality_ok
  df %>%
    group_by(across(all_of(key_cols))) %>%
    mutate(N_Attempts = n()) %>%
    ungroup() %>%
    arrange(across(all_of(key_cols)), desc(..quality_ok), Date) %>%
    group_by(across(all_of(key_cols))) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(MultipleAttempts = N_Attempts > 1) %>%
    select(-..quality_ok)
}

report_multiple_attempts <- function(df, label, key_cols) {
  if (!any(df$MultipleAttempts, na.rm = TRUE)) return(invisible(NULL))
  dup_ids <- df %>%
    filter(MultipleAttempts) %>%
    select(all_of(key_cols)) %>%
    distinct()
  cat(nrow(dup_ids), label, "had multiple analysis attempts -- using earliest",
      "accepted attempt (or earliest overall if none were accepted):\n")
  for (i in seq_len(nrow(dup_ids))) {
    cat("  ", paste(dup_ids[i, ], collapse = " / "), "\n")
  }
  cat("\n")
}

# --- Deduplicate NCs (same physical NC swabbed more than once) ---
if (nrow(nc_rows) > 0) {
  nc_rows <- select_best_attempt(
    nc_rows,
    quality_ok = nc_rows$nc_analysis_accepted %in% c("PASS", "PASS*"),
    key_cols = c("Lab", "Participant", "Surface")
  )
  report_multiple_attempts(nc_rows, "negative control(s)", c("Lab", "Participant", "Surface"))

  n_nc_pass <- sum(nc_rows$nc_analysis_accepted == "PASS", na.rm = TRUE)
  n_nc_pass_star <- sum(nc_rows$nc_analysis_accepted == "PASS*", na.rm = TRUE)
  n_nc_fail <- sum(grepl("^FAIL:", nc_rows$nc_analysis_accepted), na.rm = TRUE)
  print(paste0("NC analysis acceptance (Stage 1 -- is the run trustworthy?): ",
               "PASS=", n_nc_pass, " PASS*=", n_nc_pass_star, " FAIL=", n_nc_fail))

  n_nc_valid <- n_nc_pass + n_nc_pass_star
  n_nc_invalid <- n_nc_fail
  n_nc_negative <- sum(nc_rows$nc_result == "Negative (clean)", na.rm = TRUE)
  n_nc_positive <- sum(grepl("^Positive:", nc_rows$nc_result), na.rm = TRUE)
  n_nc_not_evaluated <- sum(nc_rows$nc_result == "Not evaluated (analysis failed)", na.rm = TRUE)
  print(paste0("NC result (Stage 2 -- contamination outcome, valid runs only): ",
               "Negative=", n_nc_negative, " Positive=", n_nc_positive,
               " Not evaluated=", n_nc_not_evaluated))
} else {
  n_nc_pass <- 0
  n_nc_pass_star <- 0
  n_nc_fail <- 0
  n_nc_valid <- 0
  n_nc_invalid <- 0
  n_nc_negative <- 0
  n_nc_positive <- 0
  n_nc_not_evaluated <- 0
}

# =========================================================
# 8. Select curated columns
# =========================================================
# --- Sample columns (excludes NCs) ---
desired_cols <- c(
  # Study identifiers (A-E)
  "Lab", "Participant", "Surface", "SurfaceName", "Repeat",
  
  # Overall acceptance (F-G) -- moved forward for quick reference
  "analysis_accepted",
  "Outcome",
  
  # IS (15N-RDX) injection-validity audit column -- basis for any
  # "IS not detected"/"IS abnormally low" failure in analysis_accepted
  "rdx_is_pa_flag",
  
  # === ALL PETN COLUMNS ===
  # PETN flags
  "petn_snr_flag",
  # PETN confirmatory (qualifier) ion evidence -- added Aug 2026, see
  # IA_QUALIFIER_IONS in Code/InjectionAcceptance.R. Not used for
  # quantification; included for audit visibility behind any "PETN
  # confirmatory ion not detected" failure in analysis_accepted.
  "petn_qual76_snr_flag",
  # PETN recovery
  "petn_recovery_dc", "petn_recovery_dc_se",
  # PETN QC brackets: 6ng first, then 0.2ng
  "petn_qc_6ng_pre", "petn_qc_6ng_post",
  "petn_qc_02ng_pre", "petn_qc_02ng_post",
  # PETN QC inherited flags: 6ng first, then 0.2ng
  "petn_qc_6ng_pre_inherited", "petn_qc_6ng_post_inherited",
  "petn_qc_02ng_pre_inherited", "petn_qc_02ng_post_inherited",
  
  # === ALL RDX COLUMNS ===
  # RDX flags
  "rdx_snr_flag",
  # RDX confirmatory (qualifier) ion evidence -- added Aug 2026, see
  # IA_QUALIFIER_IONS in Code/InjectionAcceptance.R. Both are required
  # (not just one) -- see behind any "RDX confirmatory ion not detected"
  # failure in analysis_accepted.
  "rdx_qual46_snr_flag", "rdx_qual75_snr_flag",
  # RDX recovery
  "rdx_recovery",
  # RDX QC brackets: 6ng first, then 0.2ng
  "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
  "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
  # RDX QC inherited flags: 6ng first, then 0.2ng
  "rdx_qc_6ng_pre_inherited", "rdx_qc_6ng_post_inherited",
  "rdx_qc_02ng_pre_inherited", "rdx_qc_02ng_post_inherited",
  
  # Carryover-risk evidence behind a "peak may be due to carryover" FAIL
  # (Section A/C -- see evaluate_blanks()/assign_blank_brackets() in
  # Code/InjectionAcceptance.R)
  "petn_blank_bracket", "rdx_blank_bracket",
  
  # Reanalysis audit columns (Section 7c) -- N_Attempts is always >= 1;
  # MultipleAttempts flags rows where an earlier/later attempt existed but
  # was superseded (see select_best_attempt()).
  "N_Attempts", "MultipleAttempts",
  
  # Source tracing
  "Date", "SampleName", "DataFile", "SourceFile"
)

# --- NC columns (separate sheet, no QC brackets) ---
desired_cols_nc <- c(
  # Study identifiers (A-D)
  "Lab", "Participant", "Surface", "SurfaceName",
  
  # STAGE 1 (E): was this injection/run analytically trustworthy?
  # Same PASS / PASS* / FAIL: <reason> vocabulary as the Samples sheet's
  # `analysis_accepted` -- both are now literally the SAME computation,
  # see compute_injection_acceptance() in Code/InjectionAcceptance.R. An
  # NC can only be interpreted as a real negative/positive result if this
  # is PASS or PASS*.
  "nc_analysis_accepted",
  
  # STAGE 2 (F): contamination outcome -- Negative (clean) / Positive: ...
  # ONLY meaningful when nc_analysis_accepted is PASS/PASS*; forced to
  # "Not evaluated (analysis failed)" otherwise. See derive_nc_result()
  # in Code/InjectionAcceptance.R.
  "nc_result",
  
  # === ALL PETN COLUMNS (raw evidence behind nc_result) ===
  "petn_snr_flag", "petn_ph",
  "petn_recovery_dc", "petn_recovery_dc_se",
  # PETN confirmatory (qualifier) ion evidence (added Aug 2026)
  "petn_qual76_snr_flag",
  
  # === ALL RDX COLUMNS (raw evidence behind nc_result) ===
  "rdx_snr_flag", "rdx_ph",
  "rdx_recovery",
  # RDX confirmatory (qualifier) ion evidence (added Aug 2026)
  "rdx_qual46_snr_flag", "rdx_qual75_snr_flag",
  
  # Carryover-risk evidence behind a "peak may be due to carryover" FAIL
  # (Section A/C -- see evaluate_blanks()/assign_blank_brackets())
  "petn_blank_bracket", "rdx_blank_bracket",
  
  # Reanalysis audit columns (Section 7c) -- see select_best_attempt().
  "N_Attempts", "MultipleAttempts",
  
  # QC/IS audit columns -- raw evidence behind nc_analysis_accepted
  # (IS first, then 6ng QC brackets, then 0.2ng QC brackets, matching the
  # order those checks are evaluated in compute_injection_acceptance())
  "rdx_is_snr_flag", "rdx_is_pa_flag",
  "petn_qc_6ng_pre", "petn_qc_6ng_post",
  "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
  "petn_qc_02ng_pre", "petn_qc_02ng_post",
  "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
  
  # Source tracing
  "Date", "SampleName", "DataFile", "SourceFile"
)

# --- Build samples-only master (no NCs) ---
samples_only <- samples %>% filter(!IsNegativeControl)

# Deduplicate samples analysed more than once (see Section 7c above for the
# full rationale). Applied here (not earlier) so quality_ok can reference
# the already-computed analysis_accepted.
samples_only <- select_best_attempt(
  samples_only,
  quality_ok = samples_only$analysis_accepted %in% c("PASS", "PASS*"),
  key_cols = c("Lab", "Participant", "Surface", "Repeat")
)
report_multiple_attempts(samples_only, "sample(s)", c("Lab", "Participant", "Surface", "Repeat"))

n_pass <- sum(samples_only$analysis_accepted == "PASS", na.rm = TRUE)
n_pass_star <- sum(samples_only$analysis_accepted == "PASS*", na.rm = TRUE)
n_fail <- sum(grepl("^FAIL:", samples_only$analysis_accepted), na.rm = TRUE)
print(paste0("Sample acceptance after deduplication: PASS=", n_pass,
             " PASS*=", n_pass_star, " FAIL=", n_fail))

available_cols <- intersect(desired_cols, names(samples_only))
missing_cols <- setdiff(desired_cols, names(samples_only))
if (length(missing_cols) > 0) {
  message("Note: the following columns were not found and will be omitted: ",
          paste(missing_cols, collapse = ", "))
}

master <- samples_only %>%
  select(all_of(available_cols)) %>%
  arrange(Lab, Participant, Surface, Repeat)

# --- Build NC master ---
if (nrow(nc_rows) > 0) {
  available_cols_nc <- intersect(desired_cols_nc, names(nc_rows))
  master_ncs <- nc_rows %>%
    select(all_of(available_cols_nc)) %>%
    arrange(Lab, Participant, Surface)
} else {
  master_ncs <- data.frame()
}

# =========================================================
# 9. Export master CSV
# =========================================================
csv_path <- file.path(study_root_dir, "FINEX_StudyResults.csv")
write.csv(master, csv_path, row.names = FALSE)
print(paste0("Master CSV written: ", csv_path))

# =========================================================
# 10. Build per-surface sheets
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

# Add Negative Controls sheet
if (nrow(master_ncs) > 0) {
  sheet_list[["Negative Controls"]] <- master_ncs
}

# =========================================================
# 11. Build Summary sheet
# =========================================================
compute_stats <- function(df, group_vars) {
  # Use recovery columns (drift-corrected preferred)
  petn_rec_col <- if ("petn_recovery_dc" %in% names(df)) "petn_recovery_dc" else
                  if ("petn_recovery" %in% names(df)) "petn_recovery" else NA_character_
  rdx_rec_col  <- if ("rdx_recovery" %in% names(df)) "rdx_recovery" else
                  if ("rdx_recovery_dc" %in% names(df)) "rdx_recovery_dc" else NA_character_
  
  # If no recovery columns, return empty

  if (is.na(petn_rec_col) && is.na(rdx_rec_col)) return(data.frame())
  
  # Ensure columns exist
  if (is.na(petn_rec_col)) { df$petn_recovery_placeholder <- NA_real_; petn_rec_col <- "petn_recovery_placeholder" }
  if (is.na(rdx_rec_col))  { df$rdx_recovery_placeholder <- NA_real_;  rdx_rec_col  <- "rdx_recovery_placeholder" }
  
  df %>%
    filter(!is.na(Lab)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_total = n(),
      n_PETN = sum(!is.na(!!sym(petn_rec_col))),
      PETN_Mean_recovery = mean(!!sym(petn_rec_col), na.rm = TRUE),
      PETN_SD_recovery   = sd(!!sym(petn_rec_col), na.rm = TRUE),
      PETN_RSD_pct = ifelse(
        PETN_Mean_recovery != 0 & !is.na(PETN_Mean_recovery),
        (PETN_SD_recovery / PETN_Mean_recovery) * 100,
        NA_real_
      ),
      n_RDX = sum(!is.na(!!sym(rdx_rec_col))),
      RDX_Mean_recovery = mean(!!sym(rdx_rec_col), na.rm = TRUE),
      RDX_SD_recovery   = sd(!!sym(rdx_rec_col), na.rm = TRUE),
      RDX_RSD_pct = ifelse(
        RDX_Mean_recovery != 0 & !is.na(RDX_Mean_recovery),
        (RDX_SD_recovery / RDX_Mean_recovery) * 100,
        NA_real_
      ),
      .groups = "drop"
    )
}

has_petn_rec <- "petn_recovery_dc" %in% names(master) || "petn_recovery" %in% names(master)
has_rdx_rec  <- "rdx_recovery" %in% names(master)

if (has_petn_rec || has_rdx_rec) {
  
  # Ensure columns exist for compute_stats
  if (!("petn_recovery_dc" %in% names(master)) && !("petn_recovery" %in% names(master))) {
    master$petn_recovery <- NA_real_
  }
  if (!("rdx_recovery" %in% names(master))) {
    master$rdx_recovery <- NA_real_
  }

  # Restrict Summary-sheet statistics to accepted samples only
  # (Outcome == "Complete", equivalent to analysis_accepted %in% c("PASS","PASS*")
  # -- see InjectionAcceptance.R's compute_injection_acceptance(), which sets
  # both together). Previously compute_stats() ran on ALL of `master`
  # (including "FAIL:" rows -- e.g. carryover, failed QC bracket, IS
  # abnormally low), which could pull physically impossible values (a
  # documented case: RDX recovery = 164.8% from a low-IS injection, see
  # InjectionAcceptance.R:636) into the reported Summary-sheet means/RSDs,
  # while the box plots further down this script already correctly
  # restricted to Outcome == "Complete". This makes both consistent.
  n_master_total    <- sum(!is.na(master$Lab))
  master_for_summary <- master %>% filter(Outcome == "Complete")
  n_master_accepted <- sum(!is.na(master_for_summary$Lab))
  print(paste0("Summary sheet statistics: ", n_master_accepted, " of ", n_master_total,
               " sample(s) included (Outcome == 'Complete'); ",
               n_master_total - n_master_accepted, " excluded (FAILed QC/acceptance)."))

  # Zero-substitute non-detects (NA recovery on an accepted sample) so the
  # Summary sheet uses the SAME non-detect convention as the box plots below
  # (recovery_data$petn_rec/rdx_rec, replace_na(..., 0)) -- "NA and zero both
  # mean 'not recovered'". Previously the Summary sheet used mean(x, na.rm =
  # TRUE), which silently EXCLUDED non-detects instead (inflating the
  # apparent mean recovery relative to the box plots' 0%-substituted view).
  # Only applied for an analyte with real recovery data (has_petn_rec /
  # has_rdx_rec) -- NOT for the all-NA placeholder column created above when
  # an analyte has no recovery data at all, which must stay NA, not become 0.
  if (has_petn_rec) {
    petn_col_for_summary <- if ("petn_recovery_dc" %in% names(master_for_summary)) "petn_recovery_dc" else "petn_recovery"
    master_for_summary[[petn_col_for_summary]] <- replace_na(master_for_summary[[petn_col_for_summary]], 0)
  }
  if (has_rdx_rec) {
    rdx_col_for_summary <- if ("rdx_recovery" %in% names(master_for_summary)) "rdx_recovery" else "rdx_recovery_dc"
    master_for_summary[[rdx_col_for_summary]] <- replace_na(master_for_summary[[rdx_col_for_summary]], 0)
  }

  summary_participant <- compute_stats(
    master_for_summary,
    c("Lab", "Participant", "Surface", "SurfaceName")
  )

  summary_surface <- compute_stats(
    master_for_summary,
    c("Lab", "Surface", "SurfaceName")
  )

  summary_lab <- compute_stats(
    master_for_summary,
    c("Lab")
  )

  summary_participant$Level <- "By Participant"
  summary_surface$Level     <- "By Surface"
  summary_lab$Level         <- "By Lab"

  all_summary_cols <- c("Level", "Lab", "Participant", "Surface", "SurfaceName",
                        "n_total", "n_PETN", "PETN_Mean_recovery", "PETN_SD_recovery", "PETN_RSD_pct",
                        "n_RDX", "RDX_Mean_recovery", "RDX_SD_recovery", "RDX_RSD_pct")

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
  warning("No recovery columns found. ",
          "Summary sheet will not be generated. ",
          "Run 03_Quantification.R with calibration data first.")
}

# Add CalSummary sheet (if calibration stats were found)
if (exists("all_cal_stats") && nrow(all_cal_stats) > 0) {
  sheet_list[["CalSummary"]] <- all_cal_stats
  print(paste0("  CalSummary sheet added to workbook (", 
               nrow(all_cal_stats), " rows)."))
}

# =========================================================
# 11b. Build SampleSummary sheet
# =========================================================
if (nrow(master) > 0) {
  
  # --- Surface rows (samples only) ---
  # `Complete %` tracks progress against the FULL STUDY TARGET for that
  # surface (expected_sample_totals), NOT the pass rate among samples
  # processed so far. `Total` still shows the actual count processed.
  # `N Labs` / `N Participants` (Added July 2026) show how many distinct
  # labs, and distinct lab:participant combinations, contributed data for
  # that surface -- participants are counted as (Lab, Participant) pairs
  # since Participant numbers (1/2) repeat across labs.
  sample_summary <- master %>%
    group_by(SurfaceName) %>%
    summarise(
      Total = n(),
      `N Labs` = n_distinct(Lab),
      `N Participants` = n_distinct(paste(Lab, Participant)),
      PASS = sum(analysis_accepted == "PASS", na.rm = TRUE),
      `PASS*` = sum(analysis_accepted == "PASS*", na.rm = TRUE),
      FAIL = sum(grepl("^FAIL:", analysis_accepted), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Ensure every surface in the study design appears, even if no samples
  # for it have been processed yet (shown as 0/expected = 0.0%).
  missing_surfaces <- setdiff(names(expected_sample_totals), sample_summary$SurfaceName)
  if (length(missing_surfaces) > 0) {
    sample_summary <- bind_rows(
      sample_summary,
      data.frame(
        SurfaceName = missing_surfaces,
        Total = 0L, `N Labs` = 0L, `N Participants` = 0L,
        PASS = 0L, `PASS*` = 0L, FAIL = 0L,
        check.names = FALSE
      )
    )
  }
  
  # Order surface rows to match expected_sample_totals (Steel, ABS-Smooth,
  # Glass, ABS-Textured), then compute Complete % against the fixed target.
  sample_summary <- sample_summary %>%
    mutate(
      `Complete %` = round(100 * (PASS + `PASS*`) / expected_sample_totals[SurfaceName], 1)
    ) %>%
    arrange(match(SurfaceName, names(expected_sample_totals)))
  
  # Rename first column for clarity
  names(sample_summary)[1] <- "Surface"
  
  # --- NC row ---
  # NCs are folded into the same table as a single "NC" row, using
  # Stage 1 (nc_analysis_accepted) crossed with Stage 2 (nc_result):
  #   PASS  = Stage 1 PASS/PASS* AND Stage 2 "Negative (clean)"
  #   PASS* = Stage 1 PASS/PASS* AND Stage 2 "Positive: ..." (contamination/
  #           trace detected) -- the injection itself was reliable
  #   FAIL  = Stage 1 FAIL (failed IS detection or a failed bracketing QC),
  #           regardless of Stage 2 -- this is a QC/run-reliability failure,
  #           not a positive result
  # See compute_injection_acceptance() / derive_nc_result() in Code/InjectionAcceptance.R.
  if (nrow(master_ncs) > 0 && "nc_analysis_accepted" %in% names(master_ncs)) {
    nc_stage1_ok <- master_ncs$nc_analysis_accepted %in% c("PASS", "PASS*")
    nc_valid_negative <- sum(nc_stage1_ok & master_ncs$nc_result == "Negative (clean)", na.rm = TRUE)
    nc_valid_positive <- sum(nc_stage1_ok & grepl("^Positive:", master_ncs$nc_result), na.rm = TRUE)
    nc_invalid <- sum(!nc_stage1_ok, na.rm = TRUE)
    nc_total <- nrow(master_ncs)
    nc_n_labs <- n_distinct(master_ncs$Lab)
    nc_n_participants <- n_distinct(paste(master_ncs$Lab, master_ncs$Participant))
  } else {
    nc_valid_negative <- 0
    nc_valid_positive <- 0
    nc_invalid <- 0
    nc_total <- 0
    nc_n_labs <- 0
    nc_n_participants <- 0
  }
  
  sample_summary <- bind_rows(
    sample_summary,
    data.frame(
      Surface = "NC",
      Total = nc_total,
      `N Labs` = nc_n_labs,
      `N Participants` = nc_n_participants,
      PASS = nc_valid_negative,
      `PASS*` = nc_valid_positive,
      FAIL = nc_invalid,
      # Complete % = progress against the fixed study-wide expected NC
      # total (expected_nc_total), consistent with the per-surface rows --
      # NOT nc_total (samples processed so far). Updated July 29, 2026.
      `Complete %` = round(100 * (nc_valid_negative + nc_valid_positive) / expected_nc_total, 1),
      check.names = FALSE
    )
  )
  
  # --- TOTAL row (samples only -- surface rows, excludes the NC row above) ---
  # Complete % = progress against the full study target across ALL surfaces
  # (sum of expected_sample_totals), matching the per-surface rows above.
  # N Labs / N Participants computed directly from `master` (not summed
  # from the per-surface rows) to avoid double-counting labs/participants
  # that contributed data to more than one surface.
  surface_rows <- sample_summary[sample_summary$Surface != "NC", ]
  grand_expected_total <- sum(expected_sample_totals)
  sample_summary <- bind_rows(
    sample_summary,
    data.frame(
      Surface = "TOTAL",
      Total = sum(surface_rows$Total),
      `N Labs` = n_distinct(master$Lab),
      `N Participants` = n_distinct(paste(master$Lab, master$Participant)),
      PASS = sum(surface_rows$PASS),
      `PASS*` = sum(surface_rows$`PASS*`),
      FAIL = sum(surface_rows$FAIL),
      `Complete %` = round(100 * sum(surface_rows$PASS + surface_rows$`PASS*`) /
                           grand_expected_total, 1),
      check.names = FALSE
    )
  )
  
  # --- "Successfully Run" -- a single headline count, not a full breakdown ---
  # = sample PASS + sample PASS* + analytically-valid NCs (PASS + PASS* of
  # the NC row above). Only the Total column is populated; PASS/PASS*/FAIL/
  # Complete % don't apply to a single combined count.
  total_row <- sample_summary[sample_summary$Surface == "TOTAL", ]
  successfully_run_count <- total_row$PASS + total_row$`PASS*` + nc_valid_negative + nc_valid_positive
  
  sample_summary <- bind_rows(
    sample_summary,
    data.frame(
      Surface = "Successfully Run",
      Total = successfully_run_count,
      `N Labs` = NA_integer_,
      `N Participants` = NA_integer_,
      PASS = NA_integer_,
      `PASS*` = NA_integer_,
      FAIL = NA_integer_,
      `Complete %` = NA_real_,
      check.names = FALSE
    )
  )
  
  sheet_list[["SampleSummary"]] <- sample_summary
  
  print(paste0("  SampleSummary sheet added to workbook (", 
               nrow(sample_summary), " rows)."))
} else {
  print("  Skipping SampleSummary sheet (no sample data).")
}

# =========================================================
# 12. Export Excel workbook with conditional formatting
# =========================================================
xlsx_path <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")

# Define styles
style_green  <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
style_amber  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C5700")
style_red    <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
style_grey   <- createStyle(fgFill = "#D9D9D9", fontColour = "#000000")
style_blue   <- createStyle(bgFill = "#BDD7EE", fontColour = "#1F4E79")
style_yellow <- createStyle(bgFill = "#FFFF00", fontColour = "#000000")
style_header <- createStyle(textDecoration = "Bold", border = "Bottom",
                            borderColour = "#000000", fgFill = "#4472C4",
                            fontColour = "#FFFFFF", halign = "center")

# Function to apply conditional formatting to a sheet
apply_formatting <- function(wb, sheet_name, df) {
  
  n_rows <- nrow(df)
  if (n_rows == 0) return()
  
  col_names <- names(df)
  data_rows <- 2:(n_rows + 1)  # +1 for header row
  
  # --- SNR flag columns: Quantifiable=green, Below_LOQ=amber, Below_LOD=red ---
  # (includes confirmatory/qualifier ion snr_flag columns, added Aug 2026)
  snr_cols <- intersect(c("petn_snr_flag", "rdx_snr_flag",
                           "petn_qual76_snr_flag", "rdx_qual46_snr_flag", "rdx_qual75_snr_flag"), col_names)
  for (col in snr_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Quantifiable", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Below_LOQ", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Below_LOD", style = style_red)
  }
  
  # --- QC bracket columns: PASS=green, WARN=amber, FAIL=red ---
  qc_cols <- intersect(c("petn_qc_6ng_pre", "petn_qc_6ng_post",
                          "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
                          "petn_qc_02ng_pre", "petn_qc_02ng_post",
                          "rdx_qc_02ng_pre", "rdx_qc_02ng_post"), col_names)
  for (col in qc_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "PASS", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "WARN", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "FAIL", style = style_red)
  }
  
  # --- Blank bracket columns: Clean=green, Trace=amber, Contaminated=red ---
  # (Section A/C carryover-risk evidence -- see evaluate_blanks() /
  # assign_blank_brackets() in Code/InjectionAcceptance.R)
  blank_bracket_cols <- intersect(c("petn_blank_bracket", "rdx_blank_bracket"), col_names)
  for (col in blank_bracket_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Clean", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Trace", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Contaminated", style = style_red)
  }
  
  # --- Analysis accepted column: PASS=green, PASS*=amber, FAIL:=red ---
  if ("analysis_accepted" %in% col_names) {
    col_idx <- which(col_names == "analysis_accepted")
    # Order matters: more specific rules first
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "FAIL:", style = style_red)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "PASS*", style = style_amber)
    # PASS last (since PASS* and PASS both contain "PASS")
    # Use expression-based rule for exact PASS
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "expression",
                          rule = paste0('EXACT(', int2col(col_idx), '2,"PASS")'),
                          style = style_green)
  }
  
  # --- NC Stage 1 (nc_analysis_accepted): same styling as analysis_accepted ---
  if ("nc_analysis_accepted" %in% col_names) {
    col_idx <- which(col_names == "nc_analysis_accepted")
    # Order matters: more specific rules first
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "FAIL:", style = style_red)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "PASS*", style = style_amber)
    # PASS last (since PASS* and PASS both contain "PASS")
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "expression",
                          rule = paste0('EXACT(', int2col(col_idx), '2,"PASS")'),
                          style = style_green)
  }
  
  # --- NC Stage 2 (nc_result): Negative=green, Positive trace=amber, ---
  # --- Positive contaminated=red, Not evaluated (Stage 1 failed)=grey ---
  if ("nc_result" %in% col_names) {
    col_idx <- which(col_names == "nc_result")
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Not evaluated", style = style_grey)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "contaminated", style = style_red)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "trace detected", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Negative", style = style_green)
  }
  
  # --- Apply NA grey formatting to QC bracket columns ---
  # openxlsx does not have a native "is blank/NA" conditional format,
  # so we write NA cells with direct styling after data write
  for (col in qc_cols) {
    col_idx <- which(col_names == col)
    na_rows <- which(is.na(df[[col]])) + 1  # +1 for header
    if (length(na_rows) > 0) {
      addStyle(wb, sheet_name, style = style_grey, rows = na_rows, cols = col_idx,
               gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # --- Inherited flag columns: TRUE = amber ---
  inherited_cols <- intersect(c("petn_qc_6ng_pre_inherited", "petn_qc_6ng_post_inherited",
                                "rdx_qc_6ng_pre_inherited", "rdx_qc_6ng_post_inherited",
                                "petn_qc_02ng_pre_inherited", "petn_qc_02ng_post_inherited",
                                "rdx_qc_02ng_pre_inherited", "rdx_qc_02ng_post_inherited"), col_names)
  for (col in inherited_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "TRUE", style = style_amber)
  }
  
  # --- MultipleAttempts column: TRUE = amber ("heads up, reanalysed") ---
  # (Section 7c -- see select_best_attempt() above in this script. Not a
  # pass/fail colour -- just flags that this row's value came from
  # choosing between >1 analysis attempt.)
  if ("MultipleAttempts" %in% col_names) {
    col_idx <- which(col_names == "MultipleAttempts")
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "TRUE", style = style_amber)
  }
  
  # --- Outcome column: Complete=green, Reanalyse=red, lower conc=amber, higher conc=yellow ---
  if ("Outcome" %in% col_names) {
    col_idx <- which(col_names == "Outcome")
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Complete", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "lower conc", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "higher conc", style = style_yellow)
    # "Reanalyse" without qualifier (QC failure only) — must come last as it's less specific
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "expression",
                          rule = paste0('EXACT(', int2col(col_idx), '2,"Reanalyse")'),
                          style = style_red)
  }
}

# Create workbook
wb <- createWorkbook()

for (sheet_name in names(sheet_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheet_list[[sheet_name]], headerStyle = style_header,
            keepNA = TRUE, na.string = "NA")
  
  # Auto-size columns
  setColWidths(wb, sheet_name, cols = seq_along(names(sheet_list[[sheet_name]])),
               widths = "auto")
  
  # Apply conditional formatting (skip Summary and CalSummary sheets which have different columns)
  if (!(sheet_name %in% c("Summary", "CalSummary"))) {
    apply_formatting(wb, sheet_name, sheet_list[[sheet_name]])
  }
}

# Save workbook
saveWorkbook(wb, xlsx_path, overwrite = TRUE)
print(paste0("Excel workbook written (with formatting): ", xlsx_path))

# =========================================================
# 13. Generate Box Plots
# =========================================================

library(ggplot2)

# Create output directories
plots_dir <- file.path(study_root_dir, "Plots")
plots_overall_dir <- file.path(plots_dir, "Overall")
dir.create(plots_overall_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 13a. TIC "Other Significant Peak" Log (added Aug 2026, see CONTEXT.md
# "Blank Other-Significant-Peak Detection" session; extended to cover
# ALL row types -- Cal/QC/Sample/Blank -- same session, "Recurring Peak
# Prevalence Across Injection Types")
# =========================================================
# Purely diagnostic/trend information -- explicitly does NOT gate any
# PASS/FAIL/PASS* outcome anywhere. Blanks specifically also get their
# own non-gating blank_other_peak_flag via Code/InjectionAcceptance.R's
# evaluate_blanks() (see Diagnostics/Improved_QC_Flowchart.dot Section
# A, A2/A_OTHER/INSTR_LOG) -- that stays exactly as-is, untouched here.
# This section is a BROADER exploratory extension covering every row
# type, not just Blanks, to answer "how prevalent is a given recurring
# peak across the whole study, and is it specific to blanks (e.g.
# carryover) or does it show up regardless of what's actually
# injected (e.g. septum/liner degradation, column bleed)?" -- since
# find_other_tic_peaks() (Code/ModPeaks.R) already runs identically for
# every injection type (Cal/QC/Sample/Blank) in Code/02_PeakDetection.R,
# no new extraction is needed here, only broader consumption.
#
# Two levels of output:
#   1. A flat, LONG-format log -- one row per significant peak
#      occurrence (not one row per injection -- a single injection can
#      have more than one significant peak, up to
#      tic_other_peak_max_report, and all of them are included here,
#      not just the strongest), tagged with its Type.
#   2. A recurring-peak CLUSTER summary -- peaks across the WHOLE study
#      grouped by RT proximity (sequential single-linkage grouping:
#      consecutive sorted RTs join the same cluster if the gap between
#      them is <= tic_other_peak_cluster_tolerance), with a per-Type
#      occurrence breakdown so prevalence-by-injection-type is visible
#      directly in the summary, not just the overall count.
# Backward-compatible: if no tic_other_peak_rtN columns exist at all
# (dataset(s) not yet reprocessed since this feature was added), this
# section is skipped entirely with a message, not an error. The
# numbered columns actually present are discovered dynamically (regex
# on column names), so this also degrades gracefully if different
# datasets were processed with different tic_other_peak_max_report
# values.

rt_col_names_all <- grep("^tic_other_peak_rt[0-9]+$", names(all_data), value = TRUE)
peak_indices_all <- sort(as.integer(sub("^tic_other_peak_rt", "", rt_col_names_all)))

if (length(peak_indices_all) == 0) {
  print("TIC other-peak log: no tic_other_peak_rtN columns found (dataset(s) not yet reprocessed with the numbered-peak export) -- skipping.")
} else {

  # Build the long-format log across EVERY row type (Cal/QC/Sample/
  # Blank/ResponseCheck/etc), one row per detected peak occurrence,
  # across every numbered slot that has a non-NA value.
  peak_log <- bind_rows(lapply(peak_indices_all, function(j) {
    rt_col     <- paste0("tic_other_peak_rt", j)
    height_col <- paste0("tic_other_peak_height", j)
    if (!all(c(rt_col, height_col) %in% names(all_data))) {
      return(NULL)
    }
    all_data %>%
      filter(!is.na(.data[[rt_col]])) %>%
      mutate(Dataset = basename(dirname(dirname(SourceFile))),
             rt     = .data[[rt_col]],
             height = .data[[height_col]]) %>%
      select(Dataset, any_of(c("Type", "Date", "Line", "SampleName", "DataFile")),
             rt, height)
  }))

  if (nrow(peak_log) == 0) {
    print("TIC other-peak log: no individual peak occurrences found in the numbered columns; nothing to log.")
  } else {

    n_flagged_total <- all_data %>%
      filter(if_any(all_of(rt_col_names_all), ~ !is.na(.))) %>%
      nrow()
    type_breakdown_flagged <- if ("Type" %in% names(all_data)) {
      all_data %>%
        filter(if_any(all_of(rt_col_names_all), ~ !is.na(.))) %>%
        count(Type) %>%
        arrange(desc(n))
    } else {
      NULL
    }

    print(paste0("TIC other-peak log (ALL injection types, non-gating): ",
                 n_flagged_total, " of ", nrow(all_data),
                 " total injections show a significant unexplained TIC peak"))
    if (!is.null(type_breakdown_flagged)) {
      print(paste0("  By type: ", paste(sprintf("%s=%d", type_breakdown_flagged$Type, type_breakdown_flagged$n), collapse = ", ")))
    }
    # Blank-specific figure retained for continuity with the flowchart-
    # tied evaluate_blanks()/blank_other_peak_flag metric above.
    if ("blank_other_peak_flag" %in% names(all_data)) {
      n_blank_total_ipl <- sum(all_data$Type == "Blank", na.rm = TRUE)
      n_blank_other_peak <- sum(all_data$blank_other_peak_flag, na.rm = TRUE)
      print(paste0("  Of which Blanks (matches evaluate_blanks()'s own flag): ",
                   n_blank_other_peak, " of ", n_blank_total_ipl))
    }

    peak_log <- peak_log %>% arrange(rt)

    # Clustering by RT proximity (redesigned Aug 2026, Stage 4 of the TIC
    # deinterleaving fix -- see CONTEXT.md "TIC Deinterleaving Fix" session).
    #
    # The ORIGINAL design (sequential single-linkage: start a new cluster
    # only when the gap to the previous sorted RT exceeds the tolerance)
    # was correct in principle but broke down once the underlying signal
    # was fixed and peaks became far more numerous/densely-packed: with
    # hundreds of individual peak occurrences now spread across a busy
    # region, most CONSECUTIVE gaps fall under even a generous tolerance
    # purely by density, so single-linkage chains together many genuinely
    # DIFFERENT recurring RTs into one artificially wide "cluster" (e.g.
    # 182-261s, 78s wide, produced by the old 15s tolerance) -- this
    # obscures which specific RT is actually recurring.
    #
    # Direct investigation of the real (corrected) peak log confirmed the
    # underlying peaks are NOT a continuous smear: within that same
    # 182-261s span, individual recurring RTs are tightly reproducible
    # (e.g. one dominant spike at RT=227.2s +/- 0.7s SD, n=338, seen
    # consistently across 8 different datasets -- a genuine, highly
    # reproducible recurring feature, not noise), superimposed on a
    # sparser scatter of other, less-frequent peaks nearby. Single-linkage
    # chaining was merging the real 227s spike together with everything
    # else in that 78-second span, when it should have been reported on
    # its own.
    #
    # FIX: cluster membership now requires BOTH (a) gap to the previous
    # sorted RT <= tolerance, AND (b) adding the point would not make the
    # cluster's total span (max - min RT so far) exceed the tolerance --
    # i.e. a bounded-diameter (complete-linkage-like) rule instead of
    # unbounded single-linkage chaining. This keeps a single genuinely-
    # recurring peak's repeats together (its RT jitter is tiny relative to
    # the tolerance) while preventing runaway chaining across a busy
    # region.
    #
    # Tolerance also re-derived empirically from this same investigation:
    # cluster_tolerance = 5s (was 15s) -- comfortably wider than the
    # observed single-spike SD (~0.7-0.8s, i.e. >6 SD margin) while much
    # tighter than the old value, which was wide enough to bridge entirely
    # distinct nearby recurring RTs (e.g. spikes ~4s apart) into one blob.
    cluster_tolerance <- if (exists("tic_other_peak_cluster_tolerance")) {
      tic_other_peak_cluster_tolerance
    } else {
      5  # fallback default if GlobalCode.R wasn't sourced this run
    }

    cluster_id <- integer(nrow(peak_log))
    cluster_id[1] <- 1L
    current_cluster <- 1L
    cluster_start_rt <- peak_log$rt[1]
    # NOTE: seq(2, nrow(peak_log)) is NOT safe when nrow(peak_log) == 1
    # -- seq(2, 1) counts DOWNWARD and returns c(2, 1), not an empty
    # vector, which would index out of bounds. Guarded explicitly.
    if (nrow(peak_log) > 1) {
      for (k in 2:nrow(peak_log)) {
        gap <- peak_log$rt[k] - peak_log$rt[k - 1]
        span_if_added <- peak_log$rt[k] - cluster_start_rt
        if (gap > cluster_tolerance || span_if_added > cluster_tolerance) {
          current_cluster <- current_cluster + 1L
          cluster_start_rt <- peak_log$rt[k]
        }
        cluster_id[k] <- current_cluster
      }
    }
    peak_log$ClusterID <- cluster_id

    # ---- Output 1: flat long-format log (every occurrence, every type) ----
    peak_log_export <- peak_log %>% arrange(across(any_of(c("Date", "Line"))))
    peak_log_path <- file.path(study_root_dir, "TIC_OtherPeak_Log.csv")
    write.csv(peak_log_export, peak_log_path, row.names = FALSE)
    print(paste0("-> TIC_OtherPeak_Log.csv (", nrow(peak_log_export),
                 " peak occurrence(s) across ", n_flagged_total, " flagged injection(s), all types): ",
                 peak_log_path))

    # ---- Output 2: recurring-peak cluster summary, with a per-Type
    # occurrence breakdown so prevalence-by-injection-type is visible
    # directly (e.g. "is this blank-only carryover, or does it show up
    # in Cal/QC/Sample injections too, regardless of what's injected?"). ----
    has_type <- "Type" %in% names(peak_log)
    type_counts_per_cluster <- if (has_type) {
      peak_log %>%
        count(ClusterID, Type) %>%
        tidyr::pivot_wider(names_from = Type, values_from = n, values_fill = 0,
                            names_prefix = "n_type_")
    } else {
      NULL
    }

    cluster_summary <- peak_log %>%
      group_by(ClusterID) %>%
      summarise(
        n_occurrences = n(),
        rt_mean       = mean(rt),
        rt_min        = min(rt),
        rt_max        = max(rt),
        n_datasets    = n_distinct(Dataset),
        datasets      = paste(sort(unique(Dataset)), collapse = "; "),
        mean_height   = mean(height),
        max_height    = max(height),
        first_date    = if ("Date" %in% names(peak_log)) suppressWarnings(min(Date, na.rm = TRUE)) else NA_character_,
        last_date     = if ("Date" %in% names(peak_log)) suppressWarnings(max(Date, na.rm = TRUE)) else NA_character_,
        .groups = "drop"
      ) %>%
      arrange(desc(n_occurrences), rt_mean)

    if (!is.null(type_counts_per_cluster)) {
      cluster_summary <- cluster_summary %>% left_join(type_counts_per_cluster, by = "ClusterID")
    }

    cluster_summary_path <- file.path(study_root_dir, "TIC_OtherPeak_RecurringPeaks.csv")
    write.csv(cluster_summary, cluster_summary_path, row.names = FALSE)
    print(paste0("-> TIC_OtherPeak_RecurringPeaks.csv (", nrow(cluster_summary),
                 " distinct RT cluster(s), tolerance +/-", cluster_tolerance, "s): ",
                 cluster_summary_path))

    recurring_threshold <- 2  # >=2 occurrences of the same RT cluster
    n_recurring_clusters <- sum(cluster_summary$n_occurrences >= recurring_threshold)
    if (n_recurring_clusters > 0) {
      print(paste0("NOTICE: ", n_recurring_clusters,
                    " recurring peak cluster(s) found (same approximate RT seen >=",
                    recurring_threshold, " times) -- see TIC_OtherPeak_RecurringPeaks.csv"))
    } else {
      print("-> No recurring peak clusters (every flagged peak so far is a one-off, distinct RT).")
    }

    # ---- Plot 1: RT vs time, faceted by recurring-peak cluster,
    # coloured by injection Type -- directly shows whether a cluster is
    # specific to one injection type (e.g. Blank-only carryover) or
    # shows up across Cal/QC/Sample regardless of what's injected. ----
    if ("Date" %in% names(peak_log)) {
      plot_data_rt <- peak_log %>%
        mutate(DateParsed = suppressWarnings(as.Date(Date)))

      if (!all(is.na(plot_data_rt$DateParsed))) {
        p_rt <- ggplot(plot_data_rt, aes(x = DateParsed, y = rt))
        if (has_type) {
          p_rt <- p_rt + geom_point(aes(colour = Type, size = height), alpha = 0.8)
        } else {
          p_rt <- p_rt + geom_point(aes(size = height), alpha = 0.8)
        }
        p_rt <- p_rt +
          facet_wrap(~ ClusterID, scales = "free_x",
                     labeller = labeller(ClusterID = function(x) paste0("Cluster ", x))) +
          labs(
            title = "TIC Other-Peak Log -- Peak Position Over Time, by Injection Type",
            subtitle = paste0(nrow(peak_log), " peak occurrence(s) across all injection types; ",
                               "faceted by recurring-peak cluster (RT within +/-", cluster_tolerance, "s); ",
                               "point size = baseline-corrected peak height"),
            x = "Date", y = "Retention time (s)",
            colour = "Injection type", size = "Height"
          ) +
          theme_bw() +
          theme(legend.position = "right")

        ggsave(
          file.path(plots_overall_dir, "Overall_TIC_OtherPeak_RTvsTime.png"),
          plot = p_rt, width = 12, height = 6, dpi = 300
        )
        print("-> Overall_TIC_OtherPeak_RTvsTime.png")
      }
    }

    # ---- Plot 2: peak-height magnitude trend over chronological order,
    # coloured by Type (retained from the Blanks-only design, now
    # showing magnitude across all injection types) ----
    plot_data_height <- peak_log_export %>%
      mutate(EventIndex = row_number())

    p_instr <- ggplot(plot_data_height, aes(x = EventIndex, y = height))
    if (has_type) {
      p_instr <- p_instr + geom_point(aes(colour = Type), size = 2.5)
    } else {
      p_instr <- p_instr + geom_point(colour = "steelblue", size = 2.5)
    }
    p_instr <- p_instr +
      labs(
        title = "TIC Other-Peak Log -- Unexplained TIC Peak Magnitude",
        subtitle = paste0(nrow(peak_log_export), " peak occurrence(s), all injection types (diagnostic/trend-only, does not affect PASS/FAIL)"),
        x = "Flagged peak occurrence (chronological order)",
        y = "Baseline-corrected TIC peak height"
      ) +
      theme_bw() +
      theme(legend.position = "right")

    if (exists("tic_other_peak_min_height")) {
      p_instr <- p_instr +
        geom_hline(yintercept = tic_other_peak_min_height, linetype = "dashed", colour = "grey50")
    }

    ggsave(
      file.path(plots_overall_dir, "Overall_TIC_OtherPeak_SNRTrend.png"),
      plot = p_instr, width = 10, height = 6, dpi = 300
    )
    print("-> Overall_TIC_OtherPeak_SNRTrend.png")
  }
}

# Prepare data: use drift-corrected recovery if available
# Note: master already excludes NCs (they are in master_ncs)
# Filter to Outcome == "Complete" to exclude samples needing reanalysis

# Count total samples before filtering
n_total_samples <- master %>% filter(!is.na(Lab)) %>% nrow()

# Filter to only QC-passed samples (Outcome == "Complete")
recovery_data <- master %>%
  filter(!is.na(Lab)) %>%
  filter(Outcome == "Complete") %>%
  select(Lab, Participant, Surface, SurfaceName, Outcome,
         any_of(c("petn_recovery_dc", "rdx_recovery", "petn_recovery", "rdx_recovery_dc")))

# Count samples included in plots
n_plotted_samples <- nrow(recovery_data)
n_excluded_samples <- n_total_samples - n_plotted_samples

# Report filtering
print(paste0(""))
print(paste0("=== Box Plot Data Filtering ==="))
print(paste0("  Total samples (excl. NCs):       ", n_total_samples))
print(paste0("  Samples with Outcome='Complete': ", n_plotted_samples))
print(paste0("  Samples excluded from plots:     ", n_excluded_samples, 
             " (", round(100 * n_excluded_samples / n_total_samples, 1), "%)"))
print(paste0("================================"))
print(paste0(""))

# Determine which recovery columns to use
if ("petn_recovery_dc" %in% names(recovery_data)) {
  recovery_data$petn_rec <- recovery_data$petn_recovery_dc
} else if ("petn_recovery" %in% names(recovery_data)) {
  recovery_data$petn_rec <- recovery_data$petn_recovery
} else {
  recovery_data$petn_rec <- NA_real_
}

if ("rdx_recovery" %in% names(recovery_data)) {
  recovery_data$rdx_rec <- recovery_data$rdx_recovery
} else if ("rdx_recovery_dc" %in% names(recovery_data)) {
  recovery_data$rdx_rec <- recovery_data$rdx_recovery_dc
} else {
  recovery_data$rdx_rec <- NA_real_
}

# Replace NA with zero (NA and zero both mean "not recovered")
n_petn_na <- sum(is.na(recovery_data$petn_rec))
n_rdx_na <- sum(is.na(recovery_data$rdx_rec))

recovery_data <- recovery_data %>%
  mutate(
    petn_rec = replace_na(petn_rec, 0),
    rdx_rec = replace_na(rdx_rec, 0)
  )

if (n_petn_na > 0 || n_rdx_na > 0) {
  print(paste0("  Samples with NA recovery replaced with 0%:"))
  print(paste0("    PETN: ", n_petn_na, " samples"))
  print(paste0("    RDX:  ", n_rdx_na, " samples"))
  print(paste0(""))
}

# Fixed surface order for all plots (high → low typical recovery)
fixed_surface_order <- c("Glass", "Steel", "ABS-Smooth", "ABS-Textured")

# Function to create box plot with mean diamond
create_boxplot <- function(data, x_var, y_var, x_label, y_label, title, 
                           x_order = NULL) {
  
  # Compute sample size per group
  sample_counts <- data %>%
    group_by(!!sym(x_var)) %>%
    summarise(n = n(), .groups = "drop")
  
  # Calculate y-position for labels (above maximum data value)
  y_range <- range(data[[y_var]], na.rm = TRUE)
  y_offset <- 0.05 * diff(y_range)  # 5% of data range
  label_y <- y_range[2] + y_offset
  
  # Base plot
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_boxplot(outlier.shape = 1, outlier.size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
                 fill = "red", color = "black") +
    labs(x = x_label, y = y_label, title = title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
  
  # Add sample size labels above boxes
  p <- p + geom_text(
    data = sample_counts,
    aes(x = !!sym(x_var), y = label_y, label = paste0("n=", n)),
    size = 3,
    vjust = 0,  # vjust=0 aligns text bottom with y position (label sits above)
    color = "black",
    inherit.aes = FALSE
  )
  
  # Apply x-axis ordering if specified
  if (!is.null(x_order)) {
    p <- p + scale_x_discrete(limits = x_order)
  }
  
  # Extend y-axis upward to accommodate labels
  p <- p + coord_cartesian(ylim = c(y_range[1], label_y + y_offset))
  
  return(p)
}

# --- Overall Study Plots ---
# Filter to surfaces present in data (preserves fixed order, ignores gaps)
surfaces_present_petn <- fixed_surface_order[fixed_surface_order %in% 
                          unique(recovery_data$SurfaceName[!is.na(recovery_data$petn_rec)])]

# Overall PETN plot
if (length(surfaces_present_petn) > 0) {
  p_overall_petn <- create_boxplot(
    data = recovery_data %>% filter(!is.na(petn_rec)),
    x_var = "SurfaceName",
    y_var = "petn_rec",
    x_label = "Surface",
    y_label = "PETN Recovery (%)",
    title = "PETN Recovery by Surface (All Labs)",
    x_order = surfaces_present_petn
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_Recovery.png"),
    p_overall_petn,
    width = 6, height = 5, dpi = 300
  )
}

# Overall RDX plot
# Filter to surfaces present in data (preserves fixed order, ignores gaps)
surfaces_present_rdx <- fixed_surface_order[fixed_surface_order %in% 
                          unique(recovery_data$SurfaceName[!is.na(recovery_data$rdx_rec)])]

if (length(surfaces_present_rdx) > 0) {
  p_overall_rdx <- create_boxplot(
    data = recovery_data %>% filter(!is.na(rdx_rec)),
    x_var = "SurfaceName",
    y_var = "rdx_rec",
    x_label = "Surface",
    y_label = "RDX Recovery (%)",
    title = "RDX Recovery by Surface (All Labs)",
    x_order = surfaces_present_rdx
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_RDX_Recovery.png"),
    p_overall_rdx,
    width = 6, height = 5, dpi = 300
  )
}

print("Overall study plots generated.")

# --- Overall Bar Charts by Participant (Added July 2026, error bars +
# label format updated July 2026) ---
# One grouped bar chart per analyte: x-axis = each participant (e.g.
# "Lab 6 (1)"), one bar per surface per participant (dodged), coloured by
# surface. Bar height = mean recovery across that participant's replicates
# for that surface, with error bars showing the standard error of the mean
# (SEM = SD / sqrt(n)) (recovery_data already has NA -> 0 and is restricted
# to Outcome == "Complete" samples, matching the box plots above).
surface_colors <- c(
  "Glass"        = "#1b9e77",
  "Steel"        = "#d95f02",
  "ABS-Smooth"   = "#7570b3",
  "ABS-Textured" = "#e7298a"
)

participant_bar_data <- recovery_data %>%
  filter(!is.na(Lab) & !is.na(Participant)) %>%
  mutate(ParticipantLabel = sprintf("Lab %d (%d)", Lab, Participant)) %>%
  group_by(Lab, Participant, ParticipantLabel, SurfaceName) %>%
  summarise(
    petn_mean = mean(petn_rec, na.rm = TRUE),
    petn_sd   = sd(petn_rec, na.rm = TRUE),
    petn_n    = sum(!is.na(petn_rec)),
    rdx_mean  = mean(rdx_rec, na.rm = TRUE),
    rdx_sd    = sd(rdx_rec, na.rm = TRUE),
    rdx_n     = sum(!is.na(rdx_rec)),
    .groups = "drop"
  ) %>%
  mutate(
    petn_sem = ifelse(petn_n > 1, petn_sd / sqrt(petn_n), NA_real_),
    rdx_sem  = ifelse(rdx_n > 1, rdx_sd / sqrt(rdx_n), NA_real_)
  )

# Preserve Lab/Participant NUMERIC order on the x-axis (not alphabetical,
# which would otherwise sort "Lab 10 (1)" between "Lab 1 (1)" and "Lab 2 (1)")
participant_order <- participant_bar_data %>%
  distinct(Lab, Participant, ParticipantLabel) %>%
  arrange(Lab, Participant) %>%
  pull(ParticipantLabel)

# dodge_width is used identically for the bars and error bars so the error
# bars line up exactly over their corresponding bar. Uses plain
# position_dodge() (NOT position_dodge2()) -- dodge2's "preserve = single"
# mode computes bar and error-bar slot widths differently once a geom's
# own `width` parameter (e.g. the errorbar whisker cap width) differs from
# the bar width, causing the two geoms to dodge to slightly different
# x-offsets whenever more than one surface is present at a given x
# (misalignment only became visible from the first multi-bar participant
# onward -- single-bar participants trivially "aligned" since there was
# nothing to dodge). Plain position_dodge() divides the width identically
# for every geom sharing it, guaranteeing alignment, at the cost of bar
# width varying slightly depending on how many surfaces are present for
# that particular participant.
dodge_width <- 0.9

create_participant_barchart <- function(data, y_var, sem_var, y_label, title) {
  ggplot(data, aes(x = factor(ParticipantLabel, levels = participant_order),
                    y = !!sym(y_var), fill = SurfaceName)) +
    geom_bar(stat = "identity", position = position_dodge(width = dodge_width)) +
    geom_errorbar(
      aes(ymin = !!sym(y_var) - !!sym(sem_var), ymax = !!sym(y_var) + !!sym(sem_var)),
      position = position_dodge(width = dodge_width),
      width = 0.3
    ) +
    scale_fill_manual(values = surface_colors, breaks = fixed_surface_order, name = "Surface") +
    labs(x = "Participant", y = y_label, title = title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
}

# PETN by-participant bar chart
if (any(!is.na(participant_bar_data$petn_mean))) {
  p_bar_petn <- create_participant_barchart(
    data = participant_bar_data %>% filter(!is.na(petn_mean)),
    y_var = "petn_mean",
    sem_var = "petn_sem",
    y_label = "Mean PETN Recovery (%)",
    title = "PETN Recovery by Participant and Surface"
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_ByParticipant_BarChart.png"),
    p_bar_petn,
    width = 14, height = 6, dpi = 300
  )
}

# RDX by-participant bar chart
if (any(!is.na(participant_bar_data$rdx_mean))) {
  p_bar_rdx <- create_participant_barchart(
    data = participant_bar_data %>% filter(!is.na(rdx_mean)),
    y_var = "rdx_mean",
    sem_var = "rdx_sem",
    y_label = "Mean RDX Recovery (%)",
    title = "RDX Recovery by Participant and Surface"
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_RDX_ByParticipant_BarChart.png"),
    p_bar_rdx,
    width = 14, height = 6, dpi = 300
  )
}

print("Overall by-participant bar charts generated.")

# --- PETN vs RDX Recovery Correlation Plots (Added August 2026) ---
# Simple correlation scatter plots (not a true multivariate PCA -- with only
# two variables, a PCA would just be a 45-degree rotation of this same
# scatter and would add no analytical value). Examines whether PETN and RDX
# recovery co-vary within a sample (e.g. driven by shared
# extraction/transfer efficiency) or vary independently.
#
# Both Pearson r (linear) and Spearman rho (rank-based) are reported. Rho is
# included specifically because RDX has a well-documented floor effect on
# ABS-Smooth (27.7% zeros, see Statistical Analysis sections above), which
# violates the normality assumption Pearson relies on -- this mirrors the
# project's established practice elsewhere of pairing parametric and
# non-parametric tests when a floor/non-normality issue is present.
#
# Two granularities are generated (both overall and faceted by surface):
#   - Per-sample: every individual sample (recovery_data, same rows as the
#     box plots above)
#   - Participant mean: one point per Lab x Participant x Surface
#     (participant_bar_data, same rows as the by-participant bar charts)

compute_corr_stats <- function(data, x_var, y_var) {
  x <- data[[x_var]]
  y <- data[[y_var]]
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)

  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(data.frame(
      n = n, pearson_r = NA_real_, pearson_p = NA_real_,
      spearman_rho = NA_real_, spearman_p = NA_real_,
      label = paste0("n=", n, " (insufficient data)"),
      stringsAsFactors = FALSE
    ))
  }

  pear  <- suppressWarnings(stats::cor.test(x, y, method = "pearson"))
  spear <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))

  fmt_p <- function(p) {
    if (is.na(p)) return("p=NA")
    if (p < 0.001) return("p<0.001")
    paste0("p=", formatC(p, digits = 3, format = "f"))
  }

  label <- paste0(
    "n=", n, "\n",
    "r=", round(as.numeric(pear$estimate), 2), " (", fmt_p(pear$p.value), ")\n",
    "\u03c1=", round(as.numeric(spear$estimate), 2), " (", fmt_p(spear$p.value), ")"
  )

  data.frame(
    n = n,
    pearson_r = as.numeric(pear$estimate),
    pearson_p = pear$p.value,
    spearman_rho = as.numeric(spear$estimate),
    spearman_p = spear$p.value,
    label = label,
    stringsAsFactors = FALSE
  )
}

create_correlation_plot <- function(data, x_var, y_var, x_label, y_label,
                                     title, facet_by_surface = FALSE,
                                     facet_ylim = NULL) {

  plot_data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), , drop = FALSE]

  if (facet_by_surface) {
    stats_df <- plot_data %>%
      group_by(SurfaceName) %>%
      group_modify(~ compute_corr_stats(.x, x_var, y_var)) %>%
      ungroup()
  } else {
    stats_df <- compute_corr_stats(plot_data, x_var, y_var)
  }

  # facet_ylim (Added 2026-08-11): optionally clip specific facets' visible
  # y-range, e.g. facet_ylim = list(Steel = c(0, 25)) -- a single very-high-
  # leverage point can otherwise dominate a free-scaled panel's axis,
  # compressing the bulk of the data into an unreadable sliver at the
  # bottom. stats_df above is always computed from the FULL, unclipped
  # plot_data, so r/rho/n in the annotation stay honest (based on every
  # point) even though the clipped point(s) are not drawn. Only affects
  # facet_by_surface = TRUE plots; ignored otherwise.
  plot_data_display <- plot_data
  ylim_anchors <- NULL
  if (facet_by_surface && !is.null(facet_ylim)) {
    for (surf in names(facet_ylim)) {
      rng <- facet_ylim[[surf]]
      is_this_facet <- plot_data_display$SurfaceName == surf
      out_of_range <- is_this_facet &
        (plot_data_display[[y_var]] < rng[1] | plot_data_display[[y_var]] > rng[2])
      if (any(out_of_range)) {
        message(sprintf(
          "  create_correlation_plot(): clipping %d point(s) outside [%.1f, %.1f] from view in the '%s' facet (still included in stats_df/annotation above)",
          sum(out_of_range), rng[1], rng[2], surf))
      }
      plot_data_display <- plot_data_display[!out_of_range, , drop = FALSE]

      # Invisible anchor (geom_blank) pins this facet's free y-scale exactly
      # to rng regardless of where the remaining (non-clipped) points fall.
      # x is set to that facet's own median x (from the FULL data) so the
      # facet's independent free x-scale is left untouched.
      facet_x <- plot_data[plot_data$SurfaceName == surf, x_var, drop = TRUE]
      anchor_x <- if (length(facet_x) > 0) stats::median(facet_x, na.rm = TRUE) else 0
      anchor_rows <- data.frame(SurfaceName = surf, x = anchor_x, y = rng)
      ylim_anchors <- dplyr::bind_rows(ylim_anchors, anchor_rows)
    }
  }

  p <- ggplot(plot_data_display, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = SurfaceName), alpha = 0.6, size = 2) +
    scale_color_manual(values = surface_colors, breaks = fixed_surface_order,
                        name = "Surface") +
    labs(x = x_label, y = y_label, title = title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

  if (!is.null(ylim_anchors)) {
    p <- p + geom_blank(data = ylim_anchors, aes(x = x, y = y), inherit.aes = FALSE)
  }

  if (facet_by_surface) {
    p <- p +
      facet_wrap(~ SurfaceName, scales = "free") +
      geom_text(
        data = stats_df,
        aes(x = Inf, y = Inf, label = label),
        hjust = 1.05, vjust = 1.1, size = 3, inherit.aes = FALSE
      )
  } else {
    p <- p +
      geom_text(
        data = stats_df,
        aes(x = Inf, y = Inf, label = label),
        hjust = 1.05, vjust = 1.1, size = 3.5, inherit.aes = FALSE
      )
  }

  list(plot = p, stats = stats_df)
}


corr_stats_all <- list()

# --- Per-sample correlation (all individual samples) ---
n_corr_persample <- sum(!is.na(recovery_data$petn_rec) & !is.na(recovery_data$rdx_rec))

if (n_corr_persample >= 3) {
  res_persample <- create_correlation_plot(
    data = recovery_data,
    x_var = "petn_rec", y_var = "rdx_rec",
    x_label = "PETN Recovery (%)", y_label = "RDX Recovery (%)",
    title = "PETN vs RDX Recovery (Per Sample, All Labs)"
  )
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_vs_RDX_Correlation_PerSample.png"),
    res_persample$plot, width = 7, height = 6, dpi = 300
  )
  corr_stats_all[["PerSample_Overall"]] <- res_persample$stats %>%
    mutate(Granularity = "Per Sample", Grouping = "Overall")

  res_persample_facet <- create_correlation_plot(
    data = recovery_data,
    x_var = "petn_rec", y_var = "rdx_rec",
    x_label = "PETN Recovery (%)", y_label = "RDX Recovery (%)",
    title = "PETN vs RDX Recovery by Surface (Per Sample)",
    facet_by_surface = TRUE,
    # Steel panel capped at 0-25% (Added 2026-08-11): a single very-high-
    # leverage point (~165% RDX recovery) otherwise dominates the free-
    # scaled axis, compressing the bulk of the Steel data into an
    # unreadable sliver -- r/rho/n in the annotation still reflect the full,
    # unclipped data (see create_correlation_plot()'s facet_ylim handling).
    facet_ylim = list(Steel = c(0, 25))
  )
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_vs_RDX_Correlation_PerSample_BySurface.png"),
    res_persample_facet$plot, width = 10, height = 8, dpi = 300
  )
  corr_stats_all[["PerSample_BySurface"]] <- res_persample_facet$stats %>%
    mutate(Granularity = "Per Sample", Grouping = SurfaceName) %>%
    select(-SurfaceName)
} else {
  message("Skipping PETN vs RDX per-sample correlation plots (n = ", n_corr_persample, " < 3).")
}

# --- Participant-mean correlation (one point per Lab x Participant x Surface) ---
n_corr_participant <- sum(!is.na(participant_bar_data$petn_mean) & !is.na(participant_bar_data$rdx_mean))

if (n_corr_participant >= 3) {
  res_participant <- create_correlation_plot(
    data = participant_bar_data,
    x_var = "petn_mean", y_var = "rdx_mean",
    x_label = "Mean PETN Recovery (%)", y_label = "Mean RDX Recovery (%)",
    title = "PETN vs RDX Recovery (Participant Means, All Labs)"
  )
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_vs_RDX_Correlation_ParticipantMean.png"),
    res_participant$plot, width = 7, height = 6, dpi = 300
  )
  corr_stats_all[["ParticipantMean_Overall"]] <- res_participant$stats %>%
    mutate(Granularity = "Participant Mean", Grouping = "Overall")

  res_participant_facet <- create_correlation_plot(
    data = participant_bar_data,
    x_var = "petn_mean", y_var = "rdx_mean",
    x_label = "Mean PETN Recovery (%)", y_label = "Mean RDX Recovery (%)",
    title = "PETN vs RDX Recovery by Surface (Participant Means)",
    facet_by_surface = TRUE
  )
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_vs_RDX_Correlation_ParticipantMean_BySurface.png"),
    res_participant_facet$plot, width = 10, height = 8, dpi = 300
  )
  corr_stats_all[["ParticipantMean_BySurface"]] <- res_participant_facet$stats %>%
    mutate(Granularity = "Participant Mean", Grouping = SurfaceName) %>%
    select(-SurfaceName)
} else {
  message("Skipping PETN vs RDX participant-mean correlation plots (n = ", n_corr_participant, " < 3).")
}

# Export correlation statistics table (companion to the plots above)
if (length(corr_stats_all) > 0) {
  corr_stats_export <- bind_rows(corr_stats_all) %>%
    select(Granularity, Grouping, n, pearson_r, pearson_p, spearman_rho, spearman_p)
  write.csv(
    corr_stats_export,
    file.path(plots_overall_dir, "PETN_RDX_Correlation_Stats.csv"),
    row.names = FALSE
  )
  print("PETN vs RDX correlation plots and stats generated.")
}

# --- PETN/RDX Recovery + %RSD PCA Biplot (Added August 2026) ---
# Multivariate exploratory view of Lab x Participant x Surface performance
# profiles, using 4 variables: mean PETN recovery, mean RDX recovery, PETN
# %RSD, and RDX %RSD. Unlike the 2-variable "PCA" that was considered (and
# rejected as adding nothing) for the PETN-vs-RDX correlation plots above,
# 4 variables genuinely support a PCA: it can reveal whether labs/
# participants cluster into distinct performance profiles (e.g. "high
# recovery, low variability" vs "low recovery, high variability") in a way
# that looking at each variable individually cannot. Complements the
# existing lmer models (which already show Lab/Participant is a dominant
# variance source) by showing *which* labs pattern together and on *what*
# dimensions.
#
# Granularity: one point per Lab x Participant x Surface (same as the
# correlation plots above), reusing participant_bar_data (already built
# from recovery_data -- i.e. already restricted to Outcome == "Complete"
# samples with NA recovery -> 0%, consistent with every other plot in this
# section). Points are coloured by Surface (not labelled with Lab/
# Participant identity, per user preference) so surface-level clustering is
# visible without cluttering the plot.

petn_rsd <- ifelse(participant_bar_data$petn_mean != 0,
                    participant_bar_data$petn_sd / participant_bar_data$petn_mean * 100,
                    NA_real_)
rdx_rsd <- ifelse(participant_bar_data$rdx_mean != 0,
                   participant_bar_data$rdx_sd / participant_bar_data$rdx_mean * 100,
                   NA_real_)
participant_bar_data$petn_rsd <- petn_rsd
participant_bar_data$rdx_rsd <- rdx_rsd

pca_vars <- c("petn_mean", "rdx_mean", "petn_rsd", "rdx_rsd")
pca_data <- participant_bar_data[
  stats::complete.cases(participant_bar_data[, pca_vars]),
  , drop = FALSE
]

n_pca_total <- nrow(participant_bar_data)
n_pca_excluded <- n_pca_total - nrow(pca_data)
if (n_pca_excluded > 0) {
  message("PCA: excluded ", n_pca_excluded, " of ", n_pca_total,
          " Lab x Participant x Surface groups (RSD undefined for n<2 ",
          "replicates or zero mean recovery).")
}

if (nrow(pca_data) >= 8) {

  pca_result <- prcomp(pca_data[, pca_vars], center = TRUE, scale. = TRUE)

  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

  pca_scores <- as.data.frame(pca_result$x)
  pca_scores$Lab <- pca_data$Lab
  pca_scores$Participant <- pca_data$Participant
  pca_scores$SurfaceName <- pca_data$SurfaceName
  pca_scores$ParticipantLabel <- pca_data$ParticipantLabel

  var_labels <- c(
    petn_mean = "PETN Recovery",
    rdx_mean  = "RDX Recovery",
    petn_rsd  = "PETN %RSD",
    rdx_rsd   = "RDX %RSD"
  )

  pca_loadings <- as.data.frame(pca_result$rotation)
  pca_loadings$Variable <- var_labels[rownames(pca_loadings)]

  # Rescale loading vectors to a comparable visual magnitude to the score
  # points (standard biplot convention) so the arrows are visible alongside
  # the points rather than being tiny by comparison.
  score_range <- max(abs(c(pca_scores$PC1, pca_scores$PC2)), na.rm = TRUE)
  loading_range <- max(abs(c(pca_loadings$PC1, pca_loadings$PC2)), na.rm = TRUE)
  loading_scale_factor <- (score_range * 0.8) / loading_range

  pca_loadings_scaled <- pca_loadings
  pca_loadings_scaled$PC1 <- pca_loadings$PC1 * loading_scale_factor
  pca_loadings_scaled$PC2 <- pca_loadings$PC2 * loading_scale_factor

  # Label position: nudge slightly further out along each arrow (beyond the
  # tip) and centre-justify (hjust/vjust = 0.5, ggplot's default). Combined
  # with the generous axis expansion below, this keeps every label fully
  # inside the panel regardless of which direction its arrow points,
  # without needing direction-dependent justification logic.
  pca_loadings_scaled$label_x <- pca_loadings_scaled$PC1 * 1.15
  pca_loadings_scaled$label_y <- pca_loadings_scaled$PC2 * 1.15

  pc1_label <- paste0("PC1 (", round(var_explained[1], 1), "% variance)")
  pc2_label <- paste0("PC2 (", round(var_explained[2], 1), "% variance)")

  p_pca_biplot <- ggplot() +
    geom_point(
      data = pca_scores,
      aes(x = PC1, y = PC2, color = SurfaceName),
      alpha = 0.7, size = 3
    ) +
    geom_segment(
      data = pca_loadings_scaled,
      aes(x = 0, y = 0, xend = PC1, yend = PC2),
      arrow = arrow(length = unit(0.25, "cm")),
      color = "black", linewidth = 0.6
    ) +
    geom_text(
      data = pca_loadings_scaled,
      aes(x = label_x, y = label_y, label = Variable),
      color = "black", fontface = "bold", size = 3.5
    ) +
    scale_color_manual(values = surface_colors, breaks = fixed_surface_order,
                        name = "Surface") +
    scale_x_continuous(expand = expansion(mult = 0.35)) +
    scale_y_continuous(expand = expansion(mult = 0.35)) +
    labs(
      x = pc1_label, y = pc2_label,
      title = "PETN/RDX Recovery & Variability -- Participant PCA Biplot"
    ) +
    coord_fixed() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_RDX_PCA_Biplot.png"),
    p_pca_biplot, width = 8, height = 7, dpi = 300
  )

  # --- Scree / variance-explained chart ---
  scree_data <- data.frame(
    PC = factor(paste0("PC", seq_along(var_explained)),
                levels = paste0("PC", seq_along(var_explained))),
    PctVariance = var_explained
  )

  p_pca_scree <- ggplot(scree_data, aes(x = PC, y = PctVariance)) +
    geom_bar(stat = "identity", fill = "#4472C4") +
    geom_text(aes(label = paste0(round(PctVariance, 1), "%")),
              vjust = -0.5, size = 3.5) +
    labs(
      x = "Principal Component", y = "% Variance Explained",
      title = "PETN/RDX PCA -- Variance Explained per Component"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_RDX_PCA_VarianceExplained.png"),
    p_pca_scree, width = 6, height = 5, dpi = 300
  )

  # --- Companion CSVs (scores + loadings) ---
  write.csv(
    pca_scores %>% select(Lab, Participant, SurfaceName, ParticipantLabel,
                           PC1, PC2, PC3, PC4),
    file.path(plots_overall_dir, "PETN_RDX_PCA_Scores.csv"),
    row.names = FALSE
  )
  write.csv(
    pca_loadings %>% select(Variable, PC1, PC2, PC3, PC4),
    file.path(plots_overall_dir, "PETN_RDX_PCA_Loadings.csv"),
    row.names = FALSE
  )

  print("PETN/RDX PCA biplot, scree chart, and stats generated.")

} else {
  message("Skipping PETN/RDX PCA (only ", nrow(pca_data),
          " complete Lab x Participant x Surface groups available; need >= 8).")
}

# --- Per-Lab Plots ---
labs_with_data <- recovery_data %>%
  filter(!is.na(Lab)) %>%
  distinct(Lab) %>%
  arrange(Lab) %>%
  pull(Lab)

for (lab_id in labs_with_data) {
  
  lab_data <- recovery_data %>% filter(Lab == lab_id)
  lab_str <- sprintf("Lab%02d", lab_id)
  lab_dir <- file.path(plots_dir, lab_str)
  dir.create(lab_dir, showWarnings = FALSE)
  
  # Filter fixed order to surfaces present in this lab's data
  surfaces_present_lab <- fixed_surface_order[fixed_surface_order %in% 
                            unique(lab_data$SurfaceName[!is.na(lab_data$petn_rec) | !is.na(lab_data$rdx_rec)])]
  
  # Type A: Participant x Surface detailed plots
  lab_data_a <- lab_data %>%
    mutate(ParticipantSurface = paste0("P", Participant, " ", SurfaceName))
  
  # PETN detailed
  if (any(!is.na(lab_data_a$petn_rec))) {
    p_detail_petn <- create_boxplot(
      data = lab_data_a %>% filter(!is.na(petn_rec)),
      x_var = "ParticipantSurface",
      y_var = "petn_rec",
      x_label = "Participant - Surface",
      y_label = "PETN Recovery (%)",
      title = paste(lab_str, "- PETN Recovery by Participant & Surface")
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_PETN_ParticipantSurface.png")),
      p_detail_petn,
      width = 10, height = 5, dpi = 300
    )
  }
  
  # RDX detailed
  if (any(!is.na(lab_data_a$rdx_rec))) {
    p_detail_rdx <- create_boxplot(
      data = lab_data_a %>% filter(!is.na(rdx_rec)),
      x_var = "ParticipantSurface",
      y_var = "rdx_rec",
      x_label = "Participant - Surface",
      y_label = "RDX Recovery (%)",
      title = paste(lab_str, "- RDX Recovery by Participant & Surface")
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_RDX_ParticipantSurface.png")),
      p_detail_rdx,
      width = 10, height = 5, dpi = 300
    )
  }
  
  # Type B: Surface summary plots (pooled across participants)
  # PETN surface summary
  if (any(!is.na(lab_data$petn_rec))) {
    p_surface_petn <- create_boxplot(
      data = lab_data %>% filter(!is.na(petn_rec)),
      x_var = "SurfaceName",
      y_var = "petn_rec",
      x_label = "Surface",
      y_label = "PETN Recovery (%)",
      title = paste(lab_str, "- PETN Recovery by Surface"),
      x_order = surfaces_present_lab
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_PETN_SurfaceSummary.png")),
      p_surface_petn,
      width = 6, height = 5, dpi = 300
    )
  }
  
  # RDX surface summary
  if (any(!is.na(lab_data$rdx_rec))) {
    p_surface_rdx <- create_boxplot(
      data = lab_data %>% filter(!is.na(rdx_rec)),
      x_var = "SurfaceName",
      y_var = "rdx_rec",
      x_label = "Surface",
      y_label = "RDX Recovery (%)",
      title = paste(lab_str, "- RDX Recovery by Surface"),
      x_order = surfaces_present_lab
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_RDX_SurfaceSummary.png")),
      p_surface_rdx,
      width = 6, height = 5, dpi = 300
    )
  }
  
  print(paste0("Plots generated for ", lab_str))
}

print(paste0("All box plots saved to: ", plots_dir))

# =========================================================
# 14. Final summary to console
# =========================================================
n_labs <- n_distinct(master$Lab, na.rm = TRUE)
n_participants <- master %>%
  filter(!is.na(Lab)) %>%
  distinct(Lab, Participant) %>%
  nrow()
n_surfaces <- n_distinct(master$Surface, na.rm = TRUE)
n_samples <- nrow(master)
n_ncs <- nrow(master_ncs)

print(paste0(""))
print(paste0("=== FINEX Study Collation Complete ==="))
print(paste0("  Labs:                 ", n_labs))
print(paste0("  Lab-Participant sets: ", n_participants))
print(paste0("  Surface types:        ", n_surfaces))
print(paste0("  Sample repeats:       ", n_samples))
print(paste0("  Negative controls:    ", n_ncs))
print(paste0("  --- Sample Acceptance ---"))
print(paste0("  Accepted (PASS):      ", n_pass))
print(paste0("  Accepted (PASS*):     ", n_pass_star))
print(paste0("  Rejected (FAIL):      ", n_fail))
print(paste0("  --- NC Stage 1: Analysis Acceptance (is the run trustworthy?) ---"))
print(paste0("  NC Accepted (PASS):   ", n_nc_pass))
print(paste0("  NC Accepted (PASS*):  ", n_nc_pass_star))
print(paste0("  NC Rejected (FAIL):   ", n_nc_fail))
print(paste0("  --- NC Stage 2: Result (valid runs only) ---"))
print(paste0("  NC Negative (clean):  ", n_nc_negative))
print(paste0("  NC Positive:          ", n_nc_positive))
print(paste0("  NC Not evaluated:     ", n_nc_not_evaluated))
print(paste0("  --- Outputs ---"))
print(paste0("  Output CSV:  ", csv_path))
print(paste0("  Output XLSX: ", xlsx_path))
print(paste0("  Output Plots: ", plots_dir))
print(paste0("======================================"))
