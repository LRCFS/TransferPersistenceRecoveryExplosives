#===============================================================================
# PILOT STUDY ANALYSIS: EXPLOSIVES RECOVERY - NESTED DESIGN
#
# ONE-SCRIPT WORKFLOW: run this after GCMSQuantitation's Code/01, 02, and 03
# have processed each Analysis run (Analysis1, Analysis2, ...). This script:
#   1. Auto-discovers ALL Analysis*/Results/*_GCMSResults.csv files
#   2. Applies the same QC pass/fail criteria used in the GCMSQuantitation
#      FINEX pipeline (Code/04_CollateStudyResults.R), adapted for this
#      study's design (RunID/SurfaceID/Surface_type/Pressure/Solvent/Batch
#      instead of Lab/Participant)
#   3. Calculates PETN/RDX recovery (%), updates pilot_data_nested.csv, and
#      writes ASTRA_PilotStudyResults.xlsx (analogous to FINEX_StudyResults.xlsx)
#   4. Filters to QC-passed samples only (PASS/PASS*) and analyzes the pilot
#      data from the nested 2^3 factorial design:
#      - Estimates variance components (between-surface vs within-surface)
#      - Calculates effect sizes for all factors and interactions
#      - Determines required sample size for the main study
#      - Generates diagnostic plots and a summary report
#
# Output:
#   - pilot_data_nested.csv (updated with recovery + QC/acceptance columns)
#   - ASTRA_PilotStudyResults.xlsx (collated study results, multi-sheet)
#   - Variance component estimates and ICC
#   - Effect sizes (Cohen's f, partial eta^2)
#   - Power analysis for main study
#   - Diagnostic plots
#
# Pass/fail criteria reference: GCMSQuantitation/CONTEXT.md
#   ("QC Monitoring Strategy", "Negative Control (NC) Evaluation Criteria")
# Source logic ported from: GCMSQuantitation/Code/04_CollateStudyResults.R
#
# Author: Generated for trace explosives recovery optimization
# Date: 2026-07-06 (merged into a single script: 2026-07-30)
#===============================================================================

#===============================================================================
# USER CONFIGURATION
#===============================================================================

# Analysis mode: "boxplots_only" or "full"
#   - "boxplots_only": Load data and produce box plots only (works with partial data)
#   - "full": Complete analysis (mixed model, ANOVA, power, diagnostics, plus box plots)
analysis_mode <- "full"

# QC acceptance filter for the statistical analysis (mixed-effects model,
# ANOVA, effect sizes, power analysis, and the main QC-filtered box plots/
# bar charts -- everything EXCEPT the "_alldata" plots below, which always
# ignore this and show every sample regardless, as a permanent QC-audit
# comparison view):
#   TRUE  = only QC-accepted samples (analysis_accepted %in% c("PASS",
#           "PASS*")) are included (the default, and the previous
#           unconditional behaviour). Mirrors GCMSQuantitation's July 2026
#           "Critical QC Filtering Issue" fix: including QC-failed samples
#           artificially dilutes variance components and effect sizes.
#   FALSE = run the WHOLE statistical analysis on ALL data, regardless of
#           QC acceptance (QC-failed samples included). Negative controls
#           are still excluded either way -- they have no Surface_type/
#           Pressure_level/Solvent_level to fit against, so lmer() drops
#           them from the model automatically (NA design factors), and the
#           box/bar/repeat-chart functions drop them for the same reason.
use_qc_filtered_data <- TRUE

# Input directory (design files, this script)
data_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"

# Output directory for results (plots, statistical analysis, data with recovery values)
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"

# Input/output file (recovery values + QC acceptance columns filled in by the
# collation step below)
data_file <- file.path(output_dir, "pilot_data_nested.csv")

# Full statistical analysis summary (only written when analysis_mode ==
# "full"). Captures EVERYTHING printed during the mixed-effects model /
# ANOVA / effect sizes / assumption checks / power analysis / main study
# design recommendation sections verbatim (via sink(), see "ANALYZE BOTH
# ANALYTES" section below) -- previously this was only ever printed to the
# console and not saved anywhere, so a "full" run's numeric results were
# lost once the console/session closed.
stats_summary_file <- file.path(output_dir, "pilot_analysis_summary.txt")

# Target meaningful effect (for power calculations)
meaningful_recovery_difference_pct <- 10  # 10% recovery difference is meaningful

# Externally-anchored (non-circular) expected Pressure effect, for the
# WET-SWAB-ONLY scenario's sample-size recommendation. Unlike the pilot's
# own retrospective/observed Pressure effect size (see PER-EFFECT POWER
# ANALYSIS / SCENARIO: WET-SWAB-ONLY sections), this figure comes from
# published wipe-sampling literature, NOT from this pilot's own data --
# see this file's own "Pressure selection rationale" (CONTEXT.md): "Expected
# ~6% absolute CE difference based on literature (4%/Newton)". Using an
# externally-sourced target effect avoids the retrospective-power pitfall
# entirely for the EFFECT SIZE itself (only the residual variance/ICC used
# to scale it is still pilot-derived, which is unavoidable for any power
# calculation and is not itself circular).
pressure_literature_effect_pct <- 6

# Desired power for main study
target_power <- 0.80  # 80% power

# Significance level
alpha_level <- 0.05

# Maximum surfaces available for main study
max_surfaces_per_type <- 20

#===============================================================================
# GC-MS DATA COLLATION CONFIGURATION
#===============================================================================

# GC Data root (contains Analysis1/, Analysis2/, ... each with a Results/ folder)
gc_data_dir <- file.path(output_dir, "GC Data")

# Design files (local repo)
design_file       <- file.path(data_dir, "pilot_design_nested.csv")
batch_design_file <- file.path(data_dir, "pilot_design_batch_organized.csv")

# Collated study workbook (analogous to FINEX_StudyResults.xlsx)
xlsx_out <- file.path(gc_data_dir, "ASTRA_PilotStudyResults.xlsx")

# QC acceptance threshold (mirrors GCMSQuantitation/GlobalCode.R qc_bias_limit
# -- the code's actual applied value, NOT the +/-15% figure quoted in
# CONTEXT.md's prose, which has drifted out of sync with the code)
qc_6ng_bias_limit <- 20   # percent, +/-

# Manual override: exclude specific QC injections from bracket duty even
# though the automatic "failed to inject" check (based on the internal
# standard's SNR flag, in the shared InjectionAcceptance.R) didn't catch
# them -- e.g. a QC where the IS looks fine but one analyte's own PA reads
# 0/anomalous. Built for a real case found in the FINEX study
# (GCMSQuantitation/Code/04_CollateStudyResults.R's qc_bracket_exclude,
# "Outstanding samples 4" 6ng QC at sequence Line 39: PETN PA = 0, IS not
# flagged low -- silently bracketed and FAILed every neighbouring sample on
# a bogus PETN reading until excluded). Empty by default here -- no
# equivalent bad QC has been identified in this study's own data yet. Add
# an entry in the same list("<SourceFile substring>" = c(<Line numbers>))
# format if one turns up (e.g. spotted via the QC bias plots as an
# implausible single-analyte bracket FAIL with a normal-looking IS). See
# assign_qc_brackets()'s header comment in
# GCMSQuantitation/Code/InjectionAcceptance.R for the full mechanism --
# matching is by substring against SourceFile, not bare Line number, since
# Line numbers restart at 1 per dataset/sequence.
qc_bracket_exclude <- list()

# Recovery calculation constants (NOT GCMSQuantitation's FINEX-specific
# DepositMass/DepositConc, which are for a different study)
sample_vol_uL       <- 1000        # Total extraction solvent volume (uL)
deposit_mass_ng     <- 10000       # Mass deposited per analyte (ng): 50 uL x 200 ng/uL
ideal_concentration <- deposit_mass_ng / sample_vol_uL  # ng/uL at 100% recovery (=10)

# Extraction/filtration efficiency correction -- added so this study's
# recovery formula matches GCMSQuantitation/GlobalCode.R's + FINEX's
# 04_CollateStudyResults.R's "Recovery (%) = (mass / deposit) / (extraction
# * filtration) * 100" exactly (mathematically: mass/deposit here ==
# concentration/ideal_concentration, since ideal_concentration is already
# deposit_mass_ng/sample_vol_uL -- see the Recovery_pct calculation below).
# Both currently 1 (TODO: determine experimentally) in GlobalCode.R too --
# these are LOCAL copies (this study doesn't source GlobalCode.R), kept in
# sync MANUALLY. If/when real efficiency values are determined, update
# BOTH places, and confirm whether the same values apply to both studies
# (same swab/extraction/filtration protocol) before assuming so -- do not
# assume they're interchangeable without checking.
petn_extraction_efficiency <- 1  # TODO: determine experimentally (mirrors GlobalCode.R)
rdx_extraction_efficiency  <- 1  # TODO: determine experimentally (mirrors GlobalCode.R)
petn_filtration_efficiency <- 1  # TODO: determine experimentally (mirrors GlobalCode.R)
rdx_filtration_efficiency  <- 1  # TODO: determine experimentally (mirrors GlobalCode.R)

# Study-wide expected totals (for the SampleSummary "Complete %" column)
expected_total_per_surface <- 16   # 4 surfaces x 4 conditions x 1 rep, per surface type
expected_total_samples     <- 32
expected_total_nc          <- 8    # 4 batches x 2 surface types (1 NC per surface per batch)

#===============================================================================
# PACKAGE LOADING
#===============================================================================

required_packages <- c("lme4", "lmerTest", "dplyr", "ggplot2", "car",
                       "effectsize", "pwr", "gridExtra", "emmeans",
                       "stringr", "openxlsx", "readxl")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#===============================================================================
# Shared injection-acceptance logic (Sections A-D of GCMSQuantitation's
# Diagnostics/Improved_QC_Flowchart.dot) -- the SAME file FINEX's
# GCMSQuantitation/Code/04_CollateStudyResults.R sources. Provides
# evaluate_blanks(), assign_qc_brackets(), compute_injection_acceptance(),
# derive_nc_result(). Do not duplicate any of this logic locally in this
# script -- see GCMSQuantitation/CONTEXT.md for the consolidation rationale.
#===============================================================================
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Code/InjectionAcceptance.R")

#===============================================================================
# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot in this repo. See thesis_palette.R's own
# header for the full rationale (each recurring factor gets a fixed,
# dedicated colour pair, never reused for a different factor's meaning).
#===============================================================================
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

#===============================================================================
# DATA COLLATION FROM GC-MS RESULTS
#
# Auto-discovers all Analysis*/Results/*_GCMSResults.csv files produced by
# the GCMSQuantitation pipeline for the ASTRA Swabbing Pilot Study, merges
# them with the experimental design matrix, applies the SAME QC pass/fail
# criteria used in the FINEX Swabbing Study pipeline (adapted for this
# study's design), calculates recovery, updates pilot_data_nested.csv, and
# produces ASTRA_PilotStudyResults.xlsx.
#
# This is incremental and idempotent: re-run any time new Analysis* folders
# are added (e.g. Analysis3, Analysis4, ...) -- it rediscovers everything
# from scratch each time. Wrapped in a function so its many intermediate
# variables (all_data, samples, pilot_rows, ...) stay local and cannot
# collide with the statistical analysis code below.
#===============================================================================

collate_gcms_results <- function() {

  #-----------------------------------------------------------------------------
  # 1. DISCOVER AND LOAD ALL *_GCMSResults.csv FILES
  #-----------------------------------------------------------------------------

  cat("\n========================================\n")
  cat("DISCOVERING GC-MS RESULTS FILES\n")
  cat("========================================\n\n")

  results_files <- list.files(
    gc_data_dir,
    pattern    = "_GCMSResults\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )

  # Exclude backup copies
  results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]

  if (length(results_files) == 0) {
    stop("No *_GCMSResults.csv files found under: ", gc_data_dir)
  }

  cat("Found", length(results_files), "results file(s):\n")
  for (f in results_files) cat("  ", f, "\n")
  cat("\n")

  all_data <- bind_rows(lapply(results_files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$SourceFile <- f
    # Dataset name = the Analysis* folder name (parent of parent of the file, i.e. .../Analysis1/Results/x.csv)
    df$Dataset <- basename(dirname(dirname(f)))
    df
  }))

  cat("Total rows loaded across all datasets:", nrow(all_data), "\n\n")

  #-----------------------------------------------------------------------------
  # 2. DISCOVER AND LOAD CALIBRATION STATS FILES
  #-----------------------------------------------------------------------------

  cal_stats_files <- list.files(
    gc_data_dir,
    pattern    = "_CalibrationStats\\.xlsx$",
    full.names = TRUE,
    recursive  = TRUE
  )
  cal_stats_files <- cal_stats_files[!grepl("/Backup/", cal_stats_files, fixed = TRUE)]

  cat("Found", length(cal_stats_files), "calibration stats file(s).\n\n")

  if (length(cal_stats_files) > 0) {
    all_cal_stats <- bind_rows(lapply(cal_stats_files, function(f) {
      dataset_name <- basename(dirname(dirname(f)))
      df <- readxl::read_excel(f, sheet = 1)
      df$Dataset <- dataset_name
      df
    }))

    # Attach earliest date per dataset
    date_lookup <- all_data %>%
      group_by(Dataset) %>%
      summarise(Date = suppressWarnings(min(Date, na.rm = TRUE)), .groups = "drop")

    all_cal_stats <- all_cal_stats %>%
      left_join(date_lookup, by = "Dataset") %>%
      relocate(Dataset, Date)

    if (nrow(all_cal_stats) > 0 && "CalibrationSet" %in% names(all_cal_stats)) {
      all_cal_stats <- all_cal_stats %>% arrange(Date, Dataset, CalibrationSet)
    }
  } else {
    warning("No calibration statistics files found -- CalSummary sheet will be empty.")
    all_cal_stats <- data.frame()
  }

  #-----------------------------------------------------------------------------
  # 3. BLANK EVALUATION + QC BRACKET ASSIGNMENT
  #    evaluate_blanks() / assign_qc_brackets() now live in the SHARED
  #    GCMSQuantitation/Code/InjectionAcceptance.R (sourced in USER
  #    CONFIGURATION above), implementing Sections A-B of
  #    GCMSQuantitation/Diagnostics/Improved_QC_Flowchart.dot. Do not
  #    re-add a local copy of this logic here -- FINEX's
  #    Code/04_CollateStudyResults.R sources the identical file.
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("EVALUATING BLANKS + ASSIGNING QC BRACKETS\n")
  cat("========================================\n\n")

  petn_bias_col <- if ("petn_percent_bias_dc" %in% names(all_data)) {
    "petn_percent_bias_dc"
  } else if ("petn_percent_bias" %in% names(all_data)) {
    "petn_percent_bias"
  } else {
    NA_character_
  }

  rdx_bias_col <- if ("rdx_percent_bias" %in% names(all_data)) {
    "rdx_percent_bias"
  } else if ("rdx_percent_bias_dc" %in% names(all_data)) {
    "rdx_percent_bias_dc"
  } else {
    NA_character_
  }

  cat("PETN bias column for QC brackets:", petn_bias_col, "\n")
  cat("RDX bias column for QC brackets: ", rdx_bias_col, "\n\n")

  all_data <- evaluate_blanks(all_data)
  n_blank_contam <- sum(all_data$blank_status == "Contaminated", na.rm = TRUE)
  n_blank_trace  <- sum(all_data$blank_status == "Trace detected", na.rm = TRUE)
  n_blank_clean  <- sum(all_data$blank_status == "Clean", na.rm = TRUE)
  cat("Blank evaluation: Contaminated=", n_blank_contam, " Trace detected=", n_blank_trace,
      " Clean=", n_blank_clean, "\n\n", sep = "")

  all_data <- assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col, qc_6ng_bias_limit,
                                  qc_bracket_exclude = qc_bracket_exclude)
  cat("\nQC bracket flags assigned.\n\n")

  #-----------------------------------------------------------------------------
  # 4. FILTER TO SAMPLE ROWS AND CLASSIFY: PILOT SAMPLE / NEGATIVE CONTROL / OTHER
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("CLASSIFYING SAMPLE ROWS\n")
  cat("========================================\n\n")

  samples <- all_data %>% filter(Type == "Sample")
  samples$SampleName_trimmed <- str_trim(samples$SampleName)

  pilot_pattern <- "^PILOT_(\\d{3})$"
  # Matches every NC naming variant seen across Analysis1-6 so far:
  #   PILOT_ABSBatch1NegCtrl    (Analysis1/4, no separator, "NegCtrl")
  #   PILOT_SteelBatch2_NC      (Analysis3, underscore separator, "NC")
  #   ABSBatch4 NC              (Analysis6, NO "PILOT_" prefix, space separator, "NC")
  # The "PILOT_" prefix and the Batch/NegCtrl-or-NC separator are both
  # optional/flexible ([ _]?) since naming has drifted across batches --
  # see "Negative Control Regex Fix" session summary below.
  nc_pattern    <- "^(?:PILOT_)?(Steel|ABS)Batch(\\d+)[ _]?(?:NegCtrl|NC)$"

  samples$IsPilotSample     <- grepl(pilot_pattern, samples$SampleName_trimmed)
  samples$IsNegativeControl <- grepl(nc_pattern, samples$SampleName_trimmed, ignore.case = TRUE)

  n_other <- sum(!samples$IsPilotSample & !samples$IsNegativeControl)
  if (n_other > 0) {
    other_names <- unique(samples$SampleName_trimmed[!samples$IsPilotSample & !samples$IsNegativeControl])
    cat("Excluding", n_other, "non-pilot sample row(s) (e.g. test standards):\n")
    for (nm in other_names) cat("  '", nm, "'\n", sep = "")
    cat("\n")
  }

  samples <- samples %>% filter(IsPilotSample | IsNegativeControl)

  cat("Pilot samples found:      ", sum(samples$IsPilotSample), "\n")
  cat("Negative controls found: ", sum(samples$IsNegativeControl), "\n\n")

  # Parse NC metadata (Surface_type + BatchNumber) from SampleName
  nc_parsed <- str_match(samples$SampleName_trimmed, nc_pattern)
  samples$NC_Surface_type <- ifelse(samples$IsNegativeControl,
                                     tolower(nc_parsed[, 2]), NA_character_)
  samples$NC_BatchNumber  <- ifelse(samples$IsNegativeControl, as.integer(nc_parsed[, 3]), NA_integer_)

  # Path to the Analysis run folder this row came from (e.g. ".../GC Data/Analysis1"),
  # i.e. the parent of the "Results" folder containing the *_GCMSResults.csv file
  samples$AnalysisFolder <- dirname(dirname(samples$SourceFile))

  #-----------------------------------------------------------------------------
  # 5. CALCULATE RECOVERY (%)
  #    Computed for ALL sample rows (pilot samples AND negative controls) so
  #    that NC contamination can be expressed on the same recovery-equivalent
  #    scale as real samples (mirrors GCMSQuantitation's FINEX NC sheet,
  #    which also reports petn_recovery_dc/rdx_recovery for negative controls).
  #-----------------------------------------------------------------------------
  # Recovery (%) = (drift-corrected concentration / ideal concentration) * 100
  # PETN uses drift-corrected concentration; RDX has no drift correction applied
  # in this pipeline configuration (IS ratio quantification is used instead).

  # Three flag states all represent a genuine, legitimate negative (either
  # a true non-detection, or a real-but-negligible trace) and are quanted at
  # 0% recovery rather than left as NA or at their tiny real value:
  #   - "Below_LOD": a candidate peak was found but its height/SNR was
  #     below the detection threshold. GCMSQuantitation deliberately sets
  #     concentration_dc/concentration to NA for these rows
  #     (Code/03_Quantification.R), since "no reliable signal" isn't a
  #     quantifiable value in its own pipeline.
  #   - NA (the flag column itself, not a string): no candidate peak was
  #     found at all within RT tolerance -- an even more definitive
  #     non-detection than Below_LOD. Confirmed (2026-08-05) against real
  #     data that this is a reproducible, genuine non-detection rather than
  #     a one-off instrument blip: PILOT_013/017/021/027 each show
  #     petn_snr_flag = NA consistently across 2-3 independent re-analyses
  #     (different Analysis folders, different injection dates).
  #   - "Below_LOQ" (Added 2026-08-06, both analytes): unlike the above two,
  #     a Below_LOQ row already has a real, non-NA, positive quantified
  #     concentration (GCMSQuantitation still computes concentration for
  #     Below_LOQ -- "signal is present, just low precision"). Checked EVERY
  #     petn_snr_flag=="Below_LOQ" and rdx_snr_flag=="Below_LOQ" occurrence
  #     across all Analysis runs in this study (not just the
  #     previously-failing ones): PETN's max is 1.10% recovery (PILOT_032),
  #     RDX's max is 0.17% -- both negligible relative to this study's 10%
  #     "meaningful difference" threshold, consistent with Below_LOQ's own
  #     definition (SNR 3-10, barely above the noise floor by construction --
  #     a large concentration there would be a near-contradiction). Since
  #     the concentration is real (not NA), the flag must be checked BEFORE
  #     the concentration-based branches below, or the real small value
  #     would be used instead of being forced to 0.
  # For this study, a non-detection or negligible trace is itself a
  # meaningful result under some experimental conditions (e.g. low pressure /
  # dry swab) and must not be dropped as missing data nor (see
  # compute_injection_acceptance() in the shared InjectionAcceptance.R below)
  # treated as an analytical failure.
  petn_snr_flag_col <- if ("petn_snr_flag" %in% names(samples)) samples$petn_snr_flag else rep(NA_character_, nrow(samples))
  rdx_snr_flag_col  <- if ("rdx_snr_flag"  %in% names(samples)) samples$rdx_snr_flag  else rep(NA_character_, nrow(samples))

  petn_treat_as_zero <- petn_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")

  # Recovery (%) = (concentration / ideal_concentration) / (extraction *
  # filtration efficiency) * 100 -- matches GCMSQuantitation/GlobalCode.R's
  # + FINEX's 04_CollateStudyResults.R's "(mass / deposit) / (extraction *
  # filtration) * 100" formula exactly (concentration/ideal_concentration
  # IS mass/deposit here: ideal_concentration = deposit_mass_ng /
  # sample_vol_uL, so concentration/ideal_concentration = concentration *
  # sample_vol_uL / deposit_mass_ng = mass/deposit). Efficiency terms are
  # both 1 currently (see USER CONFIGURATION above), so this is numerically
  # identical to the pre-existing formula until real values are determined.
  samples$PETN_Recovery_pct <- ifelse(
    petn_treat_as_zero,
    0,
    ifelse(
      !is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc > 0,
      ((samples$petn_concentration_dc / ideal_concentration) /
         (petn_extraction_efficiency * petn_filtration_efficiency)) * 100,
      ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc <= 0, 0, NA_real_)
    )
  )

  rdx_treat_as_zero <- rdx_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")

  samples$RDX_Recovery_pct <- ifelse(
    rdx_treat_as_zero,
    0,
    ifelse(
      !is.na(samples$rdx_concentration) & samples$rdx_concentration > 0,
      ((samples$rdx_concentration / ideal_concentration) /
         (rdx_extraction_efficiency * rdx_filtration_efficiency)) * 100,
      ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration <= 0, 0, NA_real_)
    )
  )

  #-----------------------------------------------------------------------------
  # 6. MERGE PILOT SAMPLES WITH EXPERIMENTAL DESIGN MATRIX
  #-----------------------------------------------------------------------------

  design <- read.csv(design_file, stringsAsFactors = FALSE)

  batch_design <- read.csv(batch_design_file, stringsAsFactors = FALSE) %>%
    select(RunID = SampleID, BatchNumber) %>%
    distinct()

  design_full <- design %>%
    left_join(batch_design, by = "RunID") %>%
    # Drop the design template's placeholder recovery columns (all NA) before
    # joining -- otherwise they collide with the real PETN_Recovery_pct/
    # RDX_Recovery_pct already computed on `samples` (step 5) and dplyr
    # silently suffixes both to .x/.y, dropping the actual values.
    select(-any_of(c("PETN_Recovery_pct", "RDX_Recovery_pct")))

  pilot_rows <- samples %>%
    filter(IsPilotSample) %>%
    left_join(design_full, by = c("SampleName_trimmed" = "RunID"))

  n_unmatched <- sum(is.na(pilot_rows$SurfaceID))
  if (n_unmatched > 0) {
    warning(n_unmatched, " pilot sample row(s) could not be matched to the design matrix: ",
            paste(unique(pilot_rows$SampleName_trimmed[is.na(pilot_rows$SurfaceID)]), collapse = ", "))
  }

  #-----------------------------------------------------------------------------
  # 7. COMPUTE analysis_accepted / Outcome
  #    compute_injection_acceptance() now lives in the SHARED
  #    GCMSQuantitation/Code/InjectionAcceptance.R (Sections C+D of
  #    Improved_QC_Flowchart.dot) -- called below once pilot samples and NC
  #    rows are combined. Do not re-add a local copy of this logic here.
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("COMPUTING QC ACCEPTANCE\n")
  cat("========================================\n\n")

  #-----------------------------------------------------------------------------
  # 7b. HANDLE REANALYSIS (same sample analysed in more than one Analysis run)
  #
  # If a sample failed QC, it may be re-run in a later Analysis folder
  # (Analysis4, Analysis5, ...). Since all_data is built from every
  # Analysis*/Results/*_GCMSResults.csv found, the same SampleName_trimmed
  # (RunID for pilot samples, or the raw NC name) can appear multiple times.
  # Without deduplication this would: (a) silently pick whichever attempt
  # happened to be LAST in file-discovery order when writing into
  # design_data (list.files() sorts alphabetically -- "Analysis10" sorts
  # before "Analysis2", so this is not even reliably "most recent" once
  # there are 10+ runs), and (b) double-count the sample in
  # all_data_sheet/Summary_By_Condition/SampleSummary if more than one
  # attempt happens to pass QC.
  #
  # Resolution: keep exactly one row per SampleName_trimmed --
  # preferring an accepted attempt (PASS/PASS* for samples,
  # nc_analysis_accepted %in% c("PASS","PASS*") for NCs) over a failed one, and breaking ties
  # by the EARLIEST GC-MS analysis Date among accepted attempts (not
  # file-discovery order, and -- as of August 11, 2026 -- not "most recent"
  # either; see below).
  #
  # Tie-break direction (August 11, 2026): changed from "most recent
  # accepted attempt wins" to "earliest accepted attempt wins", following
  # the RDX batch-effect investigation (CONTEXT.md, "Does Reanalysis
  # Selection Explain the RDX Batch Effect?" session). Checked directly
  # against all 17 reanalysed pilot samples at the time: every single
  # rejected/earlier attempt's own RDX 6ng/0.2ng QC brackets (pre AND post)
  # were PASS -- rejections were 100% driven by a PETN-specific QC bracket
  # failure (11/17) or pure date tie-break with no quality difference at all
  # (9/17, both attempts already PASS/PASS*); RDX was never the reason an
  # earlier attempt was rejected. Since RDX has no drift correction in this
  # pipeline and its quantitation bias measurably worsens at later calendar
  # dates (documented separately), "most recent PASS" was systematically
  # picking the MORE RDX-biased of two equally-valid-for-RDX attempts, for
  # reasons that only ever concerned PETN. "Earliest PASS" removes this bias
  # with no corresponding downside (RDX's own QC evidence never favoured the
  # later attempt in any of the 17 cases checked). Deliberately NOT split
  # into a per-analyte selection (i.e. still one selected attempt per
  # sample, applied to both PETN and RDX alike) -- per explicit user
  # decision, simplicity of a single whole-sample selection was preferred
  # over the extra complexity of decoupling PETN's and RDX's own attempt
  # choice.
  # `N_Attempts`/`MultipleAttempts` columns retain visibility that a re-run
  # happened even though only the selected attempt is used downstream.
  #-----------------------------------------------------------------------------

  select_best_attempt <- function(df, quality_ok) {
    if (nrow(df) == 0) {
      df$N_Attempts <- integer(0)
      df$MultipleAttempts <- logical(0)
      return(df)
    }
    df$..quality_ok <- quality_ok
    df %>%
      group_by(SampleName_trimmed) %>%
      mutate(N_Attempts = n()) %>%
      ungroup() %>%
      arrange(SampleName_trimmed, desc(..quality_ok), Date) %>%
      group_by(SampleName_trimmed) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(MultipleAttempts = N_Attempts > 1) %>%
      select(-..quality_ok)
  }

  report_multiple_attempts <- function(df, label) {
    ids <- unique(df$SampleName_trimmed[df$MultipleAttempts])
    if (length(ids) > 0) {
      cat(length(ids), label, "had multiple analysis attempts -- using earliest accepted attempt (or earliest overall if none were accepted):\n")
      for (id in ids) cat("  ", id, "\n")
      cat("\n")
    }
  }

  # Combine pilot samples + NC rows for acceptance computation.
  # compute_injection_acceptance() (shared, Sections C+D) evaluates BOTH
  # identically now -- there is no more structural difference between a
  # pilot sample and an NC (the old 0.2ng-bracket-FAIL override, the only
  # thing that used to distinguish them, has been removed entirely; see
  # GCMSQuantitation/Diagnostics/Improved_QC_Flowchart.dot). It also
  # computes Outcome and the per-analyte result tiers (petn_result_tier/
  # rdx_result_tier) in the same call, needed below to derive nc_result.
  nc_rows_raw <- samples %>% filter(IsNegativeControl)

  combined_for_accept <- bind_rows(
    pilot_rows %>% mutate(RowType = "Sample"),
    nc_rows_raw %>% mutate(RowType = "NC")
  )

  combined_for_accept <- compute_injection_acceptance(combined_for_accept)

  pilot_rows <- combined_for_accept %>% filter(RowType == "Sample")
  nc_rows    <- combined_for_accept %>% filter(RowType == "NC")

  # Deduplicate pilot samples analysed more than once (see section 7b above).
  pilot_rows <- select_best_attempt(pilot_rows, quality_ok = pilot_rows$analysis_accepted %in% c("PASS", "PASS*"))
  report_multiple_attempts(pilot_rows, "pilot sample(s)")

  n_pass      <- sum(pilot_rows$analysis_accepted == "PASS", na.rm = TRUE)
  n_pass_star <- sum(pilot_rows$analysis_accepted == "PASS*", na.rm = TRUE)
  n_fail      <- sum(grepl("^FAIL:", pilot_rows$analysis_accepted), na.rm = TRUE)
  cat("Pilot sample acceptance: PASS=", n_pass, " PASS*=", n_pass_star, " FAIL=", n_fail, "\n\n", sep = "")

  #-----------------------------------------------------------------------------
  # 8. DERIVE NC ANALYSIS ACCEPTANCE (Stage 1) / RESULT (Stage 2)
  #    Both now come from the SAME compute_injection_acceptance() call above
  #    (Sections C+D, GCMSQuantitation/Code/InjectionAcceptance.R):
  #      STAGE 1 -- nc_analysis_accepted: simply nc_rows$analysis_accepted,
  #                 aliased under its NC-sheet column name for backward
  #                 compatibility. Was this injection/run analytically
  #                 trustworthy?
  #      STAGE 2 -- nc_result: derived from the per-analyte result tiers via
  #                 derive_nc_result() (shared script) -- maps Quantifiable/
  #                 Trace/Negative to the NC contamination vocabulary
  #                 (contaminated/trace detected/clean), forced to "Not
  #                 evaluated (analysis failed)" whenever Stage 1 is a FAIL.
  #-----------------------------------------------------------------------------

  if (nrow(nc_rows) > 0) {
    nc_rows$nc_analysis_accepted <- nc_rows$analysis_accepted
    nc_rows$nc_result            <- derive_nc_result(nc_rows$petn_result_tier, nc_rows$rdx_result_tier,
                                                       nc_rows$nc_analysis_accepted)

    # Deduplicate NCs analysed more than once (see section 7b above) --
    # prefer an accepted attempt over a failed one, tie-break by most recent
    # Date. Sensitive to 6ng bracket failures too, since those are included
    # in nc_analysis_accepted.
    nc_rows <- select_best_attempt(nc_rows, quality_ok = nc_rows$nc_analysis_accepted %in% c("PASS", "PASS*"))
    report_multiple_attempts(nc_rows, "negative control(s)")

    n_nc_pass       <- sum(nc_rows$nc_analysis_accepted == "PASS", na.rm = TRUE)
    n_nc_pass_star  <- sum(nc_rows$nc_analysis_accepted == "PASS*", na.rm = TRUE)
    n_nc_fail       <- sum(grepl("^FAIL:", nc_rows$nc_analysis_accepted), na.rm = TRUE)
    cat("NC analysis acceptance (Stage 1 -- is the run trustworthy?): PASS=", n_nc_pass,
        " PASS*=", n_nc_pass_star, " FAIL=", n_nc_fail, "\n", sep = "")

    n_nc_negative     <- sum(nc_rows$nc_result == "Negative (clean)", na.rm = TRUE)
    n_nc_positive     <- sum(grepl("^Positive:", nc_rows$nc_result), na.rm = TRUE)
    n_nc_not_eval     <- sum(nc_rows$nc_result == "Not evaluated (analysis failed)", na.rm = TRUE)
    cat("NC result (Stage 2 -- contamination outcome, valid runs only): Negative=", n_nc_negative,
        " Positive=", n_nc_positive, " Not evaluated=", n_nc_not_eval, "\n\n", sep = "")
  }

  #-----------------------------------------------------------------------------
  # STYLES AND SHEET-WRITING HELPER (conditional formatting so PASS/FAIL/
  # WARN status is immediately visible at a glance). Defined once here so it
  # can be reused for both pilot_data_nested.xlsx (step 9) and
  # ASTRA_PilotStudyResults.xlsx (step 10).
  #-----------------------------------------------------------------------------

  style_green  <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  style_amber  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C5700")
  style_red    <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
  style_grey   <- createStyle(fgFill = "#D9D9D9", fontColour = "#000000")
  style_header <- createStyle(textDecoration = "Bold", border = "Bottom",
                               borderColour = "#000000", fgFill = "#4472C4", fontColour = "#FFFFFF")

  add_sheet <- function(wb, sheet_name, data) {
    addWorksheet(wb, sheet_name)
    if (nrow(data) == 0) {
      writeData(wb, sheet_name, data.frame(Note = "No data available"))
      return(invisible())
    }
    writeData(wb, sheet_name, data, headerStyle = style_header)
    freezePane(wb, sheet_name, firstRow = TRUE)
    setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")

    col_names <- names(data)
    data_rows <- 2:(nrow(data) + 1)

    if ("analysis_accepted" %in% col_names) {
      col_idx <- which(col_names == "analysis_accepted")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "FAIL:", style = style_red)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "PASS*", style = style_amber)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                             type = "expression", rule = paste0('EXACT($', openxlsx::int2col(col_idx), '2,"PASS")'), style = style_green)
    }
    if ("Outcome" %in% col_names) {
      col_idx <- which(col_names == "Outcome")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Complete", style = style_green)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Reanalyse", style = style_red)
    }
    for (col in c("petn_qc_6ng_pre", "petn_qc_6ng_post", "rdx_qc_6ng_pre", "rdx_qc_6ng_post")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "PASS", style = style_green)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "FAIL", style = style_red)
      }
    }
    for (col in c("petn_qc_02ng_pre", "petn_qc_02ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "PASS", style = style_green)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "WARN", style = style_amber)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "FAIL", style = style_red)
      }
    }
    for (col in c("petn_snr_flag", "rdx_snr_flag")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Quantifiable", style = style_green)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Below_LOQ", style = style_amber)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Below_LOD", style = style_red)
      }
    }
    # Carryover-risk evidence behind a "peak may be due to carryover" FAIL
    # (Section A/C, GCMSQuantitation/Code/InjectionAcceptance.R)
    for (col in c("petn_blank_bracket", "rdx_blank_bracket")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Clean", style = style_green)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Trace", style = style_amber)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Contaminated", style = style_red)
      }
    }
    if ("rdx_is_pa_flag" %in% col_names) {
      col_idx <- which(col_names == "rdx_is_pa_flag")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "OK", style = style_green)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "LOW", style = style_amber)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "NOT_DETECTED", style = style_red)
    }
    if ("nc_analysis_accepted" %in% col_names) {
      col_idx <- which(col_names == "nc_analysis_accepted")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "FAIL:", style = style_red)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "PASS*", style = style_amber)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                             type = "expression", rule = paste0('EXACT($', openxlsx::int2col(col_idx), '2,"PASS")'), style = style_green)
    }
    if ("nc_result" %in% col_names) {
      col_idx <- which(col_names == "nc_result")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Not evaluated", style = style_grey)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "contaminated", style = style_red)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "trace detected", style = style_amber)
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Negative", style = style_green)
    }
    if ("MultipleAttempts" %in% col_names) {
      col_idx <- which(col_names == "MultipleAttempts")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "TRUE", style = style_amber)
    }
  }

  #-----------------------------------------------------------------------------
  # 9. UPDATE pilot_data_nested.csv + pilot_data_nested.xlsx (canonical
  #    location: data_file). The .xlsx sibling carries the same conditional
  #    formatting as ASTRA_PilotStudyResults.xlsx so PASS/PASS*/FAIL status
  #    is immediately visible at a glance (CSV cannot carry cell formatting).
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("UPDATING pilot_data_nested.csv\n")
  cat("========================================\n\n")

  design_data <- design  # original design template, all 32 rows, RunID present

  # Ensure new columns exist in the design template
  for (col in c("analysis_accepted", "Outcome", "BatchNumber",
                "petn_snr_flag", "rdx_snr_flag", "rdx_is_pa_flag",
                "petn_qc_6ng_pre", "petn_qc_6ng_post", "petn_qc_02ng_pre", "petn_qc_02ng_post",
                "rdx_qc_6ng_pre", "rdx_qc_6ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
                "GCMS_Analysis_Date", "AnalysisFolder", "SampleType",
                "nc_analysis_accepted", "nc_result", "N_Attempts", "MultipleAttempts")) {
    if (!(col %in% names(design_data))) design_data[[col]] <- NA
  }
  design_data$SampleType <- "Pilot"  # all 32 design-matrix rows are pilot samples

  n_updated <- 0
  for (i in seq_len(nrow(pilot_rows))) {
    run_id <- pilot_rows$SampleName_trimmed[i]
    match_row <- which(design_data$RunID == run_id)
    if (length(match_row) == 1) {
      design_data$PETN_Recovery_pct[match_row] <- pilot_rows$PETN_Recovery_pct[i]
      design_data$RDX_Recovery_pct[match_row]  <- pilot_rows$RDX_Recovery_pct[i]
      design_data$analysis_accepted[match_row] <- pilot_rows$analysis_accepted[i]
      design_data$Outcome[match_row]           <- pilot_rows$Outcome[i]
      design_data$BatchNumber[match_row]       <- pilot_rows$BatchNumber[i]
      design_data$petn_snr_flag[match_row]     <- pilot_rows$petn_snr_flag[i]
      design_data$rdx_snr_flag[match_row]      <- pilot_rows$rdx_snr_flag[i]
      design_data$rdx_is_pa_flag[match_row]    <- if ("rdx_is_pa_flag" %in% names(pilot_rows)) pilot_rows$rdx_is_pa_flag[i] else NA_character_
      design_data$petn_qc_6ng_pre[match_row]   <- pilot_rows$petn_qc_6ng_pre[i]
      design_data$petn_qc_6ng_post[match_row]  <- pilot_rows$petn_qc_6ng_post[i]
      design_data$petn_qc_02ng_pre[match_row]  <- pilot_rows$petn_qc_02ng_pre[i]
      design_data$petn_qc_02ng_post[match_row] <- pilot_rows$petn_qc_02ng_post[i]
      design_data$rdx_qc_6ng_pre[match_row]    <- pilot_rows$rdx_qc_6ng_pre[i]
      design_data$rdx_qc_6ng_post[match_row]   <- pilot_rows$rdx_qc_6ng_post[i]
      design_data$rdx_qc_02ng_pre[match_row]   <- pilot_rows$rdx_qc_02ng_pre[i]
      design_data$rdx_qc_02ng_post[match_row]  <- pilot_rows$rdx_qc_02ng_post[i]
      design_data$GCMS_Analysis_Date[match_row] <- pilot_rows$Date[i]
      design_data$AnalysisFolder[match_row]    <- pilot_rows$AnalysisFolder[i]
      design_data$N_Attempts[match_row]        <- pilot_rows$N_Attempts[i]
      design_data$MultipleAttempts[match_row]  <- pilot_rows$MultipleAttempts[i]
      n_updated <- n_updated + 1
    } else {
      warning("No unique match in design for sample '", run_id, "'")
    }
  }

  # Append negative control rows (analogous to the FINEX pipeline's Negative
  # Controls sheet, but included directly in pilot_data_nested rather than a
  # separate sheet only, per user request). NCs are not part of the design
  # matrix (no RunID/SurfaceID/Pressure/Solvent), so design-specific columns
  # are left NA for these rows; NC-specific identity/status columns are
  # populated instead (NC_Surface_type -> Surface_type, NC_BatchNumber ->
  # BatchNumber, nc_analysis_accepted, nc_result).
  if (nrow(nc_rows) > 0) {
    nc_extra_rows <- data.frame(
      RunID                  = nc_rows$SampleName_trimmed,
      SampleID               = nc_rows$SampleName_trimmed,
      SurfaceProcessingOrder = NA_integer_,
      SurfaceID              = NA_character_,
      Surface_type           = nc_rows$NC_Surface_type,
      TestOrder              = NA_integer_,
      Pressure_level         = NA_character_,
      Pressure_g             = NA_real_,
      Solvent_level          = NA_character_,
      Solvent_detail         = NA_character_,
      PETN_Recovery_pct      = nc_rows$PETN_Recovery_pct,
      RDX_Recovery_pct       = nc_rows$RDX_Recovery_pct,
      Test_DateTime          = NA_character_,
      Operator               = NA_character_,
      Temp_C                 = NA_real_,
      Humidity_pct           = NA_real_,
      Notes                  = NA_character_,
      analysis_accepted      = nc_rows$analysis_accepted,
      Outcome                = nc_rows$Outcome,
      BatchNumber            = nc_rows$NC_BatchNumber,
      petn_snr_flag          = nc_rows$petn_snr_flag,
      rdx_snr_flag           = nc_rows$rdx_snr_flag,
      rdx_is_pa_flag         = if ("rdx_is_pa_flag" %in% names(nc_rows)) nc_rows$rdx_is_pa_flag else NA_character_,
      petn_qc_6ng_pre        = nc_rows$petn_qc_6ng_pre,
      petn_qc_6ng_post       = nc_rows$petn_qc_6ng_post,
      petn_qc_02ng_pre       = nc_rows$petn_qc_02ng_pre,
      petn_qc_02ng_post      = nc_rows$petn_qc_02ng_post,
      rdx_qc_6ng_pre         = nc_rows$rdx_qc_6ng_pre,
      rdx_qc_6ng_post        = nc_rows$rdx_qc_6ng_post,
      rdx_qc_02ng_pre        = nc_rows$rdx_qc_02ng_pre,
      rdx_qc_02ng_post       = nc_rows$rdx_qc_02ng_post,
      GCMS_Analysis_Date     = nc_rows$Date,
      AnalysisFolder         = nc_rows$AnalysisFolder,
      SampleType             = "NC",
      nc_analysis_accepted   = nc_rows$nc_analysis_accepted,
      nc_result              = nc_rows$nc_result,
      N_Attempts             = nc_rows$N_Attempts,
      MultipleAttempts       = nc_rows$MultipleAttempts,
      stringsAsFactors = FALSE
    )

    design_data <- bind_rows(design_data, nc_extra_rows)
    cat("Negative controls appended:", nrow(nc_extra_rows), "\n")
  }

  # Sort by GC-MS analysis date (dplyr::arrange places NA last automatically,
  # so samples not yet analyzed stay at the bottom); RunID as a stable tiebreaker
  design_data <- design_data %>% arrange(GCMS_Analysis_Date, RunID)

  write.csv(design_data, data_file, row.names = FALSE, na = "NA")

  cat("Samples updated:", n_updated, "/", nrow(design), "\n")
  cat("Written to:", data_file, "\n")

  # Formatted .xlsx sibling (same conditional formatting as ASTRA_PilotStudyResults.xlsx)
  data_file_xlsx <- sub("\\.csv$", ".xlsx", data_file)
  wb_pilot_data <- createWorkbook()
  add_sheet(wb_pilot_data, "pilot_data_nested", design_data)
  saveWorkbook(wb_pilot_data, data_file_xlsx, overwrite = TRUE)
  cat("Written to:", data_file_xlsx, "\n\n")

  #-----------------------------------------------------------------------------
  # 10. BUILD ASTRA_PilotStudyResults.xlsx
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("BUILDING ASTRA_PilotStudyResults.xlsx\n")
  cat("========================================\n\n")

  desired_cols <- c(
    "RunID", "SampleID", "BatchNumber", "SurfaceID", "Surface_type", "TestOrder",
    "Pressure_level", "Pressure_g", "Solvent_level", "Solvent_detail",
    "analysis_accepted", "Outcome", "rdx_is_pa_flag", "N_Attempts", "MultipleAttempts",
    "petn_snr_flag", "petn_concentration_dc", "PETN_Recovery_pct",
    "petn_qc_6ng_pre", "petn_qc_6ng_post", "petn_qc_02ng_pre", "petn_qc_02ng_post",
    "rdx_snr_flag", "rdx_concentration", "RDX_Recovery_pct",
    "rdx_qc_6ng_pre", "rdx_qc_6ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
    "petn_blank_bracket", "rdx_blank_bracket",
    "Date", "SampleName_trimmed", "DataFile", "Dataset", "AnalysisFolder", "SourceFile"
  )

  pilot_rows_named <- pilot_rows %>%
    rename(RunID = SampleName_trimmed) %>%
    { if (!("RunID" %in% names(.))) mutate(., RunID = NA_character_) else . }

  available_cols <- intersect(desired_cols, names(pilot_rows_named))
  all_data_sheet <- pilot_rows_named %>%
    select(all_of(available_cols)) %>%
    arrange(Date, Dataset, Surface_type, SurfaceID, TestOrder)

  steel_sheet <- all_data_sheet %>% filter(Surface_type == "steel")
  abs_sheet   <- all_data_sheet %>% filter(Surface_type == "abs")

  desired_cols_nc <- c(
    "NC_BatchNumber", "NC_Surface_type", "nc_analysis_accepted", "nc_result", "N_Attempts", "MultipleAttempts",
    "petn_snr_flag", "petn_ph", "petn_concentration_dc", "PETN_Recovery_pct",
    "rdx_snr_flag", "rdx_ph", "rdx_concentration", "RDX_Recovery_pct",
    "rdx_is_snr_flag", "rdx_is_pa_flag",
    "petn_qc_6ng_pre", "petn_qc_6ng_post", "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
    "petn_qc_02ng_pre", "petn_qc_02ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
    "petn_blank_bracket", "rdx_blank_bracket",
    "Date", "SampleName_trimmed", "DataFile", "Dataset", "AnalysisFolder", "SourceFile"
  )
  available_cols_nc <- intersect(desired_cols_nc, names(nc_rows))
  nc_sheet <- if (nrow(nc_rows) > 0) {
    nc_rows %>% select(all_of(available_cols_nc)) %>% arrange(Date, Dataset, NC_Surface_type, NC_BatchNumber)
  } else {
    data.frame()
  }

  # --- Summary by condition (Surface_type x Pressure x Solvent) ---
  summary_sheet <- all_data_sheet %>%
    filter(analysis_accepted %in% c("PASS", "PASS*")) %>%
    group_by(Surface_type, Pressure_level, Solvent_level) %>%
    summarise(
      N = n(),
      PETN_Mean = mean(PETN_Recovery_pct, na.rm = TRUE),
      PETN_SD   = sd(PETN_Recovery_pct, na.rm = TRUE),
      PETN_RSD_pct = ifelse(PETN_Mean != 0, 100 * PETN_SD / PETN_Mean, NA_real_),
      RDX_Mean  = mean(RDX_Recovery_pct, na.rm = TRUE),
      RDX_SD    = sd(RDX_Recovery_pct, na.rm = TRUE),
      RDX_RSD_pct = ifelse(RDX_Mean != 0, 100 * RDX_SD / RDX_Mean, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(Surface_type, Pressure_level, Solvent_level)

  # --- SampleSummary ---
  surface_summary <- all_data_sheet %>%
    group_by(Surface_type) %>%
    summarise(
      Total = n(),
      PASS  = sum(analysis_accepted == "PASS", na.rm = TRUE),
      `PASS*` = sum(analysis_accepted == "PASS*", na.rm = TRUE),
      FAIL  = sum(grepl("^FAIL:", analysis_accepted), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`Complete %` = round(100 * (PASS + `PASS*`) / expected_total_per_surface, 1)) %>%
    rename(Row = Surface_type)

  for (s in c("steel", "abs")) {
    if (!(s %in% surface_summary$Row)) {
      surface_summary <- bind_rows(surface_summary,
        data.frame(Row = s, Total = 0L, PASS = 0L, `PASS*` = 0L, FAIL = 0L, `Complete %` = 0.0, check.names = FALSE))
    }
  }
  surface_summary <- surface_summary %>% arrange(match(Row, c("steel", "abs")))

  nc_summary_row <- if (nrow(nc_rows) > 0) {
    nc_stage1_ok <- nc_rows$nc_analysis_accepted %in% c("PASS", "PASS*")
    data.frame(
      Row = "NC",
      Total = nrow(nc_rows),
      PASS  = sum(nc_stage1_ok & nc_rows$nc_result == "Negative (clean)", na.rm = TRUE),
      `PASS*` = sum(nc_stage1_ok & grepl("^Positive:", nc_rows$nc_result), na.rm = TRUE),
      FAIL  = sum(!nc_stage1_ok, na.rm = TRUE),
      check.names = FALSE
    )
  } else {
    data.frame(Row = "NC", Total = 0L, PASS = 0L, `PASS*` = 0L, FAIL = 0L, check.names = FALSE)
  }
  nc_summary_row$`Complete %` <- round(100 * (nc_summary_row$PASS + nc_summary_row$`PASS*`) / expected_total_nc, 1)

  total_row <- data.frame(
    Row   = "TOTAL",
    Total = sum(surface_summary$Total),
    PASS  = sum(surface_summary$PASS),
    `PASS*` = sum(surface_summary$`PASS*`),
    FAIL  = sum(surface_summary$FAIL),
    check.names = FALSE
  )
  total_row$`Complete %` <- round(100 * (total_row$PASS + total_row$`PASS*`) / expected_total_samples, 1)

  successfully_run_count <- total_row$PASS + total_row$`PASS*` + nc_summary_row$PASS + nc_summary_row$`PASS*`
  successfully_run_row <- data.frame(
    Row = "Successfully Run", Total = successfully_run_count,
    PASS = NA_integer_, `PASS*` = NA_integer_, FAIL = NA_integer_, `Complete %` = NA_real_,
    check.names = FALSE
  )

  sample_summary_sheet <- bind_rows(surface_summary, nc_summary_row, total_row, successfully_run_row)

  # --- CalSummary ---
  cal_summary_sheet <- all_cal_stats

  #-----------------------------------------------------------------------------
  # WRITE XLSX (styles + add_sheet() helper already defined before step 9)
  #-----------------------------------------------------------------------------

  wb <- createWorkbook()

  add_sheet(wb, "All Data", all_data_sheet)
  add_sheet(wb, "Steel", steel_sheet)
  add_sheet(wb, "ABS", abs_sheet)
  add_sheet(wb, "Negative Controls", nc_sheet)
  add_sheet(wb, "Summary_By_Condition", as.data.frame(summary_sheet))
  add_sheet(wb, "SampleSummary", sample_summary_sheet)
  add_sheet(wb, "CalSummary", as.data.frame(cal_summary_sheet))

  saveWorkbook(wb, xlsx_out, overwrite = TRUE)

  cat("Written to:", xlsx_out, "\n\n")

  cat("========================================\n")
  cat("COLLATION COMPLETE\n")
  cat("========================================\n\n")

  print(sample_summary_sheet)

  invisible(NULL)
}

cat("\n========================================\n")
cat("DATA EXTRACTION FROM GC-MS RESULTS\n")
cat("========================================\n\n")

tryCatch({
  collate_gcms_results()
  cat("\n")
}, error = function(e) {
  cat("Data collation failed:\n  ", conditionMessage(e), "\n")
  cat("Continuing with existing pilot_data_nested.csv (if available).\n\n")
})

#===============================================================================
# BOX PLOT FUNCTION (used in both modes)
#===============================================================================

generate_boxplots <- function(data, analyte_name, recovery_col, output_dir,
                               file_suffix = "", title_suffix = "", show_points = FALSE) {

  # Filter to rows with valid recovery data
  data_valid <- data[!is.na(data[[recovery_col]]), ]
  n_valid <- nrow(data_valid)
  n_total <- nrow(data)

  cat(sprintf("  %s: %d/%d samples have data\n", analyte_name, n_valid, n_total))

  if (n_valid == 0) {
    cat(sprintf("  Skipping %s box plots - no data available\n\n", analyte_name))
    return(invisible(NULL))
  }

  # When show_points is set (the "all data" plots -- QC filter ignored),
  # drop NC and "Untested" rows entirely (Updated 2026-08-06, per explicit
  # request) rather than plotting them: NC has no real factorial Condition
  # to begin with, and "Untested" (analysis_accepted is NA) isn't part of
  # the design either. Restricts these plots to PASS/PASS*/FAIL pilot
  # samples only -- still shows every QC outcome for the actual factorial
  # design, just not NC/Untested rows that were never really part of it.
  # Computed BEFORE Condition/means_df/counts_df below so those are also
  # built only from the retained rows.
  # NC is identified via SampleType (not analysis_accepted -- under the
  # unified compute_injection_acceptance(), NC rows get a real PASS/PASS*/
  # FAIL value like everyone else, so a literal "NC" string is never
  # produced any more; SampleType is the authoritative identifier).
  if (show_points) {
    is_untested <- "analysis_accepted" %in% names(data_valid) & is.na(data_valid$analysis_accepted)
    is_nc <- if ("SampleType" %in% names(data_valid)) {
      data_valid$SampleType == "NC"
    } else {
      rep(FALSE, nrow(data_valid))
    }
    data_valid <- data_valid[!(is_untested | is_nc), ]
  }

  if (nrow(data_valid) == 0) {
    cat(sprintf("  Skipping %s box plots - no data available after excluding NC/Untested\n\n", analyte_name))
    return(invisible(NULL))
  }

  # Create condition labels from Pressure and Solvent levels. NC rows (no
  # Pressure/Solvent -- not part of the factorial design) are already
  # excluded above when show_points is set; for the non-show_points calls
  # NC never appears in the input data in the first place (already excluded
  # upstream by the QC acceptance filter), so no special-case is needed here.
  condition_raw <- paste0(
    ifelse(data_valid$Pressure_level == "low", "50g", "200g"),
    "/",
    ifelse(data_valid$Solvent_level == "absent", "dry", "wet")
  )
  data_valid$Condition <- factor(
    condition_raw,
    levels = c("50g/dry", "50g/wet", "200g/dry", "200g/wet")
  )

  # Calculate means per condition per surface type (for red diamond)
  means_df <- aggregate(
    data_valid[[recovery_col]],
    by = list(Condition = data_valid$Condition, Surface_type = data_valid$Surface_type),
    FUN = mean, na.rm = TRUE
  )
  names(means_df)[3] <- "Mean"

  # Sample size per condition per surface type, for "n=" labels above each
  # box (Added August 2026 -- mirrors the n= labels already used in
  # GCMSQuantitation's Code/04_CollateStudyResults.R create_boxplot()).
  counts_df <- aggregate(
    data_valid[[recovery_col]],
    by = list(Condition = data_valid$Condition, Surface_type = data_valid$Surface_type),
    FUN = length
  )
  names(counts_df)[3] <- "n"

  # Outlier flag per Condition x Surface_type group (standard 1.5xIQR rule --
  # matches what the box itself would flag if outlier.shape weren't
  # suppressed below), needed by geom_jitter's aes(shape=...) further down.
  # MUST be computed before the ggplot(data_valid, ...) call immediately
  # below -- ggplot() captures data_valid by value at call time, so adding
  # a column to data_valid afterward would not propagate into the already-
  # built plot object (same pitfall previously documented for
  # AcceptanceStatus in this file's July 30, 2026 bug-fix session).
  if (show_points) {
    compute_outlier_flag <- function(x) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      x < lower | x > upper
    }
    data_valid$IsOutlier <- as.logical(ave(data_valid[[recovery_col]],
                                 data_valid$Condition, data_valid$Surface_type,
                                 FUN = compute_outlier_flag))
  }

  # Generate box plot: faceted by Surface_type, 4 boxes per facet
  p <- ggplot(data_valid, aes(x = Condition, y = .data[[recovery_col]])) +
    geom_boxplot(outlier.shape = NA, width = 0.5)

  # Overlay individual points (Updated 2026-08-11: smaller, blue points for
  # clarity, with outliers -- per the standard 1.5xIQR box plot rule computed
  # above -- shown as a different shape (triangle) from normal points
  # (circle) so they're visually distinguishable at a glance. NC/Untested
  # rows are already excluded above, so PASS/PASS*/FAIL points are shown
  # without distinguishing colour by QC status (per the prior, still-
  # standing "uniform colour" decision).
  if (show_points) {
    p <- p +
      geom_jitter(aes(shape = IsOutlier), width = 0.15, height = 0,
                  size = 1.3, alpha = 0.85, colour = "blue") +
      scale_shape_manual(
        values = c(`FALSE` = 16, `TRUE` = 17),
        labels = c(`FALSE` = "Normal", `TRUE` = "Outlier (>1.5xIQR)"),
        name = "Sample"
      )
  }

  # Fixed y-axis range (Added August 2026): 0-50% recovery, hardcoded rather
  # than the previous 0-100%. Real pilot data currently tops out around
  # ~25-29% recovery for both analytes, so 0-100% left most of the frame
  # empty; 0-50% "zooms in" while still leaving headroom for values higher
  # than observed so far. coord_cartesian() only clips the *view* -- box/
  # mean statistics are still computed from the full underlying data.
  y_axis_max <- 50
  label_y <- y_axis_max * 0.96  # fixed position near the top of the frame, for n= labels

  p <- p +
    geom_point(data = means_df, aes(x = Condition, y = Mean),
               shape = 18, size = 4, colour = "red") +
    geom_text(
      data = counts_df,
      aes(x = Condition, y = label_y, label = paste0("n=", n)),
      size = 3,
      vjust = 0,  # vjust=0 aligns text bottom with y position (label sits above)
      color = "black",
      inherit.aes = FALSE
    ) +
    facet_wrap(~ Surface_type, labeller = labeller(Surface_type = c("steel" = "Steel", "abs" = "ABS"))) +
    coord_cartesian(ylim = c(0, y_axis_max)) +
    labs(
      title = paste0(analyte_name, " Recovery by Condition", title_suffix),
      x = "Condition",
      y = "Recovery (%)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )

  # Save plot (recommended: 8x4 inches — two facets side by side, compact)
  out_file <- file.path(output_dir, paste0(analyte_name, "_pilot_boxplots", file_suffix, ".png"))
  ggsave(out_file, p, width = 8, height = 4, dpi = 300)

  cat(sprintf("  Saved: %s\n", out_file))
  return(invisible(p))
}

#===============================================================================
# BAR CHARTS WITH SEM ERROR BARS (per condition, per analyte)
#
# Companion to the box plots above: one bar per condition (Pressure x
# Solvent), faceted by Surface_type, height = mean recovery, error bar =
# standard error of the mean (SEM = SD / sqrt(n)) for quick visual
# comparison of which conditions differ. SEM is only drawn when n > 1 (a
# single-replicate condition has an undefined SD, so no error bar is shown
# for it rather than a misleading zero-width one -- same convention used
# for the by-participant bar charts in GCMSQuantitation's
# Code/04_CollateStudyResults.R).
#===============================================================================

generate_barchart <- function(data, analyte_name, recovery_col, output_dir,
                                file_suffix = "", title_suffix = "") {

  data_valid <- data[!is.na(data[[recovery_col]]), ]

  if (nrow(data_valid) == 0) {
    cat(sprintf("  Skipping %s bar chart - no data available\n\n", analyte_name))
    return(invisible(NULL))
  }

  condition_raw <- paste0(
    ifelse(data_valid$Pressure_level == "low", "50g", "200g"),
    "/",
    ifelse(data_valid$Solvent_level == "absent", "dry", "wet")
  )
  data_valid$Condition <- factor(
    condition_raw,
    levels = c("50g/dry", "50g/wet", "200g/dry", "200g/wet")
  )
  # Negative controls have no Pressure/Solvent level, so Condition is NA for
  # them -- drop rather than plot an "NA" bar (mirrors generate_boxplots(),
  # which gives NC an explicit category only when show_points is requested).
  data_valid <- data_valid[!is.na(data_valid$Condition), ]

  if (nrow(data_valid) == 0) {
    cat(sprintf("  Skipping %s bar chart - no non-NC data available\n\n", analyte_name))
    return(invisible(NULL))
  }

  summary_df <- data_valid %>%
    group_by(Surface_type, Condition) %>%
    summarise(
      Mean = mean(.data[[recovery_col]], na.rm = TRUE),
      SD   = sd(.data[[recovery_col]], na.rm = TRUE),
      N    = sum(!is.na(.data[[recovery_col]])),
      SEM  = ifelse(N > 1, SD / sqrt(N), NA_real_),
      .groups = "drop"
    )

  ymax <- max(summary_df$Mean + ifelse(is.na(summary_df$SEM), 0, summary_df$SEM), na.rm = TRUE)

  p <- ggplot(summary_df, aes(x = Condition, y = Mean)) +
    geom_bar(stat = "identity", fill = "#4472C4", width = 0.6) +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = paste0("n=", N),
                  y = Mean + ifelse(is.na(SEM), 0, SEM) + 0.03 * ymax),
              size = 3, vjust = 0) +
    facet_wrap(~ Surface_type, labeller = labeller(Surface_type = c("steel" = "Steel", "abs" = "ABS"))) +
    coord_cartesian(ylim = c(0, ymax * 1.15)) +
    labs(
      title = paste0(analyte_name, " Mean Recovery by Condition (\u00b1 SEM)", title_suffix),
      x = "Condition",
      y = "Mean Recovery (%)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )

  out_file <- file.path(output_dir, paste0(analyte_name, "_pilot_barchart", file_suffix, ".png"))
  ggsave(out_file, p, width = 8, height = 4, dpi = 300)

  cat(sprintf("  Saved: %s\n", out_file))
  return(invisible(p))
}

#===============================================================================
# INDIVIDUAL-VALUE BAR CHARTS BY REPEAT (per condition, per analyte)
#
# Companion to generate_barchart() above (which plots the MEAN recovery per
# condition +/- SEM, one bar per condition). This version plots every
# individual sample's own recovery value directly with no aggregation --
# one bar per (Repeat, Condition) combination, faceted by Surface_type,
# with Condition as the dodged fill series and the physical surface
# replicate number along the x-axis. In this design each of the 4 physical
# surfaces per type (e.g. Steel_01..04) is tested under all 4 conditions
# exactly once, so "Repeat" (parsed from the numeric suffix of SurfaceID,
# e.g. "Steel_02" -> 2) is the natural per-condition replicate identifier --
# there is no independent within-surface replication of the same condition
# to plot instead. Useful for seeing surface-to-surface variability within
# a condition directly, rather than only its mean/SEM.
#===============================================================================

generate_repeat_barchart <- function(data, analyte_name, recovery_col, output_dir,
                                      file_suffix = "", title_suffix = "") {

  data_valid <- data[!is.na(data[[recovery_col]]), ]

  if (nrow(data_valid) == 0) {
    cat(sprintf("  Skipping %s by-repeat bar chart - no data available\n\n", analyte_name))
    return(invisible(NULL))
  }

  condition_raw <- paste0(
    ifelse(data_valid$Pressure_level == "low", "50g", "200g"),
    "/",
    ifelse(data_valid$Solvent_level == "absent", "dry", "wet")
  )
  data_valid$Condition <- factor(
    condition_raw,
    levels = c("50g/dry", "50g/wet", "200g/dry", "200g/wet")
  )
  # Negative controls have no Pressure/Solvent level (Condition NA) and no
  # SurfaceID-based repeat number -- drop rather than plot a spurious bar
  # (mirrors generate_barchart()'s NC handling above).
  data_valid <- data_valid[!is.na(data_valid$Condition) & !is.na(data_valid$SurfaceID), ]

  if (nrow(data_valid) == 0) {
    cat(sprintf("  Skipping %s by-repeat bar chart - no non-NC data available\n\n", analyte_name))
    return(invisible(NULL))
  }

  # Repeat number = the physical surface replicate that produced this
  # measurement (e.g. "Steel_02" -> 2, "ABS_04" -> 4).
  data_valid$Repeat <- factor(
    as.integer(sub(".*_", "", as.character(data_valid$SurfaceID))),
    levels = 1:4
  )

  # Guard against genuine duplicate (Surface_type, Repeat, Condition) rows
  # (e.g. a reanalysed sample not yet deduplicated) -- geom_bar(stat=
  # "identity") does not aggregate, so >1 row per dodge group would draw
  # overlapping bars rather than a single combined value. Warn loudly
  # rather than silently producing a misleading plot.
  dup_check <- data_valid %>%
    dplyr::count(Surface_type, Repeat, Condition) %>%
    dplyr::filter(n > 1)
  if (nrow(dup_check) > 0) {
    warning(analyte_name, " by-repeat bar chart: ", nrow(dup_check),
            " (Surface_type, Repeat, Condition) combination(s) have more than one row -- ",
            "bars will overlap rather than represent a single value. Check for duplicate/",
            "reanalysed samples not yet deduplicated.")
  }

  ymax <- suppressWarnings(max(data_valid[[recovery_col]], na.rm = TRUE))
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  # Sample size label per bar (Surface_type x Repeat x Condition). Normally
  # n=1 -- this design has exactly one physical measurement per (repeat,
  # condition) cell -- but shown directly on the plot for transparency/
  # consistency with the n= labels already used on the box plots and mean
  # bar charts above, and as a visible flag (alongside the dup_check
  # warning() above) if a cell ever does contain >1 row (e.g. duplicate/
  # reanalysed samples not yet deduplicated, most likely to surface in the
  # "all data" call which does not restrict to QC-passed samples).
  label_df <- data_valid %>%
    dplyr::group_by(Surface_type, Repeat, Condition) %>%
    dplyr::summarise(
      BarTop = max(.data[[recovery_col]], na.rm = TRUE),
      n      = dplyr::n(),
      .groups = "drop"
    )

  p <- ggplot(data_valid, aes(x = Repeat, y = .data[[recovery_col]], fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      data = label_df,
      aes(x = Repeat, y = BarTop, label = paste0("n=", n), group = Condition),
      position = position_dodge(width = 0.8),
      size = 2.6, vjust = -0.4, inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c(
      "50g/dry"  = "#8da0cb",
      "50g/wet"  = "#66c2a5",
      "200g/dry" = "#fc8d62",
      "200g/wet" = "#e78ac3"
    ), name = "Condition", drop = FALSE) +
    facet_wrap(~ Surface_type, labeller = labeller(Surface_type = c("steel" = "Steel", "abs" = "ABS"))) +
    coord_cartesian(ylim = c(0, ymax * 1.18)) +
    labs(
      title = paste0(analyte_name, " Recovery by Repeat and Condition", title_suffix),
      x = "Repeat (Surface Replicate #)",
      y = "Recovery (%)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )

  out_file <- file.path(output_dir, paste0(analyte_name, "_pilot_byrepeat_barchart", file_suffix, ".png"))
  ggsave(out_file, p, width = 8, height = 4, dpi = 300)

  cat(sprintf("  Saved: %s\n", out_file))
  return(invisible(p))
}

#===============================================================================
# ALL-DATA BOX PLOTS (ignores QC acceptance filtering entirely)
#
# Shows every sample that has a recovery value -- PASS, PASS*, FAIL, and
# negative controls (NC) alike -- colour-coded by analysis_accepted status.
# Useful for visually reviewing what the QC filter is excluding, independent
# of analysis_mode ("boxplots_only" or "full").
#===============================================================================

generate_all_data_boxplots <- function(data, output_dir) {
  cat("\n========================================\n")
  cat("ALL-DATA BOX PLOTS (QC acceptance filter ignored)\n")
  cat("========================================\n\n")

  generate_boxplots(data, "PETN", "PETN_Recovery_pct", output_dir,
                     file_suffix = "_alldata",
                     title_suffix = " \u2014 All Data",
                     show_points = TRUE)
  generate_boxplots(data, "RDX", "RDX_Recovery_pct", output_dir,
                     file_suffix = "_alldata",
                     title_suffix = " \u2014 All Data",
                     show_points = TRUE)

  # By-repeat individual-value bar charts, also run unfiltered (every
  # sample with a recovery value, including QC FAIL -- NC is still dropped
  # inside generate_repeat_barchart() itself, since NCs have no
  # Pressure/Solvent condition or factorial SurfaceID to plot against).
  generate_repeat_barchart(data, "PETN", "PETN_Recovery_pct", output_dir,
                            file_suffix = "_alldata",
                            title_suffix = " \u2014 All Data")
  generate_repeat_barchart(data, "RDX", "RDX_Recovery_pct", output_dir,
                            file_suffix = "_alldata",
                            title_suffix = " \u2014 All Data")
  cat("\n")
}


#===============================================================================
# DATA LOADING AND VALIDATION
#===============================================================================

cat("\n========================================\n")
cat("PILOT DATA ANALYSIS\n")
cat("========================================\n\n")

# Check if data file exists
if (!file.exists(data_file)) {
  stop("ERROR: Data file not found: ", data_file, "\n",
       "Please fill in recovery values in pilot_design_nested.csv and save as pilot_data_nested.csv")
}

# Load data
pilot_data <- read.csv(data_file, stringsAsFactors = FALSE)

cat("Data loaded successfully\n")
cat("  - Total samples:", nrow(pilot_data), "\n")
cat("  - Surfaces:", length(unique(pilot_data$SurfaceID)), "\n\n")

# Check for missing recovery data
missing_petn <- sum(is.na(pilot_data$PETN_Recovery_pct))
missing_rdx <- sum(is.na(pilot_data$RDX_Recovery_pct))

if (missing_petn > 0 || missing_rdx > 0) {
  cat("WARNING: Missing recovery data detected\n")
  cat("  - PETN missing:", missing_petn, "samples\n")
  cat("  - RDX missing:", missing_rdx, "samples\n\n")
}

# Convert factors
pilot_data$Surface_type <- factor(pilot_data$Surface_type, levels = c("steel", "abs"))
pilot_data$Pressure_level <- factor(pilot_data$Pressure_level, levels = c("low", "high"))
pilot_data$Solvent_level <- factor(pilot_data$Solvent_level, levels = c("absent", "present"))
pilot_data$SurfaceID <- factor(pilot_data$SurfaceID)

# Check if batch-organized format (has BatchNumber column)
has_batch_info <- "BatchNumber" %in% names(pilot_data)
if (has_batch_info) {
  pilot_data$BatchNumber <- factor(pilot_data$BatchNumber)
  cat("  - Batch-organized format detected\n")
  cat("  - Batches:", paste(levels(pilot_data$BatchNumber), collapse=", "), "\n")
}

# Keep an unfiltered copy (post factor-conversion) for the all-data box plots
# below, before the QC acceptance filter narrows pilot_data down further.
pilot_data_all <- pilot_data

# All-data box plots: every sample with a recovery value (PASS, PASS*, FAIL,
# and negative controls alike), colour-coded by QC status. Runs unconditionally,
# independent of analysis_mode, so it's always available for reviewing what
# the QC filter below is excluding.
generate_all_data_boxplots(pilot_data_all, output_dir)

# =========================================================
# QC ACCEPTANCE FILTER (mirrors GCMSQuantitation FINEX pipeline fix, July 2026:
# "Critical QC Filtering Issue" -- statistical analysis must only include
# QC-passed samples, otherwise variance components and effect sizes are
# artificially diluted by failed-QC samples).
#
# Gated by use_qc_filtered_data (set at top of script, USER CONFIGURATION).
# When TRUE (default): only samples with analysis_accepted %in% c("PASS",
# "PASS*") are analyzed. Samples with analysis_accepted starting "FAIL:"
# are excluded here but are still visible (with their failure reason) in
# ASTRA_PilotStudyResults.xlsx, produced by the collation step above.
# When FALSE: this filter is skipped entirely and the whole downstream
# statistical analysis (mixed model, ANOVA, power analysis, box/bar/repeat
# charts) runs on ALL samples regardless of QC acceptance -- negative
# controls are still excluded downstream (see use_qc_filtered_data comment
# above for why).
# =========================================================
if (!use_qc_filtered_data) {
  cat("QC FILTER: DISABLED (use_qc_filtered_data = FALSE) -- statistical analysis\n")
  cat("  will run on ALL samples regardless of QC acceptance (QC-failed samples included).\n\n")
} else if ("analysis_accepted" %in% names(pilot_data)) {
  n_before <- sum(!is.na(pilot_data$PETN_Recovery_pct) | !is.na(pilot_data$RDX_Recovery_pct))
  pilot_data <- pilot_data %>%
    filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS", "PASS*"))
  n_after <- sum(!is.na(pilot_data$PETN_Recovery_pct) | !is.na(pilot_data$RDX_Recovery_pct))
  n_excluded <- n_before - n_after
  if (n_excluded > 0) {
    cat("QC FILTER: Excluded", n_excluded, "sample(s) that failed QC acceptance criteria\n")
    cat("  (see analysis_accepted column / ASTRA_PilotStudyResults.xlsx for reasons)\n\n")
  } else {
    cat("QC FILTER: No samples excluded (all available samples passed QC)\n\n")
  }
} else {
  cat("NOTE: No 'analysis_accepted' column found -- QC filtering skipped.\n\n")
}

# Negative controls are NEVER part of the factorial design (no Surface_type-
# level replication/Pressure/Solvent) and must be excluded here regardless
# of use_qc_filtered_data above. Previously this happened as a side effect
# of NC rows carrying a literal "analysis_accepted == 'NC'" sentinel value,
# which never matched the QC filter's PASS/PASS* check either way. Under
# the unified compute_injection_acceptance() (shared InjectionAcceptance.R),
# NC rows now get a REAL PASS/PASS*/FAIL value like everyone else -- so an
# NC that happens to be QC-accepted would otherwise silently survive the
# filter above and leak into the mixed-effects model / box plots (Pressure_
# level/Solvent_level being NA for NC rows means lmer() would still drop
# them via na.omit, but generate_boxplots()'s Condition-label construction
# does not have an equivalent automatic safeguard). Excluded explicitly by
# SampleType here so nothing downstream needs to know about this at all.
if ("SampleType" %in% names(pilot_data)) {
  n_nc_dropped <- sum(pilot_data$SampleType == "NC", na.rm = TRUE)
  pilot_data <- pilot_data %>% filter(is.na(SampleType) | SampleType != "NC")
  if (n_nc_dropped > 0) {
    cat("Excluded", n_nc_dropped, "negative control row(s) from the factorial-design dataset",
        "(not part of the Surface_type x Pressure x Solvent design).\n\n")
  }
}

# Data validation
cat("Data validation:\n")
cat("  - Surface types:", paste(levels(pilot_data$Surface_type), collapse=", "), "\n")
cat("  - Pressure levels:", paste(levels(pilot_data$Pressure_level), collapse=", "), "\n")
cat("  - Solvent levels:", paste(levels(pilot_data$Solvent_level), collapse=", "), "\n\n")

# Summary statistics
cat("Summary statistics:\n\n")

summary_stats <- pilot_data %>%
  summarise(
    PETN_Mean = mean(PETN_Recovery_pct, na.rm = TRUE),
    PETN_SD = sd(PETN_Recovery_pct, na.rm = TRUE),
    PETN_Min = min(PETN_Recovery_pct, na.rm = TRUE),
    PETN_Max = max(PETN_Recovery_pct, na.rm = TRUE),
    RDX_Mean = mean(RDX_Recovery_pct, na.rm = TRUE),
    RDX_SD = sd(RDX_Recovery_pct, na.rm = TRUE),
    RDX_Min = min(RDX_Recovery_pct, na.rm = TRUE),
    RDX_Max = max(RDX_Recovery_pct, na.rm = TRUE)
  )

print(summary_stats)
cat("\n")

#===============================================================================
# BOX PLOTS ONLY MODE (early exit)
#===============================================================================

if (analysis_mode == "boxplots_only") {
  cat("Mode: BOX PLOTS ONLY\n")
  cat("Generating box plots for available data...\n\n")

  generate_boxplots(pilot_data, "PETN", "PETN_Recovery_pct", output_dir)
  generate_boxplots(pilot_data, "RDX", "RDX_Recovery_pct", output_dir)
  generate_barchart(pilot_data, "PETN", "PETN_Recovery_pct", output_dir)
  generate_barchart(pilot_data, "RDX", "RDX_Recovery_pct", output_dir)
  generate_repeat_barchart(pilot_data, "PETN", "PETN_Recovery_pct", output_dir)
  generate_repeat_barchart(pilot_data, "RDX", "RDX_Recovery_pct", output_dir)

  cat("\n========================================\n")
  cat("BOX PLOTS COMPLETE\n")
  cat("========================================\n")
  cat("Set analysis_mode <- \"full\" for complete statistical analysis.\n\n")

  # Stop here - do not run full analysis
  if (!interactive()) quit(save = "no")
  stop("Box plots generated. Change analysis_mode to 'full' to run complete analysis.",
       call. = FALSE)
}

#===============================================================================
# ANALYSIS FUNCTION (for both PETN and RDX)
#===============================================================================

analyze_analyte <- function(data, analyte_name, recovery_col) {

  cat("\n========================================\n")
  cat(analyte_name, "ANALYSIS\n")
  cat("========================================\n\n")

  # Remove missing values for this analyte
  data_clean <- data %>%
    filter(!is.na(.data[[recovery_col]]))

  if (nrow(data_clean) < 20) {
    cat("WARNING: Only", nrow(data_clean), "valid samples. Results may be unreliable.\n\n")
  }

  #-----------------------------------------------------------------------------
  # 1. FIT NESTED MIXED-EFFECTS MODEL
  #-----------------------------------------------------------------------------

  cat("Fitting nested mixed-effects model...\n")

  # Create nested surface ID
  data_clean$SurfaceID_nested <- interaction(data_clean$Surface_type, data_clean$SurfaceID, drop = TRUE)

  # Fit full model with all interactions
  model_formula <- as.formula(paste0(
    recovery_col, " ~ Surface_type * Pressure_level * Solvent_level + (1|SurfaceID_nested)"
  ))

  model <- lmer(model_formula, data = data_clean)

  cat("Model fitted successfully\n\n")

  # Boundary (singular) fit check: with this pilot's documented low ICC
  # (~0.02-0.03 in prior runs), the between-surface variance component can
  # be estimated at exactly zero (a "singular fit") -- lme4 still returns a
  # model in this case (often with a bobyqa convergence warning, "trust
  # region step failed to reduce q"), but every surface's random-effect
  # BLUP then shrinks to an identical value, which breaks any downstream
  # normality test on those BLUPs (see Section 6 below). Flagged explicitly
  # here so it's clear *why*, rather than only surfacing as a crash later.
  if (isSingular(model)) {
    cat("[WARNING] Boundary (singular) fit: the between-surface variance component\n")
    cat("  was estimated at (or extremely near) zero. This means the model found no\n")
    cat("  detectable variability between physical surfaces beyond residual noise --\n")
    cat("  consistent with this pilot's previously-documented low ICC. Variance\n")
    cat("  component estimates and any random-effect-based diagnostics below should\n")
    cat("  be interpreted with this in mind (they are not necessarily wrong, but the\n")
    cat("  random-intercept term is not contributing meaningfully to the model).\n\n")
  }

  #-----------------------------------------------------------------------------
  # 2. VARIANCE COMPONENTS
  #-----------------------------------------------------------------------------

  cat("VARIANCE COMPONENTS:\n")
  cat("--------------------\n")

  vc <- as.data.frame(VarCorr(model))
  sigma_between <- vc$vcov[1]  # Between-surface variance
  sigma_within <- vc$vcov[2]   # Within-surface variance (residual)

  icc <- sigma_between / (sigma_between + sigma_within)
  block_variance_reduction <- icc

  cat(sprintf("  Between-surface variance: %.3f\n", sigma_between))
  cat(sprintf("  Within-surface variance:  %.3f\n", sigma_within))
  cat(sprintf("  Total variance:           %.3f\n", sigma_between + sigma_within))
  cat(sprintf("  ICC (Intraclass Correlation): %.3f\n", icc))
  cat(sprintf("  Block variance reduction: %.1f%%\n\n", block_variance_reduction * 100))

  cat("Interpretation:\n")
  if (icc < 0.10) {
    cat("  - Very low ICC: Blocking provides minimal benefit\n")
    cat("  - Consider increasing replication instead of more surfaces\n")
  } else if (icc < 0.30) {
    cat("  - Low-moderate ICC: Blocking provides modest benefit\n")
    cat("  - Balanced design with moderate surface count recommended\n")
  } else if (icc < 0.50) {
    cat("  - Moderate-high ICC: Blocking provides substantial benefit\n")
    cat("  - Prioritize more surfaces over replication\n")
  } else {
    cat("  - Very high ICC: Between-surface variability dominates\n")
    cat("  - Strongly prioritize more surfaces\n")
    cat("  - Check if surface prep needs standardization\n")
  }
  cat("\n")

  #-----------------------------------------------------------------------------
  # 3. ANOVA (Type III with correct error terms)
  #-----------------------------------------------------------------------------

  cat("ANOVA TABLE (Type III):\n")
  cat("-----------------------\n")

  anova_table <- anova(model, type = 3)
  print(anova_table)
  cat("\n")

  #-----------------------------------------------------------------------------
  # 4. EFFECT SIZES
  #-----------------------------------------------------------------------------

  cat("EFFECT SIZES:\n")
  cat("-------------\n")

  # Calculate partial eta-squared
  eta_sq <- eta_squared(model, partial = TRUE, ci = 0.95)
  eta_sq$Cohens_f <- sqrt(eta_sq$Eta2_partial / (1 - eta_sq$Eta2_partial))

  print(eta_sq[, c("Parameter", "Eta2_partial", "CI_low", "CI_high", "Cohens_f")])
  cat("\n")

  cat("Cohen's f interpretation:\n")
  cat("  Small: f = 0.10, Medium: f = 0.25, Large: f = 0.40\n\n")

  #-----------------------------------------------------------------------------
  # 4b. BATCH EFFECT CHECK (if batch-organized data)
  #-----------------------------------------------------------------------------

  if ("BatchNumber" %in% names(data_clean)) {
    cat("BATCH EFFECT ANALYSIS:\n")
    cat("----------------------\n")
    cat("Checking if BatchNumber (time-of-day proxy) affects recovery...\n\n")

    # Fit model with batch effect
    model_with_batch <- lmer(as.formula(paste0(
      recovery_col, " ~ Surface_type * Pressure_level * Solvent_level + BatchNumber + (1|SurfaceID_nested)"
    )), data = data_clean)

    # Compare models
    model_comparison <- anova(model, model_with_batch)

    # Extract p-value for batch effect
    batch_anova <- anova(model_with_batch, type = 3)
    batch_row <- which(rownames(batch_anova) == "BatchNumber")

    if (length(batch_row) > 0) {
      batch_p <- batch_anova$`Pr(>F)`[batch_row]

      cat(sprintf("  Batch effect p-value: %.4f %s\n",
                  batch_p, ifelse(batch_p < 0.05, "[FAIL] SIGNIFICANT", "[OK] Not significant")))

      if (batch_p < 0.05) {
        cat("\n  [WARNING] Significant batch effect detected!\n")
        cat("  Interpretation: Time-of-day or batch order affects recovery\n")
        cat("  Recommendation: Consider including BatchNumber in main study model\n")
        cat("  Alternative: Randomize batch order in main study\n\n")
      } else {
        cat("\n  [OK] No significant batch effect\n")
        cat("  Interpretation: Batch spiking workflow does not affect recovery\n")
        cat("  Recommendation: Safe to use batch workflow in main study\n\n")
      }

      # Show batch means
      cat("  Mean recovery by batch:\n")
      batch_means <- data_clean %>%
        group_by(BatchNumber) %>%
        summarise(Mean = mean(.data[[recovery_col]], na.rm = TRUE),
                  SD = sd(.data[[recovery_col]], na.rm = TRUE),
                  N = n(),
                  .groups = "drop")
      print(batch_means)
      cat("\n")
    }
  }

  #-----------------------------------------------------------------------------
  # 5. ESTIMATED MARGINAL MEANS
  #-----------------------------------------------------------------------------

  cat("ESTIMATED MARGINAL MEANS:\n")
  cat("-------------------------\n\n")

  # Main effects
  cat("Surface type:\n")
  emm_surface <- emmeans(model, ~ Surface_type)
  print(summary(emm_surface))
  cat("\n")

  cat("Pressure:\n")
  emm_pressure <- emmeans(model, ~ Pressure_level)
  print(summary(emm_pressure))
  cat("\n")

  cat("Solvent:\n")
  emm_solvent <- emmeans(model, ~ Solvent_level)
  print(summary(emm_solvent))
  cat("\n")

  # Pairwise comparisons
  cat("Pairwise comparisons:\n")
  cat("  Surface type: steel vs abs\n")
  print(pairs(emm_surface))
  cat("\n")

  cat("  Pressure: low vs high\n")
  print(pairs(emm_pressure))
  cat("\n")

  cat("  Solvent: absent vs present\n")
  print(pairs(emm_solvent))
  cat("\n")

  #-----------------------------------------------------------------------------
  # 6. ASSUMPTION CHECKING
  #-----------------------------------------------------------------------------

  cat("ASSUMPTION CHECKS:\n")
  cat("------------------\n")

  # Normality of residuals
  shapiro_p <- shapiro.test(resid(model))$p.value
  cat(sprintf("  Shapiro-Wilk (residuals): p = %.4f %s\n",
              shapiro_p, ifelse(shapiro_p > 0.05, "[OK]", "[FAIL]")))

  # Normality of random effects
  # Guarded: when the between-surface variance component is at/near the
  # boundary (zero) -- see the isSingular() check when the model was fit,
  # above -- every surface's BLUP shrinks to an identical value (typically
  # exactly 0), and shapiro.test() throws a hard error ("all 'x' values are
  # identical") rather than returning a normal result. Wrapped in tryCatch
  # so a degenerate/boundary random-effect fit is reported clearly instead
  # of crashing the rest of the analysis.
  ranef_vals <- ranef(model)$SurfaceID_nested[,1]
  shapiro_re_p <- tryCatch(shapiro.test(ranef_vals)$p.value, error = function(e) NA_real_)
  if (is.na(shapiro_re_p)) {
    cat("  Shapiro-Wilk (random effects): skipped -- random effect BLUPs are constant\n")
    cat("    (between-surface variance component at/near zero -- see boundary/singular\n")
    cat("    fit warning above; there is no between-surface variability to test)\n")
  } else {
    cat(sprintf("  Shapiro-Wilk (random effects): p = %.4f %s\n",
                shapiro_re_p, ifelse(shapiro_re_p > 0.05, "[OK]", "[FAIL]")))
  }

  # Check for outliers
  outliers <- abs(scale(resid(model))) > 3
  n_outliers <- sum(outliers)
  cat(sprintf("  Outliers (>3 SD): %d samples %s\n",
              n_outliers, ifelse(n_outliers == 0, "[OK]", "[WARNING]")))

  if (n_outliers > 0) {
    outlier_ids <- data_clean$SampleID[outliers]
    cat("    Outlier samples:", paste(outlier_ids, collapse=", "), "\n")
  }
  cat("\n")

  #-----------------------------------------------------------------------------
  # 7. GENERATE PLOTS
  #-----------------------------------------------------------------------------

  # Box plots (replaces bar charts)
  generate_boxplots(data_clean, analyte_name, recovery_col, output_dir)
  generate_barchart(data_clean, analyte_name, recovery_col, output_dir)
  generate_repeat_barchart(data_clean, analyte_name, recovery_col, output_dir)

  # Interaction plots
  means_data <- data_clean %>%
    group_by(Surface_type, Pressure_level, Solvent_level) %>%
    summarise(Mean = mean(.data[[recovery_col]], na.rm = TRUE),
              SE = sd(.data[[recovery_col]], na.rm = TRUE) / sqrt(n()),
              .groups = "drop")

  # Interaction plot: Surface x Applied Mass
  p4 <- ggplot(means_data %>% group_by(Surface_type, Pressure_level) %>% summarise(Mean = mean(Mean), .groups="drop"),
               aes(x = Surface_type, y = Mean, color = Pressure_level, group = Pressure_level)) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal_mass_level) +
    labs(title = paste(analyte_name, "- Surface x Applied Mass"),
         y = "Mean Recovery (%)", color = "Applied Mass") +
    theme_bw()

  # Interaction plot: Surface x Solvent
  p5 <- ggplot(means_data %>% group_by(Surface_type, Solvent_level) %>% summarise(Mean = mean(Mean), .groups="drop"),
               aes(x = Surface_type, y = Mean, color = Solvent_level, group = Solvent_level)) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal_solvent) +
    labs(title = paste(analyte_name, "- Surface x Solvent"),
         y = "Mean Recovery (%)", color = "Solvent") +
    theme_bw()

  # Interaction plot: Applied Mass x Solvent
  p6 <- ggplot(means_data %>% group_by(Pressure_level, Solvent_level) %>% summarise(Mean = mean(Mean), .groups="drop"),
               aes(x = Pressure_level, y = Mean, color = Solvent_level, group = Solvent_level)) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal_solvent) +
    labs(title = paste(analyte_name, "- Applied Mass x Solvent"),
         x = "Applied Mass", y = "Mean Recovery (%)", color = "Solvent") +
    theme_bw()

  # Diagnostic plots
  p7 <- ggplot(data.frame(Fitted = fitted(model), Residuals = resid(model)),
               aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(analyte_name, "- Residuals vs Fitted"),
         x = "Fitted Values", y = "Residuals") +
    theme_bw()

  p8 <- ggplot(data.frame(Residuals = resid(model)), aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste(analyte_name, "- Q-Q Plot"),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_bw()

  # Save plots
  boxplots_file <- file.path(output_dir, paste0(analyte_name, "_pilot_boxplots.png"))
  interactions_file <- file.path(output_dir, paste0(analyte_name, "_pilot_interactions.png"))
  ggsave(interactions_file, arrangeGrob(p4, p5, p6, ncol = 3),
         width = 14, height = 4, dpi = 300)

  diagnostics_file <- file.path(output_dir, paste0(analyte_name, "_pilot_diagnostics.png"))
  ggsave(diagnostics_file, arrangeGrob(p7, p8, ncol = 2),
         width = 10, height = 4, dpi = 300)

  cat("Plots saved:\n")
  cat("  -", boxplots_file, "\n")
  cat("  -", interactions_file, "\n")
  cat("  -", diagnostics_file, "\n\n")

  #-----------------------------------------------------------------------------
  # 8. RETURN RESULTS
  #-----------------------------------------------------------------------------

  return(list(
    model = model,
    icc = icc,
    variance_components = vc,
    anova = anova_table,
    effect_sizes = eta_sq,
    sigma_within = sigma_within,
    overall_sd = sd(data_clean[[recovery_col]], na.rm = TRUE),
    data_clean = data_clean
  ))
}

#===============================================================================
# ANALYZE BOTH ANALYTES (+ persist everything below to stats_summary_file)
#
# Wraps the mixed-effects model / ANOVA / effect sizes / assumption checks
# (inside analyze_analyte()) plus the power analysis and main study design
# recommendation below in sink(..., split = TRUE): every cat()/print() call
# is written to stats_summary_file VERBATIM, in addition to still appearing
# on the console exactly as before -- guarantees the file is a complete,
# faithful copy of everything printed (nothing manually re-transcribed, so
# nothing can drift out of sync with what the console actually shows).
# tryCatch(..., finally = ...) guarantees the sink is closed again even if
# an error occurs partway through, so a mid-run failure can't leave the
# R session's console output silently redirected to the file.
#===============================================================================

sink_con <- file(stats_summary_file, open = "wt", encoding = "UTF-8")
sink(sink_con, split = TRUE)

tryCatch({

  cat("================================================================\n")
  cat("PILOT STUDY ANALYSIS SUMMARY\n")
  cat("================================================================\n")
  cat("Generated:      ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Data file:      ", data_file, "\n")
  cat("Analysis mode:  ", analysis_mode, "\n")
  cat("QC filter (use_qc_filtered_data):", use_qc_filtered_data, "\n")
  if (!use_qc_filtered_data) {
    cat("  NOTE: QC filter DISABLED for this run -- statistical analysis includes ALL\n")
    cat("  samples regardless of QC acceptance (see analysis_accepted column /\n")
    cat("  ASTRA_PilotStudyResults.xlsx for individual failure reasons).\n")
  }
  cat("PETN samples analyzed:", sum(!is.na(pilot_data$PETN_Recovery_pct)), "\n")
  cat("RDX samples analyzed: ", sum(!is.na(pilot_data$RDX_Recovery_pct)), "\n")
  cat("Target meaningful recovery difference:", meaningful_recovery_difference_pct, "%\n")
  cat("Target power:  ", target_power, "\n")
  cat("Significance level (alpha):", alpha_level, "\n")
  cat("Max surfaces per type considered:", max_surfaces_per_type, "\n")
  cat("================================================================\n\n")

  petn_results <- analyze_analyte(pilot_data, "PETN", "PETN_Recovery_pct")
  rdx_results <- analyze_analyte(pilot_data, "RDX", "RDX_Recovery_pct")

  #===============================================================================
  # POWER ANALYSIS FOR MAIN STUDY
  #===============================================================================

  cat("\n========================================\n")
  cat("POWER ANALYSIS FOR MAIN STUDY\n")
  cat("========================================\n\n")

  cat("Target meaningful effect:", meaningful_recovery_difference_pct, "% recovery difference\n")
  cat("Target power:", target_power, "\n")
  cat("Significance level:", alpha_level, "\n\n")

  # Function to calculate power for a nested design. Generalised (Aug 2026)
  # to accept n_conditions/n_fixed_effects explicitly, rather than hardcoding
  # the full 2^3 (Surface x Pressure x Solvent) design -- needed below to
  # also evaluate a REDUCED design (e.g. Solvent fixed at one level, only
  # Surface x Pressure varying: n_conditions=2, n_fixed_effects=3).
  calculate_nested_power <- function(n_surfaces_per_type, effect_size_f, icc, alpha = 0.05,
                                      n_conditions = 4, n_fixed_effects = 7) {
    total_surfaces <- n_surfaces_per_type * 2  # 2 surface types
    total_n <- total_surfaces * n_conditions

    # Degrees of freedom
    df_effect <- 1  # Testing one effect at a time
    df_surfaces <- total_surfaces - 2  # Surface random effect df
    df_error <- total_n - n_fixed_effects - df_surfaces - 1

    if (df_error <= 0) return(NA)

    # Adjust effect size for blocking
    effective_var <- 1 - icc
    f2_adjusted <- effect_size_f^2 / effective_var

    # Calculate power
    power <- pwr.f2.test(u = df_effect, v = df_error, f2 = f2_adjusted, sig.level = alpha)$power

    return(power)
  }

  # Calculate effect size for meaningful difference
  calculate_effect_size_for_target <- function(recovery_diff_pct, overall_sd) {
    cohens_d <- recovery_diff_pct / overall_sd
    cohens_f <- cohens_d / sqrt(2)  # For comparing two groups
    return(cohens_f)
  }

  # Find the minimum n_surfaces_per_type achieving target_power, searching
  # from min_n upward (Aug 2026 fix: previously hardcoded to start at 4 --
  # i.e. the search never checked whether SMALLER designs, e.g. 2 or 3
  # surfaces per type, might already suffice, so "4 is the minimum" only
  # ever meant "4 is the smallest value we bothered to test", not a true
  # minimum). Returns NA min_n if even max_n is insufficient. Generalised to
  # accept n_conditions/n_fixed_effects for reduced-design scenarios (see
  # calculate_nested_power() above).
  find_min_n_for_power <- function(effect_size_f, icc, target_power, alpha, max_n,
                                    n_conditions = 4, n_fixed_effects = 7, min_n = 2) {
    if (is.na(effect_size_f) || is.na(icc) || effect_size_f <= 0) {
      return(list(min_n = NA, achieved_power = NA,
                  table = data.frame(n = integer(0), total_samples = integer(0), power = numeric(0))))
    }
    n_seq <- seq(min_n, max_n, 1)
    powers <- sapply(n_seq, function(n) {
      calculate_nested_power(n, effect_size_f, icc, alpha, n_conditions, n_fixed_effects)
    })
    tab <- data.frame(n = n_seq, total_samples = n_seq * 2 * n_conditions, power = powers)
    adequate <- tab[!is.na(tab$power) & tab$power >= target_power, ]
    if (nrow(adequate) == 0) {
      return(list(min_n = NA, achieved_power = NA, table = tab))
    }
    best <- adequate[which.min(adequate$n), ]
    list(min_n = best$n, achieved_power = best$power, table = tab)
  }

  # For each analyte, calculate required sample size for the target
  # (pre-specified) meaningful recovery difference, full 2^3 factorial design
  for (analyte_info in list(
    list(name = "PETN", results = petn_results),
    list(name = "RDX", results = rdx_results)
  )) {

    cat("\n", analyte_info$name, "POWER ANALYSIS:\n")
    cat("------------------\n")

    results <- analyte_info$results

    # Effect size for target meaningful difference
    target_f <- calculate_effect_size_for_target(meaningful_recovery_difference_pct,
                                                  results$overall_sd)

    cat(sprintf("  Overall SD: %.2f%%\n", results$overall_sd))
    cat(sprintf("  Target difference: %.1f%%\n", meaningful_recovery_difference_pct))
    cat(sprintf("  Equivalent Cohen's f: %.3f\n", target_f))
    cat(sprintf("  ICC from pilot: %.3f\n\n", results$icc))

    # Test different sample sizes (min_n = 2, not 4 -- see find_min_n_for_power() comment)
    cat("  Power for different sample sizes:\n")
    cat("  (surfaces per type -> total samples -> power)\n\n")

    search <- find_min_n_for_power(target_f, results$icc, target_power, alpha_level,
                                    max_surfaces_per_type, min_n = 2)

    for (i in seq_len(nrow(search$table))) {
      row <- search$table[i, ]
      if (is.na(row$power)) next
      marker <- if (!is.na(search$min_n) && row$n == search$min_n) " <- MINIMUM for 80% power" else ""
      cat(sprintf("    %2d -> %3d samples -> %.1f%%%s\n",
                  row$n, row$total_samples, row$power * 100, marker))
    }
    cat("\n")

    if (!is.na(search$min_n)) {
      cat(sprintf("  RECOMMENDATION for %s:\n", analyte_info$name))
      cat(sprintf("    Minimum: %d surfaces per type (%d total samples)\n",
                  search$min_n, search$min_n * 2 * 4))
      cat(sprintf("    Achieves: %.1f%% power\n\n", search$achieved_power * 100))
    } else {
      cat(sprintf("  WARNING: Cannot achieve %.0f%% power with <=%d surfaces per type\n",
                  target_power * 100, max_surfaces_per_type))
      cat("  Consider:\n")
      cat("    1. Increasing maximum surfaces available\n")
      cat("    2. Accepting lower power (e.g., 70%)\n")
      cat("    3. Focusing only on large effects\n\n")
    }
  }

  #===============================================================================
  # PER-EFFECT POWER ANALYSIS (Pressure vs Solvent, each analyte)
  #
  # The single "target meaningful difference" analysis above answers "how big
  # a study is needed to detect a pre-specified 10% recovery difference,
  # generically" -- it does not distinguish between factors. This section
  # asks the more specific, user-relevant question: does Pressure need more
  # replication than Solvent (or vice versa), using each factor's OWN
  # observed effect size from the pilot's ANOVA.
  #
  # CAUTION -- retrospective/observed power: these effect sizes are
  # estimated FROM this same pilot dataset. Using an effect estimated from a
  # sample to then judge whether a study of that size is "adequately
  # powered" is a well-documented statistical pitfall (Hoenig & Heisey,
  # 2001, "The Abuse of Power", Am. Statistician 55(1):19-24): an effect
  # that was itself found significant in a given sample will almost always
  # show "adequate" retrospective power, because the two calculations are
  # not independent. Two figures are given per factor to partially guard
  # against this:
  #   - "Optimistic": the point-estimate Cohen's f (what the pilot's ANOVA
  #     literally found)
  #   - "Conservative": Cohen's f computed from the LOWER bound of that
  #     effect's 95% CI (i.e. "even if the true effect is as small as the
  #     pilot's data can't rule out, how many surfaces would still be
  #     needed?") -- treat this as the more defensible planning figure.
  #===============================================================================

  cat("\n\n========================================\n")
  cat("PER-EFFECT POWER ANALYSIS (Pressure vs Solvent)\n")
  cat("========================================\n\n")
  cat("Uses each factor's OWN observed effect size (Cohen's f) from the pilot's\n")
  cat("ANOVA, instead of the single pre-specified target-difference figure above,\n")
  cat("to answer: does Pressure require more replication than Solvent, or vice\n")
  cat("versa? See the CAUTION note in the script comments above this section --\n")
  cat("these are retrospective/observed-effect power estimates, and the\n")
  cat("'Conservative' (95% CI lower bound) figures should be treated as more\n")
  cat("defensible than the 'Optimistic' (point estimate) figures.\n\n")

  for (analyte_info in list(
    list(name = "PETN", results = petn_results),
    list(name = "RDX", results = rdx_results)
  )) {

    cat("\n", analyte_info$name, ":\n")
    cat("------------------\n")

    es <- analyte_info$results$effect_sizes
    icc_val <- analyte_info$results$icc

    for (factor_name in c("Pressure_level", "Solvent_level")) {
      row <- es[es$Parameter == factor_name, ]
      if (nrow(row) == 0) {
        cat("  ", factor_name, ": not found in model\n")
        next
      }

      f_point <- row$Cohens_f
      ci_low_eta <- max(row$CI_low, 0)
      f_low <- sqrt(ci_low_eta / (1 - ci_low_eta))

      cat(sprintf("  %s: Eta2_partial = %.3f [95%% CI %.3f-%.3f]\n",
                  factor_name, row$Eta2_partial, row$CI_low, row$CI_high))
      cat(sprintf("    Cohen's f: %.3f (optimistic, point estimate) / %.3f (conservative, 95%% CI lower bound)\n",
                  f_point, f_low))

      res_opt  <- find_min_n_for_power(f_point, icc_val, target_power, alpha_level, max_surfaces_per_type, min_n = 2)
      res_cons <- find_min_n_for_power(f_low,   icc_val, target_power, alpha_level, max_surfaces_per_type, min_n = 2)

      if (!is.na(res_opt$min_n)) {
        cat(sprintf("    Optimistic:   %d surfaces/type (%d total samples), %.1f%% power\n",
                    res_opt$min_n, res_opt$min_n * 2 * 4, res_opt$achieved_power * 100))
      } else {
        cat(sprintf("    Optimistic:   cannot reach %.0f%% power within %d surfaces/type\n",
                    target_power * 100, max_surfaces_per_type))
      }
      if (!is.na(res_cons$min_n)) {
        cat(sprintf("    Conservative: %d surfaces/type (%d total samples), %.1f%% power\n",
                    res_cons$min_n, res_cons$min_n * 2 * 4, res_cons$achieved_power * 100))
      } else {
        cat("    Conservative: effect could be as small as ~0 given this pilot's uncertainty --\n")
        cat("                  cannot determine a finite sample size from this pilot alone.\n")
      }
      cat("\n")
    }
  }

  #===============================================================================
  # SCENARIO: WET-SWAB-ONLY MAIN STUDY (Solvent = present, Pressure-focused)
  #
  # Motivation: the per-effect analysis above typically shows Solvent is a far
  # larger effect than Pressure, and many dry-swab (Solvent = absent) samples
  # show ~0% recovery. This models fixing Solvent = present (wet) for every
  # sample in the main study and only varying Pressure x Surface_type -- a
  # reduced 2^2 design (n_conditions = 2, n_fixed_effects = 3: Surface_type,
  # Pressure_level, their interaction) instead of the full 2^3 -- to test
  # whether Pressure alone still needs replication once Solvent is no longer
  # varied at all.
  #===============================================================================

  fit_pressure_only_model <- function(data, recovery_col) {
    data_wet <- data %>%
      dplyr::filter(Solvent_level == "present", !is.na(.data[[recovery_col]]))
    n_valid <- nrow(data_wet)

    if (n_valid < 8 || dplyr::n_distinct(data_wet$Surface_type) < 2) {
      return(list(ok = FALSE, reason = sprintf("Insufficient wet-only data (n=%d)", n_valid), n_valid = n_valid))
    }

    data_wet$SurfaceID_nested <- interaction(data_wet$Surface_type, data_wet$SurfaceID, drop = TRUE)

    model_wet <- tryCatch(
      lmer(as.formula(paste0(recovery_col, " ~ Surface_type * Pressure_level + (1|SurfaceID_nested)")),
           data = data_wet),
      error = function(e) NULL
    )
    if (is.null(model_wet)) {
      return(list(ok = FALSE, reason = "Reduced model failed to fit (likely too few data points/groups)", n_valid = n_valid))
    }

    vc_wet <- as.data.frame(VarCorr(model_wet))
    sigma_between_wet <- vc_wet$vcov[1]
    sigma_within_wet <- vc_wet$vcov[2]
    icc_wet <- sigma_between_wet / (sigma_between_wet + sigma_within_wet)

    eta_wet <- tryCatch(eta_squared(model_wet, partial = TRUE, ci = 0.95), error = function(e) NULL)
    if (is.null(eta_wet)) {
      return(list(ok = FALSE, reason = "Could not compute effect sizes for the reduced model", n_valid = n_valid))
    }

    pressure_row <- eta_wet[eta_wet$Parameter == "Pressure_level", ]
    if (nrow(pressure_row) == 0) {
      return(list(ok = FALSE, reason = "Pressure_level term not found in reduced model", n_valid = n_valid))
    }

    ci_low_eta <- max(pressure_row$CI_low, 0)
    cohens_f <- sqrt(pressure_row$Eta2_partial / (1 - pressure_row$Eta2_partial))
    cohens_f_low <- sqrt(ci_low_eta / (1 - ci_low_eta))

    list(ok = TRUE, n_valid = n_valid, icc = icc_wet, cohens_f = cohens_f, cohens_f_low = cohens_f_low,
         eta2 = pressure_row$Eta2_partial, ci_low = pressure_row$CI_low, ci_high = pressure_row$CI_high,
         overall_sd = sd(data_wet[[recovery_col]], na.rm = TRUE))
  }

  cat("\n\n========================================\n")
  cat("SCENARIO: WET-SWAB-ONLY MAIN STUDY (Solvent = present, Pressure-focused)\n")
  cat("========================================\n\n")
  cat("Models fixing Solvent = present for every sample and only varying\n")
  cat("Pressure x Surface_type (a reduced 2^2 design), refit on the wet-only\n")
  cat("subset of THIS pilot's own data. Same retrospective-power caution as the\n")
  cat("per-effect section above applies.\n\n")

  wet_scenario_results <- list()

  for (analyte_info in list(
    list(name = "PETN", col = "PETN_Recovery_pct", data = petn_results$data_clean),
    list(name = "RDX",  col = "RDX_Recovery_pct",  data = rdx_results$data_clean)
  )) {

    cat("\n", analyte_info$name, "(wet-only subset):\n")
    cat("------------------\n")

    fit <- fit_pressure_only_model(analyte_info$data, analyte_info$col)
    wet_scenario_results[[analyte_info$name]] <- fit

    if (!fit$ok) {
      cat("  Skipped:", fit$reason, "\n\n")
      next
    }

    cat(sprintf("  Wet-only valid samples used: %d\n", fit$n_valid))
    cat(sprintf("  Pressure effect (wet-only): Eta2_partial = %.3f [95%% CI %.3f-%.3f]\n",
                fit$eta2, fit$ci_low, fit$ci_high))
    cat(sprintf("    Cohen's f: %.3f (optimistic) / %.3f (conservative, 95%% CI lower bound)\n",
                fit$cohens_f, fit$cohens_f_low))
    cat(sprintf("  ICC (wet-only): %.3f\n\n", fit$icc))

    res_opt <- find_min_n_for_power(fit$cohens_f, fit$icc, target_power, alpha_level, max_surfaces_per_type,
                                     n_conditions = 2, n_fixed_effects = 3, min_n = 2)
    res_cons <- find_min_n_for_power(fit$cohens_f_low, fit$icc, target_power, alpha_level, max_surfaces_per_type,
                                      n_conditions = 2, n_fixed_effects = 3, min_n = 2)

    if (!is.na(res_opt$min_n)) {
      cat(sprintf("    Optimistic:   %d surfaces/type (%d total wet-only samples), %.1f%% power\n",
                  res_opt$min_n, res_opt$min_n * 2 * 2, res_opt$achieved_power * 100))
    } else {
      cat(sprintf("    Optimistic:   cannot reach %.0f%% power within %d surfaces/type\n",
                  target_power * 100, max_surfaces_per_type))
    }
    if (!is.na(res_cons$min_n)) {
      cat(sprintf("    Conservative: %d surfaces/type (%d total wet-only samples), %.1f%% power\n",
                  res_cons$min_n, res_cons$min_n * 2 * 2, res_cons$achieved_power * 100))
    } else {
      cat("    Conservative: effect could be as small as ~0 given this pilot's uncertainty --\n")
      cat("                  cannot determine a finite sample size from this pilot alone.\n")
    }

    # Literature-informed (externally-anchored, non-circular) target: uses
    # pressure_literature_effect_pct (set at top of script, see its comment)
    # instead of this pilot's own retrospective Pressure effect estimate --
    # only the residual variance (this wet-only subset's own overall_sd) is
    # still pilot-derived, which is unavoidable for any power calculation.
    # Recommended as the more defensible planning figure specifically
    # because the pilot's own wet-only n (9) is too small for its retrospective
    # estimate above to be trustworthy (RDX's conservative figure couldn't
    # even be computed).
    lit_cohens_d <- pressure_literature_effect_pct / fit$overall_sd
    lit_cohens_f <- lit_cohens_d / sqrt(2)
    res_lit <- find_min_n_for_power(lit_cohens_f, fit$icc, target_power, alpha_level, max_surfaces_per_type,
                                     n_conditions = 2, n_fixed_effects = 3, min_n = 2)
    cat(sprintf("    Literature-informed (%.0f%% target, NOT from this pilot's own Pressure estimate):\n",
                pressure_literature_effect_pct))
    cat(sprintf("      Cohen's f: %.3f (wet-only overall SD = %.2f%%)\n", lit_cohens_f, fit$overall_sd))
    if (!is.na(res_lit$min_n)) {
      cat(sprintf("      %d surfaces/type (%d total wet-only samples), %.1f%% power\n",
                  res_lit$min_n, res_lit$min_n * 2 * 2, res_lit$achieved_power * 100))
    } else {
      cat(sprintf("      cannot reach %.0f%% power within %d surfaces/type\n",
                  target_power * 100, max_surfaces_per_type))
    }
    cat("\n")
  }

  #===============================================================================
  # FINAL RECOMMENDATIONS
  #===============================================================================

  cat("\n========================================\n")
  cat("FINAL RECOMMENDATIONS\n")
  cat("========================================\n\n")

  # Take the more conservative recommendation (larger sample size)
  petn_target_f <- calculate_effect_size_for_target(meaningful_recovery_difference_pct,
                                                     petn_results$overall_sd)
  rdx_target_f <- calculate_effect_size_for_target(meaningful_recovery_difference_pct,
                                                    rdx_results$overall_sd)

  # Calculate required n for both (min_n = 2 -- see find_min_n_for_power() comment above)
  petn_search <- find_min_n_for_power(petn_target_f, petn_results$icc, target_power, alpha_level,
                                       max_surfaces_per_type, min_n = 2)
  rdx_search <- find_min_n_for_power(rdx_target_f, rdx_results$icc, target_power, alpha_level,
                                      max_surfaces_per_type, min_n = 2)

  cat("RECOMMENDED MAIN STUDY: WET-SWAB-ONLY, PRESSURE-FOCUSED (Solvent = present\n")
  cat("only, reduced 2^2: Surface x Pressure)\n")
  cat("--------------------------------------------------------------------------\n")
  cat("Rationale: the PER-EFFECT POWER ANALYSIS above shows Solvent is already a\n")
  cat("large, precisely-estimated effect for both analytes (95% CI lower bound on\n")
  cat("Cohen's f well above 1.0 for both) -- collecting more data that varies\n")
  cat("Solvent again would mostly re-confirm what is already well established.\n")
  cat("Pressure's effect, by contrast, remains genuinely unresolved SPECIFICALLY\n")
  cat("under wet-swab conditions (the only condition where it is operationally\n")
  cat("relevant, since dry-swab recovery is uniformly near-zero regardless of\n")
  cat("pressure) -- this is the one open question a main study should target.\n\n")

  petn_wet <- if ("PETN" %in% names(wet_scenario_results)) wet_scenario_results[["PETN"]] else list(ok = FALSE)
  rdx_wet  <- if ("RDX"  %in% names(wet_scenario_results)) wet_scenario_results[["RDX"]]  else list(ok = FALSE)

  if (isTRUE(petn_wet$ok) && isTRUE(rdx_wet$ok)) {
    lit_f_petn <- (pressure_literature_effect_pct / petn_wet$overall_sd) / sqrt(2)
    lit_f_rdx  <- (pressure_literature_effect_pct / rdx_wet$overall_sd) / sqrt(2)
    lit_search_petn <- find_min_n_for_power(lit_f_petn, petn_wet$icc, target_power, alpha_level,
                                             max_surfaces_per_type, n_conditions = 2, n_fixed_effects = 3, min_n = 2)
    lit_search_rdx <- find_min_n_for_power(lit_f_rdx, rdx_wet$icc, target_power, alpha_level,
                                            max_surfaces_per_type, n_conditions = 2, n_fixed_effects = 3, min_n = 2)

    cat(sprintf("Using the LITERATURE-INFORMED target effect (%.0f%% expected Pressure\n",
                pressure_literature_effect_pct))
    cat("difference -- see pressure_literature_effect_pct comment at top of script --\n")
    cat("NOT this pilot's own noisy retrospective Pressure estimate, which is too\n")
    cat("uncertain at n=9 wet-only samples to trust; see SCENARIO: WET-SWAB-ONLY\n")
    cat("above):\n\n")

    if (!is.na(lit_search_petn$min_n) && !is.na(lit_search_rdx$min_n)) {
      recommended_n_wet <- max(lit_search_petn$min_n, lit_search_rdx$min_n)
      cat(sprintf("  Surfaces per type: %d steel + %d abs = %d total surfaces\n",
                  recommended_n_wet, recommended_n_wet, recommended_n_wet * 2))
      cat("  Conditions per surface: 2 (Pressure only, Solvent fixed = present)\n")
      cat(sprintf("  Total samples: %d\n\n", recommended_n_wet * 2 * 2))
      cat("EXPECTED POWER (at this n, literature-informed target):\n")
      cat(sprintf("  PETN: %.1f%% (individually needs only %d/type)\n",
                  calculate_nested_power(recommended_n_wet, lit_f_petn, petn_wet$icc, alpha_level, 2, 3) * 100,
                  lit_search_petn$min_n))
      cat(sprintf("  RDX:  %.1f%% (individually needs %d/type)\n\n",
                  calculate_nested_power(recommended_n_wet, lit_f_rdx, rdx_wet$icc, alpha_level, 2, 3) * 100,
                  lit_search_rdx$min_n))
      days_gcms_wet <- ceiling((recommended_n_wet * 2 * 2) / 28)
      cat("TIMELINE ESTIMATE:\n")
      cat(sprintf("  GC-MS analysis: %d day(s) (%.0f samples/day)\n",
                  days_gcms_wet, (recommended_n_wet * 2 * 2) / days_gcms_wet))
      cat(sprintf("  Sample collection: ~%d days (4 surfaces/day)\n\n", ceiling(recommended_n_wet * 2 / 4)))
    } else {
      cat("  Could not reach the target power within the surface limit for one or\n")
      cat("  both analytes even with the literature-informed (externally-anchored)\n")
      cat("  effect size -- consider raising max_surfaces_per_type or accepting\n")
      cat("  lower power for whichever analyte is the bottleneck.\n\n")
    }
  } else {
    cat("Could not fit the wet-only reduced model for one or both analytes -- see\n")
    cat("SCENARIO: WET-SWAB-ONLY section above for why (usually: too little\n")
    cat("wet-only data in this pilot).\n\n")
  }

  cat("IMPORTANT CAVEAT: this is a STANDALONE calculation (\"how big would a fresh,\n")
  cat("wet-only study need to be, on its own?\") -- it does NOT subtract or credit\n")
  cat("the 9 wet-only pilot samples already collected. If those are pooled into the\n")
  cat("final analysis (recommended -- see doe_nested_design.R/main_study_analysis.R\n")
  cat("combined-analysis plan in CONTEXT.md), slightly fewer new samples may suffice.\n\n")

  if (!is.na(petn_search$min_n) && !is.na(rdx_search$min_n)) {
    recommended_n <- max(petn_search$min_n, rdx_search$min_n)
    recommended_samples <- recommended_n * 2 * 4

    cat("ALTERNATIVE -- FULL FACTORIAL MAIN STUDY (2^3: Surface x Pressure x Solvent):\n")
    cat("------------------------------------------------------------------------------\n")
    cat("Not recommended as the primary plan given the reasoning above (Solvent is\n")
    cat("already well-resolved; this option spends many new samples re-confirming it\n")
    cat("while diluting the Pressure question with more dry-swab data, which cannot\n")
    cat("resolve whether Pressure matters under wet conditions -- see PER-EFFECT POWER\n")
    cat("ANALYSIS above for why the pooled/full-dataset Pressure estimate is\n")
    cat("confounded by the dry floor effect). Included for completeness/comparison:\n\n")
    cat(sprintf("  Surfaces per type: %d steel + %d abs = %d total surfaces\n",
                recommended_n, recommended_n, recommended_n * 2))
    cat("  Conditions per surface: 4 (Pressure x Solvent)\n")
    cat(sprintf("  Total samples: %d\n\n", recommended_samples))

    cat("EXPECTED POWER:\n")
    petn_expected_power <- calculate_nested_power(recommended_n, petn_target_f, petn_results$icc, alpha_level)
    rdx_expected_power <- calculate_nested_power(recommended_n, rdx_target_f, rdx_results$icc, alpha_level)
    cat(sprintf("  PETN: %.1f%%\n", petn_expected_power * 100))
    cat(sprintf("  RDX:  %.1f%%\n\n", rdx_expected_power * 100))

    cat("NEXT STEPS:\n")
    cat("  1. Review pilot results and diagnostics\n")
    cat(sprintf("  2. Prepare %d steel + %d abs surfaces for a wet-only, Pressure-focused\n",
                if (exists("recommended_n_wet")) recommended_n_wet else NA_integer_,
                if (exists("recommended_n_wet")) recommended_n_wet else NA_integer_))
    cat("     main study (recommended plan above) -- or the full factorial\n")
    cat(sprintf("     alternative (%d steel + %d abs) if you prefer to also gather more\n",
                recommended_n, recommended_n))
    cat("     Solvent/dry-swab data for other reasons\n")
    cat("  3. Refine protocol based on pilot learnings\n")
    cat("  4. Generate main study design matrix using doe_nested_design.R\n")
    cat("  5. Execute main study\n\n")

  } else {
    cat("ALTERNATIVE -- FULL FACTORIAL MAIN STUDY: WARNING: Insufficient power with\n")
    cat("available resources. Consider revising experimental design or target effect size\n\n")
  }

  cat("\n========================================\n")
  cat("PILOT ANALYSIS COMPLETE\n")
  cat("========================================\n\n")

  cat("Summary files and plots generated in:\n")
  cat("  ", output_dir, "\n\n")
  cat("This statistical summary (everything printed above) was saved to:\n")
  cat("  ", stats_summary_file, "\n\n")

}, finally = {
  sink()
  close(sink_con)
})

cat("Statistical summary saved to:", stats_summary_file, "\n\n")
