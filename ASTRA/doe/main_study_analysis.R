#===============================================================================
# MAIN STUDY ANALYSIS: EXPLOSIVES RECOVERY - PRESSURE DOSE-RESPONSE
#
# ONE-SCRIPT WORKFLOW, mirroring pilot_analysis.R's own structure and shared
# QC-acceptance logic (GCMSQuantitation/Code/InjectionAcceptance.R). Run this
# after GCMSQuantitation's Code/01, 02, 03 have processed each main-study
# Analysis run. This script:
#   1. Auto-discovers ALL Analysis*/Results/*_GCMSResults.csv files under the
#      main-study GC data folder (works fine with a PARTIAL dataset -- rerun
#      any time more Analysis* folders are added).
#   2. Applies the SAME QC pass/fail criteria as the pilot (shared
#      InjectionAcceptance.R), adapted for this study's MAIN_/NC naming.
#   3. Calculates PETN/RDX recovery (%), updates main_study_data_nested.csv,
#      and writes main_study_results.xlsx (analogous to
#      ASTRA_PilotStudyResults.xlsx).
#   4. (RETIRED 2026-09-02 -- see Section 4's own header comment) previously
#      generated a preliminary descriptive plot here; superseded by
#      plot_recovery_boxplot_actualpressure.R, now the sole descriptive
#      boxplot for this study.
#   5. Pools the pilot's own existing 50g/200g wet-only samples with the new
#      main-study data (Study covariate), builds an approximate
#      CumulativeUseNumber per physical surface, and -- only once enough
#      data exists across enough distinct pressure levels/surfaces -- fits
#      the combined mixed-effects model (updated 2026-09-02 -- Pressure_g is
#      now a categorical factor, not a continuous quadratic; see Section 5's
#      own header comment for why):
#        Recovery ~ Surface_type * Pressure_f + Study + (1|SurfaceID_nested)
#      plus emmeans() pairwise contrasts between every pair of pressure
#      levels, and per-Surface_type subset models, checking CumulativeUseNumber
#      as an additional covariate.
#
# IMPORTANT -- PATHS ARE PLACEHOLDERS. This script was written before any
# main-study GC-MS data had been located/confirmed. `gc_data_dir` and
# `output_dir` below are a best guess (mirroring the pilot's own OneDrive
# folder convention, "Pilot Study" -> "Main Study"). Update them to point at
# wherever your actual GC-MS Analysis*/ folders and lab-notes file live
# before running.
#
# Output:
#   - main_study_data_nested.csv / .xlsx (design + recovery + QC columns)
#   - main_study_results.xlsx (collated multi-sheet study workbook)
#   - main_study_analysis_summary.txt (only when analysis_mode == "full" AND
#     enough data exists to fit the combined model) -- includes
#     Pressure_Categorical_Hinge_PilotMain.png (Section 5e)
#
# Author: Generated for trace explosives recovery optimization (main study)
# Date: 2026-08-19
#===============================================================================

#===============================================================================
# USER CONFIGURATION
#===============================================================================

# Analysis mode: "preliminary" or "full"
#   - "preliminary": collation + descriptive plots only (ALWAYS safe to run,
#     works with any amount of partial data -- no minimum sample requirement)
#   - "full": also attempts the combined pilot+main mixed-effects model. This
#     needs a reasonable spread of pressure levels/surfaces to fit at all; if
#     there isn't enough data yet, the script prints a clear message
#     explaining what's missing and falls back to preliminary-only output
#     rather than erroring out.
analysis_mode <- "full"

# QC acceptance filter (mirrors pilot_analysis.R's use_qc_filtered_data):
#   TRUE  = only QC-accepted samples (analysis_accepted %in% c("PASS","PASS*"))
#           feed the preliminary plots and the combined model (recommended).
#   FALSE = include everything with a recovery value, regardless of QC status.
use_qc_filtered_data <- TRUE

# ---------------------------------------------------------------------------
# PATHS -- ***PLACEHOLDERS, CONFIRM/UPDATE BEFORE RUNNING***
# ---------------------------------------------------------------------------

# Repo directory (design files, this script)
data_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"

# Output directory for the main study's own results (plots, data-with-recovery,
# collated workbook). GUESSED to mirror the pilot's own OneDrive convention
# ("Pilot Study" -> "Main Study") -- update if your actual GC-MS results live
# somewhere else.
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study"

# GC Data root for the MAIN STUDY (contains Analysis1/, Analysis2/, ... each
# with a Results/ folder) -- ***CONFIRM THIS PATH***. Update to wherever your
# main-study GC-MS Analysis* folders actually live.
gc_data_dir <- file.path(output_dir, "GC Data")

# Design file (this repo's own generated design -- already correct, no need
# to change)
design_file <- file.path(data_dir, "main_study_design_nested.csv")

# Input/output file (recovery values + QC columns filled in by collation)
data_file <- file.path(output_dir, "main_study_data_nested.csv")

# Collated study workbook (analogous to ASTRA_PilotStudyResults.xlsx)
xlsx_out <- file.path(gc_data_dir, "main_study_results.xlsx")

# Full statistical summary (only written when the combined model is
# successfully fit -- see analysis_mode above)
stats_summary_file <- file.path(output_dir, "main_study_analysis_summary.txt")

# ---------------------------------------------------------------------------
# PILOT DATA (for pooling the existing 50g/200g wet-only samples)
# ---------------------------------------------------------------------------

# The pilot's own live canonical data file (per pilot_analysis.R's own
# `output_dir`/`data_file` -- unchanged from the pilot's own configuration)
pilot_data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/pilot_data_nested.csv"

# Main study's own surface-use tally (diagnostic reference from
# doe_nested_design.R -- used as a sanity cross-check on the approximate
# CumulativeUseNumber this script builds itself, see section 6 below; not a
# hard input requirement)
surface_use_tally_file <- file.path(data_dir, "main_study_surface_use_tally.csv")

# ---------------------------------------------------------------------------
# ACTUAL (ASTRA-MEASURED) SWABBING PRESSURE -- for Section 5e's own plot only
# (generate_categorical_hinge_plot()). Each study's own pressure-trace
# pipeline batch_summary.csv (Sample, Target, Mean_Pressure, ...), same files
# plot_recovery_boxplot_actualpressure.R/plot_recovery_vs_pressure_unified.R
# already join in. Joined onto combined_data by RunID <-> Sample so the
# hinge plot can position points/group means at the REAL achieved mass
# rather than the nominal design target -- see build_pooled_dataset(). Not
# used anywhere else in this script (the combined model itself still fits
# against nominal Pressure_g/Pressure_f/Pre100/Post100, unchanged).
main_pressure_file  <- file.path(output_dir, "Pressure Traces/ProcessedData/batch_summary.csv")
pilot_pressure_file <- file.path(dirname(pilot_data_file), "Pressure Traces/ProcessedData/batch_summary.csv")

# ---------------------------------------------------------------------------
# RECOVERY CALCULATION CONSTANTS (unchanged from the pilot -- same
# contamination protocol: 50uL x 200ng/uL = 10,000ng deposited, 1000uL
# extraction volume -- see doe_nested_design.R's own "Contamination details
# (unchanged from the pilot)" comment)
# ---------------------------------------------------------------------------

sample_vol_uL       <- 1000
deposit_mass_ng     <- 10000
ideal_concentration <- deposit_mass_ng / sample_vol_uL  # = 10 ng/uL at 100% recovery

# Extraction/filtration efficiency correction -- added so this study's
# recovery formula matches GCMSQuantitation/GlobalCode.R's + FINEX's
# 04_CollateStudyResults.R's "Recovery (%) = (mass / deposit) / (extraction
# * filtration) * 100" exactly (mathematically: mass/deposit here ==
# concentration/ideal_concentration, since ideal_concentration is already
# deposit_mass_ng/sample_vol_uL -- see the Recovery_pct calculation below).
#
# Updated 2026-09-10: GlobalCode.R's own two constants were simplified
# (2026-09-07/08) into a single measured "recovery_efficiency" per analyte
# (petn_recovery_efficiency <- 0.57, rdx_recovery_efficiency <- 0.54),
# measured directly via a spiked-swab experiment carried through the FULL
# extraction + 0.45um PTFE filtration workflow -- i.e. this figure already
# reflects BOTH loss mechanisms combined, and was never meant to be
# decomposed into separate extraction-only/filtration-only factors. This
# script's own formula still has two separate terms (extraction *
# filtration) rather than GlobalCode.R's one, so the combined 0.57/0.54
# value is entered into the *_extraction_efficiency constant here, with
# *_filtration_efficiency left at its neutral 1 (a no-op multiplier) --
# numerically identical to GlobalCode.R's single combined constant
# (0.57*1 == 0.57), without needing to restructure this formula. Do NOT
# additionally discount by a separate filtration factor -- that loss is
# already included in the 0.57/0.54 figure.
#
# NOTE: this is DIFFERENT from Extraction/ExtractionEfficiency.R's own
# 49.3%/72.4% result (PETN/RDX) -- that measures extraction ONLY (no
# filtration step), a genuinely different, non-comparable experiment, and
# was deliberately never applied anywhere (see ASTRA/doe/CONTEXT.md,
# September 4 2026 session). The 0.57/0.54 value used below is the later,
# more complete combined-loss measurement that FINEX's own pipeline now
# uses live.
#
# These are LOCAL copies (this study doesn't source GlobalCode.R), kept in
# sync MANUALLY with pilot_analysis.R and GlobalCode.R -- if the measured
# value is ever revised again, update all three places.
petn_extraction_efficiency <- 0.57  # Combined extraction+filtration recovery efficiency (mirrors GlobalCode.R's petn_recovery_efficiency, measured 2026-09-03/04)
rdx_extraction_efficiency  <- 0.54  # Combined extraction+filtration recovery efficiency (mirrors GlobalCode.R's rdx_recovery_efficiency, measured 2026-09-03/04)
petn_filtration_efficiency <- 1  # Neutral (no-op) -- filtration loss already included in petn_extraction_efficiency above, see comment above
rdx_filtration_efficiency  <- 1  # Neutral (no-op) -- filtration loss already included in rdx_extraction_efficiency above, see comment above

# QC acceptance threshold (matches pilot_analysis.R's qc_6ng_bias_limit, the
# code's actual applied +/-20% value)
qc_6ng_bias_limit <- 20

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
qc_bracket_exclude <- list("/Analysis2/" = c(39)
)

# Study-wide expected totals (for SampleSummary "Complete %"), per
# doe_nested_design.R's own verified design: 44 new samples, 10 NCs
expected_total_samples <- 44
expected_total_nc      <- 10

# Significance level (used by the combined-model section, if reached)
alpha_level <- 0.05

# Breakpoint (g) for the hinge/broken-stick model (Section 5c/5d) -- where
# the two linear segments (10-100g rise, 100-300g decline/plateau) join.
# Set to 100g based on the categorical model's own group means peaking
# there (see Section 5's header comment) -- update if a future dataset with
# more pressure levels suggests a different breakpoint.
PRESSURE_BREAKPOINT <- 100

# Target MEANINGFUL recovery difference (percentage points), for the
# externally-anchored / a priori power check (Section 6b) -- deliberately
# NOT derived from this study's own observed Pressure effect (that would be
# circular, see main_study_analysis_summary.txt's own caveats). 10 matches
# the pilot's own established "meaningful_recovery_difference_pct" target
# (pilot_analysis.R). Interpreted here as the minimum total range (max-min)
# across the 5 Pressure_g group means that would be considered practically
# important to detect -- update this to whatever YOU decide is practically
# meaningful, independent of what the data happens to show.
meaningful_recovery_difference_pct <- 10

# Desired power for the a priori check (Section 6b)
target_power <- 0.80

#===============================================================================
# PACKAGE LOADING
#===============================================================================

required_packages <- c("lme4", "lmerTest", "dplyr", "ggplot2", "car",
                        "effectsize", "emmeans", "pwr", "stringr", "openxlsx", "readxl",
                        "performance")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#===============================================================================
# Shared injection-acceptance logic -- SAME file pilot_analysis.R and
# GCMSQuantitation's FINEX pipeline source. Do not duplicate this logic
# locally -- see that file's own header for the full rationale.
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
# SECTION 1: DATA COLLATION FROM GC-MS RESULTS
#
# Mirrors pilot_analysis.R's collate_gcms_results() almost exactly, adapted
# for this study's MAIN_ naming and its already-batch-numbered design file
# (main_study_design_nested.csv's own `TestOrder` column already equals the
# design's BatchNumber -- see doe_nested_design.R, `TestOrder = BatchNumber`
# -- so unlike the pilot, no separate batch-organized file needs joining).
# Incremental/idempotent: safe to rerun any time new Analysis* folders show
# up, including with a PARTIAL dataset.
#===============================================================================

collate_gcms_results <- function() {

  cat("\n========================================\n")
  cat("DISCOVERING MAIN STUDY GC-MS RESULTS FILES\n")
  cat("========================================\n\n")

  results_files <- list.files(
    gc_data_dir,
    pattern    = "_GCMSResults\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )
  results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]

  if (length(results_files) == 0) {
    stop("No *_GCMSResults.csv files found under: ", gc_data_dir,
         "\n(Confirm gc_data_dir in USER CONFIGURATION points at your actual ",
         "main-study GC-MS output.)")
  }

  cat("Found", length(results_files), "results file(s):\n")
  for (f in results_files) cat("  ", f, "\n")
  cat("\n")

  all_data <- bind_rows(lapply(results_files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$SourceFile <- f
    df$Dataset <- basename(dirname(dirname(f)))
    df
  }))

  cat("Total rows loaded across all datasets:", nrow(all_data), "\n\n")

  #-----------------------------------------------------------------------------
  # Calibration stats collation (same as pilot, for the CalSummary sheet)
  #-----------------------------------------------------------------------------

  cal_stats_files <- list.files(
    gc_data_dir, pattern = "_CalibrationStats\\.xlsx$",
    full.names = TRUE, recursive = TRUE
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
    all_cal_stats <- data.frame()
  }

  #-----------------------------------------------------------------------------
  # Blank evaluation + QC bracket assignment (shared logic)
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("EVALUATING BLANKS + ASSIGNING QC BRACKETS\n")
  cat("========================================\n\n")

  petn_bias_col <- if ("petn_percent_bias_dc" %in% names(all_data)) {
    "petn_percent_bias_dc"
  } else if ("petn_percent_bias" %in% names(all_data)) {
    "petn_percent_bias"
  } else NA_character_

  rdx_bias_col <- if ("rdx_percent_bias" %in% names(all_data)) {
    "rdx_percent_bias"
  } else if ("rdx_percent_bias_dc" %in% names(all_data)) {
    "rdx_percent_bias_dc"
  } else NA_character_

  all_data <- evaluate_blanks(all_data)
  all_data <- assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col, qc_6ng_bias_limit,
                                  qc_bracket_exclude = qc_bracket_exclude)
  cat("QC bracket flags assigned.\n\n")

  #-----------------------------------------------------------------------------
  # Classify rows: MAIN sample / negative control / other
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("CLASSIFYING SAMPLE ROWS\n")
  cat("========================================\n\n")

  samples <- all_data %>% filter(Type == "Sample")
  samples$SampleName_trimmed <- str_trim(samples$SampleName)

  main_pattern <- "^MAIN_(\\d{3})$"
  # Flexible separator/prefix, mirroring the pilot's own regex resilience to
  # naming drift (see pilot_analysis.R "Negative Control Regex Fix" session).
  nc_pattern <- "^(?:MAIN_)?(Steel|ABS)Batch(\\d+)[ _]?(?:NegCtrl|NC)$"

  samples$IsMainSample      <- grepl(main_pattern, samples$SampleName_trimmed)
  samples$IsNegativeControl <- grepl(nc_pattern, samples$SampleName_trimmed, ignore.case = TRUE)

  n_other <- sum(!samples$IsMainSample & !samples$IsNegativeControl)
  if (n_other > 0) {
    other_names <- unique(samples$SampleName_trimmed[!samples$IsMainSample & !samples$IsNegativeControl])
    cat("Excluding", n_other, "non-main-study sample row(s) (e.g. test standards):\n")
    for (nm in other_names) cat("  '", nm, "'\n", sep = "")
    cat("\n")
  }

  samples <- samples %>% filter(IsMainSample | IsNegativeControl)
  cat("Main-study samples found:", sum(samples$IsMainSample), "\n")
  cat("Negative controls found: ", sum(samples$IsNegativeControl), "\n\n")

  samples$AnalysisFolder <- dirname(dirname(samples$SourceFile))

  #-----------------------------------------------------------------------------
  # Recovery calculation -- IDENTICAL logic/constants to the pilot (Section 5
  # of pilot_analysis.R): Below_LOD/Below_LOQ/no-candidate-peak all resolve
  # to a genuine 0% recovery (legitimate negative), not NA. See that file's
  # own extensive comments for the full evidentiary basis -- unchanged here
  # since this is the same analyte pair, same GC-MS pipeline, same deposit
  # mass/volume.
  #-----------------------------------------------------------------------------

  petn_snr_flag_col <- if ("petn_snr_flag" %in% names(samples)) samples$petn_snr_flag else rep(NA_character_, nrow(samples))
  rdx_snr_flag_col  <- if ("rdx_snr_flag"  %in% names(samples)) samples$rdx_snr_flag  else rep(NA_character_, nrow(samples))

  petn_treat_as_zero <- petn_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
  # Recovery (%) = (concentration / ideal_concentration) / (extraction *
  # filtration efficiency) * 100 -- matches GCMSQuantitation/GlobalCode.R's
  # + FINEX's 04_CollateStudyResults.R's "(mass / deposit) / (extraction *
  # filtration) * 100" formula exactly (concentration/ideal_concentration
  # IS mass/deposit here -- see USER CONFIGURATION comment above). Efficiency
  # terms set 2026-09-10 -- combined 0.57/0.54 extraction+filtration recovery
  # efficiency, matching GlobalCode.R's own measured petn_recovery_efficiency/
  # rdx_recovery_efficiency.
  samples$PETN_Recovery_pct <- ifelse(
    petn_treat_as_zero, 0,
    ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc > 0,
           ((samples$petn_concentration_dc / ideal_concentration) /
              (petn_extraction_efficiency * petn_filtration_efficiency)) * 100,
           ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc <= 0, 0, NA_real_))
  )

  rdx_treat_as_zero <- rdx_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
  samples$RDX_Recovery_pct <- ifelse(
    rdx_treat_as_zero, 0,
    ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration > 0,
           ((samples$rdx_concentration / ideal_concentration) /
              (rdx_extraction_efficiency * rdx_filtration_efficiency)) * 100,
           ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration <= 0, 0, NA_real_))
  )

  #-----------------------------------------------------------------------------
  # Merge with design matrix. Unlike the pilot, main_study_design_nested.csv
  # already includes NC rows directly AND its `TestOrder` column already IS
  # the batch number (doe_nested_design.R: `TestOrder = BatchNumber`) -- so a
  # single design read + a single RunID-matched update loop (below) handles
  # both sample and NC rows, no separate NC-row construction needed.
  #-----------------------------------------------------------------------------

  design <- read.csv(design_file, stringsAsFactors = FALSE)

  combined_rows <- samples %>%
    left_join(design %>% select(-any_of(c("PETN_Recovery_pct", "RDX_Recovery_pct"))),
              by = c("SampleName_trimmed" = "RunID"))

  n_unmatched <- sum(is.na(combined_rows$SurfaceID))
  if (n_unmatched > 0) {
    warning(n_unmatched, " sample row(s) could not be matched to the design matrix: ",
            paste(unique(combined_rows$SampleName_trimmed[is.na(combined_rows$SurfaceID)]), collapse = ", "))
  }

  #-----------------------------------------------------------------------------
  # QC acceptance (shared logic) + reanalysis dedup (identical to the pilot's
  # select_best_attempt()/report_multiple_attempts(), copied verbatim since
  # this study has no equivalent GCMSQuantitation function -- earliest
  # accepted attempt wins, per the pilot's own August 11, 2026 fix rationale)
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("COMPUTING QC ACCEPTANCE\n")
  cat("========================================\n\n")

  select_best_attempt <- function(df, quality_ok) {
    if (nrow(df) == 0) {
      df$N_Attempts <- integer(0)
      df$MultipleAttempts <- logical(0)
      return(df)
    }
    df$..quality_ok <- quality_ok
    # Tie-break order (see "Prefer Lowest QC Bracket Error on Reanalysis"
    # session, CONTEXT.md): among attempts of equal quality_ok, prefer the
    # one with the lowest qc_bracket_error (max abs %bias of whichever 6ng
    # QC(s) actually gated it -- see compute_injection_acceptance() in
    # Code/InjectionAcceptance.R) over an arbitrary date-based rule. NA
    # (Trace/Negative-tier attempts, gated by the SNR-based 0.2ng bracket
    # instead, which has no continuous error value) sorts last by dplyr's
    # own default, i.e. loses to any attempt with a defined, in-tolerance
    # error -- Date remains the final tie-break for genuine ties (equal
    # error, or both NA).
    if (!("qc_bracket_error" %in% names(df))) df$qc_bracket_error <- NA_real_
    df %>%
      group_by(SampleName_trimmed) %>%
      mutate(N_Attempts = n()) %>%
      ungroup() %>%
      arrange(SampleName_trimmed, desc(..quality_ok), qc_bracket_error, Date) %>%
      group_by(SampleName_trimmed) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(MultipleAttempts = N_Attempts > 1) %>%
      select(-..quality_ok)
  }

  report_multiple_attempts <- function(df, label) {
    ids <- unique(df$SampleName_trimmed[df$MultipleAttempts])
    if (length(ids) > 0) {
      cat(length(ids), label, "had multiple analysis attempts -- using the accepted attempt with the lowest QC bracket error (earliest date as tie-break):\n")
      for (id in ids) cat("  ", id, "\n")
      cat("\n")
    }
  }

  main_rows_raw <- combined_rows %>% filter(IsMainSample)
  nc_rows_raw   <- combined_rows %>% filter(IsNegativeControl)

  combined_for_accept <- bind_rows(
    main_rows_raw %>% mutate(RowType = "Sample"),
    nc_rows_raw   %>% mutate(RowType = "NC")
  )
  # is_nc: gates which of the two 0.2ng QC flags a Trace/Negative-tier row's
  # bracket check uses (see "Two Proper 0.2ng QC Flags" session, CONTEXT.md)
  # -- NCs use the SNR-only "detect" flag (a contamination check only needs
  # "was something detected"), real samples keep the stricter SNR+conc>0
  # "quant" flag (a reported recovery % needs an actual number).
  combined_for_accept <- compute_injection_acceptance(combined_for_accept,
                                                       is_nc = combined_for_accept$RowType == "NC")

  main_rows <- combined_for_accept %>% filter(RowType == "Sample")
  nc_rows   <- combined_for_accept %>% filter(RowType == "NC")

  main_rows <- select_best_attempt(main_rows, quality_ok = main_rows$analysis_accepted %in% c("PASS", "PASS*"))
  report_multiple_attempts(main_rows, "main-study sample(s)")

  n_pass      <- sum(main_rows$analysis_accepted == "PASS", na.rm = TRUE)
  n_pass_star <- sum(main_rows$analysis_accepted == "PASS*", na.rm = TRUE)
  n_fail      <- sum(grepl("^FAIL:", main_rows$analysis_accepted), na.rm = TRUE)
  cat("Main sample acceptance: PASS=", n_pass, " PASS*=", n_pass_star, " FAIL=", n_fail, "\n\n", sep = "")

  if (nrow(nc_rows) > 0) {
    nc_rows$nc_analysis_accepted <- nc_rows$analysis_accepted
    nc_rows$nc_result <- derive_nc_result(nc_rows$petn_result_tier, nc_rows$rdx_result_tier,
                                            nc_rows$nc_analysis_accepted)
    nc_rows <- select_best_attempt(nc_rows, quality_ok = nc_rows$nc_analysis_accepted %in% c("PASS", "PASS*"))
    report_multiple_attempts(nc_rows, "negative control(s)")

    n_nc_pass      <- sum(nc_rows$nc_analysis_accepted == "PASS", na.rm = TRUE)
    n_nc_pass_star <- sum(nc_rows$nc_analysis_accepted == "PASS*", na.rm = TRUE)
    n_nc_fail      <- sum(grepl("^FAIL:", nc_rows$nc_analysis_accepted), na.rm = TRUE)
    cat("NC acceptance (Stage 1): PASS=", n_nc_pass, " PASS*=", n_nc_pass_star, " FAIL=", n_nc_fail, "\n\n", sep = "")
  }

  #-----------------------------------------------------------------------------
  # STYLES + add_sheet() helper (same conditional-formatting convention as
  # pilot_analysis.R, copied so main_study_results.xlsx looks and behaves
  # identically)
  #-----------------------------------------------------------------------------

  style_green  <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  style_amber  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C5700")
  style_red    <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
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

    for (col in c("analysis_accepted", "nc_analysis_accepted")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "FAIL:", style = style_red)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "PASS*", style = style_amber)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                               type = "expression", rule = paste0('EXACT($', openxlsx::int2col(col_idx), '2,"PASS")'), style = style_green)
      }
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
    for (col in c("petn_snr_flag", "rdx_snr_flag")) {
      if (col %in% col_names) {
        col_idx <- which(col_names == col)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Quantifiable", style = style_green)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Below_LOQ", style = style_amber)
        conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Below_LOD", style = style_red)
      }
    }
    if ("nc_result" %in% col_names) {
      col_idx <- which(col_names == "nc_result")
      conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows, type = "contains", rule = "Not evaluated", style = createStyle(fgFill = "#D9D9D9"))
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
  # Update main_study_data_nested.csv. Since the design file already includes
  # NC rows in the SAME column set as samples (unlike the pilot), a single
  # RunID-matched update loop handles both -- no separate NC-row
  # data.frame() construction needed.
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("UPDATING main_study_data_nested.csv\n")
  cat("========================================\n\n")

  design_data <- design

  for (col in c("PETN_Recovery_pct", "RDX_Recovery_pct", "analysis_accepted", "Outcome",
                "petn_snr_flag", "rdx_snr_flag", "rdx_is_pa_flag",
                "petn_qc_6ng_pre", "petn_qc_6ng_post", "petn_qc_02ng_pre", "petn_qc_02ng_post",
                "rdx_qc_6ng_pre", "rdx_qc_6ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
                "GCMS_Analysis_Date", "AnalysisFolder", "SampleType",
                "nc_analysis_accepted", "nc_result", "N_Attempts", "MultipleAttempts")) {
    if (!(col %in% names(design_data))) design_data[[col]] <- NA
  }
  design_data$SampleType <- ifelse(grepl(nc_pattern, design_data$RunID, ignore.case = TRUE), "NC", "Main")

  # petn_qc_02ng_pre/post and rdx_qc_02ng_pre/post below display whichever
  # of the two 0.2ng flags actually gated that row's own acceptance (see
  # "Two Proper 0.2ng QC Flags" session, CONTEXT.md): the "_detect" (SNR
  # only) flag for NC rows, the stricter "_quant" (SNR+concentration>0)
  # flag for real samples -- NOT a fixed column, so this is resolved
  # per-row via IsNCRow below rather than a single static column name.
  all_result_rows <- bind_rows(main_rows, nc_rows)
  all_result_rows$IsNCRow <- c(rep(FALSE, nrow(main_rows)), rep(TRUE, nrow(nc_rows)))

  n_updated <- 0
  for (i in seq_len(nrow(all_result_rows))) {
    run_id <- all_result_rows$SampleName_trimmed[i]
    match_row <- which(design_data$RunID == run_id)
    if (length(match_row) == 1) {
      design_data$PETN_Recovery_pct[match_row]  <- all_result_rows$PETN_Recovery_pct[i]
      design_data$RDX_Recovery_pct[match_row]   <- all_result_rows$RDX_Recovery_pct[i]
      design_data$analysis_accepted[match_row]  <- all_result_rows$analysis_accepted[i]
      design_data$Outcome[match_row]            <- all_result_rows$Outcome[i]
      design_data$petn_snr_flag[match_row]      <- all_result_rows$petn_snr_flag[i]
      design_data$rdx_snr_flag[match_row]       <- all_result_rows$rdx_snr_flag[i]
      design_data$rdx_is_pa_flag[match_row]     <- if ("rdx_is_pa_flag" %in% names(all_result_rows)) all_result_rows$rdx_is_pa_flag[i] else NA_character_
      design_data$petn_qc_6ng_pre[match_row]    <- all_result_rows$petn_qc_6ng_pre[i]
      design_data$petn_qc_6ng_post[match_row]   <- all_result_rows$petn_qc_6ng_post[i]
      suffix02 <- if (isTRUE(all_result_rows$IsNCRow[i])) "_detect" else "_quant"
      design_data$petn_qc_02ng_pre[match_row]   <- all_result_rows[[paste0("petn_qc_02ng_pre", suffix02)]][i]
      design_data$petn_qc_02ng_post[match_row]  <- all_result_rows[[paste0("petn_qc_02ng_post", suffix02)]][i]
      design_data$rdx_qc_6ng_pre[match_row]     <- all_result_rows$rdx_qc_6ng_pre[i]
      design_data$rdx_qc_6ng_post[match_row]    <- all_result_rows$rdx_qc_6ng_post[i]
      design_data$rdx_qc_02ng_pre[match_row]    <- all_result_rows[[paste0("rdx_qc_02ng_pre", suffix02)]][i]
      design_data$rdx_qc_02ng_post[match_row]   <- all_result_rows[[paste0("rdx_qc_02ng_post", suffix02)]][i]
      design_data$GCMS_Analysis_Date[match_row] <- all_result_rows$Date[i]
      design_data$AnalysisFolder[match_row]     <- all_result_rows$AnalysisFolder[i]
      design_data$N_Attempts[match_row]         <- all_result_rows$N_Attempts[i]
      design_data$MultipleAttempts[match_row]   <- all_result_rows$MultipleAttempts[i]
      if (all_result_rows$RowType[i] == "NC") {
        design_data$nc_analysis_accepted[match_row] <- all_result_rows$nc_analysis_accepted[i]
        design_data$nc_result[match_row]             <- all_result_rows$nc_result[i]
      }
      n_updated <- n_updated + 1
    } else {
      warning("No unique match in design for sample '", run_id, "'")
    }
  }

  design_data <- design_data %>% arrange(GCMS_Analysis_Date, RunID)
  write.csv(design_data, data_file, row.names = FALSE, na = "NA")

  cat("Rows updated:", n_updated, "/", nrow(design), "\n")
  cat("Written to:", data_file, "\n")

  data_file_xlsx <- sub("\\.csv$", ".xlsx", data_file)
  wb_data <- createWorkbook()
  add_sheet(wb_data, "main_study_data_nested", design_data)
  saveWorkbook(wb_data, data_file_xlsx, overwrite = TRUE)
  cat("Written to:", data_file_xlsx, "\n\n")

  #-----------------------------------------------------------------------------
  # Build main_study_results.xlsx (All Data / Steel / ABS / Negative Controls
  # / Summary_By_Pressure / SampleSummary / CalSummary), mirroring
  # ASTRA_PilotStudyResults.xlsx's structure with Pressure_g in place of the
  # pilot's Pressure_level x Solvent_level condition grid (Solvent is fixed
  # "present" throughout the main study).
  #-----------------------------------------------------------------------------

  cat("========================================\n")
  cat("BUILDING main_study_results.xlsx\n")
  cat("========================================\n\n")

  desired_cols <- c(
    "RunID", "SampleID", "SurfaceID", "Surface_type", "TestOrder",
    "Pressure_level", "Pressure_g", "Solvent_level", "Solvent_detail",
    "analysis_accepted", "Outcome", "N_Attempts", "MultipleAttempts",
    "petn_snr_flag", "petn_concentration_dc", "PETN_Recovery_pct",
    "petn_qc_6ng_pre", "petn_qc_6ng_post", "petn_qc_02ng_pre", "petn_qc_02ng_post",
    "rdx_snr_flag", "rdx_concentration", "RDX_Recovery_pct",
    "rdx_qc_6ng_pre", "rdx_qc_6ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
    "Date", "SampleName_trimmed", "DataFile", "Dataset", "AnalysisFolder", "SourceFile"
  )

  main_rows_named <- main_rows %>% rename(RunID = SampleName_trimmed)
  available_cols <- intersect(desired_cols, names(main_rows_named))
  all_data_sheet <- main_rows_named %>%
    select(all_of(available_cols)) %>%
    arrange(Date, Dataset, Surface_type, SurfaceID, Pressure_g)

  steel_sheet <- all_data_sheet %>% filter(Surface_type == "steel")
  abs_sheet   <- all_data_sheet %>% filter(Surface_type == "abs")

  desired_cols_nc <- c(
    "RunID", "Surface_type", "nc_analysis_accepted", "nc_result", "N_Attempts", "MultipleAttempts",
    "petn_snr_flag", "petn_concentration_dc", "PETN_Recovery_pct",
    "rdx_snr_flag", "rdx_concentration", "RDX_Recovery_pct",
    "rdx_is_snr_flag", "rdx_is_pa_flag",
    "petn_qc_6ng_pre", "petn_qc_6ng_post", "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
    "petn_qc_02ng_pre", "petn_qc_02ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
    "Date", "SampleName_trimmed", "DataFile", "Dataset", "AnalysisFolder", "SourceFile"
  )
  nc_rows_named <- nc_rows %>% rename(RunID = SampleName_trimmed)
  available_cols_nc <- intersect(desired_cols_nc, names(nc_rows_named))
  nc_sheet <- if (nrow(nc_rows_named) > 0) {
    nc_rows_named %>% select(all_of(available_cols_nc)) %>% arrange(Date, Dataset, Surface_type)
  } else data.frame()

  summary_sheet <- all_data_sheet %>%
    filter(analysis_accepted %in% c("PASS", "PASS*")) %>%
    group_by(Surface_type, Pressure_g) %>%
    summarise(
      N = n(),
      PETN_Mean = mean(PETN_Recovery_pct, na.rm = TRUE),
      PETN_SD   = sd(PETN_Recovery_pct, na.rm = TRUE),
      RDX_Mean  = mean(RDX_Recovery_pct, na.rm = TRUE),
      RDX_SD    = sd(RDX_Recovery_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Surface_type, Pressure_g)

  surface_summary <- all_data_sheet %>%
    group_by(Surface_type) %>%
    summarise(
      Total = n(),
      PASS  = sum(analysis_accepted == "PASS", na.rm = TRUE),
      `PASS*` = sum(analysis_accepted == "PASS*", na.rm = TRUE),
      FAIL  = sum(grepl("^FAIL:", analysis_accepted), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`Complete %` = round(100 * (PASS + `PASS*`) / (expected_total_samples / 2), 1)) %>%
    rename(Row = Surface_type)
  for (s in c("steel", "abs")) {
    if (!(s %in% surface_summary$Row)) {
      surface_summary <- bind_rows(surface_summary,
        data.frame(Row = s, Total = 0L, PASS = 0L, `PASS*` = 0L, FAIL = 0L, `Complete %` = 0.0, check.names = FALSE))
    }
  }
  surface_summary <- surface_summary %>% arrange(match(Row, c("steel", "abs")))

  nc_summary_row <- if (nrow(nc_rows_named) > 0) {
    nc_ok <- nc_rows_named$nc_analysis_accepted %in% c("PASS", "PASS*")
    data.frame(
      Row = "NC", Total = nrow(nc_rows_named),
      PASS  = sum(nc_ok & nc_rows_named$nc_result == "Negative (clean)", na.rm = TRUE),
      `PASS*` = sum(nc_ok & grepl("^Positive:", nc_rows_named$nc_result), na.rm = TRUE),
      FAIL  = sum(!nc_ok, na.rm = TRUE), check.names = FALSE
    )
  } else {
    data.frame(Row = "NC", Total = 0L, PASS = 0L, `PASS*` = 0L, FAIL = 0L, check.names = FALSE)
  }
  nc_summary_row$`Complete %` <- round(100 * (nc_summary_row$PASS + nc_summary_row$`PASS*`) / expected_total_nc, 1)

  total_row <- data.frame(
    Row = "TOTAL", Total = sum(surface_summary$Total),
    PASS = sum(surface_summary$PASS), `PASS*` = sum(surface_summary$`PASS*`),
    FAIL = sum(surface_summary$FAIL), check.names = FALSE
  )
  total_row$`Complete %` <- round(100 * (total_row$PASS + total_row$`PASS*`) / expected_total_samples, 1)

  successfully_run_count <- total_row$PASS + total_row$`PASS*` + nc_summary_row$PASS + nc_summary_row$`PASS*`
  successfully_run_row <- data.frame(
    Row = "Successfully Run", Total = successfully_run_count,
    PASS = NA_integer_, `PASS*` = NA_integer_, FAIL = NA_integer_, `Complete %` = NA_real_,
    check.names = FALSE
  )
  sample_summary_sheet <- bind_rows(surface_summary, nc_summary_row, total_row, successfully_run_row)

  wb <- createWorkbook()
  add_sheet(wb, "All Data", all_data_sheet)
  add_sheet(wb, "Steel", steel_sheet)
  add_sheet(wb, "ABS", abs_sheet)
  add_sheet(wb, "Negative Controls", nc_sheet)
  add_sheet(wb, "Summary_By_Pressure", as.data.frame(summary_sheet))
  add_sheet(wb, "SampleSummary", sample_summary_sheet)
  add_sheet(wb, "CalSummary", as.data.frame(all_cal_stats))
  saveWorkbook(wb, xlsx_out, overwrite = TRUE)

  cat("Written to:", xlsx_out, "\n\n")
  cat("========================================\nCOLLATION COMPLETE\n========================================\n\n")
  print(sample_summary_sheet)

  invisible(NULL)
}

cat("\n========================================\nMAIN STUDY: DATA EXTRACTION FROM GC-MS RESULTS\n========================================\n\n")

tryCatch({
  collate_gcms_results()
  cat("\n")
}, error = function(e) {
  cat("Data collation failed:\n  ", conditionMessage(e), "\n")
  cat("Continuing with existing main_study_data_nested.csv (if available).\n\n")
})

#===============================================================================
# SECTION 2: LOAD MAIN STUDY DATA
#===============================================================================

if (!file.exists(data_file)) {
  stop("ERROR: ", data_file, " not found. Collation must succeed at least ",
       "once (i.e. gc_data_dir must point at real Analysis*/Results/ data) ",
       "before this script can do anything further.")
}

main_data <- read.csv(data_file, stringsAsFactors = FALSE)
cat("Main study data loaded:", nrow(main_data), "rows\n")

main_data$Surface_type <- factor(main_data$Surface_type, levels = c("steel", "abs"))

# QC filter (mirrors pilot_analysis.R's use_qc_filtered_data toggle). NC rows
# (SampleType == "NC") are excluded either way -- they have no Pressure_g to
# plot/model against.
main_data_all <- main_data
if (use_qc_filtered_data) {
  n_before <- sum(!is.na(main_data$PETN_Recovery_pct) | !is.na(main_data$RDX_Recovery_pct))
  main_data <- main_data %>%
    filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS", "PASS*")) %>%
    filter(is.na(SampleType) | SampleType != "NC")
  cat("QC FILTER: retained", nrow(main_data), "of", nrow(main_data_all), "rows (PASS/PASS* only, NC excluded)\n\n")
} else {
  main_data <- main_data %>% filter(is.na(SampleType) | SampleType != "NC")
  cat("QC FILTER: DISABLED (use_qc_filtered_data = FALSE) -- all non-NC rows with a recovery value included\n\n")
}

n_petn_data <- sum(!is.na(main_data$PETN_Recovery_pct))
n_rdx_data  <- sum(!is.na(main_data$RDX_Recovery_pct))
cat("Main-study samples with recovery data so far: PETN =", n_petn_data, " RDX =", n_rdx_data, "\n")
cat("(Design target: ", expected_total_samples, " new samples total)\n\n", sep = "")

#===============================================================================
# SECTION 3: POOL PILOT'S 50g/200g WET-ONLY DATA + BUILD CumulativeUseNumber
#
# Per CONTEXT.md's explicit "Still To Do" spec and doe_nested_design.R's own
# Section 5 ("Existing pilot data to pool"): pool the pilot's own wet-only
# (Solvent_level=="present") samples at Pressure_g %in% c(50,200) into the
# combined analysis, with a Study (Pilot vs Main) covariate. Built so the
# combined model (Section 5+) and its own plot (Section 5e) can show all 5
# pressure levels (10/50/100/200/300) with the pilot's 50g/200g points
# included, not just the 3 levels the main study collects itself.
#===============================================================================

build_pooled_dataset <- function() {

  if (!file.exists(pilot_data_file)) {
    cat("[NOTE] Pilot data file not found at:\n  ", pilot_data_file, "\n",
        "  Cannot pool pilot data or build CumulativeUseNumber. Update ",
        "pilot_data_file in USER CONFIGURATION if this path is wrong.\n\n", sep = "")
    return(NULL)
  }

  pilot_data <- read.csv(pilot_data_file, stringsAsFactors = FALSE)

  # Pilot's wet-only 50g/200g rows, QC-filtered the same way as the main data
  pilot_pool <- pilot_data %>%
    filter(Solvent_level == "present", Pressure_g %in% c(50, 200))
  if (use_qc_filtered_data) {
    pilot_pool <- pilot_pool %>% filter(analysis_accepted %in% c("PASS", "PASS*"))
  }
  pilot_pool <- pilot_pool %>% filter(is.na(SampleType) | SampleType != "NC")

  cat("Pilot rows available to pool (Solvent=present, Pressure_g in {50,200}, QC-filtered=",
      use_qc_filtered_data, "): ", nrow(pilot_pool), "\n", sep = "")

  if (nrow(pilot_pool) == 0) {
    cat("[NOTE] No pilot rows survive the pooling filter -- proceeding with main-study data only.\n\n")
  }

  pilot_pool$Study <- "Pilot"
  main_pool <- main_data %>% mutate(Study = "Main")

  common_cols <- intersect(names(pilot_pool), names(main_pool))
  combined <- bind_rows(
    pilot_pool %>% select(all_of(common_cols)),
    main_pool  %>% select(all_of(common_cols))
  )

  combined$Surface_type <- factor(combined$Surface_type, levels = c("steel", "abs"))
  combined$Study <- factor(combined$Study, levels = c("Pilot", "Main"))

  #---------------------------------------------------------------------------
  # Join each study's own ACTUAL (ASTRA-measured) achieved pressure
  # (Mean_Pressure) onto combined, by RunID <-> Sample. Both studies' RunIDs
  # are globally unique (PILOT_xxx / MAIN_xxx prefixes never collide), so the
  # two batch_summary.csv files can simply be stacked into one lookup and
  # joined in a single step, rather than joining each study separately as
  # plot_recovery_boxplot_actualpressure.R does (that script also needs each
  # study's own Target column for a different purpose; this one only needs
  # Mean_Pressure). Only used by Section 5e's own plot (real position of
  # points/group means) -- the combined model itself is untouched, still
  # fit against nominal Pressure_g/Pressure_f/Pre100/Post100.
  #---------------------------------------------------------------------------
  pressure_lookup <- bind_rows(
    if (file.exists(main_pressure_file)) read.csv(main_pressure_file, stringsAsFactors = FALSE) %>% select(Sample, Mean_Pressure) else data.frame(),
    if (file.exists(pilot_pressure_file)) read.csv(pilot_pressure_file, stringsAsFactors = FALSE) %>% select(Sample, Mean_Pressure) else data.frame()
  )
  if (nrow(pressure_lookup) > 0) {
    combined <- combined %>% left_join(pressure_lookup, by = c("RunID" = "Sample"))
    n_missing_actual <- sum(is.na(combined$Mean_Pressure))
    cat("Actual-pressure-trace match: ", nrow(combined) - n_missing_actual, "/", nrow(combined),
        " combined rows matched (", n_missing_actual, " unmatched -- see Section 5e's own console note).\n", sep = "")
  } else {
    cat("[NOTE] Neither pressure-trace batch_summary.csv found -- Section 5e's plot will fall back to nominal Pressure_g positions.\n")
    combined$Mean_Pressure <- NA_real_
  }

  # SurfaceID_nested: the pilot's own reused physical surfaces (Steel_01-04,
  # ABS_01-04) are THE SAME physical surface across both studies -- so the
  # random-effect grouping key must be Surface_type:SurfaceID (NOT including
  # Study), letting the pooled model correctly treat repeated measures on a
  # reused surface as repeated measures on ONE surface, exactly the pooling
  # logic doe_nested_design.R's design was built around.
  combined$SurfaceID_nested <- paste(combined$Surface_type, combined$SurfaceID, sep = ":")

  #---------------------------------------------------------------------------
  # CumulativeUseNumber -- APPROXIMATE, best-effort per-row physical-surface
  # use count, combining pilot + main sample rows AND both studies' negative
  # controls (NC "uses" wear the surface too even though NC rows themselves
  # never enter the recovery model). This is NOT a rigorous reconstruction --
  # a fully correct version would need the same two-tier
  # (SurfaceID/EventDate/BatchNumber) NC-matching approach already built for
  # the pilot alone in Diagnostics/investigate_rdx_batch_effect.R, extended
  # across both studies. Approximation used here:
  #   - Chronological ordering key per row: Study (Pilot always before Main)
  #     -> BatchNumber/TestOrder (ascending) -> NC-before-sample tiebreak
  #     within the same batch (matches the documented convention "NCs are
  #     taken at the START of their own batch's session").
  #   - CumulativeUseNumber = row_number() within each SurfaceID's own
  #     ordered event list.
  # Cross-check the result against main_study_surface_use_tally.csv's own
  # final TotalUses per surface (that file's number should be >= this
  # script's own max CumulativeUseNumber for any reused surface, since the
  # tally file already accounts for every NC + every pressure-level test).
  #---------------------------------------------------------------------------

  build_event_log <- function(study_label, df, batch_col, is_nc_col) {
    if (nrow(df) == 0) return(data.frame())
    data.frame(
      SurfaceID   = df$SurfaceID,
      Study       = study_label,
      BatchOrder  = suppressWarnings(as.numeric(df[[batch_col]])),
      IsNC        = df[[is_nc_col]],
      RunID       = df$RunID,
      stringsAsFactors = FALSE
    )
  }

  pilot_events <- if (file.exists(pilot_data_file)) {
    pd <- read.csv(pilot_data_file, stringsAsFactors = FALSE)
    build_event_log("Pilot", pd, "BatchNumber", "SampleType")
  } else data.frame()
  if (nrow(pilot_events) > 0) pilot_events$IsNC <- pilot_events$IsNC == "NC"

  main_events <- build_event_log("Main", main_data_all, "TestOrder", "SampleType")
  if (nrow(main_events) > 0) main_events$IsNC <- main_events$IsNC == "NC"

  all_events <- bind_rows(pilot_events, main_events) %>%
    filter(!is.na(SurfaceID), SurfaceID != "") %>%
    mutate(
      StudyOrder = ifelse(Study == "Pilot", 0, 1),
      NcTiebreak = ifelse(IsNC, 0, 1)   # NC sorts first within the same batch
    ) %>%
    arrange(SurfaceID, StudyOrder, BatchOrder, NcTiebreak) %>%
    group_by(SurfaceID) %>%
    mutate(CumulativeUseNumber = row_number()) %>%
    ungroup()

  cum_use_lookup <- all_events %>% filter(!IsNC) %>% select(RunID, CumulativeUseNumber)

  combined <- combined %>% left_join(cum_use_lookup, by = "RunID")

  if (file.exists(surface_use_tally_file)) {
    tally <- read.csv(surface_use_tally_file, stringsAsFactors = FALSE)
    max_by_surface <- all_events %>% group_by(SurfaceID) %>% summarise(MaxApprox = max(CumulativeUseNumber), .groups = "drop")
    check <- tally %>% left_join(max_by_surface, by = "SurfaceID")
    n_mismatch <- sum(check$MaxApprox > check$TotalUses, na.rm = TRUE)
    if (n_mismatch > 0) {
      cat("[WARNING] CumulativeUseNumber approximation exceeds the design's own recorded ",
          "TotalUses for ", n_mismatch, " surface(s) -- treat CumulativeUseNumber as approximate ",
          "only (see comments in build_pooled_dataset()).\n", sep = "")
    } else {
      cat("CumulativeUseNumber approximation cross-checked OK against main_study_surface_use_tally.csv ",
          "(never exceeds the design's own recorded TotalUses for any surface).\n", sep = "")
    }
  }

  cat("Combined (pilot-pooled + main) dataset: ", nrow(combined), " rows ",
      "(", sum(combined$Study == "Pilot"), " pooled pilot + ", sum(combined$Study == "Main"), " main)\n\n", sep = "")

  # Pressure_g as an ORDERED categorical factor -- see Section 5 below for why
  # this replaced the earlier quadratic (Pressure_g + I(Pressure_g^2))
  # treatment (2026-09-02 -- the quadratic model's forced-symmetric-parabola
  # EMM peaked at 200g, but the raw data and this categorical model both
  # agree the real peak is at 100g; see Archive/main_study_analysis_QUADRATIC_VERSION_2026-09-02.R
  # for the retired quadratic version). Ordered purely for sensible level
  # ordering in output/plots -- treated as a plain (unordered) factor in the
  # model formula, not polynomial contrasts.
  combined$Pressure_f <- factor(combined$Pressure_g, levels = sort(unique(combined$Pressure_g)))

  # Hinge/broken-stick predictors (added 2026-09-02, alongside Pressure_f
  # above -- see Section 5c/5d below): a two-segment linear model joined at
  # PRESSURE_BREAKPOINT (10-100g and 100-300g), motivated by the categorical
  # model's own group means peaking at 100g and both raw data + categorical
  # contrasts showing an asymmetric rise-then-decline shape a single
  # quadratic term can't represent. Pre100/Post100 together let a single
  # lm/lmer term report each segment's OWN slope directly (with its own SE/
  # p-value), rather than just the change between them:
  #   slope1 (10g->100g) = coefficient on Pre100
  #   slope2 (100g->300g) = coefficient on Post100
  # (An alternative, more common encoding -- Pressure_g + Post100 where
  # Post100's own coefficient is the CHANGE in slope, not slope2 itself --
  # is algebraically equivalent but requires an extra step to recover
  # slope2's own significance; this encoding reports both directly.)
  # NOTE: PRESSURE_BREAKPOINT itself is defined in USER CONFIGURATION at the
  # top of this script (global scope -- needed there too, by
  # fit_hinge_model()/fit_hinge_surface_subset_model() in Section 5c/5d).
  combined$Pre100  <- pmin(combined$Pressure_g, PRESSURE_BREAKPOINT) - PRESSURE_BREAKPOINT
  combined$Post100 <- pmax(0, combined$Pressure_g - PRESSURE_BREAKPOINT)

  combined
}

combined_data <- build_pooled_dataset()

#===============================================================================
# SECTION 4: (RETIRED 2026-09-02) -- was a preliminary descriptive boxplot
# (Recovery by Pressure_g, jittered points, no outlier detection). Retired
# because it had become redundant with plot_recovery_boxplot_actualpressure.R
# (same box+point+mean-diamond structure, but with points positioned by
# REAL achieved pressure instead of random jitter, outlier detection, and --
# as of the same date -- includes samples lacking a pressure-trace match
# too, positioned at nominal box centre with a distinct triangle marker, so
# no coverage is lost by preferring it). The "preliminary, works with
# partial data" justification that originally motivated a lightweight
# separate plot is now moot -- both studies are essentially complete. See
# CONTEXT.md for the full discussion. generate_preliminary_plot() itself
# has been removed, not just its call, to avoid leaving dead code behind.
#===============================================================================

#===============================================================================
# SECTION 5: COMBINED MIXED-EFFECTS MODEL (only in "full" mode, and only if
# there's enough data to fit it at all)
#
# Recovery ~ Surface_type * Pressure_f + Study + (1|SurfaceID_nested)
#
# Pressure_f is Pressure_g treated as a 5-level CATEGORICAL factor, not a
# continuous quadratic (Pressure_g + I(Pressure_g^2)) -- changed 2026-09-02.
# The quadratic model forces one smooth symmetric parabola through all 5
# group means, but the real Steel relationship rises sharply (10g->100g)
# then declines only gently (100g->300g) -- an asymmetric shape a single
# quadratic term structurally can't represent. This showed up in practice as
# a real bug: the quadratic model's own EMM claimed the pressure "peak" was
# at 200g, when both the raw data AND this categorical model's own group
# means peak at 100g (see ASTRA/doe/CONTEXT.md, "Main Study Full Run" +
# "Follow-up on MAIN_013..." sessions for the full investigation). The
# categorical factor makes no shape assumption at all -- it just asks
# directly "is level A's mean different from level B's mean?" for every
# pair, via emmeans() pairwise contrasts below -- at the cost of losing the
# ability to predict recovery at untested pressures, and somewhat less
# power per comparison than a curve that borrows strength across all levels.
# The retired quadratic version is preserved at
# Archive/main_study_analysis_QUADRATIC_VERSION_2026-09-02.R.
#
# Fit once per analyte, with a second model adding CumulativeUseNumber to
# check whether physical surface wear needs to be controlled for (several
# reused surfaces exceed the pilot's own previously-validated 6-use range
# once NCs and new pressure tests are added -- see doe_nested_design.R
# Section 4), plus per-Surface_type subset models (Part B) to test whether
# ABS shows any pressure effect on its own, independent of Steel's much
# larger effect dominating the combined model's interaction term.
#===============================================================================

# Shared helper (used by every model function below, categorical and hinge
# alike): Nakagawa's R^2 for mixed models (Nakagawa & Schielzeth 2013,
# extended 2017 -- performance::r2()). Marginal R^2 = variance explained by
# fixed effects only (Pressure/Study/Surface -- "how well does the pressure
# model itself explain recovery"). Conditional R^2 = fixed + random effects
# (adds each physical surface's own average level) -- "how well does the
# whole model, including which specific surface a sample came from, explain
# recovery". Returns NA for conditional (not both) when the random-effect
# variance itself is estimated at ~0 (a singular fit) -- performance()
# declines to report a marginal/conditional split it can't support in that
# case, rather than overclaiming; reported as "n/a" downstream, not hidden.
report_r2 <- function(model) {
  if (is.null(model)) { cat("  R^2: not available (model not fitted)\n"); return(invisible(NULL)) }
  r <- tryCatch(performance::r2(model), error = function(e) NULL)
  if (is.null(r)) { cat("  R^2: could not be computed\n"); return(invisible(NULL)) }
  marg <- as.numeric(r$R2_marginal)
  cond <- as.numeric(r$R2_conditional)
  cat(sprintf("  R^2 (marginal, fixed effects only): %.3f | R^2 (conditional, + between-surface): %s\n",
              marg, if (is.na(cond)) "n/a (singular fit)" else sprintf("%.3f", cond)))
  invisible(list(marginal = marg, conditional = cond))
}

fit_combined_model <- function(data, recovery_col, analyte_name) {

  data_clean <- data[!is.na(data[[recovery_col]]) & !is.na(data$Pressure_f), ]

  n_pressure_levels <- length(unique(data_clean$Pressure_f))
  n_surfaces        <- length(unique(data_clean$SurfaceID_nested))
  n_surface_types   <- length(unique(data_clean$Surface_type))

  cat("\n--- ", analyte_name, " combined model ---\n", sep = "")
  cat("  n =", nrow(data_clean), " | distinct Pressure_g levels =", n_pressure_levels,
      " | distinct physical surfaces =", n_surfaces, " | surface types =", n_surface_types, "\n")

  # Minimum data requirement to even attempt the model: need at least 3
  # distinct pressure levels, at least 2 surface types, and at least, say, 10
  # total observations. This is a pragmatic floor, not a formal power
  # calculation -- the point is to fail gracefully with a clear message
  # rather than crash lmer() on rank-deficient data.
  if (nrow(data_clean) < 10 || n_pressure_levels < 3 || n_surface_types < 2) {
    cat("  [NOT ENOUGH DATA YET] Need >=10 observations, >=3 distinct Pressure_g levels,\n",
        "  and both surface types represented to fit the combined model.\n",
        "  Currently: n=", nrow(data_clean), ", levels=", n_pressure_levels,
        ", surface types=", n_surface_types, ". Re-run once more data is collected.\n", sep = "")
    return(list(ok = FALSE, reason = "insufficient data"))
  }

  formula_base <- as.formula(paste0(
    recovery_col, " ~ Surface_type * Pressure_f + Study + (1|SurfaceID_nested)"
  ))

  model_base <- tryCatch(
    lmer(formula_base, data = data_clean, REML = TRUE),
    error = function(e) { cat("  [ERROR] Base model failed to fit: ", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(model_base)) return(list(ok = FALSE, reason = "model fit failed"))

  if (isSingular(model_base)) {
    cat("  [WARNING] Boundary (singular) fit: between-surface variance estimated at ~0.\n")
  }

  cat("\nType III ANOVA (base model):\n")
  anova_base <- tryCatch(car::Anova(model_base, type = 3, test.statistic = "F"), error = function(e) NULL)
  if (!is.null(anova_base)) print(anova_base)

  # CumulativeUseNumber check -- fit the same model plus this covariate, and
  # compare via a likelihood-ratio test (refit with ML for a fair
  # nested-model comparison).
  cum_use_result <- NULL
  if ("CumulativeUseNumber" %in% names(data_clean) && sum(!is.na(data_clean$CumulativeUseNumber)) >= nrow(data_clean) * 0.8) {
    formula_cum <- update(formula_base, . ~ . + CumulativeUseNumber)
    model_cum <- tryCatch(lmer(formula_cum, data = data_clean, REML = FALSE), error = function(e) NULL)
    model_base_ml <- tryCatch(lmer(formula_base, data = data_clean, REML = FALSE), error = function(e) NULL)
    if (!is.null(model_cum) && !is.null(model_base_ml)) {
      lrt <- tryCatch(anova(model_base_ml, model_cum), error = function(e) NULL)
      cat("\nCumulativeUseNumber covariate check (likelihood ratio test, base vs +CumulativeUseNumber):\n")
      if (!is.null(lrt)) print(lrt)
      cum_use_result <- list(model = model_cum, lrt = lrt)
    }
  } else {
    cat("\n[NOTE] CumulativeUseNumber not available/too many missing values -- skipping the covariate check.\n")
  }

  # Effect sizes (Type III, from the base model)
  eff <- tryCatch(effectsize::eta_squared(anova_base, partial = TRUE), error = function(e) NULL)
  if (!is.null(eff)) {
    cat("\nEffect sizes (partial eta^2):\n")
    print(eff)
  }

  # EMMs by Surface_type x Pressure_f, plus pairwise (Tukey-adjusted)
  # contrasts WITHIN each Surface_type across every pair of pressure levels
  # -- the direct test of "is 100g different from 200g" (etc.) that a
  # quadratic model's smoothed curve can't answer cleanly on its own.
  emm <- tryCatch(
    emmeans(model_base, ~ Pressure_f | Surface_type),
    error = function(e) { cat("  [NOTE] emmeans failed: ", conditionMessage(e), "\n"); NULL }
  )
  contrasts_within_surface <- NULL
  if (!is.null(emm)) {
    cat("\nEstimated marginal means by Surface_type x Pressure_g:\n")
    print(emm)

    cat("\nPairwise contrasts WITHIN each Surface_type (Tukey-adjusted):\n")
    contrasts_within_surface <- tryCatch(pairs(emm, adjust = "tukey"), error = function(e) NULL)
    if (!is.null(contrasts_within_surface)) print(contrasts_within_surface)
  }

  # Residual diagnostics
  shapiro_resid <- tryCatch(shapiro.test(residuals(model_base)), error = function(e) NULL)
  if (!is.null(shapiro_resid)) cat("\nShapiro-Wilk (residuals): p =", round(shapiro_resid$p.value, 4), "\n")

  cat("\nModel fit (Nakagawa R^2 for mixed models):\n")
  report_r2(model_base)

  ranef_vals <- tryCatch(unlist(ranef(model_base)$SurfaceID_nested), error = function(e) NULL)
  if (!is.null(ranef_vals) && length(unique(ranef_vals)) > 1) {
    shapiro_ranef <- tryCatch(shapiro.test(ranef_vals), error = function(e) NULL)
    if (!is.null(shapiro_ranef)) cat("Shapiro-Wilk (random effects): p =", round(shapiro_ranef$p.value, 4), "\n")
  } else {
    cat("Shapiro-Wilk (random effects): skipped -- BLUPs are constant (boundary/singular fit).\n")
  }

  list(ok = TRUE, model = model_base, anova = anova_base, effect_sizes = eff,
       emm = emm, contrasts = contrasts_within_surface, cum_use = cum_use_result, data_clean = data_clean,
       n_pressure_levels = n_pressure_levels, n_surface_types = n_surface_types)
}

#===============================================================================
# SECTION 5b: SURFACE-SPECIFIC SUBSET MODELS -- does ABS show ANY pressure
# effect on its own, independent of Steel's much larger effect dominating
# the combined model's Surface_type:Pressure_f interaction term? (Added
# 2026-09-02, alongside the categorical refit above -- see CONTEXT.md.)
#===============================================================================

fit_surface_subset_model <- function(data, recovery_col, analyte_name, surface_type) {

  subset_data <- data[data$Surface_type == surface_type &
                        !is.na(data[[recovery_col]]) & !is.na(data$Pressure_f), ]

  cat("\n--- ", analyte_name, " (", surface_type, "-only) ---\n", sep = "")
  cat("  n =", nrow(subset_data), " | distinct Pressure_g levels =", length(unique(subset_data$Pressure_f)), "\n")

  if (nrow(subset_data) < 10 || length(unique(subset_data$Pressure_f)) < 3) {
    cat("  [NOT ENOUGH DATA] Skipping.\n")
    return(list(ok = FALSE, reason = "insufficient data"))
  }

  formula_obj <- as.formula(paste0(recovery_col, " ~ Pressure_f + Study + (1|SurfaceID_nested)"))
  model <- tryCatch(lmer(formula_obj, data = subset_data, REML = TRUE),
                     error = function(e) { cat("  [ERROR] Model failed to fit: ", conditionMessage(e), "\n"); NULL })
  if (is.null(model)) return(list(ok = FALSE, reason = "model fit failed"))

  if (isSingular(model)) cat("  [WARNING] Boundary (singular) fit.\n")

  anova_res <- tryCatch(car::Anova(model, type = 3, test.statistic = "F"), error = function(e) NULL)
  cat("\nType III ANOVA:\n")
  if (!is.null(anova_res)) print(anova_res)

  emm <- tryCatch(emmeans(model, ~ Pressure_f), error = function(e) NULL)
  if (!is.null(emm)) {
    cat("\nEstimated marginal means (", surface_type, " only):\n", sep = "")
    print(emm)
    cat("\nPairwise contrasts (Tukey-adjusted):\n")
    contrasts_res <- tryCatch(pairs(emm, adjust = "tukey"), error = function(e) NULL)
    if (!is.null(contrasts_res)) print(contrasts_res)
  }

  if (!is.null(anova_res) && "Pressure_f" %in% rownames(anova_res)) {
    p_val <- anova_res["Pressure_f", "Pr(>F)"]
    verdict <- if (is.na(p_val)) "could not be tested" else if (p_val < alpha_level) {
      sprintf("SIGNIFICANT pressure effect detected on its own (p = %.4g)", p_val)
    } else {
      sprintf("NO significant pressure effect detected on its own (p = %.4g)", p_val)
    }
    cat("\n>>> VERDICT (", analyte_name, ", ", surface_type, "-only): ", verdict, " <<<\n", sep = "")
  }

  cat("\nModel fit (Nakagawa R^2 for mixed models):\n")
  report_r2(model)

  list(ok = TRUE, model = model, anova = anova_res, emm = emm)
}

#===============================================================================
# SECTION 5c/5d: HINGE (BROKEN-STICK) MODEL -- a second, complementary
# candidate shape alongside the categorical model above (added 2026-09-02,
# see CONTEXT.md "Two-Segment / Hinge Model Investigation").
#
# Motivation: the categorical model (5/5b) answers "which pressure levels
# differ" with no shape assumption at all, but doesn't give a single
# interpretable "rate of change" the way a curve does. The hinge model fits
# TWO linear slopes joined at PRESSURE_BREAKPOINT (100g, where the
# categorical model's own group means peak) -- directly testing the
# hypothesis that recovery rises to a point then levels off/declines,
# without quadratic's forced-symmetric-parabola assumption (which gets the
# peak location wrong -- see Section 5's header comment). Verified (before
# being added here) to fit better by AIC than both linear and the retired
# quadratic model, and to explain nearly as much variance (Nakagawa R^2) as
# the fully-flexible categorical model despite using far fewer parameters.
#
# Section 5c: combined model (Surface_type x Pre100/Post100 interaction).
# Section 5d: per-Surface_type subset models -- reports each surface's own
# slope1 (10-100g) and slope2 (100-300g) directly, with its own SE/p-value,
# so "is segment 2 a genuine decline, or just a plateau" has a clean answer
# per surface/analyte (this is precisely what distinguished, e.g., a
# significant Steel decline from a non-significant ABS-PETN plateau in the
# original investigation).
#===============================================================================

fit_hinge_model <- function(data, recovery_col, analyte_name) {

  data_clean <- data[!is.na(data[[recovery_col]]) & !is.na(data$Pre100), ]

  cat("\n--- ", analyte_name, " hinge model (breakpoint = ", PRESSURE_BREAKPOINT, "g) ---\n", sep = "")
  cat("  n =", nrow(data_clean), "\n")

  if (nrow(data_clean) < 10) {
    cat("  [NOT ENOUGH DATA YET] Skipping.\n")
    return(list(ok = FALSE, reason = "insufficient data"))
  }

  formula_base <- as.formula(paste0(
    recovery_col, " ~ Surface_type * (Pre100 + Post100) + Study + (1|SurfaceID_nested)"
  ))

  model <- tryCatch(
    lmer(formula_base, data = data_clean, REML = TRUE),
    error = function(e) { cat("  [ERROR] Model failed to fit: ", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(model)) return(list(ok = FALSE, reason = "model fit failed"))

  if (isSingular(model)) cat("  [WARNING] Boundary (singular) fit: between-surface variance estimated at ~0.\n")

  cat("\nFixed effects (Pre100 = slope 10-100g, Post100 = slope 100-300g, per Surface_type):\n")
  print(summary(model)$coefficients)

  cat("\nType III ANOVA:\n")
  anova_res <- tryCatch(car::Anova(model, type = 3, test.statistic = "F"), error = function(e) NULL)
  if (!is.null(anova_res)) print(anova_res)

  eff <- tryCatch(effectsize::eta_squared(anova_res, partial = TRUE), error = function(e) NULL)
  if (!is.null(eff)) {
    cat("\nEffect sizes (partial eta^2):\n")
    print(eff)
  }

  cat("\nModel fit (Nakagawa R^2 for mixed models):\n")
  report_r2(model)

  list(ok = TRUE, model = model, anova = anova_res, effect_sizes = eff, data_clean = data_clean,
       n_pressure_levels = length(unique(data_clean$Pressure_f)),
       n_surface_types = length(unique(data_clean$Surface_type)))
}

fit_hinge_surface_subset_model <- function(data, recovery_col, analyte_name, surface_type) {

  subset_data <- data[data$Surface_type == surface_type &
                        !is.na(data[[recovery_col]]) & !is.na(data$Pre100), ]

  cat("\n--- ", analyte_name, " (", surface_type, "-only) hinge model ---\n", sep = "")
  cat("  n =", nrow(subset_data), "\n")

  if (nrow(subset_data) < 10) {
    cat("  [NOT ENOUGH DATA] Skipping.\n")
    return(list(ok = FALSE, reason = "insufficient data"))
  }

  formula_obj <- as.formula(paste0(recovery_col, " ~ Pre100 + Post100 + Study + (1|SurfaceID_nested)"))
  model <- tryCatch(lmer(formula_obj, data = subset_data, REML = TRUE),
                     error = function(e) { cat("  [ERROR] Model failed to fit: ", conditionMessage(e), "\n"); NULL })
  if (is.null(model)) return(list(ok = FALSE, reason = "model fit failed"))

  if (isSingular(model)) cat("  [WARNING] Boundary (singular) fit.\n")

  co <- summary(model)$coefficients
  print(co)

  slope1 <- co["Pre100", "Estimate"]
  slope2 <- co["Post100", "Estimate"]
  p1 <- co["Pre100", "Pr(>|t|)"]
  p2 <- co["Post100", "Pr(>|t|)"]

  cat(sprintf("\n  Slope 10-100g:  %+.4f %%/g  (p = %.4g)\n", slope1, p1))
  cat(sprintf("  Slope 100-300g: %+.4f %%/g  (p = %.4g)\n", slope2, p2))

  verdict <- if (is.na(p2)) {
    "could not be tested"
  } else if (p2 < alpha_level && slope2 < 0) {
    "SIGNIFICANT DECLINE after the breakpoint"
  } else if (p2 < alpha_level && slope2 > 0) {
    "SIGNIFICANT CONTINUED RISE after the breakpoint (no leveling off)"
  } else {
    "NOT significant -- consistent with a PLATEAU, not a decline or continued rise"
  }
  cat(sprintf("\n>>> VERDICT (%s, %s-only, post-%dg slope): %s <<<\n",
              analyte_name, surface_type, PRESSURE_BREAKPOINT, verdict))

  cat("\nModel fit (Nakagawa R^2 for mixed models):\n")
  report_r2(model)

  list(ok = TRUE, model = model, slope1 = slope1, slope2 = slope2, p1 = p1, p2 = p2)
}

#===============================================================================
# SECTION 5e: CATEGORICAL + HINGE PLOT -- the primary models' own figure.
#
# Added 2026-09-02, replacing the now-orphaned Pressure_Categorical_Means_
# PilotMain.png (previously produced by the standalone
# main_study_analysis_categorical_pressure.R script, now archived and no
# longer able to regenerate it -- see CONTEXT.md). Self-contained (takes
# `data` directly, refits its own lightweight per-surface models purely for
# the plotted line), self-contained rather than threading the Section
# 5/5d model objects through as arguments.
#
# Shows BOTH primary models on one figure per Surface_type x Analyte panel:
#   - Categorical group means +/- SE (points/error bars) -- the "no shape
#     assumption" empirical view.
#   - The hinge model's own fitted two-segment line, evaluated at exactly
#     the 5 real pressure levels (10/50/100/200/300g) -- since the hinge
#     model is piecewise LINEAR with a breakpoint exactly at one of these
#     levels (100g), connecting these 5 fitted points with straight lines
#     reproduces the true fitted curve exactly, no interpolation error.
#===============================================================================

generate_categorical_hinge_plot <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    cat("  Skipping categorical+hinge plot -- no pooled data available\n")
    return(invisible(NULL))
  }

  pressure_levels <- sort(unique(data$Pressure_g))

  # NOTE: Mean_Pressure (joined in build_pooled_dataset()) is the ASTRA-
  # measured ACTUAL achieved mass for each sample -- used below to position
  # both the individual points and the categorical group means/hinge line at
  # their real achieved mass rather than the nominal design target
  # (Pressure_g). A sample with no pressure-trace match (Mean_Pressure NA --
  # 4 known cases, PILOT_015/020 x 2 analytes, ABS 200g wet) has no real
  # position to plot at and is excluded from the point layer below (still
  # counted in its group's mean_rec/se_rec/n, which use Recovery only).
  # Study (Pilot/Main) carried through to long_data (Added per explicit
  # request) so individual points can be shape-differentiated by which
  # study they came from -- see scale_shape_manual() below (Pilot = hollow
  # circle, Main = solid circle).
  long_data <- bind_rows(
    data %>% filter(!is.na(PETN_Recovery_pct), !is.na(Pressure_g)) %>%
      transmute(Surface_type, Pressure_g, Mean_Pressure, Study, Recovery = PETN_Recovery_pct, Analyte = "PETN"),
    data %>% filter(!is.na(RDX_Recovery_pct), !is.na(Pressure_g)) %>%
      transmute(Surface_type, Pressure_g, Mean_Pressure, Study, Recovery = RDX_Recovery_pct, Analyte = "RDX")
  )
  if (nrow(long_data) == 0) {
    cat("  Skipping categorical+hinge plot -- no data with both a recovery value and Pressure_g yet\n")
    return(invisible(NULL))
  }
  long_data$Analyte <- factor(long_data$Analyte, levels = c("PETN", "RDX"))

  n_missing_actual <- sum(is.na(long_data$Mean_Pressure))
  if (n_missing_actual > 0) {
    cat("  NOTE: ", n_missing_actual, " sample x analyte point(s) have no actual-pressure-trace match -- ",
        "excluded from the individual-point layer (still contribute to their group's mean_rec/n).\n", sep = "")
  }

  # Categorical group means +/- SE (y-axis, unchanged -- based on Recovery
  # only) PLUS the group's own mean ACTUAL achieved mass (x-axis position
  # for the diamond -- na.rm = TRUE so the 4 unmatched samples noted above
  # don't turn a whole group's position into NA, they just don't contribute
  # to it).
  means_data <- long_data %>%
    group_by(Surface_type, Analyte, Pressure_g) %>%
    summarise(mean_rec = mean(Recovery), se_rec = sd(Recovery) / sqrt(n()), n = n(),
              mean_actual = mean(Mean_Pressure, na.rm = TRUE), .groups = "drop")

  # Hinge model fitted line, per Surface_type x Analyte, evaluated at the
  # real pressure levels. Fit directly on `data` (not long_data, since lmer
  # needs SurfaceID_nested/Study, which were dropped by the transmute above).
  # The model itself is STILL fit against nominal Pre100/Post100 (unchanged)
  # -- only the line's PLOTTED x-position (PlotX below) is remapped to each
  # level's own actual mean achieved mass, so the line lands among the
  # points/diamonds it's meant to describe rather than at the nominal target.
  hinge_lines <- list()
  for (an in c("PETN", "RDX")) {
    recovery_col <- paste0(an, "_Recovery_pct")
    for (surf in c("steel", "abs")) {
      sub <- data[data$Surface_type == surf & !is.na(data[[recovery_col]]) & !is.na(data$Pre100), ]
      if (nrow(sub) < 10) next
      m <- tryCatch(
        lmer(as.formula(paste0(recovery_col, " ~ Pre100 + Post100 + Study + (1|SurfaceID_nested)")),
             data = sub, REML = TRUE),
        error = function(e) NULL
      )
      if (is.null(m)) next
      # Predict at Study = "Main" (arbitrary reference level -- the fitted
      # line's SHAPE, which is what this plot is for, is identical for
      # either Study level; only the intercept shifts) with each physical
      # surface's own random effect averaged out (re.form = NA).
      pred_df <- data.frame(
        Pressure_g = pressure_levels,
        Pre100  = pmin(pressure_levels, PRESSURE_BREAKPOINT) - PRESSURE_BREAKPOINT,
        Post100 = pmax(0, pressure_levels - PRESSURE_BREAKPOINT),
        Study = factor("Main", levels = levels(sub$Study))
      )
      pred_df$fitted <- predict(m, newdata = pred_df, re.form = NA)
      pred_df$Surface_type <- surf
      pred_df$Analyte <- an

      # Remap each nominal level's plotting x-position to this Surface's own
      # mean ACTUAL achieved mass at that level (falls back to the nominal
      # value itself if a level has no matched samples in `sub` at all).
      achieved_by_level <- sub %>% group_by(Pressure_g) %>%
        summarise(mean_actual = mean(Mean_Pressure, na.rm = TRUE), .groups = "drop")
      pred_df <- pred_df %>% left_join(achieved_by_level, by = "Pressure_g")
      pred_df$PlotX <- ifelse(is.na(pred_df$mean_actual), pred_df$Pressure_g, pred_df$mean_actual)

      hinge_lines[[paste(an, surf)]] <- pred_df
    }
  }
  hinge_lines_df <- bind_rows(hinge_lines)
  if (nrow(hinge_lines_df) > 0) hinge_lines_df$Analyte <- factor(hinge_lines_df$Analyte, levels = c("PETN", "RDX"))

  # Diamond (categorical mean) fill uses pal_surface_type_mean (a lighter
  # variant of pal_surface_type, shared via thesis_palette.R) rather than
  # the same hues as the individual sample points -- Added per explicit
  # request (both layers previously shared the exact same steel/abs hues,
  # distinguished only by shape, which made the two layers hard to tell
  # apart at a glance). Same steel/abs hue identity is kept (still
  # blue-ish=steel/orange-ish=abs), just lighter/more pastel, so the
  # diamonds read as a distinct "summary" layer sitting on top of the more
  # saturated individual points.

  p <- ggplot(means_data, aes(x = mean_actual, y = mean_rec, color = Surface_type)) +
    geom_point(data = long_data, aes(x = Mean_Pressure, y = Recovery, color = Surface_type, shape = Study),
               size = 2.1, alpha = 0.85, inherit.aes = FALSE) +
    geom_point(aes(fill = Surface_type), shape = 23, size = 3.8, color = "black", stroke = 0.6) +
    geom_errorbar(aes(ymin = mean_rec - se_rec, ymax = mean_rec + se_rec), width = 8, color = "black") +
    geom_text(aes(label = paste0("n=", n), y = mean_rec + se_rec + 1.5), size = 2.5, show.legend = FALSE) +
    { if (nrow(hinge_lines_df) > 0) {
        geom_line(data = hinge_lines_df, aes(x = PlotX, y = fitted, color = Surface_type),
                  linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE)
      } else NULL } +
    geom_vline(xintercept = PRESSURE_BREAKPOINT, linetype = "dotted", color = "gray50", linewidth = 0.4) +
    scale_x_continuous(breaks = pressure_levels) +
    scale_color_manual(values = pal_surface_type,
                       labels = c(steel = "Steel", abs = "ABS")) +
    scale_fill_manual(values = pal_surface_type_mean, guide = "none") +
    scale_shape_manual(values = c(Pilot = 1, Main = 19), name = "Study") +
    facet_wrap(~ Analyte, ncol = 1) +
    labs(
      title = paste0("Categorical means + hinge model fit -- Pilot + Main Study (n=", nrow(long_data), ")"),
      subtitle = paste0("Circles = individual samples at their own ACTUAL achieved mass (hollow = Pilot, solid = Main) | Diamonds (lighter fill) + black error bars = group mean +/- SE at the group's mean ACTUAL achieved mass\n",
                        "Dashed line = hinge model fit (nominal breakpoint ", PRESSURE_BREAKPOINT, "g, dotted vertical line) | x-axis breaks = nominal design targets (10/50/100/200/300g), for reference only"),
      x = "Actual Applied Mass (g)", y = "Recovery (%)", color = "Surface"
    ) +
    theme_bw(base_size = 12) +
    theme(strip.text = element_text(size = 12, face = "bold"), plot.title = element_text(size = 13, face = "bold"),
          plot.subtitle = element_text(size = 8))

  out_file <- file.path(output_dir, "Pressure_Categorical_Hinge_PilotMain.png")
  ggsave(out_file, p, width = 11, height = 9, dpi = 300)
  cat("  Saved:", out_file, "\n")
  invisible(p)
}


#
# Mirrors the pilot's own established "PER-EFFECT POWER ANALYSIS" convention
# (pilot_analysis.R, find_min_n_for_power()): for each Type III effect in the
# just-fitted combined model, converts the observed partial eta^2 into a
# Cohen's f and runs pwr::pwr.anova.test() two ways:
#   - "Optimistic": using the point-estimate Cohen's f (what the data
#     literally shows so far).
#   - "Conservative": using the lower bound of that effect's 95% CI on
#     partial eta^2 -- the more defensible planning figure, since with only
#     partial data the point estimate is itself highly uncertain (this is
#     the exact same retrospective-power caution the pilot's own analysis
#     flagged repeatedly -- see CONTEXT.md "Power Analysis Overhaul" and
#     "IMPORTANT CAVEAT" sections).
#
# k (number of "groups") per effect is chosen to match what a 1-way/interaction
# ANOVA-equivalent test of that specific effect would use -- e.g. k=5 for the
# Pressure_g main effect (5 pressure levels), k=2 for Surface_type, k=10 for
# the Surface_type:Pressure_g interaction (2 surface types x 5 levels) -- a
# standard, if approximate, way to translate a mixed-model Type III effect
# into a pwr::pwr.anova.test()-compatible design, exactly as the pilot's own
# power analysis did for its own (simpler) 2-level factors.
#
# n_target = 6 matches doe_nested_design.R's own explicit per-pressure-level,
# per-surface-type replication target (n=6/surface type/level, 12 combined,
# after pooling in the pilot's existing 50g/200g data).
#===============================================================================

run_power_analysis <- function(fit, analyte_name, n_target = 6, alpha = alpha_level) {

  if (is.null(fit) || !isTRUE(fit$ok) || is.null(fit$effect_sizes)) {
    cat("\n[NOTE] ", analyte_name, ": no fitted model/effect sizes available -- skipping power analysis.\n", sep = "")
    return(invisible(NULL))
  }

  eff <- fit$effect_sizes
  n_pressure_levels <- fit$n_pressure_levels
  n_surface_types   <- fit$n_surface_types

  k_map <- c(
    "Surface_type"            = n_surface_types,
    "Pressure_f"              = n_pressure_levels,
    "Study"                   = 2,
    "Surface_type:Pressure_f" = n_surface_types * n_pressure_levels
  )

  eta2_to_f <- function(eta2) {
    eta2 <- max(eta2, 0)
    if (eta2 >= 1) return(Inf)
    sqrt(eta2 / (1 - eta2))
  }

  power_at_n <- function(k, n, f) {
    if (is.na(f) || is.infinite(f) || f <= 0 || k < 2) return(NA_real_)
    tryCatch(pwr::pwr.anova.test(k = k, n = n, f = f, sig.level = alpha)$power,
             error = function(e) NA_real_)
  }
  min_n_for_80pct <- function(k, f) {
    if (is.na(f) || is.infinite(f) || f <= 0 || k < 2) return(NA_real_)
    tryCatch(ceiling(pwr::pwr.anova.test(k = k, f = f, sig.level = alpha, power = 0.80)$n),
             error = function(e) NA_real_)
  }

  cat("\n--- ", analyte_name, ": per-effect power analysis (target n=", n_target,
      "/level/surface-type) ---\n", sep = "")
  cat("[IMPORTANT CAVEAT] This is a RETROSPECTIVE/OBSERVED power estimate computed\n",
      "from the partial data collected so far (n=", nrow(fit$data_clean), "). Effect sizes\n",
      "will change -- possibly substantially -- as more data is collected; a retrospective\n",
      "estimate computed from a dataset and then judged against that SAME dataset's own size\n",
      "is inherently optimistic (see Hoenig & Heisey 2001, 'The Abuse of Power'). Treat this as\n",
      "a rough planning check, not a guarantee. 'Optimistic' = point-estimate Cohen's f;\n",
      "'Conservative' = Cohen's f from the lower bound of that effect's own 95% CI -- if that\n",
      "CI lower bound is ~0, the true effect could be negligible and no finite n would reach\n",
      "80% power; this is reported explicitly rather than hidden.\n\n", sep = "")

  results <- list()
  for (i in seq_len(nrow(eff))) {
    term <- eff$Parameter[i]
    if (term == "(Intercept)" || !(term %in% names(k_map))) next
    k <- unname(k_map[term])
    if (is.na(k) || k < 2) next

    eta2_opt <- eff$Eta2_partial[i]
    eta2_low <- max(eff$CI_low[i], 0)

    f_opt <- eta2_to_f(eta2_opt)
    f_con <- if (eta2_low <= 1e-6) NA_real_ else eta2_to_f(eta2_low)

    results[[length(results) + 1]] <- data.frame(
      Effect = term,
      k_groups = k,
      Eta2_partial = round(eta2_opt, 3),
      Eta2_CI_low = round(eta2_low, 3),
      Cohens_f_optimistic = round(f_opt, 3),
      Cohens_f_conservative = if (is.na(f_con)) NA_real_ else round(f_con, 3),
      Power_at_n6_optimistic = round(power_at_n(k, n_target, f_opt), 3),
      Power_at_n6_conservative = round(power_at_n(k, n_target, f_con), 3),
      MinN_for_80pct_optimistic = min_n_for_80pct(k, f_opt),
      MinN_for_80pct_conservative = min_n_for_80pct(k, f_con),
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) {
    cat("  No effects with a recognised k mapping -- nothing to report.\n")
    return(invisible(NULL))
  }

  power_table <- bind_rows(results)
  print(power_table, row.names = FALSE)

  # Plain-English verdict per effect, focused on the actual question asked:
  # "is n=6/level/surface-type sufficient for 80% power?"
  cat("\nVerdict (n=", n_target, "/level/surface-type, alpha=", alpha, "):\n", sep = "")
  for (i in seq_len(nrow(power_table))) {
    r <- power_table[i, ]
    opt_txt <- if (is.na(r$Power_at_n6_optimistic)) "n/a" else paste0(round(100 * r$Power_at_n6_optimistic), "%")
    con_txt <- if (is.na(r$Power_at_n6_conservative)) {
      "effect could be ~0 -- no finite n would reach 80% power on this data alone"
    } else {
      paste0(round(100 * r$Power_at_n6_conservative), "%")
    }
    verdict <- if (!is.na(r$Power_at_n6_optimistic) && r$Power_at_n6_optimistic >= 0.80 &&
                   !is.na(r$Power_at_n6_conservative) && r$Power_at_n6_conservative >= 0.80) {
      "YES, likely sufficient (both optimistic and conservative estimates clear 80%)"
    } else if (!is.na(r$Power_at_n6_optimistic) && r$Power_at_n6_optimistic >= 0.80) {
      "MAYBE -- optimistic estimate clears 80%, but the conservative estimate does not (or is undetermined)"
    } else {
      "NO, likely NOT sufficient on current evidence"
    }
    cat(sprintf("  %-30s optimistic power=%s | conservative power=%s -> %s\n",
                r$Effect, opt_txt, con_txt, verdict))
  }

  invisible(power_table)
}

#===============================================================================
# SECTION 6b: A PRIORI / MDES POWER CHECK -- the NON-CIRCULAR alternative to
# Section 6 above.
#
# Section 6's "retrospective power" plugs the OBSERVED effect size back into
# a power formula -- a well-documented statistical pitfall (Hoenig & Heisey,
# 2001, "The Abuse of Power", Am. Statistician 55(1):19-24): observed power
# is mathematically just a monotonic transform of the p-value, not new
# information. A significant result will nearly always show "adequate"
# retrospective power; a non-significant one will nearly always show
# "inadequate" power -- regardless of whether the TRUE effect is large or
# small. It cannot honestly answer "was n enough?".
#
# This section answers the question the RIGHT way, mirroring the exact
# approach the pilot's own script used to avoid the same trap
# (pressure_literature_effect_pct, an externally-anchored, non-circular
# target -- see pilot_analysis.R's own "SCENARIO: WET-SWAB-ONLY" section):
#   1. Fix the TARGET effect at meaningful_recovery_difference_pct (decided
#      independently of what this study's own data shows -- update the
#      constant in USER CONFIGURATION if you want a different practical
#      threshold). This is the total range (max - min) across the 5
#      Pressure_g group means that would be considered practically
#      important, NOT what was actually observed.
#   2. Combine it with the ACTUAL residual + between-surface variance from
#      the fitted model (legitimate to take from the data -- it's a nuisance/
#      noise parameter, not the effect being judged) to get a target Cohen's
#      f: f = target_range / (2*sqrt(3)*sigma_total) (the standard "equally-
#      spaced group means spanning a known range" formula used by G*Power
#      and similar tools for one-way designs).
#   3. Report BOTH directions:
#        - Power to detect the target effect at n=6/level (a priori check)
#        - Minimum Detectable Effect Size (MDES) at 80% power, n=6/level,
#          given the noise actually observed (the "what could we have found"
#          check) -- report this ALONGSIDE the actual observed effect + CI
#          from Section 6, and let the two numbers speak for themselves,
#          rather than collapsing to a single circular yes/no verdict.
#===============================================================================

get_overall_sd <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  resid_var <- sum(vc$vcov[is.na(vc$grp) | vc$grp == "Residual"], na.rm = TRUE)
  ranef_var <- sum(vc$vcov[!is.na(vc$grp) & vc$grp != "Residual"], na.rm = TRUE)
  sqrt(resid_var + ranef_var)
}

run_apriori_power_check <- function(fit, analyte_name, target_range_pct = meaningful_recovery_difference_pct,
                                     n_target = 6, alpha = alpha_level, power_goal = target_power) {
  if (is.null(fit) || !isTRUE(fit$ok)) {
    cat("\n[NOTE] ", analyte_name, ": no fitted model available -- skipping a priori/MDES power check.\n", sep = "")
    return(invisible(NULL))
  }

  sigma_total <- tryCatch(get_overall_sd(fit$model), error = function(e) NA_real_)
  k <- fit$n_pressure_levels

  if (is.na(sigma_total) || sigma_total <= 0 || k < 2) {
    cat("\n[NOTE] ", analyte_name, ": could not extract a usable variance estimate -- skipping.\n", sep = "")
    return(invisible(NULL))
  }

  # Target Cohen's f for a range of target_range_pct spread evenly across k
  # group means (standard "range known, spacing assumed even" formula):
  # var(means) = range^2/12  ->  sd(means) = range/(2*sqrt(3))  ->  f = sd(means)/sigma
  f_target <- target_range_pct / (2 * sqrt(3) * sigma_total)

  power_at_target <- tryCatch(
    pwr::pwr.anova.test(k = k, n = n_target, f = f_target, sig.level = alpha)$power,
    error = function(e) NA_real_
  )
  min_n_for_target <- tryCatch(
    ceiling(pwr::pwr.anova.test(k = k, f = f_target, sig.level = alpha, power = power_goal)$n),
    error = function(e) NA_real_
  )

  # MDES: the smallest f (hence smallest range) detectable at power_goal with
  # n_target -- inverted direction, no reference to the observed effect at all.
  f_mdes <- tryCatch(
    pwr::pwr.anova.test(k = k, n = n_target, sig.level = alpha, power = power_goal)$f,
    error = function(e) NA_real_
  )
  range_mdes <- if (!is.na(f_mdes)) f_mdes * 2 * sqrt(3) * sigma_total else NA_real_

  cat("\n--- ", analyte_name, ": A PRIORI / MDES power check (non-circular) ---\n", sep = "")
  cat("  Total noise SD (residual + between-surface, from the FITTED model): ", round(sigma_total, 2), " percentage points\n", sep = "")
  cat("  Target effect (decided independently of this study's own data): a range of ",
      target_range_pct, " percentage points across the ", k, " Pressure_g group means\n", sep = "")
  cat("  -> Cohen's f for this target: ", round(f_target, 3), "\n", sep = "")
  cat("  -> Power to detect this target at n=", n_target, "/level: ",
      ifelse(is.na(power_at_target), "n/a", paste0(round(100 * power_at_target), "%")), "\n", sep = "")
  cat("  -> Minimum n/level for ", round(100 * power_goal), "% power to detect this target: ",
      ifelse(is.na(min_n_for_target), "n/a", min_n_for_target), "\n", sep = "")
  cat("\n  Minimum Detectable Effect Size (MDES) at n=", n_target, "/level, ", round(100 * power_goal),
      "% power: a range of ", ifelse(is.na(range_mdes), "n/a", round(range_mdes, 1)),
      " percentage points across the ", k, " Pressure_g group means.\n", sep = "")
  cat("  (Compare this MDES DIRECTLY against the actual observed effect + 95% CI from Section 6 above --\n",
      "   that comparison, not a circular power verdict, is the honest answer to 'was n=", n_target, " enough?'.\n",
      "   If the MDES is smaller than what you'd consider practically meaningful, n=", n_target,
      " was almost certainly enough to detect anything that matters; if the MDES is larger than what\n",
      "   you'd consider meaningful, a real, practically-important effect could still have been missed.)\n", sep = "")

  invisible(list(sigma_total = sigma_total, f_target = f_target, power_at_target = power_at_target,
                 min_n_for_target = min_n_for_target, range_mdes = range_mdes))
}

if (analysis_mode == "full") {

  if (is.null(combined_data)) {
    cat("========================================\n")
    cat("FULL ANALYSIS MODE REQUESTED, BUT COULD NOT BUILD THE POOLED DATASET\n")
    cat("========================================\n")
    cat("(Pilot data file not found -- see NOTE above. Falling back to preliminary-only output.)\n\n")
  } else {
    sink_con <- file(stats_summary_file, open = "wt", encoding = "UTF-8")
    sink(sink_con, split = TRUE)

    tryCatch({
      cat("========================================\n")
      cat("MAIN STUDY ANALYSIS SUMMARY\n")
      cat("Generated:", format(Sys.time()), "\n")
      cat("data_file:", data_file, "\n")
      cat("pilot_data_file:", pilot_data_file, "\n")
      cat("use_qc_filtered_data:", use_qc_filtered_data, "\n")
      cat("========================================\n\n")

      petn_fit <- fit_combined_model(combined_data, "PETN_Recovery_pct", "PETN")
      rdx_fit  <- fit_combined_model(combined_data, "RDX_Recovery_pct", "RDX")

      run_power_analysis(petn_fit, "PETN")
      run_power_analysis(rdx_fit, "RDX")

      run_apriori_power_check(petn_fit, "PETN")
      run_apriori_power_check(rdx_fit, "RDX")

      cat("\n========================================\n")
      cat("SURFACE-SPECIFIC SUBSETS (does ABS show a pressure effect on its own?)\n")
      cat("========================================\n")
      for (analyte in list(list(col = "PETN_Recovery_pct", name = "PETN"),
                           list(col = "RDX_Recovery_pct",  name = "RDX"))) {
        for (surf in c("steel", "abs")) {
          fit_surface_subset_model(combined_data, analyte$col, analyte$name, surf)
        }
      }

      cat("\n========================================\n")
      cat("HINGE (BROKEN-STICK) MODEL -- complementary to the categorical model above\n")
      cat("========================================\n")
      petn_hinge_fit <- fit_hinge_model(combined_data, "PETN_Recovery_pct", "PETN")
      rdx_hinge_fit  <- fit_hinge_model(combined_data, "RDX_Recovery_pct", "RDX")

      cat("\n--- Per-surface hinge models (slope1/slope2 with their own significance) ---\n")
      for (analyte in list(list(col = "PETN_Recovery_pct", name = "PETN"),
                           list(col = "RDX_Recovery_pct",  name = "RDX"))) {
        for (surf in c("steel", "abs")) {
          fit_hinge_surface_subset_model(combined_data, analyte$col, analyte$name, surf)
        }
      }

      cat("\n--- Categorical + hinge plot ---\n")
      generate_categorical_hinge_plot(combined_data)


      cat("\n========================================\nMAIN STUDY ANALYSIS COMPLETE\n========================================\n")
    }, finally = {
      sink()
      close(sink_con)
    })

    cat("\nStatistical summary saved to:", stats_summary_file, "\n")
  }

} else {
  cat("========================================\n")
  cat("analysis_mode = \"preliminary\" -- combined model NOT fit.\n")
  cat("========================================\n")
  cat("Preliminary plots and the collated main_study_results.xlsx above are\n")
  cat("ready to review now. Once enough new-study data has been collected\n")
  cat("(multiple pressure levels, both surface types), set analysis_mode <-\n")
  cat("\"full\" at the top of this script and re-run to fit the combined\n")
  cat("pilot+main mixed-effects model and write main_study_analysis_summary.txt.\n\n")
}

cat("========================================\n")
cat("DONE\n")
cat("========================================\n")
