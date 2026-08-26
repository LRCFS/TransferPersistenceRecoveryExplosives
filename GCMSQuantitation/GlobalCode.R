###########################################################################
#
# Detection and quantitation of PETN and RDX in swab samples  - Copyright (C) 2025
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow

# Victoria Marland, Robert Reid, Andrew Brandon, Kevin Hill, Fiona Cruickshanks,
# Craig McKenzie, Caitlyn Norman, Niamh Nic Daéid, Herve Menard

# Website: https://github.com/LRCFS/GcMsDataProcess
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
###########################################################################
#
# This general code to run first
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

# Preserve caller-supplied DataFolder/study_type (e.g. set by RunAllDatasets.R
# on an isolated dataset_env before sourcing this file) across the reset below.
# Without this, a per-dataset override set by a batch driver would be silently
# wiped out here and replaced by this file's own hardcoded defaults for every
# dataset in the batch -- see CONTEXT.md / red-team review for the incident
# this fixes.
.preset_DataFolder  <- if (exists("DataFolder", inherits = FALSE))  DataFolder  else NULL
.preset_study_type  <- if (exists("study_type", inherits = FALSE))  study_type  else NULL

rm(list = setdiff(ls(), c(".preset_DataFolder", ".preset_study_type")))

#############################################################
#####                 File requirement                  #####
#############################################################
# load the libraries
library(tidyverse)
library(scales)
library(zoo)
library(baseline)
library(writexl)
library(yaml)

#############################################################
#####                Folder & Files                     #####
#############################################################

# set files extensions
extensionCSV <- ".csv"
extensionXLSX <- ".xlsx"
extensionMS1 <- ".ms1"

# Explicit study identifier -- drives dataset-specific behaviour below
# (MinPeakHeight_PETN, drift_correction_min_qc) instead of the previous
# fragile grepl("ASTRA Swabbing", DataFolder) substring match on the folder
# path, which would silently fall back to the FINEX defaults on a folder
# rename/typo/new study with no warning. validate_setup() below checks this
# is one of the known values.
study_type <- if (!is.null(.preset_study_type)) .preset_study_type else "ASTRA"

#Specify the parent folder of the GC Data
DataFolder <- if (!is.null(.preset_DataFolder)) {
  .preset_DataFolder
} else {
  "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study/GC Data/Analysis2"
}
#DataFolder <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/Outstanding samples 2"
rm(.preset_DataFolder, .preset_study_type)
ParentFolder <- sub(".*/", "", DataFolder)

# This is where the .D files from the instrument will be placed  
dir.create(file.path(paste0(DataFolder, "/RawData/")), recursive = TRUE)

# For the code dealing with the MS1 files conversion
# This is where the MS1 TIC files converted using ProteoWizard - MSConvert will be placed  
GcDataConverterMs.dir <- paste0(DataFolder,"/GcDataConverterMs/")
dir.create(file.path(GcDataConverterMs.dir),recursive = TRUE)

# This is where the MS1 TIC files are saved after their content is reordered in specific columns and rows  
GcDataConvertedRcodeTIC.dir <- paste0(DataFolder,"/GcDataConvertedRcode/TIC/")
dir.create(file.path(GcDataConvertedRcodeTIC.dir),recursive = TRUE) # will create folder if not already there.

# This is where the MS1 SIM files are saved after their content is reordered in specific columns and rows  
GcDataConvertedRcodeSIM.dir <- paste0(DataFolder,"/GcDataConvertedRcode/SIM/")
dir.create(file.path(GcDataConvertedRcodeSIM.dir),recursive = TRUE) # will create folder if not already there.

# For code dealing in background subtraction
# where the generated figures are saved, create folder if not existing
GCSampleTrace.dir <- paste0(DataFolder,"/Results/GCSampleTrace/")
dir.create(file.path(GCSampleTrace.dir),recursive = TRUE) # will create folder if not already there.
dir.create(file.path(GCSampleTrace.dir, "TIC"), recursive = TRUE)
dir.create(file.path(GCSampleTrace.dir, "Integration"), recursive = TRUE)

# where the output data is saved
GcData.dir <- paste0(DataFolder,"/GcData/")
dir.create(file.path(GcData.dir),recursive = TRUE) # will create folder if not already there.
 
Results.dir <- paste0(DataFolder,"/Results/")
dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.

Backup.dir <- paste0(DataFolder,"/Results/Backup/")
dir.create(file.path(Backup.dir),recursive = TRUE) # will create folder if not already there.

#############################################################
#####           Constants and thresholds                #####
#############################################################

#smooth.loop <- 10 # number of smoothing repeat applied to the data
SignalMaxThresholdTIC <- 100000  # minimum peak height after background removal for TIC
                                # or there will be no peaks

# Minimum peak height (after baseline correction) for SIM peak detection
# Peaks below this threshold are not detected (no PA, PH, SNR reported)
# FINEX value (200) validated against Lab10-1, Lab17-1, ABS-S-1 (June 2026).
# ASTRA Swabbing Pilot Study (Analysis1-4) re-validated separately (Aug 2026):
# that study includes ultra-high "Test Std" (10-200ng) linearity-check
# injections that produce multi-injection PETN carryover tails far larger
# than FINEX's blanks (up to PH~9269), making "max blank PH" unusable as a
# threshold basis. Restricting to blanks NOT immediately following a
# Cal/QC>=6ng/Test Std injection ("clean" blanks, n=66 across Analysis1-4)
# gives mean=83.5, SD=32.2, max=185 (and that one 185 case itself traces to
# a carryover tail from a QC 2-4 injections after a Test Std, not true
# noise). Every genuine 0.2ng QC PETN peak across all 4 datasets (n=14,
# forced-height reprocessing) is >=190, with the two adjacent blanks
# bracketing the lowest one (Analysis4 line 45, height 190) sitting flat at
# only 55-63 with no nearby carryover source. 150 sits ~2 SD above the
# clean-blank mean (excludes all but that one explained carryover-tail
# case) while clearing every genuine ASTRA QC peak.
MinPeakHeight_PETN <- if (study_type == "ASTRA") 150 else 200
MinPeakHeight_RDX  <- 50    # Blanks always NA for RDX; lowest real 0.2ng QC PH ~61
MinPeakHeight_IS   <- 40    # 15N-RDX IS injection confirmation threshold

# Minimum IS (15N-RDX) peak AREA for a sample/QC/Cal injection to be considered
# reliably injected. This is distinct from MinPeakHeight_IS above (which only
# gates peak *detection*, i.e. whether a peak is reported at all). A peak can
# be detected yet still be at blank-noise level, which is not sufficient to
# confirm the internal standard was actually present in the injected volume.
# Validated against 9 processed FINEX datasets (July 2026): blank-channel
# noise for rdx_is_pa tops out at ~241 PA; every known genuine injection has
# rdx_is_pa >= 6,373 PA (lowest observed study-wide). Real injection failures
# found in the data measured 168.7, 261.7, or NA. 1000 sits comfortably
# between these two populations (~4x above blank noise, ~6x below the lowest
# genuine value).
MinPeakArea_IS <- 1000

# =========================================================
# TIC "other significant peak" detection (added Aug 2026)
# =========================================================
# Distinct from SignalMaxThresholdTIC above, which is tuned to catch only
# gross whole-chromatogram artefacts (e.g. massive carryover tails), not
# blank-level trace significance -- see CONTEXT.md "Blank Other-
# Significant-Peak Detection" session for the full derivation.
#
# FINAL DESIGN (Aug 2026, after 3 redesigns -- see CONTEXT.md for the
# full history): a single ABSOLUTE height floor on the baseline-corrected
# signal, exactly the same design pattern as MinPeakHeight_PETN/RDX
# above. ModPeaks()'s own candidacy check (height above LOCAL minimum,
# with a minimum width) already does legitimate local peak-shape
# detection -- no separate relative/statistical significance test on top
# of it is needed, once the input signal itself is correctly extracted.
#
# Two earlier, more complex designs were tried and abandoned:
#   1. A region-pooled noise estimate (before/after both analytes) --
#      failed because a genuine recurring peak was wide enough relative
#      to its region to bake itself into its own reference.
#   2. Per-candidate local flanking windows -- fixed (1), but then failed
#      on any chromatogram with MULTIPLE real peaks close together (a
#      candidate's flanking window would catch a NEIGHBOURING real peak
#      instead of true background, again suppressing detection of both).
#      No window geometry could fix this in a chromatogram densely
#      populated with genuine peaks (confirmed directly: ASTRA Analysis5
#      Line 25 has ~9-13 real peaks, several only 5-8s apart).
#
# The REAL root cause turned out to be upstream of all of this: the raw
# TIC signal was never deinterleaved. This instrument's "SIM/Scan"
# simultaneous acquisition mode alternates every raw TIC datapoint
# between a SIM-cycle total (sum of only the 3-4 currently-monitored
# analyte/IS ions) and a Scan-cycle total (sum across the full m/z
# 41-200 survey, confirmed 16-21+ ions per scan) -- two genuinely
# different quantities, not a duplicate/redundant read of the same one
# (which is what the existing deinterleave_sim() correctly handles for
# SIM channels). Averaging and baseline-correcting the raw alternating
# mix smeared every real peak into an apparent sawtooth "noise" floor,
# making genuine chromatographic peaks statistically indistinguishable
# from noise -- not because they were noise, but because the extraction
# itself was contaminated throughout the ENTIRE chromatogram (confirmed
# across 12 injections spanning both FINEX V8/V9 configs and ASTRA --
# always exactly 50% row reduction when deinterleaved, i.e. universal,
# not dataset-specific).
#
# Fix: deinterleave_tic() (Code/ModPeaks.R) keeps whichever point of
# each interleaved pair has the HIGHER value (a robust proxy for "this
# is the Scan-cycle point", without needing to know the exact SIM group
# composition active at that retention time) -- the OPPOSITE selection
# rule from deinterleave_sim()'s "always keep the second of the pair".
# Keeping the Scan-cycle data specifically matters because it is the
# ONLY channel that can ever show a genuine compound that isn't one of
# the specifically-monitored SIM ions -- exactly the signal this whole
# feature exists to find.
#
# Once correctly deinterleaved, a simple absolute floor cleanly
# separates real peaks from noise with NO false positives: validated
# against 14 real injections spanning both studies -- a known "busy"
# injection (ASTRA Analysis5) consistently shows several real peaks
# (matching a direct visual/manual count of the raw trace, ~9 peaks at
# this floor), while every other injection checked (including 3
# different "quiet" injections across both RT configs and both
# studies) shows ZERO candidates at this floor. The known genuine
# one-off anomaly (the very first blank of both the Lab9 and ASTRA
# Analysis4 sequences) is also still correctly detected.
#
# tic_quiet_front_edge_clear excludes a universal residual solvent-front
# artefact confirmed present (even after correct deinterleaving) in
# EVERY injection checked -- it decays to a stable background by ~140s.
# tic_other_peak_rt_buffer excludes a window around each analyte's own
# configured RT -- wide enough to clear that analyte's real peak width
# AND (confirmed via each dataset's own acqmeth.txt) this SIM/Scan
# method's own acquisition-group transition, which always falls between
# the two analytes' RTs, needing no separate handling.
#
# This check is deliberately non-gating -- see
# Diagnostics/Improved_QC_Flowchart.dot Section A (A2/A_OTHER/INSTR_LOG)
# -- it feeds an instrument-performance trend log only, and must not
# alter blank_status/petn_blank_bracket/rdx_blank_bracket.
tic_quiet_front_edge_clear    <- 150    # seconds; excludes the universal front-edge artefact
tic_other_peak_rt_buffer      <- 25     # seconds; buffer around each analyte's own configured RT
tic_other_peak_min_height     <- 20000  # absolute significance floor (validated against real data, see above)

# Maximum number of individual "other significant" TIC peaks reported as
# separate numbered columns per injection (tic_other_peak_rt1/height1/
# snr1, _rt2/..., etc, up to this count) -- added so a genuinely
# recurring-but-secondary peak isn't masked by a single stronger one-off
# peak in the same injection. tic_other_peak_count still reports the
# TRUE total found, even if it exceeds this cap.
tic_other_peak_max_report <- 5

# RT tolerance (seconds) used when grouping flagged "other" peaks across
# different injections/datasets into the same recurring-peak cluster, in
# Code/04_CollateStudyResults.R's study-wide Instrument-Performance Log.
# Deliberately a separate constant from tic_other_peak_rt_buffer above
# (that one excludes real analyte peaks from detection; this one groups
# already-detected "other" peaks by approximate position for trend
# reporting).
#
# Empirically re-derived (Aug 2026, Stage 4 of the TIC deinterleaving fix
# -- see CONTEXT.md): the study-wide peak log now contains hundreds of
# individual peak occurrences, dense enough that the ORIGINAL 15s value
# (combined with simple sequential single-linkage chaining -- since fixed,
# see Code/04_CollateStudyResults.R) was letting genuinely distinct
# recurring RTs merge into one artificially wide "cluster" spanning up to
# 78 seconds. Direct inspection of real recurring peaks showed each one is
# individually tightly reproducible -- e.g. one dominant spike at RT=227.2s
# +/- 0.7s SD (n=338, seen across 8 different datasets) -- so 5s gives a
# comfortable >6 SD margin around a single genuine repeat while being much
# tighter than the old value. Also now used as a bounded CLUSTER DIAMETER
# (not just a consecutive-gap threshold), which is what actually stops the
# chaining -- see Code/04_CollateStudyResults.R's clustering code for the
# full rationale.
tic_other_peak_cluster_tolerance <- 5


rolling.average <- 3 # value for rolling average

#############################################################
#####           Analyte Definitions                     #####
#############################################################

# SNR noise window parameters
# Default: pre-peak noise window [rt - offset - width, rt - offset]
# Compound-specific positioning (before/after) is configured in analyte definitions
noise_offset <- 10  # seconds between RT and noise window edge
noise_width  <- 10  # duration of the noise window (seconds)

# SNR thresholds for peak acceptance
# Peaks below snr_detect are considered noise (Below_LOD): PA set to NA
# Peaks between snr_detect and snr_quant are real but imprecise (Below_LOQ): PA reported, concentration set to NA
# Peaks at or above snr_quant are fully quantifiable
snr_detect <- 3    # minimum SNR for peak detection (LOD)
snr_quant  <- 10   # minimum SNR for quantitation (LOQ)

# RDX Internal Standard (15N-RDX) -- SIM ion and expected retention time
# 15N-RDX is used as the IS for both RDX and (optionally) PETN.
# Uses post-peak noise window to avoid contamination from RDX analyte peak
rdx_is_config <- list(
  mz = 122,           # m/z for 15N-RDX detection
  rt = 362,           # expected retention time (s) -- V8 method
  noise_position = "after",  # use post-peak noise window
  noise_after_offset = 10,   # seconds after RT where noise window starts
  noise_after_width = 10,    # duration of noise window
  min_peak_height = MinPeakHeight_IS  # injection confirmation threshold
)

# Post-peak noise window: [rt + offset, rt + offset + width]
rdx_is_config$noise_window <- c(rdx_is_config$rt + rdx_is_config$noise_after_offset,
                                 rdx_is_config$rt + rdx_is_config$noise_after_offset + rdx_is_config$noise_after_width)

# =========================================================
# PETN quantification method toggle
# =========================================================
# When TRUE:  PETN uses 15N-RDX IS ratio calibration (with drift correction)
# When FALSE: PETN uses peak area calibration only (with drift correction)
# Note: Power law drift correction requires use_15nrdx_for_petn = FALSE
use_15nrdx_for_petn <- FALSE

# =========================================================
# RDX quantification method toggle
# =========================================================
# When TRUE:  RDX uses 15N-RDX IS ratio calibration
# When FALSE: RDX uses peak area calibration only
use_is_for_rdx <- TRUE

# =========================================================
# Confirmatory (qualifier) ion peak height floor (added Aug 2026)
# =========================================================
# Qualifier ions are, by definition, weaker/less-consistently-monitored
# fragments than the quantifier ion for the same compound (see SIM group
# dwell tables in the acquisition method -- confirmed via acqmeth.txt that
# PETN's SIM group already monitors m/z 46 (quantifier)/57/76, and RDX's
# group already monitors m/z 46/75/120 (quantifier)/122 (IS), i.e. the
# qualifier ions below have always been acquired, just never processed).
# A permissive floor is used here deliberately -- this is Stage 1 of the
# confirmatory-ion investigation (CONTEXT.md), purely for extracting raw
# qualifier PA/PH/SNR so real ion-ratio behaviour can be characterised
# empirically before any pass/fail tolerance is set. Actual reliability
# filtering happens later via the qualifier's own SNR flag, not this floor.
MinPeakHeight_Qualifier <- 20

# =========================================================
# Analytes -- each defined by SIM ion and expected retention time
# =========================================================
# Calibration is always quadratic (no linear fallback).
# Each analyte uses a single quantification method (PA or ratio):
#   PETN: ratio (if use_15nrdx_for_petn = TRUE) or PA (if FALSE)
#   RDX:  ratio (if use_is_for_rdx = TRUE) or PA (if FALSE)
#
# `qualifiers` (added Aug 2026): confirmatory/qualifier ion m/z values for
# each analyte, used for identity confirmation (ion ratio vs the quantifier
# ion `mz` above) -- see CONTEXT.md "Confirmatory Ion Detection" session.
# Each qualifier coelutes with its analyte's own quantifier peak (same SIM
# group, same RT), so it reuses that analyte's own `rt`/`noise_window` --
# no separate RT/noise config needed per qualifier.
analytes <- list(
  PETN = list(
    mz = 46,
    rt = 338,
    noise_position = "before",  # use pre-peak noise window
    min_peak_height = MinPeakHeight_PETN,
    # PETN SIM group qualifier ion(s) (acqmeth.txt). m/z 57 was investigated
    # and deliberately excluded (see CONTEXT.md "Confirmatory Ion Detection"
    # session, Stage 2/3) -- only 72.7% detection rate in pure Cal standards,
    # just 18.2% reaching Quantifiable SNR, and would fail 70-100% of all
    # Sample/QC/Cal rows if ever required for identity confirmation. m/z 76
    # is the only PETN qualifier actually used anywhere downstream.
    qualifiers = c(76)
  ),
  RDX = list(
    mz = 120,
    rt = 362,
    noise_position = "after",   # use post-peak noise window
    noise_after_offset = 10,    # seconds after RT where noise window starts
    noise_after_width = 10,     # duration of noise window
    min_peak_height = MinPeakHeight_RDX,
    qualifiers = c(46, 75)       # RDX SIM group qualifier ions (acqmeth.txt)
  )
)

analytes <- lapply(analytes, function(a) {
  if (is.null(a$noise_position) || a$noise_position == "before") {
    a$noise_window <- c(a$rt - noise_offset - noise_width, a$rt - noise_offset)
  } else {
    offset <- if (!is.null(a$noise_after_offset)) a$noise_after_offset else noise_offset
    width  <- if (!is.null(a$noise_after_width))  a$noise_after_width  else noise_width
    a$noise_window <- c(a$rt + offset, a$rt + offset + width)
  }
  a
})

#############################################################
#####                     Sample Volumes                #####
#############################################################

#Total Volume of GCMS vial in uL
VialVol <- 100

#Volume of sample in GCMS vial in uL
AliquotVol <- 90

#Calculate dilution factor
Dilution <- AliquotVol / VialVol

#Total sample volume in uL
SampleVol <- 1000

#Volume of standard deposited on surface in uL
DepositVol <- 50

#Concentration of standard in ng/uL
DepositConc <- 1000

#Mass deposited on surface
DepositMass <- DepositVol * DepositConc

# =========================================================
# Extraction and Filtration Efficiencies
# =========================================================
# Used in CollateStudyResults to calculate true surface recovery:
#   recovery (%) = (mass_measured / mass_deposited) / 
#                  (extraction_efficiency * filtration_efficiency) * 100

# Extraction efficiency: fraction recovered from swab into ethanol
# TODO: Determine experimentally from spiked swab extraction recovery
petn_extraction_efficiency <- 1  # 60%
rdx_extraction_efficiency  <- 1  # 60%

# Filtration efficiency: fraction recovered through 0.45 µm PTFE filter
# Set to 1.0 for unfiltered samples (e.g., Lab30)
petn_filtration_efficiency <- 1  # 90%
rdx_filtration_efficiency  <- 1  # 90%

#############################################################
#####              QC Configuration                     #####
#############################################################

# Acceptance criterion: maximum allowable |%bias| for QC samples
qc_bias_limit <- 20  # percent

# PETN drift correction: apply QC-based quadratic correction.
# When TRUE, a quadratic model is fitted to the QC response across the sequence.
# When FALSE, no drift correction is applied.
apply_petn_drift_correction <- TRUE

# RDX drift correction: apply QC-based quadratic correction.
# Only used when use_is_for_rdx = FALSE (PA-only mode).
# When use_is_for_rdx = TRUE, the IS ratio correction is sufficient (~1.2% RSD).
# When TRUE and use_is_for_rdx = FALSE, a quadratic model is fitted to QC PA.
apply_rdx_drift_correction <- TRUE

# NOTE: apply_rdx_drift_correction is currently a no-op while use_is_for_rdx =
# TRUE (see comment above) -- warn at source-time so this isn't silently
# misread as "RDX drift correction is being applied" when it isn't.
if (apply_rdx_drift_correction && use_is_for_rdx) {
  message("Note: apply_rdx_drift_correction is TRUE but has no effect while ",
          "use_is_for_rdx = TRUE (RDX drift correction only applies in PA-only mode).")
}

# QC concentration level (ng) to use for drift correction.
# Options:
#   NA -- Auto-select: use the highest QC level available (default)
#   numeric -- Use specified QC level (e.g., 6 for 6ng QC)
#              Falls back to highest available if specified level not found
# Warning: The 0.2ng QC is typically unreliable for drift correction
# due to non-proportional losses at trace levels.
drift_correction_qc_level <- 6

# Drift correction method for PETN.
# Options:
#   "power_law" -- Fit power law (PA ~ a * Row^b) to QC PA decay. Used
#                  uniformly for both the FINEX "Accepted Analysis" datasets
#                  and the ASTRA Swabbing pilot study (as of Aug 2026 -- see
#                  CONTEXT.md "PowerLaw Reinstated for ASTRA" session
#                  summary). Deliberately fits whatever QC data exists for a
#                  given run with NO per-dataset shape exclusions -- e.g.
#                  Analysis2's 2-QC terminal collapse and the late-sequence
#                  upticks in Analysis1/Analysis4 are not special-cased or
#                  excluded; fit quality (R²) is reported for review rather
#                  than gated on.
#   "polynomial" -- Legacy quadratic/linear fit to correction factors.
#                   Retained for backwards compatibility, and used
#                   automatically as a fallback if the power law fit fails
#                   to converge.
# Note: Power law drift correction requires use_15nrdx_for_petn = FALSE
# (PA-only quantification).
drift_correction_method <- "power_law"

# Minimum number of same-level QC points required to attempt a PowerLaw
# fit. The general default (3) reflects that a 2-point power law fit has
# zero residual degrees of freedom and is an exact/degenerate fit, not a
# meaningful curve fit (see "Minimum QC Requirement" in CONTEXT.md). For
# the ASTRA Swabbing pilot study, this is deliberately relaxed to 2 --
# fitting whatever QC data exists (including 2-QC runs, e.g. Analysis2)
# rather than skipping correction outright; the resulting R² is reported
# in the console/plot so a poor or degenerate fit is visible, not hidden.
drift_correction_min_qc <- if (study_type == "ASTRA") 2 else 3

# Sequence Line number(s) of same-level QC injections to exclude from the
# PowerLaw/polynomial drift-correction FIT itself (e.g. an anomalous QC
# whose own shape doesn't match the rest of the run -- see the Aug 5, 2026
# investigation of Analysis4's Line 67 in CONTEXT.md). An excluded QC is
# NOT removed from the dataset or from QC evaluation: it is still assigned
# a correction factor from the resulting fit (extrapolated, since it's no
# longer one of the fitted points) and still gets its own bias/PASS-FAIL
# result exactly like any other QC -- this only controls which points the
# curve itself is fit through. Default empty (no exclusions, identical to
# previous behaviour). Set e.g. c(67) or c(31, 67) to exclude those Lines.
#
# Per-dataset lookup (added for item 19/21 red-team review): keyed by
# ParentFolder (the current dataset's own folder name) so each dataset's
# exclusions are recorded permanently and auditably, rather than being a
# single flat value that has to be remembered and hand-edited per run.
# Add entries here as further silent QC-injection failures are identified.
drift_correction_exclude_qc_lines_by_dataset <- list(
  "Steel 4" = c(33)  # 2nd 6ng PETN QC: petn_pa drops from 96584 to 8757
                      # then recovers to 28321 on the very next QC --
                      # classic failed-injection pattern, not real drift.
                      # Confirmed (Aug 2026) as the only dataset with this
                      # pattern across all 17 FINEX datasets processed to
                      # date -- checked exhaustively, not just assumed.
)
drift_correction_exclude_qc_lines <- if (ParentFolder %in% names(drift_correction_exclude_qc_lines_by_dataset)) {
  drift_correction_exclude_qc_lines_by_dataset[[ParentFolder]]
} else {
  c(0)
}

# Path to the long-term QC monitoring master file

# QC results are appended here after each run for instrument tracking
qc_monitoring_file <- file.path(
  "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/System Monitoring",
  "SystemMonitoring.xlsx"
)

#############################################################
#####              Input Validation                      #####
#############################################################

validate_setup <- function() {
  # --- DataFolder must exist ---
  if (!dir.exists(DataFolder)) {
    stop("DataFolder does not exist: ", DataFolder)
  }
  
  # --- study_type must be a recognised value ---
  # (drives MinPeakHeight_PETN / drift_correction_min_qc above -- catches a
  # typo/unset value loudly instead of silently falling back to FINEX defaults)
  if (!is.character(study_type) || length(study_type) != 1 ||
      !study_type %in% c("FINEX", "ASTRA")) {
    stop("study_type must be 'FINEX' or 'ASTRA', got: ", study_type)
  }
  
  # --- SNR thresholds must be logically ordered ---
  if (!is.numeric(snr_detect) || !is.numeric(snr_quant)) {
    stop("snr_detect and snr_quant must be numeric")
  }
  if (snr_detect >= snr_quant) {
    stop("snr_detect (", snr_detect, ") must be less than snr_quant (", snr_quant, ")")
  }
  if (snr_detect <= 0) {
    stop("snr_detect must be positive, got: ", snr_detect)
  }
  
  # --- QC bias limit must be sensible ---
  if (!is.numeric(qc_bias_limit) || qc_bias_limit <= 0 || qc_bias_limit > 100) {
    stop("qc_bias_limit must be between 0 and 100, got: ", qc_bias_limit)
  }
  
  # --- Analyte configuration ---
  for (name in names(analytes)) {
    a <- analytes[[name]]
    if (!is.numeric(a$mz) || a$mz <= 0) {
      stop("Invalid m/z for ", name, ": ", a$mz)
    }
    if (!is.numeric(a$rt) || a$rt <= 0) {
      stop("Invalid RT for ", name, ": ", a$rt)
    }
    if (is.null(a$noise_window) || length(a$noise_window) != 2) {
      stop("noise_window not computed for ", name, ". Check noise_position config.")
    }
    if (a$noise_window[1] >= a$noise_window[2]) {
      stop("noise_window start >= end for ", name, ": [", 
           a$noise_window[1], ", ", a$noise_window[2], "]")
    }
  }
  
  # --- IS configuration ---
  if (!is.numeric(rdx_is_config$mz) || rdx_is_config$mz <= 0) {
    stop("Invalid IS m/z: ", rdx_is_config$mz)
  }
  if (!is.numeric(rdx_is_config$rt) || rdx_is_config$rt <= 0) {
    stop("Invalid IS RT: ", rdx_is_config$rt)
  }
  if (is.null(rdx_is_config$noise_window) || length(rdx_is_config$noise_window) != 2) {
    stop("IS noise_window not computed. Check rdx_is_config.")
  }
  
  # --- Signal thresholds ---
  if (!is.numeric(SignalMaxThresholdTIC) || SignalMaxThresholdTIC <= 0) {
    stop("SignalMaxThresholdTIC must be a positive number")
  }
  if (!is.numeric(MinPeakHeight_PETN) || MinPeakHeight_PETN <= 0) {
    stop("MinPeakHeight_PETN must be a positive number")
  }
  if (!is.numeric(MinPeakHeight_RDX) || MinPeakHeight_RDX <= 0) {
    stop("MinPeakHeight_RDX must be a positive number")
  }
  if (!is.numeric(MinPeakHeight_IS) || MinPeakHeight_IS <= 0) {
    stop("MinPeakHeight_IS must be a positive number")
  }
  if (!is.numeric(MinPeakArea_IS) || MinPeakArea_IS <= 0) {
    stop("MinPeakArea_IS must be a positive number")
  }
  
  # --- TIC "other significant peak" detection config ---
  if (!is.numeric(tic_quiet_front_edge_clear) || tic_quiet_front_edge_clear <= 0) {
    stop("tic_quiet_front_edge_clear must be a positive number")
  }
  if (!is.numeric(tic_other_peak_rt_buffer) || tic_other_peak_rt_buffer <= 0) {
    stop("tic_other_peak_rt_buffer must be a positive number")
  }
  if (!is.numeric(tic_other_peak_min_height) || tic_other_peak_min_height <= 0) {
    stop("tic_other_peak_min_height must be a positive number")
  }
  if (!is.numeric(tic_other_peak_max_report) || tic_other_peak_max_report < 1) {
    stop("tic_other_peak_max_report must be a positive integer, got: ", tic_other_peak_max_report)
  }
  if (!is.numeric(tic_other_peak_cluster_tolerance) || tic_other_peak_cluster_tolerance <= 0) {
    stop("tic_other_peak_cluster_tolerance must be a positive number")
  }
  
  # --- Drift correction QC level ---
  if (!is.na(drift_correction_qc_level)) {
    if (!is.numeric(drift_correction_qc_level) || drift_correction_qc_level <= 0) {
      stop("drift_correction_qc_level must be a positive number or NA, got: ", 
           drift_correction_qc_level)
    }
  }
  
  # --- Drift correction method ---
  if (!drift_correction_method %in% c("power_law", "polynomial")) {
    stop("drift_correction_method must be 'power_law' or 'polynomial', got: ", 
         drift_correction_method)
  }
  
  # --- Power law requires PA-only mode (only relevant if drift correction is enabled) ---
  if (apply_petn_drift_correction && drift_correction_method == "power_law" && use_15nrdx_for_petn == TRUE) {
    stop("Power law drift correction requires use_15nrdx_for_petn = FALSE (PA-only quantification)")
  }
  
  # --- Drift correction minimum QC count ---
  if (!is.numeric(drift_correction_min_qc) || drift_correction_min_qc < 2) {
    stop("drift_correction_min_qc must be numeric and >= 2, got: ", drift_correction_min_qc)
  }
  
  # --- Drift correction excluded QC lines ---
  if (!is.numeric(drift_correction_exclude_qc_lines)) {
    stop("drift_correction_exclude_qc_lines must be a numeric vector (Line numbers), got class: ",
         class(drift_correction_exclude_qc_lines))
  }
  
  # --- Dilution factor ---

  if (!is.numeric(Dilution) || Dilution <= 0 || Dilution > 1) {
    stop("Dilution must be between 0 and 1, got: ", Dilution)
  }
  
  message("Input validation passed.")
}

# Run validation
validate_setup()

#############################################################
#####              Reproducibility Logging               #####
#############################################################

log_reproducibility <- function() {
  # Capture all loaded package versions
  loaded_pkgs <- sessionInfo()$otherPkgs
  pkg_versions <- if (!is.null(loaded_pkgs)) {
    sapply(loaded_pkgs, function(p) p$Version)
  } else {
    list()
  }
  
  # Capture configuration snapshot
  config <- list(
    DataFolder = DataFolder,
    ParentFolder = ParentFolder,
    study_type = study_type,
    use_15nrdx_for_petn = use_15nrdx_for_petn,
    use_is_for_rdx = use_is_for_rdx,
    apply_petn_drift_correction = apply_petn_drift_correction,
    apply_rdx_drift_correction = apply_rdx_drift_correction,
    drift_correction_qc_level = drift_correction_qc_level,
    drift_correction_method = drift_correction_method,
    drift_correction_min_qc = drift_correction_min_qc,
    drift_correction_exclude_qc_lines = drift_correction_exclude_qc_lines,
    snr_detect = snr_detect,

    snr_quant = snr_quant,
    qc_bias_limit = qc_bias_limit,
    noise_offset = noise_offset,
    noise_width = noise_width,
    SignalMaxThresholdTIC = SignalMaxThresholdTIC,
    MinPeakHeight_PETN = MinPeakHeight_PETN,
    MinPeakHeight_RDX = MinPeakHeight_RDX,
    MinPeakHeight_IS = MinPeakHeight_IS,
    MinPeakArea_IS = MinPeakArea_IS,
    tic_quiet_front_edge_clear = tic_quiet_front_edge_clear,
    tic_other_peak_rt_buffer = tic_other_peak_rt_buffer,
    tic_other_peak_min_height = tic_other_peak_min_height,
    tic_other_peak_max_report = tic_other_peak_max_report,
    tic_other_peak_cluster_tolerance = tic_other_peak_cluster_tolerance,
    rolling.average = rolling.average,
    VialVol = VialVol,
    AliquotVol = AliquotVol,
    Dilution = Dilution,
    SampleVol = SampleVol,
    DepositMass = DepositMass
  )
  
  # Capture analyte definitions
  analyte_summary <- lapply(analytes, function(a) {
    list(mz = a$mz, rt = a$rt, noise_position = a$noise_position,
         noise_window = a$noise_window)
  })
  
  # Capture IS config
  is_summary <- list(
    mz = rdx_is_config$mz,
    rt = rdx_is_config$rt,
    noise_position = rdx_is_config$noise_position,
    noise_window = rdx_is_config$noise_window
  )
  
  # List input files (sequence logs and data)
  input_files <- c(
    list.files(DataFolder, pattern = "Sequence.*\\.(LOG|TSV)$", full.names = FALSE),
    list.files(GcData.dir, pattern = "\\.csv$", full.names = FALSE)
  )
  
  # List output files
  output_files <- list.files(Results.dir, pattern = "\\.(csv|xlsx|png)$", 
                             full.names = FALSE, recursive = FALSE)
  
  # Build the metadata structure
  run_info <- list(
    run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    r_version = R.version.string,
    platform = R.version$platform,
    os = Sys.info()["sysname"],
    packages = as.list(pkg_versions),
    configuration = config,
    analytes = analyte_summary,
    internal_standard = is_summary,
    input_files = input_files,
    output_files = output_files
  )
  
  # Write to DataFolder (alongside the experimental data)
  log_path <- file.path(DataFolder, "run_metadata.yaml")
  write_yaml(run_info, log_path)
  
  message("Reproducibility log written to: ", log_path)
}

#############################################################
#####                   Functions                       #####
#############################################################

source("Code/ModPeaks.R")

#############################################################
#####                       Codes                       #####
#############################################################

# Pipeline scripts (run in order):
#   1. source("Code/01_MsFilesReorganiser.R")  -- Convert .ms1 files
#   2. source("Code/02_PeakDetection.R")       -- Peak detection and integration
#   3. source("Code/03_Quantification.R")      -- Calibration and quantification

source("Code/01_MsFilesReorganiser.R")
source("Code/02_PeakDetection.R")
source("Code/03_Quantification.R")
#source("FINEX/RunAllDatasets.R")

# Write reproducibility log after successful pipeline completion
log_reproducibility()
