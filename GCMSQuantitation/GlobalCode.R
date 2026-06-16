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

rm(list=ls()) 

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

#Specify the parent folder of the GC Data
DataFolder <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/Lab10-1"
ParentFolder <- sub(".*/", "", DataFolder)

# This is where the .D files from the instrument will be placed  
dir.create(file.path(paste0(DataFolder, "/RawData/")))

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
SignalMaxThresholdSIM <- 40 # minimum peak height after background removal for SIM
                              # or there will be no peaks
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
  rt = 347,           # expected retention time (s) -- V8 method
  noise_position = "after",  # use post-peak noise window
  noise_after_offset = 10,   # seconds after RT where noise window starts
  noise_after_width = 10     # duration of noise window
)

# Post-peak noise window: [rt + offset, rt + offset + width]
rdx_is_config$noise_window <- c(rdx_is_config$rt + rdx_is_config$noise_after_offset,
                                 rdx_is_config$rt + rdx_is_config$noise_after_offset + rdx_is_config$noise_after_width)

# =========================================================
# PETN quantification method toggle
# =========================================================
# When TRUE:  PETN uses 15N-RDX IS ratio calibration (with drift correction)
# When FALSE: PETN uses peak area calibration only (with drift correction)
use_15nrdx_for_petn <- TRUE

# =========================================================
# RDX quantification method toggle
# =========================================================
# When TRUE:  RDX uses 15N-RDX IS ratio calibration
# When FALSE: RDX uses peak area calibration only
use_is_for_rdx <- TRUE

# =========================================================
# Analytes -- each defined by SIM ion and expected retention time
# =========================================================
# Calibration is always quadratic (no linear fallback).
# Each analyte uses a single quantification method (PA or ratio):
#   PETN: ratio (if use_15nrdx_for_petn = TRUE) or PA (if FALSE)
#   RDX:  ratio (if use_is_for_rdx = TRUE) or PA (if FALSE)
analytes <- list(
  PETN = list(
    mz = 46,
    rt = 321,
    noise_position = "before"  # use pre-peak noise window
  ),
  RDX = list(
    mz = 120,
    rt = 347,
    noise_position = "after",   # use post-peak noise window
    noise_after_offset = 10,    # seconds after RT where noise window starts
    noise_after_width = 10      # duration of noise window
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

# QC concentration level (ng) to use for drift correction.
# Options:
#   NA -- Auto-select: use the highest QC level available (default)
#   numeric -- Use specified QC level (e.g., 6 for 6ng QC)
#              Falls back to highest available if specified level not found
# Warning: The 0.2ng QC is typically unreliable for drift correction
# due to non-proportional losses at trace levels.
drift_correction_qc_level <- 6

# Path to the long-term QC monitoring master file
# QC results are appended here after each run for instrument tracking
qc_monitoring_file <- file.path(
  "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/System Monitoring",
  "SystemMonitoring.csv"
)

#############################################################
#####              Input Validation                      #####
#############################################################

validate_setup <- function() {
  # --- DataFolder must exist ---
  if (!dir.exists(DataFolder)) {
    stop("DataFolder does not exist: ", DataFolder)
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
  if (!is.numeric(SignalMaxThresholdSIM) || SignalMaxThresholdSIM <= 0) {
    stop("SignalMaxThresholdSIM must be a positive number")
  }
  
  # --- Drift correction QC level ---
  if (!is.na(drift_correction_qc_level)) {
    if (!is.numeric(drift_correction_qc_level) || drift_correction_qc_level <= 0) {
      stop("drift_correction_qc_level must be a positive number or NA, got: ", 
           drift_correction_qc_level)
    }
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
    use_15nrdx_for_petn = use_15nrdx_for_petn,
    use_is_for_rdx = use_is_for_rdx,
    apply_petn_drift_correction = apply_petn_drift_correction,
    apply_rdx_drift_correction = apply_rdx_drift_correction,
    drift_correction_qc_level = drift_correction_qc_level,
    snr_detect = snr_detect,
    snr_quant = snr_quant,
    qc_bias_limit = qc_bias_limit,
    noise_offset = noise_offset,
    noise_width = noise_width,
    SignalMaxThresholdTIC = SignalMaxThresholdTIC,
    SignalMaxThresholdSIM = SignalMaxThresholdSIM,
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

#source("Code/01_MsFilesReorganiser.R")
source("Code/02_PeakDetection.R")
source("Code/03_Quantification.R")

# Write reproducibility log after successful pipeline completion
log_reproducibility()
