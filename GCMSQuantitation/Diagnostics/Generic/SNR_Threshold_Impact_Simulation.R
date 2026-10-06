#############################################################
# SNR_Threshold_Impact_Simulation.R
#############################################################
# Standalone, READ-ONLY diagnostic. Does not modify any live
# pipeline file, results CSV, or acceptance logic.
#
# Purpose
# -------
# Quantifies the REAL-DATA impact of tightening the live SD-based
# SNR thresholds (snr_detect=3, snr_quant=10) to their measured
# peak-to-peak-EQUIVALENT values (~3.8x stricter, per the empirical
# noise investigation: 176 real noise measurements across 2 FINEX
# datasets gave peak-to-peak/SD ~3.73-3.90x for PETN/RDX) --
# i.e. snr_detect_strict ~ 11.4, snr_quant_strict ~ 38 -- WITHOUT
# actually changing any live threshold or reprocessing anything.
#
# Reuses already-exported per-row SNR/concentration columns (no
# raw-signal reprocessing) and the already-computed
# ICH_LODLOQ_PerCalibrationSet.csv (from
# ICH_CalibrationCurve_LODLOQ.R, run earlier) for the independent
# calibration-curve-based LOQ comparison.
#
# Explicit scope limitation (stated up front, not hidden):
# this script counts ROWS that WOULD RECLASSIFY tier under a
# stricter threshold, and separately flags 6ng/0.2ng QC rows whose
# OWN SNR would drop below the stricter bar. It does NOT re-run the
# full assign_qc_brackets()/compute_injection_acceptance() bracket-
# gating cascade (nearest-QC-bracket lookup, NC detectability-vs-
# quantification split, the "sample's own tier overrides a failed
# 0.2ng QC" rule, etc.) -- that would require a genuine reprocessing
# run to get an exact "N additional samples flip to Reanalyse"
# figure. This script's numbers are a directionally reliable,
# conservative LOWER BOUND on the real impact, not the final word.
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({ library(dplyr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

ich_lodloq_file <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output/ICH_LODLOQ_PerCalibrationSet.csv"
if (!file.exists(ich_lodloq_file)) stop("Run ICH_CalibrationCurve_LODLOQ.R first -- ICH_LODLOQ_PerCalibrationSet.csv not found.")
ich_lodloq <- read.csv(ich_lodloq_file, stringsAsFactors = FALSE)

study_roots <- list(
  FINEX       = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis",
  ASTRA_Main  = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study/GC Data",
  ASTRA_Pilot = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data"
)
study_tag <- c(FINEX = "FINEX", ASTRA_Main = "ASTRA", ASTRA_Pilot = "ASTRA")

output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output"

# Measured conversion factor (peak-to-peak / SD), from the real
# noise investigation (Lab9 + Lab29, 44 blanks each, both analytes).
# Using the more conservative (smaller) of the two measured ratios
# per analyte so this is a LOWER bound on the strictness increase.
p2p_factor <- c(PETN = 3.73, RDX = 3.90)

snr_detect_strict <- 3 * p2p_factor
snr_quant_strict  <- 10 * p2p_factor

cat("Strict (peak-to-peak-equivalent) thresholds used for this simulation:\n")
cat(sprintf("  PETN: snr_detect %.1f -> %.1f | snr_quant %.1f -> %.1f\n", 3, snr_detect_strict["PETN"], 10, snr_quant_strict["PETN"]))
cat(sprintf("  RDX:  snr_detect %.1f -> %.1f | snr_quant %.1f -> %.1f\n\n", 3, snr_detect_strict["RDX"], 10, snr_quant_strict["RDX"]))

discover_datasets <- function(root, tag) {
  if (!dir.exists(root)) return(data.frame())
  files <- list.files(root, pattern = "_GCMSResults\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("[Bb]ackup|[Aa]rchive|[Pp]lots", files)]
  files <- files[!grepl("^_GCMSResults\\.csv$", basename(files))]
  if (length(files) == 0) return(data.frame())
  data.frame(ResultsFile = files, Dataset = basename(dirname(dirname(files))), Study = tag, stringsAsFactors = FALSE)
}
datasets <- bind_rows(lapply(names(study_roots), function(nm) discover_datasets(study_roots[[nm]], study_tag[[nm]])))
cat(sprintf("Discovered %d dataset results file(s).\n\n", nrow(datasets)))

row_results <- list()
qc_results  <- list()

for (ds_idx in seq_len(nrow(datasets))) {
  ds <- datasets[ds_idx, ]
  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df)) next

  for (analyte in c("PETN", "RDX")) {
    prefix <- tolower(analyte)
    snr_col      <- paste0(prefix, "_snr")
    flag_col     <- paste0(prefix, "_snr_flag")
    conc_col     <- if (analyte == "PETN" && "petn_concentration_dc" %in% names(df)) "petn_concentration_dc" else paste0(prefix, "_concentration")
    if (!snr_col %in% names(df) || !flag_col %in% names(df)) next

    # --- Real Sample rows (SampleType != NC where available) ---
    sample_rows <- df %>%
      dplyr::filter(Type == "Sample") %>%
      dplyr::mutate(IsNC = if ("SampleType" %in% names(df)) SampleType == "NC" else grepl("_NC\\s*$", trimws(SampleName %||% "")))

    strict_tier <- function(snr) {
      dplyr::case_when(
        is.na(snr) ~ NA_character_,
        snr < snr_detect_strict[analyte] ~ "Below_LOD",
        snr < snr_quant_strict[analyte]  ~ "Below_LOQ",
        TRUE ~ "Quantifiable"
      )
    }

    real_samples <- sample_rows %>% dplyr::filter(!IsNC)
    nc_samples   <- sample_rows %>% dplyr::filter(IsNC)

    if (nrow(real_samples) > 0) {
      real_samples$CurrentTier <- real_samples[[flag_col]]
      real_samples$StrictTier  <- strict_tier(real_samples[[snr_col]])
      real_samples$Downgraded  <- !is.na(real_samples$CurrentTier) & !is.na(real_samples$StrictTier) &
        real_samples$CurrentTier == "Quantifiable" & real_samples$StrictTier != "Quantifiable"

      # ICH-calibration LOQ comparison for currently-Quantifiable rows
      ich_match <- ich_lodloq %>%
        dplyr::filter(Dataset == ds$Dataset, Analyte == analyte) %>%
        dplyr::select(CalibrationSet, ICH_LOQ_ng)
      if (nrow(ich_match) > 0 && "CalibrationSet" %in% names(real_samples)) {
        real_samples <- real_samples %>% dplyr::left_join(ich_match, by = "CalibrationSet")
      } else {
        real_samples$ICH_LOQ_ng <- NA_real_
      }
      real_samples$BelowIchLoq <- !is.na(real_samples$CurrentTier) & real_samples$CurrentTier == "Quantifiable" &
        !is.na(real_samples[[conc_col]]) & !is.na(real_samples$ICH_LOQ_ng) &
        real_samples[[conc_col]] < real_samples$ICH_LOQ_ng

      row_results[[length(row_results) + 1]] <- data.frame(
        Study = ds$Study, Dataset = ds$Dataset, Analyte = analyte,
        N_real_samples = nrow(real_samples),
        N_currently_Quantifiable = sum(real_samples$CurrentTier == "Quantifiable", na.rm = TRUE),
        N_downgraded_under_strict_SNR = sum(real_samples$Downgraded, na.rm = TRUE),
        N_below_ICH_LOQ = sum(real_samples$BelowIchLoq, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }

    if (nrow(nc_samples) > 0) {
      nc_samples$CurrentTier <- nc_samples[[flag_col]]
      nc_samples$StrictTier  <- strict_tier(nc_samples[[snr_col]])
      nc_samples$Upgraded_to_clean <- !is.na(nc_samples$CurrentTier) & !is.na(nc_samples$StrictTier) &
        nc_samples$CurrentTier %in% c("Below_LOQ", "Quantifiable") & nc_samples$StrictTier == "Below_LOD"

      row_results[[length(row_results) + 1]] <- data.frame(
        Study = ds$Study, Dataset = ds$Dataset, Analyte = paste0(analyte, "_NC"),
        N_real_samples = nrow(nc_samples),
        N_currently_Quantifiable = sum(nc_samples$CurrentTier %in% c("Below_LOQ", "Quantifiable"), na.rm = TRUE),
        N_downgraded_under_strict_SNR = sum(nc_samples$Upgraded_to_clean, na.rm = TRUE),
        N_below_ICH_LOQ = NA_integer_,
        stringsAsFactors = FALSE
      )
    }

    # --- QC rows: does the SNR-component of 6ng/0.2ng QC flags change? ---
    qc_rows <- df %>% dplyr::filter(Type == "QC", !is.na(CalLevel))
    if (nrow(qc_rows) > 0) {
      for (lvl in c(6, 0.2)) {
        lvl_rows <- qc_rows %>% dplyr::filter(CalLevel == lvl)
        if (nrow(lvl_rows) == 0) next
        currently_pass_snr <- sum(lvl_rows[[snr_col]] >= 10, na.rm = TRUE)
        strict_pass_snr    <- sum(lvl_rows[[snr_col]] >= snr_quant_strict[analyte], na.rm = TRUE)
        qc_results[[length(qc_results) + 1]] <- data.frame(
          Study = ds$Study, Dataset = ds$Dataset, Analyte = analyte, CalLevel = lvl,
          N_QC = nrow(lvl_rows),
          N_currently_pass_SNR10 = currently_pass_snr,
          N_pass_under_strict_SNR = strict_pass_snr,
          N_newly_failing_SNR_component = currently_pass_snr - strict_pass_snr,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

row_tbl <- bind_rows(row_results)
qc_tbl  <- bind_rows(qc_results)

write.csv(row_tbl, file.path(output_dir, "SNRThreshold_Impact_PerDataset.csv"), row.names = FALSE)
write.csv(qc_tbl, file.path(output_dir, "SNRThreshold_Impact_QC_PerDataset.csv"), row.names = FALSE)

cat("=== REAL SAMPLES (excl. NC): tier downgrade + below-ICH-LOQ impact, per analyte ===\n")
real_summary <- row_tbl %>%
  dplyr::filter(!grepl("_NC$", Analyte)) %>%
  dplyr::group_by(Analyte) %>%
  dplyr::summarise(
    Total_real_samples = sum(N_real_samples),
    Currently_Quantifiable = sum(N_currently_Quantifiable),
    Downgraded_under_strict_SNR = sum(N_downgraded_under_strict_SNR),
    Pct_downgraded_of_Quantifiable = round(100 * sum(N_downgraded_under_strict_SNR) / sum(N_currently_Quantifiable), 1),
    Below_ICH_LOQ = sum(N_below_ICH_LOQ),
    Pct_below_ICH_LOQ_of_Quantifiable = round(100 * sum(N_below_ICH_LOQ) / sum(N_currently_Quantifiable), 1),
    .groups = "drop"
  )
print(as.data.frame(real_summary), row.names = FALSE)

cat("\n=== NEGATIVE CONTROLS: how many currently-detected (Below_LOQ/Quantifiable) NC rows\n")
cat("    would flip to 'Negative/clean' (Below_LOD) under the stricter threshold ===\n")
nc_summary <- row_tbl %>%
  dplyr::filter(grepl("_NC$", Analyte)) %>%
  dplyr::group_by(Analyte) %>%
  dplyr::summarise(
    Total_NC_rows = sum(N_real_samples),
    Currently_detected = sum(N_currently_Quantifiable),
    Would_flip_to_clean = sum(N_downgraded_under_strict_SNR),
    .groups = "drop"
  )
print(as.data.frame(nc_summary), row.names = FALSE)

cat("\n=== QC INJECTIONS: SNR-component of the live PASS/FAIL flag, by level ===\n")
cat("    (6ng QC also requires |bias|<=20% -- a QC counted here as 'newly failing SNR'\n")
cat("     will only actually flip from PASS to FAIL if its bias was already within limits;\n")
cat("     if its bias already failed, this changes nothing for that QC)\n")
qc_summary <- qc_tbl %>%
  dplyr::group_by(Analyte, CalLevel) %>%
  dplyr::summarise(
    Total_QC = sum(N_QC),
    Currently_pass_SNR = sum(N_currently_pass_SNR10),
    Pass_under_strict_SNR = sum(N_pass_under_strict_SNR),
    Newly_failing_SNR_component = sum(N_newly_failing_SNR_component),
    Pct_of_currently_passing_at_risk = round(100 * sum(N_newly_failing_SNR_component) / sum(N_currently_pass_SNR10), 1),
    .groups = "drop"
  )
print(as.data.frame(qc_summary), row.names = FALSE)

cat("\nFull per-dataset detail saved to SNRThreshold_Impact_PerDataset.csv / _QC_PerDataset.csv\n")
cat("\nREMINDER: these are row-level reclassification counts, not a full bracket-gating\n")
cat("re-simulation -- see this script's own header for exactly what is and isn't captured.\n")
