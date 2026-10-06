#############################################################
# ReplicateQC_LowLevel_LODLOQ.R
#############################################################
# Standalone, READ-ONLY diagnostic. Tests whether using the
# ALREADY-ACQUIRED 0.2ng QC replicate injections (5-6 per
# dataset, run throughout each sequence for routine QC
# purposes -- NOT new data, NOT new reanalysis) to directly
# characterise low-level repeatability gives a tighter, more
# defensible LOD/LOQ than extrapolating from the full-range
# (0.2-10ng) weighted quadratic calibration curve's own
# residual SD + local slope (the method used in
# ICH_CalibrationCurve_LODLOQ.R).
#
# Method (ICH Q2(R2)'s own first approach, "based on standard
# deviation of the response" -- here taken directly in
# concentration units from repeated low-level standard
# injections, a completely standard practice: inject n>=5
# replicates of a known low-concentration standard, compute
# SD of the resulting back-calculated concentrations, then
#   LOD = 3.3 x SD        LOQ = 10 x SD
# No slope term needed at all -- sidesteps the whole
# quadratic-local-slope question entirely, since the QC is
# already at a known fixed concentration.
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({ library(dplyr) })

ich_lodloq_file <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output/ICH_LODLOQ_PerCalibrationSet.csv"
ich_lodloq <- read.csv(ich_lodloq_file, stringsAsFactors = FALSE)

output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output"

study_roots <- list(
  FINEX       = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis",
  ASTRA_Main  = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study/GC Data",
  ASTRA_Pilot = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data"
)
study_tag <- c(FINEX = "FINEX", ASTRA_Main = "ASTRA", ASTRA_Pilot = "ASTRA")

discover_datasets <- function(root, tag) {
  if (!dir.exists(root)) return(data.frame())
  files <- list.files(root, pattern = "_GCMSResults\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("[Bb]ackup|[Aa]rchive|[Pp]lots", files)]
  files <- files[!grepl("^_GCMSResults\\.csv$", basename(files))]
  if (length(files) == 0) return(data.frame())
  data.frame(ResultsFile = files, Dataset = basename(dirname(dirname(files))), Study = tag, stringsAsFactors = FALSE)
}
datasets <- bind_rows(lapply(names(study_roots), function(nm) discover_datasets(study_roots[[nm]], study_tag[[nm]])))

results <- list()
min_replicates <- 4  # need a minimally reliable SD estimate

for (ds_idx in seq_len(nrow(datasets))) {
  ds <- datasets[ds_idx, ]
  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df) || !"CalLevel" %in% names(df)) next

  for (analyte in c("PETN", "RDX")) {
    conc_col <- if (analyte == "PETN" && "petn_concentration_dc" %in% names(df)) "petn_concentration_dc" else paste0(tolower(analyte), "_concentration")
    if (!conc_col %in% names(df)) next

    qc_02 <- df %>% dplyr::filter(Type == "QC", CalLevel == 0.2)
    n_total <- nrow(qc_02)
    conc_vals <- qc_02[[conc_col]]
    n_usable <- sum(!is.na(conc_vals))
    n_na     <- n_total - n_usable

    old_lodloq <- ich_lodloq %>% dplyr::filter(Dataset == ds$Dataset, Analyte == analyte)
    old_lod <- if (nrow(old_lodloq) > 0) mean(old_lodloq$ICH_LOD_ng) else NA_real_
    old_loq <- if (nrow(old_lodloq) > 0) mean(old_lodloq$ICH_LOQ_ng) else NA_real_

    if (n_usable >= min_replicates) {
      cv <- conc_vals[!is.na(conc_vals)]
      new_sd  <- sd(cv)
      new_lod <- 3.3 * new_sd
      new_loq <- 10 * new_sd
    } else {
      new_sd <- new_lod <- new_loq <- NA_real_
    }

    results[[length(results) + 1]] <- data.frame(
      Study = ds$Study, Dataset = ds$Dataset, Analyte = analyte,
      N_QC_02ng_total = n_total, N_QC_02ng_usable = n_usable, N_QC_02ng_NA = n_na,
      QC_replicate_SD_ng = new_sd,
      QuadCurve_LOD_ng = old_lod, QuadCurve_LOQ_ng = old_loq,
      QCReplicate_LOD_ng = new_lod, QCReplicate_LOQ_ng = new_loq,
      Improvement_factor_LOQ = old_loq / new_loq,
      stringsAsFactors = FALSE
    )
  }
}

tbl <- bind_rows(results)
write.csv(tbl, file.path(output_dir, "ReplicateQC_vs_QuadCurve_LODLOQ.csv"), row.names = FALSE)

cat("=== Replicate 0.2ng-QC-based LOD/LOQ vs full-range quadratic-curve-based LOD/LOQ ===\n\n")
for (an in c("PETN", "RDX")) {
  sub <- tbl %>% dplyr::filter(Analyte == an)
  cat(sprintf("--- %s (%d datasets) ---\n", an, nrow(sub)))
  cat(sprintf("  Datasets with >=%d usable 0.2ng QC replicates: %d of %d\n",
              min_replicates, sum(sub$N_QC_02ng_usable >= min_replicates), nrow(sub)))
  cat(sprintf("  Mean N usable / N total 0.2ng QC replicates: %.1f / %.1f (NA rate: %.1f%%)\n",
              mean(sub$N_QC_02ng_usable), mean(sub$N_QC_02ng_total),
              100 * sum(sub$N_QC_02ng_NA) / sum(sub$N_QC_02ng_total)))
  valid <- sub %>% dplyr::filter(!is.na(QCReplicate_LOQ_ng), !is.na(QuadCurve_LOQ_ng))
  cat(sprintf("  Median LOQ -- quadratic-curve method: %.3f ng | QC-replicate method: %.3f ng\n",
              median(valid$QuadCurve_LOQ_ng), median(valid$QCReplicate_LOQ_ng)))
  cat(sprintf("  Median improvement factor (quad LOQ / QC-replicate LOQ): %.1fx\n\n",
              median(valid$Improvement_factor_LOQ, na.rm = TRUE)))
}

cat("Full detail saved to ReplicateQC_vs_QuadCurve_LODLOQ.csv\n")
