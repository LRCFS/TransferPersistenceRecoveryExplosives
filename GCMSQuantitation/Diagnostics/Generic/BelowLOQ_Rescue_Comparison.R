#############################################################
# BelowLOQ_Rescue_Comparison.R
#############################################################
# Standalone, READ-ONLY diagnostic. Depends on the already-saved
# outputs of RDX_PETN_BelowLOQ_BySurface.R and
# ReplicateQC_LowLevel_LODLOQ.R (run those first).
#
# Re-evaluates the same real-sample rows previously flagged as
# "below the full-range quadratic-curve ICH LOQ" against the
# tighter, QC-replicate-based LOQ (SD of the already-acquired
# 0.2ng QC replicates' own back-calculated concentrations --
# ICH Q2(R2)'s first LOD/LOQ approach, no new data/reanalysis
# needed) to quantify how many rows are "rescued" -- i.e. judged
# reliably quantified under the better-justified method with zero
# new lab work.
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({ library(dplyr) })

out_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output"

below_detail <- read.csv(file.path(out_dir, "BelowLOQ_BySurface_Detail.csv"), stringsAsFactors = FALSE)
qc_replicate <- read.csv(file.path(out_dir, "ReplicateQC_vs_QuadCurve_LODLOQ.csv"), stringsAsFactors = FALSE) %>%
  dplyr::select(Dataset, Analyte, QCReplicate_LOQ_ng)

merged <- below_detail %>%
  dplyr::left_join(qc_replicate, by = c("Dataset", "Analyte")) %>%
  dplyr::filter(!is.na(QCReplicate_LOQ_ng))

merged$BelowQCReplicateLoq <- merged$Conc < merged$QCReplicate_LOQ_ng

cat("=== Re-evaluating the same 'currently Quantifiable' rows against the NEW QC-replicate-based LOQ ===\n\n")
for (an in c("PETN", "RDX")) {
  sub <- merged %>% dplyr::filter(Analyte == an)
  n_quad_below <- sum(sub$BelowIchLoq)
  n_qcrep_below <- sum(sub$BelowQCReplicateLoq)
  cat(sprintf("%s (n=%d rows with both methods available):\n", an, nrow(sub)))
  cat(sprintf("  Below quadratic-curve ICH LOQ:   %d (%.1f%%)\n", n_quad_below, 100*n_quad_below/nrow(sub)))
  cat(sprintf("  Below QC-replicate-based LOQ:    %d (%.1f%%)\n", n_qcrep_below, 100*n_qcrep_below/nrow(sub)))
  n_rescued <- sum(sub$BelowIchLoq & !sub$BelowQCReplicateLoq)
  cat(sprintf("  Rows RESCUED (were below quad-curve LOQ, now above QC-replicate LOQ): %d (%.1f%% of those previously below)\n\n",
              n_rescued, 100*n_rescued/n_quad_below))
}

write.csv(merged, file.path(out_dir, "BelowLOQ_Rescue_Comparison.csv"), row.names = FALSE)
cat("Saved: BelowLOQ_Rescue_Comparison.csv\n")
