#############################################################
# RDX_PETN_BelowLOQ_BySurface.R
#############################################################
# Standalone, READ-ONLY diagnostic. Follow-up to
# ICH_CalibrationCurve_LODLOQ.R / SNR_Threshold_Impact_Simulation.R.
#
# Question: the earlier simulation found 50.5% of RDX
# "Quantifiable" real-sample results sit below the independent
# ICH calibration-curve LOQ (vs 24.6% for PETN). This script
# breaks that down by Surface, to test the hypothesis that this
# is substantially explained by the already-documented RDX/ABS
# acetonitrile-solvent compatibility issue (RedTeam_Findings.md
# item #17 -- RDX standards spiked in acetonitrile damage/interact
# with ABS, artifactually suppressing RDX recovery on ABS-Smooth/
# ABS-Textured specifically; already excluded from FINEX's own
# statistical interpretation via `rdx_surface_exclude`).
#
# Surface is attached per real-sample row by:
#   - FINEX: parsing SampleName with the IDENTICAL regex/map
#     Code/FINEX/04_CollateStudyResults.R uses
#     (sample_pattern / surface_map), not a re-derived
#     approximation.
#   - ASTRA: joining on RunID against the already-collated
#     main_study_data_nested.csv / pilot_data_nested.csv
#     (Surface_type column).
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

ich_lodloq_file <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output/ICH_LODLOQ_PerCalibrationSet.csv"
if (!file.exists(ich_lodloq_file)) stop("Run ICH_CalibrationCurve_LODLOQ.R first.")
ich_lodloq <- read.csv(ich_lodloq_file, stringsAsFactors = FALSE)

output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output"

# ===============================================================================
# FINEX surface parsing -- identical to Code/FINEX/04_CollateStudyResults.R
# ===============================================================================
sample_pattern <- "^Lab(\\d+)\\s*P(\\d+)\\s*(ABS[- ]S|ABS[- ]T|S|G)\\s*(NC|\\d+)$"
surface_map <- c("S" = "Steel", "G" = "Glass", "ABS-S" = "ABS-Smooth", "ABS-T" = "ABS-Textured")

parse_finex_surface <- function(sample_name) {
  nm <- trimws(sample_name)
  parsed <- str_match(nm, sample_pattern)
  surf <- parsed[, 4]
  surf <- gsub("ABS T", "ABS-T", surf, fixed = TRUE)
  surf <- gsub("ABS S", "ABS-S", surf, fixed = TRUE)
  unname(surface_map[surf])
}

# ===============================================================================
# ASTRA surface lookup -- join on RunID against the already-collated nested CSVs
# ===============================================================================
astra_main_file  <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study/main_study_data_nested.csv"
astra_pilot_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/pilot_data_nested.csv"

astra_surface_lookup <- bind_rows(
  if (file.exists(astra_main_file)) read.csv(astra_main_file, stringsAsFactors = FALSE) %>%
    dplyr::transmute(RunID = trimws(RunID), Surface_type) else data.frame(),
  if (file.exists(astra_pilot_file)) read.csv(astra_pilot_file, stringsAsFactors = FALSE) %>%
    dplyr::transmute(RunID = trimws(RunID), Surface_type) else data.frame()
) %>% distinct(RunID, .keep_all = TRUE)

astra_surface_map <- c(steel = "Steel", abs = "ABS")

# ===============================================================================
# DISCOVER DATASETS (same convention as the prior two scripts)
# ===============================================================================
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
cat(sprintf("Discovered %d dataset results file(s).\n\n", nrow(datasets)))

# ===============================================================================
# MAIN LOOP
# ===============================================================================
row_results <- list()
unmatched_surface <- 0

for (ds_idx in seq_len(nrow(datasets))) {
  ds <- datasets[ds_idx, ]
  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df)) next

  sample_rows <- df %>% dplyr::filter(Type == "Sample")
  if (nrow(sample_rows) == 0) next

  is_nc <- if ("SampleType" %in% names(sample_rows)) sample_rows$SampleType == "NC" else grepl("_NC\\s*$", trimws(sample_rows$SampleName))
  sample_rows <- sample_rows[!is_nc, , drop = FALSE]
  if (nrow(sample_rows) == 0) next

  if (ds$Study == "FINEX") {
    sample_rows$Surface <- parse_finex_surface(sample_rows$SampleName)
  } else {
    matched <- astra_surface_lookup$Surface_type[match(trimws(sample_rows$SampleName), astra_surface_lookup$RunID)]
    sample_rows$Surface <- unname(astra_surface_map[matched])
  }
  n_unmatched <- sum(is.na(sample_rows$Surface))
  unmatched_surface <- unmatched_surface + n_unmatched

  for (analyte in c("PETN", "RDX")) {
    prefix <- tolower(analyte)
    flag_col <- paste0(prefix, "_snr_flag")
    conc_col <- if (analyte == "PETN" && "petn_concentration_dc" %in% names(sample_rows)) "petn_concentration_dc" else paste0(prefix, "_concentration")
    if (!flag_col %in% names(sample_rows) || !conc_col %in% names(sample_rows)) next
    if (!"CalibrationSet" %in% names(sample_rows)) next

    ich_match <- ich_lodloq %>%
      dplyr::filter(Dataset == ds$Dataset, Analyte == analyte) %>%
      dplyr::select(CalibrationSet, ICH_LOQ_ng)
    if (nrow(ich_match) == 0) next

    sub <- sample_rows %>%
      dplyr::select(Surface, CalibrationSet, CurrentTier = all_of(flag_col), Conc = all_of(conc_col)) %>%
      dplyr::left_join(ich_match, by = "CalibrationSet") %>%
      dplyr::filter(!is.na(CurrentTier), CurrentTier == "Quantifiable", !is.na(Conc), !is.na(ICH_LOQ_ng))

    if (nrow(sub) == 0) next

    sub$BelowIchLoq <- sub$Conc < sub$ICH_LOQ_ng

    row_results[[length(row_results) + 1]] <- sub %>%
      dplyr::mutate(Study = ds$Study, Dataset = ds$Dataset, Analyte = analyte) %>%
      dplyr::select(Study, Dataset, Analyte, Surface, Conc, ICH_LOQ_ng, BelowIchLoq)
  }
}

all_rows <- bind_rows(row_results)
cat(sprintf("Total 'Quantifiable' real-sample rows evaluated: %d (%d with unmatched/NA Surface, excluded below)\n\n",
            nrow(all_rows), sum(is.na(all_rows$Surface))))

all_rows <- all_rows %>% dplyr::filter(!is.na(Surface))

write.csv(all_rows, file.path(output_dir, "BelowLOQ_BySurface_Detail.csv"), row.names = FALSE)

cat("=== Below-ICH-LOQ breakdown by Surface, per analyte ===\n\n")
for (an in c("PETN", "RDX")) {
  sub <- all_rows %>% dplyr::filter(Analyte == an)
  cat(sprintf("--- %s (n=%d Quantifiable rows with a matched Surface) ---\n", an, nrow(sub)))
  tbl <- sub %>%
    dplyr::group_by(Surface) %>%
    dplyr::summarise(
      N_Quantifiable = dplyr::n(),
      N_Below_ICH_LOQ = sum(BelowIchLoq),
      Pct_Below_ICH_LOQ = round(100 * sum(BelowIchLoq) / dplyr::n(), 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(N_Below_ICH_LOQ))
  print(as.data.frame(tbl), row.names = FALSE)

  n_below_total <- sum(sub$BelowIchLoq)
  n_below_abs   <- sum(sub$BelowIchLoq & grepl("ABS", sub$Surface))
  cat(sprintf("\n  Of %d total below-ICH-LOQ %s rows: %d (%.1f%%) are ABS surfaces (ABS-Smooth/ABS-Textured/ABS)\n\n",
              n_below_total, an, n_below_abs, 100 * n_below_abs / n_below_total))
}

cat("Full per-row detail saved to BelowLOQ_BySurface_Detail.csv\n")
