#############################################################
# RDX_AugmentedCalibration_Test.R
#############################################################
# Standalone, READ-ONLY diagnostic. Follow-up to
# CalibrationModel_Comparison.R -- for RDX specifically (PETN's
# model is confirmed staying quadratic, out of scope here).
#
# Tests ONE further option not yet checked: does actually
# INCLUDING the already-acquired 0.2ng QC replicates as
# additional data points in the quadratic fit itself (not just
# using them as held-out validation, as CalibrationModel_Comparison.R
# did) meaningfully tighten the low-end local slope / reduce the
# resulting LOD/LOQ, versus the current production fit using only
# the 6 Cal standards?
#
# This is scientifically legitimate: the 0.2ng QC replicates are
# physically the exact same thing as the 0.2ng Cal standard (same
# nominal concentration, same analyte, same instrument conditions
# within that sequence) -- "QC" vs "Cal" is a sequence-log ROLE
# label, not a different scientific category. Augmenting the fit
# with them simply uses the real replicate structure the sequence
# design already provides at the one level that currently has
# zero within-level replication in the Cal block itself.
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({ library(dplyr); library(yaml) })

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
  data.frame(ResultsFile = files, DatasetDir = dirname(dirname(files)),
             Dataset = basename(dirname(dirname(files))), Study = tag, stringsAsFactors = FALSE)
}
datasets <- bind_rows(lapply(names(study_roots), function(nm) discover_datasets(study_roots[[nm]], study_tag[[nm]])))

read_dataset_config <- function(dataset_dir) {
  meta_file <- file.path(dataset_dir, "run_metadata.yaml")
  defaults <- list(Dilution = 1, use_is_for_rdx = TRUE, cal_exclude_lines = integer(0))
  if (!file.exists(meta_file)) return(defaults)
  meta <- tryCatch(yaml::read_yaml(meta_file), error = function(e) NULL)
  cfg <- meta$configuration
  if (is.null(cfg)) return(defaults)
  list(
    Dilution = if (!is.null(cfg$Dilution)) as.numeric(cfg$Dilution) else defaults$Dilution,
    use_is_for_rdx = isTRUE(cfg$use_is_for_rdx) || identical(cfg$use_is_for_rdx, "yes"),
    cal_exclude_lines = if (!is.null(cfg$cal_exclude_lines) && length(cfg$cal_exclude_lines) > 0) as.numeric(unlist(cfg$cal_exclude_lines)) else integer(0)
  )
}

results <- list()

for (ds_idx in seq_len(nrow(datasets))) {
  ds <- datasets[ds_idx, ]
  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df) || !"CalLevel" %in% names(df)) next
  cfg <- read_dataset_config(ds$DatasetDir)

  resp_col <- if (cfg$use_is_for_rdx) "rdx_ratio" else "rdx_pa"
  flag_col <- "rdx_snr_flag"
  if (!resp_col %in% names(df)) next

  cal <- df %>% dplyr::filter(Type == "Cal", !is.na(CalLevel)) %>%
    dplyr::mutate(CalLevelAdj = CalLevel * cfg$Dilution, Source = "Cal")
  if (nrow(cal) == 0 || !"CalibrationSet" %in% names(cal)) next

  for (cs in sort(unique(cal$CalibrationSet))) {
    cal_fit <- cal %>% dplyr::filter(CalibrationSet == cs)
    if (flag_col %in% names(cal_fit)) {
      cal_fit <- cal_fit %>% dplyr::filter(is.na(.data[[flag_col]]) | .data[[flag_col]] != "Below_LOD")
    }
    if (length(cfg$cal_exclude_lines) > 0 && "Line" %in% names(cal_fit)) {
      cal_fit <- cal_fit %>% dplyr::filter(!(Line %in% cfg$cal_exclude_lines))
    }
    if (nrow(cal_fit) < 5 || all(is.na(cal_fit[[resp_col]]))) next

    qc_02 <- df %>% dplyr::filter(Type == "QC", CalLevel == 0.2, CalibrationSet == cs) %>%
      dplyr::mutate(CalLevelAdj = 0.2 * cfg$Dilution, Source = "QC")
    if (nrow(qc_02) < 3 || !resp_col %in% names(qc_02)) next
    qc_02 <- qc_02 %>% dplyr::filter(!is.na(.data[[resp_col]]))
    if (nrow(qc_02) < 3) next

    # --- ORIGINAL fit: Cal standards only (current production) ---
    fit_orig <- lm(reformulate(c("CalLevelAdj", "I(CalLevelAdj^2)"), response = resp_col),
                   data = cal_fit, weights = 1 / cal_fit$CalLevelAdj)
    co <- coef(fit_orig)
    x_lowest_orig <- min(cal_fit$CalLevelAdj)
    slope_orig <- 2 * co["I(CalLevelAdj^2)"] * x_lowest_orig + co["CalLevelAdj"]
    sigma_orig <- summary(fit_orig)$sigma
    LOQ_orig <- 10 * sigma_orig / abs(slope_orig)

    # --- AUGMENTED fit: Cal standards + 0.2ng QC replicates pooled in ---
    aug_data <- bind_rows(
      cal_fit %>% dplyr::select(CalLevelAdj, all_of(resp_col)),
      qc_02 %>% dplyr::select(CalLevelAdj, all_of(resp_col))
    )
    fit_aug <- tryCatch(
      lm(reformulate(c("CalLevelAdj", "I(CalLevelAdj^2)"), response = resp_col),
         data = aug_data, weights = 1 / aug_data$CalLevelAdj),
      error = function(e) NULL
    )
    if (is.null(fit_aug)) next
    ca <- coef(fit_aug)
    x_lowest_aug <- min(aug_data$CalLevelAdj)
    slope_aug <- 2 * ca["I(CalLevelAdj^2)"] * x_lowest_aug + ca["CalLevelAdj"]
    sigma_aug <- summary(fit_aug)$sigma
    LOQ_aug <- 10 * sigma_aug / abs(slope_aug)

    results[[length(results) + 1]] <- data.frame(
      Study = ds$Study, Dataset = ds$Dataset, CalibrationSet = cs,
      N_cal_only = nrow(cal_fit), N_augmented = nrow(aug_data),
      Sigma_orig = sigma_orig, Sigma_aug = sigma_aug,
      LOQ_orig_ng = LOQ_orig, LOQ_aug_ng = LOQ_aug,
      Improvement_factor = LOQ_orig / LOQ_aug,
      stringsAsFactors = FALSE
    )
  }
}

tbl <- bind_rows(results)
write.csv(tbl, file.path(output_dir, "RDX_AugmentedCalibration_Detail.csv"), row.names = FALSE)

cat(sprintf("=== RDX: augmenting the quadratic fit with 0.2ng QC replicates (n=%d calibration sets) ===\n\n", nrow(tbl)))
cat(sprintf("Median LOQ -- Cal-only (current production):      %.3f ng\n", median(tbl$LOQ_orig_ng, na.rm = TRUE)))
cat(sprintf("Median LOQ -- Cal + QC-replicates augmented fit:   %.3f ng\n", median(tbl$LOQ_aug_ng, na.rm = TRUE)))
cat(sprintf("Median improvement factor: %.2fx\n", median(tbl$Improvement_factor, na.rm = TRUE)))
cat(sprintf("Datasets where augmented fit is actually BETTER (factor > 1): %d of %d\n",
            sum(tbl$Improvement_factor > 1, na.rm = TRUE), nrow(tbl)))
cat(sprintf("Datasets where augmented fit is WORSE (factor < 1): %d of %d\n",
            sum(tbl$Improvement_factor < 1, na.rm = TRUE), nrow(tbl)))

cat("\nFull detail saved to RDX_AugmentedCalibration_Detail.csv\n")
