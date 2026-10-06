#############################################################
# ICH_CalibrationCurve_LODLOQ.R
#############################################################
# Standalone, READ-ONLY diagnostic script. Does not modify any
# live pipeline file, calibration model, or results CSV -- it
# only reads already-exported `*_GCMSResults.csv` files (plus
# each dataset's own `run_metadata.yaml`, logged by GlobalCode.R's
# log_reproducibility()) and re-fits the identical calibration
# model structure already used in production, purely to extract
# the residual SD and local slope needed for an independent LOD/LOQ
# estimate.
#
# Purpose
# -------
# The pipeline's live per-injection QC acceptance uses an SD-based
# SNR convention (snr_detect=3 / snr_quant=10, see GlobalCode.R and
# Code/ModPeaks.R::calculate_snr()) whose relationship to the
# traditional peak-to-peak-based "3:1 / 10:1" chromatography
# convention is itself ambiguous (see CONTEXT.md's SNR-convention
# investigation). This script computes a SECOND, INDEPENDENT
# LOD/LOQ estimate via ICH Q2(R2) Section 3.2.3's third approach
# ("based on the standard deviation of the response and the slope
# of the calibration curve"):
#
#     LOD = 3.3 x sigma / S         LOQ = 10 x sigma / S
#
# This does NOT change snr_detect/snr_quant or any live acceptance
# logic. It produces a separate, defensible, ng-denominated
# validation number to report alongside (or cross-check against)
# the existing SNR-based system.
#
# Methodology decisions (agreed before implementation)
# ------------------------------------------------------
# sigma: residual standard error of the weighted quadratic
#   calibration fit (R's `summary(model)$sigma`) -- Dolan's
#   "standard error of the calibration curve" variant of ICH's
#   formula (Dolan JW, "Chromatographic Measurements, Part 5:
#   Determining LOD and LOQ Based on the Calibration Curve",
#   LCGC/Separation Science, sepscience.com/hplc-solutions-126).
#
# S: because every calibration curve in this pipeline is a
#   WEIGHTED QUADRATIC (not linear -- see Code/03_Quantification.R's
#   `quad_formula <- reformulate(c("CalLevelAdj","I(CalLevelAdj^2)"), ...)`),
#   there is no single constant slope. ICH's formula implicitly
#   assumes linearity. Per the agreed decision, S is evaluated as the
#   LOCAL slope (first derivative, 2*a*x+b) of the fitted quadratic
#   at the LOWEST calibration standard's concentration -- the region
#   where the LOD/LOQ actually lands, matching:
#     (a) Cuadros-Rodriguez L. et al., "An IUPAC-based approach to
#         estimate the detection limit in co-extraction-based optical
#         sensors for anions with sigmoidal response calibration
#         curves", Anal. Bioanal. Chem. 2011, 401(9), 2881-2889
#         (DOI: 10.1007/s00216-011-5366-8) -- adapts the same IUPAC/
#         ICH detection-limit methodology to a different nonlinear
#         (sigmoidal) calibration shape;
#     (b) the delta method / first-order Taylor expansion (JCGM
#         100:2008, GUM) for propagating a response-domain SD into a
#         concentration-domain SD through a nonlinear function --
#         the SAME technique this codebase's own "Item 19" PETN
#         drift-correction uncertainty propagation already uses
#         (`local_slope <- 2 * qa * res$value + qb` in
#         Code/03_Quantification.R's delta-method SE block).
#
# Scope: computed per calibration set (every dataset x every
# CalibrationSet x PETN/RDX), then aggregated per-analyte-per-study
# as the primary citable number, with the full per-dataset
# distribution kept for a robustness/variability check.
#
# Cross-check (lightweight, no raw-signal reprocessing needed):
# for each calibration set, the lowest real Sample/QC concentration
# in that same set that the LIVE pipeline already called
# "Quantifiable" (SNR>=10) is reported alongside the ICH LOQ --
# both numbers are in ng, directly comparable, no noise
# re-measurement required.
#
# Outputs (this script's own folder, ICH_LODLOQ_Output/):
#   - ICH_LODLOQ_PerCalibrationSet.csv  (one row per dataset x
#     CalibrationSet x analyte -- full detail, for the robustness
#     check)
#   - ICH_LODLOQ_Summary.csv            (one row per analyte x
#     study -- the primary citable numbers)
#   - ICH_LODLOQ_Comparison.png         (ICH-derived vs
#     empirically-observed-Quantifiable-boundary, per analyte)
#
# Author: OpenCode
# Date: 2026-10-06
#############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required (used to read each dataset's run_metadata.yaml). Install with: install.packages('yaml')")
}

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) --
# single source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# ===============================================================================
# CONFIGURATION
# ===============================================================================

study_roots <- list(
  FINEX      = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis",
  ASTRA_Main  = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Main Study/GC Data",
  ASTRA_Pilot = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data"
)
# Both ASTRA roots are tagged as the same "ASTRA" study for aggregation
study_tag <- c(FINEX = "FINEX", ASTRA_Main = "ASTRA", ASTRA_Pilot = "ASTRA")

output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/Generic/ICH_LODLOQ_Output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Minimum calibration points required to fit a 3-parameter quadratic
# with at least 1 residual degree of freedom.
min_cal_points <- 4

# ===============================================================================
# STEP 1: DISCOVER DATASETS
# ===============================================================================
# Mirrors the collation scripts' own discovery convention: recursive
# search for "*_GCMSResults.csv", excluding Backup/Archive/Plots
# folders, and excluding any file with an empty dataset-name prefix
# (the Lab29/Lab33 stale-file class of bug documented in CONTEXT.md).

discover_datasets <- function(root, tag, phase_label = NA_character_) {
  if (!dir.exists(root)) {
    message("  [skip] Study root not found: ", root)
    return(data.frame())
  }
  files <- list.files(root, pattern = "_GCMSResults\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("[Bb]ackup|[Aa]rchive|[Pp]lots", files)]
  files <- files[!grepl("^_GCMSResults\\.csv$", basename(files))]  # empty-prefix guard
  if (length(files) == 0) return(data.frame())
  dataset_name <- basename(dirname(dirname(files)))
  # ASTRA's Main Study and Pilot Study folders both reuse "Analysis1"/"Analysis3"/
  # "Analysis4" -- the underlying numbers are correctly distinct (keyed on the
  # full DatasetDir path throughout this script), but the plain folder name
  # alone is ambiguous in any printed table. Disambiguate the DISPLAY label only.
  if (!is.na(phase_label)) dataset_name <- paste0(dataset_name, " (", phase_label, ")")
  data.frame(
    ResultsFile = files,
    DatasetDir  = dirname(dirname(files)),  # .../<Dataset>/Results/<file> -> .../<Dataset>
    Dataset     = dataset_name,
    Study       = tag,
    stringsAsFactors = FALSE
  )
}

datasets <- bind_rows(lapply(names(study_roots), function(nm) {
  phase <- switch(nm, ASTRA_Main = "Main", ASTRA_Pilot = "Pilot", NA_character_)
  discover_datasets(study_roots[[nm]], study_tag[[nm]], phase_label = phase)
}))

cat(sprintf("Discovered %d dataset results file(s) across %d study root(s).\n",
            nrow(datasets), length(study_roots)))
if (nrow(datasets) == 0) stop("No datasets found -- check study_roots paths.")

# ===============================================================================
# STEP 2: READ PER-DATASET METADATA (Dilution, resp_col selection, cal exclusions)
# ===============================================================================

read_dataset_config <- function(dataset_dir) {
  meta_file <- file.path(dataset_dir, "run_metadata.yaml")
  defaults <- list(Dilution = 1, use_15nrdx_for_petn = FALSE, use_is_for_rdx = TRUE,
                    cal_exclude_lines = integer(0))
  if (!file.exists(meta_file)) {
    message("    [warn] No run_metadata.yaml for ", basename(dataset_dir), " -- using defaults (Dilution=1).")
    return(defaults)
  }
  meta <- tryCatch(yaml::read_yaml(meta_file), error = function(e) NULL)
  cfg <- meta$configuration
  if (is.null(cfg)) return(defaults)
  list(
    Dilution = if (!is.null(cfg$Dilution)) as.numeric(cfg$Dilution) else defaults$Dilution,
    use_15nrdx_for_petn = isTRUE(cfg$use_15nrdx_for_petn) || identical(cfg$use_15nrdx_for_petn, "yes"),
    use_is_for_rdx      = isTRUE(cfg$use_is_for_rdx) || identical(cfg$use_is_for_rdx, "yes"),
    cal_exclude_lines   = if (!is.null(cfg$cal_exclude_lines) && length(cfg$cal_exclude_lines) > 0) {
      as.numeric(unlist(cfg$cal_exclude_lines))
    } else integer(0)
  )
}

# ===============================================================================
# STEP 3: PER-DATASET x PER-CALIBRATION-SET x PER-ANALYTE ICH LOD/LOQ
# ===============================================================================

compute_ich_lodloq_one <- function(cal_fit, resp_col, x_col = "CalLevelAdj") {
  # Replicates Code/03_Quantification.R's exact weighted quadratic fit,
  # then derives sigma (residual SE) and the local slope at the lowest
  # calibration level, per the agreed methodology (see header comment).
  if (nrow(cal_fit) < min_cal_points || all(is.na(cal_fit[[resp_col]]))) return(NULL)

  cal_weights <- 1 / cal_fit[[x_col]]
  quad_formula <- reformulate(c(x_col, paste0("I(", x_col, "^2)")), response = resp_col)
  quad_model <- tryCatch(lm(quad_formula, data = cal_fit, weights = cal_weights),
                          error = function(e) NULL)
  if (is.null(quad_model)) return(NULL)

  qsum <- summary(quad_model)
  sigma_resid <- qsum$sigma

  coefs <- coef(quad_model)
  a <- coefs[paste0("I(", x_col, "^2)")]
  b <- coefs[x_col]
  x_lowest <- min(cal_fit[[x_col]], na.rm = TRUE)
  local_slope <- 2 * a * x_lowest + b

  if (!is.finite(local_slope) || abs(local_slope) < .Machine$double.eps) return(NULL)

  list(
    N_points     = nrow(cal_fit),
    R2           = qsum$r.squared,
    sigma_resid  = sigma_resid,
    x_lowest     = x_lowest,
    local_slope  = as.numeric(local_slope),
    LOD_ng       = 3.3 * sigma_resid / abs(local_slope),
    LOQ_ng       = 10  * sigma_resid / abs(local_slope)
  )
}

results <- list()

for (ds_idx in seq_len(nrow(datasets))) {

  ds <- datasets[ds_idx, ]
  cat(sprintf("[%d/%d] %s (%s)\n", ds_idx, nrow(datasets), ds$Dataset, ds$Study))

  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df) || !"CalLevel" %in% names(df)) {
    message("    [skip] Could not read or missing required columns.")
    next
  }

  cfg <- read_dataset_config(ds$DatasetDir)

  cal <- df %>%
    dplyr::filter(Type == "Cal", !is.na(CalLevel)) %>%
    dplyr::mutate(CalLevelAdj = CalLevel * cfg$Dilution)

  if (nrow(cal) == 0 || !"CalibrationSet" %in% names(cal)) next

  for (analyte in c("PETN", "RDX")) {

    if (analyte == "PETN") {
      resp_col <- if (cfg$use_15nrdx_for_petn) "petn_ratio" else "petn_pa"
      quant_col <- if ("petn_concentration_dc" %in% names(df)) "petn_concentration_dc" else "petn_concentration"
    } else {
      resp_col <- if (cfg$use_is_for_rdx) "rdx_ratio" else "rdx_pa"
      quant_col <- "rdx_concentration"
    }
    snr_flag_col <- paste0(tolower(analyte), "_snr_flag")

    if (!resp_col %in% names(cal)) next

    for (cs in sort(unique(cal$CalibrationSet))) {

      cal_fit <- cal %>% dplyr::filter(CalibrationSet == cs)

      # Below_LOD filter -- matches Code/03_Quantification.R exactly
      if (snr_flag_col %in% names(cal_fit)) {
        cal_fit <- cal_fit %>%
          dplyr::filter(is.na(.data[[snr_flag_col]]) | .data[[snr_flag_col]] != "Below_LOD")
      }

      # Manual per-dataset calibration-point exclusions (cal_exclude_lines)
      if (length(cfg$cal_exclude_lines) > 0 && "Line" %in% names(cal_fit)) {
        cal_fit <- cal_fit %>% dplyr::filter(!(Line %in% cfg$cal_exclude_lines))
      }

      fit_result <- compute_ich_lodloq_one(cal_fit, resp_col)
      if (is.null(fit_result)) next

      # Lightweight cross-check: lowest real Sample/QC concentration in
      # this same calibration set already called "Quantifiable" (SNR>=10)
      # by the LIVE pipeline -- no raw-signal reprocessing needed, both
      # numbers already share the same ng units.
      empirical_loq <- NA_real_
      if (snr_flag_col %in% names(df) && quant_col %in% names(df) && "CalibrationSet" %in% names(df)) {
        quant_rows <- df %>%
          dplyr::filter(CalibrationSet == cs, Type %in% c("Sample", "QC"),
                        .data[[snr_flag_col]] == "Quantifiable",
                        !is.na(.data[[quant_col]]), .data[[quant_col]] > 0)
        if (nrow(quant_rows) > 0) empirical_loq <- min(quant_rows[[quant_col]], na.rm = TRUE)
      }

      results[[length(results) + 1]] <- data.frame(
        Study = ds$Study, Dataset = ds$Dataset, CalibrationSet = cs, Analyte = analyte,
        RespCol = resp_col, N_points = fit_result$N_points, R2 = fit_result$R2,
        Dilution = cfg$Dilution, x_lowest_ng = fit_result$x_lowest,
        sigma_resid = fit_result$sigma_resid, local_slope = fit_result$local_slope,
        ICH_LOD_ng = fit_result$LOD_ng, ICH_LOQ_ng = fit_result$LOQ_ng,
        Empirical_Quantifiable_Min_ng = empirical_loq,
        stringsAsFactors = FALSE
      )
    }
  }
}

per_calset <- bind_rows(results)
cat(sprintf("\nComputed ICH LOD/LOQ for %d dataset x CalibrationSet x analyte combination(s).\n", nrow(per_calset)))

if (nrow(per_calset) == 0) stop("No calibration sets yielded a usable fit -- nothing to report.")

write.csv(per_calset, file.path(output_dir, "ICH_LODLOQ_PerCalibrationSet.csv"), row.names = FALSE)
cat("Saved: ICH_LODLOQ_PerCalibrationSet.csv\n")

# ===============================================================================
# STEP 4: AGGREGATE -- PER ANALYTE x PER STUDY (the primary citable numbers)
# ===============================================================================

summary_tbl <- per_calset %>%
  dplyr::group_by(Study, Analyte) %>%
  dplyr::summarise(
    N_CalSets           = dplyr::n(),
    LOD_ng_mean          = mean(ICH_LOD_ng, na.rm = TRUE),
    LOD_ng_median         = median(ICH_LOD_ng, na.rm = TRUE),
    LOD_ng_min            = min(ICH_LOD_ng, na.rm = TRUE),
    LOD_ng_max            = max(ICH_LOD_ng, na.rm = TRUE),
    LOQ_ng_mean           = mean(ICH_LOQ_ng, na.rm = TRUE),
    LOQ_ng_median         = median(ICH_LOQ_ng, na.rm = TRUE),
    LOQ_ng_min            = min(ICH_LOQ_ng, na.rm = TRUE),
    LOQ_ng_max            = max(ICH_LOQ_ng, na.rm = TRUE),
    Empirical_Quant_Min_median = median(Empirical_Quantifiable_Min_ng, na.rm = TRUE),
    .groups = "drop"
  )

# Pooled (both studies combined) row per analyte, for a single
# whole-instrument number
pooled_tbl <- per_calset %>%
  dplyr::group_by(Analyte) %>%
  dplyr::summarise(
    Study = "Pooled (FINEX+ASTRA)",
    N_CalSets = dplyr::n(),
    LOD_ng_mean = mean(ICH_LOD_ng, na.rm = TRUE), LOD_ng_median = median(ICH_LOD_ng, na.rm = TRUE),
    LOD_ng_min = min(ICH_LOD_ng, na.rm = TRUE), LOD_ng_max = max(ICH_LOD_ng, na.rm = TRUE),
    LOQ_ng_mean = mean(ICH_LOQ_ng, na.rm = TRUE), LOQ_ng_median = median(ICH_LOQ_ng, na.rm = TRUE),
    LOQ_ng_min = min(ICH_LOQ_ng, na.rm = TRUE), LOQ_ng_max = max(ICH_LOQ_ng, na.rm = TRUE),
    Empirical_Quant_Min_median = median(Empirical_Quantifiable_Min_ng, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::select(Study, Analyte, dplyr::everything())

summary_tbl <- bind_rows(summary_tbl, pooled_tbl)

write.csv(summary_tbl, file.path(output_dir, "ICH_LODLOQ_Summary.csv"), row.names = FALSE)
cat("Saved: ICH_LODLOQ_Summary.csv\n\n")

cat("=== ICH Q2(R2) calibration-curve LOD/LOQ (ng), per analyte x study ===\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

# ===============================================================================
# STEP 5: VISUAL CROSS-CHECK -- ICH-derived vs the SNR-system's own
# empirically-observed "Quantifiable" boundary, per analyte
# ===============================================================================

plot_data <- per_calset %>%
  dplyr::filter(!is.na(ICH_LOQ_ng)) %>%
  dplyr::mutate(Analyte = factor(Analyte, levels = c("PETN", "RDX")))

p <- ggplot(plot_data, aes(x = Dataset, y = ICH_LOQ_ng, color = Analyte)) +
  geom_point(size = 2.5, alpha = 0.85, position = position_dodge(width = 0.3)) +
  geom_point(aes(y = Empirical_Quantifiable_Min_ng, shape = "Empirical \"Quantifiable\" boundary (live SNR system)"),
             size = 2.5, alpha = 0.7, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = pal_analyte) +
  scale_shape_manual(values = c("Empirical \"Quantifiable\" boundary (live SNR system)" = 4), name = NULL) +
  facet_wrap(~ Study, scales = "free_x") +
  coord_cartesian(ylim = c(0, quantile(plot_data$ICH_LOQ_ng, 0.95, na.rm = TRUE) * 1.3)) +
  labs(
    title = "ICH Q2(R2) calibration-curve LOQ vs the live SNR system's empirical Quantifiable boundary",
    subtitle = paste0("Circles = LOQ = 10 x sigma_resid / local_slope (ng), per calibration set | ",
                      "X = lowest real sample/QC concentration already called \"Quantifiable\" (SNR>=10) by the live pipeline\n",
                      "Circle clearly ABOVE its X: the live SNR system is more lenient than the ICH calibration-curve LOQ at that point"),
    x = NULL, y = "Concentration (ng)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 8.5),
    legend.position = "bottom"
  )

out_plot <- file.path(output_dir, "ICH_LODLOQ_Comparison.png")
pre_write_mtime <- if (file.exists(out_plot)) file.info(out_plot)$mtime else as.POSIXct(-Inf)
ggsave(out_plot, p, width = 13, height = 7, dpi = 300)
if (file.exists(out_plot) && file.info(out_plot)$mtime > pre_write_mtime) {
  cat(sprintf("Saved: %s\n", out_plot))
} else {
  warning("ggsave() returned normally but ", out_plot, " does not appear to have just been written.")
}

cat("\nDone. All outputs in: ", output_dir, "\n", sep = "")
