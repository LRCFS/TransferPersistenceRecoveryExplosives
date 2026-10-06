#############################################################
# CalibrationModel_Comparison.R
#############################################################
# Standalone, READ-ONLY diagnostic. Tests whether a DIFFERENT
# calibration fit (weighting scheme, or model order) would
# characterise the low end of the curve better than the current
# production model (weighted quadratic, weights = 1/CalLevelAdj).
#
# Critical design point: judging "which fit is better" by R^2 on
# the SAME 6 calibration points it was fit on is close to
# meaningless here -- a quadratic has 3 parameters fit to 6
# points, so R^2 will look excellent (0.99+) almost regardless of
# whether the curve is well-constrained at the low end specifically.
#
# Instead, this script uses the ALREADY-ACQUIRED 0.2ng QC
# replicates (5-6 per dataset, NOT used in the calibration fit
# itself) as an INDEPENDENT validation set: for each candidate
# model, back-calculate concentration from each QC replicate's
# own RAW RESPONSE (not its already-computed concentration), then
# compare:
#   - BIAS: how close is the mean predicted concentration to the
#     true value (0.2ng x Dilution)?
#   - PRECISION: what is the SD of the 5-6 predictions?
# The best-characterised low end is the model with the smallest
# bias AND smallest SD on this held-out QC data -- not the best
# R^2 on the calibration points themselves.
#
# Candidate models (all using the SAME already-acquired Cal
# standards, no new data):
#   QuadX1   -- current production: quadratic, weights = 1/x
#   QuadX2   -- quadratic, weights = 1/x^2 (more aggressive
#               down-weighting of the high end; common in
#               bioanalytical "1/x^2" calibration practice)
#   LinX1    -- linear, weights = 1/x (tests whether the
#               quadratic term is even earning its place)
#   LinX2    -- linear, weights = 1/x^2
#   QuadOLS  -- quadratic, unweighted (for comparison only)
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
  defaults <- list(Dilution = 1, use_15nrdx_for_petn = FALSE, use_is_for_rdx = TRUE, cal_exclude_lines = integer(0))
  if (!file.exists(meta_file)) return(defaults)
  meta <- tryCatch(yaml::read_yaml(meta_file), error = function(e) NULL)
  cfg <- meta$configuration
  if (is.null(cfg)) return(defaults)
  list(
    Dilution = if (!is.null(cfg$Dilution)) as.numeric(cfg$Dilution) else defaults$Dilution,
    use_15nrdx_for_petn = isTRUE(cfg$use_15nrdx_for_petn) || identical(cfg$use_15nrdx_for_petn, "yes"),
    use_is_for_rdx      = isTRUE(cfg$use_is_for_rdx) || identical(cfg$use_is_for_rdx, "yes"),
    cal_exclude_lines   = if (!is.null(cfg$cal_exclude_lines) && length(cfg$cal_exclude_lines) > 0) as.numeric(unlist(cfg$cal_exclude_lines)) else integer(0)
  )
}

# Invert a fitted model (linear or quadratic) at response value y,
# returning the smallest non-negative real root/solution -- mirrors
# Code/03_Quantification.R's solve_concentration() root-selection rule.
invert_model <- function(model, y, is_quadratic) {
  coefs <- coef(model)
  if (is_quadratic) {
    a <- coefs["I(x^2)"]; b <- coefs["x"]; c_int <- coefs["(Intercept)"]
    disc <- b^2 - 4 * a * (c_int - y)
    if (is.na(disc) || disc < 0) return(0)
    roots <- c((-b + sqrt(disc)) / (2 * a), (-b - sqrt(disc)) / (2 * a))
    roots <- roots[is.finite(roots)]
    pos <- roots[roots >= 0]
    if (length(pos) == 0) return(0)
    return(min(pos))
  } else {
    b <- coefs["x"]; c_int <- coefs["(Intercept)"]
    val <- (y - c_int) / b
    return(max(0, val))
  }
}

fit_and_validate <- function(cal_fit, qc_resp, true_conc, resp_col) {
  x <- cal_fit$CalLevelAdj
  y <- cal_fit[[resp_col]]
  d <- data.frame(x = x, y = y)
  names(d)[2] <- "y"

  candidates <- list(
    QuadX1  = list(formula = y ~ x + I(x^2), weights = 1/x, quad = TRUE),
    QuadX2  = list(formula = y ~ x + I(x^2), weights = 1/x^2, quad = TRUE),
    LinX1   = list(formula = y ~ x,          weights = 1/x, quad = FALSE),
    LinX2   = list(formula = y ~ x,          weights = 1/x^2, quad = FALSE),
    QuadOLS = list(formula = y ~ x + I(x^2), weights = rep(1, length(x)), quad = TRUE)
  )

  out <- list()
  for (nm in names(candidates)) {
    cand <- candidates[[nm]]
    m <- tryCatch(lm(cand$formula, data = d, weights = cand$weights), error = function(e) NULL)
    if (is.null(m)) next
    preds <- sapply(qc_resp, function(yy) invert_model(m, yy, cand$quad))
    preds <- preds[is.finite(preds)]
    if (length(preds) < 3) next
    out[[nm]] <- data.frame(
      Model = nm,
      R2 = summary(m)$r.squared,
      QC_pred_mean = mean(preds), QC_pred_sd = sd(preds),
      QC_pred_bias_pct = 100 * (mean(preds) - true_conc) / true_conc,
      QC_pred_n = length(preds)
    )
  }
  bind_rows(out)
}

results <- list()

for (ds_idx in seq_len(nrow(datasets))) {
  ds <- datasets[ds_idx, ]
  df <- tryCatch(read.csv(ds$ResultsFile, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"Type" %in% names(df) || !"CalLevel" %in% names(df)) next
  cfg <- read_dataset_config(ds$DatasetDir)

  cal <- df %>% dplyr::filter(Type == "Cal", !is.na(CalLevel)) %>%
    dplyr::mutate(CalLevelAdj = CalLevel * cfg$Dilution)
  if (nrow(cal) == 0 || !"CalibrationSet" %in% names(cal)) next

  for (analyte in c("PETN", "RDX")) {
    resp_col <- if (analyte == "PETN") { if (cfg$use_15nrdx_for_petn) "petn_ratio" else "petn_pa" } else { if (cfg$use_is_for_rdx) "rdx_ratio" else "rdx_pa" }
    flag_col <- paste0(tolower(analyte), "_snr_flag")
    if (!resp_col %in% names(cal)) next

    for (cs in sort(unique(cal$CalibrationSet))) {
      cal_fit <- cal %>% dplyr::filter(CalibrationSet == cs)
      if (flag_col %in% names(cal_fit)) {
        cal_fit <- cal_fit %>% dplyr::filter(is.na(.data[[flag_col]]) | .data[[flag_col]] != "Below_LOD")
      }
      if (length(cfg$cal_exclude_lines) > 0 && "Line" %in% names(cal_fit)) {
        cal_fit <- cal_fit %>% dplyr::filter(!(Line %in% cfg$cal_exclude_lines))
      }
      if (nrow(cal_fit) < 5 || all(is.na(cal_fit[[resp_col]]))) next

      qc_02 <- df %>% dplyr::filter(Type == "QC", CalLevel == 0.2, CalibrationSet == cs)
      if (nrow(qc_02) < 3 || !resp_col %in% names(qc_02)) next
      qc_resp <- qc_02[[resp_col]]
      qc_resp <- qc_resp[!is.na(qc_resp)]
      if (length(qc_resp) < 3) next

      true_conc <- 0.2 * cfg$Dilution

      cmp <- fit_and_validate(cal_fit, qc_resp, true_conc, resp_col)
      if (nrow(cmp) == 0) next
      cmp$Study <- ds$Study; cmp$Dataset <- ds$Dataset; cmp$Analyte <- analyte; cmp$CalibrationSet <- cs
      results[[length(results) + 1]] <- cmp
    }
  }
}

tbl <- bind_rows(results)
write.csv(tbl, file.path(output_dir, "CalibrationModel_Comparison_Detail.csv"), row.names = FALSE)

cat("=== Calibration model comparison: held-out prediction of 0.2ng QC replicates ===\n")
cat("    (judged by BIAS vs true concentration AND precision/SD across replicates --\n")
cat("     NOT by R2 on the same calibration points the model was fit on)\n\n")

summary_tbl <- tbl %>%
  dplyr::group_by(Analyte, Model) %>%
  dplyr::summarise(
    N_calsets = dplyr::n(),
    Median_R2 = median(R2, na.rm = TRUE),
    Median_abs_bias_pct = median(abs(QC_pred_bias_pct), na.rm = TRUE),
    Median_SD_ng = median(QC_pred_sd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Analyte, Median_SD_ng)

print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\nFull per-calibration-set detail saved to CalibrationModel_Comparison_Detail.csv\n")
