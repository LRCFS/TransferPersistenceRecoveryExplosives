# =========================================================
# InjectionAcceptance.R
# =========================================================
#
# Shared injection-acceptance logic for GC-MS PETN/RDX quantitation
# pipelines. Implements the flowchart in
# GCMSQuantitation/Diagnostics/Improved_QC_Flowchart.dot (Sections A-D),
# as finalised in the "flowchart implementation" planning session
# (see CONTEXT.md).
#
# This file is SOURCED (not run standalone) by BOTH study pipelines:
#   - GCMSQuantitation/FINEX/04_CollateStudyResults.R     (FINEX study)
#   - ASTRA/doe/pilot_analysis.R                          (ASTRA study)
#
# Before this file existed, both study scripts contained independent,
# hand-ported copies of this logic (assign_qc_brackets(),
# compute_analysis_accepted(), compute_nc_analysis_accepted(),
# compute_nc_result()), which had already begun to silently diverge
# (e.g. ASTRA treating non-quantifiable samples as legitimate negatives,
# FINEX still hard-failing them). This file is now the SINGLE source of
# truth for "was this injection/run trustworthy, and what does it mean
# for this sample/NC" -- study scripts should only implement genuinely
# study-specific concerns (recovery-calculation constants, sample-name
# parsing, design-matrix merges, summary-sheet layout, statistics).
#
# -----------------------------------------------------------------
# Required input columns (already produced by 02_PeakDetection.R /
# 03_Quantification.R and read from *_GCMSResults.csv by the calling
# study script):
#   Line, SourceFile, Type ("Blank"/"QC"/"Sample"/"Cal"), CalLevel,
#   IsNegativeControl,
#   petn_snr_flag, rdx_snr_flag        ("Below_LOD"/"Below_LOQ"/"Quantifiable"/NA)
#   petn_snr, rdx_snr                  (numeric)
#   petn_concentration, petn_concentration_dc, rdx_concentration
#   rdx_is_pa_flag, rdx_is_pa, rdx_is_snr_flag
#   petn_percent_bias_dc (or petn_percent_bias), rdx_percent_bias
#   petn_qc_flag_dc, rdx_qc_flag       (as written by 03_Quantification.R)
#   tic_other_peak_count, _rt1/_height1 .. _rtN/_heightN
#     (as written by 02_PeakDetection.R's find_other_tic_peaks();
#     optional -- see evaluate_blanks() below, backward-compatible if
#     absent)
#
# -----------------------------------------------------------------
# Section A's "other significant peak" (instrument-performance-only)
# branch (added Aug 2026 -- see CONTEXT.md "Blank Other-Significant-
# Peak Detection" session): implemented via a full-chromatogram TIC
# scan in 02_PeakDetection.R (find_other_tic_peaks(), Code/ModPeaks.R),
# consumed here in evaluate_blanks(). Deliberately non-gating, per the
# flowchart -- see evaluate_blanks()'s own header comment for detail.
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# =========================================================
# Shared thresholds (defaults match both studies' existing values;
# pass overrides only if a study genuinely needs to diverge)
# =========================================================
IA_DEFAULT_SNR_DETECT      <- 3    # LOD
IA_DEFAULT_SNR_QUANT       <- 10   # LOQ
IA_DEFAULT_QC_6NG_BIAS_LIM <- 20   # +/- 20%

# =========================================================
# Confirmatory (qualifier) ion identity-confirmation config (added Aug
# 2026 -- see CONTEXT.md "Confirmatory Ion Detection" session). Maps
# each analyte prefix to the qualifier-ion column suffixes that MUST
# show independent detection (SNR >= detect threshold, i.e. snr_flag is
# "Below_LOQ" or "Quantifiable") at the Quantifiable SNR tier before
# that analyte's own quantifier result can be trusted as a genuine
# identification, not just a peak that happens to sit at the right
# retention time. Derived empirically (Stage 2/3 real-data analysis,
# all 11 "Accepted Analysis" FINEX datasets):
#   - PETN: only m/z 76 (`qual76`) is reliable enough to require -- the
#     other monitored qualifier, m/z 57, reached a measurable SNR in
#     only 18.2% of pure calibration standards (i.e. even the cleanest,
#     highest-confidence injections), so requiring it would fail
#     ~70-100% of genuine Cal/QC/Sample detections rather than catch
#     misidentifications. Excluded entirely -- not even reported.
#   - RDX: both m/z 46 and m/z 75 (`qual46`, `qual75`) are required --
#     both showed ~100% detection in Cal standards, and requiring both
#     costs zero additional failures on any real Sample/QC/Cal row in
#     the study (the only rows affected were 2 Blank rows, which this
#     flowchart doesn't gate on identity confirmation anyway).
# A ratio-based check (qualifier/quantifier PA ratio within a tolerance)
# was investigated and deliberately rejected: RDX's ratio showed a
# real, strong concentration-dependence (r=-0.72 for m/z46, r=-0.69 for
# m/z75, both p<1e-10) consistent with the same non-linear response
# that is the reason this codebase already uses quadratic (not linear)
# calibration for RDX/PETN -- a single fixed ratio+tolerance would not
# be valid across the calibration range. A simple qualitative detection
# check was used instead.
#
# Only applied when the qualifier's own flag COLUMN exists in the data
# at all (not just non-NA for a given row) -- datasets processed before
# Stage 1 of this feature (or by other studies, e.g. ASTRA's
# `Design of Experiments/pilot_analysis.R`, which also sources this
# file) simply won't have these columns, and the check is skipped
# entirely rather than treated as a failure -- fully backward-compatible.
IA_QUALIFIER_IONS <- list(
  petn = c("qual76"),
  rdx  = c("qual46", "qual75")
)

IA_is_qualifier_detected <- function(flag_value) {
  !is.na(flag_value) && flag_value %in% c("Below_LOQ", "Quantifiable")
}

# =========================================================
# SECTION A -- Blank Injection Evaluation
# =========================================================
# For every Blank row, classify PETN and RDX independently into the same
# three-tier SNR system used everywhere else in this file (Negative /
# Trace / Quantifiable), then summarise into blank_status. Only a
# Quantifiable tier on either analyte counts as HIGH carryover risk
# (the calling script's bracketing-blank check, below, hard-fails on
# this); a Trace tier is a softer signal (contributes a caveat, not a
# hard fail) to the bracketing sample/NC; a Negative tier is clean.
#
# "Other significant peak" detection (added Aug 2026 -- see CONTEXT.md
# "Blank Other-Significant-Peak Detection" session): implements the A2/
# A_OTHER branch of Diagnostics/Improved_QC_Flowchart.dot Section A,
# sourced from the tic_other_peak_* columns computed per-injection in
# Code/02_PeakDetection.R (find_other_tic_peaks(), Code/ModPeaks.R).
# Per the flowchart, this is DELIBERATELY non-gating -- an unexplained
# TIC peak is instrument-performance information only ("does NOT set a
# carryover-risk flag -- no PETN/RDX signal was seen, so there is
# nothing to carry over"), so it must NOT alter blank_status (or,
# downstream, petn_blank_bracket/rdx_blank_bracket). It is exposed here
# purely so a separate, trend-only Instrument-Performance Log can be
# built from it. Backward-compatible: if the tic_other_peak_* columns
# don't exist at all (older datasets, or datasets not yet reprocessed
# since this feature was added), blank_other_peak_flag is always FALSE,
# not treated as an error -- same pattern as IA_is_qualifier_detected()
# elsewhere in this file.
evaluate_blanks <- function(all_data,
                             snr_detect_threshold = IA_DEFAULT_SNR_DETECT,
                             snr_quant_threshold  = IA_DEFAULT_SNR_QUANT) {

  classify_tier <- function(snr_flag_value) {
    if (is.na(snr_flag_value)) return("Negative")
    if (snr_flag_value == "Quantifiable") return("Quantifiable")
    if (snr_flag_value == "Below_LOQ") return("Trace")
    return("Negative")  # "Below_LOD" or any other value
  }

  all_data$blank_petn_tier <- NA_character_
  all_data$blank_rdx_tier  <- NA_character_
  all_data$blank_status    <- NA_character_
  all_data$blank_other_peak_flag   <- FALSE
  all_data$blank_other_peak_rt     <- NA_real_
  all_data$blank_other_peak_height <- NA_real_

  has_tic_other_cols <- all(c("tic_other_peak_count", "tic_other_peak_rt1",
                               "tic_other_peak_height1") %in% names(all_data))

  blank_idx <- which(all_data$Type == "Blank")
  if (length(blank_idx) == 0) {
    return(all_data)
  }

  for (i in blank_idx) {
    petn_flag <- if ("petn_snr_flag" %in% names(all_data)) all_data$petn_snr_flag[i] else NA_character_
    rdx_flag  <- if ("rdx_snr_flag" %in% names(all_data))  all_data$rdx_snr_flag[i]  else NA_character_

    petn_tier <- classify_tier(petn_flag)
    rdx_tier  <- classify_tier(rdx_flag)

    all_data$blank_petn_tier[i] <- petn_tier
    all_data$blank_rdx_tier[i]  <- rdx_tier

    if (petn_tier == "Quantifiable" || rdx_tier == "Quantifiable") {
      all_data$blank_status[i] <- "Contaminated"
    } else if (petn_tier == "Trace" || rdx_tier == "Trace") {
      all_data$blank_status[i] <- "Trace detected"
    } else {
      all_data$blank_status[i] <- "Clean"
    }

    # Other-significant-peak flag -- informational only, does NOT feed
    # into blank_status above (see header comment). NA-safe: cnt should
    # never actually be NA (find_other_tic_peaks() always returns an
    # integer count), but guarded defensively anyway. blank_other_peak_rt/
    # height surface only the single LARGEST peak (tic_other_peak_*1)
    # for a quick-glance column here -- the full set of all detected
    # peaks (tic_other_peak_rt1..N etc, up to tic_other_peak_max_report)
    # remains directly available in all_data for the study-level
    # recurring-peak clustering in FINEX/04_CollateStudyResults.R, which
    # reads those numbered columns itself rather than through this
    # function.
    if (has_tic_other_cols) {
      cnt <- all_data$tic_other_peak_count[i]
      if (!is.na(cnt) && cnt > 0) {
        all_data$blank_other_peak_flag[i]   <- TRUE
        all_data$blank_other_peak_rt[i]     <- all_data$tic_other_peak_rt1[i]
        all_data$blank_other_peak_height[i] <- all_data$tic_other_peak_height1[i]
      }
    }
  }

  return(all_data)
}

# Find the nearest blank to `sample_line`, in the given direction, within
# the same SourceFile. Unlike QC brackets (assign_qc_brackets, below),
# a missing blank on one side is NOT a hard requirement -- blanks run
# far more frequently than QCs in these sequences, and the flowchart
# discussion did not extend the QC "both sides required" rule to
# blanks. A missing blank on one side is therefore treated leniently
# (as if clean on that side), not as a failure. Revisit if this proves
# wrong in practice.
find_nearest_blank <- function(blank_df, sample_line, direction = c("pre", "post")) {
  direction <- match.arg(direction)
  candidates <- if (direction == "pre") {
    blank_df %>% filter(Line < sample_line) %>% arrange(desc(Line))
  } else {
    blank_df %>% filter(Line > sample_line) %>% arrange(Line)
  }
  if (nrow(candidates) == 0) return(NULL)
  candidates[1, , drop = FALSE]
}

# =========================================================
# SECTION B -- QC Standard Evaluation
# =========================================================
# Evaluates 6ng (quantitation, bias-based) and 0.2ng (sensitivity,
# SNR-based, now a symmetric 3-tier PASS/WARN/FAIL for BOTH PETN and
# RDX) QC brackets, pre- and post- each sample block, with automatic
# substitution of the next QC further out if the nearest one failed to
# inject (find_injected_qc_pre/post below) -- this substitution
# mechanism already existed in both studies' prior duplicated code and
# is carried over unchanged, since it already matched the flowchart's
# intended design.
#
# `qc_bracket_exclude` (added Aug 2026): the automatic "failed to inject"
# check below is based ONLY on the internal standard's SNR flag (i.e. "did
# the syringe fire at all") -- it does NOT catch a QC where the IS looks
# fine but ONE analyte's own peak is obviously wrong (e.g. a real case:
# FINEX "Outstanding samples 4", 6ng QC at sequence Line 39, PETN PA = 0
# while the IS was not flagged low -- this QC still got used as a bracket
# for neighbouring samples, failing them on a bogus PETN 6ng bracket even
# though their own PETN signal was fine). Rather than guess at a general
# per-analyte automatic detection rule (risk of false positives on
# genuinely low-but-real QC responses), `qc_bracket_exclude` is an explicit,
# auditable manual override: a named list, `list("<SourceFile substring>" =
# c(<Line numbers>))`, e.g. `list("Outstanding samples 4" = c(39))`. Matched
# QC rows are treated as NOT injected for bracket-assignment purposes only
# (both PETN and RDX -- the whole injection is being excluded from bracket
# duty, not just one analyte's reading of it) -- causing the next QC on
# that side to be substituted instead, exactly like the automatic check.
# The excluded QC is NOT removed from the dataset or from its own PASS/FAIL
# evaluation, calibration, or drift-correction fit -- only from being used
# as someone else's bracket. Matching is by substring against SourceFile
# (the full results-CSV path), NOT by bare Line number, since Line numbers
# restart at 1 for every dataset/sequence and are not unique study-wide.
#
# Also attaches, to every Sample/NC row, the nearest bracketing blank's
# status on each side (for the carryover check in Section C).
assign_qc_brackets <- function(all_data, petn_bias_col, rdx_bias_col,
                                qc_6ng_limit = IA_DEFAULT_QC_6NG_BIAS_LIM,
                                snr_detect_threshold = IA_DEFAULT_SNR_DETECT,
                                snr_quant_threshold  = IA_DEFAULT_SNR_QUANT,
                                qc_bracket_exclude = list()) {

  init_bracket_cols <- function(df) {
    for (col in c("petn_qc_6ng_pre", "petn_qc_6ng_post", "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
                  "petn_qc_02ng_pre", "petn_qc_02ng_post", "rdx_qc_02ng_pre", "rdx_qc_02ng_post")) {
      df[[col]] <- NA_character_
    }
    for (col in c("petn_qc_6ng_pre_inherited", "petn_qc_6ng_post_inherited",
                  "rdx_qc_6ng_pre_inherited", "rdx_qc_6ng_post_inherited",
                  "petn_qc_02ng_pre_inherited", "petn_qc_02ng_post_inherited",
                  "rdx_qc_02ng_pre_inherited", "rdx_qc_02ng_post_inherited")) {
      df[[col]] <- FALSE
    }
    df$petn_blank_bracket <- NA_character_
    df$rdx_blank_bracket  <- NA_character_
    df
  }

  if (!("Line" %in% names(all_data))) {
    warning("No 'Line' column found. QC/blank brackets cannot be assigned.")
    return(init_bracket_cols(all_data))
  }

  # -------------------------------------------------------
  # 0.2ng symmetric 3-tier flag (PASS/WARN/FAIL), computed fresh here for
  # BOTH analytes from petn_snr/petn_concentration_dc and
  # rdx_snr/rdx_concentration respectively, so PETN and RDX get the
  # identical tiering rule (concentration<=0 -> FAIL; SNR<detect -> FAIL;
  # SNR<quant -> WARN; else PASS). Replaces the old asymmetric pairing
  # (PETN's own petn_qc_flag_dc vs RDX's 2-tier-only rdx_qc_flag).
  compute_sensitivity_tier <- function(snr_val, conc_val) {
    if (is.na(snr_val) || is.na(conc_val)) return("SENSITIVITY_CHECK_FAIL")
    if (conc_val <= 0) return("SENSITIVITY_CHECK_FAIL")
    if (snr_val < snr_detect_threshold) return("SENSITIVITY_CHECK_FAIL")
    if (snr_val < snr_quant_threshold) return("SENSITIVITY_CHECK_WARN")
    return("SENSITIVITY_CHECK_PASS")
  }

  petn_conc_col_for_02ng <- if ("petn_concentration_dc" %in% names(all_data)) {
    "petn_concentration_dc"
  } else {
    "petn_concentration"
  }

  all_data$petn_qc_flag_symmetric <- NA_character_
  all_data$rdx_qc_flag_symmetric  <- NA_character_
  qc_02_idx <- which(all_data$Type == "QC" & all_data$CalLevel == 0.2)
  for (i in qc_02_idx) {
    petn_snr_i  <- if ("petn_snr" %in% names(all_data)) all_data$petn_snr[i] else NA_real_
    rdx_snr_i   <- if ("rdx_snr" %in% names(all_data))  all_data$rdx_snr[i]  else NA_real_
    petn_conc_i <- if (petn_conc_col_for_02ng %in% names(all_data)) all_data[[petn_conc_col_for_02ng]][i] else NA_real_
    rdx_conc_i  <- if ("rdx_concentration" %in% names(all_data)) all_data$rdx_concentration[i] else NA_real_

    all_data$petn_qc_flag_symmetric[i] <- compute_sensitivity_tier(petn_snr_i, petn_conc_i)
    all_data$rdx_qc_flag_symmetric[i]  <- compute_sensitivity_tier(rdx_snr_i, rdx_conc_i)
  }

  # Extract QC rows with bias columns AND SNR columns AND the new
  # symmetric 0.2ng flags
  bias_cols_to_select <- c(petn_bias_col, rdx_bias_col)
  bias_cols_to_select <- bias_cols_to_select[!is.na(bias_cols_to_select)]

  qc_rows <- all_data %>%
    filter(Type == "QC") %>%
    select(SourceFile, Line, CalLevel,
           any_of(c(bias_cols_to_select, "petn_snr", "rdx_snr",
                    "petn_qc_flag_symmetric", "rdx_qc_flag_symmetric",
                    "rdx_is_snr_flag")))

  # Determine which QCs actually injected (RDX IS detected)
  if ("rdx_is_snr_flag" %in% names(qc_rows)) {
    qc_rows$injected <- !is.na(qc_rows$rdx_is_snr_flag) &
                         qc_rows$rdx_is_snr_flag != "Not_Detected"
  } else {
    qc_rows$injected <- TRUE
  }

  # Apply manual qc_bracket_exclude overrides (see header comment above) --
  # force matched QC rows to injected = FALSE so the automatic
  # find_injected_qc_pre/post substitution below skips them and uses the
  # next QC on that side instead. Silently ignores patterns/lines that
  # don't match anything currently loaded (e.g. a leftover entry for a
  # dataset not in this run), so a stale entry can't break other datasets.
  if (length(qc_bracket_exclude) > 0 && nrow(qc_rows) > 0) {
    for (pattern in names(qc_bracket_exclude)) {
      lines_to_exclude <- qc_bracket_exclude[[pattern]]
      match_rows <- grepl(pattern, qc_rows$SourceFile, fixed = TRUE) &
                    qc_rows$Line %in% lines_to_exclude
      if (any(match_rows)) {
        message("  qc_bracket_exclude: excluding QC Line(s) ",
                paste(qc_rows$Line[match_rows], collapse = ", "),
                " from bracket assignment (matched '", pattern,
                "') -- neighbouring samples will use the next QC on each side instead.")
        qc_rows$injected[match_rows] <- FALSE
      }
    }
  }

  if (nrow(qc_rows) == 0) {
    warning("No QC rows found. QC brackets cannot be assigned.")
    all_data <- init_bracket_cols(all_data)
    return(assign_blank_brackets(all_data))
  }

  if (!is.na(petn_bias_col) && petn_bias_col %in% names(qc_rows)) {
    qc_rows$petn_bias <- qc_rows[[petn_bias_col]]
  } else {
    qc_rows$petn_bias <- NA_real_
  }
  if (!is.na(rdx_bias_col) && rdx_bias_col %in% names(qc_rows)) {
    qc_rows$rdx_bias <- qc_rows[[rdx_bias_col]]
  } else {
    qc_rows$rdx_bias <- NA_real_
  }
  if (!("petn_snr" %in% names(qc_rows))) qc_rows$petn_snr <- NA_real_
  if (!("rdx_snr" %in% names(qc_rows)))  qc_rows$rdx_snr  <- NA_real_

  cal_levels <- sort(unique(qc_rows$CalLevel[!is.na(qc_rows$CalLevel)]))
  level_6ng <- if (6 %in% cal_levels) 6 else if (length(cal_levels) > 0) cal_levels[which.max(cal_levels)] else NA_real_
  level_02ng <- if (0.2 %in% cal_levels) 0.2 else if (length(cal_levels) > 1) cal_levels[which.min(cal_levels)] else NA_real_
  print(paste0("QC levels identified: 6ng = ", level_6ng, ", 0.2ng = ", level_02ng))

  # Bias-based (6ng) evaluator. snr_value distinguishes genuine
  # non-detection (real failure) from a structurally-unavailable bias
  # column (left NA/unevaluated).
  evaluate_qc_bias <- function(bias_value, limit, snr_value = NA_real_) {
    if (is.na(bias_value)) {
      if (is.na(snr_value)) return("FAIL")
      return(NA_character_)
    }
    if (abs(bias_value) <= limit) return("PASS")
    return("FAIL")
  }

  # Maps the symmetric SENSITIVITY_CHECK_* flag to bracket vocabulary.
  evaluate_qc_flag <- function(qc_flag_value) {
    if (length(qc_flag_value) == 0 || is.na(qc_flag_value)) return(NA_character_)
    if (qc_flag_value == "SENSITIVITY_CHECK_PASS") return("PASS")
    if (qc_flag_value == "SENSITIVITY_CHECK_WARN") return("WARN")
    if (qc_flag_value == "SENSITIVITY_CHECK_FAIL") return("FAIL")
    return(NA_character_)
  }

  # Nearest-injected-QC lookups (substitution mechanism carried over
  # unchanged -- already matches the flowchart's Section B fallback).
  find_injected_qc_pre <- function(qc_df, sample_line) {
    pre_qcs <- qc_df %>% filter(Line < sample_line) %>% arrange(desc(Line))
    if (nrow(pre_qcs) == 0) return(list(row = NULL, inherited = FALSE))
    if (pre_qcs$injected[1]) return(list(row = pre_qcs[1, , drop = FALSE], inherited = FALSE))
    for (k in seq_len(nrow(pre_qcs))) {
      if (pre_qcs$injected[k]) return(list(row = pre_qcs[k, , drop = FALSE], inherited = TRUE))
    }
    return(list(row = NULL, inherited = FALSE))
  }

  find_injected_qc_post <- function(qc_df, sample_line) {
    post_qcs <- qc_df %>% filter(Line > sample_line) %>% arrange(Line)
    if (nrow(post_qcs) == 0) return(list(row = NULL, inherited = FALSE))
    if (post_qcs$injected[1]) return(list(row = post_qcs[1, , drop = FALSE], inherited = FALSE))
    for (k in seq_len(nrow(post_qcs))) {
      if (post_qcs$injected[k]) return(list(row = post_qcs[k, , drop = FALSE], inherited = TRUE))
    }
    return(list(row = NULL, inherited = FALSE))
  }

  all_data <- init_bracket_cols(all_data)

  source_files <- unique(all_data$SourceFile[all_data$Type == "Sample"])

  for (sf in source_files) {
    sf_qc <- qc_rows %>% filter(SourceFile == sf)

    qc_6ng <- if (!is.na(level_6ng)) sf_qc %>% filter(CalLevel == level_6ng) %>% arrange(Line) else data.frame()
    qc_02ng <- if (!is.na(level_02ng)) sf_qc %>% filter(CalLevel == level_02ng) %>% arrange(Line) else data.frame()

    sf_sample_idx <- which(all_data$SourceFile == sf & all_data$Type == "Sample")

    for (i in sf_sample_idx) {
      sample_line <- all_data$Line[i]

      if (nrow(qc_6ng) > 0) {
        pre_result <- find_injected_qc_pre(qc_6ng, sample_line)
        if (!is.null(pre_result$row)) {
          all_data$petn_qc_6ng_pre[i] <- evaluate_qc_bias(pre_result$row[["petn_bias"]], qc_6ng_limit, pre_result$row[["petn_snr"]])
          all_data$rdx_qc_6ng_pre[i]  <- evaluate_qc_bias(pre_result$row[["rdx_bias"]], qc_6ng_limit, pre_result$row[["rdx_snr"]])
          all_data$petn_qc_6ng_pre_inherited[i] <- pre_result$inherited
          all_data$rdx_qc_6ng_pre_inherited[i]  <- pre_result$inherited
        }
        post_result <- find_injected_qc_post(qc_6ng, sample_line)
        if (!is.null(post_result$row)) {
          all_data$petn_qc_6ng_post[i] <- evaluate_qc_bias(post_result$row[["petn_bias"]], qc_6ng_limit, post_result$row[["petn_snr"]])
          all_data$rdx_qc_6ng_post[i]  <- evaluate_qc_bias(post_result$row[["rdx_bias"]], qc_6ng_limit, post_result$row[["rdx_snr"]])
          all_data$petn_qc_6ng_post_inherited[i] <- post_result$inherited
          all_data$rdx_qc_6ng_post_inherited[i]  <- post_result$inherited
        }
      }

      if (nrow(qc_02ng) > 0) {
        pre_result <- find_injected_qc_pre(qc_02ng, sample_line)
        if (!is.null(pre_result$row)) {
          all_data$petn_qc_02ng_pre[i] <- evaluate_qc_flag(pre_result$row[["petn_qc_flag_symmetric"]])
          all_data$rdx_qc_02ng_pre[i]  <- evaluate_qc_flag(pre_result$row[["rdx_qc_flag_symmetric"]])
          all_data$petn_qc_02ng_pre_inherited[i] <- pre_result$inherited
          all_data$rdx_qc_02ng_pre_inherited[i]  <- pre_result$inherited
        }
        post_result <- find_injected_qc_post(qc_02ng, sample_line)
        if (!is.null(post_result$row)) {
          all_data$petn_qc_02ng_post[i] <- evaluate_qc_flag(post_result$row[["petn_qc_flag_symmetric"]])
          all_data$rdx_qc_02ng_post[i]  <- evaluate_qc_flag(post_result$row[["rdx_qc_flag_symmetric"]])
          all_data$petn_qc_02ng_post_inherited[i] <- post_result$inherited
          all_data$rdx_qc_02ng_post_inherited[i]  <- post_result$inherited
        }
      }
    }
  }

  all_data <- assign_blank_brackets(all_data)

  return(all_data)
}

# Attaches petn_blank_bracket / rdx_blank_bracket to every Sample/NC row:
# "Clean" / "Trace" / "Contaminated", combining the nearest bracketing
# blank on each side (worst case wins: Contaminated > Trace > Clean). A
# missing blank on one/both sides is treated as Clean on that side (see
# find_nearest_blank() comment).
assign_blank_brackets <- function(all_data) {
  all_data$petn_blank_bracket <- NA_character_
  all_data$rdx_blank_bracket  <- NA_character_

  if (!("Line" %in% names(all_data)) || !("blank_petn_tier" %in% names(all_data))) {
    # evaluate_blanks() hasn't been run yet -- leave as NA (unevaluated),
    # the acceptance function treats this leniently (see Section C).
    return(all_data)
  }

  worse_of <- function(a, b) {
    rank <- c("Clean" = 1, "Trace" = 2, "Contaminated" = 3)
    vals <- c(a, b)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return("Clean")
    names(rank)[which.max(rank[vals])]
  }

  source_files <- unique(all_data$SourceFile[all_data$Type == "Sample"])
  for (sf in source_files) {
    sf_blanks <- all_data %>% filter(SourceFile == sf, Type == "Blank") %>%
      select(Line, blank_petn_tier, blank_rdx_tier)

    sf_sample_idx <- which(all_data$SourceFile == sf & all_data$Type == "Sample")
    for (i in sf_sample_idx) {
      sample_line <- all_data$Line[i]
      pre  <- find_nearest_blank(sf_blanks, sample_line, "pre")
      post <- find_nearest_blank(sf_blanks, sample_line, "post")

      tier_to_bracket <- function(tier) {
        if (is.null(tier) || is.na(tier)) return("Clean")
        if (tier == "Quantifiable") return("Contaminated")
        if (tier == "Trace") return("Trace")
        return("Clean")
      }

      petn_pre_b  <- if (!is.null(pre))  tier_to_bracket(pre$blank_petn_tier)  else "Clean"
      petn_post_b <- if (!is.null(post)) tier_to_bracket(post$blank_petn_tier) else "Clean"
      rdx_pre_b   <- if (!is.null(pre))  tier_to_bracket(pre$blank_rdx_tier)   else "Clean"
      rdx_post_b  <- if (!is.null(post)) tier_to_bracket(post$blank_rdx_tier)  else "Clean"

      all_data$petn_blank_bracket[i] <- worse_of(petn_pre_b, petn_post_b)
      all_data$rdx_blank_bracket[i]  <- worse_of(rdx_pre_b, rdx_post_b)
    }
  }

  return(all_data)
}

# =========================================================
# SECTION C+D -- Per-Analyte Evaluation & Row-Level Combination
# =========================================================
# Single unified function for BOTH Samples and Negative Controls --
# under the finalised flowchart there is no remaining structural
# difference between them (the old 0.2ng-bracket-FAIL override, which
# was the only thing that distinguished NC handling, has been removed
# entirely: a missing/failed bracket is now always a hard FAIL, for
# samples and NCs alike).
#
# For each row, PETN and RDX are evaluated independently:
#   1. Peak present? (height gate, i.e. snr_flag is not NA)
#        No  -> tier = Negative (pending LOD confirmation), skip carryover
#        Yes -> carryover check (petn_blank_bracket / rdx_blank_bracket):
#                 Contaminated -> hard FAIL (carryover)
#                 Trace        -> caveat noted, continue
#                 Clean        -> continue
#              then SNR tier: >=quant -> Quantifiable; >=detect -> Trace;
#                              else -> Negative (pending LOD confirmation)
#   2. Confirmatory ion check (Quantifiable tier only, added Aug 2026 --
#      see IA_QUALIFIER_IONS above): the analyte's designated qualifier
#      ion(s) must ALSO show independent detection (SNR>=3) at the same
#      retention time. Missing -> hard FAIL (identity not confirmed).
#      Not applied at Trace/Negative tier (qualifier signal too weak to
#      be meaningful there), and skipped entirely (not a failure) if the
#      qualifier columns don't exist in the data at all.
#   3. Tier-conditional bracket gate (both pre & post required; a missing
#      side counts as FAIL, not WARN -- both are REQUIRED):
#        Quantifiable        -> 6ng bracket only. FAIL -> hard FAIL.
#        Trace / Negative    -> 0.2ng bracket only. PASS -> clean;
#                                WARN -> caveat; FAIL/missing -> hard FAIL
#                                (no override, for samples or NCs).
#   4. Per-analyte result: Quantifiable -> "Quantified"; Trace ->
#      "Trace" (ALWAYS a caveat, even with clean brackets); Negative ->
#      "Negative".
#
# Row-level combination (first-failure-only, fixed order IS -> PETN ->
# RDX): the first FAIL-class result found is reported and everything
# else is irrelevant; otherwise PASS* if any caveat exists anywhere
# (IS-LOW, blank-trace, bracket-WARN, or a Trace-tier result), else PASS.
compute_injection_acceptance <- function(df) {

  n <- nrow(df)
  accepted  <- character(n)
  outcome   <- character(n)
  petn_tier <- rep(NA_character_, n)
  rdx_tier  <- rep(NA_character_, n)

  classify_snr_tier <- function(snr_flag_value, snr_quant_threshold = IA_DEFAULT_SNR_QUANT) {
    if (is.na(snr_flag_value)) return("Negative")
    if (snr_flag_value == "Quantifiable") return("Quantifiable")
    if (snr_flag_value == "Below_LOQ") return("Trace")
    return("Negative")  # "Below_LOD"
  }

  evaluate_analyte <- function(i, prefix) {
    # Returns list(result = "Quantified"/"Trace"/"Negative"/NULL,
    #               fail_reason = NULL or string, caveat = logical, tier = tier)
    snr_flag_col <- paste0(prefix, "_snr_flag")
    snr_flag <- if (snr_flag_col %in% names(df)) df[[snr_flag_col]][i] else NA_character_
    peak_present <- !is.na(snr_flag)

    caveat <- FALSE

    if (peak_present) {
      bracket_col <- paste0(prefix, "_blank_bracket")
      blank_bracket <- if (bracket_col %in% names(df)) df[[bracket_col]][i] else NA_character_
      if (!is.na(blank_bracket) && blank_bracket == "Contaminated") {
        return(list(result = NULL,
                     fail_reason = paste0(toupper(prefix), " peak may be due to carryover (bracketing blank contaminated)"),
                     caveat = FALSE, tier = NA_character_))
      }
      if (!is.na(blank_bracket) && blank_bracket == "Trace") {
        caveat <- TRUE
      }
    }

    tier <- classify_snr_tier(snr_flag)

    if (tier == "Quantifiable") {
      # --- Confirmatory ion check (added Aug 2026, see IA_QUALIFIER_IONS
      # above) -- only gates entry into "Quantified", i.e. only checked
      # at the Quantifiable tier. Trace/Negative tiers are deliberately
      # exempt: qualifier signal is often too weak to reliably measure
      # at those low levels, so a missing qualifier there would not
      # actually indicate a misidentification, just insufficient signal.
      qualifiers_required <- IA_QUALIFIER_IONS[[prefix]]
      if (!is.null(qualifiers_required)) {
        qual_flag_cols <- paste0(prefix, "_", qualifiers_required, "_snr_flag")
        if (all(qual_flag_cols %in% names(df))) {
          qual_detected <- sapply(qual_flag_cols, function(col) IA_is_qualifier_detected(df[[col]][i]))
          if (!all(qual_detected)) {
            missing_ions <- qualifiers_required[!qual_detected]
            return(list(result = NULL,
                         fail_reason = paste0(toupper(prefix), " confirmatory ion not detected (",
                                               paste(missing_ions, collapse = ", "),
                                               ") -- identity not confirmed"),
                         caveat = FALSE, tier = tier))
          }
        }
      }

      pre  <- df[[paste0(prefix, "_qc_6ng_pre")]][i]
      post <- df[[paste0(prefix, "_qc_6ng_post")]][i]
      bracket_ok <- !is.na(pre) && !is.na(post) && pre == "PASS" && post == "PASS"
      if (!bracket_ok) {
        return(list(result = NULL,
                     fail_reason = paste0(toupper(prefix), " 6ng QC bracket FAILED or unavailable"),
                     caveat = FALSE, tier = tier))
      }
      return(list(result = "Quantified", fail_reason = NULL, caveat = caveat, tier = tier))
    }

    # Trace or Negative -> 0.2ng bracket only
    pre  <- df[[paste0(prefix, "_qc_02ng_pre")]][i]
    post <- df[[paste0(prefix, "_qc_02ng_post")]][i]

    if (is.na(pre) || is.na(post) || pre == "FAIL" || post == "FAIL") {
      return(list(result = NULL,
                   fail_reason = paste0(toupper(prefix), " 0.2ng QC bracket FAILED or unavailable"),
                   caveat = FALSE, tier = tier))
    }
    if (pre == "WARN" || post == "WARN") {
      caveat <- TRUE
    }

    result_label <- if (tier == "Trace") "Trace" else "Negative"
    if (tier == "Trace") caveat <- TRUE  # Trace ALWAYS carries a caveat

    return(list(result = result_label, fail_reason = NULL, caveat = caveat, tier = tier))
  }

  for (i in seq_len(n)) {

    # --- IS injection-validity check: short-circuits immediately ---
    # Both NOT_DETECTED and LOW are hard FAILs (Fixed 2026-08-11 -- see
    # CONTEXT.md "Low IS PA Now Hard-Fails Again" session). A LOW internal
    # standard peak area means the entire injection is suspect (partial
    # injection, syringe issue, etc.) -- for any analyte quantified via an
    # IS ratio (this study's RDX, and PETN when use_15nrdx_for_petn=TRUE),
    # dividing by an abnormally small denominator inflates the reported
    # concentration/recovery arbitrarily, up to physically impossible
    # values (e.g. Lab6 P1 Steel Repeat 4: rdx_is_pa_flag="LOW" produced
    # RDX recovery = 164.8%, impossible for a real sample). This was
    # previously a hard FAIL (see the July 31, 2026 "IS Peak Area
    # Injection-Validity Check" session) but was inadvertently downgraded
    # to a caveat-only (PASS*) during the August 10, 2026 flowchart-based
    # consolidation into this shared file -- restored here.
    is_pa_flag <- if ("rdx_is_pa_flag" %in% names(df)) df$rdx_is_pa_flag[i] else NA_character_

    if (!is.na(is_pa_flag) && is_pa_flag == "NOT_DETECTED") {
      accepted[i] <- "FAIL: Injection failure (IS not detected)"
      outcome[i]  <- "Reanalyse"
      next
    }

    if (!is.na(is_pa_flag) && is_pa_flag == "LOW") {
      is_pa_val <- if ("rdx_is_pa" %in% names(df)) df$rdx_is_pa[i] else NA_real_
      accepted[i] <- if (!is.na(is_pa_val)) {
        sprintf("FAIL: IS abnormally low (PA=%s)", round(is_pa_val))
      } else {
        "FAIL: IS abnormally low"
      }
      outcome[i] <- "Reanalyse"
      next
    }

    petn_eval <- evaluate_analyte(i, "petn")
    rdx_eval  <- evaluate_analyte(i, "rdx")

    petn_tier[i] <- petn_eval$tier
    rdx_tier[i]  <- rdx_eval$tier

    # --- Fixed order, first failure wins: IS (handled above) -> PETN -> RDX ---
    if (!is.null(petn_eval$fail_reason)) {
      accepted[i] <- paste0("FAIL: ", petn_eval$fail_reason)
      outcome[i]  <- "Reanalyse"
      next
    }
    if (!is.null(rdx_eval$fail_reason)) {
      accepted[i] <- paste0("FAIL: ", rdx_eval$fail_reason)
      outcome[i]  <- "Reanalyse"
      next
    }

    any_caveat <- isTRUE(petn_eval$caveat) || isTRUE(rdx_eval$caveat)
    accepted[i] <- if (any_caveat) "PASS*" else "PASS"
    outcome[i]  <- "Complete"
  }

  df$analysis_accepted <- accepted
  df$Outcome           <- outcome
  df$petn_result_tier  <- petn_tier   # "Quantifiable" / "Trace" / "Negative" / NA (IS failure)
  df$rdx_result_tier   <- rdx_tier

  return(df)
}

# Derives the Negative-Control-specific "Stage 2" contamination-result
# label from the same per-analyte tiers computed above -- NCs use the
# identical acceptance logic (compute_injection_acceptance); this just
# translates the resulting tiers into NC reporting vocabulary. Forced to
# "Not evaluated (analysis failed)" whenever the row-level accepted
# value is a FAIL, since an untrustworthy run cannot tell us the truth
# about contamination either way.
derive_nc_result <- function(petn_tier, rdx_tier, accepted) {
  n <- length(accepted)
  result <- character(n)

  tier_word <- function(tier) {
    if (is.na(tier)) return(NA_character_)
    if (tier == "Quantifiable") return("contaminated")
    if (tier == "Trace") return("trace detected")
    return("clean")
  }

  for (i in seq_len(n)) {
    if (grepl("^FAIL:", accepted[i])) {
      result[i] <- "Not evaluated (analysis failed)"
      next
    }

    petn_word <- tier_word(petn_tier[i])
    rdx_word  <- tier_word(rdx_tier[i])

    petn_positive <- !is.na(petn_word) && petn_word != "clean"
    rdx_positive  <- !is.na(rdx_word) && rdx_word != "clean"

    if (!petn_positive && !rdx_positive) {
      result[i] <- "Negative (clean)"
    } else if (petn_positive && rdx_positive && petn_word == "contaminated" && rdx_word == "contaminated") {
      result[i] <- "Positive: PETN + RDX contaminated"
    } else if (petn_positive && rdx_positive && petn_word == "trace detected" && rdx_word == "trace detected") {
      result[i] <- "Positive: PETN + RDX trace detected"
    } else if (petn_positive && rdx_positive) {
      # Mixed severity (e.g. PETN contaminated, RDX trace) -- report both
      # at their own severity rather than collapsing to one word.
      result[i] <- paste0("Positive: PETN ", petn_word, ", RDX ", rdx_word)
    } else if (petn_positive) {
      result[i] <- paste0("Positive: PETN ", petn_word)
    } else {
      result[i] <- paste0("Positive: RDX ", rdx_word)
    }
  }

  return(result)
}
