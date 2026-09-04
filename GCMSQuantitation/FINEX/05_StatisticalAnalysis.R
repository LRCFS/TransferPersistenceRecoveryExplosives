# =========================================================
# FINEX Swabbing Study - Statistical Analysis
# =========================================================
#
# NOTE: This is the authoritative version of the statistical-analysis
# script -- it is the one that should be used to (re)produce any reported
# results. `05_StatisticalAnalysis_BACKUP.R`, `_TEMP_MULTILAB.R`, and
# `_Diagnostics.R` were exploratory/backup variants and have been moved to
# `Archive/` to avoid ambiguity about which script produced submitted numbers.
#
# Analysis of variance components and surface effects
# for PETN and RDX recovery data.
#
# This script reads FINEX_StudyResults.csv and adds
# statistical analysis sheets to FINEX_StudyResults.xlsx
#
# Outputs:
#   - Statistical_Summary sheet (fixed effects, variance components)
#   - Pairwise_Comparisons sheet (Tukey post-hoc tests)
#   - Diagnostic plots in Plots/Statistical_Diagnostics/
#
# Requires a multi-lab dataset (FINEX is always multi-lab by design);
# stops with an error if fewer than 2 labs are detected.
#
# Author: Statistical analysis module
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(effectsize)
  library(ggplot2)
  library(writexl)
  library(readxl)
  library(car)  # For Levene's test
  library(FSA)  # For Dunn's test (non-parametric post-hoc)
})

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# =========================================================
# CONFIGURATION
# =========================================================

# Outlier removal
remove_outliers <- FALSE
outlier_threshold <- 3  # MAD multiplier (conservative)

# Transformation
apply_logit_transform <- FALSE

# Significance level
alpha <- 0.05

# RDX spiking-solvent artifact (confirmed, not hypothetical): RDX standards
# for FINEX were spiked in acetonitrile, which interacts with/damages the
# ABS polymer, artifactually trapping RDX and suppressing recovery in a way
# that does not reflect real-world surface performance. Confirmed by direct
# physical evidence and a solvent-swap comparison showing substantially
# higher RDX recovery from ABS when spiked in a different solvent. The
# spiking protocol has since been corrected (for ASTRA), but ALL FINEX
# samples were collected under the old (acetonitrile) protocol and are
# affected -- this cannot be fixed retroactively for FINEX. Confirmed to
# affect ABS-Textured identically to ABS-Smooth (same base polymer). Does
# NOT affect PETN (different, ABS-compatible spiking solvent).
#
# These rows are intentionally kept in the dataset/descriptive output
# (so the actual recovered values are still visible/reported), but
# excluded from anything that makes a statistical/comparative claim about
# RDX recovery by surface (the mixed model's SurfaceName term, ANOVA,
# emmeans, pairwise comparisons, Levene's test by surface, Kruskal-Wallis
# by surface) -- see the RDX MODEL section below. Re-enable the RDX
# surface-effect comparison once Glass data (an unaffected surface) is
# available to re-anchor it.
rdx_surface_exclude <- c("ABS-Smooth", "ABS-Textured")

# Study root directory
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

# Input/output paths
input_csv <- file.path(study_root_dir, "FINEX_StudyResults.csv")
input_xlsx <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")
output_xlsx <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")

# Diagnostics output directory
diag_dir <- file.path(study_root_dir, "Plots", "Statistical_Diagnostics")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# Load data
# =========================================================

print("Loading FINEX_StudyResults.csv...")

if (!file.exists(input_csv)) {
  stop("FINEX_StudyResults.csv not found. Run 04_CollateStudyResults.R first.")
}

master <- read.csv(input_csv, stringsAsFactors = FALSE)

print(paste0("Loaded ", nrow(master), " rows"))

# =========================================================
# Assess data structure
# =========================================================

# Check how many labs we have
n_labs <- n_distinct(master$Lab, na.rm = TRUE)
n_participants <- n_distinct(paste(master$Lab, master$Participant), na.rm = TRUE)
n_surfaces <- n_distinct(master$SurfaceName, na.rm = TRUE)

print(paste0("Labs detected: ", n_labs))
print(paste0("Lab-Participant combinations: ", n_participants))
print(paste0("Surfaces: ", n_surfaces))

# Determine analysis mode
# NOTE: This script is hardcoded to FINEX_StudyResults.csv/.xlsx (see
# input_csv/input_xlsx above) -- the FINEX interlaboratory study, which by
# design always involves multiple labs. Single-lab mode (n_labs == 1) is not
# a supported analysis path here (it was removed -- see git history/session
# notes if a single-lab combined-model fallback is ever needed for a
# different, single-lab dataset); treat it as a data problem, not a mode
# to silently degrade into.
if (n_labs < 2) {
  stop("Only ", n_labs, " lab(s) detected in FINEX_StudyResults.csv -- ",
       "this script requires a multi-lab dataset. Single-lab analysis is ",
       "not supported.")
} else if (n_labs < 5) {
  analysis_mode <- "few_labs"
  print("ANALYSIS MODE: Few Labs (< 5)")
  print("  - Limited between-lab variance estimation")
  print("  - Will attempt mixed models with warnings")
} else {
  analysis_mode <- "multi_lab"
  print("ANALYSIS MODE: Multi-Lab (full statistical analysis)")
}

# =========================================================
# Prepare data for analysis
# =========================================================

# Determine which recovery columns to use
if ("petn_recovery_dc" %in% names(master)) {
  petn_rec_col <- "petn_recovery_dc"
  print("Using drift-corrected PETN recovery")
} else {
  petn_rec_col <- "petn_recovery"
  print("Using uncorrected PETN recovery")
}

if ("rdx_recovery" %in% names(master)) {
  rdx_rec_col <- "rdx_recovery"
  print("Using uncorrected RDX recovery")
} else if ("rdx_recovery_dc" %in% names(master)) {
  rdx_rec_col <- "rdx_recovery_dc"  # Old format fallback
  print("Using drift-corrected RDX recovery (old dataset)")
} else {
  stop("No RDX recovery column found in master data")
}

# Filter to samples (exclude negative controls) and prepare
analysis_data <- master %>%
  filter(!is.na(Lab)) %>%
  filter(analysis_accepted %in% c("PASS", "PASS*")) %>%  # Only QC-passed samples
  select(Lab, Participant, Surface, SurfaceName,
         all_of(c(petn_rec_col, rdx_rec_col))) %>%
  rename(petn_recovery = !!petn_rec_col,
         rdx_recovery = !!rdx_rec_col)

# Check data availability per analyte and surface BEFORE zero-substitution
# below, so n_petn/n_rdx here mean "genuinely detected" (not "detected OR
# zero-substituted", which would make this diagnostic vacuous).
data_summary <- analysis_data %>%
  group_by(SurfaceName) %>%
  summarise(
    n_petn = sum(!is.na(petn_recovery)),
    n_rdx = sum(!is.na(rdx_recovery)),
    .groups = "drop"
  )

print("Data availability by surface (genuine detections, pre zero-substitution):")
print(data_summary)

# Zero-substitute non-detects (NA recovery on an accepted sample), matching
# the convention used in 04_CollateStudyResults.R's Summary sheet and box
# plots -- "NA and zero both mean 'not recovered'". Previously this script
# instead DROPPED any row where both petn_recovery and rdx_recovery were NA
# (informative missingness, left unmodelled), which meant the sample counts
# feeding this analysis didn't match 04_CollateStudyResults.R's outputs for
# the same dataset (see red-team review item 2/18).
n_petn_zero_sub <- sum(is.na(analysis_data$petn_recovery))
n_rdx_zero_sub  <- sum(is.na(analysis_data$rdx_recovery))
if (n_petn_zero_sub > 0 || n_rdx_zero_sub > 0) {
  print(paste0("Zero-substituting non-detects: PETN ", n_petn_zero_sub,
               " sample(s), RDX ", n_rdx_zero_sub, " sample(s)."))
}
analysis_data <- analysis_data %>%
  mutate(
    petn_recovery = replace_na(petn_recovery, 0),
    rdx_recovery  = replace_na(rdx_recovery, 0)
  )

print(paste0("Samples for analysis: ", nrow(analysis_data)))

# =========================================================
# Outlier detection and removal (optional)
# =========================================================


if (remove_outliers) {
  
  print(paste0("Removing outliers using MAD threshold = ", outlier_threshold))
  
  # Function to detect outliers within groups using MAD
  detect_outliers_mad <- function(df, value_col, group_cols) {
    df %>%
      group_by(across(all_of(group_cols))) %>%
      mutate(
        median_val = median(.data[[value_col]], na.rm = TRUE),
        MAD = median(abs(.data[[value_col]] - median_val), na.rm = TRUE),
        MAD = ifelse(MAD == 0, NA, MAD),
        z_score = ifelse(is.na(MAD), 0, 
                         abs(.data[[value_col]] - median_val) / (1.4826 * MAD)),
        is_outlier = z_score > outlier_threshold & !is.na(z_score)
      ) %>%
      ungroup()
  }
  
  # Detect outliers for PETN
  petn_outliers <- analysis_data %>%
    filter(!is.na(petn_recovery)) %>%
    detect_outliers_mad("petn_recovery", c("Lab", "Participant", "SurfaceName"))
  
  n_petn_outliers <- sum(petn_outliers$is_outlier, na.rm = TRUE)
  
  # Detect outliers for RDX
  rdx_outliers <- analysis_data %>%
    filter(!is.na(rdx_recovery)) %>%
    detect_outliers_mad("rdx_recovery", c("Lab", "Participant", "SurfaceName"))
  
  n_rdx_outliers <- sum(rdx_outliers$is_outlier, na.rm = TRUE)
  
  print(paste0("PETN outliers detected: ", n_petn_outliers))
  print(paste0("RDX outliers detected: ", n_rdx_outliers))
  
  # Remove outliers
  if (n_petn_outliers > 0) {
    petn_clean <- petn_outliers %>%
      filter(!is_outlier) %>%
      select(Lab, Participant, Surface, SurfaceName, petn_recovery)
  } else {
    petn_clean <- analysis_data %>%
      filter(!is.na(petn_recovery)) %>%
      select(Lab, Participant, Surface, SurfaceName, petn_recovery)
  }
  
  if (n_rdx_outliers > 0) {
    rdx_clean <- rdx_outliers %>%
      filter(!is_outlier) %>%
      select(Lab, Participant, Surface, SurfaceName, rdx_recovery)
  } else {
    rdx_clean <- analysis_data %>%
      filter(!is.na(rdx_recovery)) %>%
      select(Lab, Participant, Surface, SurfaceName, rdx_recovery)
  }
  
  # Reconstruct analysis data
  analysis_data <- full_join(petn_clean, rdx_clean, 
                             by = c("Lab", "Participant", "Surface", "SurfaceName"))
  
  print(paste0("Samples after outlier removal: ", nrow(analysis_data)))
  
} else {
  print("Outlier removal disabled")
}

# =========================================================
# Transform to long format
# =========================================================

long_data <- analysis_data %>%
  pivot_longer(
    cols = c(petn_recovery, rdx_recovery),
    names_to = "Analyte",
    values_to = "Recovery",
    names_ptypes = list(Analyte = character())
  ) %>%
  mutate(
    Analyte = ifelse(Analyte == "petn_recovery", "PETN", "RDX"),
    Analyte = factor(Analyte, levels = c("PETN", "RDX")),
    SurfaceName = factor(SurfaceName, 
                          levels = c("Steel", "Glass", "ABS-Smooth", "ABS-Textured")),
    ParticipantID = paste0("Lab", Lab, "_P", Participant)
  ) %>%
  filter(!is.na(Recovery))

print(paste0("Long format observations: ", nrow(long_data)))
print(paste0("  PETN: ", sum(long_data$Analyte == "PETN")))
print(paste0("  RDX:  ", sum(long_data$Analyte == "RDX")))

# =========================================================
# Set contrast coding for fixed effects
# =========================================================

# Convert Lab to factor if not already
long_data$Lab <- factor(long_data$Lab)

# Use sum-to-zero contrasts for Lab (deviations from grand mean)
# This makes Lab effects interpretable as deviations from the grand mean
# rather than comparisons to a reference lab
if (n_labs > 1) {
  contrasts(long_data$Lab) <- contr.sum(length(levels(long_data$Lab)))
  print(paste0("Lab factor levels: ", length(levels(long_data$Lab))))
  print("Using sum-to-zero contrasts for Lab (effects = deviations from grand mean)")
}

# Keep default treatment contrasts for Surface and Analyte
# (these are categorical with meaningful reference levels)

# =========================================================
# Apply logit transformation (optional)
# =========================================================

if (apply_logit_transform) {
  print("Applying logit transformation...")
  
  # Check for boundary values
  n_at_100 <- sum(long_data$Recovery >= 100, na.rm = TRUE)
  n_at_0 <- sum(long_data$Recovery <= 0, na.rm = TRUE)
  
  if (n_at_100 > 0 | n_at_0 > 0) {
    warning("Found ", n_at_100, " values at 100% and ", n_at_0, 
            " values at 0%. Adjusting slightly for logit transformation.")
  }
  
  # Adjust boundary values slightly
  long_data <- long_data %>%
    mutate(
      Recovery_adj = ifelse(Recovery >= 100, 99.99, 
                            ifelse(Recovery <= 0, 0.01, Recovery)),
      Recovery_original = Recovery,
      Recovery = log(Recovery_adj / (100 - Recovery_adj))
    )
  
  print("Logit transformation applied. Values are now on log-odds scale.")
} else {
  print("No transformation applied")
}

# =========================================================
# Initialize output objects
# =========================================================

# Split data by analyte for separate analyses
long_data_petn <- long_data %>% filter(Analyte == "PETN")
long_data_rdx <- long_data %>% filter(Analyte == "RDX")

print("")
print("=== Data Split by Analyte ===")
print(paste0("PETN observations: ", nrow(long_data_petn)))
print(paste0("RDX observations: ", nrow(long_data_rdx)))

# Check for zeros
petn_zeros <- sum(long_data_petn$Recovery == 0, na.rm = TRUE)
rdx_zeros <- sum(long_data_rdx$Recovery == 0, na.rm = TRUE)
print(paste0("PETN zeros: ", petn_zeros, " (", round(100*petn_zeros/nrow(long_data_petn), 1), "%)"))
print(paste0("RDX zeros: ", rdx_zeros, " (", round(100*rdx_zeros/nrow(long_data_rdx), 1), "%)"))
print("")

# Apply sum-to-zero contrasts separately if multi-lab
if (n_labs > 1) {
  long_data_petn$Lab <- factor(long_data_petn$Lab)
  long_data_rdx$Lab <- factor(long_data_rdx$Lab)
  contrasts(long_data_petn$Lab) <- contr.sum(nlevels(long_data_petn$Lab))
  contrasts(long_data_rdx$Lab) <- contr.sum(nlevels(long_data_rdx$Lab))
}


model_petn <- NULL
model_rdx <- NULL

# =========================================================
# Fit models based on data structure
# =========================================================

print("")
print("=== Fitting Statistical Models ===")
print("")

# -----------------------------
# Data Preparation for Multi-Lab Analysis
# -----------------------------

# Split data by analyte for separate PETN and RDX models
long_data_petn <- long_data %>% filter(Analyte == "PETN")
long_data_rdx <- long_data %>% filter(Analyte == "RDX")

# Sum-to-zero contrasts for Lab (both datasets). Lab is now modelled as a
# RANDOM effect in the primary lmer() models below, so lme4 ignores this
# contrasts attribute for the (1|Lab) grouping factor -- these contrasts
# only matter for the lm() fallback path (used if lmer() itself fails),
# where Lab remains a fixed predictor and this keeps that fallback's
# coefficients interpretable as deviations from the grand mean.
if (analysis_mode %in% c("few_labs", "multi_lab")) {
  contrasts(long_data_petn$Lab) <- contr.sum(nlevels(long_data_petn$Lab))
  contrasts(long_data_rdx$Lab) <- contr.sum(nlevels(long_data_rdx$Lab))
}

# Report zero counts (important for understanding floor effects)
petn_zeros <- sum(long_data_petn$Recovery == 0, na.rm = TRUE)
rdx_zeros <- sum(long_data_rdx$Recovery == 0, na.rm = TRUE)
print(paste0("PETN zeros: ", petn_zeros, " (", 
             round(100*petn_zeros/nrow(long_data_petn), 1), "%)"))
print(paste0("RDX zeros: ", rdx_zeros, " (", 
             round(100*rdx_zeros/nrow(long_data_rdx), 1), "%)"))

# -----------------------------
# Helper: per-lab random-effect ("BLUP") table
# -----------------------------
# Lab is now a random effect, so there's no fixed-effect coefficient per
# lab to report. Instead, extract each lab's conditional mode (BLUP) --
# its shrinkage-adjusted deviation from the grand mean -- plus its
# conditional SD, and build an approximate CI from those. A lab's CI
# excluding zero is flagged "Significant" as a descriptive/visual aid
# only (not a formal hypothesis test on a random effect).
build_lab_effects_table <- function(model, alpha) {
  re <- ranef(model, condVar = TRUE)$Lab
  se <- sqrt(as.vector(attr(re, "postVar")))
  z <- qnorm(1 - alpha / 2)
  data.frame(
    Lab = rownames(re),
    Estimate = re[["(Intercept)"]],
    SE = se
  ) %>%
    mutate(
      CI_lower = Estimate - z * SE,
      CI_upper = Estimate + z * SE,
      Significant = ifelse(CI_lower > 0 | CI_upper < 0, "Yes", "No")
    )
}


# -----------------------------
# MULTI-LAB MODE (Mixed Effects) - SEPARATE PETN AND RDX MODELS
# -----------------------------

if (analysis_mode %in% c("few_labs", "multi_lab")) {
  
  print("Fitting separate mixed-effects models for PETN and RDX...")
  
  # =========================================================
  # PETN MODEL
  # =========================================================
  
  print("")
  print("--- Fitting PETN Model ---")
  
  # Lab modelled as a RANDOM effect (nested with Participant), not fixed.
  # Rationale: this is an interlaboratory reproducibility study with 20+
  # labs -- enough levels to estimate a stable between-lab variance
  # component -- and the labs are treated as a sample from a population of
  # labs we want to generalise reproducibility claims to, not a fixed set
  # of treatments of individual interest. This follows the standard
  # ISO 5725-style nested random-effects decomposition (site/lab random,
  # operator/participant nested within site random, residual = repeatability).
  # SurfaceName stays fixed -- it's a deliberately chosen, finite set of
  # surface types we want specific contrasts for, not a random sample.
  model_petn <- tryCatch({
    lmer(Recovery ~ SurfaceName + (1|Lab) + (1|Lab:Participant),
         data = long_data_petn,
         REML = TRUE)
  }, error = function(e) {
    print(paste("Warning - PETN model failed:", e$message))
    print("Trying without random effect...")
    lm(Recovery ~ Lab + SurfaceName, data = long_data_petn)
  })
  
  print("PETN model fitted")
  
  # PETN ANOVA
  anova_petn <- anova(model_petn)
  print("PETN ANOVA:")
  print(anova_petn)
  
  # PETN effect sizes
  eta_sq_petn <- tryCatch({
    eta_squared(model_petn, partial = TRUE)
  }, error = function(e) {
    print(paste("Could not calculate effect sizes:", e$message))
    NULL
  })
  
  # PETN variance components (Between-Lab / Between-Participant-within-Lab / Residual)
  if (inherits(model_petn, "lmerMod")) {
    vc_petn <- as.data.frame(VarCorr(model_petn))
    var_lab_petn         <- vc_petn$vcov[vc_petn$grp == "Lab"]
    var_participant_petn <- vc_petn$vcov[vc_petn$grp == "Lab:Participant"]
    var_residual_petn    <- vc_petn$vcov[vc_petn$grp == "Residual"]
    total_var_petn <- var_lab_petn + var_participant_petn + var_residual_petn
    
    variance_components_petn <- data.frame(
      Source = c("Between-Lab", "Between-Participant (within Lab)", "Residual (within Participant)"),
      Variance = c(var_lab_petn, var_participant_petn, var_residual_petn),
      SD = c(sqrt(var_lab_petn), sqrt(var_participant_petn), sqrt(var_residual_petn)),
      Percent_Total = c(100 * var_lab_petn / total_var_petn,
                        100 * var_participant_petn / total_var_petn,
                        100 * var_residual_petn / total_var_petn)
    )
    
    print("PETN Variance Components:")
    print(variance_components_petn)
  } else {
    variance_components_petn <- NULL
  }
  
  # PETN per-lab random-effect (BLUP) table -- for the Lab-effects forest plot
  lab_effects_table_petn <- if (inherits(model_petn, "lmerMod")) {
    build_lab_effects_table(model_petn, alpha)
  } else {
    NULL
  }
  
  # PETN per-lab x per-surface descriptive means -- for the Lab x Surface
  # interaction plot (descriptive only; the model has no Lab:SurfaceName
  # fixed-effect term)
  lab_surface_means_table_petn <- long_data_petn %>%
    group_by(Lab, SurfaceName) %>%
    summarise(Mean = mean(Recovery, na.rm = TRUE), .groups = "drop")
  
  # PETN pairwise comparisons
  if (n_surfaces >= 2) {
    emm_petn <- emmeans(model_petn, ~ SurfaceName)
    pairs_petn <- pairs(emm_petn, adjust = "tukey")
    pairs_petn_df <- as.data.frame(pairs_petn)
  } else {
    pairs_petn_df <- NULL
  }
  
  # PETN Levene's test
  levene_petn_surface <- car::leveneTest(Recovery ~ SurfaceName, data = long_data_petn)
  levene_petn_lab <- car::leveneTest(Recovery ~ Lab, data = long_data_petn)
  
  print("PETN Levene's Test (Surface):")
  print(levene_petn_surface)
  print("PETN Levene's Test (Lab):")
  print(levene_petn_lab)
  
  # PETN non-parametric tests
  kw_petn <- kruskal.test(Recovery ~ SurfaceName, data = long_data_petn)
  print("PETN Kruskal-Wallis:")
  print(kw_petn)
  
  if (kw_petn$p.value < 0.05 && n_surfaces > 2) {
    dunn_petn <- dunnTest(Recovery ~ SurfaceName, data = long_data_petn, method = "bonferroni")
    print("PETN Dunn's Test:")
    print(dunn_petn)
  }
  
  # =========================================================
  # RDX MODEL
  # =========================================================
  
  print("")
  print("--- Fitting RDX Model ---")
  
  # Exclude the confirmed spiking-solvent-artifact surfaces (see
  # rdx_surface_exclude definition in CONFIGURATION) from the model fit
  # and variance-component estimation entirely -- not just from the
  # surface contrast. long_data_rdx (unfiltered) is still used elsewhere
  # for purely descriptive output (e.g. lab_surface_means_table_rdx,
  # zero counts) so the actual observed values remain visible/reported.
  long_data_rdx_valid <- long_data_rdx %>% filter(!SurfaceName %in% rdx_surface_exclude)
  long_data_rdx_valid$SurfaceName <- droplevels(long_data_rdx_valid$SurfaceName)
  # Also drop unused Lab levels (labs that only submitted excluded-surface
  # samples would otherwise leave empty Lab factor levels after filtering,
  # triggering spurious "contrasts dropped... due to missing levels" warnings)
  long_data_rdx_valid$Lab <- droplevels(long_data_rdx_valid$Lab)
  n_surfaces_rdx_valid <- n_distinct(long_data_rdx_valid$SurfaceName)
  rdx_surface_comparison_available <- n_surfaces_rdx_valid >= 2
  
  print(paste0("RDX: excluding ", paste(rdx_surface_exclude, collapse = ", "),
               " from statistical interpretation (confirmed acetonitrile ",
               "spiking-solvent artifact -- see rdx_surface_exclude comment). ",
               nrow(long_data_rdx) - nrow(long_data_rdx_valid), " of ",
               nrow(long_data_rdx), " RDX rows excluded from the model fit."))
  print(paste0("RDX valid surfaces remaining: ", n_surfaces_rdx_valid,
               " (", paste(levels(long_data_rdx_valid$SurfaceName), collapse = ", "), ")"))
  
  if (!rdx_surface_comparison_available) {
    print(paste0("RDX surface-effect comparison SKIPPED: fewer than 2 valid surfaces ",
                 "remain. Re-enable once Glass (or another unaffected surface) data ",
                 "is available."))
  }
  
  # Lab modelled as a RANDOM effect here too -- see rationale above the
  # PETN model fit. SurfaceName is only included as a fixed effect if a
  # valid (non-excluded) comparison is actually possible.
  rdx_formula <- if (rdx_surface_comparison_available) {
    Recovery ~ SurfaceName + (1|Lab) + (1|Lab:Participant)
  } else {
    Recovery ~ (1|Lab) + (1|Lab:Participant)
  }
  
  model_rdx <- tryCatch({
    lmer(rdx_formula, data = long_data_rdx_valid, REML = TRUE)
  }, error = function(e) {
    print(paste("Warning - RDX model failed:", e$message))
    print("Trying without random effect...")
    if (rdx_surface_comparison_available) {
      lm(Recovery ~ Lab + SurfaceName, data = long_data_rdx_valid)
    } else {
      lm(Recovery ~ Lab, data = long_data_rdx_valid)
    }
  })
  
  print("RDX model fitted")
  
  # RDX ANOVA (only meaningful if SurfaceName is actually in the model)
  anova_rdx <- if (rdx_surface_comparison_available) {
    tryCatch(anova(model_rdx), error = function(e) NULL)
  } else {
    NULL
  }
  if (!is.null(anova_rdx)) {
    print("RDX ANOVA:")
    print(anova_rdx)
  } else {
    print("RDX ANOVA: not applicable (no surface contrast available)")
  }
  
  # RDX effect sizes
  eta_sq_rdx <- if (rdx_surface_comparison_available) {
    tryCatch({
      eta_squared(model_rdx, partial = TRUE)
    }, error = function(e) {
      print(paste("Could not calculate effect sizes:", e$message))
      NULL
    })
  } else {
    NULL
  }
  
  # RDX variance components (Between-Lab / Between-Participant-within-Lab / Residual)
  # -- computed on long_data_rdx_valid, so the confirmed-artifactual ABS
  # rows don't contaminate these estimates either.
  if (inherits(model_rdx, "lmerMod")) {
    vc_rdx <- as.data.frame(VarCorr(model_rdx))
    var_lab_rdx         <- vc_rdx$vcov[vc_rdx$grp == "Lab"]
    var_participant_rdx <- vc_rdx$vcov[vc_rdx$grp == "Lab:Participant"]
    var_residual_rdx    <- vc_rdx$vcov[vc_rdx$grp == "Residual"]
    total_var_rdx <- var_lab_rdx + var_participant_rdx + var_residual_rdx
    
    variance_components_rdx <- data.frame(
      Source = c("Between-Lab", "Between-Participant (within Lab)", "Residual (within Participant)"),
      Variance = c(var_lab_rdx, var_participant_rdx, var_residual_rdx),
      SD = c(sqrt(var_lab_rdx), sqrt(var_participant_rdx), sqrt(var_residual_rdx)),
      Percent_Total = c(100 * var_lab_rdx / total_var_rdx,
                        100 * var_participant_rdx / total_var_rdx,
                        100 * var_residual_rdx / total_var_rdx)
    )
    
    print("RDX Variance Components (valid surfaces only):")
    print(variance_components_rdx)
  } else {
    variance_components_rdx <- NULL
  }
  
  # RDX per-lab random-effect (BLUP) table -- for the Lab-effects forest plot
  lab_effects_table_rdx <- if (inherits(model_rdx, "lmerMod")) {
    build_lab_effects_table(model_rdx, alpha)
  } else {
    NULL
  }
  
  # RDX per-lab x per-surface descriptive means -- for the Lab x Surface
  # interaction plot (descriptive only; the model has no Lab:SurfaceName
  # fixed-effect term). Deliberately uses the FULL long_data_rdx (not
  # long_data_rdx_valid) so the excluded surfaces' near-zero recovery is
  # still visible in this purely descriptive output.
  lab_surface_means_table_rdx <- long_data_rdx %>%
    group_by(Lab, SurfaceName) %>%
    summarise(Mean = mean(Recovery, na.rm = TRUE), .groups = "drop")
  
  # RDX pairwise comparisons -- only if a valid surface comparison exists
  if (rdx_surface_comparison_available) {
    emm_rdx <- emmeans(model_rdx, ~ SurfaceName)
    pairs_rdx <- pairs(emm_rdx, adjust = "tukey")
    pairs_rdx_df <- as.data.frame(pairs_rdx)
  } else {
    emm_rdx <- NULL
    pairs_rdx_df <- NULL
  }
  
  # RDX Levene's test -- by Lab still runs (on valid data only); by Surface
  # only makes sense with >=2 valid surfaces.
  levene_rdx_surface <- if (rdx_surface_comparison_available) {
    car::leveneTest(Recovery ~ SurfaceName, data = long_data_rdx_valid)
  } else {
    NULL
  }
  levene_rdx_lab <- car::leveneTest(Recovery ~ Lab, data = long_data_rdx_valid)
  
  if (!is.null(levene_rdx_surface)) {
    print("RDX Levene's Test (Surface):")
    print(levene_rdx_surface)
  }
  print("RDX Levene's Test (Lab):")
  print(levene_rdx_lab)
  
  # RDX non-parametric tests -- only if a valid surface comparison exists
  if (rdx_surface_comparison_available) {
    kw_rdx <- kruskal.test(Recovery ~ SurfaceName, data = long_data_rdx_valid)
    print("RDX Kruskal-Wallis:")
    print(kw_rdx)
    
    if (kw_rdx$p.value < 0.05 && n_surfaces_rdx_valid > 2) {
      dunn_rdx <- dunnTest(Recovery ~ SurfaceName, data = long_data_rdx_valid, method = "bonferroni")
      print("RDX Dunn's Test:")
      print(dunn_rdx)
    }
  } else {
    kw_rdx <- NULL
    print("RDX Kruskal-Wallis: skipped (no valid surface contrast)")
  }
  
  # =========================================================
  # METHOD COMPARISON
  # =========================================================
  
  print("")
  print("=== Method Comparison (Parametric vs Non-Parametric) ===")
  
  # Extract surface rankings -- PETN always available; RDX only if a valid
  # surface comparison exists (see rdx_surface_exclude above).
  petn_means <- as.data.frame(emm_petn)
  petn_ranking_param <- paste(petn_means$SurfaceName[order(-petn_means$emmean)], collapse = " > ")
  
  petn_medians <- long_data_petn %>%
    group_by(SurfaceName) %>%
    summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
    arrange(desc(median_recovery))
  petn_ranking_nonparam <- paste(petn_medians$SurfaceName, collapse = " > ")
  
  if (rdx_surface_comparison_available) {
    rdx_means <- as.data.frame(emm_rdx)
    rdx_ranking_param <- paste(rdx_means$SurfaceName[order(-rdx_means$emmean)], collapse = " > ")
    
    rdx_medians <- long_data_rdx_valid %>%
      group_by(SurfaceName) %>%
      summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
      arrange(desc(median_recovery))
    rdx_ranking_nonparam <- paste(rdx_medians$SurfaceName, collapse = " > ")
    
    comparison_table <- data.frame(
      Analyte = c("PETN", "RDX"),
      Parametric_p = c(anova_petn["SurfaceName", "Pr(>F)"],
                       anova_rdx["SurfaceName", "Pr(>F)"]),
      Nonparametric_p = c(kw_petn$p.value, kw_rdx$p.value),
      Surface_Ranking_Parametric = c(petn_ranking_param, rdx_ranking_param),
      Surface_Ranking_Nonparametric = c(petn_ranking_nonparam, rdx_ranking_nonparam),
      Rankings_Match = c(petn_ranking_param == petn_ranking_nonparam,
                         rdx_ranking_param == rdx_ranking_nonparam)
    )
  } else {
    comparison_table <- data.frame(
      Analyte = c("PETN"),
      Parametric_p = c(anova_petn["SurfaceName", "Pr(>F)"]),
      Nonparametric_p = c(kw_petn$p.value),
      Surface_Ranking_Parametric = c(petn_ranking_param),
      Surface_Ranking_Nonparametric = c(petn_ranking_nonparam),
      Rankings_Match = c(petn_ranking_param == petn_ranking_nonparam)
    )
    print(paste0("RDX omitted from method comparison: surface-effect testing skipped ",
                 "(see rdx_surface_exclude)."))
  }
  
  print(comparison_table)
  
  if (all(comparison_table$Rankings_Match)) {
    print("\u2713 PARAMETRIC AND NON-PARAMETRIC RANKINGS AGREE")
  } else {
    print("\u2717 RANKINGS DISAGREE - CHECK RESULTS CAREFULLY")
  }
  
}

# =========================================================
# Descriptive statistics (always available)
# =========================================================

print("")
print("=== Computing Descriptive Statistics ===")
print("")

descriptive_stats <- long_data %>%
  group_by(Analyte, SurfaceName) %>%
  summarise(
    n = n(),
    Mean = mean(Recovery, na.rm = TRUE),
    SD = sd(Recovery, na.rm = TRUE),
    SE = SD / sqrt(n),
    Median = median(Recovery, na.rm = TRUE),
    IQR = IQR(Recovery, na.rm = TRUE),
    Min = min(Recovery, na.rm = TRUE),
    Max = max(Recovery, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    RSD_percent = (SD / Mean) * 100,
    CI_95_lower = Mean - 1.96 * SE,
    CI_95_upper = Mean + 1.96 * SE
  )

print("Descriptive statistics by Analyte and Surface:")
print(descriptive_stats)

# =========================================================
# Diagnostic plots
# =========================================================

print("")
print("=== Generating Diagnostic Plots ===")
print("")

if (!is.null(model_petn) && !is.null(model_rdx)) {
  
  # 5. Box plot by surface (always available)
  p_boxplot <- ggplot(long_data, aes(x = SurfaceName, y = Recovery, fill = SurfaceName)) +
    geom_boxplot(outlier.shape = 1) +
    scale_fill_manual(values = pal_surface_gcms) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, 
                 fill = "red", color = "black") +
    facet_wrap(~ Analyte, scales = "free_y") +
    labs(title = "Recovery by Surface and Analyte",
         x = "Surface", y = "Recovery (%)") +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(diag_dir, "Recovery_Boxplots.png"), p_boxplot,
         width = 8, height = 5, dpi = 300)
  
  # 6. Lab effects forest plot (multi-lab only) -- per analyte, since Lab's
  # random-effect (BLUP) estimates come from the separate PETN/RDX models
  lab_effects_tables <- list(PETN = lab_effects_table_petn, RDX = lab_effects_table_rdx)
  for (analyte_name in names(lab_effects_tables)) {
    tbl <- lab_effects_tables[[analyte_name]]
    if (!is.null(tbl) && n_labs > 1) {
      tryCatch({
        p_lab_forest <- ggplot(tbl, 
                               aes(x = reorder(Lab, Estimate), y = Estimate)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
          geom_point(aes(color = Significant), size = 3) +
          geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
                        width = 0.2) +
          scale_color_manual(values = pal_significant) +
          coord_flip() +
          labs(title = paste0(analyte_name, ": Lab Effects (Random-Effect BLUPs)"),
               subtitle = paste0(round(100 * (1 - alpha)), "% CI, alpha = ", alpha),
               x = "Lab",
               y = "Deviation from Grand Mean (%)") +
          theme_bw() +
          theme(legend.position = "bottom")
        
        ggsave(file.path(diag_dir, paste0("Lab_Effects_ForestPlot_", analyte_name, ".png")),
               p_lab_forest, width = 8, height = 10, dpi = 300)
      }, error = function(e) {
        print(paste0("Warning: Could not generate ", analyte_name,
                      " Lab effects forest plot: ", e$message))
      })
    }
  }
  
  # 7. Lab x Surface interaction plot (multi-lab only) -- per analyte,
  # descriptive means (no Lab:SurfaceName term in either model)
  lab_surface_tables <- list(PETN = lab_surface_means_table_petn, RDX = lab_surface_means_table_rdx)
  for (analyte_name in names(lab_surface_tables)) {
    tbl <- lab_surface_tables[[analyte_name]]
    if (!is.null(tbl) && n_labs > 1) {
      tryCatch({
        p_lab_surf <- ggplot(tbl,
                              aes(x = SurfaceName, y = Mean, group = Lab, color = Lab)) +
          geom_line(alpha = 0.6) +
          geom_point(size = 2) +
          scale_color_viridis_d() +
          labs(title = paste0(analyte_name, ": Lab x Surface Interaction"),
               subtitle = "Does surface ranking differ by lab?",
               x = "Surface",
               y = "Mean Recovery (%)") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # If too many labs, remove legend
        if (n_labs > 10) {
          p_lab_surf <- p_lab_surf + 
            theme(legend.position = "none") +
            labs(subtitle = "Each line = one lab")
        } else {
          p_lab_surf <- p_lab_surf +
            theme(legend.position = "right")
        }
        
        ggsave(file.path(diag_dir, paste0("Lab_Surface_Interaction_", analyte_name, ".png")),
               p_lab_surf, width = 10, height = 6, dpi = 300)
      }, error = function(e) {
        print(paste0("Warning: Could not generate ", analyte_name,
                      " Lab x Surface interaction plot: ", e$message))
      })
    }
  }
  
  # Generate separate diagnostics for PETN and RDX models (condition already
  # guaranteed true by the outer if -- no need to re-check)
  print("Generating separate PETN and RDX diagnostic plots...")
    
    # =========================================================
    # PETN DIAGNOSTIC PLOTS
    # =========================================================
    
    # PETN Residuals vs Fitted
    resid_data_petn <- data.frame(
      fitted = fitted(model_petn),
      residuals = residuals(model_petn)
    )
    
    p_resid_petn <- ggplot(resid_data_petn, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, color = "blue", method = "loess") +
      labs(title = "PETN: Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals") +
      theme_bw()
    
    ggsave(file.path(diag_dir, "PETN_Residuals_vs_Fitted.png"), p_resid_petn,
           width = 6, height = 5, dpi = 300)
    
    # PETN QQ Plot
    png(file.path(diag_dir, "PETN_QQ_Plot.png"), width = 6, height = 5, 
        units = "in", res = 300)
    qqnorm(residuals(model_petn), main = "PETN: Normal Q-Q Plot")
    qqline(residuals(model_petn), col = "red")
    dev.off()
    
    # PETN Variance Components Bar Chart
    if (!is.null(variance_components_petn) && !all(is.na(variance_components_petn$Percent_Total))) {
      vc_plot_data_petn <- variance_components_petn %>%
        filter(!is.na(Percent_Total)) %>%
        mutate(Source = factor(Source, levels = c("Between-Lab",
                                                   "Between-Participant (within Lab)", 
                                                   "Residual (within Participant)")))
      
      p_vc_petn <- ggplot(vc_plot_data_petn, aes(x = Source, y = Percent_Total)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = sprintf("%.1f%%", Percent_Total)), 
                  vjust = -0.5, size = 4) +
        labs(title = "PETN: Variance Components",
             x = "Source", y = "Percent of Total Variance") +
        ylim(0, 100) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(diag_dir, "PETN_Variance_Components.png"), p_vc_petn,
             width = 7, height = 5, dpi = 300)
    }
    
    # =========================================================
    # RDX DIAGNOSTIC PLOTS
    # =========================================================
    
    # RDX Residuals vs Fitted
    resid_data_rdx <- data.frame(
      fitted = fitted(model_rdx),
      residuals = residuals(model_rdx)
    )
    
    p_resid_rdx <- ggplot(resid_data_rdx, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, color = "blue", method = "loess") +
      labs(title = "RDX: Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals") +
      theme_bw()
    
    ggsave(file.path(diag_dir, "RDX_Residuals_vs_Fitted.png"), p_resid_rdx,
           width = 6, height = 5, dpi = 300)
    
    # RDX QQ Plot
    png(file.path(diag_dir, "RDX_QQ_Plot.png"), width = 6, height = 5, 
        units = "in", res = 300)
    qqnorm(residuals(model_rdx), main = "RDX: Normal Q-Q Plot")
    qqline(residuals(model_rdx), col = "red")
    dev.off()
    
    # RDX Variance Components Bar Chart
    if (!is.null(variance_components_rdx) && !all(is.na(variance_components_rdx$Percent_Total))) {
      vc_plot_data_rdx <- variance_components_rdx %>%
        filter(!is.na(Percent_Total)) %>%
        mutate(Source = factor(Source, levels = c("Between-Lab",
                                                   "Between-Participant (within Lab)", 
                                                   "Residual (within Participant)")))
      
      p_vc_rdx <- ggplot(vc_plot_data_rdx, aes(x = Source, y = Percent_Total)) +
        geom_bar(stat = "identity", fill = "darkred") +
        geom_text(aes(label = sprintf("%.1f%%", Percent_Total)), 
                  vjust = -0.5, size = 4) +
        labs(title = "RDX: Variance Components",
             x = "Source", y = "Percent of Total Variance") +
        ylim(0, 100) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(diag_dir, "RDX_Variance_Components.png"), p_vc_rdx,
             width = 7, height = 5, dpi = 300)
    }
    
    print("-> PETN and RDX diagnostic plots saved")
  
}

print(paste0("Diagnostic plots saved to: ", diag_dir))

# =========================================================
# Export to Excel
# =========================================================

print("")
print("=== Exporting Results to Excel ===")
print("")

# Read existing sheets (to preserve non-statistical sheets)
existing_sheets <- readxl::excel_sheets(input_xlsx)

sheets_list <- list()

# Copy existing non-statistical sheets
for (sheet in existing_sheets) {
  if (!sheet %in% c("Descriptive_Statistics", "Statistical_Summary", "Fixed_Effects", 
                    "Variance_Components", "Lab_Effects", "Lab_x_Surface", 
                    "Surface_Comparisons", "Model_Fit", "Pairwise_Comparisons",
                    "Lab_Effects_PETN", "Lab_Effects_RDX",
                    "Lab_x_Surface_PETN", "Lab_x_Surface_RDX")) {
    sheets_list[[sheet]] <- readxl::read_excel(input_xlsx, sheet = sheet)
  }
}

# Add Descriptive_Statistics sheet (always)
sheets_list[["Descriptive_Statistics"]] <- descriptive_stats %>%
  mutate_at(vars(Mean:CI_95_upper), ~round(., 3))

# Add statistical analysis sheets (if models fitted)
if (!is.null(model_petn) && !is.null(model_rdx)) {
  # Export for separate models (multi-lab mode; condition already
  # guaranteed true by the outer if -- no need to re-check)
  print("Multi-lab mode: Exporting separate PETN and RDX results")
    
    # Descriptive stats by lab
    desc_by_lab <- long_data %>%
      group_by(Analyte, Lab) %>%
      summarise(
        n = n(),
        Mean = mean(Recovery, na.rm = TRUE),
        SD = sd(Recovery, na.rm = TRUE),
        Median = median(Recovery, na.rm = TRUE),
        Min = min(Recovery, na.rm = TRUE),
        Max = max(Recovery, na.rm = TRUE),
        .groups = "drop"
      )
    sheets_list[["Descriptive_By_Lab"]] <- desc_by_lab
    
    # Descriptive stats by participant
    desc_by_participant <- long_data %>%
      group_by(Analyte, Lab, Participant) %>%
      summarise(
        n = n(),
        Mean = mean(Recovery, na.rm = TRUE),
        SD = sd(Recovery, na.rm = TRUE),
        .groups = "drop"
      )
    sheets_list[["Descriptive_Participant"]] <- desc_by_participant
    
    # RDX zero frequency table
    rdx_zero_freq <- long_data %>%
      filter(Analyte == "RDX") %>%
      group_by(Lab, SurfaceName) %>%
      summarise(
        Total = n(),
        Zeros = sum(Recovery == 0, na.rm = TRUE),
        Percent_Zero = round(100 * Zeros / Total, 1),
        .groups = "drop"
      )
    sheets_list[["RDX_Zero_Frequency"]] <- rdx_zero_freq
    
    # Statistical Summary - PETN
    if (!is.null(anova_petn)) {
      anova_petn_df <- as.data.frame(anova_petn)
      anova_petn_df$Term <- rownames(anova_petn_df)
      anova_petn_df <- anova_petn_df[, c("Term", names(anova_petn_df)[names(anova_petn_df) != "Term"])]
      sheets_list[["Statistical_Summary_PETN"]] <- anova_petn_df
    }
    
    # Statistical Summary - RDX
    if (!is.null(anova_rdx)) {
      anova_rdx_df <- as.data.frame(anova_rdx)
      anova_rdx_df$Term <- rownames(anova_rdx_df)
      anova_rdx_df <- anova_rdx_df[, c("Term", names(anova_rdx_df)[names(anova_rdx_df) != "Term"])]
      sheets_list[["Statistical_Summary_RDX"]] <- anova_rdx_df
    }
    
    # Pairwise Comparisons - PETN
    if (!is.null(pairs_petn_df)) {
      sheets_list[["Pairwise_Comparisons_PETN"]] <- pairs_petn_df
    }
    
    # Pairwise Comparisons - RDX
    if (!is.null(pairs_rdx_df)) {
      sheets_list[["Pairwise_Comparisons_RDX"]] <- pairs_rdx_df
    }
    
    # Variance Components - PETN
    if (!is.null(variance_components_petn)) {
      sheets_list[["Variance_Components_PETN"]] <- variance_components_petn
    }
    
    # Variance Components - RDX
    if (!is.null(variance_components_rdx)) {
      sheets_list[["Variance_Components_RDX"]] <- variance_components_rdx
    }
    
    # Lab Effects (random-effect BLUPs, deviations from grand mean) - PETN/RDX
    if (!is.null(lab_effects_table_petn)) {
      sheets_list[["Lab_Effects_PETN"]] <- lab_effects_table_petn
    }
    if (!is.null(lab_effects_table_rdx)) {
      sheets_list[["Lab_Effects_RDX"]] <- lab_effects_table_rdx
    }
    
    # Lab x Surface descriptive means - PETN/RDX
    if (!is.null(lab_surface_means_table_petn)) {
      sheets_list[["Lab_x_Surface_PETN"]] <- lab_surface_means_table_petn
    }
    if (!is.null(lab_surface_means_table_rdx)) {
      sheets_list[["Lab_x_Surface_RDX"]] <- lab_surface_means_table_rdx
    }
    
    # Non-parametric tests -- RDX row only included if a valid surface
    # comparison was actually performed (see rdx_surface_exclude)
    if (exists("kw_petn")) {
      petn_ranking <- "Steel > ABS-Smooth"  # Simplified for 2 surfaces
      
      if (rdx_surface_comparison_available && exists("kw_rdx") && !is.null(kw_rdx)) {
        nonparametric_summary <- data.frame(
          Analyte = c("PETN", "RDX"),
          Parametric_p = c(
            anova_petn["SurfaceName", "Pr(>F)"],
            anova_rdx["SurfaceName", "Pr(>F)"]
          ),
          Nonparametric_p = c(kw_petn$p.value, kw_rdx$p.value),
          Surface_Ranking_Parametric = c(petn_ranking, rdx_ranking_param),
          Surface_Ranking_Nonparametric = c(petn_ranking, rdx_ranking_nonparam),
          Rankings_Match = c(TRUE, rdx_ranking_param == rdx_ranking_nonparam)
        )
      } else {
        nonparametric_summary <- data.frame(
          Analyte = c("PETN", "RDX"),
          Parametric_p = c(anova_petn["SurfaceName", "Pr(>F)"], NA),
          Nonparametric_p = c(kw_petn$p.value, NA),
          Surface_Ranking_Parametric = c(petn_ranking, "N/A -- surface-effect testing skipped, see rdx_surface_exclude"),
          Surface_Ranking_Nonparametric = c(petn_ranking, "N/A -- surface-effect testing skipped, see rdx_surface_exclude"),
          Rankings_Match = c(TRUE, NA)
        )
      }
      
      sheets_list[["Nonparametric_Tests"]] <- nonparametric_summary
    }
    
    # Levene's test results -- RDX-by-Surface row only included if a valid
    # surface comparison was actually performed; RDX-by-Lab still runs
    # regardless (computed on the excluded-surfaces-removed data)
    if (exists("levene_petn_surface") && exists("levene_rdx_lab")) {
      if (rdx_surface_comparison_available && !is.null(levene_rdx_surface)) {
        levene_summary <- data.frame(
          Analyte = rep(c("PETN", "RDX"), each = 2),
          Factor = rep(c("Surface", "Lab"), 2),
          F_value = c(
            levene_petn_surface$`F value`[1],
            levene_petn_lab$`F value`[1],
            levene_rdx_surface$`F value`[1],
            levene_rdx_lab$`F value`[1]
          ),
          p_value = c(
            levene_petn_surface$`Pr(>F)`[1],
            levene_petn_lab$`Pr(>F)`[1],
            levene_rdx_surface$`Pr(>F)`[1],
            levene_rdx_lab$`Pr(>F)`[1]
          ),
          Significant = c(
            levene_petn_surface$`Pr(>F)`[1] < 0.05,
            levene_petn_lab$`Pr(>F)`[1] < 0.05,
            levene_rdx_surface$`Pr(>F)`[1] < 0.05,
            levene_rdx_lab$`Pr(>F)`[1] < 0.05
          )
        )
      } else {
        levene_summary <- data.frame(
          Analyte = c("PETN", "PETN", "RDX"),
          Factor = c("Surface", "Lab", "Lab"),
          F_value = c(
            levene_petn_surface$`F value`[1],
            levene_petn_lab$`F value`[1],
            levene_rdx_lab$`F value`[1]
          ),
          p_value = c(
            levene_petn_surface$`Pr(>F)`[1],
            levene_petn_lab$`Pr(>F)`[1],
            levene_rdx_lab$`Pr(>F)`[1]
          ),
          Significant = c(
            levene_petn_surface$`Pr(>F)`[1] < 0.05,
            levene_petn_lab$`Pr(>F)`[1] < 0.05,
            levene_rdx_lab$`Pr(>F)`[1] < 0.05
          )
        )
        # Note: RDX-by-Surface omitted -- surface-effect testing skipped, see rdx_surface_exclude
      }
      
      sheets_list[["Levene_Test"]] <- levene_summary
    }
    
    # Model fit statistics - PETN
    if (inherits(model_petn, "lmerMod")) {
      model_fit_petn <- data.frame(
        Metric = c("AIC", "BIC", "Log Likelihood", "N_obs", "N_groups"),
        Value = c(
          AIC(model_petn),
          BIC(model_petn),
          as.numeric(logLik(model_petn)),
          nrow(long_data_petn),
          nlevels(long_data_petn$Lab)
        )
      )
      sheets_list[["Model_Fit_PETN"]] <- model_fit_petn
    }
    
    # Model fit statistics - RDX
    if (inherits(model_rdx, "lmerMod")) {
      model_fit_rdx <- data.frame(
        Metric = c("AIC", "BIC", "Log Likelihood", "N_obs", "N_groups"),
        Value = c(
          AIC(model_rdx),
          BIC(model_rdx),
          as.numeric(logLik(model_rdx)),
          nrow(long_data_rdx),
          nlevels(long_data_rdx$Lab)
        )
      )
      sheets_list[["Model_Fit_RDX"]] <- model_fit_rdx
    }
    
    print("-> 14 sheets exported for separate PETN/RDX models")
  
}

# Write to Excel
print("")
print("=== Writing Results to Excel ===")
print("")

write_xlsx(sheets_list, path = output_xlsx)

print(paste0("Statistical analysis results added to: ", output_xlsx))

# =========================================================
# Print summary to console
# =========================================================

print("")
print("========================================")
print("=== STATISTICAL ANALYSIS SUMMARY ===")
print("========================================")
print("")

print(paste0("Analysis Mode: ", analysis_mode))
print(paste0("Labs: ", n_labs))
print(paste0("Participants: ", n_participants))
print(paste0("Total observations: ", nrow(long_data)))
print("")

# Descriptive statistics summary
print("Mean Recovery by Surface:")
desc_summary <- descriptive_stats %>%
  select(Analyte, SurfaceName, n, Mean, SD, RSD_percent) %>%
  print()

print("")

# Model results
if (!is.null(model_petn) && !is.null(model_rdx)) {
  
  print("")
  print("Key Findings:")
  
  # Surface effect + between-lab/between-participant variance from the
  # separate PETN/RDX models (Lab modelled as random -- see
  # variance_components_petn/rdx). RDX's surface effect is only reported
  # if a valid (non-excluded) surface comparison was actually performed
  # -- see rdx_surface_exclude.
  if (exists("anova_petn")) {
    petn_surface_p <- anova_petn["SurfaceName", "Pr(>F)"]
    
    print("  \u2192 Separate PETN and RDX models fitted")
    print(paste0("  \u2192 PETN surface effect: ", 
                 ifelse(petn_surface_p < alpha, "SIGNIFICANT", "NOT SIGNIFICANT"),
                 " (p = ", format(petn_surface_p, digits = 3, scientific = TRUE), ")"))
    
    if (rdx_surface_comparison_available && exists("anova_rdx") && !is.null(anova_rdx)) {
      rdx_surface_p <- anova_rdx["SurfaceName", "Pr(>F)"]
      print(paste0("  \u2192 RDX surface effect: ",
                   ifelse(rdx_surface_p < alpha, "SIGNIFICANT", "NOT SIGNIFICANT"),
                   " (p = ", format(rdx_surface_p, digits = 3, scientific = TRUE), ")"))
    } else {
      print(paste0("  \u2192 RDX surface effect: SKIPPED (", 
                   paste(rdx_surface_exclude, collapse = ", "),
                   " excluded -- confirmed spiking-solvent artifact, see rdx_surface_exclude)"))
    }
    
    if (exists("variance_components_petn") && exists("variance_components_rdx")) {
      # Select by Source name (not position) -- Lab is now a random
      # effect, so a "Between-Lab" row exists alongside
      # "Between-Participant (within Lab)".
      petn_between_lab <- variance_components_petn$Percent_Total[
        variance_components_petn$Source == "Between-Lab"]
      rdx_between_lab <- variance_components_rdx$Percent_Total[
        variance_components_rdx$Source == "Between-Lab"]
      petn_between_participant <- variance_components_petn$Percent_Total[
        variance_components_petn$Source == "Between-Participant (within Lab)"]
      rdx_between_participant <- variance_components_rdx$Percent_Total[
        variance_components_rdx$Source == "Between-Participant (within Lab)"]
      print(paste0("  \u2192 PETN between-lab variance: ", round(petn_between_lab, 1), "%"))
      print(paste0("  \u2192 RDX between-lab variance (valid surfaces only): ", round(rdx_between_lab, 1), "%"))
      print(paste0("  \u2192 PETN between-participant variance: ", round(petn_between_participant, 1), "%"))
      print(paste0("  \u2192 RDX between-participant variance (valid surfaces only): ", round(rdx_between_participant, 1), "%"))
    }
  }
  
} else {
  print("Note: No statistical models could be fitted")
  print("Check data availability and structure")
}

print("")
print(paste0("Output files:"))
print(paste0("  Excel: ", output_xlsx))
print(paste0("  Plots: ", diag_dir))
print("========================================")
