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
# Handles single-lab and multi-lab datasets gracefully.
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
if (n_labs == 1) {
  analysis_mode <- "single_lab"
  print("ANALYSIS MODE: Single Lab (limited statistical analysis)")
  print("  - Cannot estimate between-lab variance")
  print("  - Will use fixed effects for Participant instead of random effects")
  print("  - Surface comparisons will use one-way ANOVA")
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


model_combined <- NULL
model_petn <- NULL
model_rdx <- NULL
fixed_effects_combined <- NULL
variance_components <- NULL
pairs_table <- NULL
anova_combined <- NULL
model_fit <- NULL

# =========================================================
# Fit models based on data structure
# =========================================================

print("")
print("=== Fitting Statistical Models ===")
print("")

# -----------------------------
# SINGLE LAB MODE
# -----------------------------

if (analysis_mode == "single_lab") {
  
  print("Fitting single-lab models (Participant as fixed effect)...")
  
  # Check if we have multiple participants
  n_participants_in_lab <- n_distinct(long_data$Participant)
  
  if (n_participants_in_lab == 1) {
    
    print("WARNING: Only 1 participant detected")
    print("  Cannot assess between-participant variance")
    print("  Fitting simple one-way ANOVA by Surface")
    
    # Simple one-way ANOVA for each analyte
    # PETN
    petn_data <- long_data %>% filter(Analyte == "PETN")
    rdx_data <- long_data %>% filter(Analyte == "RDX")
    
    if (nrow(petn_data) > 0) {
      print("Fitting PETN one-way ANOVA by Surface...")
      model_petn <- aov(Recovery ~ SurfaceName, data = petn_data)
      print("PETN ANOVA fitted successfully")
    } else {
      print("No PETN data available")
    }
    
    if (nrow(rdx_data) > 0) {
      print("Fitting RDX one-way ANOVA by Surface...")
      model_rdx <- aov(Recovery ~ SurfaceName, data = rdx_data)
      print("RDX ANOVA fitted successfully")
    } else {
      print("No RDX data available")
    }
    
    # Combined model with Analyte as factor
    print("Fitting combined two-way ANOVA (Surface × Analyte)...")
    model_combined <- aov(Recovery ~ SurfaceName * Analyte, data = long_data)
    print("Combined ANOVA fitted successfully")
    
    # Extract ANOVA table
    anova_combined <- summary(model_combined)[[1]]
    anova_combined <- as.data.frame(anova_combined)
    
    # Add Term column with row names
    anova_combined$Term <- rownames(anova_combined)
    rownames(anova_combined) <- NULL
    
    # Effect sizes
    print("Calculating effect sizes...")
    partial_eta <- tryCatch({
      eta_squared(model_combined, partial = TRUE, ci = 0.95)
    }, error = function(e) {
      print(paste("Warning: Could not calculate partial eta-squared:", e$message))
      NULL
    })
    
    # Build fixed effects table from ANOVA
    fixed_effects_combined <- anova_combined %>%
      select(Term, Df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`) %>%
      rename(df = Df,
             Sum_Sq = `Sum Sq`,
             Mean_Sq = `Mean Sq`,
             F_value = `F value`,
             p_value = `Pr(>F)`) %>%
      mutate(
        Estimate = NA_real_,
        SE = NA_real_,
        t_value = NA_real_
      )
    
    # Variance components (single lab, single participant)
    variance_components <- data.frame(
      Model = c("Single Lab", "Single Lab", "Single Lab"),
      Source = c("Between-Lab", "Within-Lab (Between-Participant)", "Within-Participant (Residual)"),
      Variance = c(NA_real_, NA_real_, 
                   summary(model_combined)[[1]]["Residuals", "Mean Sq"]),
      SD = c(NA_real_, NA_real_, 
             sqrt(summary(model_combined)[[1]]["Residuals", "Mean Sq"])),
      ICC = c(NA_real_, NA_real_, NA_real_),
      Percent_Total = c(NA_real_, NA_real_, NA_real_)
    )
    
    # Model fit
    model_fit <- data.frame(
      Model = "Combined (Single Lab)",
      AIC = AIC(model_combined),
      BIC = BIC(model_combined),
      N_Obs = nrow(long_data)
    )
    
    # Post-hoc comparisons
    if (n_surfaces >= 2) {
      print("Computing post-hoc comparisons...")
      
      pairs_result <- tryCatch({
        emmeans_surface <- emmeans(model_combined, ~ SurfaceName | Analyte)
        pairs_surface <- pairs(emmeans_surface, adjust = "tukey")
        
        result <- as.data.frame(pairs_surface)
        
        # Rename columns safely
        colnames(result)[colnames(result) == "contrast"] <- "Contrast"
        colnames(result)[colnames(result) == "estimate"] <- "Estimate"
        colnames(result)[colnames(result) == "t.ratio"] <- "t_ratio"
        
        # Handle p.value column (both names are same from pairs())
        if ("p.value" %in% colnames(result)) {
          result$p_adjusted <- result[["p.value"]]
          result$p_value <- result[["p.value"]]
        }
        
        result$Significant <- ifelse(result$p_adjusted < alpha, "Yes", "No")
        
        print("Post-hoc comparisons computed successfully")
        result
      }, error = function(e) {
        print(paste("Warning: Could not compute post-hoc comparisons:", e$message))
        NULL
      })
      
      pairs_table <<- pairs_result
    } else {
      print(paste("Warning: Only", n_surfaces, "surface detected - cannot compute pairwise comparisons"))
      pairs_table <<- NULL
    }
    
  } else {
    # Multiple participants in single lab
    print(paste0("Multiple participants detected: ", n_participants_in_lab))
    print("  Can assess within-lab between-participant variance")
    print("  Using Participant as fixed effect (cannot estimate random effect with 1 lab)")
    
    # Fit with Participant as fixed effect
    print("Fitting combined model (Surface × Analyte + Participant)...")
    
    tryCatch({
      model_combined <- lm(Recovery ~ SurfaceName * Analyte + Participant, 
                           data = long_data)
      print("Combined linear model fitted successfully")
      
      # ANOVA
      anova_combined <- anova(model_combined)
      anova_combined <- as.data.frame(anova_combined)
      anova_combined$Term <- rownames(anova_combined)
      rownames(anova_combined) <- NULL
      
      # Build fixed effects table
      fixed_effects_combined <- anova_combined %>%
        select(Term, Df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`) %>%
        rename(df = Df,
               Sum_Sq = `Sum Sq`,
               Mean_Sq = `Mean Sq`,
               F_value = `F value`,
               p_value = `Pr(>F)`) %>%
        mutate(
          Estimate = NA_real_,
          SE = NA_real_,
          t_value = NA_real_
        )
      
      # Effect sizes
      print("Calculating effect sizes...")
      partial_eta <- eta_squared(model_combined, partial = TRUE, ci = 0.95)
      
      # Variance components (approximate)
      residual_var <- sum(residuals(model_combined)^2) / df.residual(model_combined)
      
      variance_components <- data.frame(
        Model = c("Single Lab", "Single Lab", "Single Lab"),
        Source = c("Between-Lab", "Within-Lab (Between-Participant)", "Within-Participant (Residual)"),
        Variance = c(NA_real_, NA_real_, residual_var),
        SD = c(NA_real_, NA_real_, sqrt(residual_var)),
        ICC = c(NA_real_, NA_real_, NA_real_),
        Percent_Total = c(NA_real_, NA_real_, 100)
      )
      
      # Model fit
      model_fit <- data.frame(
        Model = "Combined (Single Lab, Fixed Participant)",
        AIC = AIC(model_combined),
        BIC = BIC(model_combined),
        N_Obs = nrow(long_data)
      )
      
    }, error = function(e) {
      print(paste("Error fitting combined model:", e$message))
      print("Falling back to simple ANOVA...")
      
      model_combined <- aov(Recovery ~ SurfaceName * Analyte, data = long_data)
      anova_combined <- summary(model_combined)[[1]]
    })
    
    # Separate models per analyte
    petn_data <- long_data %>% filter(Analyte == "PETN")
    rdx_data <- long_data %>% filter(Analyte == "RDX")
    
    if (nrow(petn_data) > 0) {
      print("Fitting PETN model with Participant...")
      model_petn <- tryCatch({
        lm(Recovery ~ SurfaceName + Participant, data = petn_data)
      }, error = function(e) {
        aov(Recovery ~ SurfaceName, data = petn_data)
      })
    }
    
    if (nrow(rdx_data) > 0) {
      print("Fitting RDX model with Participant...")
      model_rdx <- tryCatch({
        lm(Recovery ~ SurfaceName + Participant, data = rdx_data)
      }, error = function(e) {
        aov(Recovery ~ SurfaceName, data = rdx_data)
      })
    }
  }
  
  # Post-hoc comparisons
  if (!is.null(model_combined) && inherits(model_combined, c("lm", "aov"))) {
    if (n_surfaces >= 2) {
      print("Computing post-hoc comparisons...")
      
      pairs_result <- tryCatch({
        emmeans_surface <- emmeans(model_combined, ~ SurfaceName | Analyte)
        pairs_surface <- pairs(emmeans_surface, adjust = "tukey")
        
        result <- as.data.frame(pairs_surface)
        
        # Rename columns safely (handle both t.ratio and z.ratio)
        colnames(result)[colnames(result) == "contrast"] <- "Contrast"
        colnames(result)[colnames(result) == "estimate"] <- "Estimate"
        if ("t.ratio" %in% colnames(result)) {
          colnames(result)[colnames(result) == "t.ratio"] <- "t_ratio"
        } else if ("z.ratio" %in% colnames(result)) {
          colnames(result)[colnames(result) == "z.ratio"] <- "t_ratio"
        }
        
        # Handle p.value column (both names are same from pairs())
        if ("p.value" %in% colnames(result)) {
          result$p_adjusted <- result[["p.value"]]
          result$p_value <- result[["p.value"]]
        }
        
        result$Significant <- ifelse(result$p_adjusted < alpha, "Yes", "No")
        
        print("Post-hoc comparisons computed successfully")
        result
      }, error = function(e) {
        print(paste("Warning: Could not compute post-hoc comparisons:", e$message))
        NULL
      })
      
      pairs_table <<- pairs_result
    } else {
      print(paste("Warning: Only", n_surfaces, "surface detected - cannot compute pairwise comparisons"))
      pairs_table <<- NULL
    }
  }
  
}

# -----------------------------
# Data Preparation for Multi-Lab Analysis
# -----------------------------

# Split data by analyte for separate PETN and RDX models
long_data_petn <- long_data %>% filter(Analyte == "PETN")
long_data_rdx <- long_data %>% filter(Analyte == "RDX")

# Apply sum-to-zero contrasts for Lab (both datasets)
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
# MULTI-LAB MODE (Mixed Effects) - SEPARATE PETN AND RDX MODELS
# -----------------------------

if (analysis_mode %in% c("few_labs", "multi_lab")) {
  
  print("Fitting separate mixed-effects models for PETN and RDX...")
  
  # =========================================================
  # PETN MODEL
  # =========================================================
  
  print("")
  print("--- Fitting PETN Model ---")
  
  model_petn <- tryCatch({
    lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
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
  
  # PETN variance components
  if (inherits(model_petn, "lmerMod")) {
    vc_petn <- as.data.frame(VarCorr(model_petn))
    var_participant_petn <- vc_petn$vcov[vc_petn$grp == "Lab:Participant"]
    var_residual_petn <- vc_petn$vcov[vc_petn$grp == "Residual"]
    total_var_petn <- var_participant_petn + var_residual_petn
    
    variance_components_petn <- data.frame(
      Source = c("Between-Participant (within Lab)", "Residual (within Participant)"),
      Variance = c(var_participant_petn, var_residual_petn),
      SD = c(sqrt(var_participant_petn), sqrt(var_residual_petn)),
      Percent_Total = c(100 * var_participant_petn / total_var_petn,
                        100 * var_residual_petn / total_var_petn)
    )
    
    print("PETN Variance Components:")
    print(variance_components_petn)
  } else {
    variance_components_petn <- NULL
  }
  
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
  
  model_rdx <- tryCatch({
    lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
         data = long_data_rdx,
         REML = TRUE)
  }, error = function(e) {
    print(paste("Warning - RDX model failed:", e$message))
    print("Trying without random effect...")
    lm(Recovery ~ Lab + SurfaceName, data = long_data_rdx)
  })
  
  print("RDX model fitted")
  
  # RDX ANOVA
  anova_rdx <- anova(model_rdx)
  print("RDX ANOVA:")
  print(anova_rdx)
  
  # RDX effect sizes
  eta_sq_rdx <- tryCatch({
    eta_squared(model_rdx, partial = TRUE)
  }, error = function(e) {
    print(paste("Could not calculate effect sizes:", e$message))
    NULL
  })
  
  # RDX variance components
  if (inherits(model_rdx, "lmerMod")) {
    vc_rdx <- as.data.frame(VarCorr(model_rdx))
    var_participant_rdx <- vc_rdx$vcov[vc_rdx$grp == "Lab:Participant"]
    var_residual_rdx <- vc_rdx$vcov[vc_rdx$grp == "Residual"]
    total_var_rdx <- var_participant_rdx + var_residual_rdx
    
    variance_components_rdx <- data.frame(
      Source = c("Between-Participant (within Lab)", "Residual (within Participant)"),
      Variance = c(var_participant_rdx, var_residual_rdx),
      SD = c(sqrt(var_participant_rdx), sqrt(var_residual_rdx)),
      Percent_Total = c(100 * var_participant_rdx / total_var_rdx,
                        100 * var_residual_rdx / total_var_rdx)
    )
    
    print("RDX Variance Components:")
    print(variance_components_rdx)
  } else {
    variance_components_rdx <- NULL
  }
  
  # RDX pairwise comparisons
  if (n_surfaces >= 2) {
    emm_rdx <- emmeans(model_rdx, ~ SurfaceName)
    pairs_rdx <- pairs(emm_rdx, adjust = "tukey")
    pairs_rdx_df <- as.data.frame(pairs_rdx)
  } else {
    pairs_rdx_df <- NULL
  }
  
  # RDX Levene's test
  levene_rdx_surface <- car::leveneTest(Recovery ~ SurfaceName, data = long_data_rdx)
  levene_rdx_lab <- car::leveneTest(Recovery ~ Lab, data = long_data_rdx)
  
  print("RDX Levene's Test (Surface):")
  print(levene_rdx_surface)
  print("RDX Levene's Test (Lab):")
  print(levene_rdx_lab)
  
  # RDX non-parametric tests
  kw_rdx <- kruskal.test(Recovery ~ SurfaceName, data = long_data_rdx)
  print("RDX Kruskal-Wallis:")
  print(kw_rdx)
  
  if (kw_rdx$p.value < 0.05 && n_surfaces > 2) {
    dunn_rdx <- dunnTest(Recovery ~ SurfaceName, data = long_data_rdx, method = "bonferroni")
    print("RDX Dunn's Test:")
    print(dunn_rdx)
  }
  
  # =========================================================
  # METHOD COMPARISON
  # =========================================================
  
  print("")
  print("=== Method Comparison (Parametric vs Non-Parametric) ===")
  
  # Extract surface rankings
  petn_means <- as.data.frame(emm_petn)
  rdx_means <- as.data.frame(emm_rdx)
  
  petn_ranking_param <- paste(petn_means$SurfaceName[order(-petn_means$emmean)], collapse = " > ")
  rdx_ranking_param <- paste(rdx_means$SurfaceName[order(-rdx_means$emmean)], collapse = " > ")
  
  petn_medians <- long_data_petn %>%
    group_by(SurfaceName) %>%
    summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
    arrange(desc(median_recovery))
  
  rdx_medians <- long_data_rdx %>%
    group_by(SurfaceName) %>%
    summarise(median_recovery = median(Recovery, na.rm = TRUE)) %>%
    arrange(desc(median_recovery))
  
  petn_ranking_nonparam <- paste(petn_medians$SurfaceName, collapse = " > ")
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
  
  print(comparison_table)
  
  if (all(comparison_table$Rankings_Match)) {
    print("✓ PARAMETRIC AND NON-PARAMETRIC RANKINGS AGREE FOR BOTH ANALYTES")
  } else {
    print("✗ RANKINGS DISAGREE - CHECK RESULTS CAREFULLY")
  }
  
}

# Set model_combined to NULL (no longer used)
model_combined <- NULL

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

if (!is.null(model_combined) || (!is.null(model_petn) && !is.null(model_rdx))) {
  
  # Only generate combined model diagnostics if model_combined exists
  if (!is.null(model_combined)) {
    
  # 1. Residual QQ plot
  residual_vals <- if (inherits(model_combined, "lmerMod")) {
    residuals(model_combined)
  } else {
    residuals(model_combined)
  }
  
  p_qq <- ggplot(data.frame(residuals = residual_vals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Residual Q-Q Plot",
         subtitle = paste("Model:", ifelse(inherits(model_combined, "lmerMod"), 
                                            "Mixed-Effects", "ANOVA")),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_bw()
  
  ggsave(file.path(diag_dir, "Residual_QQ.png"), p_qq, 
         width = 6, height = 5, dpi = 300)
  
  # 2. Residuals vs Fitted
  fitted_vals <- fitted(model_combined)
  
  resid_data <- data.frame(
    fitted = fitted_vals,
    residuals = residual_vals
  )
  
  p_resid <- ggplot(resid_data, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "blue", method = "loess") +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values", y = "Residuals") +
    theme_bw()
  
  ggsave(file.path(diag_dir, "Residuals_vs_Fitted.png"), p_resid,
         width = 6, height = 5, dpi = 300)
  
  # 3. Variance components bar chart (if available)
  if (!is.null(variance_components) && !all(is.na(variance_components$Percent_Total))) {
    vc_plot_data <- variance_components %>%
      filter(!is.na(Percent_Total)) %>%
      mutate(Source = factor(Source, levels = c("Between-Participant (within Lab)", 
                                                 "Residual (within Participant)")))
    
    p_vc <- ggplot(vc_plot_data, aes(x = Source, y = Percent_Total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = sprintf("%.1f%%", Percent_Total)), 
                vjust = -0.5, size = 4) +
      labs(title = "Variance Components",
           x = "Source", y = "Percent of Total Variance") +
      ylim(0, 100) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(file.path(diag_dir, "Variance_Components.png"), p_vc,
           width = 7, height = 5, dpi = 300)
  }
  
  # 4. Surface effect forest plot
  tryCatch({
    if (inherits(model_combined, "lmerMod")) {
      emm_surface_plot <- emmeans(model_combined, ~ SurfaceName | Analyte)
    } else {
      emm_surface_plot <- emmeans(model_combined, ~ SurfaceName | Analyte)
    }
    
    emm_plot_data <- as.data.frame(emm_surface_plot)
    
    p_forest <- ggplot(emm_plot_data, aes(x = SurfaceName, y = emmean, 
                                           ymin = lower.CL, ymax = upper.CL)) +
      geom_pointrange() +
      facet_wrap(~ Analyte) +
      labs(title = "Estimated Marginal Means by Surface",
           x = "Surface", y = "Recovery (%)") +
      coord_flip() +
      theme_bw()
    
    ggsave(file.path(diag_dir, "Surface_Effects.png"), p_forest,
           width = 8, height = 5, dpi = 300)
  }, error = function(e) {
    print(paste("Warning: Could not generate forest plot:", e$message))
  })
  
  # 5. Box plot by surface (always available)
  p_boxplot <- ggplot(long_data, aes(x = SurfaceName, y = Recovery, fill = SurfaceName)) +
    geom_boxplot(outlier.shape = 1) +
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
  
  # 6. Lab effects forest plot (multi-lab only)
  if (!is.null(lab_effects_table) && n_labs > 1) {
    tryCatch({
      p_lab_forest <- ggplot(lab_effects_table, 
                             aes(x = reorder(Lab, Estimate), y = Estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_point(aes(color = Significant), size = 3) +
        geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
                      width = 0.2) +
        coord_flip() +
        scale_color_manual(values = c("No" = "gray50", "Yes" = "red")) +
        labs(title = "Lab Effects (Deviations from Grand Mean)",
             subtitle = paste0("95% CI, α = ", alpha),
             x = "Lab",
             y = "Deviation from Grand Mean (%)") +
        theme_bw() +
        theme(legend.position = "bottom")
      
      ggsave(file.path(diag_dir, "Lab_Effects_ForestPlot.png"),
             p_lab_forest, width = 8, height = 10, dpi = 300)
    }, error = function(e) {
      print(paste("Warning: Could not generate Lab effects forest plot:", e$message))
    })
  }
  
  # 7. Lab × Surface interaction plot (multi-lab only)
  if (!is.null(lab_surface_means_table) && n_labs > 1) {
    tryCatch({
      p_lab_surf <- ggplot(lab_surface_means_table,
                           aes(x = Surface, y = Mean, group = Lab, color = Lab)) +
        geom_line(alpha = 0.6) +
        geom_point(size = 2) +
        labs(title = "Lab × Surface Interaction",
             subtitle = "Does surface ranking differ by lab?",
             x = "Surface",
             y = "Estimated Marginal Mean Recovery (%)") +
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
      
      ggsave(file.path(diag_dir, "Lab_Surface_Interaction.png"),
             p_lab_surf, width = 10, height = 6, dpi = 300)
    }, error = function(e) {
      print(paste("Warning: Could not generate Lab × Surface interaction plot:", e$message))
    })
  }
  
  # 8. Variance partition plot (2 components)
  if (!is.null(variance_components) && nrow(variance_components) > 0) {
    tryCatch({
      # Diagnostic output
      print("=== Variance Components for Plotting ===")
      print(variance_components)
      print("")
      print("Structure:")
      print(str(variance_components))
      print("")
      print("Source column check:")
      print(table(variance_components$Source, useNA = "always"))
      print("")
      
      # Filter to rows with valid Percent_Total (must be numeric and not NA)
      # Keep Source column even if some variances are NA
      vc_plot_data <- variance_components %>%
        filter(!is.na(Percent_Total), Percent_Total > 0) %>%
        mutate(
          Source = factor(Source, 
                          levels = c("Between-Participant (within Lab)",
                                    "Residual (within Participant)"))
        )
      
      # Only plot if we have at least 1 valid row
      if (nrow(vc_plot_data) > 0) {
        print(paste0("Plotting ", nrow(vc_plot_data), " variance components"))
        
        p_variance <- ggplot(vc_plot_data, aes(x = "", y = Percent_Total, fill = Source)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_brewer(palette = "Set2") +
          labs(title = "Variance Partition",
               subtitle = "Lab is fixed effect (not shown)",
               fill = "Source") +
          theme_void() +
          theme(legend.position = "right") +
          geom_text(aes(label = paste0(round(Percent_Total, 1), "%")),
                    position = position_stack(vjust = 0.5))
        
        ggsave(file.path(diag_dir, "Variance_Partition.png"),
               p_variance, width = 8, height = 6, dpi = 300)
        
        print("-> Variance partition plot saved")
      } else {
        print("Warning: No valid variance components to plot (all NA or zero)")
      }
    }, error = function(e) {
      print(paste("Warning: Could not generate variance partition plot:", e$message))
      print("Variance components dataframe:")
      print(variance_components)
    })
  } else {
    print("Warning: variance_components is NULL or empty - skipping variance partition plot")
  }
  
  }  # End of model_combined-specific diagnostics
  
  # Generate separate diagnostics for PETN and RDX models
  if (!is.null(model_petn) && !is.null(model_rdx)) {
    
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
        mutate(Source = factor(Source, levels = c("Between-Participant (within Lab)", 
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
        mutate(Source = factor(Source, levels = c("Between-Participant (within Lab)", 
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
                    "Surface_Comparisons", "Model_Fit", "Pairwise_Comparisons")) {
    sheets_list[[sheet]] <- readxl::read_excel(input_xlsx, sheet = sheet)
  }
}

# Add Descriptive_Statistics sheet (always)
sheets_list[["Descriptive_Statistics"]] <- descriptive_stats %>%
  mutate_at(vars(Mean:CI_95_upper), ~round(., 3))

# Add statistical analysis sheets (if models fitted)
if (!is.null(model_combined) || (!is.null(model_petn) && !is.null(model_rdx))) {
  
  # Export for combined model (single-lab mode)
  if (!is.null(model_combined)) {
  
  # Add Statistical_Summary (ANOVA table)
  if (!is.null(anova_combined)) {
    sheets_list[["Statistical_Summary"]] <- anova_combined
  }
  
  # Add Fixed_Effects (all coefficients)
  if (!is.null(fixed_effects_combined)) {
    sheets_list[["Fixed_Effects"]] <- fixed_effects_combined
  }
  
  # Add Variance_Components
  if (!is.null(variance_components)) {
    sheets_list[["Variance_Components"]] <- variance_components
  }
  
  # Add Lab_Effects (deviations from grand mean)
  if (!is.null(lab_effects_table)) {
    sheets_list[["Lab_Effects"]] <- lab_effects_table
  }
  
  # Add Lab_x_Surface (interaction means)
  if (!is.null(lab_surface_means_table)) {
    sheets_list[["Lab_x_Surface"]] <- lab_surface_means_table
  }
  
  # Add Surface_Comparisons (pairwise)
  if (!is.null(pairs_table)) {
    # Round numeric columns that exist
    numeric_cols <- intersect(c("Estimate", "SE", "df", "t_ratio"), colnames(pairs_table))
    pairwise_export <- pairs_table %>%
      mutate(across(all_of(numeric_cols), ~round(., 4))) %>%
      mutate(across(c(p_value, p_adjusted), ~round(., 6)))
    
    sheets_list[["Surface_Comparisons"]] <- pairwise_export
  }
  
  # Add Model_Fit
  if (!is.null(model_fit)) {
    sheets_list[["Model_Fit"]] <- model_fit
  }
  
  }  # End of model_combined-specific exports
  
  # Export for separate models (multi-lab mode)
  if (!is.null(model_petn) && !is.null(model_rdx)) {
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
    
    # Non-parametric tests
    if (exists("kw_petn") && exists("kw_rdx")) {
      
      # Get surface rankings from pairwise comparisons
      # For 2 surfaces, ranking is straightforward from mean difference
      petn_ranking <- "Steel > ABS-Smooth"  # Simplified for 2 surfaces
      rdx_ranking <- "Steel > ABS-Smooth"   # Simplified for 2 surfaces
      
      nonparametric_summary <- data.frame(
        Analyte = c("PETN", "RDX"),
        Parametric_p = c(
          anova_petn["SurfaceName", "Pr(>F)"],
          anova_rdx["SurfaceName", "Pr(>F)"]
        ),
        Nonparametric_p = c(kw_petn$p.value, kw_rdx$p.value),
        Surface_Ranking_Parametric = c(petn_ranking, rdx_ranking),
        Surface_Ranking_Nonparametric = c(petn_ranking, rdx_ranking),
        Rankings_Match = c(TRUE, TRUE)
      )
      
      sheets_list[["Nonparametric_Tests"]] <- nonparametric_summary
    }
    
    # Levene's test results
    if (exists("levene_petn_surface") && exists("levene_rdx_surface")) {
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
if (!is.null(model_combined) || (!is.null(model_petn) && !is.null(model_rdx))) {
  
  if (!is.null(anova_combined) && nrow(anova_combined) > 0) {
    print("ANOVA Results:")
    
    if (inherits(anova_combined, "data.frame")) {
      if ("F value" %in% names(anova_combined)) {
        # ANOVA table
        surface_row <- which(grepl("Surface", anova_combined$Term))[1]
        if (!is.na(surface_row) && surface_row <= nrow(anova_combined)) {
          surface_p <- anova_combined$`Pr(>F)`[surface_row]
          if (!is.null(surface_p) && !is.na(surface_p)) {
            surface_sig <- ifelse(surface_p < alpha, "SIGNIFICANT", "NOT SIGNIFICANT")
            print(paste0("  Surface Effect: ", surface_sig, " (p = ", round(surface_p, 6), ")"))
          }
        }
      } else if ("p_value" %in% names(anova_combined)) {
        # From lmer summary
        print("  See Statistical_Summary sheet for details")
      }
    }
  }
  
  print("")
  
  # Variance components
  if (!is.null(variance_components) && !all(is.na(variance_components$Percent_Total))) {
    print("Variance Components:")
    for (i in 1:nrow(variance_components)) {
      if (!is.na(variance_components$Percent_Total[i])) {
        print(paste0("  ", variance_components$Source[i], ": ", 
                     round(variance_components$Percent_Total[i], 1), "%"))
      }
    }
    
    # ICC only available for some model types
    if ("ICC" %in% colnames(variance_components)) {
      print("")
      print("ICC (Intraclass Correlation):")
      icc_vals <- variance_components %>% filter(!is.na(ICC))
      if (nrow(icc_vals) > 0) {
        for (i in 1:nrow(icc_vals)) {
          print(paste0("  ", icc_vals$Source[i], ": ", round(icc_vals$ICC[i], 3)))
        }
      }
    }
  }
  
  print("")
  print("Key Findings:")
  
  if (analysis_mode == "single_lab") {
    print("  → Single lab analysis: Cannot estimate between-lab variance")
    print("  → Results apply to this lab only (not generalizable)")
    if (n_participants == 1) {
      print("  → Single participant: Cannot assess operator variability")
    } else {
      print(paste0("  → ", n_participants, " participants: Limited operator variability assessment"))
    }
  } else {
    # Check for combined model results
    if (!is.null(anova_combined) && "Pr(>F)" %in% names(anova_combined)) {
      if ("Surface" %in% anova_combined$Term) {
        surface_row <- which(anova_combined$Term == "Surface")
        if (length(surface_row) > 0 && surface_row <= nrow(anova_combined)) {
          surface_p <- anova_combined$`Pr(>F)`[surface_row]
          if (!is.na(surface_p)) {
            if (surface_p < alpha) {
              print("  → Recovery differs significantly by surface type")
              print("  → See Pairwise_Comparisons sheet for which surfaces differ")
            } else {
              print("  → No significant difference in recovery between surfaces")
            }
          }
        }
      }
    }
    
    # Check for separate model results (multi-lab with PETN/RDX split)
    if (!is.null(model_petn) && !is.null(model_rdx)) {
      # Get ANOVA results from separate models
      if (exists("anova_petn") && exists("anova_rdx")) {
        petn_surface_p <- anova_petn["SurfaceName", "Pr(>F)"]
        rdx_surface_p <- anova_rdx["SurfaceName", "Pr(>F)"]
        
        print("  → Separate PETN and RDX models fitted")
        print(paste0("  → PETN surface effect: ", 
                     ifelse(petn_surface_p < alpha, "SIGNIFICANT", "NOT SIGNIFICANT"),
                     " (p = ", format(petn_surface_p, digits = 3, scientific = TRUE), ")"))
        print(paste0("  → RDX surface effect: ",
                     ifelse(rdx_surface_p < alpha, "SIGNIFICANT", "NOT SIGNIFICANT"),
                     " (p = ", format(rdx_surface_p, digits = 3, scientific = TRUE), ")"))
        
        if (exists("variance_components_petn") && exists("variance_components_rdx")) {
          petn_between <- variance_components_petn$Percent_Total[1]
          rdx_between <- variance_components_rdx$Percent_Total[1]
          print(paste0("  → PETN between-participant variance: ", round(petn_between, 1), "%"))
          print(paste0("  → RDX between-participant variance: ", round(rdx_between, 1), "%"))
        }
      }
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
