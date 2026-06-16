# =========================================================
# FINEX Swabbing Study - Statistical Analysis
# =========================================================
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
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Lab10-1/Results"

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

if ("rdx_recovery_dc" %in% names(master)) {
  rdx_rec_col <- "rdx_recovery_dc"
  print("Using drift-corrected RDX recovery")
} else {
  rdx_rec_col <- "rdx_recovery"
  print("Using uncorrected RDX recovery")
}

# Filter to samples (exclude negative controls) and prepare
analysis_data <- master %>%
  filter(!IsNegativeControl, !is.na(Lab)) %>%
  select(Lab, Participant, Surface, SurfaceName,
         all_of(c(petn_rec_col, rdx_rec_col))) %>%
  rename(petn_recovery = !!petn_rec_col,
         rdx_recovery = !!rdx_rec_col) %>%
  filter(!is.na(petn_recovery) | !is.na(rdx_recovery))

print(paste0("Samples for analysis: ", nrow(analysis_data)))

# Check data availability per analyte and surface
data_summary <- analysis_data %>%
  group_by(SurfaceName) %>%
  summarise(
    n_petn = sum(!is.na(petn_recovery)),
    n_rdx = sum(!is.na(rdx_recovery)),
    .groups = "drop"
  )

print("Data availability by surface:")
print(data_summary)

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
  }
  
}

# -----------------------------
# MULTI-LAB MODE (Mixed Effects)
# -----------------------------

if (analysis_mode %in% c("few_labs", "multi_lab")) {
  
  print("Fitting mixed-effects models...")
  
  # Combined model
  print("Fitting combined model (Surface × Analyte)...")
  
  model_combined <- tryCatch({
    lmer(Recovery ~ SurfaceName * Analyte + (1 | Lab) + (1 | Lab:Participant),
         data = long_data,
         REML = TRUE)
  }, error = function(e) {
    print(paste("Warning - full model failed:", e$message))
    print("Trying simplified model without Lab:Participant random effect...")
    
    tryCatch({
      lmer(Recovery ~ SurfaceName * Analyte + (1 | Lab),
           data = long_data,
           REML = TRUE)
    }, error = function(e2) {
      print(paste("Warning - simplified model also failed:", e2$message))
      print("Falling back to fixed effects model...")
      lm(Recovery ~ SurfaceName * Analyte + Lab, data = long_data)
    })
  })
  
  print("Combined model fitted")
  
  # Separate models per analyte
  petn_data <- long_data %>% filter(Analyte == "PETN")
  rdx_data <- long_data %>% filter(Analyte == "RDX")
  
  print("Fitting PETN-only model...")
  model_petn <- tryCatch({
    lmer(Recovery ~ SurfaceName + (1 | Lab) + (1 | Lab:Participant),
         data = petn_data,
         REML = TRUE)
  }, error = function(e) {
    tryCatch({
      lmer(Recovery ~ SurfaceName + (1 | Lab), data = petn_data, REML = TRUE)
    }, error = function(e2) {
      lm(Recovery ~ SurfaceName + Lab, data = petn_data)
    })
  })
  
  print("Fitting RDX-only model...")
  model_rdx <- tryCatch({
    lmer(Recovery ~ SurfaceName + (1 | Lab) + (1 | Lab:Participant),
         data = rdx_data,
         REML = TRUE)
  }, error = function(e) {
    tryCatch({
      lmer(Recovery ~ SurfaceName + (1 | Lab), data = rdx_data, REML = TRUE)
    }, error = function(e2) {
      lm(Recovery ~ SurfaceName + Lab, data = rdx_data)
    })
  })
  
  # Extract fixed effects
  print("Extracting fixed effects...")
  
  if (inherits(model_combined, "lmerMod")) {
    anova_combined <- anova(model_combined)
    
    fixed_effects_combined <- summary(model_combined)$coefficients %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Term") %>%
      rename(Estimate = Estimate, 
             SE = `Std. Error`, 
             df = `df`,
             t_value = `t value`,
             p_value = `Pr(>|t|)`)
  } else {
    anova_combined <- anova(model_combined)
    
    fixed_effects_combined <- summary(model_combined)$coefficients %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Term") %>%
      rename(Estimate = Estimate,
             SE = `Std. Error`,
             t_value = `t value`,
             p_value = `Pr(>|t|)`) %>%
      mutate(df = NA_real_)
  }
  
  # Effect sizes
  print("Calculating effect sizes...")
  
  partial_eta <- tryCatch({
    eta_squared(model_combined, partial = TRUE, ci = 0.95)
  }, error = function(e) {
    print(paste("Warning: Could not calculate partial eta-squared:", e$message))
    NULL
  })
  
  # Variance components
  print("Extracting variance components...")
  
  if (inherits(model_combined, "lmerMod")) {
    vc <- VarCorr(model_combined)
    
    var_lab <- as.numeric(vc$Lab)
    var_participant <- if (!is.null(vc$`Lab:Participant`)) {
      as.numeric(vc$`Lab:Participant`)
    } else {
      NA_real_
    }
    var_residual <- attr(vc, "sc")^2
    
    total_var <- sum(c(var_lab, var_participant, var_residual), na.rm = TRUE)
    
    variance_components <- data.frame(
      Model = c("Combined", "Combined", "Combined"),
      Source = c("Between-Lab", "Within-Lab (Between-Participant)", "Within-Participant (Residual)"),
      Variance = c(var_lab, var_participant, var_residual),
      SD = c(sqrt(var_lab), sqrt(var_participant), sqrt(var_residual)),
      ICC = c(var_lab/total_var, var_participant/total_var, NA_real_),
      Percent_Total = c(var_lab/total_var*100, var_participant/total_var*100, var_residual/total_var*100)
    )
  } else {
    # Fixed effects model
    residual_var <- sum(residuals(model_combined)^2) / df.residual(model_combined)
    
    variance_components <- data.frame(
      Model = c("Combined", "Combined", "Combined"),
      Source = c("Between-Lab", "Within-Lab (Between-Participant)", "Within-Participant (Residual)"),
      Variance = c(NA_real_, NA_real_, residual_var),
      SD = c(NA_real_, NA_real_, sqrt(residual_var)),
      ICC = c(NA_real_, NA_real_, NA_real_),
      Percent_Total = c(NA_real_, NA_real_, 100)
    )
  }
  
  # Model fit
  model_fit <- data.frame(
    Model = "Combined",
    AIC = AIC(model_combined),
    BIC = BIC(model_combined),
    N_Obs = nrow(long_data)
  )
  
  # Post-hoc comparisons
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
}

# =========================================================
# Build fixed effects table
# =========================================================

if (!is.null(fixed_effects_combined) || !is.null(anova_combined)) {
  
  print("")
  print("=== Fixed Effects Summary ===")
  print("")
  
  if (is.null(fixed_effects_combined) && !is.null(anova_combined)) {
    # Build from ANOVA table - columns already renamed earlier
    fixed_effects_combined <- anova_combined
  }
  
  # Add effect sizes if available
  if (!is.null(partial_eta)) {
    eta_table <- as.data.frame(partial_eta) %>%
      rename(Term = Parameter,
             Partial_Eta2 = Eta2_partial,
             CI_lower = CI_low,
             CI_upper = CI_high)
    
    fixed_effects_combined <- fixed_effects_combined %>%
      left_join(eta_table, by = "Term")
  }
  
  print("Fixed effects extracted")
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
      mutate(Source = factor(Source, levels = c("Between-Lab", 
                                                 "Within-Lab (Between-Participant)", 
                                                 "Within-Participant (Residual)")))
    
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
  
  # 6. Lab BLUPs (only for mixed models with multiple labs)
  if (inherits(model_combined, "lmerMod") && n_labs > 1) {
    tryCatch({
      lab_blups <- ranef(model_combined)$Lab %>%
        tibble::rownames_to_column("Lab") %>%
        rename(LabEffect = `(Intercept)`) %>%
        mutate(Lab = as.integer(Lab))
      
      p_blups <- ggplot(lab_blups, aes(x = reorder(Lab, LabEffect), y = LabEffect)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Lab Random Effects (BLUPs)",
             x = "Lab", y = "Random Effect Estimate") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
      
      ggsave(file.path(diag_dir, "Lab_BLUPs.png"), p_blups,
             width = 10, height = 5, dpi = 300)
    }, error = function(e) {
      print(paste("Warning: Could not generate BLUPs plot:", e$message))
    })
  }
  
}

print(paste0("Diagnostic plots saved to: ", diag_dir))

# =========================================================
# Export to Excel
# =========================================================

print("")
print("=== Exporting Results to Excel ===")
print("")

# Read existing sheets
existing_sheets <- readxl::excel_sheets(input_xlsx)

sheet_list <- list()

# Copy existing sheets
for (sheet in existing_sheets) {
  sheet_list[[sheet]] <- readxl::read_excel(input_xlsx, sheet = sheet)
}

# Add Descriptive_Statistics sheet
sheet_list[["Descriptive_Statistics"]] <- descriptive_stats %>%
  mutate_at(vars(Mean:CI_95_upper), ~round(., 3))

# Add Statistical_Summary sheet (if models fitted)
if (!is.null(model_combined)) {
  
  stat_summary_combined <- NULL
  
  # Fixed effects section
  if (!is.null(fixed_effects_combined)) {
    fixed_effects_export <- fixed_effects_combined %>%
      mutate(Significant = ifelse(p_value < alpha, "Yes", "No")) %>%
      select(any_of(c("Term", "Estimate", "SE", "df", "t_value", "p_value", 
                      "Significant", "Partial_Eta2", "CI_lower", "CI_upper"))) %>%
      mutate(Section = "Fixed Effects")
    
    stat_summary_combined <- fixed_effects_export
  }
  
  # Variance components section
  if (!is.null(variance_components)) {
    variance_export <- variance_components %>%
      select(-ICC) %>%
      mutate(Section = "Variance Components")
    
    if (is.null(stat_summary_combined)) {
      stat_summary_combined <- variance_export
    } else {
      stat_summary_combined <- bind_rows(stat_summary_combined, variance_export)
    }
  }
  
  # ICC summary
  if (!is.null(variance_components)) {
    icc_summary <- variance_components %>%
      filter(!is.na(ICC)) %>%
      select(Model, Source, ICC) %>%
      mutate(Section = "ICC Summary")
    
    stat_summary_combined <- bind_rows(stat_summary_combined, icc_summary)
  }
  
  # Model fit
  if (!is.null(model_fit)) {
    model_fit_export <- model_fit %>%
      mutate(Section = "Model Fit")
    
    stat_summary_combined <- bind_rows(stat_summary_combined, model_fit_export)
  }
  
  # Round numeric columns
  if (!is.null(stat_summary_combined)) {
    numeric_cols <- sapply(stat_summary_combined, is.numeric)
    stat_summary_combined[, numeric_cols] <- round(stat_summary_combined[, numeric_cols], 4)
  }
  
  sheet_list[["Statistical_Summary"]] <- stat_summary_combined
  
  # Add Pairwise_Comparisons sheet
  if (!is.null(pairs_table)) {
    pairwise_export <- pairs_table %>%
      select(Analyte, Contrast, Estimate, SE, df, t_ratio, p_value, p_adjusted, Significant) %>%
      mutate_at(vars(Estimate:SE, t_ratio), ~round(., 4)) %>%
      mutate_at(vars(p_value, p_adjusted), ~round(., 6))
    
    sheet_list[["Pairwise_Comparisons"]] <- pairwise_export
  }
}

# Write to Excel
write_xlsx(sheet_list, path = output_xlsx)

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
if (!is.null(model_combined)) {
  
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
    
    print("")
    print("ICC (Intraclass Correlation):")
    icc_vals <- variance_components %>% filter(!is.na(ICC))
    for (i in 1:nrow(icc_vals)) {
      print(paste0("  ", icc_vals$Source[i], ": ", round(icc_vals$ICC[i], 3)))
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
