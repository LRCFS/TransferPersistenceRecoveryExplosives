# =========================================================
# FINEX Swabbing Study - Collate Results Across All Runs
# =========================================================
#
# Run this script at any time to (re)build the study-level
# master dataset from all individually processed runs.
#
# This script is incremental: it discovers whatever
# *_GCMSResults.csv files exist at that moment and combines
# them. Safe to re-run at any stage of data collection.
#
# Outputs:
#   {study_root_dir}/FINEX_StudyResults.csv
#   {study_root_dir}/FINEX_StudyResults.xlsx (with conditional formatting)
#
# Author: <add name>
# License: GNU AGPL v3
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

# =========================================================
# CONFIGURATION - set the study root folder
# =========================================================
study_root_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"

# =========================================================
# QC Bracket Acceptance Thresholds
# =========================================================
qc_6ng_bias_limit  <- 20   # +/- 20% for 6ng QC (bias-based)
# 0.2ng QC uses SNR-based criteria (not bias):
#   PASS: SNR > 10 (Quantifiable)
#   WARN: 3 <= SNR <= 10 (Below_LOQ - detected but below quantification)
#   FAIL: SNR < 3 (Below_LOD - not reliably detected)
qc_02ng_snr_pass <- 10     # SNR threshold for PASS
qc_02ng_snr_warn <- 3      # SNR threshold for WARN (below this = FAIL)

# =========================================================
# Load parameters from GlobalCode.R
# =========================================================
global_code_path <- "C:/Users/A Bruce - User/Documents/ClaudeSpace/GCMSQuantitation/GlobalCode.R"

if (!exists("SampleVol") || !exists("DepositMass")) {
  source(global_code_path)
}

# =========================================================
# Surface code mapping
# =========================================================
surface_map <- c(
  "S"     = "Steel",
  "G"     = "Glass",
  "ABS-S" = "ABS-Smooth",
  "ABS-T" = "ABS-Textured"
)

# =========================================================
# 1. Discover all *_GCMSResults.csv files recursively
# =========================================================
results_files <- list.files(
  study_root_dir,
  pattern = "_GCMSResults\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Exclude backup files (stored in Results/Backup/ folders)
results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]

if (length(results_files) == 0) {
  stop("No *_GCMSResults.csv files found under: ", study_root_dir)
}

print(paste0("Found ", length(results_files), " results file(s):"))
for (f in results_files) {
  print(paste0("  ", f))
}

# =========================================================
# 2. Read and combine all results files
# =========================================================
all_data <- bind_rows(lapply(results_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$SourceFile <- f
  df
}))

print(paste0("Total rows loaded: ", nrow(all_data)))

# =========================================================
# 2b. Handle column naming differences between script versions
# =========================================================
# For backward compatibility with older results files:
if ("is_pa" %in% names(all_data) && !("rdx_is_pa" %in% names(all_data))) {
  all_data$rdx_is_pa <- all_data$is_pa
}

# Determine PETN quantification method from columns present
if (!("petn_quant_method" %in% names(all_data))) {
  all_data$petn_quant_method <- ifelse(
    "petn_concentration_ratio" %in% names(all_data) && 
    !is.na(all_data$petn_concentration_ratio),
    "15N-RDX_Ratio",
    ifelse("petn_concentration_pa" %in% names(all_data), "Peak_Area", "Unknown")
  )
}

# Map old suffixed columns to new unified names for PETN
if (!("petn_concentration" %in% names(all_data))) {
  all_data$petn_concentration <- ifelse(
    all_data$petn_quant_method == "15N-RDX_Ratio",
    all_data$petn_concentration_ratio,
    all_data$petn_concentration_pa
  )
}
if (!("petn_range_flag" %in% names(all_data))) {
  all_data$petn_range_flag <- ifelse(
    all_data$petn_quant_method == "15N-RDX_Ratio",
    all_data$petn_range_flag_ratio,
    all_data$petn_range_flag_pa
  )
}

# Drift-corrected PETN concentration
if (!("petn_concentration_dc" %in% names(all_data))) {
  if ("petn_concentration_pa_dc" %in% names(all_data)) {
    all_data$petn_concentration_dc <- all_data$petn_concentration_pa_dc
  } else if ("petn_concentration_ratio_dc" %in% names(all_data)) {
    all_data$petn_concentration_dc <- all_data$petn_concentration_ratio_dc
  }
}

# Map RDX columns (always uses ratio)
if (!("rdx_concentration" %in% names(all_data)) && "rdx_concentration_ratio" %in% names(all_data)) {
  all_data$rdx_concentration <- all_data$rdx_concentration_ratio
}
if (!("rdx_range_flag" %in% names(all_data)) && "rdx_range_flag_ratio" %in% names(all_data)) {
  all_data$rdx_range_flag <- all_data$rdx_range_flag_ratio
}

# =========================================================
# 3. Extract QC bracket information (BEFORE filtering)
# =========================================================
# Determine bias column names for PETN and RDX
# PETN: use drift-corrected bias if available, else uncorrected
petn_bias_col <- if ("petn_percent_bias_dc" %in% names(all_data)) {
  "petn_percent_bias_dc"
} else if ("petn_percent_bias_pa_dc" %in% names(all_data)) {
  "petn_percent_bias_pa_dc"
} else if ("petn_percent_bias" %in% names(all_data)) {
  "petn_percent_bias"
} else {
  NA_character_
}

# RDX: use uncorrected (IS ratio already handles drift)
rdx_bias_col <- if ("rdx_percent_bias" %in% names(all_data)) {
  "rdx_percent_bias"
} else if ("rdx_percent_bias_dc" %in% names(all_data)) {
  "rdx_percent_bias_dc"
} else {
  NA_character_
}

print(paste0("PETN bias column for QC brackets: ", petn_bias_col))
print(paste0("RDX bias column for QC brackets: ", rdx_bias_col))

# Build QC lookup per source file
# 6ng QCs: evaluated on bias (drift-corrected for PETN, uncorrected for RDX)
# 0.2ng QCs: evaluated on SNR (can the system still detect at trace level?)
assign_qc_brackets <- function(all_data, petn_bias_col, rdx_bias_col,
                                qc_6ng_limit, snr_pass, snr_warn) {
  
  # Ensure Line column exists
  if (!("Line" %in% names(all_data))) {
    warning("No 'Line' column found. QC brackets cannot be assigned.")
    all_data$petn_qc_6ng_pre  <- NA_character_
    all_data$petn_qc_6ng_post <- NA_character_
    all_data$rdx_qc_6ng_pre   <- NA_character_
    all_data$rdx_qc_6ng_post  <- NA_character_
    all_data$petn_qc_02ng_pre  <- NA_character_
    all_data$petn_qc_02ng_post <- NA_character_
    all_data$rdx_qc_02ng_pre   <- NA_character_
    all_data$rdx_qc_02ng_post  <- NA_character_
    return(all_data)
  }
  
  # Extract QC rows with bias columns AND SNR columns
  bias_cols_to_select <- c(petn_bias_col, rdx_bias_col)
  bias_cols_to_select <- bias_cols_to_select[!is.na(bias_cols_to_select)]
  snr_cols_to_select <- c("petn_snr", "rdx_snr")
  
  qc_rows <- all_data %>%
    filter(Type == "QC") %>%
    select(SourceFile, Line, CalLevel,
           any_of(c(bias_cols_to_select, snr_cols_to_select)))
  
  if (nrow(qc_rows) == 0) {
    warning("No QC rows found. QC brackets cannot be assigned.")
    all_data$petn_qc_6ng_pre  <- NA_character_
    all_data$petn_qc_6ng_post <- NA_character_
    all_data$rdx_qc_6ng_pre   <- NA_character_
    all_data$rdx_qc_6ng_post  <- NA_character_
    all_data$petn_qc_02ng_pre  <- NA_character_
    all_data$petn_qc_02ng_post <- NA_character_
    all_data$rdx_qc_02ng_pre   <- NA_character_
    all_data$rdx_qc_02ng_post  <- NA_character_
    return(all_data)
  }
  
  # Rename bias columns for consistency (6ng evaluation)
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
  
  # Ensure SNR columns exist
  if (!("petn_snr" %in% names(qc_rows))) qc_rows$petn_snr <- NA_real_
  if (!("rdx_snr" %in% names(qc_rows)))  qc_rows$rdx_snr  <- NA_real_
  
  # Determine which CalLevels are present
  cal_levels <- sort(unique(qc_rows$CalLevel[!is.na(qc_rows$CalLevel)]))
  
  # Identify 6ng level (highest, or closest to 6)
  level_6ng <- if (6 %in% cal_levels) {
    6
  } else if (length(cal_levels) > 0) {
    cal_levels[which.max(cal_levels)]
  } else {
    NA_real_
  }
  
  # Identify 0.2ng level (lowest, or closest to 0.2)
  level_02ng <- if (0.2 %in% cal_levels) {
    0.2
  } else if (length(cal_levels) > 1) {
    cal_levels[which.min(cal_levels)]
  } else {
    NA_real_
  }
  
  print(paste0("QC levels identified: 6ng = ", level_6ng, ", 0.2ng = ", level_02ng))
  
  # Helper: evaluate 6ng QC bracket (bias-based)
  evaluate_qc_bias <- function(bias_value, limit) {
    if (is.na(bias_value)) return(NA_character_)
    if (abs(bias_value) <= limit) return("PASS")
    return("FAIL")
  }
  
  # Helper: evaluate 0.2ng QC bracket (SNR-based)
  # PASS: SNR > snr_pass (system can quantify at this level)
  # WARN: snr_warn <= SNR <= snr_pass (detected but marginal)
  # FAIL: SNR < snr_warn (cannot reliably detect)
  evaluate_qc_snr <- function(snr_value, snr_pass_threshold, snr_warn_threshold) {
    if (is.na(snr_value)) return(NA_character_)
    if (snr_value > snr_pass_threshold) return("PASS")
    if (snr_value >= snr_warn_threshold) return("WARN")
    return("FAIL")
  }
  
  # Initialize QC bracket columns
  all_data$petn_qc_6ng_pre  <- NA_character_
  all_data$petn_qc_6ng_post <- NA_character_
  all_data$rdx_qc_6ng_pre   <- NA_character_
  all_data$rdx_qc_6ng_post  <- NA_character_
  all_data$petn_qc_02ng_pre  <- NA_character_
  all_data$petn_qc_02ng_post <- NA_character_
  all_data$rdx_qc_02ng_pre   <- NA_character_
  all_data$rdx_qc_02ng_post  <- NA_character_
  
  # Process each source file independently
  source_files <- unique(all_data$SourceFile[all_data$Type == "Sample"])
  
  for (sf in source_files) {
    # QCs for this source file
    sf_qc <- qc_rows %>% filter(SourceFile == sf)
    
    # Split by level
    qc_6ng <- if (!is.na(level_6ng)) {
      sf_qc %>% filter(CalLevel == level_6ng) %>% arrange(Line)
    } else {
      data.frame()
    }
    
    qc_02ng <- if (!is.na(level_02ng)) {
      sf_qc %>% filter(CalLevel == level_02ng) %>% arrange(Line)
    } else {
      data.frame()
    }
    
    # Samples for this source file
    sf_sample_idx <- which(all_data$SourceFile == sf & all_data$Type == "Sample")
    
    for (i in sf_sample_idx) {
      sample_line <- all_data$Line[i]
      
      # --- 6ng QC brackets (bias-based) ---
      if (nrow(qc_6ng) > 0) {
        # Preceding: largest QC Line < sample Line
        pre_6ng <- qc_6ng %>% filter(Line < sample_line)
        if (nrow(pre_6ng) > 0) {
          pre_6ng_row <- pre_6ng[nrow(pre_6ng), ]
          all_data$petn_qc_6ng_pre[i] <- evaluate_qc_bias(pre_6ng_row$petn_bias, qc_6ng_limit)
          all_data$rdx_qc_6ng_pre[i]  <- evaluate_qc_bias(pre_6ng_row$rdx_bias, qc_6ng_limit)
        }
        
        # Following: smallest QC Line > sample Line
        post_6ng <- qc_6ng %>% filter(Line > sample_line)
        if (nrow(post_6ng) > 0) {
          post_6ng_row <- post_6ng[1, ]
          all_data$petn_qc_6ng_post[i] <- evaluate_qc_bias(post_6ng_row$petn_bias, qc_6ng_limit)
          all_data$rdx_qc_6ng_post[i]  <- evaluate_qc_bias(post_6ng_row$rdx_bias, qc_6ng_limit)
        }
      }
      
      # --- 0.2ng QC brackets (SNR-based) ---
      if (nrow(qc_02ng) > 0) {
        # Preceding
        pre_02ng <- qc_02ng %>% filter(Line < sample_line)
        if (nrow(pre_02ng) > 0) {
          pre_02ng_row <- pre_02ng[nrow(pre_02ng), ]
          all_data$petn_qc_02ng_pre[i] <- evaluate_qc_snr(pre_02ng_row$petn_snr, snr_pass, snr_warn)
          all_data$rdx_qc_02ng_pre[i]  <- evaluate_qc_snr(pre_02ng_row$rdx_snr, snr_pass, snr_warn)
        }
        
        # Following
        post_02ng <- qc_02ng %>% filter(Line > sample_line)
        if (nrow(post_02ng) > 0) {
          post_02ng_row <- post_02ng[1, ]
          all_data$petn_qc_02ng_post[i] <- evaluate_qc_snr(post_02ng_row$petn_snr, snr_pass, snr_warn)
          all_data$rdx_qc_02ng_post[i]  <- evaluate_qc_snr(post_02ng_row$rdx_snr, snr_pass, snr_warn)
        }
      }
    }
  }
  
  return(all_data)
}

# Assign QC brackets
all_data <- assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col,
                                qc_6ng_bias_limit, qc_02ng_snr_pass, qc_02ng_snr_warn)

print("QC bracket flags assigned.")

# =========================================================
# 4. Filter to Sample rows only (includes NCs)
# =========================================================
samples <- all_data %>%
  filter(Type == "Sample")

print(paste0("Sample rows (including NCs): ", nrow(samples)))

if (nrow(samples) == 0) {
  stop("No Sample rows found in the results files. Check that sequences have been processed.")
}

# =========================================================
# 5. Parse SampleName into structured columns
# =========================================================
samples$SampleName_trimmed <- str_trim(samples$SampleName)

sample_pattern <- "^Lab(\\d+)\\s+P(\\d+)\\s+(ABS-S|ABS-T|S|G)\\s*(NC|\\d+)$"

parsed <- str_match(samples$SampleName_trimmed, sample_pattern)

samples$Lab         <- as.integer(parsed[, 2])
samples$Participant <- as.integer(parsed[, 3])
samples$Surface     <- parsed[, 4]
samples$RepeatRaw   <- parsed[, 5]

samples$IsNegativeControl <- !is.na(samples$RepeatRaw) & samples$RepeatRaw == "NC"

samples$Repeat <- ifelse(
  samples$IsNegativeControl,
  NA_integer_,
  as.integer(samples$RepeatRaw)
)

samples$SurfaceName <- surface_map[samples$Surface]

n_unparsed <- sum(is.na(samples$Lab))
if (n_unparsed > 0) {
  warning(n_unparsed, " sample row(s) could not be parsed from SampleName. ",
          "These will be included with NA identifiers.")
  unparsed_names <- unique(samples$SampleName_trimmed[is.na(samples$Lab)])
  for (nm in unparsed_names) {
    warning("  Unparsed: '", nm, "'")
  }
}

# =========================================================
# 6. Calculate mass and recovery
# =========================================================
# Mass in sample (ng) = concentration (ng/uL) * SampleVol (uL) / Dilution
samples$petn_mass <- samples$petn_concentration * SampleVol / Dilution
samples$rdx_mass  <- samples$rdx_concentration * SampleVol / Dilution

# Drift-corrected mass
if ("petn_concentration_dc" %in% names(samples)) {
  samples$petn_mass_dc <- samples$petn_concentration_dc * SampleVol / Dilution
}

# Recovery (%) = (mass / deposit) / (extraction * filtration) * 100
samples$petn_recovery <- (samples$petn_mass / DepositMass) / 
                          (petn_extraction_efficiency * petn_filtration_efficiency) * 100
samples$rdx_recovery  <- (samples$rdx_mass / DepositMass) / 
                          (rdx_extraction_efficiency * rdx_filtration_efficiency) * 100

# Drift-corrected recovery
if ("petn_mass_dc" %in% names(samples)) {
  samples$petn_recovery_dc <- (samples$petn_mass_dc / DepositMass) / 
                               (petn_extraction_efficiency * petn_filtration_efficiency) * 100
}

# RDX drift-corrected mass (if columns exist)
if ("rdx_concentration_dc" %in% names(samples)) {
  samples$rdx_mass_dc <- samples$rdx_concentration_dc * SampleVol / Dilution
}

# RDX drift-corrected recovery
if ("rdx_mass_dc" %in% names(samples)) {
  samples$rdx_recovery_dc <- (samples$rdx_mass_dc / DepositMass) / 
                              (rdx_extraction_efficiency * rdx_filtration_efficiency) * 100
}

# =========================================================
# 7. Compute analysis_accepted column
# =========================================================
compute_analysis_accepted <- function(df) {
  
  accepted <- character(nrow(df))
  
  for (i in seq_len(nrow(df))) {
    
    # Negative controls are exempt
    if (!is.na(df$IsNegativeControl[i]) && df$IsNegativeControl[i]) {
      accepted[i] <- "NC"
      next
    }
    
    failures <- character(0)
    warnings_na <- character(0)
    
    # --- SNR checks ---
    petn_snr <- if ("petn_snr_flag" %in% names(df)) df$petn_snr_flag[i] else NA_character_
    rdx_snr  <- if ("rdx_snr_flag" %in% names(df)) df$rdx_snr_flag[i] else NA_character_
    
    if (!is.na(petn_snr) && petn_snr != "Quantifiable") {
      failures <- c(failures, paste0("PETN ", petn_snr))
    }
    if (!is.na(rdx_snr) && rdx_snr != "Quantifiable") {
      failures <- c(failures, paste0("RDX ", rdx_snr))
    }
    
    # --- Range checks ---
    petn_range <- if ("petn_range_flag" %in% names(df)) df$petn_range_flag[i] else NA_character_
    rdx_range  <- if ("rdx_range_flag" %in% names(df)) df$rdx_range_flag[i] else NA_character_
    
    if (!is.na(petn_range) && petn_range != "Within_range") {
      failures <- c(failures, paste0("PETN ", petn_range))
    }
    if (!is.na(rdx_range) && rdx_range != "Within_range") {
      failures <- c(failures, paste0("RDX ", rdx_range))
    }
    
    # --- QC bracket checks ---
    # 6ng QC brackets: FAIL disqualifies, NA warns
    # 0.2ng QC brackets: FAIL disqualifies, WARN triggers PASS*, NA warns
    qc_6ng_cols <- c("petn_qc_6ng_pre", "petn_qc_6ng_post",
                     "rdx_qc_6ng_pre", "rdx_qc_6ng_post")
    qc_6ng_labels <- c("PETN 6ng pre", "PETN 6ng post",
                       "RDX 6ng pre", "RDX 6ng post")
    
    qc_02ng_cols <- c("petn_qc_02ng_pre", "petn_qc_02ng_post",
                      "rdx_qc_02ng_pre", "rdx_qc_02ng_post")
    qc_02ng_labels <- c("PETN 0.2ng pre", "PETN 0.2ng post",
                        "RDX 0.2ng pre", "RDX 0.2ng post")
    
    # Check 6ng QCs (bias-based: PASS/FAIL/NA)
    for (j in seq_along(qc_6ng_cols)) {
      col_name <- qc_6ng_cols[j]
      if (col_name %in% names(df)) {
        val <- df[[col_name]][i]
        if (is.na(val)) {
          warnings_na <- c(warnings_na, paste0("No ", qc_6ng_labels[j], " QC"))
        } else if (val == "FAIL") {
          failures <- c(failures, paste0(qc_6ng_labels[j], " QC FAIL"))
        }
      }
    }
    
    # Check 0.2ng QCs (SNR-based: PASS/WARN/FAIL/NA)
    for (j in seq_along(qc_02ng_cols)) {
      col_name <- qc_02ng_cols[j]
      if (col_name %in% names(df)) {
        val <- df[[col_name]][i]
        if (is.na(val)) {
          warnings_na <- c(warnings_na, paste0("No ", qc_02ng_labels[j], " QC"))
        } else if (val == "FAIL") {
          failures <- c(failures, paste0(qc_02ng_labels[j], " QC FAIL"))
        } else if (val == "WARN") {
          warnings_na <- c(warnings_na, paste0(qc_02ng_labels[j], " QC low SNR"))
        }
      }
    }
    
    # --- Determine final value ---
    if (length(failures) > 0) {
      accepted[i] <- paste0("FAIL: ", paste(failures, collapse = "; "))
    } else if (length(warnings_na) > 0) {
      accepted[i] <- "PASS*"
    } else {
      accepted[i] <- "PASS"
    }
  }
  
  return(accepted)
}

samples$analysis_accepted <- compute_analysis_accepted(samples)

# Print summary of acceptance (excludes NCs from counts)
n_pass <- sum(samples$analysis_accepted == "PASS", na.rm = TRUE)
n_pass_star <- sum(samples$analysis_accepted == "PASS*", na.rm = TRUE)
n_fail <- sum(grepl("^FAIL:", samples$analysis_accepted), na.rm = TRUE)
n_nc <- sum(samples$analysis_accepted == "NC", na.rm = TRUE)
print(paste0("Analysis acceptance: PASS=", n_pass, " PASS*=", n_pass_star,
             " FAIL=", n_fail, " NC=", n_nc))

# =========================================================
# 7b. Compute NC status (separate evaluation for negative controls)
# =========================================================
# NC criteria:
#   PASS: Both PETN and RDX SNR flags = "Below_LOD" (no analyte detected)
#   WARN: Either/both = "Below_LOQ" (trace detected, not quantifiable)
#   FAIL: Either/both = "Quantifiable" (contamination present)
compute_nc_status <- function(df) {
  
  nc_status <- character(nrow(df))
  
  for (i in seq_len(nrow(df))) {
    petn_flag <- if ("petn_snr_flag" %in% names(df)) df$petn_snr_flag[i] else NA_character_
    rdx_flag  <- if ("rdx_snr_flag" %in% names(df)) df$rdx_snr_flag[i] else NA_character_
    
    # Classify each analyte
    petn_status <- if (is.na(petn_flag)) "clean" else
                   if (petn_flag == "Below_LOD") "clean" else
                   if (petn_flag == "Below_LOQ") "trace" else "contaminated"
    
    rdx_status <- if (is.na(rdx_flag)) "clean" else
                  if (rdx_flag == "Below_LOD") "clean" else
                  if (rdx_flag == "Below_LOQ") "trace" else "contaminated"
    
    # Determine combined status
    if (petn_status == "contaminated" && rdx_status == "contaminated") {
      nc_status[i] <- "NC FAIL: PETN + RDX contaminated"
    } else if (petn_status == "contaminated") {
      nc_status[i] <- "NC FAIL: PETN contaminated"
    } else if (rdx_status == "contaminated") {
      nc_status[i] <- "NC FAIL: RDX contaminated"
    } else if (petn_status == "trace" && rdx_status == "trace") {
      nc_status[i] <- "NC WARN: PETN + RDX trace detected"
    } else if (petn_status == "trace") {
      nc_status[i] <- "NC WARN: PETN trace detected"
    } else if (rdx_status == "trace") {
      nc_status[i] <- "NC WARN: RDX trace detected"
    } else {
      nc_status[i] <- "NC PASS"
    }
  }
  
  return(nc_status)
}

# Apply NC status to NC rows
nc_rows <- samples %>% filter(IsNegativeControl)
if (nrow(nc_rows) > 0) {
  nc_rows$nc_status <- compute_nc_status(nc_rows)
  
  n_nc_pass <- sum(nc_rows$nc_status == "NC PASS", na.rm = TRUE)
  n_nc_warn <- sum(grepl("^NC WARN:", nc_rows$nc_status), na.rm = TRUE)
  n_nc_fail <- sum(grepl("^NC FAIL:", nc_rows$nc_status), na.rm = TRUE)
  print(paste0("NC evaluation: PASS=", n_nc_pass, " WARN=", n_nc_warn, " FAIL=", n_nc_fail))
} else {
  n_nc_pass <- 0
  n_nc_warn <- 0
  n_nc_fail <- 0
}

# =========================================================
# 8. Select curated columns
# =========================================================
# --- Sample columns (excludes NCs) ---
desired_cols <- c(
  # Study identifiers
  "Lab", "Participant", "Surface", "SurfaceName", "Repeat",
  # SNR flags
  "petn_snr_flag", "rdx_snr_flag",
  # Range flags
  "petn_range_flag", "rdx_range_flag",
  # Concentrations
  "petn_concentration_dc", "rdx_concentration",
  # Recovery
  "petn_recovery_dc", "rdx_recovery",
  # QC bracket flags
  "petn_qc_6ng_pre", "petn_qc_6ng_post",
  "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
  "petn_qc_02ng_pre", "petn_qc_02ng_post",
  "rdx_qc_02ng_pre", "rdx_qc_02ng_post",
  # Overall acceptance
  "analysis_accepted",
  # Source tracing
  "Date", "SampleName", "DataFile", "SourceFile"
)

# --- NC columns (separate sheet, no QC brackets) ---
desired_cols_nc <- c(
  # Study identifiers
  "Lab", "Participant", "Surface", "SurfaceName",
  # SNR flags (key diagnostic for NCs)
  "petn_snr_flag", "rdx_snr_flag",
  # Range flags
  "petn_range_flag", "rdx_range_flag",
  # Concentrations (shows what level if detected)
  "petn_concentration_dc", "rdx_concentration",
  # Recovery
  "petn_recovery_dc", "rdx_recovery",
  # NC-specific status
  "nc_status",
  # Source tracing
  "Date", "SampleName", "DataFile", "SourceFile"
)

# --- Build samples-only master (no NCs) ---
samples_only <- samples %>% filter(!IsNegativeControl)

available_cols <- intersect(desired_cols, names(samples_only))
missing_cols <- setdiff(desired_cols, names(samples_only))
if (length(missing_cols) > 0) {
  message("Note: the following columns were not found and will be omitted: ",
          paste(missing_cols, collapse = ", "))
}

master <- samples_only %>%
  select(all_of(available_cols)) %>%
  arrange(Lab, Participant, Surface, Repeat)

# --- Build NC master ---
if (nrow(nc_rows) > 0) {
  available_cols_nc <- intersect(desired_cols_nc, names(nc_rows))
  master_ncs <- nc_rows %>%
    select(all_of(available_cols_nc)) %>%
    arrange(Lab, Participant, Surface)
} else {
  master_ncs <- data.frame()
}

# =========================================================
# 9. Export master CSV
# =========================================================
csv_path <- file.path(study_root_dir, "FINEX_StudyResults.csv")
write.csv(master, csv_path, row.names = FALSE)
print(paste0("Master CSV written: ", csv_path))

# =========================================================
# 10. Build per-surface sheets
# =========================================================
sheet_list <- list()
sheet_list[["All Data"]] <- master

for (surf_code in names(surface_map)) {
  surf_name <- surface_map[surf_code]
  surf_data <- master %>% filter(Surface == surf_code)
  if (nrow(surf_data) > 0) {
    sheet_list[[surf_name]] <- surf_data
  }
}

# Add Negative Controls sheet
if (nrow(master_ncs) > 0) {
  sheet_list[["Negative Controls"]] <- master_ncs
}

# =========================================================
# 11. Build Summary sheet
# =========================================================
compute_stats <- function(df, group_vars) {
  # Use recovery columns (drift-corrected preferred)
  petn_rec_col <- if ("petn_recovery_dc" %in% names(df)) "petn_recovery_dc" else
                  if ("petn_recovery" %in% names(df)) "petn_recovery" else NA_character_
  rdx_rec_col  <- if ("rdx_recovery" %in% names(df)) "rdx_recovery" else
                  if ("rdx_recovery_dc" %in% names(df)) "rdx_recovery_dc" else NA_character_
  
  # If no recovery columns, return empty

  if (is.na(petn_rec_col) && is.na(rdx_rec_col)) return(data.frame())
  
  # Ensure columns exist
  if (is.na(petn_rec_col)) { df$petn_recovery_placeholder <- NA_real_; petn_rec_col <- "petn_recovery_placeholder" }
  if (is.na(rdx_rec_col))  { df$rdx_recovery_placeholder <- NA_real_;  rdx_rec_col  <- "rdx_recovery_placeholder" }
  
  df %>%
    filter(!is.na(Lab)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_PETN = sum(!is.na(!!sym(petn_rec_col))),
      PETN_Mean_recovery = mean(!!sym(petn_rec_col), na.rm = TRUE),
      PETN_SD_recovery   = sd(!!sym(petn_rec_col), na.rm = TRUE),
      PETN_RSD_pct = ifelse(
        PETN_Mean_recovery != 0 & !is.na(PETN_Mean_recovery),
        (PETN_SD_recovery / PETN_Mean_recovery) * 100,
        NA_real_
      ),
      n_RDX = sum(!is.na(!!sym(rdx_rec_col))),
      RDX_Mean_recovery = mean(!!sym(rdx_rec_col), na.rm = TRUE),
      RDX_SD_recovery   = sd(!!sym(rdx_rec_col), na.rm = TRUE),
      RDX_RSD_pct = ifelse(
        RDX_Mean_recovery != 0 & !is.na(RDX_Mean_recovery),
        (RDX_SD_recovery / RDX_Mean_recovery) * 100,
        NA_real_
      ),
      .groups = "drop"
    )
}

has_petn_rec <- "petn_recovery_dc" %in% names(master) || "petn_recovery" %in% names(master)
has_rdx_rec  <- "rdx_recovery" %in% names(master)

if (has_petn_rec || has_rdx_rec) {
  
  # Ensure columns exist for compute_stats
  if (!("petn_recovery_dc" %in% names(master)) && !("petn_recovery" %in% names(master))) {
    master$petn_recovery <- NA_real_
  }
  if (!("rdx_recovery" %in% names(master))) {
    master$rdx_recovery <- NA_real_
  }

  summary_participant <- compute_stats(
    master,
    c("Lab", "Participant", "Surface", "SurfaceName")
  )

  summary_surface <- compute_stats(
    master,
    c("Lab", "Surface", "SurfaceName")
  )

  summary_lab <- compute_stats(
    master,
    c("Lab")
  )

  summary_participant$Level <- "By Participant"
  summary_surface$Level     <- "By Surface"
  summary_lab$Level         <- "By Lab"

  all_summary_cols <- c("Level", "Lab", "Participant", "Surface", "SurfaceName",
                        "n_PETN", "PETN_Mean_recovery", "PETN_SD_recovery", "PETN_RSD_pct",
                        "n_RDX", "RDX_Mean_recovery", "RDX_SD_recovery", "RDX_RSD_pct")

  for (col in all_summary_cols) {
    if (!(col %in% names(summary_participant))) summary_participant[[col]] <- NA
    if (!(col %in% names(summary_surface)))     summary_surface[[col]]     <- NA
    if (!(col %in% names(summary_lab)))         summary_lab[[col]]         <- NA
  }

  summary_combined <- bind_rows(
    summary_participant %>% select(all_of(all_summary_cols)),
    summary_surface     %>% select(all_of(all_summary_cols)),
    summary_lab         %>% select(all_of(all_summary_cols))
  ) %>%
    arrange(Level, Lab, Participant, Surface)

  sheet_list[["Summary"]] <- summary_combined

} else {
  warning("No recovery columns found. ",
          "Summary sheet will not be generated. ",
          "Run 03_Quantification.R with calibration data first.")
}

# =========================================================
# 12. Export Excel workbook with conditional formatting
# =========================================================
xlsx_path <- file.path(study_root_dir, "FINEX_StudyResults.xlsx")

# Define styles
style_green  <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
style_amber  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C5700")
style_red    <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
style_grey   <- createStyle(fgFill = "#D9D9D9", fontColour = "#000000")
style_blue   <- createStyle(bgFill = "#BDD7EE", fontColour = "#1F4E79")
style_header <- createStyle(textDecoration = "Bold", border = "Bottom",
                            borderColour = "#000000", fgFill = "#4472C4",
                            fontColour = "#FFFFFF", halign = "center")

# Function to apply conditional formatting to a sheet
apply_formatting <- function(wb, sheet_name, df) {
  
  n_rows <- nrow(df)
  if (n_rows == 0) return()
  
  col_names <- names(df)
  data_rows <- 2:(n_rows + 1)  # +1 for header row
  
  # --- SNR flag columns: Quantifiable=green, Below_LOQ=amber, Below_LOD=red ---
  snr_cols <- intersect(c("petn_snr_flag", "rdx_snr_flag"), col_names)
  for (col in snr_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Quantifiable", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Below_LOQ", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Below_LOD", style = style_red)
  }
  
  # --- Range flag columns: Within_range=green, Below_range=amber, Above_range=blue ---
  range_cols <- intersect(c("petn_range_flag", "rdx_range_flag"), col_names)
  for (col in range_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Within_range", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Below_range", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "Above_range", style = style_blue)
  }
  
  # --- QC bracket columns: PASS=green, WARN=amber, FAIL=red ---
  qc_cols <- intersect(c("petn_qc_6ng_pre", "petn_qc_6ng_post",
                          "rdx_qc_6ng_pre", "rdx_qc_6ng_post",
                          "petn_qc_02ng_pre", "petn_qc_02ng_post",
                          "rdx_qc_02ng_pre", "rdx_qc_02ng_post"), col_names)
  for (col in qc_cols) {
    col_idx <- which(col_names == col)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "PASS", style = style_green)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "WARN", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "FAIL", style = style_red)
  }
  
  # --- Analysis accepted column: PASS=green, PASS*=amber, FAIL:=red ---
  if ("analysis_accepted" %in% col_names) {
    col_idx <- which(col_names == "analysis_accepted")
    # Order matters: more specific rules first
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "FAIL:", style = style_red)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "PASS*", style = style_amber)
    # PASS last (since PASS* and PASS both contain "PASS")
    # Use expression-based rule for exact PASS
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "expression",
                          rule = paste0('EXACT(', int2col(col_idx), '2,"PASS")'),
                          style = style_green)
  }
  
  # --- NC status column: NC PASS=green, NC WARN=amber, NC FAIL=red ---
  if ("nc_status" %in% col_names) {
    col_idx <- which(col_names == "nc_status")
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "NC FAIL:", style = style_red)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "NC WARN:", style = style_amber)
    conditionalFormatting(wb, sheet_name, cols = col_idx, rows = data_rows,
                          type = "contains", rule = "NC PASS", style = style_green)
  }
  
  # --- Apply NA grey formatting to QC bracket columns ---
  # openxlsx does not have a native "is blank/NA" conditional format,
  # so we write NA cells with direct styling after data write
  for (col in qc_cols) {
    col_idx <- which(col_names == col)
    na_rows <- which(is.na(df[[col]])) + 1  # +1 for header
    if (length(na_rows) > 0) {
      addStyle(wb, sheet_name, style = style_grey, rows = na_rows, cols = col_idx,
               gridExpand = TRUE, stack = TRUE)
    }
  }
}

# Create workbook
wb <- createWorkbook()

for (sheet_name in names(sheet_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheet_list[[sheet_name]], headerStyle = style_header,
            keepNA = TRUE, na.string = "NA")
  
  # Auto-size columns
  setColWidths(wb, sheet_name, cols = seq_along(names(sheet_list[[sheet_name]])),
               widths = "auto")
  
  # Apply conditional formatting (skip Summary sheet which has different columns)
  if (sheet_name != "Summary") {
    apply_formatting(wb, sheet_name, sheet_list[[sheet_name]])
  }
}

# Save workbook
saveWorkbook(wb, xlsx_path, overwrite = TRUE)
print(paste0("Excel workbook written (with formatting): ", xlsx_path))

# =========================================================
# 13. Generate Box Plots
# =========================================================

library(ggplot2)

# Create output directories
plots_dir <- file.path(study_root_dir, "Plots")
plots_overall_dir <- file.path(plots_dir, "Overall")
dir.create(plots_overall_dir, recursive = TRUE, showWarnings = FALSE)

# Prepare data: use drift-corrected recovery if available
# Note: master already excludes NCs (they are in master_ncs)
recovery_data <- master %>%
  filter(!is.na(Lab)) %>%
  select(Lab, Participant, Surface, SurfaceName, 
         any_of(c("petn_recovery_dc", "rdx_recovery", "petn_recovery", "rdx_recovery_dc")))

# Determine which recovery columns to use
if ("petn_recovery_dc" %in% names(recovery_data)) {
  recovery_data$petn_rec <- recovery_data$petn_recovery_dc
} else if ("petn_recovery" %in% names(recovery_data)) {
  recovery_data$petn_rec <- recovery_data$petn_recovery
} else {
  recovery_data$petn_rec <- NA_real_
}

if ("rdx_recovery" %in% names(recovery_data)) {
  recovery_data$rdx_rec <- recovery_data$rdx_recovery
} else if ("rdx_recovery_dc" %in% names(recovery_data)) {
  recovery_data$rdx_rec <- recovery_data$rdx_recovery_dc
} else {
  recovery_data$rdx_rec <- NA_real_
}

# Define fixed surface order for per-lab plots
fixed_surface_order <- c("Steel", "Glass", "ABS-Smooth", "ABS-Textured")

# Function to create box plot with mean diamond
create_boxplot <- function(data, x_var, y_var, x_label, y_label, title, 
                           x_order = NULL) {
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_boxplot(outlier.shape = 1, outlier.size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
                 fill = "red", color = "black") +
    labs(x = x_label, y = y_label, title = title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
  
  if (!is.null(x_order)) {
    p <- p + scale_x_discrete(limits = x_order)
  }
  
  return(p)
}

# --- Overall Study Plots ---
# Order surfaces by median PETN recovery
surface_order_petn <- recovery_data %>%
  filter(!is.na(petn_rec)) %>%
  group_by(SurfaceName) %>%
  summarise(median_rec = median(petn_rec, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_rec) %>%
  pull(SurfaceName)

surface_order_rdx <- recovery_data %>%
  filter(!is.na(rdx_rec)) %>%
  group_by(SurfaceName) %>%
  summarise(median_rec = median(rdx_rec, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_rec) %>%
  pull(SurfaceName)

# Overall PETN plot
if (length(surface_order_petn) > 0) {
  p_overall_petn <- create_boxplot(
    data = recovery_data %>% filter(!is.na(petn_rec)),
    x_var = "SurfaceName",
    y_var = "petn_rec",
    x_label = "Surface",
    y_label = "PETN Recovery (%)",
    title = "PETN Recovery by Surface (All Labs)",
    x_order = surface_order_petn
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_PETN_Recovery.png"),
    p_overall_petn,
    width = 6, height = 5, dpi = 300
  )
}

# Overall RDX plot
if (length(surface_order_rdx) > 0) {
  p_overall_rdx <- create_boxplot(
    data = recovery_data %>% filter(!is.na(rdx_rec)),
    x_var = "SurfaceName",
    y_var = "rdx_rec",
    x_label = "Surface",
    y_label = "RDX Recovery (%)",
    title = "RDX Recovery by Surface (All Labs)",
    x_order = surface_order_rdx
  )
  
  ggsave(
    file.path(plots_overall_dir, "Overall_RDX_Recovery.png"),
    p_overall_rdx,
    width = 6, height = 5, dpi = 300
  )
}

print("Overall study plots generated.")

# --- Per-Lab Plots ---
labs_with_data <- recovery_data %>%
  filter(!is.na(Lab)) %>%
  distinct(Lab) %>%
  arrange(Lab) %>%
  pull(Lab)

for (lab_id in labs_with_data) {
  
  lab_data <- recovery_data %>% filter(Lab == lab_id)
  lab_str <- sprintf("Lab%02d", lab_id)
  lab_dir <- file.path(plots_dir, lab_str)
  dir.create(lab_dir, showWarnings = FALSE)
  
  # Type A: Participant x Surface detailed plots
  lab_data_a <- lab_data %>%
    mutate(ParticipantSurface = paste0("P", Participant, " ", SurfaceName))
  
  # PETN detailed
  if (any(!is.na(lab_data_a$petn_rec))) {
    p_detail_petn <- create_boxplot(
      data = lab_data_a %>% filter(!is.na(petn_rec)),
      x_var = "ParticipantSurface",
      y_var = "petn_rec",
      x_label = "Participant - Surface",
      y_label = "PETN Recovery (%)",
      title = paste(lab_str, "- PETN Recovery by Participant & Surface")
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_PETN_ParticipantSurface.png")),
      p_detail_petn,
      width = 10, height = 5, dpi = 300
    )
  }
  
  # RDX detailed
  if (any(!is.na(lab_data_a$rdx_rec))) {
    p_detail_rdx <- create_boxplot(
      data = lab_data_a %>% filter(!is.na(rdx_rec)),
      x_var = "ParticipantSurface",
      y_var = "rdx_rec",
      x_label = "Participant - Surface",
      y_label = "RDX Recovery (%)",
      title = paste(lab_str, "- RDX Recovery by Participant & Surface")
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_RDX_ParticipantSurface.png")),
      p_detail_rdx,
      width = 10, height = 5, dpi = 300
    )
  }
  
  # Type B: Surface summary plots (pooled across participants)
  # PETN surface summary
  if (any(!is.na(lab_data$petn_rec))) {
    p_surface_petn <- create_boxplot(
      data = lab_data %>% filter(!is.na(petn_rec)),
      x_var = "SurfaceName",
      y_var = "petn_rec",
      x_label = "Surface",
      y_label = "PETN Recovery (%)",
      title = paste(lab_str, "- PETN Recovery by Surface"),
      x_order = fixed_surface_order
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_PETN_SurfaceSummary.png")),
      p_surface_petn,
      width = 6, height = 5, dpi = 300
    )
  }
  
  # RDX surface summary
  if (any(!is.na(lab_data$rdx_rec))) {
    p_surface_rdx <- create_boxplot(
      data = lab_data %>% filter(!is.na(rdx_rec)),
      x_var = "SurfaceName",
      y_var = "rdx_rec",
      x_label = "Surface",
      y_label = "RDX Recovery (%)",
      title = paste(lab_str, "- RDX Recovery by Surface"),
      x_order = fixed_surface_order
    )
    
    ggsave(
      file.path(lab_dir, paste0(lab_str, "_RDX_SurfaceSummary.png")),
      p_surface_rdx,
      width = 6, height = 5, dpi = 300
    )
  }
  
  print(paste0("Plots generated for ", lab_str))
}

print(paste0("All box plots saved to: ", plots_dir))

# =========================================================
# 14. Final summary to console
# =========================================================
n_labs <- n_distinct(master$Lab, na.rm = TRUE)
n_participants <- master %>%
  filter(!is.na(Lab)) %>%
  distinct(Lab, Participant) %>%
  nrow()
n_surfaces <- n_distinct(master$Surface, na.rm = TRUE)
n_samples <- nrow(master)
n_ncs <- nrow(master_ncs)

print(paste0(""))
print(paste0("=== FINEX Study Collation Complete ==="))
print(paste0("  Labs:                 ", n_labs))
print(paste0("  Lab-Participant sets: ", n_participants))
print(paste0("  Surface types:        ", n_surfaces))
print(paste0("  Sample repeats:       ", n_samples))
print(paste0("  Negative controls:    ", n_ncs))
print(paste0("  --- Sample Acceptance ---"))
print(paste0("  Accepted (PASS):      ", n_pass))
print(paste0("  Accepted (PASS*):     ", n_pass_star))
print(paste0("  Rejected (FAIL):      ", n_fail))
print(paste0("  --- NC Status ---"))
print(paste0("  NC Clean (PASS):      ", n_nc_pass))
print(paste0("  NC Trace (WARN):      ", n_nc_warn))
print(paste0("  NC Contaminated:      ", n_nc_fail))
print(paste0("  --- Outputs ---"))
print(paste0("  Output CSV:  ", csv_path))
print(paste0("  Output XLSX: ", xlsx_path))
print(paste0("  Output Plots: ", plots_dir))
print(paste0("======================================"))
