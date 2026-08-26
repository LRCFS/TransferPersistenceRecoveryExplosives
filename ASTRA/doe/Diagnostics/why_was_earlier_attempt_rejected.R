#===============================================================================
# why_was_earlier_attempt_rejected.R
#
# For each of the 17 reanalysed pilot samples, determine WHY the non-selected
# (typically earlier) attempt(s) failed QC -- was the failure reason specific
# to RDX itself (meaning the earlier RDX quant really was less trustworthy),
# or specific to PETN/IS/an unrelated check (meaning the earlier attempt's
# OWN RDX signal may have been fine, and it was only reanalysed because of a
# problem that has nothing to do with RDX accuracy)?
#
# READ-ONLY: replicates pilot_analysis.R's collate_gcms_results() Sections
# 1-7 (discovery -> blank eval -> QC brackets -> classification -> recovery
# calc -> design merge -> compute_injection_acceptance()) up to the point
# BEFORE select_best_attempt() dedup and BEFORE any file writing. Never
# touches any live study file. Sources the same shared
# GCMSQuantitation/Code/InjectionAcceptance.R used by the real pipeline, so
# results are byte-for-byte consistent with what pilot_data_nested.csv itself
# was computed from.
#===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Code/InjectionAcceptance.R")

data_dir    <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"
output_dir  <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
gc_data_dir <- file.path(output_dir, "GC Data")
design_file       <- file.path(data_dir, "pilot_design_nested.csv")
batch_design_file <- file.path(data_dir, "pilot_design_batch_organized.csv")
qc_6ng_bias_limit <- 20
sample_vol_uL       <- 1000
deposit_mass_ng     <- 10000
ideal_concentration <- deposit_mass_ng / sample_vol_uL

# --- Section 1: discover + load ---
results_files <- list.files(gc_data_dir, pattern = "_GCMSResults\\.csv$", full.names = TRUE, recursive = TRUE)
results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]

all_data <- bind_rows(lapply(results_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$SourceFile <- f
  df$Dataset <- basename(dirname(dirname(f)))
  df
}))

# --- Section 3: blank eval + QC brackets ---
petn_bias_col <- if ("petn_percent_bias_dc" %in% names(all_data)) "petn_percent_bias_dc" else "petn_percent_bias"
rdx_bias_col  <- if ("rdx_percent_bias" %in% names(all_data)) "rdx_percent_bias" else "rdx_percent_bias_dc"

all_data <- evaluate_blanks(all_data)
all_data <- assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col, qc_6ng_bias_limit)

# --- Section 4: classify sample rows ---
samples <- all_data %>% filter(Type == "Sample")
samples$SampleName_trimmed <- str_trim(samples$SampleName)

pilot_pattern <- "^PILOT_(\\d{3})$"
nc_pattern    <- "^(?:PILOT_)?(Steel|ABS)Batch(\\d+)[ _]?(?:NegCtrl|NC)$"
samples$IsPilotSample     <- grepl(pilot_pattern, samples$SampleName_trimmed)
samples$IsNegativeControl <- grepl(nc_pattern, samples$SampleName_trimmed, ignore.case = TRUE)
samples <- samples %>% filter(IsPilotSample | IsNegativeControl)
samples$AnalysisFolder <- dirname(dirname(samples$SourceFile))

# --- Section 5: recovery calc (not strictly needed here, but keep for parity) ---
petn_snr_flag_col <- samples$petn_snr_flag
rdx_snr_flag_col  <- samples$rdx_snr_flag
petn_treat_as_zero <- petn_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
samples$PETN_Recovery_pct <- ifelse(petn_treat_as_zero, 0,
  ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc > 0,
         samples$petn_concentration_dc / ideal_concentration * 100,
         ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc <= 0, 0, NA_real_)))
rdx_treat_as_zero <- rdx_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
samples$RDX_Recovery_pct <- ifelse(rdx_treat_as_zero, 0,
  ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration > 0,
         samples$rdx_concentration / ideal_concentration * 100,
         ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration <= 0, 0, NA_real_)))

# --- Section 6: merge with design (only needed to build pilot_rows exactly
#     as the real pipeline does; RunID = SampleName_trimmed for pilot rows) ---
pilot_rows <- samples %>% filter(IsPilotSample)
nc_rows_raw <- samples %>% filter(IsNegativeControl)

combined_for_accept <- bind_rows(
  pilot_rows %>% mutate(RowType = "Sample"),
  nc_rows_raw %>% mutate(RowType = "NC")
)

# --- Section 7: compute acceptance (the real per-attempt PASS/FAIL, with reasons) ---
combined_for_accept <- compute_injection_acceptance(combined_for_accept)

pilot_rows <- combined_for_accept %>% filter(RowType == "Sample")

# --- Report: for each of the 17 known reanalysed samples, show every
#     attempt's analysis_accepted (with fail reason), ordered chronologically ---
multi_ids <- c("PILOT_002","PILOT_003","PILOT_004","PILOT_006","PILOT_012",
               "PILOT_013","PILOT_015","PILOT_017","PILOT_019","PILOT_020",
               "PILOT_021","PILOT_022","PILOT_023","PILOT_026","PILOT_027",
               "PILOT_030","PILOT_032")

report <- pilot_rows %>%
  filter(SampleName_trimmed %in% multi_ids) %>%
  arrange(SampleName_trimmed, Date, Line) %>%
  select(SampleName_trimmed, Date, Dataset, Line, analysis_accepted, Outcome,
         petn_snr_flag, rdx_snr_flag, rdx_is_pa_flag,
         rdx_qc_6ng_pre, rdx_qc_6ng_post, rdx_qc_02ng_pre, rdx_qc_02ng_post,
         RDX_Recovery_pct, PETN_Recovery_pct)

cat("========== Per-attempt analysis_accepted (with fail reasons), all 17 reanalysed samples ==========\n")
print(as.data.frame(report), row.names = FALSE)

cat("\n\n========== Classify each REJECTED (non-selected) attempt's failure reason ==========\n")
# Selected attempt per the real pipeline's own rule: prefer PASS/PASS* over
# FAIL, tie-break by most recent Date (mirrors select_best_attempt() exactly)
sel <- report %>%
  mutate(quality_ok = analysis_accepted %in% c("PASS","PASS*")) %>%
  arrange(SampleName_trimmed, desc(quality_ok), desc(Date)) %>%
  group_by(SampleName_trimmed) %>%
  mutate(is_selected = row_number() == 1) %>%
  ungroup()

rejected <- sel %>% filter(!is_selected)
cat("\nRejected attempts and their failure reason + RDX's OWN bracket status (independent of PETN):\n")
print(as.data.frame(rejected %>% select(SampleName_trimmed, Date, Dataset, analysis_accepted,
                                          rdx_qc_6ng_pre, rdx_qc_6ng_post, rdx_qc_02ng_pre, rdx_qc_02ng_post)),
      row.names = FALSE)

cat("\n\n========== Summary: how many rejected attempts failed for an RDX-specific reason vs something else? ==========\n")
rejected$FailCategory <- case_when(
  grepl("^PASS", rejected$analysis_accepted) ~ "Accepted (rejected only by tie-break, not a real failure)",
  grepl("^FAIL: RDX", rejected$analysis_accepted) ~ "RDX-specific failure",
  grepl("^FAIL: PETN", rejected$analysis_accepted) ~ "PETN-specific failure",
  grepl("^FAIL: Injection failure \\(IS", rejected$analysis_accepted) ~ "IS/injection failure",
  TRUE ~ "Other"
)
print(table(rejected$FailCategory))
