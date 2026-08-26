# =========================================================
# Confirmatory (Qualifier) Ion Ratio Analysis -- Stage 2
# =========================================================
# Added Aug 2026 -- see CONTEXT.md "Confirmatory Ion Detection" session.
#
# Purpose: derive an empirical expected qualifier/quantifier ion ratio
# (and its natural variability) for PETN (qualifiers m/z 57, 76) and RDX
# (qualifiers m/z 46, 75), using the calibration standard injections
# (known-pure identity, the gold-standard reference population) across
# all 11 "Accepted Analysis" datasets -- now that Stage 1 has populated
# petn_qual57_pa/petn_qual76_pa/rdx_qual46_pa/rdx_qual75_pa (and PH/SNR/
# flag columns) in every dataset's *_GCMSResults.csv.
#
# Deliberately reads the already-produced *_GCMSResults.csv files
# directly, rather than sourcing GlobalCode.R/04_CollateStudyResults.R,
# to avoid the documented risk of an unconditional GlobalCode.R source()
# re-triggering full raw-data reprocessing of whatever dataset
# `DataFolder` currently points to.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

base_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"
datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1", "Lab29", "Lab33",
              "Lab9", "Steel-1", "Steel 2", "Steel 3", "Steel 4")

read_one <- function(ds) {
  f <- file.path(base_dir, ds, "Results", paste0(ds, "_GCMSResults.csv"))
  if (!file.exists(f)) {
    warning("Missing results file for ", ds)
    return(NULL)
  }
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Dataset <- ds
  df
}

all_data <- bind_rows(lapply(datasets, read_one))
message("Loaded ", nrow(all_data), " total rows across ", length(datasets), " datasets.")

# =========================================================
# Compute ion ratios (qualifier PA / quantifier PA) for every row where
# both the quantifier and qualifier PA are present and the quantifier
# PA is > 0 (avoid divide-by-zero/NA propagation).
# =========================================================
safe_ratio <- function(qual_pa, quant_pa) {
  ifelse(!is.na(qual_pa) & !is.na(quant_pa) & quant_pa > 0, qual_pa / quant_pa, NA_real_)
}

all_data <- all_data %>%
  mutate(
    petn_qual57_ratio = safe_ratio(petn_qual57_pa, petn_pa),
    petn_qual76_ratio = safe_ratio(petn_qual76_pa, petn_pa),
    rdx_qual46_ratio  = safe_ratio(rdx_qual46_pa, rdx_pa),
    rdx_qual75_ratio  = safe_ratio(rdx_qual75_pa, rdx_pa)
  )

# =========================================================
# Section A: Calibration standards (Type == "Cal") -- the gold-standard
# known-pure-identity reference population for deriving expected ratios.
# =========================================================
cals <- all_data %>% filter(Type == "Cal")
message("\n=== Calibration standard rows: ", nrow(cals), " ===")

ratio_cols <- c("petn_qual57_ratio", "petn_qual76_ratio", "rdx_qual46_ratio", "rdx_qual75_ratio")
quant_snr_cols <- list(petn_qual57_ratio = "petn_qual57_snr_flag",
                        petn_qual76_ratio = "petn_qual76_snr_flag",
                        rdx_qual46_ratio  = "rdx_qual46_snr_flag",
                        rdx_qual75_ratio  = "rdx_qual75_snr_flag")

# --- A1: Detection rate of each qualifier among Cal standards ---
message("\n--- Qualifier detection rate among Cal standards ---")
det_summary <- data.frame(
  Qualifier = ratio_cols,
  N_Cal_Total = nrow(cals),
  N_Detected = sapply(ratio_cols, function(c) sum(!is.na(cals[[c]]))),
  N_Quantifiable = sapply(quant_snr_cols, function(flagcol) sum(cals[[flagcol]] == "Quantifiable", na.rm = TRUE))
)
det_summary$Pct_Detected <- round(100 * det_summary$N_Detected / det_summary$N_Cal_Total, 1)
det_summary$Pct_Quantifiable <- round(100 * det_summary$N_Quantifiable / det_summary$N_Cal_Total, 1)
print(det_summary, row.names = FALSE)

# --- A2: Ratio stats by CalLevel (to check concentration-dependence) ---
message("\n--- Ratio mean/SD/CV%% by CalLevel (pooled across all datasets) ---")
for (rc in ratio_cols) {
  cat("\n", rc, ":\n", sep = "")
  by_level <- cals %>%
    filter(!is.na(.data[[rc]])) %>%
    group_by(CalLevel) %>%
    summarise(
      n = n(),
      mean = mean(.data[[rc]], na.rm = TRUE),
      sd = sd(.data[[rc]], na.rm = TRUE),
      cv_pct = 100 * sd / mean,
      .groups = "drop"
    ) %>%
    arrange(CalLevel)
  print(as.data.frame(by_level), row.names = FALSE)
}

# --- A3: Pooled stats (all Cal levels combined) -- candidate "flat tolerance" ---
message("\n--- Pooled ratio stats (ALL Cal levels combined) ---")
pooled_stats <- data.frame(
  Qualifier = ratio_cols,
  N = sapply(ratio_cols, function(c) sum(!is.na(cals[[c]]))),
  Mean = sapply(ratio_cols, function(c) mean(cals[[c]], na.rm = TRUE)),
  SD = sapply(ratio_cols, function(c) sd(cals[[c]], na.rm = TRUE)),
  Min = sapply(ratio_cols, function(c) suppressWarnings(min(cals[[c]], na.rm = TRUE))),
  Max = sapply(ratio_cols, function(c) suppressWarnings(max(cals[[c]], na.rm = TRUE)))
)
pooled_stats$CV_pct <- round(100 * pooled_stats$SD / pooled_stats$Mean, 1)
print(pooled_stats, row.names = FALSE)

# --- A4: Concentration-dependence check (Pearson correlation, ratio vs CalLevel) ---
message("\n--- Correlation between ratio and CalLevel (concentration-dependence check) ---")
for (rc in ratio_cols) {
  d <- cals %>% filter(!is.na(.data[[rc]]), !is.na(CalLevel))
  if (nrow(d) >= 3 && length(unique(d$CalLevel)) >= 2) {
    ct <- suppressWarnings(cor.test(d$CalLevel, d[[rc]], method = "pearson"))
    cat(sprintf("  %-20s r = %6.3f  p = %.4g  (n=%d)\n", rc, ct$estimate, ct$p.value, nrow(d)))
  } else {
    cat(sprintf("  %-20s insufficient data\n", rc))
  }
}

# --- A5: Restrict to the "Quantifiable" SNR tier only (both quantifier
#     AND qualifier Quantifiable) -- the actual population the ion-ratio
#     gate will be applied to per the agreed design (Stage 4).
message("\n--- Ratio stats restricted to Quantifiable tier (both quantifier & qualifier SNR = Quantifiable) ---")
quant_flag_for <- list(petn_qual57_ratio = "petn_snr_flag", petn_qual76_ratio = "petn_snr_flag",
                        rdx_qual46_ratio = "rdx_snr_flag", rdx_qual75_ratio = "rdx_snr_flag")
quant_tier_stats <- data.frame()
for (rc in ratio_cols) {
  own_flag <- quant_flag_for[[rc]]
  qual_flag <- quant_snr_cols[[rc]]
  d <- cals %>% filter(.data[[own_flag]] == "Quantifiable", .data[[qual_flag]] == "Quantifiable", !is.na(.data[[rc]]))
  quant_tier_stats <- rbind(quant_tier_stats, data.frame(
    Qualifier = rc, N = nrow(d),
    Mean = if (nrow(d) > 0) mean(d[[rc]]) else NA,
    SD = if (nrow(d) > 1) sd(d[[rc]]) else NA
  ))
}
quant_tier_stats$CV_pct <- round(100 * quant_tier_stats$SD / quant_tier_stats$Mean, 1)
print(quant_tier_stats, row.names = FALSE)

# =========================================================
# Save outputs
# =========================================================
out_dir <- "Diagnostics"
write.csv(cals %>% select(Dataset, Line, SampleName, Type, CalLevel,
                           petn_pa, petn_qual57_pa, petn_qual76_pa, petn_qual57_ratio, petn_qual76_ratio,
                           rdx_pa, rdx_qual46_pa, rdx_qual75_pa, rdx_qual46_ratio, rdx_qual75_ratio),
          file.path(out_dir, "ConfirmatoryIon_CalRatios_RawData.csv"), row.names = FALSE)
write.csv(pooled_stats, file.path(out_dir, "ConfirmatoryIon_PooledRatioStats.csv"), row.names = FALSE)
write.csv(quant_tier_stats, file.path(out_dir, "ConfirmatoryIon_QuantifiableTierRatioStats.csv"), row.names = FALSE)

message("\nSaved: Diagnostics/ConfirmatoryIon_CalRatios_RawData.csv, _PooledRatioStats.csv, _QuantifiableTierRatioStats.csv")
message("\n=== Stage 2 analysis complete ===")
