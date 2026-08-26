# =========================================================
# Confirmatory (Qualifier) Ion Detection Analysis -- Stage 3
# =========================================================
# Added Aug 2026 -- see CONTEXT.md "Confirmatory Ion Detection" session.
#
# Revised design (per explicit decision after Stage 2 findings):
#   - Ratio-based confirmation abandoned. RDX's qualifier/quantifier
#     ratio showed strong, real concentration-dependence (Stage 2:
#     r=-0.72/-0.69, p<1e-10 for m/z46/m/z75), consistent with the same
#     non-linear response that is the whole reason this codebase uses
#     quadratic (not linear) calibration for RDX/PETN. A single fixed
#     ratio+tolerance would not be valid across the calibration range.
#   - Instead: a simple qualitative DETECTION check. "Confirmed" means
#     the designated qualifier ion(s) were themselves detected (SNR >=
#     3, i.e. snr_flag %in% c("Below_LOQ","Quantifiable")) at the same
#     retention time as the quantifier peak -- independent evidence a
#     real compound eluted there, without requiring a specific ratio.
#   - PETN: single qualifier, m/z 76 only (m/z 57 excluded -- Stage 2
#     showed only 18.2% of even pure Cal standards reach a measurable
#     SNR for it; requiring it would fail almost every genuine
#     detection, not just misidentifications).
#   - RDX: two candidate qualifiers (m/z 46, m/z 75), both well-detected
#     in Cal standards (100%). This script reports both "both required"
#     and "either required" scenarios so the choice can be made from
#     real data, per the original request.
#   - Scope: only evaluated at the Quantifiable SNR tier of the
#     analyte's OWN quantifier (per the agreed design -- Trace/Negative
#     tier qualifier signal is often too weak to be a meaningful check).

suppressPackageStartupMessages({
  library(dplyr)
})

base_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis"
datasets <- c("ABS-S-1", "ABS-S-2", "Lab10-1", "Lab17-1", "Lab29", "Lab33",
              "Lab9", "Steel-1", "Steel 2", "Steel 3", "Steel 4")

read_one <- function(ds) {
  f <- file.path(base_dir, ds, "Results", paste0(ds, "_GCMSResults.csv"))
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Dataset <- ds
  df
}

all_data <- bind_rows(lapply(datasets, read_one))
message("Loaded ", nrow(all_data), " total rows across ", length(datasets), " datasets.")
message("Type breakdown:")
print(table(all_data$Type))

is_detected <- function(flag) flag %in% c("Below_LOQ", "Quantifiable")

all_data <- all_data %>%
  mutate(
    petn_qual76_detected = is_detected(petn_qual76_snr_flag),
    rdx_qual46_detected  = is_detected(rdx_qual46_snr_flag),
    rdx_qual75_detected  = is_detected(rdx_qual75_snr_flag),
    rdx_both_detected    = rdx_qual46_detected & rdx_qual75_detected,
    rdx_either_detected  = rdx_qual46_detected | rdx_qual75_detected
  )

# =========================================================
# PETN: impact at Quantifiable tier, by Type
# =========================================================
message("\n=== PETN: rows at Quantifiable tier, by Type -- m/z76 confirmatory detection ===")
petn_quant <- all_data %>% filter(petn_snr_flag == "Quantifiable")
petn_summary <- petn_quant %>%
  group_by(Type) %>%
  summarise(
    N_Quantifiable = n(),
    N_Confirmed = sum(petn_qual76_detected, na.rm = TRUE),
    N_Failed = sum(!petn_qual76_detected, na.rm = TRUE),
    Pct_Failed = round(100 * N_Failed / N_Quantifiable, 1),
    .groups = "drop"
  )
print(as.data.frame(petn_summary), row.names = FALSE)

# =========================================================
# RDX: impact at Quantifiable tier, by Type, both scenarios
# =========================================================
message("\n=== RDX: rows at Quantifiable tier, by Type -- 'BOTH required' scenario ===")
rdx_quant <- all_data %>% filter(rdx_snr_flag == "Quantifiable")
rdx_both_summary <- rdx_quant %>%
  group_by(Type) %>%
  summarise(
    N_Quantifiable = n(),
    N_Confirmed = sum(rdx_both_detected, na.rm = TRUE),
    N_Failed = sum(!rdx_both_detected, na.rm = TRUE),
    Pct_Failed = round(100 * N_Failed / N_Quantifiable, 1),
    .groups = "drop"
  )
print(as.data.frame(rdx_both_summary), row.names = FALSE)

message("\n=== RDX: rows at Quantifiable tier, by Type -- 'EITHER required' scenario ===")
rdx_either_summary <- rdx_quant %>%
  group_by(Type) %>%
  summarise(
    N_Quantifiable = n(),
    N_Confirmed = sum(rdx_either_detected, na.rm = TRUE),
    N_Failed = sum(!rdx_either_detected, na.rm = TRUE),
    Pct_Failed = round(100 * N_Failed / N_Quantifiable, 1),
    .groups = "drop"
  )
print(as.data.frame(rdx_either_summary), row.names = FALSE)

# =========================================================
# Detail: which specific Sample rows would newly FAIL
# =========================================================
message("\n=== PETN Sample rows that would FAIL confirmatory check (Quantifiable but m/z76 not detected) ===")
petn_fail_detail <- petn_quant %>%
  filter(Type == "Sample", !petn_qual76_detected) %>%
  select(Dataset, Line, SampleName, petn_pa, petn_snr, petn_qual76_pa, petn_qual76_snr, petn_qual76_snr_flag)
print(as.data.frame(petn_fail_detail), row.names = FALSE)

message("\n=== RDX Sample rows that would FAIL under 'BOTH required' but PASS under 'EITHER required' ===")
rdx_diff_detail <- rdx_quant %>%
  filter(Type == "Sample", !rdx_both_detected, rdx_either_detected) %>%
  select(Dataset, Line, SampleName, rdx_pa, rdx_snr,
         rdx_qual46_pa, rdx_qual46_snr_flag, rdx_qual75_pa, rdx_qual75_snr_flag)
print(as.data.frame(rdx_diff_detail), row.names = FALSE)

message("\n=== RDX Sample rows that would FAIL under BOTH scenarios (neither qualifier detected) ===")
rdx_fail_detail <- rdx_quant %>%
  filter(Type == "Sample", !rdx_either_detected) %>%
  select(Dataset, Line, SampleName, rdx_pa, rdx_snr,
         rdx_qual46_pa, rdx_qual46_snr_flag, rdx_qual75_pa, rdx_qual75_snr_flag)
print(as.data.frame(rdx_fail_detail), row.names = FALSE)

# Save outputs
write.csv(petn_summary, "Diagnostics/ConfirmatoryIon_PETN_ImpactSummary.csv", row.names = FALSE)
write.csv(rdx_both_summary, "Diagnostics/ConfirmatoryIon_RDX_BothRequired_ImpactSummary.csv", row.names = FALSE)
write.csv(rdx_either_summary, "Diagnostics/ConfirmatoryIon_RDX_EitherRequired_ImpactSummary.csv", row.names = FALSE)
write.csv(all_data %>% select(Dataset, Line, SampleName, Type,
                               petn_snr_flag, petn_qual76_snr_flag, petn_qual76_detected,
                               rdx_snr_flag, rdx_qual46_snr_flag, rdx_qual75_snr_flag,
                               rdx_qual46_detected, rdx_qual75_detected, rdx_both_detected, rdx_either_detected),
          "Diagnostics/ConfirmatoryIon_FullDetectionData.csv", row.names = FALSE)

message("\n=== Stage 3 impact analysis complete ===")
