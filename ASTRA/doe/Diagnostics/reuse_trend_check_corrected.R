#===============================================================================
# reuse_trend_check_corrected.R
#
# Re-checks whether RDX (and PETN) recovery trends with physical surface
# reuse count (CumulativeUseNumber), using the CORRECTED recovery values
# from the live pilot_data_nested.csv (post select_best_attempt() earliest-
# attempt fix) -- NOT the stale pilot_data_lab_notes.xlsx values (confirmed
# out of sync with the fix; still reflects the old most-recent-attempt
# selection for all 17 reanalysed samples).
#
# Approach: reuse pilot_data_lab_notes.xlsx ONLY for the metadata needed to
# reconstruct true chronological physical-surface-use order (EventDate,
# Time Spiked, BatchNumber, TestOrder, SurfaceID -- none of which changed
# due to the reanalysis-selection fix, since that's swab-time metadata, not
# which GC-MS attempt was selected), but overwrite its RDX_Recovery_pct/
# PETN_Recovery_pct with the current, corrected values from
# pilot_data_nested.csv before fitting anything. Read-only with respect to
# both live files.
#===============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lme4)
  library(lmerTest)
})

output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
lab_notes_file <- file.path(output_dir, "pilot_data_lab_notes.xlsx")
nested_file    <- file.path(output_dir, "pilot_data_nested.csv")

lab    <- as.data.frame(read_excel(lab_notes_file, sheet = "pilot_data_nested"))
nested <- read.csv(nested_file, stringsAsFactors = FALSE)

# --- Overwrite recovery values (and provenance columns) with the CORRECTED
#     ones from pilot_data_nested.csv, matched by RunID, for ALL rows
#     (pilot samples AND NCs) ---
correction <- nested %>%
  select(RunID, RDX_Recovery_pct_new = RDX_Recovery_pct,
         PETN_Recovery_pct_new = PETN_Recovery_pct,
         AnalysisFolder_new = AnalysisFolder,
         GCMS_Analysis_Date_new = GCMS_Analysis_Date)

lab <- lab %>%
  left_join(correction, by = "RunID") %>%
  mutate(
    RDX_Recovery_pct  = ifelse(!is.na(RDX_Recovery_pct_new), RDX_Recovery_pct_new, RDX_Recovery_pct),
    PETN_Recovery_pct = ifelse(!is.na(PETN_Recovery_pct_new), PETN_Recovery_pct_new, PETN_Recovery_pct),
    AnalysisFolder     = ifelse(!is.na(AnalysisFolder_new), AnalysisFolder_new, AnalysisFolder),
    GCMS_Analysis_Date = ifelse(!is.na(GCMS_Analysis_Date_new), GCMS_Analysis_Date_new, GCMS_Analysis_Date)
  ) %>%
  select(-ends_with("_new"))

cat("Confirmed corrected values now loaded (spot check PILOT_004, PILOT_032):\n")
print(lab %>% filter(RunID %in% c("PILOT_004","PILOT_032")) %>% select(RunID, RDX_Recovery_pct, PETN_Recovery_pct))

pilot <- lab[!is.na(lab$SampleType) & lab$SampleType == "Pilot", ]
pilot$EventDate        <- as.Date(pilot$Test_DateTime)
pilot$BatchNumber       <- factor(pilot$BatchNumber)
pilot$SurfaceID_nested  <- interaction(pilot$Surface_type, pilot$SurfaceID, drop = TRUE)

#-------------------------------------------------------------------------------
# Rebuild true physical-surface usage history (pilot + NC combined), exactly
# per investigate_rdx_batch_effect.R Section 2 (two-tier NC matching).
# NC placement metadata (EventDate/Time Spiked/BatchNumber/TestOrder) is
# untouched by the reanalysis-selection fix, so this reconstruction is still
# valid as-is.
#-------------------------------------------------------------------------------
all_dated <- lab
all_dated$EventDate <- as.Date(all_dated$Test_DateTime)
all_dated$TimeSpikedChar <- format(all_dated$`Time Spiked`, "%H:%M:%S")

pilot_times_batch <- all_dated %>%
  filter(SampleType == "Pilot", !is.na(TimeSpikedChar)) %>%
  group_by(SurfaceID, EventDate, BatchNumber) %>%
  summarise(EarliestPilotTime_Batch = min(TimeSpikedChar), .groups = "drop")

pilot_times_testorder <- all_dated %>%
  filter(SampleType == "Pilot", !is.na(TimeSpikedChar)) %>%
  group_by(SurfaceID, EventDate, TestOrder) %>%
  summarise(EarliestPilotTime_TO = min(TimeSpikedChar), .groups = "drop")

all_dated <- all_dated %>%
  left_join(pilot_times_batch, by = c("SurfaceID","EventDate","BatchNumber")) %>%
  left_join(pilot_times_testorder, by = c("SurfaceID","EventDate","BatchNumber" = "TestOrder")) %>%
  mutate(
    MatchedTime = coalesce(EarliestPilotTime_Batch, EarliestPilotTime_TO),
    TimeKey = case_when(
      SampleType == "Pilot" & !is.na(TimeSpikedChar) ~ TimeSpikedChar,
      SampleType == "NC" & !is.na(MatchedTime) ~ format(as.POSIXct(MatchedTime, format="%H:%M:%S") - 300, "%H:%M:%S"),
      SampleType == "NC" ~ "00:00:00",
      TRUE ~ "12:00:00"
    )
  )

usage <- all_dated %>%
  filter(!is.na(SurfaceID)) %>%
  arrange(SurfaceID, EventDate, TimeKey) %>%
  group_by(SurfaceID) %>%
  mutate(CumulativeUseNumber = row_number(), TotalUsesOnThisSurface = n()) %>%
  ungroup()

pilot <- pilot %>%
  left_join(usage %>% filter(SampleType == "Pilot") %>% select(RunID, CumulativeUseNumber, TotalUsesOnThisSurface),
            by = "RunID")

cat("\n=== Total physical uses per surface (pilot + NC), corrected data ===\n")
print(usage %>% group_by(SurfaceID) %>%
        summarise(TotalUses = n(), PilotUses = sum(SampleType == "Pilot"), NCUses = sum(SampleType == "NC")))

cat("\n=== RDX/PETN recovery by CumulativeUseNumber (raw means, corrected data) ===\n")
print(as.data.frame(pilot %>% group_by(CumulativeUseNumber) %>%
        summarise(N = n(), RDX_mean = mean(RDX_Recovery_pct, na.rm=TRUE),
                  PETN_mean = mean(PETN_Recovery_pct, na.rm=TRUE))))

#-------------------------------------------------------------------------------
# Direct trend tests: does CumulativeUseNumber predict recovery, with and
# without BatchNumber in the model (batch/use-order are correlated by
# construction in the pilot's own design, so both views matter)
#-------------------------------------------------------------------------------
base_f <- "%s ~ Surface_type * Pressure_level * Solvent_level"

cat("\n\n========== RDX: CumulativeUseNumber alone (no Batch) ==========\n")
m_rdx_use <- lmer(as.formula(paste(sprintf(base_f, "RDX_Recovery_pct"), "+ CumulativeUseNumber + (1|SurfaceID_nested)")), data = pilot)
print(anova(m_rdx_use)["CumulativeUseNumber", ])
cat("Spearman correlation (RDX vs CumulativeUseNumber, raw, unadjusted):\n")
print(cor.test(pilot$CumulativeUseNumber, pilot$RDX_Recovery_pct, method = "spearman"))

cat("\n\n========== RDX: CumulativeUseNumber WITH BatchNumber in the model ==========\n")
m_rdx_use_batch <- lmer(as.formula(paste(sprintf(base_f, "RDX_Recovery_pct"), "+ CumulativeUseNumber + BatchNumber + (1|SurfaceID_nested)")), data = pilot)
print(anova(m_rdx_use_batch)[c("CumulativeUseNumber","BatchNumber"), ])
cat("AIC (CumulativeUseNumber alone):", AIC(m_rdx_use), " | AIC (Batch alone, for comparison):",
    AIC(lmer(as.formula(paste(sprintf(base_f, "RDX_Recovery_pct"), "+ BatchNumber + (1|SurfaceID_nested)")), data = pilot)),
    " | AIC (both):", AIC(m_rdx_use_batch), "\n")

cat("\n\n========== PETN: CumulativeUseNumber alone (no Batch) ==========\n")
m_petn_use <- lmer(as.formula(paste(sprintf(base_f, "PETN_Recovery_pct"), "+ CumulativeUseNumber + (1|SurfaceID_nested)")), data = pilot)
print(anova(m_petn_use)["CumulativeUseNumber", ])
cat("Spearman correlation (PETN vs CumulativeUseNumber, raw, unadjusted):\n")
print(cor.test(pilot$CumulativeUseNumber, pilot$PETN_Recovery_pct, method = "spearman"))

cat("\n\n========== Restricting to the actually-observed reuse range (max = 4 in this pilot) ==========\n")
cat("Max CumulativeUseNumber observed in this pilot:", max(pilot$CumulativeUseNumber, na.rm=TRUE), "\n")
cat("(A main-study design testing 5 reuses/surface would extrapolate 1 use beyond this pilot's own tested range.)\n")
