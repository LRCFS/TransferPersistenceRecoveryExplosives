#===============================================================================
# does_reanalysis_explain_rdx_batch_effect.R
#
# Direct test: does the fact that 17 pilot samples were reanalysed (and
# select_best_attempt() prefers the most recent PASSING attempt) explain the
# RDX BatchNumber/EventDate decline documented in CONTEXT.md /
# investigate_rdx_batch_effect.R?
#
# Logic:
#   1. The batch/date effect is fit against EventDate/BatchNumber = the
#      PHYSICAL SWAB date (lab notes' Test_DateTime) -- NOT the GC-MS
#      injection date. So which attempt is "selected" cannot change which
#      batch a sample belongs to.
#   2. BUT selection *can* change the recorded RDX value for a reanalysed
#      sample, because select_best_attempt() prefers the most recent PASSING
#      attempt, and RDX quantitation drift gets WORSE at later GC-MS dates
#      (documented separately). If reanalysis is systematically pushing
#      certain batches' values down via late-run drift (rather than a true
#      recovery decline), the batch effect could be partly/wholly an artifact
#      of this selection rule.
#   3. Direct test: for the 17 reanalysed samples, rebuild RDX_Recovery_pct
#      using the EARLIEST attempt instead of the officially-selected one, and
#      re-fit the exact same BatchNumber/EventDate models used in
#      investigate_rdx_batch_effect.R. Compare significance/AIC before vs
#      after. If the effect survives using earliest-attempt values (which are
#      NOT subject to the "prefers most-recent-drifted-run" bias), reanalysis
#      selection cannot be the explanation.
#===============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lme4)
  library(lmerTest)
})

output_dir  <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
gc_data_dir <- file.path(output_dir, "GC Data")
lab_notes_file <- file.path(output_dir, "pilot_data_lab_notes.xlsx")

ideal_concentration <- 10  # ng/uL at 100% recovery (10,000 ng / 1000 uL)

#-------------------------------------------------------------------------------
# 1. LOAD LAB NOTES (official/selected values + true swab EventDate/Batch)
#-------------------------------------------------------------------------------
lab <- as.data.frame(read_excel(lab_notes_file, sheet = "pilot_data_nested"))
pilot <- lab[!is.na(lab$SampleType) & lab$SampleType == "Pilot", ]
pilot$EventDate    <- as.Date(pilot$Test_DateTime)
pilot$BatchNumber  <- factor(pilot$BatchNumber)
pilot$SurfaceID_nested <- interaction(pilot$Surface_type, pilot$SurfaceID, drop = TRUE)
pilot$SelectedDataset  <- basename(pilot$AnalysisFolder)
pilot$SelectedDate     <- pilot$GCMS_Analysis_Date

cat("Pilot rows:", nrow(pilot), " | multi-attempt rows:", sum(pilot$MultipleAttempts, na.rm = TRUE), "\n\n")

#-------------------------------------------------------------------------------
# 2. RAW PER-ATTEMPT DATA (all Analysis*/Results/*_GCMSResults.csv, all attempts)
#-------------------------------------------------------------------------------
result_files <- list.files(gc_data_dir, pattern = "_GCMSResults\\.csv$", recursive = TRUE, full.names = TRUE)
result_files <- result_files[!grepl("[/\\\\]Backup[/\\\\]", result_files)]

raw <- bind_rows(lapply(result_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Dataset <- basename(dirname(dirname(f)))
  df
})) %>%
  filter(Type == "Sample") %>%
  mutate(SampleName_trimmed = str_trim(SampleName)) %>%
  filter(grepl("^PILOT_\\d{3}$", SampleName_trimmed))

petn_zero <- raw$petn_snr_flag %in% c(NA_character_, "Below_LOD", "Below_LOQ")
raw$PETN_Recovery_pct_attempt <- ifelse(petn_zero, 0,
  ifelse(!is.na(raw$petn_concentration_dc) & raw$petn_concentration_dc > 0,
         raw$petn_concentration_dc / ideal_concentration * 100,
         ifelse(!is.na(raw$petn_concentration_dc) & raw$petn_concentration_dc <= 0, 0, NA_real_)))

rdx_zero <- raw$rdx_snr_flag %in% c(NA_character_, "Below_LOD", "Below_LOQ")
raw$RDX_Recovery_pct_attempt <- ifelse(rdx_zero, 0,
  ifelse(!is.na(raw$rdx_concentration) & raw$rdx_concentration > 0,
         raw$rdx_concentration / ideal_concentration * 100,
         ifelse(!is.na(raw$rdx_concentration) & raw$rdx_concentration <= 0, 0, NA_real_)))

# Earliest attempt per RunID (by GC-MS Date, then Line as tiebreak)
earliest <- raw %>%
  arrange(SampleName_trimmed, Date, Line) %>%
  group_by(SampleName_trimmed) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(RunID = SampleName_trimmed,
            Earliest_Dataset = Dataset,
            Earliest_Date = Date,
            Earliest_PETN_Recovery_pct = PETN_Recovery_pct_attempt,
            Earliest_RDX_Recovery_pct = RDX_Recovery_pct_attempt)

# Latest attempt per RunID (by GC-MS Date, then Line) -- for reference/comparison
latest <- raw %>%
  arrange(SampleName_trimmed, Date, Line) %>%
  group_by(SampleName_trimmed) %>%
  slice(n()) %>%
  ungroup() %>%
  transmute(RunID = SampleName_trimmed,
            Latest_Dataset = Dataset,
            Latest_Date = Date,
            Latest_PETN_Recovery_pct = PETN_Recovery_pct_attempt,
            Latest_RDX_Recovery_pct = RDX_Recovery_pct_attempt)

n_attempts <- raw %>% count(SampleName_trimmed, name = "N_Attempts_raw")

pilot <- pilot %>%
  left_join(earliest, by = "RunID") %>%
  left_join(latest, by = "RunID") %>%
  left_join(n_attempts, by = c("RunID" = "SampleName_trimmed"))

#-------------------------------------------------------------------------------
# 3. CHECK: for multi-attempt samples, is the OFFICIAL (selected) value the
#    same as the EARLIEST attempt, or does it differ (i.e. did selection
#    actually swap in a different -- typically later -- attempt)?
#-------------------------------------------------------------------------------
multi <- pilot %>% filter(N_Attempts_raw > 1) %>%
  select(RunID, EventDate, BatchNumber, SelectedDataset, SelectedDate,
         Earliest_Dataset, Earliest_Date, Latest_Dataset, Latest_Date,
         RDX_Recovery_pct, Earliest_RDX_Recovery_pct, Latest_RDX_Recovery_pct,
         PETN_Recovery_pct, Earliest_PETN_Recovery_pct, Latest_PETN_Recovery_pct)

multi$SelectedIsEarliest <- multi$SelectedDate == multi$Earliest_Date
multi$RDX_diff_selected_minus_earliest <- multi$RDX_Recovery_pct - multi$Earliest_RDX_Recovery_pct
multi$AnalysisDelay_selected_days <- as.numeric(as.Date(as.character(multi$SelectedDate), format = "%Y%m%d") - multi$EventDate)

cat("========== Per-sample: official (selected) vs earliest-attempt RDX value ==========\n")
print(as.data.frame(multi %>% arrange(EventDate) %>%
  select(RunID, EventDate, BatchNumber, SelectedIsEarliest, AnalysisDelay_selected_days,
         Earliest_RDX_Recovery_pct, RDX_Recovery_pct, RDX_diff_selected_minus_earliest)))

cat("\nHow many of the 17 reanalysed samples kept the EARLIEST attempt as 'official'? ",
    sum(multi$SelectedIsEarliest), "of", nrow(multi), "\n")
cat("Mean (selected - earliest) RDX difference:", round(mean(multi$RDX_diff_selected_minus_earliest, na.rm=TRUE), 3),
    "pp | SD:", round(sd(multi$RDX_diff_selected_minus_earliest, na.rm=TRUE), 3), "\n")

cat("\nDoes analysis delay (selected GC-MS date minus swab EventDate) correlate with swab EventDate itself?\n")
print(cor.test(as.numeric(multi$EventDate), multi$AnalysisDelay_selected_days))

#-------------------------------------------------------------------------------
# 4. THE DECISIVE TEST: refit the BatchNumber model using EARLIEST-attempt
#    RDX values for the 17 reanalysed samples (all other 15 samples,
#    analysed only once, are unchanged) and compare to the official model.
#-------------------------------------------------------------------------------
pilot$RDX_official <- pilot$RDX_Recovery_pct
pilot$RDX_earliest_substituted <- ifelse(!is.na(pilot$Earliest_RDX_Recovery_pct),
                                          pilot$Earliest_RDX_Recovery_pct, pilot$RDX_Recovery_pct)

base_f <- "%s ~ Surface_type * Pressure_level * Solvent_level + BatchNumber + (1|SurfaceID_nested)"

m_official  <- lmer(as.formula(sprintf(base_f, "RDX_official")), data = pilot)
m_earliest  <- lmer(as.formula(sprintf(base_f, "RDX_earliest_substituted")), data = pilot)

cat("\n\n========== MODEL A: BatchNumber effect using OFFICIAL (selected-attempt) RDX values ==========\n")
print(anova(m_official)["BatchNumber", ])
cat("AIC:", AIC(m_official), "\n")

cat("\n========== MODEL B: BatchNumber effect using EARLIEST-attempt RDX values for reanalysed samples ==========\n")
print(anova(m_earliest)["BatchNumber", ])
cat("AIC:", AIC(m_earliest), "\n")

cat("\n========== Batch means: official vs earliest-attempt substitution ==========\n")
print(as.data.frame(pilot %>% group_by(BatchNumber) %>%
  summarise(N = n(),
            Mean_RDX_official = mean(RDX_official, na.rm = TRUE),
            Mean_RDX_earliest = mean(RDX_earliest_substituted, na.rm = TRUE))))

# Same test but using true continuous EventDate/ElapsedDays instead of
# categorical BatchNumber (matches Section 6 of investigate_rdx_batch_effect.R)
pilot$ElapsedDays <- as.numeric(pilot$EventDate - min(pilot$EventDate))
base_f_days <- "%s ~ Surface_type * Pressure_level * Solvent_level + ElapsedDays + (1|SurfaceID_nested)"
m_official_days <- lmer(as.formula(sprintf(base_f_days, "RDX_official")), data = pilot)
m_earliest_days <- lmer(as.formula(sprintf(base_f_days, "RDX_earliest_substituted")), data = pilot)

cat("\n\n========== MODEL C: ElapsedDays (continuous) effect, OFFICIAL values ==========\n")
print(anova(m_official_days)["ElapsedDays", ])
cat("Fixed-effect coefficient:", fixef(m_official_days)["ElapsedDays"], "\n")

cat("\n========== MODEL D: ElapsedDays (continuous) effect, EARLIEST-attempt substituted ==========\n")
print(anova(m_earliest_days)["ElapsedDays", ])
cat("Fixed-effect coefficient:", fixef(m_earliest_days)["ElapsedDays"], "\n")
