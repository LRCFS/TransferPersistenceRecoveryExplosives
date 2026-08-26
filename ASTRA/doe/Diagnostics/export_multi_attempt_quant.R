#===============================================================================
# EXPORT: PETN/RDX quant (concentration + recovery) from EVERY analysis attempt,
# for every pilot sample that has been analysed more than once.
#
# READ-ONLY with respect to the study's live files: only reads the raw
# Analysis*/Results/*_GCMSResults.csv files (never touches Backup/, never
# rewrites pilot_data_nested.csv/.xlsx or ASTRA_PilotStudyResults.xlsx).
# Writes a single new workbook: pilot_data_multi_attempt_quant.xlsx
#
# Sample-name parsing, duplicate-attempt detection, and the recovery formula
# (incl. the Below_LOD/Below_LOQ/NA -> 0% "legitimate negative" treatment)
# mirror pilot_analysis.R's collate_gcms_results() (Sections re: pilot_pattern/
# nc_pattern, Section 5 recovery calc, select_best_attempt()) so the numbers
# here are directly comparable to the "winning" attempt already recorded in
# pilot_data_nested.csv.
#===============================================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
}))

output_dir  <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
gc_data_dir <- file.path(output_dir, "GC Data")
xlsx_out    <- file.path(output_dir, "pilot_data_multi_attempt_quant.xlsx")

sample_vol_uL       <- 1000
deposit_mass_ng     <- 10000
ideal_concentration <- deposit_mass_ng / sample_vol_uL   # = 10 ng/uL at 100% recovery

#-------------------------------------------------------------------------------
# 1. DISCOVER AND LOAD ALL *_GCMSResults.csv FILES (top-level per Analysis
#    folder only -- excludes Backup/ subfolders)
#-------------------------------------------------------------------------------

result_files <- list.files(
  gc_data_dir,
  pattern    = "_GCMSResults\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
)
result_files <- result_files[!grepl("[/\\\\]Backup[/\\\\]", result_files)]

if (length(result_files) == 0) stop("No *_GCMSResults.csv files found under: ", gc_data_dir)

cat("Found", length(result_files), "result file(s):\n")
for (f in result_files) cat(" -", f, "\n")

all_data <- bind_rows(lapply(result_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Dataset        <- basename(dirname(dirname(f)))   # Analysis1, Analysis2, ...
  df$AnalysisFolder  <- dirname(dirname(f))
  df$SourceFile      <- f
  df
}))

#-------------------------------------------------------------------------------
# 2. FILTER TO SAMPLE ROWS, IDENTIFY PILOT SAMPLES (same regex as
#    pilot_analysis.R's pilot_pattern)
#-------------------------------------------------------------------------------

samples <- all_data %>% filter(Type == "Sample")
samples$SampleName_trimmed <- str_trim(samples$SampleName)

pilot_pattern <- "^PILOT_(\\d{3})$"
nc_pattern    <- "^(?:PILOT_)?(Steel|ABS)Batch(\\d+)[ _]?(?:NegCtrl|NC)$"

samples$IsPilotSample     <- grepl(pilot_pattern, samples$SampleName_trimmed)
samples$IsNegativeControl <- grepl(nc_pattern, samples$SampleName_trimmed, ignore.case = TRUE)

#-------------------------------------------------------------------------------
# 3. RECOVERY CALCULATION (mirrors pilot_analysis.R Section 5 exactly)
#-------------------------------------------------------------------------------

petn_snr_flag_col <- if ("petn_snr_flag" %in% names(samples)) samples$petn_snr_flag else rep(NA_character_, nrow(samples))
rdx_snr_flag_col  <- if ("rdx_snr_flag"  %in% names(samples)) samples$rdx_snr_flag  else rep(NA_character_, nrow(samples))

petn_treat_as_zero <- petn_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
samples$PETN_Recovery_pct <- ifelse(
  petn_treat_as_zero,
  0,
  ifelse(
    !is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc > 0,
    (samples$petn_concentration_dc / ideal_concentration) * 100,
    ifelse(!is.na(samples$petn_concentration_dc) & samples$petn_concentration_dc <= 0, 0, NA_real_)
  )
)

rdx_treat_as_zero <- rdx_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")
samples$RDX_Recovery_pct <- ifelse(
  rdx_treat_as_zero,
  0,
  ifelse(
    !is.na(samples$rdx_concentration) & samples$rdx_concentration > 0,
    (samples$rdx_concentration / ideal_concentration) * 100,
    ifelse(!is.na(samples$rdx_concentration) & samples$rdx_concentration <= 0, 0, NA_real_)
  )
)

#-------------------------------------------------------------------------------
# 4. BUILD PER-ATTEMPT TABLES (pilot samples, and separately NCs), ordered
#    chronologically by GC-MS Date within each sample
#-------------------------------------------------------------------------------

build_attempt_table <- function(df, id_col_name) {
  df <- df %>%
    arrange(.data[[id_col_name]], Date, Line) %>%
    group_by(.data[[id_col_name]]) %>%
    mutate(N_Attempts = n(), Attempt = row_number()) %>%
    ungroup()
  df
}

pilot_rows <- samples %>% filter(IsPilotSample)
pilot_rows <- build_attempt_table(pilot_rows, "SampleName_trimmed")
pilot_multi <- pilot_rows %>% filter(N_Attempts > 1)

nc_rows <- samples %>% filter(IsNegativeControl)
nc_rows <- build_attempt_table(nc_rows, "SampleName_trimmed")
nc_multi <- nc_rows %>% filter(N_Attempts > 1)

cat("\nPilot samples analysed more than once:", length(unique(pilot_multi$SampleName_trimmed)), "\n")
cat("Negative controls analysed more than once:", length(unique(nc_multi$SampleName_trimmed)), "\n")

#-------------------------------------------------------------------------------
# 5. LONG-FORMAT SHEET (one row per sample x attempt)
#-------------------------------------------------------------------------------

make_long <- function(df) {
  df %>%
    transmute(
      RunID              = SampleName_trimmed,
      Attempt            = Attempt,
      N_Attempts         = N_Attempts,
      GCMS_Analysis_Date = Date,
      Line               = Line,
      Dataset            = Dataset,
      AnalysisFolder     = AnalysisFolder,
      PETN_snr_flag      = petn_snr_flag,
      PETN_concentration_ng_uL = petn_concentration_dc,
      PETN_Recovery_pct  = round(PETN_Recovery_pct, 2),
      RDX_snr_flag       = rdx_snr_flag,
      RDX_concentration_ng_uL = rdx_concentration,
      RDX_Recovery_pct   = round(RDX_Recovery_pct, 2)
    ) %>%
    arrange(RunID, Attempt)
}

pilot_long <- make_long(pilot_multi)
nc_long    <- make_long(nc_multi)

#-------------------------------------------------------------------------------
# 6. WIDE-FORMAT SHEET (one row per sample, attempts side-by-side)
#-------------------------------------------------------------------------------

## Columns are grouped BY FIELD (all attempts' PETN concentrations together,
## all attempts' RDX concentrations together, etc.) rather than by attempt --
## puts the values you actually want to compare side-by-side directly next
## to each other instead of scattered one-per-attempt-block.
make_wide <- function(df) {
  ids <- unique(df$RunID)
  max_attempts <- max(df$Attempt)
  attempt_nums <- seq_len(max_attempts)

  get_field <- function(sub, a, field) {
    s <- sub %>% filter(Attempt == a)
    if (nrow(s) == 1) s[[field]] else NA
  }

  # Field groups, in the order they should appear across the sheet.
  # Each entry: (label used in column name, source column in long df)
  field_groups <- list(
    Date              = "GCMS_Analysis_Date",
    Dataset           = "Dataset",
    PETN_conc_ng_uL   = "PETN_concentration_ng_uL",
    PETN_Recovery_pct = "PETN_Recovery_pct",
    PETN_SNR          = "PETN_snr_flag",
    RDX_conc_ng_uL    = "RDX_concentration_ng_uL",
    RDX_Recovery_pct  = "RDX_Recovery_pct",
    RDX_SNR           = "RDX_snr_flag"
  )

  out <- lapply(ids, function(id) {
    sub <- df %>% filter(RunID == id) %>% arrange(Attempt)
    row <- list(RunID = id, N_Attempts = unique(sub$N_Attempts))
    for (label in names(field_groups)) {
      src_col <- field_groups[[label]]
      for (a in attempt_nums) {
        row[[paste0("Attempt", a, "_", label)]] <- get_field(sub, a, src_col)
      }
    }
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  })
  bind_rows(out) %>% arrange(RunID)
}

pilot_wide <- make_wide(pilot_long)
nc_wide    <- if (nrow(nc_long) > 0) make_wide(nc_long) else data.frame()

#-------------------------------------------------------------------------------
# 7. WRITE WORKBOOK
#-------------------------------------------------------------------------------

wb <- createWorkbook()

hdr_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")

add_sheet_simple <- function(wb, name, df) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr_style)
  freezePane(wb, name, firstRow = TRUE)
  setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
}

add_sheet_simple(wb, "Pilot Samples (wide)", pilot_wide)
add_sheet_simple(wb, "Pilot Samples (long)", pilot_long)
if (nrow(nc_wide) > 0) add_sheet_simple(wb, "Negative Controls (wide)", nc_wide)
if (nrow(nc_long) > 0) add_sheet_simple(wb, "Negative Controls (long)", nc_long)

# Notes sheet
notes <- c(
  "This workbook lists every PILOT_xxx sample (and, on separate sheets, negative",
  "controls) that was analysed by GC-MS more than once across Analysis1-Analysis7,",
  "showing the PETN and RDX quant (concentration, ng/uL) and calculated recovery (%)",
  "from EACH individual analysis attempt -- not just the single 'best attempt' that",
  "pilot_data_nested.csv keeps after select_best_attempt() collapses duplicates.",
  "",
  "Recovery (%) = concentration / 10 ng/uL * 100 (10,000 ng deposited in 1000 uL",
  "extraction volume = 10 ng/uL at 100% recovery). PETN uses the drift-corrected",
  "concentration (petn_concentration_dc); RDX uses the raw 15N-RDX IS-ratio",
  "concentration (no drift correction applied to RDX in this pipeline).",
  "",
  "A concentration/recovery of 0 for a row whose SNR flag is NA, 'Below_LOD', or",
  "'Below_LOQ' reflects this study's deliberate 'legitimate non-detection -> 0%'",
  "treatment (see CONTEXT.md), not a literal zero measurement -- check the SNR",
  "flag column alongside the recovery value.",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Source: ", gc_data_dir)
)
addWorksheet(wb, "Notes")
writeData(wb, "Notes", data.frame(Notes = notes), colNames = FALSE)
setColWidths(wb, "Notes", cols = 1, widths = 100)

saveWorkbook(wb, xlsx_out, overwrite = TRUE)
cat("\nWritten:", xlsx_out, "\n")
