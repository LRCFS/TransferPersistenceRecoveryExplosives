# ============================================================
# PETN_AllIons_Scan.R
# Full-scan (m/z 41-200) mass spectrum survey at the PETN retention
# time, across calibration standards spanning 0.2-10ng plus blanks,
# to identify additional candidate confirmatory/qualifier ions for
# PETN beyond the current SIM group (46 quantifier, 57/76 qualifiers).
#
# Added Aug 2026 -- see CONTEXT.md "Confirmatory Ion Detection" session.
# Reuses the parse_ms1_file()/get_all_ions_at_rt() approach already
# established in Diagnostics/TIC_AllIons_Scan.R (built for the
# alternative-IS search), applied here to PETN's own RT instead.
#
# Rationale: the current SIM method's PETN group only ever acquired
# m/z 46 (quantifier), 57, 76 (qualifiers) -- fixed at method-design
# time. The instrument ALSO runs in simultaneous SIM/Scan mode
# (m/z 41-200 full scan, confirmed in acqmeth.txt), so the *raw* .ms1
# files already contain the complete mass spectrum at every retention
# time, including PETN's -- independent of which ions the SIM groups
# were configured to monitor. This lets us look for a genuinely better
# second confirmatory ion (to replace m/z 57, shown in Stage 2/3 to be
# too weak/unreliable) BEFORE committing to a SIM method change.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/Lab29/GcDataConverterMs/"

# Lab29 Cal standards (from Lab29_GCMSResults.csv): DataFile prefix -> CalLevel
cal_files <- c("007" = 0.2, "008" = 2, "009" = 4, "010" = 6, "011" = 8, "012" = 10)
blank_files <- c("001", "002", "003")  # clean blanks, no PETN detected

petn_rt_sec <- 339.4  # from Lab9's own petn_rt (SIM pipeline units: seconds)

parse_ms1_file <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  scans <- list()
  current_scan <- NULL
  current_rt <- NULL
  ions <- list()

  for (line in lines) {
    if (grepl("^S\\t", line)) {
      if (!is.null(current_scan) && length(ions) > 0) {
        ions_df <- do.call(rbind, ions)
        colnames(ions_df) <- c("mz", "intensity")
        ions_df <- as.data.frame(ions_df)
        ions_df$intensity <- as.numeric(ions_df$intensity)
        ions_df$mz <- as.numeric(ions_df$mz)
        scans[[current_scan]] <- list(rt = current_rt, ions = ions_df)
      }
      parts <- strsplit(line, "\t")[[1]]
      current_scan <- as.integer(parts[2])
      ions <- list()
    } else if (grepl("^I\\tRTime\\t", line)) {
      parts <- strsplit(line, "\t")[[1]]
      current_rt <- as.numeric(parts[3])
    } else if (grepl("^\\d", line) && !grepl("^I\\t", line)) {
      parts <- strsplit(line, "\\s+")[[1]]
      if (length(parts) >= 2) {
        ions[[length(ions) + 1]] <- c(parts[1], parts[2])
      }
    }
  }
  if (!is.null(current_scan) && length(ions) > 0) {
    ions_df <- do.call(rbind, ions)
    colnames(ions_df) <- c("mz", "intensity")
    ions_df <- as.data.frame(ions_df)
    ions_df$intensity <- as.numeric(ions_df$intensity)
    ions_df$mz <- as.numeric(ions_df$mz)
    scans[[current_scan]] <- list(rt = current_rt, ions = ions_df)
  }
  scans
}

get_all_ions_at_rt <- function(scans, target_rt, window) {
  all_ions <- data.frame()
  for (scan in scans) {
    if (is.null(scan$rt) || is.null(scan$ions)) next
    if (abs(scan$rt - target_rt) > window) next
    all_ions <- rbind(all_ions, scan$ions)
  }
  if (nrow(all_ions) == 0) return(data.frame(mz = integer(), intensity = numeric()))
  all_ions$mz_int <- round(all_ions$mz)
  all_ions %>%
    group_by(mz_int) %>%
    summarise(intensity = max(intensity, na.rm = TRUE), .groups = "drop") %>%
    rename(mz = mz_int)
}

# --- Step 1: determine the RT unit used in these .ms1 files (minutes vs seconds) ---
probe <- parse_ms1_file(file.path(ms1_dir, "011.ms1"))
rt_values <- sapply(probe, function(s) s$rt)
cat("RT range in scan metadata (011.ms1):", range(rt_values, na.rm = TRUE), "\n")
# If max RT is ~10-12, it's minutes; if ~600-700, it's seconds.
rt_is_minutes <- max(rt_values, na.rm = TRUE) < 30
target_rt <- if (rt_is_minutes) petn_rt_sec / 60 else petn_rt_sec
window <- if (rt_is_minutes) 0.15 else 8
cat("Using target_rt =", target_rt, " (", ifelse(rt_is_minutes, "minutes", "seconds"), "), window =", window, "\n\n")

# --- Step 2: extract ions at PETN's RT for each Cal standard + blank ---
all_ion_data <- list()
for (f in names(cal_files)) {
  scans <- parse_ms1_file(file.path(ms1_dir, paste0(f, ".ms1")))
  ions <- get_all_ions_at_rt(scans, target_rt, window)
  if (nrow(ions) > 0) { ions$file <- paste0("Cal_", cal_files[f], "ng"); all_ion_data[[f]] <- ions }
  cat("Cal", cal_files[f], "ng (file", f, "): found", nrow(ions), "distinct ions\n")
}
for (f in blank_files) {
  scans <- parse_ms1_file(file.path(ms1_dir, paste0(f, ".ms1")))
  ions <- get_all_ions_at_rt(scans, target_rt, window)
  if (nrow(ions) > 0) { ions$file <- paste0("Blank_", f); all_ion_data[[f]] <- ions }
  cat("Blank (file", f, "): found", nrow(ions), "distinct ions\n")
}

all_ions_df <- do.call(rbind, all_ion_data)
ion_matrix <- all_ions_df %>%
  pivot_wider(names_from = file, values_from = intensity)

cal_cols <- paste0("Cal_", cal_files, "ng")
blank_cols <- paste0("Blank_", blank_files)

# Ions present in ALL 6 Cal standards
ion_matrix_present <- ion_matrix %>% filter(if_all(all_of(cal_cols), ~ !is.na(.)))
cat("\nTotal unique m/z values seen across all files:", nrow(ion_matrix), "\n")
cat("m/z values present in ALL 6 Cal standards:", nrow(ion_matrix_present), "\n\n")

# For each such ion, compute: mean blank intensity, min/max Cal intensity,
# ratio to the known quantifier (m/z 46) and to the existing m/z 76 qualifier,
# and correlation with CalLevel (to flag concentration-dependence like the
# RDX qualifiers showed).
mz46_int <- ion_matrix_present$Cal_0.2ng[ion_matrix_present$mz == 46]
report <- ion_matrix_present %>%
  rowwise() %>%
  mutate(
    mean_blank = mean(c_across(all_of(blank_cols)), na.rm = TRUE),
    min_cal = min(c_across(all_of(cal_cols)), na.rm = TRUE),
    max_cal = max(c_across(all_of(cal_cols)), na.rm = TRUE),
    signal_to_blank = ifelse(is.na(mean_blank) | mean_blank == 0, Inf, min_cal / mean_blank)
  ) %>%
  ungroup() %>%
  arrange(desc(signal_to_blank))

# Exclude already-known/used ions and out-of-range noise for clarity, but keep for reference
report$known_role <- case_when(
  report$mz == 46 ~ "CURRENT QUANTIFIER",
  report$mz == 57 ~ "CURRENT QUALIFIER (unreliable)",
  report$mz == 76 ~ "CURRENT QUALIFIER (good)",
  TRUE ~ ""
)

cat("=== Candidate ions present in ALL Cal standards, ranked by signal-to-blank ratio ===\n")
print(as.data.frame(report %>% select(mz, all_of(cal_cols), mean_blank, signal_to_blank, known_role)),
      row.names = FALSE)

# Correlation with CalLevel for the top candidates (excluding already-known ions)
cat("\n=== Concentration-dependence check (ratio to m/z46 vs CalLevel) for top new candidates ===\n")
cal_levels <- as.numeric(cal_files)
top_candidates <- report %>% filter(!mz %in% c(46, 57, 76), signal_to_blank > 3) %>% pull(mz)
for (candidate_mz in top_candidates) {
  row <- ion_matrix_present %>% filter(mz == candidate_mz)
  cal_ints <- as.numeric(row[1, cal_cols])
  q_ints <- as.numeric((ion_matrix_present %>% filter(mz == 46))[1, cal_cols])
  ratio <- cal_ints / q_ints
  ct <- suppressWarnings(cor.test(cal_levels, ratio))
  cat(sprintf("  m/z %3d : ratio range [%.4f - %.4f], r vs CalLevel = %.3f, p = %.3g\n",
              candidate_mz, min(ratio), max(ratio), ct$estimate, ct$p.value))
}

write.csv(report, "Diagnostics/PETN_AllIons_ScanResults_Lab29.csv", row.names = FALSE)
cat("\nSaved: Diagnostics/PETN_AllIons_ScanResults_Lab29.csv\n")
