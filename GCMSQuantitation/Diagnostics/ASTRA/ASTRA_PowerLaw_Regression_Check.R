library(dplyr)
library(readr)

backup_dir <- "C:/Users/ABRUCE~1/AppData/Local/Temp/opencode/PowerLawASTRA/PreChange_Backup"
root <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data"

# Columns expected to change (drift-correction-derived) -- excluded from the
# regression check since they are the deliberate target of this change.
drift_cols <- c(
  "petn_drift_correction_factor", "petn_drift_model", "petn_concentration_dc",
  "petn_percent_bias_dc", "petn_qc_flag_dc", "petn_pa_dc", "petn_ratio_dc"
)

for (i in 1:5) {
  run <- paste0("Analysis", i)
  old <- read_csv(file.path(backup_dir, paste0(run, "_GCMSResults_PRECHANGE.csv")), show_col_types = FALSE)
  new <- read_csv(file.path(root, run, "Results", paste0(run, "_GCMSResults.csv")), show_col_types = FALSE)

  compare_cols <- intersect(names(old), names(new))
  compare_cols <- setdiff(compare_cols, drift_cols)

  old_c <- old %>% select(Line, all_of(compare_cols)) %>% arrange(Line)
  new_c <- new %>% select(Line, all_of(compare_cols)) %>% arrange(Line)

  if (!identical(dim(old_c), dim(new_c))) {
    cat(run, ": DIMENSION MISMATCH old=", paste(dim(old_c), collapse="x"),
        " new=", paste(dim(new_c), collapse="x"), "\n")
    next
  }

  diffs <- 0
  diff_report <- c()
  for (col in compare_cols) {
    if (col == "Line") next
    o <- old_c[[col]]
    n <- new_c[[col]]
    if (is.numeric(o) && is.numeric(n)) {
      mismatch <- which(!(is.na(o) & is.na(n)) & (is.na(o) != is.na(n) | abs(o - n) > 1e-6))
    } else {
      mismatch <- which(!(is.na(o) & is.na(n)) & (is.na(o) != is.na(n) | as.character(o) != as.character(n)))
    }
    if (length(mismatch) > 0) {
      diffs <- diffs + length(mismatch)
      diff_report <- c(diff_report, paste0(col, ": ", length(mismatch), " row(s) differ (Lines: ",
                                            paste(head(old_c$Line[mismatch], 5), collapse=","), ")"))
    }
  }

  cat("===", run, "===\n")
  cat("  Rows compared:", nrow(old_c), "| Columns compared:", length(compare_cols) - 1, "\n")
  if (diffs == 0) {
    cat("  ZERO unexpected differences.\n")
  } else {
    cat("  ", diffs, "total differing cells:\n")
    for (r in diff_report) cat("    -", r, "\n")
  }
  cat("\n")
}
