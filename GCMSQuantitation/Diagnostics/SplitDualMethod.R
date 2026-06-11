# ============================================================
# SplitDualMethod.R
# Splits the 20260428 Cal Check sequence into two subfolders
# (V8 and V4) with their own SIM files and sequence logs,
# ready for independent pipeline runs.
#
# Run once to set up the folders, then run the pipeline
# twice with different RTs in GlobalCode.R.
# ============================================================

data_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260428 Cal Check"

# Method assignment by sequence line
v8_lines <- c(1:18, 29)
v4_lines <- c(19:28, 30:33)

# ============================================================
# 1. Create subfolder structures
# ============================================================

v8_dir <- file.path(data_dir, "V8")
v4_dir <- file.path(data_dir, "V4")

for (d in c(v8_dir, v4_dir)) {
  dir.create(file.path(d, "GcDataConvertedRcode", "SIM"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "GcDataConvertedRcode", "TIC"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "GcData"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "Results"), recursive = TRUE, showWarnings = FALSE)
}

message("Created V8 and V4 subfolders")

# ============================================================
# 2. Copy SIM and TIC files to appropriate subfolder
# ============================================================

sim_source <- file.path(data_dir, "GcDataConvertedRcode", "SIM")
tic_source <- file.path(data_dir, "GcDataConvertedRcode", "TIC")

for (line_num in v8_lines) {
  fname <- sprintf("%03d.csv", line_num)
  src <- file.path(sim_source, fname)
  if (file.exists(src)) file.copy(src, file.path(v8_dir, "GcDataConvertedRcode", "SIM", fname), overwrite = TRUE)
  src_tic <- file.path(tic_source, fname)
  if (file.exists(src_tic)) file.copy(src_tic, file.path(v8_dir, "GcDataConvertedRcode", "TIC", fname), overwrite = TRUE)
}

for (line_num in v4_lines) {
  fname <- sprintf("%03d.csv", line_num)
  src <- file.path(sim_source, fname)
  if (file.exists(src)) file.copy(src, file.path(v4_dir, "GcDataConvertedRcode", "SIM", fname), overwrite = TRUE)
  src_tic <- file.path(tic_source, fname)
  if (file.exists(src_tic)) file.copy(src_tic, file.path(v4_dir, "GcDataConvertedRcode", "TIC", fname), overwrite = TRUE)
}

message("Copied SIM/TIC files:")
message("  V8: ", length(list.files(file.path(v8_dir, "GcDataConvertedRcode", "SIM"))), " SIM files")
message("  V4: ", length(list.files(file.path(v4_dir, "GcDataConvertedRcode", "SIM"))), " SIM files")

# ============================================================
# 3. Create trimmed .LOG sequence files
# ============================================================

# Read the original .LOG file
log_files <- list.files(data_dir, pattern = "Sequence Log.*\\.LOG$", full.names = TRUE)
if (length(log_files) == 0) stop("No sequence log .LOG file found")
log_file <- log_files[length(log_files)]
all_log_lines <- readLines(log_file)

# Find the "Starting sequence" line
start_idx <- grep("Starting sequence", all_log_lines)
if (length(start_idx) == 0) stop("No 'Starting sequence' line found in .LOG")
start_line <- all_log_lines[start_idx[1]]

# Find all numbered sequence lines (e.g. "  1) Blank  ...")
numbered_pattern <- "^\\s*\\d+\\)"
numbered_indices <- grep(numbered_pattern, all_log_lines)

# Extract the line number from each numbered line
get_seq_num <- function(line) {
  m <- regmatches(line, regexec("^\\s*(\\d+)\\)", line))
  if (length(m[[1]]) >= 2) return(as.integer(m[[1]][2]))
  return(NA_integer_)
}

# Build a mapping: for each numbered line, also grab the following MPS tray line (if present)
# Each entry in the LOG is 2 lines: the numbered line + the MPS line
build_log_subset <- function(target_lines) {
  output <- character()
  output <- c(output, paste0("    ", start_line))
  output <- c(output, "     ")

  for (idx in numbered_indices) {
    seq_num <- get_seq_num(all_log_lines[idx])
    if (!is.na(seq_num) && seq_num %in% target_lines) {
      # Add the numbered line
      output <- c(output, all_log_lines[idx])
      # Add the next line (MPS tray info) if it exists and isn't another numbered line
      if (idx + 1 <= length(all_log_lines) && !grepl(numbered_pattern, all_log_lines[idx + 1])) {
        output <- c(output, all_log_lines[idx + 1])
      }
    }
  }

  output
}

# Write V8 .LOG
v8_log_content <- build_log_subset(v8_lines)
v8_log_path <- file.path(v8_dir, "Sequence Log V8.LOG")
writeLines(v8_log_content, v8_log_path)

# Write V4 .LOG
v4_log_content <- build_log_subset(v4_lines)
v4_log_path <- file.path(v4_dir, "Sequence Log V4.LOG")
writeLines(v4_log_content, v4_log_path)

message("Created .LOG sequence files:")
message("  V8: ", v8_log_path)
message("  V4: ", v4_log_path)

# ============================================================
# 4. Print instructions
# ============================================================

message("\n=== Setup Complete ===")
message("\nNext steps:")
message("  1. In GlobalCode.R, set DataFolder to:")
message("     \"", v8_dir, "\"")
message("     Keep current V8 RTs (PETN=273.1, RDX=321.7, NG=137, 15N-RDX=321.7)")
message("     Set reprocess_all <- TRUE")
message("     Run the full pipeline (GlobalCode.R -> Test.R -> MetadataLinearWeighted.R)")
message("")
message("  2. Then change DataFolder to:")
message("     \"", v4_dir, "\"")
message("     Change RTs to V4 values:")
message("       rdx_is_config$rt  <- 261")
message("       petn_is_config$rt <- 77")
message("       analytes$PETN$rt  <- 213")
message("       analytes$RDX$rt   <- 261")
message("     Run the full pipeline again")
message("")
message("  3. Compare the Results/ outputs from each subfolder")
