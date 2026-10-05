# Combined Pressure-Trace Comparison: 50g/200g Samples -- Pilot + Main Study
# ===============================================================================
# Extends the existing per-study "Xg Samples - Full Applied Mass Traces
# (Distance-Based)" comparison figures (produced independently by
# ASTRA/main_study_batch_process.R for Main Study samples, and
# ASTRA/time_based_batch_process_adaptive_DISTANCE.R for Pilot Study
# samples) to show, for the two pressure levels the two studies actually
# SHARE (50g and 200g), exactly the sample set that
# ASTRA/doe/main_study_analysis.R's build_pooled_dataset() pools into its
# own combined statistical model/hinge plot:
#   - Main Study:  SampleType == "Main", analysis_accepted %in% c("PASS","PASS*")
#   - Pilot Study: SampleType == "Pilot", Solvent_level == "present" (wet-only),
#                  Pressure_g %in% c(50, 200), analysis_accepted %in% c("PASS","PASS*")
# This is deliberately narrower than each study's own existing "Xg Samples"
# plot, which includes EVERY sample with a trace file regardless of QC
# status or wet/dry condition (e.g. the Pilot's own 50g plot currently
# includes 8 dry-swab samples and 1 QC-FAIL sample that build_pooled_dataset()
# correctly excludes from the statistical model) -- see this repo's
# investigation notes for the full sample-by-sample breakdown.
#
# Does NOT modify main_study_batch_process.R or
# time_based_batch_process_adaptive_DISTANCE.R (both are explicitly
# self-contained, single-study "SINGLE-SCRIPT DESIGN" pipelines per their
# own header comments) -- this is a new, standalone script that reads each
# study's own ALREADY-COMPUTED trace-processing outputs directly:
#   - <RunID>.csv                      (raw balance-log trace, in .../Pressure Traces/)
#   - <RunID>_summary_statistics.txt   (has "Stable range: X - Y g", the
#                                        adaptively-discovered per-sample
#                                        contact threshold band used to
#                                        recompute cumulative distance)
#   - <RunID>_detected_cycles.csv      (has Cycle 1's own start_time/end_time/
#                                        start_distance_mm, used for the
#                                        Cycle-1-peak alignment below)
# all under .../Pressure Traces/ProcessedData/<RunID>/ -- so no raw-data
# reprocessing or re-running of the (adaptive, non-trivial) cycle-detection
# algorithm is needed; the same deterministic distance-alignment formulas
# from main_study_batch_process.R (calculate_cumulative_distance(), and the
# Cycle-1-peak search window inside plot_full_traces_comparison()) are
# replicated verbatim below, just fed each study's own already-saved
# per-sample outputs.
#
# Styling: colour = Surface_type (matching every other combined ASTRA
# recovery/pressure figure) instead of the original wet/dry "Condition"
# colour -- since every sample in this combined set is wet by construction,
# a wet/dry colour scale would render as a single flat colour and convey no
# information. The facet strip label (RunID, e.g. "MAIN_037"/"PILOT_005")
# already makes which study each panel comes from unambiguous.
#
# Output:
#   - Main Study/50g_samples_full_traces_PilotMain.png
#   - Main Study/200g_samples_full_traces_PilotMain.png
#
# Author: OpenCode
# Date: 2026-09-10

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# ===============================================================================
# CONFIGURATION
# ===============================================================================

ASTRA_SWABBING_DIR <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing"
MAIN_DIR  <- file.path(ASTRA_SWABBING_DIR, "Main Study")
PILOT_DIR <- file.path(ASTRA_SWABBING_DIR, "Pilot Study")

MAIN_TRACE_DIR      <- file.path(MAIN_DIR, "Pressure Traces")
MAIN_PROCESSED_DIR  <- file.path(MAIN_DIR, "Pressure Traces/ProcessedData")
MAIN_METADATA_FILE  <- file.path(MAIN_DIR, "main_study_data_nested.csv")

PILOT_TRACE_DIR     <- file.path(PILOT_DIR, "Pressure Traces")
PILOT_PROCESSED_DIR <- file.path(PILOT_DIR, "Pressure Traces/ProcessedData")
PILOT_METADATA_FILE <- file.path(PILOT_DIR, "pilot_data_nested.csv")

OUTPUT_DIR <- MAIN_DIR

PRESSURE_LEVELS_TO_PLOT <- c(50, 200)

# Matches gcode_params$swab_speed_mm_s in both main_study_batch_process.R and
# time_based_batch_process_adaptive_DISTANCE.R (F5000 mm/min = 83.33 mm/s).
GCODE_PARAMS <- list(swab_speed_mm_s = 83.33)

DIST_LIMIT_MM       <- 1000   # matches params$comparison_distance_limit_mm
NCOL_FACET          <- 5      # matches params$comparison_ncol
PANEL_WIDTH_IN      <- 3.0
PANEL_HEIGHT_IN     <- 2.2
EXTRA_HEIGHT_IN     <- 1.2
PLOT_DPI            <- 300
TRACE_LINE_WIDTH    <- 0.7
TRACE_ALPHA         <- 0.9

# ===============================================================================
# REPLICATED HELPER FUNCTIONS
# (copied/adapted from main_study_batch_process.R -- see header for why this
# is a standalone copy rather than a source() of that file)
# ===============================================================================

clean_and_read_csv <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  proper_header <- "timestamp,elapsed_sec,analogValue,load,distance"
  header_line <- NULL

  for (i in 1:min(20, length(lines))) {
    line <- lines[i]
    if (grepl("analogValue.*load.*distance", line)) {
      if (i > 1 && (grepl("^[0-9]", lines[i - 1]) || grepl("timestamp.*elapsed_sec", lines[i - 1]))) {
        header_line <- i + 1
        lines <- c(proper_header, lines[header_line:length(lines)])
        header_line <- 1
        break
      } else {
        header_line <- i
        break
      }
    }
    if (line == proper_header) {
      header_line <- i
      break
    }
  }

  if (is.null(header_line)) {
    first_line <- lines[1]
    if (grepl("timestamp,elapsed_sec", first_line) && grepl("[0-9]", first_line)) {
      lines[1] <- proper_header
      header_line <- 1
    }
  }

  if (!is.null(header_line) && header_line > 1) {
    lines <- lines[header_line:length(lines)]
  }

  if (is.null(header_line)) {
    warning(sprintf("Could not find valid header in %s", filepath))
    return(NULL)
  }

  temp_file <- tempfile(fileext = ".csv")
  writeLines(lines, temp_file)
  data <- tryCatch(read.csv(temp_file, stringsAsFactors = FALSE),
                    error = function(e) NULL)
  unlink(temp_file)

  if (!is.null(data)) {
    required_cols <- c("timestamp", "elapsed_sec", "analogValue", "load", "distance")
    if (!all(required_cols %in% colnames(data)) || !is.numeric(data$elapsed_sec)) {
      warning(sprintf("Malformed/unexpected columns in %s", filepath))
      return(NULL)
    }
  }
  data
}

calculate_cumulative_distance <- function(data, stable_range_low, stable_range_high, gcode_params) {
  data$is_contact <- FALSE
  data$distance_mm <- 0.0
  cumulative_distance <- 0.0

  for (i in 1:nrow(data)) {
    is_contact <- data$load[i] >= stable_range_low & data$load[i] <= stable_range_high
    data$is_contact[i] <- is_contact
    if (i > 1 && is_contact) {
      time_delta <- data$elapsed_sec[i] - data$elapsed_sec[i - 1]
      if (time_delta > 0 && time_delta < 1.0) {
        cumulative_distance <- cumulative_distance + time_delta * gcode_params$swab_speed_mm_s
      }
    }
    data$distance_mm[i] <- cumulative_distance
  }
  data
}

# Parses "  Stable range: 39.8 - 55.8 g" out of <RunID>_summary_statistics.txt.
parse_stable_range <- function(summary_file) {
  if (!file.exists(summary_file)) return(NULL)
  lines <- readLines(summary_file, warn = FALSE)
  m <- regmatches_first("Stable range:\\s*([0-9.]+)\\s*-\\s*([0-9.]+)\\s*g", lines)
  if (is.null(m)) return(NULL)
  list(low = as.numeric(m[2]), high = as.numeric(m[3]))
}

regmatches_first <- function(pattern, lines) {
  hit <- grep(pattern, lines, value = TRUE)
  if (length(hit) == 0) return(NULL)
  m <- regmatches(hit[1], regexec(pattern, hit[1]))[[1]]
  if (length(m) < 3) return(NULL)
  m
}

# Builds one sample's fully-aligned trace (time_sec, pressure_g, distance_mm),
# replicating load_raw_trace_data(compute_distance=TRUE) + the Cycle-1-peak
# alignment block inside plot_full_traces_comparison() exactly.
build_sample_trace <- function(run_id, trace_dir, processed_dir, target_pressure) {
  raw_file <- file.path(trace_dir, paste0(run_id, ".csv"))
  summary_file <- file.path(processed_dir, run_id, paste0(run_id, "_summary_statistics.txt"))
  cycles_file <- file.path(processed_dir, run_id, paste0(run_id, "_detected_cycles.csv"))

  if (!file.exists(raw_file) || !file.exists(summary_file) || !file.exists(cycles_file)) {
    cat(sprintf("    %s: SKIPPED (missing raw trace / summary / cycles file)\n", run_id))
    return(NULL)
  }

  stable_range <- parse_stable_range(summary_file)
  if (is.null(stable_range)) {
    cat(sprintf("    %s: SKIPPED (could not parse stable range)\n", run_id))
    return(NULL)
  }

  raw_data <- clean_and_read_csv(raw_file)
  if (is.null(raw_data)) {
    cat(sprintf("    %s: SKIPPED (raw trace file unreadable)\n", run_id))
    return(NULL)
  }

  raw_data <- calculate_cumulative_distance(raw_data, stable_range$low, stable_range$high, GCODE_PARAMS)

  trace <- raw_data %>%
    select(elapsed_sec, load, distance_mm) %>%
    rename(time_sec = elapsed_sec, pressure_g = load)

  cycles <- read.csv(cycles_file, stringsAsFactors = FALSE)
  cycle1 <- cycles[cycles$cycle_num == 1 & !is.na(cycles$start_distance_mm), ]

  distance_offset <- 0
  if (nrow(cycle1) > 0) {
    search_start <- cycle1$start_time[1] - 5
    search_end <- cycle1$end_time[1] + 1
    peak_window <- trace[trace$time_sec >= search_start & trace$time_sec <= search_end, ]
    if (nrow(peak_window) > 0) {
      distance_offset <- peak_window$distance_mm[which.max(peak_window$pressure_g)]
    } else {
      distance_offset <- cycle1$start_distance_mm[1]
    }
  }
  trace$distance_mm <- trace$distance_mm - distance_offset

  cat(sprintf("    %s: %d data points (cycle-1 offset %.1fmm)\n", run_id, nrow(trace), distance_offset))
  trace
}

# ===============================================================================
# DETERMINE ELIGIBLE SAMPLES (exactly matching main_study_analysis.R's
# build_pooled_dataset() filter)
# ===============================================================================

main_meta <- read.csv(MAIN_METADATA_FILE, stringsAsFactors = FALSE)
pilot_meta <- read.csv(PILOT_METADATA_FILE, stringsAsFactors = FALSE)

get_eligible_main <- function(pressure_g) {
  main_meta %>%
    filter(SampleType == "Main", Pressure_g == pressure_g,
           analysis_accepted %in% c("PASS", "PASS*")) %>%
    transmute(RunID, Surface_type, Study = "Main")
}

get_eligible_pilot <- function(pressure_g) {
  pilot_meta %>%
    filter(SampleType == "Pilot", Pressure_g == pressure_g, Solvent_level == "present",
           analysis_accepted %in% c("PASS", "PASS*")) %>%
    transmute(RunID, Surface_type, Study = "Pilot")
}

# ===============================================================================
# BUILD + SAVE ONE COMBINED FIGURE PER PRESSURE LEVEL
# ===============================================================================

written_files <- character(0)

for (target_pressure in PRESSURE_LEVELS_TO_PLOT) {

  cat(sprintf("\n=== %dg: building combined Pilot+Main trace comparison ===\n", target_pressure))

  eligible <- bind_rows(get_eligible_main(target_pressure), get_eligible_pilot(target_pressure))
  cat(sprintf("  Eligible samples (QC-passed, wet-only for Pilot): %s\n",
              paste(eligible$RunID, collapse = ", ")))

  all_traces <- list()
  for (i in seq_len(nrow(eligible))) {
    run_id <- eligible$RunID[i]
    is_main <- eligible$Study[i] == "Main"
    trace <- build_sample_trace(
      run_id,
      trace_dir     = if (is_main) MAIN_TRACE_DIR else PILOT_TRACE_DIR,
      processed_dir = if (is_main) MAIN_PROCESSED_DIR else PILOT_PROCESSED_DIR,
      target_pressure = target_pressure
    )
    if (!is.null(trace)) {
      trace$sample_id <- run_id
      trace$Surface_type <- eligible$Surface_type[i]
      trace$Study <- eligible$Study[i]
      all_traces[[run_id]] <- trace
    }
  }

  if (length(all_traces) == 0) {
    cat("  No traces could be built -- skipping this pressure level.\n")
    next
  }

  plot_data <- bind_rows(all_traces) %>%
    filter(distance_mm >= 0, distance_mm <= DIST_LIMIT_MM)

  n_samples <- length(unique(plot_data$sample_id))
  ncol_facet <- min(NCOL_FACET, n_samples)
  n_rows <- ceiling(n_samples / ncol_facet)

  p <- ggplot(plot_data, aes(x = distance_mm, y = pressure_g, color = Surface_type, group = sample_id)) +
    geom_line(linewidth = TRACE_LINE_WIDTH, alpha = TRACE_ALPHA) +
    scale_color_manual(values = pal_surface_type, labels = c(steel = "Steel", abs = "ABS"), name = "Surface") +
    scale_x_continuous(breaks = seq(0, DIST_LIMIT_MM, DIST_LIMIT_MM / 5)) +
    facet_wrap(~ sample_id, ncol = ncol_facet) +
    labs(
      title = sprintf("%dg Samples - Full Applied Mass Traces (Distance-Based) -- Pilot + Main Study", target_pressure),
      subtitle = sprintf(
        "Only samples pooled into main_study_analysis.R's combined model (QC-passed; Pilot restricted to wet-only)\nFirst %.0fmm, aligned to each sample's Cycle 1 start | Shared y-axis scale across panels",
        DIST_LIMIT_MM
      ),
      x = "Cumulative swabbing distance (mm)",
      y = "Applied Mass (g)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "top",
      strip.text = element_text(face = "bold", size = 8)
    )

  plot_width <- ncol_facet * PANEL_WIDTH_IN
  plot_height <- n_rows * PANEL_HEIGHT_IN + EXTRA_HEIGHT_IN
  out_file <- file.path(OUTPUT_DIR, sprintf("%dg_samples_full_traces_PilotMain.png", target_pressure))

  pre_write_mtime <- if (file.exists(out_file)) file.info(out_file)$mtime else as.POSIXct(-Inf)
  ggsave(out_file, p, width = plot_width, height = plot_height, dpi = PLOT_DPI, limitsize = FALSE)

  if (file.exists(out_file) && file.info(out_file)$mtime > pre_write_mtime) {
    cat(sprintf("  Saved: %s\n", out_file))
    written_files <- c(written_files, out_file)
  } else {
    warning("ggsave() returned normally but ", out_file,
            " does not appear to have just been written -- check for a file lock (e.g. OneDrive sync) and re-run.")
  }
}

cat("\nDone. Files written:\n")
cat(paste0("  ", written_files, collapse = "\n"), "\n")
