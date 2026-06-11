# ============================================================
# DualMethodProcess.R
# One-off script to process the 20260428 Cal Check sequence
# which uses two GC methods (V8 and GERSTEL V4) with different
# retention times and oven programs.
#
# This script:
#   1. Processes all SIM files with method-appropriate RT windows
#   2. Builds separate calibration curves for each method
#   3. Quantifies QCs against their respective calibration
#   4. Outputs comparison plots and results CSV
#
# Run standalone — sources GlobalCode.R and ModPeaks.R for
# signal processing functions.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# ============================================================
# CONFIGURATION
# ============================================================

data_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260428 Cal Check"

# RT configurations (seconds) for each method
# Adjust these if peaks are not found on first run
rt_V8 <- list(
  petn_is = 137,     # NG
  petn    = 273.1,   # PETN
  rdx_is  = 321.7,   # 15N-RDX
  rdx     = 321.7    # RDX
)

rt_V4 <- list(
  petn_is = 77,      # NG (~60s earlier)
  petn    = 213,     # PETN (~60s earlier)
  rdx_is  = 261,     # 15N-RDX (~60s earlier)
  rdx     = 261      # RDX (~60s earlier)
)

# Method assignment: sequence lines using each method (from TSV)
# V8: lines 1-18, 29
# V4: lines 19-28, 30-33
v8_lines <- c(1:18, 29)
v4_lines <- c(19:28, 30:33)

# RT window half-width for peak searching (seconds)
rt_half_width <- 15

# SNR thresholds
snr_detect <- 3
snr_quant  <- 10

# Noise window parameters
noise_offset <- 6
noise_width  <- 6

# Calibration model
cal_model_type <- "quadratic"  # "quadratic" or "linear"

# ============================================================
# SETUP: Source dependencies
# ============================================================

message("=== Dual Method Processing: 20260428 Cal Check ===")

# Source ModPeaks.R for signal processing functions
source("C:/Users/A Bruce - User/Documents/ClaudeSpace/GCMSQuantitation/v2-NG/Code/ModPeaks.R")

# Paths
sim_dir <- file.path(data_dir, "GcDataConvertedRcode", "SIM")
results_dir <- file.path(data_dir, "Results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Rolling average setting (from GlobalCode.R)
rolling.average <- 7

# Signal threshold
SignalMaxThresholdSIM <- 100

# ============================================================
# PARSE SEQUENCE LOG
# ============================================================

tsv_files <- list.files(data_dir, pattern = "Sequence Log.*\\.TSV$", full.names = TRUE)
if (length(tsv_files) == 0) stop("No sequence log TSV found in ", data_dir)
tsv_file <- tsv_files[length(tsv_files)]  # use most recent

tsv_raw <- readLines(tsv_file)
# Skip header lines (first 2)
tsv_data <- read.delim(text = tsv_raw[3:length(tsv_raw)], sep = "\t",
                       header = FALSE, stringsAsFactors = FALSE)

# Extract key columns
seq_info <- data.frame(
  Line = tsv_data$V1,
  SampleName = trimws(tsv_data$V2),
  Vial = tsv_data$V3,
  MethodFile = trimws(tsv_data$V6),
  DataFile = trimws(tsv_data$V8),
  RunType = trimws(tsv_data$V9),
  stringsAsFactors = FALSE
)

# Assign method group
seq_info$MethodGroup <- ifelse(seq_info$Line %in% v8_lines, "V8",
                               ifelse(seq_info$Line %in% v4_lines, "V4", "Unknown"))

# Derive file number from DataFile (e.g. "001.D" -> "001")
seq_info$FileNum <- gsub("\\.D$", "", seq_info$DataFile)

message("  Sequence: ", nrow(seq_info), " lines")
message("  V8 lines: ", sum(seq_info$MethodGroup == "V8"))
message("  V4 lines: ", sum(seq_info$MethodGroup == "V4"))

# ============================================================
# HELPER: Build RT configs for a given method
# ============================================================

build_configs <- function(rt_config) {
  # Returns analyte and IS configs with noise windows
  petn_is <- list(mz = 76, rt = rt_config$petn_is)
  # NG at V4 may be very early - use post-peak noise if rt < 125
  if (rt_config$petn_is < 125) {
    petn_is$noise_window <- c(rt_config$petn_is + noise_offset,
                               rt_config$petn_is + noise_offset + noise_width)
  } else {
    petn_is$noise_window <- c(rt_config$petn_is - noise_offset - noise_width,
                               rt_config$petn_is - noise_offset)
  }

  rdx_is <- list(mz = 122, rt = rt_config$rdx_is)
  rdx_is$noise_window <- c(rt_config$rdx_is - noise_offset - noise_width,
                            rt_config$rdx_is - noise_offset)

  analytes_cfg <- list(
    PETN = list(mz = 46, rt = rt_config$petn),
    RDX  = list(mz = 120, rt = rt_config$rdx)
  )
  for (nm in names(analytes_cfg)) {
    analytes_cfg[[nm]]$noise_window <- c(analytes_cfg[[nm]]$rt - noise_offset - noise_width,
                                          analytes_cfg[[nm]]$rt - noise_offset)
  }

  list(petn_is = petn_is, rdx_is = rdx_is, analytes = analytes_cfg)
}

# ============================================================
# PROCESS ALL SIM FILES
# ============================================================

message("\n  Processing SIM files...")

all_results <- list()

for (i in seq_len(nrow(seq_info))) {
  line_info <- seq_info[i, ]
  file_num <- line_info$FileNum
  method_group <- line_info$MethodGroup

  # Select RT config
  if (method_group == "V8") {
    cfg <- build_configs(rt_V8)
  } else if (method_group == "V4") {
    cfg <- build_configs(rt_V4)
  } else {
    message("  Skipping line ", line_info$Line, " (unknown method)")
    next
  }

  # Read SIM data
  sim_file <- file.path(sim_dir, paste0(file_num, ".csv"))
  if (!file.exists(sim_file)) {
    message("  WARNING: SIM file not found: ", sim_file)
    next
  }

  sim_data <- read.csv(sim_file, stringsAsFactors = FALSE)
  sim_data <- sim_data %>% mutate(across(everything(), ~as.numeric(as.character(.x))))

  # Build RT windows for all m/z channels needed
  all_mz <- unique(c(cfg$rdx_is$mz, cfg$petn_is$mz,
                     sapply(cfg$analytes, function(a) a$mz)))

  rt_windows_by_mz <- list()
  for (mz_val in all_mz) {
    windows <- list()
    if (cfg$rdx_is$mz == mz_val) windows <- c(windows, list(c(cfg$rdx_is$rt, rt_half_width)))
    if (cfg$petn_is$mz == mz_val) windows <- c(windows, list(c(cfg$petn_is$rt, rt_half_width)))
    for (a in cfg$analytes) {
      if (a$mz == mz_val) windows <- c(windows, list(c(a$rt, rt_half_width)))
    }
    rt_windows_by_mz[[as.character(mz_val)]] <- windows
  }

  # Process each m/z channel
  channel_results <- list()
  for (mz_val in all_mz) {
    channel_results[[as.character(mz_val)]] <- process_sim_channel(
      sim_data, mz_val,
      rt_windows_by_mz[[as.character(mz_val)]],
      rolling_avg = rolling.average,
      min_peak_height = SignalMaxThresholdSIM
    )
  }

  # Extract RDX IS
  rdx_is_ch <- channel_results[[as.character(cfg$rdx_is$mz)]]
  rdx_is_result <- extract_peak_area(rdx_is_ch$peaks, rdx_is_ch$signal,
                                      cfg$rdx_is$rt, rdx_is_ch$step_size,
                                      baseline_corrected = rdx_is_ch$baseline_corrected,
                                      noise_window = cfg$rdx_is$noise_window,
                                      snr_lod = snr_detect, snr_loq = snr_quant)

  # Extract PETN IS (NG)
  petn_is_ch <- channel_results[[as.character(cfg$petn_is$mz)]]
  petn_is_result <- extract_peak_area(petn_is_ch$peaks, petn_is_ch$signal,
                                       cfg$petn_is$rt, petn_is_ch$step_size,
                                       baseline_corrected = petn_is_ch$baseline_corrected,
                                       noise_window = cfg$petn_is$noise_window,
                                       snr_lod = snr_detect, snr_loq = snr_quant)

  # Extract analytes
  analyte_results <- list()
  for (nm in names(cfg$analytes)) {
    a <- cfg$analytes[[nm]]
    a_ch <- channel_results[[as.character(a$mz)]]
    analyte_results[[nm]] <- extract_peak_area(
      a_ch$peaks, a_ch$signal, a$rt, a_ch$step_size,
      baseline_corrected = a_ch$baseline_corrected,
      noise_window = a$noise_window,
      snr_lod = snr_detect, snr_loq = snr_quant
    )
  }

  # Store results
  row <- data.frame(
    Line = line_info$Line,
    FileNum = file_num,
    SampleName = line_info$SampleName,
    Type = line_info$RunType,
    MethodGroup = method_group,
    rdx_is_rt = rdx_is_result$rt,
    rdx_is_pa = rdx_is_result$pa,
    rdx_is_snr_flag = rdx_is_result$snr_flag,
    petn_is_rt = petn_is_result$rt,
    petn_is_pa = petn_is_result$pa,
    petn_is_snr_flag = petn_is_result$snr_flag,
    petn_rt = analyte_results$PETN$rt,
    petn_pa = analyte_results$PETN$pa,
    petn_snr_flag = analyte_results$PETN$snr_flag,
    rdx_rt = analyte_results$RDX$rt,
    rdx_pa = analyte_results$RDX$pa,
    rdx_snr_flag = analyte_results$RDX$snr_flag,
    stringsAsFactors = FALSE
  )

  all_results[[i]] <- row

  message("  Line ", sprintf("%02d", line_info$Line), " (", method_group, ") ",
          line_info$SampleName,
          " | PETN PA: ", ifelse(is.na(analyte_results$PETN$pa), "NA",
                                  round(analyte_results$PETN$pa, 0)),
          " | RDX PA: ", ifelse(is.na(analyte_results$RDX$pa), "NA",
                                 round(analyte_results$RDX$pa, 0)))
}

# Combine results
Results <- bind_rows(all_results)

# ============================================================
# PARSE CALIBRATION LEVELS
# ============================================================

# Parse CalLevel from SampleName for Cal/QC rows
Results$CalLevel <- NA_real_
cal_pattern <- "^(\\d+\\.?\\d*)ng"
for (i in seq_len(nrow(Results))) {
  if (Results$Type[i] %in% c("Cal", "QC")) {
    m <- regmatches(Results$SampleName[i], regexec(cal_pattern, Results$SampleName[i]))
    if (length(m[[1]]) >= 2) {
      Results$CalLevel[i] <- as.numeric(m[[1]][2])
    }
  }
}

# Compute ratios
Results$petn_ratio <- Results$petn_pa / Results$petn_is_pa
Results$rdx_ratio <- Results$rdx_pa / Results$rdx_is_pa

message("\n  Results compiled: ", nrow(Results), " rows")
message("  Cal rows: ", sum(Results$Type == "Cal", na.rm = TRUE))
message("  QC rows: ", sum(Results$Type == "QC", na.rm = TRUE))

# ============================================================
# BUILD CALIBRATION CURVES (one per method)
# ============================================================

message("\n  Building calibration curves...")

# solve_concentration function (copied from MetadataLinearWeighted.R)
solve_concentration <- function(y, lin_model, cal_y_range, quad_model = NULL) {
  if (is.na(y)) return(list(value = NA_real_, method = "No_peak", range_flag = NA_character_))
  if (y < cal_y_range[1]) { range_flag <- "Below_range"
  } else if (y > cal_y_range[2]) { range_flag <- "Above_range"
  } else { range_flag <- "Within_range" }

  if (!is.null(quad_model)) {
    coefs <- coef(quad_model)
    a <- coefs["I(CalLevelAdj^2)"]
    b <- coefs["CalLevelAdj"]
    c_int <- coefs["(Intercept)"]
    disc <- b^2 - 4 * a * (c_int - y)
    if (is.finite(disc) && disc >= 0 && abs(a) > .Machine$double.eps) {
      roots <- c((-b + sqrt(disc)) / (2 * a), (-b - sqrt(disc)) / (2 * a))
      roots <- roots[is.finite(roots) & roots >= 0]
      if (length(roots) > 0) return(list(value = min(roots), method = "Quadratic", range_flag = range_flag))
    }
  }
  lin <- coef(lin_model)
  slope <- lin[2]; intercept <- lin[1]
  if (is.na(slope) || abs(slope) < .Machine$double.eps * 100) {
    return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
  }
  conc <- (y - intercept) / slope
  if (is.finite(conc) && conc >= 0) {
    return(list(value = conc, method = "Linear", range_flag = range_flag))
  }
  return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
}

# Function to build calibration for one method group
build_calibration <- function(results_df, method_name) {
  cal_data <- results_df %>%
    filter(Type == "Cal", !is.na(CalLevel), !is.na(petn_pa))

  if (nrow(cal_data) < 3) {
    message("    WARNING: <3 calibrants for ", method_name, " -- skipping")
    return(NULL)
  }

  cal_data$CalLevelAdj <- cal_data$CalLevel

  # PETN calibration (peak area)
  petn_lin <- lm(petn_pa ~ CalLevelAdj, data = cal_data, weights = 1 / CalLevelAdj)
  petn_quad <- NULL
  if (cal_model_type == "quadratic" && nrow(cal_data) >= 3) {
    petn_quad <- lm(petn_pa ~ CalLevelAdj + I(CalLevelAdj^2), data = cal_data, weights = 1 / CalLevelAdj)
  }
  petn_y_range <- range(cal_data$petn_pa, na.rm = TRUE)

  # RDX calibration (peak area)
  rdx_cal <- cal_data %>% filter(!is.na(rdx_pa))
  rdx_lin <- lm(rdx_pa ~ CalLevelAdj, data = rdx_cal, weights = 1 / CalLevelAdj)
  rdx_quad <- NULL
  if (cal_model_type == "quadratic" && nrow(rdx_cal) >= 3) {
    rdx_quad <- lm(rdx_pa ~ CalLevelAdj + I(CalLevelAdj^2), data = rdx_cal, weights = 1 / CalLevelAdj)
  }
  rdx_y_range <- range(rdx_cal$rdx_pa, na.rm = TRUE)

  # RDX ratio calibration
  rdx_ratio_cal <- cal_data %>% filter(!is.na(rdx_ratio))
  rdx_ratio_lin <- NULL; rdx_ratio_quad <- NULL; rdx_ratio_y_range <- c(NA, NA)
  if (nrow(rdx_ratio_cal) >= 3) {
    rdx_ratio_lin <- lm(rdx_ratio ~ CalLevelAdj, data = rdx_ratio_cal, weights = 1 / CalLevelAdj)
    if (cal_model_type == "quadratic") {
      rdx_ratio_quad <- lm(rdx_ratio ~ CalLevelAdj + I(CalLevelAdj^2), data = rdx_ratio_cal, weights = 1 / CalLevelAdj)
    }
    rdx_ratio_y_range <- range(rdx_ratio_cal$rdx_ratio, na.rm = TRUE)
  }

  list(
    method = method_name,
    cal_data = cal_data,
    petn = list(lin = petn_lin, quad = petn_quad, y_range = petn_y_range),
    rdx = list(lin = rdx_lin, quad = rdx_quad, y_range = rdx_y_range),
    rdx_ratio = list(lin = rdx_ratio_lin, quad = rdx_ratio_quad, y_range = rdx_ratio_y_range)
  )
}

cal_V8 <- build_calibration(Results %>% filter(MethodGroup == "V8"), "V8")
cal_V4 <- build_calibration(Results %>% filter(MethodGroup == "V4"), "V4")

# ============================================================
# QUANTIFY QCs
# ============================================================

message("\n  Quantifying QC injections...")

quantify_rows <- function(results_df, cal_obj) {
  if (is.null(cal_obj)) return(results_df)

  results_df$petn_conc <- NA_real_
  results_df$rdx_conc <- NA_real_
  results_df$rdx_conc_ratio <- NA_real_
  results_df$petn_bias <- NA_real_
  results_df$rdx_bias <- NA_real_
  results_df$rdx_bias_ratio <- NA_real_

  for (i in seq_len(nrow(results_df))) {
    if (results_df$Type[i] == "QC" && !is.na(results_df$CalLevel[i])) {
      true_conc <- results_df$CalLevel[i]

      # PETN PA
      if (!is.na(results_df$petn_pa[i])) {
        res <- solve_concentration(results_df$petn_pa[i], cal_obj$petn$lin,
                                    cal_obj$petn$y_range, cal_obj$petn$quad)
        results_df$petn_conc[i] <- res$value
        if (!is.na(res$value)) {
          results_df$petn_bias[i] <- (res$value - true_conc) / true_conc * 100
        }
      }

      # RDX PA
      if (!is.na(results_df$rdx_pa[i])) {
        res <- solve_concentration(results_df$rdx_pa[i], cal_obj$rdx$lin,
                                    cal_obj$rdx$y_range, cal_obj$rdx$quad)
        results_df$rdx_conc[i] <- res$value
        if (!is.na(res$value)) {
          results_df$rdx_bias[i] <- (res$value - true_conc) / true_conc * 100
        }
      }

      # RDX ratio
      if (!is.na(results_df$rdx_ratio[i]) && !is.null(cal_obj$rdx_ratio$lin)) {
        res <- solve_concentration(results_df$rdx_ratio[i], cal_obj$rdx_ratio$lin,
                                    cal_obj$rdx_ratio$y_range, cal_obj$rdx_ratio$quad)
        results_df$rdx_conc_ratio[i] <- res$value
        if (!is.na(res$value)) {
          results_df$rdx_bias_ratio[i] <- (res$value - true_conc) / true_conc * 100
        }
      }
    }
  }
  results_df
}

Results_V8 <- quantify_rows(Results %>% filter(MethodGroup == "V8"), cal_V8)
Results_V4 <- quantify_rows(Results %>% filter(MethodGroup == "V4"), cal_V4)

Results_all <- bind_rows(Results_V8, Results_V4) %>% arrange(Line)

# ============================================================
# OUTPUT: Results CSV
# ============================================================

csv_path <- file.path(results_dir, "CalCheck_Results.csv")
write.csv(Results_all, csv_path, row.names = FALSE)
message("\n  Results saved: ", csv_path)

# ============================================================
# PLOT: Calibration Comparison (side by side)
# ============================================================

message("  Generating calibration comparison plot...")

plot_cal <- function(cal_obj, analyte_name, pa_col) {
  if (is.null(cal_obj)) return(ggplot() + theme_void() + labs(title = "No data"))

  cal_data <- cal_obj$cal_data
  if (analyte_name == "PETN") {
    cal_sub <- cal_data %>% filter(!is.na(.data[[pa_col]]))
    mod <- cal_obj$petn
  } else {
    cal_sub <- cal_data %>% filter(!is.na(.data[[pa_col]]))
    mod <- cal_obj$rdx
  }

  pred_x <- seq(0, max(cal_sub$CalLevelAdj) * 1.1, length.out = 100)
  if (!is.null(mod$quad)) {
    pred_y <- predict(mod$quad, newdata = data.frame(CalLevelAdj = pred_x))
  } else {
    pred_y <- predict(mod$lin, newdata = data.frame(CalLevelAdj = pred_x))
  }
  pred_df <- data.frame(x = pred_x, y = pred_y)

  r2 <- ifelse(!is.null(mod$quad),
               summary(mod$quad)$adj.r.squared,
               summary(mod$lin)$adj.r.squared)

  ggplot() +
    geom_point(data = cal_sub, aes(x = CalLevelAdj, y = .data[[pa_col]]),
               size = 3, colour = "steelblue") +
    geom_line(data = pred_df, aes(x = x, y = y), colour = "red", linewidth = 0.8) +
    expand_limits(x = 0, y = 0) +
    theme_bw() +
    labs(
      title = paste0(analyte_name, " — ", cal_obj$method),
      subtitle = paste0("Adj R² = ", round(r2, 5)),
      x = "Concentration (ng/µL)",
      y = "Peak Area"
    )
}

p_petn_v8 <- plot_cal(cal_V8, "PETN", "petn_pa")
p_petn_v4 <- plot_cal(cal_V4, "PETN", "petn_pa")
p_rdx_v8 <- plot_cal(cal_V8, "RDX", "rdx_pa")
p_rdx_v4 <- plot_cal(cal_V4, "RDX", "rdx_pa")

p_cal_compare <- arrangeGrob(
  p_petn_v8, p_petn_v4, p_rdx_v8, p_rdx_v4,
  ncol = 2,
  top = grid::textGrob("Calibration Comparison: V8 (left) vs V4 (right)",
                        gp = grid::gpar(fontsize = 13, fontface = "bold"))
)

ggsave(file.path(results_dir, "CalCheck_Calibration_Comparison.png"),
       p_cal_compare, width = 12, height = 8, dpi = 300)
message("  Saved: CalCheck_Calibration_Comparison.png")

# ============================================================
# PLOT: QC Accuracy Summary
# ============================================================

message("  Generating QC summary plot...")

qc_data <- Results_all %>%
  filter(Type == "QC") %>%
  select(Line, MethodGroup, CalLevel, petn_bias, rdx_bias, rdx_bias_ratio) %>%
  pivot_longer(cols = c(petn_bias, rdx_bias, rdx_bias_ratio),
               names_to = "Metric", values_to = "Bias") %>%
  filter(!is.na(Bias)) %>%
  mutate(Metric = case_when(
    Metric == "petn_bias" ~ "PETN (PA)",
    Metric == "rdx_bias" ~ "RDX (PA)",
    Metric == "rdx_bias_ratio" ~ "RDX (Ratio)"
  ))

if (nrow(qc_data) > 0) {
  p_qc <- ggplot(qc_data, aes(x = Line, y = Bias, colour = MethodGroup, shape = factor(CalLevel))) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_hline(yintercept = c(-20, 20), linetype = "dashed", colour = "red") +
    facet_wrap(~Metric, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(
      title = "QC % Bias by Method",
      subtitle = "Dashed red lines = ±20% acceptance limit",
      x = "Sequence Line",
      y = "% Bias",
      colour = "Method",
      shape = "QC Level (ng)"
    )

  ggsave(file.path(results_dir, "CalCheck_QC_Summary.png"),
         p_qc, width = 10, height = 8, dpi = 300)
  message("  Saved: CalCheck_QC_Summary.png")
} else {
  message("  No QC bias data to plot.")
}

# ============================================================
# PLOT: QC Peak Area Trends
# ============================================================

message("  Generating QC PA trend plot...")

qc_pa <- Results_all %>%
  filter(Type == "QC") %>%
  select(Line, MethodGroup, CalLevel, petn_pa, rdx_pa) %>%
  pivot_longer(cols = c(petn_pa, rdx_pa), names_to = "Analyte", values_to = "PA") %>%
  filter(!is.na(PA)) %>%
  mutate(Analyte = ifelse(Analyte == "petn_pa", "PETN", "RDX"))

if (nrow(qc_pa) > 0) {
  p_pa <- ggplot(qc_pa, aes(x = Line, y = PA, colour = MethodGroup, shape = factor(CalLevel))) +
    geom_point(size = 3) +
    geom_line(aes(group = interaction(MethodGroup, CalLevel)), alpha = 0.4) +
    facet_wrap(~Analyte, ncol = 1, scales = "free_y") +
    expand_limits(y = 0) +
    theme_bw() +
    labs(
      title = "QC Peak Area Across Sequence",
      x = "Sequence Line",
      y = "Peak Area (Arb. Units)",
      colour = "Method",
      shape = "QC Level (ng)"
    )

  ggsave(file.path(results_dir, "CalCheck_QC_PA_Trend.png"),
         p_pa, width = 10, height = 7, dpi = 300)
  message("  Saved: CalCheck_QC_PA_Trend.png")
}

# ============================================================
# PRINT SUMMARY TABLE
# ============================================================

message("\n  === QC Summary ===")
qc_summary <- Results_all %>%
  filter(Type == "QC") %>%
  select(Line, MethodGroup, CalLevel, petn_conc, petn_bias, rdx_conc, rdx_bias, rdx_conc_ratio, rdx_bias_ratio)

print(as.data.frame(qc_summary), row.names = FALSE)

# Print detected RTs for verification
message("\n  === Detected Retention Times ===")
rt_check <- Results_all %>%
  filter(Type %in% c("Cal", "QC"), !is.na(petn_rt) | !is.na(rdx_rt)) %>%
  group_by(MethodGroup) %>%
  summarise(
    PETN_RT_mean = round(mean(petn_rt, na.rm = TRUE), 1),
    PETN_IS_RT_mean = round(mean(petn_is_rt, na.rm = TRUE), 1),
    RDX_RT_mean = round(mean(rdx_rt, na.rm = TRUE), 1),
    RDX_IS_RT_mean = round(mean(rdx_is_rt, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(as.data.frame(rt_check), row.names = FALSE)

message("\n=== Dual Method Processing Complete ===")
