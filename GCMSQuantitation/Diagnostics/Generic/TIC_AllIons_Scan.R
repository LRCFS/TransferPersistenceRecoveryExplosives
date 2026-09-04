# ============================================================
# TIC_AllIons_Scan.R
# Comprehensive scan of ALL ions at RDX retention time to find
# IS candidates that are constant across calibrants but absent in blanks.
#
# Purpose: Find ions from 15N-RDX IS that are NOT concentration-dependent
#          and could serve as alternative quantification ions
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

message("=== TIC All Ions Scan for IS Candidates ===")

# ============================================================
# Configuration
# ============================================================

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConverterMs/"
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/TIC_Diagnostics/"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rdx_rt_target <- 5.79
rdx_rt_window <- 0.15

calibrants <- c("002", "003", "004", "005", "006", "007")
cal_conc <- c(0.2, 2, 4, 6, 8, 10)
names(cal_conc) <- calibrants

blanks <- c("001", "008", "010")

samples <- c("009", "011")
sample_conc <- c("009" = 6, "011" = 0.2)

all_files <- c(calibrants, blanks, samples)

# ============================================================
# Functions
# ============================================================

parse_ms1_file <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  scans <- list()
  current_scan <- NULL
  current_rt <- NULL
  ions <- list()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
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
  
  return(scans)
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
  
  max_ions <- all_ions %>%
    group_by(mz_int) %>%
    summarise(intensity = max(intensity, na.rm = TRUE), .groups = "drop") %>%
    rename(mz = mz_int)
  
  return(max_ions)
}

# ============================================================
# Processing: Extract ions from all files
# ============================================================

message("\n--- Extracting ions from all files ---")

all_ion_data <- list()

for (file in all_files) {
  ms1_path <- file.path(ms1_dir, paste0(file, ".ms1"))
  
  if (!file.exists(ms1_path)) {
    message("File not found: ", ms1_path)
    next
  }
  
  message("Processing ", file, "...")
  
  scans <- parse_ms1_file(ms1_path)
  ions <- get_all_ions_at_rt(scans, rdx_rt_target, rdx_rt_window)
  
  if (nrow(ions) > 0) {
    ions$file <- file
    all_ion_data[[file]] <- ions
  }
  
  message("  Found ", nrow(ions), " ions")
}

if (length(all_ion_data) == 0) {
  stop("No ions extracted from any file")
}

# ============================================================
# Build Master Ion Matrix
# ============================================================

message("\n--- Building master ion matrix ---")

all_ions_df <- do.call(rbind, all_ion_data)

ion_matrix <- all_ions_df %>%
  pivot_wider(
    names_from = file,
    values_from = intensity,
    names_prefix = "int_"
  )

calibrant_cols <- paste0("int_", calibrants)
blank_cols <- paste0("int_", blanks)
sample_cols <- paste0("int_", samples)

calibrant_present <- ion_matrix %>%
  filter(if_all(all_of(calibrant_cols), ~ !is.na(.)))

message("Total unique ions detected: ", nrow(ion_matrix))
message("Ions present in ALL calibrants: ", nrow(calibrant_present))

# ============================================================
# Calculate Metrics
# ============================================================

message("\n--- Calculating metrics ---")

ion_stats <- calibrant_present %>%
  rowwise() %>%
  mutate(
    cal_intensities = list(c_across(all_of(calibrant_cols))),
    blank_intensities = list(c_across(all_of(blank_cols))),
    sample_intensities = list(c_across(all_of(sample_cols)))
  ) %>%
  ungroup() %>%
  mutate(
    mean_cal = sapply(cal_intensities, mean, na.rm = TRUE),
    sd_cal = sapply(cal_intensities, sd, na.rm = TRUE),
    cv_cal = ifelse(mean_cal > 0, sd_cal / mean_cal * 100, NA),
    
    mean_blank = sapply(blank_intensities, mean, na.rm = TRUE),
    sd_blank = sapply(blank_intensities, sd, na.rm = TRUE),
    max_blank = sapply(blank_intensities, max, na.rm = TRUE),
    
    mean_sample = sapply(sample_intensities, mean, na.rm = TRUE),
    sd_sample = sapply(sample_intensities, sd, na.rm = TRUE)
  ) %>%
  select(-cal_intensities, -blank_intensities, -sample_intensities)

cal_values <- matrix(nrow = nrow(ion_stats), ncol = length(calibrants))
for (i in seq_along(calibrant_cols)) {
  cal_values[, i] <- ion_stats[[calibrant_cols[i]]]
}
ion_stats$cor_vs_conc <- apply(cal_values, 1, function(x) cor(x, cal_conc, use = "complete.obs"))

ion_stats <- ion_stats %>%
  mutate(
    signal_to_blank = mean_cal / (mean_blank + 1),
    excess_signal = mean_cal - max_blank,
    sample_deviation = ifelse(mean_cal > 0, abs(mean_sample - mean_cal) / mean_cal * 100, NA)
  )

# ============================================================
# Classification
# ============================================================

message("\n--- Classifying ions ---")

ion_stats <- ion_stats %>%
  mutate(
    classification = case_when(
      cv_cal < 15 & signal_to_blank > 10 & abs(cor_vs_conc) < 0.3 & 
        (is.na(sample_deviation) | sample_deviation < 30) ~ "Excellent IS candidate",
      
      cv_cal < 25 & signal_to_blank > 5 & abs(cor_vs_conc) < 0.5 & 
        (is.na(sample_deviation) | sample_deviation < 50) ~ "Good IS candidate",
      
      cv_cal > 30 & abs(cor_vs_conc) > 0.8 ~ "RDX-dependent (problem)",
      
      cv_cal < 25 & abs(cor_vs_conc) > 0.8 ~ "Ion enhancement effect",
      
      !is.na(sample_deviation) & sample_deviation > 50 ~ "Matrix effect warning",
      
      signal_to_blank < 2 ~ "Background only",
      
      TRUE ~ "Other"
    )
  )

table_class <- table(ion_stats$classification)
message("\nClassification summary:")
for (class in names(table_class)) {
  message(sprintf("  %s: %d ions", class, table_class[class]))
}

# ============================================================
# Analyze m/z 122 Specifically (Current IS)
# ============================================================

mz122_stats <- ion_stats %>% filter(mz == 122)

if (nrow(mz122_stats) > 0) {
  message("\n=== Current IS (m/z 122) Analysis ===")
  message(sprintf("Mean calibrant intensity: %.0f", mz122_stats$mean_cal))
  message(sprintf("CV across calibrants: %.1f%%", mz122_stats$cv_cal))
  message(sprintf("Correlation with concentration: %.3f", mz122_stats$cor_vs_conc))
  message(sprintf("Mean blank intensity: %.0f", mz122_stats$mean_blank))
  message(sprintf("Signal/Blank ratio: %.1f", mz122_stats$signal_to_blank))
  message(sprintf("Mean sample intensity: %.0f", mz122_stats$mean_sample))
  message(sprintf("Sample vs calibrant deviation: %.1f%%", mz122_stats$sample_deviation))
  message(sprintf("Classification: %s", mz122_stats$classification))
  
  if (mz122_stats$cor_vs_conc > 0.9) {
    message("\n>>> DIAGNOSIS: IS shows strong concentration dependence - RDX contamination confirmed")
  } else if (mz122_stats$cv_cal > 20) {
    message("\n>>> DIAGNOSIS: IS shows high variability - check for ion enhancement or matrix effects")
  } else {
    message("\n>>> DIAGNOSIS: IS appears relatively stable - check correlation for subtle effects")
  }
}

# ============================================================
# Identify Best Candidates
# ============================================================

is_candidates <- ion_stats %>%
  filter(grepl("candidate", classification, ignore.case = TRUE)) %>%
  arrange(cv_cal)

message("\n=== IS Candidates (sorted by CV) ===")
if (nrow(is_candidates) > 0) {
  for (i in 1:min(10, nrow(is_candidates))) {
    row <- is_candidates[i, ]
    message(sprintf("m/z %3d: CV=%.1f%%, S/B=%.1f, cor=%.2f, sample_dev=%.0f%%",
                    row$mz, row$cv_cal, row$signal_to_blank, row$cor_vs_conc,
                    ifelse(is.na(row$sample_deviation), 0, row$sample_deviation)))
  }
} else {
  message("No IS candidates found!")
}

# ============================================================
# Identify Problem Ions
# ============================================================

problem_ions <- ion_stats %>%
  filter(grepl("problem|enhancement", classification, ignore.case = TRUE)) %>%
  arrange(desc(cor_vs_conc))

message("\n=== Problem Ions (RDX-dependent) ===")
for (i in 1:min(10, nrow(problem_ions))) {
  row <- problem_ions[i, ]
  message(sprintf("m/z %3d: CV=%.1f%%, cor=%.3f, mean=%.0f",
                  row$mz, row$cv_cal, row$cor_vs_conc, row$mean_cal))
}

# ============================================================
# Output Tables
# ============================================================

output_cols <- c("mz", "mean_cal", "sd_cal", "cv_cal", "cor_vs_conc",
                 "mean_blank", "max_blank", "signal_to_blank",
                 "mean_sample", "sd_sample", "sample_deviation",
                 "classification")

write.csv(
  ion_stats %>% select(all_of(c(output_cols, calibrant_cols, blank_cols, sample_cols))),
  file.path(output_dir, "TIC_AllIons_FullResults.csv"),
  row.names = FALSE
)

if (nrow(is_candidates) > 0) {
  write.csv(
    is_candidates %>% select(all_of(output_cols)),
    file.path(output_dir, "TIC_IS_Candidates.csv"),
    row.names = FALSE
  )
}

# ============================================================
# Plots
# ============================================================

message("\n--- Generating plots ---")

# Plot 1: CV vs Signal/Blank
p1 <- ggplot(ion_stats, aes(x = signal_to_blank, y = cv_cal)) +
  geom_point(aes(color = cor_vs_conc), alpha = 0.7, size = 2) +
  scale_x_log10() +
  scale_color_gradient2(
    low = "blue", mid = "gray50", high = "red",
    midpoint = 0.5,
    name = "Cor vs Conc"
  ) +
  geom_hline(yintercept = 15, linetype = "dashed", color = "green3") +
  geom_hline(yintercept = 25, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "green3") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "green3") +
  geom_point(data = ion_stats %>% filter(mz == 122), 
             aes(x = signal_to_blank, y = cv_cal), 
             color = "black", size = 4, shape = 21, stroke = 2) +
  annotate("text", x = ion_stats$signal_to_blank[ion_stats$mz == 122],
           y = ion_stats$cv_cal[ion_stats$mz == 122] + 3,
           label = "m/z 122", fontface = "bold") +
  labs(
    title = "Ion Stability vs Signal-to-Blank Ratio",
    subtitle = "Low CV + High S/B = Good IS candidate | Circle = current IS (m/z 122)",
    x = "Signal/Blank Ratio (log scale)",
    y = "Coefficient of Variation (%)"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "TIC_CV_vs_SignalBlank.png"), p1, 
       width = 10, height = 7, dpi = 150)

# Plot 2: Correlation vs CV
p2 <- ggplot(ion_stats, aes(x = cor_vs_conc, y = cv_cal)) +
  geom_point(aes(color = classification), alpha = 0.7, size = 2) +
  scale_color_manual(values = pal_ion_classification) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_point(data = ion_stats %>% filter(mz == 122),
             aes(x = cor_vs_conc, y = cv_cal),
             color = "black", size = 4, shape = 21, stroke = 2) +
  annotate("text", x = ion_stats$cor_vs_conc[ion_stats$mz == 122] + 0.05,
           y = ion_stats$cv_cal[ion_stats$mz == 122],
           label = "m/z 122", fontface = "bold") +
  labs(
    title = "Ion Correlation vs Variability",
    subtitle = "Low cor + Low CV = IS candidate | High cor + High CV = RDX fragment",
    x = "Correlation with Concentration",
    y = "Coefficient of Variation (%)"
  ) +
  theme_bw()

ggsave(file.path(output_dir, "TIC_Correlation_vs_CV.png"), p2,
       width = 10, height = 7, dpi = 150)

# Plot 3: Top candidates intensity vs concentration
if (nrow(is_candidates) > 0) {
  
  top_candidates <- is_candidates %>%
    filter(!is.na(mean_cal), mean_cal > 0) %>%
    head(12)
  
  if (nrow(top_candidates) > 0) {
    
    plot_data <- data.frame()
    
    for (i in 1:nrow(top_candidates)) {
      mz_val <- top_candidates$mz[i]
      
      cal_int <- sapply(calibrant_cols, function(col) {
        val <- ion_stats %>% filter(mz == mz_val) %>% pull(col)
        if (length(val) > 0) val[1] else NA
      })
      
      blank_int <- sapply(blank_cols, function(col) {
        val <- ion_stats %>% filter(mz == mz_val) %>% pull(col)
        if (length(val) > 0) val[1] else NA
      })
      
      sample_int <- sapply(sample_cols, function(col) {
        val <- ion_stats %>% filter(mz == mz_val) %>% pull(col)
        if (length(val) > 0) val[1] else NA
      })
      
      cal_df <- data.frame(
        mz = mz_val,
        file = calibrants,
        type = "Calibrant",
        conc = cal_conc,
        intensity = cal_int
      )
      
      blank_df <- data.frame(
        mz = mz_val,
        file = blanks,
        type = "Blank",
        conc = 0,
        intensity = blank_int
      )
      
      sample_df <- data.frame(
        mz = mz_val,
        file = samples,
        type = "QC Sample",
        conc = c(cal_conc[calibrants == "005"], cal_conc[calibrants == "002"]),
        intensity = sample_int
      )
      
      plot_data <- rbind(plot_data, cal_df, blank_df, sample_df)
    }
    
    p3 <- ggplot(plot_data, aes(x = conc, y = intensity)) +
      geom_point(aes(color = type, shape = type), size = 2.5) +
      geom_line(aes(group = mz), alpha = 0.3) +
      facet_wrap(~ paste0("m/z ", mz), scales = "free_y") +
      scale_color_manual(values = pal_sample_role) +
      scale_shape_manual(values = c("Calibrant" = 16, "Blank" = 1, "QC Sample" = 17)) +
      labs(
        title = "IS Candidates: Intensity vs Concentration",
        subtitle = "Constant intensity across concentration = independent IS signal",
        x = "RDX Concentration (ng)",
        y = "Intensity"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggsave(file.path(output_dir, "TIC_IS_Candidates_Intensity.png"), p3,
           width = 12, height = 10, dpi = 150)
  }
}

# Plot 4: m/z 122 detailed analysis
mz122_plot_data <- data.frame()

if (nrow(mz122_stats) > 0) {
  
  cal_int <- sapply(calibrant_cols, function(col) {
    val <- ion_stats %>% filter(mz == 122) %>% pull(col)
    if (length(val) > 0) val[1] else NA
  })
  
  blank_int <- sapply(blank_cols, function(col) {
    val <- ion_stats %>% filter(mz == 122) %>% pull(col)
    if (length(val) > 0) val[1] else NA
  })
  
  sample_int <- sapply(sample_cols, function(col) {
    val <- ion_stats %>% filter(mz == 122) %>% pull(col)
    if (length(val) > 0) val[1] else NA
  })
  
  mz122_plot_data <- rbind(
    data.frame(mz = 122, file = calibrants, type = "Calibrant",
               conc = cal_conc, intensity = cal_int),
    data.frame(mz = 122, file = blanks, type = "Blank",
               conc = 0, intensity = blank_int),
    data.frame(mz = 122, file = samples, type = "QC Sample",
               conc = c(cal_conc[calibrants == "005"], cal_conc[calibrants == "002"]),
               intensity = sample_int)
  )
  
  mean_blank_val <- mean(blank_int, na.rm = TRUE)
  
  p4 <- ggplot(mz122_plot_data, aes(x = conc, y = intensity)) +
    geom_hline(yintercept = mean_blank_val, linetype = "dashed", color = "gray50") +
    annotate("text", x = 0.5, y = mean_blank_val + 200, 
             label = sprintf("Mean blank: %.0f", mean_blank_val), color = "gray50") +
    geom_point(aes(color = type, shape = type), size = 3) +
    geom_smooth(data = subset(mz122_plot_data, type == "Calibrant"),
                method = "lm", se = FALSE, color = "red", alpha = 0.5) +
    scale_color_manual(values = pal_sample_role) +
    scale_shape_manual(values = c("Calibrant" = 16, "Blank" = 1, "QC Sample" = 17)) +
    labs(
      title = "Current IS (m/z 122) Analysis",
      subtitle = sprintf("CV = %.1f%% | Cor vs Conc = %.3f | S/B = %.1f",
                         mz122_stats$cv_cal, mz122_stats$cor_vs_conc, mz122_stats$signal_to_blank),
      x = "RDX Concentration (ng)",
      y = "Intensity"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "TIC_mz122_Analysis.png"), p4,
         width = 8, height = 6, dpi = 150)
}

# ============================================================
# Final Summary
# ============================================================

message("\n============================================================")
message("SUMMARY")
message("============================================================")
message(sprintf("Output directory: %s", output_dir))
message("\nFiles written:")
message("  - TIC_AllIons_FullResults.csv")
if (nrow(is_candidates) > 0) message("  - TIC_IS_Candidates.csv")
message("  - TIC_CV_vs_SignalBlank.png")
message("  - TIC_Correlation_vs_CV.png")
if (nrow(is_candidates) > 0) message("  - TIC_IS_Candidates_Intensity.png")
message("  - TIC_mz122_Analysis.png")

message("\n============================================================")
message("KEY FINDINGS")
message("============================================================")
message(sprintf("Total ions at RDX RT: %d", nrow(ion_stats)))
message(sprintf("Potential IS candidates: %d", nrow(is_candidates)))
message(sprintf("RDX-dependent ions: %d", nrow(problem_ions)))

if (nrow(is_candidates) > 0) {
  message("\n>>> RECOMMENDATION: Examine m/z ", 
          paste(head(is_candidates$mz, 3), collapse = ", "),
          " as potential alternative IS ions")
} else {
  message("\n>>> RECOMMENDATION: No suitable IS candidates found.")
  message("    Consider mathematical correction for m/z 122 IS contamination")
}

message("\n=== Done ===")
