# ============================================================
# TIC_IonCorrelation_Diagnostic.R
# Extract candidate ions from TIC (full scan) data at RDX retention time
# across calibration series to identify ions independent of RDX concentration.
#
# Purpose: Find alternative ions for RDX analysis that are NOT correlated
#          with RDX signal (potential alternative internal standard candidates)
#
# Input: .ms1 files from TIC acquisition
# Output: Correlation table, diagnostic plots
# ============================================================

library(ggplot2)
library(dplyr)

message("=== TIC Ion Correlation Diagnostic for RDX ===")

# ============================================================
# Configuration
# ============================================================

ms1_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Method Development/20260522 Filter Test/GcDataConverterMs/"

calibrant_info <- data.frame(
  file = c("002", "003", "004", "005", "006", "007"),
  conc_ng = c(0.2, 2, 4, 6, 8, 10),
  stringsAsFactors = FALSE
)

rdx_rt_target <- 5.79
rdx_rt_window <- 0.15

candidate_ions <- c(46, 75, 120, 122, 128, 129, 76, 81, 47, 54, 150)

mz_tolerance <- 0.5

# ============================================================
# Functions
# ============================================================

parse_ms1_file <- function(filepath) {
  lines <- readLines(filepath)
  
  scans <- list()
  current_scan <- NULL
  current_rt <- NULL
  ions <- list()
  in_ions <- FALSE
  
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
      in_ions <- FALSE
      
    } else if (grepl("^I\\tRTime\\t", line)) {
      parts <- strsplit(line, "\t")[[1]]
      current_rt <- as.numeric(parts[3])
      
    } else if (grepl("^\\d", line) && !grepl("^I\\t", line)) {
      parts <- strsplit(line, " ")[[1]]
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

find_rdx_peak_scans <- function(scans, target_rt, window, mz_track = 120, mz_tol = 0.5) {
  rdx_intensities <- numeric(length(scans))
  scan_nums <- names(scans)
  
  for (i in seq_along(scans)) {
    scan <- scans[[i]]
    if (is.null(scan$rt) || is.null(scan$ions)) {
      rdx_intensities[i] <- NA
      next
    }
    
    if (abs(scan$rt - target_rt) > window) {
      rdx_intensities[i] <- NA
      next
    }
    
    mz_match <- scan$ions$mz >= (mz_track - mz_tol) & scan$ions$mz <= (mz_track + mz_tol)
    if (any(mz_match)) {
      rdx_intensities[i] <- max(scan$ions$intensity[mz_match])
    } else {
      rdx_intensities[i] <- 0
    }
  }
  
  valid_idx <- which(!is.na(rdx_intensities) & rdx_intensities > 0)
  if (length(valid_idx) == 0) {
    return(NULL)
  }
  
  peak_idx <- valid_idx[which.max(rdx_intensities[valid_idx])]
  peak_pos <- which(names(scans) == scan_nums[peak_idx])
  
  scan_range <- max(1, peak_pos - 2):min(length(scans), peak_pos + 2)
  
  all_ions <- do.call(rbind, lapply(scan_range, function(j) {
    if (is.null(scans[[j]]$ions)) return(NULL)
    scans[[j]]$ions
  }))
  
  if (is.null(all_ions) || nrow(all_ions) == 0) {
    return(NULL)
  }
  
  max_ions <- aggregate(intensity ~ mz, data = all_ions, FUN = max)
  
  return(list(
    scan_num = scan_nums[peak_idx],
    rt = scans[[peak_idx]]$rt,
    rdx_intensity = rdx_intensities[peak_idx],
    ions = max_ions
  ))
}

extract_ion_intensity <- function(ions_df, target_mz, tolerance = 0.5) {
  if (is.null(ions_df) || nrow(ions_df) == 0) return(NA)
  
  mz_match <- ions_df$mz >= (target_mz - tolerance) & ions_df$mz <= (target_mz + tolerance)
  if (any(mz_match, na.rm = TRUE)) {
    return(max(ions_df$intensity[mz_match], na.rm = TRUE))
  }
  return(NA)
}

# ============================================================
# Main Processing
# ============================================================

results <- data.frame()

for (i in 1:nrow(calibrant_info)) {
  file_base <- calibrant_info$file[i]
  conc <- calibrant_info$conc_ng[i]
  
  ms1_path <- file.path(ms1_dir, paste0(file_base, ".ms1"))
  
  if (!file.exists(ms1_path)) {
    message("File not found: ", ms1_path)
    next
  }
  
  message("Processing ", file_base, " (", conc, " ng)...")
  
  scans <- parse_ms1_file(ms1_path)
  
  peak_info <- find_rdx_peak_scans(scans, rdx_rt_target, rdx_rt_window)
  
  if (is.null(peak_info)) {
    message("  No RDX peak found in RT window")
    next
  }
  
  message("  RDX peak at RT ", round(peak_info$rt, 3), " min, m/z 120 = ", round(peak_info$rdx_intensity, 0))
  
  ion_intensities <- sapply(candidate_ions, function(mz) {
    extract_ion_intensity(peak_info$ions, mz, mz_tolerance)
  })
  names(ion_intensities) <- paste0("mz_", candidate_ions)
  
  row_data <- c(conc_ng = conc, rt = peak_info$rt, ion_intensities)
  results <- rbind(results, as.data.frame(t(row_data)))
}

if (nrow(results) == 0) {
  stop("No results extracted")
}

# ============================================================
# Calculate Correlations
# ============================================================

cor_table <- data.frame(
  ion = paste0("m/z ", candidate_ions),
  mz = candidate_ions,
  cor_vs_conc = NA_real_,
  cor_vs_rdx = NA_real_,
  cor_vs_is = NA_real_,
  stringsAsFactors = FALSE
)

for (i in 1:nrow(cor_table)) {
  mz <- cor_table$mz[i]
  intensity_col <- paste0("mz_", mz)
  
  if (!(intensity_col %in% names(results))) next
  
  vals <- results[[intensity_col]]
  conc_vals <- results$conc_ng
  
  valid <- !is.na(vals) & !is.na(conc_vals)
  if (sum(valid) >= 3) {
    cor_table$cor_vs_conc[i] <- cor(vals[valid], conc_vals[valid])
  }
  
  rdx_col <- "mz_120"
  if (rdx_col %in% names(results)) {
    rdx_vals <- results[[rdx_col]]
    valid_rdx <- !is.na(vals) & !is.na(rdx_vals)
    if (sum(valid_rdx) >= 3) {
      cor_table$cor_vs_rdx[i] <- cor(vals[valid_rdx], rdx_vals[valid_rdx])
    }
  }
  
  is_col <- "mz_122"
  if (is_col %in% names(results)) {
    is_vals <- results[[is_col]]
    valid_is <- !is.na(vals) & !is.na(is_vals)
    if (sum(valid_is) >= 3) {
      cor_table$cor_vs_is[i] <- cor(vals[valid_is], is_vals[valid_is])
    }
  }
}

cor_table$interpretation <- with(cor_table, {
  case_when(
    cor_vs_rdx > 0.95 & cor_vs_conc > 0.95 ~ "RDX fragment/correlated",
    cor_vs_is > 0.90 ~ "IS fragment/correlated",
    abs(cor_vs_rdx) < 0.5 & abs(cor_vs_conc) < 0.5 ~ "Independent candidate",
    abs(cor_vs_rdx) < 0.7 ~ "Weak correlation",
    TRUE ~ "Moderate correlation"
  )
})

message("\n=== Correlation Results ===")
print(cor_table[, c("ion", "cor_vs_conc", "cor_vs_rdx", "cor_vs_is", "interpretation")])

# ============================================================
# Plots
# ============================================================

results_long <- results %>%
  tidyr::pivot_longer(
    cols = starts_with("mz_"),
    names_to = "mz_label",
    values_to = "intensity"
  ) %>%
  mutate(mz = as.numeric(gsub("mz_", "", mz_label)))

results_long <- merge(results_long, cor_table[, c("mz", "interpretation")], by = "mz", all.x = TRUE)

p1 <- ggplot(results_long, aes(x = conc_ng, y = intensity, color = interpretation)) +
  geom_point(size = 2) +
  geom_line(aes(group = mz_label), alpha = 0.5) +
  facet_wrap(~ paste0("m/z ", mz), scales = "free_y") +
  scale_y_log10() +
  labs(
    title = "Ion Intensity vs. RDX Concentration",
    subtitle = "At RDX peak apex (~RT 5.79 min)",
    x = "RDX Concentration (ng)",
    y = "Intensity (log scale)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(cor_table, aes(x = reorder(ion, -cor_vs_rdx), y = cor_vs_rdx)) +
  geom_bar(stat = "identity", aes(fill = interpretation)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  labs(
    title = "Correlation of Each Ion with RDX (m/z 120)",
    x = "Ion (m/z)",
    y = "Correlation with m/z 120"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rdx_vals <- results$mz_120
scatter_data <- results_long %>%
  filter(mz %in% candidate_ions) %>%
  mutate(rdx_intensity = rdx_vals[match(conc_ng, results$conc_ng)])

p3 <- ggplot(scatter_data, aes(x = rdx_intensity, y = intensity)) +
  geom_point(aes(color = interpretation)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50") +
  facet_wrap(~ paste0("m/z ", mz), scales = "free") +
  labs(
    title = "Ion Intensity vs. RDX (m/z 120) Intensity",
    subtitle = "Independence check: flat = independent, diagonal = correlated",
    x = "RDX Intensity (m/z 120)",
    y = "Ion Intensity"
  ) +
  theme_bw()

# ============================================================
# Save Outputs
# ============================================================

output_dir <- file.path(dirname(ms1_dir), "TIC_Diagnostics")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  file.path(output_dir, "TIC_IonCorrelation_RDX.png"),
  p1, width = 12, height = 10, dpi = 150
)

ggsave(
  file.path(output_dir, "TIC_RDX_CorrelationBarplot.png"),
  p2, width = 10, height = 6, dpi = 150
)

ggsave(
  file.path(output_dir, "TIC_Ion_vs_RDX_Scatter.png"),
  p3, width = 12, height = 10, dpi = 150
)

write.csv(results, file.path(output_dir, "TIC_IonCorrelation_Results.csv"), row.names = FALSE)
write.csv(cor_table, file.path(output_dir, "TIC_IonCorrelation_Correlations.csv"), row.names = FALSE)

message("\nOutput saved to: ", output_dir)
message("  - TIC_IonCorrelation_Results.csv")
message("  - TIC_IonCorrelation_Correlations.csv")
message("  - TIC_IonCorrelation_RDX.png")
message("  - TIC_RDX_CorrelationBarplot.png")
message("  - TIC_Ion_vs_RDX_Scatter.png")

# ============================================================
# Summary
# ============================================================

independent_ions <- cor_table %>%
  filter(abs(cor_vs_rdx) < 0.5, abs(cor_vs_conc) < 0.5)

message("\n=== Summary ===")
message("Candidate ions examined: ", paste(candidate_ions, collapse = ", "))
message("\nIons correlated with RDX (r > 0.95):")
print(cor_table$ion[cor_table$cor_vs_rdx > 0.95])
message("\nIons correlated with IS (r > 0.90 to m/z 122):")
print(cor_table$ion[cor_table$cor_vs_is > 0.90])
message("\nIndependent candidates (|r| < 0.5):")
print(independent_ions$ion)

if (nrow(independent_ions) > 0) {
  message("\n>>> POTENTIAL ALTERNATIVE IONS: ", paste(independent_ions$ion, collapse = ", "))
} else {
  message("\n>>> No independent ions found. Consider mathematical IS correction.")
}
