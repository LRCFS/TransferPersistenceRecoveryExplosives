# ============================================
# Extraction Efficiency - 12-Repeat Single-Extraction Study
# ============================================
#
# Purpose: Determine the average PETN/RDX "extraction efficiency" (the
# fraction of a KNOWN spiked mass recovered from a swab into ethanol) from
# N single-extraction replicate swabs, as a direct estimate for
# GCMSQuantitation/GlobalCode.R's petn_extraction_efficiency /
# rdx_extraction_efficiency constants (currently both hardcoded to 1 --
# see that file's own "TODO: Determine experimentally" comment, and item #6
# of the red-team review, Documents/ClaudeSpace/Red Teaming/RedTeam_Findings.md).
#
# This is a DIFFERENT question from the other two scripts in this folder:
#   - InitialExtractionTest.R compares no-swab vs wet-swab vs dry-swab
#     recovery (does the swab itself retain material?).
#   - PlotExtractionRecovery.R measures how much comes out across 5
#     SEQUENTIAL extractions of the same swab (how many passes to exhaust it?).
#   - THIS script: N independent swabs, each spiked with a known mass and
#     extracted ONCE, to get a single mean fraction-recovered statistic.
#
# Dataset: Extraction/20260903ExtractionTest, samples "Extracted Swab 1"
# through "Extracted Swab 12" (Type == "Sample"). Processed through the main
# GCMSQuantitation pipeline (Code/02_PeakDetection.R / 03_Quantification.R),
# so concentrations are already quantified (ng/uL); SampleVol = 1000 uL per
# that run's own run_metadata.yaml (confirmed, matches SAMPLE_VOLUME below).
#
# Spike amount: confirmed with the user (04/09/2026) as 40 uL x 50 ng/uL =
# 2000 ng per swab, for BOTH PETN and RDX (assumes a combined spiking
# standard at 50 ng/uL of each analyte -- adjust SPIKE_AMOUNT_PETN/RDX below
# separately if that assumption is wrong for this experiment).
#
# Author: A Bruce
# Date: September 4, 2026
# ============================================

# --- SECTION 1: SETUP ---
cat("\n========================================\n")
cat("EXTRACTION EFFICIENCY ANALYSIS\n")
cat("========================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(openxlsx)
})

# Shared thesis-wide colour palette (Okabe-Ito, colourblind-safe) -- single
# source of truth for every plot across the whole thesis repo.
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/thesis_palette.R")

# Define constants
SPIKE_AMOUNT_PETN <- 2000  # ng per swab (40 uL x 50 ng/uL)
SPIKE_AMOUNT_RDX  <- 2000  # ng per swab (40 uL x 50 ng/uL)
SAMPLE_VOLUME     <- 1000  # uL (extraction solvent volume; matches run_metadata.yaml)

# Set file paths
data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Extraction/20260904ExtractionTest/Results/20260904ExtractionTest_GCMSResults.csv"

output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Extraction/20260904ExtractionTest/Results/"

# Verify files/directories exist
if (!file.exists(data_file)) {
  stop("ERROR: Data file not found at: ", data_file)
}
if (!dir.exists(output_dir)) {
  stop("ERROR: Output directory not found at: ", output_dir)
}

cat("Data file:", basename(data_file), "\n")
cat("Spike amount: PETN", SPIKE_AMOUNT_PETN, "ng, RDX", SPIKE_AMOUNT_RDX, "ng per swab\n")
cat("Sample volume:", SAMPLE_VOLUME, "uL\n")
cat("Note: Concentrations in CSV are ng/uL\n\n")

# --- SECTION 2: DATA IMPORT ---
cat("--- PROCESSING ---\n")

# Read CSV file
raw_data <- read.csv(data_file, stringsAsFactors = FALSE)

# Filter to the 12 single-extraction replicate swabs (Type == "Sample" only
# -- excludes the Cal/QC/Test-Std rows also processed in this same sequence,
# see the Sequence Log TSV for this run).
swab_data <- raw_data %>%
  filter(Type == "Sample") %>%
  filter(str_detect(str_trim(SampleName), "^Extracted Swab \\d+$"))

cat("Found", nrow(swab_data), "extraction-efficiency swab samples\n")

# --- SECTION 3: DATA PROCESSING ---

# Parse swab replicate number from sample names
swab_data <- swab_data %>%
  mutate(
    SwabNum = as.integer(str_extract(str_trim(SampleName), "\\d+$"))
  ) %>%
  arrange(SwabNum)

n_failed <- sum(is.na(swab_data$petn_concentration_dc) & is.na(swab_data$rdx_concentration))
if (n_failed > 0) {
  cat("NOTE:", n_failed, "swab(s) have no quantifiable result (excluded from the mean, not from n):\n")
  cat("  ", paste(swab_data$SampleName[
    is.na(swab_data$petn_concentration_dc) & is.na(swab_data$rdx_concentration)
  ], collapse = ", "), "\n")
}

# Calculate extraction efficiency as a FRACTION (0-1) of the known spiked
# mass -- this is the exact quantity GlobalCode.R's petn_extraction_efficiency/
# rdx_extraction_efficiency expect (see that file's own formula comment:
# "Extraction efficiency: fraction recovered from swab into ethanol").
# PETN uses the drift-corrected concentration, RDX uses the raw (IS-ratio)
# concentration -- same convention as InitialExtractionTest.R and the main
# pipeline itself (PETN has no internal standard and relies on drift
# correction instead; RDX's 15N-RDX IS ratio needs no drift correction).
swab_data <- swab_data %>%
  mutate(
    petn_efficiency = (petn_concentration_dc * SAMPLE_VOLUME) / SPIKE_AMOUNT_PETN,
    rdx_efficiency  = (rdx_concentration      * SAMPLE_VOLUME) / SPIKE_AMOUNT_RDX,
    # Efficiency cannot be physically negative -- floor at 0 (same convention
    # as the recovery% calculations elsewhere in this folder).
    petn_efficiency = pmax(0, petn_efficiency),
    rdx_efficiency  = pmax(0, rdx_efficiency)
  )

# Reshape to long format for plotting/summarising
data_long <- swab_data %>%
  select(SwabNum, petn_efficiency, rdx_efficiency) %>%
  pivot_longer(
    cols = c(petn_efficiency, rdx_efficiency),
    names_to = "Analyte",
    values_to = "Efficiency"
  ) %>%
  mutate(
    Analyte = factor(Analyte,
                      levels = c("petn_efficiency", "rdx_efficiency"),
                      labels = c("PETN", "RDX"))
  )

# --- SECTION 4: SUMMARY STATISTICS ---

summary_stats <- data_long %>%
  group_by(Analyte) %>%
  summarise(
    N_Total     = n(),
    N_Usable    = sum(!is.na(Efficiency)),
    N_Excluded  = sum(is.na(Efficiency)),
    Mean        = mean(Efficiency, na.rm = TRUE),
    SD          = sd(Efficiency, na.rm = TRUE),
    SE          = SD / sqrt(N_Usable),
    Min         = min(Efficiency, na.rm = TRUE),
    Max         = max(Efficiency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_Percent = round(Mean * 100, 1),
    SD_Percent   = round(SD * 100, 1),
    SE_Percent   = round(SE * 100, 1)
  )

# Print summary to console
cat("\n--- EXTRACTION EFFICIENCY SUMMARY ---\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf(
    "  %s: mean = %.1f%% (SD %.1f%%, SE %.1f%%), n = %d/%d usable, range %.1f%%-%.1f%%\n",
    summary_stats$Analyte[i],
    summary_stats$Mean_Percent[i], summary_stats$SD_Percent[i], summary_stats$SE_Percent[i],
    summary_stats$N_Usable[i], summary_stats$N_Total[i],
    summary_stats$Min[i] * 100, summary_stats$Max[i] * 100
  ))
}

# Ready-to-paste values for GCMSQuantitation/GlobalCode.R -- printed as a
# fraction (0-1), matching the existing petn_extraction_efficiency <- 1 /
# rdx_extraction_efficiency <- 1 placeholder syntax exactly.
petn_row <- summary_stats %>% filter(Analyte == "PETN")
rdx_row  <- summary_stats %>% filter(Analyte == "RDX")
cat("\n--- PASTE INTO GlobalCode.R (replace the current placeholder values of 1) ---\n")
cat(sprintf("petn_extraction_efficiency <- %.4f  # %.1f%% (n=%d, SD %.1f%%), measured %s\n",
            petn_row$Mean, petn_row$Mean_Percent, petn_row$N_Usable, petn_row$SD_Percent,
            format(Sys.Date(), "%Y-%m-%d")))
cat(sprintf("rdx_extraction_efficiency  <- %.4f  # %.1f%% (n=%d, SD %.1f%%), measured %s\n",
            rdx_row$Mean, rdx_row$Mean_Percent, rdx_row$N_Usable, rdx_row$SD_Percent,
            format(Sys.Date(), "%Y-%m-%d")))
cat("(filtration_efficiency is a separate, still-undetermined step -- not measured by this script)\n")

# --- SECTION 5: VISUALIZATION ---

cat("\n--- GENERATING PLOT ---\n")

mean_lines <- summary_stats %>% select(Analyte, Mean)

plot <- ggplot(data_long, aes(x = factor(SwabNum), y = Efficiency * 100, fill = Analyte)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(Efficiency), "", sprintf("%.0f%%", Efficiency * 100))),
            vjust = -0.5, size = 3, na.rm = TRUE) +
  geom_hline(data = mean_lines, aes(yintercept = Mean * 100),
             linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = pal_analyte) +
  facet_wrap(~ Analyte, ncol = 1) +
  scale_y_continuous(
    limits = c(0, max(data_long$Efficiency * 100, na.rm = TRUE) * 1.15),
    expand = c(0, 0)
  ) +
  labs(
    title = "Extraction Efficiency - 12 Single-Extraction Replicate Swabs",
    subtitle = sprintf(
      "Spiked with %d ng PETN / %d ng RDX per swab; dashed line = mean. Missing bars = no quantifiable result.",
      SPIKE_AMOUNT_PETN, SPIKE_AMOUNT_RDX
    ),
    x = "Swab replicate",
    y = "Extraction efficiency (%)",
    fill = "Analyte"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(color = "grey90"),
    axis.text = element_text(size = 10)
  )

# Save plot
plot_file <- file.path(output_dir, "ExtractionEfficiency_BarChart.png")
ggsave(
  filename = plot_file,
  plot = plot,
  width = 9,
  height = 8,
  dpi = 300,
  units = "in"
)

cat("Bar chart saved:", basename(plot_file), "\n")

# --- SECTION 6: EXPORT DATA ---

# Per-swab detail
detail_out <- swab_data %>%
  select(SwabNum, SampleName, petn_concentration_dc, petn_efficiency,
         rdx_concentration, rdx_efficiency) %>%
  mutate(
    petn_efficiency_pct = round(petn_efficiency * 100, 1),
    rdx_efficiency_pct  = round(rdx_efficiency * 100, 1)
  )

excel_file <- file.path(output_dir, "ExtractionEfficiency_Summary.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)
addWorksheet(wb, "PerSwabDetail")
writeData(wb, "PerSwabDetail", detail_out)
saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("Summary saved:", basename(excel_file), "\n")

csv_file <- file.path(output_dir, "ExtractionEfficiency_Summary.csv")
write.csv(summary_stats, csv_file, row.names = FALSE)
cat("Summary saved:", basename(csv_file), "\n")

detail_csv_file <- file.path(output_dir, "ExtractionEfficiency_PerSwabDetail.csv")
write.csv(detail_out, detail_csv_file, row.names = FALSE)
cat("Per-swab detail saved:", basename(detail_csv_file), "\n")

# --- SECTION 7: COMPLETION ---
cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("Output files saved to:\n")
cat("  ", output_dir, "\n\n")

cat("Generated files:\n")
cat("  1. ExtractionEfficiency_BarChart.png - Per-swab bar chart with mean line\n")
cat("  2. ExtractionEfficiency_Summary.xlsx - Summary stats + per-swab detail\n")
cat("  3. ExtractionEfficiency_Summary.csv - Summary statistics (CSV)\n")
cat("  4. ExtractionEfficiency_PerSwabDetail.csv - Per-swab detail (CSV)\n\n")
