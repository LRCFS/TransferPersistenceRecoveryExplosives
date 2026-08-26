# ============================================
# Sequential Extraction Recovery Analysis
# ============================================
# 
# Purpose: Analyze extraction efficiency from swabbing study
# Swabs spiked with: 5 × 10 µL × 200 ng/µL = 10,000 ng
# Analytes: PETN (drift-corrected), RDX (uncorrected)
# 
# Author: A Bruce
# Date: July 22, 2026
# ============================================

# --- SECTION 1: SETUP ---
cat("\n========================================\n")
cat("EXTRACTION RECOVERY ANALYSIS\n")
cat("========================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(openxlsx)
})

# Define constants
SPIKE_AMOUNT <- 10000  # ng (5 × 10 µL × 200 ng/µL)
SAMPLE_VOLUME <- 1000  # µL

# Set file paths
data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Extraction/20260721ExtractionTest/Results/20260721ExtractionTest_GCMSResults.csv"

output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Extraction/20260721ExtractionTest/Results/"

# Verify files/directories exist
if (!file.exists(data_file)) {
  stop("ERROR: Data file not found at: ", data_file)
}
if (!dir.exists(output_dir)) {
  stop("ERROR: Output directory not found at: ", output_dir)
}

cat("Data file:", basename(data_file), "\n")
cat("Spike amount:", SPIKE_AMOUNT, "ng per swab\n")
cat("Sample volume:", SAMPLE_VOLUME, "µL\n")
cat("Note: Concentrations in CSV are ng/µL\n\n")

# --- SECTION 2: DATA IMPORT ---
cat("--- PROCESSING ---\n")

# Read CSV file
raw_data <- read.csv(data_file, stringsAsFactors = FALSE)

# Filter to extraction samples only
extraction_data <- raw_data %>%
  filter(Type == "Sample") %>%
  filter(str_detect(str_trim(SampleName), "^Swab \\d+ Ext \\d+"))

cat("✓ Found", nrow(extraction_data), "extraction samples\n")

# --- SECTION 3: DATA PROCESSING ---

# Parse swab and extraction numbers from sample names
extraction_data <- extraction_data %>%
  mutate(
    Swab = as.numeric(str_extract(str_trim(SampleName), "(?<=Swab )\\d+")),
    Extraction = as.numeric(str_extract(str_trim(SampleName), "(?<=Ext )\\d+"))
  )

# Check for expected swabs
unique_swabs <- sort(unique(extraction_data$Swab))
cat("✓ Found", length(unique_swabs), "swabs (Swabs", paste(unique_swabs, collapse = ", "), ")\n")

# Calculate recovery percentages
# Concentrations are in ng/µL, need to multiply by sample volume (1000 µL)
# Recovery (%) = (concentration_ng_per_uL × 1000 µL / 10000 ng) × 100
extraction_data <- extraction_data %>%
  mutate(
    petn_recovery = (petn_concentration_dc * SAMPLE_VOLUME / SPIKE_AMOUNT) * 100,
    rdx_recovery = (rdx_concentration * SAMPLE_VOLUME / SPIKE_AMOUNT) * 100
  ) %>%
  # Handle missing/NA values - replace with 0
  replace_na(list(petn_recovery = 0, rdx_recovery = 0)) %>%
  # Floor negative values at 0
  mutate(
    petn_recovery = pmax(0, petn_recovery),
    rdx_recovery = pmax(0, rdx_recovery)
  )

# Check for missing extractions
expected_combinations <- expand.grid(Swab = 1:3, Extraction = 1:5)
actual_combinations <- extraction_data %>%
  select(Swab, Extraction) %>%
  distinct()

missing_combinations <- anti_join(expected_combinations, actual_combinations, 
                                   by = c("Swab", "Extraction"))

if (nrow(missing_combinations) > 0) {
  cat("⚠ Missing data:\n")
  for (i in 1:nrow(missing_combinations)) {
    cat("  Swab", missing_combinations$Swab[i], "Ext", 
        missing_combinations$Extraction[i], "(plotted as 0% recovery)\n")
  }
} else {
  cat("✓ All swab-extraction combinations present\n")
}

# Add missing combinations with 0 recovery
if (nrow(missing_combinations) > 0) {
  missing_data <- missing_combinations %>%
    mutate(
      petn_recovery = 0,
      rdx_recovery = 0
    )
  
  extraction_data <- extraction_data %>%
    select(Swab, Extraction, petn_recovery, rdx_recovery) %>%
    bind_rows(missing_data)
}

# Reshape to long format for plotting
data_long <- extraction_data %>%
  select(Swab, Extraction, petn_recovery, rdx_recovery) %>%
  pivot_longer(
    cols = c(petn_recovery, rdx_recovery),
    names_to = "Analyte",
    values_to = "Recovery"
  ) %>%
  mutate(
    Analyte = factor(Analyte, 
                     levels = c("petn_recovery", "rdx_recovery"),
                     labels = c("PETN", "RDX")),
    Swab = factor(Swab, levels = 1:3, labels = paste("Swab", 1:3)),
    # Reverse factor levels so Ext 1 appears at bottom of stacked bar
    Extraction = factor(Extraction, levels = 5:1, labels = paste("Ext", 5:1))
  )

# --- SECTION 4: SUMMARY STATISTICS ---

# Calculate summary statistics per swab
summary_stats <- data_long %>%
  group_by(Swab, Analyte) %>%
  summarise(
    Ext1 = sum(Recovery[Extraction == "Ext 1"], na.rm = TRUE),
    Ext2 = sum(Recovery[Extraction == "Ext 2"], na.rm = TRUE),
    Ext3 = sum(Recovery[Extraction == "Ext 3"], na.rm = TRUE),
    Ext4 = sum(Recovery[Extraction == "Ext 4"], na.rm = TRUE),
    Ext5 = sum(Recovery[Extraction == "Ext 5"], na.rm = TRUE),
    Total_Recovery = sum(Recovery, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Pct_First_Extraction = round((Ext1 / Total_Recovery) * 100, 1),
    Remaining_After_First = round(100 - Pct_First_Extraction, 1)
  )

# Round recovery values for cleaner output
summary_stats <- summary_stats %>%
  mutate(across(c(Ext1, Ext2, Ext3, Ext4, Ext5, Total_Recovery), ~round(.x, 1)))

# Calculate cumulative totals for plot labels
cumulative_totals <- data_long %>%
  group_by(Swab, Analyte) %>%
  summarise(Total = sum(Recovery, na.rm = TRUE), .groups = "drop")

# Print summary to console
cat("\n--- TOTAL RECOVERY ---\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("  %s %s: %.1f%%\n", 
              summary_stats$Swab[i], 
              summary_stats$Analyte[i], 
              summary_stats$Total_Recovery[i]))
}

# --- SECTION 5: VISUALIZATION ---

cat("\n--- GENERATING PLOT ---\n")

# Create stacked bar chart
plot <- ggplot(data_long, aes(x = Swab, y = Recovery, fill = Extraction)) +
  # Stacked bars with white borders
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  
  # Total recovery labels on top
  geom_text(
    data = cumulative_totals,
    aes(x = Swab, y = Total, label = sprintf("%.1f%%", Total)),
    vjust = -0.5, 
    inherit.aes = FALSE, 
    size = 3.5, 
    fontface = "bold"
  ) +
  
  # Sequential blues color palette (Ext 1 = darkest, Ext 5 = lightest)
  scale_fill_manual(
    values = c(
      "Ext 5" = "#EFF3FF",  # Lightest (top of stack)
      "Ext 4" = "#BDD7E7",
      "Ext 3" = "#6BAED6",
      "Ext 2" = "#3182BD",
      "Ext 1" = "#08519C"   # Darkest (bottom of stack)
    ),
    # Reverse legend order so Ext 1 appears first
    guide = guide_legend(reverse = TRUE)
  ) +
  
  # Side-by-side panels: PETN | RDX
  facet_wrap(~ Analyte, ncol = 2) +
  
  # Labels
  labs(
    title = "Sequential Extraction Recovery - PETN and RDX",
    subtitle = "Swabs spiked with 10,000 ng (5 × 10 µL × 200 ng/µL). Sample volume: 1000 µL. PETN: drift-corrected, RDX: uncorrected.",
    x = "Swab ID",
    y = "Recovery (%)",
    fill = "Extraction"
  ) +
  
  # Y-axis: add 15% headroom for labels
  scale_y_continuous(
    limits = c(0, max(cumulative_totals$Total) * 1.15),
    expand = c(0, 0),
    breaks = seq(0, 100, 20)
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(color = "grey90"),
    axis.text = element_text(size = 10)
  )

# Save plot
plot_file <- file.path(output_dir, "ExtractionRecovery_Cumulative_Stacked.png")
ggsave(
  filename = plot_file,
  plot = plot,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

cat("✓ Stacked bar plot saved:", basename(plot_file), "\n")

# --- SECTION 5B: CUMULATIVE SCATTER PLOT ---

cat("✓ Generating cumulative scatter plot...\n")

# Calculate cumulative recovery for each swab
# Need to sort by extraction number and calculate running sum
cumulative_data <- data_long %>%
  # Convert Extraction back to numeric for proper ordering
  mutate(Extraction_num = as.numeric(str_extract(as.character(Extraction), "\\d+"))) %>%
  arrange(Swab, Analyte, Extraction_num) %>%
  group_by(Swab, Analyte) %>%
  mutate(Cumulative_Recovery = cumsum(Recovery)) %>%
  ungroup()

# Create scatter plot with lines
scatter_plot <- ggplot(cumulative_data, aes(x = Extraction_num, y = Cumulative_Recovery, color = Swab, group = Swab)) +
  # Lines connecting points
  geom_line(linewidth = 0.8, alpha = 0.7) +
  
  # Points
  geom_point(size = 3, alpha = 0.9) +
  
  # Color scheme for swabs
  scale_color_manual(
    values = c(
      "Swab 1" = "#E41A1C",  # Red
      "Swab 2" = "#377EB8",  # Blue
      "Swab 3" = "#4DAF4A"   # Green
    )
  ) +
  
  # Facet by analyte
  facet_wrap(~ Analyte, ncol = 2) +
  
  # X-axis: extraction number (1-5)
  scale_x_continuous(
    breaks = 1:5,
    labels = paste("Ext", 1:5)
  ) +
  
  # Y-axis: recovery percentage
  scale_y_continuous(
    limits = c(0, max(cumulative_data$Cumulative_Recovery) * 1.05),
    expand = c(0, 0),
    breaks = seq(0, 100, 10)
  ) +
  
  # Labels
  labs(
    title = "Cumulative Extraction Recovery - PETN and RDX",
    subtitle = "Progressive recovery across sequential extractions. Swabs spiked with 10,000 ng.",
    x = "Extraction Number",
    y = "Cumulative Recovery (%)",
    color = "Swab ID"
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text = element_text(size = 10)
  )

# Save scatter plot
scatter_file <- file.path(output_dir, "ExtractionRecovery_Cumulative_Scatter.png")
ggsave(
  filename = scatter_file,
  plot = scatter_plot,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

cat("✓ Cumulative scatter plot saved:", basename(scatter_file), "\n")

# --- SECTION 6: EXPORT DATA ---

# Export summary statistics to Excel
excel_file <- file.path(output_dir, "ExtractionRecovery_Summary.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)
saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("✓ Summary saved:", basename(excel_file), "\n")

# Export summary statistics to CSV
csv_file <- file.path(output_dir, "ExtractionRecovery_Summary.csv")
write.csv(summary_stats, csv_file, row.names = FALSE)
cat("✓ Summary saved:", basename(csv_file), "\n")

# --- SECTION 7: COMPLETION ---
cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("Output files saved to:\n")
cat("  ", output_dir, "\n\n")

cat("Generated plots:\n")
cat("  1. ExtractionRecovery_Cumulative_Stacked.png - Stacked bar chart\n")
cat("  2. ExtractionRecovery_Cumulative_Scatter.png - Cumulative scatter plot\n\n")
