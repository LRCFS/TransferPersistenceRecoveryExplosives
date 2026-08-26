# ============================================
# Initial Extraction Test - Test Standards 1, 2, 3
# ============================================
# 
# Purpose: Analyze single extraction recovery from test standards
# Test Standards spiked with: 10,000 ng (same as swabs)
# Analytes: PETN (drift-corrected), RDX (uncorrected)
# 
# Author: A Bruce
# Date: July 22, 2026
# ============================================

# --- SECTION 1: SETUP ---
cat("\n========================================\n")
cat("TEST STANDARD RECOVERY ANALYSIS\n")
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
SPIKE_AMOUNT <- 10000  # ng (same as swabs)
SAMPLE_VOLUME <- 1000  # µL

# Set file paths
data_file <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data/Analysis1/Results/_GCMSResults.csv"

output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data/Analysis1/Results/"

# Verify files/directories exist
if (!file.exists(data_file)) {
  stop("ERROR: Data file not found at: ", data_file)
}
if (!dir.exists(output_dir)) {
  stop("ERROR: Output directory not found at: ", output_dir)
}

cat("Data file:", basename(data_file), "\n")
cat("Spike amount:", SPIKE_AMOUNT, "ng per test standard\n")
cat("Sample volume:", SAMPLE_VOLUME, "µL\n")
cat("Note: Concentrations in CSV are ng/µL\n\n")

# --- SECTION 2: DATA IMPORT ---
cat("--- PROCESSING ---\n")

# Read CSV file
raw_data <- read.csv(data_file, stringsAsFactors = FALSE)

# Filter to test standard samples (Test Std 1, 2, 3 only)
test_std_data <- raw_data %>%
  filter(Type == "Sample") %>%
  filter(str_detect(str_trim(SampleName), "^Test Std [1-3] - 10ng"))

cat("✓ Found", nrow(test_std_data), "test standard samples\n")

# --- SECTION 3: DATA PROCESSING ---

# Parse test standard number from sample names
test_std_data <- test_std_data %>%
  mutate(
    TestStd = as.numeric(str_extract(str_trim(SampleName), "(?<=Test Std )\\d+"))
  )

# Check for expected test standards
unique_stds <- sort(unique(test_std_data$TestStd))
cat("✓ Found test standards:", paste(unique_stds, collapse = ", "), "\n")

# Calculate recovery percentages
# Concentrations are in ng/µL, need to multiply by sample volume (1000 µL)
# Recovery (%) = (concentration_ng_per_uL × 1000 µL / 10000 ng) × 100
test_std_data <- test_std_data %>%
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

# Reshape to long format for plotting
data_long <- test_std_data %>%
  select(TestStd, petn_recovery, rdx_recovery) %>%
  pivot_longer(
    cols = c(petn_recovery, rdx_recovery),
    names_to = "Analyte",
    values_to = "Recovery"
  ) %>%
  mutate(
    Analyte = factor(Analyte, 
                     levels = c("petn_recovery", "rdx_recovery"),
                     labels = c("PETN", "RDX")),
    TestStd = factor(TestStd, 
                     levels = 1:3, 
                     labels = c("100% recovery\n(no swab)", 
                                "Spiked swab\n(wet)", 
                                "Spiked swab\n(dry)"))
  )

# --- SECTION 4: SUMMARY STATISTICS ---

# Calculate summary statistics
summary_stats <- data_long %>%
  group_by(TestStd, Analyte) %>%
  summarise(
    Recovery_Percent = round(Recovery, 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Analyte,
    values_from = Recovery_Percent,
    names_prefix = "Recovery_"
  ) %>%
  # Add descriptive column
  mutate(
    Sample_Type = case_when(
      TestStd == "100% recovery\n(no swab)" ~ "No swab (100% control)",
      TestStd == "Spiked swab\n(wet)" ~ "Spiked swab (wet)",
      TestStd == "Spiked swab\n(dry)" ~ "Spiked swab (dry)"
    )
  ) %>%
  select(Sample_Type, Recovery_PETN, Recovery_RDX)

# Print summary to console
cat("\n--- RECOVERY SUMMARY ---\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("  %s - PETN: %.1f%%, RDX: %.1f%%\n", 
              summary_stats$Sample_Type[i],
              summary_stats$Recovery_PETN[i],
              summary_stats$Recovery_RDX[i]))
}

# --- SECTION 5: VISUALIZATION ---

cat("\n--- GENERATING PLOT ---\n")

# Create bar chart
plot <- ggplot(data_long, aes(x = TestStd, y = Recovery, fill = TestStd)) +
  # Bars
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  
  # Recovery percentage labels on top of bars
  geom_text(aes(label = sprintf("%.1f%%", Recovery)),
            vjust = -0.5, size = 4, fontface = "bold") +
  
  # Color scheme
  scale_fill_manual(
    values = c(
      "100% recovery\n(no swab)" = "#E41A1C",  # Red
      "Spiked swab\n(wet)" = "#377EB8",  # Blue
      "Spiked swab\n(dry)" = "#4DAF4A"   # Green
    )
  ) +
  
  # Facet by analyte
  facet_wrap(~ Analyte, ncol = 2) +
  
  # Y-axis limits
  scale_y_continuous(
    limits = c(0, max(data_long$Recovery) * 1.15),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  
  # Labels
  labs(
    title = "Initial Extraction Test - PETN and RDX Recovery",
    subtitle = "Comparison of recovery from no swab (100% control), wet swab, and dry swab. Spiked with 10,000 ng.",
    x = "",
    y = "Recovery (%)",
    fill = "Sample Type"
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
plot_file <- file.path(output_dir, "TestStandard_Recovery_BarChart.png")
ggsave(
  filename = plot_file,
  plot = plot,
  width = 10,
  height = 7,
  dpi = 300,
  units = "in"
)

cat("✓ Bar chart saved:", basename(plot_file), "\n")

# --- SECTION 6: EXPORT DATA ---

# Export summary statistics to Excel
excel_file <- file.path(output_dir, "TestStandard_Recovery_Summary.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)
saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("✓ Summary saved:", basename(excel_file), "\n")

# Export summary statistics to CSV
csv_file <- file.path(output_dir, "TestStandard_Recovery_Summary.csv")
write.csv(summary_stats, csv_file, row.names = FALSE)
cat("✓ Summary saved:", basename(csv_file), "\n")

# --- SECTION 7: COMPLETION ---
cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("Output files saved to:\n")
cat("  ", output_dir, "\n\n")

cat("Generated files:\n")
cat("  1. TestStandard_Recovery_BarChart.png - Bar chart visualization\n")
cat("  2. TestStandard_Recovery_Summary.xlsx - Summary statistics\n")
cat("  3. TestStandard_Recovery_Summary.csv - Summary statistics (CSV)\n\n")
