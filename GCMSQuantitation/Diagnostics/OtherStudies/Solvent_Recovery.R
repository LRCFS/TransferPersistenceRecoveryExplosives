# ===================================================================
# Recovery Calculation Script for RDX and PETN
# ===================================================================
# Purpose: Calculate recovery percentages from 50,000 ng surface spike
#          and create visualization plots for different solvent conditions
# 
# Author: OpenCode
# Date: 2026-07-16
# ===================================================================

# === LOAD LIBRARIES ===
library(tidyverse)

# === PARAMETERS ===
# File path to GC-MS results
file_path <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Solvents/RDX_ACN_Test/Results/RDX_ACN_Test_GCMSResults.csv"

# Spike amount in nanograms
spike_amount <- 50000

# Sample volume (µL) - concentration is per µL, extract is 1000 µL
sample_volume_uL <- 1000

# Output directory for plots (same folder as data)
output_dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Solvents/RDX_ACN_Test/Results"

# === LOAD AND FILTER DATA ===
cat("Loading data from:", file_path, "\n")
data <- read_csv(file_path, show_col_types = FALSE)

# Filter for Sample type only and exclude SW Neg Ctrl
samples <- data %>%
  filter(Type == "Sample") %>%
  filter(!str_detect(SampleName, "SW Neg Ctrl"))

cat("Filtered", nrow(samples), "samples\n\n")

# === MAP SAMPLE NAMES TO CONDITIONS ===
# Create mapping for display names
samples <- samples %>%
  mutate(Condition = case_when(
    str_detect(SampleName, "Sample 1-Stock Std in ACN") ~ "1000ng/uL in ACN swabbed",
    str_detect(SampleName, "Sample 2-Stock Std.*SW") ~ "1000ng/uL in ACN solvent washed",
    str_detect(SampleName, "Sample 3-Std in EtOH") ~ "200ng/uL in EtOH",
    TRUE ~ NA_character_
  )) %>%
  # Remove samples that weren't mapped (including Sample 4-Std in ACN)
  filter(!is.na(Condition))

# Set factor levels for proper ordering in plots
condition_order <- c(
  "1000ng/uL in ACN swabbed",
  "1000ng/uL in ACN solvent washed",
  "200ng/uL in EtOH"
)

samples <- samples %>%
  mutate(Condition = factor(Condition, levels = condition_order))

# === CALCULATE RECOVERY PERCENTAGES ===
# Concentration is in ng/µL, so multiply by sample volume to get total ng recovered
# Then divide by spike amount and multiply by 100 for percentage
samples <- samples %>%
  mutate(
    RDX_Recovery_pct = (rdx_concentration * sample_volume_uL / spike_amount) * 100,
    PETN_Recovery_pct = (petn_concentration * sample_volume_uL / spike_amount) * 100
  )

# === CREATE SUMMARY TABLE ===
cat("=== Recovery Summary ===\n")
summary_table <- samples %>%
  select(Condition, RDX_Recovery_pct, PETN_Recovery_pct) %>%
  arrange(Condition)

print(summary_table, n = Inf)
cat("\n")

# === PLOT RDX RECOVERY ===
cat("Creating RDX recovery plot...\n")

rdx_plot <- ggplot(samples, aes(x = Condition, y = RDX_Recovery_pct)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(size = 4, color = "#2166ac") +
  labs(
    title = "RDX Recovery from 50,000 ng Spike",
    x = "Condition",
    y = "Recovery (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))

# Save RDX plot
rdx_output <- file.path(output_dir, "RDX_Recovery.png")
ggsave(rdx_output, rdx_plot, width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved RDX plot to:", rdx_output, "\n")

# === PLOT PETN RECOVERY ===
cat("Creating PETN recovery plot...\n")

petn_plot <- ggplot(samples, aes(x = Condition, y = PETN_Recovery_pct)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(size = 4, color = "#b2182b") +
  labs(
    title = "PETN Recovery from 50,000 ng Spike",
    x = "Condition",
    y = "Recovery (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))

# Save PETN plot
petn_output <- file.path(output_dir, "PETN_Recovery.png")
ggsave(petn_output, petn_plot, width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved PETN plot to:", petn_output, "\n")

# === COMPLETION MESSAGE ===
cat("\n=== Script completed successfully! ===\n")
cat("Output files created:\n")
cat("  -", rdx_output, "\n")
cat("  -", petn_output, "\n")
