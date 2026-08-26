# ==============================================================================
# ASTRA Swabbing Pilot Study - Pressure Trace Analysis
# ==============================================================================
# 
# This script analyzes pressure trace data from the ASTRA swabbing pilot study.
# It creates individual plots for each sample, grouped plots by pressure level 
# and surface type, and performs statistical analysis.
#
# Author: Generated for ASTRA Swabbing Pilot Study
# Date: 2026-07-16
# ==============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(scales)
  library(gridExtra)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Define paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
METADATA_FILE <- file.path(BASE_PATH, "pilot_data_nested.csv")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs"

# Create output directories
dir.create(file.path(OUTPUT_PATH, "individual_plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "grouped_plots"), recursive = TRUE, showWarnings = FALSE)

# Define samples with pressure traces
SAMPLES_WITH_TRACES <- c("PILOT_001", "PILOT_005", "PILOT_009", "PILOT_013", 
                         "PILOT_017", "PILOT_021", "PILOT_025", "PILOT_029")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Read a single pressure trace CSV file
#' @param run_id Sample identifier (e.g., "PILOT_001")
#' @return Data frame with pressure trace data
read_pressure_trace <- function(run_id) {
  file_path <- file.path(TRACES_PATH, paste0(run_id, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read CSV as character first to handle mixed content
  # Use col_names = FALSE to avoid header issues
  df <- read_csv(file_path, 
                 col_names = FALSE,
                 col_types = cols(.default = "c"),
                 show_col_types = FALSE)
  
  # Determine number of columns
  n_cols <- ncol(df)
  
  # Set proper column names based on number of columns
  if (n_cols == 5) {
    colnames(df) <- c("timestamp", "elapsed_sec", "analog_value", "load_g", "distance_mm")
  } else if (n_cols == 3) {
    # Some files might have format: timestamp, elapsed_sec, combined data
    colnames(df) <- c("timestamp", "elapsed_sec", "extra")
  } else {
    warning(paste("Unexpected column count:", n_cols, "in", run_id))
    return(NULL)
  }
  
  # Remove rows where ANY column contains text like "analogValue", "load", "distance", "timestamp"
  if ("analog_value" %in% colnames(df)) {
    df <- df %>%
      filter(!grepl("analog|load|distance|timestamp", timestamp, ignore.case = TRUE),
             !grepl("analog|load|distance", analog_value, ignore.case = TRUE))
  }
  
  # Convert to proper types
  df <- df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
      elapsed_sec = as.numeric(elapsed_sec),
      analog_value = as.numeric(analog_value),
      load_g = as.numeric(load_g),
      distance_mm = as.numeric(distance_mm),
      run_id = run_id
    ) %>%
    filter(!is.na(elapsed_sec), !is.na(load_g), load_g >= 0)  # Keep zero values, remove only NA
  
  return(df)
}

#' Read metadata JSON file for a sample
#' @param run_id Sample identifier (e.g., "PILOT_001")
#' @return List with metadata
read_metadata <- function(run_id) {
  file_path <- file.path(TRACES_PATH, paste0(run_id, "_metadata.json"))
  
  if (!file.exists(file_path)) {
    return(list())
  }
  
  metadata <- fromJSON(file_path)
  return(metadata)
}

#' Calculate statistics for a pressure trace
#' @param data Data frame with pressure trace
#' @return Data frame with statistics
calculate_statistics <- function(data) {
  stats <- data %>%
    summarize(
      run_id = first(run_id),
      mean_pressure_g = mean(load_g, na.rm = TRUE),
      sd_pressure_g = sd(load_g, na.rm = TRUE),
      cv_percent = (sd_pressure_g / mean_pressure_g) * 100,
      min_pressure_g = min(load_g, na.rm = TRUE),
      max_pressure_g = max(load_g, na.rm = TRUE),
      median_pressure_g = median(load_g, na.rm = TRUE),
      n_observations = n(),
      duration_sec = max(elapsed_sec, na.rm = TRUE) - min(elapsed_sec, na.rm = TRUE)
    )
  
  return(stats)
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("Loading data...\n")

# Load experimental metadata
exp_metadata <- read_csv(METADATA_FILE, col_types = cols())

# Filter to samples with traces
exp_metadata_filtered <- exp_metadata %>%
  filter(RunID %in% SAMPLES_WITH_TRACES)

# Load all pressure traces
all_traces <- list()
for (sample_id in SAMPLES_WITH_TRACES) {
  cat(paste("  Reading", sample_id, "..."))
  trace <- tryCatch({
    read_pressure_trace(sample_id)
  }, error = function(e) {
    cat(" ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(trace)) {
    cat(paste(" OK (", nrow(trace), "rows)\n"))
    all_traces[[sample_id]] <- trace
  } else {
    cat(" FAILED\n")
  }
}

# Combine all traces into one data frame
combined_traces <- bind_rows(all_traces)

# Merge with experimental metadata
combined_data <- combined_traces %>%
  left_join(exp_metadata_filtered, by = c("run_id" = "RunID"))

cat(paste("Loaded", length(all_traces), "pressure traces\n"))
cat(paste("Total observations:", nrow(combined_data), "\n\n"))

# ==============================================================================
# INDIVIDUAL PLOTS
# ==============================================================================

cat("Creating individual plots...\n")

for (sample_id in SAMPLES_WITH_TRACES) {
  sample_data <- combined_data %>% filter(run_id == sample_id)
  
  if (nrow(sample_data) == 0) next
  
  # Get sample metadata
  sample_info <- exp_metadata_filtered %>% filter(RunID == sample_id)
  pressure_level <- sample_info$Pressure_g[1]
  surface_type <- toupper(sample_info$Surface_type[1])
  
  # Create plot
  p <- ggplot(sample_data, aes(x = elapsed_sec, y = load_g)) +
    geom_line(color = "#2C3E50", linewidth = 0.5, alpha = 0.8) +
    labs(
      title = paste(sample_id, "- Pressure Trace"),
      subtitle = paste0(pressure_level, "g target pressure | ", surface_type, " surface"),
      x = "Time (seconds)",
      y = "Load (g)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "#7F8C8D", size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
      axis.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Add horizontal line for target pressure
  p <- p + geom_hline(yintercept = pressure_level, 
                      linetype = "dashed", 
                      color = "#E74C3C", 
                      alpha = 0.5,
                      linewidth = 0.8)
  
  # Save plot
  output_file <- file.path(OUTPUT_PATH, "individual_plots", 
                           paste0(sample_id, "_pressure_trace.png"))
  ggsave(output_file, p, width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("  Saved:", sample_id, "\n"))
}

cat("\n")

# ==============================================================================
# GROUPED PLOTS
# ==============================================================================

cat("Creating grouped plots...\n")

# Define color palette for samples
color_palette <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", 
                   "#9B59B6", "#1ABC9C", "#34495E", "#E67E22")

# Create plots for each combination of pressure level and surface type
pressure_levels <- c(50, 200)
surface_types <- c("steel", "abs")

for (pressure in pressure_levels) {
  for (surface in surface_types) {
    
    # Filter data
    plot_data <- combined_data %>%
      filter(Pressure_g == pressure, tolower(Surface_type) == tolower(surface))
    
    if (nrow(plot_data) == 0) {
      cat(paste("  No data for", pressure, "g", toupper(surface), "\n"))
      next
    }
    
    # Get unique samples
    samples <- unique(plot_data$run_id)
    n_samples <- length(samples)
    
    # Create plot
    p <- ggplot(plot_data, aes(x = elapsed_sec, y = load_g, color = run_id, group = run_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      labs(
        title = paste0(pressure, "g Pressure Traces - ", toupper(surface), " Surface"),
        subtitle = paste("Combined view of", n_samples, "sample(s)"),
        x = "Time (seconds)",
        y = "Load (g)",
        color = "Sample ID"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "#7F8C8D", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", color = "#BDC3C7", linewidth = 0.5),
        legend.title = element_text(face = "bold", size = 10)
      ) +
      scale_color_manual(values = color_palette[1:n_samples]) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
    
    # Add horizontal line for target pressure
    p <- p + geom_hline(yintercept = pressure, 
                        linetype = "dashed", 
                        color = "#95A5A6", 
                        alpha = 0.5,
                        linewidth = 0.8)
    
    # Save plot
    output_file <- file.path(OUTPUT_PATH, "grouped_plots", 
                             paste0(pressure, "g_", surface, "_combined.png"))
    ggsave(output_file, p, width = 12, height = 7, dpi = 300, bg = "white")
    
    cat(paste("  Saved:", pressure, "g", toupper(surface), "combined plot\n"))
  }
}

cat("\n")

# ==============================================================================
# STATISTICAL ANALYSIS
# ==============================================================================

cat("Performing statistical analysis...\n")

# Calculate per-sample statistics
sample_stats <- combined_data %>%
  group_by(run_id) %>%
  do(calculate_statistics(.)) %>%
  ungroup() %>%
  left_join(exp_metadata_filtered %>% 
              select(RunID, Pressure_level, Pressure_g, Surface_type, 
                     Solvent_level, PETN_Recovery_pct, RDX_Recovery_pct),
            by = c("run_id" = "RunID"))

# Summary by pressure level
pressure_summary <- sample_stats %>%
  group_by(Pressure_g) %>%
  summarize(
    n_samples = n(),
    mean_avg_pressure = mean(mean_pressure_g, na.rm = TRUE),
    sd_avg_pressure = sd(mean_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_percent, na.rm = TRUE),
    mean_min_pressure = mean(min_pressure_g, na.rm = TRUE),
    mean_max_pressure = mean(max_pressure_g, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by surface type
surface_summary <- sample_stats %>%
  group_by(Surface_type) %>%
  summarize(
    n_samples = n(),
    mean_avg_pressure = mean(mean_pressure_g, na.rm = TRUE),
    sd_avg_pressure = sd(mean_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_percent, na.rm = TRUE),
    mean_min_pressure = mean(min_pressure_g, na.rm = TRUE),
    mean_max_pressure = mean(max_pressure_g, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by pressure AND surface
combined_summary <- sample_stats %>%
  group_by(Pressure_g, Surface_type) %>%
  summarize(
    n_samples = n(),
    mean_avg_pressure = mean(mean_pressure_g, na.rm = TRUE),
    sd_avg_pressure = sd(mean_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_percent, na.rm = TRUE),
    mean_min_pressure = mean(min_pressure_g, na.rm = TRUE),
    mean_max_pressure = mean(max_pressure_g, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# EXPORT STATISTICS
# ==============================================================================

cat("Exporting statistical summaries...\n")

# Export per-sample statistics
write_csv(sample_stats, 
          file.path(OUTPUT_PATH, "sample_statistics.csv"))

# Export pressure level summary
write_csv(pressure_summary, 
          file.path(OUTPUT_PATH, "pressure_level_summary.csv"))

# Export surface type summary
write_csv(surface_summary, 
          file.path(OUTPUT_PATH, "surface_type_summary.csv"))

# Export combined summary
write_csv(combined_summary, 
          file.path(OUTPUT_PATH, "pressure_surface_summary.csv"))

# ==============================================================================
# PRINT SUMMARY TO CONSOLE
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("                     STATISTICAL ANALYSIS SUMMARY\n")
cat("================================================================================\n\n")

cat("PER-SAMPLE STATISTICS:\n")
cat("----------------------\n")
print(sample_stats %>% 
        select(run_id, Pressure_g, Surface_type, mean_pressure_g, 
               sd_pressure_g, cv_percent, min_pressure_g, max_pressure_g) %>%
        arrange(Pressure_g, Surface_type), 
      n = Inf)

cat("\n\nSUMMARY BY PRESSURE LEVEL:\n")
cat("--------------------------\n")
print(pressure_summary, n = Inf)

cat("\n\nSUMMARY BY SURFACE TYPE:\n")
cat("------------------------\n")
print(surface_summary, n = Inf)

cat("\n\nSUMMARY BY PRESSURE AND SURFACE:\n")
cat("--------------------------------\n")
print(combined_summary, n = Inf)

cat("\n")
cat("================================================================================\n")
cat("                            ANALYSIS COMPLETE\n")
cat("================================================================================\n")
cat("\nOutputs saved to:", OUTPUT_PATH, "\n")
cat("\nGenerated files:\n")
cat("  - Individual plots:", length(SAMPLES_WITH_TRACES), "PNG files\n")
cat("  - Grouped plots: 4 PNG files (50g/200g x steel/ABS)\n")
cat("  - Statistics: 4 CSV files\n")
cat("\n")
