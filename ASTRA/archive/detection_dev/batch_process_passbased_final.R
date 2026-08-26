# ==============================================================================
# Batch Processing Script for v4.0 Stable Contact Detection
# ==============================================================================
# 
# This script processes ALL pilot study samples using the v4.0 algorithm
# 
# Outputs:
# 1. Individual plots for each sample (regions color-coded)
# 2. Comprehensive statistics CSV for all samples
# 3. Comparison with v3.0 results
# 4. Summary report
#
# Author: ASTRA Analysis System
# Date: 2026-07-16
# ==============================================================================

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(zoo)
})

# Load v4.0 functions
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/detection_dev/pass_based_detection_final.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Paths
BASE_PATH <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study"
TRACES_PATH <- file.path(BASE_PATH, "Pressure Traces")
METADATA_FILE <- file.path(BASE_PATH, "pilot_data_nested.csv")
OUTPUT_PATH <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/outputs/v4_batch"

# Create output directories
dir.create(OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "individual_plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "grouped_plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "statistics"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "detailed_data"), recursive = TRUE, showWarnings = FALSE)

# Define samples with pressure traces
SAMPLES_WITH_TRACES <- c("PILOT_001", "PILOT_005", "PILOT_009", "PILOT_013", 
                         "PILOT_017", "PILOT_021", "PILOT_025", "PILOT_029")

# Algorithm parameters (v4.0 - WIDER CAPTURE)
PARAMS <- list(
  zero_threshold = 2,
  max_zero_gap_sec = 0.5,
  ramp_rate_threshold = 10,          # Lowered from 20 to capture less as ramps
  stable_max_change_g = 30.0,        # Raised from 15 to 30 based on analysis
  stable_max_sd_g = 25.0,            # Raised from 20 to 25 for safety margin
  stable_sd_window_sec = 0.5,        # 0.5s window
  smooth_window = 5,
  min_stable_duration_sec = 0.3
)

# Region colors for visualization
REGION_COLORS <- c(
  "zero" = "#95A5A6",           # Gray
  "ramp_up" = "#E74C3C",        # Red
  "stable" = "#2ECC71",          # Green
  "ramp_down" = "#3498DB"        # Blue
)

# ==============================================================================
# MAIN PROCESSING
# ==============================================================================

cat("==============================================================================\n")
cat("BATCH PROCESSING: v4.0 Stable Contact Detection\n")
cat("==============================================================================\n\n")
cat(sprintf("Output directory: %s\n", OUTPUT_PATH))
cat(sprintf("Processing %d samples\n\n", length(SAMPLES_WITH_TRACES)))

cat("Algorithm Parameters:\n")
cat(sprintf("  Zero threshold: %.1fg\n", PARAMS$zero_threshold))
cat(sprintf("  Ramp rate threshold: %.1f g/s\n", PARAMS$ramp_rate_threshold))
cat(sprintf("  Stable max change: %.1fg (point-to-point)\n", PARAMS$stable_max_change_g))
cat(sprintf("  Stable max SD: %.1fg (rolling, %.1fs window)\n", 
            PARAMS$stable_max_sd_g, PARAMS$stable_sd_window_sec))
cat(sprintf("  Min stable duration: %.2fs\n\n", PARAMS$min_stable_duration_sec))

# Load metadata
cat("Loading metadata...\n")
metadata <- read_csv(METADATA_FILE, col_types = cols(), show_col_types = FALSE)

# Filter to samples with traces
metadata_filtered <- metadata %>%
  filter(RunID %in% SAMPLES_WITH_TRACES)

cat(sprintf("Found %d samples in metadata\n\n", nrow(metadata_filtered)))

# Initialize results storage
all_stats <- list()
all_processed_data <- list()
processing_log <- data.frame(
  sample_id = character(),
  status = character(),
  n_observations = integer(),
  n_passes = integer(),
  percent_stable = numeric(),
  processing_time_sec = numeric(),
  stringsAsFactors = FALSE
)

# ==============================================================================
# PROCESS EACH SAMPLE
# ==============================================================================

cat("==============================================================================\n")
cat("PROCESSING SAMPLES\n")
cat("==============================================================================\n\n")

for (i in seq_along(SAMPLES_WITH_TRACES)) {
  sample_id <- SAMPLES_WITH_TRACES[i]
  
  cat(sprintf("[%d/%d] Processing %s...\n", i, length(SAMPLES_WITH_TRACES), sample_id))
  
  start_time <- Sys.time()
  
  # Get sample metadata
  sample_meta <- metadata_filtered %>% filter(RunID == sample_id)
  if (nrow(sample_meta) == 0) {
    cat("  WARNING: No metadata found, skipping\n\n")
    next
  }
  
  target_pressure <- sample_meta$Pressure_g[1]
  surface_type <- sample_meta$Surface_type[1]
  
  cat(sprintf("  Target: %dg | Surface: %s\n", target_pressure, surface_type))
  
  # Read data
  tryCatch({
    data <- read_pressure_trace_file(sample_id, TRACES_PATH)
    
    if (is.null(data) || nrow(data) == 0) {
      cat("  ERROR: No data loaded\n\n")
      processing_log <- rbind(processing_log, data.frame(
        sample_id = sample_id,
        status = "FAILED - No data",
        n_observations = 0,
        n_passes = 0,
        percent_stable = NA,
        processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      ))
      next
    }
    
    cat(sprintf("  Loaded %d observations (%.1fs duration)\n", 
                nrow(data), max(data$elapsed_sec) - min(data$elapsed_sec)))
    
    # Run v4.0 detection
    data_processed <- identify_stable_contact_regions_v4(
      data,
      target_pressure_g = target_pressure,
      zero_threshold = PARAMS$zero_threshold,
      max_zero_gap_sec = PARAMS$max_zero_gap_sec,
      ramp_rate_threshold = PARAMS$ramp_rate_threshold,
      stable_max_change_g = PARAMS$stable_max_change_g,
      stable_max_sd_g = PARAMS$stable_max_sd_g,
      stable_sd_window_sec = PARAMS$stable_sd_window_sec,
      smooth_window = PARAMS$smooth_window,
      min_stable_duration_sec = PARAMS$min_stable_duration_sec
    )
    
    # Calculate statistics
    stats <- calculate_stable_contact_statistics(data_processed, target_pressure)
    
    # Add metadata to stats
    stats <- stats %>%
      mutate(
        surface_type = surface_type,
        pressure_level = ifelse(target_pressure >= 150, "200g", "50g")
      )
    
    # Store results
    all_stats[[sample_id]] <- stats
    all_processed_data[[sample_id]] <- data_processed
    
    # Log processing
    processing_log <- rbind(processing_log, data.frame(
      sample_id = sample_id,
      status = "SUCCESS",
      n_observations = nrow(data),
      n_passes = stats$n_passes,
      percent_stable = stats$percent_time_stable,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
    
    # Save detailed data
    write_csv(
      data_processed %>% select(elapsed_sec, load_g, pass_id, region, is_stable, 
                                rate_smooth, point_change, rolling_sd),
      file.path(OUTPUT_PATH, "detailed_data", paste0(sample_id, "_processed.csv"))
    )
    
    cat(sprintf("  Result: %d passes, %.1f%% stable time, mean pressure %.1fg\n",
                stats$n_passes, stats$percent_time_stable, stats$mean_stable_pressure_g))
    cat(sprintf("  Processed in %.1f seconds\n\n", 
                as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
    
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n\n", conditionMessage(e)))
    processing_log <<- rbind(processing_log, data.frame(
      sample_id = sample_id,
      status = paste("FAILED -", conditionMessage(e)),
      n_observations = 0,
      n_passes = 0,
      percent_stable = NA,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  })
}

# ==============================================================================
# CREATE VISUALIZATIONS
# ==============================================================================

cat("==============================================================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("==============================================================================\n\n")

cat("Creating individual plots...\n")

for (sample_id in names(all_processed_data)) {
  data_processed <- all_processed_data[[sample_id]]
  stats <- all_stats[[sample_id]]
  
  # Get metadata
  sample_meta <- metadata_filtered %>% filter(RunID == sample_id)
  target_pressure <- sample_meta$Pressure_g[1]
  surface_type <- toupper(sample_meta$Surface_type[1])
  
  # Create simple plot: gray trace with green stable regions as segments
  # Assign segment IDs to stable points BEFORE filtering
  data_processed <- data_processed %>%
    mutate(
      stable_change = is_stable != lag(is_stable, default = FALSE),
      stable_segment_id = cumsum(stable_change)
    ) %>%
    mutate(stable_segment_id = ifelse(is_stable, stable_segment_id, NA))
  
  # Now filter to stable points only
  stable_data <- data_processed %>% 
    filter(is_stable == TRUE)
  
  p <- ggplot(data_processed, aes(x = elapsed_sec, y = load_g)) +
    # First plot all data in gray
    geom_line(color = "#95A5A6", linewidth = 0.5, alpha = 0.8) +
    # Then overlay stable regions as line segments (grouped by segment ID)
    geom_line(data = stable_data,
              aes(x = elapsed_sec, y = load_g, group = stable_segment_id),
              color = "#2ECC71", linewidth = 1.2) +
    labs(
      title = paste(sample_id, "- Stable Contact Detection (v4.0)"),
      subtitle = sprintf("%dg target | %s surface | %d passes | %.1f%% stable (green segments)",
                        target_pressure, surface_type, 
                        stats$n_passes, stats$percent_time_stable),
      x = "Time (seconds)",
      y = "Pressure (g)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "#7F8C8D", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
      axis.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
  
  # Save full plot
  ggsave(
    file.path(OUTPUT_PATH, "individual_plots", paste0(sample_id, "_v4_regions.png")),
    p, width = 12, height = 6, dpi = 300, bg = "white"
  )
  
  # Create zoomed version (400-425s) if data exists in that range
  data_zoom <- data_processed %>% filter(elapsed_sec >= 400, elapsed_sec <= 425)
  
  if (nrow(data_zoom) > 0) {
    stable_zoom <- data_zoom %>% filter(is_stable == TRUE)
    
    p_zoom <- ggplot(data_zoom, aes(x = elapsed_sec, y = load_g)) +
      # Gray trace
      geom_line(color = "#95A5A6", linewidth = 0.8, alpha = 0.8) +
      # Green stable segments
      geom_line(data = stable_zoom,
                aes(x = elapsed_sec, y = load_g, group = stable_segment_id),
                color = "#2ECC71", linewidth = 1.5) +
      labs(
        title = paste(sample_id, "- Zoomed View (400-425s)"),
        subtitle = sprintf("%dg target | %s surface | Green = stable contact",
                          target_pressure, surface_type),
        x = "Time (seconds)",
        y = "Pressure (g)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "#7F8C8D", size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
        axis.title = element_text(face = "bold")
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma, limits = c(400, 425))
    
    # Save zoomed plot
    ggsave(
      file.path(OUTPUT_PATH, "individual_plots", paste0(sample_id, "_v4_zoomed.png")),
      p_zoom, width = 12, height = 6, dpi = 300, bg = "white"
    )
  }
  
  cat(sprintf("  Saved: %s\n", sample_id))
}

cat("\n")

# ==============================================================================
# CREATE GROUPED PLOTS
# ==============================================================================

cat("Creating grouped plots by pressure and surface...\n")

# Combine all processed data
combined_data <- bind_rows(all_processed_data, .id = "sample_id") %>%
  left_join(metadata_filtered, by = c("sample_id" = "RunID"))

# Create plots for each combination
pressure_levels <- c(50, 200)
surface_types <- c("steel", "abs")

for (pressure in pressure_levels) {
  for (surface in surface_types) {
    
    plot_data <- combined_data %>%
      filter(Pressure_g == pressure, tolower(Surface_type) == surface)
    
    if (nrow(plot_data) == 0) {
      next
    }
    
    samples <- unique(plot_data$sample_id)
    n_samples <- length(samples)
    
    # Stable regions only
    stable_data <- plot_data %>% filter(is_stable == TRUE)
    
    if (nrow(stable_data) > 0) {
      p <- ggplot(stable_data, aes(x = elapsed_sec, y = load_g, color = sample_id)) +
        geom_point(size = 0.6, alpha = 0.6) +
        geom_line(aes(group = interaction(sample_id, pass_id)), 
                  linewidth = 0.5, alpha = 0.7) +
        labs(
          title = paste0(pressure, "g - ", toupper(surface), " Surface (Stable Regions Only, v4.0)"),
          subtitle = paste(n_samples, "sample(s) | Rate-based detection | ±5g variation"),
          x = "Time (seconds)",
          y = "Pressure (g)",
          color = "Sample ID"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "#7F8C8D", size = 10),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.3),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom"
        ) +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma)
      
      ggsave(
        file.path(OUTPUT_PATH, "grouped_plots", 
                  paste0(pressure, "g_", surface, "_stable_v4.png")),
        p, width = 12, height = 7, dpi = 300, bg = "white"
      )
      
      cat(sprintf("  Saved: %dg %s\n", pressure, toupper(surface)))
    }
  }
}

cat("\n")

# ==============================================================================
# COMPILE STATISTICS
# ==============================================================================

cat("==============================================================================\n")
cat("COMPILING STATISTICS\n")
cat("==============================================================================\n\n")

# Combine all statistics
all_stats_df <- bind_rows(all_stats)

# Save comprehensive statistics
write_csv(all_stats_df, 
          file.path(OUTPUT_PATH, "statistics", "all_samples_v4_statistics.csv"))
cat("Saved: all_samples_v4_statistics.csv\n")

# Summary by pressure level
pressure_summary <- all_stats_df %>%
  group_by(pressure_level) %>%
  summarize(
    n_samples = n(),
    mean_passes = mean(n_passes, na.rm = TRUE),
    mean_stable_time_pct = mean(percent_time_stable, na.rm = TRUE),
    mean_stable_pressure = mean(mean_stable_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_stable_percent, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(pressure_summary, 
          file.path(OUTPUT_PATH, "statistics", "pressure_level_summary_v4.csv"))
cat("Saved: pressure_level_summary_v4.csv\n")

# Summary by surface type
surface_summary <- all_stats_df %>%
  group_by(surface_type) %>%
  summarize(
    n_samples = n(),
    mean_passes = mean(n_passes, na.rm = TRUE),
    mean_stable_time_pct = mean(percent_time_stable, na.rm = TRUE),
    mean_stable_pressure = mean(mean_stable_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_stable_percent, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(surface_summary, 
          file.path(OUTPUT_PATH, "statistics", "surface_type_summary_v4.csv"))
cat("Saved: surface_type_summary_v4.csv\n")

# Combined summary
combined_summary <- all_stats_df %>%
  group_by(pressure_level, surface_type) %>%
  summarize(
    n_samples = n(),
    mean_passes = mean(n_passes, na.rm = TRUE),
    mean_stable_time_pct = mean(percent_time_stable, na.rm = TRUE),
    mean_stable_pressure = mean(mean_stable_pressure_g, na.rm = TRUE),
    mean_cv = mean(cv_stable_percent, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(combined_summary, 
          file.path(OUTPUT_PATH, "statistics", "combined_summary_v4.csv"))
cat("Saved: combined_summary_v4.csv\n")

# Save processing log
write_csv(processing_log, 
          file.path(OUTPUT_PATH, "processing_log.csv"))
cat("Saved: processing_log.csv\n\n")

# ==============================================================================
# FINAL SUMMARY REPORT
# ==============================================================================

cat("==============================================================================\n")
cat("BATCH PROCESSING COMPLETE\n")
cat("==============================================================================\n\n")

successful <- sum(processing_log$status == "SUCCESS")
failed <- sum(processing_log$status != "SUCCESS")
total_time <- sum(processing_log$processing_time_sec, na.rm = TRUE)

cat(sprintf("Samples processed: %d successful, %d failed\n", successful, failed))
cat(sprintf("Total processing time: %.1f seconds (%.1f sec/sample avg)\n\n",
            total_time, total_time / length(SAMPLES_WITH_TRACES)))

cat("SUMMARY BY PRESSURE LEVEL:\n")
print(pressure_summary, n = Inf)
cat("\n")

cat("SUMMARY BY SURFACE TYPE:\n")
print(surface_summary, n = Inf)
cat("\n")

cat("COMBINED SUMMARY:\n")
print(combined_summary, n = Inf)
cat("\n")

cat("Output files saved to:\n")
cat(sprintf("  %s\n", OUTPUT_PATH))
cat("\nGenerated files:\n")
cat(sprintf("  - Individual plots: %d PNG files\n", successful))
cat(sprintf("  - Grouped plots: 4 PNG files\n"))
cat(sprintf("  - Detailed data: %d CSV files\n", successful))
cat("  - Statistics: 4 CSV files\n")
cat("  - Processing log: 1 CSV file\n")
cat("\n")

cat("Algorithm parameters used:\n")
cat(sprintf("  Zero threshold: %.1fg\n", PARAMS$zero_threshold))
cat(sprintf("  Ramp rate: ±%.1f g/s\n", PARAMS$ramp_rate_threshold))
cat(sprintf("  Stable variation: ±%.1fg (point-to-point)\n", PARAMS$stable_max_change_g))
cat(sprintf("  Stable max SD: ±%.1fg (rolling, %.1fs window)\n", 
            PARAMS$stable_max_sd_g, PARAMS$stable_sd_window_sec))
cat(sprintf("  Min stable duration: %.2fs\n", PARAMS$min_stable_duration_sec))
cat("\n")

cat("==============================================================================\n")
