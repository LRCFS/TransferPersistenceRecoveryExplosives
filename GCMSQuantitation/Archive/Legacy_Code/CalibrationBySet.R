# =========================================================
# Calibration Graphs Separated by Physical Standard Set
# =========================================================
#
# Run this script AFTER MetadataModified.R has been executed
# and the _GCMSResults.csv file exists in the Results folder.
#
# Produces per-analyte, per-physical-set calibration plots
# with the three replicate calibration curves overlaid.
#
# Requires: ggplot2, dplyr, stringr (loaded by GlobalCode.R)
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

# =========================================================
# Load results
# =========================================================
results_file <- file.path(Results.dir, paste0(ParentFolder, "_GCMSResults.csv"))

if (!file.exists(results_file)) {
  stop("Results file not found: ", results_file,
       "\nRun MetadataModified.R first.")
}

results <- read.csv(results_file, stringsAsFactors = FALSE)

# =========================================================
# Identify physical standard set from SampleName
# =========================================================
# SampleName format: "Xng RDX PETN - Set 1" / "... - Set 2"
results$PhysicalSet <- str_extract(results$SampleName, "Set \\d+")

# Filter to calibration standards only
cal_data <- results %>%
  filter(Type == "Cal", !is.na(CalLevel), !is.na(PhysicalSet))

if (nrow(cal_data) == 0) {
  stop("No calibration data found with 'Set' labels in SampleName.")
}

# Label each replicate analysis within a physical set
# CalibrationSet 1,3,5 = Set 1 replicates 1,2,3
# CalibrationSet 2,4,6 = Set 2 replicates 1,2,3
cal_data <- cal_data %>%
  group_by(PhysicalSet) %>%
  arrange(CalibrationSet, .by_group = TRUE) %>%
  mutate(
    Replicate = match(CalibrationSet, sort(unique(CalibrationSet))),
    ReplicateLabel = paste0("Replicate ", Replicate)
  ) %>%
  ungroup()

# Use the dilution-adjusted concentration as x-axis
cal_data$ConcAdj <- cal_data$CalLevel * Dilution

# =========================================================
# Build faceted point + curve data for one response column
# =========================================================
# Returns a list(points, curves) with a Method column for
# faceting, or NULL if no data is available.

build_facet_data <- function(data, response, method_label) {

  plot_data <- data %>% filter(!is.na(.data[[response]]))
  if (nrow(plot_data) == 0) return(NULL)

  # Fit quadratic model per replicate for smooth curves
  curve_data <- plot_data %>%
    group_by(ReplicateLabel) %>%
    do({
      df <- .
      model <- lm(reformulate(c("ConcAdj", "I(ConcAdj^2)"),
                               response = response), data = df)
      x_seq <- seq(min(df$ConcAdj), max(df$ConcAdj), length.out = 200)
      data.frame(
        ConcAdj = x_seq,
        Fitted = predict(model, newdata = data.frame(ConcAdj = x_seq)),
        ReplicateLabel = df$ReplicateLabel[1]
      )
    }) %>%
    ungroup()

  # Rename response to a common y column for faceting
  points <- plot_data %>%
    mutate(y_val = .data[[response]], Method = method_label)

  curves <- curve_data %>%
    mutate(Method = method_label)

  list(points = points, curves = curves)
}

# =========================================================
# Generate plots for each analyte and physical set
# =========================================================
analyte_defs <- list(
  PETN = list(
    pa_col      = "petn_pa",
    ratio_col   = "petn_ratio",
    pa_label    = "Peak Area",
    ratio_label = "PA / IS Ratio"
  ),
  RDX = list(
    pa_col      = "rdx_pa",
    ratio_col   = "rdx_ratio",
    pa_label    = "Peak Area",
    ratio_label = "PA / IS Ratio"
  )
)

physical_sets <- sort(unique(cal_data$PhysicalSet))

for (analyte_name in names(analyte_defs)) {
  adef <- analyte_defs[[analyte_name]]

  for (pset in physical_sets) {

    subset_data <- cal_data %>% filter(PhysicalSet == pset)

    # Build facet data for both response types
    pa_data    <- build_facet_data(subset_data, adef$pa_col,    adef$pa_label)
    ratio_data <- build_facet_data(subset_data, adef$ratio_col, adef$ratio_label)

    # Combine whichever facets have data
    all_points <- bind_rows(
      if (!is.null(pa_data))    pa_data$points,
      if (!is.null(ratio_data)) ratio_data$points
    )
    all_curves <- bind_rows(
      if (!is.null(pa_data))    pa_data$curves,
      if (!is.null(ratio_data)) ratio_data$curves
    )

    if (nrow(all_points) == 0 || nrow(all_curves) == 0) next

    p <- ggplot() +
      geom_point(
        data = all_points,
        aes(x = ConcAdj, y = y_val,
            colour = ReplicateLabel, shape = ReplicateLabel),
        size = 3
      ) +
      geom_line(
        data = all_curves,
        aes(x = ConcAdj, y = Fitted, colour = ReplicateLabel),
        linewidth = 0.8
      ) +
      facet_wrap(~Method, scales = "free_y") +
      theme_bw() +
      labs(
        title = paste0(analyte_name, " Calibration - ", pset),
        x = expression("Concentration / " * mu * "g" %.% "mL"^{-1}),
        y = "Response",
        colour = "Analysis",
        shape  = "Analysis"
      ) +
      theme(
        text = element_text(size = 12),
        legend.position = "bottom"
      )

    fname <- paste0(analyte_name, "_Calibration_", gsub(" ", "", pset), ".png")
    ggsave(fname, p, path = Results.dir,
           width = 12, height = 5, dpi = 300)
    print(p)
  }
}

cat("Calibration plots by physical set saved to:", Results.dir, "\n")
