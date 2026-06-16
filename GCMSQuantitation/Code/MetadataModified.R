
# =========================================================
# GC-MS PETN Quantification Script
# =========================================================
#
# This script performs quantitative GC-MS analysis of PETN
# using external calibration with the following features:
#
#  - Multiple calibration sets (bracketed in a sequence run)
#  - Quadratic calibration with linear fallback
#  - Back-calculation of calibration standards
#  - Protection against extrapolation
#  - Full plotting:
#       * One plot per calibration (curve + standards + samples)
#       * One combined plot of all calibrations with curves
#  - Calculation of adjusted concentrations and %bias
#
# This script is intended to be open-source, readable,
# auditable, and defensible for analytical chemistry workflows.
#
# Author: <add name>
# License: MIT (recommended for open-source)
# =========================================================

# -----------------------------
# Load required libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)     # data manipulation
  library(ggplot2)   # plotting
  library(stringr)   # regex-based string parsing
})

# -----------------------------
# Required user-defined globals
# -----------------------------
# These must be defined before running the script
required_globals <- c(
  "DataFolder",     # folder containing sequence log
  "GcData.dir",     # folder containing *Summary.csv files
  "Results.dir",    # output folder
  "ParentFolder",   # name used for output CSV
  "Dilution",       # dilution factor applied to standards/samples
  "SampleVol",      # injected or extracted sample volume
  "DepositMass"     # reference mass for recovery calculation
)

missing_globals <- required_globals[!sapply(required_globals, exists)]
if (length(missing_globals) > 0) {
  stop("Missing required globals: ",
       paste(missing_globals, collapse = ", "))
}

# =========================================================
# Concentration solver
# =========================================================
# Attempts to invert a quadratic calibration curve to obtain
# concentration from peak area. If quadratic inversion fails,
# a linear fallback is used. Extrapolation is explicitly blocked.
#
# Args:
#   y            : observed peak area
#   quad_model   : quadratic lm model
#   lin_model    : linear lm model
#   cal_y_range  : valid peak-area range of the calibration
#
# Returns:
#   list(value, method)
# =========================================================
solve_concentration <- function(y, quad_model, lin_model, cal_y_range) {

  # Missing or absent peak
  if (is.na(y)) {
    return(list(value = NA_real_, method = "No_peak"))
  }

  # Block extrapolation outside calibration domain
  if (y < cal_y_range[1] || y > cal_y_range[2]) {
    return(list(value = NA_real_, method = "Out_of_range"))
  }

  # Extract quadratic coefficients
  coefs <- coef(quad_model)
  a <- coefs["I(CalLevelAdj^2)"]
  b <- coefs["CalLevelAdj"]
  c <- coefs["(Intercept)"]

  # Quadratic discriminant
  disc <- b^2 - 4 * a * (c - y)

  # Attempt quadratic inversion
  if (is.finite(disc) && disc >= 0 && abs(a) > .Machine$double.eps) {

    roots <- c(
      (-b + sqrt(disc)) / (2 * a),
      (-b - sqrt(disc)) / (2 * a)
    )

    # Keep only physically meaningful roots
    roots <- roots[is.finite(roots) & roots >= 0]

    if (length(roots) > 0) {
      return(list(value = min(roots), method = "Quadratic"))
    }
  }

  # Linear fallback
  lin <- coef(lin_model)
  slope <- lin[2]
  intercept <- lin[1]

  if (is.na(slope) || slope == 0) {
    return(list(value = NA_real_, method = "Unquantifiable"))
  }

  x <- (y - intercept) / slope
  if (x < 0) {
    return(list(value = NA_real_, method = "Unquantifiable"))
  }

  list(value = x, method = "Linear")
}

# =========================================================
# Load GC-MS summary files
# =========================================================
summary_files <- list.files(
  GcData.dir,
  pattern = "Summary.csv$",
  full.names = TRUE
)

GcResults <- bind_rows(lapply(summary_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$File <- basename(f)
  df
}))

# =========================================================
# Parse sequence metadata
# =========================================================
log_file <- list.files(DataFolder, pattern = "Sequence", full.names = TRUE)[1]
log_lines <- readLines(log_file)

# Extract run date
date_line <- log_lines[str_detect(log_lines, "Starting sequence")]
date_str <- str_extract(
  date_line,
  "\\w+ \\w+ \\d+ \\d{2}:\\d{2}:\\d{2} \\d{4}"
)
run_date <- format(
  as.Date(as.POSIXct(date_str, format = "%a %b %d %H:%M:%S %Y")),
  "%Y%m%d"
)

# Extract sequence table lines
metadata_lines <- log_lines[str_detect(log_lines, "^\\s*\\d+\\)")]

# Parse a single sequence line
parse_line <- function(line) {

  m <- str_match(
    line,
    "^\\s*(\\d+)\\)\\s+(\\w+)\\s+(\\d+)\\s+(\\d+)\\s+(.*)$"
  )
  if (any(is.na(m))) return(NULL)

  cal_level <- NA_real_
  if (m[3] == "Cal") {
    cm <- str_match(m[6], "(\\d+(\\.\\d+)?)ng")
    if (!is.na(cm[1])) cal_level <- as.numeric(cm[2])
  }

  data.frame(
    Date = run_date,
    Line = as.integer(m[2]),
    Type = m[3],
    Vial = as.integer(m[4]),
    DataFile = paste0(m[5], "Summary.csv"),
    SampleName = m[6],
    CalLevel = cal_level,
    stringsAsFactors = FALSE
  )
}

Metadata <- bind_rows(lapply(metadata_lines, parse_line))

# =========================================================
# Combine metadata and GC-MS results
# =========================================================
Combined <- Metadata %>%
  left_join(GcResults, by = c("DataFile" = "File")) %>%
  mutate(Row = row_number())

# =========================================================
# Build calibration sets
# =========================================================
CalData <- Combined %>%
  filter(Type == "Cal", !is.na(CalLevel)) %>%
  arrange(Row) %>%
  mutate(CalLevelAdj = CalLevel * Dilution)

# Assume equal number of calibration levels per set
n_levels <- n_distinct(CalData$CalLevelAdj)
CalSets <- split(
  CalData,
  rep(seq_len(nrow(CalData) / n_levels), each = n_levels)
)

Combined$Concentration   <- NA_real_
Combined$QuantMethod     <- NA_character_
Combined$CalibrationSet  <- NA_integer_

# =========================================================
# Apply calibration models and generate plots
# =========================================================
curve_store <- list()

for (i in seq_along(CalSets)) {

  cal <- CalSets[[i]]

  quad_model <- lm(PETN.PA ~ CalLevelAdj + I(CalLevelAdj^2), data = cal)
  lin_model  <- lm(PETN.PA ~ CalLevelAdj, data = cal)

  cal_y_range <- range(cal$PETN.PA, na.rm = TRUE)

  # Rows quantified by this calibration
  rows_std <- Combined$Row %in% cal$Row
  rows_smp <- Combined$Row > max(cal$Row)
  if (i < length(CalSets)) {
    rows_smp <- rows_smp & Combined$Row < min(CalSets[[i + 1]]$Row)
  }
  rows <- which(rows_std | rows_smp)

  # Quantify rows
  for (j in rows) {
    res <- solve_concentration(
      Combined$PETN.PA[j],
      quad_model,
      lin_model,
      cal_y_range
    )
    Combined$Concentration[j]  <- res$value
    Combined$QuantMethod[j]    <- res$method
    Combined$CalibrationSet[j] <- i
  }

  # Store curve for combined plot
  curve_x <- seq(min(cal$CalLevelAdj), max(cal$CalLevelAdj), length.out = 200)
  curve_store[[i]] <- data.frame(
    CalibrationSet = i,
    CalLevelAdj = curve_x,
    Quad = predict(quad_model, newdata = data.frame(CalLevelAdj = curve_x))
  )

  # -------- Per-calibration plot --------
  plot_data <- Combined %>%
    filter(CalibrationSet == i & !is.na(PETN.PA)) %>%
    mutate(
      Xplot = ifelse(Type == "Cal", CalLevel * Dilution, Concentration),
      Group = ifelse(Type == "Cal", "Standard", "Sample")
    )

  p <- ggplot() +
    geom_point(
      data = plot_data,
      aes(Xplot, PETN.PA, color = Group, shape = Group),
      size = 3
    ) +
    geom_line(
      data = curve_store[[i]],
      aes(CalLevelAdj, Quad),
      color = "black"
    ) +
    theme_bw() +
    labs(
      title = paste("Calibration", i),
      x = "PETN concentration (adjusted)",
      y = "PETN peak area"
    )

  ggsave(
    paste0("Calibration_", i, "_Detail.png"),
    p,
    path = Results.dir,
    width = 7.5,
    height = 5,
    dpi = 300
  )
}

# =========================================================
# Combined calibration plot (all curves and points)
# =========================================================
all_cals <- Combined %>%
  filter(Type == "Cal") %>%
  mutate(Xplot = CalLevel * Dilution)

all_curves <- bind_rows(curve_store)

p_all <- ggplot() +
  geom_point(
    data = all_cals,
    aes(Xplot, PETN.PA, color = factor(CalibrationSet)),
    size = 3
  ) +
  geom_line(
    data = all_curves,
    aes(CalLevelAdj, Quad, color = factor(CalibrationSet)),
    linewidth = 1
  ) +
  theme_bw() +
  labs(
    title = "All Calibration Curves",
    x = "PETN concentration",
    y = "PETN peak area",
    color = "Calibration Set"
  )

ggsave(
  "All_Calibrations.png",
  p_all,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300
)

# =========================================================
# Final QC calculations
# =========================================================
Combined <- Combined %>%
  mutate(
    TrueCalConcAdj = ifelse(Type == "Cal", CalLevel * Dilution, NA_real_),
    PercentBias = ifelse(
      Type == "Cal" & !is.na(Concentration),
      (Concentration - TrueCalConcAdj) / TrueCalConcAdj * 100,
      NA_real_
    ),
    SampleConc = Concentration / Dilution,
    MassinSample = SampleConc * SampleVol,
    PercentRecovery = MassinSample / DepositMass * 100
  ) %>%
  arrange(SampleName)

# =========================================================
# Export final results
# =========================================================
outfile <- file.path(
  Results.dir,
  paste0(ParentFolder, "_GCMSResults.csv")
)
write.csv(Combined, outfile, row.names = FALSE)

# ======================= END OF SCRIPT ====================
