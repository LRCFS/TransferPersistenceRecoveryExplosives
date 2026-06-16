ModPeaks <- function(x, y = NULL, minPH = NULL, minPW = 0, thr = NULL, stepF = 0.49) {
  # Extract x/y from a two-column data frame or separate vectors
  # (replaces IDPmisc::getXY dependency)
  if (is.null(y)) {
    xx <- x[[1]]
    yy <- x[[2]]
  } else {
    xx <- x
    yy <- y
  }
  len.yy <- length(yy)
  
  min.yy <- min(yy, na.rm = TRUE)
  max.yy <- max(yy, na.rm = TRUE)
  yy <- c(min.yy, yy)
  xx <- c(xx[1], xx)
  
  if (is.null(thr)) thr <- min.yy
  if (is.null(minPH)) minPH <- (max.yy - min.yy) / 10
  if (stepF >= 0.5) stop("'stepF' must be smaller than 0.5")
  
  peak.x <- peak.y <- peak.w <- numeric(0)
  lev <- thr - stepF * minPH
  
  repeat {
    lev <- lev + stepF * minPH
    if (lev >= max.yy) break
    
    hi <- yy > lev
    start <- which(diff(c(FALSE, hi)) > 0)
    end <- which(diff(c(hi, FALSE)) < 0)
    
    for (ii in seq_along(start)) {
      seg_x <- xx[start[ii]:end[ii]]
      seg_y <- yy[start[ii]:end[ii]]
      i <- which.max(seg_y)
      x_peak <- seg_x[i]
      
      if (x_peak %in% peak.x) next
      
      miny <- min(yy[max(1, start[ii] - 1):min(end[ii] + 1, len.yy)], na.rm = TRUE)
      PH <- seg_y[i] - miny
      half_height <- miny + PH / 2
      PW_indices <- which(seg_y > half_height)
      
      if (length(PW_indices) > 1) {
        PW <- abs(seg_x[max(PW_indices)] - seg_x[min(PW_indices)])
      } else {
        PW <- 0
      }
      
      if (PH >= minPH && PW >= minPW) {
        peak.x <- c(peak.x, x_peak)
        peak.y <- c(peak.y, seg_y[i])
        peak.w <- c(peak.w, PW)
      }
    }
  }
  
  if (length(peak.x) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), w = numeric(0)))
  }
  
  res <- data.frame(x = peak.x, y = peak.y, w = peak.w)
  res <- res[order(res$x), ]
  return(res)
}

#############################################################
#####       Baseline Correction (rollingBall)           #####
#############################################################
# Applies rolling-ball baseline correction to a chromatographic
# signal column within a data frame. Returns the original data
# frame with three appended columns: CorrectedTrend,
# BaselineTrend, and Subtracted.
#
# Args:
#   df         : data frame with RetentionTime as the first
#                column and the signal in signal_col
#   signal_col : name (string) of the signal column to correct
#   wm         : rollingBall window width (default 120)
#   ws         : rollingBall smoothing width (default 40)
#
# Returns:
#   data frame with columns: RetentionTime, <signal_col>,
#   CorrectedTrend, BaselineTrend, Subtracted

apply_baseline_correction <- function(df, signal_col, wm = 120, ws = 40) {

  signal_vec <- df[[signal_col]]
  n <- length(signal_vec)

  # rollingBall needs sufficient data points to avoid internal
  # index errors. Minimum viable signal length is ~10 points.
  if (n < 10) {
    warning("Signal too short for baseline correction (n=", n,
            "). Returning uncorrected data.")
    result <- df
    names(result)[1] <- "RetentionTime"
    result$CorrectedTrend <- signal_vec
    result$BaselineTrend <- rep(0, n)
    result$Subtracted <- signal_vec
    return(result)
  }

  # Clamp wm and ws to safe values for short signals
  # rollingBall requires wm < n; use floor(n/2) - 1 as safe upper bound
  wm <- min(wm, floor(n / 2) - 1)
  ws <- min(ws, wm)

  # Convert to single-row numeric matrix for baseline()
  signal_matrix <- t(as.matrix(signal_vec))
  signal_matrix <- data.matrix(signal_matrix)

  bl <- baseline::baseline(signal_matrix[1, , drop = FALSE],
                           method = "rollingBall", wm = wm, ws = ws)

  # Extract corrected spectrum and baseline as single-column data frames
  corrected <- as.data.frame(t(as.data.frame(bl@corrected)))
  names(corrected)[1] <- "CorrectedTrend"

  baseline_trend <- as.data.frame(t(as.data.frame(bl@baseline)))
  names(baseline_trend)[1] <- "BaselineTrend"

  result <- cbind(df, corrected, baseline_trend)
  names(result)[1] <- "RetentionTime"

  result$Subtracted <- result[[signal_col]] - result$BaselineTrend

  return(result)
}

#############################################################
#####       SIM Scan-Group Deinterleaving                #####
#############################################################
# Many GC-MS SIM methods run two (or more) scan groups that
# both monitor the same m/z ion. The groups fire in rapid
# succession within each scan cycle, producing interleaved
# data points at slightly different retention times and
# systematically different intensity levels.
#
# This function detects paired scans (consecutive data points
# separated by less than `pair_gap` seconds) and keeps only
# the second point of each pair (Group B). Group B is
# typically higher-intensity and lower-noise on Agilent
# instruments. Unpaired (orphan) points are retained.
#
# Args:
#   df         : data frame with RetentionTime and Intensity
#                columns, sorted by RetentionTime
#   pair_gap   : maximum time gap (seconds) between two
#                consecutive points to be considered a pair
#                (default 0.1 s; intra-pair gap is ~0.054 s
#                on typical Agilent SIM methods)
#
# Returns:
#   data frame with the same columns, paired first-scans
#   removed. Row count is approximately halved when
#   interleaving is present.

deinterleave_sim <- function(df, pair_gap = 0.1) {

  n <- nrow(df)
  if (n < 2) return(df)

  # Ensure sorted by retention time
  df <- df[order(df$RetentionTime), ]

  # Time gaps between consecutive rows
  deltas <- diff(df$RetentionTime)

  # Identify which rows are the SECOND of a closely-spaced pair
  # (i.e., the row at index i+1 where delta[i] < pair_gap)
  is_group_b <- c(FALSE, deltas < pair_gap)

  # The row BEFORE each Group B is Group A (the first of the pair)
  is_group_a <- c(deltas < pair_gap, FALSE)

  # Keep: Group B rows (second of pair) + orphan rows (not part of any pair)
  keep <- is_group_b | (!is_group_a & !is_group_b)

  df[keep, ]
}

#############################################################
#####          Trapezoidal Peak Area Integration        #####
#############################################################
# Computes peak area by trapezoidal integration over ordered
# retention-time / intensity pairs.
#
# Args:
#   rtime     : numeric vector of retention times
#   intensity : numeric vector of signal intensities
#
# Returns:
#   numeric scalar -- the integrated peak area

trapz_area <- function(rtime, intensity) {
  id <- order(rtime)
  sum(diff(rtime[id]) * zoo::rollmean(intensity[id], 2))
}

#############################################################
#####        SIM Channel Processing Pipeline            #####
#############################################################
# Processes a single SIM m/z channel: filters by mass (using
# a +/- 0.5 Da window to capture centroid drift, e.g. m/z 46
# and 46.1), deinterleaves paired SIM scan groups (keeping
# only the second scan of each pair -- Group B), applies
# rolling average smoothing, baseline correction, filters to
# retention-time windows of interest, and runs peak detection.
#
# Deinterleaving rationale: many GC-MS SIM methods acquire
# two scan groups that both monitor the same ion. The groups
# fire in rapid succession (~54 ms apart), producing pairs of
# data points at systematically different intensity levels.
# This creates a sawtooth noise pattern. Keeping only the
# second scan (Group B) eliminates the sawtooth and reduces
# baseline noise by ~25-50%.
#
# Args:
#   raw_data         : data frame with Mass, RetentionTime,
#                      Intensity columns (full SIM file)
#   mz               : target m/z value (ions within +/- 0.5 Da
#                      are included)
#   rt_windows       : list of c(centre, half_width) pairs
#                      defining RT windows of interest
#   rolling_avg      : rolling average window size (default 3)
#   min_peak_height  : minimum peak height for ModPeaks()
#   rt_upper_limit   : upper RT cutoff for filtering (default 440)
#   pair_gap         : maximum time gap (s) between consecutive
#                      points to be treated as a paired scan
#                      (default 0.1 s)
#
# Returns:
#   list with:
#     baseline_corrected : full baseline-corrected data frame
#     signal             : filtered SignalMinusBg (RTime, Intensity)
#     peaks              : ModPeaks() result data frame
#     step_size          : RT step between adjacent data points

process_sim_channel <- function(raw_data, mz, rt_windows,
                                rolling_avg = 3, min_peak_height = 80,
                                rt_upper_limit = 440, pair_gap = 0.1) {

  # Filter to this m/z using a +/- 0.5 Da window to capture
  # centroid drift (e.g. m/z 46 vs 46.1 from different scan groups)
  channel_data <- raw_data %>%
    dplyr::filter(abs(Mass - mz) < 0.5) %>%
    dplyr::select(RetentionTime, Intensity) %>%
    dplyr::filter(RetentionTime < rt_upper_limit)

  # Guard: no data for this m/z channel
  if (nrow(channel_data) == 0) {
    warning("No data found for m/z ", mz, ". Returning empty results.")
    return(list(
      baseline_corrected = data.frame(RetentionTime = numeric(0),
                                       Intensity = numeric(0),
                                       CorrectedTrend = numeric(0),
                                       BaselineTrend = numeric(0),
                                       Subtracted = numeric(0)),
      signal = data.frame(RTime = numeric(0), Intensity = numeric(0)),
      peaks = data.frame(x = numeric(0), y = numeric(0), w = numeric(0)),
      step_size = NA_real_
    ))
  }

  # Deinterleave paired SIM scan groups: keep only Group B
  # (the second point of each closely-spaced pair)
  channel_data <- deinterleave_sim(channel_data, pair_gap = pair_gap)

  # Rolling average smoothing
  smoothed <- channel_data %>%
    dplyr::mutate(srate_ma02 = zoo::rollmean(Intensity, k = rolling_avg, fill = NA))

  channel_data <- smoothed %>%
    dplyr::select(RetentionTime, Intensity = srate_ma02)

  channel_data <- na.omit(channel_data)

  # Baseline correction
  baseline_corrected <- apply_baseline_correction(as.data.frame(channel_data), "Intensity")

  # Extract signal minus background
  signal <- baseline_corrected %>%
    dplyr::select(RetentionTime, Subtracted)
  names(signal)[1] <- "RTime"
  names(signal)[2] <- "Intensity"

  # Filter to RT windows of interest (if provided)
  if (length(rt_windows) > 0) {
    rt_filter <- rep(FALSE, nrow(signal))
    for (w in rt_windows) {
      rt_filter <- rt_filter | dplyr::between(signal$RTime, w[1] - w[2], w[1] + w[2])
    }
    signal <- signal[rt_filter, ]
  }

  # Peak detection
  if (nrow(signal) == 0) {
    signal_max <- -Inf
  } else {
    signal_max <- max(signal$Intensity, na.rm = TRUE)
  }

  if (signal_max >= min_peak_height) {
    peaks <- ModPeaks(signal, minPH = min_peak_height, minPW = 0.2)
  } else {
    peaks <- data.frame(x = numeric(0), y = numeric(0), w = numeric(0))
  }

  # Step size: use median of consecutive differences to be robust
  # against interleaved double-scan points
  if (nrow(channel_data) >= 3) {
    step_size <- median(diff(channel_data$RetentionTime))
  } else if (nrow(channel_data) >= 2) {
    step_size <- channel_data$RetentionTime[2] - channel_data$RetentionTime[1]
  } else {
    step_size <- NA_real_
  }

  list(
    baseline_corrected = baseline_corrected,
    signal = signal,
    peaks = peaks,
    step_size = step_size
  )
}

#############################################################
#####        Signal-to-Noise Ratio Calculation           #####
#############################################################
# Calculates the signal-to-noise ratio (SNR) for a detected
# peak using the peak height and the standard deviation of
# baseline noise in a user-defined signal-free region.
#
# Definition:  SNR = peak_height / sd(noise_region)
#
# Args:
#   baseline_corrected : data frame from process_sim_channel()
#                        or apply_baseline_correction(), must
#                        contain RetentionTime and Subtracted
#   peak_height        : height of the detected peak above
#                        baseline (from ModPeaks() y value)
#   noise_window       : numeric vector c(start_rt, end_rt)
#                        defining the RT range (s) for noise
#                        estimation
#   min_noise_points   : minimum data points required in the
#                        noise window for a reliable estimate
#                        (default 10)
#
# Returns:
#   numeric scalar -- the SNR value, or NA if the noise
#   region has insufficient data or zero variance

calculate_snr <- function(baseline_corrected, peak_height,
                          noise_window, min_noise_points = 10) {

  # Guard: missing inputs
  if (is.na(peak_height) || is.null(noise_window) ||
      length(noise_window) != 2) {
    return(NA_real_)
  }

  # Extract noise region from baseline-corrected signal
  noise_data <- baseline_corrected %>%
    dplyr::filter(
      RetentionTime >= noise_window[1] &
      RetentionTime <= noise_window[2]
    )

  if (nrow(noise_data) < min_noise_points) {
    warning("Noise window [", noise_window[1], ", ", noise_window[2],
            "] contains only ", nrow(noise_data),
            " points (minimum ", min_noise_points, " required).")
    return(NA_real_)
  }

  noise_sd <- sd(noise_data$Subtracted, na.rm = TRUE)

  if (is.na(noise_sd) || noise_sd < .Machine$double.eps) {
    warning("Noise standard deviation is zero or NA in window [",
            noise_window[1], ", ", noise_window[2], "].")
    return(NA_real_)
  }

  peak_height / noise_sd
}

#############################################################
#####            Peak Area Extraction                   #####
#############################################################
# Extracts the peak area for a single compound from detected
# peaks and baseline-corrected signal data. Identifies the
# peak closest to the expected retention time and integrates
# within a defined boundary. Optionally computes the signal-
# to-noise ratio (SNR) when baseline-corrected data and a
# noise window are provided.
#
# When snr_lod and snr_loq are supplied, the function applies
# an SNR-based acceptance filter:
#   - SNR < snr_lod  -> snr_flag = "Below_LOD",  pa set to NA
#   - SNR >= snr_lod but < snr_loq -> snr_flag = "Below_LOQ"
#   - SNR >= snr_loq -> snr_flag = "Quantifiable"
# If SNR cannot be computed (no noise window) or the thresholds
# are NA, snr_flag is NA (no filtering applied).
#
# Args:
#   peaks              : data frame from ModPeaks() (x, y, w)
#   signal             : baseline-corrected data (RTime, Intensity)
#   expected_rt        : expected retention time of the compound
#   step_size          : RT step between adjacent data points
#   rt_tolerance       : +/- tolerance for peak identification
#                        (default 2s)
#   range_limit        : max acceptable deviation from expected RT
#                        (default 15s)
#   boundary_steps     : number of step_size units for integration
#                        window (default 15)
#   baseline_corrected : (optional) full baseline-corrected data
#                        frame from process_sim_channel(), with
#                        RetentionTime and Subtracted columns.
#                        Required for SNR calculation.
#   noise_window       : (optional) numeric c(start_rt, end_rt)
#                        defining the noise region for SNR.
#                        Required for SNR calculation.
#   snr_lod            : (optional) minimum SNR for detection.
#                        Peaks below this are considered noise and
#                        their peak area is set to NA.
#   snr_loq            : (optional) minimum SNR for quantitation.
#                        Peaks between snr_lod and snr_loq are
#                        flagged as detected but not quantifiable.
#
# Returns:
#   list with:
#     rt       : retention time of detected peak (or NA)
#     pa       : integrated peak area (or NA if below LOD)
#     ph       : peak height above baseline (or NA)
#     snr      : signal-to-noise ratio (or NA)
#     snr_flag : "Below_LOD", "Below_LOQ", "Quantifiable", or NA

extract_peak_area <- function(peaks, signal, expected_rt, step_size,
                              rt_tolerance = 2, range_limit = 15,
                              boundary_steps = 15,
                              baseline_corrected = NULL,
                              noise_window = NULL,
                              snr_lod = NA_real_,
                              snr_loq = NA_real_) {

  na_result <- list(rt = NA_real_, pa = NA_real_,
                    ph = NA_real_, snr = NA_real_,
                    snr_flag = NA_character_)

  # If expected RT is not defined, cannot extract
  if (is.na(expected_rt)) {
    return(na_result)
  }

  # Filter peaks to expected RT window
  candidate <- peaks %>%
    dplyr::filter(x < (expected_rt + rt_tolerance) & x > (expected_rt - rt_tolerance))

  if (nrow(candidate) == 0) {
    return(na_result)
  }

  idx <- which.max(candidate$y)
  peak_rt <- candidate$x[idx]
  peak_height <- candidate$y[idx]

  # Validate range
  test_range <- abs(expected_rt - peak_rt)
  if (is.na(test_range) || test_range >= range_limit) {
    return(na_result)
  }

  # Set integration boundaries
  lmin <- peak_rt - (boundary_steps * step_size)
  lmax <- peak_rt + (boundary_steps * step_size)

  peak_data <- signal %>%
    dplyr::filter(dplyr::between(RTime, lmin, lmax))

  if (nrow(peak_data) < 2) {
    return(list(rt = peak_rt, pa = NA_real_,
                ph = peak_height, snr = NA_real_,
                snr_flag = NA_character_))
  }

  pa <- trapz_area(peak_data$RTime, peak_data$Intensity)

  # Compute SNR if baseline data and noise window are provided
  snr <- NA_real_
  if (!is.null(baseline_corrected) && !is.null(noise_window)) {
    snr <- calculate_snr(baseline_corrected, peak_height, noise_window)
  }

  # Apply SNR-based acceptance filter
  snr_flag <- NA_character_

  if (!is.na(snr) && !is.na(snr_lod) && !is.na(snr_loq)) {
    if (snr < snr_lod) {
      snr_flag <- "Below_LOD"
      pa <- NA_real_  # peak is indistinguishable from noise
    } else if (snr < snr_loq) {
      snr_flag <- "Below_LOQ"
      # pa is retained -- peak is real but imprecise
    } else {
      snr_flag <- "Quantifiable"
    }
  }

  list(rt = peak_rt, pa = pa, ph = peak_height, snr = snr, snr_flag = snr_flag)
}

# =========================================================
# Integration verification plot
# =========================================================
# Generates a zoomed plot around a detected peak showing:
#   - Baseline-corrected signal (grey) for context
#   - Shaded integration area (what trapz_area integrates)
#   - Detected peak RT (black dashed), expected RT (blue dotted)
#   - Integration boundaries (red solid)
#   - Annotation with peak area, height, SNR, flag
#
# Arguments:
#   baseline_corrected : data.frame with RetentionTime, Subtracted columns
#   signal             : data.frame with RTime, Intensity columns (baseline-subtracted, RT-windowed)
#   peak_result        : list from extract_peak_area (rt, pa, ph, snr, snr_flag)
#   expected_rt        : expected retention time (seconds)
#   step_size          : median RT step between data points
#   compound_name      : display name (e.g. "RDX IS", "PETN")
#   boundary_steps     : number of step_size units for integration window (default 15)
#   save_path          : directory to save the plot
#   filename           : output filename (without path)
#   ylim               : optional numeric vector c(ymin, ymax) for shared y-axis
#                         scaling across plots.  When NULL (default), ggplot2
#                         auto-scales to the data in each plot.
# =========================================================
plot_integration <- function(baseline_corrected, signal, peak_result,
                              expected_rt, step_size, compound_name,
                              boundary_steps = 15, save_path, filename,
                              noise_window = NULL,
                              ylim = NULL) {

  # Nothing to plot if no peak was detected
  if (is.na(peak_result$rt)) return(invisible(NULL))

  peak_rt <- peak_result$rt

  # Integration boundaries (same as extract_peak_area)
  lmin <- peak_rt - (boundary_steps * step_size)
  lmax <- peak_rt + (boundary_steps * step_size)

  # Plot range: 2x padding beyond integration window on each side
  plot_margin <- boundary_steps * step_size * 2
  plot_min <- peak_rt - (boundary_steps * step_size) - plot_margin
  plot_max <- peak_rt + (boundary_steps * step_size) + plot_margin

  # Extend plot range to include noise window if provided
  if (!is.null(noise_window) && length(noise_window) == 2) {
    plot_min <- min(plot_min, noise_window[1])
    plot_max <- max(plot_max, noise_window[2])
  }

  # Filter baseline-corrected data to plot range (context trace)
  context_data <- baseline_corrected %>%
    dplyr::filter(RetentionTime >= plot_min & RetentionTime <= plot_max)

  if (nrow(context_data) == 0) return(invisible(NULL))

  # Filter signal data to integration window (shaded area)
  integration_data <- signal %>%
    dplyr::filter(dplyr::between(RTime, lmin, lmax))

  # Build annotation text
  ann_parts <- paste0(compound_name)
  if (!is.na(peak_result$pa)) {
    ann_parts <- paste0(ann_parts, "  |  PA: ", formatC(peak_result$pa, format = "f", digits = 0))
  }
  if (!is.na(peak_result$ph)) {
    ann_parts <- paste0(ann_parts, "  |  PH: ", formatC(peak_result$ph, format = "f", digits = 0))
  }
  if (!is.na(peak_result$snr)) {
    ann_parts <- paste0(ann_parts, "  |  SNR: ", formatC(peak_result$snr, format = "f", digits = 1))
  }
  if (!is.na(peak_result$snr_flag)) {
    ann_parts <- paste0(ann_parts, "  |  ", peak_result$snr_flag)
  }

  # Determine annotation y-positions
  # When a shared ylim is provided, use it for consistent annotation placement;
  # otherwise fall back to the per-plot data range.
  if (!is.null(ylim)) {
    ann_y_top    <- ylim[2] * 0.95
    ann_y_bottom <- ylim[1]
  } else {
    ann_y_top    <- max(context_data$Subtracted, na.rm = TRUE) * 0.95
    ann_y_bottom <- min(context_data$Subtracted, na.rm = TRUE)
  }

  # Build the plot
  p <- ggplot() +
    geom_line(data = context_data,
              aes(x = RetentionTime, y = Subtracted),
              colour = "grey50", linewidth = 0.4) +
    geom_area(data = integration_data,
              aes(x = RTime, y = Intensity),
              fill = "#1f77b4", alpha = 0.3) +
    geom_line(data = integration_data,
              aes(x = RTime, y = Intensity),
              colour = "#1f77b4", linewidth = 0.6) +
    geom_vline(xintercept = lmin, colour = "red", linetype = "solid", linewidth = 0.5) +
    geom_vline(xintercept = lmax, colour = "red", linetype = "solid", linewidth = 0.5) +
    geom_vline(xintercept = peak_rt, colour = "black", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = expected_rt, colour = "blue", linetype = "dotted", linewidth = 0.5)

  if (!is.null(noise_window) && length(noise_window) == 2) {
    p <- p +
      geom_vline(xintercept = noise_window[1], colour = "orange", linetype = "dashed", linewidth = 0.5) +
      geom_vline(xintercept = noise_window[2], colour = "orange", linetype = "dashed", linewidth = 0.5)
  }

  p <- p +
    annotate("text", x = plot_min + (plot_max - plot_min) * 0.02,
             y = ann_y_top,
             label = ann_parts, hjust = 0, size = 3) +
    annotate("text", x = peak_rt, y = ann_y_bottom,
             label = sprintf(" RT: %.1fs", peak_rt), hjust = 0, vjust = 1, size = 2.5) +
    annotate("text", x = expected_rt, y = ann_y_bottom,
             label = sprintf(" Exp: %.1fs", expected_rt), hjust = 0, vjust = 0, size = 2.5,
             colour = "blue") +
    theme_bw() +
    labs(
      title = paste0("Integration Check: ", compound_name),
      x = "Retention Time (s)",
      y = "Baseline-Corrected Intensity"
    )

  # Apply shared y-axis limits if provided
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  ggsave(
    filename,
    plot = p,
    device = NULL,
    path = save_path,
    scale = 1,
    width = 7.5,
    height = 5.0,
    units = "in",
    dpi = 300,
    limitsize = TRUE
  )

  invisible(NULL)
}
