#
#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# The MS1 files needs to be converted using the MsFilesReoganiser.R code

# *********************************************************************
# CONSOLIDATED VERSION: Uses only 15N-RDX as internal standard (no NG)
# This version extracts:
#   - RDX IS (15N-RDX) peak area from m/z 122 channel
#   - Analyte peaks (PETN m/z 46, RDX m/z 120)
# *********************************************************************

#####################################################################
# Set to TRUE to reprocess all files from scratch;
# set to FALSE to skip files whose Summary output already exists
reprocess_tic <- FALSE
reprocess_sim <- FALSE
#####################################################################

# Load Metadata files
filenameGcData <- list.files(GcDataConvertedRcodeTIC.dir, extensionCSV, full.names=TRUE)

# Report how many TIC files will be processed vs skipped
if (!reprocess_tic) {
  n_tic_total <- length(filenameGcData)
  n_tic_skip <- sum(sapply(filenameGcData, function(f) {
    name <- gsub(".*/", "", gsub(extensionCSV, "", f))
    file.exists(paste0(GcData.dir, name, "TIC.csv"))
  }))
  print(paste0("TIC processing: ", n_tic_total, " files found, ",
               n_tic_skip, " already processed, ", n_tic_total - n_tic_skip, " to process"))
}

for (file in filenameGcData) {
  GcDataName <- gsub(extensionCSV, "", file)
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  # GcDataName <- gsub(extensionCSV, "", filenameGcData[3])
  
  GcDataName <- gsub(".*/", "", GcDataName)
  File <- GcDataName
  
  # Skip if already processed (TIC file exists) and not reprocessing TIC
  if (!reprocess_tic && file.exists(paste0(GcData.dir, GcDataName, "TIC.csv"))) {
    print(paste0("Skipping ", GcDataName, " TIC -- already processed"))
    next
  }
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  #GcDataCodeOrdered <- read.csv2(filenameGcData[14], sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- read.csv2(file, sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- GcDataCodeOrdered %>% mutate(across(everything(), ~as.numeric(as.character(.x))))
  
  #########################################################
  #######      Calculate PA for TIC                 #######
  #########################################################
  
  DataTIC <- GcDataCodeOrdered
  
  # De-interleave the TIC channel BEFORE any smoothing/baseline correction
  # (added Aug 2026 -- see CONTEXT.md "Blank Other-Significant-Peak
  # Detection" session for the full investigation). This instrument's
  # "SIM/Scan" simultaneous acquisition mode alternates every raw TIC
  # datapoint between a SIM-cycle total (sum of only the few currently
  # time-windowed-monitored analyte/IS ions) and a Scan-cycle total (sum
  # across the full m/z 41-200 survey sweep) -- two genuinely different
  # signals, not a duplicate/redundant read of the same one. Rolling-
  # averaging and baseline-correcting the raw alternating mix smeared
  # real chromatographic peaks into an apparent sawtooth "noise" floor
  # throughout the WHOLE run (confirmed directly: real, cleanly-resolved
  # peaks visually confirmed on ASTRA Analysis5 were statistically
  # indistinguishable from noise until this fix). deinterleave_tic()
  # keeps the HIGHER-value point of each interleaved pair (a robust
  # proxy for "this is the Scan-cycle point") -- the only channel that
  # can ever show a genuine compound not among the specifically-
  # monitored SIM ions, which is exactly what find_other_tic_peaks()
  # below needs. This is the OPPOSITE selection rule from
  # deinterleave_sim() (used for SIM channels, where both interleaved
  # points genuinely measure the SAME m/z and the SECOND is correctly
  # kept) -- do not swap these two functions' usage.
  DataTIC <- deinterleave_tic(DataTIC, value_col = "TIC", pair_gap = 0.1)
  
  # rolling average across the data, value set in Global
  smoothDataTIC <- DataTIC %>%
    mutate(srate_ma02 = rollmean(TIC, k = rolling.average, fill = NA))
  
  DataTIC <- smoothDataTIC %>%
    select(RetentionTime, TIC=srate_ma02)
  
  DataTIC <- DataTIC %>%
    filter(RetentionTime > 120)  # lowered from 180
  
  DataTIC <- na.omit(DataTIC)
  
  ########################################################
  #######         baseline corrections               #######
  ########################################################
  
  Combined.bc.fillPeakTIC <- apply_baseline_correction(as.data.frame(DataTIC), "TIC")
  
  p <- ggplot(Combined.bc.fillPeakTIC, aes(x=RetentionTime)) +
    geom_line(aes(y = TIC, colour = "GC trace")) +
    geom_line(aes(y = BaselineTrend, colour = "baseline")) +
    scale_colour_manual(values = pal_trace_baseline) +
    labs(
      x = "Retention Time (s)",
      y = "TIC")
  show (p)
  
  # When running single file don't run line below (skip to ggsave)
  #  GcDataName <- gsub('.{4}$', '', GcDataName)
  ggsave(
    sprintf("%s_TICBaseline.tiff",GcDataName),
    plot = p,
    device = NULL,
    path = file.path(GCSampleTrace.dir, "TIC"),
    scale = 1,
    width = 7.5,
    height = 5.0,
    units = c("in"),
    dpi = 300,
    limitsize = TRUE
  )
  
  # as an alternative to HyperSpec curve fitting 
  SignalMinusBgTIC <- Combined.bc.fillPeakTIC %>%
    select(RetentionTime,Subtracted)
  
  names(SignalMinusBgTIC)[1] <- "RTime"
  names(SignalMinusBgTIC)[2] <- "TIC"
  
  SignalMaxTIC <- max(SignalMinusBgTIC$TIC)
  
  # put a minimum threshold for peak detection
  # below this limit, there will be no peak and output with zeros
  # will still be created to avoid errors
  
  if (SignalMaxTIC >= SignalMaxThresholdTIC) {
    # determine the position of the peaks
    QTIC <- ModPeaks(SignalMinusBgTIC,minPH=SignalMaxThresholdTIC, minPW=0.2)
    
  } else{
    QTIC <- data.frame (x = c(0),
                        y = c(0),
                        w = c(0))
  }
  
  names(QTIC)[1] <- "RTime"
  names(QTIC)[2] <- "TIC"
  QTIC <- QTIC%>%
    select(RTime,TIC)
  
  # -------------------------------------------------------------------
  # "Other significant peak" detection (added Aug 2026) -- see
  # CONTEXT.md "Blank Other-Significant-Peak Detection" session, and
  # find_other_tic_peaks() in Code/ModPeaks.R for the full design notes.
  # Purely additive/diagnostic: scans the same baseline-corrected TIC
  # trace (Combined.bc.fillPeakTIC, already computed above) for peaks
  # NOT explained by either analyte's own RT, using a much lower,
  # noise-relative threshold than SignalMaxThresholdTIC above (which is
  # tuned for gross artefacts only, not blank-level significance).
  # Computed for every injection type (cheap -- reuses the already
  # baseline-corrected signal), but only acted upon downstream for
  # Blank-type rows (Code/InjectionAcceptance.R's evaluate_blanks()).
  # Does NOT affect SignalMaxTIC/QTIC/the existing RTimeN/TICN columns
  # above in any way.
  other_peak <- find_other_tic_peaks(
    Combined.bc.fillPeakTIC,
    analyte_rts = c(analytes$PETN$rt, analytes$RDX$rt),
    front_edge_clear = tic_quiet_front_edge_clear,
    rt_buffer = tic_other_peak_rt_buffer,
    min_height = tic_other_peak_min_height
  )
  
  if (nrow(QTIC) > 0){
    # Initialize an empty list to store the new columns
    columns <- list()
    
    # Loop through each row of the data frame
    for (i in 1:nrow(QTIC)) {
      rtime <- QTIC[i, 1]  # Assuming RTime is in column 1
      tic <- QTIC[i, 2]    # Assuming TIC is in column 2
      
      columns[[paste0("RTime", i)]] <- rtime
      columns[[paste0("TIC", i)]] <- tic
    }
    
    # Convert the list to a data frame (single row with multiple columns)
    columns <- na.omit(columns)

    # Append "other significant peak" columns (see above) -- kept
    # separate from the na.omit() step above (untouched, existing
    # behaviour) since these are not subject to the same NA handling.
    # count is the TRUE total found; rtN/heightN report up to
    # tic_other_peak_max_report of them (largest first) so a
    # recurring-but-secondary peak isn't masked by a single stronger
    # one-off peak in the same injection -- see CONTEXT.md "Blank
    # Other-Significant-Peak Detection: Recurring Peak Tracking".
    columns[["tic_other_peak_count"]] <- other_peak$count
    for (j in seq_len(tic_other_peak_max_report)) {
      if (j <= nrow(other_peak$peaks)) {
        columns[[paste0("tic_other_peak_rt", j)]]     <- other_peak$peaks$rt[j]
        columns[[paste0("tic_other_peak_height", j)]] <- other_peak$peaks$height[j]
      } else {
        columns[[paste0("tic_other_peak_rt", j)]]     <- NA_real_
        columns[[paste0("tic_other_peak_height", j)]] <- NA_real_
      }
    }

    TICPeaks <- as.data.frame(columns)
    write.table(TICPeaks, file=paste0(GcData.dir,GcDataName, "TIC.csv"), sep = ",", row.names = FALSE) 
  }
  
  print(paste0(file, " TIC processed"))
}
#########################################################
#######      Calculate PA for SIM                 #######
#########################################################

# Build a list of "channels" to process. Each channel has a unique key,
# an m/z, a set of RT windows, and a minimum peak height (added Aug 2026
# -- see CONTEXT.md "Confirmatory Ion Detection" session). Historically
# there was exactly one channel per unique m/z (quantifier/IS roles never
# shared a m/z). Qualifier (confirmatory) ions break that assumption --
# e.g. m/z 46 is BOTH PETN's own quantifier ion AND RDX's qualifier ion
# (confirmed in the real acqmeth.txt SIM group ions) -- so qualifier
# channels are ALWAYS given their own dedicated, role-specific key
# (never reusing/merging with a quantifier/IS channel's key), each with
# its OWN RT window and its OWN (deliberately lenient) detection floor.
# This is essential: process_sim_channel()/ModPeaks() apply a single
# peak-height floor to whatever they're given, so sharing one call
# between a strict quantifier role (needs a high floor to reject blank
# noise, see MinPeakHeight_PETN's validation notes above) and a lenient
# qualifier role (needs a low floor to detect a weaker fragment ion)
# would silently weaken the quantifier's own noise rejection everywhere
# in the channel -- exactly the kind of regression a per-role dedicated
# channel avoids entirely, at the cost of (harmless) duplicate
# baseline-correction work for the one shared m/z.
channel_specs <- list()
add_channel <- function(key, mz, rt, height, make_plots = TRUE) {
  channel_specs[[key]] <<- list(mz = mz, windows = list(c(rt, 5)), height = height,
                                 make_plots = make_plots)
}

# Quantifier/IS roles -- key = plain m/z (unchanged from prior behaviour).
# Diagnostic plots (baseline-correction TIFF + integration TIFF) are kept
# for these, as before.
add_channel(as.character(rdx_is_config$mz), rdx_is_config$mz,
            rdx_is_config$rt, rdx_is_config$min_peak_height)
for (name in names(analytes)) {
  a <- analytes[[name]]
  add_channel(as.character(a$mz), a$mz, a$rt, a$min_peak_height)
}

# Qualifier (confirmatory) roles -- key = "<analyte>_qual<mz>", always
# its own dedicated channel regardless of whether the m/z collides with
# a quantifier/IS role above. `make_plots = FALSE`: per-injection
# diagnostic TIFF plots (baseline-correction + integration) are
# deliberately NOT generated for qualifier channels. Numeric extraction
# (PA/PH/SNR -- the only thing the confirmatory-ion analysis actually
# needs) still happens unconditionally below. This was found necessary
# during real-data verification: with ~90 injections/dataset, uncompressed
# TIFF plots for 4 qualifier channels + their integration plots consumed
# ~10 GB per dataset (verified on Lab9), repeatedly exhausting disk space
# during batch reprocessing -- see CONTEXT.md "Confirmatory Ion Detection"
# session for the full incident. If per-injection visual QA of a specific
# qualifier peak is ever needed, it can be regenerated ad hoc for that one
# injection/channel rather than for the whole study by default.
for (name in names(analytes)) {
  a <- analytes[[name]]
  if (is.null(a$qualifiers) || length(a$qualifiers) == 0) next
  for (qmz in a$qualifiers) {
    role_key <- paste0(tolower(name), "_qual", qmz)
    add_channel(role_key, qmz, a$rt, MinPeakHeight_Qualifier, make_plots = FALSE)
  }
}

channel_keys <- names(channel_specs)

# Create SIM subfolders only for channels that will actually generate plots
# (qualifier channels have make_plots = FALSE -- see above -- so no
# subfolder is created for them at all)
for (key in channel_keys) {
  if (isTRUE(channel_specs[[key]]$make_plots)) {
    dir.create(file.path(GCSampleTrace.dir, sprintf("SIM_mz%s", key)), recursive = TRUE)
  }
}

# --- Integration plot queue for shared y-axis scaling ---
# Instead of generating integration plots inside the SIM loop (where
# Integration plots are generated inline during SIM processing (auto-scaled per plot)

filenameGcData <- list.files(GcDataConvertedRcodeSIM.dir, extensionCSV, full.names = TRUE)

# Report how many SIM files will be processed vs skipped
if (!reprocess_sim) {
  n_sim_total <- length(filenameGcData)
  n_sim_skip <- sum(sapply(filenameGcData, function(f) {
    name <- gsub(".*/", "", gsub(extensionCSV, "", f))
    file.exists(paste0(GcData.dir, name, "SIM.csv"))
  }))
  print(paste0("SIM processing: ", n_sim_total, " files found, ",
               n_sim_skip, " already processed, ", n_sim_total - n_sim_skip, " to process"))
}

for (file in filenameGcData) {
  GcDataName <- gsub(extensionCSV, "", file)
  GcDataName <- gsub(".*/", "", GcDataName)
  File <- GcDataName

  # Skip if already processed (SIM file exists) and not reprocessing SIM
  if (!reprocess_sim && file.exists(paste0(GcData.dir, GcDataName, "SIM.csv"))) {
    print(paste0("Skipping ", GcDataName, " SIM -- already processed"))
    next
  }

  GcDataCodeOrdered <- read.csv2(file, sep = ",", header = TRUE)
  GcDataCodeOrdered <- GcDataCodeOrdered %>% mutate(across(everything(), ~as.numeric(as.character(.x))))

  # Process each channel (one process_sim_channel() call per channel key,
  # NOT per unique m/z -- see channel_specs comment above)
  channel_results <- list()
  for (key in channel_keys) {
    spec <- channel_specs[[key]]
    channel_results[[key]] <- process_sim_channel(
      GcDataCodeOrdered, spec$mz,
      spec$windows,
      rolling_avg = rolling.average,
      min_peak_height = spec$height
    )
  }

  # Plot each channel (skip qualifier channels -- make_plots = FALSE)
  for (key in channel_keys) {
    if (!isTRUE(channel_specs[[key]]$make_plots)) next
    ch <- channel_results[[key]]
    p <- ggplot(ch$baseline_corrected, aes(x = RetentionTime, y = Subtracted)) +
      geom_line(color = "red") +
      labs(
        title = paste0("Baseline-Corrected SIM m/z ", channel_specs[[key]]$mz, " (", key, ")"),
        x = "Retention Time (s)",
        y = "Corrected Intensity"
      ) +
      theme_minimal()
    print(p)
    ggsave(
      sprintf("%s_SIM_mz%s.tiff", GcDataName, key),
      plot = p,
      device = NULL,
      path = file.path(GCSampleTrace.dir, sprintf("SIM_mz%s", key)),
      scale = 1,
      width = 7.5,
      height = 5.0,
      units = c("in"),
      dpi = 300,
      limitsize = TRUE
    )
  }

  # Extract RDX IS (15N-RDX) peak area from m/z 122 channel
  rdx_is_ch <- channel_results[[as.character(rdx_is_config$mz)]]
  rdx_is_result <- extract_peak_area(rdx_is_ch$peaks, rdx_is_ch$signal,
                                      rdx_is_config$rt, rdx_is_ch$step_size,
                                      baseline_corrected = rdx_is_ch$baseline_corrected,
                                      noise_window = rdx_is_config$noise_window,
                                      snr_lod = snr_detect,
                                      snr_loq = snr_quant)

  # Generate RDX IS integration plot (auto-scaled)
  print(paste0("  RDX IS noise_window: ", 
               ifelse(is.null(rdx_is_config$noise_window), "NULL",
                      paste(rdx_is_config$noise_window, collapse = "-"))))
  plot_integration(
    baseline_corrected = rdx_is_ch$baseline_corrected,
    signal             = rdx_is_ch$signal,
    peak_result        = rdx_is_result,
    expected_rt        = rdx_is_config$rt,
    step_size          = rdx_is_ch$step_size,
    compound_name      = "RDX IS (15N-RDX, m/z 122)",
    save_path          = file.path(GCSampleTrace.dir, "Integration"),
    filename           = sprintf("%s_Integration_RDX_IS.tiff", GcDataName),
    noise_window       = rdx_is_config$noise_window
  )

  # Extract each analyte's peak area from its own m/z channel
  analyte_results <- list()
  for (name in names(analytes)) {
    a <- analytes[[name]]
    a_ch <- channel_results[[as.character(a$mz)]]
    analyte_results[[name]] <- extract_peak_area(
      a_ch$peaks, a_ch$signal, a$rt, a_ch$step_size,
      baseline_corrected = a_ch$baseline_corrected,
      noise_window = a$noise_window,
      snr_lod = snr_detect,
      snr_loq = snr_quant
    )

    # Generate analyte integration plot (auto-scaled)
    print(paste0("  ", name, " noise_window: ", 
                 ifelse(is.null(a$noise_window), "NULL",
                        paste(a$noise_window, collapse = "-"))))
    plot_integration(
      baseline_corrected = a_ch$baseline_corrected,
      signal             = a_ch$signal,
      peak_result        = analyte_results[[name]],
      expected_rt        = a$rt,
      step_size          = a_ch$step_size,
      compound_name      = paste0(name, " (m/z ", a$mz, ")"),
      save_path          = file.path(GCSampleTrace.dir, "Integration"),
      filename           = sprintf("%s_Integration_%s.tiff", GcDataName, name),
      noise_window       = a$noise_window
    )
  }

  # Extract each analyte's qualifier (confirmatory) ion peak area, at the
  # SAME expected RT/noise window as that analyte's own quantifier (added
  # Aug 2026 -- qualifiers coelute with the quantifier peak, same SIM
  # group). Not used for quantification; retained purely for identity
  # confirmation (ion ratio vs the quantifier, computed downstream in
  # 03_Quantification.R / InjectionAcceptance.R).
  analyte_qualifier_results <- list()
  for (name in names(analytes)) {
    a <- analytes[[name]]
    if (is.null(a$qualifiers) || length(a$qualifiers) == 0) next
    analyte_qualifier_results[[name]] <- list()
    for (qmz in a$qualifiers) {
      q_key <- paste0(tolower(name), "_qual", qmz)
      q_ch <- channel_results[[q_key]]
      q_result <- extract_peak_area(
        q_ch$peaks, q_ch$signal, a$rt, q_ch$step_size,
        baseline_corrected = q_ch$baseline_corrected,
        noise_window = a$noise_window,
        snr_lod = snr_detect,
        snr_loq = snr_quant
      )
      analyte_qualifier_results[[name]][[as.character(qmz)]] <- q_result

      # NOTE (Aug 2026): no per-injection integration plot is generated
      # here, deliberately -- see the make_plots = FALSE comment above
      # channel_specs. Only the numeric PA/PH/SNR extraction (above) is
      # needed for the confirmatory-ion ratio analysis.
    }
  }

  # Build Results data frame dynamically
  Results <- data.frame(
    File = File,
    rdx_is_rt       = rdx_is_result$rt,
    rdx_is_pa       = rdx_is_result$pa,
    rdx_is_ph       = rdx_is_result$ph,
    rdx_is_snr      = rdx_is_result$snr,
    rdx_is_snr_flag = rdx_is_result$snr_flag
  )
  for (name in names(analytes)) {
    prefix <- tolower(name)
    Results[[paste0(prefix, "_rt")]]       <- analyte_results[[name]]$rt
    Results[[paste0(prefix, "_pa")]]       <- analyte_results[[name]]$pa
    Results[[paste0(prefix, "_ph")]]       <- analyte_results[[name]]$ph
    Results[[paste0(prefix, "_snr")]]      <- analyte_results[[name]]$snr
    Results[[paste0(prefix, "_snr_flag")]] <- analyte_results[[name]]$snr_flag

    # Qualifier (confirmatory) ion columns (added Aug 2026)
    a <- analytes[[name]]
    if (!is.null(a$qualifiers) && length(a$qualifiers) > 0) {
      for (qmz in a$qualifiers) {
        qres <- analyte_qualifier_results[[name]][[as.character(qmz)]]
        qprefix <- paste0(prefix, "_qual", qmz)
        Results[[paste0(qprefix, "_rt")]]       <- qres$rt
        Results[[paste0(qprefix, "_pa")]]       <- qres$pa
        Results[[paste0(qprefix, "_ph")]]       <- qres$ph
        Results[[paste0(qprefix, "_snr")]]      <- qres$snr
        Results[[paste0(qprefix, "_snr_flag")]] <- qres$snr_flag
      }
    }
  }

  write.table(Results, file = paste0(GcData.dir, GcDataName, "SIM.csv"), sep = ",", row.names = FALSE)
  print(paste0(file, " SIM processed"))
}

# Integration plots generated inline during SIM processing (auto-scaled per plot)


TICFile <- list.files(path = GcData.dir, pattern = "\\TIC.csv$")
SIMFile <- list.files(path = GcData.dir, pattern = "\\SIM.csv$")
NumInj <- length(TICFile)

# Extract sample name prefixes by stripping the TIC/SIM suffix
tic_prefixes <- sub("TIC\\.csv$", "", TICFile)
sim_prefixes <- sub("SIM\\.csv$", "", SIMFile)

# Report how many Summary files will be merged vs skipped
# Summary is regenerated if either TIC or SIM was reprocessed
reprocess_summary <- reprocess_tic || reprocess_sim
if (!reprocess_summary) {
  n_sum_skip <- sum(sapply(tic_prefixes, function(p) {
    file.exists(paste0(GcData.dir, p, "Summary.csv"))
  }))
  print(paste0("Summary merge: ", NumInj, " files found, ",
               n_sum_skip, " already processed, ", NumInj - n_sum_skip, " to merge"))
}

for (i in seq_len(NumInj)) {
  sample_prefix <- tic_prefixes[i]
  
  # Skip if already processed and not reprocessing
  if (!reprocess_summary && file.exists(paste0(GcData.dir, sample_prefix, "Summary.csv"))) {
    print(paste0("Skipping ", sample_prefix, " Summary -- already processed"))
    next
  }
  
  # Match this TIC file to its corresponding SIM file by name prefix
  sim_match <- which(sim_prefixes == sample_prefix)
  
  if (length(sim_match) == 0) {
    warning("No matching SIM file for TIC prefix: ", sample_prefix, " -- skipping.")
    next
  }
  
  TICData <- read.csv2(paste0(GcData.dir, TICFile[i]), sep = ",") 
  SIMData <- read.csv2(paste0(GcData.dir, SIMFile[sim_match[1]]), sep = ",")
  SummaryData <- cbind(SIMData, TICData)

  # Convert columns to numeric where appropriate, but preserve
  # character flag columns (e.g. snr_flag) that cannot be coerced
  SummaryData[] <- lapply(SummaryData, function(col) {
    num <- suppressWarnings(as.numeric(as.character(col)))
    if (all(is.na(num) == is.na(col)) || all(is.na(num) & nchar(trimws(as.character(col))) == 0)) {
      return(num)
    }
    # Column contains non-numeric values -- keep as character
    return(as.character(col))
  })
  
  write.table(SummaryData, file = paste0(GcData.dir, sample_prefix, "Summary.csv"), sep = ",", row.names = FALSE)
  print(paste0(sample_prefix, " Summary processed"))
}

