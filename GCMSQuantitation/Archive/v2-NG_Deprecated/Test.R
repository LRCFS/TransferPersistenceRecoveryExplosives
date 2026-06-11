#
#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# The MS1 files needs to be converted using the MsFilesReoganiser.R code

#####################################################################
# Set to TRUE to reprocess all files from scratch;
# set to FALSE to skip files whose Summary output already exists
reprocess_tic <- FALSE
reprocess_sim <- TRUE
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
  
  # rolling average across the data, value set in Global
  smoothDataTIC <- DataTIC %>%
    mutate(srate_ma02 = rollmean(TIC, k = rolling.average, fill = NA))
  
  DataTIC <- smoothDataTIC %>%
    select(RetentionTime, TIC=srate_ma02)
  
  DataTIC <- DataTIC %>%
    filter(RetentionTime > 120)  # lowered from 180 to include NG region (~138s)
  
  DataTIC <- na.omit(DataTIC)
  
  ########################################################
  #######         baseline corrections               #######
  ########################################################
  
  Combined.bc.fillPeakTIC <- apply_baseline_correction(as.data.frame(DataTIC), "TIC")
  
  p <- ggplot(Combined.bc.fillPeakTIC, aes(x=RetentionTime)) +
    geom_line(aes(y = TIC, colour = "GC trace")) +
    geom_line(aes(y = BaselineTrend, colour = "baseline")) +   
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
    TICPeaks <- as.data.frame(columns)
    write.table(TICPeaks, file=paste0(GcData.dir,GcDataName, "TIC.csv"), sep = ",", row.names = FALSE) 
  }
  
  print(paste0(file, " TIC processed"))
}
#########################################################
#######      Calculate PA for SIM                 #######
#########################################################

# Collect all unique m/z values needed across both IS and analytes
all_mz <- unique(c(rdx_is_config$mz, petn_is_config$mz, sapply(analytes, function(a) a$mz)))

# Create SIM subfolders for each m/z channel
for (mz_val in all_mz) {
  dir.create(file.path(GCSampleTrace.dir, sprintf("SIM_mz%s", mz_val)), recursive = TRUE)
}

# Build RT windows for each m/z channel
# Each window is c(centre, half_width) -- determines which RT regions
# are kept after baseline correction for peak detection
rt_windows_by_mz <- list()
for (mz_val in all_mz) {
  windows <- list()
  if (rdx_is_config$mz == mz_val && !is.na(rdx_is_config$rt)) {
    windows <- c(windows, list(c(rdx_is_config$rt, 5)))
  }
  if (petn_is_config$mz == mz_val && !is.na(petn_is_config$rt)) {
    windows <- c(windows, list(c(petn_is_config$rt, 5)))
  }
  for (a in analytes) {
    if (a$mz == mz_val && !is.na(a$rt)) {
      windows <- c(windows, list(c(a$rt, 5)))
    }
  }
  rt_windows_by_mz[[as.character(mz_val)]] <- windows
}

# --- Integration plot queue for shared y-axis scaling ---
# Instead of generating integration plots inside the SIM loop (where
# only the current file's data is available), we queue the plot data
# and track the maximum y-value per compound across all files.  After
# the loop, plots are generated with a shared y-axis per compound so
# that visual comparison across injections is meaningful.
integration_plot_queue <- list()
y_max_by_compound     <- list()

# Helper: compute the max baseline-corrected intensity within the
# plot range that plot_integration() would use for a given peak.
compute_plot_range_ymax <- function(baseline_corrected, peak_rt, step_size,
                                     boundary_steps = 15) {
  if (is.na(peak_rt)) return(NA_real_)
  plot_margin <- boundary_steps * step_size * 2
  plot_min <- peak_rt - (boundary_steps * step_size) - plot_margin
  plot_max <- peak_rt + (boundary_steps * step_size) + plot_margin
  vals <- baseline_corrected$Subtracted[
    baseline_corrected$RetentionTime >= plot_min &
    baseline_corrected$RetentionTime <= plot_max
  ]
  if (length(vals) == 0) return(NA_real_)
  max(vals, na.rm = TRUE)
}

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

  # Process each m/z channel
  channel_results <- list()
  for (mz_val in all_mz) {
    channel_results[[as.character(mz_val)]] <- process_sim_channel(
      GcDataCodeOrdered, mz_val,
      rt_windows_by_mz[[as.character(mz_val)]],
      rolling_avg = rolling.average,
      min_peak_height = SignalMaxThresholdSIM
    )
  }

  # Plot each m/z channel
  for (mz_val in all_mz) {
    ch <- channel_results[[as.character(mz_val)]]
    p <- ggplot(ch$baseline_corrected, aes(x = RetentionTime, y = Subtracted)) +
      geom_line(color = "red") +
      labs(
        title = paste0("Baseline-Corrected SIM m/z ", mz_val),
        x = "Retention Time (s)",
        y = "Corrected Intensity"
      ) +
      theme_minimal()
    print(p)
    ggsave(
      sprintf("%s_SIM_mz%s.tiff", GcDataName, mz_val),
      plot = p,
      device = NULL,
      path = file.path(GCSampleTrace.dir, sprintf("SIM_mz%s", mz_val)),
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

  # Queue RDX IS integration plot (deferred for shared y-axis scaling)
  rdx_is_key <- "RDX_IS"
  rdx_is_ymax <- compute_plot_range_ymax(rdx_is_ch$baseline_corrected,
                                          rdx_is_result$rt, rdx_is_ch$step_size)
  if (!is.na(rdx_is_ymax)) {
    y_max_by_compound[[rdx_is_key]] <- max(y_max_by_compound[[rdx_is_key]] %||% 0, rdx_is_ymax)
  }
  integration_plot_queue[[length(integration_plot_queue) + 1]] <- list(
    baseline_corrected = rdx_is_ch$baseline_corrected,
    signal             = rdx_is_ch$signal,
    peak_result        = rdx_is_result,
    expected_rt        = rdx_is_config$rt,
    step_size          = rdx_is_ch$step_size,
    compound_name      = "RDX IS (15N-RDX, m/z 122)",
    save_path          = file.path(GCSampleTrace.dir, "Integration"),
    filename           = sprintf("%s_Integration_RDX_IS.tiff", GcDataName),
    compound_key       = rdx_is_key
  )

  # Extract PETN IS (NG) peak area from m/z 76 channel
  petn_is_ch <- channel_results[[as.character(petn_is_config$mz)]]
  petn_is_result <- extract_peak_area(petn_is_ch$peaks, petn_is_ch$signal,
                                       petn_is_config$rt, petn_is_ch$step_size,
                                       baseline_corrected = petn_is_ch$baseline_corrected,
                                       noise_window = petn_is_config$noise_window,
                                       snr_lod = snr_detect,
                                       snr_loq = snr_quant)

  # Queue PETN IS integration plot (deferred for shared y-axis scaling)
  petn_is_key <- "PETN_IS"
  petn_is_ymax <- compute_plot_range_ymax(petn_is_ch$baseline_corrected,
                                           petn_is_result$rt, petn_is_ch$step_size)
  if (!is.na(petn_is_ymax)) {
    y_max_by_compound[[petn_is_key]] <- max(y_max_by_compound[[petn_is_key]] %||% 0, petn_is_ymax)
  }
  integration_plot_queue[[length(integration_plot_queue) + 1]] <- list(
    baseline_corrected = petn_is_ch$baseline_corrected,
    signal             = petn_is_ch$signal,
    peak_result        = petn_is_result,
    expected_rt        = petn_is_config$rt,
    step_size          = petn_is_ch$step_size,
    compound_name      = "PETN IS (NG, m/z 76)",
    save_path          = file.path(GCSampleTrace.dir, "Integration"),
    filename           = sprintf("%s_Integration_PETN_IS.tiff", GcDataName),
    compound_key       = petn_is_key
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

    # Queue analyte integration plot (deferred for shared y-axis scaling)
    analyte_key <- name
    analyte_ymax <- compute_plot_range_ymax(a_ch$baseline_corrected,
                                             analyte_results[[name]]$rt, a_ch$step_size)
    if (!is.na(analyte_ymax)) {
      y_max_by_compound[[analyte_key]] <- max(y_max_by_compound[[analyte_key]] %||% 0, analyte_ymax)
    }
    integration_plot_queue[[length(integration_plot_queue) + 1]] <- list(
      baseline_corrected = a_ch$baseline_corrected,
      signal             = a_ch$signal,
      peak_result        = analyte_results[[name]],
      expected_rt        = a$rt,
      step_size          = a_ch$step_size,
      compound_name      = paste0(name, " (m/z ", a$mz, ")"),
      save_path          = file.path(GCSampleTrace.dir, "Integration"),
      filename           = sprintf("%s_Integration_%s.tiff", GcDataName, name),
      compound_key       = analyte_key
    )
  }

  # Build Results data frame dynamically
  Results <- data.frame(
    File = File,
    rdx_is_rt       = rdx_is_result$rt,
    rdx_is_pa       = rdx_is_result$pa,
    rdx_is_ph       = rdx_is_result$ph,
    rdx_is_snr      = rdx_is_result$snr,
    rdx_is_snr_flag = rdx_is_result$snr_flag,
    petn_is_rt       = petn_is_result$rt,
    petn_is_pa       = petn_is_result$pa,
    petn_is_ph       = petn_is_result$ph,
    petn_is_snr      = petn_is_result$snr,
    petn_is_snr_flag = petn_is_result$snr_flag
  )
  for (name in names(analytes)) {
    prefix <- tolower(name)
    Results[[paste0(prefix, "_rt")]]       <- analyte_results[[name]]$rt
    Results[[paste0(prefix, "_pa")]]       <- analyte_results[[name]]$pa
    Results[[paste0(prefix, "_ph")]]       <- analyte_results[[name]]$ph
    Results[[paste0(prefix, "_snr")]]      <- analyte_results[[name]]$snr
    Results[[paste0(prefix, "_snr_flag")]] <- analyte_results[[name]]$snr_flag
  }

  write.table(Results, file = paste0(GcData.dir, GcDataName, "SIM.csv"), sep = ",", row.names = FALSE)
  print(paste0(file, " SIM processed"))
}

# =========================================================
# Generate integration plots with shared y-axis scales
# =========================================================
# All integration plot data was queued during the SIM loop above.
# Now that we know the global max y-value per compound, generate
# every plot with a consistent y-axis so that visual comparison
# across injections is meaningful.  Low-signal samples (blanks,
# negative controls) will show visibly smaller peaks against the
# same scale as the highest-response injection.

if (length(integration_plot_queue) > 0) {
  print(paste0("Generating ", length(integration_plot_queue),
               " integration plots with shared y-axis scales..."))

  for (entry in integration_plot_queue) {
    # Look up the shared y-max for this compound; add 5% headroom
    shared_ymax <- y_max_by_compound[[entry$compound_key]]
    if (!is.null(shared_ymax) && !is.na(shared_ymax) && shared_ymax > 0) {
      shared_ylim <- c(0, shared_ymax * 1.05)
    } else {
      shared_ylim <- NULL  # fall back to auto-scale
    }

    plot_integration(
      baseline_corrected = entry$baseline_corrected,
      signal             = entry$signal,
      peak_result        = entry$peak_result,
      expected_rt        = entry$expected_rt,
      step_size          = entry$step_size,
      compound_name      = entry$compound_name,
      save_path          = entry$save_path,
      filename           = entry$filename,
      ylim               = shared_ylim
    )
  }

  # Report the shared scales used
  for (cmpd in names(y_max_by_compound)) {
    print(paste0("  ", cmpd, " shared y-max: ",
                 round(y_max_by_compound[[cmpd]], 0)))
  }

  # Free queue memory
  integration_plot_queue <- NULL
  y_max_by_compound <- NULL

  print("Integration plots complete.")
}

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

