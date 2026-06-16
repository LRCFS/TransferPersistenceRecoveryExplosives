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

# Collect all unique m/z values needed for IS and analytes
all_mz <- unique(c(rdx_is_config$mz, sapply(analytes, function(a) a$mz)))

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
  for (a in analytes) {
    if (a$mz == mz_val && !is.na(a$rt)) {
      windows <- c(windows, list(c(a$rt, 5)))
    }
  }
  rt_windows_by_mz[[as.character(mz_val)]] <- windows
}

# Build per-m/z minimum peak height from compound definitions
mz_min_peak_height <- list()
mz_min_peak_height[[as.character(rdx_is_config$mz)]] <- rdx_is_config$min_peak_height
for (a in analytes) {
  mz_min_peak_height[[as.character(a$mz)]] <- a$min_peak_height
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

  # Process each m/z channel
  channel_results <- list()
  for (mz_val in all_mz) {
    channel_results[[as.character(mz_val)]] <- process_sim_channel(
      GcDataCodeOrdered, mz_val,
      rt_windows_by_mz[[as.character(mz_val)]],
      rolling_avg = rolling.average,
      min_peak_height = mz_min_peak_height[[as.character(mz_val)]]
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

