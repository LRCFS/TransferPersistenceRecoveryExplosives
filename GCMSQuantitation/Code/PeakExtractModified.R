#
#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# The MS1 files needs to be converted using the MsFilesReoganiser.R code

# Load Metadata files
filenameGcData <- list.files(GcDataConvertedRcodeTIC.dir, extensionCSV, full.names=TRUE)
for (file in filenameGcData) {
  GcDataName <- gsub(extensionCSV, "", file)
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  # GcDataName <- gsub(extensionCSV, "", filenameGcData[3])
  
  GcDataName <- gsub(".*/", "", GcDataName)
  File <- GcDataName
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  #GcDataCodeOrdered <- read.csv2(filenameGcData[14], sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- read.csv2(file, sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- mutate_all(GcDataCodeOrdered, function(x) as.numeric(as.character(x)))

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
    filter(RetentionTime > 180)
  
  DataTIC <- na.omit(DataTIC)
  
  ########################################################
  #######    baseline corrections using hyperSpec  #######
  ########################################################
  
  # baseline corrections adapted from and using the library package hyperSpec
  # for more information: https://cran.r-project.org/web/packages/hyperSpec/index.html
  # the data must be in hyperspectral format (this could be extended to creating matrix of data)
  
  # convert data to hyperSpec format
  fileTIC <- as.data.frame(DataTIC)
  
  tempFileDataTIC <- t(as.matrix(fileTIC$TIC))
  
  tempFileDataTICmatrix <-data.matrix(tempFileDataTIC)
  
  # Description
  # A translation from Kevin R. Coombes et al.’s MATLAB code for detecting peaks and removing
  # baselines
  # https://cran.r-project.org/web/packages/baseline/baseline.pdf
  # method should be: “als”, “fillPeaks”, “irls”, “lowpass”, “medianWindow”, “modpolyfit”, “peakDetection”, “rfbaseline”, “rollingBall”, “shirley”, “TAP”
  
  baseline.peakDetectionTIC <- baseline(tempFileDataTICmatrix[1,, drop=FALSE], method='rollingBall',  wm=120, ws=40)
  
  # extract the corrected spectra
  tempFileCorrectedTIC <- baseline.peakDetectionTIC@corrected
  
  # convert to dataframe and transpose to 1 column
  tempFileCorrectedTIC <- as.data.frame(t(as.data.frame(tempFileCorrectedTIC)))
  
  # rename column to Corrected
  names(tempFileCorrectedTIC)[1] <- "CorrectedTrend"
  
  # extract the baseline
  tempFileBaselineTIC <- baseline.peakDetectionTIC@baseline
  
  # convert to dataframe and transpose to 1 column
  tempFileBaselineTIC <- as.data.frame(t(as.data.frame(tempFileBaselineTIC)))
  
  # rename column to BaselineTrend
  names(tempFileBaselineTIC)[1] <- "BaselineTrend"
  
  Combined.bc.fillPeakTIC <- cbind(fileTIC,tempFileCorrectedTIC, tempFileBaselineTIC)
  names(Combined.bc.fillPeakTIC)[1] <- "RetentionTime"
  
  Combined.bc.fillPeakTIC$Subtracted <- Combined.bc.fillPeakTIC$TIC-Combined.bc.fillPeakTIC$BaselineTrend

  # When running single file don't run line below (skip to ggsave)
  #  GcDataName <- gsub('.{4}$', '', GcDataName)
  ggsave(
    sprintf("%s_TICBaseline.tiff",GcDataName),
    plot = p,
    device = NULL,
    path = file.path(GCSampleTrace.dir),
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

filenameGcData <- list.files(GcDataConvertedRcodeSIM.dir, extensionCSV, full.names=TRUE)

for (file in filenameGcData) {
  GcDataName <- gsub(extensionCSV, "", file)
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  # GcDataName <- gsub(extensionCSV, "", filenameGcData[3])
  
  GcDataName <- gsub(".*/", "", GcDataName)
  File <- GcDataName
  
  # for testing code on a single file (first on list) in filenameGcDataConverterMs
  #GcDataCodeOrdered <- read.csv2(filenameGcData[14], sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- read.csv2(file, sep = ",", header = TRUE)
  
  GcDataCodeOrdered <- mutate_all(GcDataCodeOrdered, function(x) as.numeric(as.character(x)))
  
  #########################################################
  #######      Calculate PA for m/z 46              #######
  #########################################################
  
  DataSIM46 <- GcDataCodeOrdered %>%
    filter(Mass == 46) %>%
    select(RetentionTime, Intensity) %>%
    filter(RetentionTime < 440)
  
  # rolling average accross the data, value set in Global
  smoothData46 <- DataSIM46 %>%
    mutate(srate_ma02 = rollmean(Intensity, k = rolling.average, fill = NA))
  
  DataSIM46 <- smoothData46 %>%
    select(RetentionTime, Intensity=srate_ma02)
  
  DataSIM46 <- na.omit(DataSIM46)
  
  ########################################################
  #######    baseline corrections using hyperSpec  #######
  ########################################################
  
  # baseline corrections adapted from and using the library package hyperSpec
  # for more information: https://cran.r-project.org/web/packages/hyperSpec/index.html
  # the data must be in hyperspectral format (this could be extended to creating matrix of data)
  
  # convert data to hyperSpec format
  fileSIM46 <- as.data.frame(DataSIM46)
  
  tempFileDataSIM46 <- fileSIM46$Intensity
  
  tempFileDataSIM46 <- as.data.frame(t(as.data.frame(tempFileDataSIM46)))
  
  tempFileDataSIM46matrix <-data.matrix(tempFileDataSIM46)
  
  # Description
  # A translation from Kevin R. Coombes et al.’s MATLAB code for detecting peaks and removing
  # baselines
  # https://cran.r-project.org/web/packages/baseline/baseline.pdf
  # method should be: “als”, “fillPeaks”, “irls”, “lowpass”, “medianWindow”, “modpolyfit”, “peakDetection”, “rfbaseline”, “rollingBall”, “shirley”, “TAP”

  baseline.peakDetection46 <- baseline(tempFileDataSIM46matrix[1,, drop=FALSE], method='rollingBall',  wm=120, ws=40)
  
  # extract the corrected spectra
  tempFileCorrected46 <- baseline.peakDetection46@corrected
  
  # convert to dataframe and transpose to 1 column
  tempFileCorrected46 <- as.data.frame(t(as.data.frame(tempFileCorrected46)))
  
  # rename column to Corrected
  names(tempFileCorrected46)[1] <- "CorrectedTrend"

  # extract the baseline
  tempFileBaseline46 <- baseline.peakDetection46@baseline
  
  # convert to dataframe and transpose to 1 column
  tempFileBaseline46 <- as.data.frame(t(as.data.frame(tempFileBaseline46)))
  
  # rename column to BaselineTrend
  names(tempFileBaseline46)[1] <- "BaselineTrend"
  
  Combined.bc.fillPeak46 <- cbind(fileSIM46,tempFileCorrected46, tempFileBaseline46)
  names(Combined.bc.fillPeak46)[1] <- "RetentionTime"
  
  Combined.bc.fillPeak46$Subtracted <- Combined.bc.fillPeak46$Intensity-Combined.bc.fillPeak46$BaselineTrend
  
  # as an alternative to HyperSpec curve fitting 
  SignalMinusBg46 <- Combined.bc.fillPeak46 %>%
    select(RetentionTime,Subtracted)
  names(SignalMinusBg46)[1] <- "RTime"
  names(SignalMinusBg46)[2] <- "Intensity"
  
  SignalMinusBg46 <- SignalMinusBg46 %>%
    filter(between(RTime, (PETN.Exp.Range - 0.5), (PETN.Exp.Range + 0.5)))
  
  SignalMax46 <- max(SignalMinusBg46$Intensity)
  
  # put a minimum threshold for peak detection
  # below this limit, there will be no peak and output with zeros
  # will still be created to avoid errors
  
  if (SignalMax46 >= SignalMaxThresholdSIM) {
    # determine the position of the peaks: sample and internal standard
    Q46 <- ModPeaks(SignalMinusBg46,minPH=SignalMaxThresholdSIM, minPW=0.2)
    
  } else{
    Q46 <- data.frame (x = c(0),
                       y = c(0),
                       w = c(0))
  }
  
  # step size between two retention time
  Step.size <- DataSIM46$RetentionTime[5] - DataSIM46$RetentionTime[4]
  
  #########################################################
  #######      Calculate PA for m/z 251              #######
  #########################################################
  
  DataSIM251 <- GcDataCodeOrdered %>%
    filter(Mass == 251) %>%
    select(RetentionTime, Intensity)
  
  # rolling average accross the data, value set in Global
  smoothData251 <- DataSIM251 %>%
    mutate(srate_ma02 = rollmean(Intensity, k = rolling.average, fill = NA))
  
  DataSIM251 <- smoothData251 %>%
    select(RetentionTime, Intensity=srate_ma02)
  
  DataSIM251 <- na.omit(DataSIM251)

  ########################################################
  #######    baseline corrections using hyperSpec  #######
  ########################################################
  
  # baseline corrections adapted from and using the library package hyperSpec
  # for more information: https://cran.r-project.org/web/packages/hyperSpec/index.html
  # the data must be in hyperspectral format (this could be extended to creating matrix of data)
  
  # convert data to hyperSpec format
  fileSIM251 <- as.data.frame(DataSIM251)
  
  tempFileDataSIM251 <- fileSIM251$Intensity
  
  tempFileDataSIM251 <- as.data.frame(t(as.data.frame(tempFileDataSIM251)))
  
  tempFileDataSIM251matrix <-data.matrix(tempFileDataSIM251)
  
  # Description
  # A translation from Kevin R. Coombes et al.’s MATLAB code for detecting peaks and removing
  # baselines
  # https://cran.r-project.org/web/packages/baseline/baseline.pdf
  # method should be: “als”, “fillPeaks”, “irls”, “lowpass”, “medianWindow”, “modpolyfit”, “peakDetection”, “rfbaseline”, “rollingBall”, “shirley”, “TAP”
  
  baseline.peakDetection251 <- baseline(tempFileDataSIM251matrix[1,, drop=FALSE], method='rollingBall',  wm=120, ws=40)
  
  # extract the corrected spectra
  tempFileCorrected251 <- baseline.peakDetection251@corrected
  
  # convert to dataframe and transpose to 1 column
  tempFileCorrected251 <- as.data.frame(t(as.data.frame(tempFileCorrected251)))
  
  # rename column to Corrected
  names(tempFileCorrected251)[1] <- "CorrectedTrend"
  
  # extract the baseline
  tempFileBaseline251 <- baseline.peakDetection251@baseline
  
  # convert to dataframe and transpose to 1 column
  tempFileBaseline251 <- as.data.frame(t(as.data.frame(tempFileBaseline251)))
  
  # rename column to BaselineTrend
  names(tempFileBaseline251)[1] <- "BaselineTrend"
  
  Combined.bc.fillPeak251 <- cbind(fileSIM251,tempFileCorrected251, tempFileBaseline251)
  names(Combined.bc.fillPeak251)[1] <- "RetentionTime"
  
  Combined.bc.fillPeak251$Subtracted <- Combined.bc.fillPeak251$Intensity-Combined.bc.fillPeak251$BaselineTrend
  
  # Combine the two datasets with a new column to indicate the source
  Combined.bc.fillPeak46$Source <- "m/z 46"
  Combined.bc.fillPeak251$Source <- "m/z 251"
  
  # Combine both into one dataframe
  CombinedPlotData <- rbind(
    Combined.bc.fillPeak46[, c("RetentionTime", "Subtracted", "Source")],
    Combined.bc.fillPeak251[, c("RetentionTime", "Subtracted", "Source")]
  )
  
  p <- ggplot(CombinedPlotData, aes(x = RetentionTime, y = Subtracted, color = Source)) +
    geom_line() +
    labs(
      title = "Baseline-Corrected GC Traces",
      x = "Retention Time (s)",
      y = "Corrected Intensity"
    ) +
    theme_minimal()
  
  # Show and save the plot
  print(p)

  # When running single file don't run line below (skip to ggsave)
  #  GcDataName <- gsub('.{4}$', '', GcDataName)
  ggsave(
    sprintf("%s_Combined_GC_Traces.tiff",GcDataName),
    plot = p,
    device = NULL,
    path = file.path(GCSampleTrace.dir),
    scale = 1,
    width = 7.5,
    height = 5.0,
    units = c("in"),
    dpi = 300,
    limitsize = TRUE
  )
  
  SignalMinusBg251 <- Combined.bc.fillPeak251 %>%
    select(RetentionTime,Subtracted)
  
  names(SignalMinusBg251)[1] <- "RTime"
  names(SignalMinusBg251)[2] <- "Intensity"
  
  SignalMax251 <- max(SignalMinusBg251$Intensity)
  
  # put a minimum threshold for peak detection
  # below this limit, there will be no peak and output with zeros
  # will still be created to avoid errors
  
  if (SignalMax251 >= SignalMaxThresholdSIM) {
    # determine the position of the peaks: sample and internal standard
    Q251 <- ModPeaks(SignalMinusBg251,minPH=SignalMaxThresholdSIM, minPW=0.2)
    
  } else{
    Q251 <- data.frame (x = c(0),
                        y = c(0),
                        w = c(0))
  }
  
  ################################################################
  #####             Internal standard Peak Area             ######
  ################################################################
  
  #filter data to identify IS based on expected RTime +- 1%
  Q.IS <- Q251 %>%
    filter(x < (IS.Exp.Range+0.5) & x > IS.Exp.Range-0.5)
  
  Q.P.IS <- Q.IS[1,1]
  
  Q.IS.testRange <- abs(IS.Exp.Range - Q.IS[1,1])
  
  
  if (is.na(Q.IS.testRange) || Q.IS.testRange < 10){
    
    # limits
    Peak.IS.Lmin <- Q.IS$x[1] - (15 * Step.size)
    Peak.IS.Lmax <- Q.IS$x[1] + (15 * Step.size)
    
    PeakIS <- SignalMinusBg251 %>%
      filter( between(RTime, Peak.IS.Lmin, Peak.IS.Lmax) )  # 398, 403
    
    #plot figure including all data
    p <- ggplot(PeakIS, aes(RTime, Intensity))+
      geom_line()
    show(p)
    
    idIS <- order(PeakIS$RTime)
    I.S.PA <- sum(diff(PeakIS$RTime[idIS])*rollmean(PeakIS$Intensity[idIS],2))
    
    # # this is to check the boundaries are not too narrow
    # Peak.IS.Lmin <- Q.IS$x[1] - (15 * Step.size)
    # Peak.IS.Lmax <- Q.IS$x[1] + (20 * Step.size)
    # 
    # PeakIS <- SignalMinusBg %>%
    #   filter( between(RTime, Peak.IS.Lmin, Peak.IS.Lmax) )  # 398, 403
    # 
    # idIS <- order(PeakIS$RTime)
    # I.S.PA.Check <- sum(diff(PeakIS$RTime[idIS])*rollmean(PeakIS$TIC[idIS],2))
    
  } else{ I.S.PA <- NA

  }
  
  ################################################################
  #####                 PETN Peak Area                  ######
  ################################################################

  #filter data to identify PETN based on expected RTime +- 1%
  Q.PETN <- Q46 %>%
    filter(x < (PETN.Exp.Range+0.5) & x > PETN.Exp.Range-0.5)
  
  if (nrow(Q.PETN) != 0){
    Q.P.PETN <- Q.PETN[1,1]
    
    Q.PETN.testRange <- abs(PETN.Exp.Range - Q.PETN[1,1])
    
    if (!is.na(Q.PETN.testRange) || Q.PETN.testRange < 15) {
      # limits
      Peak.PETN.Lmin <- Q.PETN$x[1] - (15 * Step.size)
      Peak.PETN.Lmax <- Q.PETN$x[1] + (15 * Step.size)
      
      PeakPETN <- SignalMinusBg46 %>%
        filter( between(RTime, Peak.PETN.Lmin, Peak.PETN.Lmax) ) 

      idEti <- order(PeakPETN$RTime)
      PETN.PA <- sum(diff(PeakPETN$RTime[idEti])*rollmean(PeakPETN$Intensity[idEti],2))
      
      # # this is to check the boundaries are not too narrow
      # Peak.PETN.Lmin <- Q.PETN$x[1] - (25 * Step.size)
      # Peak.PETN.Lmax <- Q.PETN$x[1] + (31 * Step.size)
      # 
      # PeakPETN <- SignalMinusBg %>%
      #   filter( between(RTime, Peak.PETN.Lmin, Peak.PETN.Lmax) ) 
      # 
      # idEti <- order(PeakPETN$RTime)
      # PA.Check <- sum(diff(PeakPETN$RTime[idEti])*rollmean(PeakPETN$TIC[idEti],2))
      # 
      # # this is to check the boundaries are not too narrow
      # Peak.PETN.Lmin <- Q.PETN$x[1] - (50 * Step.size)
      # Peak.PETN.Lmax <- Q.PETN$x[1] + (62 * Step.size)
      # 
      # PeakPETN <- SignalMinusBg %>%
      #   filter( between(RTime, Peak.PETN.Lmin, Peak.PETN.Lmax) )  # 608, 643
      # 
      # idEti <- order(PeakPETN$RTime)
      # PA.Check2 <- sum(diff(PeakPETN$RTime[idEti])*rollmean(PeakPETN$TIC[idEti],2))
      
    } else{PETN.PA <- NA
    Q.P.PETN <- NA
    }
  } else{PETN.PA <- NA
  Q.P.PETN <- NA}

    Results <-data.frame(File,Q.P.IS,I.S.PA,Q.P.PETN,PETN.PA)

  write.table(Results, file=paste0(GcData.dir,GcDataName, "SIM.csv"), sep = ",", row.names = FALSE)
  print(paste0(file, " SIM processed"))
}

TICFile <- list.files(path = GcData.dir, pattern = "\\TIC.csv$")
SIMFile <- list.files(path = GcData.dir, pattern = "\\SIM.csv$")
NumInj <- length(TICFile)

for (i in 1:NumInj) {
  filename <- TICFile[i]
  filename <- substr(filename,1,3)
  TICData <- read.csv2(paste0(GcData.dir,TICFile[i]), sep = ",") 
  SIMData <- read.csv2(paste0(GcData.dir,SIMFile[i]), sep = ",")
  SummaryData <- cbind(SIMData,TICData)
  SummaryData[] <- lapply(SummaryData, function(col) as.numeric(as.character(col)))

  write.table(SummaryData, file=paste0(GcData.dir,filename, "Summary.csv"), sep = ",", row.names = FALSE)
  print(paste0(filename, " Summary processed"))
}
