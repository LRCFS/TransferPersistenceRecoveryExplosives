#
#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# The MS1 files needs to be converted using the MsFilesReoganiser.R code

# Load Metadata files
filenameGcData <- list.files(GcDataConvertedRcode.dir, extensionCSV, full.names=TRUE)

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
####### filter data according to threshold value  #######
#########################################################
# # determine mass peak intensity per group
# DataHeatMap <- DataHeatMap %>%
#   group_by(RTime) %>%
#   mutate(freq = Intensity / sum(Intensity))
# 
# # scale TIC intensity 0 to 1 
# DataHeatMap$TICfreqScaled <- rescale(DataHeatMap$TIC)
# 
# # filter values higher than threshold
# DataHeatMapThreshold <- DataHeatMap %>%
#   filter(TICfreqScaled >= threshold)
# 
# #plot figure including all data
# p <- ggplot(DataHeatMapThreshold, aes(RTime, Mass))+
#   geom_tile(aes(fill = Intensity))
# show(p)
# 
#########################################################
#######                Plot TIC                   #######
#########################################################

DataTicRentention <- GcDataCodeOrdered %>%
  select(RetentionTime, TIC) %>%
  distinct()

 DataTicRentention <- DataTicRentention %>%
    filter(RetentionTime > 150 & RetentionTime < 750)
  
# # plot figure including all data
  p <- ggplot(DataTicRentention, aes(RetentionTime, TIC))+
  geom_line() +  #ylim(0,100000) +
  theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  labs (x ="Retention Time (s)", y = "Abundance")
   show(p)
   ggsave(
     sprintf("%s_TIC.tiff",GcDataName),
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

#########################################################
#######      Calculate PA for m/z 46              #######
#########################################################
 # smoothDataLowess <- data.frame(lowess(DataTicRentention$RetentionTime,DataTicRentention$TIC, f=0.001))
 # p <- ggplot(smoothDataLowess, aes(x, y))+
 #   geom_line()+ ylim(0,100000)
 # show(p)
   
   DataSIM46 <- GcDataCodeOrdered %>%
     filter(Mass == 46) %>%
     select(RetentionTime, Intensity)
 
  # rolling average accross the data, value set in Global
  smoothData46 <- DataSIM46 %>%
    mutate(srate_ma02 = rollmean(Intensity, k = rolling.average, fill = NA))
  
  DataSIM46 <- smoothData46 %>%
    select(RetentionTime, Intensity=srate_ma02)
  
  DataSIM46 <- na.omit(DataSIM46)
  
 #  p <- smoothData %>%
 #    gather(metric, value, TIC:srate_ma09) %>%
 #    ggplot(aes(RetentionTime, value, color = metric)) +
 #    geom_line() + ylim(0,25000) +xlim(600,620)
 #  
 #  show(p)
 #  
 # smooth.repeat <- 1
 # while (smooth.repeat <= smooth.loop){
 #  smoothData <- data.frame(
 #   RetentionTime = as.vector(DataTicRentention$RetentionTime),
 #   TIC = as.vector(smooth(DataTicRentention$TIC)))
 #  DataTicRentention <- smoothData
 #  smooth.repeat <- smooth.repeat + 1
 # }
 
 
 # plot figure including all data
  # p <- ggplot(DataTicRentention, aes(RetentionTime, TIC))+
  #   geom_line()+ ylim(0,25000)
  # show(p)
 
 # smoothingSpline = smooth.spline(DataTicRentention$RetentionTime, smooth(DataTicRentention$TIC, spar=0.35))
 
 
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
 
 baseline.peakDetection46 <- baseline(tempFileDataSIM46matrix[1,, drop=FALSE], method='peakDetection', snminimum=10)
 
 baseline.peakDetection46 <- baseline(tempFileDataSIM46matrix[1,, drop=FALSE], method='rollingBall',  wm=150, ws=40)
 
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
 
 # Plot it
 p <- ggplot(Combined.bc.fillPeak46, aes(x=RetentionTime)) +
   geom_line(aes(y = Intensity, colour = "GC trace")) +
   geom_line(aes(y = BaselineTrend, colour = "baseline")) +
   ylim(0,80000) +
   xlim(440,475)
 
  show(p)
 
  # When running single file don't run line below (skip to ggsave)
 #  GcDataName <- gsub('.{4}$', '', GcDataName)
 ggsave(
   sprintf("%s_mz46.tiff",GcDataName),
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

 
# # convert data to hyperSpec format
# file <- as.data.frame(DataTicRentention)
# temp_hyper_0 <- new ("hyperSpec", wavelength = file [,1], spc = t (file [, -1]))
# 
# # apply the background
# bl <- spc.rubberband (temp_hyper_0 [,, 350 ~ 700], noise = 3000, df = 42)
# 
# # plot the background to check fit to data
#  plot(bl)
#  plot(temp_hyper_0,add=TRUE)
# 
# # subtract the background to the original data and plot to check
# temp_hyper_1 <- temp_hyper_0-bl
#  plot(temp_hyper_1, plot.args = list (ylim = c(0, 4500)))
# 
# # export the data in temp folder
# write.txt.long(temp_hyper_1, file = paste0(TempData.dir, "temp.txt"))
# 
# # open the data from temp folder
# SignalMinusBg <- read.table(file = paste0(TempData.dir, "temp.txt"), sep = "\t", header = TRUE)

 
# as an alternative to HyperSpec curve fitting 
SignalMinusBg46 <- Combined.bc.fillPeak46 %>%
  select(RetentionTime,Subtracted)


names(SignalMinusBg46)[1] <- "RTime"
names(SignalMinusBg46)[2] <- "Intensity"

SignalMax46 <- max(SignalMinusBg46$Intensity)

# put a minimum threshold for peak detection
# below this limit, there will be no peak and output with zeros
# will still be created to avoid errors

if (SignalMax46 >= SignalMaxThreshold) {
  # determine the position of the peaks: sample and internal standard
Q46 <- peaks(SignalMinusBg46,minPH=SignalMaxThreshold, minPW=0.2)

} else{
  Q46 <- data.frame (x = c(0),
                   y = c(0),
                   w = c(0))
  }

# step size between two retention time
Step.size <- DataTicRentention$RetentionTime[5] - DataTicRentention$RetentionTime[4]

# apply limits to RTime: peak integration boundaries

#########################################################
#######      Calculate PA for m/z 251              #######
#########################################################
# smoothDataLowess <- data.frame(lowess(DataTicRentention$RetentionTime,DataTicRentention$TIC, f=0.001))
# p <- ggplot(smoothDataLowess, aes(x, y))+
#   geom_line()+ ylim(0,100000)
# show(p)

DataSIM251 <- GcDataCodeOrdered %>%
  filter(Mass == 251) %>%
  select(RetentionTime, Intensity)

# rolling average accross the data, value set in Global
smoothData251 <- DataSIM251 %>%
  mutate(srate_ma02 = rollmean(Intensity, k = rolling.average, fill = NA))

DataSIM251 <- smoothData251 %>%
  select(RetentionTime, Intensity=srate_ma02)

DataSIM251 <- na.omit(DataSIM251)

#  p <- smoothData %>%
#    gather(metric, value, TIC:srate_ma09) %>%
#    ggplot(aes(RetentionTime, value, color = metric)) +
#    geom_line() + ylim(0,25000) +xlim(600,620)
#  
#  show(p)
#  
# smooth.repeat <- 1
# while (smooth.repeat <= smooth.loop){
#  smoothData <- data.frame(
#   RetentionTime = as.vector(DataTicRentention$RetentionTime),
#   TIC = as.vector(smooth(DataTicRentention$TIC)))
#  DataTicRentention <- smoothData
#  smooth.repeat <- smooth.repeat + 1
# }


# plot figure including all data
# p <- ggplot(DataTicRentention, aes(RetentionTime, TIC))+
#   geom_line()+ ylim(0,25000)
# show(p)

# smoothingSpline = smooth.spline(DataTicRentention$RetentionTime, smooth(DataTicRentention$TIC, spar=0.35))


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

baseline.peakDetection251 <- baseline(tempFileDataSIM251matrix[1,, drop=FALSE], method='peakDetection', snminimum=10)

baseline.peakDetection251 <- baseline(tempFileDataSIM251matrix[1,, drop=FALSE], method='rollingBall',  wm=10, ws=5)

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

# Plot it
p <- ggplot(Combined.bc.fillPeak251, aes(x=RetentionTime)) +
  geom_line(aes(y = Intensity, colour = "GC trace")) +
  geom_line(aes(y = BaselineTrend, colour = "baseline")) +
  ylim(0,5000) +
  xlim(510,550)

show(p)

# When running single file don't run line below (skip to ggsave)
#  GcDataName <- gsub('.{4}$', '', GcDataName)
ggsave(
  sprintf("%s_mz251.tiff",GcDataName),
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

if (SignalMax251 >= SignalMaxThreshold) {
  # determine the position of the peaks: sample and internal standard
  Q251 <- peaks(SignalMinusBg251,minPH=SignalMaxThreshold, minPW=0.2)
  
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
  filter(x < (IS.Exp.Range+1) & x > IS.Exp.Range-1)

Q.P.IS <- Q.IS[1,1]

Q.IS.testRange <- abs(IS.Exp.Range - Q.IS[1,1])


if (is.na(Q.IS.testRange) || Q.IS.testRange < 10){
  
  # limits
  Peak.IS.Lmin <- Q.IS$x[1] - (5 * Step.size)
  Peak.IS.Lmax <- Q.IS$x[1] + (10 * Step.size)
  
  PeakIS <- SignalMinusBg251 %>%
    filter( between(RTime, Peak.IS.Lmin, Peak.IS.Lmax) )  # 398, 403
  
  # plot figure including all data
  # p <- ggplot(PeakIS, aes(RTime, TIC))+
  #   geom_line()
  # show(p)
  
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
  #I.S.PA.Check <- NA
}

################################################################
#####                 PETN Peak Area                  ######
################################################################
# PETN is the second peak on list (of the peak of interest), other peaks may be present on that list !!!!

##If you need to remove rows#Q <- as.data.frame(Q[-c(3), ])
#Q.PETN <- top_n(Q,-1,x)

#filter data to identify PETN based on expected RTime +- 1%
Q.PETN <- Q46 %>%
  filter(x < (PETN.Exp.Range+1) & x > PETN.Exp.Range-1)

if (nrow(Q.PETN) != 0){
Q.P.PETN <- Q.PETN[1,1]

Q.PETN.testRange <- abs(PETN.Exp.Range - Q.PETN[1,1])

if (!is.na(Q.PETN.testRange) || Q.PETN.testRange < 15) {
  # limits
  Peak.PETN.Lmin <- Q.PETN$x[1] - (15 * Step.size)
  Peak.PETN.Lmax <- Q.PETN$x[1] + (21 * Step.size)
  
  PeakPETN <- SignalMinusBg46 %>%
  filter( between(RTime, Peak.PETN.Lmin, Peak.PETN.Lmax) )  # 608, 643
  
  # plot figure including all data
  # p <- ggplot(PeakPETN, aes(RTime, TIC))+
  #   geom_line() +ylim(0,100000)
  # show(p)
  
  idEti <- order(PeakPETN$RTime)
  PA <- sum(diff(PeakPETN$RTime[idEti])*rollmean(PeakPETN$Intensity[idEti],2))
  
  # # this is to check the boundaries are not too narrow
  # Peak.PETN.Lmin <- Q.PETN$x[1] - (25 * Step.size)
  # Peak.PETN.Lmax <- Q.PETN$x[1] + (31 * Step.size)
  # 
  # PeakPETN <- SignalMinusBg %>%
  #   filter( between(RTime, Peak.PETN.Lmin, Peak.PETN.Lmax) )  # 608, 643
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
  
} else{PA <- NA
      Q.P.PETN <- NA
  #PA.Check <- NA
  # PA.Check2 <- NA
}
} else{PA <- NA
      Q.P.PETN <- NA}


Results <-data.frame(File,Q.P.IS,I.S.PA,Q.P.PETN,PA)

write.table(Results, file=paste0(GcData.dir,GcDataName, ".csv"), sep = ",", row.names = FALSE)

}

