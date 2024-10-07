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
#######           calculate TIC peak areas        #######
#########################################################

DataTicRentention <- GcDataCodeOrdered %>%
  select(RetentionTime, TIC) %>%
  distinct()

  DataTicRentention <- DataTicRentention %>%
    filter(RetentionTime > 365 & RetentionTime < 750)
  
# # plot figure including all data
  p <- ggplot(DataTicRentention, aes(RetentionTime, TIC))+
  geom_line() +  #ylim(0,100000) +
  theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  labs (x ="Retention Time (s)", y = "Abundance")
  # show(p)

 # smoothDataLowess <- data.frame(lowess(DataTicRentention$RetentionTime,DataTicRentention$TIC, f=0.001))
 # p <- ggplot(smoothDataLowess, aes(x, y))+
 #   geom_line()+ ylim(0,100000)
 # show(p)
 
  # rolling average accross the data, value set in Global
  smoothData <- DataTicRentention %>%
    select(RetentionTime, TIC) %>%
    mutate(srate_ma02 = rollmean(TIC, k = rolling.average, fill = NA))
  
  DataTicRentention <- smoothData %>%
    select(RetentionTime, TIC=srate_ma02)
  
  DataTicRentention <- na.omit(DataTicRentention)
  
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
 fileTIC <- as.data.frame(DataTicRentention)
 
 tempFileDataTIC <- fileTIC$TIC
 
 tempFileDataTIC <- as.data.frame(t(as.data.frame(tempFileDataTIC)))
 
 tempFileDataTICmatrix <-data.matrix(tempFileDataTIC)
 
 # Description
 # A translation from Kevin R. Coombes et al.’s MATLAB code for detecting peaks and removing
 # baselines
 # https://cran.r-project.org/web/packages/baseline/baseline.pdf
 # method should be: “als”, “fillPeaks”, “irls”, “lowpass”, “medianWindow”, “modpolyfit”, “peakDetection”, “rfbaseline”, “rollingBall”, “shirley”, “TAP”
 
 baseline.peakDetection <- baseline(tempFileDataTICmatrix[1,, drop=FALSE], method='peakDetection', snminimum=1000,
                                     left=30, right=350, lwin=60, rwin=300)
 
 #baseline.peakDetection <- baseline(tempFileDataTICmatrix[1,, drop=FALSE], method='rollingBall',  wm=150, ws=40)
 
 # extract the corrected spectra
 tempFileCorrected <- baseline.peakDetection@corrected
 
 # convert to dataframe and transpose to 1 column
 tempFileCorrected <- as.data.frame(t(as.data.frame(tempFileCorrected)))
 
 # rename column to Corrected
 names(tempFileCorrected)[1] <- "CorrectedTrend"
 
 # extract the baseline
 tempFileBaseline <- baseline.peakDetection@baseline
 
 # convert to dataframe and transpose to 1 column
 tempFileBaseline <- as.data.frame(t(as.data.frame(tempFileBaseline)))
 
 # rename column to BaselineTrend
 names(tempFileBaseline)[1] <- "BaselineTrend"
 
 Combined.bc.fillPeak <- cbind(fileTIC,tempFileCorrected, tempFileBaseline)
 names(Combined.bc.fillPeak)[1] <- "RetentionTime"
 
 Combined.bc.fillPeak$Subtracted <- Combined.bc.fillPeak$TIC-Combined.bc.fillPeak$BaselineTrend
 
 # Plot it
 p <- ggplot(Combined.bc.fillPeak, aes(x=RetentionTime)) +
   geom_line(aes(y = TIC, colour = "GC trace")) +
   geom_line(aes(y = BaselineTrend, colour = "baseline")) +
   ylim(0,25000) +
   xlim(350,750)
 
 # show(p)
 # When running single file don't run line below (skip to ggsave)
 #  GcDataName <- gsub('.{4}$', '', GcDataName)
 ggsave(
   sprintf("%s_BL.tiff",GcDataName),
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
SignalMinusBg <- Combined.bc.fillPeak %>%
  select(RetentionTime,Subtracted)


names(SignalMinusBg)[1] <- "RTime"
names(SignalMinusBg)[2] <- "TIC"

SignalMax <- max(SignalMinusBg$TIC)

# put a minimum threshold for peak detection
# below this limit, there will be no peak and output with zeros
# will still be created to avoid errors

if (SignalMax >= SignalMaxThreshold) {
  # determine the position of the peaks: sample and internal standard
Q <- peaks(SignalMinusBg,minPH=SignalMaxThreshold, minPW=0.2)

} else{
  Q <- data.frame (x = c(0),
                   y = c(0),
                   w = c(0))
  }

# step size between two retention time
Step.size <- DataTicRentention$RetentionTime[5] - DataTicRentention$RetentionTime[4]

# apply limits to RTime: peak integration boundaries
################################################################
#####             Internal standard Peak Area             ######
################################################################
# Internal standard is first peak on list 
Q.IS <- top_n(Q,-1,x)

# Internal standard is second peak on list 
# Q.IS <- Q[2,]

Q.P.IS <- Q.IS[1,1]

Q.IS.testRange <- abs(IS.Exp.Range - Q.IS[1,1])


if (Q.IS.testRange < 10) {
  
  # limits
  Peak.IS.Lmin <- Q.IS$x[1] - (5 * Step.size)
  Peak.IS.Lmax <- Q.IS$x[1] + (10 * Step.size)
  
  PeakIS <- SignalMinusBg %>%
    filter( between(RTime, Peak.IS.Lmin, Peak.IS.Lmax) )  # 398, 403
  
  # plot figure including all data
  # p <- ggplot(PeakIS, aes(RTime, TIC))+
  #   geom_line()
  # show(p)
  
  idIS <- order(PeakIS$RTime)
  I.S.PA <- sum(diff(PeakIS$RTime[idIS])*rollmean(PeakIS$TIC[idIS],2))
  
  # # this is to check the boundaries are not too narrow
  # Peak.IS.Lmin <- Q.IS$x[1] - (15 * Step.size)
  # Peak.IS.Lmax <- Q.IS$x[1] + (20 * Step.size)
  # 
  # PeakIS <- SignalMinusBg %>%
  #   filter( between(RTime, Peak.IS.Lmin, Peak.IS.Lmax) )  # 398, 403
  # 
  # idIS <- order(PeakIS$RTime)
  # I.S.PA.Check <- sum(diff(PeakIS$RTime[idIS])*rollmean(PeakIS$TIC[idIS],2))
  
} else{I.S.PA <- NA
  #I.S.PA.Check <- NA
  }


################################################################
#####                 Etizolam Peak Area                  ######
################################################################
# Etizolam is the second peak on list (of the peak of interest), other peaks may be present on that list !!!!

##If you need to remove rows#Q <- as.data.frame(Q[-c(3), ])
Q.Etizolam <- top_n(Q,1,x)

Q.P.Etizolam <- Q.Etizolam[1,1]

Q.Etizolam.testRange <- abs(Etizolam.Exp.Range - Q.Etizolam[1,1])

if (Q.Etizolam.testRange < 15) {
  # limits
  Peak.Etizolam.Lmin <- Q.Etizolam$x[1] - (15 * Step.size)
  Peak.Etizolam.Lmax <- Q.Etizolam$x[1] + (21 * Step.size)
  
  PeakEtizolam <- SignalMinusBg %>%
  filter( between(RTime, Peak.Etizolam.Lmin, Peak.Etizolam.Lmax) )  # 608, 643
  
  # plot figure including all data
  # p <- ggplot(PeakEtizolam, aes(RTime, TIC))+
  #   geom_line() +ylim(0,100000)
  # show(p)
  
  idEti <- order(PeakEtizolam$RTime)
  PA <- sum(diff(PeakEtizolam$RTime[idEti])*rollmean(PeakEtizolam$TIC[idEti],2))
  
  # # this is to check the boundaries are not too narrow
  # Peak.Etizolam.Lmin <- Q.Etizolam$x[1] - (25 * Step.size)
  # Peak.Etizolam.Lmax <- Q.Etizolam$x[1] + (31 * Step.size)
  # 
  # PeakEtizolam <- SignalMinusBg %>%
  #   filter( between(RTime, Peak.Etizolam.Lmin, Peak.Etizolam.Lmax) )  # 608, 643
  # 
  # idEti <- order(PeakEtizolam$RTime)
  # PA.Check <- sum(diff(PeakEtizolam$RTime[idEti])*rollmean(PeakEtizolam$TIC[idEti],2))
  # 
  # # this is to check the boundaries are not too narrow
  # Peak.Etizolam.Lmin <- Q.Etizolam$x[1] - (50 * Step.size)
  # Peak.Etizolam.Lmax <- Q.Etizolam$x[1] + (62 * Step.size)
  # 
  # PeakEtizolam <- SignalMinusBg %>%
  #   filter( between(RTime, Peak.Etizolam.Lmin, Peak.Etizolam.Lmax) )  # 608, 643
  # 
  # idEti <- order(PeakEtizolam$RTime)
  # PA.Check2 <- sum(diff(PeakEtizolam$RTime[idEti])*rollmean(PeakEtizolam$TIC[idEti],2))
  
} else{PA <- NA
  #PA.Check <- NA
  # PA.Check2 <- NA
}

Results <-data.frame(File,Q.P.IS,I.S.PA,Q.P.Etizolam,PA)

write.table(Results, file=paste0(GcData.dir,GcDataName, ".csv"), sep = ",", row.names = FALSE)

}

