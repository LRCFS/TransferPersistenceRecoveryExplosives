#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################
# Load Metadata files
filenameMetadata <- list.files(Metadata.dir, pattern=extensionXLSX, full.names=TRUE)

Metadata <- read_xlsx(filenameMetadata)

# create the date from DataName
Metadata$Date <- str_sub(Metadata$DataName,1,6)

# Load GC results
filenameGcData <- list.files(GcData.dir, pattern=extensionCSV, full.names=TRUE)

GcResults <- do.call(rbind, lapply(filenameGcData, function(x) transform(read.csv(x), File = basename(x))))

# GcResults <- read.csv(filenameGcData, header = TRUE, encoding = "UTF-8")
# names(GcResults)[1] <- "DataName"

# Extract the date from the filename
GcResults$DataName <- gsub('.{4}$', '', GcResults$File)
# Rename column 
#colnames(GcResults)[colnames(GcResults) == 'File'] <- 'DataName'
# change to numeric
GcResults$DataName <- as.numeric(GcResults$DataName)

# join GC results to metadata
CombinedResults <- full_join(Metadata,GcResults)

# peak area ratios
CombinedResults$ratio <- CombinedResults$PA/CombinedResults$I.S.PA


# To read simulated data
# CombinedResults <-read.csv2("Results/CombinedResultsFALSE.csv",header = T, sep = ",")

#Determine how many calibrations are in the data
# calibration range
calibration <- c("25","50","75","100","150","200","250")

#convert to a dataframe
calibration <- as.data.frame(calibration)
calibration$calibration <- as.numeric(calibration$calibration)

#Number of calibration standards
CalLevels <- nrow(calibration)

NumCal <- as.list(1:(nrow(calibrationValues <- CombinedResults %>%
                 filter(CombinedResults$Type=="Cal"))/CalLevels))

CalibrationValues <- CombinedResults %>%
  filter(CombinedResults$Type=="Cal")

for (d in NumCal){
  if(d == 1){
    CalibrationValues$CalNum <-   assign(paste0("Cal", d), CombinedResults[which(CombinedResults$SampleType=='Cal', arr.ind=TRUE)[d:CalLevels],][1,1])
  }
  
  
  else{Num <- (CalLevels*d-(CalLevels-1))
  assign(paste0("Cal", d), CombinedResults[which(CombinedResults$SampleType=='Cal', arr.ind=TRUE)[Num:(Num+6)],])}
}

Test1 <- data.frame(matrix(ncol = 2, nrow = 0))
names(Test1)[1] <- "CalStart"
names(Test1)[2] <- "CalEnd"
TestVar1 <- as.numeric("-1")
TestVar2 <- as.numeric("-1")

for (d in 1:nrow(CombinedResults)){
  if (CombinedResults[d,3]=='Cal' && TestVar1 == "-1"){
  TestVar1 <- d}
  else if (CombinedResults[d,3] !='Cal' && TestVar1 != "-1"){
    Test1[d,1] <- TestVar1
    Test1[d,2] <- d-1
    TestVar1 <- "-1"
  }
}

Test1 <- Test1[complete.cases(Test1),]





for (d in NumCal){
  if(d == 1){
  CalibrationValues$CalNum <-   assign(paste0("Cal", d), CombinedResults[which(CombinedResults$SampleType=='Cal', arr.ind=TRUE)[d:CalLevels],])
  }
  
  
  else{Num <- (CalLevels*d-(CalLevels-1))
    assign(paste0("Cal", d), CombinedResults[which(CombinedResults$SampleType=='Cal', arr.ind=TRUE)[Num:(Num+6)],])}
}

#For x in 1:nrow(Test1){
 # Test1[x,]

CalList <- as.list("1","2")

CalPartition <- list()
  
for (d in 1:nrow(Test1)){
CalPartition <- (CombinedResults[Test1[d,1]:Test1[d,2],])
if (exists("CalSelection")){
  CalSelection[[length(CalSelection)+1]] <- CalPartition
}
else {CalSelection <- as.list(CalPartition)}
}



#####for (d in NumCal){
  # filter the calibration out of data
  calibrationValues <- CombinedResults[Cal"d",] %>%
    filter(CombinedResults$Type=="Cal")
######}
  # bind calibration range and GC output together
  CalibrationData <- cbind(calibration,calibrationValues)
  
  # determine the quadratic fit 
  model <- lm(CalibrationData$ratio ~ poly(CalibrationData$calibration, degree = 2, raw = T))
  
  # extract the values from list
  QuadraticValues <- model[[1]]
  
  # convert to dataframe
  QuadraticValues <- as.data.frame(QuadraticValues)
  
  
}




# solving for x the quadractic equation: y = c*x^2 + b*x + a
# x = (±(b^2+4c(y−a))^(1/2)−b)/2c
# The order from the list [1], [2] and [3] are for a, b and c
# only the positive part of the equation is considered
CombinedResults$ValuesPositive <- (((QuadraticValues$QuadraticValues[2])^2 + 4*QuadraticValues$QuadraticValues[3]*(CombinedResults$ratio-QuadraticValues$QuadraticValues[1]))^(1/2)-QuadraticValues$QuadraticValues[2])/(2*QuadraticValues$QuadraticValues[3])

CalibrationData$ValuesPositive <- (((QuadraticValues$QuadraticValues[2])^2 + 4*QuadraticValues$QuadraticValues[3]*(CalibrationData$ratio-QuadraticValues$QuadraticValues[1]))^(1/2)-QuadraticValues$QuadraticValues[2])/(2*QuadraticValues$QuadraticValues[3])

# filter out the sample only results
sample.data <- CombinedResults %>%
  filter(CombinedResults$Type =="Sample")

# plot the results for the metadata
p<- ggplot() + 
  geom_point(data=CalibrationData, aes(x=calibration, y=ratio), colour="red") + 
  geom_line(data=CalibrationData, aes(x=ValuesPositive, y=ratio), colour="green") +
  geom_point(data=sample.data, aes(x = ValuesPositive, y=ratio), colour="black", size=2) +
  theme_bw() +
  ylab("Etizolam - Internal Standard peak area ratio / arb. unit") +
  xlab(bquote("Concentration Etizolam /" ~mu~g%.%mL^{-1})) +
  theme(text = element_text(size = 12))

 show(p)
# save figures in output folder in metadata
filenameMetadata <- gsub('\\..*', '', filenameMetadata)
filenameMetadata <- gsub('\\//*', '-', filenameMetadata)
ggsave(
  sprintf("%s.tiff",filenameMetadata),
  plot = p,
  device = NULL,
  path = file.path(MetadataOutput.dir),
  scale = 1,
  width = 7.5,
  height = 5.0,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE
)

#Add columns to account for dilution, convert to mg and calculate total etizolam in sample 
CombinedResults$CorrectedConcentration <- CombinedResults$ValuesPositive * CombinedResults$Dilution
CombinedResults$ConcentrationMg <- CombinedResults$CorrectedConcentration /1000
CombinedResults$SampleTotal <- CombinedResults$ConcentrationMg * CombinedResults$Volume

# If a tablet or powder calculate % Etizolam in total sample and total Etizolam (in mg) in original Tablet/Powder
CombinedResults$EtizolamPercentage <- NA
CombinedResults$TotalEtizolam <- NA
for (i in 1:nrow(CombinedResults)) {
  a <-  CombinedResults[i,2]
  if (a == "Powder" | a == "Tablet") {
    CombinedResults$EtizolamPercentage[i] <- (CombinedResults$SampleTotal[i]/CombinedResults$QuantWeight[i])*100
    CombinedResults$TotalEtizolam <- (CombinedResults$TotalWeight/100)*CombinedResults$EtizolamPercentage
    i <- i+1
  } 
  
}

# This is to create the export of the results
# If previous series of data already exists, an archive copy will be saved and
# the new data file including the latest results will be created

# file to check if it is present in the designated folder
filename_data <- paste0(Results.dir,'GCMSResultsCardData.csv')

if(file.exists(filename_data)){
  
  # read file
  ProcessedData <- read.csv(filename_data)
  
  ###### Saving an archived copy of the library #####
  filename.date = paste(gsub(":", "-", Sys.time()),"_GCMSResults.csv",sep="")
  
  # to write the archive using system date
  write.csv(ProcessedData, file=paste0(Backup.dir,filename.date), row.names = F)
  
  # Combined the existing results to the new one
  CombinedData <- rbind(ProcessedData,CombinedResults)
  
  write.table(CombinedData,file = paste0(Results.dir,"GCMSResultsCardData.csv"),  sep = ",", row.names = F)
  } else {
    write.table(CombinedResults,file = paste0(Results.dir,"GCMSResultsCardData.csv"),  sep = ",", row.names = F)
    }

print("Processing complete. Please check 'Results' folder for output")
