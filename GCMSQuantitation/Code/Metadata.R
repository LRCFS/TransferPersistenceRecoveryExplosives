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
CombinedResults$ratio <- as.numeric(CombinedResults$PA/CombinedResults$I.S.PA)

# To read simulated data
#CombinedResults <-read.csv2("Results/CombinedResultsFALSE.csv",header = T, sep = ",")
CombinedResults$ratio <- as.numeric(CombinedResults$ratio)

#Determine how many calibrations are in the data
# calibration range
calibration <- c("25","50","75","100","150","200","250")

#convert to a dataframe
calibration <- as.data.frame(calibration)
calibration$calibration <- as.numeric(calibration$calibration)

#Number of calibration standards
CalLevels <- nrow(calibration)

NumCal <- as.list(1:(nrow(CalibrationValues <- CombinedResults %>%
                 filter(CombinedResults$Type=="Cal"))/CalLevels))

Cals <- list()

for (i in NumCal){
  Num <- (CalLevels*i-(CalLevels-1))
  Cals[[i]] <- CombinedResults[which(CombinedResults$SampleType=='Cal', arr.ind=TRUE)[Num:(Num+6)],]
  CalData <- Cals[[i]]
  Cals[[i]] <- cbind(calibration,CalData)
  # determine the quadratic fit 
  model <- lm(Cals[[i]]$ratio ~ poly(Cals[[i]]$calibration, degree = 2, raw = T))
  
  # extract the values from list
  QuadraticValues <- model[[1]]
  
   if (exists("AllQuadraticValues")){
      AllQuadraticValues[[i]] <- as.data.frame(QuadraticValues)
      } else {AllQuadraticValues <- as.list("NA")
       AllQuadraticValues[[i]] <- as.data.frame(QuadraticValues)
       }
  }
  #Associate the data with the correct calibration
  for (i in NumCal){ 
   if (i < length(NumCal)) {  
      if (exists("SampleBrackets")) {
      SampleBrackets[[i]] <- as.data.frame(CombinedResults[(Cals[[i]][CalLevels,2]+1):(Cals[[i+1]][1,2]-1), ])
      } else {
        SampleBrackets <- list()
        SampleBrackets[[i]] <- as.data.frame(CombinedResults[(Cals[[i]][CalLevels,2]+1):(Cals[[i+1]][1,2]-1), ])
      }
    } else { 
      if (exists("SampleBrackets")) {
          SampleBrackets[[i]] <- as.data.frame(CombinedResults[(Cals[[i]][CalLevels,2]+1):nrow(CombinedResults), ])
    } else {
      SampleBrackets <- as.list("NA")
      SampleBrackets[[i]] <- as.data.frame(CombinedResults[(Cals[[i]][CalLevels,2]+1):nrow(CombinedResults), ])     } 
    }
  }


# solving for x the quadractic equation: y = c*x^2 + b*x + a
# x = (±(b^2+4c(y−a))^(1/2)−b)/2c
# The order from the list [1], [2] and [3] are for a, b and c
# only the positive part of the equation is considered

for (i in NumCal){
SampleBrackets[[i]]$ValuesPositive <- (((AllQuadraticValues[[i]][2,1])^2 + 4*(AllQuadraticValues[[i]][3,1])*(SampleBrackets[[i]]$ratio-(AllQuadraticValues[[i]][1,1])))^(1/2)-(AllQuadraticValues[[i]][2,1]))/(2*(AllQuadraticValues[[i]][3,1]))

Cals[[i]]$ValuesPositive <- (((AllQuadraticValues[[i]][2,1])^2 + 4*(AllQuadraticValues[[i]][3,1])*(Cals[[i]]$ratio-(AllQuadraticValues[[i]][1,1])))^(1/2)-(AllQuadraticValues[[i]][2,1]))/(2*(AllQuadraticValues[[i]][3,1]))

if (i==1){
  CombinedResults <- bind_rows(SampleBrackets[[i]],Cals[[i]])
} else {
  CombinedResults <- bind_rows(CombinedResults, SampleBrackets[[i]], Cals[[i]])
}

CombinedResults <- CombinedResults[order(CombinedResults$X), ]
CombinedResults <- CombinedResults[complete.cases(CombinedResults[ , c('ValuesPositive')]), ] 
SampleBrackets[[i]] <- SampleBrackets[[i]][complete.cases(SampleBrackets[[i]][ , c('ValuesPositive')]), ] 

# plot the results for the metadata
p<- ggplot() + 
  geom_point(data=Cals[[i]][1:CalLevels,], aes(x=calibration, y=ratio), colour="red") + 
  geom_line(data=Cals[[i]][1:CalLevels,], aes(x=ValuesPositive, y=ratio), colour="green") +
  geom_point(data=SampleBrackets[[i]], aes(x = ValuesPositive, y=ratio), colour="black", size=2) +
  theme_bw() +
  ylab("Etizolam - Internal Standard peak area ratio / arb. unit") +
  xlab(bquote("Concentration Etizolam /" ~mu~g%.%mL^{-1})) +
  theme(text = element_text(size = 12))

 show(p)
# save figures in output folder in metadata
filenameMetadata <- gsub('\\..*', '', filenameMetadata)
filenameMetadata <- gsub('\\//*', '-', filenameMetadata)
ggsave(
  sprintf(paste0(i,"%s.tiff"),filenameMetadata),
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
}

#Add columns to account for dilution, convert to mg and calculate total etizolam in sample 
CombinedResults$CorrectedConcentration <- CombinedResults$ValuesPositive * CombinedResults$Dilution
CombinedResults$ConcentrationMg <- CombinedResults$CorrectedConcentration /1000
CombinedResults$SampleTotal <- as.numeric(CombinedResults$ConcentrationMg) * as.numeric(CombinedResults$Volume)

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
