#
# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# This code will convert the MS1 files to a readable format and save them in a folder called in the subsequent code 
# 
# Generate file list for Proteo Wizard/MsConvert
#FilestoConvert <- list.files("Example Data", full.names = FALSE)\
#write.table(FilestoConvert, file = "files.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################
#####################################################################
#####                      Convert TIC Files                    #####
#####################################################################
# Load Metadata files
filenameGcDataConverterMs <- list.files(GcDataConverterMs.dir, extensionMS1, full.names=TRUE)

# Count how many files have already been processed (both TIC and SIM outputs exist)
already_processed <- sapply(filenameGcDataConverterMs, function(f) {
  name <- gsub(".*/", "", gsub(extensionMS1, "", f))
  file.exists(paste0(GcDataConvertedRcodeTIC.dir, name, ".csv")) &&
    file.exists(paste0(GcDataConvertedRcodeSIM.dir, name, ".csv"))
})
n_total <- length(filenameGcDataConverterMs)
n_skip  <- sum(already_processed)
print(paste0("MsFilesReorganiser: ", n_total, " ms1 files found, ",
             n_skip, " already processed, ", n_total - n_skip, " to process"))

for (file in filenameGcDataConverterMs) {
DataName <- gsub(extensionMS1, "", file)

# for testing code on a single file (first on list) in filenameGcDataConverterMsTIC
#DataName <- gsub(extensionMS1, "", filenameGcDataConverterMsTIC[2])

DataName <- gsub(".*/", "", DataName)

# Check if this file has already been processed (both TIC and SIM outputs exist)
tic_output <- paste0(GcDataConvertedRcodeTIC.dir, DataName, ".csv")
sim_output <- paste0(GcDataConvertedRcodeSIM.dir, DataName, ".csv")
if (file.exists(tic_output) && file.exists(sim_output)) {
  print(paste0("Skipping ", DataName, " -- already processed"))
  next
}

#####################################################################
#####  re-organise converted data from  Proteo Wizard MsConvert #####
#####################################################################
# for testing code on a single file (first on list) in filenameGcDataConverterMsTIC
# GcDataConverter <- read.csv2(filenameGcDataConverterMsTIC[2], sep = "\t", header = FALSE)

GcDataConverter <- read.csv2(file, sep = "\t", header = FALSE)

ConvertedData <- strsplit(as.character(GcDataConverter$V1),' ') 
#do.call(rbind, ConvertedData)

ConvertedData <-data.frame(GcDataConverter,do.call(rbind,ConvertedData))
# remove rows containing "H"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V1, 'H'))

# remove rows containing "NativeID"  
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V2, 'NativeID'))

# remove rows containing "BPI"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V2, 'BPI'))

# remove rows containing "BPM"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V2, 'BPM'))

# replace blank with "NA"
ConvertedData$V3 <- as.numeric(ConvertedData$V3)

# fill cells with previous values in the same column
ConvertedData <- ConvertedData %>%
  fill(V3)
# remove rows containing "TIC"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(ConvertedData$V2, "TIC"))

# replace blank with "NA"
ConvertedData$V2[ConvertedData$V2==""]<-0

n <- nrow(ConvertedData)

# fill cells with values taken in different column
for(i in 1:nrow(ConvertedData)) { # for-loop over rows
  m <- ConvertedData[i,2]
  if (m =="RTime") {
    Rtime <- ConvertedData[i,3]
    i <- i+1
    q <- ConvertedData[i,2]
    
    while (q == "0" & i <= n ) {
      ConvertedData[i,2] <- Rtime
      i <- i+1
      q <- ConvertedData[i,2]
      }
  }
  i <- i+1
}
# remove rows containing "S"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V1, 'S'))

# remove rows containing "S"
ConvertedData <- ConvertedData %>% 
  filter(!str_detect(V1, 'I'))

DataHeatMap <- ConvertedData %>%
  select(-V1)

names(DataHeatMap)[1] <- "RetentionTime"
names(DataHeatMap)[2] <- "TIC"
names(DataHeatMap)[3] <- "Mass"
names(DataHeatMap)[4] <- "Intensity"

# convert all columns to numeric
DataHeatMap <- DataHeatMap %>% mutate(across(everything(), ~as.numeric(as.character(.x))))

# convert the retention time to sec
DataHeatMap$RetentionTime <- DataHeatMap$RetentionTime *60

#Strip out TIC data
DataHeatMapTIC <- DataHeatMap %>%
  select(RetentionTime,TIC) %>%
  unique()

# write the TIC files in the converted folder
write.csv(DataHeatMapTIC, file=paste0(GcDataConvertedRcodeTIC.dir,DataName,".csv"), row.names = F)

#Strip out the SIM data
DataHeatMapSIM <- DataHeatMap %>%
  select(RetentionTime,Mass,Intensity)

# write the SIM files in the converted folder
write.csv(DataHeatMapSIM, file=paste0(GcDataConvertedRcodeSIM.dir,DataName,".csv"), row.names = F)

print(paste0(file," complete",now()))
}
