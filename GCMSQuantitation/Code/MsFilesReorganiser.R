#
# Prior running, GC-MS data must be converted using Proteo Wizard/ MsConvert
# This code will convert the MS1 files to a readable format and save them in a folder called in the subsequent code 
# 
# Generate file list for Proteo Wizard/MsConvert
FilestoConvert <- list.files("Example Data", full.names = FALSE)
write.table(FilestoConvert, file = "files.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################
# Load Metadata files
filenameGcDataConverterMs <- list.files(GcDataConverterMs.dir, extensionMS1, full.names=TRUE)

for (file in filenameGcDataConverterMs) {
DataName <- gsub(extensionMS1, "", file)

# for testing code on a single file (first on list) in filenameGcDataConverterMs
# DataName <- gsub(extensionMS1, "", filenameGcDataConverterMs[2])

DataName <- gsub(".*/", "", DataName)

#####################################################################
#####  re-organise converted data from  Proteo Wizard MsConvert #####
#####################################################################
# for testing code on a single file (first on list) in filenameGcDataConverterMs
# GcDataConverter <- read.csv2(filenameGcDataConverterMs[2], sep = "\t", header = FALSE)

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

# replace blanck with "NA"
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
DataHeatMap <- mutate_all(DataHeatMap, function(x) as.numeric(as.character(x)))

# convert the retention time to sec
DataHeatMap$RetentionTime <- DataHeatMap$RetentionTime *60

# write the files in the converted folder
write.csv(DataHeatMap, file=paste0(GcDataConvertedRcode.dir,DataName,".csv"), row.names = F)
}
