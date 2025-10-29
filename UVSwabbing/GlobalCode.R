###Run this to prepare files for image analysis####
# Load necessary libraries

library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)

#Specify the directory of the threshold data
DataFolder.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/FEL Swabbing Study/"

#Specify where the output data is saved
#For Blank analysis
BlankThresholdResults.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/FEL Swabbing Study/Blank Threshold Analysis"
dir.create(file.path(BlankThresholdResults.dir),recursive = TRUE) # will create folder if not already there.

#For Before swabbing analysis
BeforeThresholdResults.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/FEL Swabbing Study/Unswabbed Threshold Analysis"
dir.create(file.path(BeforeThresholdResults.dir),recursive = TRUE) # will create folder if not already there.

####Read results from directory####
filenameFolders <- list.dirs(path = DataFolder.dir, full.names = TRUE, recursive = FALSE)
filenameThresholdResults <- filenameFolders[grepl("Participant", basename(filenameFolders))]
for (i in filenameThresholdResults) {
  participant <- sub(".* ", "", i)
  tempfile <- list.files(i, pattern = "Summary.csv", recursive = TRUE)
  
  for (j in tempfile) {
    version <- sub("/.*", "", j)
    tempresults <- read.csv(paste0(i, "/", j)) %>%
      rename(Area = X.Area) %>%
      rename(Threshold = Slice) %>%
      select(Threshold, Area)
    
    tempresults$Threshold <- str_sub(tempresults$Threshold, 10, -6)
    
    # Split results by photograph
    Blanktempresults <- tempresults[1:100, ]
    Beforetempresults <- tempresults[101:200, ]
    
    write.csv(
      Blanktempresults,
      paste0(BlankThresholdResults.dir, "/", participant, "_", version, "_Blank.csv"),
      row.names = FALSE
    )
    
    write.csv(
      Beforetempresults,
      paste0(BeforeThresholdResults.dir, "/", participant, "_", version, "_Before.csv"),
      row.names = FALSE
    )
  }
}
