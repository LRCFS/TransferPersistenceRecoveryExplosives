### Split Summary.csv into Blank and Before files for analysis ###
# This script reads the combined Summary.csv files and splits them
# into separate Blank and Before files for individual analysis
#
# PREREQUISITE:
#   1. Run 00-GlobalCode.R first
#   2. Run 02-CombineResults.R to create Summary.csv files

####Read results from directory####
filenameFolders <- list.dirs(path = ThresholdResults.dir, full.names = TRUE, recursive = FALSE)

##Extract Blank and Before Results from summary files
for (i in filenameFolders) {
  surfacerep <- basename(i)
  tempfile <- list.files(i, pattern = "Summary.csv", recursive = TRUE)
  
  for (j in tempfile) {
    version <- sub("/.*", "", j)
    tempresults <- read.csv(paste0(i, "/", j)) %>%
      rename(Area = X.Area) %>%
      select(Area)
    
    # Add Threshold values (100:1 for each segment)
    # Summary.csv has 300 rows: 1-100=Blank, 101-200=Before, 201-300=After
    tempresults$Threshold <- rep(100:1, 3)
    tempresults <- tempresults[, c("Threshold", "Area")]
    
    # Split results by photograph
    Blanktempresults <- tempresults[1:100, ]
    Beforetempresults <- tempresults[101:200, ]
    
    write.csv(
      Blanktempresults,
      paste0(BlankThresholdResults.dir, "/", surfacerep, "_Blank.csv"),
      row.names = FALSE
    )
    
    write.csv(
      Beforetempresults,
      paste0(BeforeThresholdResults.dir, "/", surfacerep, "_Before.csv"),
      row.names = FALSE
    )
    
    cat(sprintf("Processed: %s\n", surfacerep))
  }
}

cat("\n=== COMPLETE ===\n")
cat(sprintf("Blank files saved to: %s\n", BlankThresholdResults.dir))
cat(sprintf("Before files saved to: %s\n", BeforeThresholdResults.dir))
