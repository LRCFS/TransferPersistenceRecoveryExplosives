
#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Load MetaData Files

# Read the log file

# List all .txt files in the directory
log_file <- list.files(path = DataFolder, pattern = "Sequence", full.names = TRUE)[1]

# Read the log file
log_lines <- readLines(log_file)

# Extract the date from the "Starting sequence" line
date_line <- log_lines[grepl("Starting sequence", log_lines)]
date <- str_extract(date_line, "\\w+ \\w+ \\d+ \\d{2}:\\d{2}:\\d{2} \\d{4}")

# Find lines that match the metadata entries
metadata_lines <- log_lines[grepl("^\\s*\\d+\\)", log_lines)]

# Parse each line into components
parsed_data <- lapply(metadata_lines, function(line) {
  matches <- str_match(line, "^\\s*(\\d+)\\)\\s+(\\w+)\\s+(\\d+)\\s+(\\d+)\\s+(.*)$")
  if (!is.na(matches[1])) {
    sample_name <- matches[6]
    
    # Extract calibration level if it's a Cal type
    cal_level <- NA
    if (matches[3] == "Cal") {
      cal_level_match <- str_match(sample_name, "(\\d+(\\.\\d+)?)ng")
      if (!is.na(cal_level_match[1])) {
        cal_level <- as.numeric(cal_level_match[2])
      }
    }
    
    return(data.frame(
      Date = date,
      Line = as.integer(matches[2]),
      Type = matches[3],
      Vial = as.integer(matches[4]),
      DataFile = matches[5],
      SampleName = sample_name,
      CalLevel = cal_level,
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
})

# Combine all parsed rows into a single dataframe
Metadata <- do.call(rbind, parsed_data)
Metadata$DataFile <- paste0(Metadata$DataFile, "Summary.csv")
Metadata$Date <- as.POSIXct(Metadata$Date, format = "%a %b %d %H:%M:%S %Y", tz = "UTC")
Metadata$Date <- format(Metadata$Date, "%Y%m%d")

# Load GC results
filenameGcData <- list.files(GcData.dir, pattern = "\\Summary.csv$", full.names = TRUE)

# Get all column names across files
ColNames <- unique(unlist(lapply(filenameGcData, function(x) names(read.csv(x)))))

# Read and align each file
GcResults <- do.call(rbind, lapply(filenameGcData, function(x) {
  df <- read.csv(x)
  df$File <- basename(x)
  missing_cols <- setdiff(ColNames, names(df))
  df[missing_cols] <- NA
  df <- df[ColNames]
  return(df)
}))

# Join GC results to metadata
CombinedResults <- full_join(Metadata, GcResults, by = join_by(DataFile == File))

#CombinedResults[8,9] <- CombinedResults[8,9]/2
#CombinedResults[9,9] <- CombinedResults[9,9]/2
#CombinedResults <- CombinedResults[1:36,]

# Peak area ratios
CombinedResults$ratio <- as.numeric(CombinedResults$PETN.PA / CombinedResults$I.S.PA)
CombinedResults$Row <- 1:nrow(CombinedResults)

# Filter calibration data
CalibrationValues <- CombinedResults %>%
  filter(Type == "Cal")

CalLevels <-  length(unique(CalibrationValues[["CalLevel"]]))

if (CalLevels != 0){
  
  #Get calibration concentrations
  original_conc <- unique(CalibrationValues[["CalLevel"]])
  
  # Adjust calibration concentrations based on dilution factor given in Global Code
  #adjusted_conc <- original_conc * Dilution
  #calibration <- data.frame(calibration = adjusted_conc)
  #CalLevels <- nrow(calibration)
  
  # Calculate number of calibration sets
  NumCal <- seq_len(nrow(CalibrationValues) / CalLevels)
  
  Cals <- list()
  AllQuadraticValues <- list()
  
  for (i in NumCal) {
    Num <- (CalLevels * i - (CalLevels - 1))
    Cals[[i]] <- CalibrationValues[Num:(Num + CalLevels - 1), ]
    #Get calibration concentrations
    original_conc <- (Cals[[i]][["CalLevel"]])
    
    # Adjust calibration concentrations based on dilution factor given in Global Code
    adjusted_conc <- original_conc * Dilution
    CalLevel <- data.frame(CalLevel = adjusted_conc)
    
    CalLevels <- nrow(CalLevel)
    Cals[[i]][,7] <- CalLevel
    Cals[[i]]$CalibrationSet <- i
    
    # Fit quadratic model
    model <- lm(ratio ~ CalLevel + I(CalLevel^2), data = Cals[[i]])
    AllQuadraticValues[[i]] <- coef(model)
  }
  
  # Associate the data with the correct calibration
  SampleBrackets <- list()
  
  if (length(Cals) == 1) {
    start_row <- max(Cals[[1]]$Row) + 1
    end_row <- nrow(CombinedResults)
    
    cat("Bracket 1: rows", start_row, "to", end_row, "\n")
    SampleBrackets[[1]] <- CombinedResults[start_row:end_row, ]
    SampleBrackets[[1]]$CalibrationSet <- 1
  } else {
    for (i in seq_along(Cals)) {
      start_row <- max(Cals[[i]]$Row) + 1
      
      if (i < length(Cals)) {
        end_row <- min(Cals[[i + 1]]$Row) - 1
        cat("Bracket", i, ": rows", start_row, "to", end_row, "\n")
        SampleBrackets[[i]] <- CombinedResults[start_row:end_row, ]
      } else {
        if (start_row > nrow(CombinedResults)) {
          cat("Bracket", i, ": no data after last calibration\n")
          SampleBrackets[[i]] <- CombinedResults[0, ]  # Create empty bracket with correct structure
        } else {
          end_row <- nrow(CombinedResults)
          cat("Bracket", i, ": rows", start_row, "to", end_row, "\n")
          SampleBrackets[[i]] <- CombinedResults[start_row:end_row, ]
        }
      }
      
      if (nrow(SampleBrackets[[i]]) > 0){
        SampleBrackets[[i]]$CalibrationSet <- i
      }
    }
  }
  
  # Solve quadratic and apply to samples
  solve_quadratic <- function(y, a, b, c) {
    if (is.na(y) || is.na(a) || is.na(b) || is.na(c)) return(NA)
    discriminant <- b^2 - 4 * a * (c - y)
    if (is.na(discriminant) || discriminant < 0) return(NA)
    x1 <- (-b + sqrt(discriminant)) / (2 * a)
    x2 <- (-b - sqrt(discriminant)) / (2 * a)
    return(ifelse(x1 >= 0, x1, x2))
  }
  
  for (i in NumCal) {
    QuadraticValues <- AllQuadraticValues[[i]]
    a <- as.numeric(QuadraticValues["I(CalLevel^2)"])
    b <- as.numeric(QuadraticValues["CalLevel"])
    c <- as.numeric(QuadraticValues["(Intercept)"])
    
    SampleBrackets[[i]]$ValuesPositive <- sapply(SampleBrackets[[i]]$ratio, solve_quadratic, a = a, b = b, c = c)
    Cals[[i]]$ValuesPositive <- sapply(Cals[[i]]$ratio, solve_quadratic, a = a, b = b, c = c)
    
    if (nrow(SampleBrackets[[i]])) {
      QuantResults <- bind_rows(SampleBrackets[[i]], Cals[[i]]) %>%
        select(DataFile, ValuesPositive, CalLevel, CalibrationSet)
    } else {
      QuantResults <- Cals[[i]] %>%
        select(DataFile, ValuesPositive, CalLevel, CalibrationSet)
    }
    CombinedResults <- full_join(CombinedResults, QuantResults, by = "DataFile")
    SampleBrackets
    
    if ("ValuesPositive.x" %in% names(CombinedResults)) {
      CombinedResults <- CombinedResults %>%
        mutate(
          ValuesPositive = coalesce(ValuesPositive.x, ValuesPositive.y),
        ) %>%
        select(-ValuesPositive.x, -ValuesPositive.y)
    }
    
    if ("CalLevel.x" %in% names(CombinedResults)) {
      CombinedResults <- CombinedResults %>%
        mutate(
          CalLevel = coalesce(CalLevel.y, CalLevel.x),
        ) %>%
        select(-CalLevel.x, -CalLevel.y)
    }
    
    if ("CalibrationSet.x" %in% names(CombinedResults)) {
      CombinedResults <- CombinedResults %>%
        mutate(
          CalibrationSet = coalesce(CalibrationSet.x, CalibrationSet.y),
        ) %>%
        select(-CalibrationSet.x, -CalibrationSet.y)
    }
    
    # Reorder columns
    CombinedResults <- CombinedResults %>%
      relocate("ratio", .after = "PETN.PA") %>%
      relocate("ValuesPositive", .after = "ratio") %>%
      relocate("CalibrationSet", .after = "ValuesPositive")
    
    CombinedResults <- CombinedResults[order(CombinedResults$DataFile), ]
    if (!all(is.na(SampleBrackets[[i]]$ValuesPositive))){  
      # Plot the results
      p <- ggplot() +
        geom_point(data = Cals[[i]][1:CalLevels, ], aes(x = CalLevel, y = ratio), colour = "red") +
        geom_line(data = Cals[[i]][1:CalLevels, ], aes(x = ValuesPositive, y = ratio), colour = "black") +
        geom_point(data = SampleBrackets[[i]], aes(x=ValuesPositive, y = ratio, colour = Type))+
        theme_bw() +
        ylab("PETN - Internal Standard peak area ratio / arb. unit") +
        xlab(bquote("Concentration PETN /" ~ mu * g %.% mL^{-1})) +
        theme(text = element_text(size = 12))
      
      show(p)
    }else{
      # Plot the results
      p <- ggplot() +
        geom_point(data = Cals[[i]][1:CalLevels, ], aes(x = CalLevel, y = ratio), colour = "red") +
        geom_line(data = Cals[[i]][1:CalLevels, ], aes(x = ValuesPositive, y = ratio), colour = "black") +
        theme_bw() +
        ylab("PETN - Internal Standard peak area ratio / arb. unit") +
        xlab(bquote("Concentration PETN /" ~ mu * g %.% mL^{-1})) +
        theme(text = element_text(size = 12))  
      
      show(p)
    }
    
    ggsave(
      paste0("Calibration Results_", i, "_.png"),
      plot = p,
      path = file.path(MetadataOutput.dir),
      width = 7.5,
      height = 5.0,
      dpi = 300
    )
  }
  
  # Save model statistics to CSV
  ModelStats <- data.frame(
    CalibrationSet = integer(),
    Intercept = numeric(),
    Linear = numeric(),
    Quadratic = numeric(),
    R_Squared = numeric(),
    Adj_R_Squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in NumCal) {
    model <- lm(ratio ~ CalLevel + I(CalLevel^2), data = Cals[[i]])
    coefs <- coef(model)
    stats <- summary(model)
    
    ModelStats[i, ] <- list(
      CalibrationSet = i,
      Intercept = coefs["(Intercept)"],
      Linear = coefs["CalLevel"],
      Quadratic = coefs["I(CalLevel^2)"],
      R_Squared = stats$r.squared,
      Adj_R_Squared = stats$adj.r.squared
    )
  }
  
  write.csv(ModelStats, file = file.path(Results.dir, "CalibrationModelStats.csv"), row.names = FALSE)
  
  # Plot all calibration curves on one plot
  AllCalData <- do.call(rbind, lapply(seq_along(Cals), function(i) {
    df <- Cals[[i]]
    df$Set <- paste0("Cal ", i)
    df
  }))
  
  AllCalData$Fitted <- NA
  for (i in seq_along(Cals)) {
    model <- lm(ratio ~ CalLevel + I(CalLevel^2), data = Cals[[i]])
    AllCalData$Fitted[AllCalData$Set == paste0("Cal ", i)] <- predict(model, newdata = Cals[[i]])
  }
  
  p_combined <- ggplot(AllCalData, aes(x = CalLevel, y = ratio, color = Set)) +
    geom_point() +
    geom_line(aes(y = Fitted)) +
    theme_bw() +
    labs(
      title = "Combined Calibration Curves",
      x = expression("Calibration Concentration (ng/"*mu*"L)"),
      y = "Peak Area Ratio"
    ) +
    theme(text = element_text(size = 12))
  
  print(p_combined)
  ggsave(
    filename = "Combined_Calibration_Curves.png",
    plot = p_combined,
    path = Results.dir,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Add columns to account for dilution, convert to mg and calculate total PETN in sample
  CombinedResults <- CombinedResults %>%
    rename(Concentration = ValuesPositive)
  CombinedResults$SampleConc <- CombinedResults$Concentration / Dilution
  CombinedResults$MassinSample <- CombinedResults$SampleConc * SampleVol
  CombinedResults$PercentRecovery <- CombinedResults$MassinSample / DepositMass * 100
  
  #Reorder columns
  CombinedResults <- CombinedResults %>%
    relocate("CalLevel", .after = "ratio") %>%
    relocate("SampleConc", .after = "CalibrationSet") %>%
    relocate("MassinSample", .after = "SampleConc") %>%
    relocate("PercentRecovery", .after = "MassinSample") %>%
    arrange(SampleName)
  
}

# Export results: append new data to existing file if it exists
filename_data <- file.path(Results.dir, paste0(ParentFolder, "_GCMSResults.csv"))

if (file.exists(filename_data)) {
  # Read existing data
  ExistingData <- read.csv(filename_data)
  CombinedResults$Date <- as.integer(CombinedResults$Date)
  
  # Combine with new results
  CombinedData <- bind_rows(ExistingData, CombinedResults)
  
  # Optional: remove duplicates based on DataFile
  CombinedData <- CombinedData %>%
    distinct(DataFile, .keep_all = TRUE)
  
  # Backup old file
  filename.date <- paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_GCMSResults.csv")
  write.csv(ExistingData, file = file.path(Backup.dir, filename.date), row.names = FALSE)
  
  # Save updated data
  write.csv(CombinedData, file = filename_data, row.names = FALSE)
  
} else {
  # Save new results if file doesn't exist
  write.csv(CombinedResults, file = filename_data, row.names = FALSE)
}

# Save ResponseCheck results to master spreadsheet
if ("QC" %in% CombinedResults$Type){
  QCResults <- CombinedResults %>%
    filter(Type == "QC") %>%
    select(Date, DataFile, CalLevel,SampleConc, Q.P.IS, I.S.PA, Q.P.PETN, PETN.PA)
  
  QC_file <- file.path("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/System Monitoring", "SystemMonitoring.csv")
  
  if (file.exists(QC_file)) {
    ExistingQC <- read.csv(QC_file)
    QCResults$Date <- as.integer(QCResults$Date)
    
    # Combine and remove duplicates
    CombinedQC <- bind_rows(ExistingQC, QCResults) %>%
      distinct(DataFile, .keep_all = TRUE)
    
    # Backup old file
    qc_backup <- paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_QC_Master.csv")
    write.csv(ExistingQC, file = file.path(Backup.dir, qc_backup), row.names = FALSE)
    
    # Save updated file
    write.csv(CombinedQC, file = QC_file, row.names = FALSE)
    
  } else {
    # Save new file if it doesn't exist
    write.csv(QCResults, file = QC_file, row.names = FALSE)
  }
  
  # # Save ResponseCheck results to master spreadsheet
  # QCResults <- CombinedResults %>%
  #   filter(Type == "QC") %>%
  #   select(Date, DataFile, CalLevel,SampleConc, Q.P.IS, I.S.PA, Q.P.PETN, PETN.PA)
  # 
  # QC_file <- file.path("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/GC Data/System Monitoring", "QCMonitoring.csv")
  # 
  # if (file.exists(QC_file)) {
  #   ExistingQC <- read.csv(QC_file)
  #   
  #   # Combine and remove duplicates
  #   CombinedQC <- bind_rows(ExistingQC, QCResults) %>%
  #     distinct(DataFile, .keep_all = TRUE)
  #   
  #   # Backup old file
  #   qc_backup <- paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_QC_Master.csv")
  #   write.csv(ExistingQC, file = file.path(Backup.dir, qc_backup), row.names = FALSE)
  #   
  #   # Save updated file
  #   write.csv(CombinedQC, file = QC_file, row.names = FALSE)
  #   
  # } else {
  #   # Save new file if it doesn't exist
  #   write.csv(QCResults, file = QC_file, row.names = FALSE)
  #}
}

#Look at PETN PA
PETNResults <- CombinedResults %>%
  filter(SampleName == "2ng PETN+1ng MT (Ac...ned)") %>%
  filter(Vial != "28")

#Plot peak area results
p <- ggplot(PETNResults, aes(x = Row, y = PETN.PA)) +
  geom_point() +
  labs(
    title = "PETN in Acetone-Cleaned Swab Extract",
    x = "Injection Number",
    y = "Peak Area"
  ) +
  theme(text = element_text(size = 12))

print(p)

ggsave(
  filename = "PETN in ACetoneCleaned Swab Extract PA.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Plot scaled PA results
p_scaled <- ggplot(PETNResults, aes(x = Row, y = PETN.PA)) +
  geom_point() +
  labs(
    title = "PETN in Acetone-Cleaned Swab Extract",
    x = "Injection Number",
    y = "Peak Area",
  ) +
  ylim(0,15000) +
  theme(text = element_text(size = 12))

print(p_scaled)

ggsave(
  filename = "PETN in Acetone-Cleaned Swab Extract PA_Scaled.png",
  plot = p_scaled,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Determine summary statistics

AveragePA <- mean(PETNResults$PETN.PA)
StdDevPA <- sd(PETNResults$PETN.PA)
RSDPA <- StdDevPA/AveragePA*100

#Create a summary dataframe
SummaryStats <- data.frame(
  Statistic = c("Mean", "StdDev", "RSD"),
  Value = c(AveragePA, StdDevPA, RSDPA)
)

# Save to CSV
write.csv(SummaryStats, file = paste0(Results.dir, "PETNinAcetoneCleanedSwabExtractSummaryStats.csv"), row.names = FALSE)

#Produce Box Plot to investigate spread of data
p <- ggplot(PETNResults, aes(x = SampleName, y = PETN.PA)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  labs(title = "Box Plot of PETN PA in Acetone-Cleaned Swab Extract",
       x = "",
       y = "Peak Area") +
  theme_minimal()

print(p)

ggsave(
  filename = "Box Plot of PETN PA in Acetone-Cleaned Swab Extract.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Produce histogram Plot to investigate spread of data
p <- ggplot(PETNResults, aes(x = PETN.PA)) +
  geom_histogram(binwidth = 400, fill = "steelblue", color = "black") +
  labs(title = "Histogram of PETN PA  in Acetone-Cleaned Swab Extract",
       x = "Peak Area",
       y = "Count") +
  theme_minimal()

print(p)

ggsave(
  filename = "Histogram Plot of PETN PA in Acetone-Cleaned Swab Extract.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Look at IS PA
# ISResults <- CombinedResults %>%
#   filter(SampleName == "2ng PETN+1ng MT in ...ract")

ISResults <- PETNResults

#Plot peak area results
p <- ggplot(ISResults, aes(x = Row, y = I.S.PA)) +
  geom_point() +
  labs(
    title = "IS PA in Acetone-Cleaned Swab Extract",
    x = "Injection Number",
    y = "Peak Area"
  ) +
  theme(text = element_text(size = 12))

print(p)

ggsave(
  filename = "IS PA in Acetone-Cleaned Swab Extract.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Plot scaled PA results
p_scaled <- ggplot(ISResults, aes(x = Row, y = I.S.PA)) +
  geom_point() +
  labs(
    title = "IS PA in Acetone-Cleaned Swab Extract",
    x = "Injection Number",
    y = "Peak Area",
  ) +
  ylim(0,100000) +
  theme(text = element_text(size = 12))

print(p_scaled)

ggsave(
  filename = "IS PA in Acetone-Cleaned Swab Extract_Scaled.png",
  plot = p_scaled,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Determine summary statistics

AveragePA <- mean(ISResults$I.S.PA)
StdDevPA <- sd(ISResults$I.S.PA)
RSDPA <- StdDevPA/AveragePA*100

#Create a summary dataframe
SummaryStats <- data.frame(
  Statistic = c("Mean", "StdDev", "RSD"),
  Value = c(AveragePA, StdDevPA, RSDPA)
)

# Save to CSV
write.csv(SummaryStats, file = paste0(Results.dir, "ISinAcetone-CleanedSwabSummaryStats.csv"), row.names = FALSE)

#Produce Box Plot to investigate spread of data
p <- ggplot(ISResults, aes(x = SampleName, y = I.S.PA)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  labs(title = "Box Plot of IS PA in Acetone-Cleaned Swab Extract)",
       x = "",
       y = "Peak Area") +
  theme_minimal()

print(p)

ggsave(
  filename = "Box Plot of IS PA in Acetone-Cleaned Swab Extract.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Produce histogram Plot to investigate spread of data
p <- ggplot(ISResults, aes(x = I.S.PA)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Acetone-Cleaned PA in FEL Swab Extract",
       x = "Peak Area",
       y = "Count") +
  theme_minimal()

print(p)

ggsave(
  filename = "Histogram Plot of IS PA in Acetone-Cleaned Swab Extract.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Plot IS/PETN Ratio
p <- ggplot(ISResults, aes(x = Row, y = ratio)) +
  geom_point() +
  labs(
    title = "PETN/IS Ratio in  Acetone-Cleaned Swab",
    x = "Injection Number",
    y = "Peak Area",
  ) +
  ylim(0,1) +
  theme(text = element_text(size = 12))

print(p)

ggsave(
  filename = "PETN_Is Ratio in  Acetone-Cleaned Swab.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Produce Box Plot to investigate spread of data
p <- ggplot(ISResults, aes(x = SampleName, y = ratio)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  labs(title = "Box Plot of PETN_IS Ratio in  Acetone-Cleaned Swab",
       x = "",
       y = "Peak Area") +
  theme_minimal()

print(p)

ggsave(
  filename = "Box Plot of PETN_IS PA in  Acetone-Cleaned Swab.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

#Produce histogram Plot to investigate spread of data
p <- ggplot(ISResults, aes(x = ratio)) +
  geom_histogram(binwidth = 0.008, fill = "steelblue", color = "black") +
  labs(title = "Histogram of PETN_IS Ratio in  Acetone-Cleaned Swab)",
       x = "Ratio",
       y = "Count") +
  theme_minimal()

print(p)

ggsave(
  filename = "Histogram Plot of PETN_IS in  Acetone-Cleaned Swab.png",
  plot = p,
  path = Results.dir,
  width = 8,
  height = 6,
  dpi = 300)

print("Results saved and updated successfully.")
