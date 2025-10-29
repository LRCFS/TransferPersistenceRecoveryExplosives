#####################################################################
#####              Run Global Code in first instance            #####
#####################################################################

# Load Metadata files
filenameMetadata <- list.files(Metadata.dir, pattern = extensionXLSX, full.names = TRUE)
Metadata <- readxl::read_xlsx(filenameMetadata)

# Load GC results
filenameGcData <- list.files(GcData.dir, pattern = "TIC\\.csv$", full.names = TRUE)
GcResults <- do.call(rbind, lapply(filenameGcData, function(x) transform(read.csv(x), File = basename(x))))

f# Extract the date from the filename
GcResults$DataName <- gsub('.{4}$', '', GcResults$File)
GcResults$DataName <- as.numeric(GcResults$DataName)

# Join GC results to metadata
CombinedResults <- full_join(Metadata, GcResults)

# Peak area ratios
CombinedResults$ratio <- as.numeric(CombinedResults$PA / CombinedResults$I.S.PA)
CombinedResults$Row <- 1:nrow(CombinedResults)

# Calibration range
calibration <- data.frame(calibration = as.numeric(c("10", "8", "6", "4", "2", "0.5")))
CalLevels <- nrow(calibration)

NumCal <- as.list(1:(nrow(CalibrationValues <- dplyr::filter(CombinedResults, Type == "Cal")) / CalLevels))
Cals <- list()

# Initialize model comparison table
ModelComparison <- data.frame(
  Calibration = integer(),
  Model = character(),
  Intercept = numeric(),
  Slope = numeric(),
  Quadratic_Term = numeric(),
  R_squared = numeric(),
  Adj_R_squared = numeric(),
  stringsAsFactors = FALSE
)

for (i in NumCal) {
  Num <- (CalLevels * i - (CalLevels - 1))
  Cals[[i]] <- CombinedResults[which(CombinedResults$Type == 'Cal', arr.ind = TRUE)[Num:(Num + 5)], ]
  CalData <- Cals[[i]]
  CalData <- CalData[!is.na(CalData$Name), ]
  Cals[[i]] <- cbind(calibration, CalData)
  
  # Fit linear model
  linear_model <- lm(ratio ~ calibration, data = Cals[[i]])
  linear_summary <- summary(linear_model)
  
  ModelComparison <- rbind(ModelComparison, data.frame(
    Calibration = i,
    Model = "Linear",
    Intercept = coef(linear_model)[1],
    Slope = coef(linear_model)[2],
    Quadratic_Term = NA,
    R_squared = linear_summary$r.squared,
    Adj_R_squared = linear_summary$adj.r.squared
  ))
  
  # Fit quadratic model
  quad_model <- lm(ratio ~ poly(calibration, 2, raw = TRUE), data = Cals[[i]])
  quad_summary <- summary(quad_model)
  
  ModelComparison <- rbind(ModelComparison, data.frame(
    Calibration = i,
    Model = "Quadratic",
    Intercept = coef(quad_model)[1],
    Slope = coef(quad_model)[2],
    Quadratic_Term = coef(quad_model)[3],
    R_squared = quad_summary$r.squared,
    Adj_R_squared = quad_summary$adj.r.squared
  ))
  
  # Residual plots
  png(filename = file.path(Results.dir, sprintf("Residuals_Calibration_%d_Linear.png", i)))
  plot(Cals[[i]]$calibration, resid(linear_model), main = sprintf("Residuals - Calibration %d (Linear)", i),
       xlab = "Calibration", ylab = "Residuals", pch = 19)
  abline(h = 0, col = "red", lty = 2)
  dev.off()
  
  png(filename = file.path(Results.dir, sprintf("Residuals_Calibration_%d_Quadratic.png", i)))
  plot(Cals[[i]]$calibration, resid(quad_model), main = sprintf("Residuals - Calibration %d (Quadratic)", i),
       xlab = "Calibration", ylab = "Residuals", pch = 19)
  abline(h = 0, col = "red", lty = 2)
  dev.off()
}

# Save model comparison table
write.csv(ModelComparison, file = file.path(Results.dir, "Calibration_Model_Comparison.csv"), row.names = FALSE)

cat("Model comparison and residual plots saved to Results folder.\n")
