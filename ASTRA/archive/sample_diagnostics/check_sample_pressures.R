# Check max pressure for all samples
source('C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process.R')

samples <- c('PILOT_001', 'PILOT_005', 'PILOT_009', 'PILOT_013', 'PILOT_017', 'PILOT_021', 'PILOT_025', 'PILOT_029')

cat("Sample Pressure Analysis:\n")
cat("=========================\n\n")

for(s in samples) {
  file <- file.path(PARAMS$data_dir, paste0(s, '.csv'))
  cat(sprintf('%s: ', s))
  
  data <- clean_and_read_csv(file)
  
  if(!is.null(data) && 'load' %in% colnames(data)) {
    max_load <- max(data$load, na.rm=TRUE)
    mean_load <- mean(data$load[data$load > 10], na.rm=TRUE)
    cat(sprintf('Max=%.1fg, Mean(>10g)=%.1fg - ', max_load, mean_load))
    
    # Determine target
    if(max_load > 150) {
      cat('200g target\n')
    } else if(max_load > 40 && max_load < 100) {
      cat('50g target\n')
    } else {
      cat('UNCLEAR\n')
    }
  } else {
    cat('FAILED\n')
  }
}
