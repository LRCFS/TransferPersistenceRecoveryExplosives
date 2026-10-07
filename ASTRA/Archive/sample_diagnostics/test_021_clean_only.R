# Debug PILOT_021 CSV cleaning - just the cleaning function

clean_and_read_csv <- function(filepath) {
  
  # Read all lines
  lines <- readLines(filepath, warn = FALSE)
  
  # The proper header should be: "timestamp,elapsed_sec,analogValue,load,distance"
  proper_header <- "timestamp,elapsed_sec,analogValue,load,distance"
  
  header_line <- NULL
  
  # Strategy: Find the line that contains "analogValue,load,distance"
  # This is the most reliable signature of the actual header
  for (i in 1:min(20, length(lines))) {
    line <- lines[i]
    
    # Check if this line contains the key header elements
    if (grepl("analogValue.*load.*distance", line)) {
      # Check if this line is JUST the column names (header row as data)
      if (i > 1 && (grepl("^[0-9]", lines[i-1]) || grepl("timestamp.*elapsed_sec", lines[i-1]))) {
        # Previous line had timestamp/elapsed_sec, this line has the rest
        # Remove both and start from next line
        cat(sprintf("  Found split header: Line %d has timestamp/elapsed_sec, Line %d has analogValue/load/distance\n", i-1, i))
        header_line <- i + 1
        # But we need to prepend the proper header
        lines <- c(proper_header, lines[header_line:length(lines)])
        header_line <- 1
        cat(sprintf("  Cleaning CSV: Reconstructed header from split rows\n"))
        break
      } else {
        # This line has the full proper header
        header_line <- i
        cat(sprintf("  Found full header at line %d\n", i))
        break
      }
    }
    
    # Check if this is already a proper full header
    if (line == proper_header) {
      header_line <- i
      cat(sprintf("  Found proper full header at line %d\n", i))
      break
    }
  }
  
  # If we didn't find proper header, check for the case where first row 
  # has "timestamp,elapsed_sec,<numbers>" (PILOT_005, PILOT_021 case)
  if (is.null(header_line)) {
    first_line <- lines[1]
    if (grepl("timestamp,elapsed_sec", first_line) && grepl("[0-9]", first_line)) {
      # First row has header + data mixed - replace it with proper header
      cat(sprintf("  Cleaning CSV: Replacing malformed first row with proper header\n"))
      lines[1] <- proper_header
      header_line <- 1
    }
  }
  
  # If we found a proper header line and it's not the first, remove lines before it
  if (!is.null(header_line) && header_line > 1) {
    cat(sprintf("  Cleaning CSV: Removing %d line(s) before header\n", header_line - 1))
    lines <- lines[header_line:length(lines)]
  }
  
  # If we still haven't found it, the file might be too corrupted
  if (is.null(header_line)) {
    cat("  ERROR: Could not find valid header in CSV file\n")
    return(NULL)
  }
  
  # Write cleaned lines to temporary file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(lines, temp_file)
  
  # Read the cleaned CSV
  data <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat(sprintf("  Error reading cleaned CSV: %s\n", e$message))
    NULL
  })
  
  # Clean up temp file
  unlink(temp_file)
  
  # Validate that we have the expected columns
  if (!is.null(data)) {
    required_cols <- c("timestamp", "elapsed_sec", "analogValue", "load", "distance")
    if (!all(required_cols %in% colnames(data))) {
      cat(sprintf("  ERROR: Missing required columns. Found: %s\n", 
                  paste(colnames(data), collapse=", ")))
      return(NULL)
    }
    
    # Check if elapsed_sec is actually numeric
    if (!is.numeric(data$elapsed_sec)) {
      cat("  ERROR: elapsed_sec column is not numeric\n")
      cat(sprintf("  First few values: %s\n", paste(head(data$elapsed_sec), collapse=", ")))
      return(NULL)
    }
  }
  
  return(data)
}

# Test on PILOT_021
filepath <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/PILOT_021.csv"

cat("Testing PILOT_021 cleaning:\n")
cat("===========================\n\n")

data <- clean_and_read_csv(filepath)

if (!is.null(data)) {
  cat("\n✓ Successfully cleaned!\n")
  cat(sprintf("Loaded %d rows\n", nrow(data)))
  cat(sprintf("Time range: %.2f - %.2fs\n", min(data$elapsed_sec), max(data$elapsed_sec)))
  cat(sprintf("Load range: %.2f - %.2fg\n", min(data$load, na.rm=TRUE), max(data$load, na.rm=TRUE)))
  cat("\nFirst 10 data rows:\n")
  print(head(data[, c("elapsed_sec", "load", "distance")], 10))
} else {
  cat("\n✗ Cleaning failed\n")
}
