# Test full trace plot generation for PILOT_001
# Quick test to verify the new function works before running full batch

# Source the main script (loads all functions)
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/archive/time_based_lineage/time_based_batch_process_adaptive.R")

# Test on PILOT_001 only
cat("Testing full trace plot generation on PILOT_001...\n\n")

# Run processing on single sample
result <- process_sample("PILOT_001", SAMPLE_CONFIG[["PILOT_001"]], PARAMS)

if (result$success) {
  cat("\n\n========================================\n")
  cat("TEST SUCCESSFUL!\n")
  cat("========================================\n")
  cat("Check the following location for output:\n")
  cat("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/ProcessedData/PILOT_001/\n")
  cat("\nExpected files:\n")
  cat("  - PILOT_001_full_trace.png (NEW!)\n")
  cat("  - 25s_windows/ folder with plots\n")
  cat("  - PILOT_001_detected_cycles.csv\n")
  cat("  - PILOT_001_summary_statistics.txt\n")
} else {
  cat("\n\n========================================\n")
  cat("TEST FAILED!\n")
  cat("========================================\n")
  cat("Error:", result$error, "\n")
}
