#===============================================================================
# PILOT STUDY - BATCH ORGANIZER
# 
# Purpose:
#   Reorganize pilot_design_nested.csv for batch-spiking workflow
#   
# Workflow:
#   - All 4 surfaces of same type spiked at once
#   - Batch 1 = All surfaces' TestOrder 1
#   - Batch 2 = All surfaces' TestOrder 2, etc.
#   - Within batch: Steel group, then ABS group
#   - Surface order randomized within each spike group
#   - Process samples in CSV row order (no TestSequence needed)
#
# Pressures (updated 2026-07-06):
#   - Low: 50g (light pressure, ~6.7 kPa on Q-tip)
#   - High: 200g (firm pressure, ~27 kPa on Q-tip)
#
# Output:
#   - 12-column simplified CSV for lab execution
#   - Process in row order (top to bottom)
#   - Each row = one sample with all needed info
#
# Author: Generated for trace explosives recovery optimization
# Date: 2026-07-06
#===============================================================================

#===============================================================================
# CONFIGURATION
#===============================================================================

input_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"
input_file <- file.path(input_dir, "pilot_design_nested.csv")
output_file <- file.path(input_dir, "pilot_design_batch_organized.csv")

# Timing parameters (minutes)
spike_time_per_surface <- 0.5
drying_time <- 5
test_time_per_sample <- 2
cleaning_time_between_batches <- 15

#===============================================================================
# LOAD PACKAGES
#===============================================================================

required_packages <- c("dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#===============================================================================
# LOAD AND VALIDATE DATA
#===============================================================================

cat("\n========================================\n")
cat("BATCH ORGANIZER FOR PILOT STUDY\n")
cat("========================================\n\n")

# Load original nested design
if (!file.exists(input_file)) {
  stop("ERROR: Input file not found: ", input_file)
}

pilot_design <- read.csv(input_file, stringsAsFactors = FALSE)

cat("Original design loaded:\n")
cat("  - Total samples:", nrow(pilot_design), "\n")
cat("  - Surfaces:", length(unique(pilot_design$SurfaceID)), "\n")
cat("  - Surface types:", paste(unique(pilot_design$Surface_type), collapse=", "), "\n\n")

# Validate structure
expected_samples <- 32
if (nrow(pilot_design) != expected_samples) {
  stop("ERROR: Expected ", expected_samples, " samples, found ", nrow(pilot_design))
}

#===============================================================================
# REORGANIZE BY BATCH
#===============================================================================

cat("Reorganizing design for batch workflow...\n")
cat("  - Surfaces randomized within each spike group\n")
cat("  - Different surface order per batch\n\n")

# Create batch-organized version with within-batch randomization
batch_design <- pilot_design %>%
  # Use TestOrder as BatchNumber
  mutate(BatchNumber = TestOrder,
         OriginalTestOrder = TestOrder) %>%
  # Create spike group identifier
  mutate(SpikeGroup = ifelse(Surface_type == "steel", "Steel", "ABS")) %>%
  # Randomize surface order within each batch and spike group
  group_by(BatchNumber, SpikeGroup) %>%
  mutate(SurfaceOrder = sample(row_number())) %>%
  ungroup() %>%
  # Sort by BatchNumber, SpikeGroup (steel first), then randomized SurfaceOrder
  arrange(BatchNumber, desc(SpikeGroup), SurfaceOrder)

# Add TestSequence within each batch
batch_design <- batch_design %>%
  group_by(BatchNumber) %>%
  mutate(TestSequence = row_number()) %>%
  ungroup()

# Add batch name
batch_design <- batch_design %>%
  mutate(BatchName = paste0("Batch_", BatchNumber, "_TestOrder", OriginalTestOrder))

#===============================================================================
# CALCULATE ESTIMATED TIMING
#===============================================================================

cat("Calculating estimated timing for each sample...\n\n")

# Function to calculate time within batch
calculate_batch_time <- function(test_sequence) {
  if (test_sequence <= 4) {
    # Steel group
    spike_end <- 4 * spike_time_per_surface
    dry_end <- spike_end + drying_time
    test_time <- dry_end + (test_sequence - 1) * test_time_per_sample
  } else {
    # ABS group
    steel_complete <- 4 * spike_time_per_surface + drying_time + 4 * test_time_per_sample
    abs_spike_end <- steel_complete + 4 * spike_time_per_surface
    abs_dry_end <- abs_spike_end + drying_time
    test_time <- abs_dry_end + (test_sequence - 5) * test_time_per_sample
  }
  return(test_time)
}

# Add estimated time within batch
batch_design <- batch_design %>%
  mutate(TimeInBatch_min = sapply(TestSequence, calculate_batch_time))

# Add cumulative time from start
batch_time_per_batch <- max(batch_design$TimeInBatch_min) + cleaning_time_between_batches

batch_design <- batch_design %>%
  mutate(CumulativeTime_min = (BatchNumber - 1) * batch_time_per_batch + TimeInBatch_min)

# Format time as HH:MM
batch_design <- batch_design %>%
  mutate(
    EstimatedTime = sprintf("%d:%02d", 
                            floor(CumulativeTime_min / 60), 
                            round(CumulativeTime_min %% 60)),
    TimeInBatch = sprintf("%d:%02d",
                          floor(TimeInBatch_min / 60),
                          round(TimeInBatch_min %% 60))
  )

#===============================================================================
# ADD PROTOCOL NOTES
#===============================================================================

batch_design <- batch_design %>%
  mutate(
    ProtocolPhase = case_when(
      TestSequence <= 4 ~ "Steel_Group",
      TestSequence > 4 ~ "ABS_Group",
      TRUE ~ "Unknown"
    ),
    Action = case_when(
      TestSequence == 1 ~ "First steel test after spiking all 4 steel",
      TestSequence == 5 ~ "First ABS test after spiking all 4 ABS",
      TRUE ~ paste0("Test ", ifelse(TestSequence <= 4, TestSequence, TestSequence - 4),
                    " in ", ProtocolPhase)
    )
  )

#===============================================================================
# VALIDATION (before column reduction)
#===============================================================================

cat("VALIDATION CHECKS:\n")
cat("------------------\n")

# Check 1: Total samples
if (nrow(batch_design) == expected_samples) {
  cat("✓ Sample count: ", nrow(batch_design), "/", expected_samples, "\n")
} else {
  cat("✗ ERROR: Sample count mismatch\n")
}

# Check 2: Each batch has 8 samples
batch_counts <- batch_design %>%
  group_by(BatchNumber) %>%
  summarise(n = n(), .groups = "drop")

if (all(batch_counts$n == 8)) {
  cat("✓ Each batch has 8 samples\n")
} else {
  cat("✗ ERROR: Unequal batch sizes\n")
  print(batch_counts)
}

# Check 3: Each batch has 4 steel + 4 ABS
type_per_batch <- batch_design %>%
  group_by(BatchNumber, Surface_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Surface_type, values_from = n)

if (all(type_per_batch$steel == 4) && all(type_per_batch$abs == 4)) {
  cat("✓ Each batch has 4 steel + 4 ABS\n")
} else {
  cat("✗ ERROR: Incorrect surface type distribution\n")
  print(type_per_batch)
}

# Check 4: Each surface tested exactly 4 times
surface_test_counts <- batch_design %>%
  group_by(SurfaceID) %>%
  summarise(n = n(), .groups = "drop")

if (all(surface_test_counts$n == 4)) {
  cat("✓ Each surface tested exactly 4 times\n")
} else {
  cat("✗ ERROR: Surfaces not tested 4 times each\n")
  print(surface_test_counts)
}

# Check 5: Condition balance
condition_balance <- batch_design %>%
  group_by(Surface_type, Pressure_level, Solvent_level) %>%
  summarise(n = n(), .groups = "drop")

if (all(condition_balance$n == 4)) {
  cat("✓ All conditions balanced (4 replicates each)\n")
} else {
  cat("⚠ WARNING: Condition imbalance detected\n")
  print(condition_balance)
}

cat("\n")

#===============================================================================
# DISPLAY BATCH ORGANIZATION (before column reduction)
#===============================================================================

cat("BATCH ORGANIZATION:\n")
cat("-------------------\n\n")

for (batch in 1:4) {
  batch_data <- batch_design %>%
    filter(BatchNumber == batch)
  
  cat("BATCH", batch, "- TestOrder", unique(batch_data$OriginalTestOrder), "\n")
  cat("Estimated time:", min(batch_data$EstimatedTime), "to", max(batch_data$EstimatedTime), "\n")
  cat("Duration:", round(max(batch_data$CumulativeTime_min) - min(batch_data$CumulativeTime_min)), "min\n\n")
  
  cat("  Steel Group (Sequence 1-4):\n")
  steel_data <- batch_data %>% filter(SpikeGroup == "Steel")
  for (i in 1:nrow(steel_data)) {
    row <- steel_data[i, ]
    cat(sprintf("    %d. %s: %s pressure (%dg), %s (%s) [%s]\n",
                row$TestSequence,
                row$SurfaceID,
                row$Pressure_level,
                row$Pressure_g,
                row$Solvent_level,
                row$Solvent_detail,
                row$RunID))
  }
  
  cat("\n  ABS Group (Sequence 5-8):\n")
  abs_data <- batch_data %>% filter(SpikeGroup == "ABS")
  for (i in 1:nrow(abs_data)) {
    row <- abs_data[i, ]
    cat(sprintf("    %d. %s: %s pressure (%dg), %s (%s) [%s]\n",
                row$TestSequence,
                row$SurfaceID,
                row$Pressure_level,
                row$Pressure_g,
                row$Solvent_level,
                row$Solvent_detail,
                row$RunID))
  }
  cat("\n")
}

#===============================================================================
# SUMMARY (before column reduction)
#===============================================================================

cat("SUMMARY:\n")
cat("--------\n")
cat("  Total samples: ", nrow(batch_design), "\n")
cat("  Batches: 4\n")
cat("  Samples per batch: 8 (4 steel + 4 ABS)\n\n")

cat("TIMING ESTIMATE:\n")
cat("----------------\n")
cat("  Batch duration: ~", round(max(batch_design$TimeInBatch_min)), " min\n")
cat("  Cleaning between batches: ", cleaning_time_between_batches, " min\n")
cat("  Total time (4 batches): ~", round(max(batch_design$CumulativeTime_min)), " min (",
    round(max(batch_design$CumulativeTime_min) / 60, 1), " hours)\n\n")

#===============================================================================
# SIMPLIFY COLUMNS FOR OUTPUT
#===============================================================================

batch_design <- batch_design %>%
  select(
    # Batch and sample identification
    BatchNumber,
    SampleID = RunID,      # Rename RunID to SampleID (PILOT_001, etc.)
    SurfaceID,             # Steel_04, ABS_01, etc.
    # Experimental conditions
    Pressure_g,            # 50 or 200
    Solvent_detail,        # dry or 100uL_EtOH
    # Data collection
    PETN_Recovery_pct, RDX_Recovery_pct,
    Test_DateTime, Operator, Temp_C, Humidity_pct, Notes
  )

#===============================================================================
# SAVE BATCH-ORGANIZED DESIGN
#===============================================================================

write.csv(batch_design, output_file, row.names = FALSE)

# Also create a simplified timing checklist (now identical to main file)
checklist_file <- file.path(input_dir, "batch_timing_checklist.csv")
# Checklist is now identical to main file (user processes in row order)
write.csv(batch_design, checklist_file, row.names = FALSE)

cat("========================================\n")
cat("BATCH-ORGANIZED DESIGN SAVED\n")
cat("========================================\n\n")
cat("Output files:\n")
cat("  - ", output_file, "\n")
cat("  - ", checklist_file, "\n\n")

cat("WORKFLOW:\n")
cat("---------\n")
cat("  1. Batch 1: Spike all 4 steel → dry 5min → test (randomized order) →\n")
cat("              spike all 4 ABS → dry 5min → test (randomized order)\n")
cat("  2. Clean all 8 surfaces (15 min)\n")
cat("  3. Repeat for Batches 2, 3, 4\n")
cat("  4. IMPORTANT: Process samples in row order from CSV file\n")
cat("     (order varies between batches)\n\n")

cat("NEXT STEPS:\n")
cat("-----------\n")
cat("  1. Review pilot_design_batch_organized.csv\n")
cat("  2. Print batch_timing_checklist.csv for lab use\n")
cat("  3. Review batch_protocol.md for detailed instructions\n")
cat("  4. Execute pilot following batch workflow\n")
cat("  5. After data collection, fill in recovery values\n")
cat("  6. Run pilot_analysis.R\n\n")
