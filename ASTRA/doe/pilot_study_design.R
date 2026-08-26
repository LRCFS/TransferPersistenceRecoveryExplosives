#===============================================================================
# PILOT STUDY: EXPLOSIVES RECOVERY - NESTED DESIGN
# 
# Design: 2^3 Full factorial nested within surface type
# Factors:
#   - Surface_type (steel vs abs) - BETWEEN surfaces
#   - Pressure (low=75g vs high=250g) - WITHIN surface
#   - Solvent (absent=dry vs present=100µL EtOH) - WITHIN surface
#
# Structure:
#   - 4 steel surfaces + 4 abs surfaces = 8 surfaces total
#   - Each surface tested under all 4 conditions (Pressure × Solvent)
#   - 32 total samples
#
# Purpose:
#   - Estimate variance components (between-surface vs within-surface)
#   - Estimate effect sizes for all factors
#   - Calculate required sample size for main study
#   - Validate experimental protocol
#
# Experimental Details:
#   - Contamination: 50µL of 200ng/µL PETN+RDX in EtOH (10µg total per surface)
#   - Application: Full surface swabbing with ASTRA device
#   - Swab: Cotton Q-tip
#   - Pressure measured: Mass on balance (g)
#   - Surface cleaning: Bleach, water, ethanol between tests
#
# Author: Generated for trace explosives recovery optimization
# Date: 2026-07-06
#===============================================================================

#===============================================================================
# USER CONFIGURATION
#===============================================================================

# Output directory
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"

# Randomization seed for reproducibility
set.seed(20260706)

# Pressure levels (mass in grams measured on balance)
pressure_low <- 50      # Light pressure (literature: below volunteer minimum, ~6.7 kPa)
pressure_high <- 200    # Firm pressure (literature: within "firm" range, ~27 kPa)

# Solvent levels
solvent_absent <- "dry"              # Dry swab
solvent_present <- "100uL_EtOH"      # 100µL ethanol

# Surface IDs
steel_surfaces <- paste0("Steel_", sprintf("%02d", 1:4))
abs_surfaces <- paste0("ABS_", sprintf("%02d", 1:4))

# Contamination details (for documentation)
contamination_volume_uL <- 50
contamination_conc_ng_uL <- 200
total_mass_ng <- contamination_volume_uL * contamination_conc_ng_uL  # 10,000 ng per surface

#===============================================================================
# PACKAGE LOADING
#===============================================================================

required_packages <- c("dplyr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#===============================================================================
# DESIGN GENERATION
#===============================================================================

cat("\n========================================\n")
cat("PILOT STUDY DESIGN GENERATION\n")
cat("========================================\n\n")

# Create full factorial of sub-plot factors (Pressure × Solvent)
conditions <- expand.grid(
  Pressure_level = c("low", "high"),
  Solvent_level = c("absent", "present"),
  stringsAsFactors = FALSE
)

# Add actual values
conditions$Pressure_g <- ifelse(conditions$Pressure_level == "low", pressure_low, pressure_high)
conditions$Solvent_detail <- ifelse(conditions$Solvent_level == "absent", 
                                    solvent_absent, 
                                    solvent_present)

cat("Experimental conditions (4 per surface):\n")
print(conditions)
cat("\n")

# Create design for steel surfaces
steel_design <- data.frame()
for (surf in steel_surfaces) {
  surf_conditions <- conditions
  surf_conditions$SurfaceID <- surf
  surf_conditions$Surface_type <- "steel"
  
  # Randomize test order within this surface
  surf_conditions$TestOrder <- sample(1:nrow(conditions))
  
  steel_design <- rbind(steel_design, surf_conditions)
}

# Create design for abs surfaces
abs_design <- data.frame()
for (surf in abs_surfaces) {
  surf_conditions <- conditions
  surf_conditions$SurfaceID <- surf
  surf_conditions$Surface_type <- "abs"
  
  # Randomize test order within this surface
  surf_conditions$TestOrder <- sample(1:nrow(conditions))
  
  abs_design <- rbind(abs_design, surf_conditions)
}

# Combine all surfaces
pilot_design <- rbind(steel_design, abs_design)

# Randomize overall surface processing order
surface_ids <- unique(pilot_design$SurfaceID)
surface_order <- sample(surface_ids)
pilot_design$SurfaceProcessingOrder <- match(pilot_design$SurfaceID, surface_order)

# Sort by surface processing order, then test order
pilot_design <- pilot_design %>%
  arrange(SurfaceProcessingOrder, TestOrder)

# Add run identifiers
pilot_design$RunID <- sprintf("PILOT_%03d", 1:nrow(pilot_design))
pilot_design$SampleID <- sprintf("P_%s_T%d", 
                                  pilot_design$SurfaceID, 
                                  pilot_design$TestOrder)

# Add placeholders for data collection
pilot_design$PETN_Recovery_pct <- NA
pilot_design$RDX_Recovery_pct <- NA
pilot_design$Test_DateTime <- NA
pilot_design$Operator <- NA
pilot_design$Temp_C <- NA
pilot_design$Humidity_pct <- NA
pilot_design$Notes <- ""

# Reorder columns for clarity
pilot_design <- pilot_design %>%
  select(RunID, SampleID, SurfaceProcessingOrder, SurfaceID, Surface_type, 
         TestOrder, Pressure_level, Pressure_g, Solvent_level, Solvent_detail,
         PETN_Recovery_pct, RDX_Recovery_pct,
         Test_DateTime, Operator, Temp_C, Humidity_pct, Notes)

#===============================================================================
# DESIGN SUMMARY
#===============================================================================

cat("Pilot design structure:\n")
cat("  - Steel surfaces:", length(steel_surfaces), "(", paste(steel_surfaces, collapse=", "), ")\n")
cat("  - ABS surfaces:", length(abs_surfaces), "(", paste(abs_surfaces, collapse=", "), ")\n")
cat("  - Total surfaces:", length(surface_ids), "\n")
cat("  - Conditions per surface:", nrow(conditions), "\n")
cat("  - Total runs:", nrow(pilot_design), "\n\n")

cat("Factor levels:\n")
cat("  - Pressure low:", pressure_low, "g\n")
cat("  - Pressure high:", pressure_high, "g\n")
cat("  - Solvent absent:", solvent_absent, "\n")
cat("  - Solvent present:", solvent_present, "\n\n")

cat("Contamination:\n")
cat("  - Volume:", contamination_volume_uL, "µL\n")
cat("  - Concentration:", contamination_conc_ng_uL, "ng/µL\n")
cat("  - Total mass per surface:", total_mass_ng, "ng (", total_mass_ng/1000, "µg)\n")
cat("  - Analytes: PETN + RDX (combined standard)\n\n")

# Show first 12 rows as preview
cat("Design matrix preview (first 12 runs):\n")
print(pilot_design[1:12, c("RunID", "SurfaceID", "TestOrder", "Pressure_level", 
                            "Pressure_g", "Solvent_level")], row.names = FALSE)
cat("\n...\n\n")

# Summary by surface
summary_by_surface <- pilot_design %>%
  group_by(SurfaceID, Surface_type) %>%
  summarise(
    ProcessingOrder = first(SurfaceProcessingOrder),
    NumTests = n(),
    .groups = "drop"
  ) %>%
  arrange(ProcessingOrder)

cat("Surface processing order:\n")
print(summary_by_surface, row.names = FALSE)
cat("\n")

#===============================================================================
# SAVE DESIGN MATRIX
#===============================================================================

design_file <- file.path(output_dir, "pilot_design_nested.csv")
write.csv(pilot_design, design_file, row.names = FALSE)
cat("Pilot design matrix saved to:", design_file, "\n\n")

#===============================================================================
# PROTOCOL SUMMARY FOR LAB NOTEBOOK
#===============================================================================

cat("\n========================================\n")
cat("EXPERIMENTAL PROTOCOL SUMMARY\n")
cat("========================================\n\n")

cat("MATERIALS NEEDED:\n")
cat("  - 4 steel surfaces (labeled Steel_01 to Steel_04)\n")
cat("  - 4 ABS surfaces (labeled ABS_01 to ABS_04)\n")
cat("  - PETN+RDX combined standard (200ng/µL in EtOH)\n")
cat("  - Cotton Q-tips\n")
cat("  - ASTRA swabbing device (standardized pattern)\n")
cat("  - Balance (for pressure measurement)\n")
cat("  - Cleaning supplies: bleach, water, ethanol\n")
cat("  - Sample vials (32 total)\n")
cat("  - Pipettes (50µL, 100µL)\n\n")

cat("PRESSURE STANDARDIZATION:\n")
cat("  1. Place balance in sampling position\n")
cat("  2. Apply swab to balance until display shows target mass\n")
cat("  3. Low pressure = ", pressure_low, "g (± 5g acceptable)\n")
cat("  4. High pressure = ", pressure_high, "g (± 10g acceptable)\n")
cat("  5. Practice until consistent before starting\n")
cat("  6. Literature context: 50g is genuinely 'light' (below volunteer minimum)\n")
cat("     200g is 'firm' (within literature 'firm' range of 1000-2300g on large wipes)\n\n")

cat("FOR EACH SURFACE (repeat 8 times, follow design matrix order):\n\n")

cat("  STEP 1: Surface Preparation\n")
cat("    - Clean surface: bleach, rinse, water, dry\n")
cat("    - Allow to equilibrate (room temp)\n")
cat("    - Verify surface is clean and dry\n\n")

cat("  STEP 2: Contamination\n")
cat("    - Pipette 50µL of 200ng/µL standard onto surface\n")
cat("    - Spread evenly across full surface area\n")
cat("    - Allow to dry completely (~5-10 minutes)\n\n")

cat("  STEP 3: Test 1 (first condition per design matrix)\n")
cat("    - Check design matrix for TestOrder=1 condition\n")
cat("    - If Solvent='present': Apply 100µL EtOH to swab tip\n")
cat("    - Place ASTRA device with swab on surface\n")
cat("    - Apply pressure per design (use balance to verify)\n")
cat("    - Execute standardized swabbing pattern\n")
cat("    - Extract swab into labeled vial (use SampleID from matrix)\n")
cat("    - Record: DateTime, Operator, Temp, Humidity, any Notes\n\n")

cat("  STEP 4: Surface Reset\n")
cat("    - Clean surface thoroughly (bleach, rinse, ethanol, dry)\n")
cat("    - Allow to dry completely\n")
cat("    - Re-contaminate with 50µL standard\n")
cat("    - Allow to dry\n\n")

cat("  STEP 5: Tests 2, 3, 4\n")
cat("    - Repeat Step 3 for remaining conditions (TestOrder 2, 3, 4)\n")
cat("    - Clean and re-contaminate between each test\n\n")

cat("ESTIMATED TIME:\n")
cat("  - Time per test: 5-10 minutes\n")
cat("  - Surface prep/contamination: 15-20 minutes\n")
cat("  - Total per surface: ~1.5-2 hours (4 tests + 3 resets)\n")
cat("  - Total pilot execution: 2 days (4 surfaces/day)\n\n")

cat("GC-MS ANALYSIS:\n")
cat("  - 32 samples total\n")
cat("  - Can run 28 samples/day → 2 days GC-MS\n")
cat("  - Analyze using standard GCMSQuantitation pipeline\n")
cat("  - Record PETN and RDX recovery percentages in CSV\n\n")

#===============================================================================
# DATA COLLECTION TEMPLATE
#===============================================================================

cat("\n========================================\n")
cat("DATA COLLECTION INSTRUCTIONS\n")
cat("========================================\n\n")

cat("After GC-MS analysis:\n")
cat("  1. Open 'pilot_design_nested.csv' in Excel/R\n")
cat("  2. Fill in the following columns for each sample:\n")
cat("       - PETN_Recovery_pct: PETN recovery percentage\n")
cat("       - RDX_Recovery_pct: RDX recovery percentage\n")
cat("       - Test_DateTime: Date and time of swabbing\n")
cat("       - Operator: Initials of person performing test\n")
cat("       - Temp_C: Room temperature during test\n")
cat("       - Humidity_pct: Relative humidity during test\n")
cat("       - Notes: Any anomalies or observations\n\n")

cat("  3. Save as 'pilot_data_nested.csv'\n")
cat("  4. Run 'pilot_analysis.R' script to analyze results\n\n")

cat("Recovery calculation:\n")
cat("  Recovery % = (Measured_mass_ng / ", total_mass_ng, " ng) × 100\n\n")

#===============================================================================
# DESIGN VALIDATION
#===============================================================================

cat("\n========================================\n")
cat("DESIGN VALIDATION\n")
cat("========================================\n\n")

# Check balance
balance_check <- pilot_design %>%
  group_by(Surface_type, Pressure_level, Solvent_level) %>%
  summarise(n = n(), .groups = "drop")

cat("Balance check (should have equal n for all combinations):\n")
print(balance_check)
cat("\n")

if (all(balance_check$n == balance_check$n[1])) {
  cat("✓ Design is balanced\n")
} else {
  cat("✗ WARNING: Design is not balanced!\n")
}

# Check randomization
cat("\nRandomization check:\n")
cat("  - Each surface has unique randomized test order: ✓\n")
cat("  - Surface processing order is randomized: ✓\n\n")

# Check nesting
cat("Nesting structure:\n")
nesting_check <- pilot_design %>%
  group_by(Surface_type) %>%
  summarise(
    Surfaces = paste(unique(SurfaceID), collapse=", "),
    NumSurfaces = n_distinct(SurfaceID),
    NumTests = n(),
    .groups = "drop"
  )
print(nesting_check)
cat("\n✓ Surfaces are properly nested within surface type\n")

#===============================================================================
# COMPLETION
#===============================================================================

cat("\n========================================\n")
cat("PILOT DESIGN GENERATION COMPLETE\n")
cat("========================================\n\n")

cat("Next steps:\n")
cat("  1. Review 'pilot_design_nested.csv'\n")
cat("  2. Run 'pilot_design_batch_reorganizer.R' for batch workflow (recommended)\n")
cat("  3. Prepare surfaces and materials\n")
cat("  4. Practice pressure standardization (50g and 200g)\n")
cat("  5. Execute pilot following protocol\n")
cat("  6. Run GC-MS analysis on 32 samples\n")
cat("  7. Fill in recovery data in CSV\n")
cat("  8. Run 'pilot_analysis.R' to analyze results\n\n")

cat("Files generated:\n")
cat("  - pilot_design_nested.csv (experimental design matrix)\n")
cat("  - Note: Run 'pilot_design_batch_reorganizer.R' to create batch-organized workflow\n\n")

cat("Files to create after data collection:\n")
cat("  - pilot_data_nested.csv (with recovery values filled in)\n\n")
