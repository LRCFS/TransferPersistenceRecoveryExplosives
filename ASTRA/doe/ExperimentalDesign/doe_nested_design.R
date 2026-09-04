#===============================================================================
# MAIN STUDY: EXPLOSIVES RECOVERY - PRESSURE DOSE-RESPONSE DESIGN
#
# Follows on from the pilot study (pilot_study_design.R / pilot_analysis.R).
# Purpose: characterise the shape of the Pressure-recovery relationship
# (not just a 2-level low/high contrast) and test whether that shape differs
# between steel and ABS -- both explicitly flagged as unresolved by the
# pilot's own per-effect power analysis (Pressure's 95% CI touched zero;
# Surface_type:Pressure_level interaction was never significant at n=32,
# but the raw pilot means hinted steel recovery rising with pressure while
# ABS recovery falls, which a 2-level test cannot properly characterise).
#
# Design summary (see CONTEXT.md session "Full DOE Design: Multi-Level
# Pressure Incorporation" for the full design-logic discussion):
#   - Solvent fixed = "present" (100uL EtOH) throughout -- dry swabs
#     deliberately excluded per explicit decision (dry floors recovery near
#     zero regardless of pressure, confirmed in the pilot).
#   - 5 pressure levels: 10, 50, 100, 200, 300 g. 50g/200g are the pilot's
#     own original "low"/"high" levels -- reusing them lets the pilot's
#     existing 16 wet-only samples (4 steel + 4 ABS at each of 50g/200g) be
#     pooled into the combined analysis instead of re-collecting them from
#     scratch.
#   - REUSED PILOT SURFACES (Steel_01-04, ABS_01-04, 4/type): already carry
#     50g-wet and 200g-wet data from the pilot. Tested at the 3 NEW pressure
#     levels only (10g, 50g... no -- 10g, 100g, 300g) to bring each of them
#     up to a full 5-level profile. This pushes their total physical use
#     count to 7 (4 pilot conditions + 3 new) -- one step beyond the 6-use
#     maximum already confirmed trend-free (Diagnostics/
#     reuse_trend_check_corrected.R: RDX p=0.14-0.48, PETN p=0.27, Spearman
#     rho ~ -0.04 for both, i.e. no hint of degradation up to 6 reuses).
#   - NEW SURFACES (Steel_05-06, ABS_05-06, 2/type): fresh, never used in
#     the pilot. Tested at ALL 5 pressure levels, giving an independent
#     between-surface check on the pooled 50g/200g values and completing
#     the n=6-per-level target without needing more new surfaces.
#   - Net result: n=6 at EVERY pressure level once pooled with the pilot's
#     existing data (4 reused-surface contributions + 2 new-surface
#     contributions per level), using only 4 brand-new surfaces total (2
#     steel + 2 ABS) instead of needing 8-16.
#   - Negative controls: 1 per surface type per batch (matches the pilot's
#     own SteelBatchN_NC / ABSBatchN_NC convention), host surface drawn
#     uniformly at random from whichever surfaces of that type are active
#     in that batch (reused + new surfaces pooled into the same random
#     draw) -- explicit user decision to accept the resulting extra,
#     unevenly-distributed reuse this adds to whichever surfaces happen to
#     be drawn.
#
# Author: Generated for trace explosives recovery optimization (main study)
# Date: 2026-08-11
#===============================================================================

#===============================================================================
# USER CONFIGURATION
#===============================================================================

output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"
set.seed(20260811)

# Pressure levels (grams on balance, matching the pilot's own measurement convention)
shared_levels   <- c(10, 100, 300)   # tested by BOTH reused and new surfaces (batches 1-3)
new_only_levels <- c(50, 200)        # tested by NEW surfaces only (batches 4-5) -- reused
                                      # surfaces already have these from the pilot

all_levels <- sort(c(shared_levels, new_only_levels))

# Solvent fixed throughout (wet-only, per pilot recommendation + explicit decision
# to exclude dry swabs from this study)
solvent_level  <- "present"
solvent_detail <- "100uL_EtOH"

# Surfaces
reused_steel <- paste0("Steel_", sprintf("%02d", 1:4))   # already used in the pilot
reused_abs   <- paste0("ABS_",   sprintf("%02d", 1:4))
new_steel    <- paste0("Steel_", sprintf("%02d", 5:6))   # brand new
new_abs      <- paste0("ABS_",   sprintf("%02d", 5:6))

# Contamination details (unchanged from the pilot)
contamination_volume_uL  <- 50
contamination_conc_ng_uL <- 200
total_mass_ng <- contamination_volume_uL * contamination_conc_ng_uL

#===============================================================================
# PACKAGE LOADING
#===============================================================================

required_packages <- c("dplyr", "tidyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#===============================================================================
# 1. BUILD PER-SURFACE PRESSURE ASSIGNMENTS
#
# Reused surfaces: randomly assign their 3 new levels (shared_levels) to
# Batch slots 1-3 (own random order per surface, mirroring the pilot's own
# per-surface TestOrder randomization).
# New surfaces: randomly assign shared_levels to Batch slots 1-3 (same as
# reused surfaces, so all 6 surfaces/type are active in batches 1-3), AND
# randomly assign new_only_levels to Batch slots 4-5.
#===============================================================================

cat("\n========================================\n")
cat("MAIN STUDY DESIGN GENERATION\n")
cat("========================================\n\n")

build_surface_rows <- function(surface_id, surface_type, surface_group) {
  # Shared levels -> Batches 1-3, own random order per surface
  batch_order_shared <- sample(1:3)
  shared_rows <- data.frame(
    SurfaceID    = surface_id,
    Surface_type = surface_type,
    SurfaceGroup = surface_group,
    Pressure_g   = shared_levels[order(batch_order_shared)],
    BatchNumber  = 1:3,
    stringsAsFactors = FALSE
  )

  if (surface_group == "New") {
    # New-only levels -> Batches 4-5, own random order per surface
    batch_order_new <- sample(1:2)
    new_only_rows <- data.frame(
      SurfaceID    = surface_id,
      Surface_type = surface_type,
      SurfaceGroup = surface_group,
      Pressure_g   = new_only_levels[order(batch_order_new)],
      BatchNumber  = 4:5,
      stringsAsFactors = FALSE
    )
    return(bind_rows(shared_rows, new_only_rows))
  }

  shared_rows
}

design_list <- list()
idx <- 1
for (s in reused_steel) { design_list[[idx]] <- build_surface_rows(s, "steel", "Reused"); idx <- idx + 1 }
for (s in reused_abs)   { design_list[[idx]] <- build_surface_rows(s, "abs",   "Reused"); idx <- idx + 1 }
for (s in new_steel)    { design_list[[idx]] <- build_surface_rows(s, "steel", "New");    idx <- idx + 1 }
for (s in new_abs)      { design_list[[idx]] <- build_surface_rows(s, "abs",   "New");    idx <- idx + 1 }

main_design <- bind_rows(design_list)

main_design$Solvent_level  <- solvent_level
main_design$Solvent_detail <- solvent_detail

# Pressure category label (readability only -- Pressure_g is the analysis variable)
main_design$Pressure_label <- factor(main_design$Pressure_g,
                                      levels = all_levels,
                                      labels = c("VeryLow","Low","Medium","High","VeryHigh"))

cat("New samples to collect:", nrow(main_design), "\n")
cat("  - From reused pilot surfaces (8 surfaces x 3 new levels):",
    sum(main_design$SurfaceGroup == "Reused"), "\n")
cat("  - From new surfaces (4 surfaces x 5 levels):",
    sum(main_design$SurfaceGroup == "New"), "\n\n")

#===============================================================================
# 2. ADD RUN IDENTIFIERS
#===============================================================================

main_design <- main_design %>% arrange(BatchNumber, Surface_type, SurfaceID)
main_design$RunID    <- sprintf("MAIN_%03d", 1:nrow(main_design))
# SampleID matches RunID exactly (same convention as the pilot's GC-MS-facing
# identifier, "PILOT_001" etc.) -- this is what gets written on the vial and
# entered as the GC-MS SampleName, so it must stay a plain sequential label,
# not a descriptive one (surface/pressure info lives in its own columns and
# in the batch checklist instead).
main_design$SampleID <- main_design$RunID

# Randomize processing order within each batch (steel group then ABS group,
# random surface order within each group -- same convention as
# pilot_design_batch_reorganizer.R)
main_design <- main_design %>%
  mutate(SpikeGroup = ifelse(Surface_type == "steel", "Steel", "ABS")) %>%
  group_by(BatchNumber, SpikeGroup) %>%
  mutate(SurfaceOrder = sample(row_number())) %>%
  ungroup() %>%
  arrange(BatchNumber, desc(SpikeGroup), SurfaceOrder) %>%
  group_by(BatchNumber) %>%
  mutate(TestSequence = row_number()) %>%
  ungroup()

# Placeholders for data collection
main_design$Test_DateTime     <- NA
main_design$Operator          <- NA
main_design$Temp_C             <- NA
main_design$Humidity_pct       <- NA

# Overall SurfaceProcessingOrder -- a random permutation across ALL physical
# surfaces engaged in this study (reused + new), constant per surface across
# all its own rows -- mirrors pilot_study_design.R's own convention exactly
# (`surface_order <- sample(surface_ids); design$SurfaceProcessingOrder <-
# match(design$SurfaceID, surface_order)`). Note this is a nominal/reference
# ordering, not the actual day-of execution order -- for that, use
# main_study_batch_checklist.csv (this mirrors the pilot's own file split:
# pilot_design_nested.csv used SurfaceProcessingOrder/TestOrder as its sort
# key, while the *separate* pilot_design_batch_organized.csv held the real
# randomized within-batch execution order).
all_surface_ids_ordered <- sample(unique(main_design$SurfaceID))
surface_processing_order <- data.frame(
  SurfaceID = all_surface_ids_ordered,
  SurfaceProcessingOrder = seq_along(all_surface_ids_ordered),
  stringsAsFactors = FALSE
)
main_design <- main_design %>% left_join(surface_processing_order, by = "SurfaceID")

#===============================================================================
# 3. NEGATIVE CONTROLS -- 1 per surface type per batch, host drawn randomly
#    from ALL active surfaces of that type in that batch (reused + new pooled)
#===============================================================================

cat("========================================\n")
cat("NEGATIVE CONTROL ASSIGNMENT\n")
cat("========================================\n\n")

# Cap: no single surface may host more than max_nc_hits_per_surface NCs
# across the whole study -- keeps the draw random among eligible surfaces
# each batch, but prevents a pathological streak (e.g. one surface drawn
# 4/5 times) from concentrating extra use count on a single surface.
max_nc_hits_per_surface <- 2
all_surface_ids <- unique(main_design$SurfaceID)
nc_hit_count <- setNames(rep(0L, length(all_surface_ids)), all_surface_ids)

nc_list <- list()
nc_idx <- 1
for (b in sort(unique(main_design$BatchNumber))) {
  for (st in c("steel", "abs")) {
    active_surfaces <- main_design %>%
      filter(BatchNumber == b, Surface_type == st) %>%
      pull(SurfaceID) %>%
      unique()

    if (length(active_surfaces) == 0) next

    eligible <- active_surfaces[nc_hit_count[active_surfaces] < max_nc_hits_per_surface]
    if (length(eligible) == 0) {
      # Fallback (shouldn't occur with these pool sizes/cap): allow any
      # active surface rather than leaving a batch without an NC.
      eligible <- active_surfaces
      cat(sprintf("  [WARNING] Batch %d, %-5s: all active surfaces already at the NC cap -- drawing anyway.\n", b, st))
    }

    host <- if (length(eligible) == 1) eligible else sample(eligible, 1)
    nc_hit_count[host] <- nc_hit_count[host] + 1L
    label <- ifelse(st == "steel", "Steel", "ABS")

    nc_list[[nc_idx]] <- data.frame(
      RunID        = sprintf("MAIN_%sBatch%d_NC", label, b),
      SampleID     = sprintf("MAIN_%sBatch%d_NC", label, b),
      SurfaceID    = host,
      Surface_type = st,
      BatchNumber  = b,
      HostSurfaceGroup = main_design$SurfaceGroup[main_design$SurfaceID == host][1],
      stringsAsFactors = FALSE
    )
    nc_idx <- nc_idx + 1

    cat(sprintf("  Batch %d, %-5s NC -> hosted on %s (%s surface, NC hit #%d for this surface)\n",
                b, label, host,
                main_design$SurfaceGroup[main_design$SurfaceID == host][1],
                nc_hit_count[host]))
  }
}
nc_design <- bind_rows(nc_list)
cat("\nTotal negative controls:", nrow(nc_design), "\n\n")

#===============================================================================
# 4. TALLY ACTUAL PHYSICAL SURFACE USE COUNTS (pilot uses + new pressure
#    tests + NC hits) -- to check nothing has run away to an extreme value
#===============================================================================

cat("========================================\n")
cat("PHYSICAL SURFACE USE COUNT CHECK\n")
cat("========================================\n\n")

pilot_base_uses <- data.frame(
  SurfaceID = c(reused_steel, reused_abs),
  PilotUses = 4,   # each reused surface already has 4 pilot conditions
  stringsAsFactors = FALSE
)

new_sample_uses <- main_design %>% count(SurfaceID, name = "NewSampleUses")
nc_uses <- nc_design %>% count(SurfaceID, name = "NCUses")

use_tally <- data.frame(SurfaceID = c(reused_steel, reused_abs, new_steel, new_abs)) %>%
  left_join(pilot_base_uses, by = "SurfaceID") %>%
  left_join(new_sample_uses, by = "SurfaceID") %>%
  left_join(nc_uses, by = "SurfaceID") %>%
  mutate(
    PilotUses = ifelse(is.na(PilotUses), 0, PilotUses),
    NewSampleUses = ifelse(is.na(NewSampleUses), 0, NewSampleUses),
    NCUses = ifelse(is.na(NCUses), 0, NCUses),
    TotalUses = PilotUses + NewSampleUses + NCUses
  ) %>%
  arrange(desc(TotalUses))

print(use_tally, row.names = FALSE)
cat("\nMax total use count:", max(use_tally$TotalUses),
    "| Previously validated trend-free up to: 6\n")
if (max(use_tally$TotalUses) > 6) {
  cat("[NOTE] ", sum(use_tally$TotalUses > 6),
      " surface(s) exceed the previously-validated 6-use range (by design, for the",
      "reused surfaces, and possibly further via random NC assignment).\n")
  cat("        Recommendation: include CumulativeUseNumber as a covariate in the\n")
  cat("        combined main-study analysis to directly check for any wear effect\n")
  cat("        emerging at these higher counts, per the design-logic discussion.\n\n")
}

#===============================================================================
# 5. EXISTING PILOT DATA REFERENCE (for pooling at analysis time)
#===============================================================================

cat("========================================\n")
cat("EXISTING PILOT DATA TO POOL (50g / 200g, wet-only)\n")
cat("========================================\n\n")

cat("At analysis time, pool in the pilot's own wet-only samples at 50g and\n")
cat("200g (RunIDs PILOT_001-032, Solvent_level == 'present', Pressure_g %in%\n")
cat("c(50, 200)) from pilot_data_nested.csv -- 4 steel + 4 ABS samples at each\n")
cat("of the two levels (16 total). Include a Study (Pilot vs Main) covariate\n")
cat("in the combined model to guard against the same kind of date-linked\n")
cat("confound already found and corrected for within the pilot itself (see\n")
cat("CONTEXT.md, RDX batch-effect investigation sessions).\n\n")

#===============================================================================
# 6. VALIDATION CHECKS
#===============================================================================

cat("========================================\n")
cat("DESIGN VALIDATION\n")
cat("========================================\n\n")

expected_new_samples <- length(c(reused_steel, reused_abs)) * length(shared_levels) +
                         length(c(new_steel, new_abs)) * length(all_levels)
cat(sprintf("Expected new samples: %d | Generated: %d -> %s\n",
            expected_new_samples, nrow(main_design),
            ifelse(expected_new_samples == nrow(main_design), "[OK]", "[MISMATCH]")))

level_counts_new_only <- main_design %>% count(Pressure_g, name = "N_new")
cat("\nNew samples per pressure level:\n")
print(level_counts_new_only, row.names = FALSE)

level_counts_combined <- level_counts_new_only %>%
  mutate(N_pilot_pooled = ifelse(Pressure_g %in% c(50, 200), 8, 0),
         N_combined = N_new + N_pilot_pooled)
cat("\nCombined (new + pooled pilot) samples per pressure level (target: 6 x 2 surfaces = 12 each):\n")
print(level_counts_combined, row.names = FALSE)

if (all(level_counts_combined$N_combined == 12)) {
  cat("\n[OK] Every pressure level reaches the target combined n (6/surface type, 12 total)\n")
} else {
  cat("\n[MISMATCH] Not every level reaches the target combined n -- review design\n")
}

surface_use_check <- main_design %>% count(SurfaceID, name = "NewTestsThisSurface")
cat("\nNew tests per surface (expect 3 for Reused, 5 for New):\n")
print(main_design %>% distinct(SurfaceID, SurfaceGroup) %>%
        left_join(surface_use_check, by = "SurfaceID"), row.names = FALSE)

#===============================================================================
# 7. SAVE OUTPUTS
#===============================================================================

main_design_out <- main_design %>%
  transmute(
    RunID, SampleID, SurfaceProcessingOrder, SurfaceID, Surface_type,
    TestOrder = BatchNumber,
    Pressure_level = as.character(Pressure_label),
    Pressure_g,
    Solvent_level, Solvent_detail,
    Test_DateTime, Operator, Temp_C, Humidity_pct,
    `Time Cleaned` = NA, `Time Spiked` = NA, `Time Swabbed` = NA, `Date Extracted` = NA,
    # kept for internal use by other outputs below, dropped before final write
    BatchNumber, Surface_type_internal = Surface_type
  )

# Negative controls, in the SAME column structure -- unlike the pilot (where
# NCs were never part of pilot_design_nested.csv and had to be written in
# separately by hand during execution), these are included directly this
# time per explicit request. Design-specific columns that don't apply to an
# NC (Pressure_level/Pressure_g/Solvent_level/Solvent_detail) are left NA,
# matching how NC rows are handled everywhere else in this study's pipeline
# (see pilot_analysis.R's own NC row construction).
nc_processing_order <- nc_design %>%
  left_join(surface_processing_order, by = "SurfaceID")

nc_design_out <- nc_processing_order %>%
  transmute(
    RunID, SampleID, SurfaceProcessingOrder, SurfaceID, Surface_type,
    TestOrder = BatchNumber,
    Pressure_level = NA_character_, Pressure_g = NA_real_,
    Solvent_level = NA_character_, Solvent_detail = NA_character_,
    Test_DateTime = NA, Operator = NA, Temp_C = NA, Humidity_pct = NA,
    `Time Cleaned` = NA, `Time Spiked` = NA, `Time Swabbed` = NA, `Date Extracted` = NA,
    BatchNumber, Surface_type_internal = Surface_type
  )

#===============================================================================
# 7a. REAL EXECUTION ORDER (per batch: NC first, then Surface 1-6 in the
# randomized TestSequence) -- computed FIRST so it can be used to sort the
# main master file below, not just the separate checklist.
#===============================================================================

sample_rows_checklist <- main_design %>%
  select(BatchNumber, TestSequence, RunID, SampleID, SurfaceID, Surface_type, Pressure_g, Solvent_detail) %>%
  mutate(RowType = "Sample") %>%
  arrange(BatchNumber, desc(Surface_type == "steel"), TestSequence) %>%
  group_by(BatchNumber, Surface_type) %>%
  mutate(SurfaceNumber = row_number()) %>%
  ungroup()

surface_number_lookup <- sample_rows_checklist %>%
  distinct(BatchNumber, Surface_type, SurfaceID, SurfaceNumber)

nc_rows_checklist <- nc_design %>%
  transmute(BatchNumber, TestSequence = NA_integer_, RunID, SampleID,
            SurfaceID, Surface_type, Pressure_g = NA_real_,
            Solvent_detail = solvent_detail, RowType = "NegativeControl") %>%
  left_join(surface_number_lookup, by = c("BatchNumber", "Surface_type", "SurfaceID"))

execution_order <- bind_rows(sample_rows_checklist, nc_rows_checklist) %>%
  # NC listed FIRST within its own (Batch, Surface_type) group -- negative
  # controls are collected before spiking begins for that group, not
  # interspersed among the numbered surfaces (confirmed convention: "NCs are
  # taken at the START of their own batch's session, not the end" -- see
  # CONTEXT.md RDX batch-effect investigation).
  arrange(BatchNumber, desc(Surface_type == "steel"),
          desc(RowType == "NegativeControl"), SurfaceNumber) %>%
  mutate(ExecutionOrder = row_number()) %>%
  select(RunID, SurfaceNumber, RowType, ExecutionOrder)

#===============================================================================
# 7b. MASTER LAB-NOTES FILE -- full pilot_design_nested.csv column structure
# (RunID,SampleID,SurfaceProcessingOrder,SurfaceID,Surface_type,TestOrder,
# Pressure_level,Pressure_g,Solvent_level,Solvent_detail,Test_DateTime,
# Operator,Temp_C,Humidity_pct,Time Cleaned,Time Spiked,Time Swabbed,
# Date Extracted), NCs included directly, but sorted in the REAL execution
# order (ExecutionOrder above -- NC first per batch/surface-type group, then
# Surface 1-6 in randomized TestSequence) rather than the nominal
# SurfaceProcessingOrder/TestOrder sort. This is the single file to use for
# lab notes -- previously the columns (this file) and the correct order
# (main_study_batch_checklist.csv) were split across two separate files;
# consolidated per explicit request.
#===============================================================================

main_design_full <- bind_rows(main_design_out, nc_design_out) %>%
  left_join(execution_order, by = "RunID") %>%
  arrange(ExecutionOrder) %>%
  select(-BatchNumber, -Surface_type_internal, -SurfaceNumber, -RowType, -ExecutionOrder)

design_file <- file.path(output_dir, "main_study_design_nested.csv")
write.csv(main_design_full, design_file, row.names = FALSE)
cat("\nSaved:", design_file, "(", nrow(main_design_full), "rows:",
    nrow(main_design_out), "samples +", nrow(nc_design_out), "negative controls,",
    "sorted in real execution order )\n")

nc_file <- file.path(output_dir, "main_study_negative_controls.csv")
write.csv(nc_design, nc_file, row.names = FALSE)
cat("Saved:", nc_file, "\n")

use_tally_file <- file.path(output_dir, "main_study_surface_use_tally.csv")
write.csv(use_tally, use_tally_file, row.names = FALSE)
cat("Saved:", use_tally_file, "\n")

# Simplified batch checklist (same row order as main_study_design_nested.csv
# above, reduced to just the execution-order-relevant columns -- kept as a
# quick-reference/printable view, e.g. for taping to the bench, while
# main_study_design_nested.csv remains the single complete lab-notes record).
batch_checklist <- bind_rows(sample_rows_checklist, nc_rows_checklist) %>%
  arrange(BatchNumber, desc(Surface_type == "steel"),
          desc(RowType == "NegativeControl"), SurfaceNumber) %>%
  select(BatchNumber, Surface_type, SurfaceNumber, RowType, RunID, SampleID,
         SurfaceID, Pressure_g, Solvent_detail)

checklist_file <- file.path(output_dir, "main_study_batch_checklist.csv")
write.csv(batch_checklist, checklist_file, row.names = FALSE)
cat("Saved:", checklist_file, "\n\n")

cat("========================================\n")
cat("BATCH-BY-BATCH TESTING ORDER (for lab execution)\n")
cat("========================================\n\n")
for (b in sort(unique(batch_checklist$BatchNumber))) {
  cat(sprintf("--- BATCH %d ---\n", b))
  for (st in c("steel", "abs")) {
    bd <- batch_checklist %>% filter(BatchNumber == b, Surface_type == st)
    if (nrow(bd) == 0) next
    cat(sprintf("  %s:\n", toupper(st)))
    for (i in seq_len(nrow(bd))) {
      row <- bd[i, ]
      if (row$RowType == "Sample") {
        cat(sprintf("    Surface %d: %-10s -> %4dg -> %s\n",
                    row$SurfaceNumber, row$SurfaceID, row$Pressure_g, row$SampleID))
      } else {
        cat(sprintf("    [NC, use Surface %d]: %-10s -> %s\n",
                    row$SurfaceNumber, row$SurfaceID, row$SampleID))
      }
    }
  }
  cat("\n")
}

#===============================================================================
# 8. SUMMARY
#===============================================================================

cat("========================================\n")
cat("MAIN STUDY DESIGN GENERATION COMPLETE\n")
cat("========================================\n\n")

cat("Design summary:\n")
cat("  - Reused pilot surfaces: 4 steel + 4 ABS (already have 50g/200g wet data)\n")
cat("  - New surfaces: 2 steel + 2 ABS (fresh, tested at all 5 levels)\n")
cat("  - Pressure levels: 10, 50, 100, 200, 300 g (Solvent fixed = present)\n")
cat("  - New samples to collect:", nrow(main_design), "\n")
cat("  - Negative controls:", nrow(nc_design), "(1 per surface type per batch, host randomized)\n")
cat("  - Total new runs (samples + NC):", nrow(main_design) + nrow(nc_design), "\n")
cat("  - Existing pilot samples to pool at analysis time: 16 (8 at 50g, 8 at 200g)\n")
cat("  - Combined n per pressure level after pooling: 6/surface type (12 total)\n\n")

cat("Next steps:\n")
cat("  1. Review main_study_design_nested.csv and main_study_batch_checklist.csv\n")
cat("  2. Confirm reused surfaces (Steel_01-04, ABS_01-04) are still in usable\n")
cat("     condition; re-clean per standard protocol before reuse\n")
cat("  3. Execute batches 1-3 (all 6 surfaces/type active), then batches 4-5\n")
cat("     (new surfaces only)\n")
cat("  4. Run GC-MS analysis; fill in recovery values\n")
cat("  5. Build main_study_analysis.R: pool with pilot_data_nested.csv's\n")
cat("     50g/200g wet-only rows, include a Study covariate, fit\n")
cat("     Recovery ~ Surface_type * (Pressure_g + I(Pressure_g^2)) +\n")
cat("     (1|SurfaceID_nested), and check CumulativeUseNumber as a covariate\n")
cat("     given several surfaces now exceed the previously-tested 6-use range\n\n")
