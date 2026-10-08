# UV Swabbing Analysis - Project Context

## Project Overview

**Goal**: Validate UV fluorescent powder as a surrogate/training aid for explosive trace recovery (PETN) from surface swabbing.

**Hypothesis**: UV powder recovery should correlate positively with analyte (explosive) recovery - operators who recover more UV powder should also recover more explosive traces.

**Current Finding**: NO positive correlation found between UV recovery and PETN recovery in the original FINEX study (all metrics negative). However, a calibration experiment (June 2026) confirms the UV method IS valid when powder loading is reduced to 25% of original concentration.

---

## Key Results

### Correlation Summary (Current Method)
- UV Recovery vs Analyte (High pressure): r = -0.34
- UV Recovery vs Analyte (Normal pressure): r = -0.76
- UV Recovery vs Analyte (Low pressure): r = 0.02

### Best Alternative Metrics Tested
| Metric | Correlation with Analyte (Normal) |
|--------|-----------------------------------|
| Ratio_k20 (Before/Blank at threshold 20) | r = -0.35 |
| Single_k15 (Before-Blank difference) | r = -0.34 |
| AUC_Diff_Steep_10_20 | r = -0.32 |

**All correlations are weak or negative** - no metric shows the expected positive relationship.

---

## Identified Problems

### 1. Powder Overloading (Primary Suspect)
- Many samples show 97-99% coverage at threshold k=10 (saturation)
- High powder samples (Before_k20 > 70%) show random UV-analyte relationships
- 1D threshold analysis cannot distinguish thick piles from thin layers
- Clumping/piling causes measurement non-linearity

**Evidence**: Samples with highest powder (6_Original, 9_Original) show no consistent pattern with analyte

### 2. Lighting/Background Variation
- Blank curves vary 8.2-fold between samples (CV = 54.9%)
- Expected: Identical blanks (darkroom/controlled lighting)
- Actual: Large variation indicates uncontrolled conditions
- Confirmed: Not using darkroom during imaging

### 3. Current Method Artifacts
- 6/17 samples have physically impossible recoveries (<0% or >100%)
- Alignment procedure (90% point) creates artificial data
- Order enforcement masks real problems

---

## Data Structure

### Participants
- 11 volunteers performing swabbing
- Some have "Original" and "Repeat" trials (same conditions, different session)
- Participant mapping: see `Participants.csv`

### UV Images
- 3 images per trial: Blank (unswabbed clean surface), Before (powder deposited), After (post-swab)
- Threshold analysis: k=1 to k=100
- File location: `UV Analysis/[Participant]/[Trial]/Threshold Analysis/`

### Analyte Data
- Explosive: PETN
- Recovery measured via LC-MS at 3 pressures: High, Normal, Low
- Recovery = (CalConc / 10 ng/uL) x 100%
- File: `087 Analysis/DataProcessing/AverageRecoveries.csv`

---

## Analysis Pipeline

### Scripts
1. `00_GlobalCode.R` through `07_PatternComparison.R` - Original analysis pipeline
2. `UV_Analyte_Diagnostic.R` - Comprehensive correlation testing (original method + alternatives)
3. `UV_Recovery_Reprocessing.R` - Alternative metric testing (5 methods, 26 parameter combinations, no alignment)
4. `UV_Calibration_Analysis.R` - Powder loading calibration experiment processing

### Output
- `UV Analysis/Diagnostic_Output/` - Contains correlation plots and summary CSV
- `UV Analysis/Reprocessing_Output/` - Alternative method results and correlation summary
- `Shared with Oliver/Calibration_Output/` - Calibration experiment results and recommendation
- Key file: `07_Metric_Correlations.csv`

---

## Key Threshold Regions

| Threshold | Region | Characteristics |
|-----------|--------|-----------------|
| k=1-10 | Saturation | High powder, prone to overload |
| k=10-20 | Steep | Most sensitive to powder amount |
| k=20-35 | Transition | Moderate discrimination |
| k=35-65 | Flat | Current method uses this range |

**Recommendation**: Use k=15-25 range for optimal discrimination (if powder loading reduced)

---

## Project Constraints

- Powder deposition: Aerosolized, difficult to control evenly
- Camera: Fixed position and settings for all images
- Lighting: NOT in darkroom (source of variation)
- No calibration standards (unknown powder masses)
- No 3D imaging capability

---

## Next Steps Recommended

### Completed (June 2026)
1. ~~Reduce powder deposition by 50-70%~~ → **Calibration experiment confirmed 25% is optimal**
2. ~~Run calibration series with known relative powder amounts~~ → **Done: 50%, 25%, 10% tested**
3. ~~Determine saturation threshold~~ → **k=20 at 25% concentration**
4. ~~Establish optimal operating range~~ → **Before_k20 = 71%, Recovery scales linearly with R²=0.979**

### Still Recommended (if repeating the study)
1. **Use darkroom or lighting box** - Would further reduce blank variation (CV was 13.4% in calibration vs 54.9% in original)
2. **Add fluorescence reference card** - Normalize for any remaining lighting changes
3. **Use single threshold k=20 at 25% powder** - No alignment procedure needed
4. **Verify with analyte correlation** - Repeat swabbing study with reduced powder to confirm positive UV-PETN correlation

---

## Files Reference

### UV Data
- `UV Analysis/Blank Threshold Analysis/` - Blank curves for each sample
- `UV Analysis/Unswabbed Threshold Analysis/` - Before curves for each sample
- `UV Analysis/AverageRecovery.csv` - Current method results
- `UV Analysis/Diagnostic_Output/07_Metric_Correlations.csv` - All metric correlations
- `UV Analysis/Reprocessing_Output/Correlation_Summary.csv` - Alternative method correlations (26 methods)
- `UV Analysis/Reprocessing_Output/Recovery_AllMethods.csv` - Full recovery results (443 rows)

### Calibration Data
- `Shared with Oliver/output/` - Raw ImageJ particle CSVs (4,500 files, 45 images × 100 thresholds)
- `Shared with Oliver/Calibration_Output/Recommendation.txt` - Optimal configuration
- `Shared with Oliver/Calibration_Output/Calibration_Results.csv` - Full recovery data
- `Shared with Oliver/Calibration_Output/Image_Mapping.csv` - DSC number → condition mapping

### Analyte Data
- `087 Analysis/DataProcessing/AverageRecoveries.csv` - PETN recovery by participant
- `087 Analysis/Analysis 1/` and `Analysis 2/` - Raw LC-MS data

### Scripts (in TransferPersistenceRecoveryExplosives/UVSwabbing/)
- `00_GlobalCode.R` through `07_PatternComparison.R` - Original analysis pipeline
- `UV_Analyte_Diagnostic.R` - Original diagnostic script
- `UV_Recovery_Reprocessing.R` - Alternative metric testing (5 methods, standalone)
- `UV_Calibration_Analysis.R` - Calibration experiment processing (standalone)

---

## Critical Questions Addressed

1. **Why negative correlation?** - Powder saturation (97-99% at k=10) destroyed measurement sensitivity. At correct loading (25%), recovery is proportional and linear.
2. **Which metric to use?** - Single threshold k=20 with no alignment. Recovery = (Before - After) / (Before - Blank) × 100.
3. **Is alignment necessary?** - No. Alignment creates artifacts and is unnecessary when powder loading is correct. Confirmed by calibration experiment (R²=0.979 without alignment).
4. **What threshold range?** - k=20 is optimal at 25% concentration (Before=71%, discrimination=66.6%, CV=13.4%).
5. **What powder concentration?** - 25% of original. 50% still saturates (R²=0.77, non-proportional). 10% has insufficient signal (swab redistribution dominates).
6. **Is recovery proportional to area swiped?** - YES at 25%: 1 swipe=3.5%, 2 swipes=7.1% (2.06×), 3 swipes=9.3% (2.70×). PASS.

---

## Reprocessing Analysis (June 2026)

### Script: `UV_Recovery_Reprocessing.R`

**Purpose**: Test whether alternative image processing methods (without alignment) could produce a positive UV-analyte correlation from the original FINEX study data.

**Methods tested** (5 approaches, 26 parameter combinations):
1. **Direct single-threshold**: Recovery at fixed k values (k=5 to k=50)
2. **Crossing-point shift**: Horizontal distance between curves at fixed %Area levels (30-70%)
3. **AUC (Area Under Curve)**: Integrated area difference over threshold ranges
4. **Per-sample optimal threshold**: Recovery at k where Before-After is maximised
5. **Blank-normalised ratio**: Ratio-based correction for lighting variation

**Results** (n=16 samples, all with complete Blank/Before/After data):
- **No method produced a positive correlation with PETN recovery** (Normal pressure)
- Best correlation: CrossingShift 60% (r = -0.146, p = 0.59) — essentially zero
- Most negative: SingleThreshold k=30 (r = -0.494, p = 0.052) — borderline significant negative
- All 26 method/parameter combinations yield negative r with Normal pressure
- The weak negative correlation (r ≈ -0.4) suggests operators who deposit more visible powder may swab less effectively

**Conclusion**: The failure of the original study is NOT caused by the processing method. No alignment, threshold selection, AUC, or normalisation approach can rescue a positive correlation from data collected with saturated powder loading.

**Output**: `UV Analysis/Reprocessing_Output/` — Correlation_Summary.csv, Recovery_AllMethods.csv, 5 diagnostic plots

---

## Calibration Experiment (June 2026)

### Script: `UV_Calibration_Analysis.R`

**Purpose**: Determine optimal powder loading and verify that UV recovery is proportional to area swiped when saturation is eliminated.

### Experimental Design
- **3 concentrations**: 50%, 25%, 10% of original powder amount
- **3 surfaces** (A, B, C) per concentration — tests reproducibility
- **5 conditions** per surface: Blank, Before (powder deposited), 1 swipe, 2 swipes, 3 swipes
- **Total**: 45 images, each thresholded at k=1 to k=100 in ImageJ
- **Input**: Raw ImageJ "Analyze Particles" CSV files (4,500 files)
- **Image sequence**: Sorted by DSC camera number, mapped to conditions by position

### Data Processing
1. **Stage 1**: Aggregate raw particle CSVs → %Area curves (sum particle areas / total image area)
2. **Stage 2**: Map images to experimental conditions by DSC number order
3. **Stage 3**: Compute recovery at all thresholds: `(Before - After) / (Before - Blank) × 100`
4. **Stage 4**: Score each threshold by discrimination × linearity × precision, with saturation filter
5. **Stage 5**: Generate recommendation based on composite scoring

### Key Results

**Recommended configuration: 25% powder at threshold k=20**

| Metric | Value |
|--------|-------|
| Linearity (R²) | **0.979** |
| Slope | 2.93% recovery per swipe |
| Surface-to-surface CV | 13.4% |
| Before %Area at k=20 | 71.0% |
| Before-Blank discrimination | 66.6% |
| Proportionality test | **PASS** |

**Recovery by swipe count (25%, k=20):**

| Swipes | Recovery | Ratio to 1 swipe | Expected |
|--------|----------|-------------------|----------|
| 1 | 3.5% | 1.00 | 1.0 |
| 2 | 7.1% | 2.06 | 2.0 |
| 3 | 9.3% | 2.70 | 3.0 |

### Concentration Comparison

| Concentration | Best R² | Proportional? | Why? |
|---------------|---------|---------------|------|
| **50%** | 0.775 | NO | Still partially saturated; poor surface reproducibility (CV 31-34%) |
| **25%** | **0.979** | **YES** | Optimal range; excellent linearity and precision |
| **10%** | — | NO | Insufficient signal; swab redistributes rather than removes powder; Surface B shows After > Before |

### Why 10% Failed
At 10% powder loading:
- The Before-After difference is within noise for most surfaces
- Swiping redistributes powder rather than removing detectable amounts
- Surface B shows higher %Area after swiping than before (physically impossible for genuine removal)
- Alignment cannot fix this — the problem is insufficient y-axis signal, not x-axis offset

### Conclusions
1. **The original FINEX study failed due to powder overloading**, not a fundamental flaw in the UV surrogate concept
2. **At 25% concentration, recovery IS proportional to area swiped** (R²=0.98), validating the underlying method
3. **A single threshold (k=20) with no alignment** produces reliable, reproducible measurements
4. **The alignment procedure used in the original study was unnecessary** and introduced artifacts
5. **Goldilocks principle**: 50% too much (saturates), 10% too little (no signal), 25% optimal

### Output
- `Shared with Oliver/Calibration_Output/` — Recommendation.txt, Calibration_Results.csv, 6 diagnostic plots
- Excluded images: DSC2635 (mis-photo), DSC2650 (duplicate B Blank)

---

## Open Questions

1. ~~What is the exact powder deposition method?~~ → Aerosolized; tested at 50%, 25%, 10% concentrations
2. ~~Can powder amount be reduced while maintaining even distribution?~~ → Yes, 25% works
3. Is the powder homogeneous or subject to settling/separation?
4. What is the particle size distribution of UV powder?
5. ~~Is fluorescence intensity linear with thickness in the expected range?~~ → Yes, at 25% loading (R²=0.979)
6. Would the positive calibration result (proportional recovery at 25%) translate to a positive UV-PETN correlation in a repeat swabbing study?

---

## Colour Palette (September 2026)

All plotting scripts in this folder now source a shared, thesis-wide colour palette (`TransferPersistenceRecoveryExplosives/thesis_palette.R`, Okabe-Ito colourblind-safe qualitative palette + viridis for ordinal/high-cardinality data), added via `00-GlobalCode.R`'s own `source()` call (inherited by every script that requires `00-GlobalCode.R` first) or, for the standalone scripts that don't, their own `source()` near the top.

Fixed two real cross-file colour inconsistencies found during this work: the Blank/Before/After factor previously had DIFFERENT colours in `Diagnostic_ThresholdPlots.R` (blue/green/red) vs `UV_Recovery_Reprocessing.R` (blue/red/green3) for the exact same three categories -- both now use the shared `pal_image_stage` (Blue/Orange/Bluish-Green). `06-RecoveryAnalysis.R`'s `ColourPalette2` (previously an unnamed/positional 3-colour vector relying on alphabetical factor-level order to line up correctly) is now a proper named alias of `pal_image_stage`.

The 17-level `Sample` factor (`UV_Analyte_Diagnostic.R`) and the genuinely-ordered `Pressure` (High/Normal/Low) and `Concentration` (50%/25%/10%) factors now use `scale_colour_viridis_d()` instead of ggplot's default hue wheel or a hand-picked qualitative set -- neither fits a small fixed qualitative palette well (too many levels; or genuinely ordered data, which a qualitative palette isn't designed for).

**Bug found and fixed in `Diagnostic_ThresholdPlots.R` (unrelated to colour, pre-existing)**: line 437 contained `cat("=".join(rep("=", 60)), ...)` -- Python's `.join()` method syntax, invalid in R. This causes the ENTIRE script to fail to parse, meaning none of its code (including its own plots) can currently run at all. Left unfixed (out of scope for a colour-only change) but flagged here explicitly since it blocks even verifying this script's own palette colours by execution.

**Bug found and fixed in `UV_Recovery_Reprocessing.R`**: its `Diagnostic_RawCurves.png` plot had a subtitle caption ("Blue=Blank, Red=Before swab, Green=After swab") that would have gone stale/wrong the moment the underlying colours changed (Before is now Orange, After is now Bluish-Green, not Red/Green) -- caught by actually rendering the plot and visually comparing the legend against the caption text, not just by reading the code. Fixed to match the new colours.

**Verified**: `UV_Recovery_Reprocessing.R` executed end-to-end against real data (exit code 0); all 5 output PNGs regenerated; visually confirmed `pal_image_stage` (Diagnostic_RawCurves.png) and `pal_method_uv` (Comparison_Boxplot.png) both render with the correct new colours and the corrected caption. The other 7 files in this folder were parse-checked (all pass) and had their `pal_*` key strings verified by direct comparison against real column values, but were not individually executed this session (`UV_Calibration_Analysis.R`'s own input folder isn't currently populated in this OneDrive checkout; the remaining scripts require `00-GlobalCode.R` to be run first in the same interactive session, which wasn't done for all of them here).

### Update (same day, continued) -- user confirmed running everything

- **`Diagnostic_ThresholdPlots.R`'s pre-existing Python-syntax bug (documented above) was fixed** (`cat("=".join(rep("=", 60)), ...)` -> `cat(paste(rep("=", 60), collapse=""), ...)`, mirroring the identical working line immediately above it). Executed end-to-end afterward (exit code 0); visually confirmed `pal_image_stage` renders correctly (Blue/Orange/Bluish-Green for Blank/Before/After) in `Surface1_Rep1_01_Uncorrected.png`.
- **Attempted the full `00`->`04`->`07` numbered pipeline** (source `00-GlobalCode.R`, then `04`/`05`/`06`/`07` in sequence): failed at `04-BlankImageAnalysis.R` with `no applicable method for 'pivot_longer' applied to an object of class "NULL"`. Root cause: `BlankThresholdResults.dir`/`Unswabbed Threshold Analysis`/`ThresholdResults`/`Analysis` under `Shared with Oliver/` are all **empty in this local OneDrive checkout** -- the upstream `01`-`03` data-prep scripts (not touched by this session, not part of the colour work) have never been run/synced here. This is the same root cause as `UV_Calibration_Analysis.R`'s already-documented missing-data issue above, now confirmed to affect `04`-`07` too. Not fixable from within this session (would require re-running the raw ImageJ/image-organisation pipeline against original images, well outside a colour-palette change) -- these 4 files' colours remain verified by code review/exact-key-matching only, not by execution.
- **`UV_Analyte_Diagnostic.R`**: found a genuine pre-existing, unrelated bug while attempting to run it -- `Error in left_join(): Can't join x$Participant with y$ParticipantNumber due to incompatible types` (`x$Participant` is `<integer>`, `y$ParticipantNumber`'s actual type depends on the real `AverageRecoveries.csv` contents). Occurs well before this session's edited plot code is reached. Not a one-line typo like the `Diagnostic_ThresholdPlots.R`/`SwabMountAngles.R` fixes -- left uninvestigated/unfixed, disclosed here rather than guess-fixed.

**Final tally for this folder**: 2 of 8 files executed successfully with colours visually confirmed (`Diagnostic_ThresholdPlots.R`, `UV_Recovery_Reprocessing.R`); 5 blocked by the missing upstream pipeline data (`04`-`07`, `UV_Calibration_Analysis.R`); 1 blocked by a genuine pre-existing unrelated bug (`UV_Analyte_Diagnostic.R`).

---

## Session Summary (September 4, 2026) — `UV_Analyte_Diagnostic.R`'s `left_join()` Bug Fixed and Verified

### Root cause

The previously-disclosed `Error in left_join(): Can't join x$Participant with y$ParticipantNumber due to incompatible types` was traced to `087 Analysis/DataProcessing/AverageRecoveries.csv` itself: after the 10 real per-participant data rows, the file has 2 fully-blank trailing rows followed by a final `average` summary row (`ParticipantNumber = "average"`). `read.csv()` reads the whole `ParticipantNumber` column as character because of this one non-numeric value, while `uv_valid$Participant` (derived from parsing UV sample filenames) is integer -- `dplyr::left_join()` refuses to join columns of mismatched type.

### Fix

Immediately after `analyte_data <- read.csv(...)`, drop any row where `ParticipantNumber` isn't numeric (catches both the blank rows and the `"average"` row) and coerce the column back to integer, so every downstream use (`left_join()`, the `%in%` check, and the `==` comparisons in the alternative-metrics loop) sees a clean, consistently-typed column. The `AverageRecoveries.csv` file itself was left untouched -- this is a real experimental-results export from another process, not something to hand-edit.

### Verification

Ran the full script end-to-end (exit code 0, previously impossible). Confirmed correct behaviour: correlation analysis now runs (`vs Normal Pressure: r = -0.779`, consistent with the already-known negative-correlation finding elsewhere in this file), and the participant-7 gap is now handled gracefully (`Skipping 7_Original - no analyte data for participant 7`) rather than crashing. All 15 output PNGs + `07_Metric_Correlations.csv` regenerated (confirmed by timestamp). Visually confirmed `01_Blank_Curves_All.png` (17-level `Sample` factor, `scale_colour_viridis_d()`) and `06_Correlation_All.png` (ordered `Pressure` factor High/Normal/Low, viridis) both render with correct, properly-ordered colours.

**Updated tally for this folder**: 3 of 8 files now executed successfully with colours visually confirmed (`Diagnostic_ThresholdPlots.R`, `UV_Recovery_Reprocessing.R`, `UV_Analyte_Diagnostic.R`); 5 still blocked by the missing upstream pipeline data (`04`-`07`, `UV_Calibration_Analysis.R`) -- unchanged, not investigated this session.

### Files Modified

`UV_Analyte_Diagnostic.R` (added a filter + `as.integer()` coercion on `analyte_data$ParticipantNumber` right after loading). `CONTEXT.md` (this entry).

---

## Session Summary (October 8, 2026) — Red-Team Review: 3 Confirmed Bugs Fixed, 2 Scripts Unblocked by Finding Their Real Data

### Background

Follow-on from a full-repo red-team code review (see root `OPEN_ITEMS.md`, items tagged "Oct 7 2026 red-team review") that audited every sub-project for code defects and analytical-workflow errors. This entry covers the 3 confirmed, fixed bugs in this project, plus two scripts/datasets that turned out not to be genuinely `[Blocked]` after all -- the real data existed, just not at the stale `Shared with Oliver/` path hardcoded as these scripts' defaults. See `GCMSQuantitation/CONTEXT.md` and `ASTRA/doe/CONTEXT.md` for the equivalent entries in those projects from the same review.

### Bug 1: `%Area` Extraction Picked Up Raw Pixel Counts Instead of the Percentage

**Files**: `02_CombineResults.R:37-56`, `02b_CombineResults_ImageJResults.R:44-56` (`read_percent_area()`/`read_percent_area_from_folder()`).

**Root cause** (confirmed by direct test): R's `read.csv()`/`make.names()` sanitizes a literal `%Area` ImageJ Summarize-table header to `X.Area` (ONE dot), never to the literal `%Area` string and never to `X..Area` (TWO dots). Both scripts' explicit column checks tested for the two-dot spelling, so neither could ever match -- every row silently fell through to a generic `grep("Area", ...)` fallback. In `02_CombineResults.R` specifically, that fallback had no exclusion for `Total Area`, so it picked up the raw pixel count (e.g. `23,348,160`) instead of the percentage (`100`). `02b_CombineResults_ImageJResults.R` had the identical two broken checks but happened to avoid the bug in practice via one extra fallback line already excluding `"Total"` from candidates.

**Fix**: corrected the literal/regex to the one-dot spelling in both files; added the `"Total"`-exclusion to `02_CombineResults.R`'s own fallback too (so both scripts now have the same two-layer defense, rather than one relying on a coincidence); added a 0-100 plausibility guard (any extracted value outside that range is rejected to `NA` with a loud `warning()` instead of silently propagating a six-figure pixel count); made the destructive end-of-script raw-CSV cleanup conditional on that guard passing, not just on file *count* being complete; added a `DELETE_INDIVIDUAL_CSVS <- FALSE` safety toggle (default off) to both scripts.

**Verified against real data, not synthetic**: found the real UV Powder Swabbing pattern-study data at `.../Experimental Results/UV Powder Swabbing/ASTRA Testing/OrganizedImages/` (1,800 real per-threshold ImageJ CSVs, 6 Surface/Rep folders) -- not the stale `Shared with Oliver/` default. Re-ran via a one-off driver overriding `OrganizedImages.dir`/`ThresholdResults.dir` after sourcing `00_GlobalCode.R` (neither script's hardcoded default was changed). Diffed the regenerated `Summary.csv` files against the pre-existing historical ones (backed up first): 1,796 of 1,800 values identical. Traced all 4 differences to a second, previously-unknown finding (see Bug 2) rather than a remaining flaw in the extraction fix itself.

### Bug 2: 4 Raw ImageJ Exports Are Per-Particle Dumps, Not Summarize Rows (Found While Verifying Bug 1)

Investigating those 4 differing values traced them to 4 real raw CSVs (`Surface2_Rep1/Before[18].csv`, `Surface2_Rep2/Before[83].csv` + `Blank[79].csv`, `Surface3_Rep1/After[56].csv`) that are genuine per-particle "Analyze Particles" exports (header `" ,Area,Mean,Min,Max"`, thousands of rows) mis-saved under Summarize-format filenames, rather than true single-row Summarize CSVs. These have a column literally named `"Area"` too (just the wrong kind -- one particle's pixel area, not `%Area`), so the Bug 1 column-name checks can't distinguish them, and the first particle's small pixel area (4/31/7/15 in the 4 real cases) coincidentally passed the 0-100 plausibility guard undetected.

**Fix**: added a second, structural guard -- `nrow(csv_data) != 1` -- to both scripts' extraction functions: a genuine ImageJ Summarize CSV always has exactly one data row, regardless of column names. Re-ran against the real data: confirmed all 4 affected files are now correctly flagged by name via `warning()`, set to `NA` instead of a coincidentally-plausible-looking wrong value, and their 3 containing folders excluded from cleanup eligibility.

**Remaining action item (lab work, not a code fix)**: those 4 raw ImageJ exports need to be redone in Summarize mode.

### Bug 3: `01_OrganizeImages.R`'s Pattern Labels Didn't Match `06`/`07` -- Fixed in the Opposite Direction Than First Planned

**File**: `01_OrganizeImages.R:58-70`.

Originally planned to change `06_RecoveryAnalysis.R`'s/`07_PatternComparison.R`'s `pattern_order` vector (`"50g_BackandForth"`/`"50g_Snake"`/`"50g_Ratchet"`) to match `01_OrganizeImages.R`'s current source code (`"Snake"`/`"BaF"`/`"Ratchet"`, no prefix). **Checked the real, already-existing `ImageMapping.csv` for this study first** and found it already uses the `"50g_"`-prefixed names -- i.e. `06`/`07` were already correct, and `01`'s current code (which has always read `"Snake"`/`"BaF"`/`"Ratchet"` in this repo's git history) was the one out of sync with the real data. Fixing `06`/`07` instead would have broken the one real dataset that exists for this study, reproducing the exact bug on a different pair of scripts.

**Fix**: changed `01_OrganizeImages.R`'s three pattern string literals to `"50g_Snake"`/`"50g_BackandForth"`/`"50g_Ratchet"` instead. The underlying surface/rep -> pattern assignment logic was already correct (confirmed to reproduce the real `ImageMapping.csv`'s mapping exactly); only the label strings were out of sync.

**Verified via a safe dry run**: sourced the fixed script with `OrganizedImages.dir` redirected to a temp folder (`SourceImages.dir` pointed at the real raw `Images/` folder, read-only) -- confirmed the regenerated mapping's `Pattern` column is identical to the real `ImageMapping.csv` for all 6 Surface/Rep folders.

### Bug 4 (found while verifying Bug 3): `06_RecoveryAnalysis.R` Crashed on NA Instead of Skipping Cleanly

Re-running the full `01`->`06`->`07` chain against the real data (using the real `ImageMapping.csv` + the `Summary.csv` files regenerated by the Bug 1/2 fix) surfaced a further, previously-latent crash: `06_RecoveryAnalysis.R`'s 50%-order-check (`if (Before50 <= After50)`) has no NA handling, and 3 of the 6 real folders now correctly carry an NA (from the Bug 2 fix) at a threshold landing on this comparison -- crashing with `"missing value where TRUE/FALSE needed"` instead of the previous (silent, wrong) pre-fix behaviour.

**Fix**: added an NA guard immediately after `UncorrectedThresholdResults` is built -- skips the folder cleanly with a clear, actionable message if any `Blank`/`Before`/`After` Area value is `NA`, rather than crashing partway through or computing a result from incomplete data.

**Real consequence found**: all 3 affected folders share the same pattern (`"50g_BackandForth"`), so the pattern comparison now correctly shows **zero** `BackandForth` samples (previously n=3, computed from silently-wrong values pre-fix) -- only `Snake` (n=2) and `Ratchet` (n=1) remain, insufficient for the ANOVA to run at all. This is a more honest reflection of how little valid data currently exists for this sub-study, not a regression -- re-exporting the 4 flagged raw ImageJ files (Bug 2) would restore the `BackandForth` group. Confirmed zero regression for the 3 unaffected folders (Surface1_Rep1/Rep2, Surface3_Rep2) -- their recomputed recovery means matched the pre-existing historical `AverageRecovery.csv` values exactly (84.027/59.715/44.832%).

### Bug 5: `UV_Calibration_Analysis.R`'s Cross-Concentration "Best Configuration" Pick Was Methodologically Invalid

**File**: `UV_Calibration_Analysis.R:458-473`.

`Composite_Score` (= `Norm_Discrimination * Norm_Linearity * Norm_Precision`) uses `Norm_Discrimination`, which is min-max-normalised SEPARATELY within each `Concentration` group. That's valid for `optimal_per_conc` (picking the best THRESHOLD within a fixed concentration -- the per-group rescaling cancels out) and the per-concentration "top 3 alternatives" list in `Recommendation.txt`, but `overall_best` then used the SAME `Composite_Score` to pick the single best CONCENTRATION *and* threshold together via `slice_max()` -- comparing scores each independently rescaled to their own concentration's own observed range (so a 0.9 at 50% and a 0.9 at 25% don't represent the same absolute discrimination). `Norm_Linearity` (R-squared, already absolute 0-1 by definition) and `Norm_Precision` (already an absolute fraction) were never part of the problem.

**Fix**: added a second, GLOBALLY-normalised discrimination score (`Norm_Discrimination_Global`, min/max taken across ALL concentrations) and a corresponding `Composite_Score_Global`, used only by `overall_best`. Left `optimal_per_conc`, the per-concentration top-3 list, and the `Optimal_Threshold_Score.png` plot's underlying data unchanged (all legitimate within-group comparisons) -- added a clarifying subtitle/axis label to that plot instead, since it visually invited the same cross-colour peak-height misreading the code itself used to make.

**Also found while fixing this**: the `[Blocked]` status this script carried in `OPEN_ITEMS.md` ("input folder not populated") wasn't actually accurate -- the real 4,500 raw ImageJ particle CSVs exist at `.../Experimental Results/UV Powder Swabbing/Powder Loading/output/`, same root cause as Bug 1/2's data being found at `ASTRA Testing/` rather than the stale `Shared with Oliver/` default.

**Verified against the full real dataset** (4,500 raw CSVs, 45 images, via an in-memory text patch of `Data.dir`/`Output.dir` -- nothing written back to the repo script): confirmed zero regression in every upstream stage (`Linearity_Results.csv`/`Calibration_Results.csv`/`ThresholdCurves_All.csv`/`Image_Mapping.csv` byte-identical to the pre-existing historical output). Confirmed **the final recommendation is unchanged** -- `25% @ k=20` wins under both the old (invalid) and new (valid) scoring mechanisms, with the top 5 globally-ranked configurations all still from the `25%` group. The flawed methodology did not actually produce a wrong headline conclusion for this dataset, but it's now correctly justified by a valid comparison rather than coincidentally correct despite an invalid one.

### Files Modified

`02_CombineResults.R`, `02b_CombineResults_ImageJResults.R`, `01_OrganizeImages.R`, `06_RecoveryAnalysis.R`, `UV_Calibration_Analysis.R`, root `OPEN_ITEMS.md` (all items marked fixed with cross-reference back to this entry, and the two incorrectly-`[Blocked]` items corrected). This entry (`CONTEXT.md`). Real-data output regenerated in place (with backups taken first) at `ASTRA Testing/ThresholdResults/`, `ASTRA Testing/Analysis/`, and `Powder Loading/Calibration_Output/`.

---

*Last updated: October 2026 (red-team review fixes added)*
*Key scripts: UV_Recovery_Reprocessing.R, UV_Calibration_Analysis.R*
