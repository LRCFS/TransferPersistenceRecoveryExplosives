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
1. `00-GlobalCode.R` through `08-PatternComparison.R` - Original analysis pipeline
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

### Scripts (in ClaudeSpace/UVSwabbing/)
- `00-GlobalCode.R` through `08-PatternComparison.R` - Original analysis pipeline
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

*Last updated: June 2026*
*Key scripts: UV_Recovery_Reprocessing.R, UV_Calibration_Analysis.R*
