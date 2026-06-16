# GCMSQuantitation - Project Context

## Project Overview

R-based forensic science data pipeline for **detecting and quantifying explosive compounds (PETN and RDX)** in swab samples analysed by GC-MS. Developed by the Leverhulme Research Centre for Forensic Science (University of Strathclyde / University of Dundee), licensed AGPL-3.0.

## Project Structure (Consolidated - May 2026)

```
GCMSQuantitation/
├── GlobalCode.R              # Main configuration (set DataFolder, RTs, toggle use_15nrdx_for_petn)
├── Code/                     # Active pipeline scripts
│   ├── 01_MsFilesReorganiser.R  # Convert .ms1 files to TIC/SIM CSVs
│   ├── 02_PeakDetection.R        # Peak detection and integration
│   ├── 03_Quantification.R       # Calibration, quantification, drift correction
│   ├── 04_CollateStudyResults.R  # Collate results across all runs
│   ├── 05_StatisticalAnalysis.R  # Statistical analysis of recovery data
│   └── ModPeaks.R                # Signal processing functions
├── Diagnostics/               # Diagnostic/analysis scripts
│   ├── SequenceDiagnostics.R
│   ├── DriftCorrectionComparison.R
│   ├── DriftCorrectionDiagram.R
│   ├── Partial_Drift_Correction.R  # Apply drift correction to specific region
│   ├── SplitDualMethod.R
│   └── DualMethodProcess.R
├── Archive/                  # Deprecated scripts (for reference)
│   ├── Legacy_Code/          # Original Code/ folder contents
│   ├── v1-NoNG/              # Previous v1-NoNG folder
│   └── v2-NG_Deprecated/     # Previous v2-NG/Code/ contents
├── Test Data/                # Example output (documentation)
└── CONTEXT.md                # This file
```

## Pipeline Architecture

Sequential stages, orchestrated from `GlobalCode.R`:

| Stage | Script | Purpose |
|---|---|---|
| 0 | `GlobalCode.R` | Configuration, constants, library loading. Sources `Code/ModPeaks.R`. |
| 1 | `Code/01_MsFilesReorganiser.R` | Convert `.ms1` files to structured TIC/SIM CSVs |
| 2 | `Code/02_PeakDetection.R` | Signal processing: baseline correction, peak detection, SNR, integration |
| 3 | `Code/03_Quantification.R` | Parse sequence logs, build calibrations, quantify unknowns, QC assessment |
| 4 | `Code/04_CollateStudyResults.R` | (Optional) Collate all runs into study-level dataset |
| 5 | `Code/05_StatisticalAnalysis.R` | (Optional) Statistical analysis of recovery variance components |

**NG (nitroglycerin) has been removed** from the current pipeline. Only 15N-RDX (m/z 122) is used as the internal standard for both PETN and RDX ratio calibration.

### Configuration Toggle

In `GlobalCode.R`:
```r
use_15nrdx_for_petn <- TRUE   # TRUE: PETN uses 15N-RDX IS for ratio calibration
                               # FALSE: PETN uses PA only (no ratio)
```

Data flow: Raw Agilent `.D` files -> ProteoWizard MSConvert -> `.ms1` -> R pipeline -> calibration plots + quantitated results (CSV/XLSX/PNG)

### ModPeaks.R -- Signal Processing Engine (v2-NG: 632 lines)

**File**: `Code/ModPeaks.R` (sourced by GlobalCode.R)

Contains the core signal processing functions used by Test.R:

| Function | Purpose |
|---|---|
| `ModPeaks()` | Peak detection algorithm: sweeps threshold levels to find peaks above minimum peak height/width |
| `apply_baseline_correction()` | Rolling-ball baseline correction via the `baseline` package |
| `deinterleave_sim()` | Removes interleaved SIM scan-group pairs, keeping only Group B (higher-intensity second scan) |
| `trapz_area()` | Trapezoidal integration of retention-time/intensity pairs |
| `process_sim_channel()` | Full SIM channel pipeline: m/z filter (+/- 0.5 Da) -> deinterleave -> smooth -> baseline correct -> RT window filter -> peak detect |
| `calculate_snr()` | Signal-to-noise ratio: peak_height / sd(noise_region) |
| `extract_peak_area()` | Peak area extraction with SNR-based acceptance filter (Below_LOD / Below_LOQ / Quantifiable) |
| `plot_integration()` | Generates integration verification TIFF plots showing baseline, peak, integration boundaries, and annotations |

## Study Context: FINEX Swabbing Study

Analysing swab samples for explosives (PETN, RDX) across multiple laboratories.

### Study Design

- **Labs**: 1 to 37
- **Participants per lab**: 1 or 2 (some labs only have 1)
- **Surfaces per participant**: Steel (S), Glass (G), ABS-Smooth (ABS-S), ABS-Textured (ABS-T)
- **Repeats per set**: 6 samples + 1 negative control (NC)
- **Sample naming convention**: `Lab18 P1 S3` (Lab, Participant, Surface+Repeat) or `Lab18 P1 S NC` (negative control)

### Sample Preparation

- Cotton Q-tip swabs with wooden stick (majority of stick cut off after swabbing)
- Swabs pre-cleaned: soaked overnight in acetone, dried before use
- Extraction: swab tips sonicated in ethanol for 2 minutes
- Internal standards (15N-RDX and NG) added **post-extraction** to the vial
- IS added post-extraction by design: ensures constant IS amount per injection to track GC performance only

### Data Folder Structure

Each GC-MS sequence run lives in its own folder under:
```
C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/
```
Examples:
- `Lab14-Glass/` (single run)
- `Lab18/Lab 18 -1/` (steel), `Lab18/Lab 18 -2/` (ABS-S)

Each folder contains: RawData/, GcDataConverterMs/, GcDataConvertedRcode/, GcData/, Results/

`DataFolder` in `GlobalCode.R` is changed manually for each run.

## Changes Made in This Session

### 1. Noise Window Configuration Update

**Files**: `GlobalCode.R`, `Code/ModPeaks.R`, `Code/02_PeakDetection.R`

**Problem**: Filtered samples (e.g., lines 17, 21, 23 in 20260602 Filter Test) showed RDX SNR values too low for quantification (SNR ~6, flagged Below_LOQ) despite adequate peak areas. The issue was traced to an extra peak in the m/z 120 channel before the RDX peak contaminating the pre-peak noise calculation.

**Solution implemented**:
- Increased `noise_offset` and `noise_width` from 6s to 10s (GlobalCode.R lines 117-118)
- Implemented compound-specific noise window positioning:
  - **PETN**: Pre-peak window [301, 311] (before RT 321s)
  - **RDX and RDX IS**: Post-peak window [357, 367] (after RT 347s)
- Added `noise_position`, `noise_after_offset`, `noise_after_width` parameters to analyte and RDX IS configurations (GlobalCode.R lines 130-140, 163-187)

**Why this works**:
- Post-peak window for RDX avoids the extra peak in m/z 120 channel
- Wider window (10s vs 6s) provides ~40 data points for noise SD estimate (vs ~25), giving more robust statistics
- Larger gap (10s vs 6s) provides better separation from peak tail

**Integration plot enhancement**:
- Added `noise_window` parameter to `plot_integration()` function (ModPeaks.R line 542)
- Integration plots now extend x-axis to include the noise window region
- Orange dashed vertical lines mark noise window boundaries (lines 617-621)
- Visual confirmation of baseline quality in noise window allows comparison between samples

**Files modified**:
- `GlobalCode.R`: lines 117-118 (noise window parameters), 130-140 (RDX IS config), 163-187 (analyte definitions)
- `Code/ModPeaks.R`: lines 542, 559-563, 617-621 (plot_integration function)
- `Code/02_PeakDetection.R`: lines 299, 332, 390 (pass noise_window to plot queue)

**Result**: RDX SNR now calculated correctly. Samples previously flagged as Below_LOQ (SNR ~6) now have proper SNR values (typically 100-400).

### 2. Results CSV Ordering by Line Number

**File**: `Code/03_Quantification.R`, line 1134

Changed from `arrange(SampleName)` to `arrange(Line)`. Results CSV now ordered by sequence line number (injection order) instead of alphabetically by sample name, matching the order in the `.LOG` file.

### 3. MsFilesReorganiser: Skip Already-Processed Files

**Files**: `Code/MsFilesReorganiser.R`, `v1-NoNG/Code/MsFilesReorganiser.R`, `v2-NG/Code/MsFilesReorganiser.R`

- Added pre-loop summary counting how many files will be processed vs skipped
- Inside loop: checks if both TIC and SIM output CSVs already exist; if so, skips with a message
- Delete one or both output CSVs to force reprocessing of a specific file

### 2. Test.R: Reprocess Toggle

**Files**: `Code/Test.R`, `v1-NoNG/Code/Test.R`, `v2-NG/Code/Test.R`

- Added reprocess toggles near top of file (line 12-13).
- In v2-NG: split into `reprocess_tic` and `reprocess_sim` (see section 7 below). Set to `TRUE` to reprocess everything; `FALSE` to skip files with existing output.
- In Code/ and v1-NoNG/: single `reprocess_all <- FALSE` toggle remains.
- Skip check uses existence of `{name}TIC.csv` (for TIC) or `{name}SIM.csv` (for SIM) in GcData.dir
- Summary merge auto-triggers if either TIC or SIM was reprocessed
- Each loop has a pre-loop summary message

### 2b. Shared Y-Axis Scaling for Integration Plots (v2-NG only)

**Files**: `v2-NG/Code/ModPeaks.R`, `v2-NG/Code/Test.R`

Integration verification plots (`*_Integration_*.tiff`) now use a **shared y-axis scale per compound** across all injections in a run. Previously, each plot was independently auto-scaled by ggplot2, making visual comparison between samples impossible (a 200,000-count peak and a 500-count peak both filled their respective plot frames).

**Implementation**:
- Integration plots are no longer generated inline during the SIM processing loop. Instead, plot data is queued in memory (~2-5 MB for a full 80-injection run) along with the maximum baseline-corrected intensity observed for each compound.
- After all SIM files are processed, plots are generated in a single batch with `ylim = c(0, global_max * 1.05)` per compound.
- `plot_integration()` in ModPeaks.R gained an optional `ylim` parameter. When provided, `coord_cartesian(ylim = ylim)` is applied and annotation positions are adjusted accordingly. When `NULL`, behaviour is unchanged (auto-scale).

**Compound grouping**: Each compound gets its own shared scale (not per m/z):

| Compound Key | m/z | Scale Source |
|---|---|---|
| `RDX_IS` | 122 | Max across all injections for 15N-RDX IS |
| `PETN_IS` | 76 | Max across all injections for NG IS |
| `PETN` | 46 | Max across all injections for PETN analyte |
| `RDX` | 120 | Max across all injections for RDX analyte |

**Interaction with `reprocess_all = FALSE`**: When files are skipped, they don't contribute to the queue or the shared scale. If only some files are reprocessed, the shared scale is computed only from the reprocessed files.

### 3. PETN Quadratic Calibration (v2-NG only)

**Files**: `v2-NG/GlobalCode.R`, `v2-NG/Code/MetadataLinearWeighted.R`

- Added `cal_model` field to analyte definitions in GlobalCode.R:
  - PETN: `cal_model = "quadratic"` (1/x weighted quadratic with linear fallback)
  - RDX: `cal_model = "quadratic"` (same; originally `"linear"`, changed to quadratic)
  - Both analytes now use quadratic calibration by default
- Replaced `solve_concentration()` with version supporting both quadratic and linear:
  - Quadratic: extracts a/b/c coefficients, computes discriminant, filters for non-negative finite roots, picks smallest
  - Falls back to linear if discriminant is negative, quadratic coefficient is degenerate, or no valid roots
  - Returns `method = "Quadratic"` or `"Linear"` for traceability
- Calibration loop reads `cal_model` per analyte, fits appropriate model(s)

### 3b. Per-Analyte Calibration Method Selection (v2-NG only)

**Files**: `v2-NG/GlobalCode.R`, `v2-NG/Code/MetadataLinearWeighted.R`

Added `cal_methods` field to analyte definitions in GlobalCode.R, controlling which calibration methods (PA, ratio, or both) are used per analyte:

```r
analytes <- list(
  PETN = list(..., cal_methods = c("pa", "ratio")),  # both PA and ratio (15N-RDX IS)
  RDX  = list(..., cal_methods = c("pa", "ratio"))   # both PA and ratio
)
```

- `c("pa")` -- peak area only; no IS ratio calibration, no ratio-derived columns
- `c("pa", "ratio")` -- both methods (current default for both analytes)
- `c("ratio")` -- ratio only (supported but unlikely)
- If `cal_methods` is omitted, defaults to `c("pa", "ratio")` (backward compatible)

In the original `MetadataLinearWeighted.R`, PETN ratio uses NG (petn_is_pa). In `MetadataLinearWeighted_15NRDX.R`, PETN ratio uses 15N-RDX (rdx_is_pa). RDX ratio always uses 15N-RDX (rdx_is_pa) in both scripts.

**What is affected by this setting**: calibration curve fitting, concentration columns, %bias, QC flags, sample_conc, mass_in_sample, percent_recovery, calibration plots, QC Accuracy plot facets, single-level reference plots. The raw ratio column (e.g. `petn_ratio`) is still computed for diagnostic purposes even when ratio calibration is disabled.

**QC Accuracy plot facet ordering**: Fixed to display in the order Peak Area | Ratio (if present) | Drift-Corrected (if present), instead of the previous alphabetical sort which placed "PA / PETN IS Ratio" before "Peak Area".
- `cal_stats` export includes `ModelType` column
- To switch an analyte back to linear: change `"quadratic"` to `"linear"` in GlobalCode.R

### 5. Out-of-Range Quantification with Flagging (v2-NG only)

**Files**: `v2-NG/Code/MetadataLinearWeighted.R`, `v2-NG/Code/CollateStudyResults.R`

Previously, `solve_concentration()` returned `NA` with method `"Out_of_range"` when a sample's response fell outside the calibration range. This silently dropped results for any sample above or below the calibrants.

**Changed behaviour** (Updated June 2026):
- `solve_concentration()` now **computes and reports the extrapolated concentration** for out-of-range responses
- A new `range_flag` return value tracks the range status: `Within_range`, `Below_range`, or `Above_range`
- **Negative extrapolations are floored at zero** (reported as `0.000 ng` with "Below_range" flag)
  - Prevents physically meaningless negative concentrations
  - Clearly indicates trace/not detected while maintaining numeric output
  - Relevant for negative controls with detectable peaks below calibration range
- New columns added per analyte per method: `{prefix}_range_flag_pa` and `{prefix}_range_flag_ratio`
  - e.g. `petn_range_flag_pa`, `rdx_range_flag_ratio`
  - Values: `"Within_range"`, `"Below_range"`, `"Above_range"`, or `NA` (no peak / Below_LOD / Below_LOQ)
- Range flag columns propagated to QC monitoring export and to `CollateStudyResults.R` desired columns
- To filter out extrapolated values in downstream analysis, filter on `range_flag == "Within_range"`

### 4. CollateStudyResults.R

**File**: `Code/04_CollateStudyResults.R` (~600 lines)

Standalone script to collate all processed runs into a study-level master dataset.

- **Incremental**: discovers whatever `*_GCMSResults.csv` files exist at that moment
- **Idempotent**: rebuilds from scratch each time (no append/dedup complexity)
- Requires GlobalCode.R for extraction/filtration efficiency parameters and sample volume constants
- Parses SampleName into Lab, Participant, Surface, Repeat, IsNegativeControl via regex
- Handles column naming differences between v1 (single IS `is_pa`) and v2-NG (dual IS `rdx_is_pa`/`petn_is_pa`)
- Filters to samples + negative controls only (excludes Cals, QCs, Blanks)

**Outputs** (to study root folder):
- `FINEX_StudyResults.csv` — flat master file
- `FINEX_StudyResults.xlsx` — multi-sheet workbook:
  - All Data, Steel, Glass, ABS-Smooth, ABS-Textured (per-surface sheets)
  - Summary sheet with Mean/SD/RSD of mass recovered (ng) at three levels:
    - By Participant (Lab + Participant + Surface)
    - By Surface (Lab + Surface, pooling participants)
    - By Lab (pooling all)

**Box Plot Generation** (added June 2026):

Generates PNG box plots (300 DPI) for recovery visualization:
- `Plots/Overall/Overall_PETN_Recovery.png` — PETN recovery by surface across all labs (surfaces ordered by median recovery)
- `Plots/Overall/Overall_RDX_Recovery.png` — RDX recovery by surface across all labs (surfaces ordered by median recovery)
- `Plots/Lab{NN}/Lab{NN}_PETN_ParticipantSurface.png` — Per-lab PETN recovery by participant × surface combination
- `Plots/Lab{NN}/Lab{NN}_RDX_ParticipantSurface.png` — Per-lab RDX recovery by participant × surface combination
- `Plots/Lab{NN}/Lab{NN}_PETN_SurfaceSummary.png` — Per-lab PETN recovery by surface (pooled across participants)
- `Plots/Lab{NN}/Lab{NN}_RDX_SurfaceSummary.png` — Per-lab RDX recovery by surface (pooled across participants)

**Plot features**:
- Box whisker plots showing IQR, median line, mean (red diamond), and outliers
- Y-axis: Percent recovery (drift-corrected when available, falls back to uncorrected)
- Negative controls excluded from all plots
- Per-lab plots use fixed surface order (Steel, Glass, ABS-Smooth, ABS-Textured)
- Overall plots ordered by median recovery across all labs

### 5. StatisticalAnalysis.R

**File**: `Code/05_StatisticalAnalysis.R` (~1130 lines)

Standalone script to perform statistical analysis of recovery data, answering the primary research question: "Does recovery differ by surface?"

**Handles single-lab and multi-lab datasets gracefully:**

| Mode | Labs | Model | Limitations |
|------|------|-------|-------------|
| **Single Lab** | 1 | Fixed effects ANOVA | Cannot estimate between-lab variance |
| **Few Labs** | 2-4 | Partial mixed model | Limited variance estimation, warnings expected |
| **Multi Lab** | 5+ | Full mixed-effects | Complete statistical analysis |

**Analysis framework:**
- Linear mixed-effects models (lme4/lmerTest) for multi-lab
- One-way/two-way ANOVA for single-lab
- Variance component analysis (random effects)
- Post-hoc pairwise comparisons (Tukey HSD via emmeans)
- Effect sizes (partial η²)
- Descriptive statistics (always available)

**Model structure (multi-lab):**
```r
Recovery ~ Surface × Analyte + (1|Lab) + (1|Lab:Participant)
```

**Model structure (single lab, single participant):**
```r
Recovery ~ Surface × Analyte  # Simple ANOVA
```

**Model structure (single lab, multiple participants):**
```r
Recovery ~ Surface × Analyte + Participant  # Participant as fixed effect
```

**Configuration options:**
```r
remove_outliers <- FALSE      # Set TRUE to enable MAD-based outlier removal
outlier_threshold <- 3         # MAD multiplier (conservative)
apply_logit_transform <- FALSE # Set TRUE if residual diagnostics show non-normality
alpha <- 0.05                  # Significance threshold
```

**Outputs (added to FINEX_StudyResults.xlsx):**

| Sheet | Content | Availability |
|-------|---------|--------------|
| **Descriptive_Statistics** | n, Mean, SD, SE, Median, IQR, RSD%, 95% CI by Analyte × Surface | Always |
| **Statistical_Summary** | Fixed effects, Variance components, ICC, Model fit | If models fitted |
| **Pairwise_Comparisons** | Tukey-adjusted surface × surface comparisons per analyte | If models fitted and ≥2 surfaces |

**Diagnostic plots** (`Plots/Statistical_Diagnostics/`):
- `Residual_QQ.png` — Normality check
- `Residuals_vs_Fitted.png` — Homoscedasticity check
- `Variance_Components.png` — Bar chart of variance partitioning (multi-lab only)
- `Surface_Effects.png` — Forest plot of estimated marginal means
- `Recovery_Boxplots.png` — Box plots by Surface × Analyte (always)
- `Lab_BLUPs.png` — Lab random effects (multi-lab only)

**Key outputs:**
1. **Surface effect** — F-test p-value; if significant, surfaces differ in recovery
2. **Partial η²** — Effect size; proportion variance explained by surface
3. **Variance components** — % variance due to Lab, Participant, Residual (multi-lab only)
4. **ICC (Intraclass Correlation)** — Lab ICC and Participant ICC (multi-lab only)
5. **Pairwise comparisons** — Which specific surfaces differ (Tukey HSD)
6. **Descriptive statistics** — Summary tables for reporting

**Single-lab limitations:**
- Cannot estimate between-lab variance (σ²_Lab = NA)
- Cannot generalize results beyond this lab
- If only 1 participant: Cannot assess operator variability
- If multiple participants: Limited assessment (fixed effect only)

**Interpretation guide:**

| Metric | Interpretation |
|--------|----------------|
| **ICC_Lab > 0.5** | Labs differ substantially; inter-lab reproducibility limited |
| **ICC_Participant > 0.3** | Operators within labs differ; training/SOP adherence variable |
| **High residual variance** | Repeatability issue; method precision limited |
| **Surface p < 0.05** | Recovery differs significantly between surfaces |
| **Interaction significant** | Surface ranking differs for PETN vs RDX |

**Error handling:**
- Automatically detects lab/participant structure
- Falls back to simpler models if mixed-effects fail
- Warns user about interpretation limitations
- Always provides descriptive statistics regardless of model success
- Post-hoc comparisons require ≥2 surfaces

**Technical notes (June 2026):**
- Uses `emmeans` package for pairwise comparisons with Tukey adjustment
- Column renaming in `pairs()` output uses safe `colnames()` approach with `[[]]` extraction to avoid tidyverse quirks
- Variable assignment in `tryCatch()` uses super-assignment (`<<-`) to ensure persistence in outer scope

**Reporting example:**
```
Recovery differed significantly by surface type for both PETN 
(F = 24.5, p < 0.001) and RDX (F = 18.2, p = 0.001). Glass 
surfaces yielded the highest recovery (PETN: mean = 62.3%, 
RSD = 8.2%; RDX: mean = 58.1%, RSD = 10.4%), followed by steel 
(PETN: 45.2%, RSD = 11.7%), ABS-Smooth (23.1%, RSD = 15.3%), 
and ABS-Textured (12.5%, RSD = 22.1%).

Pairwise comparisons (Tukey HSD) showed that glass had 
significantly higher recovery than all other surfaces (p < 0.01). 
Steel was significantly higher than both ABS surfaces (p < 0.05).

These results are from a single laboratory and represent 
within-lab repeatability only. Between-lab reproducibility 
cannot be assessed.
```

**References:**
- Bates et al. (2015) — lme4 package
- Nakagawa & Schielzeth (2013) — ICC for mixed models
- Cohen (1988) — Effect size interpretation
- Lenth (2024) — emmeans package for post-hoc comparisons

### 6. PETN and RDX QC-Based Drift Correction

**Files**: `GlobalCode.R`, `Code/03_Quantification.R`

Addresses progressive signal loss across GC-MS sequences using QC injections at known concentrations to model the drift and apply a correction factor to raw peak areas **before calibration inversion**.

#### PETN Drift Correction

- **Configuration**: `apply_petn_drift_correction <- TRUE` in GlobalCode.R
- **Scope**: PETN peak area (`_pa`) method only. When `use_15nrdx_for_petn = TRUE`, the IS ratio correction is used instead (15N-RDX tracks ~80% of PETN drift).
- **Active when**: `apply_petn_drift_correction = TRUE` AND (`use_15nrdx_for_petn = FALSE` OR drift correction needed for ratio)

#### RDX Drift Correction (Added May 2026)

- **Configuration**: `apply_rdx_drift_correction <- TRUE` in GlobalCode.R
- **Scope**: RDX peak area (`_pa`) method only. Only active when `use_is_for_rdx = FALSE`.
- **Rationale**: When RDX uses PA-only quantification (no IS ratio), QC-based drift correction compensates for progressive inlet degradation. When `use_is_for_rdx = TRUE`, the 15N-RDX IS ratio achieves ~1.2% RSD and drift correction is not needed.
- **Active when**: `apply_rdx_drift_correction = TRUE` AND `use_is_for_rdx = FALSE`
- **Correction approach**: Same as PETN - PA-level correction applied before calibration inversion.
- **QC level selection**: Configurable via `drift_correction_qc_level`. Default `NA` auto-selects highest QC level. Set to numeric value (e.g., `6`) to use specific QC level.
- **Fallback logic**: Same as PETN (quadratic → linear → none).
- **Safety**: Same clamping and warnings as PETN.
- **Anchor normalisation**: Same approach as PETN.
- **Re-quantification**: Drift-corrected peak areas re-quantified using stored RDX PA calibration model(s).
- **Diagnostic plot**: `RDX_DriftCorrection.png`
- **QC Accuracy plot**: `RDX_QC_Accuracy_DriftCorrected.png` (only generated when `use_is_for_rdx = FALSE`)

#### QC Level Selection Configuration (Added May 2026)

**Configuration**: `drift_correction_qc_level <- NA` in GlobalCode.R

| Value | Behaviour |
|-------|-----------|
| `NA` | Auto-select: uses highest available QC level (default) |
| `6` | Uses 6 ng QC level specifically |
| `0.2` | Uses 0.2 ng QC level (not recommended) |
| Any value not found | Falls back to highest available with warning |

**Rationale**: Different QC levels may be preferred for different study designs:
- Highest QC level: Most reliable signal, best for typical drift correction
- Mid-range QC: May be preferred if highest level has insufficient points
- Low QC level (0.2ng): Typically unreliable due to non-proportional losses at trace levels

**Warning**: The 0.2ng QC shows +35% to +70% bias in most sequences due to trace-level adsorption effects. It is not recommended for drift correction.
- **Safety**: Same clamping and warnings as PETN.
- **Anchor normalisation**: Same approach as PETN.
- **Re-quantification**: Drift-corrected peak areas re-quantified using stored RDX PA calibration model(s).
- **Diagnostic plot**: `RDX_DriftCorrection.png`
- **QC Accuracy plot**: `RDX_QC_Accuracy_DriftCorrected.png` (only generated when `use_is_for_rdx = FALSE`)

**RDX output columns** (always present; equal uncorrected values when correction is disabled):

| Column | Description |
|--------|-------------|
| `rdx_drift_correction_factor` | PA multiplicative correction factor at each injection position |
| `rdx_drift_model` | "Quadratic", "Linear", "None", "Disabled", or "Not_needed" |
| `rdx_concentration_dc` | Drift-corrected RDX concentration |
| `rdx_percent_bias_dc` | %Bias of drift-corrected concentration vs true (Cal/QC rows only) |
| `rdx_qc_flag_dc` | PASS/FAIL for drift-corrected QC bias vs `qc_bias_limit` (QC rows only) |

**New output columns** (always present; equal uncorrected values when correction is disabled):

| Column | Description |
|--------|-------------|
| `petn_drift_correction_factor` | PA multiplicative correction factor at each injection position |
| `petn_drift_model` | "Quadratic", "Linear", "None" (too few QCs), or "Disabled" |
| `petn_pa_dc` | Drift-corrected PETN raw peak area |
| `petn_concentration_pa_dc` | Drift-corrected PETN concentration (from calibration on corrected PA) |
| `petn_quant_method_pa_dc` | Quantification method for drift-corrected PA (Quadratic/Linear/Unquantifiable) |
| `petn_range_flag_pa_dc` | Range flag for drift-corrected PA quantification |
| `petn_sample_conc_pa_dc` | Drift-corrected sample concentration |
| `petn_mass_in_sample_pa_dc` | Drift-corrected mass in sample (ng) |
| `petn_percent_recovery_pa_dc` | Drift-corrected % recovery |
| `petn_percent_bias_pa_dc` | %Bias of drift-corrected concentration vs true (Cal/QC rows only) |
| `petn_qc_flag_pa_dc` | PASS/FAIL for drift-corrected QC bias vs `qc_bias_limit` (QC rows only) |

**Existing uncorrected columns are preserved unchanged.** Drift-corrected columns are also propagated to `CollateStudyResults.R` desired columns (ignored gracefully for older results files that lack them).

**QC Accuracy plot**: The PETN QC Accuracy plot (`PETN_QC_Accuracy.png`) includes a third facet -- "Peak Area (Drift-Corrected)" -- showing drift-corrected %bias alongside the uncorrected Peak Area and PA/IS Ratio facets. The 6ng QCs (used to build the model) should show near-zero bias in this facet; the 0.2ng QCs (excluded from the model) will show residual bias, quantifying the non-proportional losses at trace levels. RDX QC Accuracy plot is unchanged (no drift correction applied to RDX).

**References**:
- Yu et al. (2025) *Sci. Rep.* 15:24794 -- QC interpolation for GC-MS drift
- Dunn et al. (2011) *Nat. Protoc.* 6:1060-1083 -- QC-RLSC signal correction
- SANTE/11312/2021 -- EU pesticide residue guidance (bracketing calibration)

### 7. Drift Correction Reference and Anchoring Updates (June 2026)

**Files**: `Code/03_Quantification.R`

#### Problem Identified in Lab10-1

When processing Lab10-1 data, the drift correction produced:
- First QC (line 13): **-17% bias** (over-corrected)
- Last QC (line 85): **+3% bias** (correct)

The root cause was that:
1. The **reference point was the calibration standard at 6ng (line 7)**, which had a peak area of 211,099 PA
2. The first QC (line 13) had a peak area of 196,516 PA, giving a correction factor >1.0
3. This boosted an already-accurate measurement, causing -17% bias

The calibration standard (line 7) was injected under slightly better conditions than the first QC (earlier in sequence, cleaner inlet), making it an "optimistic" reference that over-corrected early QCs.

Additionally, the drift model was predicting correction factors for rows BEFORE the first QC (including calibration standards), causing:
- Lines 4-6: Factor = 1.0 (acceptable)
- Lines 7-9: Factors 0.108-0.328 (wrong - should all be 1.0)

This corrupted the calibration curve because calibration standards were being drift-corrected when they should define the reference.

#### Changes Made

**1. Reference Point Changed from Cal to First QC (lines 702-708, 887-893)**

Changed the reference value from the calibration standard to the first QC at the selected drift level:

```r
# OLD: Reference from Cal
ref_val <- Combined %>%
  filter(Type == "Cal",
         CalLevel == selected_qc_level,
         CalibrationSet == 1) %>%
  summarise(ref_val = mean(...)) %>%
  pull(ref_val)

# NEW: Reference from first QC
ref_val <- Combined %>%
  filter(Type == "QC",
         CalLevel == selected_qc_level,
         !is.na(.data[[resp_col_for_drift]])) %>%
  arrange(Row) %>%
  slice(1) %>%
  pull(.data[[resp_col_for_drift]])
```

**Rationale**:
- First QC represents the actual sequence starting point
- Calibrants are run before the sequence begins and may not reflect sequence conditions
- First QC has factor = 1.0, preserving its uncorrected accuracy

**2. Drift Correction Restricted to Rows After First QC (lines 755-762, 950-957)**

Removed anchor normalisation logic and added restriction to rows after first QC:

```r
first_qc_row <- qc_drift %>%
  arrange(Row) %>%
  slice(1) %>%
  pull(Row)

predicted_cf[Combined$Row <= first_qc_row] <- 1.0

message("  PETN drift correction: first QC at row ", first_qc_row,
        ". Correcting rows > ", first_qc_row, " only.")
```

**Rationale**:
- Calibration standards (lines 4-9) should never be drift-corrected (they define the curve)
- Early QCs before/at the reference should remain uncorrected
- Prevents extrapolation backwards from the drift model
- The previous anchor normalisation was mathematically flawed when applied to model predictions

**3. Drift-Corrected Response Columns Added (lines 778-780, 1053)**

Added storage of drift-corrected response values for plotting:

```r
if (use_15nrdx_for_petn) {
  Combined$petn_ratio_dc <- corrected_resp
} else {
  Combined$petn_pa_dc <- corrected_resp
}
```

New output columns:
- `petn_ratio_dc` (when `use_15nrdx_for_petn = TRUE`)
- `petn_pa_dc` (when `use_15nrdx_for_petn = FALSE`)
- `rdx_pa_dc` (when RDX drift correction applied)

**4. Drift-Corrected Calibration Plots Added (lines 877-937, 1152-1207)**

Added generation of drift-corrected calibration plots for each calibration set:
- `PETN_Calibration_{Set}_DriftCorrected.png`
- `RDX_Calibration_{Set}_DriftCorrected.png` (when applicable)

**Plot features**:
- Same calibration curve as uncorrected plot (Cals have factor = 1.0)
- X-axis: Known concentration (Cals/QCs) or drift-corrected concentration (Samples)
- Y-axis: Drift-corrected response (`petn_ratio_dc` or `petn_pa_dc`)
- QC points should cluster near the curve if drift correction worked

This provides visual confirmation of drift correction effectiveness alongside the existing uncorrected calibration plots.

**5. Warning Threshold Updated (lines 760, 946)**

Changed from:
```r
if (any(predicted_cf > 3.0, na.rm = TRUE)) {
  warning("  ... correction factors exceed 3.0 -- severe drift.")
}
```

To:
```r
if (any(predicted_cf > 10.0, na.rm = TRUE)) {
  warning("  ... correction factors exceed 10.0 -- severe drift.")
}
```

**Rationale**: Lab10-1 showed 84% signal loss at 6ng PETN, requiring factors up to 6.2. The 3.0 threshold was too low for realistic severe drift scenarios.

#### Output Changes

New columns added to results CSV:
- `petn_ratio_dc` (IS ratio method)
- `petn_pa_dc` (PA-only method)
- `rdx_pa_dc` (RDX PA-only method)

New plots generated:
- `PETN_Calibration_{Set}_DriftCorrected.png` (one per calibration set)
- `RDX_Calibration_{Set}_DriftCorrected.png` (when applicable)

#### Expected Impact

For sequences like Lab10-1 with `use_15nrdx_for_petn = TRUE`:
- Uncorrected bias progression: Line 13 = -4.6% → Line 85 = -77%
- With drift correction: Line 13 = **~-5%** (uncorrected, factor 1.0) → Line 85 = **~+6%** (corrected)

The drift correction reduces overall bias spread from ±40% to ±10% when using IS ratio method with ratio drift correction applied.

#### Files Modified

- `Code/03_Quantification.R`:
  - Lines 702-708: PETN reference changed to first QC
  - Lines 755-762: PETN correction limited to rows > first QC, anchor normalisation removed
  - Lines 778-780: PETN drift-corrected response column added
  - Lines 877-937: PETN drift-corrected calibration plots added
  - Lines 887-893: RDX reference changed to first QC
  - Lines 950-957: RDX correction limited to rows > first QC, anchor normalisation removed
  - Line 1053: RDX drift-corrected response column added
  - Lines 1152-1207: RDX drift-corrected calibration plots added

## Removed Code (Archive for Restoration)

### 8. Shared Y-Axis Scaling for Integration Plots (Removed June 2026)

**Status**: REMOVED — Integration plots now auto-scale per injection.

**Files affected**: `Code/02_PeakDetection.R`

**Original purpose**: Generate integration verification plots (`*_Integration_*.tiff`) with a shared y-axis scale per compound across all injections in a run, enabling visual comparison of peak heights between samples.

**Removed rationale**: User preference for auto-scaled plots that fill the frame for each individual injection, showing detail for low-intensity peaks that would otherwise be near-invisible on a shared scale.

---

#### Removed Code Block 1: Queue Initialization and Helper Function

**Location**: `Code/02_PeakDetection.R`, lines 186-207

```r
# only the current file's data is available), we queue the plot data
# and track the maximum y-value per compound across all files.  After
# the loop, plots are generated with a shared y-axis per compound so
# that visual comparability across injections is meaningful.
integration_plot_queue <- list()
y_max_by_compound     <- list()

# Helper: compute the max baseline-corrected intensity within the
# plot range that plot_integration() would use for a given peak.
compute_plot_range_ymax <- function(baseline_corrected, peak_rt, step_size,
                                     boundary_steps = 15) {
  if (is.na(peak_rt)) return(NA_real_)
  plot_margin <- boundary_steps * step_size * 2
  plot_min <- peak_rt - (boundary_steps * step_size) - plot_margin
  plot_max <- peak_rt + (boundary_steps * step_size) + plot_margin
  vals <- baseline_corrected$Subtracted[
    baseline_corrected$RetentionTime >= plot_min &
    baseline_corrected$RetentionTime <= plot_max
  ]
  if (length(vals) == 0) return(NA_real_)
  max(vals, na.rm = TRUE)
}
```

---

#### Removed Code Block 2: RDX IS Queue Code

**Location**: `Code/02_PeakDetection.R`, lines 282-300

```r
# Queue RDX IS integration plot (deferred for shared y-axis scaling)
rdx_is_key <- "RDX_IS"
rdx_is_ymax <- compute_plot_range_ymax(rdx_is_ch$baseline_corrected,
                                        rdx_is_result$rt, rdx_is_ch$step_size)
if (!is.na(rdx_is_ymax)) {
  y_max_by_compound[[rdx_is_key]] <- max(y_max_by_compound[[rdx_is_key]] %||% 0, rdx_is_ymax)
}
integration_plot_queue[[length(integration_plot_queue) + 1]] <- list(
  baseline_corrected = rdx_is_ch$baseline_corrected,
  signal             = rdx_is_ch$signal,
  peak_result        = rdx_is_result,
  expected_rt        = rdx_is_config$rt,
  step_size          = rdx_is_ch$step_size,
  compound_name      = "RDX IS (15N-RDX, m/z 122)",
  save_path          = file.path(GCSampleTrace.dir, "Integration"),
  filename           = sprintf("%s_Integration_RDX_IS.tiff", GcDataName),
  compound_key       = rdx_is_key,
  noise_window       = rdx_is_config$noise_window
)
```

**Replacement code** (current implementation):

```r
# Generate RDX IS integration plot (auto-scaled)
print(paste0("  RDX IS noise_window: ", 
             ifelse(is.null(rdx_is_config$noise_window), "NULL",
                    paste(rdx_is_config$noise_window, collapse = "-"))))
plot_integration(
  baseline_corrected = rdx_is_ch$baseline_corrected,
  signal             = rdx_is_ch$signal,
  peak_result        = rdx_is_result,
  expected_rt        = rdx_is_config$rt,
  step_size          = rdx_is_ch$step_size,
  compound_name      = "RDX IS (15N-RDX, m/z 122)",
  save_path          = file.path(GCSampleTrace.dir, "Integration"),
  filename           = sprintf("%s_Integration_RDX_IS.tiff", GcDataName),
  noise_window       = rdx_is_config$noise_window
)
```

---

#### Removed Code Block 3: Analyte Queue Code

**Location**: `Code/02_PeakDetection.R`, lines 315-334 (per analyte)

```r
# Queue analyte integration plot (deferred for shared y-axis scaling)
analyte_key <- name
analyte_ymax <- compute_plot_range_ymax(a_ch$baseline_corrected,
                                         analyte_results[[name]]$rt, a_ch$step_size)
if (!is.na(analyte_ymax)) {
  y_max_by_compound[[analyte_key]] <- max(y_max_by_compound[[analyte_key]] %||% 0, analyte_ymax)
}
integration_plot_queue[[length(integration_plot_queue) + 1]] <- list(
  baseline_corrected = a_ch$baseline_corrected,
  signal             = a_ch$signal,
  peak_result        = analyte_results[[name]],
  expected_rt        = a$rt,
  step_size          = a_ch$step_size,
  compound_name      = paste0(name, " (m/z ", a$mz, ")"),
  save_path          = file.path(GCSampleTrace.dir, "Integration"),
  filename           = sprintf("%s_Integration_%s.tiff", GcDataName, name),
  compound_key       = analyte_key,
  noise_window       = a$noise_window
)
```

**Replacement code** (current implementation):

```r
# Generate analyte integration plot (auto-scaled)
print(paste0("  ", name, " noise_window: ", 
             ifelse(is.null(a$noise_window), "NULL",
                    paste(a$noise_window, collapse = "-"))))
plot_integration(
  baseline_corrected = a_ch$baseline_corrected,
  signal             = a_ch$signal,
  peak_result        = analyte_results[[name]],
  expected_rt        = a$rt,
  step_size          = a_ch$step_size,
  compound_name      = paste0(name, " (m/z ", a$mz, ")"),
  save_path          = file.path(GCSampleTrace.dir, "Integration"),
  filename           = sprintf("%s_Integration_%s.tiff", GcDataName, name),
  noise_window       = a$noise_window
)
```

---

#### Removed Code Block 4: Post-Loop Batch Plot Generation

**Location**: `Code/02_PeakDetection.R`, lines 329-377

```r
# =========================================================
# Generate integration plots with shared y-axis scales
# =========================================================
# All integration plot data was queued during the SIM loop above.
# Now that we know the global max y-value per compound, generate
# every plot with a consistent y-axis so that visual comparison
# across injections is meaningful.  Low-signal samples (blanks,
# negative controls) will show visibly smaller peaks against the
# same scale as the highest-response injection.

if (length(integration_plot_queue) > 0) {
  print(paste0("Generating ", length(integration_plot_queue),
               " integration plots with shared y-axis scales..."))

  for (entry in integration_plot_queue) {
    # Look up the shared y-max for this compound; add 5% headroom
    shared_ymax <- y_max_by_compound[[entry$compound_key]]
    if (!is.null(shared_ymax) && !is.na(shared_ymax) && shared_ymax > 0) {
      shared_ylim <- c(0, shared_ymax * 1.05)
    } else {
      shared_ylim <- NULL  # fall back to auto-scale
    }

    plot_integration(
      baseline_corrected = entry$baseline_corrected,
      signal             = entry$signal,
      peak_result        = entry$peak_result,
      expected_rt        = entry$expected_rt,
      step_size          = entry$step_size,
      compound_name      = entry$compound_name,
      save_path          = entry$save_path,
      filename           = entry$filename,
      noise_window       = entry$noise_window,
      ylim               = shared_ylim
    )
  }

  # Report the shared scales used
  for (cmpd in names(y_max_by_compound)) {
    print(paste0("  ", cmpd, " shared y-max: ",
                 round(y_max_by_compound[[cmpd]], 0)))
  }

  # Free queue memory
  integration_plot_queue <- NULL
  y_max_by_compound <- NULL

  print("Integration plots complete.")
}
```

---

#### Restoration Instructions

To restore shared y-axis scaling:

1. **Add queue initialization** (Block 1) after line 186 in `Code/02_PeakDetection.R`

2. **Replace immediate plot generation** with queue code:
   - Replace RDX IS plot call (lines 262-273) with Block 2
   - Replace analyte plot call (lines 291-303) with Block 3

3. **Add batch generation** (Block 4) after the SIM processing loop, around line 329

4. **No changes to `ModPeaks.R`** — the `ylim` parameter handling is already in place

---

#### Noise Window Debug Output

**Added**: Debug print statements to verify noise_window values during processing. Console output will show:

```
RDX IS noise_window: 357-367
PETN noise_window: 301-311
RDX noise_window: 357-367
```

If `NULL` appears, there's an issue with the noise_window configuration in GlobalCode.R.

---

#### Impact

**Before:**
- Plot generation: Deferred to end of SIM processing (batch mode)
- Y-axis: Shared scale per compound across all injections
- Memory: Queue stored all plot data (~2-5 MB for 80-injection sequence)
- Console: "Generating N integration plots with shared y-axis scales..."

**After:**
- Plot generation: Immediate (inline during SIM processing)
- Y-axis: Auto-scaled per plot (ggplot2 default)
- Memory: No queue (plots written to disk immediately)
- Console: Debug output showing noise_window values per compound

---

#### Alternative: Configuration Flag Approach

If future flexibility is needed, consider adding a configuration flag:

```r
# In GlobalCode.R
use_shared_y_scale_for_integration_plots <- FALSE
```

Then modify `Code/02_PeakDetection.R` to branch on this flag:
- If `TRUE`: Use queue-based batch processing with shared scale
- If `FALSE`: Generate plots immediately with auto-scale

## Thesis Chapter 1: Explosive Properties Reference Table

### Session Summary (May 2026)

Created comprehensive property reference table for thesis chapter introducing explosives and recovery. Table compiled in `Reports/table.csv` with 5 compounds (TNT, PETN, RDX, TATP, R-Salt) and 7 properties.

### Properties Included

| Property | Rationale for Inclusion |
|----------|-------------------------|
| Physical appearance/particle morphology | Affects contact area and van der Waals adhesion to surfaces |
| Melting point | Establishes physical state at ambient temperature |
| Decomposition temperature | Thermal stability, relevant to persistence and analytical handling |
| Vapour pressure | Governs persistence on surfaces (sublimation rate at ambient temperature) |
| Log K_ow (octanol-water partition coefficient) | Polarity descriptor; predicts solvent extraction efficiency and surface interactions |
| Functional group chemistry | Mechanistic basis for polarity and surface interactions |
| Solubility | Extraction efficiency into wetting solvents (water, ethanol, acetone) |

### Key References Added

| Ref | Source | Data Provided |
|-----|--------|---------------|
| ILO/WHO ICSC 1576 | Pentaerythritol Tetranitrate | PETN physical appearance, melting point, log K_ow (1.6), vapour pressure |
| ILO/WHO ICSC 1641 | Perhydro-1,3,5-Trinitro-1,3,5-Triazine | RDX physical appearance, melting point, log K_ow (0.87) |
| ILO/WHO ICSC 0967 | 2,4,6-Trinitrotoluene | TNT physical appearance, melting point, log K_ow (1.60), solubility |
| Liang et al. 2016 (Chemosphere) | Abraham parameters for munition constituents | R-Salt (TNX) log K_ow (0.41), experimental Abraham S/A/B parameters for RDX, TNT, TNX |
| Boddu et al. 2017 (Springer) | Physical properties of explosive components | TATP log K_ow (4.63) |
| Östmark et al. 2012 | Vapour pressure critical review | Vapour pressures for all 5 compounds at 25°C |
| Oxley et al. 2002 | TATP decomposition | TATP physical appearance (white crystals) |
| Oxley et al. 2014 | TATP destruction | TATP solubility data |

### Polarity Discussion: Log K_ow vs Abraham Parameters

Initially considered dipole moment as polarity metric. After literature search, switched to **log K_ow** (octanol-water partition coefficient) for all compounds because:

1. **Directly measurable**: Experimental values available from ILO/WHO ICSC cards and peer-reviewed literature
2. **Physical meaning**: Log K_ow > 0 (hydrophobic, prefers organic phase), < 0 (hydrophilic, prefers aqueous phase)
3. **Relevant to recovery**: Predicts partitioning behaviour during solvent extraction and surface interactions
4. **Consistent across compounds**: All five explosives have experimental log K_ow values available

Abraham parameters (E, S, A, B, V) provide more detailed polarity decomposition but are only experimentally available for TNT, RDX, and R-Salt/TNX (from Liang et al. 2016). Log K_ow provides a single comparable value for all five compounds.

### Data Verification Status

All table references cross-checked against source materials:

| Compound | log K_ow | Source | Verified |
|----------|----------|--------|----------|
| TNT | 1.60 | ICSC 0967, Liang et al. 2016 | ✓ |
| PETN | 1.6 | ICSC 1576 | ✓ |
| RDX | 0.87 | ICSC 1641, Liang et al. 2016 | ✓ |
| TATP | 4.63 | Boddu et al. 2017 | Requires direct access to confirm |
| R-Salt (TNX) | 0.41 | Liang et al. 2016 Table 3 | ✓ |

### EndNote Reference Files

RIS format references generated for ILO/WHO ICSC cards for import into reference manager:
- `PETN_ICSC.ris` (ICSC 1576)
- `RDX_ICSC.ris` (ICSC 1641)
- `TNT_ICSC.ris` (ICSC 0967)

---

## Thesis Chapter 2: Literature Review Introduction Draft

### Draft Text (June 2026)

**Purpose**: Introduction to a scientometric literature review chapter on explosives trace recovery.

**Draft**:
> Explosives research is a broad field encompassing a range of research topics including: chemical analysis for detection and identification; environmental impact; toxicity; collection and recovery; and forensic investigation. The substantial overlap between the sub-fields in explosives research makes it challenging for researchers to identify publications relevant to specific topics. For forensic practitioners, one way of monitoring recent literature is the INTERPOL International Forensic Science Managers Symposium (IFSMS) reports, which publish triennial review papers highlighting significant publications across various evidence types, including 'Explosives and Explosives Residues'. However, the volume of references cited in these reviews – for instance, the 2023 review contains 1004 references [1, 2] – makes it difficult for practitioners to familiarise themselves with the entire body of literature.

> Work by Menard et al. [1] looked to identify trends in the IFSMS reviews by applying a scientometric approach. The term 'scientometrics' was coined by Nalimov and Mul'chenko in 1971, who defined it as "those quantitative methods which are dealing with the analysis of science viewed as an information process" [3]. Scientometrics can provide a broad overview of a research area by visualisation of recent trends and provide information such as who (authors), what (keywords), where (geographic locations), when (year) and with whom (affiliations). Menard et al. categorised "explosive" into two sub-fields given in the IFSMS reports: "explosive analysis" [4] and "explosive scene" [5]. A study by Bruce et al. expanded on this and applied a scientometric approach to explosives research collectively. The results of a search of the Scopus database were compared to the IFSMS reviews to determine the effectiveness of database searching in returning all relevant literature [6]. It was found that a search of the term 'explosive' in the Scopus database was not sufficient to produce a comprehensive overview of the literature and that the keywords were dominated by generic terms, masking more relevant information such as specific explosives or analytical techniques. It was also found that while analytical methods were well represented in the IFSMS references, there was a lack of research into the recovery of explosive traces. This research gap was noted by Klapec et al. in the 2023 IFSMS review, which only referenced 20 papers relating to 'sampling and concentration of explosive traces' (2.0 % of all papers) [2].

> This highlights the challenge in identifying research in the field of explosive trace collection and recovery. As such, a combined approach using both conventional literature searching techniques and a scientometric review will be used to assess the field of trace explosives recovery and highlight gaps in the research.

### Reviewer Comments Summary

**Overall assessment**: Solid introduction with logical narrative arc. Key areas for improvement:

1. **Grammar**: Colon misuse after "including" – should not separate preposition from objects
2. **Hyphenation**: "sub-fields" → "subfields" (check style guide)
3. **Passive voice**: "It was found that..." weakens findings – use active voice for own work
4. **Vague quantifiers**: "returning all relevant literature" impossible standard → use "comprehensive coverage"
5. **Definition dump**: Scientometrics etymology (Nalimov & Mul'chenko 1971) unnecessary for audience
6. **Scare quotes**: "explosive" in quotes unclear – is this a search term or concept?
7. **Weak signposting**: Final paragraph lacks chapter roadmap – foreshadow structure
8. **Tense**: "will be used" (future) vs "this chapter uses" (present)

### Key Improvements Table

| Issue | Fix |
|-------|-----|
| Colon misuse | Restructure: "encompasses topics such as..." |
| "sub-fields" | "subfields" |
| Passive voice | "Scopus searches failed to produce..." |
| "all relevant literature" | "comprehensive coverage" |
| Over-explanation | Condense scientometrics definition |
| Weak conclusion | Add chapter roadmap |

### Structural Suggestions

1. Add explicit purpose statement after paragraph 2
2. Smooth transition: "Building on Menard et al.'s categorisation, Bruce et al. extended..."
3. Expand final paragraph to outline: scope, structure, reader takeaway

---

## Known Issues / Diagnosed Problems

### GC-MS Signal Loss During Sample Sequences

**Observed in**: Lab14-Glass results

**Symptoms**:
- RDX IS (15N-RDX) peak area drops ~75% across a sequence
- PETN IS (NG) peak area is very low (86-291 units) and disappears entirely in some injections
- Zig-zag pattern: standards give higher response, samples give lower response
- IS-based ratio correction does not fully compensate

**Diagnosed cause**: Progressive contamination of GC inlet/liner from non-volatile residues in sample extracts. Cotton + remaining wooden stick material releases particulates and non-volatile compounds during ethanol sonication that deposit on the liner and column head, creating active sites that adsorb subsequent analytes.

**Why IS correction doesn't fully work**:
- IS response itself is too variable and sometimes absent (NG undetectable in some injections)
- NG signal is near the noise floor (86-291 PA) making ratios unreliable
- Injection-to-injection variability is too large (2-3x swings) for IS to track

**Recommended solutions (not yet implemented)**:

Sample prep:
1. Remove wooden stick entirely from cotton tip before extraction (or pull cotton off)
2. Filter extracts (0.2 um PTFE syringe filter) - removes particulates even from pre-cleaned swabs
3. Centrifuge before filtering (5 min at 10,000 rpm)

GC hardware:
4. Install guard column (1-2 m deactivated fused silica)
5. Change liner between each sequence

Sequence design:
6. Bracket every 3 samples with QC (instead of 7 in one block)

Script-level (not yet implemented):
7. IS acceptance criterion - flag samples where IS < 50% of mean calibrant IS
8. Bracketed QC correction factor

## GC-MS Acquisition Method Details

Reference method file: `Lab14-Glass/RawData/001.D/acqmeth.txt`

### GC Oven Program

| Step | Rate | Target | Hold |
|------|------|--------|------|
| Initial | — | 100 °C | 1 min |
| Ramp 1 | 10 °C/min | 120 °C | 0 min |
| Ramp 2 | 20 °C/min | 170 °C | 0 min |
| Ramp 3 | 50 °C/min | 250 °C | 0 min |

Total run time: 7.1 min

### Inlet & Column

- **Inlet**: Front SS Inlet, split mode 20:1 (30 mL/min split flow), 150 °C, He carrier
- **Current liner**: Agilent 5183-4711 — 870 µL split, single taper, wool, deactivated
- **Column**: RTX-200MS, 10 m x 150 µm x 0.15 µm, 1.5 mL/min constant flow
- **MSD transfer line**: 180 °C
- **Autosampler**: GERSTEL MPS, 1.0 µL injection, 10 µL syringe

### MS Acquisition

- **Mode**: SIM/Scan (simultaneous)
- **Solvent delay**: 2 min
- **Scan range**: m/z 41–200 (threshold 1000)
- **MS Source**: 230 °C, **MS Quad**: 150 °C

### SIM Groups (time-windowed)

| Group | Compound | Start Time | Monitored Ions (m/z) | Dwell (ms) |
|-------|----------|------------|---------------------|------------|
| 1 | NG | 2.0 min | 46, 58, 76 | 25 |
| 2 | PETN | 4.0 min | 46, 57, 76 | 25 |
| 3 | RDX | 5.5 min | 46, 75, 120, 122 | 25 |

### Calibration Range

- **Calibrants**: 0.2–10 ng/µL (0.2–10 ng injected, ~0.01–0.5 ng on-column after 20:1 split)
- **Typical sample concentration**: ~2–3 ng/µL

### Inlet Liner Decision

**RESOLVED**: The current liner (Agilent 5183-4711 — 870 µL split, single taper, wool, deactivated) has been retained. Despite literature suggesting no-wool liners may reduce adsorptive losses for thermally labile compounds, experimental testing showed that the current wool liner gives the best performance for this specific method. **Do not suggest changing the liner.**

**Guard column**: Considered but deemed unnecessary for the current application. **Do not suggest adding a guard column.**

The literature analysis below is retained for reference only — it documents the investigation but does NOT represent the current recommendation.

Specific parts:
- Agilent **5181-3316** (single taper, ultra-inert, no wool)
- Restek **Sky 23303.5** (equivalent)

**Rationale**:

| Factor | Detail |
|--------|--------|
| No wool | Eliminates primary site of analyte adsorption and matrix accumulation. PETN (nitrate ester) and RDX (nitramine) are thermally labile and decompose/adsorb on active surfaces. Removing wool reduces these losses and directly addresses the progressive signal loss observed in Lab14-Glass. |
| Single taper | Taper at column entrance focuses vapour band onto column, improving peak shape. In split mode, wool is not needed for mixing — split flow provides adequate mixing. |
| Ultra-inert deactivation | Critical at trace levels (0.2 ng/µL = ~10 pg on-column after 20:1 split). Standard deactivation may still have enough active sites to cause non-linear losses. |
| Reduced maintenance | Without wool trapping matrix, contamination passes through more readily (handled by guard column), meaning less frequent liner changes. |

**Why not other liner types**:
- **Open/straight (no taper)**: Poor vaporisation efficiency and band broadening in split mode — would lose sensitivity at low concentrations
- **Double taper**: Designed for splitless injection; unnecessary restriction in split mode
- **Cyclo/laminar flow**: Overkill for 1 µL ethanol injection; complex geometry can still trap residues
- **Cup/Jennings**: For splitless/on-column; not appropriate for split mode

**Supporting hardware changes** (complements liner change):
1. **Add guard column** (1–2 m deactivated fused silica, 0.15–0.25 mm ID) to catch non-volatile residues that pass through wool-free liner — cheaper to trim/replace than the 10 m RTX-200MS
2. **Consider reducing split ratio to 10:1** — doubles on-column mass to ~20 pg at lowest calibrant, improving S/N (verify peak shape is acceptable)
3. **Keep inlet at 150 °C** — good compromise minimising thermal decomposition of PETN/RDX while vaporising ethanol
4. **Filter extracts** (0.2 µm PTFE syringe filter) — as important as liner choice for extending liner and column lifetime with cotton swab matrix

### Literature Support for No-Wool Liner

#### Marder et al. (2018) — Indirect support

**Paper**: Marder D, Tzanani N, Prihed H, Gura S. "Trace detection of explosives with a unique large volume injection gas chromatography-mass spectrometry (LVI-GC-MS) method." *Anal. Methods*, 2018, 10, 2712–2721. DOI: 10.1039/c8ay00480c

**Relevance**: This paper targets the same analytes (PETN, RDX, NG) but uses a substantially different instrumental setup (splitless PTV-LVI, 20 µL injection, NCI ionization, MTBE:acetone solvent, double-column configuration) vs our split-mode EI method with 1 µL ethanol injection. Despite these differences, two findings are directly relevant:

1. **RDX adsorbs on liner packing.** For RDX specifically, the authors switched from a CarboFrit-packed liner to a baffled (empty) liner because "for the low-volatile compound RDX, the CarboFrit trapped the compound, and an empty liner was preferred." If even relatively inert CarboFrit causes adsorption problems for RDX, glass wool (which has a much higher surface area and more active silanol sites) will be worse.

2. **Higher flow reduces active-site interactions for RDX.** Column flow was increased from 2 to 3.5 mL/min for RDX to "decrease the interactions between the compound and active sites, consequently preventing its degradation."

**Limitations as a reference for our system**: The paper uses splitless injection (where wool/packing serves a different purpose than in split mode), NCI ionization, and a different solvent. It does not directly test wool vs no-wool in a split-mode configuration.

#### Emmrich et al. (2001) — Most applicable reference found

**Paper**: Emmrich M, Lahrz T, Spyra W. "Influence of solvents and gas chromatographic injector conditions on the detectability of nitroaromatic compounds." *J. Chromatogr. A*, 2001, 918(1), 121–126. DOI: 10.1016/S0021-9673(01)00758-0

**Emmrich method details**: HP 5890 GC with dual ECD detection (+ HP 6890/MSD 5973 for compound identification). Split/splitless injector operated in **splitless mode** (30 s purge time), 2 µL injection, **inlet 240 °C**, H2 carrier gas. 2 m deactivated retention gap into dual columns (60 m MDN-12 + 57 m XTI-5, both 0.25 mm ID × 0.25 µm). Oven: 60 °C (1 min), 20 °C/min to 120 °C, 3 °C/min to 250 °C, 10 min hold (~55 min run). Analytes: 13 nitroaromatic compounds (TNT isomers, TNB, DNTs, DNBs, amino-DNTs, mononitrotoluenes) at 0.55–3.0 mg/L in toluene/methanol/ACN/MTBE. Liners tested: empty (deactivated single-taper, 4 mm ID), quartz wool, DMCS-treated glass wool, prepacked CarboFrit.

**Key findings**:

1. **Glass wool causes catastrophic signal loss for nitro-explosives.** TNT isomers and 1,3,5-trinitrobenzene peaks decreased to 0–3% of empty-liner heights with DMCS glass wool, and to 0–39% with quartz wool. DMCS deactivation was insufficient to prevent degradation.

2. **Empty liner or CarboFrit gives highest peaks.** CarboFrit recoveries were comparable to the empty liner (up to 118%), confirming that a low-activity packing can be acceptable but wool cannot.

3. **Contact surface area drives degradation.** "The larger the contact surface the greater the number of active sites enhancing adsorption, degradation or other negative effects."

4. **Solvent choice also matters.** Methanol reduced TNT peak heights by >78%; acetonitrile caused total disappearance of TNT peaks. The authors identified a methoxylation transformation product (dinitromethoxytoluene, m/z 212), showing that methanol actively participates in chemical reactions at the inlet. Toluene gave the best results; MTBE was intermediate.

5. **TNT isomers and TNB are most affected; DNTs and mono-NTs less so.** The compounds most sensitive to wool degradation are those with three nitro groups (TNT, TNB). Dinitro- and mononitro-compounds showed relative heights >61% with wool (except 1,4-DNB on DMCS wool). This suggests the degradation mechanism is related to the number/density of nitro groups.

**Comparison with our FINEX method — key differences and their impact**:

| Parameter | Emmrich et al. | FINEX | Impact on relevance |
|---|---|---|---|
| **Injection mode** | Splitless (30 s purge) | Split (20:1) | **Weakens applicability.** In splitless mode the liquid sample falls back in the liner and contacts wool for up to 30 s. In split mode, vapour is swept through quickly (~ms residence time). Wool contact time is much shorter in our system, so the degradation effect will be less severe. |
| **Inlet temperature** | 240 °C | 150 °C | **Weakens applicability.** Higher temperature drives more thermal decomposition on active sites. Emmrich's 240 °C is a worst-case; our 150 °C will produce less thermal degradation on wool. |
| **Analyte class** | Nitroaromatics (TNT, DNTs) | Nitrate ester (PETN), nitramine (RDX) | **Strengthens applicability.** PETN and RDX are more thermally labile than TNT. TNT has a relatively stable aromatic ring; PETN decomposes via O-NO2 homolysis and RDX via N-NO2 homolysis at lower temperatures. If wool destroys TNT at 240 °C, it is plausible it would affect PETN/RDX even at 150 °C. |
| **On-column mass** | ~1–6 ng (splitless) | ~10–500 pg (after 20:1 split) | **Strengthens applicability.** At 10–100× lower on-column mass, any fractional loss to active sites has a proportionally larger impact on quantitation accuracy. |
| **Solvent** | Toluene (best), methanol (worst) | Ethanol | **Strengthens applicability.** Ethanol is chemically similar to methanol (both protic, both can participate in inlet reactions). Emmrich showed methanol caused >78% peak loss for TNT and generated transformation products via methoxylation at 240 °C. Ethanol could cause analogous ethoxylation reactions, though likely less severe at 150 °C. |
| **Detector** | ECD | EI-MSD (SIM/Scan) | **Neutral.** Both detectors would show the same signal loss from inlet degradation; the finding is about what happens in the inlet, not the detector. |

**Overall assessment**: Emmrich et al. is the most applicable reference found for the no-wool recommendation, but it is not a direct validation for our specific conditions. Their splitless mode at 240 °C represents a worst-case scenario: longer wool contact time and higher thermal stress than our split mode at 150 °C. The wool-induced degradation in our system would be less dramatic than the near-total peak disappearance Emmrich observed. However, two factors specific to our system make even a partial effect significant: (1) our analytes are more thermally labile than TNT, and (2) our on-column masses are 10–100× lower, meaning even modest fractional losses compromise quantitation at the low end of our calibration range (0.2 ng/µL = ~10 pg on-column).

The paper supports removing wool as a precautionary measure but does not prove that wool is causing the specific progressive signal loss observed in our sequences — that could also be (and likely is) driven by matrix accumulation on the wool and liner surfaces over many injections, which is a different mechanism from the acute single-injection degradation Emmrich measured.

#### Additional supporting references

- **Grob K, Neukom HP Jr, Hilling P.** "Glass wool in the inserts of split injectors for capillary GC?" *J. High Resolut. Chromatogr.*, 1981, 4, 203–205. Foundational paper testing glass wool specifically in **split mode**. Found that wool did not improve precision as claimed, worsened accuracy of peak area ratios, and "promotes degradation reactions of labile sample components." Concluded "the use of evaporation aids such as glass wool cannot be generally recommended." This is the most relevant reference for our split-mode configuration specifically, though it did not test explosives.

- **Mol JGJ, Hendriks PJM, Janssen JGM, et al.** "Large volume injection in capillary GC using PTV injectors: comparison of inertness of packing materials." *J. High Resolut. Chromatogr.*, 1995, 18, 124–128. Systematic comparison of packing materials in PTV injectors finding that "thermal degradation of labile analytes and adsorption" are caused by "active sites on the glass wool surface." Tested alternatives and found glass wool to be among the worst for thermally labile compounds.

- **Bailey R.** "Injectors for capillary gas chromatography and their application to environmental analysis." *J. Environ. Monit.*, 2005, 7, 823–829. DOI: 10.1039/b500038e. Review stating that "liners with glass wool show significant degradation for thermally labile compounds" in pulsed-splitless injection.

#### Summary of literature evidence

The literature consistently shows that glass wool causes adsorption and/or thermal decomposition of nitro-compounds in GC inlet liners. This effect persists even with DMCS-deactivated (silanized) wool (Emmrich et al. 2001). The effect is best documented for splitless injection at high inlet temperatures (240 °C), where it is catastrophic for trinitro-compounds. For our split-mode system at 150 °C, the acute single-injection effect is expected to be less severe, but: (a) our analytes are more thermally labile than TNT, (b) our on-column masses are much lower making losses proportionally more significant, and (c) the progressive contamination of wool over many injections (matrix accumulation creating new active sites) is an additional mechanism beyond what Emmrich tested. Grob (1981) provides the most direct evidence against wool in split injection specifically, though not for explosives. The recommendation to switch to a single-taper, ultra-inert, no-wool liner remains well supported as a precautionary and best-practice measure.

## Filtering Study Results (April 2026)

### Datasets

Three GC-MS runs provide data on the effect of sample filtration on IS stability and system performance:

| Dataset | Path | Sample Type | Filtration | Injections | Date |
|---------|------|-------------|------------|------------|------|
| **Method Dev** | `.../Method Development/20260416- Filtering/` | Simulated: clean surface swabbed, extracted, spiked with IS only (no analyte). All from vial 7. | 0.45 µm Mini UniPrep | 76 (19 QC, 19 sample, 38 blank) | 16 Apr 2026 |
| **Lab6-1** | `.../FINEX Swabbing Study/Lab6-1/` | Real FINEX swab samples (P1, 4 surfaces: S, ABS-S, G, ABS-T) | 0.45 µm Mini UniPrep | 80 (8 QC, 28 sample, 38 blank, 6 cal) | 17 Apr 2026 |
| **Lab30** | `.../FINEX Swabbing Study/Lab30 (unfilterd)/` | Real FINEX swab samples (P1, S surface only) | **None** | 37 (7 QC, 7 sample, 16 blank, 6 cal, 1 extra QC) | 20 Apr 2026 |

**Note**: Lab6-1 and Lab30 are from different labs/participants so sample concentrations are not directly comparable. IS trends and QC performance within each run are the meaningful comparisons.

**Method Dev vial error**: Injection "Filtered Sample -17" (line 62) was drawn from vial 9 (the 6ng QC standard) instead of vial 7. Its signals match the flanking QCs exactly. This row should be excluded from sample analysis.

### IS Stability: Key Finding -- Matrix Protects NG

The central finding is that NG (PETN internal standard) behaves differently in real samples versus simulated (clean) samples:

#### NG (PETN IS, petn_is_pa) Trends

| Dataset | QC Trend | Sample Trend | Interpretation |
|---------|----------|--------------|----------------|
| **Method Dev (simulated, filtered)** | 971 → 591 (**-39%**) | 880 → 562 (**-36%**) | Both decline together. Clean matrix offers no protection. |
| **Lab6-1 (real, filtered)** | 1077 → 865 (**-20%**) | 1057 → 1162 (**+10%, stable**) | QCs decline but sample NG is flat/rising. Real matrix protects NG. |
| **Lab30 (real, unfiltered)** | 1240 → 685 (**-45%**) | 1317 → 1170 (**-11%, near-stable**) | QCs decline steeply but sample NG drops only modestly. |

**In simulated samples** (clean swab extract, no analyte matrix), NG declines at the same rate as the QCs -- roughly 36-39% across the sequence. The clean extract provides no protective effect against progressive inlet degradation.

**In real samples** (swab extracts containing explosive residues and surface matrix), NG is stable or near-stable across 7-28 sample injections, even while QC injections of the same NG standard show clear decline. The real sample co-extractives appear to competitively occupy active sites in the inlet, shielding NG from adsorption/degradation during those injections.

This is consistent with the **matrix-induced chromatographic response enhancement** effect described in the analyte protectants literature (Anastassiades et al. 2003): matrix components mask active sites, reducing analyte losses.

#### 15N-RDX (RDX IS, rdx_is_pa) Trends

| Dataset | QC Trend | Sample Trend |
|---------|----------|--------------|
| **Method Dev (simulated, filtered)** | 12,587 → 6,471 (**-49%**) | 7,544 → 5,425 (**-28%**) |
| **Lab6-1 (real, filtered)** | 9,563 → 9,783 (**+2%, stable**) | 8,294 → 16,312 (**+97%, increasing**) |
| **Lab30 (real, unfiltered)** | 11,634 → 6,608 (**-43%**) | 15,738 → 20,167 (**+28%, increasing**) |

15N-RDX IS in real samples trends upward rather than declining. This is likely driven by matrix ion enhancement: real sample co-extractives enhance the IS signal at m/z 122 in a way that overwhelms any degradation losses.

In the method dev simulated data, 15N-RDX declines monotonically in both QCs and samples (same pattern as NG), confirming the clean matrix offers no protection for either IS.

#### RDX Ratio Stability (Method Dev)

Despite the raw signal decline (~49% for RDX IS), the IS-normalised RDX ratio in method dev QCs is remarkably stable:

| QC # | rdx_is_pa | rdx_pa | rdx_ratio |
|------|-----------|--------|-----------|
| 1 (line 4) | 12,587 | 14,023 | 1.114 |
| 5 (line 20) | 10,299 | 11,320 | 1.099 |
| 10 (line 40) | 7,879 | 8,622 | 1.094 |
| 15 (line 60) | 7,408 | 8,033 | 1.084 |
| 19 (line 76) | 6,471 | 6,954 | 1.075 |

RDX ratio %RSD across 19 QCs: **~1.2%**. The IS correction effectively eliminates the raw signal drift for RDX, because 15N-RDX and RDX co-elute and experience identical degradation.

#### PETN Ratio Stability (Method Dev)

| QC # | petn_is_pa (NG) | petn_pa | petn_ratio |
|------|-----------------|---------|------------|
| 1 (line 4) | 971 | 131,039 | 135.0 |
| 5 (line 20) | 787 | 105,366 | 133.9 |
| 10 (line 40) | 660 | 80,585 | 122.2 |
| 15 (line 60) | 672 | 74,996 | 111.7 |
| 19 (line 76) | 591 | 65,436 | 110.6 |

PETN ratio declines **~18%** despite IS correction. NG (the PETN IS) and PETN elute at different retention times and have different thermal lability profiles, so NG cannot fully track PETN's degradation. This is a fundamental limitation of using NG as the PETN internal standard.

### QC Performance Across Datasets

#### Lab6-1 (Filtered, Real) -- 6ng QC PETN Accuracy Progression

| QC Bracket | Position | PETN conc (PA) | PETN %Bias | PETN QC Flag |
|------------|----------|----------------|------------|--------------|
| 1 | After S samples | 4.81 ng | -20% | PASS (borderline) |
| 2 | After ABS-S | 3.95 ng | -34% | FAIL |
| 3 | After G | 3.75 ng | -38% | FAIL |
| 4 | After ABS-T | 2.82 ng | -53% | FAIL |

PETN accuracy degrades from -20% to -53% across 4 QC brackets. RDX 6ng QCs pass all brackets (-6% to +3% bias).

#### Lab30 (Unfiltered, Real) -- End-of-Run QC Decline

| QC | PETN PA (start) | PETN PA (end) | Change | RDX PA (start) | RDX PA (end) | Change |
|----|-----------------|---------------|--------|-----------------|---------------|--------|
| 6ng (vial 42) | 135,189 | 66,964 | **-50%** | 29,329 | 13,316 | **-55%** |
| 6ng (cal curve) | -- | 42,761 | **-68%** vs start | -- | 6,954 | **-76%** vs start |

Unfiltered Lab30 shows steeper QC signal decline than filtered Lab6-1, consistent with faster inlet degradation from unfiltered matrix.

#### Low-Level QC (0.2ng) Reliability

In both Lab6-1 and Lab30, the 0.2ng QC is unreliable:
- Lab6-1: RDX 0.2ng QC shows +194% to +355% bias (massive over-quantification, likely from carryover/contamination buildup)
- Lab6-1: PETN 0.2ng QC goes out of range after the first bracket
- Lab30: End-of-run 0.2ng QC shows -86% PETN bias (signal effectively gone)

### Lab6-1 Surface Ranking (Filtered Real Samples)

Lab6-1 analysed 4 surfaces (6 replicates each + negative control). Mean peak areas (samples only, PA method):

| Surface | Mean PETN PA | Mean RDX PA | PETN Quantifiable? | RDX Quantifiable? |
|---------|-------------|-------------|--------------------|--------------------|
| Glass (G) | 246,089 | 13,403 | Yes (some out-of-range high) | Yes |
| Steel (S) | 74,616 | 6,953 | Yes | Yes |
| ABS-Smooth | 25,510 | 360 | Yes (low end) | All below calibration range |
| ABS-Textured | 12,545 | 536 | Marginal | All below calibration range |

Glass surfaces gave the highest recovery. ABS surfaces gave very low RDX (all below the 0.2ng calibration range). All negative controls were clean.

### Conclusions

1. **Filtering slows but does not eliminate IS degradation.** QC IS decline is ~20% (filtered Lab6-1) vs ~43-45% (unfiltered Lab30) over comparable injection counts, roughly halving the degradation rate.

2. **Real sample matrix protects NG (PETN IS).** In simulated clean extracts, NG declines at the same rate as QCs (-36% to -39%). In real sample injections, NG is stable or near-stable despite QCs declining on either side. This is a matrix-induced protective effect: co-extractives occupy active inlet sites that would otherwise adsorb NG.

3. **PETN remains the problem analyte.** PETN signal degrades across all sequences, and IS correction with NG only partially compensates (~18% residual drift in the method dev ratio). The RDX/15N-RDX ratio is near-perfect (~1.2% RSD) because the analyte and IS co-elute and share the same degradation pathway.

4. **NG is a suboptimal IS for PETN quantification.** NG elutes at a different RT from PETN and has a different thermal lability profile, so it cannot fully track PETN losses. A deuterated or 13C-labelled PETN analogue would be the ideal IS, but none is commercially available.

5. **Progressive inlet contamination is the dominant instrument effect.** Even filtered clean extracts show ~40-49% IS decline over 76 injections. The contamination source is not solely the sample matrix -- solvent residue, IS decomposition products, and column bleed all contribute.

6. **The 0.2ng QC is not reliable as a quantitative check** in either filtered or unfiltered runs, due to carryover/contamination effects at this low level.

## Surface Chemistry and Surface Free Energy

The FINEX swabbing study uses four surfaces: mild steel (S), glass (G), ABS-Smooth (ABS-S), and ABS-Textured (ABS-T). Surface free energy (SFE) and surface functional groups influence both the initial adhesion of explosive particles to the surface and the efficiency of swab recovery into ethanol. This section documents the surface chemistry of each material and provides literature references for SFE values and surface characterisation.

**Surface descriptions:**

A glass surface is dominated by silanol groups (Si-OH) [Zhuravlev, 2000] which produce a hydrophilic surface with a large SFE (~84 mJ/m² for a clean surface [Rhee, 1977]) due to the strong polar and hydrogen-bonding interactions.

A mild steel surface exposed to ambient air rapidly forms a native oxide layer consisting of Fe₂O₃, Fe₃O₄, and FeOOH [Fehlner & Mott, 1970]. The surface Fe-OH hydroxyl groups are amphoteric and capable of hydrogen bonding, contributing to a high energy surface (SFE ~44 mJ/m² for a polished mild steel surface [Ghanbari & Attar, 2014]).

ABS is a two-phase polymer consisting of a matrix of non-polar poly(acrylonitrile-styrene) with a dispersed polybutadiene phase [McKeen, 2010]. It is a lower energy surface (SFE ~35-42 mJ/m² [Lewin et al., 2005]) with only a weak polar contribution from nitrile (C≡N) groups.

### Mild Steel

#### Composition

Mild steel (low-carbon steel) is typically AISI 1018/1020 or ASTM A36 grade:

| Element | Typical wt% |
|---------|------------|
| Fe | ~98.5-99.2% (balance) |
| C | 0.15-0.25% |
| Mn | 0.30-0.60% |
| Si | ≤0.40% |
| P | ≤0.040% |
| S | ≤0.050% |

Unlike stainless steel, mild steel has **no chromium** and therefore no passive Cr₂O₃ layer. The surface relevant to swabbing is the **native oxide layer** (formed within seconds of air exposure), not bare iron.

#### Surface Groups

The native oxide on mild steel consists of:

- **Iron oxides**: Fe₂O₃ (hematite), Fe₃O₄ (magnetite), FeOOH (goethite/lepidocrocite). Typical layered structure: inner Fe₃O₄, outer FeOOH/Fe₂O₃.
- **Surface hydroxyl groups**: Fe-OH (amphoteric, pKa ~7-9, can act as both proton donors and acceptors)
- **Adsorbed water and organics** (in ambient conditions)

The Fe-OH groups on mild steel are more polar and hydrogen-bonding capable than Cr-OH on stainless steel, which may contribute to stronger initial adhesion of polar explosive residues (PETN nitrate ester groups, RDX nitramine groups) and potentially better release into polar ethanol during swab extraction.

#### Surface Free Energy

**Reported SFE**: ~44 mJ/m² (as-received/ambient, oxide-covered). Ghanbari & Attar (2014) measured γ^LW = 42.1, γ^+ = 0.1, γ^- = 10.2 mJ/m² (γ^TOT ≈ 44 mJ/m²) for polished ST37 mild steel using the van Oss-Good method. The SFE is governed by the oxide/hydroxide layer and adsorbed contaminants, not the bulk metal. Freshly polished mild steel rapidly oxidises in air, so the swabbed surface is always the oxide.

#### Mild Steel References

- **ASTM A36/A36M** -- *Standard Specification for Carbon Structural Steel.* C ≤ 0.26%, Mn 0.60-0.90%. The most commonly available "mild steel" in laboratory and commercial settings.
- **BS EN 10025-2:2019** -- *Hot rolled products of structural steels. Technical delivery conditions for non-alloy structural steels.* European standard specifying composition of S235, S275, S355 structural mild steels.
- **ASM International.** *ASM Handbook, Volume 1: Properties and Selection: Irons, Steels, and High-Performance Alloys.* ASM International, 1990. Standard reference for steel grades and compositions.
- **Callister WD Jr, Rethwisch DG.** *Materials Science and Engineering: An Introduction.* 10th ed., Wiley, 2018. Textbook reference for steel phase diagrams and composition tables; chapter on ferrous alloys covers mild/low-carbon steel composition and microstructure (ferrite + pearlite).
- **Cornell RM, Schwertmann U.** *The Iron Oxides: Structure, Properties, Reactions, Occurrences and Uses.* 2nd ed., Wiley-VCH, 2003. Definitive reference for iron oxide phases (hematite, magnetite, goethite, lepidocrocite, ferrihydrite), surface hydroxyl group density, surface charge, and reactivity.
- **Biesinger MC, Payne BP, Grosvenor AP, Lau LWM, Gerson AR, Smart RSC.** "Resolving surface chemical states in XPS analysis of first row transition metals, oxides and hydroxides." *Appl. Surf. Sci.*, 2011, 257(7), 2717-2730. Definitive XPS reference for iron oxide identification (Fe₂O₃, Fe₃O₄, FeOOH) on steel surfaces.
- **Stratmann M, Muller J.** "The mechanism of the oxygen reduction on rust-covered metal substrates." *Corrosion Sci.*, 1994, 36(2), 327-359. Characterisation of mild steel oxide/rust layers including layered structure.
- **Ghods P, Isgor OB, McRae GA, Gu GP.** "Electrochemical investigation of chloride-induced depassivation of black steel rebar under simulated service conditions." *Corrosion Sci.*, 2010, 52(5), 1649-1659. XPS characterisation of mild (black) steel oxide films showing Fe₂O₃ and FeOOH composition.
- **Fehlner FP, Mott NF.** "Low-temperature oxidation." *Oxid. Met.*, 1970, 2(1), 59-99. DOI: 10.1007/BF00603582. Seminal paper on native oxide formation on metals at room temperature. Covers the Cabrera-Mott mechanism of oxide growth, the transition from chemisorbed monolayer to bulk oxide, and specifically discusses iron oxide formation at 25°C with logarithmic growth kinetics. Classifies iron oxide as a network modifier allowing cation migration.
- **Ghanbari A, Attar MM.** "Surface free energy characterization and adhesion performance of mild steel treated based on zirconium conversion coating: A comparative study." *Surf. Coat. Technol.*, 2014, 246, 26-33. DOI: 10.1016/j.surfcoat.2014.02.057. Measured SFE components of bare mild steel (ST37, equivalent to ASTM A36) using van Oss-Good method with three probe liquids (water, formamide, diiodomethane) via Wilhelmy plate technique. Reports γ^LW = 42.1, γ^+ = 0.1, γ^- = 10.2 mJ/m² for polished/degreased mild steel (Table 2), giving γ^TOT ≈ 44 mJ/m². Attributes the electron donor (γ^-) component to the oxide/hydroxide layer formed under ambient atmospheric conditions.

### Glass (Soda-Lime)

#### Surface Groups

- **Silanol groups (Si-OH)**: dominant surface functionality, density ~4-5 OH/nm². Types: isolated, vicinal, geminal.
- **Siloxane bridges (Si-O-Si)**: hydrophobic when dehydroxylated
- **Metal oxide sites**: Na₂O, CaO, MgO from soda-lime composition (affect surface basicity)
- **Adsorbed water layer**

#### Surface Free Energy

**Reported SFE**: ~84 mJ/m² for a clean surface (Rhee, 1977). Highest of the three FINEX materials. Varies significantly with cleaning protocol and relative humidity; as-received/ambient values may be lower (~45-75 mJ/m²) due to adsorbed contaminants.

#### Glass References

- **Zhuravlev LT.** "The surface chemistry of amorphous silica. Zhuravlev model." *Colloids Surf. A*, 2000, 173(1-3), 1-38. Definitive reference for silanol group types (isolated, vicinal, geminal) and surface hydroxyl density on silica/glass surfaces.
- **Rhee SK.** "Surface energies of silicate glasses calculated from their wettability data." *J. Mater. Sci.*, 1977, 12(4), 823-824. DOI: 10.1007/BF00548176. Calculated surface energies of silicate glasses from contact angle data; reports ~84 mJ/m² for clean soda-lime glass.
- **Vigil G, Xu Z, Steinberg S, Israelachvili J.** "Interactions of silica surfaces." *J. Colloid Interface Sci.*, 1994, 165(2), 367-385. Surface forces and hydration of glass/silica surfaces.
- **Stoch L, Stoch A.** "The surface chemistry of glass." *Opt. Appl.*, 2005, 35(4), 823-831. Reviews surface composition of soda-lime and borosilicate glass, including alkali metal leaching and effect on surface energy.

### ABS (Acrylonitrile-Butadiene-Styrene)

#### Surface Groups

- **Nitrile groups (C≡N)**: from the acrylonitrile component -- weak polar interaction
- **Phenyl groups (C₆H₅)**: from styrene -- dispersive interactions
- **Butadiene rubber phase**: unsaturated C=C, susceptible to oxidation
- **Oxidised surface groups** (C=O, C-O, COOH): present after UV exposure, corona/plasma treatment, or ageing
- ABS is a two-phase polymer (glassy SAN matrix + rubber particles); surface composition depends on processing and which phase is exposed

#### Surface Free Energy

**Reported SFE**: ~35-42 mJ/m² (lowest of the three FINEX materials, mostly dispersive with a small polar component).

**Smooth vs Textured**: The Wenzel roughness equation predicts that surface roughness amplifies the intrinsic wetting behaviour -- for a hydrophobic material like ABS, roughness makes the surface effectively more hydrophobic, increasing contact angle hysteresis and mechanically trapping particles in surface features. This is consistent with ABS-Textured giving the lowest explosive recovery in Lab6-1.

#### ABS References

- **McKeen LW.** "Chapter 4 - Styrenic Plastics." In *Fatigue and Tribological Properties of Plastics and Elastomers* (2nd ed.). Oxford: William Andrew Publishing, 2010, pp. 51-71. Reference for ABS structure and composition as a two-phase polymer (SAN matrix + dispersed polybutadiene).
- **Lewin M, Mey-Marom A, Frank R.** "Surface free energies of polymeric materials, additives and minerals." *Polym. Adv. Technol.*, 2005, 16(6), 429-441. DOI: 10.1002/pat.605. Compilation of SFE values for polymeric materials; reports ABS SFE in the range ~35-42 mJ/m².
- **Shimizu RN, Demarquette NR.** "Evaluation of surface energy of solid polymers using different models." *J. Appl. Polym. Sci.*, 2000, 76(12), 1831-1845. Measured SFE of ABS and other polymers using Owens-Wendt, Wu harmonic mean, and van Oss acid-base methods. Reports dispersive and polar components for ABS.
- **Kwok DY, Neumann AW.** "Contact angle measurement and contact angle interpretation." *Adv. Colloid Interface Sci.*, 1999, 81(3), 167-249. Comprehensive review with SFE data for many polymer surfaces including styrenic materials.
- **Wenzel RN.** "Resistance of solid surfaces to wetting by water." *Ind. Eng. Chem.*, 1936, 28(8), 988-994. Foundational reference for roughness effects on wettability -- directly relevant to ABS-Smooth vs ABS-Textured differences.
- **Extrand CW.** "A thermodynamic model for contact angle hysteresis." *J. Colloid Interface Sci.*, 1998, 207(1), 11-19. Relevant for understanding the difference between smooth and textured surfaces: roughness amplifies both hydrophobicity and hysteresis.

### Cotton Swab Material

#### Cotton Swab References

- **Fekete E, Csiszár E.** "Characterization of the Surface Properties of Cellulosic Fibers in Fibrous and Ground Forms by IGC and Contact Angle Measurements." *Fibers Polym.*, 2017, 18(7), 1255-1262. DOI: 10.1007/s12221-017-6862-z. Measured SFE of raw cotton fabric by Owens-Wendt (water + diiodomethane contact angles): γ_s^d = 31.4, γ_s^p ≈ 0 mJ/m² (Table 2). The low value and negligible polar component are attributed to the continuous waxy outer layer on raw cotton fibres.
- **Luner PE, Oh E.** "Characterization of the surface free energy of cellulose ether films." *Colloids Surf. A*, 2001, 181, 31-48. DOI: 10.1016/S0927-7757(00)00805-0. Reports SFE of cellulose (from Toussaint & Luner, 1993) by the van Oss LW:AB method: γ^LW = 39.1, γ^+ = 2.0, γ^- = 39.7, γ^AB = 17.6, γ^TOT = 56.7 mJ/m² (Table 4). Representative of scoured/wax-free cellulose surface with exposed -OH groups.
- **Toussaint AF, Luner P.** "The wetting properties of hydrophobically modified cellulose surfaces." *J. Adhesion Sci. Technol.*, 1993, 7(6), 635-648. Primary source for cellulose SFE by van Oss contact angle method.

### SFE Summary Table

| Surface | SFE (mJ/m²) | Key Surface Groups | Dominant Interaction | Expected Adhesion to Explosive Residues |
|---------|-------------|-------------------|---------------------|----------------------------------------|
| Glass | ~84 (clean) | Si-OH (polar, H-bonding), Si-O-Si | Polar + dispersive | Highest; strong H-bonding with nitrate ester/nitramine groups |
| Cellulose (scoured cotton swab) | ~57 | Cellulose -OH (polar, H-bonding) | Polar + dispersive | Swab material; polar -OH groups facilitate pickup and release of polar analytes |
| Mild Steel | ~44 | Fe-OH, Fe₂O₃, FeOOH | Polar + dispersive | Moderate; oxide OH groups bind polar analytes |
| ABS-Smooth | ~35-42 | C≡N, phenyl (mostly dispersive) | Predominantly dispersive | Lower; less polar interaction with PETN/RDX |
| ABS-Textured | ~35-42 (same chemistry) | Same as ABS-S + mechanical roughness | Dispersive + mechanical trapping | Lowest recovery; Wenzel roughness traps particles |
| Raw cotton (waxy) | ~31 | Wax layer masks cellulose -OH | Predominantly dispersive | Not representative of commercial swabs |

### Consistency with FINEX Recovery Data

The Lab6-1 surface ranking (Glass >> Steel >> ABS-Smooth > ABS-Textured) for both PETN and RDX recovery is consistent with the SFE ordering. Higher SFE surfaces with polar functional groups (glass Si-OH, steel Fe-OH) provide stronger initial adhesion of polar explosive particles but also better release into the polar extraction solvent (ethanol). ABS, with its predominantly dispersive surface, has weaker initial interaction with polar explosive residues, and the textured variant additionally traps particles mechanically in surface features, reducing swab recovery efficiency.

### General SFE Methodology References

- **Owens DK, Wendt RC.** "Estimation of the surface free energy of polymers." *J. Appl. Polym. Sci.*, 1969, 13(8), 1741-1747. Foundational method for measuring SFE via contact angle using dispersive and polar components. Used universally for polymers, metals, and glass.
- **van Oss CJ, Chaudhury MK, Good RJ.** "Interfacial Lifshitz-van der Waals and polar interactions in macroscopic systems." *Chem. Rev.*, 1988, 88(6), 927-941. Acid-base approach decomposing SFE into Lifshitz-van der Waals, electron donor, and electron acceptor components.
- **Good RJ.** "Contact angle, wetting, and adhesion: a critical review." *J. Adhesion Sci. Technol.*, 1992, 6(12), 1269-1302. Comprehensive review of contact angle methods and SFE determination.
- **Chibowski E, Perea-Carpio R.** "Problems of contact angle and solid surface free energy determination." *Adv. Colloid Interface Sci.*, 2002, 98(2), 245-264. Reviews practical difficulties in SFE measurement on metal surfaces, including the effect of oxide layers.

### Explosive-Surface Adhesion References

- **Yu HA, Becker T, Nic Daeid N, Lewis SW.** "Fundamental studies of the adhesion of explosives to textile and non-textile surfaces." *Forensic Sci. Int.*, 2017, 273, 88-95. DOI: 10.1016/j.forsciint.2017.02.008. Measured adhesion of PETN, RDX and TNT to surfaces using AFM; glass exhibited higher adhesion than polypropylene and aluminium foil.
- **Fisher D, Zach R, Matana Y, Elia P, Shustack S, Sharon Y, Zeiri Y.** "Bomb swab: Can trace explosive particle sampling and detection be improved?" *Talanta*, 2017, 174, 92-99. DOI: 10.1016/j.talanta.2017.05.085. AFM adhesion measurements of explosive particles (ETN, PETN, RDX, HMX, Tetryl) to glass, aluminium and ABS surfaces, and to swipe materials (Muslin, Nomex, Teflon, polyamide membrane). Key findings: RDX exhibited ~5x stronger adhesion to glass than other explosives; capillary forces make major contribution on hydrophilic surfaces; adhesion forces to substrates were generally larger than to swipe materials; rougher swipe surfaces collected more particles.
- **Zakon Y, Lemcoff NG, Marmur A, Zeiri Y.** "Adhesion of standard explosive particles to model surfaces." *J. Phys. Chem. C*, 2012, 116(44), 22815-22822. DOI: 10.1021/jp303622n. Studies adhesion of explosive particles to model surfaces using AFM and surface energy measurements.
- **Verkouteren JR.** "Particle Characteristics of Trace High Explosives: RDX and PETN." *J. Forensic Sci.*, 2007, 52(2), 335-340. DOI: 10.1111/j.1556-4029.2006.00354.x. Characterises particle size and morphology of RDX and PETN trace particles.
- **Verkouteren JR, Coleman JL, Fletcher RA, Smith WJ, Klouda GA, Gillen G.** "A method to determine collection efficiency of particles by swipe sampling." *Meas. Sci. Technol.*, 2008, 19(11), 115101. DOI: 10.1088/0957-0233/19/11/115101. Method for determining swipe/swab sampling collection efficiency for particles from surfaces.

### Surface Texture and Roughness References

- **Ranade MB.** "Adhesion and Removal of Fine Particles on Surfaces." *Aerosol Sci. Technol.*, 1987, 7(2), 161-176. DOI: 10.1080/02786828708959155. Review of particle adhesion mechanics; discusses how roughness reduces real contact area and van der Waals adhesion forces, and how surface features can mechanically trap particles.
- **Wenzel RN.** "Resistance of solid surfaces to wetting by water." *Ind. Eng. Chem.*, 1936, 28(8), 988-994. Roughness amplifies intrinsic wetting behaviour of surfaces (already listed in ABS references).

### Explosive Selection References

- **Bruce A, Timmerman LE, Fiakpui NA, Lessey L, Beardah MS, Nic Daéid N, Ménard H.** "A scientometric review of explosives research: Challenges and opportunities." *Forensic Sci. Int.*, 2025, 373, 112513. DOI: 10.1016/j.forsciint.2025.112513. Scientometric analysis of explosives literature using INTERPOL IFSMS reports and Scopus database. TNT most frequently referenced explosive (27.3% of IFSMS references), followed by RDX and DNT as only other explosives in >10% of references. RDX most frequently mentioned in full-text searching (62.4% of documents). Establishes prevalence of military explosives in forensic science literature.
- **Östmark H, Wallin S, Ang HG.** "Vapor Pressure of Explosives: A Critical Review." *Propellants Explos. Pyrotech.*, 2012, 37(1), 12-23. DOI: 10.1002/prep.201100083. Definitive compilation of vapour pressure data for explosives. Reports at 25°C: PETN ~1.16 × 10⁻⁸ Torr; RDX ~3.30 × 10⁻⁹ Torr. Both extremely low, confirming negligible sublimation losses at ambient temperature.

### Cotton Fibre Composition Reference

- **Elmogahzy YE.** "8 - Fibers." In *Engineering Textiles* (2nd ed.). Woodhead Publishing, 2020, pp. 191-222. Reference for cotton being primarily composed of cellulose fibres.

## GC-MS Method Development

### PETN Inlet Sensitivity References

- **Walsh ME, Ranney TA.** "Determination of Nitroaromatic, Nitramine, and Nitrate Ester Explosives in Water Using Solid-Phase Extraction and Gas Chromatography-Electron Capture Detection." *J. Chromatogr. Sci.*, 1998, 36(8), 406-416. DOI: 10.1093/chromsci/36.8.406. Explicitly states: "The GC method requires more care than HPLC. Specifically, the injection port liner must be changed frequently for accurate determinations." For nitrate esters including PETN and nitramines including RDX.
- **Cruse CA, Tipple CA, Miller ML.** "Confirmation of Solid Phase Extracted Post-Blast Explosives Residues Analyzed Using Pulsed Split Injection GC/MS." *Propellants Explos. Pyrotech.*, 2025, 50(1), e202400184. DOI: 10.1002/prep.202400184. GC-MS method for post-blast explosive residues using internal standards. Relevant as an example of IS use in forensic explosive GC-MS.

### Internal Standards

**Approach**: Dual IS system — 15N-RDX for RDX, nitroglycerin (NG) for PETN.

**Theory**: Isotopically labelled analogues of target analytes are the ideal internal standards as they share identical chromatographic behaviour and can be distinguished by mass (Burke & Mackay, 2008). The internal standard tracks and compensates for injection variability and progressive signal loss.

**RDX IS performance**: 15N-RDX co-elutes with RDX and shares the same degradation pathway. The IS ratio achieves ~1.2% RSD across 19 QC injections (Method Dev dataset) despite raw signal declining ~49%. Highly effective correction.

**PETN IS limitation**: NG elutes at a different retention time from PETN and has a different thermal lability profile. IS correction only partially compensates — ~18% residual drift remains in the PETN ratio. A deuterated/isotopically-labelled PETN analogue would be ideal but is not commercially available.

#### Internal Standard References

- **Burke DG, Mackay LG.** "Complete Equation for the Measurement of Organic Molecules Using Stable Isotope Labeled Internal Standards, Exact Matching, and Mass Spectrometry." *Anal. Chem.*, 2008, 80(13), 5071-5078. DOI: 10.1021/ac800270u. Derives the complete measurement equation for organic molecules using isotope-labelled IS. Key statement: "When the internal standard is a stable isotope labeled analogue of the target molecule, the physicochemical properties of the molecule that determine workup and analysis yield may not be significantly altered so there is a high probability that factors affecting the yield of the target molecule will also affect the labeled analogue to the same extent."
- **Cruse CA, Tipple CA, Miller ML.** (2025) — see above. Example of IS use in forensic explosive GC-MS.
- **Bünning TH, Strehse JS, Hollmann AC, Bötticher T, Maser E.** "A Toolbox for the Determination of Nitroaromatic Explosives in Marine Water, Sediment, and Biota Samples on Femtogram Levels by GC-MS/MS." *Toxics*, 2021, 9(3), 60. DOI: 10.3390/toxics9030060. GC-MS/MS method for nitroaromatic explosives using internal standards.
- **Galmiche M, Colin A, Clavos M-C, Pallez C, Rosin C, Dauchy X.** "Determination of nitroaromatic explosive residues in water by stir bar sorptive extraction-gas chromatography-tandem mass spectrometry." *Anal. Bioanal. Chem.*, 2021, 413(1), 159-169. DOI: 10.1007/s00216-020-02985-y. GC-MS/MS method for explosives in water using internal standards.

### Analyte Protectants

**Concept**: Matrix-induced chromatographic response enhancement — co-injected compounds occupy active inlet sites, reducing adsorptive losses of target analytes (Anastassiades et al., 2003).

**Observation from FINEX samples**: In real sample injections, NG was stable or near-stable while QC injections showed clear decline. The real sample matrix acted as an unintentional protectant (documented in Filtering Study Results section above).

**Deliberate protectant testing** (20260423 - Protectants dataset):
- Tested co-injection of 1 µL of 1000 ng/µL xylitol and sorbitol with samples
- Both compounds resulted in similar increases in peak area: PETN ~50% increase, RDX ~30% increase
- However, progressive signal loss still observed over 10 QC/sample alternations: PETN drops 15%, RDX drops 43%
- Conclusion: analyte protectants increase initial signal response by competing for active sites during injection, but do not prevent the progressive build-up of active sites from matrix residues in the inlet
- **Not adopted for the final method** — QC-based drift correction retained as the primary strategy

#### Analyte Protectant References

- **Anastassiades M, Maštovská K, Lehotay SJ.** "Evaluation of analyte protectants to improve gas chromatographic analysis of pesticides." *J. Chromatogr. A*, 2003, 1015(1-2), 163-184. Foundational paper on analyte protectants. Demonstrates that co-injected compounds (including sugar alcohols like xylitol and sorbitol) competitively occupy active sites in the GC inlet, reducing adsorptive losses of thermally labile analytes.

### Alternative IS Ion Investigation (May 2026)

#### Background

The current internal standard (15N-RDX, m/z 122) shows **concentration dependence** across calibration standards, indicating potential contamination of the IS with unlabeled RDX. This compromises its effectiveness for ratio-based quantification. An investigation was conducted to find alternative IS ions that could provide stable, concentration-independent signal.

#### Approach

Comprehensive TIC analysis of the 20260522 Filter Test dataset at the RDX retention time (5.79 min):

1. **TIC_AllIons_Scan.R**: Scanned ALL ions present at RDX RT across calibrants, blanks, and samples
2. **mz52_Diagnostic.R**: Detailed analysis of m/z 52 as potential alternative IS
3. **mz52_Quantification_Test.R**: Standalone quantification test comparing m/z 52 vs m/z 122 IS
4. **mz53_Quantification_Test.R**: Similar test for m/z 53

Classification criteria for IS candidates:
- CV < 15% across calibrants
- Signal/Blank > 10
- Correlation with concentration < 0.3
- Low sample deviation (< 30%)

#### Key Findings

**m/z 122 (Current IS) Performance:**
- Strong positive correlation with RDX concentration (r > 0.9)
- Confirms RDX contamination of the IS stock
- m/z 122 is NOT an independent IS signal

**m/z 52 Investigation:**
- Initially promising as a potential 15N-RDX fragment ion
- CV similar to m/z 122 (~25%)
- However, shows strong concentration dependence
- Derived from RDX fragmentation, not an independent IS signal

**m/z 53 Investigation:**
- Similar results to m/z 52
- Not suitable as alternative IS

**Full Ion Scan Result:**
- **No viable alternative IS candidates found** in the TIC data
- All ions at RDX RT are either RDX fragments or co-eluting analytes
- No ion shows both stability AND independence from RDX concentration

#### Conclusion

There are no alternative ions from 15N-RDX that can serve as an independent internal standard. All detectable ions at the RDX retention time are either:
1. RDX fragments (m/z 46, 75, 120) - directly concentration-dependent
2. 15N-RDX fragments (m/z 47, 76, 122) - partially concentration-dependent due to contamination
3. Background/ambient ions (various m/z) - unreliable signal

#### Next Steps

**Current direction**: Experiment with no internal standard (PA-only quantification):
- QC-based drift correction for PETN (already implemented)
- Evaluate if PA-only method provides acceptable accuracy
- Consider mathematical correction for m/z 122 contamination if ratio method required

#### Files Added to Diagnostics/

| Script | Lines | Purpose |
|--------|-------|---------|
| `TIC_AllIons_Scan.R` | 561 | Comprehensive ion scan at RDX RT |
| `TIC_IonCorrelation_Diagnostic.R` | 437 | Ion correlation analysis |
| `mz52_Diagnostic.R` | 453 | m/z 52 IS candidate analysis |
| `mz52_Analysis.R` | 116 | Simplified m/z 52 comparison |
| `mz52_Quantification_Test.R` | 443 | Full quantification test m/z 52 vs 122 |
| `mz53_Quantification_Test.R` | ~450 | Full quantification test m/z 53 vs 122 |

### Drift Correction Description (for report)

**Problem**: Despite filtering, IS correction, and analyte protectants, PETN signal still degrades progressively across long injection sequences.

**Approach**: QC-based drift correction. QC injections of known concentration (6 ng/µL) are measured at regular intervals. For each QC, a correction factor is calculated as: CF = Reference Cal PA / Measured QC PA. These CFs are fitted with a polynomial model as a function of injection position. An anchor normalisation step divides all predicted CFs by the predicted factor at the calibration position, forcing CF = 1.0 at the Cal point. The anchored CFs are then multiplied by raw peak areas before calibration inversion.

**Key references for report**:
- Yu et al. (2025) *Sci. Rep.* 15:24794 — QC interpolation for GC-MS drift
- Dunn et al. (2011) *Nat. Protoc.* 6:1060-1083 — QC-RLSC signal correction

## Diagnostic Scripts

### Current Diagnostics (Diagnostics/)

| Script | Purpose |
|--------|---------|
| `SequenceDiagnostics.R` | QC PA/ratio trends, sample concentration box plots by surface |
| `DriftCorrectionComparison.R` | Compare drift correction methods (with/without anchor) |
| `DriftCorrectionDiagram.R` | 3-panel diagram of anchor normalisation |
| `Partial_Drift_Correction.R` | Apply drift correction to specific region of sequence only |
| `SplitDualMethod.R` | Split dual-method sequences for independent processing |
| `DualMethodProcess.R` | Process dual-method sequences |
| `TIC_AllIons_Scan.R` | Comprehensive ion scan for IS candidates |
| `TIC_IonCorrelation_Diagnostic.R` | Ion correlation analysis |
| `mz52_Diagnostic.R` | m/z 52 alternative IS investigation |
| `mz52_Analysis.R` | Simplified m/z 52 comparison |
| `mz52_Quantification_Test.R` | Full quantification test m/z 52 vs 122 |
| `mz53_Quantification_Test.R` | Full quantification test m/z 53 vs 122 |

**Configuration notes**:
- `SequenceDiagnostics.R`: Set `qc_lines_to_plot <- c(...)` or `NULL` at top to filter QC plot to specific sequence line numbers. Uses `Line` (sequence log line number), not `Row`.
- `DriftCorrectionComparison.R`: Produces two outputs — method comparison and before/after plots
- All scripts use `gridExtra` for multi-panel layouts (not `patchwork`, due to ggplot2 version compatibility)
- Box plots show: box = IQR, line = median, red diamond = mean, outlier points

#### Partial_Drift_Correction.R

**Purpose**: Standalone script to apply drift correction only to a specific region of a GC-MS sequence where drift is present, while leaving earlier injections unchanged.

**Use case**: Split-sequence experiments where one set of samples shows drift while another does not. For example, 20260602 Filter Test:
- Lines 1-46 (filtered samples): QCs stable, no drift
- Lines 47-75 (unfiltered samples): QCs show drift

Standard drift correction would apply to the entire sequence, but this would incorrectly "correct" the stable filtered region. This script corrects only the drifted region.

**How it works**:
1. Loads existing `_GCMSResults.csv` (requires prior pipeline run)
2. Builds drift model from QCs in user-defined drifted region
3. Predicts correction factors: CF = 1.0 for non-drifted region, CF > 1.0 for drifted region
4. Re-fits calibration using corrected peak areas
5. Re-quantifies all concentrations with new calibration
6. Outputs corrected CSV with diagnostic plots

**Configuration parameters** (set at top of script):
```r
DataFolder <- "path/to/experiment"
drift_start_line <- 47    # First line with drift
drift_end_line   <- 75    # Last line with drift
qc_level_for_drift <- 6   # QC level for drift model (6ng recommended)
```

**Output files** (saved to Results/ folder):
- `*_DriftCorrected.csv`: Corrected results with new columns:
  - `petn_drift_cf_partial`: Correction factor per injection
  - `petn_pa_corrected`: Drift-corrected peak area
  - `petn_concentration_corrected`: Re-quantified concentration
  - `petn_percent_bias_corrected`: %Bias after correction
- `Partial_DriftCorrection_Diagnostic.png`: Two-panel plot:
  - Panel 1: Drift model with correction factors, unfiltered region shaded
  - Panel 2: QC accuracy before/after correction

**Assumptions**:
- Drift is monotonic signal loss (not enhancement)
- Calibrants are in non-drifted region (reference value from Cal set 1)
- QC level selected for drift model has sufficient points in drifted region (≥2)

**Limitations**:
- Designed for PETN PA-only correction (RDX uses IS ratio and is left unchanged)
- Does not modify SNR values (uses existing SNR from pipeline output)
- Linear drift model only (with 2 QC points); quadratic if ≥3 QC points available

### Deprecated Diagnostics (v2-NG/Code/) - ARCHIVED

**Status**: These scripts are retained in `Archive/v2-NG_Deprecated/` for reference only. The v2-NG pipeline has been superseded by the consolidated Code/ pipeline (May 2026).

| Script | Purpose | Status |
|--------|---------|--------|
| `Test.R` | Peak detection and integration | Replaced by `Code/02_PeakDetection.R` |
| `MetadataLinearWeighted.R` | Calibration and quantification | Replaced by `Code/03_Quantification.R` |
| `MetadataLinearWeighted_15NRDX.R` | PETN ratio via 15N-RDX IS | Variant not needed in consolidated pipeline |
| `CollateStudyResults.R` | Study-level collation | Retained as `Code/04_CollateStudyResults.R` |

**Key differences from v2-NG**:
- Single IS system (15N-RDX only, NG removed)
- Simplified configuration (no `cal_methods` field)
- Single quantification method per analyte (PA or ratio, not both)
- Multi-facet QC Accuracy plots replaced with single facet per analyte

## Dual-Method Sequences (20260428 Cal Check)

The 20260428 Cal Check sequence uses two GC methods with different retention times:
- **V8** (`METHOD G ACB - 100 V8.M`): Lines 1-18, 29. Standard method with PETN~273s, RDX~322s.
- **V4** (`METHOD GERSTEL ACB V4.M`): Lines 19-28, 30-33. Faster method, analytes elute ~60s earlier (PETN~213s, RDX~261s, NG~77s).

**Processing approach**: Use `SplitDualMethod.R` to split into V8/ and V4/ subfolders with their own .LOG sequence files and SIM data. Run the main pipeline independently on each subfolder, changing RTs in GlobalCode.R between runs.

**V4 RT estimates** (adjust if peaks not found):
- `rdx_is_config$rt <- 261`
- `petn_is_config$rt <- 77`
- `analytes$PETN$rt <- 213`
- `analytes$RDX$rt <- 261`

**Note**: The pipeline parses `.LOG` files (not `.TSV`). The sequence log parser looks for lines matching `"^\\s*\\d+\\)\\s+(\\w+)\\s+(\\d+)\\s+(\\d+)\\s+(.*)$"` in `.LOG` files found via `list.files(DataFolder, pattern = "Sequence.*\\.LOG$")`.

## Existing Code Issues (Not Yet Addressed)

- No automated tests (testthat) - significant concern for forensic software
- Global state dependency (~14+ global variables, no parameter passing)
- Hardcoded absolute paths throughout
- No error recovery (zero tryCatch blocks)
- No dependency management (renv)

**Resolved in May 2026**:
- ~~Three near-identical pipeline copies~~ → Consolidated into single `Code/` folder
- ~~Legacy Code/ directory with variants~~ → Moved to `Archive/Legacy_Code/`
- ~~Dead code accumulation~~ → Cleaned up in consolidated pipeline
- v1-NoNG and v2-NG variants archived in `Archive/`

**Current Project Structure**:
```
GCMSQuantitation/
├── GlobalCode.R              # Main configuration
├── Code/                     # Active pipeline (5 scripts)
│   ├── 01_MsFilesReorganiser.R  # Convert .ms1 files
│   ├── 02_PeakDetection.R       # Peak detection and integration
│   ├── 03_Quantification.R      # Calibration and quantification
│   ├── 04_CollateStudyResults.R # Study-level collation
│   └── ModPeaks.R               # Signal processing functions
├── Diagnostics/              # Analysis and investigation scripts
├── Archive/                  # Deprecated versions (for reference)
│   ├── Legacy_Code/
│   ├── v1-NoNG/
│   └── v2-NG_Deprecated/
└── CONTEXT.md
```

## Code Quality Policy

This codebase supports forensic research (method development and validation for a swabbing study) rather than operational forensic casework. The following quality measures are implemented:

### Implemented (June 2026)

**Reproducibility logging** (`GlobalCode.R` — `log_reproducibility()` function):
- Writes `run_metadata.yaml` to the DataFolder after each successful pipeline run
- Captures: R version, platform, loaded package versions, full configuration snapshot, analyte definitions, IS configuration, input file list, output file list
- Overwrites on rerun (latest run is authoritative; version control tracks history)
- Location: alongside the experimental data (e.g., `Lab10-1/run_metadata.yaml`)

**Input validation** (`GlobalCode.R` — `validate_setup()` function):
- Runs automatically before pipeline execution
- Halts execution (`stop()`) on invalid configuration:
  - DataFolder must exist
  - SNR thresholds must be logically ordered (snr_detect < snr_quant, both positive)
  - QC bias limit must be 0-100%
  - Analyte m/z and RT must be positive numeric
  - Noise windows must be correctly computed (start < end)
  - IS configuration must be complete
  - Signal thresholds must be positive
  - Drift correction QC level must be positive (if specified)
  - Dilution factor must be 0 < D ≤ 1

**Division-by-zero protection** (`Code/03_Quantification.R`):
- IS ratio computation checks for NA, zero, or negative IS peak area before division
- Affected rows set to NA with a warning message indicating how many injections were affected
- Prevents Inf/NaN from propagating through calibration and quantification

### Not Required for This Context

**Unit tests (testthat)**: This is research code for method development, not operational forensic software. Scripts are version-controlled and manually validated against QC performance criteria (±20% bias, calibration back-calculation within 15%). Formal unit testing would be necessary if this code transitioned to routine casework processing.

**Comprehensive error handling (tryCatch)**: Scripts are run iteratively during method development on single datasets. No long unattended batch processing occurs. If an error occurs, the user identifies and fixes it manually. This is acceptable for a research workflow where the analyst is always present.

### Remaining Known Issues (Accepted for Research Context)

- Global state dependency (~14+ global variables) — acceptable for single-user research workflow
- Hardcoded absolute paths — DataFolder changed manually per run; acceptable when only one analyst uses the code
- No dependency management (renv) — R environment is stable on the single analysis workstation

### Future Consideration

If this code transitions to operational forensic use (routine casework), the following upgrades would be required:
- Full unit test coverage (testthat)
- Comprehensive tryCatch error handling with recovery
- Audit trail logging (user identity, action timestamps)
- Input/output file hash verification (digest package)
- Parameterised paths (no hardcoded absolute paths)
- Package version locking (renv)

## Report Progress (TMC4.txt)

**Location**: `C:\Users\A Bruce - User\Documents\ClaudeSpace\Reports\TMC4.txt`

**Current status**: Sections written through 2.3.2.5 (Drift Correction). Standard preparation section (2.3.1) is incomplete.

**Sections completed**:
- International Swabbing Study introduction (operator variability, ENFSI/FINEX context)
- Study participation (37 labs, 25 countries, 60 practitioners, 1158 samples)
- Experimental Design (explosive selection, surface selection, SFE discussion, adhesion literature)
- Methodology (surface prep, swab prep, deposition, swabbing, extraction)
- Method Development:
  - Initial GC-MS method description (Agilent 5977C, RTX-200MS, SIM/Scan, weighted quadratic calibration)
  - Sample Filtration (0.45 µm PTFE, reduced RDX degradation, PETN still problematic)
  - Internal Standards (15N-RDX for RDX works well; NG for PETN is suboptimal)
  - Analyte Protectants (xylitol/sorbitol tested; increase PA but don't prevent progressive loss)
  - Drift Correction (QC-based, anchor normalisation, applied to PETN PA only)

**Key report review items flagged** (from detailed review):
- Figure numbering inconsistencies throughout
- Line 154: "slower decline" for RDX contradicts data (55% vs 50% — RDX drops more in initial unfiltered test)
- Line 156: Factor of ~10/~2 reduction claims need verification against actual data
- Line 165: 34% ratio drop vs 18% in CONTEXT.md — different datasets, needs clarification
- Line 172: Figure reference error ("Figure 1" should be "Figure 7")
- Line 170: Grammar issue ("thereby and improving" → "thereby improving")
- Missing: final optimised method summary, results section, LOD/LOQ information

## Changes Made (May 2026 Session)

### ARCHIVED: v2-NG Pipeline Features

The following sections (originally numbered 1-8) document features from the v2-NG pipeline (`Archive/v2-NG_Deprecated/`). These are **no longer applicable** to the current consolidated pipeline in `Code/`.

**Sections preserved for historical reference**:
- Section 1: MsFilesReorganiser skip logic (retained in `Code/01_MsFilesReorganiser.R`)
- Section 2: Test.R reprocess toggles (retained as `reprocess_tic`/`reprocess_sim` in `Code/02_PeakDetection.R`)
- Section 2b: Shared Y-axis scaling for integration plots (retained in `Code/02_PeakDetection.R`)
- Section 3: PETN quadratic calibration (simplified in `Code/03_Quantification.R`, no linear fallback)
- Section 3b: Per-analyte calibration method selection (REMOVED - now single method per analyte)
- Section 5: Out-of-range quantification with flagging (retained, simplified)
- Section 6: PETN QC-based drift correction (retained in `Code/03_Quantification.R`)
- Section 7: Separate TIC/SIM reprocess toggles (retained)
- Section 8: MetadataLinearWeighted_15NRDX.R variant (ARCHIVED - consolidated into single script)

### Current Pipeline Simplifications (May 2026)

**Configuration Changes** (`GlobalCode.R`):
```r
use_15nrdx_for_petn <- FALSE  # TRUE: ratio calibration, FALSE: PA only
use_is_for_rdx <- FALSE       # TRUE: ratio calibration, FALSE: PA only
apply_petn_drift_correction <- TRUE  # QC-based drift correction for PETN
apply_rdx_drift_correction <- TRUE   # QC-based drift correction for RDX (PA-only mode)
drift_correction_qc_level <- NA     # NA: auto-select highest QC, or specify level (e.g., 6)
```

**Quantification Method Selection**:
- Each analyte uses ONE method (user-selected via configuration flags)
- No dual-method columns (`_pa`, `_ratio` suffixes removed from output)
- Ratio column still computed for diagnostic purposes regardless of flag

**Output Column Simplification**:
- `petn_concentration` (unified, not `petn_concentration_pa` or `_ratio`)
- `rdx_concentration` (unified)
- Drift correction columns: `petn_concentration_dc`, `rdx_concentration_dc`
- Range flags retained: `petn_range_flag`, `rdx_range_flag`

**NG (Nitroglycerin) Removed**:
- PETN IS columns (`petn_is_pa`, `petn_is_rt`) no longer generated
- Pipeline uses only 15N-RDX (m/z 122) as IS for ratio calculations
- When `use_15nrdx_for_petn = FALSE`, PETN uses PA-only quantification with drift correction

### 7. Test.R: Separate TIC/SIM Reprocess Toggles (v2-NG)

**File**: `v2-NG/Code/Test.R`

The single `reprocess_all` flag has been split into two independent controls:

```r
reprocess_tic <- TRUE
reprocess_sim <- TRUE
```

- `reprocess_tic = FALSE` -- skips TIC files where `{name}TIC.csv` already exists in GcData.dir
- `reprocess_sim = FALSE` -- skips SIM files where `{name}SIM.csv` already exists in GcData.dir
- Summary merge is automatically triggered if either was reprocessed: `reprocess_summary <- reprocess_tic || reprocess_sim`

**Use case**: When only RT/peak detection parameters change (SIM processing), set `reprocess_tic <- FALSE` to skip the unchanged TIC baseline correction (~50% time saving on large sequences).

### 8. MetadataLinearWeighted_15NRDX.R: PETN Ratio via 15N-RDX IS (v2-NG, new file)

**File**: `v2-NG/Code/MetadataLinearWeighted_15NRDX.R` (~830 lines)

A variant of `MetadataLinearWeighted.R` that uses **15N-RDX (m/z 122) as the internal standard for PETN** instead of NG (m/z 76). This addresses the fundamental limitation that NG is unreliable in V9 method sequences (often not detected at all) and even when detected does not adequately track PETN drift (~18% residual after IS correction).

**Key differences from original MetadataLinearWeighted.R**:

| Feature | Original | 15NRDX variant |
|---------|----------|----------------|
| PETN IS mapping | `petn_is_pa` (NG, m/z 76) | `rdx_is_pa` (15N-RDX, m/z 122) |
| PETN ratio formula | `petn_pa / petn_is_pa` | `petn_pa / rdx_is_pa` |
| PETN cal_methods | `c("pa")` only | `c("pa", "ratio")` |
| Plot labels | "PA / PETN IS Ratio" | "PA / 15N-RDX IS Ratio" |
| Ratio model storage | Not stored | `stored_petn_ratio_models` keyed by cal set |
| Ratio drift correction | Not present | Full QC-based drift correction on ratio |
| QC Accuracy PETN facets | 2 (PA, PA+Drift) | 4 (PA, Ratio, PA+Drift, Ratio+Drift) |
| Diagnostic plots | `PETN_DriftCorrection.png` | `PETN_PA_DriftCorrection.png` + `PETN_Ratio_DriftCorrection.png` |

**Rationale for 15N-RDX as PETN IS**: Analysis of Lab22-1 data showed that 15N-RDX tracks ~80% of PETN's progressive signal decline across a sequence. At the 6ng QC level:
- Raw PETN PA %RSD: **15.3%**
- PETN/15N-RDX ratio %RSD: **3.0%** (81% reduction in variability)
- QC |%bias| reduced from ~15.6% (raw PA) to ~4.7% (ratio with linear calibration)

15N-RDX does not perfectly track PETN drift (~6% residual decline in ratio over a full sequence at 6ng), because the two compounds elute at different RTs and have different adsorption kinetics. The combined ratio + drift correction approach addresses this residual.

**PETN ratio drift correction**: Mirrors the existing PA drift correction (same QC-based approach, anchor normalisation, quadratic/linear model fitting) but operates on `petn_ratio` values. New output columns:

| Column | Description |
|--------|-------------|
| `petn_ratio_drift_correction_factor` | Ratio multiplicative correction factor |
| `petn_ratio_drift_model` | "Quadratic", "Linear", "None", or "Disabled" |
| `petn_ratio_dc` | Drift-corrected PETN/15N-RDX ratio |
| `petn_concentration_ratio_dc` | Concentration from drift-corrected ratio calibration |
| `petn_quant_method_ratio_dc` | Quantification method for drift-corrected ratio |
| `petn_range_flag_ratio_dc` | Range flag for drift-corrected ratio quantification |
| `petn_sample_conc_ratio_dc` | Drift-corrected sample concentration |
| `petn_mass_in_sample_ratio_dc` | Drift-corrected mass in sample (ng) |
| `petn_percent_recovery_ratio_dc` | Drift-corrected % recovery |
| `petn_percent_bias_ratio_dc` | %Bias of drift-corrected ratio vs true |
| `petn_qc_flag_ratio_dc` | PASS/FAIL for drift-corrected ratio bias |

**QC Accuracy plot for PETN** now shows 4 facets:
1. **Peak Area** -- raw PA (shows full drift)
2. **PA / 15N-RDX IS Ratio** -- IS-normalised (removes ~80% of drift)
3. **Peak Area (Drift-Corrected)** -- PA + QC model correction
4. **Ratio (Drift-Corrected)** -- Ratio + QC model correction (expected best)

**How to use**: ~~Run `Code/MetadataLinearWeighted_15NRDX.R` instead of `Code/MetadataLinearWeighted.R` after GlobalCode.R and Test.R. Both scripts are standalone alternatives (not meant to be run together).~~

**ARCHIVED**: This variant is no longer needed. The consolidated `Code/03_Quantification.R` handles both PA and ratio methods via configuration flags.

**GlobalCode.R change**: ~~PETN `cal_methods` updated from `c("pa")` to `c("pa", "ratio")` to enable ratio calibration.~~

**Current approach**: Use `use_15nrdx_for_petn <- TRUE/FALSE` flag to select ratio vs PA quantification.

## V9 Method RT Investigation (20260429 Robustness)

### Context

The 20260429 Robustness experiment used a new GC method: `PETN RDX 15NRDX LI HES METHOD G ACB - 100 V9.M`. The pipeline (configured for V8 RTs) initially failed to detect any analyte peaks because retention times shifted dramatically.

### V9 vs V8 Retention Times (from raw chromatogram analysis)

| Compound | m/z | V8 Expected RT | V9 Actual RT | Shift |
|----------|-----|----------------|--------------|-------|
| PETN | 46 | 269.7 s | ~348 s | +78 s |
| NG (PETN IS) | 76 | 273.1 s | ~348 s | +75 s |
| RDX | 120 | 317.5 s | ~409 s | +91 s |
| 15N-RDX IS | 122 | 317.5 s | ~408 s | +91 s |

**Note**: The V9 RTs vary between sequences. In Lab22-1 (also V9), the observed RTs were:
- PETN: ~321 s
- RDX/15N-RDX: ~347.5 s

This suggests the V9 method has been modified between the robustness run (29 Apr) and Lab22-1 (30 Apr), or column/conditions differ.

### 20260429 Robustness Dataset Details

- **78 injections**: 20 Cal replicates (all 0.2ng, vials 1-10), 20 Samples (vial 40), 38 Blanks (vial 41)
- **Single concentration level** (0.2ng) -- cannot build calibration curve
- **No QC injections**
- **Purpose**: Assess signal repeatability at a single low level
- **Method**: V9-100 (first injection used V5-200MSD, rest V9-100)

### Negative Peak Area Issue

After updating RTs for V9, RDX showed negative peak areas in some injections. Root cause: the rolling-ball baseline correction (`wm=120, ws=40`) can overshoot the true signal baseline in low-signal regions. The `Subtracted = Signal - BaselineTrend` computation has no floor; when baseline > signal, the subtracted values become negative. The trapezoidal integration (`trapz_area()`) faithfully integrates negative values, and the fixed-width integration window (`peak_rt +/- 15*step_size`) can extend into negative-valued flanking regions that outweigh the small positive peak.

**Locations where a floor/clamp is absent**:
- `apply_baseline_correction()` line 128: no `max(0, ...)` on Subtracted
- `trapz_area()`: integrates negative intensities
- `extract_peak_area()`: no `max(0, pa)` after integration

## Lab22-1 Dataset (30 April 2026)

### Overview

- **Path**: `.../FINEX Swabbing Study/Lab22-1/`
- **Method**: V9 (`PETN RDX 15NRDX LI HES METHOD G ACB - 100 V9.M`)
- **52 injections**: 26 blanks (bracketing), 5 Cal levels (0.2, 2, 4, 8, 10ng), 6 QCs (0.2ng + 6ng), 14 samples
- **Samples**: Lab 22 P1 -- ABS-S (NC + 1-6), S (NC + 1-6)
- **Status**: Fully processed with MetadataLinearWeighted.R

### Observed RTs (V9 for this sequence)

- PETN: ~321 s
- RDX / 15N-RDX IS: ~347.5 s

### Key Findings

- **PETN IS (NG)**: All NA -- not detected in any injection
- **RDX IS (15N-RDX)**: Detected in all Cal/QC/Sample injections, PA 53,000-93,000
- **PETN drift correction**: Applied (quadratic model, factor 0.94 -> 1.35)
- **RDX QC performance**: PA method fails mid/end-run QCs; ratio method consistently passes
- **Samples**: PETN 0.3-1.1 ng, RDX 0.001-0.4 ng; S surface > ABS-S

### 15N-RDX as PETN IS -- Effectiveness Analysis (Lab22-1 data)

Signal decay across the sequence at 6ng level:

| Metric | Start (line 9) | End (line 51) | Total decline |
|--------|---------------|---------------|---------------|
| petn_pa | 534,717 | 394,661 | -26.2% |
| rdx_is_pa | 75,486 | 59,137 | -21.7% |
| petn/rdx_is ratio | 7.084 | 6.674 | -5.8% |

**Stability comparison (6ng QCs, %RSD)**:
- Raw petn_pa: **15.3%**
- petn/rdx_is ratio: **3.0%**
- Drift-corrected PA: ~0.6% bias (model-based)

**QC accuracy at 6ng (|%bias|)**:
- Raw PA: 11-20%
- Ratio (linear calib): 4.6-7.4%
- Drift-corrected PA: ~0.6%

**Conclusion**: 15N-RDX tracks ~80% of PETN drift. Ratio reduces variability by 81%. Combined ratio + drift correction provides the best results for both precision and accuracy.

---

## Session Summary (May 28, 2026)

### Major Changes This Session

1. **Project Restructuring**
   - Consolidated pipeline into single `Code/` folder (5 scripts)
   - Archived v1-NoNG and v2-NG variants to `Archive/`
   - Cleaned up legacy Code/ variants

2. **Pipeline Simplification**
   - Single IS system (15N-RDX at m/z 122 only)
   - NG (m/z 76) removed entirely
   - Single quantification method per analyte (configurable)
   - Simplified output columns (no `_pa`/`_ratio` suffixes)

3. **Alternative IS Investigation**
   - Comprehensive TIC ion scan conducted (TIC_AllIons_Scan.R)
   - Tested m/z 52 and m/z 53 as alternative IS ions
   - **Finding**: No viable alternative IS candidates found
   - All ions at RDX RT are concentration-dependent RDX fragments
   - m/z 122 shows strong correlation with RDX concentration (contamination)

4. **Current Direction**
   - Experimenting with PA-only quantification (no IS)
   - QC-based drift correction for PETN retained
   - **RDX drift correction added** for PA-only mode
   - Configuration flags `use_15nrdx_for_petn` and `use_is_for_rdx` control method selection

### Files Modified

| File | Changes |
|------|---------|
| `GlobalCode.R` | Simplified config, removed NG references, added extraction/filtration efficiency params, added `apply_rdx_drift_correction` flag |
| `Code/01_MsFilesReorganiser.R` | Retained (skip logic for processed files) |
| `Code/02_PeakDetection.R` | Consolidated from Test.R, single IS system |
| `Code/03_Quantification.R` | Consolidated from MetadataLinearWeighted.R, simplified calibration, added RDX drift correction |
| `Code/04_CollateStudyResults.R` | Retained for study-level collation |
| `Code/ModPeaks.R` | Signal processing functions (unchanged) |

### New Diagnostic Scripts

| Script | Purpose |
|--------|---------|
| `TIC_AllIons_Scan.R` | Comprehensive ion scan for IS candidates |
| `mz52_Diagnostic.R` | m/z 52 alternative IS analysis |
| `mz52_Quantification_Test.R` | Quantification test m/z 52 vs 122 |
| `mz53_Quantification_Test.R` | Quantification test m/z 53 vs 122 |

### Outstanding Tasks

- Evaluate PA-only quantification accuracy
- Consider mathematical correction for m/z 122 contamination if ratio needed
- Complete thesis chapter (TMC4.txt) method development section
- Investigate LOD/LOQ determination for final method

---

## Session Summary (June 2026)

### Major Changes: Statistical Analysis Implementation

**1. 04_CollateStudyResults.R Updates**

| Change | Description |
|--------|-------------|
| Removed hardcoded efficiency defaults | Lines 43-47 deleted, values now sourced from GlobalCode.R |
| Added RDX drift-corrected mass/recovery | Future-proofing for when RDX drift correction is enabled |
| Updated `desired_cols` | Added RDX drift correction columns |
| Modified `compute_stats` | Summary statistics now prioritize drift-corrected values |
| Added box plot generation | ~200 lines added for visualization |

**Box plot outputs:**
- Overall study plots (surfaces ordered by median recovery)
- Per-lab detailed plots (Participant × Surface combinations)
- Per-lab surface summary plots
- PNG format at 300 DPI
- Box = IQR, median line, mean diamond (red), outliers

**2. 05_StatisticalAnalysis.R Implementation**

Created statistical analysis script (~1130 lines) with:

**Analysis modes:**
- Single-lab mode: Fixed effects ANOVA
- Multi-lab mode: Mixed-effects models with variance components
- Automatic detection and fallback logic

**Statistical outputs:**
- Descriptive statistics (always available)
- ANOVA tables with F-tests
- Partial η² effect sizes
- Tukey HSD pairwise comparisons (via emmeans)
- Variance components and ICC (multi-lab only)
- Diagnostic plots (QQ, residuals, boxplots)

**Technical issues resolved:**
- Column naming in `pairs()` output: Used safe `colnames()` with `[[]]` extraction
- Variable scoping in `tryCatch()`: Used super-assignment (`<<-`)
- Post-hoc comparisons: Added to single-participant ANOVA block
- Surface count check: Warns if < 2 surfaces (cannot compute pairwise)

**3. Interpretation Guidance Provided**

Documented how to interpret:
- Descriptive_Statistics sheet (n, Mean, SD, RSD, CI)
- Statistical_Summary sheet (F-tests, effect sizes, variance components)
- Pairwise_Comparisons sheet (Tukey-adjusted contrasts)
- Diagnostic plots (normality, homoscedasticity)
- Single-lab limitations (cannot estimate between-lab variance)

**Reporting template provided:**
```
Recovery differed significantly by surface type (F = XX, p = XXX). 
Glass surfaces yielded the highest recovery (mean = XX%, RSD = XX%), 
followed by Steel (XX%, RSD = XX%)...
```

### Files Modified This Session

| File | Changes |
|------|---------|
| `Code/04_CollateStudyResults.R` | Removed hardcoded efficiencies, added RDX drift columns, added box plot generation (+200 lines) |
| `Code/05_StatisticalAnalysis.R` | New file created (1130 lines) |
| `GlobalCode.R` | Already had extraction/filtration efficiency params |
| `CONTEXT.md` | Updated documentation for all changes |

### Key Decisions Made

1. **Y-axis variable**: Percent recovery (not mass)
2. **Data source**: Drift-corrected when available, falls back to uncorrected
3. **Effect size**: Partial η² for ANOVA/mixed models
4. **Significance threshold**: α = 0.05
5. **Transformation**: Optional logit (disabled by default)
6. **Outlier removal**: Optional MAD-based (disabled by default)
7. **Surface ordering**: Fixed for per-lab, by median for overall
8. **Single-lab handling**: Graceful degradation with clear warnings

### Implementation Challenges Solved

| Issue | Problem | Solution |
|-------|---------|----------|
| Hardcoded efficiencies | Values didn't match GlobalCode.R | Deleted lines, use GlobalCode.R values |
| Missing RDX drift columns | future-proofing | Added to desired_cols |
| Summary uncorrected only | Didn't reflect drift correction | Modified compute_stats to prioritize _dc columns |
| `pairs_table` NULL after tryCatch | Local variable scope | Changed to `pairs_result <- tryCatch()` then `pairs_table <<- pairs_result` |
| Column rename error | `mutate(p_adjusted = p.value)` failed | Used `colnames()` with `[[]]` extraction |
| Single-participant missing post-hoc | Code only in multi-participant block | Added post-hoc to single-participant ANOVA |
| emmeans warning about p.value | Column naming inconsistency | Safe column existence checks before assignment |

### Testing Status

- Script runs successfully on Lab10-1 data
- Produces expected outputs:
  - Descriptive_Statistics sheet ✓
  - Statistical_Summary sheet ✓
  - Pairwise_Comparisons sheet ✓
  - Diagnostic plots ✓
  - Box plots ✓

### Remaining Work

- Test on multi-lab dataset when available
- Validate interpretation with domain experts
- Consider adding to thesis methodology section
- Test RDX drift correction on real datasets with PA-only mode

---

## Session Summary (June 10, 2026)

### Bug Fix: QC and Calibration Quantitation Returning Zero

**Problem**: All QC (and calibration standard) concentrations were reported as `0` in the results CSV, causing -100% bias and universal QC FAIL flags. Observed when processing Lab10-1 data.

**Root cause**: The `solve_concentration()` function in `Code/03_Quantification.R` was modified on June 10 to handle negative controls (report `0` instead of `NA` for below-range samples). The change replaced:

```r
# OLD: Filter to positive roots only
roots <- roots[is.finite(roots) & roots >= 0]
if (length(roots) == 0) return(list(value = NA_real_, ...))
list(value = min(roots), ...)
```

With:

```r
# NEW (broken): Pick minimum root, floor at zero
roots <- roots[is.finite(roots)]
concentration <- max(0, min(roots))
```

The bug: A quadratic equation has two roots — one physically meaningful (positive concentration) and one spurious (negative). `min(roots)` always selects the **negative** spurious root, which then gets floored to `0` by `max(0, ...)`. This affected ALL samples, not just below-range ones.

**Fix applied** (line 109-116):

```r
# Select smallest positive root (physically meaningful concentration)
# If no positive root exists, floor at zero (below-range extrapolation)
positive_roots <- roots[roots >= 0]
if (length(positive_roots) > 0) {
  concentration <- min(positive_roots)
} else {
  concentration <- 0
}
```

This preserves the June 10 intent (report `0` for negative controls instead of `NA`) while correctly selecting the positive root when one exists.

**Files modified**: `Code/03_Quantification.R`, lines 109-116

---

### Bug Fix: QC Rows Excluded from Calibration Set Quantification

**Problem**: QC rows that fall between calibration standards and samples were not included in the `rows` vector used for quantification. In Lab10-1, calibration standards are rows 4-9 and samples start at row 15, but QCs at rows 11 and 13 were missed by the selection logic.

**Root cause**: The row selection logic only captured:
- `rows_std`: Calibration standard rows
- `rows_smp`: Rows after the last calibration standard

QC rows between the calibration block and the sample block were not captured by either condition.

**Fix applied** (lines 362-377):

```r
rows_std <- Combined$Row %in% cal$Row
rows_qc_smp <- Combined$Row > max(cal$Row)
if (i < length(CalSets)) {
  rows_qc_smp <- rows_qc_smp & Combined$Row < min(CalSets[[i + 1]]$Row)
}
# Explicitly include QC rows that belong to this calibration set
if (i == length(CalSets)) {
  # Last cal set: include all QCs after this cal
  rows_qc <- Combined$Type == "QC" & Combined$Row > max(cal$Row)
} else {
  # Not last cal set: include QCs after this cal but before next cal
  rows_qc <- Combined$Type == "QC" & 
             Combined$Row > max(cal$Row) & 
             Combined$Row < min(CalSets[[i + 1]]$Row)
}
rows <- which(rows_std | rows_qc_smp | rows_qc)
```

Uses `if/else` to avoid subscript-out-of-bounds when accessing `CalSets[[i + 1]]` on the last calibration set.

**Files modified**: `Code/03_Quantification.R`, lines 356-377

---

### Bug Fix: `resp_col` Variable Scoping in Drift-Corrected Calibration Plot

**Problem**: The PETN drift-corrected calibration plot section (line 920) used `resp_col`, which was last set inside the `for (analyte_name in names(analytes))` loop. After the loop completes, `resp_col` holds the value from the last analyte processed (RDX), causing the PETN plot to filter on `rdx_ratio` instead of `petn_ratio`.

**Fix applied** (line 914): Added explicit `petn_resp_col` variable:

```r
petn_resp_col <- ifelse(use_15nrdx_for_petn, "petn_ratio", "petn_pa")
```

Used `petn_resp_col` instead of `resp_col` in the calibration plot data filtering (line 921).

**Files modified**: `Code/03_Quantification.R`, lines 914, 921

---

### Improvement: Removed Unused `FiguresOutput` Folder Creation

**Problem**: `GlobalCode.R` defined and created `MetadataOutput.dir` (`{DataFolder}/Results/FiguresOutput/`) on every pipeline run, but no active pipeline script references this variable. It was only used by the archived `Archive/Legacy_Code/Metadata.R`.

**Fix applied**: Removed lines 90-92 from `GlobalCode.R`:

```r
# REMOVED:
# This is for the code for processing the data
MetadataOutput.dir <- paste0(DataFolder,"/Results/FiguresOutput/")
dir.create(file.path(MetadataOutput.dir),recursive = TRUE)
```

**Files modified**: `GlobalCode.R`, lines 90-92 removed

---

### Improvement: Results File Backup Before Overwrite

**Problem**: Reprocessing a batch (e.g. Lab17-1) silently overwrites the previous `Lab17-1_GCMSResults.csv` with no backup. If the reprocessing introduces an error or uses incorrect parameters, the previous results are lost.

**Fix applied** (lines 1475-1483 in `Code/03_Quantification.R`):

```r
# Backup existing results before overwriting
if (file.exists(outfile)) {
  backup_name <- paste0(
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    "_", ParentFolder, "_GCMSResults.csv"
  )
  file.copy(outfile, file.path(Backup.dir, backup_name))
  message("  Previous results backed up to: ", file.path(Backup.dir, backup_name))
}
```

**Backup location**: `{DataFolder}/Results/Backup/` (e.g. `Lab17-1/Results/Backup/2026-06-10_19-30-45_Lab17-1_GCMSResults.csv`)

**Files modified**: `Code/03_Quantification.R`, lines 1475-1483

---

### Improvement: QC Monitoring File Backup Location Fixed

**Problem**: The QC monitoring file backup was written to `dirname(qc_monitoring_file)` (i.e. `.../System Monitoring/`), cluttering the parent directory with timestamped backup files alongside the active monitoring CSV.

**Fix applied** (lines 1535-1544 in `Code/03_Quantification.R`):

```r
qc_backup_dir <- file.path(dirname(qc_monitoring_file), "Backup")
dir.create(qc_backup_dir, recursive = TRUE, showWarnings = FALSE)
qc_backup_name <- paste0(
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
  "_SystemMonitoring_backup.csv"
)
write.csv(ExistingQC,
          file = file.path(qc_backup_dir, qc_backup_name),
          row.names = FALSE)
message("  QC monitoring file backed up to: ", file.path(qc_backup_dir, qc_backup_name))
```

**Backup location**: `.../System Monitoring/Backup/` (central, alongside the monitoring file, in a dedicated subfolder)

**Files modified**: `Code/03_Quantification.R`, lines 1535-1544

---

### Files Modified This Session

| File | Changes |
|------|---------|
| `Code/03_Quantification.R` | Fixed `solve_concentration()` root selection; added QC rows to calibration set; fixed `resp_col` scoping; added results backup; fixed QC monitoring backup location |
| `GlobalCode.R` | Removed unused `MetadataOutput.dir` / `FiguresOutput` folder creation |

### Backup Strategy (Current)

| What | When | Location | Filename Format |
|------|------|----------|-----------------|
| Batch results CSV | Before each reprocessing overwrite | `{DataFolder}/Results/Backup/` | `YYYY-MM-DD_HH-MM-SS_{ParentFolder}_GCMSResults.csv` |
| QC monitoring CSV | Before appending new QC data | `.../System Monitoring/Backup/` | `YYYY-MM-DD_HH-MM-SS_SystemMonitoring_backup.csv` |

---

### Feature: XLSX Export with Separate Sheets per Sample Type

**File**: `Code/03_Quantification.R`, lines 1487-1542

Added XLSX workbook export alongside the existing CSV. Written to `{Results.dir}/{ParentFolder}_GCMSResults.xlsx` using `writexl::write_xlsx()` (already loaded in GlobalCode.R).

**Sheets**:

| Sheet | Filter | Column Focus |
|-------|--------|--------------|
| Calibration | `Type == "Cal"` | Identification, raw measurements, IS, concentrations, back-calculated %bias vs TrueCalConcAdj |
| QC | `Type == "QC"` | Identification, raw measurements, IS, concentrations, drift-corrected concentrations, %bias (uncorrected + DC), PASS/FAIL flags |
| Samples | `Type == "Sample"` | Identification, raw measurements, IS, concentrations, drift-corrected concentrations, range flags, quant methods |
| Blanks | `Type == "Blank"` | Identification, raw signal (RT, PA, PH, SNR, SNR flag) — no concentrations or ratios |

**Design decisions**:
- Uses `any_of()` for column selection — missing columns (e.g. when drift correction disabled) are silently skipped
- Empty sheets are removed before writing (e.g. sequences without blanks)
- No formatting/styling — raw data export matching the CSV content
- Column ordering follows logical grouping: Identification → IS → PETN raw → RDX raw → Quantitation → Drift correction → Accuracy