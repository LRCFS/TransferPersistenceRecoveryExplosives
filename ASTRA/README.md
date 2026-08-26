# ASTRA Pressure Trace Analysis - Documentation

This is the single consolidated documentation file for the ASTRA pressure
trace analysis system. It replaces the previous set of separate files
(`README.md`, `SCRIPT_REFERENCE_GUIDE.md`, `DISTANCE_BASED_APPROACH.md`,
`DOCUMENTATION_INDEX.md`, `CHANGELOG.md`), which have been merged into the
sections below.

**Current script:** `time_based_batch_process_adaptive_DISTANCE.R`
**Script version:** v1.3.0-CONSOLIDATED
**Last updated:** 2026-07-30

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Script Reference Guide](#script-reference-guide)
   - [Overview](#overview)
   - [System Architecture](#system-architecture)
   - [G-Code Motion Parameters](#g-code-motion-parameters)
   - [Analysis Approaches](#analysis-approaches)
   - [Key Functions](#key-functions)
   - [Configuration Parameters](#configuration-parameters)
   - [Data Flow](#data-flow)
   - [Output Files](#output-files)
   - [Validation & Troubleshooting](#validation--troubleshooting)
   - [Usage Examples](#usage-examples)
   - [Interpreting Results](#interpreting-results)
   - [Key Insights](#key-insights)
   - [Script Modifications Guide](#script-modifications-guide)
   - [Technical Notes](#technical-notes)
3. [Distance-Based Approach (Detailed)](#distance-based-approach-detailed)
   - [G-Code Analysis: Understanding the Swabbing Motion](#g-code-analysis-understanding-the-swabbing-motion)
   - [Distance Calculation Methodology](#distance-calculation-methodology)
   - [Metrics Comparison: Time vs. Distance](#metrics-comparison-time-vs-distance)
   - [Distance Analysis Output Files](#distance-analysis-output-files)
   - [Plot Interpretation Guide](#plot-interpretation-guide)
   - [Validation Diagnostics](#validation-diagnostics)
   - [Distance Calculation Troubleshooting](#distance-calculation-troubleshooting)
   - [Technical Implementation Notes](#technical-implementation-notes)
   - [Summary: When to Use Time vs. Distance Metrics](#summary-when-to-use-time-vs-distance-metrics)
4. [Changelog](#changelog)

---

## Quick Start

### Running the analysis

**In R/RStudio:**
```r
source("C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/time_based_batch_process_adaptive_DISTANCE.R")
```

**From the command line:**
```bash
Rscript "C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\ASTRA\time_based_batch_process_adaptive_DISTANCE.R"
```

**Or:** double-click `RUN_BATCH_ANALYSIS.bat`.

The script is fully self-contained - it does not `source()` any other file,
and no other file should `source()` it.

### Run modes

Near the top of the script:

```r
RUN_MODE <- "batch"              # process every configured sample (default)
# RUN_MODE <- "single"           # quick test of one sample
SINGLE_TEST_SAMPLE <- "PILOT_001"
```

- `"batch"` processes every sample found in the metadata CSV, generates all
  per-sample plots/statistics, and produces the batch summary + comparison
  plots.
- `"single"` runs just one sample and prints a short pass/fail report
  (useful for testing parameter changes before committing to a full run).

### Required R packages

```r
install.packages(c("ggplot2", "dplyr", "scales", "writexl"))
```

### What the script does

1. Reads the pilot metadata CSV and every `PILOT_*.csv` pressure trace it finds.
2. For each sample, discovers a sample-specific adaptive stable-pressure
   range (falling back to pressure-level defaults if discovery fails).
3. Calculates cumulative swabbing distance using that stable range.
4. Runs time-based periodic cycle detection (predicts and locates each swab
   pass using strict periodicity).
5. Calculates time-based and distance-based statistics.
6. Generates per-sample plots (full trace by time, full trace by distance,
   zoomed distance view of passes 1-5, 25-second window plots), a detected
   cycles CSV, and a summary statistics text file.
7. Aggregates everything into `batch_summary.csv` / `.xlsx` plus
   cross-sample comparison plots.

### Output location

```
.../Pilot Study/Pressure Traces/ProcessedData/
  batch_summary.csv / batch_summary.xlsx
  Figures/                          (comparison plots)
  PILOT_*/                          (one folder per sample)
    PILOT_*_full_trace_time.png
    PILOT_*_full_trace_distance.png
    PILOT_*_zoomed_distance.png
    PILOT_*_detected_cycles.csv
    PILOT_*_summary_statistics.txt
    25s_windows/*.png
```

### Troubleshooting

See [Validation & Troubleshooting](#validation--troubleshooting) and
[Distance Calculation Troubleshooting](#distance-calculation-troubleshooting)
below for detailed guidance.

**PILOT_014 / PILOT_018 - recovered low-pressure runs.** These two pilot
samples (both 50g protocol) previously failed with "Could not find any
stable region after startup" because they never reached the nominal
45-65g stable band, nor the adaptive discovery band (42.5-55g). Direct
inspection of the raw traces showed a clean, highly periodic,
non-noisy signal that simply plateaus lower than target:

- `PILOT_014`: sustained plateau ~36-37g (~73% of the 50g target)
- `PILOT_018`: sustained plateau ~39-41g (~80% of the 50g target)

This is **not** sensor noise or a script bug - region durations/values
are highly repeatable cycle-to-cycle, and the same detection logic
already works correctly for 22 other samples. Both runs happen to have
been executed back-to-back (10 minutes apart, same day, same
`swab_mount` config), which is circumstantial and does not by itself
establish a cause - **no physical/mechanical cause has been
independently confirmed** (e.g. no logged calibration event or operator
observation of anything unusual during collection). Treat the low
pressure as an observed, unexplained characteristic of these two runs,
not a diagnosed root cause. Rather than leave this real data
unprocessed, `PILOT_014` and `PILOT_018` now have sample-specific
overrides in `SAMPLE_OVERRIDES` that set `stable_low`/`stable_high` to
match each sample's *actual* achieved pressure band (`[30,42]g` and
`[34,48]g` respectively), so the existing detection pipeline can
recover real cycle/distance/pressure statistics for them instead of
aborting.

`target` is intentionally left at 50 for both (the nominal/intended
protocol) - only the achieved stable band was overridden. As a result,
`batch_summary.csv`/`.xlsx` now shows `Success=TRUE` for both, but their
`Mean_Pressure` legitimately reads ~35g and ~41g - well below the 50g
target of every other 50g sample (44-58g). **Anyone computing
"at-target" aggregate statistics across the 50g samples should check
`Mean_Pressure` and exclude or separately flag `PILOT_014`/`PILOT_018`**,
since their swabbing pressure did not actually reach the intended
protocol level even though the pipeline now successfully processes them.

### Repository layout

- `time_based_batch_process_adaptive_DISTANCE.R` - the only active script.
- `archive/` - superseded algorithm versions and one-off per-sample
  debug/diagnose/test scripts, kept for historical reference only.
- `RUN_BATCH_ANALYSIS.bat` - double-click launcher for the batch run.
- `outputs/`, `test_outputs/` - local exploratory output (git-ignored).

### Data locations

- **Raw pressure traces:** `.../Pilot Study/Pressure Traces/PILOT_*.csv`
- **Metadata:** `.../Pilot Study/pilot_data_nested.csv`
- **G-code reference:** `.../3D Printer/Swabbing Patterns/Ratchet9x9-50g.gcode.txt`
  and `Ratchet9x9-200g.gcode.txt`
- **Processed output:** `.../Pressure Traces/ProcessedData/`

---

## Script Reference Guide

**Script:** `time_based_batch_process_adaptive_DISTANCE.R`

### Overview

#### Purpose
Automated analysis of pressure trace data from ASTRA swabbing experiments
using a modified 3D printer. The script processes pressure sensor data to
identify stable swabbing regions and calculate quality metrics.

#### Dual Analysis Approach
The script implements **TWO complementary analysis methods**:

1. **Time-Based Analysis** (original approach)
   - Analyzes pressure stability over elapsed time
   - Reports % of total experiment time at stable pressure
   - Useful for temporal efficiency and cycle timing

2. **Distance-Based Analysis** (newer approach)
   - Analyzes pressure stability over actual swabbing distance
   - Reports % of swabbing distance at stable pressure
   - Provides a direct quality metric for swabbing motion

Both analyses run in parallel and produce complementary insights.

### System Architecture

#### Physical Setup
```
Modified 3D Printer (G-code controlled)
    v
Automated Swabbing Head
    v
Pressure Sensor (100 Hz sampling)
    v
CSV Data Files
    v
R Analysis Script (this script)
    v
Plots + Statistics + CSV Summaries
```

#### Software Stack
- **R 4.4.2+**
- **Required packages:** `ggplot2`, `dplyr`, `scales`, `writexl`
- **Platform:** Windows (paths use forward slashes or double backslashes)

### G-Code Motion Parameters

#### Ratchet Swabbing Pattern
Based on `Ratchet9x9-50g.gcode.txt` and `Ratchet9x9-200g.gcode.txt`:

```
Cycle Structure (one complete swab pass):
1. Horizontal swab   -> 90mm at 83.33 mm/s (1.08s)
2. Lift vertically   -> 11mm or 22mm at 5 mm/s
3. Return in air     -> 90mm at 300 mm/s (0.3s)
4. Lower to contact  -> 11mm or 22mm at 5 mm/s
```

#### Protocol Comparison

| Parameter | 50g Protocol | 200g Protocol |
|-----------|--------------|---------------|
| **Target pressure** | 50g | 200g |
| **Swab distance** | 90mm/pass | 90mm/pass |
| **Z-lift height** | 11mm | 22mm |
| **Expected cycle time** | ~5.78s | ~10.18s |
| **Total passes** | 45 | 45 |
| **Total distance** | 4,050mm | 4,050mm |
| **Swab speed** | 83.33 mm/s | 83.33 mm/s |

#### Why These Parameters Matter

**For Time-Based Analysis:**
- Cycle period determines detection window sizes
- Lift/lower phases create zero-pressure gaps
- Only ~10-20% of cycle time is actual swabbing

**For Distance-Based Analysis:**
- Only horizontal swabbing motion accumulates distance
- Distance plateaus during lift/return phases
- Theoretical maximum: 4,050mm (45 x 90mm)

### Analysis Approaches

#### 1. Time-Based Periodic Detection

**Method:** Predict cycle timing based on periodicity

**Algorithm:**
```
1. Skip startup period (first 8 seconds)
2. Detect initial 5 cycles using threshold search
3. Calculate median period from intervals
4. Predict remaining cycles: next_time = last_time + period
5. Search within tolerance window (+-2s or +-2xSD)
6. Force placeholder if cycle not found (strict periodicity)
```

**Key Features:**
- Adaptive threshold discovery (analyzes first 60s)
- Robust period estimation (median, not mean)
- Strict periodicity enforcement
- Handles missing cycles gracefully

**Outputs:**
- Number of cycles detected
- % of time at stable pressure
- Cycle-by-cycle timing
- Period statistics (mean, SD, CV)

#### 2. Adaptive Threshold Discovery

**Method:** Automatically determine sample-specific stable pressure range

**Algorithm:**
```
1. Search first 60s for wide pressure range (85-110% of target)
2. Find all stable regions (minimum 0.3s duration)
3. Calculate mean and SD of discovered pressures
4. Set thresholds: mean +- tolerance
   - tolerance = 2 x SD
   - bounded below by adaptive_tolerance_200g (10g) or
     adaptive_tolerance_50g (8g), whichever matches this sample's
     pressure level
   - bounded above by 20g
5. Use these thresholds for cycle detection
```

> As of v1.1-CONSOLIDATED, the minimum tolerance bound is genuinely
> pressure-level specific (10g for 200g samples, 8g for 50g samples).
> Earlier versions defined these per-level values in `PARAMS` but the
> code silently used a hardcoded 5g minimum for every sample - see the
> [Changelog](#changelog) for details.

**Why Adaptive?**
- Samples vary in actual achieved pressure
- Manual thresholds may be too strict or loose
- Sample-specific ranges improve detection accuracy
- Handles both 50g and 200g protocols automatically

**Fallback (`PARAMS$adaptive_fallback_to_defaults`):**
- If discovery fails (< 3 regions found) or the threshold calculation
  fails, and `adaptive_fallback_to_defaults` is `TRUE` (default): use
  config defaults - 175-210g (200g) or 45-65g (50g).
- If `adaptive_fallback_to_defaults` is `FALSE`: the sample is marked as
  failed instead of silently using defaults.

#### 3. Distance-Based Analysis

**Method:** Calculate cumulative swabbing distance using stable pressure range

**Algorithm:**
```
For each data point:
1. Check if pressure is within adaptive stable range
2. If YES (stable contact):
   - Calculate time since last point
   - distance_delta = time_delta x 83.33 mm/s
   - cumulative_distance += distance_delta
3. If NO (lifted/transitioning):
   - distance_delta = 0 (no accumulation)
4. Store cumulative_distance for plotting
```

**Key Features:**
- Uses adaptive stable range (not simple threshold)
- Only accumulates during stable horizontal swabbing
- Excludes ramp-up, ramp-down, lift, and return phases
- Validates against theoretical maximum (4,050mm)

**Outputs:**
- Total swabbing distance (mm)
- % of distance at stable pressure
- Distance efficiency (actual vs. theoretical)
- Mean distance per cycle

### Key Functions

#### Configuration Functions

##### `build_sample_config(metadata_file, data_dir, pressure_configs, overrides)`
**Purpose:** Automatically configure all samples from metadata CSV

**Process:**
1. Read metadata to get pressure levels
2. Find available pressure trace CSV files
3. Apply pressure-level defaults (50g or 200g)
4. Apply sample-specific overrides if defined
5. Return configuration list for batch processing

**Returns:** Named list of sample configurations

#### Data Loading Functions

##### `clean_and_read_csv(filepath)`
**Purpose:** Robust CSV reading with malformed header handling

**Handles:**
- Split headers (timestamp on one line, rest on next)
- Malformed first rows
- Mixed data types
- Missing columns

**Returns:** Data frame with columns: `timestamp`, `elapsed_sec`, `analogValue`, `load`, `distance`

#### Distance Calculation Functions

##### `calculate_cumulative_distance(data, target_pressure, stable_range_low, stable_range_high, gcode_params, verbose)`
**Purpose:** Calculate cumulative swabbing distance during stable contact

**Algorithm:**
```r
for (i in 1:nrow(data)) {
  is_stable <- (load[i] >= stable_range_low &
                load[i] <= stable_range_high)

  if (i > 1 && is_stable) {
    time_delta <- elapsed_sec[i] - elapsed_sec[i-1]
    if (time_delta > 0 && time_delta < 1.0) {
      distance_delta <- time_delta * 83.33  # mm/s
      cumulative_distance <- cumulative_distance + distance_delta
    }
  }

  data$distance_mm[i] <- cumulative_distance
}
```

**Validation Diagnostics:**
- Contact points vs total points
- Implied swab speed vs expected (83.33 mm/s)
- Speed error percentage
- Estimated number of passes (guarded against divide-by-zero if no
  distance accumulated)
- Distance efficiency vs theoretical max

**Returns:** Data frame with added columns: `is_contact`, `distance_mm`

#### Threshold Discovery Functions

##### `discover_stable_pressure_range(data, config, params)`
**Purpose:** Analyze data to find actual stable pressure range

**Process:**
1. Use wide discovery range (85-110% of target)
2. Search first 60s (after 8s startup skip)
3. Find all stable regions (>=0.3s duration)
4. Calculate statistics: mean, median, SD, min, max
5. Return pressure statistics

**Returns:** List with `mean`, `sd`, `median`, `min`, `max`, `n_regions`

##### `calculate_adaptive_thresholds(pressure_stats, config, params)`
**Purpose:** Calculate adaptive thresholds from discovered pressures

**Logic:**
```r
tolerance <- 2 * sd

min_tolerance <- if (config$target >= 150) params$adaptive_tolerance_200g
                  else params$adaptive_tolerance_50g  # 10g or 8g
max_tolerance <- 20

tolerance <- max(min_tolerance, min(tolerance, max_tolerance))
adaptive_low  <- mean - tolerance
adaptive_high <- mean + tolerance
```

**Returns:** List with `low`, `high`, `tolerance_used`, `sd_based`

#### Cycle Detection Functions

##### `detect_stable_by_threshold(data, start_time, end_time, low_thresh, high_thresh, min_duration, return_all)`
**Purpose:** Find stable pressure regions within time window

**Process:**
1. Filter data to search window
2. Mark points within threshold range
3. Identify continuous stable segments
4. Calculate durations
5. Filter by minimum duration
6. Return longest (or all) regions

**Returns:** Data frame with columns:
- `start_time`, `end_time`, `duration`
- `mean_load`, `n_points`
- `start_distance_mm`, `end_distance_mm`, `distance_span_mm` (if distance data exists)

##### `detect_initial_cycles(data, n_cycles, params)`
**Purpose:** Detect first N cycles for period estimation

**Strategy:**
1. Skip startup period (first 8 seconds)
2. Search for first cycle in wide window
3. Skip cycles longer than 2s (startup artifacts)
4. Use expected period for subsequent searches
5. Refine period estimate after 3 cycles found

**Returns:** Data frame of detected cycles with `cycle_num` assigned

##### `predict_and_detect_remaining(data, initial_cycles, period_stats, params)`
**Purpose:** Detect remaining cycles using strict periodicity

**Strategy:**
```
while (predicted_start < max_time):
  predicted_start = last_start + median_period
  search_window = predicted_start +- tolerance

  region = detect_in_window(search_window)

  if (region found):
    add to results
    last_start = region$start_time
  else:
    force placeholder (duration = 0)
    last_start = predicted_start
```

**STRICT MODE:** Always predicts next cycle, forces placeholder if not found

**Returns:** Data frame of remaining cycles (some may have `duration = 0`)

#### Statistics Functions

##### `calculate_statistics(all_cycles, data, params, gcode_params)`
**Purpose:** Calculate comprehensive statistics for time AND distance

**Time-Based Metrics:**
```r
total_stable_time <- sum(real_cycles$duration)
total_pass_time <- sum(data$load > 0) * median(diff(elapsed_sec))
pct_stable_time <- (total_stable_time / total_pass_time) * 100  # guarded if total_pass_time == 0
```

**Distance-Based Metrics:**
```r
total_distance_mm <- max(data$distance_mm)
total_stable_distance_mm <- sum(real_cycles$distance_span_mm)
pct_stable_distance <- (total_stable_distance_mm / total_distance_mm) * 100  # guarded if 0

theoretical_max <- gcode_params$total_distance_theoretical_mm  # 4050 = 45 x 90mm
expected_from_cycles <- n_cycles_detected * gcode_params$swab_distance_mm
distance_efficiency_actual <- (total_distance / expected_from_cycles) * 100  # guarded if n_cycles_detected == 0
```

**Pressure Statistics:**
- Mean, median, SD, min, max (from stable regions only)
- RSD (relative standard deviation)

**Returns:** List with 23+ metrics (time, distance, pressure, cycles). As of
v1.1-CONSOLIDATED, `gcode_params` is passed explicitly rather than read
from the global `GCODE_PARAMS`, and several metrics are `NA` (with a
console warning) instead of `NaN`/`Inf` when their denominator would be
zero.

#### Plotting Functions

##### `create_full_trace_plot(data, all_cycles, params, sample_name)`
**Purpose:** Time-based full pressure trace plot

**Features:**
- Gray line: full trace
- Blue dashed lines: stable thresholds
- Green shaded: detected stable regions
- Saved as: `{sample}_full_trace_time.png`

##### `create_full_trace_plot_distance(data, all_cycles, params, sample_name, output_dir_override = NULL)`
**Purpose:** Distance-based full pressure trace plot

**Features:**
- X-axis: cumulative distance (mm)
- Pressure plateaus visible during distance accumulation
- Horizontal gaps indicate lift-off periods
- Saved as: `{sample}_full_trace_distance.png`
- `output_dir_override`: if set, saves there instead of `params$output_dir`
  (used to generate the `TransientComparison/` folders - see below)

##### `create_zoomed_plot_distance(data, all_cycles, params, sample_name, output_dir_override = NULL)`
**Purpose:** Zoomed view of first 5 passes (0-450mm)

**Features:**
- Gray vertical lines at 90mm intervals (pass boundaries)
- Labels: "Pass 1", "Pass 2", etc.
- Green shaded: stable regions within each pass
- Saved as: `{sample}_zoomed_distance.png`
- `output_dir_override`: same as above

##### `mask_noncontact_data(data)`
**Purpose:** Returns a copy of a trace data frame with the pressure
column (`load` or `pressure_g`, whichever is present) set to `NA`
wherever `is_contact == FALSE`, so `geom_line()`/base `plot(type = "l")`
break the line there instead of drawing a near-vertical segment down to
0g and back. Used to generate the `NoTransients/` half of
`TransientComparison/` (see [Output Files](#output-files)) - applied for
display only, never before an alignment/detection step that depends on
the true (unmasked) values.

##### `plot_full_traces_comparison(target_pressure, sample_results, metadata_df, data_dir, params, gcode_params, suppress_transients = FALSE, output_dir_override = NULL)`
**Purpose:** Compare multiple samples at the same pressure level as small multiples

**Features:**
- Distance-based (x-axis is cumulative swabbing distance, recomputed per
  sample from its discovered adaptive stable range - no time-alignment
  trick is needed since distance naturally starts at 0 for every sample)
- **One panel per sample** (`facet_wrap(~ sample_id)`), not a single
  overlaid panel. With ~10-13 samples, overlaying every trace on one
  panel produces an illegible "spaghetti" of overlapping sawtooth lines
  where individual samples can't be visually distinguished even with
  distinct colors - faceting makes each sample's trace independently
  legible.
- **Y-axis scale is shared (fixed) across all panels** within a figure -
  this is deliberate, not an oversight: it means a sample sitting at a
  different absolute pressure level than its peers (e.g. a 50g-target
  sample that only reached ~35g) is immediately visible by comparing
  panel heights, without reading any numbers. Panel count and grid
  layout (`comparison_ncol`) scale the output image dimensions
  automatically.
- Plots each sample's **complete** raw trace (same data
  `create_full_trace_plot_distance()` uses for a single sample) - this
  intentionally includes brief drops toward 0g at each pass boundary,
  since distance stays flat during lift-off/return and those points
  render as thin near-vertical excursions rather than a bug
- Each sample is re-zeroed to its own Cycle 1 **peak pressure** (the sharp
  ramp-up overshoot, found by searching pressure_g within a window around
  the detected Cycle 1) rather than the raw first row of its CSV, since
  each ASTRA run is started manually (both the data logger and the swab
  motion) - without this, pass peaks land at essentially random
  x-positions relative to each other even though the underlying cycles
  are the same nominal 90mm apart. The peak is used instead of Cycle 1's
  entry into the (narrower) adaptive stable band, since the overshoot
  typically exceeds that band and is a sharper, more consistently
  identifiable physical event (the swab head impact) to align on -
  empirically this produced visibly tighter peak clustering than aligning
  to band entry. Residual drift over many passes (from small differences
  in each sample's true period) and a few unusually noisy runs can still
  cause some spread within a "band" of peaks - this offset removes the
  dominant misalignment, not all of it.
- Windowed to the first `params$comparison_distance_limit_mm` (default
  1000mm, ~11 passes) of each trace - individual passes become hard to
  distinguish beyond this even in a single panel
- Color-coded by wet/dry condition only (`params$color_wet`/`color_dry`,
  two fixed colors) - since each sample already has its own panel, color
  no longer needs to identify individual samples. This replaces an
  earlier per-sample recycled 5-color-per-condition palette that could
  silently assign the *same* color to two different samples whenever a
  group had more than 5 wet or 5 dry samples (confirmed to happen with
  the 200g group's 7 dry / 6 wet samples) - a real legibility bug, not
  just a style issue, now structurally impossible with only 2 colors.
- Saved as: `{pressure}g_samples_full_traces.png`

##### `plot_batch_qc_overview(summary_data, params, outlier_pct)`
**Purpose:** Visual, batch-level counterpart to `Flag_15Pct_Outlier` -
see how every sample compares to its `Target` at a glance, without
reading `batch_summary.csv` as a table.

**Features:**
- Cleveland dot plot: one row per successful sample, x = `Mean_Pressure`.
- Faceted by `Target` (50g/200g get independent x-axis scales, since
  their absolute pressures aren't comparable); rows within each facet
  sorted by `Mean_Pressure`.
- Dashed vertical line at `Target`; shaded band showing
  `Target × (1 ± outlier_pct)` (the same threshold used for
  `Flag_15Pct_Outlier`), so it's visually obvious *why* a point falls
  outside it.
- Points colored by `Flag_15Pct_Outlier` (red = flagged, blue = within
  threshold) with a connecting segment back to the `Target` line.
- Saved as: `batch_qc_overview.png`.

##### `test_single_sample(sample_id, sample_config, params)`
**Purpose:** Quick single-sample smoke test (added in v1.1-CONSOLIDATED,
replacing the old standalone `test_distance_single_sample.R` /
`test_distance_PILOT_001.R` scripts)

**Process:**
1. Looks up `sample_id` in `sample_config` (errors clearly if not found)
2. Runs `process_sample()` for that one sample
3. Prints time-based metrics, distance-based metrics, pressure statistics
4. Checks that all expected output files were actually created

**Invoked via:** setting `RUN_MODE <- "single"` and `SINGLE_TEST_SAMPLE`
at the top of the script.

### Configuration Parameters

#### Global Parameters (PARAMS)

```r
PARAMS <- list(
  # File paths
  data_dir = ".../Pressure Traces",
  output_dir = ".../ProcessedData",

  # Detection parameters
  min_stable_duration = 0.3,        # Minimum stable region (s)
  max_normal_duration = 2.0,        # Max normal cycle (longer = artifact)
  skip_startup_sec = 8,             # Skip initial startup period

  # Period estimation
  n_cycles_for_period = 5,          # Cycles for period estimation
  period_tolerance_factor = 2,      # Tolerance = factor x SD
  min_period_tolerance = 2.0,       # Minimum search window (s)
  use_median_period = TRUE,         # Use median (robust)

  # Initial detection
  initial_search_window = 5,        # Search window size (s)
  max_time_for_initial = 120,       # Max time to search for initial cycles

  # Adaptive thresholds
  use_adaptive_thresholds = TRUE,   # Enable adaptive discovery
  adaptive_discovery_duration = 60, # Discovery window (s)
  adaptive_tolerance_200g = 10,     # Minimum tolerance for 200g (+-g) - now actually used
  adaptive_tolerance_50g = 8,       # Minimum tolerance for 50g (+-g) - now actually used
  adaptive_min_regions = 3,         # Min regions for reliable stats
  adaptive_fallback_to_defaults = TRUE  # Now actually respected
)
```

#### Pressure-Level Configurations

```r
PRESSURE_CONFIGS <- list(
  "200" = list(
    target = 200,
    stable_low = 175,           # Default if adaptive fails
    stable_high = 210,
    expected_period = 10        # Expected cycle time (s)
  ),
  "50" = list(
    target = 50,
    stable_low = 45,
    stable_high = 65,
    expected_period = 5
  )
)
```

#### G-Code Parameters

```r
GCODE_PARAMS <- list(
  swab_distance_mm = 90,              # USED
  pass_spacing_mm = 2,                 # reference only
  total_passes_nominal = 45,           # reference only
  swab_speed_mm_s = 83.33,             # USED
  lift_speed_mm_s = 5.0,               # reference only
  return_speed_mm_s = 300.0,           # reference only

  pressure_params = list(              # reference only (documents timing
    "50"  = list(lift_height_mm = 11, lift_time_s = 2.2,
                 expected_cycle_time_s = 5.78,  contact_threshold_g = 35),
    "200" = list(lift_height_mm = 22, lift_time_s = 4.4,
                 expected_cycle_time_s = 10.18, contact_threshold_g = 150)
  ),                                    # per protocol; not read by any function)

  contact_time_s = 1.08,               # reference only
  return_time_s = 0.30,                # reference only
  total_distance_theoretical_mm = 4050 # USED (45 x 90mm)
)
```

> Only `swab_distance_mm`, `swab_speed_mm_s`, and
> `total_distance_theoretical_mm` are actually read by the calculation
> functions. The rest of `GCODE_PARAMS` documents the physical G-code
> motion (useful context, e.g. when sanity-checking cycle timing) but is
> not a live configuration knob - changing those values has no effect on
> the analysis.

#### Sample-Specific Overrides

```r
SAMPLE_OVERRIDES <- list()
# Can override any parameter per sample:
# SAMPLE_OVERRIDES <- list(
#   "PILOT_009" = list(stable_high = 210)
# )
```

### Data Flow

#### Complete Processing Pipeline

```
1. INITIALIZATION
   - Load libraries (ggplot2, dplyr, scales, writexl)
   - Read configuration (PARAMS, PRESSURE_CONFIGS, GCODE_PARAMS)
   - Build sample configuration from metadata CSV

2. FOR EACH SAMPLE:
   - Load raw pressure trace CSV
     - Handle malformed headers/data

   - ADAPTIVE THRESHOLD DISCOVERY
     - Search first 60s with wide range (85-110% target)
     - Calculate mean +- tolerance (pressure-level-specific minimum)
     - Set sample-specific thresholds (or fail/fall back per
       adaptive_fallback_to_defaults)

   - DISTANCE CALCULATION
     - Use adaptive stable range as contact threshold
     - Accumulate distance only during stable contact
     - Validate: speed error, efficiency, estimated passes

   - CYCLE DETECTION
     - Detect initial 5 cycles
     - Estimate period (median of intervals)
     - Predict remaining cycles (strict periodicity)
     - Track both time and distance ranges

   - STATISTICS CALCULATION
     - Time-based: % stable time, cycle stats
     - Distance-based: % stable distance, efficiency
     - Pressure: mean, SD, RSD (stable regions only)

   - PLOTTING
     - Full trace (time) -> *_full_trace_time.png
     - Full trace (distance) -> *_full_trace_distance.png
     - Zoomed (distance) -> *_zoomed_distance.png
     - 25s windows -> 25s_windows/*.png

   - SAVE RESULTS
     - Detected cycles CSV
     - Summary statistics TXT

3. BATCH SUMMARY
   - Aggregate all sample results
   - Create summary table (time + distance metrics)
   - Save batch_summary.csv and .xlsx
   - Calculate overall statistics by pressure level

4. COMPARISON PLOTS
   - 50g samples small-multiples grid (one panel per sample)
   - 200g samples small-multiples grid (one panel per sample)
   - Individual stable region overlays
   - Batch QC overview (Mean_Pressure vs Target, all samples)

5. COMPLETE
   - All outputs saved to ProcessedData/
```

### Output Files

#### Directory Structure

```
ProcessedData/
  batch_summary.csv           # Main summary (all samples)
  batch_summary.xlsx          # Excel version

  Figures/                    # Comparison plots
    50g_samples_full_traces.png   # Small-multiples grid, one panel/sample
    200g_samples_full_traces.png  # Small-multiples grid, one panel/sample
    batch_qc_overview.png         # Mean_Pressure vs Target, all samples
    PILOT_*_stable_overlay.png (one per sample)

  PILOT_*/                    # One folder per sample
    PILOT_*_full_trace_time.png      # Time-based full trace
    PILOT_*_full_trace_distance.png  # Distance-based full trace
    PILOT_*_zoomed_distance.png      # Passes 1-5 zoom
    PILOT_*_detected_cycles.csv      # Cycle-by-cycle data
    PILOT_*_summary_statistics.txt   # Full statistics
    25s_windows/*.png                # Individual 25s plots

  TransientComparison/         # Side-by-side A/B comparison (see below)
    WithTransients/
      Figures/{50,200}g_samples_full_traces.png
      PILOT_*/PILOT_*_full_trace_distance.png
      PILOT_*/PILOT_*_zoomed_distance.png
    NoTransients/
      Figures/{50,200}g_samples_full_traces.png
      PILOT_*/PILOT_*_full_trace_distance.png
      PILOT_*/PILOT_*_zoomed_distance.png
```

#### TransientComparison folder (with vs. without non-contact transients)

All **distance-based** plots (per-sample full trace/zoomed, and the
small-multiples comparison plots) inherently include brief near-vertical
excursions toward 0g at every pass boundary: non-contact points (lift-off
between passes, `is_contact == FALSE`) leave `distance_mm` essentially
flat, so on a distance x-axis they collapse into a thin vertical line
rather than a normal-looking dip. This is expected, not a bug - but it's
visually noisy.

`TransientComparison/` duplicates the distance-based plots (only) into
two self-contained folders so both versions can be compared side by side
without altering the standard output above:

- **`WithTransients/`** - identical to the standard output (raw trace,
  all points included).
- **`NoTransients/`** - `mask_noncontact_data()` sets the pressure value
  to `NA` wherever `is_contact == FALSE` before plotting, so
  `geom_line()`/base `plot(type = "l")` break the line there instead of
  drawing a connecting segment down to 0 and back. This also removes the
  ramp-up overshoot peaks (which exceed `stable_range_high`, i.e. are
  also `is_contact == FALSE`), leaving only the in-band plateau shape
  visible - useful for judging pass-to-pass consistency without the
  transition noise, at the cost of no longer seeing the ramp-up/ramp-down
  shape in *this particular plot* (still fully visible in the unaffected
  time-based and 25s-window plots).

Masking is applied for display only, after any distance-alignment
calculations that depend on the true (unmasked) peak location - notably
the small-multiples comparison plot's Cycle-1-peak alignment, which would
break if the peak itself (typically outside the stable band) were masked
before that offset was computed.

#### batch_summary.csv Columns

| Column | Description | Units | Type |
|--------|-------------|-------|------|
| `Sample` | Sample ID | - | String |
| `Target` | Target pressure | g | Numeric |
| `Success` | Processing success | - | Logical |
| `Cycles` | Total cycles (strict periodicity) | count | Integer |
| `Detected` | Cycles with data detected | count | Integer |
| `First_Cycle_Time` | First cycle start time | s | Numeric |
| `Last_Cycle_Time` | Last cycle end time | s | Numeric |
| `Stable_Time_Pct` | % of time at stable pressure | % | Numeric |
| `Total_Distance_mm` | Total swabbing distance | mm | Numeric |
| `Stable_Distance_Pct` | % of distance at stable pressure | % | Numeric |
| `Distance_Efficiency_Pct` | Actual vs expected distance | % | Numeric |
| `Mean_Pressure` | Mean pressure (stable regions) | g | Numeric |
| `RSD_Pressure` | Relative SD (stable regions) | % | Numeric |
| `Flag_15Pct_Outlier` | `TRUE` if `Mean_Pressure` deviates from its own `Target` by more than `PARAMS$mean_pressure_outlier_pct` (default 15%); `NA` for failed samples. | - | Logical |

> **`Flag_15Pct_Outlier`** compares each sample's `Mean_Pressure`
> directly against its own `Target` (not against other samples). At the
> default 15% threshold, across the current 23-sample dataset it flags
> four samples: `PILOT_014` (-30.0%), `PILOT_018` (-18.6%), `PILOT_011`
> (+15.7%), and `PILOT_030` (-15.2%). **No physical/mechanical cause has
> been independently confirmed for any of these four** - the flag
> reflects a measured statistical deviation only, not a diagnosis.
> Review each flagged sample individually (raw trace, run notes,
> collection observations) before drawing conclusions; do not treat the
> flag as evidence of a known or explained problem. The threshold is
> configurable via `PARAMS$mean_pressure_outlier_pct`.

#### Detected Cycles CSV Columns

| Column | Description |
|--------|-------------|
| `cycle_num` | Sequential cycle number |
| `start_time` | Cycle start time (s) |
| `end_time` | Cycle end time (s) |
| `duration` | Cycle duration (s) |
| `mean_load` | Mean load in cycle (g) |
| `n_points` | Number of data points |
| `start_distance_mm` | Distance at cycle start (mm) |
| `end_distance_mm` | Distance at cycle end (mm) |
| `distance_span_mm` | Distance covered in cycle (mm) |

### Validation & Troubleshooting

#### Distance Calculation Validation

**Metrics to Check:**
```
Speed Error: (implied_speed - 83.33) / 83.33 x 100%
  Good:    < 5%
  Warning: 5-10%
  Bad:     > 10%

Distance Efficiency: total_distance / expected_distance x 100%
  Good:    95-125%
  Warning: 80-95% or 125-150%
  Bad:     < 80% or > 150%

Estimated Passes: total_distance / 90
  Good:    40-50 passes
  Warning: 35-40 or 50-60 passes
  Bad:     < 35 or > 60 passes
```

#### Common Issues & Solutions

##### Issue 1: Speed Error > 10%

**Possible Causes:**
- Contact threshold too loose (capturing transitions)
- Contact threshold too strict (missing swabbing)
- Data quality issues

**Diagnostics:**
```r
# Check contact percentage
contact_pct <- (n_contact_points / total_points) * 100
# Expected: ~10-15% for 200g, ~25-35% for 50g

# Check adaptive thresholds
# Should be: target +- 8-20g
```

**Solutions:**
- Review adaptive threshold discovery output
- Check if stable range is appropriate
- Manually inspect pressure trace plots

##### Issue 2: Distance Efficiency Too High (>150%)

**Cause:** Threshold too loose, capturing ramp-up/down phases

**Solution:**
```r
# Tighten adaptive tolerance bounds
PARAMS$adaptive_tolerance_200g <- 5  # Lower from 10
PARAMS$adaptive_tolerance_50g <- 5   # Lower from 8
```

##### Issue 3: Distance Efficiency Too Low (<80%)

**Cause:** Threshold too strict, missing valid swabbing

**Solution:**
```r
# Relax tolerance bounds
PARAMS$adaptive_tolerance_200g <- 15  # Increase from 10
PARAMS$adaptive_tolerance_50g <- 12   # Increase from 8
```

##### Issue 4: Few Cycles Detected

**Possible Causes:**
- Incomplete run (stopped early)
- Adaptive discovery failed
- Period estimation incorrect

**Diagnostics:**
```r
# Check console output:
# - "Discovered: mean=X, SD=X (n=X regions)"
# - If n < 3, discovery failed

# Check period estimation:
# - CV period should be < 10% for good consistency
```

**Solutions:**
- Review first 60s of data visually
- Check if target pressure was achieved
- Consider manual threshold override for problematic samples

##### Issue 5: Plotting Error

**Error:** `'names' attribute [X] must be the same length as the vector [Y]`

**Cause:** Mismatch in color assignment for wet/dry samples

**Solution:** Fixed by using `setNames()` with proper length handling in
`plot_full_traces_comparison()`.

### Usage Examples

#### Example 1: Process All Samples (Full Batch)

```r
# Run main script
Rscript "C:/Users/.../ASTRA/time_based_batch_process_adaptive_DISTANCE.R"

# Output:
# - Processes all samples in metadata CSV
# - Generates all plots and statistics
# - Creates batch summary
# - Saves to ProcessedData/
```

#### Example 2: Test a Single Sample

```r
# At the top of time_based_batch_process_adaptive_DISTANCE.R, set:
RUN_MODE <- "single"
SINGLE_TEST_SAMPLE <- "PILOT_001"

# Then run:
Rscript "C:/Users/.../ASTRA/time_based_batch_process_adaptive_DISTANCE.R"

# Output:
# - Console shows validation diagnostics for just that sample
# - Generates plots for PILOT_001 only
# - Confirms all expected output files were created
# - Quick way to test parameter changes before a full batch run
```

#### Example 3: Process Specific Samples Only (interactive)

```r
# In an interactive R console (not from another script file):
source("time_based_batch_process_adaptive_DISTANCE.R")

# Override SAMPLE_CONFIG to process subset
SAMPLE_CONFIG <- SAMPLE_CONFIG[c("PILOT_001", "PILOT_005", "PILOT_009")]

# Run processing
results <- list()
for (sample_name in names(SAMPLE_CONFIG)) {
  config <- SAMPLE_CONFIG[[sample_name]]
  result <- process_sample(sample_name, config, PARAMS)
  results[[sample_name]] <- result
}
```

#### Example 4: Custom Parameters for a Specific Sample

```r
# Add sample-specific override
SAMPLE_OVERRIDES <- list(
  "PILOT_013" = list(
    stable_low = 178,
    stable_high = 195,
    expected_period = 9.5
  )
)

# Rebuild configuration
SAMPLE_CONFIG <- build_sample_config(
  METADATA_FILE,
  PARAMS$data_dir,
  PRESSURE_CONFIGS,
  SAMPLE_OVERRIDES
)

# Run batch processing
results <- main_batch()
```

#### Example 5: Adjust Global Parameters

```r
# Modify parameters before running
PARAMS$min_stable_duration <- 0.5        # More conservative (was 0.3)
PARAMS$skip_startup_sec <- 10            # Skip more startup (was 8)
PARAMS$use_adaptive_thresholds <- FALSE  # Use defaults only

# Then run script
results <- main_batch()
```

### Interpreting Results

#### Time-Based Metrics

**Stable Time %:**
- **8-16%** (200g): Only a small fraction of total time is stable swabbing
- **25-40%** (50g): More time at stable pressure (shorter cycles)

**Why So Low?**
- Includes lift time (4.4s or 2.2s per cycle)
- Includes return time (0.3s per cycle)
- Includes lowering time (4.4s or 2.2s per cycle)
- Only ~1.08s per cycle is actual swabbing (~10-20% of cycle time)

**Use For:**
- Understanding temporal efficiency
- Comparing experiment duration
- Optimizing cycle timing

#### Distance-Based Metrics

**Stable Distance %:**
- **60-80%** (typical): Most swabbing distance maintains stable pressure
- **<60%**: Poor pressure control or premature lift-off
- **>80%**: Excellent pressure control

**Distance Efficiency:**
- **95-110%**: Good match to theoretical
- **110-160%**: Some transition capture (increasingly common now that the
  adaptive minimum tolerance is pressure-level-specific - see the
  [Changelog](#changelog))
- **>160%**: Threshold likely too loose for that sample

**Use For:**
- Direct swabbing quality assessment
- Comparing samples independent of timing
- Validating expected vs actual coverage

#### Pressure Statistics

**Mean Pressure:**
- Should be close to target
- **200g samples:** Typically 190-210g
- **50g samples:** Typically 45-58g

**RSD (Relative Standard Deviation):**
- **<2%**: Excellent consistency
- **2-5%**: Good consistency
- **>5%**: Variable pressure control

### Key Insights

#### Why Distance-Based Analysis Matters

**Time-based analysis says:** "Only ~10% of time is stable"
- Sounds poor, but misleading
- Includes non-swabbing phases (lift, return)

**Distance-based analysis says:** "60-80% of swabbing distance is stable"
- Much more meaningful quality metric
- Excludes lift/return phases
- Direct measure of swabbing performance

#### Typical Results Summary

**200g Samples (High Pressure):**
- Stable time: 8-17%
- Stable distance: 60-75%
- Distance efficiency: 110-165%
- RSD: 0.5-3.5% (excellent control)

**50g Samples (Low Pressure):**
- Stable time: 25-40%
- Stable distance: 60-80%
- Distance efficiency: 85-165%
- RSD: 2-5% (good control, more variable)

**Key Finding:** Lower pressure (50g) is harder to control consistently,
reflected in higher RSD and more variable distance efficiency.

### Script Modifications Guide

#### To Change Contact Threshold Logic

**Location:** `calculate_cumulative_distance()`

**Current:** Uses adaptive stable range
```r
is_stable <- (load >= stable_range_low & load <= stable_range_high)
```

**Alternative:** Use simple threshold
```r
is_stable <- (load >= simple_threshold)
```

#### To Change Cycle Detection Strategy

**Location:** `detect_initial_cycles()` and `predict_and_detect_remaining()`

**Current:** Strict periodicity with forced placeholders

**To make less strict:**
```r
# In predict_and_detect_remaining():
# Remove forced placeholder logic
if (is.null(region)) {
  next  # Skip this cycle instead of forcing
}
```

#### To Add a New Plot Type

**Template:**
```r
create_my_custom_plot <- function(data, all_cycles, params, sample_name) {
  filepath <- file.path(params$output_dir,
                        paste0(sample_name, "_custom.png"))

  png(filepath, width = 1800, height = 600, res = 100)
  # ... plotting code ...
  dev.off()

  cat(sprintf("  Custom plot saved: %s\n", filepath))
}

# Call in process_sample() after other plots
create_my_custom_plot(data, all_cycles, sample_params, sample_name)
```

#### To Add a New Statistic

**Location:** `calculate_statistics()`

```r
# Add calculation
my_new_metric <- <calculation>

# Add to return list
return(list(
  # ... existing metrics ...
  my_new_metric = my_new_metric
))

# Update batch summary dataframe structure in main_batch()
summary_data <- data.frame(
  # ... existing columns ...
  My_New_Metric = numeric()
)

# Add to row building in main_batch()
summary_data <- rbind(summary_data, data.frame(
  # ... existing values ...
  My_New_Metric = result$stats$my_new_metric
))
```

### Technical Notes

#### Performance

**Typical Processing Time:**
- Single sample: 10-20 seconds
- Full batch (24 pilot samples): roughly 5-10 minutes
- Memory usage: <1GB RAM

#### Data Requirements

**Minimum CSV Requirements:**
- Columns: `timestamp`, `elapsed_sec`, `load`
- `distance` column is optional (recalculated anyway)
- Sampling rate: 10-200 Hz (tested at 100 Hz)
- Duration: >30 seconds recommended

**Metadata CSV Requirements:**
- Columns: `RunID`, `Pressure_g`, `Solvent_level`
- `RunID` must match CSV filename (without `.csv`)

#### Limitations

1. **Assumes constant swab speed** (83.33 mm/s)
   - Based on G-code F5000 parameter
   - Actual speed may vary due to acceleration

2. **Strict periodicity assumption**
   - May over-predict cycles if run stopped early
   - Forced placeholders for missing cycles

3. **Single pressure threshold per sample**
   - Adaptive threshold is constant across sample
   - Doesn't adapt to pressure drift

4. **No real-time analysis**
   - Post-processing only
   - Cannot provide feedback during experiment

5. **Row-by-row loops** in `calculate_cumulative_distance()`,
   `detect_initial_cycles()`/`predict_and_detect_remaining()` (cycle
   accumulation via repeated `rbind()`), and `calculate_statistics()`
   (stable-pressure extraction) are not vectorized. Fine at current data
   volumes, but a candidate for future performance work if trace lengths
   or sample counts grow substantially.

#### Future Enhancement Ideas

- Real-time processing capability
- Pressure drift correction
- Automatic outlier detection
- Machine learning for cycle detection
- Web-based result viewer
- Automated report generation
- Vectorize the hot loops noted above
- `testthat` regression suite covering `clean_and_read_csv()` (including
  the known malformed-header cases), `detect_stable_by_threshold()`, and
  `calculate_cumulative_distance()`

---

## Distance-Based Approach (Detailed)

### G-Code Analysis: Understanding the Swabbing Motion

The ASTRA swabbing system uses a modified 3D printer controlled by G-code
to perform automated swabbing with precise, repeatable motion.

#### G-Code Movement Parameters

Based on analysis of `Ratchet9x9-50g.gcode.txt` and `Ratchet9x9-200g.gcode.txt`:

| Parameter | 50g Protocol | 200g Protocol | Notes |
|-----------|--------------|---------------|-------|
| **Swab distance per pass** | 90 mm | 90 mm | Horizontal X-axis motion (X90 command) |
| **Pass spacing** | 2 mm | 2 mm | Y-axis increment between passes (Y2 command) |
| **Total passes** | 45 | 45 | Nominal number of swab passes |
| **Z-lift height** | 11 mm | 22 mm | Vertical distance between passes (Z11/Z22) |
| **Swab speed** | 83.33 mm/s | 83.33 mm/s | Horizontal speed during contact (F5000 mm/min) |
| **Lift/lower speed** | 5.0 mm/s | 5.0 mm/s | Vertical Z-axis speed (F300 mm/min) |
| **Return speed** | 300 mm/s | 300 mm/s | Air return speed (F18000 mm/min) |

#### Theoretical Cycle Structure

**200g Protocol (one complete swab cycle):**
1. **Contact/Swab phase:** 90mm / 83.33 mm/s = **1.08 seconds**
   - Swab moves horizontally across surface at constant pressure
   - This is where stable pressure data is recorded
2. **Lift phase:** 22mm / 5 mm/s = **4.4 seconds**
   - Swab lifts vertically off surface
   - Pressure drops to zero
3. **Return phase:** 90mm / 300 mm/s = **0.3 seconds**
   - Swab returns to starting position in air
   - Zero pressure
4. **Lower phase:** 22mm / 5 mm/s = **4.4 seconds**
   - Swab lowers back to contact surface
   - Pressure ramps up

**Total cycle time (200g):** 1.08 + 4.4 + 0.3 + 4.4 = **~10.18 seconds**

**50g Protocol (same structure):**
- Contact: 1.08s
- Lift: 2.2s (11mm / 5 mm/s)
- Return: 0.3s
- Lower: 2.2s
**Total cycle time (50g):** **~5.78 seconds**

#### Expected Total Distance

- **Theoretical maximum:** 45 passes x 90mm = **4,050 mm** (4.05 meters)
- **Per cycle (nominal):** **90 mm**

### Distance Calculation Methodology

#### Core Principle

**Distance only accumulates during stable swabbing contact**, not during
lift-off, air return, or pressure ramp phases.

#### Algorithm

```
For each data point:
1. Determine if pressure is within stable swabbing range (adaptive thresholds)
2. If in stable range:
   - Calculate time elapsed since previous point
   - distance_delta = time_delta x swab_speed (83.33 mm/s)
   - cumulative_distance += distance_delta
3. If not in stable range (lifted/transitioning):
   - distance_delta = 0
   - cumulative_distance unchanged
4. Store cumulative_distance_mm for this point
```

#### Contact Threshold Logic

The system uses **adaptive threshold discovery** to determine the stable
swabbing range:

1. **Discover stable pressure regions** in first 60 seconds of data
2. **Calculate mean and standard deviation** of discovered stable pressures
3. **Set contact range:** mean +- tolerance
   - Minimum tolerance: pressure-level specific -
     `PARAMS$adaptive_tolerance_200g` (10g) or
     `PARAMS$adaptive_tolerance_50g` (8g)
   - Maximum tolerance: 20g
4. **Distance accumulates only within this range**

**Example (PILOT_001, current script version):**
- Discovered mean: 193.2g
- SD: 1.6g
- Tolerance: 10.0g (pressure-level minimum applied, since 2xSD = 3.2g is
  below the 200g-sample minimum of 10g)
- **Contact range: 183.2 - 203.2g**
- Distance accumulates only when pressure is in this range
- Resulting diagnostics: 746 contact points (14.6%), 74.71s contact time,
  6225.8mm total distance, 0.0% speed error, distance efficiency 153.7%

> Earlier versions of this document showed a tolerance of "5.0g (minimum
> applied)" for this same sample. That reflected a bug where the
> pressure-level-specific minimums in `PARAMS` were defined but never
> used - every sample got a hardcoded 5g minimum regardless of pressure
> level. This was fixed in v1.1-CONSOLIDATED; see the
> [Changelog](#changelog).

#### Why Use Stable Range (not simple threshold)?

**Simple threshold** (e.g., load > 50g):
- Captures ramp-up and ramp-down phases
- Includes unstable transition periods
- Overestimates actual swabbing distance

**Stable range** (e.g., 183-203g for 200g target):
- Captures only stable horizontal swabbing motion
- Excludes pressure transitions
- Accurately represents actual swabbing work

### Metrics Comparison: Time vs. Distance

#### Time-Based Metrics (Definition)

**Definition:** Percentage of total experiment time where pressure is stable

**Calculation:**
```
stable_time_% = (total_stable_time_s / total_experiment_time_s) x 100
```

**Typical values:**
- 200g samples: 8-17%
- 50g samples: 25-40%

**Interpretation:** Shows how much of the total time (including lift,
return, lower phases) is spent at stable pressure.

**Use cases:**
- Understanding temporal efficiency of swabbing cycles
- Comparing experiment duration across samples
- Identifying timing variations in swabbing protocol

#### Distance-Based Metrics (Definition)

**Definition:** Percentage of actual swabbing distance where pressure is stable

**Calculation:**
```
stable_distance_% = (total_stable_distance_mm / total_swabbing_distance_mm) x 100
```

**Typical values:**
- Expected: 60-80% (most of horizontal swabbing distance should be stable)
- PILOT_001 example: ~66%

**Interpretation:** Shows what fraction of horizontal swabbing motion
maintains stable pressure.

**Use cases:**
- Assessing quality of pressure control during actual swabbing
- Identifying premature lift-off or delayed contact
- Comparing swabbing technique independent of timing

#### Expected vs. Observed Values

| Metric | Theoretical | Typical Observed | Notes |
|--------|------------|------------------|-------|
| **Total swabbing distance** | 4,050 mm (45x90mm) | 3,900-6,800 mm | May exceed due to data collection during transitions |
| **Distance efficiency** | 100% | 95-165% | >100% indicates some transition capture |
| **Mean distance per cycle** | 90 mm | 80-150 mm | Variability due to contact timing |
| **Stable distance %** | ~90% (ideal) | 60-80% | Depends on pressure control quality |
| **Estimated passes** | 45 | 40-70 | Calculated from total_distance / 90mm |

### Distance Analysis Output Files

#### Files Generated

**Per sample:**
1. `{sample}_full_trace_time.png` - Time-based full trace
2. `{sample}_full_trace_distance.png` - Distance-based full trace
3. `{sample}_zoomed_distance.png` - Zoomed view of passes 1-5 (0-450mm)

**Batch summary:**
- `batch_summary.csv` - includes both time and distance columns

#### batch_summary.csv Distance Columns

| Column | Description | Units |
|--------|-------------|-------|
| `Stable_Time_Pct` | % of time at stable pressure | % |
| `Total_Distance_mm` | Total swabbing distance | mm |
| `Stable_Distance_Pct` | % of distance at stable pressure | % |
| `Distance_Efficiency_Pct` | Actual vs. expected distance | % |

**Example row (PILOT_001):**
```
PILOT_001, 200, TRUE, 50, 45, ..., 11.85%, 6225.8mm, 66.18%, 153.7%, 191.6g, 1.5%
```

### Plot Interpretation Guide

#### Distance-Based Full Trace Plot

**X-axis:** Cumulative swabbing distance (mm)
**Y-axis:** Load (g)

**Features:**
- Gray line: Full pressure trace
- Blue dashed lines: Stable threshold boundaries
- Green shaded regions: Detected stable contacts
- Distance only advances during green regions

**Key observations:**
- **Plateaus in X-axis:** Indicate lift-off periods (no distance accumulation)
- **Steep sections:** Rapid distance accumulation during stable swabbing
- **Smooth progression:** Good pressure control throughout swabbing distance

#### Zoomed Plot (Passes 1-5)

**Window:** 0-450mm (first 5 passes x 90mm)

**Features:**
- Gray vertical lines: Pass boundaries (every 90mm)
- Labels: "Pass 1", "Pass 2", etc.
- Green regions: Stable pressure during each pass

**Use for:**
- Examining startup behavior
- Verifying pass-to-pass consistency
- Identifying early-pass anomalies

### Validation Diagnostics

When processing runs, validation metrics are displayed:

```
=== DISTANCE CALCULATION ===
  Target pressure: 200g
  Contact range: 183.2 - 203.2g (stable swabbing range)
  Swab speed: 83.33 mm/s

=== VALIDATION DIAGNOSTICS ===
  Total data points: 5098
  Contact points: 746 (14.6%)
  Total contact time: 74.71 s
  Total swabbing distance: 6225.8 mm
  Implied swab speed: 83.33 mm/s
  Expected swab speed: 83.33 mm/s
  Speed error: 0.0%
  Estimated passes: 69.2 (90.0 mm per pass)
  Theoretical maximum: 4050 mm (45 passes)
  Distance efficiency: 153.7%
```

#### Interpreting Validation Metrics

**Good indicators:**
- Speed error < 5%: Distance calculation is accurate
- Distance efficiency 95-160%: Reasonable capture of swabbing motion given
  the current pressure-level-specific tolerance minimums
- Estimated passes in a plausible range for the sample's protocol

**Warning signs:**
- Speed error > 10%: Check contact threshold settings
- Distance efficiency > 180%: Threshold too loose, capturing transitions
- Distance efficiency < 80%: Threshold too strict, missing swabbing
- Estimated passes far below 45: Significant data loss or incomplete run

### Distance Calculation Troubleshooting

#### Issue: Distance efficiency too high

**Cause:** Contact threshold too loose, capturing ramp-up/down phases

**Solution:**
- Check adaptive threshold discovery results
- Verify stable range is appropriate for sample
- Consider manual threshold adjustment if needed (see
  [Distance Efficiency Too High](#issue-2-distance-efficiency-too-high-150)
  in the Script Reference Guide)

#### Issue: Distance efficiency too low (<80%)

**Cause:** Contact threshold too strict, missing valid swabbing

**Solution:**
- Review pressure trace for actual stable regions
- Check if adaptive discovery found sufficient regions
- May need to relax minimum tolerance

#### Issue: Distance plot shows no progression

**Cause:** No data points meeting stable range criteria

**Solution:**
- Check raw pressure trace for stability
- Verify target pressure is achievable
- Review adaptive threshold discovery output

### Technical Implementation Notes

#### Changes from the Original (Time-Only) Script

1. Added G-code configuration section with movement parameters
2. New function: `calculate_cumulative_distance()` with validation
3. Enhanced cycle detection to track distance ranges
4. New plotting functions:
   - `create_full_trace_plot_distance()`
   - `create_zoomed_plot_distance()`
5. Enhanced statistics with distance metrics
6. Modified CSV output with additional columns

#### Backward Compatibility

- **Time-based analysis unchanged** - original metrics still calculated
- **Plot files renamed:** `*_full_trace.png` -> `*_full_trace_time.png`
- **Both analyses run in parallel** - no functionality removed

### Summary: When to Use Time vs. Distance Metrics

#### Use Time-Based Metrics For:
- Understanding overall experiment efficiency
- Comparing cycle timing across samples
- Evaluating temporal consistency
- Process optimization (reducing total time)

#### Use Distance-Based Metrics For:
- Assessing swabbing quality (pressure control during motion)
- Comparing samples independent of timing
- Validating expected vs. actual swabbing distance
- Identifying spatial pressure variations

#### Use Both Together For:
- Complete understanding of swabbing performance
- Diagnosing specific issues (timing vs. pressure control)
- Comprehensive quality control
- Publication-ready analysis

---

## Changelog

### New Sibling Script - Main Study Pressure Processing (2026-08-25)

**`main_study_batch_process.R`** (new file, adapted from this repo's own
`time_based_batch_process_adaptive_DISTANCE.R` - the Pilot Study script,
which remains completely untouched and is still the reference for the
Pilot workflow documented throughout the rest of this README). Processes
the Main Study's `MAIN_001`-`MAIN_044` pressure traces instead of the
Pilot Study's `PILOT_*` traces, generalising several places that were
hardcoded for the Pilot's 2 pressure levels (50g/200g) to instead support
the Main Study's 5 levels (10/50/100/200/300g - VeryLow/Low/Medium/High/
VeryHigh). See that script's own `MAIN_STUDY_ADAPTATION` comments
throughout for exactly what changed and why; not duplicated here since
the two scripts are meant to be read side by side, not merged.

**Real bugs found and fixed while running it against real Main Study
data** (not Main-Study-specific in nature - these could in principle have
affected a future Pilot Study re-run too, just never triggered by any of
the 27 Pilot samples so far):

1. **Infinite loop** in `predict_and_detect_remaining()`'s cycle-prediction
   `while (TRUE)` loop: if the Phase 2 period estimate degenerates to a
   spuriously tiny value (observed for one 10g sample, `MAIN_008`, whose
   initial 3-5 cycles overlapped instead of being cleanly separated),
   `last_start` can land on a fixed point where the same real stable
   region keeps getting re-found in every subsequent search window
   regardless of where that window is centred - the loop then never
   terminates (observed: over 60,000 identical iterations, still running
   after 20 minutes). Fixed with two independent guards: a hard iteration
   cap (`max_total_cycles`) and an explicit stall detector that aborts
   after a few consecutive non-advancing iterations, returning whatever
   cycles were collected so far with a `warning()` instead of hanging.
2. **Silent sample-dropping**: three early-exit points in `process_sample()`
   (file not found, unreadable CSV, "not enough initial cycles detected")
   used a bare `return(NULL)`. In `main_batch()`'s processing loop,
   `results[[sample_name]] <- NULL` **deletes that key from the list**
   in R rather than storing a NULL value in it - silently dropping the
   sample from the entire batch summary (no row at all, not even a
   `Success = FALSE` one). Never observed across the Pilot Study's 27
   samples, but 8 of the Main Study's 44 samples hit this exact condition
   ("Could not detect enough initial cycles") and were vanishing
   entirely. Fixed by having all three paths return a proper
   `list(success = FALSE, target = ..., error = ...)` instead of `NULL`
   (matching the shape the `tryCatch` error handler elsewhere in the
   script already used), plus a defensive fallback in `main_batch()`
   itself in case any future code path still returns a bare `NULL`.
3. **`NA` search tolerance**: if Phase 1 finds exactly 2 initial cycles
   (the bare minimum, one interval), `sd()` of a single value is `NA`,
   which propagates through `predict_and_detect_remaining()`'s tolerance
   calculation, making every subsequent search window `NA` and causing
   every remaining cycle to be forced/undetected - `calculate_statistics()`
   then fails outright ("no rows to aggregate", hit by `MAIN_024`, a 100g
   sample). Fixed by falling back to `min_period_tolerance` directly when
   `sd_period` is `NA`, rather than propagating the `NA` through the
   `max()` comparison.
4. Forgot to re-declare the `overall_stats` data frame (with its column
   types) before the loop that populates it, while generalising the
   Pilot script's hardcoded 50g/200g-only summary-statistics block into a
   dynamic loop over however many nominal pressure levels are actually
   present - caught immediately on the first run (`object 'overall_stats'
   not found`), fixed by adding the initialisation back before the loop.

**Retuning** (after the above fixes, still needed before all 44 samples
processed cleanly): the initial `expected_period` guess for the new 300g
level (12s, an interpolation with no G-code file to derive an exact value
from) was too far from the true ~14.1s period (measured directly from the
one 300g sample that succeeded on the first attempt) for the ±2.5s initial
search window - causing 10 of 12 300g samples to fail with "could not
detect enough initial cycles". Corrected to 14s, after which all 12
succeeded. Also generalised two more Pilot-script binary `target >= 150`
checks (the adaptive-tolerance-tightening minimum in
`calculate_adaptive_thresholds()`, and the stable-region-overlay plot's
y-axis zoom range) into proper 5-level lookup tables
(`adaptive_tolerance_by_target`, `y_limits_by_target`), since a single
50g/200g binary split can't represent 5 levels.

**Sample-specific overrides** (same mechanism/spirit as the Pilot script's
existing `PILOT_014`/`PILOT_018` entries, added after directly inspecting
the raw traces on request): `MAIN_030` (10g target, but the raw trace
shows a real, non-noisy plateau drifting ~16g early in the run down to
~12g by the end - stable band widened to 10-18g to capture the whole
drift) and `MAIN_043` (50g target, raw trace plateaus at a consistent
~38-40g throughout, well below both the adaptive discovery band and the
default fallback - stable band set to 34-46g). Both now process
successfully; final result **44 of 44 Main Study samples processed**,
6 flagged as outliers (`Flag_15Pct_Outlier`, all genuinely off-target
achieved pressures, not detection failures) - `batch_summary.csv`/`.xlsx`
plus the full standard figure set (per-sample time/distance/zoomed/
stable-overlay plots, per-target comparison plots, batch QC overview),
all under `Main Study/Pressure Traces/ProcessedData/`.

### New Plotting Scripts - Recovery vs Actual Swabbing Pressure (2026-08-25)

Four new, independent plotting scripts (each can be run standalone; none
of them modify each other or `main_study_batch_process.R`), joining the
above `Mean_Pressure` results with the GCMSQuantitation-derived recovery
data (`PETN_Recovery_pct`/`RDX_Recovery_pct` from
`main_study_data_nested.csv`, and - since the Main Study's own design
deliberately reused the Pilot Study's existing 50g/200g wet-only
replicates rather than re-collecting them - also `pilot_data_nested.csv`'s
matching samples, joined against the Pilot Study's own
`Pressure Traces/ProcessedData/batch_summary.csv`):

- **`plot_main_study_recovery_vs_pressure.R`** - one figure per analyte,
  faceted Surface (rows) x nominal pressure level (columns, free x-axis
  scale per panel so the actual within-level spread is visible instead of
  compressed against a shared 0-300g axis), points coloured by Study
  (Pilot/Main).
- **`plot_recovery_boxplot_actualpressure.R`** - boxplots by nominal
  pressure level (pooling Pilot+Main per box), but each point's
  x-*position* is driven by its own real achieved pressure within that
  box's column (linearly rescaled into the box's width) rather than a
  random jitter, so left-to-right ordering within a box reflects genuine
  achieved-pressure ordering. Boxplot outliers (standard 1.5xIQR rule)
  rendered as hollow circles.
- **`plot_recovery_vs_pressure_unified.R`** (+ `_quadratic`/`_logarithmic`
  variants) - a single continuous, unified-x-axis-scale scatter (all
  nominal levels pooled into one panel per Surface x Analyte cell, not
  split into per-level facets), with a fitted trend line (linear,
  quadratic, or logarithmic respectively) + 95% CI per panel, plus an
  AIC/BIC-based comparison of all three functional forms (see the
  `Design of Experiments` repo's own `CONTEXT.md`, August 25 2026 session,
  for the follow-on statistical modelling built on top of these plots -
  a properly-pooled mixed-effects model using this same actual-pressure
  data, plus a re-examination of the pre-existing RDX batch/date effect
  investigation in light of the newly-available Main Study dates).

### Version 1.3.0-CONSOLIDATED - Side-by-Side With/Without-Transients Comparison Folders (2026-07-30)

Added `mask_noncontact_data()`, which sets the pressure value to `NA`
wherever `is_contact == FALSE` before plotting (already-computed by
`calculate_cumulative_distance()`, so no new detection logic needed).
This lets `geom_line()`/base `plot(type = "l")` break the line at
non-contact points instead of drawing a connecting segment down to 0 and
back - decluttering the distance-based plots' "comb" pattern down to just
the in-band plateau shape.

Rather than replace the existing distance-based plots outright, generated
both versions side by side into a new `TransientComparison/` folder
(`WithTransients/` and `NoTransients/` subfolders, each self-contained
with their own `Figures/` and `PILOT_*/` structure) so the two can be
evaluated visually before deciding whether to adopt the change more
broadly. Covers the three distance-based plot types:
`create_full_trace_plot_distance()`, `create_zoomed_plot_distance()`
(both via a new `output_dir_override` parameter, reusing the same
detection results already computed in `process_sample()` - no
recomputation), and `plot_full_traces_comparison()` (via new
`suppress_transients`/`output_dir_override` parameters).

One correctness subtlety: masking must happen *after*
`plot_full_traces_comparison()`'s Cycle-1-peak alignment step, not
before - that alignment searches for the true pressure peak to compute
each sample's distance offset, but the peak typically exceeds
`stable_range_high` (i.e. is itself `is_contact == FALSE`), so masking
first would make `which.max()` find the wrong, lower local peak instead.

Verified via a full batch re-run and direct visual inspection: the
`NoTransients` versions clearly show cleaner plateau shapes (and, as a
side effect, no longer show the ramp-up overshoot peaks either, since
those are also outside the stable band) while `PILOT_014`/`PILOT_018`
remain just as visually distinct from their peers as in the standard
`WithTransients` versions. Confirmed `WithTransients/` output is
pixel-equivalent to the standard (non-duplicated) output. The NA-masking
intentionally triggers ggplot2's "Removed N rows containing missing
values" warning on the two `NoTransients` comparison-plot saves; wrapped
those two specific `ggsave()` calls in `suppressWarnings()` (confirmed
via a temporary `options(warn=1)` diagnostic run that ~20 pre-existing,
unrelated warnings from `plot_stable_regions_overlay()`'s hardcoded
y-axis limits were already present before this change and are left
untouched) so the console warning count returns to its prior baseline.

### Version 1.2.9-CONSOLIDATED - Visualization Redesign: Small Multiples + Batch QC Overview Plot (2026-07-30)

Reviewed all 6 plotting functions' actual rendered output (not just the
code) and found two concrete problems in `plot_full_traces_comparison()`
(the 50g/200g "all samples" comparison plots):

1. **Illegible overlay.** With 10-13 samples overlaid on a single panel,
   the result was a dense "spaghetti" of overlapping sawtooth traces
   (each pass boundary produces a near-vertical drop toward 0g) - individual
   samples could not be visually distinguished even with distinct colors.
2. **Confirmed color-collision bug.** The per-sample color assignment
   recycled only 5 hex colors per wet/dry condition via `rep_len()`. The
   200g group has 7 dry and 6 wet samples - meaning at least 2 pairs of
   genuinely different samples were rendered in the **literal identical
   color**, distinguishable only by reading the legend text, not the plot.

**Fix:** `plot_full_traces_comparison()` now renders each sample as its
own panel via `facet_wrap(~ sample_id)` instead of overlaying all samples
on one panel. Color now only needs to encode wet/dry condition (2 fixed
colors, `PLOTTING_PARAMS$color_wet`/`color_dry`), which structurally
eliminates the collision (2 colors, 2 categories, no recycling possible).
Critically, **panels share a fixed y-axis scale** (not `scales = "free"`)
so a sample sitting at a different absolute pressure level than its
peers is immediately visible by comparing panel heights - confirmed by
inspecting the regenerated output: PILOT_014/PILOT_018's panels visibly
sit at a lower plateau (~35-40g) than every other 50g panel (~45-70g),
with no numbers required to spot it. Image dimensions now scale
automatically with panel count (`comparison_ncol`,
`comparison_panel_width_in`/`height_in`).

**Added:** new `plot_batch_qc_overview()` function/plot
(`Figures/batch_qc_overview.png`) - a Cleveland dot plot, one row per
sample, `Mean_Pressure` vs `Target`, faceted by Target level, with a
shaded band at `Target × (1 ± mean_pressure_outlier_pct)` and points
colored by `Flag_15Pct_Outlier`. This is the visual counterpart to that
CSV column added in v1.2.7 - previously the only way to see how samples
compare to target was reading the table; verified the regenerated plot
correctly renders all 4 currently-flagged samples (PILOT_011, PILOT_014,
PILOT_018, PILOT_030) in red against the shaded/dashed reference lines.

Both new/changed plots verified via a full batch re-run; no new R
warnings introduced (same pre-existing warning count as before), and
all other plot types (full trace time/distance, zoomed, 25s windows,
stable region overlay) are unaffected - their code was not touched.

### Version 1.2.8-CONSOLIDATED - Removed Flag_2SD_Outlier Column (2026-07-30)

Removed `Flag_2SD_Outlier` (added in v1.2.6) from `batch_summary.csv`/
`.xlsx` and `main_batch()`. It never flagged any sample in the current
23-sample dataset - including `PILOT_014`/`PILOT_018`, the very samples
it was meant to help screen for - due to the outlier-masking effect
documented in v1.2.6/v1.2.7 (their own values inflate the group SD used
to test them). `Flag_15Pct_Outlier` (v1.2.7), which compares each
sample's `Mean_Pressure` directly against its own `Target` rather than
group statistics, remains as the sole QC flag column and is not subject
to this failure mode. See v1.2.6/v1.2.7 below for the original
rationale and the masking analysis that led to this removal.

### Version 1.2.7-CONSOLIDATED - Add Flag_15Pct_Outlier Absolute-Threshold Column (2026-07-30)

`Flag_2SD_Outlier` (v1.2.6) failed to catch `PILOT_014`/`PILOT_018` due
to outlier masking. Added a second, independent QC column,
`Flag_15Pct_Outlier`, that compares each sample's `Mean_Pressure`
directly against its own `Target` (not against other samples), avoiding
that failure mode entirely. Threshold is configurable via the new
`PARAMS$mean_pressure_outlier_pct` (default `0.15`, i.e. 15%).

Before picking 15%, compared several thresholds against the full
23-sample dataset:

| Threshold | Samples flagged |
|---|---|
| ±10% | 9 (PILOT_007/011/013/014/018/021/022/030/031) - too noisy, includes several samples with no other sign of a problem |
| ±15% | 4 (PILOT_011/014/018/030) |
| ±18% | 2 (PILOT_014/018 only) |
| ±20-30% | 1 (PILOT_014 only) |

±15% was chosen as a round, principled number rather than reverse-
engineering a threshold to fit exactly `PILOT_014`/`PILOT_018`. At 15%
it flags those two plus two borderline cases (`PILOT_011` at +15.7%,
`PILOT_030` at -15.2%). **No physical/mechanical cause has been
independently confirmed for any of these four samples** - all four
currently have the same evidentiary status (a measured deviation from
Target exceeding the threshold, nothing more); review each one
individually before drawing conclusions. Verified live in
`main_batch()` and cross-checked externally against
`batch_summary.csv`; results match exactly.

### Version 1.2.6-CONSOLIDATED - Add Flag_2SD_Outlier Column to Batch Summary (2026-07-30)

Added a `Flag_2SD_Outlier` column to `batch_summary.csv`/`.xlsx`
(`main_batch()`), flagging any sample whose `Mean_Pressure` falls
outside its own Target group's mean ± 2 standard deviations (50g and
200g evaluated as separate groups, using all `Success == TRUE` samples;
`NA` for failed samples).

Verified against the current 23-sample dataset: computed live in
`main_batch()` and cross-checked independently by replicating the same
calculation externally against `batch_summary.csv` - results match
exactly. However, **neither `PILOT_014` (35.0g) nor `PILOT_018` (40.7g)
is flagged**, despite both being genuinely off-target 50g runs (see
Troubleshooting above and v1.2.5 below): with both included, the 50g
group's SD is 7.21g, giving a ±2SD range of [33.41, 62.27]g, which both
values fall inside. This is outlier masking, not a bug - tested a
leave-one-out variant and a robust median/MAD variant, and neither
catches both simultaneously either, since ~20% of the 50g group being
simultaneous low-pressure outliers is enough to defeat simple
univariate ±2SD-style screens at this small a sample size (n=10).
Documented this limitation directly in the batch_summary.csv Columns
table so it isn't mistaken for a working outlier detector.

### Version 1.2.5-CONSOLIDATED - Recovered PILOT_014/PILOT_018 via Sample-Specific Stable Bands (2026-07-30)

Both previously-failing 50g samples (`PILOT_014`, `PILOT_018`) were
investigated in depth. The raw traces are clean, complete (3000/3000
expected samples, no gaps or truncation), and highly periodic - but
neither ever sustains inside the nominal 45-65g band nor the adaptive
discovery band (42.5-55g, i.e. `target * [0.85, 1.10]`):

- `PILOT_014`: longest in-band run in either band was ~0.10s (need
  ≥0.3s); replicating the detection logic directly against the raw CSV
  found the sample's real sustained plateau at **[30,42]g, mean ~35g**
  (20 clean regions, 0.6-0.8s each, within the same 8-128s search
  window `detect_initial_cycles()` uses).
- `PILOT_018`: longest in-band run was ~0.20s; real sustained plateau at
  **[34,48]g, mean ~41g** (21 clean regions, 0.8-1.2s each).

Both samples happen to have been run back-to-back (10 minutes apart,
same day) with identical `swab_mount` hardware config. That timing
coincidence does not by itself establish a cause - **no
physical/mechanical root cause has been independently confirmed** (no
logged calibration event, no operator-observed anomaly during
collection). What is confirmed is that this is not sensor noise (region
pressures are repeatable cycle-to-cycle, σ≈1-2g) and not a script bug
(the same detection logic already works correctly for 22 other
samples, including other 50g samples whose plateaus sit at 44-58g,
in-band). The low pressure itself is a confirmed, measured
characteristic of these two runs; *why* it occurred is not known.

Added both samples to `SAMPLE_OVERRIDES` with `stable_low`/`stable_high`
set to their actual achieved bands above. `target` was left at 50 for
both (documents the intended protocol); only the achieved stable band
was overridden. Since `discover_stable_pressure_range()`'s discovery
window is based on `config$target` (not `stable_low`/`stable_high`),
adaptive discovery still correctly fails for both samples (0 qualifying
regions in the 42.5-55g discovery band) and `process_sample()` falls
back to the (now-overridden) `config$stable_low`/`stable_high` per its
existing `adaptive_fallback_to_defaults` logic - no changes to any
detection function were needed, only two new config entries.

Verified via `RUN_MODE = "single"` on each sample individually before a
full batch re-run: both now succeed, with `Mean_Pressure` of 35.0g
(PILOT_014) and 40.7g (PILOT_018) - legitimately below the 50g target,
as expected given the root cause. All previously-successful 22 samples
were re-verified unchanged (`SAMPLE_OVERRIDES` was empty for them
before and after this change). See the Troubleshooting section above
for guidance on excluding these two from "at-target" aggregate
statistics.

### Version 1.2.4-CONSOLIDATED - Align Comparison Traces to Cycle 1 Peak Instead of Band Entry (2026-07-28)

Tried aligning to each sample's Cycle 1 peak pressure instead of its entry
into the adaptive stable band (v1.2.3), to see if it improved on the
already-better-but-imperfect alignment. Mechanically: the ramp-up
overshoot typically exceeds `stable_range_high`, so "Cycle 1 start" (first
entry into the band) is recognized strictly *after* the peak, on the way
back down - the peak is a sharper, more consistently identifiable physical
event (the swab head impact) to align samples on.

`plot_full_traces_comparison()` now searches `pressure_g` for its maximum
within a window from 5s before Cycle 1's recognized start through its end,
and uses that point's `distance_mm` as the alignment offset instead of
`start_distance_mm`.

Verified this is a real improvement, not just a different-but-equally-bad
offset: on a 6-sample test (PILOT_001/002/006/007 at 200g,
PILOT_003/005 at 50g), the well-behaved pairs (PILOT_001/002,
PILOT_003/005) had visibly tighter peak-to-peak alignment across the full
1000mm window than with the band-entry offset. On the full 24-sample
batch, peaks across all 21 successful samples now cluster into
noticeably tighter, more distinct bands (e.g. near 0, 130, 280, 400,
530, 650, 800, 930mm) than the v1.2.3 version. PILOT_006/007 (the two
unusually noisy 200g runs) remain out of phase with the rest, confirming
that's a genuine characteristic of those specific runs rather than an
alignment shortcoming.

### Version 1.2.3-CONSOLIDATED - Align Comparison Traces to Cycle 1 Start (2026-07-28)

Even after windowing to 1000mm (v1.2.2), pass peaks across samples in the
50g/200g comparison plots were essentially unaligned - each sample's
`distance_mm = 0` corresponds to the first raw CSV row, not a physically
meaningful reference point, and since both the data logger and the ASTRA
swab motion are started manually, that raw start offset differs randomly
per sample. `plot_full_traces_comparison()` now re-zeros each sample's
trace using `result$cycles`' Cycle 1 `start_distance_mm` (already computed
by the periodic cycle-detection algorithm, no new calculation needed) -
i.e. `distance_mm = 0` now means "this sample's first detected stable
pass," which is directly comparable across samples.

This meaningfully improved alignment (verified visually - peaks now
cluster into distinguishable bands instead of being smeared uniformly
across the window) but does not perfectly align every pass for every
sample: small differences between each sample's true cycle period cause
gradual drift over many passes, and a couple of unusually noisy runs
(e.g. PILOT_006/007 at 200g, with SD 6-7g vs ~1-2g for well-behaved
samples) show more spread than the rest. If tighter alignment is needed
later, consider aligning to each cycle's peak pressure instead of its
entry into the (narrower) adaptive stable band, or a cross-correlation-based
shift against a reference sample.

### Version 1.2.2-CONSOLIDATED - Window Comparison Plots to First 1000mm (2026-07-28)

With ~8-13 samples' complete multi-thousand-mm traces overlaid on one plot
(per the v1.2.1 change below), individual passes became impossible to
distinguish - just a dense wall of overlapping lines. Re-added
`PLOTTING_PARAMS$comparison_distance_limit_mm` (this time actually wired
in, default 1000mm) and now pre-filter each sample's trace to
`distance_mm <= comparison_distance_limit_mm` before plotting, rather than
relying on `scale_x_continuous(limits = ...)` to drop rows afterward (which
also spammed the console with "Removed N rows" warnings for traces with
thousands of out-of-window points). The plot subtitle now states how much
of each trace is shown.

### Version 1.2.1-CONSOLIDATED - Restore Continuous Full Traces in Comparison Plots (2026-07-28)

The v1.2 change below briefly filtered `plot_full_traces_comparison()` down
to stable/contact-only points (to avoid what looked like spikes to 0g),
which made the plot too sparse - disconnected dashes instead of a
continuous-looking trace. Investigation confirmed the raw pressure data
genuinely drops toward 0g between passes, and the existing single-sample
`create_full_trace_plot_distance()` plot already renders this (as thin,
easy-to-miss near-vertical excursions at each of the ~45 pass boundaries,
since distance stays flat while pressure crashes during lift-off/return).
Reverted `plot_full_traces_comparison()` to plot each sample's **complete**
raw trace - i.e. the comparison plot is now literally multiple
`create_full_trace_plot_distance()`-style traces overlaid on one set of
axes, matching the reference single-sample plot's appearance. Also removed
the now-unused `PLOTTING_PARAMS$comparison_distance_limit_mm` (no fixed
x-axis window; each sample's line naturally extends to its own total
distance).

### Version 1.2-CONSOLIDATED - Comparison Plots Switched to Distance (2026-07-28)

- `plot_full_traces_comparison()` (the `50g_samples_full_traces.png` /
  `200g_samples_full_traces.png` cross-sample overlays) and
  `plot_stable_regions_overlay()` (the per-sample `*_stable_overlay.png`
  plots) now plot against **cumulative swabbing distance** instead of
  elapsed time.
- This removes the need for the old time-alignment hacks
  (`align_to_first_zero` for 200g, "align to first stable region start"
  for 50g) - distance naturally starts at 0 for every sample, so traces
  line up on the x-axis without any special-casing.
- `load_raw_trace_data()` gained a `compute_distance` option that
  recomputes `distance_mm` for a raw trace using a given sample's
  discovered adaptive stable range (the same range `process_sample()`
  used originally), via the existing `calculate_cumulative_distance()`
  function.
- `process_sample()` now returns `stable_range_low`/`stable_range_high`
  in its result list so the comparison-plot functions can recompute
  distance consistently with what was used for that sample's main
  analysis.
- New `PLOTTING_PARAMS$comparison_distance_limit_mm` (default 450mm, i.e.
  first 5 passes) and `PLOTTING_PARAMS$overlay_distance_limit_mm`
  (default 90mm, one nominal pass) control the x-axis window for the two
  plot types respectively.
- `plot_full_traces_comparison()` now only considers samples with
  `success == TRUE` (previously it attempted to load raw traces for
  failed samples too, even though they have no analysis results to
  overlay meaningfully against).

### Version 1.1-CONSOLIDATED - Single-Script & Single-Doc Consolidation, Bug Fixes (2026-07-28)

#### Documentation Consolidation
- Merged `README.md`, `SCRIPT_REFERENCE_GUIDE.md`,
  `DISTANCE_BASED_APPROACH.md`, `DOCUMENTATION_INDEX.md`, and this
  changelog into this single `README.md` file. The old Quick Start
  content (which described the long-superseded contact-tolerance-band
  script, `analyze_pressure_traces_v2.R`) was rewritten to accurately
  describe the current `time_based_batch_process_adaptive_DISTANCE.R`
  pipeline instead of being carried over verbatim.

#### Repository Cleanup
- Initialized git version control for the project (previously no VCS).
- Moved 40 superseded/one-off scripts into `archive/`: all prior detection
  algorithm versions (pass-based, plateau, pointchange, microplateau,
  stability, sustained-pressure) and per-sample debug/diagnose/test scripts
  (PILOT_013, PILOT_021, PILOT_009 investigations, etc.). Only
  `time_based_batch_process_adaptive_DISTANCE.R` remains as the active
  pipeline at the project root.
- Fixed `RUN_BATCH_ANALYSIS.bat`, which was still pointing at the
  long-superseded `batch_process_v4.R` and describing v4 outputs.

#### Single-Script Consolidation
- `test_distance_single_sample.R` and `test_distance_PILOT_001.R` were
  folded into `time_based_batch_process_adaptive_DISTANCE.R` as a new
  `test_single_sample()` function, selectable via `RUN_MODE <- "single"`.
  The script is now fully self-contained: it does not `source()` any other
  file, and no other file sources it.

#### Bug Fixes
- **Adaptive tolerance config was dead code.** `PARAMS$adaptive_tolerance_200g`
  (10g) and `PARAMS$adaptive_tolerance_50g` (8g) were defined but never
  referenced; `calculate_adaptive_thresholds()` hardcoded a single 5g
  minimum tolerance for all samples regardless of pressure level. Fixed to
  use the pressure-level-specific minimum. This changes computed stable
  ranges (and therefore Stable_Time_Pct / cycle counts) for samples whose
  natural 2xSD tolerance was below the new minimum - verified via full
  batch re-run: all 22 previously-successful samples remained successful
  with the same 2 known failures (PILOT_014, PILOT_018) unchanged; changes
  were bounded to a few percentage points of stable time and at most +1-2
  detected cycles per sample.
- **`PARAMS$adaptive_fallback_to_defaults` was ignored.** `process_sample()`
  always fell back to config default ranges when adaptive discovery/
  threshold calculation failed, regardless of this flag. Now respected -
  when `FALSE`, a failed discovery/calculation marks the sample as failed
  instead of silently using defaults.
- Removed leftover `DEBUG:` `cat()` statements in `process_sample()`.
- Added divide-by-zero/NaN guards (with explicit warnings) for
  `pct_stable`, `pct_stable_distance`, `mean_distance_per_cycle`,
  `distance_efficiency_actual`, and the `estimated_passes` diagnostic in
  `calculate_cumulative_distance()`.
- `calculate_statistics()` now receives `gcode_params` as an explicit
  argument instead of reading the global `GCODE_PARAMS`, matching the
  pattern used by every other function in the script.
- Annotated `GCODE_PARAMS` fields that are documentation-only (e.g.
  `lift_speed_mm_s`, `pressure_params`, `contact_time_s`) vs. actually used
  in calculations (`swab_speed_mm_s`, `swab_distance_mm`,
  `total_distance_theoretical_mm`), so future readers aren't misled into
  thinking every field is a live configuration knob.

---

### Version 2.2 - Tolerance Band Implementation (2026-07-16)

> Note: this and the versions below describe the older contact-tolerance-band
> script (`analyze_pressure_traces_v2.R` and predecessors), which has since
> been superseded by the time-based/distance-based pipeline described
> above and is now kept in `archive/` for historical reference.

#### Major Change: Contact Detection Algorithm Revised

**Previous (v2.0-2.1):** Threshold-based detection
- Detected contact when pressure exceeded a percentage of target
- Captured everything above threshold (ramp-up, peak, ramp-down, over-pressure)

**Current (v2.2):** Tolerance band detection
- **Detects contact when pressure is within +-10% of target**
- 200g samples: Contact when **180g <= pressure <= 220g**
- 50g samples: Contact when **45g <= pressure <= 55g**

#### Rationale

This approach identifies periods where the swab is being applied at the
intended target pressure (within acceptable tolerance), not just when it's
touching the surface. This is more appropriate for:
- Quality control: Shows how well operator maintains target pressure
- Process validation: Quantifies time spent at correct pressure
- Comparison: Allows meaningful comparison between samples and operators

#### What This Captures

**Included (within tolerance band):**
- Stable swabbing at target pressure +-10%
- Small variations around target (expected during manual operation)

**Excluded (outside tolerance band):**
- Initial ramp-up phase (approaching target)
- Over-pressure periods (pressing too hard)
- Under-pressure periods (lifting off or too light)
- Air movement (zero pressure)

#### Expected Results

**Contact percentage will be lower** than previous versions:
- v2.0 (70% threshold): ~65-75% contact time
- v2.1 (10% threshold): ~85-95% contact time
- **v2.2 (+-10% tolerance): ~40-60% contact time** (more selective)

**Mean contact pressure will be closer to target:**
- Previous versions included ramp periods (lower average)
- Tolerance band focuses on stable target pressure maintenance
- Should see mean very close to target (e.g., 195-205g for 200g target)

**CV% may be lower:**
- Narrower pressure range captured
- Reflects stability at target, not overall variability

#### Visualization Changes

**Plots now show two reference lines:**
- Upper bound (dotted): 220g for 200g target (110%)
- Lower bound (dotted): 180g for 200g target (90%)
- Contact periods are data points between these lines

**Plot subtitles updated:**
- "Contact periods within +-10% of target pressure"

#### Impact on Analysis

This is a **fundamental change** in what "contact" means:
- **Before:** Contact = swab touching surface (any pressure above threshold)
- **After:** Contact = swab at correct target pressure (within tolerance)

The new approach is more appropriate for evaluating:
- Operator technique and consistency
- Equipment performance
- Process reproducibility
- Adherence to protocol

---

### Version 2.1 - Adjusted Contact Threshold (2026-07-16) [SUPERSEDED]

#### Changes
- Contact threshold reduced from 70% to 10% of target pressure
- This version has been superseded by v2.2 tolerance band approach

---

### Version 2.0 - Contact-Only Analysis Implementation (2026-07-16)

#### Major New Features

##### 1. Contact Period Detection
- **Tolerance band detection**: Identifies when pressure is within +-10% of target
  - 200g samples: Contact when 180g <= pressure <= 220g
  - 50g samples: Contact when 45g <= pressure <= 55g
- **Gap merging**: Automatically merges contact periods separated by < 0.2 seconds
  - Handles brief sensor dropouts or minor variations
- **Pass counting**: Identifies and counts distinct periods at target pressure

##### 2. New Functions Added

**`identify_contact_periods(data, target_pressure_g, threshold_pct, merge_gap_sec)`**
- Detects contact periods using threshold method
- Merges nearby segments based on time gap
- Returns data with `is_contact` and `pass_id` columns

**`calculate_contact_statistics(data, target_pressure_g)`**
- Calculates statistics for contact periods only
- Returns comprehensive metrics including:
  - Mean, SD, CV%, min, max, median contact pressure
  - Number of observations and passes
  - Total contact time and % time in contact
  - Mean pass duration

##### 3. Enhanced Visualizations

**Individual Plots** (now 2 per sample):
- `*_full_trace.png` - Complete time series
- `*_contact_only.png` - Contact periods only
  - Connected dots/points showing discontinuity
  - Lines connect points within each pass
  - Auto-scaled Y-axis to contact pressure range
  - Dotted reference line showing 70% threshold

**Grouped Plots** (now 16 total):
- Full trace versions (4 plots)
- Contact-only versions (4 plots)
- Zoomed full trace (400-500s, 4 plots)
- Zoomed contact-only (400-500s, 4 plots)

Each grouped plot:
- Color-coded by sample ID
- Shows all samples in same pressure/surface category
- Auto-scaled Y-axis for contact-only plots
- Threshold reference line

##### 4. Statistics Focus Shift

**Contact-Only Statistics Now Primary:**
- All statistics calculated from contact periods only
- More accurate representation of actual swabbing pressure
- Excludes lift-off, air movement, and approach phases

**New Metrics:**
- `mean_contact_pressure_g` - Average pressure during contact
- `cv_contact_percent` - Coefficient of variation during contact
- `percent_time_in_contact` - Percentage of trace spent in contact
- `number_of_passes` - Count of distinct contact periods
- `mean_pass_duration_sec` - Average duration of each pass
- `contact_threshold_used_g` - Threshold value used for detection

**Output Files:**
- `statistics/sample_statistics_contact_only.csv`
- `statistics/pressure_level_summary_contact.csv`
- `statistics/surface_type_summary_contact.csv`
- `statistics/pressure_surface_summary_contact.csv`

##### 5. Enhanced Diagnostics

**During Data Loading:**
```
Reading PILOT_001... OK (4093 rows)
  Contact: 2847 points (69.5%), 12 passes
```

**Console Output:**
- Shows only contact-based statistics
- Full-trace statistics hidden (not calculated)
- Clear indication of threshold parameters used

#### Technical Details

##### Contact Detection Algorithm
1. Calculate tolerance band: `lower = target * 0.90`, `upper = target * 1.10`
2. Apply band: `is_contact = (load_g >= lower) & (load_g <= upper)`
3. Identify consecutive contact segments
4. Calculate gaps between segments
5. Merge segments if gap < 0.2 seconds
6. Assign unique `pass_id` to each merged segment

##### Visualization Approach
**Contact-Only Plots:**
- Use `geom_point()` for individual data points
- Use `geom_line(aes(group = pass_id))` to connect within passes
- Creates visual gaps showing non-contact periods
- Y-axis auto-scales to contact pressure range only

#### File Structure Changes

**New Directory:**
```
outputs/
  statistics/          # New subdirectory for CSV files
```

**Plot Files:**
- Individual: 16 files (8 full + 8 contact-only)
- Grouped: 16 files (4 full + 4 contact + 4 zoomed full + 4 zoomed contact)
- **Total: 32 PNG files**

**Statistics Files:**
- All CSV files now in `statistics/` subdirectory
- All statistics based on contact periods
- File naming updated to reflect contact-only nature

#### Breaking Changes

1. **Statistics calculation completely replaced**
   - Old `calculate_statistics()` function removed
   - Now uses `calculate_contact_statistics()`

2. **CSV file locations changed**
   - Moved from `outputs/*.csv` to `outputs/statistics/*_contact.csv`

3. **Console output format changed**
   - Shows contact-only statistics
   - Full-trace statistics not displayed

4. **Plot file naming**
   - Grouped plots: `*_combined_*.png` -> `*_full.png` or `*_contact_only.png`

#### Parameters

**Configurable (in function calls):**
- `tolerance_pct = 0.10` - Tolerance band (+-10% of target)
- `merge_gap_sec = 0.2` - Maximum gap to merge (0.2 seconds)

**Fixed:**
- Zoomed window: 400-500 seconds
- Contact detection method: Threshold-based
- Visualization style: Connected dots/points

#### Output Summary

**Total Files Generated:**
- 32 PNG plot files
- 4 CSV statistics files

**Comprehensive Analysis:**
- 8 samples analyzed
- ~40-60% of trace time at target pressure +-10% (typical with tolerance band)
- Variable number of passes depending on operator technique
- Quantifies time spent at correct target pressure

---

### Version 1.0 - Initial Release (2026-07-16)

#### Initial Features
- Basic data loading from CSV files
- Full trace visualization
- Simple statistics calculation
- Grouped plots by pressure/surface type

#### Known Issues (Fixed in v2.0)
- Statistics included non-contact periods
- No distinction between swab contact and air movement
- CSV parsing issues with some file formats
