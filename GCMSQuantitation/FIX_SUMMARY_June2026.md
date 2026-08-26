# Fix Applied: Report Extrapolated Concentrations Below Calibration Range

**Date**: June 10, 2026  
**Issue**: Negative controls showing "Quantifiable" SNR flag but NA concentration  
**Root Cause**: Code was filtering out negative roots from quadratic extrapolation  
**Solution**: Remove negative filter, floor concentrations at zero

---

## Changes Made

### 1. Code Fix: `Code/03_Quantification.R`

**Location**: Lines 98-112 (`solve_concentration()` function)

**Before**:
```r
roots <- roots[is.finite(roots) & roots >= 0]

if (length(roots) == 0) {
  return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
}

list(value = min(roots), method = "Quadratic", range_flag = range_flag)
```

**After**:
```r
roots <- roots[is.finite(roots)]

if (length(roots) == 0) {
  return(list(value = NA_real_, method = "Unquantifiable", range_flag = range_flag))
}

# Floor concentration at zero (negative extrapolations reported as 0)
concentration <- max(0, min(roots))

list(value = concentration, method = "Quadratic", range_flag = range_flag)
```

**Effect**:
- Negative roots are no longer filtered out (removed `& roots >= 0` check)
- Concentration is floored at zero using `max(0, min(roots))`
- Negative controls will now show `0.000 ng` instead of `NA`
- Range flag "Below_range" provides clear context

---

### 2. Documentation Update: `CONTEXT.md`

**Location**: Line 239

**Updated text**:
```markdown
**Changed behaviour** (Updated June 2026):
- `solve_concentration()` now **computes and reports the extrapolated concentration** for out-of-range responses
- A new `range_flag` return value tracks the range status: `Within_range`, `Below_range`, or `Above_range`
- **Negative extrapolations are floored at zero** (reported as `0.000 ng` with "Below_range" flag)
  - Prevents physically meaningless negative concentrations
  - Clearly indicates trace/not detected while maintaining numeric output
  - Relevant for negative controls with detectable peaks below calibration range
```

---

## Expected Results After Re-processing

### Example: Lab10 P1 S NC (Steel Negative Control)

**Current output** (line 15 in Lab10-1_GCMSResults.csv):
- `rdx_snr_flag` = "Quantifiable"
- `rdx_concentration` = **NA** ❌
- `rdx_range_flag` = "Below_range"

**Expected output after fix**:
- `rdx_snr_flag` = "Quantifiable" (unchanged)
- `rdx_concentration` = **0.000** or very small value (e.g., 0.001) ✓
- `rdx_range_flag` = "Below_range" (unchanged)

### Other Negative Controls Expected to Change

All NCs with detectable RDX peaks below calibration range:
- Lab10 P1 S NC (Steel) - line 15
- Lab10 P1 ABS-S NC (ABS-Smooth) - line 33
- Lab10 P2 S NC (Steel P2) - line 51
- Lab10 P2 ABS-S NC (ABS-Smooth P2) - line 69

---

## To Apply This Fix

### Step 1: Re-run Quantification for Lab10-1

**In RStudio**:
```r
# Load configuration
source('C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/GlobalCode.R')

# Run quantification (this will overwrite existing results)
source('C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Code/03_Quantification.R')
```

**Expected output**:
- New `Lab10-1_GCMSResults.csv` with concentrations for NCs
- Updated calibration plots
- Updated QC accuracy plots

### Step 2: Verify the Fix

**Check negative control concentrations**:
```r
# Read results
results <- read.csv("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/Lab10-1/Results/Lab10-1_GCMSResults.csv")

# Filter to negative controls
nc_data <- results %>%
  filter(Type == "Sample", grepl("NC", SampleName)) %>%
  select(SampleName, rdx_snr_flag, rdx_concentration, rdx_range_flag)

print(nc_data)
```

**Expected output**:
```
                SampleName rdx_snr_flag rdx_concentration rdx_range_flag
1 Lab10 P1 S NC           Quantifiable         0.000xxx    Below_range
2 Lab10 P1 ABS-S NC       Quantifiable         0.000xxx    Below_range
3 Lab10 P2 S NC           Quantifiable         0.000xxx    Below_range
4 Lab10 P2 ABS-S NC       Quantifiable         0.000xxx    Below_range
```

### Step 3: Re-run Study Collation

Once Lab10-1 is re-processed, regenerate the study-level results:

```r
source('C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/FINEX/04_CollateStudyResults.R')
```

This will update `FINEX_StudyResults.csv` and `FINEX_StudyResults.xlsx` with the new NC concentrations.

### Step 4: Check Other Labs (Optional)

If other labs have similar issues (NCs with "Quantifiable" but NA concentration), re-process them:

1. Update `DataFolder` in GlobalCode.R
2. Run 03_Quantification.R
3. Check results
4. Re-run 04_CollateStudyResults.R when all labs are updated

---

## Testing Checklist

- [ ] Lab10-1 re-processed successfully
- [ ] Negative controls now show numeric concentrations (0.000 or small values)
- [ ] Range flags remain "Below_range" for NCs
- [ ] Regular samples (within range) unchanged
- [ ] QC performance unchanged
- [ ] Study results collation updated
- [ ] Excel file shows consistent data

---

## Rollback Instructions (if needed)

If this change causes issues, revert the code:

```r
# In Code/03_Quantification.R line 103, change back to:
roots <- roots[is.finite(roots) & roots >= 0]

# And remove lines 111-112:
# concentration <- max(0, min(roots))
# Replace with:
list(value = min(roots), method = "Quadratic", range_flag = range_flag)
```

---

## Design Rationale

**Why floor at zero instead of reporting negative values?**
- Negative concentrations are physically meaningless
- Zero clearly indicates "trace/not detected"
- Standard practice in analytical chemistry
- "Below_range" flag provides necessary context

**Why not keep returning NA?**
- Inconsistent: peak detected ("Quantifiable") but no value
- Users expect numeric output when detection succeeds
- Downstream analysis can filter on `range_flag == "Within_range"` if needed

**Why not set minimum reportable (e.g., LOQ)?**
- LOQ is study-specific and not universally defined
- Zero is unambiguous and allows user interpretation
- Users can apply their own LOQ thresholds in post-processing
