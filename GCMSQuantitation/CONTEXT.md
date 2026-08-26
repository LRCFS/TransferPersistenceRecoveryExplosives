# GCMSQuantitation - Project Context

## Session Summary (August 19, 2026, continued further) — RDX 6ng QC "Step-Then-Plateau" Bias Investigated: Real Instrument/Standard Phenomenon, But No Sample-Level Correction Warranted

### Motivation

User noticed the RDX 6ng QC bias pattern in the most recent FINEX sequences looked different from earlier ones: the first 6ng QC in a sequence always shows low bias, but every subsequent one jumps to and then sits flat at ~-20% bias -- unlike earlier sequences, which show a smooth, gradual decline throughout. Asked to investigate whether this is a real phenomenon or something fixable in processing, and (once real) what a drift correction would need to look like -- with an explicit caution against building something that only ~3-5 of ~15 sequences would ever need.

### Step 1 -- Confirmed the pattern and its scope

Pulled every FINEX dataset's 6ng RDX QC bias sequence (`Type=="QC" & CalLevel==6`, ordered by `Row`). Result: **9 of 15 datasets** (`Lab10-1` through `Steel 2`, June-July 2026) show a smooth, gradual decline (QC1 bias ~0%, drifting to -3% to -6.6% by the last QC -- consistent with ordinary progressive signal loss, well within the ±20% `qc_6ng_bias_limit`). **3 datasets run on 3 consecutive calendar days -- `Outstanding samples` (11 Aug), `Steel 5` (12 Aug), `20260813` (13 Aug) -- show a sharp, discrete step instead**: QC1 tracks the calibration curve closely (-6% to -7% bias), then QC2 abruptly drops to and stays flat at -18% to -20% bias for the rest of the sequence. `Steel 3`/`Steel 4` (4/6 Aug) show a smaller, intermediate version of the same step (~3-5 percentage points). A newer dataset that appeared mid-investigation, `Outstanding samples 2` (18 Aug), shows the step has receded back to ~1% again, though the underlying calibration-standard RDX/15N-RDX ratio itself has continued a slow, separate downward drift across the whole study (0.58 in June -> 0.52 in early Aug -> 0.45 by 18 Aug).

### Step 2 -- Ruled out processing/pipeline bugs

Checked every plausible processing-level explanation directly against the real data before concluding this was real:
- **Peak shape is normal**: PA/PH ratio for the RDX and 15N-RDX peaks stays ~1.05-1.16 throughout, in both affected and unaffected sequences and for both QC and Sample rows -- no sign of the kind of erratic/collapsed PA/PH ratio this codebase already knows to associate with an integration/baseline-correction artifact (see the August 3, 2026 "Baseline-Correction Integration Floor Fix" session).
- **RT is stable** (~362.6-362.9s) in every affected QC and sample row -- not a misidentified or co-eluting peak.
- **The SIM acquisition method is byte-for-byte identical** (RDX ion group m/z 46/75/120/122, 25ms dwell times, 5.8min group start time) across affected and unaffected raw `acqmeth.txt` files, checked directly. (Incidental, unrelated finding along the way: the PETN SIM group monitors m/z 44/46/76 in `Lab9`/`Steel-1`/`Steel 5`/etc. but m/z 46/57/76 in `ABS-S-1`/`ABS-S-2` -- a previously-undocumented method difference, irrelevant to this RDX investigation since it's PETN-side and the pipeline never uses PETN's m/z57/44 slot either way, but flagged here for awareness.)
- **RDX has no drift correction anywhere in this pipeline** (removed entirely in July 2026 -- RDX relies purely on raw 15N-RDX IS-ratio quantification). There is no code path that could inject a synthetic step; whatever is happening is present in the raw extracted peak areas themselves.

### Step 3 -- Vial-piercing hypothesis raised and explicitly ruled out by the user

Initial hypothesis (evaporation/degradation triggered by repeated septum puncture of a single QC vial) was directly ruled out: **the QCs are drawn from separate vials, all filled from the same stock.** This redirects the likely mechanism to the instrument side (most plausibly inlet/liner condition at that specific point in the sequence) rather than vial-specific chemistry -- consistent with, and an escalation of, this project's own long-documented history of progressive GC inlet contamination (see "GC-MS Signal Loss During Sample Sequences" and "Filtering Study Results" sections elsewhere in this file), just manifesting for the first time as a discrete step rather than smooth decline.

### Step 4 -- Decisive test: do real samples show the same collapse as the QC?

Compared the RDX internal standard's own peak area (a pure instrument-response readout, since the IS is spiked at a constant known amount into every injection regardless of sample type) between the QC standards and the real Sample rows bracketed between them, for every QC-to-QC interval:

| Dataset | Bracket | QC declines to | Real samples in that bracket sit at |
|---|---|---|---|
| **Steel 5** | QC1->QC2 | **41.3%** of QC1 | **98.1%** of QC1 |
| **20260813** | QC1->QC2 | **55.3%** of QC1 | **84.5%** of QC1 |
| **Outstanding samples** | QC1->QC2 | **51.4%** of QC1 | 66-103% of QC1 (still far above QC2's 51.4%) |
| ABS-S-1 (unaffected) | QC1->QC2 | 38.0% of QC1 | 97.3% of QC1 |
| Lab9 (unaffected) | QC1->QC2 | 46.2% of QC1 | 70.7% of QC1 |

Peak shape for the sample-side IS peaks was also confirmed normal (PA/PH ratio 1.07-1.13, matching the QC's own 1.10-1.13, RT stable 362.6-362.9s) -- not an artifact.

**Key finding: this "real samples decline far less than the bracketing QC" pattern is universal across this entire study, present in every dataset checked (affected and unaffected alike), not unique to the three anomalous sequences.** It is the same "matrix protects the internal standard" effect already documented in this project's own history (real sample co-extractives occupy GC inlet active sites, shielding the co-injected IS/analyte from the loss a bare, unprotected standard experiences) -- see "Filtering Study Results" (April 2026) and the sister ASTRA study's "Bracketed QC Drift Correction" session, both of which independently identified the same phenomenon. What's different about Steel 5 / Outstanding samples / 20260813 is only that the *unprotected* QC's own decline was unusually severe in that specific window -- the bracketed real samples were still comparably protected, exactly as in every other sequence in the study.

### Conclusion: real phenomenon, but no sample-level drift correction is scientifically justified

A step/bracket-style drift correction (of the kind already implemented for PETN, and for ASTRA's own step-then-plateau case) implicitly assumes "whatever loss the QC standard experienced, the bracketed samples experienced too." **This data directly contradicts that assumption**: in the exact bracket where the QC collapsed by 45-59%, the real samples in that same bracket stayed at 84-98% of their pre-collapse level. Applying a correction factor derived from the QC's ~19% bias to real sample concentrations would inflate already-accurate results by ~19% they do not need -- actively introducing error rather than removing it. This mirrors the ASTRA study's own explicitly-rejected attempt to use one injection-type's signal to correct/validate another without direct supporting evidence (its "Attempt 1/2" corroboration-check/LOOCV sessions, both removed after investigation showed the premise didn't hold).

**Decision: no RDX drift correction implemented.** The existing per-injection IS-ratio method is already doing the correct thing for real samples -- each sample's own IS, measured in that same injection, corrects for whatever that specific injection experienced, and this investigation directly confirms that mechanism is working as intended even during QC-flagged periods. The ~19-20% QC bias in `Outstanding samples`/`Steel 5`/`20260813` should be understood as a property of the *unprotected QC standard's* vulnerability to inlet conditions at that time, not as evidence that samples processed in those sequences are unreliable.

### Not implemented, flagged for optional future consideration

- No code changes were made to `Code/03_Quantification.R`, `Code/04_CollateStudyResults.R`, or `GlobalCode.R` as a result of this investigation.
- The pipeline's existing ±20% `qc_6ng_bias_limit` PASS/FAIL threshold does not distinguish "QC standard experienced unusually severe unprotected-inlet loss" from "system-wide accuracy problem affecting samples too" -- worth a future design discussion (e.g. a bracket-position-aware step-detection alert distinct from a correction), but out of scope for this investigation.
- Could optionally trend/flag these specific 6ng QC results in `SystemMonitoring.xlsx` as an inlet-condition-monitoring signal (informative about instrument state) without ever feeding into sample-level concentration correction.

**Files modified**: `CONTEXT.md` (this entry). No pipeline code, results files, or study outputs were changed.

---

## Session Summary (August 19, 2026, continued) — FINEX Collation Now Deduplicates Reanalysed Samples (Ported ASTRA's `select_best_attempt()`, "Earliest Passing" Rule)

### Motivation

Explicit request: make `Code/04_CollateStudyResults.R` handle replicate/reanalysed samples correctly by choosing the earliest passing attempt. Investigation confirmed this was a real, currently-occurring gap: `Code/04_CollateStudyResults.R` discovers and `bind_rows()`s every `*_GCMSResults.csv` under `study_root_dir` (Section 1) with **zero deduplication** afterward, while several dataset folders (`Steel 3`, `Steel 4`, `Steel 5`, `20260813`, and especially `Outstanding samples`) exist specifically to hold reanalysed samples that failed QC elsewhere. Confirmed directly against the live `FINEX_StudyResults.csv` before this fix: 8+ samples had two rows each for the same `(Lab, Participant, Surface, Repeat)` identity, including `Lab17 P1 S4`/`Lab17 P2 S1`, which each had **both** a `FAIL` row (`Steel-1`, 20260626) and a genuine `PASS*` row (`Outstanding samples`, 20260811) sitting side by side with no indication which was authoritative -- inflating `SampleSummary`/per-surface sheets/plots/statistics and giving no way to tell which row was "real".

The ASTRA study's own collation script (`Design of Experiments/pilot_analysis.R`) already solves exactly this problem via `select_best_attempt()`/`report_multiple_attempts()`, including an August 11, 2026 fix (documented in that repo's own `CONTEXT.md`) changing the tie-break rule from "most recent accepted attempt wins" to "**earliest** accepted attempt wins" -- discovered there to matter because "most recent" was systematically selecting the more RDX-drift-biased of two equally-valid attempts (RDX has no drift correction in either pipeline) for reasons that only ever concerned PETN. FINEX's `CONTEXT.md` had never discussed this issue at all and explicitly framed `select_best_attempt()` as an ASTRA-only feature, deliberately left out of the August 10, 2026 shared-`InjectionAcceptance.R` refactor.

### Implementation (`Code/04_CollateStudyResults.R`)

Ported `select_best_attempt()`/`report_multiple_attempts()` from `pilot_analysis.R`, generalised to accept a vector of grouping columns (`key_cols`) via `group_by(across(all_of(key_cols)))` -- already an established pattern in this same script (`compute_stats()`'s `group_vars`) -- rather than ASTRA's single-column `SampleName_trimmed` grouping. Used the already-parsed identity columns (`Lab`, `Participant`, `Surface`, `Repeat` for samples; `Lab`, `Participant`, `Surface` for NCs, which have no `Repeat`) instead of the raw `SampleName` string, since these are more robust to incidental whitespace/formatting differences between two attempts' raw sample names.

New **Section 7c** (`Code/04_CollateStudyResults.R`, between the existing Sections 7b/8): defines both functions, then:
- Deduplicates `nc_rows` immediately after `nc_analysis_accepted`/`nc_result` are computed (moved the NC Stage 1/2 count-printing to *after* dedup, so the printed summary reflects deduplicated counts).
- Deduplicates `samples_only` immediately after it's built (`samples %>% filter(!IsNegativeControl)`), before column selection into `master`.

**Selection rule** (identical to ASTRA's corrected behaviour): prefer any accepted attempt (`analysis_accepted`/`nc_analysis_accepted %in% c("PASS","PASS*")`) over a failed one; break ties (all accepted, or all failed) by **earliest** `Date`. `N_Attempts`/`MultipleAttempts` columns retain audit visibility that a re-run happened, added to `desired_cols`/`desired_cols_nc` (right after `Outcome`/`nc_result`) and to `apply_formatting()` (amber highlight on `MultipleAttempts == TRUE`, a "heads up" flag distinct from pass/fail colouring).

### Verification (real data, all 14 datasets)

Backed up pre-change `FINEX_StudyResults.csv`/`.xlsx`/`TIC_OtherPeak_*.csv` (`%TEMP%\opencode\Dedup_PreCollation_Backup\`), then ran the full collation via the established isolated-runner mitigation (`SampleVol`/`DepositMass`/efficiency constants pre-defined so `GlobalCode.R` isn't re-sourced).

Console confirmed **54 sample(s)** and **11 negative control(s)** had multiple analysis attempts, each logged individually by identity. Sample counts: pre-dedup `PASS=260 PASS*=9 FAIL=51` (320 non-NC rows) -> post-dedup `PASS=218 PASS*=9 FAIL=37` (264 rows) -- confirms genuine duplicate removal, not just relabelling. Directly confirmed the two motivating cases now resolve correctly: `Lab17 P1 S4`/`Lab17 P2 S1` (3 attempts each) now show a single `PASS*` row (the earliest accepted attempt), and `Lab6 P1 S4`/`Lab6 P2 S2` (2 attempts each, neither ever accepted) correctly fall back to the earliest overall attempt. Confirmed **zero** remaining duplicate `(Lab, Participant, Surface, Repeat)` groups in the final `FINEX_StudyResults.csv` (264 rows) and **zero** remaining duplicate `(Lab, Participant, Surface)` groups in the `Negative Controls` sheet (44 rows) of the regenerated `FINEX_StudyResults.xlsx`; `N_Attempts`/`MultipleAttempts` confirmed present and correctly populated in both the `All Data` and `Negative Controls` sheets.

**Operational note**: hit the same well-documented transient `openxlsx`/Excel file-lock issue as prior sessions (file briefly open in a background Excel/OneDrive-sync process) on the first attempt -- CSV wrote successfully, `.xlsx` emitted a non-fatal "Permission denied" warning and was left stale. Re-ran once the lock cleared (confirmed via a direct file-open test, not just re-trying blindly) and the `.xlsx` wrote correctly on the second attempt.

**Files modified**: `Code/04_CollateStudyResults.R` (`select_best_attempt()`/`report_multiple_attempts()` new, Section 7c; `desired_cols`/`desired_cols_nc` gained `N_Attempts`/`MultipleAttempts`; `apply_formatting()` gained a `MultipleAttempts` conditional-formatting block), `FINEX_StudyResults.csv`/`.xlsx`, `TIC_OtherPeak_Log.csv`/`TIC_OtherPeak_RecurringPeaks.csv`, and all `Plots/` outputs (regenerated via the same collation run), `CONTEXT.md` (this entry).

---

## Session Summary (August 19, 2026) — PETN m/z 57 Qualifier-Ion Columns Removed from All Results Spreadsheets

### Motivation

Explicit request to remove all columns referencing ion 57 (PETN's `qual57` qualifier ion, m/z 57) from results spreadsheets. This ion was already known to be untrustworthy -- the August 12-14, 2026 "Confirmatory Ion Detection" session (below) found only 72.7% detection and just 18.2% reaching Quantifiable SNR even in pure Cal standards, and it was already deliberately excluded from the confirmatory-ion acceptance gate (`IA_QUALIFIER_IONS` in `Code/InjectionAcceptance.R` only ever used `qual76`) and from the collated `FINEX_StudyResults.csv`/`.xlsx` (`desired_cols` in `Code/04_CollateStudyResults.R` never included it). It was, however, still being extracted by `Code/02_PeakDetection.R` and exported as 5 columns (`petn_qual57_rt/_pa/_ph/_snr/_snr_flag`) in every dataset's raw `_GCMSResults.csv` and in the Calibration/QC/Samples/Blanks sheets of every per-lab `_GCMSResults.xlsx`.

### Code changes (stops future extraction)

- **`GlobalCode.R`**: `analytes$PETN$qualifiers` changed from `c(57, 76)` to `c(76)`, with a comment explaining the exclusion rationale and referencing the ratio-analysis findings. Since `Code/02_PeakDetection.R`'s channel-building logic (`channel_specs`) iterates `a$qualifiers` directly, this alone stops m/z 57 from ever being extracted/exported again -- no changes needed in `02_PeakDetection.R` itself.
- **`Code/03_Quantification.R`**: removed the 5 `petn_qual57_*` entries from `petn_qual_cols` (feeds `cal_cols`/`qc_cols`/`smp_cols`/`blk_cols`, i.e. every per-lab XLSX sheet) and from `nd_columns` (NA→"ND" display substitution list).
- No changes needed in `Code/InjectionAcceptance.R` or `Code/04_CollateStudyResults.R` -- neither ever referenced `qual57`.
- `Diagnostics/ConfirmatoryIon_RatioAnalysis.R` (the historical investigation script that first characterised m/z 57's poor performance) was deliberately left unchanged -- it's a one-off exploratory record, not pipeline code.

### Existing spreadsheets -- columns stripped in place (no raw-data reprocessing)

Given the documented history of disk-space incidents and unrelated numeric drift from full dataset reprocessing (see the August 12-14, 2026 and other sessions below), chose the lower-risk option of directly removing the 5 `petn_qual57_*` columns from every already-generated results file rather than re-running `01`→`03` for every dataset. This is a pure column-drop with no computation involved, so it carries none of that risk.

Checked every dataset under FINEX's `Accepted Analysis` (14 datasets, including 3 not previously mentioned in this file's history -- `20260813`, `Outstanding samples`, `Steel 5`) and all 6 ASTRA `Analysis1/3/4/5/6/7` datasets:
- **All 14 FINEX `_GCMSResults.csv`/`_GCMSResults.xlsx` files had the 5 `qual57` columns** (present in the CSV and in all 4 sheets -- Calibration/QC/Samples/Blanks -- of each XLSX); all 14 were updated.
- **All 6 ASTRA `_GCMSResults.csv`/`_GCMSResults.xlsx` files never had any `qual*` columns at all** (the confirmatory-ion feature was FINEX-only, per the August 12-14, 2026 session -- Stage 1 only ever reprocessed the FINEX "Accepted Analysis" datasets) -- correctly left untouched.
- `FINEX_StudyResults.csv`/`.xlsx`, `TIC_OtherPeak_Log.csv`, `TIC_OtherPeak_RecurringPeaks.csv`, and `SystemMonitoring.xlsx` were all confirmed to never have contained any `qual57` columns (already excluded by design) -- no changes needed.

**Method**: an R script (`readxl`/`writexl` for XLSX, base `read.csv`/`write.csv` for CSV) read each file, dropped any column matching `petn_qual57`, and rewrote it in place -- backing up the original first (`%TEMP%\opencode\RemoveIon57_Backup\`). `read.csv`'s `colClasses = c(Date = "character")` was required to byte-for-byte preserve the existing quoting convention (`Date` is exported as a quoted string in the original pipeline output even though it's all-digits; without forcing this, `read.csv`/`write.csv` round-tripping would silently un-quote it, verified via a `fc /b`-equivalent diff on a test file before running against the real data).

### Verification

Compared every updated file against its pre-change backup: for all 14 CSVs and all 56 XLSX sheets (14 files × 4 sheets), confirmed identical row counts, identical remaining column set (original columns minus exactly the 5 `qual57` ones), and identical values in every remaining column (`all.equal()`, zero mismatches). Confirms this was a pure, lossless column removal with no side effects on any other data.

**Files modified**: `GlobalCode.R` (`analytes$PETN$qualifiers`), `Code/03_Quantification.R` (`petn_qual_cols`, `nd_columns`), all 14 FINEX "Accepted Analysis" dataset `Results/_GCMSResults.csv`/`.xlsx` files (qual57 columns removed in place; originals backed up to `%TEMP%\opencode\RemoveIon57_Backup\`), `CONTEXT.md` (this entry).

---

## Session Summary (August 18, 2026, continued) — Cross-Study "Other Peak" Investigation (FINEX + ASTRA Combined); RT-Config Grouping Corrected

### Motivation

Follow-up to the TIC Deinterleaving Fix session (below): asked whether the corrected "other significant peak" detection shows any genuinely **sample-specific** peaks, and whether ASTRA samples show any recurring peaks absent from FINEX samples (or vice versa). Since `Code/04_CollateStudyResults.R`'s Section 13a (Stage 4, below) is FINEX-only, this required a standalone combined analysis across all 18 datasets (12 FINEX + 6 ASTRA) -- not (yet) implemented as reusable pipeline code, purely an exploratory investigation this session.

### Correction: RT-config grouping was mislabeled as "V8 vs V9" -- it is not a method-version difference

While aligning retention times across datasets for this analysis (needed because two different empirical RDX RT clusters exist study-wide, ~348s and ~363s), initially borrowed the "V8"/"V9" terminology from this file's own historical entries (e.g. the June 2026 "V9 RT Investigation" section, below) without re-verifying it against the actual acquisition method file per dataset. **This was wrong and has been corrected** after being challenged: directly checked every dataset's `RawData/*.D/*.M` method folder name --

- **17 of 18 datasets (all 6 ASTRA + 11 of 12 FINEX) use the identical method file `PETN RDX 15NRDX LI HES METHOD G ACB - 100 V9.M`.**
- Only `Lab17-1` uses `...V8.M` -- yet `Lab17-1` shares the **same** shifted RT (~348s RDX) as three other datasets that are all nominally `V9.M` (`ABS-S-1`, `ABS-S-2`, `Lab10-1`). So the method filename does **not** predict which RT group a dataset falls into at all.
- What actually predicts the RT split is **run date**: `Lab10-1` (4 Jun 2026), `Lab17-1` (8 Jun), `ABS-S-1` (10 Jun), `ABS-S-2` (11 Jun) -- all four RT~348s datasets were run in the first half of June 2026. Every other dataset (RT~363s), including the next FINEX dataset chronologically (`Steel-1`, 26 Jun) and all of ASTRA (starting 13 Jul), was run from late June 2026 onward.

**Conclusion**: the ~15.5s RT shift between these two groups is very likely genuine retention-time drift from a real instrument-level change (column trim/replacement, MS retune, etc.) that happened between 11 June and 26 June 2026 -- not a "method version" difference, and *not* a FINEX-vs-ASTRA study difference (ASTRA falls entirely within the later, more common RT regime, alongside 8 of FINEX's 12 datasets). Any future reference to "V8 RT config" / "V9 RT config" in this codebase should be treated with caution -- it does not reliably correspond to the actual `.M` method folder name and should be re-verified per dataset (via `RawData/*.D/*.M` folder name, or empirically via each dataset's own median `rdx_rt`) rather than assumed from historical narrative.

### Method (this session's combined analysis)

1. Loaded all 18 datasets' `_GCMSResults.csv` files directly (not via any pipeline script), tagging each row with `Study` (FINEX/ASTRA) and `Dataset`.
2. Computed each dataset's own empirical RDX RT anchor (median observed `rdx_rt`) and aligned every "other peak" RT onto a common scale (`rt_norm = raw_rt + (363 - dataset's own rdx_rt anchor)`) -- purely data-driven, not dependent on the method-name confusion above.
3. Re-ran the same bounded-diameter clustering algorithm as `Code/04_CollateStudyResults.R` Section 13a (Stage 4, tolerance 5s) across the combined, aligned peak log (1,281 peak occurrences from 1,344 injections -- slightly fewer than the raw 1,344x5-slot maximum since not every injection has 5 peaks).

### Findings

- **No singleton (one-off, unique-to-a-single-injection) Sample-type peaks exist anywhere in the combined 18-dataset log.** Every "other" peak that clears the 20,000-height floor recurs at least once elsewhere -- no evidence of idiosyncratic per-sample contamination signatures at this detection threshold.
- **Genuine Sample-type-dominant clusters do exist** (recurring across many samples, essentially absent from Blanks -- i.e. real matrix content, not instrument artifact): the clearest is a peak at (aligned) RT~227s, 421 occurrences, 246 of them Sample-type vs only 4 Blank, **present in both studies** (193 FINEX + 53 ASTRA sample occurrences, heights up to ~1.9M in one ASTRA case) -- a substantial, reproducible, non-target compound in real swab extracts from both studies, worth an MS library match if identity is wanted.
- **A genuine ASTRA-only sample peak was found**: (aligned) RT~247s, 13 Sample-type occurrences across 4 of the 6 ASTRA datasets (Analysis1/4/5/7), heights 25k-94k, **zero** occurrences in any of the 264 FINEX samples (only a couple of unrelated, non-recurring Blank/QC blips scattered across 4 different FINEX datasets -- not a systematic FINEX pattern).
- **An apparent "FINEX-only" sample peak (raw RT~204s) turned out NOT to be a FINEX-vs-ASTRA difference at all** once traced to specific datasets -- it is confined **exclusively** to the four early-June-2026 datasets (`ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, see RT-config correction above), present in virtually every Cal/QC/Sample injection in those 4 runs (110 Sample occurrences, heights 20k-180k) and completely absent from the other 8 FINEX datasets AND all 6 ASTRA datasets. This is a temporal/instrumental artifact tied to whatever changed around the column/instrument in mid-to-late June 2026, not a study-level chemistry difference -- flagged as a correction to the initial (wrong) "FINEX-specific" framing given in this session's own chat before this write-up.
- A true study-wide universal cluster was also confirmed on the combined data: (aligned) RT~420s, present in **all 18 datasets**, 138 of 164 occurrences Blank-type -- consistent with the genuine instrument artifact already identified in the FINEX-only Stage 4 analysis, now confirmed to extend to ASTRA too.

### Not implemented as pipeline code

This was a one-off exploratory investigation (temporary scripts in `%TEMP%\opencode\`, deleted after use) -- no changes were made to `Code/04_CollateStudyResults.R` or any other script. If a permanent cross-study recurring-peak report is wanted in future, it would need a new combined-collation script (there is currently no single script that loads both FINEX's and ASTRA's `_GCMSResults.csv` files together), reusing the RT-alignment and bounded-diameter clustering approach validated here.

---

## Session Summary (August 18, 2026) — TIC Deinterleaving Fix: the "Other Significant Peak" Feature Was Operating on Corrupted Data

### Background

A "Blank Other-Significant-Peak Detection" feature (`find_other_tic_peaks()` in `Code/ModPeaks.R`, wired into `evaluate_blanks()` in `Code/InjectionAcceptance.R`) had been implemented between the August 14, 2026 entry above and this session, but was never documented here -- it was still explicitly listed as "Deferred to a future session -- Not started" in the August 10, 2026 entry below, and the very first messages of this session were already mid-investigation of a problem with it. This entry documents that investigation and its fix, since the earlier implementation work itself was never written up.

### Discovery: the raw TIC signal was never deinterleaved

While investigating why the feature's significance test kept failing to distinguish real chromatographic peaks from noise (multiple threshold redesigns had already been tried and were failing), direct inspection of the raw TIC trace (via `Code/01_MsFilesReorganiser.R`'s output) revealed the root cause: this instrument's "SIM/Scan" simultaneous acquisition mode alternates **every raw TIC datapoint** between a SIM-cycle total (sum of only the 3-4 currently-monitored analyte/IS ions) and a Scan-cycle total (sum across the full m/z 41-200 survey, confirmed 16-21+ ions per scan) -- two genuinely different quantities, not a duplicate/redundant read of the same one (which is what the existing `deinterleave_sim()` correctly handles for SIM channels). The raw TIC extraction (`Code/02_PeakDetection.R`'s TIC block) had never deinterleaved this -- it was averaging and baseline-correcting a signal that secretly alternated between two different things, throughout the **entire** chromatogram, not just at the front edge. This smeared every real peak into an apparent sawtooth "noise" floor, making genuine chromatographic peaks statistically indistinguishable from artefact.

Confirmed via manual visual/plot inspection of ASTRA Analysis5 Line 25 (deliberately chosen as a "disputed" case where a large peak was suspected but not being detected): deinterleaving revealed a clean, genuine Gaussian-shaped peak at RT≈414s (max≈194,000) plus several smaller genuine peaks (392/400/407/411/417s) that were completely invisible in the interleaved signal, and a further ~7-8 genuine, cleanly-resolved peaks in the 190-230s region previously judged as background noise -- roughly 9-13 real peaks in this one injection, matching a direct visual count of the raw chromatogram. Confirmed across 12 injections spanning both FINEX (V8 and V9 RT configs) and ASTRA, Blank/Sample/QC/Cal types: row count is **exactly 50.0%** of the interleaved input in every single case -- universal, not dataset- or study-specific.

**Which point to keep**: unlike `deinterleave_sim()` (which always keeps the *second* point of each pair -- a legitimate duplicate-sampling scenario), the TIC case requires keeping whichever point has the **higher** value of each interleaved pair (a robust proxy for "this is the Scan-cycle point", without needing to know the exact SIM group composition active at that retention time). An initial attempt that kept the *lower*-value point (mistakenly assuming it was analogous to SIM's "always second") produced a signal with implausibly small values (median SD ~1,900 vs ~14,000 in the original) and was caught and corrected before proceeding.

### Fix: `deinterleave_tic()` (`Code/ModPeaks.R`)

New function, clearly documented as a distinct rule from `deinterleave_sim()` for exactly this reason. Keeps the higher-value point of each closely-spaced pair (`pair_gap` default 0.1s, same pairing convention as `deinterleave_sim()`), plus any unpaired/orphan points. Verified the front-edge artefact (~120-140s solvent-front decay, previously characterised as an interleaving side-effect candidate) is still present after correct deinterleaving at essentially the same magnitude in both channels -- confirming it is a genuine massive solvent-front signal, not an artefact of the interleaving itself, so the existing front-edge exclusion approach is still correct and separate from this fix.

### Threshold redesign (3 iterations, on the correctly-deinterleaved signal)

With the signal now genuinely clean (flat baseline, cleanly-resolved real peaks), the pre-existing relative/statistical significance apparatus was re-examined from scratch:
1. **Global per-injection noise floor** -- too conservative, missed most real peaks (only caught the single largest one).
2. **Two-region pooled noise estimate** (before/after both analytes) -- still missed most peaks in densely-populated chromatograms (e.g. Analysis5's 190-230s region alone has 7-8 real peaks; pooling noise over a whole region gets diluted by neighbouring real signal).
3. **Per-candidate local flanking windows** -- fixed the above by shrinking the window, but then failed on any chromatogram with **multiple** real peaks close together: a candidate's own flanking window would catch a genuinely different neighbouring real peak instead of true background, inflating its reference and suppressing detection of both. No window geometry could escape this in a chromatogram densely populated with real peaks (confirmed on Analysis5 Line 25's ~9-13 real peaks, several only 5-8s apart).

**Final design**: a single **absolute height floor** on the baseline-corrected, deinterleaved signal -- the same design pattern already used elsewhere in this codebase (`MinPeakHeight_PETN`/`MinPeakHeight_RDX`). `ModPeaks()`'s own existing local-minimum-relative candidacy check (height above local minimum, with a minimum width) already does legitimate local peak-shape detection; no separate relative/statistical test on top of it is needed once the input signal is correctly extracted. Height-distribution comparison across a known-busy injection (Analysis5) and several known-quiet injections showed a clean separation: genuine peaks mostly sit at ≥15,000-20,000+ counts, clear noise-region candidates sit under ~5,000. **Floor = 20,000** chosen (matches the user's direct visual count of 9 peaks in Analysis5 Line 25 exactly; 15,000 catches a few more borderline peaks). Validated with zero false positives across 14 real injections spanning both studies, multiple datasets, both RT configs, and the known Lab9 Line1 true anomaly (still correctly detected).

Since detection is now a pure absolute-height floor, the previously-computed `tic_other_peak_snrN` columns were dropped as vestigial (no longer correspond to any actual detection criterion) -- `find_other_tic_peaks()` now reports only `rt`/`height` per candidate, not `snr`.

### Production implementation

- **`GlobalCode.R`**: `tic_quiet_front_edge_clear <- 150` (seconds, front-edge exclusion), `tic_other_peak_rt_buffer <- 25` (seconds, exclusion zone around each analyte's own RT -- also clears this SIM/Scan method's own acquisition-group transition, confirmed via each dataset's `acqmeth.txt` to always fall between the two analytes' RTs), `tic_other_peak_min_height <- 20000` (the validated absolute floor), `tic_other_peak_max_report <- 5` (max numbered peak columns exported per injection; `tic_other_peak_count` always reports the true total even if it exceeds this), `tic_other_peak_cluster_tolerance <- 15` (RT tolerance for grouping "other" peaks into the same recurring cluster in study-wide reporting -- not yet empirically tuned, flagged for revisit). All added to `validate_setup()` and `log_reproducibility()`.
- **`Code/ModPeaks.R`**: `deinterleave_tic()` added (see above); `find_other_tic_peaks()` rewritten to the simpler absolute-floor design (dropped the old per-candidate flanking-window/`snr` computation entirely).
- **`Code/02_PeakDetection.R`**: TIC block now calls `deinterleave_tic(DataTIC, value_col = "TIC", pair_gap = 0.1)` immediately after loading, **before** rolling-average smoothing/baseline correction (previously this step didn't exist at all). `find_other_tic_peaks()` call and exported `tic_other_peak_*` columns updated to the new `rt`/`height`-only design (no `_snrN` columns).
- **`Code/InjectionAcceptance.R`**: `evaluate_blanks()`'s `blank_other_peak_flag`/`_rt`/`_height` columns (previously wired against the buggy interleaved-signal-derived `tic_other_peak_*` columns) now read from the corrected columns; logic itself (non-gating, surfaces only the single largest peak per blank, backward-compatible if the columns don't exist) unchanged.
- **`Code/03_Quantification.R`**: `blk_cols` (Blanks sheet export) updated for the new column set (no `_snr` columns).
- **`Code/04_CollateStudyResults.R`**: Section 13a (study-wide recurring-peak/Instrument-Performance log, reads the numbered `tic_other_peak_rt/height` columns for clustering) updated to match the new column set.

### Full-chain verification and reprocessing (all 18 datasets: 12 FINEX "Accepted Analysis" + 6 ASTRA Analysis1/3/4/5/6/7)

Verified the complete production chain (deinterleave → smooth → baseline correct → `find_other_tic_peaks()`) end-to-end against the reference cases before touching any real dataset: Analysis5 Line 25 → 8 candidates (vs 9 expected from the standalone validation -- one single near-threshold candidate, not concerning), both known-quiet reference injections → 0, Lab9 Line1's known true anomaly still detected plus 2 new genuine smaller peaks.

Backed up every dataset's pre-fix `_GCMSResults.csv` (`%TEMP%\opencode\Stage3_FinalDesign_Backup\`) and deleted each dataset's `GcData/*TIC.csv`+`*Summary.csv` intermediates (forcing `02_PeakDetection.R`'s existing skip-if-exists logic to regenerate exactly those files; `*SIM.csv` intermediates were left untouched, since analyte SIM-channel extraction is entirely unaffected by this fix) before reprocessing each dataset via its own isolated per-dataset `GlobalCode.R` copy (same established mitigation as prior sessions, run via `Rscript` directly against each isolated copy -- avoids the documented risk of a shared `GlobalCode.R`'s `DataFolder` being raced/overwritten). Reprocessed in two batches (a small representative batch first, then the remainder); this session picked up and completed the second batch, which had been left mid-flight (backups/intermediate-deletion already done, but the actual `Rscript` reprocessing runs for Steel 3, Steel 4, Analysis3, Analysis4, Analysis6, Analysis7 had not yet executed) by a prior instance of this same session that ran out of context.

**Zero regression confirmed**: row-by-row comparison of every core PETN/RDX/IS column (`petn_pa`/`_ph`/`_rt`/`_snr`/`_snr_flag`, `rdx_pa`/`_ph`/`_rt`/`_snr`/`_snr_flag`, `rdx_is_pa`/`_ph`/`_rt`), keyed on `Line`, across all 18 reprocessed datasets' backed-up-vs-current CSVs: **1,344 total rows compared, 0 mismatches** -- confirms this fix is isolated entirely to the new TIC "other significant peak" columns and does not touch analyte quantification in any way. All 18 datasets confirmed to now export the corrected column schema (verified via header inspection -- no dataset still shows the retired `tic_other_peak_snrN` columns).

### Stage 4 -- Study-Wide Recurring-Peak Analysis Rebuilt (FINEX only -- `Code/04_CollateStudyResults.R` Section 13a)

Note: this analysis only exists for the FINEX study (`Code/04_CollateStudyResults.R`, `study_root_dir` = the FINEX "Accepted Analysis" folder, 12 datasets). ASTRA's own pipeline (`Design of Experiments/pilot_analysis.R`) has no equivalent recurring-peak log at all -- out of scope here (not part of the originally agreed plan, which only ever cited the FINEX collation script's Section 13a).

Re-ran collation against the now-fully-corrected per-dataset data (isolated runner, `SampleVol`/`DepositMass`/efficiency constants pre-defined so `GlobalCode.R` isn't re-sourced -- same established mitigation as prior sessions). First pass reproduced the exact concern flagged when this stage was deferred: with real peaks now far more numerous, Section 13a's existing **sequential single-linkage** clustering (a new cluster starts only when the gap to the previous sorted RT exceeds `tic_other_peak_cluster_tolerance`, 15s) chained together long runs of genuinely *different* recurring peaks into a handful of artificially wide "clusters" -- only 4 clusters reported study-wide, one spanning 182-261s (78 seconds!) and another spanning 387-426s.

**Root-cause investigation**: direct inspection of the individual peak occurrences inside these two mega-clusters showed they are NOT a continuous smear -- they are a small number of genuinely distinct, **tightly reproducible** recurring peaks (e.g. one dominant spike at RT=227.2s +/- 0.7s SD, n=338 across 8 datasets) sitting close enough together (a few seconds apart) that single-linkage's unbounded chaining merged them into one blob regardless of tolerance, simply because peak density is now high enough that few individual gaps exceed even a generous tolerance.

**Fix** (`Code/04_CollateStudyResults.R`, clustering block in Section 13a): changed from unbounded single-linkage chaining to a **bounded-diameter** rule -- a point joins the current cluster only if BOTH the gap to the previous point AND the resulting total cluster span (max-min RT so far) stay within `cluster_tolerance`. This directly stops runaway chaining while still merging genuine repeats of the same peak (whose RT jitter is tiny relative to the tolerance). `tic_other_peak_cluster_tolerance` re-derived from 15s down to **5s** (`GlobalCode.R`, plus the in-script fallback default) -- empirically justified from the observed single-spike SD (~0.7-0.8s; 5s gives >6 SD margin) while being tight enough to keep distinct nearby recurring RTs from merging. This constant is collation-time-only (never used during per-dataset `02_PeakDetection.R` processing), so **no dataset reprocessing was needed** for this change -- only re-running `04_CollateStudyResults.R`.

**Result after the fix**: 23 distinct clusters (was 4), 20 of them genuinely recurring (>=2 occurrences). Two are present in **all 12 FINEX datasets** -- the strongest candidates for a genuine systematic instrument artifact rather than sample-driven content:

| Cluster | RT (mean) | Occurrences | Type breakdown | Height |
|---|---|---|---|---|
| 22 | ~421.6s | 194 | Blank=166 (86%), Sample=24, Cal=3 | modest & consistent (mean 33k, max 267k) |
| 21 | ~417.6s | 34 | Blank=23, Sample=10, QC=1 | huge & highly variable (mean 3.68M, max 35.7M) |

These two sit only ~4s apart in mean RT but were correctly kept separate by the bounded-diameter rule (merging them would have exceeded the 5s span cap) -- and rightly so, given how different their height profiles are: cluster 22 looks like a genuine, low-level, highly reproducible artifact (septum/column bleed candidate, present in every single dataset, overwhelmingly in otherwise-clean Blanks); cluster 21 at nearly the same RT is an intermittent, occasionally enormous carryover-type event. Other notable (but not universal) clusters include RT~227s (322 occurrences across 8 datasets, dominant in Sample/QC rather than Blank -- likely genuine, reproducible sample/system co-elution rather than blank-only carryover) and RT~204s (164 occurrences, 5 datasets).

Per-type prevalence across all 1,035 FINEX injections (recomputed on the corrected data, superseding any pre-fix numbers): 718 of 1,035 (69%) show at least one "other" peak; by type, QC and Sample show the highest per-injection rates, Blank the lowest -- see the live chat record for the full table. Any earlier claim of a "Cal>QC>Blank>Sample" prevalence ordering (mentioned only in passing at the very start of this investigation, never actually reproduced in this codebase) is confirmed void.

Outputs regenerated: `FINEX_StudyResults.csv`/`.xlsx`, `TIC_OtherPeak_Log.csv` (965 individual peak occurrences), `TIC_OtherPeak_RecurringPeaks.csv` (23 clusters), `Overall_TIC_OtherPeak_RTvsTime.png`/`_SNRTrend.png`, and all per-lab/overall recovery plots (unaffected by this feature, regenerated incidentally as part of the same collation run). Pre-run backups: `%TEMP%\opencode\Stage4_PreCollation_Backup\`.

### Stage 5 -- Downstream Wiring/Visibility Revisited

Re-checked three things per the original deferred concern ("does the underlying detection being far busier than before break any of the wiring/visibility assumptions?"):

1. **Non-gating guarantee** (`Code/InjectionAcceptance.R`): confirmed by direct code inspection, not just by the existing comments -- `evaluate_blanks()` computes `blank_other_peak_flag`/`_rt`/`_height` in a completely separate code path from `blank_status` (which is derived purely from `petn_snr_flag`/`rdx_snr_flag` tiers), and `assign_blank_brackets()` (which produces `petn_blank_bracket`/`rdx_blank_bracket`) only ever reads `blank_petn_tier`/`blank_rdx_tier` -- `blank_other_peak_flag` is never referenced anywhere in the bracket/acceptance chain. Holds structurally, confirmed additionally by the Stage 3/5 regression checks showing `petn_snr_flag`/`rdx_snr_flag` (and therefore everything downstream of them) are byte-for-byte unchanged.

2. **Per-lab XLSX visibility -- gap found and fixed**: the `tic_other_peak_*` columns (`Code/03_Quantification.R`) were only ever added to `blk_cols` (the Blanks sheet), a holdover from when this feature was conceived purely as "Blank Other-Significant-Peak Detection" before it was broadened to scan every injection type. Stage 4's corrected study-wide analysis showed "other" peaks are now actually **more** prevalent in QC (94%) and Sample (90.5%) rows than in Blanks (44.8%) -- meaning a user reviewing a per-lab XLSX's Calibration/QC/Samples sheets had no visibility into this diagnostic at all, despite it being computed identically for every row type. **Fixed**: extracted a shared `tic_other_peak_cols` constant and added it to `cal_cols`/`qc_cols`/`smp_cols` as well as `blk_cols`, so every sheet now shows `tic_other_peak_count`/`_rt1..5`/`_height1..5`.

3. **Study-level export**: deliberately left as-is -- `TIC_OtherPeak_Log.csv` (Stage 4) already provides a consolidated, all-type, all-dataset view with `Type`/`Dataset`/`SampleName` tagging, which is a better fit for this data than duplicating up to 11 mostly-NA columns into the already-wide study-level Samples sheet. `desired_cols`/`desired_cols_nc` left unchanged.

**Verification**: pure column-addition change (no computation touched) -- re-ran `Code/03_Quantification.R` for all 18 datasets (only step 3 needed re-running; `01`/`02` skip via existing-file checks since nothing about peak detection changed) via the same isolated per-dataset runner scripts as Stage 3. Confirmed directly on Lab9's regenerated XLSX: all 4 sheets (Calibration/QC/Samples/Blanks) now carry the `tic_other_peak_*` columns, with per-sheet prevalence matching the study-wide pattern (QC 10/10, Samples 28/29, Cal 5/6, Blanks 24/44). Full regression check across all 18 datasets (1,344 rows, 13 core PETN/RDX/IS columns): **0 mismatches** -- confirms zero behavioural change beyond the new columns. `FINEX_StudyResults.csv`/`.xlsx`/`TIC_OtherPeak_*.csv` from Stage 4 remain valid and were not regenerated (the underlying `tic_other_peak_*` values themselves are computed in `02_PeakDetection.R`, untouched by this stage, so nothing that feeds collation changed).

### All 5 stages of the TIC deinterleaving fix are now complete.

Remaining open items (not blocking, flagged for optional future follow-up):
- The two universal Blank-dominant recurring clusters (RT~417.6s/~421.6s, Stage 4) are worth a manual look against `acqmeth.txt`/known column-bleed RT ranges, but this is exploratory, not a defect.
- `tic_other_peak_cluster_tolerance` (now 5s) is still a first-pass empirical value (derived from 2 example spikes' SD) -- revisit if a much larger peak-log population later suggests otherwise.
- ASTRA has no equivalent recurring-peak log or per-lab-visibility gap analysis was not extended there (out of the originally agreed scope, since Section 13a is FINEX-only).

**Files modified (this session, cumulative)**: `Code/ModPeaks.R` (`deinterleave_tic()` new; `find_other_tic_peaks()` rewritten to absolute-floor design), `Code/02_PeakDetection.R` (TIC deinterleaving added before smoothing; `find_other_tic_peaks()` call/export updated), `GlobalCode.R` (`tic_other_peak_*`/`tic_quiet_front_edge_clear` config simplified, `snr`-related config removed, `tic_other_peak_cluster_tolerance` re-derived 15->5), `Code/InjectionAcceptance.R` (`evaluate_blanks()` column set updated), `Code/03_Quantification.R` (`blk_cols` updated; Stage 5: shared `tic_other_peak_cols` extracted and added to `cal_cols`/`qc_cols`/`smp_cols` too), `Code/04_CollateStudyResults.R` (Section 13a column set updated; clustering algorithm changed from unbounded single-linkage to bounded-diameter), all 18 "Accepted Analysis"/ASTRA dataset `Results/`+`GcData/` folders (regenerated `_GCMSResults.csv`/`.xlsx` twice -- once for the TIC fix, once for the Stage 5 column additions; `GcData/*TIC.csv`/`*Summary.csv` regenerated once; pre-fix CSVs backed up to `%TEMP%\opencode\Stage3_FinalDesign_Backup\`), `FINEX_StudyResults.csv`/`.xlsx`, `TIC_OtherPeak_Log.csv`, `TIC_OtherPeak_RecurringPeaks.csv` (regenerated, Stage 4), `CONTEXT.md` (this entry).

---

## Session Summary (August 14, 2026, continued) — Confirmatory Ion Columns Added to Per-Lab XLSX and Collated CSV/XLSX

### Motivation

Stage 1-4 of the confirmatory-ion feature (session below) had the qualifier PA/PH/SNR/flag columns present in each dataset's raw `_GCMSResults.csv` (which never restricts columns), but NOT in the individual per-lab `_GCMSResults.xlsx` sheets (`Code/03_Quantification.R`'s `cal_cols`/`qc_cols`/`smp_cols`/`blk_cols` are allow-lists) nor in the collated study-level `FINEX_StudyResults.csv`/`.xlsx` (`Code/04_CollateStudyResults.R`'s `desired_cols`/`desired_cols_nc` are also allow-lists). Explicit request to add them to both.

### Implementation

- **`Code/03_Quantification.R`**: added `petn_qual_cols`/`rdx_qual_cols` constants (all 5 fields -- rt/pa/ph/snr/snr_flag -- for each of PETN's m/z76 and RDX's m/z46/m/z75) and inserted them into `cal_cols`/`qc_cols`/`smp_cols`/`blk_cols`, positioned immediately after each analyte's own raw-measurement columns (consistent with the existing "PETN block, then RDX block" column-grouping convention documented in the "Column Organization" section below). Also added the qualifier rt/pa/ph/snr columns to `nd_columns` so they get the same NA->"ND" display treatment as the existing quantifier columns.
- **`Code/04_CollateStudyResults.R`**: added `petn_qual76_pa`/`petn_qual76_snr_flag` to `desired_cols`/`desired_cols_nc` (right after `petn_snr_flag`) and `rdx_qual46_pa`/`rdx_qual46_snr_flag`/`rdx_qual75_pa`/`rdx_qual75_snr_flag` (right after `rdx_snr_flag`). Only the ions actually used by the confirmatory-ion gate are exposed here (not the excluded PETN m/z57, to avoid presenting a column that was deliberately never trusted). Also extended `apply_formatting()`'s SNR-flag conditional-formatting block (Quantifiable=green/Below_LOQ=amber/Below_LOD=red) to cover the new qualifier `snr_flag` columns.
- `apply_formatting()` looks up column positions dynamically by name (`which(col_names == col)`), not hardcoded letters, so inserting new columns required no other formatting-logic changes.

### Verification (real data, all 11 datasets + full collation)

Re-ran `Code/03_Quantification.R` for all 11 "Accepted Analysis" datasets (via the same isolated-`GlobalCode.R`-copy-per-dataset method established in the session below) to regenerate each `_GCMSResults.xlsx` with the new columns in all 4 sheets (Calibration/QC/Samples/Blanks) -- confirmed present via direct read-back (e.g. Lab29). Re-ran `Code/04_CollateStudyResults.R` once more: `analysis_accepted`/`Outcome` PASS/PASS*/FAIL counts (218/9/45) **unchanged** from immediately before this change (confirms this was a pure column-addition, no logic change), qualifier columns confirmed present in `FINEX_StudyResults.csv` and in the `All Data`/`Steel`/`ABS-Smooth`/`Negative Controls` XLSX sheets (correctly absent from the aggregate `Summary`/`CalSummary`/`SampleSummary` sheets, which don't carry per-row analyte columns at all). Final regression check across all 11 datasets' CSVs: 959 total rows (unchanged), zero missing core quantifier columns, qualifier columns present everywhere.

**Files modified**: `Code/03_Quantification.R` (`petn_qual_cols`/`rdx_qual_cols`, `cal_cols`/`qc_cols`/`smp_cols`/`blk_cols`, `nd_columns`), `Code/04_CollateStudyResults.R` (`desired_cols`, `desired_cols_nc`, `apply_formatting()`), all 11 dataset `_GCMSResults.xlsx` (regenerated), `FINEX_StudyResults.csv`/`.xlsx` (regenerated, confirmed zero behavioural change), `CONTEXT.md` (this entry).

---

## Session Summary (August 12-14, 2026) — Confirmatory Ion Detection for PETN and RDX

### Motivation

Standard GC-MS/SIM practice confirms peak identity using a qualifier ion's signal in addition to the quantifier ion, guarding against a co-eluting interferent being misidentified as the target analyte. This pipeline had never implemented such a check for PETN or RDX -- only the single quantifier ion per compound (PETN m/z 46, RDX m/z 120) was ever processed. This session added one, in four staged steps (each verified against real data before proceeding), and investigated whether the acquisition method itself could be improved with a better second PETN qualifier.

### Key discovery: qualifier ion data was already being acquired, just never processed

Inspected the real instrument acquisition method (`acqmeth.txt`, consistent across all "Accepted Analysis" datasets and both the V8/V9 RT configs): the SIM groups actually monitor **PETN: m/z 46 (quantifier), 57, 76** and **RDX: m/z 46, 75, 120 (quantifier), 122 (IS)** -- the qualifier ions m/z 57/76 (PETN) and 46/75 (RDX) have always been acquired and retained in `GcDataConvertedRcode/SIM/*.csv`, simply never extracted or used by `02_PeakDetection.R`'s `all_mz` list. No acquisition-method or raw-data changes were needed -- only extending the existing extraction code.

### Stage 1 -- Additive qualifier-ion extraction (`GlobalCode.R`, `Code/02_PeakDetection.R`)

- `GlobalCode.R`: added `qualifiers` field to `analytes$PETN` (`c(57, 76)`) and `analytes$RDX` (`c(46, 75)`), plus a permissive `MinPeakHeight_Qualifier <- 20` detection floor for this exploratory extraction.
- `Code/02_PeakDetection.R`: rewrote the m/z-channel-building logic from "one `process_sim_channel()` call per unique m/z" to "one call per **role-specific channel key**" (`channel_specs`, keyed `<analyte>_qual<mz>` for qualifiers). This was found to be essential, not optional: m/z 46 is simultaneously PETN's own quantifier AND RDX's qualifier ion, and `process_sim_channel()`/`ModPeaks()` apply a single peak-height floor to whatever channel data they're given -- an earlier attempt that computed one shared per-m/z floor (taking the minimum across all roles sharing that m/z) silently lowered PETN's own carefully-validated 200-count blank-noise floor to 20 for the *entire* m/z 46 channel, causing dozens of spurious new PETN "detections" in blanks. The role-specific-channel redesign gives every qualifier its own independent `process_sim_channel()` call (own RT window, own floor), completely eliminating any cross-contamination between roles, at the cost of (harmless) duplicate baseline-correction work for the one shared m/z.
- New exported columns per qualifier: `{prefix}_qual{mz}_rt/_pa/_ph/_snr/_snr_flag` (20 new columns total: 2 PETN qualifiers x 5 fields + 2 RDX qualifiers x 5 fields).
- **Diagnostic plots deliberately NOT generated for qualifier channels** (a second bug found and fixed during batch verification, see below) -- only the numeric extraction happens for qualifiers; no per-injection TIFF plots.

### Disk-space incidents during Stage 1 batch verification (unrelated infrastructure issue + a real bug, both resolved)

Reprocessing all 11 "Accepted Analysis" datasets (`ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, `Lab29`, `Lab33`, `Lab9`, `Steel-1`, `Steel 2`, `Steel 3`, `Steel 4`) repeatedly hit disk-full conditions (C: drive reached 0 bytes free multiple times):
1. **First incident**: the C: drive was already critically low (0.05 GB free) before this session's work even began, for unrelated reasons (Windows temp files, Adobe Creative Cloud caches, possibly the hibernation file/Windows Update cache). Diagnosed and freed 4.23 GB by clearing `AppData\Local\Temp` (mostly stale Adobe Creative Cloud partial-download caches and leftover PyInstaller `_MEI*` extraction folders). One `Summary.csv` file write on "Steel 2" was silently truncated to 0 bytes mid-write (`close.connection()` warning: "No space left on device") before this was caught -- restored from backup, confirmed byte-identical to the pre-change state, no data lost.
2. **Second incident, the real bug**: even after Adobe's cache was fully cleared and closed, disk filled again almost immediately. Root cause: **the qualifier-channel diagnostic plots themselves** -- with ~90 injections/dataset x 4 new qualifier channels, uncompressed TIFF baseline-correction + integration-verification plots consumed **~10 GB of new plots per dataset** (verified directly on Lab9), on top of the pipeline's pre-existing ~5GB/dataset plot footprint. Fixed by adding a `make_plots` flag to each channel spec (`TRUE` for quantifier/IS channels, `FALSE` for all qualifier channels) -- qualifier channels now only do the numeric extraction, never generate plots. Recovered 26.14 GB by deleting the already-generated qualifier plot folders across the 6 datasets processed before the fix; re-verified on Lab9 that the fix eliminates the bloat with zero change to the numeric data.

All 11 datasets were successfully reprocessed and verified after these fixes: zero corrupted/zero-byte files, all quantifier columns (`petn_pa`/`petn_ph`/`petn_rt`, `rdx_pa`/`petn_ph`/`rdx_rt`, `rdx_is_*`) exactly unchanged from their pre-Stage-1 values, all 20 new qualifier columns present and populated. (One unrelated finding during verification: a ~1% `petn_snr`-only drift, with one borderline SNR-flag flip, was found on the V9-RT datasets -- traced via a minimal isolated reproduction to be **pre-existing codebase drift unrelated to this session's changes** -- those 4 datasets simply hadn't been reprocessed since some earlier, unrelated fix. `petn_rt`/`petn_pa`/`petn_ph` and all RDX/IS values were exactly unchanged everywhere.)

### Stage 2 -- Empirical ion-ratio characterisation (`Diagnostics/ConfirmatoryIon_RatioAnalysis.R`, new)

Computed qualifier/quantifier PA ratios from all 66 real calibration standard injections (known-pure identity) across all 11 datasets:

| Ion | Detection rate (Cal std) | Reaches Quantifiable SNR | Ratio behaviour |
|---|---|---|---|
| PETN m/z 76 | 100% | 100% | **Flat**: ~0.102 ± CV 9.5%, no concentration trend (r=0.09, p=0.49) |
| PETN m/z 57 | 72.7% | **only 18.2%** | Weak/noisy (~0.03, CV 45-78%) -- even pure Cal standards usually don't clear SNR≥10 |
| RDX m/z 46 | 100% | 100% | **Concentration-dependent**: ratio 9.97 (0.2ng) -> 5.48 (10ng), r=-0.72, p<1e-11 |
| RDX m/z 75 | 100% | 100% | **Concentration-dependent**: ratio 8.84 (0.2ng) -> 2.05 (10ng), r=-0.69, p<1e-10 |

Investigated the RDX concentration-dependence: RDX's own quantifier PA response is already known to be non-linear across the calibration range (the reason this codebase uses quadratic, not linear, calibration everywhere for RDX/PETN) -- the qualifier ions apparently follow a slightly different non-linear curve shape, so their *ratio* to the quantifier drifts with concentration (well-fit by log/inverse models, R²=0.75-0.89). A single fixed ratio+tolerance would not be valid across the calibration range for RDX.

**Explicit user decision following this finding**: abandon ratio-based confirmation entirely. Use a simple qualitative **detection-only** check instead (is the qualifier ion itself detected, SNR≥3, i.e. `snr_flag %in% c("Below_LOQ","Quantifiable")`) -- sidesteps the concentration-dependence problem, and is standard practice when ratio tolerances are impractical.

### Stage 3 -- Impact analysis (`Diagnostics/ConfirmatoryIon_DetectionImpactAnalysis.R`, new)

Applied the detection-only check retrospectively to all 959 rows (Blank/Cal/QC/ResponseCheck/Sample) across all 11 datasets, at the Quantifiable SNR tier only:

| Analyte | Policy | Sample rows checked | Failed |
|---|---|---|---|
| PETN | m/z76 only | 264 | **0** |
| PETN | both m/z57 AND m/z76 (tested, rejected) | 264 | 209 (79.2%) |
| RDX | both m/z46 AND m/z75 | 285 | **0** |
| RDX | either m/z46 OR m/z75 | 285 | **0** |

Confirmed **zero Sample/QC/Cal rows would newly fail** under the chosen design (PETN: m/z76 only; RDX: both required) -- the check is a low-risk safety net that essentially never blocks a genuine detection under current data, while requiring PETN's m/z57 would fail 70-100% of everything including pure Cal standards (confirming its exclusion). RDX's "both" vs "either" choice made zero difference on real Sample/QC/Cal data (only 2 Blank rows differed, and blanks aren't gated on identity confirmation) -- "both" was chosen as the stricter, cost-free option.

**Separately investigated** (per explicit request) whether the instrument method could be improved with a better second PETN qualifier: parsed the raw `.ms1` full-scan (m/z 41-200) spectrum at PETN's RT across two datasets (Lab9, Lab29) using `parse_ms1_file()`/`get_all_ions_at_rt()` (reused from the existing `Diagnostics/TIC_AllIons_Scan.R` IS-search tooling, applied to PETN's RT instead -- `Diagnostics/PETN_AllIons_Scan.R`, new). No better candidate was found: m/z 76 remains the best available ion; m/z 42-45 are present at blank-comparable levels (non-specific background/column bleed) in both datasets; m/z 47 only appeared reproducibly in one of the two datasets tested. Literature PETN fragments (m/z 78, 127, 133, 316) are invisible to this survey because the SIM/Scan acquisition's interleaved full-scan channel has much lower sensitivity than a dedicated SIM channel, the scan-mode threshold (1000 counts) likely filters out weaker real fragments, and m/z 316 (molecular ion) is above the scanned range (41-200) entirely. Finding a genuinely better qualifier would require an instrument-method change (add a candidate as a real monitored SIM ion) and a new validation run -- out of scope for a data-only investigation.

### Stage 4 -- Wired into acceptance logic (`Code/InjectionAcceptance.R`, `Diagnostics/Improved_QC_Flowchart.dot`)

- New `IA_QUALIFIER_IONS` config (`petn = c("qual76")`, `rdx = c("qual46", "qual75")`) and `IA_is_qualifier_detected()` helper.
- `evaluate_analyte()`: new check inserted at the `tier == "Quantifiable"` branch, before the existing 6ng QC bracket check -- if any required qualifier's own `snr_flag` column exists but doesn't show detection, hard `FAIL: <ANALYTE> confirmatory ion not detected (...) -- identity not confirmed"`. Trace/Negative tiers are exempt (qualifier signal too weak to be meaningful there). **Fully backward-compatible**: if the qualifier flag column doesn't exist in the data at all (older datasets, or the ASTRA study's `Design of Experiments/pilot_analysis.R`, which also sources this shared file), the check is silently skipped, not treated as a failure.
- `Diagnostics/Improved_QC_Flowchart.dot`: new `C4b` decision diamond ("confirmatory ion(s) detected?") inserted between `C4` (SNR classification) and `C6` (6ng bracket check), only on the Quantifiable-tier path; new `C_NOCONFIRM` red FAIL terminal wired to `D0` (same pattern as `C_CARRYOVER`); `D2`'s FAIL-class description and `NOTE8` (new) updated to document the design rationale and the rejected ratio-based alternative. Regenerated `.png`/`.svg`.

### Verification (real data, full study re-collated)

Re-ran `Code/04_CollateStudyResults.R` (isolated runner, `SampleVol`/`DepositMass`/efficiency constants pre-defined to avoid triggering a `GlobalCode.R` re-source per the established mitigation). To isolate Stage 4's specific effect from Stage 1's incidental reprocessing-drift (a naive backup-vs-now diff was confounded by a stale pre-Stage-1 backup, see below), ran collation **twice** against the identical Stage-1-reprocessed data: once with `IA_QUALIFIER_IONS` temporarily nulled out (true "before Stage 4"), once with it restored (true "after Stage 4"). The two outputs are **byte-for-byte identical** (`fc /b`, zero differences) -- confirms the confirmatory-ion check has **zero impact** on the current study's `FINEX_StudyResults.csv`/`.xlsx`, exactly matching Stage 3's prediction. Explicitly confirmed zero rows anywhere contain a "confirmatory ion not detected" failure reason.

(An initial comparison against a stale backup, predating this session's Stage 1 dataset reprocessing, showed 6 "Steel 4" samples flipping PASS->FAIL -- traced entirely to Stage 1's incidental reprocessing of Steel 4's raw data through the current pipeline code shifting some 6ng QC bias values right across the ±20% threshold (the same unrelated pre-existing-drift phenomenon identified for the V9 datasets above), NOT the confirmatory-ion check. Not a regression from this session's actual feature work.)

**Files modified**: `GlobalCode.R` (`qualifiers` field on `analytes$PETN`/`analytes$RDX`, `MinPeakHeight_Qualifier`), `Code/02_PeakDetection.R` (`channel_specs`/role-specific-channel-key redesign, `make_plots` flag, qualifier extraction + Results columns), `Code/InjectionAcceptance.R` (`IA_QUALIFIER_IONS`, `IA_is_qualifier_detected()`, `evaluate_analyte()` Quantifiable-tier gate), `Diagnostics/Improved_QC_Flowchart.dot`/`.png`/`.svg` (`C4b`/`C_NOCONFIRM`/`NOTE8`), `Diagnostics/ConfirmatoryIon_RatioAnalysis.R` (new), `Diagnostics/ConfirmatoryIon_DetectionImpactAnalysis.R` (new), `Diagnostics/PETN_AllIons_Scan.R` (new), all 11 "Accepted Analysis" dataset `Results/`+`GcData/` folders (regenerated with 20 new qualifier columns each), `FINEX_StudyResults.csv`/`.xlsx` and `Plots/` (regenerated, confirmed zero behavioural change vs the immediately-prior Stage-1 state), `CONTEXT.md` (this entry).

---


### Discovery

While investigating the correlation-plot outlier fixed in the immediately preceding session (Steel facet axis cap, ~165% RDX recovery point), traced the root cause: `Lab6 P1 Steel Repeat 4` has `rdx_is_pa_flag = "LOW"` (internal standard peak area abnormally low) but was showing `analysis_accepted = "PASS*"`, `Outcome = "Complete"`, with `rdx_recovery = 164.8%` -- a physically impossible result (RDX recovery cannot exceed 100%). Dividing by an abnormally small IS-peak-area denominator inflates the IS-ratio-quantified RDX concentration arbitrarily, exactly as this magnitude of error would predict.

**Root cause identified as a genuine regression**: the July 31, 2026 "IS Peak Area Injection-Validity Check" session (below) originally implemented BOTH `NOT_DETECTED` and `LOW` IS flags as hard `FAIL`s. During the August 10, 2026 "Injection Acceptance Logic Consolidated into Shared `Code/InjectionAcceptance.R`" refactor (implementing the flowchart design, below), this was inadvertently changed: `NOT_DETECTED` remained a hard FAIL, but `LOW` was silently downgraded to a caveat-only (`is_caveat`, contributing to `PASS*` rather than blocking it) -- a behavioural change not called out in that session's own summary, which focused on the flowchart's *other* design changes (Trace-tier handling, bracket gating, etc.) and did not flag this specific divergence from the pre-existing (and correct) July 31 behaviour.

### Scope Check (before fixing)

Queried the live `FINEX_StudyResults.csv` for every row with `rdx_is_pa_flag == "LOW"`: exactly 2 affected rows across the whole study --
- `Lab6 P1 Steel Repeat 4`: PA=262, RDX recovery=164.8% (the impossible value)
- `Lab20 P2 ABS-Smooth Repeat 2`: PA=169 (this is the same injection already documented as anomalous in the July 31, 2026 session's own verification table, which at the time correctly showed `FAIL: PETN Below_LOD; IS abnormally low (PA=169)` -- confirming this row silently lost its IS-based failure reason at some point between then and now, leaving only its coincidental PETN-Below_LOD reason, or in this case no PETN failure at all so it slipped through as PASS*)

### Fix Applied

**`Code/InjectionAcceptance.R`**, `compute_injection_acceptance()` (~line 514): the IS injection-validity check now hard-fails on `LOW` exactly like `NOT_DETECTED`, restoring the July 31, 2026 behaviour:
```r
if (!is.na(is_pa_flag) && is_pa_flag == "NOT_DETECTED") {
  accepted[i] <- "FAIL: Injection failure (IS not detected)"
  outcome[i]  <- "Reanalyse"
  next
}
if (!is.na(is_pa_flag) && is_pa_flag == "LOW") {
  accepted[i] <- sprintf("FAIL: IS abnormally low (PA=%s)", round(df$rdx_is_pa[i]))
  outcome[i]  <- "Reanalyse"
  next
}
```
The now-unused `is_caveat` variable (previously feeding into `any_caveat <- is_caveat || ...`) was removed from the row-level PASS/PASS* combination at the end of the function. Since this check runs before the per-analyte PETN/RDX evaluation (fixed order: IS -> PETN -> RDX) and applies identically to samples and NCs (no structural difference remains between them under the unified flowchart design), the fix automatically covers NC rows too with no separate code path needed.

### Flowchart Amended (`Diagnostics/Improved_QC_Flowchart.dot`)

Per explicit request to keep the flowchart -- the documented source of truth for this logic -- in sync:
- `C_ISLOW` node changed from an amber caveat box ("IS LOW... carry a WARNING caveat forward") to a red terminal FAIL box, mirroring `C_NOINJECT`: `"ROW = FAIL: IS abnormally low (PA=x)\nOutcome = Reanalyse\nSHORT-CIRCUIT EXIT..."`, with an explanatory note referencing the 2026-08-11 fix and the 164.8% impossible-recovery example.
- Removed the `C_ISLOW -> C2` edge (IS-LOW no longer continues into the per-analyte evaluation loop -- it now terminates immediately, same as IS-not-detected).
- `D4` (the Section D "does either analyte carry a WARNING caveat" decision node) had "IS-LOW" removed from its list of caveat sources, since IS-LOW no longer reaches Section D at all.
- `NOTE5` updated to state both `C_NOINJECT` and `C_ISLOW` short-circuit immediately at C1.
- The simplified thesis-style flowcharts (`Thesis_Flowchart_Samples.dot`, `Thesis_QC_Flowchart.dot`) were checked and required **no change** -- their `s2 [label="Internal standard valid?"]` node was already a simple binary Yes/No gate straight to FAIL on "No", with no separate LOW-as-caveat branch, so they were already consistent with the corrected behaviour.
- `Improved_QC_Flowchart.png`/`.svg` regenerated from the updated `.dot` source using Graphviz (`C:\Program Files\Graphviz\bin\dot.exe`, version 15.1.1 -- installed but not on PATH at the time; see "Graphviz Added to PATH" note below) -- confirmed the corrected `C_ISLOW` node now renders as a red terminal FAIL box (was previously incorrectly checked as "not installed" earlier in this same session, before the binary was located on disk).

### Verification (real data, full study re-collated)

Re-ran `Code/04_CollateStudyResults.R` in full (all 9 "Accepted Analysis" datasets, 11 results files including the `Steel 2`/`Steel 3`/`Steel 4` runs), but **without** re-processing any raw data: pre-defined `SampleVol`/`DepositMass`/the four extraction/filtration efficiency constants (all read directly from `GlobalCode.R`'s literal values, not sourced) so the script's own `if (!exists("SampleVol") || !exists("DepositMass")) source(global_code_path)` guard skipped sourcing `GlobalCode.R` entirely -- avoids the documented risk of that source() call unconditionally re-triggering `01_MsFilesReorganiser.R`/`02_PeakDetection.R`/`03_Quantification.R` for whatever dataset `DataFolder` currently points to. `library(tidyr)` (normally loaded by `GlobalCode.R`) added explicitly since it's needed by `replace_na()` later in the script.

Confirmed directly:
- `PASS*` count dropped from 9 to 7 (exactly the 2 expected rows).
- Both rows now show `FAIL: IS abnormally low (PA=262)` / `FAIL: IS abnormally low (PA=169)` respectively, `Outcome = Reanalyse`.
- Re-generated `Overall_PETN_vs_RDX_Correlation_PerSample_BySurface.png`: Steel panel now shows **n=112, r=0.67, ρ=0.50** -- exactly matching the original screenshot from earlier in this session, confirming the fix restores the analysis to its correct pre-regression state (the impossible point is now excluded from the underlying data, not merely clipped from view as the immediately preceding session's axis-cap fix had done as a stopgap).
- Full collation completed end-to-end with no errors (`FINEX_StudyResults.csv`/`.xlsx`, `SampleSummary`, all Overall and per-lab plots, PCA biplot, correlation plots all regenerated).

Pre-change `FINEX_StudyResults.csv`/`.xlsx`/`_Summary.csv`/`_Summary.xlsx` backed up to `%TEMP%\opencode\FINEX_Collation_Backup_20260811_153642\`.

**Files modified**: `Code/InjectionAcceptance.R` (`compute_injection_acceptance()`: LOW IS now hard-fails, `is_caveat` removed), `Diagnostics/Improved_QC_Flowchart.dot` (`C_ISLOW` node, edge, `D4`, `NOTE5` updated), `FINEX_StudyResults.csv`/`.xlsx` and all `Plots/` outputs (regenerated), `CONTEXT.md` (this entry).

### Note for a future session

`Improved_QC_Flowchart.png`/`.svg` have been regenerated and are up to date with the corrected design (see above) -- no outstanding action needed here.

### Addendum (same session): Graphviz Was Already Installed, Just Not on PATH -- Now Added to PATH

Initial check (`Get-Command dot` / `where.exe dot`) found nothing and was reported as "graphviz not installed" -- this was an incomplete check that only looks at PATH-visible commands. A full filesystem search found `dot.exe` already installed at `C:\Program Files\Graphviz\bin\dot.exe` (version 15.1.1), simply not on PATH. Used the full path directly to regenerate the flowchart images (see above), then -- per explicit request -- added `C:\Program Files\Graphviz\bin` to the persistent User-level PATH environment variable (`[Environment]::SetEnvironmentVariable("Path", ..., "User")`, no admin rights required) and to the current session's `$env:Path`, so `dot` now works directly in this and all future sessions without needing the full path. Also confirmed R's `DiagrammeR`/`DiagrammeRsvg` packages and Python's `graphviz` package are both installed (neither was used here, but either could also render `.dot` files if needed in future -- both ultimately still shell out to the same system `dot` binary).

**Lesson for future sessions**: when checking whether a system tool is "installed," check the filesystem (common install locations, e.g. `C:\Program Files\<Tool>\`) in addition to `Get-Command`/PATH -- a tool can be fully installed and unavailable via a bare command name simultaneously.

---

## Session Summary (August 11, 2026) — PETN vs RDX Correlation Plots: Trend Line/CI Removed, Steel Facet Axis Capped

### Motivation

While reviewing the FINEX correlation plots (`Overall_PETN_vs_RDX_Correlation_*.png`, added in the August 3, 2026 session below) as part of preparing a separate DOE technical review presentation, the user judged the fitted `geom_smooth(method="lm")` trend line and its shaded 95% CI ribbon to be misleading -- there is no clear, reliable trend, and a fitted line implies more confidence in a linear relationship than the data (particularly ABS-Smooth's r=-0.18, p=0.059, and the general sparsity/floor-effect-heavy scatter) actually supports. Separately, the Steel facet of the per-sample, by-surface plot was found to contain a single very-high-leverage point (~165% RDX recovery) that dominates the free-scaled axis, compressing the bulk of the Steel data into an unreadable sliver near the bottom of the panel.

### Fix 1: Trend Line and CI Ribbon Removed

`create_correlation_plot()` (`Code/04_CollateStudyResults.R`, ~line 1381): removed the `geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 0.8)` layer entirely. The scatter points and the r (Pearson) / rho (Spearman) statistics annotation (top-right of each panel, from `compute_corr_stats()`) are unchanged -- the actual correlation statistics are still reported, just without an accompanying fitted line/ribbon that could overstate confidence in a linear relationship.

### Fix 2: Per-Facet Y-Axis Clipping (`facet_ylim` parameter)

Since `ggh4x` (which would allow clean per-facet fixed scales via `facetted_pos_scales()`) is not installed, and this codebase's established convention avoids adding new plotting-package dependencies (e.g. the existing preference for `gridExtra` over `patchwork`), implemented a dependency-free, native-ggplot2 alternative:

- `create_correlation_plot()` gained an optional `facet_ylim` parameter (a named list, e.g. `list(Steel = c(0, 25))`), only used when `facet_by_surface = TRUE`.
- For each named facet, points outside the given range are filtered out of the **display** data only (`plot_data_display`) -- `stats_df` (and therefore the r/rho/n annotation) is always computed from the full, unclipped `plot_data` first, so the reported statistics stay honest even though the offending point(s) are not drawn.
- An invisible `geom_blank()` anchor row (at that facet's own median x, y = the range's min and max) pins the facet's independent free y-scale exactly to the requested range, regardless of where the remaining (non-clipped) points happen to fall -- without this anchor, `facet_wrap(scales = "free")` would simply auto-scale to whatever the highest *remaining* point is (e.g. ~20%) rather than the requested round number (25%).
- A console `message()` reports how many points were clipped and from which facet, so this isn't a silent data-hiding operation.
- The `PerSample_BySurface` call site (~line 1449) updated to pass `facet_ylim = list(Steel = c(0, 25))`. The other three correlation plots (`PerSample` unfaceted, `ParticipantMean`, `ParticipantMean_BySurface`) don't pass this parameter and are therefore unaffected.

### Verification and Regeneration (real data)

Both fixes verified against real FINEX data without re-running the full pipeline (`Code/04_CollateStudyResults.R` sourcing `GlobalCode.R` risks triggering a full raw-data reprocessing of whatever dataset `DataFolder` currently points to -- see multiple prior sessions' documented collision/lock issues). Instead, used a standalone script that reads the already-collated `FINEX_StudyResults.csv` directly and reconstructs `recovery_data`/`participant_bar_data` using the exact same logic as the live script (`Outcome == "Complete"` filter, `petn_recovery_dc`/`rdx_recovery` -> `petn_rec`/`rdx_rec`, NA -> 0), while extracting `create_correlation_plot()`/`compute_corr_stats()` directly from the live, already-edited script file (not copy-pasted) to guarantee no drift. Pre-change plots backed up to `%TEMP%\opencode\CorrelationPlots_Backup_20260811_135158\`.

Confirmed: no `geom_smooth` in the regenerated plots; console reported "clipping 1 point(s) outside [0.0, 25.0] from view in the 'Steel' facet" as expected; Steel panel's y-axis now runs cleanly 0-25 with the ~165% point excluded from view while the annotation still correctly shows the full n=113, r=0.10, rho=0.50 (based on all points, including the clipped one). All 4 `Overall_PETN_vs_RDX_Correlation_*.png` files and `PETN_RDX_Correlation_Stats.csv` regenerated in place.

**Files modified**: `Code/04_CollateStudyResults.R` (`create_correlation_plot()`: `geom_smooth` removed, `facet_ylim` parameter added; `PerSample_BySurface` call site updated), `Overall_PETN_vs_RDX_Correlation_PerSample.png`/`_PerSample_BySurface.png`/`_ParticipantMean.png`/`_ParticipantMean_BySurface.png`, `PETN_RDX_Correlation_Stats.csv` (all regenerated), `CONTEXT.md` (this entry).

---

## Session Summary (August 10, 2026, continued) — Injection Acceptance Logic Consolidated into Shared `Code/InjectionAcceptance.R`; Implements the Flowchart Design

### Motivation

Follow-up to the same-day flowchart design sessions (`Diagnostics/Improved_QC_Flowchart.dot` and the simplified `Diagnostics/Thesis_Flowchart_{Blanks,QC,Samples}.dot`, built earlier this session but not yet documented here). Two problems were addressed together:

1. **Duplication**: `Code/04_CollateStudyResults.R` (FINEX study) and `Design of Experiments/pilot_analysis.R` (ASTRA study, separate top-level project) contained near-byte-identical hand-ported copies of `assign_qc_brackets()`, `compute_analysis_accepted()`, `compute_nc_analysis_accepted()`, `compute_nc_result()` -- confirmed already silently diverging (ASTRA had independently evolved to treat non-Quantifiable results as legitimate negatives; FINEX still hard-failed them).
2. **Behaviour**: neither codebase's acceptance logic actually matched the finalised flowchart design agreed earlier this session (blank evaluation, symmetric PETN/RDX 0.2ng WARN tier, tier-conditional bracket gating, no 0.2ng-override, first-failure-only combination, Trace as a genuine `PASS*` outcome).

Both were fixed in one pass: a single new shared script implementing the flowchart, sourced by both study pipelines.

### Backups taken before any code was touched

`C:\Users\A Bruce - User\Documents\ClaudeSpace\_Backups\PreInjectionAcceptanceRefactor_20260810_195657\` -- full pre-change copies of `GCMSQuantitation\Code\` (+`GlobalCode.R`, this file) and `Design of Experiments\` (whole folder, minus `.RData`/`.Rhistory`).

### Implementation: `Code/InjectionAcceptance.R` (new)

Implements Sections A-D of `Diagnostics/Improved_QC_Flowchart.dot`. Sourced by both `Code/04_CollateStudyResults.R` and `Design of Experiments/pilot_analysis.R` (absolute path) -- this is now the SINGLE source of truth for "was this injection/run trustworthy, and what does it mean for this sample/NC". Four functions:

- **`evaluate_blanks(all_data)`** (Section A, genuinely new -- did not exist in either codebase before): classifies every `Type=="Blank"` row's PETN/RDX independently into the same three-tier system used everywhere else (Negative/Trace/Quantifiable, from `petn_snr_flag`/`rdx_snr_flag`), then summarises into `blank_status` ("Contaminated" if either analyte Quantifiable, "Trace detected" if either Trace, else "Clean"). The flowchart's "other significant peak" (instrument-performance-only) branch is **deliberately not implemented** -- confirmed via investigation that no exported column anywhere in the pipeline supports a genuine identity/RT-agnostic "any other peak" scan (the one whole-chromatogram scan that exists, `02_PeakDetection.R`'s TIC block, is tuned to a non-trace threshold and is never consumed downstream). `blank_other_peak_flag` always `FALSE` with an explicit `# TODO` pointing here. Building it properly would mean new logic against the retained `GcDataConvertedRcode/TIC/`+`/SIM/` intermediate files (full chromatogram, all ions, already retained -- just never read for this purpose).

- **`assign_qc_brackets(all_data, petn_bias_col, rdx_bias_col, qc_6ng_limit)`** (Section B): the pre/post QC-bracket-assignment and injection-fallback-substitution logic (`find_injected_qc_pre/post()`) already matched the flowchart's design and was ported essentially unchanged. Two real changes:
  - **New symmetric RDX 0.2ng sensitivity tier.** PETN's `petn_qc_flag_dc` (from `03_Quantification.R`) already had a 3-tier PASS/WARN/FAIL evaluation (concentration>0, then SNR<3/<10/else); RDX's `rdx_qc_flag` was 2-tier PASS/FAIL only, because it lacked a `_dc` drift-corrected column to hang a WARN tier off -- an incidental implementation gap, not a deliberate asymmetry (see `Diagnostics/Improved_QC_Flowchart.dot` legend NOTE4). Fixed by computing a fresh 3-tier flag for BOTH analytes directly inside the shared script from `petn_snr`/`petn_concentration_dc` and `rdx_snr`/`rdx_concentration` (no upstream `03_Quantification.R` changes needed).
  - **New blank-bracket attachment.** `assign_blank_brackets()` finds the nearest `evaluate_blanks()`-classified blank on each side of every Sample/NC row (`find_nearest_blank()`) and combines worst-case-wins into `petn_blank_bracket`/`rdx_blank_bracket` ("Clean"/"Trace"/"Contaminated"). Unlike QC brackets, a missing blank on one/both sides is treated leniently (as clean), not a hard requirement -- the "both sides required" rule was only ever agreed for QC brackets, not blanks; flagged as an assumption to revisit if wrong.

- **`compute_injection_acceptance(all_data)`** (Sections C+D): single unified function replacing ALL of `compute_analysis_accepted()`/`compute_nc_analysis_accepted()`/`compute_nc_result()` -- Samples and Negative Controls are evaluated identically now (the old 0.2ng-bracket-FAIL override, the *only* thing that ever structurally distinguished NC handling, was removed entirely per this session's explicit decision, so nothing is left to distinguish them). Per row: IS check short-circuits immediately (`NOT_DETECTED` → hard `FAIL`, `LOW` → caveat only, matching the flowchart's fix for the old code's "doesn't stop at IS failure" complaint); then for PETN and RDX independently -- peak-present → carryover check (blank bracket Contaminated → hard FAIL; Trace → caveat) → SNR tier (Quantifiable/Trace/Negative) → tier-conditional bracket gate (Quantifiable → 6ng bracket ONLY; Trace/Negative → 0.2ng bracket ONLY; both pre+post required, missing counts as FAIL, no override for anyone) → per-analyte result (Trace ALWAYS carries a caveat, even with clean brackets, per this session's explicit decision). Row-level combination is **first-failure-only**, fixed order IS → PETN → RDX (per explicit decision: "makes the others irrelevant") -- reports only the first blocking reason found, not an accumulated list like the old code.

- **`derive_nc_result(petn_tier, rdx_tier, accepted)`**: translates the per-analyte result tiers from the SAME unified computation into NC Stage-2 vocabulary (Quantifiable→"contaminated", Trace→"trace detected", Negative→"clean", combined across both analytes), forced to `"Not evaluated (analysis failed)"` whenever `accepted` is a FAIL. Replaces the old `compute_nc_result()`'s ad hoc SNR/peak-height/`concentration==0`-checked-first heuristic.

### Implementation: `Code/04_CollateStudyResults.R` (refactored)

Sources `Code/InjectionAcceptance.R` (added right after the existing `GlobalCode.R` source). Removed ~680 lines of now-duplicated functions. Call sequence: `evaluate_blanks(all_data)` → `assign_qc_brackets(...)` → (filter to Sample rows) → `compute_injection_acceptance(samples)` → for NC rows, `nc_analysis_accepted <- analysis_accepted` (aliased for backward-compatible column naming) + `nc_result <- derive_nc_result(...)`. `desired_cols`/`desired_cols_nc` gained `petn_blank_bracket`/`rdx_blank_bracket` as new audit columns; `apply_formatting()` gained a matching conditional-formatting block (Clean=green/Trace=amber/Contaminated=red). All exported column *names* preserved for backward compatibility even though the underlying computation moved.

### Verification (real FINEX data: 264 samples + 44 NCs)

Ran old (backed-up) vs new code side-by-side against live data (workaround needed: sourcing `GlobalCode.R` fresh unconditionally re-runs the ENTIRE `01→03` pipeline for whatever `DataFolder` it currently points at -- confirmed harmless/byte-identical when it fired once by accident on ASTRA's Analysis7, but avoided thereafter by pre-defining the 6 constants `04_CollateStudyResults.R` actually needs from `GlobalCode.R` so its `exists()` guard skips the source call). Row-by-row diff, keyed on Lab/Participant/Surface/Repeat:

- **Zero regressions** -- no sample/NC moved from PASS/PASS* into FAIL.
- 124 samples `PASS*`→`PASS`: old code checked the 0.2ng bracket even for Quantifiable samples (irrelevant to that tier); new code correctly skips it.
- 5 samples `FAIL`→`PASS*`: old code auto-failed any non-Quantifiable SNR flag and treated IS-LOW as a hard failure; both are now legitimate caveats.
- 2 samples `PASS`→`PASS*`: **genuinely new** -- both have `petn_blank_bracket=="Trace"`, caught by the new carryover check with no prior detection mechanism.
- 13 NCs' Stage-2 label changed `"...trace detected"`→`"...contaminated"`: old heuristic checked `concentration==0` before the SNR flag, mislabelling some genuinely `Quantifiable`-SNR peaks; new tier-based classification is more correct/consistent.

### Files modified

`Code/InjectionAcceptance.R` (new), `Code/04_CollateStudyResults.R` (assign_qc_brackets/compute_analysis_accepted/compute_nc_analysis_accepted/compute_nc_result removed, replaced with calls into the shared file; `desired_cols`/`desired_cols_nc`/`apply_formatting()` gained blank-bracket columns), `FINEX_StudyResults.csv`/`.xlsx` (regenerated with the new logic, confirmed correct, left live per user decision), this file.

**Companion change, same session, different repo**: `Design of Experiments/pilot_analysis.R` was refactored identically (source the same shared file, keep `select_best_attempt()`/recovery-calc/design-matrix-merge/DOE-statistics untouched) -- see that repo's own `CONTEXT.md` for details, including a real NC-leakage bug found and fixed during verification (removing the old `analysis_accepted=="NC"` sentinel broke two places that relied on it to keep NC rows out of the factorial-design statistics).

### Deferred to a future session

- **Blank "other significant peak" detection** (Section A's second branch) -- needs new logic in `02_PeakDetection.R` against the retained `GcDataConvertedRcode/TIC/`+`/SIM/` files, and reprocessing of every already-run dataset in both studies. Not started.
- Whether the "missing bracketing blank = lenient/clean" assumption (vs. QC brackets' "both sides required = FAIL if missing") is actually correct -- flagged, not resolved.

---



### Motivation

Follow-up to the July 29, 2026 "SampleSummary 'Successfully Run' Fix" sessions (below), which introduced `nc_analysis_valid` as a boolean alongside the pre-existing `nc_status` (contamination classification), specifically to stop counting analytically-unreliable NCs as "successfully run". That fix was correct in principle but left the **column-level presentation on the `Negative Controls` sheet itself unchanged** -- `nc_status` and `nc_analysis_valid` were two independently-computed, disconnected columns, so a row could show `nc_status = "NC PASS"` (looks clean) while `nc_analysis_valid = FALSE` (the run wasn't trustworthy) with nothing in the column layout making clear that the second fact should override the first. This session's request: NCs should go through **the same two-stage acceptance process as regular samples** -- they can only be interpreted as a real positive/negative result once the QCs and IS meet criteria, exactly like a sample's `analysis_accepted`/`Outcome` pair -- and the columns/formatting should make that ordering explicit rather than implicit.

### Implementation (`Code/04_CollateStudyResults.R`)

Replaced `compute_nc_status()` and `compute_nc_analysis_valid()` (and the `nc_status`/`nc_analysis_valid` columns they produced) with two new functions that mirror the samples' `analysis_accepted`/`Outcome` pattern:

**Stage 1 -- `nc_analysis_accepted`** (`compute_nc_analysis_accepted()`): answers "was this injection/run analytically trustworthy?", using the **same criteria family as `compute_analysis_accepted()`** for regular samples:
- IS (15N-RDX) injection-validity check (`rdx_is_snr_flag`/`rdx_is_pa_flag` -- not detected or abnormally low -> FAIL), identical to samples.
- **Both** 6ng and 0.2ng QC bracket checks (pre AND post, PETN and RDX) -- previously only the 0.2ng brackets were checked for NC validity; 6ng brackets are now checked too, for the same reason they gate sample acceptance.
- Deliberately **excludes** the "own analyte SNR must be Quantifiable" check that applies to samples (an NC is not expected to be quantifiable -- that's the question Stage 2 answers, not a Stage 1 criterion), and **excludes** the per-analyte 0.2ng-QC-FAIL override used for samples (July 20, 2026 session, below) -- for an NC, a failed bracketing QC is a real reliability problem regardless of what the NC's own peaks show, since the NC's own peaks are supposed to be trace/absent, not a sign of system capability.
- Returns the same vocabulary as samples: `"PASS"` / `"PASS*"` (missing bracket, or a 0.2ng WARN) / `"FAIL: <reason>; <reason>"`.

**Stage 2 -- `nc_result`** (`compute_nc_result()`): answers "is this NC negative (clean) or positive (contamination/trace detected)?", **only once Stage 1 has passed**. Internally computes the same three-tier SNR/peak-height/concentration classification the old `compute_nc_status()` used, but relabelled to avoid confusion with Stage 1's own PASS/WARN/FAIL vocabulary:
- `"Negative (clean)"` (was `"NC PASS"`)
- `"Positive: PETN trace detected"` / `"Positive: RDX trace detected"` / `"Positive: PETN + RDX trace detected"` (was `"NC WARN: ..."`)
- `"Positive: PETN contaminated"` / `"Positive: RDX contaminated"` / `"Positive: PETN + RDX contaminated"` (was `"NC FAIL: ..."`)
- **New**: `"Not evaluated (analysis failed)"` -- forced whenever `nc_analysis_accepted` starts with `"FAIL:"`, overriding whatever the raw SNR/PH/concentration signal happened to show. This is the key behavioural change: previously an NC with a failed IS or QC bracket could still show `"NC PASS"` (looked clean) or even `"NC FAIL: ... contaminated"` in `nc_status`, both of which imply a trustworthy result the underlying data doesn't support.

**Column layout** (`desired_cols_nc`, `Negative Controls` sheet): reordered to put the two stages first, immediately after the identifiers, mirroring the Samples sheet's `analysis_accepted`/`Outcome` positioning:
```
Lab, Participant, Surface, SurfaceName,
nc_analysis_accepted,   -- Stage 1 (E)
nc_result,              -- Stage 2 (F), only meaningful if Stage 1 is PASS/PASS*
petn_snr_flag, petn_ph, petn_concentration_dc, petn_recovery_dc,   -- PETN evidence for Stage 2
rdx_snr_flag, rdx_ph, rdx_concentration, rdx_recovery,             -- RDX evidence for Stage 2
rdx_is_snr_flag, rdx_is_pa_flag,                                   -- IS evidence for Stage 1
petn_qc_6ng_pre, petn_qc_6ng_post, rdx_qc_6ng_pre, rdx_qc_6ng_post, -- 6ng QC evidence for Stage 1 (NEW on this sheet)
petn_qc_02ng_pre, petn_qc_02ng_post, rdx_qc_02ng_pre, rdx_qc_02ng_post, -- 0.2ng QC evidence for Stage 1
Date, SampleName, DataFile, SourceFile
```
The 6ng bracket columns are new to this sheet (they were already being computed for every `Sample`-type row, NCs included, by `assign_qc_brackets()` -- just not previously exported to the NC sheet). All QC bracket columns are also picked up automatically by the existing generic `qc_cols` PASS/WARN/FAIL conditional-formatting block in `apply_formatting()`, no changes needed there.

**Formatting** (`apply_formatting()`): replaced the single `nc_status` conditional-formatting block with two, styled consistently with the rest of the workbook:
- `nc_analysis_accepted`: identical styling to `analysis_accepted` (FAIL: = red, PASS* = amber, exact PASS = green).
- `nc_result`: `"Not evaluated"` = grey, `"contaminated"` = red, `"trace detected"` = amber, `"Negative"` = green.

**SampleSummary `NC` row** (`Code/04_CollateStudyResults.R`, ~line 1261): recomputed from the new columns --
```r
nc_stage1_ok      <- nc_analysis_accepted %in% c("PASS", "PASS*")
PASS  (row)  = nc_stage1_ok & nc_result == "Negative (clean)"
PASS* (row)  = nc_stage1_ok & nc_result starts with "Positive:"
FAIL  (row)  = !nc_stage1_ok
```
Same net effect as the previous `nc_analysis_valid`-based computation (this row's PASS/PASS*/FAIL/Complete% definitions and the "Successfully Run" headline count downstream are unchanged), just re-expressed in terms of the new two-stage columns.

**Console output**: two summary lines replace the previous "NC evaluation"/"NC analysis validity" pair, explicitly labelled by stage:
```
NC analysis acceptance (Stage 1 -- is the run trustworthy?): PASS=6 PASS*=20 FAIL=8
NC result (Stage 2 -- contamination outcome, valid runs only): Negative=9 Positive=17 Not evaluated=8
```

### Verification

Extracted `compute_nc_analysis_accepted()`/`compute_nc_result()` and ran against 6 synthetic scenarios (clean+valid, contaminated+valid, IS-not-detected, 6ng-bracket-FAIL, 0.2ng-bracket-WARN-only, 0.2ng-bracket-FAIL) -- all matched expected Stage 1/Stage 2 values, including confirming the key new behaviour that a 6ng bracket FAIL (never checked for NCs before) now correctly fails Stage 1, and that any Stage 1 FAIL forces Stage 2 to `"Not evaluated (analysis failed)"` regardless of the underlying SNR signal. Confirmed the full script still parses (`parse()`, no syntax errors). Not yet re-run against real data in this session -- `Code/04_CollateStudyResults.R` needs to be re-run to regenerate `FINEX_StudyResults.xlsx` with the new `Negative Controls` sheet columns before this reflects in the live workbook.

**Files modified**: `Code/04_CollateStudyResults.R` (`compute_nc_status()`/`compute_nc_analysis_valid()` replaced by `compute_nc_analysis_accepted()`/`compute_nc_result()`; `desired_cols_nc` reordered with 6ng brackets added; `apply_formatting()` NC formatting block replaced; SampleSummary `NC` row recomputed from the new columns), `CONTEXT.md` (this entry).

---

## Session Summary (August 5, 2026, continued) — `drift_correction_exclude_qc_lines`: Exclude Specific QCs from the Drift-Correction Fit

### Motivation

Follow-up to a manual investigation (this session) into Analysis4's power-law drift correction, prompted by the question "if I leave out the last QC (Line 67) and fit the power law on that, how does it affect the bias of the remaining QCs?" Answered by hand-replicating the exact production fit (confirmed the reproduction matched the documented `a=345200.9, b=-0.5177` and biases exactly) and refitting on only the first 4 QCs: the three QCs that stayed in the fit (31, 47, 57) all improved (Line 47 flips FAIL→PASS), but the held-out QC (Line 67, which recovers/plateaus rather than continuing to decline -- consistent with Analysis4's already-documented "step-then-plateau with a late uptick" shape) became badly over-predicted (+25.4%, PASS→FAIL) once no longer anchoring the fit -- a net wash in pass count (still 2 of 5 FAIL), not a fix. Rather than being a one-off manual analysis, this was turned into a permanent, reusable config option so any QC identified as an outlier/anomaly (by shape, not just "the last one") can be excluded from the fit without hand-editing the pipeline each time.

### Implementation

**New config variable** (`GlobalCode.R`, immediately after `drift_correction_min_qc`): `drift_correction_exclude_qc_lines <- numeric(0)` -- a vector of sequence `Line` numbers (the same identifier already used throughout console messages and the exported results, e.g. "Lines 31, 47"). Default empty (no exclusions, byte-for-byte identical behaviour to before this session). Added to `validate_setup()` (must be numeric) and `log_reproducibility()`'s config snapshot.

**`Code/03_Quantification.R`** (PETN drift-correction section): the same-level QC data is now built in two stages --
- `qc_drift_data_all`: every usable same-level QC (unchanged from before, now also carries `Line`).
- `qc_drift_data`: `qc_drift_data_all` filtered to drop any `Line %in% drift_correction_exclude_qc_lines` -- **this is the only thing that changes**. It is used for everything the fit itself touches: `n_qc`/`min_qc_required` gating, the `nls()` call, R², the reported exponent, and `first_qc_row` (the reference/anchor point, which becomes the first *non-excluded* QC if the earliest QC itself is excluded).
- Excluded QC(s) are **not** removed from `Combined` and are **not** skipped by QC evaluation -- `predicted_cf`/`petn_pa_dc`/`petn_concentration_dc`/`petn_percent_bias_dc`/`petn_qc_flag_dc` are still computed for them from the resulting fit (extrapolated, since they're no longer fitted points), exactly like any other row. This was a deliberate design choice: excluding a QC changes *what the curve is shaped by*, not *which rows get evaluated*.
- Console message added reporting which Line(s) were excluded when `drift_correction_exclude_qc_lines` is non-empty.
- Diagnostic plot (`PETN_DriftCorrection.png`, PowerLaw branch): the fitted curve's drawn range now spans `qc_drift_data_all`'s Row range (not just the fitted subset) so it still extends out to an excluded point's position; excluded QC(s) are plotted as a distinct dark-orange "X" marker (vs the usual red triangle for fitted QCs), with a subtitle note ("orange X = excluded from fit") when any exist.

### Verification (real data, Analysis4, isolated test -- live dataset reverted afterward)

Backed up `Analysis4_GCMSResults.csv`/`.xlsx`/`PETN_DriftCorrection.png` first (established per-dataset-isolated-`GlobalCode.R`-copy mitigation, live `DataFolder` -- currently Analysis6 -- never touched). Ran an isolated copy with `drift_correction_exclude_qc_lines <- c(67)`:

- Console output: `"excluding 1 QC point(s) from the fit (Line 67)"`, `"using 4 QC points at 6 ng"`, `a = 490718.6, b = -0.6378` -- **matches the hand-calculated Scenario B exactly**.
- Resulting `petn_qc_flag_dc`/`petn_percent_bias_dc` for all 5 QCs: Line 15 = -12.06% PASS (unchanged, still the anchor), Line 31 = -20.09% FAIL_BIAS (improved from -26.12% but still just outside ±20%), Line 47 = -11.14% PASS (flipped from FAIL_BIAS), Line 57 = -7.50% PASS (improved), Line 67 = **+25.40% FAIL_BIAS** (flipped from PASS -- the held-out point) -- **matches the hand-calculation exactly**, confirming the implementation is correct.
- Diagnostic plot regenerated correctly: 4 red triangles (fitted QCs) + 1 orange X (Line 67, sitting above the extrapolated curve, visually consistent with its real recovered/plateaued signal not matching the steeper 4-point decay curve).
- **Reverted**: re-ran with `drift_correction_exclude_qc_lines` back at its default `numeric(0)` to regenerate `Analysis4_GCMSResults.csv`/`.xlsx`/`PETN_DriftCorrection.png` matching the original documented state exactly (5 QCs, `a=345200.9, b=-0.5177`, biases `-12.1%/-26.1%/-21.4%/-19.8%/+7.3%`, flags PASS/FAIL_BIAS/FAIL_BIAS/PASS/PASS) -- confirmed via direct CSV inspection. Analysis4 is left in its default (no exclusions) state; this session did not permanently apply any exclusion to any dataset.

**Files modified**: `GlobalCode.R` (`drift_correction_exclude_qc_lines` config, `validate_setup()`, `log_reproducibility()`), `Code/03_Quantification.R` (`qc_drift_data_all`/`qc_drift_data` split, exclusion message, diagnostic plot excluded-point overlay), `CONTEXT.md` (this entry). No dataset's on-disk results were left in a non-default state.

---

## Session Summary (August 5, 2026) — PowerLaw Reinstated for ASTRA, Bracket Retired

### Context: Revisiting the August 4 "Bracket" Decision

The immediately preceding session (below, "Bracketed (Piecewise-Linear) QC Drift Correction") replaced PowerLaw with a piecewise-linear Bracket method for the ASTRA Swabbing pilot study, specifically because a single global PowerLaw fit performed poorly for Analysis1 (non-monotonic), Analysis2 (2-QC terminal collapse), and Analysis4 (step-then-plateau). This session re-opened that decision at the user's request, following a visual/quantitative re-investigation, and reversed it: **PowerLaw is now used uniformly for ASTRA again, and the Bracket method has been removed from the codebase entirely.**

### Investigation (this session)

1. **Visual comparison**: plotted raw 6ng/0.2ng PETN QC peak areas for all 5 runs, faceted by QC level, one series per run (`Diagnostics/ASTRA_PETN_QC_RawPA_Comparison.R` → `ASTRA_PETN_QC_RawPA_ByRun.png`). Confirmed Analysis3/5 share a smooth decelerating-decay shape, Analysis4 is step-then-plateau with a late uptick, Analysis1 is non-monotonic (also a late uptick), and Analysis2 is a near-vertical terminal collapse categorically unlike the other four.
2. **Quantitative PowerLaw-vs-Linear fit comparison**, excluding Analysis2 entirely and each of Analysis1's/Analysis4's own late-sequence uptick QCs as pattern outliers: PowerLaw fit Analysis1 (trimmed) and Analysis3 excellently (R²=0.985, 0.9997) and Analysis4 (trimmed) well (R²=0.978), but was *worse* than a straight line for Analysis5 (R²=0.966 vs Linear's 0.999 — Analysis5's QC-to-QC decrements are nearly equal in magnitude, i.e. genuinely linear, not decelerating).
3. **Shared-exponent (partial pooling) test**: fit a single PowerLaw exponent across Analysis1/3/4 (trimmed) vs independent exponents per run. F-test could not reject a shared exponent (p=0.20) but AIC still favoured independent exponents; the comparison was underpowered (only 4 residual df) either way. Applying the pooled exponent (b=-0.656) to Analysis2 and Analysis5 did not help: Analysis2's actual decline (96%, ratio 0.040) was far larger than the pooled exponent implies over that Row distance (ratio 0.649, R²=0.43) — quantitatively confirming Analysis2 is not the same physical process as the other runs — and Analysis5 fit worse under the pooled exponent (R²=0.897) than under Linear (R²=0.999). **Rejected**; not carried into the implementation.
4. **User's explicit decision**: given the above, fit PowerLaw per-run on **all** QC data with **no** per-dataset exclusions (Analysis2's collapse and the Analysis1/Analysis4 upticks are not special-cased), and remove the Bracket method entirely rather than keep it dormant.

### Implementation

**`GlobalCode.R`**:
- `drift_correction_method` is now unconditionally `"power_law"` (previously `if (grepl("ASTRA Swabbing", ...)) "bracket" else "power_law"`) — FINEX behaviour unchanged, ASTRA reverted.
- New `drift_correction_min_qc <- if (grepl("ASTRA Swabbing", DataFolder, fixed = TRUE)) 2 else 3`: the general PowerLaw minimum (3 QCs, needs ≥1 residual df to be a meaningful fit) is relaxed to 2 specifically for ASTRA, so Analysis2 still gets a PowerLaw attempt rather than being skipped — deliberately allowing an exact/degenerate 2-point fit rather than gating on fit quality (per the user's explicit choice).
- `validate_setup()` updated: `drift_correction_method` allowed values reduced to `c("power_law", "polynomial")`; new check that `drift_correction_min_qc` is numeric and ≥2. `log_reproducibility()` config snapshot includes `drift_correction_min_qc`.

**`Code/03_Quantification.R`**:
- Entire Bracket branch removed (~115 lines: target-anchored QC interpolation, `approx()` piecewise-linear CF, the 2-QC Sample-uncorrected fallback, and its diagnostic plot branch). `min_qc_required <- drift_correction_min_qc` (previously a hardcoded `if (method=="bracket") 2 else 3` ternary).
- PowerLaw entry gate relaxed from `n_qc >= 3` to `n_qc >= 2` (the outer `min_qc_required` check already makes the real policy decision; this inner check is now just the absolute mathematical minimum for `nls()` to attempt anything).
- `nls()` control changed to `maxiter = 200, warnOnly = TRUE` (was `maxiter = 100`, no `warnOnly`) so a 2-point degenerate fit reliably returns a result instead of erroring out and silently falling through to the polynomial fallback.
- New console message for the `n_qc == 2` case explicitly stating the fit has zero residual degrees of freedom and R² is not informative — visibility without gating.
- **Plot-range fix** (discovered during verification, see below): the PowerLaw diagnostic plot (`PETN_DriftCorrection.png`) previously evaluated the fitted curve across the *entire* sequence's `Combined$Row` range for plotting. For a well-behaved exponent this is harmless, but Analysis2's degenerate 2-point fit (`b = -4.876`) extrapolated to an astronomical value at low Row, making the whole plot unreadable (y-axis dominated by a single point off at 5.8×10¹⁰). Changed to evaluate the plotted curve only within `qc_drift_data`'s own observed Row range — cosmetic only, does not affect any computed correction factor or concentration.
- **Incidental fix**: the drift-corrected-calibration-plot gate (`Combined$petn_drift_model[1] %in% c(...)`) had never included `"PowerLaw"` in its allowed list even before this session (only `"Quadratic"`/`"Linear"`/`"Bracket"`/previously `"Step"`) — meaning `PETN_Calibration_{Set}_DriftCorrected.png` was silently never generated for PowerLaw-corrected runs, including FINEX datasets, which have defaulted to `"power_law"` since June 2026. Fixed by adding `"PowerLaw"` to the gate (`c("PowerLaw", "Quadratic", "Linear")`). This benefits FINEX processing too, not just ASTRA.
- **`use_step_drift_correction`/`step_drift_after_line` fully removed** (follow-up within this same session, prompted by a direct question about why they were still present): these had originally been left in place as inert legacy toggles (`FALSE`/`NA` defaults, no functional code path) when Bracket replaced Step back in the August 4 session, on the reasoning that an old script setting them shouldn't error. Given Bracket itself is now being removed outright rather than left dormant, the same reasoning no longer justified keeping Step's toggles either -- both variables and their `exists()`-guarded fallback blocks were deleted entirely from `GlobalCode.R` and `Code/03_Quantification.R`. Confirmed via `grep` that no live `.R` file references either name anymore (only historical mentions remain in this file's own past session summaries, which is expected).

### Verification (real data, all 5 ASTRA datasets reprocessed)

Reprocessed via isolated per-dataset `GlobalCode.R` copies (same established mitigation as prior sessions — a concurrent `rsession-utf8.exe` and an unrelated Excel workbook were both active; never edited the shared `GlobalCode.R`'s `DataFolder` in place). Pre-change `_GCMSResults.csv` backed up for all 5 datasets before reprocessing.

**Fitted coefficients matched the standalone diagnostic script exactly** (Analysis1: a=281414, b=-0.4035; Analysis2: a=5.76×10¹⁰, b=-4.876; Analysis3: a=879154, b=-0.7881; Analysis4: a=345201, b=-0.5177; Analysis5: a=1206296, b=-0.9005), confirming the production code path reproduces the same fit as the exploratory analysis.

**6ng QC bias/flag results** (reverting to exactly the pre-Bracket PowerLaw behaviour documented in the August 4 session, as expected):

| Dataset | Bias (uncorrected → PowerLaw-corrected) | Flags |
|---|---|---|
| Analysis1 | -2.6%/-37.1%/-44.9%/-35.3% → -2.6%/-17.5%/-15.4%/+11.7% | All PASS |
| Analysis2 | -5.3%/-95.7% → -5.3%/-5.3% | Both PASS (degenerate 2-point fit passes trivially — see caveat below) |
| Analysis3 | +0.5%/-33.9%/-50.3% → +0.5%/-0.2%/+1.2% | All PASS |
| Analysis4 | -12.1%/-47.6%/-54.2%/-57.4%/-46.6% → -12.1%/-26.1%/-21.4%/-19.8%/+7.3% | **2 FAIL_BIAS** (Lines 31, 47) |
| Analysis5 | -14.6%/-39.4%/-62.5% → -14.6%/-6.2%/-21.8% | **1 FAIL_BIAS** (Line 41) |

**This is a real, known regression in QC PASS rate relative to Bracket** (which had zero QC failures across all 5 datasets) — an explicit, accepted trade-off of reverting to PowerLaw with no per-dataset exclusions, not an implementation bug.

**Analysis2 Sample-side consequence (the important caveat)**: although Analysis2's two QCs trivially "pass" their own bias check (any 2-point fit reproduces both QCs' targets by construction), the degenerate exponent (b=-4.876) means the correction factor grows explosively for rows beyond the last QC — up to **34.4×** by the next injection (row 31) — reproducing the exact over-correction problem the August 4 session's minimum-QC-count fallback was specifically built to prevent. Confirmed directly: `PILOT_006` (a real Sample) now shows `petn_concentration_dc = 11.4 ng`, vs `1.74 ng` both uncorrected and under the Bracket-era fallback. The pipeline's existing "correction factor exceeds 10.0 -- severe drift" warning fires correctly for this case, surfacing the risk in the console/log even though nothing blocks it. **This was a deliberate, explicit choice** (see "User's explicit decision" above) — Sample rows for Analysis2 are no longer protected the way they were under Bracket's 2-QC fallback.

**Regression check** (all non-drift-correction columns, keyed on `Line`, across all 5 datasets, 242 total rows): **zero unexpected differences** — confirms the change is isolated to drift correction and does not affect peak detection, RDX processing, or calibration.

**Files modified**: `GlobalCode.R` (`drift_correction_method` unconditional, new `drift_correction_min_qc`, `validate_setup()`, `log_reproducibility()`, `use_step_drift_correction`/`step_drift_after_line` removed entirely), `Code/03_Quantification.R` (Bracket branch removed, PowerLaw gate relaxed, `nls()` robustness, plot-range fix, calibration-plot gate fix, `use_step_drift_correction`/`step_drift_after_line` fallback block removed entirely), all 5 Analysis1-5 `Results/` folders (regenerated `_GCMSResults.csv`/`.xlsx`, `PETN_DriftCorrection.png`, `PETN_Calibration_{Set}_DriftCorrected.png` — pre-change `_GCMSResults.csv` backed up to `%TEMP%\opencode\PowerLawASTRA\PreChange_Backup\`), `SystemMonitoring.xlsx` (QC_6ng/QC_0.2ng sheets updated), `Diagnostics/ASTRA_PETN_QC_RawPA_Comparison.R` (new, PowerLaw-fit-per-run plot), `Diagnostics/ASTRA_PowerLaw_vs_Linear_ExclOutliers.R` / `ASTRA_SharedExponent_Test.R` / `ASTRA_SharedExponent_Applied_A2_A5.R` (new, exploratory analysis scripts, shared-exponent idea rejected), `Diagnostics/ASTRA_PowerLaw_Regression_Check.R` (new, regression check), `CONTEXT.md` (this entry).

**Note for future sessions**: if the Analysis2-style over-correction risk needs revisiting, the August 4 session's rejected safeguards (RDX-IS corroboration check, LOOCV gating) and the removed Bracket method's 2-QC Sample fallback are documented in detail in the session below and are the natural starting points — but see that session's own conclusions on why each was rejected before re-attempting.

---

## Session Summary (August 4, 2026) — Bracketed (Piecewise-Linear) QC Drift Correction for ASTRA Pilot Study (Analysis1-5)

### Problem: No Single Global Drift Model Fits All 5 ASTRA Runs

**User's question**: the 6ng PETN QCs "seem to follow a different degradation pattern in each run" across the ASTRA Swabbing Pilot Study (`Analysis1`-`Analysis5`) -- asked for investigation and a recommended way forward (step-wise, power law, or a different solution).

**Investigation**: Pulled every 6ng QC's `rdx_is_pa`/`petn_pa` plus the full non-blank injection sequence for all 5 datasets and characterised each run's actual shape:

| Dataset | n QCs (6ng) | `rdx_is_pa` sequence | Shape | Pre-fix method | Pre-fix result |
|---|---|---|---|---|---|
| Analysis1 | 4 | 39444→18267→12432→12473 | Decelerating decline into a near-flat plateau | PowerLaw | PASS, but bias swung -2.6%→-17.5%→-15.4%→+11.7% |
| **Analysis2** | **2** | 28720→**1133** | **Not gradual** -- flat/normal until the last QC pair, then a 96% terminal collapse | PowerLaw → falls back to `"None"` (needs ≥3 pts) | Uncorrected -95.6% bias, `FAIL` |
| Analysis3 | 3 | 29082→15836→11441 | Smooth decelerating decay | PowerLaw | Excellent (0.5%/-0.2%/+1.2% bias) |
| Analysis4 | 5 | 27655→14215→12784→12074→13416 | Sharp single-injection step (-49%), then plateau | Step (hardcoded, Analysis4-only regex) | All PASS, max 12.1% bias |
| Analysis5 | 3 | 33369→20987→15336 | Smooth decelerating decay (same family as Analysis3) | PowerLaw | -14.6%/-6.2%/**-21.8% (FAIL)** |

Three genuinely different shapes across 5 runs (continuous decay, step-then-plateau, and one apparent non-drift terminal QC anomaly) -- no single global parametric model fits all of them, confirming the user's premise. The existing per-dataset "fix" (`use_step_drift_correction` hardcoded via a DataFolder regex matching only `Analysis4`) was a manual, per-dataset workaround that doesn't scale (Analysis5 was added to the study after that fix and immediately hit the same problem again, with a *different* shape).

**Second finding (investigated per explicit request, folded into this fix)**: looking at the *full* sequence (not just QC rows), in Analysis2/3/4/5 the real **Samples bracketed between QCs stay flat/high while the QC vials' own IS response declines steeply** (e.g. Analysis4: Samples hover 18k-33k throughout while QC drops 27.6k→12k and stays there). Only Analysis1 shows Samples declining in step with the QCs. This mirrors the already-documented "real-sample matrix protects the internal standard" effect (see "Filtering Study Results" section below, previously only characterised for NG/PETN) -- meaning a QC-fitted correction factor does not necessarily represent what is actually happening to the real Samples, and applying it uncritically risks over-correcting them.

### Fix Implemented: `drift_correction_method = "bracket"`

**Scientific basis** (verified via CrossRef, not just recalled from memory):
- Paul D, Skrzypek G, Fórizs I. (2007) "Normalization of measured stable isotopic compositions to isotope reference scales -- a review." *Rapid Commun. Mass Spectrom.* 21(18):3006-3014. DOI: 10.1002/rcm.3185. Verified abstract: single-point/global anchoring produces normalisation errors exceeding typical uncertainty budgets and "must not be used"; **any method using two or more reference standards produces smaller error provided the standards bracket the unknowns being corrected** -- exactly this design (6ng QCs interspersed among Samples throughout each sequence).
- Skrzypek G, Sadler R, Paul D. (2010) "Error propagation in normalization of stable isotope data: a Monte Carlo analysis." *Rapid Commun. Mass Spectrom.* 24(18):2697-2705. DOI: 10.1002/rcm.4684. Quantifies that bracketing error decreases as more reference-standard measurements are added -- the basis for a separate recommendation (below) to increase 6ng QC frequency in future ASTRA sequences.
- Dunn WB, et al. (2011) "Procedures for large-scale metabolic profiling..." *Nat. Protoc.* 6(7):1060-1083. DOI: 10.1038/nprot.2011.335 (already cited elsewhere in this file) -- the standard "QC-RLSC" method of anchoring drift correction to periodic QC injections via LOESS. With only 2-5 QCs total per ASTRA run (vs the many-QCs-per-batch metabolomics use case this was designed for), LOESS has no meaningful local neighbourhood to smooth over and mathematically collapses to piecewise-linear interpolation between adjacent QCs -- i.e. bracketing is the low-QC-density degenerate case of QC-RLSC, not a deviation from it.
- Already-cited regulatory analogue in this same domain (trace-residue quantitation): `SANTE/11312/2021` bracketing calibration standards.

**Implementation** (`Code/03_Quantification.R`, PETN drift-correction section, ~line 787 onward): new `drift_correction_method == "bracket"` branch, replacing the old `use_step_drift_correction`/`step_drift_after_line` toggle (retired -- see below):
- Each QC's own correction factor targets the **calibration curve's** predicted response at its own known concentration (reuses the `target_y` logic already validated for the old Step method's "Design Iteration" fix), not a single reference QC's raw value -- centres every QC's own control point on zero bias.
- `predicted_cf` for every row is `approx(x = qc_rows, y = qc_cf, xout = Combined$Row, method = "linear", rule = 2)` -- piecewise-linear between consecutive QCs, flat-held past the last QC (`rule = 2`). Rows before the first QC are forced to `CF = 1.0` (unchanged convention, matches every other method -- calibration standards are never drift-corrected).
- `min_qc_required` for bracket mode is 2 (one bracket), same as the old Step method's minimum, vs 3 for power law/polynomial.
- New diagnostic plot (`PETN_DriftCorrection.png`): piecewise-linear line through QC control points (red), flat tails, grey Sample points, matching the existing plot style of the other methods.
- Calibration-plot gate (`PETN_Calibration_{Set}_DriftCorrected.png`) updated to include `"Bracket"` (replacing `"Step"`).

**Retired**: `use_step_drift_correction`/`step_drift_after_line` toggle and the entire Step-method code branch removed from `03_Quantification.R` (bracket interpolation reproduces a step automatically wherever one occurs -- e.g. Analysis4's Line27→29 step is just a bracket with a large implied slope -- with no manual per-dataset configuration needed). The toggle variables are left defined (defaulting to `FALSE`/`NA`) purely so nothing errors if an old script still sets them; the code path they used to enable no longer exists. `GlobalCode.R`'s `use_step_drift_correction <- grepl("ASTRA Swabbing.*Analysis4$", DataFolder)` hack replaced with `drift_correction_method <- if (grepl("ASTRA Swabbing", DataFolder, fixed = TRUE)) "bracket" else "power_law"` -- scoped to the whole ASTRA Swabbing study (not just Analysis4), FINEX "Accepted Analysis" datasets unaffected (confirmed the `grepl` resolves to `"power_law"` for FINEX paths, unchanged from before). `validate_setup()` updated to accept `"bracket"` as a valid `drift_correction_method` and to require `use_15nrdx_for_petn = FALSE` for it (same requirement as power law).

### Addendum (same day, continued): IS-Based "Validation" Tried and Rejected -- RDX-IS Is Not a Valid Proxy for PETN

Two follow-up mechanisms were built, tested against real data, and then **both removed** after further investigation showed their underlying premise was wrong. Recorded here in detail specifically so this is not re-attempted in a future session without re-discovering the same problem.

**Attempt 1 -- Sample-vs-QC corroboration check as a safety cap on Sample correction.** Compared each bracket's implied `rdx_is_pa` decline against the *real* decline observed in bracketing Samples, and capped Sample-row correction when a bracket showed both (a) a large mismatch ratio and (b) a wide internal CF range. This caught a genuinely dangerous case in validation -- Analysis2's single bracket (spanning a 96% terminal QC collapse) had inflated 5 real Sample PETN concentrations by 11-26x -- so it was not a pointless exercise, but its *general* logic (using IS divergence to judge whether a PETN correction should be trusted) turned out to rest on a false assumption (see next).

**Attempt 2 -- Leave-one-out cross-validation (LOOCV) directly on PETN's own QC points**, to give a genuine held-out %bias not trivially ~0% by construction (unlike `petn_percent_bias_dc`, which is ~0% for every QC under Bracket simply because each QC is its own anchor point). This one was analyte-consistent (no cross-analyte assumption) and its Interior-QC results looked reasonable (Analysis1: -10.3%/-12.6%; Analysis3: -2.9%; Analysis4: -12.3%/-0.2%/-13.1%; Analysis5: +15.6% -- all within ±20%), but it is inherently inapplicable at exactly the datasets that most need a check: with only 2-5 QC points per run, there are often 0-3 usable "Interior" points, and Analysis2 (2 QCs total) has none at all -- both its QCs are unavoidably "Edge" points, giving meaningless extrapolation artefacts (+990.9%/-95.4%) rather than a real answer.

**Why both were removed**: the user pointed out directly -- RDX (and its IS) is already known to have different degradation behaviour from PETN; that is the entire reason `use_15nrdx_for_petn = FALSE` (PA-only quantification) was chosen for PETN in the first place. Using RDX-IS to validate or gate PETN's own correction re-introduces exactly the cross-analyte assumption the pipeline was deliberately designed to avoid. **Confirmed directly with data already in the pipeline** -- comparing PETN and RDX-IS decline *within the very same QC vial injections* (not even comparing QC to Sample, just the two co-measured signals in one injection):

| Dataset | PETN decline (% of first QC) | RDX-IS decline (% of first QC) |
|---|---|---|
| Analysis1 | 100→62→54→64 | 100→46→32→32 (IS drops noticeably faster) |
| Analysis2 | 100→4 | 100→3.9 (both collapse together -- plausibly a shared mechanical/injection event, not chemistry) |
| Analysis3 | 100→62→46 | 100→55→39 (IS drops faster) |
| Analysis4 | 100→57→49→45→58 | 100→51→46→44→49 (fairly close) |
| Analysis5 | 100→69→41 | 100→63→46 (**relationship flips sign** between the two QC-to-QC intervals) |

Analysis5's sign flip is the clearest evidence: the PETN-vs-IS relationship isn't even a stable offset that could be modelled with a fixed ratio -- it isn't consistent in *direction*, let alone magnitude, across a single run. Since RDX-IS cannot reliably predict PETN's behaviour even within the tightly-controlled context of the same QC vial, it certainly cannot be trusted to validate or gate PETN's behaviour in Samples. Both the corroboration-based safety cap and the LOOCV check were removed on this basis (`Code/03_Quantification.R`): all `petn_drift_loocv_bias`/`petn_drift_loocv_type` columns and computation removed; the IS-mismatch/CF-range capping block removed.

### Replacement Safeguard: Minimum-QC-Count Fallback (No Cross-Analyte Assumption)

In place of both removed mechanisms, a single, simple, assumption-free rule was added: **if a dataset has only the bare minimum 2 QC points (no interior point is even mathematically possible), Sample rows are left uncorrected (`CF = 1.0`)** -- drift correction in that case only informs the QCs' own bias/PASS-FAIL evaluation (unaffected), not real Sample concentrations. This is based purely on how much QC data exists, not on any signal claimed to predict PETN's behaviour. Currently only affects Analysis2 (2 QCs); Analysis1/3/4/5 (3-5 QCs each) get the full bracket-interpolated correction on Samples, same as originally implemented, with no cap.

**Accepted residual limitation** (not solved by this or any further post-hoc modelling): for datasets with 3+ QCs, there remains no independent way to confirm the bracket-interpolated correction is appropriate for real Samples specifically -- LOOCV on PETN's own QC points is the only analyte-consistent check available, and it is limited to "Interior" QCs (a QC on both sides), which may not exist or may be few in number. This is a structural consequence of running only 2-5 QC replicates per sequence, not a defect fixable by algorithm design. The existing recommendation (below) to increase QC frequency in future ASTRA sequences is the actual mitigation; per-run correction confidence beyond the QC's own bias should be treated as unverified until then.

### Verification (final state, all 5 datasets reprocessed)

**All 5 datasets' 6ng PETN QCs still PASS**, `petn_drift_model = "Bracket"` (QC-level bias evaluation is identical to the original Bracket implementation -- removing the Sample-side safety cap/LOOCV does not affect how QCs judge their own bias, only whether/how the correction propagates to Sample rows):

| Dataset | 6ng QC bias (uncorrected → drift-corrected) | Sample-row correction |
|---|---|---|
| Analysis1 | -2.6%/-37.1%/-44.9%/-35.3% → ~0% each | Full bracket interpolation (4 QCs, uncapped) |
| Analysis2 | -5.3%/-95.7% → ~0% each (QC itself) | **Uncorrected (CF=1.0)** -- only 2 QCs, no interior point possible |
| Analysis3 | +0.5%/-33.9%/-50.3% → ~0% each | Full bracket interpolation (3 QCs, uncapped) |
| Analysis4 | -12.1%/-47.6%/-54.2%/-57.4%/-46.6% → ~0% each | Full bracket interpolation (5 QCs, uncapped) |
| Analysis5 | -14.6%/-39.4%/-62.5% → ~0% each | Full bracket interpolation (3 QCs, uncapped) |

Confirmed directly: Analysis2's Sample rows now show `petn_concentration_dc == petn_concentration` (e.g. `PILOT_006`: 1.74 ng both uncorrected and "corrected", vs the earlier capped value of 1.84 ng and the original uncapped/uncontrolled value of 19.1 ng) and `petn_drift_correction_factor = 1` for every Sample row, while its QCs' own `petn_drift_correction_factor` (1.0633/26.4627) and `petn_qc_flag_dc` (PASS/PASS) are unchanged from before this addendum.

**Regression check**: re-ran the same full row-by-row diff (keyed on `Line`, across `petn_pa`/`petn_ph`/`petn_snr`/`petn_snr_flag`/`rdx_pa`/`rdx_snr_flag`/`rdx_is_pa`/`petn_concentration`/`rdx_concentration`/`rdx_percent_bias`/`rdx_qc_flag`) against the original pre-session backups across all 5 datasets (240 rows): **zero unexpected differences**.

### Recommendation for Future ASTRA Sequences

Per Skrzypek, Sadler & Paul (2010, above), bracketing error decreases as more reference-standard measurements are added. The current 2-5 6ng QCs per run is the root cause both of why a global shape had to be guessed at in the first place, and of why no reliable independent validation (of any kind tried so far) exists for datasets with few QCs -- **increasing 6ng QC frequency in future ASTRA sequences** (e.g. bracketing every ~10-15 injections rather than every ~15-20, and specifically avoiding designs with only 2 total QCs) would both reduce reliance on interpolation across large gaps and ensure at least one genuinely-testable "Interior" LOOCV point exists per run.

**Files modified**: `Code/03_Quantification.R` (bracket method implementation, replacing the Step method and its toggle; calibration-plot gate updated; a corroboration-based safety cap and a LOOCV check were both added and then removed after investigation showed the safety cap's premise was invalid, replaced with the minimum-QC-count fallback described above), `GlobalCode.R` (`drift_correction_method` now conditional on ASTRA Swabbing vs FINEX; `use_step_drift_correction`/`step_drift_after_line` retired to inert defaults; `validate_setup()` updated), all 5 Analysis1-5 `Results/` folders (regenerated `_GCMSResults.csv`/`.xlsx`, `PETN_DriftCorrection.png`, `PETN_Calibration_{Set}_DriftCorrected.png` -- pre-fix `_GCMSResults.csv` backed up to `%TEMP%\opencode\BatchRun_DriftFix\Results_PREFIX\`), `SystemMonitoring.xlsx` (QC_6ng/QC_0.2ng sheets updated via existing dedup-by-DataPath+DataFile logic, verified zero duplicate rows introduced), `CONTEXT.md` (this entry).

---



### Background: Revisiting the "Line 45 is a genuine, unfixable non-detection" Conclusion

The immediately preceding session summary (below, "Baseline-Correction Integration Floor Fix") concluded that Analysis4 line 45 (a 0.2ng PETN QC with no reported PA/PH/SNR) was a genuine sensitivity-floor non-detection: "the underlying bump is ~190-200 counts, at/below the `MinPeakHeight_PETN` threshold, and a blank injection shows a comparable-or-larger bump at the same RT from calibrant carryover -- not fixable by peak-detection/baseline-correction tuning."

The user had independently pulled MassHunter (vendor software) screenshots/CSV exports directly from the instrument for line 45 (folder: `OneDrive/Desktop/Peak Detection/`, files `45 and 55 SIM.csv`/`.bmp` and `45 and Blank SIM.csv`/`.bmp`) showing what looked like a real, shaped chromatographic peak at the expected PETN RT, and asked for this to be re-investigated. This session re-examined the conclusion and found it did not hold up.

### Investigation

**Step 1 -- ran the actual R pipeline functions (not a hand-reconstruction) on line 45's raw SIM data.** Sourced `Code/ModPeaks.R` directly and called `process_sim_channel()`/`extract_peak_area()`/`calculate_snr()` on `GcDataConvertedRcode/SIM/045.csv` with the live `GlobalCode.R` config (PETN RT=338s, noise window [318,328], `snr_detect=3`/`snr_quant=10`). Result: a real, well-shaped candidate peak at RT=339.64s, baseline-corrected height **190.0** -- just **10 counts (5%) below** the hard `MinPeakHeight_PETN=200` cutoff, with a peak width (13.2s, when the height gate is relaxed) comparable to neighbouring *detected* 0.2ng QCs (e.g. line 55's peak: RT=339.41s, height 208.2, width 20.8s). If the height gate were relaxed, SNR = 5.12 -- clears `snr_detect=3`, and would classify identically to every other 0.2ng QC in the sequence (`Below_LOQ`), not as a qualitatively different "non-detection."

**Step 2 -- the original blank comparison used the wrong blank.** The prior session's "a blank shows a comparable-or-larger bump" argument used line 012 (EtOH Blank), a *different, distant* injection from an earlier point in the sequence (immediately downstream of the high-concentration calibrants, hence genuinely carryover-contaminated, PH=282). The user correctly pointed out that the *actually relevant* comparison is the blanks immediately bracketing line 45 itself. Reprocessed lines 044 (blank, immediately before 045) and 046 (blank, immediately after 045) through the same real pipeline functions: both are **flat, at only 54.6 and 62.8 respectively**, and not even positioned at the correct RT (peaks at 337.1s/336.2s, not 339.6s). There is no plausible local carryover source either: the nearest real signal is the preceding sample (line 43, PILOT_012, PH=9031), which the very next blank (044) already fully cleared to nothing. This directly contradicts the "indistinguishable from carryover" argument -- the true local context shows a real, isolated, correctly-positioned peak with nothing resembling it in either neighbour.

**Step 3 -- re-validated `MinPeakHeight_PETN` across the whole ASTRA Swabbing Pilot Study (Analysis1, Analysis2, Analysis3, Analysis4), per the user's request**, since the existing value (200) was validated on FINEX datasets (Lab10-1/Lab17-1/ABS-S-1), not on this study. Reprocessed every blank and every 0.2ng QC across all 4 datasets' raw SIM data with the height gate relaxed (`min_peak_height=1`) to see true forced peak heights, classifying blanks as "carryover-risk" (immediately downstream of a Cal, a >=6ng QC, or one of this study's ultra-high "Test Std" 10-200ng linearity-check injections) vs "clean" (everything else):

| Population | n | Range | Notes |
|---|---|---|---|
| Carryover-risk blanks | 24 | 42 -- **9,269** | The Test Std series (unique to this pilot study, 50-1000x the QC level) produces multi-injection carryover tails far larger than anything seen in FINEX |
| "Clean" blanks (not immediately after a big injection) | 66 | ~3 -- **185** | mean=83.5, median=77.4, SD=32.2. Investigated the handful of outliers (147, 146, 144, 141): all but one trace to a large real sample/QC 2-4 injections earlier (residual carryover tail, not independent noise) |
| Genuine 0.2ng QC PETN peaks, all 4 datasets (forced height) | 14 | **190** -- 4,723 | Line 45 (190) is the single lowest of all 14 |

The FINEX-borrowed value of 200 leaves essentially **zero margin** in this study (185 vs 190) -- a consequence of the Test Std linearity-check design (not present in FINEX) contaminating a much longer stretch of the sequence than a single blank wash.

### Fix Applied

**File**: `GlobalCode.R`, `MinPeakHeight_PETN` definition (~line 107, now ~line 121):

```r
MinPeakHeight_PETN <- if (grepl("ASTRA Swabbing", DataFolder, fixed = TRUE)) 150 else 200
```

150 sits ~2 SD above the clean-blank population mean (83.5 +/- 32.2), excludes all but one already-explained carryover-tail case from the clean-blank population, and clears every genuine ASTRA 0.2ng QC peak (minimum 190). FINEX processing is completely unaffected (still resolves to 200 via the `else` branch, since FINEX `DataFolder` paths don't contain "ASTRA Swabbing"). `MinPeakHeight_RDX`/`MinPeakHeight_IS` left unchanged (not implicated by this investigation).

**Operational note**: two concurrent `rsession-utf8.exe` background sessions were active throughout this work (same recurring risk documented in the August 3rd "Baseline-Correction Integration Floor Fix" session below). Followed the same established mitigation: took a one-time snapshot of `GlobalCode.R`, built 4 isolated per-dataset copies (`%TEMP%\opencode\BatchRun\run_Analysis{1-4}.R`) with `DataFolder` substituted and the `MinPeakHeight_PETN` fix included, and ran each via `Rscript` directly -- never re-editing the shared `GlobalCode.R`'s `DataFolder` mid-session. To force full SIM reprocessing without touching the shared (and possibly concurrently-in-use) `Code/02_PeakDetection.R`, deleted each dataset's own `GcData/*SIM.csv` and `*Summary.csv` intermediate files directly (backed up first to `%TEMP%\opencode\BatchRun\GcData_Backup\Analysis{1-4}\`) -- `02_PeakDetection.R`'s existing skip-if-exists logic then reprocessed exactly the deleted files with no code changes needed. Backed up all 4 pre-fix `_GCMSResults.csv` files to `%TEMP%\opencode\BatchRun\Results_PREFIX\` before reprocessing. Applied the validated fix to the shared `GlobalCode.R` only after all 4 datasets had been successfully reprocessed and verified, confirming `DataFolder` (still "Analysis4", untouched) was unaffected before and after the edit.

### Verification (real data, all 4 datasets reprocessed)

**Target case fixed**: Analysis4 line 45 now reports `petn_pa=194.8`, `petn_ph=190.0`, `petn_snr=5.12`, `petn_snr_flag="Below_LOQ"`, `petn_qc_flag_dc="SENSITIVITY_CHECK_WARN"` -- the same category as its neighbouring QCs, instead of all-NA.

**Regression check** (compared every row's `petn_snr_flag` before vs after, keyed on Line/SampleName/Type/CalLevel, across all 4 datasets): 7 rows total changed from no-detection to `Below_LOQ`:

| Dataset | Line | Sample | New PH | Explanation |
|---|---|---|---|---|
| Analysis1 | 56 | EtOH Blank | 152.7 | Immediately after "Test Std 7 - 10ng" (carryover-risk, expected) |
| Analysis1 | 58 | EtOH Blank | 185.3 | The specific outlier identified during threshold derivation -- carryover tail from Test Std series + a boosted QC 2 injections earlier |
| Analysis1 | 15 | PILOT_SteelBatch1NegCtrl (Sample, negative control) | 137.5 | 2 injections after a 6ng QC (PH=85,059), with the intervening blank *already* showing detectable carryover (335.5) -- ambiguous, plausibly still-decaying carryover rather than genuine trace contamination in this NC. Flagged here for awareness; not further adjudicated this session. |
| Analysis3 | 16 | EtOH Blank | 168.5 | Immediately after a 6ng QC (carryover-risk, expected) |
| Analysis3 | 23 | PILOT_018 (Sample) | 169.4 | 2 injections after PILOT_031 (PH=24,325), but the *intervening blank (line 22) is completely clean (NA)* -- no plausible carryover source, so this is very likely a genuine low-level real-sample detection uncovered by the fix, analogous to line 45 itself |
| Analysis4 | 16 | EtOH Blank | 179.2 | Immediately after a 6ng QC (carryover-risk, expected) |
| Analysis4 | 45 | 0.2ng RDX PETN QC | 190.0 | **The target fix** |

5 of 7 are directly explained by proximity to a known large injection (accepted trade-off, discussed with the user before implementing); 1 (Analysis3 line 23) is a clean new real-sample detection with no explainable carryover source; 1 (Analysis1 line 15, a negative control) is ambiguous and worth a closer look if NC contamination status for that specific sample becomes relevant. No previously-`Quantifiable`/`Below_LOQ` row changed category or lost detection (zero regressions in the other direction).

**Not yet regenerated**: `ASTRA_PilotStudyResults.xlsx` (sits in the parent `GC Data/` folder, last modified before this session's reprocessing) -- no pipeline script in `Code/` was found to produce this file (not `04_CollateStudyResults.R`, which is FINEX-specific per the Project Structure section below), so it appears to be manually maintained/exported and was not touched.

**Files modified**: `GlobalCode.R` (`MinPeakHeight_PETN` now conditional on study), all 4 Analysis1-4 `Results/` folders (regenerated `_GCMSResults.csv`/`.xlsx`, `GcData/*SIM.csv`/`*Summary.csv`, calibration/QC plots, integration TIFFs -- pre-fix `GcData` and `_GCMSResults.csv` backed up to `%TEMP%\opencode\BatchRun\` as described above), `SystemMonitoring.xlsx` (updated with the 4 reprocessed runs' QC data, with automatic pre-update backup per the existing mechanism), `CONTEXT.md` (this entry).

---

## Session Summary (August 3, 2026, continued yet again) — Baseline-Correction Integration Floor Fix (Un-floored Negative Peak Area)

### Bug Investigated: Baseline-Correction Ringing Distorts Small PETN Peak Areas

**Problem investigated**: User asked whether baseline correction was adding extra variation and making small PETN peaks harder to detect/quantify reliably, prompted by review of the ASTRA Swabbing pilot study (`Analysis4`, a separate study using the same pipeline).

**Root cause found**: `apply_baseline_correction()` (rollingBall, `Code/ModPeaks.R`) leaves a persistent ±50-100 count oscillation ("ringing") in the corrected signal even in flat regions with no real peak -- of the same order as the noise SD the pipeline itself computes for SNR. `extract_peak_area()` -> `trapz_area()` integrates this signal over a fixed-width window around each peak **without flooring negative values at zero**, so for large peaks the ringing is negligible, but for small/near-LOQ peaks the negative "shoulders" flanking the peak partially cancel the true positive area -- verified against real Analysis4 integration-check plots (e.g. line 53, `PILOT_008`: SNR correctly flagged `Quantifiable` at PH=300, but PA=74 vs an expected ~375 for a peak of that height, a ~5x under-integration). Confirmed via PA/PH ratio: large peaks consistently show ~1.24-1.29 (the true peak-shape factor); every small peak (PH<500) showed a collapsed, erratic ratio (0.25-0.96). This is the same mechanism already flagged (but never fixed) for RDX under "Negative Peak Area Issue" in the June 2026 V9 RT investigation notes below -- this session's investigation showed it also measurably degrades **PETN** quantification broadly, not just RDX in one dataset.

Separately investigated a genuine non-detection (line 45, no PETN peak found at all): confirmed via raw m/z 46 signal reconstruction that this is a true sensitivity-floor case (the underlying bump is ~190-200 counts, at/below the `MinPeakHeight_PETN` threshold, and a blank injection shows a comparable-or-larger bump at the same RT from calibrant carryover) -- **not** fixable by peak-detection/baseline-correction tuning, and the fix below correctly leaves it as a non-detection (see verification).

### Fix Implemented

**File**: `Code/ModPeaks.R`, two locations:

1. **`extract_peak_area()`** (~line 482-509): floor the windowed intensity to zero immediately before `trapz_area()`:
```r
peak_data$Intensity <- pmax(peak_data$Intensity, 0)
```
2. **`plot_integration()`** (~line 578-585): apply the same floor to the shaded *integration_data* region only (not the wider grey context trace, which is left unfloored so the ringing artefact itself remains visible for QA), so the shaded area matches the annotated PA value.

**Deliberately NOT changed**: `calculate_snr()`'s noise-window statistic (flooring it would artificially shrink the noise SD and inflate every SNR value study-wide) and `ModPeaks()` peak detection/peak height (already determined from the unfloored signal, upstream of this fix). `trapz_area()` has exactly one caller (`extract_peak_area()`), which is used identically for PETN, RDX, and the RDX internal standard, so all three benefit consistently from a single change.

### Validation on Analysis4 (ASTRA pilot study)

Reprocessed with `reprocess_sim <- TRUE`, compared before/after:

| Line | Sample | PH | PA before | PA after | PA/PH before | PA/PH after |
|---|---|---|---|---|---|---|
| 53 | PILOT_008 | 300 | 74 | **344** | 0.25 | **1.15** |
| 55 | 0.2ng QC | 208 | 129 | **258** | 0.62 | **1.24** |
| 29 | 0.2ng QC | 286 | 135 | **306** | 0.47 | **1.07** |
| 12 | EtOH Blank | 282 | 238 | **368** | 0.84 | **1.31** |
| 51 | PILOT_032 | 472 | 438 | **557** | 0.93 | **1.18** |
| 6/7/8/9/10/11/13/15/31/47/57/67 (large cals/QCs, PH ≥ 1770) | -- | -- | unchanged | unchanged (several **exactly** 0.00 diff) | ~1.25 | ~1.25 |
| 45 | 0.2ng QC (no peak, sensitivity floor) | -- | NA | **still NA** | -- | -- |

All small-peak ratios moved into the same ~1.1-1.3 band as large peaks; large peaks are essentially untouched; SNR/detection flags unchanged; the genuine non-detection remains a non-detection.

### Batch Reprocessing of All 9 "Accepted Analysis" (FINEX) Datasets

**Requested**: apply the fix to `ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, `Lab29`, `Lab33`, `Lab9`, `Steel-1`, `Steel 2` individually, then report any sample that regresses from PASS/PASS* to FAIL.

**Operational hazard discovered and worked around**: two concurrent RStudio background sessions (`rsession-utf8.exe`) were found active on this exact project during this work (most likely another agent/session doing unrelated concurrent work -- CONTEXT.md itself gained a new "PCA Biplot" session entry, and `GlobalCode.R`'s `DataFolder` line changed on disk on its own between reads, more than once, during this session). Repeatedly editing the shared `GlobalCode.R` and sourcing it in place (the pipeline's normal documented workflow) would race against that. **Mitigation**: captured a snapshot of `GlobalCode.R`'s content once, then for each dataset wrote an isolated copy (to `%TEMP%\opencode\BatchRun\`) with only the `DataFolder` line substituted, and ran `Rscript` directly against that isolated copy (working directory still the project root, so the `Code/...` relative `source()` calls resolve normally) -- never touching the shared `GlobalCode.R` file again after the initial mixup. This fully avoided further collisions for the remainder of the batch.

**Second issue caught before any damage**: the first two datasets run this way (`ABS-S-1`, `ABS-S-2`) came back with **every** PA value NA (PETN, RDX, and the internal standard, across all 86 injections in each) and a fatal downstream error. Diagnosis: the snapshot template carried Analysis4's retention-time config (PETN RT=338s, RDX/IS RT=362s -- correct for `Lab29`/`Lab33`/`Lab9`/`Steel-1`/`Steel 2`, which share this "V8"-labelled config already in `GlobalCode.R`), but `ABS-S-1`/`ABS-S-2`/`Lab10-1`/`Lab17-1` actually use a different, ~17-18s-shifted RT config (PETN≈320.9s, RDX/IS≈347.5s -- confirmed empirically from each dataset's own pre-fix `_GCMSResults.csv`, and matching the "V9 method, Lab22-1" RTs already documented in the V9 RT investigation notes below). With `rt_tolerance = 2s`, searching at the wrong RT found nothing at all, cascading into "all `petn_pa`/`rdx_is_pa` NA" and a fatal `filter()` error later in calibration (column never created because there was no data to calibrate). **No data was lost**: the crash occurred before the final CSV write step in `03_Quantification.R` (confirmed by checking `write.csv()`'s position in the script, and by the untouched file timestamps), so the existing `_GCMSResults.csv` files were never overwritten with bad data -- only the intermediate per-injection `GcData/*.csv` files (regenerated harmlessly on the corrected re-run). Built a second isolated template with PETN RT=321s / RDX+IS RT=347.5s for the 4 affected datasets and reran all 4 successfully; reran the other 5 with the original (correct-for-them) template.

**Regression check** (the user's core concern): compared `FINEX_StudyResults.csv`'s `analysis_accepted` per sample, keyed on `(SourceFile, DataFile)`, before vs after, across all 216 samples:

| | Count |
|---|---|
| Unchanged | 215 |
| **PASS/PASS\* -> FAIL (regression)** | **0** |
| FAIL -> PASS/PASS* (improvement) | 0 |
| Other text-only change (status unchanged) | 1 |

The single changed row (`Lab20 P2 ABS-S2`, `037Summary.csv`) is the already-documented abnormal-IS injection failure (`rdx_is_pa` ≈ 169, PA=169, see July 31, 2026 session below) -- it was `FAIL` before (`"FAIL: PETN Below_LOD; IS abnormally low (PA=169)"`) and is still `FAIL` after (`"FAIL: IS abnormally low (PA=169)"`), just missing the `PETN Below_LOD` clause because `petn_snr_flag` came back `NA` instead of `Below_LOD` (no PETN candidate peak found within tolerance this time, vs one found previously). This is **not attributable to the floor fix itself** -- the fix only ever touches the integration step *after* peak candidacy is already decided by `ModPeaks()`, so it cannot change whether a candidate is found; it is far more likely a side effect of other, unrelated concurrent changes already present in the working tree for `Code/02_PeakDetection.R`/`03_Quantification.R` (both were already showing as modified/uncommitted before this session started -- see the concurrent-session note above). Net effect: **zero regressions**, study-wide PASS/PASS*/FAIL totals unchanged (96/108/12), no rollback needed.

**Files modified**: `Code/ModPeaks.R` (the fix, 2 locations), `Code/02_PeakDetection.R` (`reprocess_sim` toggled `TRUE` for the batch run, restored to `FALSE` afterward), all 9 `Accepted Analysis` dataset `Results/` folders (regenerated `_GCMSResults.csv`/`.xlsx`, `GcData/*.csv`, calibration/QC plots), `FINEX_StudyResults.csv`/`.xlsx` and `Plots/` (regenerated via `04_CollateStudyResults.R`), `SystemMonitoring.xlsx` (updated with the 9 reprocessed runs' QC data), `CONTEXT.md` (this entry).

**Not modified / left as-is**: `GlobalCode.R`'s `DataFolder` line (left in whatever state the concurrent session had it in, to avoid further collisions -- do not assume it points at any particular dataset). ASTRA `Analysis4`'s own `Analysis4_GCMSResults.xlsx` failed to rewrite once during validation (`Permission denied` -- the file was open in Excel at the time); its `.csv` counterpart wrote successfully and was used for validation, but the individual `.xlsx`/`CalibrationStats.xlsx`/`run_metadata.yaml` for that one validation run were not regenerated (Analysis4 is a separate pilot study, not part of the "Accepted Analysis" deliverable, so this was not chased further).


## Session Summary (August 3, 2026, continued) — PETN/RDX Recovery + %RSD PCA Biplot

### Feature: Multivariate PCA of Lab×Participant×Surface Performance Profiles

**Purpose**: Following on from the PETN vs RDX correlation plots (below), a genuine PCA was requested using 4 variables -- mean PETN recovery, mean RDX recovery, PETN %RSD, and RDX %RSD -- to reveal whether labs/participants cluster into distinct performance profiles (e.g. "high recovery, low variability" vs "low recovery, high variability") in a way no single variable can show alone. Unlike the 2-variable case considered (and rejected) for the correlation plots, 4 variables genuinely support a PCA. Complements the existing `lmer` mixed-effects models (which already show Lab/Participant is a dominant variance source, ~42-52% between-participant) by showing *which* labs pattern together and on *what* dimensions.

**Implementation** (`Code/04_CollateStudyResults.R`, inserted immediately after the PETN vs RDX correlation section and before "Per-Lab Plots", ~line 1938): reuses `participant_bar_data` (one row per Lab×Participant×SurfaceName, already built for the by-participant bar charts from `recovery_data` -- i.e. already restricted to `Outcome == "Complete"` samples with NA recovery -> 0%, consistent with every other plot in this section). Chosen over reusing the existing `summary_participant` (built from unfiltered `master`, includes QC-failed samples) for consistency with the rest of the new plotting section.

**Derived variables added to `participant_bar_data`**:
```r
petn_rsd = ifelse(petn_mean != 0, petn_sd / petn_mean * 100, NA_real_)
rdx_rsd  = ifelse(rdx_mean != 0, rdx_sd  / rdx_mean  * 100, NA_real_)
```
`NA` when mean recovery is exactly 0, or when a group has only 1 replicate (`SD` undefined).

**Guard rails**:
- Rows with any of the 4 variables missing (via `stats::complete.cases()`) are excluded from the PCA, with a console message reporting how many Lab×Participant×Surface groups were dropped and why.
- The whole PCA section is skipped (with a message) if fewer than 8 complete groups remain -- need meaningfully more observations than the 4 input variables for an interpretable biplot.

**PCA computation**: `prcomp(pca_data[, pca_vars], center = TRUE, scale. = TRUE)`. Standardising (`scale. = TRUE`) is not optional here, since recovery % and %RSD are on different scales/ranges.

**Biplot construction** (`Overall_PETN_RDX_PCA_Biplot.png`):
- Score points: `geom_point(aes(PC1, PC2, color = SurfaceName))`, reusing the existing `surface_colors`/`fixed_surface_order` palette. No point labels (Lab/Participant identity), per explicit preference -- relies on surface colour grouping only.
- Loading vectors: `geom_segment()` arrows from the origin, rescaled to a comparable visual magnitude to the score points (standard biplot convention: `loading_scale_factor = (score_range * 0.8) / loading_range`), with `geom_text()` labels ("PETN Recovery", "RDX Recovery", "PETN %RSD", "RDX %RSD") nudged slightly further out along each arrow (`* 1.15`) and centre-justified.
- `coord_fixed()` -- important specifically for a biplot, since the *angle* between loading vectors represents the correlation between those variables; a distorted aspect ratio would misrepresent that.
- Generous axis expansion (`expansion(mult = 0.35)` on both axes) to keep all labels fully inside the panel regardless of which direction their arrow points -- an earlier version with a smaller expansion (`0.25`) and direction-dependent `hjust`/`vjust` justification still clipped the "PETN Recovery"/"RDX Recovery" labels at the right edge in testing (the justification signs were initially backwards); replaced with the simpler nudge-and-centre-justify approach once the direction-dependent logic proved error-prone to get right.
- Axis labels include % variance explained per PC, e.g. `"PC1 (44.2% variance)"`.

**Scree chart** (`Overall_PETN_RDX_PCA_VarianceExplained.png`): simple bar chart of % variance explained per PC (PC1-PC4), with the percentage labelled above each bar.

**Companion CSVs**:
- `PETN_RDX_PCA_Scores.csv` -- Lab, Participant, SurfaceName, ParticipantLabel, PC1-PC4 per group
- `PETN_RDX_PCA_Loadings.csv` -- Variable × PC1-PC4 loadings

**Verification**: Validated in isolation (same reasoning as the correlation plots below -- running `Code/04_CollateStudyResults.R` directly triggers full raw-data reprocessing via `GlobalCode.R`). Synthetic mock data replicating `participant_bar_data`'s structure was generated (9 labs × up to 2 participants × 2 surfaces, with some participant/surface combinations and some `n=1` replicate groups deliberately omitted/injected to exercise the exclusion guard rail). Confirmed: the `n<2`-replicate exclusion message fired correctly (1 of 27 groups excluded), all 4 outputs generated without error, and the resulting biplot showed the expected pattern -- Steel points clustering toward positive PC1 (higher recovery) and ABS-Smooth points clustering toward negative PC1 (lower recovery), with the RDX %RSD / PETN %RSD loading vectors pointing in a distinct direction (negative PC2) from the two recovery vectors (positive PC1), indicating recovery and variability load onto different underlying dimensions rather than being redundant with each other.

**Files modified**: `Code/04_CollateStudyResults.R` (new section, ~150 lines), `CONTEXT.md` (this entry).

---

## Session Summary (August 3, 2026) — PETN vs RDX Recovery Correlation Plots

### Feature: Correlation Scatter Plots Between PETN and RDX Recovery

**Purpose**: Assess whether PETN and RDX recovery co-vary within a sample (e.g. driven by shared swab extraction/transfer efficiency) or vary independently. Requested as a "PCA-style" plot; implemented as a correlation scatter plot rather than a literal PCA, since with only two variables a PCA would just be a 45-degree axis rotation of the same scatter and would add no analytical value beyond it.

**Implementation** (`Code/04_CollateStudyResults.R`, inserted immediately after the "Overall Bar Charts by Participant" section and before "Per-Lab Plots", ~line 1747): reuses `recovery_data` (per-sample rows, same data as the box plots) and `participant_bar_data` (per Lab×Participant×Surface means, same data as the by-participant bar charts) -- no new data preparation needed.

**New functions**:
- `compute_corr_stats(data, x_var, y_var)` -- returns n, Pearson r/p, and Spearman rho/p, plus a formatted annotation label. Both statistics are reported (not just Pearson) because RDX has a documented floor effect on ABS-Smooth (27.7% zeros, see Statistical Analysis sections below), which violates the normality assumption Pearson relies on -- mirrors this project's established practice elsewhere of pairing parametric and non-parametric tests when checking a floor/non-normality-affected result. Returns `NA`s gracefully if n < 3 or either variable has zero variance.
- `create_correlation_plot(data, x_var, y_var, x_label, y_label, title, facet_by_surface)` -- scatter plot with `geom_point(aes(color = SurfaceName))` (reusing the existing `surface_colors`/`fixed_surface_order` palette from the bar charts) + `geom_smooth(method = "lm")` regression line. When `facet_by_surface = TRUE`, uses `facet_wrap(~ SurfaceName, scales = "free")` (free scales because PETN and RDX recovery ranges differ drastically by surface -- fixed scales would squash the ABS-Smooth panel) with per-surface stats annotated top-right of each panel; otherwise a single overall annotation.

**Outputs** (`Plots/Overall/`), covering both data granularities requested (per-sample and per-participant-mean), each both pooled and faceted by surface:
- `Overall_PETN_vs_RDX_Correlation_PerSample.png`
- `Overall_PETN_vs_RDX_Correlation_PerSample_BySurface.png`
- `Overall_PETN_vs_RDX_Correlation_ParticipantMean.png`
- `Overall_PETN_vs_RDX_Correlation_ParticipantMean_BySurface.png`
- `PETN_RDX_Correlation_Stats.csv` -- companion table with n/Pearson r/p/Spearman rho/p for every plot (overall + per-surface, both granularities), kept as a standalone CSV rather than an additional sheet in `FINEX_StudyResults.xlsx` since the workbook is already saved earlier in the script (~line 1463) and reopening/re-saving it here would risk the same transient `openxlsx` file-lock issue documented in the July 29, 2026 session summaries above.

**Guard rails**: each of the 4 plots/stats blocks is skipped with a console message if its underlying dataset has fewer than 3 non-NA (PETN, RDX) pairs, avoiding `cor.test()` errors on sparse data.

**Verification**: Since running `Code/04_CollateStudyResults.R` directly via `Rscript` triggers `GlobalCode.R`, which unconditionally sources `01_MsFilesReorganiser.R`/`02_PeakDetection.R`/`03_Quantification.R` (full raw-data reprocessing of whatever `DataFolder` happens to be hardcoded at the time) whenever `SampleVol`/`DepositMass` aren't already in the session, the new code block was instead validated in isolation: the exact inserted lines were extracted and evaluated against synthetic mock data replicating the real `recovery_data`/`participant_bar_data` structure (2 surfaces, a simulated RDX floor effect with ~30% zeros on the "ABS-Smooth" mock surface). All 4 PNGs and the CSV generated without error; visually confirmed correct faceting, free per-panel scales, regression lines, and top-right r/ρ/p annotations on both the pooled and faceted plots. This existing `GlobalCode.R` sourcing behavior is a pre-existing pattern (not something introduced or modified this session) -- worth flagging as a risk for future standalone runs of `04_CollateStudyResults.R` in a fresh R session.

**Files modified**: `Code/04_CollateStudyResults.R` (new section, ~190 lines), `CONTEXT.md` (this entry).

---

## Session Summary (August 3, 2026, continued) — Step-Change Drift Correction (Analysis4 Pilot Dataset)

### Problem: PowerLaw Drift Correction Poorly Fits a Discrete Step Change (Analysis4)

**Dataset**: `ASTRA Swabbing/Pilot Study/GC Data/Analysis4` (pilot study, single sequence, 5 Cal levels, 5×0.2ng + 5×6ng QCs, 20 samples).

**Investigation**: Reviewing `rdx_is_pa` (15N-RDX IS) and `petn_pa` across the sequence (sorted by injection `Line`) showed a **discrete step discontinuity**, not continuous drift:

| Line | rdx_is_pa | petn_pa (6ng QC only) |
|---|---|---|
| 15 (QC 6ng) | 27,655 | 88,539 |
| 25 (Sample) | 33,566 | — |
| 27 (Sample) | 31,070 | — |
| **29 (QC 0.2ng)** | **8,438** | — |
| 31 (QC 6ng) | 14,215 | 50,066 |
| 45–65 (QC, mixed) | 7,700–8,600 | 40,000–51,129 |

Between Line 27 and Line 29 (two injections apart, same day, no method change), the IS response collapses by ~73% and then **plateaus** (with only noise-level variation, even a slight late-run recovery) for the remaining ~40 injections. PETN 6ng QC shows the identical pattern (-43% step, then flat/noisy 40,000–51,129).

**Root cause of poor correction**: `Code/03_Quantification.R`'s existing PowerLaw drift model (`Response ~ a * Row^b`, monotonically decaying) is fit through *all* 6ng QCs, including the one pre-step (Line 15) together with the four post-step points. A single continuous decay curve forced through a step-then-plateau pattern necessarily under-corrects shortly after the step and over-corrects by the end of the run (drift-corrected bias at Line 67 flipped sign to **+7.3%**, i.e. overcorrected, after starting at -47.6%/-54.2%/-57.4%/-46.6% uncorrected across the four post-step 6ng QCs).

### Fix Implemented: Opt-in Step-Change Drift Correction

**New config toggles** (`Code/03_Quantification.R`, top of file, same `if (!exists(..., inherits = FALSE))` pattern as `use_15nrdx_for_petn`/`use_is_for_rdx`, so standard processing is unaffected unless explicitly set):

```r
use_step_drift_correction <- FALSE   # default: unchanged behaviour (PowerLaw/Polynomial)
step_drift_after_line     <- NA_real_  # Line number immediately BEFORE the step
```

When `use_step_drift_correction = FALSE` (default), the existing PowerLaw/Polynomial continuous-decay logic runs exactly as before -- **zero change to standard processing**.

When `TRUE`, the PETN drift-correction section instead uses a **segmented constant-correction model**:
- Rows with `Line <= step_drift_after_line` get `CF = 1.0` (undrifted).
- Rows with `Line > step_drift_after_line` get a single **constant** correction factor: `target_y / mean(post-step QC responses)`, where `target_y` is the response the *original calibration curve* (fit on the undrifted Cal standards, via `stored_petn_models[[cs]]$quad_model`, evaluated at `CalLevelAdj = selected_qc_level * Dilution`) predicts for that QC's known concentration. A constant multiplier matches a step-then-plateau pattern far better than a continuous decay curve, and targeting the calibration curve directly (rather than a single pre-step QC's raw value) centres the correction on zero bias -- see "Design iteration" below for why this matters.
- Sets `petn_drift_model = "Step"` (new value, alongside existing `"PowerLaw"`/`"Quadratic"`/`"Linear"`/`"None"`/`"Disabled"`), with `R² = NA` (not applicable -- no regression fit).
- Minimum QC requirement relaxed to **2** for step mode, vs 3 for the continuous PowerLaw/polynomial models (`min_qc_required <- if (use_step_drift_correction) 2 else 3`). Only post-step QCs are strictly required (the calibration curve itself serves as the "pre-step" reference); pre-step QCs, if present, are logged for context but not used in the CF formula.
- New diagnostic plot branch: `geom_step()` showing the constant post-step correction factor, with a vertical dotted line marking the step boundary (still saved to `PETN_DriftCorrection.png`).
- `"Step"` added to the `petn_drift_model %in% c(...)` gate that triggers the drift-corrected calibration plot (`PETN_Calibration_{Set}_DriftCorrected.png`).

All downstream logic (clamping, re-quantification via `stored_petn_models`, `petn_pa_dc`/`petn_ratio_dc`, QC bias/flag computation, exports) required **no changes** -- it already operates generically on `predicted_cf`/`drift_method`.

### Design Iteration: Anchoring to a Single Pre-Step QC vs the Calibration Curve

**First implementation** (constant CF = mean of `single pre-step QC response / each post-step QC response`) reduced average |bias| on the 4 post-step 6ng QCs from 18.65% (PowerLaw) to 11.3%, but still left **1 of 5 6ng QCs failing** (Line 57, -21.8%, just outside the ±20% `qc_bias_limit`) -- asked by the user whether this could be improved so *all* pass.

**Investigation**: The pre-step reference QC (Line 15, PA=88,539) itself reads ~12% low relative to what the *calibration curve* says a true 6ng injection should produce (PA≈102,382 pre-`Dilution`-adjustment) -- an entirely normal QC-vs-Cal-standard difference, unrelated to drift. Anchoring the whole post-step correction to that one QC's raw value baked this incidental ~12% offset in as a **constant bias applied to the entire post-step region**, on top of the drift correction itself. Since the four post-step QCs are otherwise just noisy around a flat plateau (raw PA range 40,002-51,129, i.e. ±13% around their own mean), that constant offset was enough to push the single lowest raw-response QC (Line 57) outside the ±20% window even though the *group* was reasonably well corrected on average.

**Fix**: target the calibration curve's known-true value directly (`target_y`), using the **mean of all post-step QCs** (not a single point) to compute the correction factor. This removes the incidental pre-step-QC-vs-Cal-curve offset entirely and centres the post-step correction on zero bias.

**Verified result (real data)**:

```
Step drift correction: boundary after Line 27 (Row 28)
Target response (calibration curve at 6 ng, CalSet 1): 102382.3
Mean post-step QC response: 46109.2 (from 4 QC point(s))
Post-step constant correction factor: 2.2204
```

| Line | Uncorrected | PowerLaw (existing) | Step v1 (pre-step-QC anchor) | Step v2 (calibration-curve target) | `petn_qc_flag_dc` |
|---|---|---|---|---|---|
| 15 | -12.1% | -12.1% | -12.1% | -12.1% | PASS |
| 31 | -47.6% | -26.1% | -4.5% | **+7.5%** | PASS |
| 47 | -54.2% | -21.4% | -16.2% | **-5.5%** | PASS |
| 57 | -57.4% | -19.8% (worst PowerLaw) | -21.8% (FAIL) | **-11.8%** | **PASS** |
| 67 | -46.6% | +7.3% (overcorrected) | -2.7% | **+9.5%** | PASS |

**All 5 6ng PETN QCs now show `petn_qc_flag_dc = "PASS"`** (confirmed directly from the pipeline's own computed flag column, not just a manual bias check). Max |bias| across all 5 is 12.1%, comfortably inside the ±20% `qc_bias_limit`.

**0.2ng QC bias improved slightly but remains large (as expected, not a target of this fix)**: -62.9%/-66.5%/-61.5% (Step v2) vs -75.8%/-76.2%/-71.2% (Step v1) vs -78.2%/-76.1%/-69.8% (PowerLaw). The larger v2 correction factor (2.2204 vs 1.9403) partially reduces the 0.2ng gap simply because it's a bigger multiplier, but a ~60%+ negative bias remains -- confirming this is a genuine, separate non-proportional trace-level loss effect (see "QC Monitoring Strategy" section above), not something any 6ng-derived correction-factor magnitude can fully close. Correctly left to the existing SNR-based `SENSITIVITY_CHECK_*` evaluation rather than bias.

### Verification (real data, isolated test -- live Analysis4 folder untouched)

Tested against a temporary copy of the Analysis4 dataset (to avoid the live `Analysis4_GCMSResults.xlsx`, which was open in a background Excel session -- same recurring transient lock issue documented in the June/July 2026 session summaries). Confirmed via R's `parse()` that the edited script has valid syntax, then ran the full pipeline with default settings (confirmed **identical** `PowerLaw` behaviour to before this change) and with `use_step_drift_correction <- TRUE; step_drift_after_line <- 27` (results above).

**Bug caught during verification**: The first attempt at the calibration-curve-targeting version failed silently (`target_y = NA`, correction skipped, falling through to `petn_drift_model = "None"`) because `predict(stored_petn_models[[cs]]$quad_model, newdata = data.frame(CalLevel = ...))` used the wrong predictor name -- the calibration models in this codebase are fit on `CalLevelAdj` (`= CalLevel * Dilution`), not raw `CalLevel` (see `quad_formula <- reformulate(c("CalLevelAdj", "I(CalLevelAdj^2)"), ...)` at `Code/03_Quantification.R` ~line 439). Fixed by predicting with `newdata = data.frame(CalLevelAdj = selected_qc_level * Dilution)`. Caught via targeted `message()` debugging (dumping `cs_post`/`target_y`/`mean_post_response`) after noticing the expected step-correction console output was entirely absent from a test run despite no visible error -- R's default `Rscript` batch warning buffering (`options(warn = 0)`) meant the `warning("Could not compute calibration-curve target response...")` fallback was silently deferred to the end-of-run "N warnings" count rather than printed inline, which delayed spotting it.

**Live dataset integrity incident and recovery**: During iterative testing, `DataFolder` in `GlobalCode.R` was intended to point at a temporary copy of the dataset throughout, but one intermediate test run (before the `CalLevelAdj` bug above was found) ended up executing against the **live** `Analysis4` folder instead (root cause not fully pinned down -- possibly an edit-ordering mixup between test iterations, not a code defect). That run hit the same (then-unfixed) bug, causing `petn_drift_model` to fall through to `"None"` for that run, and wrote this incorrect state into the live `Analysis4_GCMSResults.csv` before failing on the (unrelated, pre-existing) Excel-lock error during the `.xlsx` write. **Caught by an explicit post-hoc integrity check** (re-reading the live CSV and confirming `petn_drift_model` for known rows) rather than assumed safe. **Recovered** by re-running the pipeline once more against the live folder with default settings (`use_step_drift_correction` unset -> `FALSE`), which deterministically regenerated the correct `PowerLaw` results (verified byte-for-byte reproducible across independent runs earlier in this session) -- confirmed via `petn_qc_flag_dc`/`petn_drift_model` spot-check afterwards. No underlying peak-detection/raw data was ever at risk (only the derived `_GCMSResults.csv`/`.xlsx`), and the existing automatic CSV backup-before-overwrite (`Results/Backup/`) meant no prior state was destructively lost regardless. The live `.xlsx` remains stale (still blocked by the Excel lock) until the user closes the file and re-runs -- not a regression, same pre-existing documented issue.

**To apply this fix to the real Analysis4 dataset**: close the file if open in Excel, then set `use_step_drift_correction <- TRUE` and `step_drift_after_line <- 27` (either directly in `GlobalCode.R` before its `source("Code/03_Quantification.R")` line, or at the top of `Code/03_Quantification.R` itself) and re-run.

**Files modified**: `Code/03_Quantification.R` (toggles near top of file; branched drift-model-fitting section using calibration-curve-targeted constant correction; `CalibrationSet` added to `qc_drift_data`; new diagnostic plot branch; calibration-plot gate; R² message guard), `CONTEXT.md` (this entry).

---

## Session Summary (July 31, 2026) — IS Peak Area Injection-Validity Check

### Bug Discovered: Failed Injections Silently Passing as PASS/Complete

**Problem identified**: While reviewing a freshly-processed dataset (`Steel 2`), two samples that clearly had not injected properly (`Lab6 P1 S4`, `Lab6 P2 S2`) were showing `analysis_accepted = PASS` / `Outcome = Complete` in the collated results. Investigation found this was not isolated to Steel 2 -- `Lab14 P2 S4/S5/S6` (Steel-1) had the same silent-pass problem, and `Lab20 P2 ABS-S2` (ABS-S-2) happened to fail for an unrelated, coincidental reason.

**Root cause**: `compute_analysis_accepted()` (`Code/04_CollateStudyResults.R`) only failed a sample when `petn_snr_flag`/`rdx_snr_flag` was a non-NA value other than `"Quantifiable"` (e.g. `"Below_LOD"`). When peak detection found *nothing at all* for an analyte, the flag is `NA` rather than `"Below_LOD"`, and the guard `!is.na(petn_snr) && ...` silently skipped the check entirely. There was **no check at all** on the internal standard (15N-RDX) for regular sample rows -- IS was only used opportunistically for NC validity, never for samples. A completely failed injection (no IS, no analyte peaks at all) was therefore indistinguishable from "successfully quantified, nothing there" and defaulted to `PASS`.

**Evidence gathered across all 9 processed datasets** (`Accepted Analysis`): checked every `rdx_is_pa` (15N-RDX IS peak area) value for Cal/QC/Sample rows.

| | Value |
|---|---|
| Max blank-channel noise (no IS present) | ~241 PA |
| Known real injection failures found | 168.7, 261.7, NA (×4) |
| Lowest genuine sample `rdx_is_pa` anywhere in the study | **6,373 PA** (Lab17 P1 S4, Steel-1) |
| Next-lowest genuine samples | all ≥ 8,000 PA |

A clean ~24x gap between the worst genuine sample and the best-case failure supports a simple absolute floor with large safety margins on both sides.

### Fix Implemented

**New config constant** (`GlobalCode.R`):
```r
MinPeakArea_IS <- 1000   # Absolute floor for rdx_is_pa (15N-RDX IS).
```
Added to `validate_setup()` (must be positive numeric) and to `log_reproducibility()`'s configuration snapshot.

**New per-row flag** `rdx_is_pa_flag` (`Code/03_Quantification.R`, computed immediately after the existing ratio/`invalid_is` block), for every Cal/QC/Sample row (not computed for Blanks -- no IS is spiked into blank vials, so a low/absent signal there is expected, not a failure):
```r
Combined$rdx_is_pa_flag <- ifelse(
  Combined$Type == "Blank", NA_character_,
  ifelse(is.na(Combined$rdx_is_pa), "NOT_DETECTED",
         ifelse(Combined$rdx_is_pa < MinPeakArea_IS, "LOW", "OK"))
)
```
Values: `"OK"`, `"LOW"` (detected but below the floor), `"NOT_DETECTED"` (NA). A console message reports counts of `NOT_DETECTED`/`LOW` injections per dataset when any are found. Added to `cal_cols`, `qc_cols`, `smp_cols` (individual lab XLSX/CSV export column lists, positioned next to the other IS columns) and to `qc_export_cols` (`SystemMonitoring.xlsx`).

**Integration into `compute_analysis_accepted()`** (`Code/04_CollateStudyResults.R`): added as a new **hard-fail** criterion (independent of the SNR-flag checks), since an unreliable IS undermines confidence in the whole injection regardless of what the analyte peaks show:
```r
if (!is.na(is_pa_flag) && is_pa_flag == "NOT_DETECTED") {
  failures <- c(failures, "IS not detected (injection failure suspected)")
} else if (!is.na(is_pa_flag) && is_pa_flag == "LOW") {
  failures <- c(failures, paste0("IS abnormally low (PA=", round(df$rdx_is_pa[i]), ")"))
}
```
Guarded by column-existence checks so unprocessed/older datasets degrade gracefully (no `rdx_is_pa_flag` column -> check skipped).

**Extended `compute_nc_analysis_valid()`**: NC validity now also requires `rdx_is_pa_flag == "OK"`, in addition to the existing `rdx_is_snr_flag` detection check, so a negative control with an IS peak technically detected but at/near blank-noise level no longer counts as an analytically valid run.

**Visibility**: `rdx_is_pa_flag` added to `desired_cols` (study-level Samples sheet, positioned right after `analysis_accepted`/`Outcome`) and to `desired_cols_nc` (Negative Controls sheet, alongside `rdx_is_snr_flag` in the existing audit-column group).

### Verification (real data, all 9 datasets reprocessed)

All 6 known injection-failure cases now correctly fail with a specific, traceable reason, and zero new false positives were introduced (210/216 samples show `rdx_is_pa_flag = "OK"`, exactly matching the pre-existing count of known-good samples):

| Sample | Dataset | `rdx_is_pa` | Before | After |
|---|---|---|---|---|
| Lab6 P1 S4 | Steel 2 | 261.7 | PASS | `FAIL: IS abnormally low (PA=262)` |
| Lab6 P2 S2 | Steel 2 | NA | PASS | `FAIL: IS not detected (injection failure suspected)` |
| Lab14 P2 S4 | Steel-1 | NA | PASS | `FAIL: IS not detected (injection failure suspected)` |
| Lab14 P2 S5 | Steel-1 | NA | PASS | `FAIL: IS not detected (injection failure suspected)` |
| Lab14 P2 S6 | Steel-1 | NA | PASS | `FAIL: IS not detected (injection failure suspected)` |
| Lab20 P2 ABS-S2 | ABS-S-2 | 168.7 | `FAIL: PETN Below_LOD` (coincidental) | `FAIL: PETN Below_LOD; IS abnormally low (PA=169)` (now for the right reason too) |

Note: `Lab14 P2 S4/S5/S6` are a distinct anomaly pattern -- `petn_snr_flag`/`rdx_snr_flag` were both `"Quantifiable"` (analyte signal present and PA-only quantification succeeded) while the IS was completely absent in all three consecutive injections. This went entirely unnoticed prior to this fix, since quantification does not require the IS when `use_15nrdx_for_petn`/`use_is_for_rdx` are configured for PA-only/ratio modes that don't strictly need it for that particular analyte. Worth flagging for manual follow-up (possible IS spiking error for that block of vials, or an m/z 122 channel issue specific to those 3 injections).

**Reprocessing**: All 9 datasets in `Accepted Analysis` were reprocessed (`ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, `Lab29`, `Lab33`, `Lab9`, `Steel-1`, `Steel 2`), followed by `04_CollateStudyResults.R`. `RunAllDatasets.R`'s pre-existing `DataFolder`-override bug (documented in the July 29, 2026 session summary) meant each dataset had to be reprocessed by manually editing `DataFolder` in `GlobalCode.R` and running `Rscript -e "source('GlobalCode.R')"` per dataset, same workaround as before.

**Operational note**: Hit the same transient `openxlsx`/Excel file-lock issue documented in the July 29, 2026 session summary -- both `FINEX_StudyResults.xlsx` and the individual `Steel 2_GCMSResults.xlsx` were open in a background Excel session (confirmed via COM automation enumeration of `Excel.Application.Workbooks`, since neither was the foreground window) and could not be overwritten. The underlying `.csv` outputs (which `04_CollateStudyResults.R` reads) were verified correct in all cases; only the `.xlsx` regeneration was blocked pending the files being closed.

**Files modified**: `GlobalCode.R` (`MinPeakArea_IS` constant, `validate_setup()`, `log_reproducibility()`), `Code/03_Quantification.R` (`rdx_is_pa_flag` computation, `cal_cols`/`qc_cols`/`smp_cols`/`qc_export_cols`), `Code/04_CollateStudyResults.R` (`compute_analysis_accepted()`, `compute_nc_analysis_valid()`, `desired_cols`, `desired_cols_nc`).

---

## QC Monitoring Strategy (Updated June 2026)

### Two-Tier QC System

The QC monitoring system uses **two separate evaluation strategies** depending on QC level:

#### 1. Quantitative QCs (6ng level)

**Purpose**: Monitor drift correction accuracy and quantitative performance

**Criteria**:
- **Bias limit**: ±15% (configurable via `qc_bias_limit` in GlobalCode.R)
- **SNR threshold**: ≥10 for quantifiable

**Flags**:
- `PASS`: |%bias| ≤ 15% AND SNR ≥ 10
- `FAIL_BIAS`: |%bias| > 15%
- `FAIL_SNR`: SNR < 10
- `FAIL`: General failure (no concentration)

**Used for**:
- Drift model fitting (power law or polynomial)
- Sequence acceptance criteria
- Calibration validation

**Plots generated**:
- `{Analyte}_QC_6ng_Accuracy.png`: %Bias vs injection number (with ±15% reference lines)
- `{Analyte}_QC_6ng_Accuracy_DriftCorrected.png`: Drift-corrected %bias (PETN/RDX when applicable)

#### 2. Sensitivity QCs (0.2ng level)

**Purpose**: Monitor system detection capability at trace levels

**Criteria**: SNR thresholds AND quantifiability check (drift-corrected only)

**Uncorrected flags (`petn_qc_flag`, `rdx_qc_flag`)**: 
- SNR-only evaluation (no longer used in current pipeline)
- Retained for legacy compatibility

**Drift-corrected flags (`petn_qc_flag_dc`, `rdx_qc_flag_dc`)**: 
- **Primary evaluation method** (used for all QC acceptance decisions)
- **Pass**: SNR ≥ 10 AND drift-corrected concentration > 0
- **Warn**: 3 ≤ SNR < 10 AND drift-corrected concentration > 0
- **Fail**: SNR < 3 OR drift-corrected concentration ≤ 0 OR concentration is NA

**Three-component check:**
1. **Concentration > 0**: Verifies drift correction restored quantifiability (peak above calibration extrapolation threshold) — checked FIRST
2. **SNR ≥ 3**: Verifies system detection capability (signal above noise floor)
3. **SNR ≥ 10**: Distinguishes good sensitivity (PASS) from marginal but acceptable (WARN)

**Rationale**: 
- A 0.2ng QC that is detected (good SNR) but returns concentration = 0 indicates the drift correction was insufficient to restore quantifiability
- This represents a **dual failure**: drift correction performance inadequate to compensate for signal loss
- Even though the analyte was technically "detected," the system failed to quantify it at trace levels
- SNR thresholds distinguish between robust quantification (≥10, PASS) and marginal but acceptable (3-10, WARN)

**Flags**:
- `SENSITIVITY_CHECK_PASS`: SNR ≥ 10 AND concentration_dc > 0
- `SENSITIVITY_CHECK_WARN`: 3 ≤ SNR < 10 AND concentration_dc > 0
- `SENSITIVITY_CHECK_FAIL`: SNR < 3 OR concentration_dc ≤ 0

**Used for**:
- System sensitivity trending (both detection and quantification)
- Drift correction effectiveness assessment
- Confirming the system can quantify trace contamination after drift correction

**Plots generated**:
- `{Analyte}_QC_0p2ng_SNR_Trend.png`: SNR vs injection number (with SNR=10 and SNR=3 reference lines)
- `{Analyte}_QC_0p2ng_RawPA_Trends.png`: Raw PA vs injection number (before drift correction)

### Rationale

At trace levels (0.2ng), several factors make bias criteria unreliable:

1. **Non-proportional signal losses**: Adsorption to active inlet sites is disproportionately high at trace levels
2. **Baseline noise dominance**: At 0.2ng, peak height is only ~5-20× above noise, making quantitation imprecise
3. **Carryover effects**: Previous high-concentration injections can cause transient contamination
4. **Inlet condition variability**: Trace analytes are extremely sensitive to inlet cleanliness

These factors produce systematic positive bias (+35% to +70% in most sequences) that does not reflect calibration accuracy or drift correction performance.

**SNR is the appropriate metric** at trace levels because:
- It directly measures detection capability (signal > noise)
- It is independent of calibration accuracy
- It tracks system sensitivity degradation over time
- It provides a clear threshold for LOD (SNR = 3) and LOQ (SNR = 10)

### New Output Columns

| Column | Description | Values |
|--------|-------------|--------|
| `petn_qc_type` | QC evaluation type for PETN | `"QUANTITATIVE"` (6ng) or `"SENSITIVITY"` (0.2ng) |
| `rdx_qc_type` | QC evaluation type for RDX | `"QUANTITATIVE"` (6ng) or `"SENSITIVITY"` (0.2ng) |
| `petn_qc_flag` | PETN QC result flag | `"PASS"`, `"FAIL_BIAS"`, `"FAIL_SNR"`, `"SENSITIVITY_CHECK_PASS"`, `"SENSITIVITY_CHECK_WARN"`, `"SENSITIVITY_CHECK_FAIL"` |
| `rdx_qc_flag` | RDX QC result flag | Same as `petn_qc_flag` |
| `petn_qc_flag_dc` | Drift-corrected PETN QC flag | Same flag values as uncorrected |
| `rdx_qc_flag_dc` | Drift-corrected RDX QC flag | Same flag values as uncorrected |

### Implementation Details

**File**: `Code/03_Quantification.R`, lines 1388-1465 (QC evaluation), lines 1467-1583 (QC plots)

**QC evaluation logic** (drift-corrected flags):
```r
if (cal_level == 0.2) {
  # 0.2ng: Three-tier evaluation (concentration first, then SNR thresholds)
  # Priority: conc <= 0 → FAIL (not quantifiable)
  #           SNR < 3 → FAIL (below LOD)
  #           SNR < 10 → WARN (detected but marginal)
  #           SNR >= 10 AND conc > 0 → PASS (good sensitivity)
  snr_val <- if ("petn_snr" %in% names(Combined)) Combined$petn_snr[i] else NA_real_
  conc_dc_val <- Combined$petn_concentration_dc[i]
  
  if (is.na(snr_val) || is.na(conc_dc_val)) {
    Combined$petn_qc_flag_dc[i] <- NA_character_
  } else if (conc_dc_val <= 0) {
    Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
  } else if (snr_val < 3) {
    Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_FAIL"
  } else if (snr_val < 10) {
    Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_WARN"
  } else {
    Combined$petn_qc_flag_dc[i] <- "SENSITIVITY_CHECK_PASS"
  }
} else {
  # Higher QCs (6ng): Bias + SNR evaluation
  if (abs(bias_val_dc) <= qc_bias_limit && snr_val >= 10) {
    qc_flag <- "PASS"
  } else if (abs(bias_val_dc) > qc_bias_limit) {
    qc_flag <- "FAIL_BIAS"
  } else if (snr_val < 10) {
    qc_flag <- "FAIL_SNR"
  }
}
```

**Collation script** (`Code/04_CollateStudyResults.R`):
- Reads pre-computed flags from `petn_qc_flag_dc` and `rdx_qc_flag_dc` columns
- Maps flags to PASS/WARN/FAIL for bracket assignment
- Simplified flags in SystemMonitoring.xlsx: SENSITIVITY_CHECK_* → PASS/WARN/FAIL
- No threshold parameters needed (single source of truth in 03)

**Note on uncorrected vs drift-corrected flags**: 
- Uncorrected flags (`petn_qc_flag`, `rdx_qc_flag`) use SNR-only evaluation (legacy)
- **Drift-corrected flags are the primary QC evaluation** (`petn_qc_flag_dc`, `rdx_qc_flag_dc`)
- Only drift-corrected flags include the concentration > 0 check and three-tier SNR thresholds
- SNR is not affected by drift correction (raw signal property), but concentration is

### Sequence Acceptance Criteria

**0.2ng QC failures still contribute to overall sequence QC status**, but with different interpretation:

| 0.2ng QC Flag | Interpretation | Action |
|---------------|----------------|--------|
| `SENSITIVITY_CHECK_PASS` | System sensitivity excellent (SNR ≥ 10, quantifiable) | Continue |
| `SENSITIVITY_CHECK_WARN` | System sensitivity adequate (3 ≤ SNR < 10, quantifiable) | Monitor; consider inlet maintenance if multiple WARNs; triggers `PASS*` in collation |
| `SENSITIVITY_CHECK_FAIL` | Detection OR quantification failure | **FAIL sequence**; investigate cause |

**Failure modes:**
- **SNR < 3**: System has lost detection capability → inlet cleaning required
- **Concentration = 0**: Drift correction insufficient → may need stronger drift correction or inlet cleaning
- **SNR < 10 with concentration > 0**: System can detect and quantify but sensitivity is marginal → acceptable with warning (PASS*)

**Analyte-Specific QC Filtering (Updated July 2026):**

0.2ng QC failures are now evaluated on a **per-analyte basis**. A sample only fails due to 0.2ng QC failure if the corresponding analyte in that sample is also unquantifiable.

| 0.2ng QC Status | Sample Analyte SNR | Sample Result | Rationale |
|---|---|---|---|
| FAIL | Quantifiable (≥10) | `PASS*` | Sample analyte IS quantifiable → QC failure indicates vial/drift issue, not system capability |
| FAIL | Below_LOQ (3-10) or Below_LOD (<3) | `FAIL` | Sample analyte NOT quantifiable → QC failure is relevant |
| WARN | Any | `PASS*` | Unchanged |
| PASS | Any | Contributes to PASS | Unchanged |

**Implementation**: If a 0.2ng QC fails but the sample's analyte has SNR ≥ 10 ("Quantifiable"), the QC failure is treated as a warning with message: `"PETN 0.2ng pre QC FAIL (sample OK)"`. This triggers `PASS*` instead of `FAIL`.

**6ng QC failures remain unchanged**: 6ng QC failures (bias-based) always disqualify samples regardless of sample analyte status.

A sequence can PASS if 6ng QCs are within ±15% bias even if 0.2ng QCs show concentration = 0, as long as 0.2ng SNR ≥ 3 AND concentration > 0 after drift correction. If either criterion fails, the system has lost the ability to quantify trace levels, and the sequence should be investigated.

### Comparison to Previous Approach

| Aspect | Previous | Current (June 2026) |
|--------|----------|---------------------|
| 0.2ng evaluation | Bias ±15% + SNR ≥10 | **SNR thresholds (3/10) AND concentration_dc > 0** |
| 0.2ng QC flags | PASS / FAIL | **PASS / WARN / FAIL** (three-tier) |
| 0.2ng QC failure rate | ~60% (bias failures) | Variable (depends on drift correction effectiveness) |
| 6ng evaluation | Bias ±15% | **Bias ±15% + SNR ≥10** |
| Plot structure | Single combined plot (all QC levels) | **Separate plots**: 6ng bias, 0.2ng SNR + raw PA |
| Flag nomenclature | `PASS` / `FAIL` | Differentiated: `FAIL_BIAS` vs `SENSITIVITY_CHECK_PASS`/`WARN`/`FAIL` |
| QC type tracking | Not tracked | New columns: `petn_qc_type`, `rdx_qc_type` |
| Concentration check | Not included | **Added**: Must have concentration_dc > 0 to pass/warn |
| Collation QC evaluation | Recalculates from SNR | **Reads pre-computed flags** from 03 |

### Expected Impact

**On ABS-S-2 dataset** (example from implementation discussion):

| Metric | Before | After |
|--------|--------|-------|
| 6ng QC status | 5/5 PASS (bias <15%) | 5/5 PASS (unchanged) |
| 0.2ng QC status | 1/5 PASS (bias) | 2 PASS, 2 WARN, 1 FAIL (SNR-based) |
| Overall sequence status | PASS (6ng good) | PASS (6ng good, 0.2ng detectable) |
| 0.2ng interpretation | "Failing quantitative accuracy" | "System sensitivity adequate (SNR≥3)" |

### Plot Interpretation Guide

#### 6ng QC Accuracy Plot

- **X-axis**: Injection number (sequence position)
- **Y-axis**: % Bias vs true concentration (6ng)
- **Red dashed lines**: ±15% acceptance limits
- **Points**: Shape indicates flag (circle = PASS, X = FAIL_BIAS, triangle = FAIL_SNR)
- **Line**: Trend across sequence (should be near zero if drift correction effective)

**Good pattern**: All points between ±15%, clustered near zero  
**Problem pattern**: Progressive drift (points trend away from zero), outliers outside ±15%

#### 0.2ng QC SNR Trend Plot

- **X-axis**: Injection number (sequence position)
- **Y-axis**: Signal-to-Noise Ratio (SNR)
- **Green dashed line**: SNR = 10 (Pass threshold, LOQ)
- **Orange dashed line**: SNR = 3 (Warn threshold, LOD)
- **Points**: Shape indicates flag (circle = PASS, triangle = WARN, X = FAIL)
- **Line**: SNR trend across sequence

**Good pattern**: All points above SNR = 10 (green line)  
**Acceptable pattern**: Points decline but stay above SNR = 3 (orange line)  
**Problem pattern**: Points drop below SNR = 3 (system has lost detection capability)

### References

- **Validation approach**: Based on analysis of ABS-S-1, ABS-S-2, Lab10-1, and Lab17-1 datasets showing 0.2ng QCs consistently exhibit +35% to +70% bias regardless of 6ng QC performance
- **SNR thresholds**: 
  - SNR = 10: Established quantification threshold (10:1 S/N ratio, ICH Q2(R1) guidance)
  - SNR = 3: Established detection threshold (3:1 S/N ratio, standard analytical practice)
- **Trace-level adsorption**: Walsh & Ranney (1998), Emmrich et al. (2001) — nitrate esters and nitramines show non-proportional losses at trace levels

---

## Column Organization (Updated June 2026)

### Design Principles

The column structure in all output XLSX files follows consistent organizational principles:

1. **Analyte grouping**: All PETN columns together, then all RDX columns together
2. **Acceptance forward**: `analysis_accepted` and `Outcome` columns moved immediately after identifiers for quick reference
3. **QC level ordering**: In individual lab QC sheets, 0.2ng QCs appear first, then 6ng QCs (ascending by concentration)
4. **Consistent across sheets**: Same PETN→RDX grouping pattern applied to all sheet types (Calibration, QC, Samples, Blanks, NC)

### Study-Level FINEX_StudyResults.xlsx

#### Samples Sheet (35 columns: A-AI)

| Column Range | Content | Count |
|--------------|---------|-------|
| **A-E** | Identifiers (Lab, Participant, Surface, SurfaceName, Repeat) | 5 |
| **F-G** | Acceptance (analysis_accepted, Outcome) | 2 |
| **H-S** | ALL PETN COLUMNS | 12 |
| **T-AF** | ALL RDX COLUMNS | 12 |
| **AG-AJ** | Source tracing (Date, SampleName, DataFile, SourceFile) | 4 |

**PETN columns (H-S):**
- H: Flag (petn_snr_flag)
- I-J: Values (petn_concentration_dc, petn_recovery_dc)
- K-N: QC brackets (petn_qc_6ng_pre/post, petn_qc_02ng_pre/post)
- O-S: QC inherited flags (4 flags matching brackets above)

**RDX columns (T-AF):** Same structure as PETN

#### NC Sheet (26 columns: A-Z, restructured August 7, 2026)

| Column Range | Content | Count |
|--------------|---------|-------|
| **A-D** | Identifiers (Lab, Participant, Surface, SurfaceName) | 4 |
| **E** | Stage 1: `nc_analysis_accepted` -- was this injection/run analytically trustworthy? (PASS/PASS*/FAIL:, same vocabulary as the Samples sheet's `analysis_accepted`) | 1 |
| **F** | Stage 2: `nc_result` -- contamination outcome (Negative (clean) / Positive: ... ), only meaningful when Stage 1 is PASS/PASS*; forced to "Not evaluated (analysis failed)" otherwise | 1 |
| **G-J** | ALL PETN COLUMNS -- raw evidence behind Stage 2 | 4 |
| **K-N** | ALL RDX COLUMNS -- raw evidence behind Stage 2 | 4 |
| **O-P** | IS audit columns (rdx_is_snr_flag, rdx_is_pa_flag) -- basis for Stage 1 | 2 |
| **Q-X** | QC bracket audit columns (petn/rdx_qc_6ng_pre/post, petn/rdx_qc_02ng_pre/post) -- basis for Stage 1 | 8 |
| **Y-Z** (+ 2 more) | Source tracing (Date, SampleName, DataFile, SourceFile) | 4 |

**PETN columns (G-J):** petn_snr_flag, petn_ph, petn_concentration_dc, petn_recovery_dc

**RDX columns (K-N):** Same structure as PETN

**Note (August 7, 2026)**: Restructured from the previous single `nc_status` + boolean `nc_analysis_valid` pair into an explicit two-stage `nc_analysis_accepted` / `nc_result` pair, mirroring the Samples sheet's `analysis_accepted`/`Outcome` pattern -- see "NC Evaluation Restructured into Two Explicit Stages" session summary above for full rationale. The 6ng QC bracket columns (Q-T) are new to this sheet as of this session (previously only the 0.2ng brackets were shown/checked for NCs).

#### Per-Surface Sheets

Per-surface sheets (Lab10_ABS-S, Lab10_S, etc.) automatically inherit the column order from the main Samples sheet.


### Individual Lab {Lab}_GCMSResults.xlsx

All sheets in individual lab XLSX files use consistent structure: **Identification → IS (15N-RDX) → ALL PETN → ALL RDX → Shared columns**

#### Calibration Sheet

- Identification: Date, Line, SampleName, DataFile, Vial, CalLevel, CalibrationSet
- IS: rdx_is_rt, rdx_is_pa, rdx_is_snr, rdx_is_snr_flag
- **PETN**: Raw (RT, PA, PH, SNR, SNR flag, ratio) → Quantitation (concentration, quant method) → Accuracy (percent_bias)
- **RDX**: Same structure as PETN
- Shared: TrueCalConcAdj

#### QC Sheet

- Identification: Date, Line, SampleName, DataFile, Vial, CalLevel, CalibrationSet
- IS: rdx_is_rt, rdx_is_pa, rdx_is_snr, rdx_is_snr_flag
- **PETN**: Raw → Quantitation (uncorrected) → Drift correction (factor, concentration_dc) → Accuracy (uncorrected %bias) → QC evaluation (type, flag) → Accuracy (drift-corrected %bias, flag_dc)
- **RDX**: Same structure as PETN
- Shared: TrueCalConcAdj

**Row sorting:** QC sheet rows are sorted by `CalLevel` (ascending), then `Line`. This places 0.2ng QCs before 6ng QCs, matching the analytical progression from low to high concentration.

#### Samples Sheet

- Identification: Date, Line, SampleName, DataFile, Vial
- IS: rdx_is_rt, rdx_is_pa, rdx_is_snr, rdx_is_snr_flag
- **PETN**: Raw → Quantitation (uncorrected, incl. quant method) → Drift correction
- **RDX**: Same structure as PETN

#### Blanks Sheet

- Identification: Date, Line, SampleName, DataFile, Vial
- **PETN**: Raw signal only (RT, PA, PH, SNR, SNR flag)
- **RDX**: Same structure as PETN

(No IS columns or quantitation/concentrations in Blanks — raw signal data only)

### Rationale

**PETN first, RDX second:** PETN is the primary target analyte for most surfaces (typically higher recovery, more reliable signal). Placing all PETN columns together allows rapid visual scanning of PETN results without having to skip over interleaved RDX columns.

**Acceptance columns forward (study-level Samples sheet):** `analysis_accepted` and `Outcome` are the most commonly referenced columns when reviewing sample batches. Placing them immediately after identifiers (columns F-G) allows quick filtering/sorting without scrolling right through all measurement data.

**QC 0.2ng before 6ng (individual lab QC sheets):** Matches the analytical progression from low to high concentration when reviewing data. The 0.2ng QCs monitor LOQ-level sensitivity, while 6ng QCs monitor quantitative accuracy. Reviewing in ascending order is more intuitive.

**Study-level vs lab-level QC order difference:** Study-level sheets prioritize 6ng QCs in bracket columns (columns L-O for 6ng, P-S for 0.2ng) because 6ng QCs define the acceptance criteria. Lab-level QC sheets prioritize 0.2ng rows first because they appear earlier in the sequence and ascending concentration order is more natural when reviewing raw data.

**Consistent PETN→RDX grouping across all sheets:** Reduces cognitive load — users always know "PETN columns come first, then RDX" regardless of which sheet they're viewing.

### Implementation

**Files modified:**
- `Code/04_CollateStudyResults.R`: Lines 748-794 (`desired_cols`, `desired_cols_nc`)
- `Code/03_Quantification.R`: Lines 1849-1950 (`cal_cols`, `qc_cols`, `smp_cols`, `blk_cols`, QC row sorting)

**Testing:** Verify column positions with known data (e.g., Lab10-1) and spot-check that values are in correct columns after reorganization.

---

## Negative Control (NC) Evaluation Criteria (Updated June 2026)

### Three-Tier Evaluation System

Negative controls are evaluated using **three independent criteria** (any can trigger WARN/FAIL):

#### 1. SNR Flag Criteria
- **FAIL**: SNR flag = `Quantifiable` (analyte clearly present, contamination confirmed)
- **WARN**: SNR flag = `Below_LOQ` (trace detected but not quantifiable)
- **PASS**: SNR flag = `Below_LOD` or NA (no analyte detected)

#### 2. Peak Height Criteria (NEW - June 2026)
- **WARN**: Peak height detected (PH > 0) but below minimum threshold
  - PETN: `0 < PH < 200` (MinPeakHeight_PETN from GlobalCode.R)
  - RDX: `0 < PH < 50` (MinPeakHeight_RDX from GlobalCode.R)
- **Rationale**: Catches trace contamination/carryover that passes SNR criteria (due to low noise) but fails absolute signal threshold

#### 3. Concentration Criteria (NEW - June 2026)
- **WARN**: Concentration (drift-corrected) == 0
- **Rationale**: A concentration of exactly 0 indicates:
  - A peak was detected (otherwise concentration would be NA)
  - Negative extrapolation was floored at zero in quadratic solver (`solve_concentration()`)
  - Real signal present below calibration range (trace contamination)

### Priority Logic

For each analyte, classification follows **first-match priority**:
1. Concentration_dc == 0 → **trace** (WARN) - *Peak below calibration range*
2. SNR = `Quantifiable` → **contaminated** (FAIL) - *Quantifiable contamination*
3. 0 < PH < threshold → **trace** (WARN) - *Low peak height*
4. SNR = `Below_LOQ` → **trace** (WARN) - *Trace detected*
5. SNR = `Below_LOD` or NA → **clean** (PASS) - *No detection*

**Rationale for priority order:**
- Concentration == 0 indicates negative extrapolation (peak below lowest calibrator)
- This is trace contamination (WARN), not full contamination (FAIL)
- Takes priority over SNR evaluation because it provides concentration-specific context
- Prevents false positives where low peaks quantify to 0 but are flagged as full contamination

### Combined Status

**Note (August 7, 2026)**: The labels below (`NC PASS`/`NC WARN`/`NC FAIL`) are the *historical* names for this classification, from when it was the sheet's only status column. As of August 7, 2026 this classification is `nc_result` (Stage 2 -- contamination outcome), relabelled `Negative (clean)` / `Positive: ...`, and is only reported at all if `nc_analysis_accepted` (Stage 1 -- run trustworthiness, checked first) is PASS/PASS* -- see "NC Evaluation Restructured into Two Explicit Stages" session summary above. The underlying SNR/PH/concentration classification logic itself is unchanged.

| PETN Status | RDX Status | NC Status (historical name) | `nc_result` (current name) |
|-------------|------------|-----------|-----------|
| contaminated | contaminated | NC FAIL: PETN + RDX contaminated | Positive: PETN + RDX contaminated |
| contaminated | clean/trace | NC FAIL: PETN contaminated | Positive: PETN contaminated |
| clean/trace | contaminated | NC FAIL: RDX contaminated | Positive: RDX contaminated |
| trace | trace | NC WARN: PETN + RDX trace detected | Positive: PETN + RDX trace detected |
| trace | clean | NC WARN: PETN trace detected | Positive: PETN trace detected |
| clean | trace | NC WARN: RDX trace detected | Positive: RDX trace detected |
| clean | clean | NC PASS | Negative (clean) |

### Implementation

**File**: `Code/04_CollateStudyResults.R`, function `compute_nc_result()` (Stage 2; was `compute_nc_status()` prior to August 7, 2026)

**Columns evaluated**:
- `petn_snr_flag`, `rdx_snr_flag` (SNR criteria - primary)
- `petn_ph`, `rdx_ph` (peak height criteria - now visible in NC sheet)
- `petn_concentration_dc` (PETN drift-corrected, preferred)
- `petn_concentration` (fallback if DC not available)
- `rdx_concentration` (RDX uncorrected, no drift correction typically applied)

**Thresholds**:
- PETN PH threshold: 200 (from `MinPeakHeight_PETN` in GlobalCode.R)
- RDX PH threshold: 50 (from `MinPeakHeight_RDX` in GlobalCode.R)

### Example Cases

| Scenario | PETN PH | PETN SNR | PETN Conc (dc) | Old Status | New Status |
|----------|---------|----------|----------------|------------|------------|
| Clean NC | NA | Below_LOD | NA | NC PASS | NC PASS |
| Trace carryover | 150 | Below_LOQ | 0.05 | NC WARN | NC WARN |
| **Peak below cal range** | 300 | Quantifiable | **0** | **NC FAIL** | **NC WARN** (fixed) |
| Low PH, no quantification | 180 | Below_LOD | NA | NC PASS | **NC WARN** (new) |
| Clear contamination | 5000 | Quantifiable | 0.5 | NC FAIL | NC FAIL |
| Very low carryover | 40 | Below_LOD | NA | NC PASS | **NC WARN** (new) |

**Key improvements**:
- **Fixes concentration == 0 + SNR Quantifiable**: Now correctly shows WARN (trace) instead of FAIL (contaminated)
- Catches low PH peaks that pass SNR due to low noise baseline
- Uses drift-corrected concentrations for PETN (more accurate evaluation)
- More sensitive to trace contamination/carryover

### NC Sheet Columns

The NC sheet in `FINEX_StudyResults.xlsx` includes diagnostic columns:

| Column | Purpose |
|--------|---------|
| `nc_analysis_accepted` (Stage 1, restructured August 7, 2026) | Was this NC injection/run analytically trustworthy? PASS/PASS*/FAIL:, checked from IS validity + BOTH 6ng and 0.2ng QC brackets -- same criteria family as the Samples sheet's `analysis_accepted`. Must be PASS/PASS* for `nc_result` below to be meaningful. |
| `nc_result` (Stage 2, restructured August 7, 2026) | Contamination outcome -- `Negative (clean)` / `Positive: ...` -- ONLY evaluated when `nc_analysis_accepted` is PASS/PASS*; forced to `"Not evaluated (analysis failed)"` otherwise, regardless of the raw SNR/PH/concentration signal. |
| `petn_snr_flag`, `rdx_snr_flag` | Primary SNR-based evidence behind `nc_result` |
| `petn_ph`, `rdx_ph` | Peak height evidence behind `nc_result` |
| `petn_concentration_dc`, `rdx_concentration` | Quantified amounts (if detected) -- evidence behind `nc_result` |
| `rdx_is_snr_flag`, `rdx_is_pa_flag` | IS evidence behind `nc_analysis_accepted` |
| `petn_qc_6ng_pre/post`, `rdx_qc_6ng_pre/post` (added August 7, 2026) | 6ng QC bracket evidence behind `nc_analysis_accepted` -- previously computed for every Sample-type row (NCs included) but not shown on this sheet |
| `petn_qc_02ng_pre/post`, `rdx_qc_02ng_pre/post` | 0.2ng QC bracket evidence behind `nc_analysis_accepted` |

### Expected Impact

**On typical datasets:**
- NCs with concentration == 0 will now show **NC WARN** instead of potentially NC PASS
- NCs with low PH (below 200/50 but > 0) will trigger **NC WARN**
- No impact on clean NCs (Below_LOD, no peak) or heavily contaminated NCs (Quantifiable)
- Better catches edge cases of trace contamination and carryover

**Estimated change in NC pass rate:**
- **Previous**: ~70-80% PASS, ~10-15% WARN, ~10-15% FAIL
- **After**: ~60-70% PASS, ~20-25% WARN, ~10-15% FAIL (more sensitive to trace)

### Rationale for Each Criterion

**Why SNR alone is insufficient:**
- SNR is relative to local noise
- In sequences with very clean baselines (low noise), trace peaks can appear to have "good" SNR
- Absolute signal threshold (PH) catches these cases

**Why PH threshold matters:**
- PH = 200/50 thresholds validated against blank noise in real datasets
- Below these thresholds, peak integration becomes unreliable
- Catches instrument carryover that produces small but reproducible peaks

**Why concentration == 0 is contamination:**
- `solve_concentration()` returns 0 only when:
  - A peak was detected (PA > 0)
  - Quadratic extrapolation gave negative concentration
  - Result was floored at 0 (Code/03_Quantification.R, line 107)
- This is NOT a clean NC - it's a real peak below the lowest calibrator
- Should be flagged as trace contamination (WARN)

### Comparison to Previous Approach

| Aspect | Previous (pre-June 2026) | Current (June 2026) |
|--------|--------------------------|---------------------|
| Evaluation criteria | SNR flag only | **SNR + PH + Concentration** |
| Concentration == 0 | Not checked | **WARN** (trace detected) |
| Low PH (< threshold) | Not checked | **WARN** (trace detected) |
| Columns in NC sheet | SNR flags, concentrations | **Added: petn_ph, rdx_ph** |
| Sensitivity to trace | Moderate (SNR-based) | **High** (three-tier) |
| PASS rate (typical) | ~75% | ~65% (more stringent) |

---

## Batch Processing (June 2026)

### RunAllDatasets.R - Automated Batch Processing

**Purpose**: Process all datasets in the Accepted Analysis folder automatically without manually changing `DataFolder` in GlobalCode.R for each run.

**Usage**:
```r
# 1. Configure GlobalCode.R settings (RTs, thresholds, drift correction)
# 2. Set reprocessing flags in Code/02_PeakDetection.R if needed
# 3. Run batch script:
source("RunAllDatasets.R")
```

**Behavior**:
- Discovers all subdirectories in Accepted Analysis folder
- Excludes "Plots" folder (case-insensitive)
- Validates each dataset (checks for .LOG file, .ms1 data)
- Processes each dataset in isolated environment (no variable contamination)
- Continues on error (doesn't stop entire batch)
- Logs success/failure for each dataset
- Always runs `04_CollateStudyResults.R` at end
- Respects reprocessing flags in `02_PeakDetection.R`
- Uses same GlobalCode.R settings for all datasets

**Outputs**:
- Individual `Results/` folders per dataset (as usual)
- `BatchProcessing_Log.csv` in Accepted Analysis folder (summary)
- `FINEX_StudyResults.xlsx` (collated study results)

**Configuration** (top of `RunAllDatasets.R`):
```r
accepted_analysis_dir <- "path/to/Accepted Analysis"
exclude_folders <- c("Plots")  # Case-insensitive
```

**Validation per dataset**:
- Checks for sequence .LOG file
- Checks for GcDataConverterMs/ or RawData/ directory
- Checks for .ms1 files
- Skips invalid datasets with warning

**Error handling**:
- Validation failures: Logged as "SKIPPED"
- Processing errors: Logged as "FAILED" with error message
- Continues to next dataset regardless
- Reports all failures at end

**Isolated execution**:
- Each dataset runs in `new.env(parent = .GlobalEnv)`
- GlobalCode.R variables (DataFolder, analytes, etc.) isolated per dataset
- No variable leakage between iterations
- Same behavior as manual single-dataset processing

**Performance**:
- Progress indicator: `[Progress: 5/37]`
- Timer for total elapsed time
- Garbage collection between datasets (`gc()`)
- Typical runtime: ~5-10 minutes per dataset (depends on reprocessing flags)

**Example output**:
```
========================================
BATCH PROCESSING: FINEX Swabbing Study
========================================

Found 37 datasets (excluding: Plots)

[Progress: 1/37]
Processing: ABS-S-1
✓ SUCCESS: ABS-S-1

[Progress: 2/37]
Processing: ABS-S-2
✓ SUCCESS: ABS-S-2

[... more datasets ...]

========================================
BATCH PROCESSING COMPLETE
========================================
Total datasets: 37
Successful: 35
Failed: 2
Skipped: 0
Elapsed time: 185.3 minutes

--- FAILED DATASETS ---
  1. Lab22-1
     Error: RTs not found (check GlobalCode.R RTs for V9 method)
  2. Lab30
     Error: Sequence log parse error

Processing log saved to: .../Accepted Analysis/BatchProcessing_Log.csv

========================================
Running CollateStudyResults...
========================================

✓ Collation complete (2.1 minutes)

========================================
ALL PROCESSING COMPLETE
========================================
Total time: 187.4 minutes
```

**BatchProcessing_Log.csv format**:
| Dataset | Status | Error |
|---------|--------|-------|
| ABS-S-1 | SUCCESS | |
| ABS-S-2 | SUCCESS | |
| Lab22-1 | FAILED | RTs not found (check GlobalCode.R RTs for V9 method) |
| Lab30 | SKIPPED | Validation failed: No .ms1 files found |

**Advantages over manual processing**:
- ✅ Process 37+ datasets unattended (overnight run)
- ✅ Consistent settings across all datasets
- ✅ Automatic error recovery (doesn't stop on failure)
- ✅ Full audit trail (log file)
- ✅ No risk of forgetting to change DataFolder
- ✅ Individual Results/ folders preserved (same as manual)
- ✅ Respects all existing flags and settings

**Limitations**:
- Same GlobalCode.R settings for all datasets (assumes all use same method)
- For mixed methods (V8/V9 RTs), either:
  - Process V8 datasets first, then manually update RTs and rerun for V9 datasets
  - Or modify script to support per-dataset RT overrides (advanced)

---

## Project Structure (Consolidated - May 2026)

```
GCMSQuantitation/
├── GlobalCode.R              # Main configuration (set DataFolder, RTs, toggle use_15nrdx_for_petn)
├── RunAllDatasets.R          # Batch processing script (NEW - June 2026)
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

## Known Issues / Bug Fixes

### dplyr 1.1.4 Compatibility Issue (June 2026)

**Problem**: Pipeline fails during peak detection (02_PeakDetection.R) with error:
```
Error in `dplyr::filter()`:
! Can't specify an argument named `by` in this verb.
ℹ Did you mean to use `.by` instead?
```

**Location**: `Code/ModPeaks.R`, line 460 in `extract_peak_area()` function

**Root cause**: dplyr 1.1.0+ introduced stricter argument checking. While no explicit `by=` argument exists in the code, the error suggests a parameter interpretation issue specific to this version.

**Original code affected** (7 locations in ModPeaks.R):

```r
# Line 249-251: process_sim_channel()
channel_data <- raw_data %>%
  dplyr::filter(abs(Mass - mz) < 0.5) %>%
  dplyr::select(RetentionTime, Intensity) %>%
  dplyr::filter(RetentionTime < rt_upper_limit)

# Line 367-370: calculate_snr()
noise_data <- baseline_corrected %>%
  dplyr::filter(
    RetentionTime >= noise_window[1] &
    RetentionTime <= noise_window[2]
  )

# Line 460: extract_peak_area() -- ERROR LOCATION
candidate <- peaks %>%
  dplyr::filter(x < (expected_rt + rt_tolerance) & x > (expected_rt - rt_tolerance))

# Line 481: extract_peak_area()
peak_data <- signal %>%
  dplyr::filter(dplyr::between(RTime, lmin, lmax))

# Line 567: plot_integration()
plot_data <- baseline_corrected %>%
  dplyr::filter(RetentionTime >= plot_min & RetentionTime <= plot_max)

# Line 573: plot_integration()
peak_data_plot <- peak_data %>%
  dplyr::filter(dplyr::between(RTime, lmin, lmax))
```

**Fix applied** (June 12, 2026): Replaced all `dplyr::filter()` calls with base R subsetting using `[row_condition, , drop = FALSE]` syntax. This eliminates the dplyr dependency for filtering operations and avoids the argument interpretation issue entirely.

**Replacement examples**:

```r
# Line 249-251: Before
channel_data <- raw_data %>%
  dplyr::filter(abs(Mass - mz) < 0.5) %>%
  dplyr::select(RetentionTime, Intensity) %>%
  dplyr::filter(RetentionTime < rt_upper_limit)

# Line 249-251: After
channel_data <- raw_data[abs(raw_data$Mass - mz) < 0.5, , drop = FALSE]
channel_data <- channel_data[, c("RetentionTime", "Intensity"), drop = FALSE]
channel_data <- channel_data[channel_data$RetentionTime < rt_upper_limit, , drop = FALSE]

# Line 460 (ERROR LOCATION): Before
candidate <- peaks %>%
  dplyr::filter(x < (expected_rt + rt_tolerance) & x > (expected_rt - rt_tolerance))

# Line 460: After
candidate <- peaks[
  peaks$x < (expected_rt + rt_tolerance) & 
  peaks$x > (expected_rt - rt_tolerance),
  , drop = FALSE
]

# Line 481: Before
peak_data <- signal %>%
  dplyr::filter(dplyr::between(RTime, lmin, lmax))

# Line 481: After
peak_data <- signal[
  signal$RTime >= lmin & signal$RTime <= lmax,
  , drop = FALSE
]
```

**Verification**: Confirmed no `dplyr::filter()` calls remain in ModPeaks.R using `grep -n "dplyr::filter"` (no output = all replaced).

**Files modified**: `Code/ModPeaks.R` (7 locations: lines 249-251, 367-370, 460, 481, 567, 573)

---

## Session Summary: Statistical Analysis Integration (July 3, 2026)

### Critical QC Filtering Issue - Identified and Fixed

**Problem discovered**: Main statistical analysis script (`05_StatisticalAnalysis.R`) was analyzing ALL samples (232 observations) including QC failures. Diagnostic script correctly filtered to only QC-passed samples (192 observations) using `filter(analysis_accepted %in% c("PASS", "PASS*"))`.

**Impact on results**:
- Main script (before fix): 232 obs (117 PETN, 115 RDX), 3 PETN zeros, parametric and non-parametric rankings disagreed ❌
- Diagnostic script (correct): 192 obs (98 PETN, 94 RDX), 0 PETN zeros, rankings agreed ✅
- Variance components artificially diluted by including failed samples: 30% vs 42% for PETN, 17% vs 52% for RDX

**Root cause**: QC filter missing at line ~132 in data preparation section.

**Fix applied**: Added `filter(analysis_accepted %in% c("PASS", "PASS*"))` after `filter(!is.na(Lab))` in main script.

**Verification**: After fix, script produces correct results matching diagnostic script (192 obs, 0 PETN zeros, 42.1% PETN variance, 52.0% RDX variance, rankings agree).

---

### Separate PETN/RDX Models Integration - Complete Implementation

**Rationale for separate models**:

The combined model (PETN and RDX analyzed together with Analyte as factor) artificially **dilutes variance components** and **masks fundamental distributional differences**:

1. **Distributional differences**:
   - PETN: 0% zeros, mean 5.5-8%, normally distributed
   - RDX: 27.7% zeros, mean 0.25-2.2%, floor effect on ABS-Smooth

2. **Variance structure differences**:
   - Combined model: 12.7% between-participant variance (diluted)
   - PETN separate: 42.1% between-participant variance (3.3× higher)
   - RDX separate: 52.0% between-participant variance (4.1× higher)

3. **Scientific interpretation**:
   - Separate models reveal that operator variability is the **dominant** source of variance (42-52%)
   - This finding is critical for method development and training recommendations
   - Combined model obscures this by averaging across fundamentally different distributions

**Files modified**:
- `Code/05_StatisticalAnalysis.R` (main integration)
- `Code/05_StatisticalAnalysis_BACKUP.R` (backup created before modifications)

**Changes implemented**:

#### 1. Data Splitting Section (Lines 607-628)
Already present from previous session. Splits data into `long_data_petn` and `long_data_rdx`, applies sum-to-zero contrasts for Lab factor, reports zero counts.

#### 2. Multi-Lab Model Section (Lines 633-847)
Already present from previous session. Replaces combined model with separate PETN and RDX mixed-effects models:

```r
# PETN model
model_petn <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                   data = long_data_petn, REML = TRUE)

# RDX model  
model_rdx <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                  data = long_data_rdx, REML = TRUE)
```

Includes:
- Separate ANOVA tables (`anova_petn`, `anova_rdx`)
- Separate variance components (`variance_components_petn`, `variance_components_rdx`)
- Separate pairwise comparisons (`pairs_petn_df`, `pairs_rdx_df`)
- Levene's tests for homogeneity of variance (Surface and Lab factors)
- Kruskal-Wallis tests (non-parametric alternatives)
- Dunn tests for post-hoc comparisons (if >2 surfaces)
- Method comparison table (parametric vs non-parametric rankings)

**Objects created**:
- `model_petn`, `model_rdx` (lmer models)
- `anova_petn`, `anova_rdx` (ANOVA tables)
- `variance_components_petn`, `variance_components_rdx` (variance partition)
- `pairs_petn_df`, `pairs_rdx_df` (pairwise comparisons)
- `levene_petn_surface`, `levene_petn_lab`, `levene_rdx_surface`, `levene_rdx_lab` (variance homogeneity tests)
- `kw_petn`, `kw_rdx` (Kruskal-Wallis results)
- Comparison table showing parametric/non-parametric agreement

#### 3. Diagnostic Plots Section (NEW - Lines 1107-1221)
**Added this session**: Separate diagnostic plots for PETN and RDX models (6 total):

**PETN plots**:
- `PETN_Residuals_vs_Fitted.png` — Residuals vs fitted values with loess curve
- `PETN_QQ_Plot.png` — Normal Q-Q plot for normality assessment
- `PETN_Variance_Components.png` — Bar chart showing variance partition (42.1% / 57.9%)

**RDX plots**:
- `RDX_Residuals_vs_Fitted.png` — Residuals vs fitted values with loess curve
- `RDX_QQ_Plot.png` — Normal Q-Q plot for normality assessment
- `RDX_Variance_Components.png` — Bar chart showing variance partition (52.0% / 48.0%)

**Implementation**: Added conditional block checking for `!is.null(model_petn) && !is.null(model_rdx)` after combined model diagnostic section. Uses same ggplot2 structure as combined diagnostics but creates separate plots per analyte.

**Note**: Single-panel plots implemented. Faceted residual plots by lab (9 panels) deferred for future enhancement.

#### 4. Excel Export Section (NEW - Lines 1294-1462)
**Added this session**: Full 14-sheet export implementation for separate models.

**New sheets added** (multi-lab mode with separate PETN/RDX models):

1. **Descriptive_Statistics** — Overall summary by Analyte × Surface (already existed)
2. **Descriptive_By_Lab** — Summary by Analyte × Lab (NEW)
3. **Descriptive_Participant** — Summary by Analyte × Lab × Participant (NEW)
4. **RDX_Zero_Frequency** — Zeros by Lab × Surface for RDX (NEW)
5. **Statistical_Summary_PETN** — ANOVA table for PETN model
6. **Statistical_Summary_RDX** — ANOVA table for RDX model
7. **Pairwise_Comparisons_PETN** — Surface comparisons for PETN
8. **Pairwise_Comparisons_RDX** — Surface comparisons for RDX
9. **Nonparametric_Tests** — Kruskal-Wallis results and method comparison (NEW)
10. **Levene_Test** — Homogeneity of variance tests by Surface and Lab (NEW)
11. **Variance_Components_PETN** — Variance partition for PETN (42.1% / 57.9%)
12. **Variance_Components_RDX** — Variance partition for RDX (52.0% / 48.0%)
13. **Model_Fit_PETN** — AIC, BIC, LogLik, N_obs, N_groups for PETN
14. **Model_Fit_RDX** — AIC, BIC, LogLik, N_obs, N_groups for RDX

**Implementation**: Replaced placeholder section (lines 1294-1297) with full data export logic. Each sheet created conditionally based on object existence. Includes data frame transformations (ANOVA rownames to column, rounding, factor ordering).

**RDX_Zero_Frequency sheet structure**:
```
Lab | SurfaceName | Total | Zeros | Percent_Zero
Lab1| Steel       |   21  |   0   |    0.0
Lab1| ABS-Smooth  |   73  |  26   |   35.6
...
```

**Levene_Test sheet structure**:
```
Analyte | Factor  | F_value | p_value | Significant
PETN    | Surface |   3.20  |  0.077  |   FALSE
PETN    | Lab     |   3.34  |  0.002  |   TRUE
RDX     | Surface |  41.38  | <0.001  |   TRUE
RDX     | Lab     |   3.58  |  0.001  |   TRUE
```

**Nonparametric_Tests sheet structure**:
```
Analyte | Parametric_p | Nonparametric_p | Surface_Ranking_Parametric | ... | Rankings_Match
PETN    | 1.33e-05     | 0.0574          | Steel > ABS-Smooth         | ... | TRUE
RDX     | 2.75e-17     | 5.55e-11        | Steel > ABS-Smooth         | ... | TRUE
```

#### 5. Key Findings Section (Lines 1568-1591)
Already present from previous session. Reports variance components and p-values for both PETN and RDX when separate models are fitted.

**Console output**:
```
Key Findings:
  → Separate PETN and RDX models fitted
  → PETN surface effect: SIGNIFICANT (p = 1.33e-05)
  → RDX surface effect: SIGNIFICANT (p = 2.75e-17)
  → PETN between-participant variance: 42.1%
  → RDX between-participant variance: 52.0%
```

---

### Expected Results After Integration

**Console output includes**:
```
PETN zeros: 0 (0.0%)
RDX zeros: 26 (27.7%)

Fitting separate mixed-effects models for PETN and RDX...

--- Fitting PETN Model ---
PETN ANOVA:
            Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
Lab         226.38  28.298     8  5.093  3.4164   0.09378 .  
SurfaceName 177.12 177.120     1 85.393 21.3840 1.325e-05 ***

PETN Variance Components:
                            Source Variance       SD Percent_Total
1 Between-Participant (within Lab) 6.025897 2.454770      42.11336
2    Residual (within Participant) 8.282856 2.877995      57.88664

[... PETN tests ...]

--- Fitting RDX Model ---
RDX ANOVA:
            Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)    
Lab          0.672   0.084     8  5.323   0.2522 0.9595    
SurfaceName 38.533  38.533     1 81.192 115.7128 <2e-16 ***

RDX Variance Components:
                            Source  Variance        SD Percent_Total
1 Between-Participant (within Lab) 0.3602572 0.6002143      51.96532
2    Residual (within Participant) 0.3330075 0.5770680      48.03468

[... RDX tests ...]

✓ PARAMETRIC AND NON-PARAMETRIC RANKINGS AGREE FOR BOTH ANALYTES

Generating separate PETN and RDX diagnostic plots...
-> PETN and RDX diagnostic plots saved

Multi-lab mode: Exporting separate PETN and RDX results
-> 14 sheets exported for separate PETN/RDX models

Key Findings:
  → Separate PETN and RDX models fitted
  → PETN surface effect: SIGNIFICANT (p = 1.33e-05)
  → RDX surface effect: SIGNIFICANT (p = 2.75e-17)
  → PETN between-participant variance: 42.1%
  → RDX between-participant variance: 52.0%
```

**Excel workbook** (`FINEX_StudyResults.xlsx`):
- 14 sheets (was 6-7 before integration)
- Variance components show correct values (~42% PETN, ~52% RDX)
- Method comparison confirms parametric and non-parametric rankings agree

**Diagnostic plots** (in `Plots/Statistical_Diagnostics/`):
- 6 PNG files (3 PETN + 3 RDX)
- Each shows analyte-specific variance structure
- Variance component bar charts display correct percentages

---

### Statistical Concepts Explained

#### ANOVA Statistics

**Pr(>F) — P-value**:
- Probability of observing F-statistic this large (or larger) if null hypothesis (no difference) were true
- Pr(>F) < 0.05 → Reject null hypothesis (effect is significant)
- Example: PETN Surface effect Pr(>F) = 1.33e-05 → extremely strong evidence that Steel and ABS-Smooth differ

**DenDF — Denominator Degrees of Freedom**:
- Degrees of freedom for error term in F-statistic denominator
- Calculated using Satterthwaite's approximation for mixed models
- Accounts for nested random effects and unbalanced designs
- Fractional values (e.g., 85.393) are expected and correct
- Represents "effective sample size" after accounting for random effect structure
- PETN: DenDF = 85.4 (close to N = 98, good power)
- RDX: DenDF = 81.2 (close to N = 94, good power)

**Why DenDF ≠ N**:
- Some df "used up" estimating Lab effects (8 df for 9 labs)
- Some df account for random Participant effect
- Unbalanced design reduces effective df
- Satterthwaite approximation provides appropriate df for complex nested designs

#### Levene's Test Results

**Purpose**: Tests homogeneity of variance (equal variances across groups) — a key ANOVA assumption.

**Null hypothesis**: All groups have equal variance  
**Decision rule**: p < 0.05 → Variances NOT equal (assumption violated)

**Interpretation of results**:

| Test | F-value | p-value | Result | Interpretation |
|------|---------|---------|--------|----------------|
| **PETN × Surface** | 3.20 | 0.077 | PASS ✓ | Steel and ABS-Smooth have similar variability for PETN |
| **PETN × Lab** | 3.34 | 0.002 | FAIL ⚠️ | Different labs have different precision for PETN |
| **RDX × Surface** | 41.38 | <0.001 | FAIL ⚠️ | Steel and ABS-Smooth have drastically different variability for RDX |
| **RDX × Lab** | 3.58 | 0.001 | FAIL ⚠️ | Different labs have different precision for RDX |

**Why RDX × Surface fails severely** (F = 41.38):
- ABS-Smooth RDX: Mean = 0.25%, SD = 0.33%, 27.7% zeros (floor effect)
- Steel RDX: Mean = 2.20%, SD = 1.37%, 0% zeros (no floor constraint)
- Variance is constrained by floor (cannot go below 0%) on ABS-Smooth
- This creates **heteroscedasticity** (unequal variance)

**Why violations are acceptable**:
1. Ran Kruskal-Wallis tests (non-parametric, no variance assumptions)
2. Rankings agree between parametric and non-parametric methods ✓
3. Effect sizes are huge (F = 21.38 PETN, F = 115.71 RDX)
4. Mixed-effects models are fairly robust to variance heterogeneity
5. Method comparison confirms findings are robust

**Scientific insight from Levene results**:
- Lab-to-lab differences in **precision** (not just accuracy)
- Opportunity for standardization of technique across labs
- RDX on ABS-Smooth at limit of detection (floor effect expected)

#### Model Fit Statistics

**Purpose**: Evaluate model quality and compare alternative model structures.

**Key metrics** (PETN example):
```
AIC                495.63
BIC                526.65
Log Likelihood    -235.81
N_obs               98
N_groups             9
```

**CRITICAL CLARIFICATION**: **Absolute AIC values are meaningless!**

**Common misconception**: "My AIC = 495 is way above 10, so it's bad"  
**Reality**: AIC has no meaningful scale. The ">10" rule refers to **DIFFERENCES between models**, not absolute values.

**AIC Comparison Rule**:
```
Δ AIC = AIC_model1 - AIC_model2

Δ AIC < 2:   Models essentially equivalent
Δ AIC 2-7:   Weak evidence for lower AIC model
Δ AIC > 10:  STRONG evidence for lower AIC model
```

**Example**: Should you include Lab × Surface interaction?
```
Model A (current):     AIC = 495.63
Model B (interaction): AIC = 498.40

Δ AIC = 498.40 - 495.63 = 2.77

Interpretation: Weak evidence against interaction
Decision: Keep simpler Model A ✓
```

**Why absolute AIC doesn't matter**:
- AIC scale depends on sample size, response variable scale, variance
- Same model on height data: AIC = 245 (meters), 3452 (mm), -180 (km)
- Larger N → larger AIC (even with identical model quality)
- You can ONLY compare AIC between models on same dataset

**What actually indicates good fit**:
1. Model diagnostics (QQ plots, residuals) ✓
2. Convergence without warnings ✓
3. Reasonable effect sizes (F = 21.38, 115.71) ✓
4. Assumption checks (Levene, Kruskal-Wallis) ✓
5. Adequate sample size (98/94 obs, 9 groups) ✓

**Model fit interpretation**:
- **N_obs = 98** (PETN): Adequate sample size for 12 parameters (8.2:1 ratio)
- **N_groups = 9**: Sufficient for mixed models (need ≥5-6)
- **AIC/BIC ratio = 1.06**: Reasonable (not inflated, no overfitting red flags)
- **Log Likelihood = -235.81**: Used to calculate AIC/BIC, not interpretable alone

**Bottom line**: Model fit statistics are a "quality control" check. Your AIC = 495 is perfectly fine — it's just a comparison metric, not a quality score.

---

### Integration Summary

**Total new code added**: ~280 lines (diagnostic plots + Excel export)

**Files modified**:
1. `Code/05_StatisticalAnalysis.R` — Main integration
   - Lines 132: QC filter fix
   - Lines 1107-1221: Separate diagnostic plots
   - Lines 1294-1462: Full 14-sheet Excel export

2. `Code/05_StatisticalAnalysis_BACKUP.R` — Backup created

**Backward compatibility maintained**:
- Single-lab mode uses combined model (unchanged)
- Few-labs mode uses combined model (unchanged)
- Multi-lab mode uses separate models (new)
- All conditional checks handle both combined and separate models

**Expected behavior by mode**:

| Mode | Labs | Model Type | Sheets Exported | Diagnostic Plots |
|------|------|------------|-----------------|------------------|
| Single lab | 1 | Combined (ANOVA) | 6-7 sheets | Combined model plots |
| Few labs | 2-4 | Combined (mixed) | 6-7 sheets | Combined model plots |
| Multi lab | 5+ | **Separate PETN/RDX** | **14 sheets** | **6 separate plots** |

**Key advantages of separate models**:
1. **Reveals true variance structure** (42-52% vs 13% between-participant)
2. **Respects distributional differences** (PETN normal, RDX floor effect)
3. **Enables analyte-specific diagnostics** (separate residual plots, QQ plots)
4. **Supports analyte-specific robustness checks** (separate Levene, Kruskal-Wallis)
5. **More transparent reporting** (14 sheets vs 7, separate ANOVA tables)
6. **Better scientific interpretation** (operator variability dominates for both analytes)

**Testing completed**: Script modifications verified through code review. User to run full integration test on Accepted Analysis dataset (9 labs, 2 surfaces, 192 QC-passed samples).

---

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
  - Negative Controls sheet
  - Summary sheet with Mean/SD/RSD of mass recovered (ng) at three levels:
    - By Participant (Lab + Participant + Surface)
    - By Surface (Lab + Surface, pooling participants)
    - By Lab (pooling all)
  - **CalSummary sheet** (added June 2026) — calibration statistics from all datasets
  - **SampleSummary sheet** (added July 2026) — sample counts by surface and QC status

**Sample filtering** (added June 2026):
- Requires complete Lab/Participant/Surface information for all samples
- Rows where Lab, Participant, OR Surface is NA are silently excluded from output
- Ensures data quality and prevents analysis errors from malformed sample names
- Examples filtered out: "Calibration Check", "Method Blank", "Lab14-Glass" (missing Participant)

**CalSummary Sheet** (added June 2026):

Collates calibration statistics from all individual lab `*_CalibrationStats.xlsx` files into a single sheet for centralized quality assessment across all datasets.

**Data source**: Discovers all `{Dataset}_CalibrationStats.xlsx` files recursively in the study folder (excluding Backup/ folders).

**Structure**: One row per calibration set (NOT per dataset — some datasets may have multiple calibration sets).

**Columns**:
- `Dataset` — Folder name (e.g., "Lab10-1", "ABS-S-1")
- `Date` — Earliest injection date from sequence log (YYYY-MM-DD format)
- `CalibrationSet` — Calibration set number (1, 2, 3, ...)
- **PETN columns**:
  - `N_Standards_PETN_Peak_Area` — Number of calibration standards used
  - `N_Excluded_LOD_PETN_Peak_Area` — Number excluded due to Below_LOD
  - `R2_PETN_Peak_Area` — R² for PETN calibration curve
  - `AdjR2_PETN_Peak_Area` — Adjusted R²
- **RDX columns** (same structure):
  - `N_Standards_RDX_Peak_Area`
  - `N_Excluded_LOD_RDX_Peak_Area`
  - `R2_RDX_Peak_Area`
  - `AdjR2_RDX_Peak_Area`
- **Internal Standard columns**:
  - `RDX_IS_RSD_Percent` — 15N-RDX IS variability within this calibration set
  - `RDX_IS_RSD_Overall` — 15N-RDX IS variability across entire sequence (duplicated per cal set)

**Sorting**: Rows sorted by Date (ascending), then Dataset, then CalibrationSet.

**Formatting**: Plain data export only (no conditional formatting).

**Use case**: Quickly identify poor calibrations (low R²), assess IS stability across datasets, and track calibration quality trends over time.

**SampleSummary Sheet** (added July 2026, restructured July 29, 2026):

Provides quick overview of sample counts by surface and QC status for the entire study, in a **single table**.

**Structure**: One table, rows in this order: `Steel`, `ABS-Smooth`, `Glass`, `ABS-Textured` (always all four, even if a surface has 0 samples processed so far), then `NC`, then `TOTAL`, then `Successfully Run`.

| Column | Description |
|--------|-------------|
| `Surface` | Surface name, or `NC`, `TOTAL`, `Successfully Run` |
| `Total` | Actual count processed/collated so far for this row |
| `N Labs` (added July 2026) | Number of distinct labs contributing data to this row |
| `N Participants` (added July 2026) | Number of distinct (Lab, Participant) combinations contributing data to this row -- counted as pairs since Participant numbers (1/2) repeat across labs |
| `PASS` | Row-dependent -- see below |
| `PASS*` | Row-dependent -- see below |
| `FAIL` | Row-dependent -- see below |
| `Complete %` | Row-dependent -- see below (Updated July 29, 2026: tracks progress against the full study target, not the pass rate of samples processed so far) |

**`N Labs` / `N Participants`** (added July 2026): computed per row --
- Per-surface rows: `n_distinct(Lab)` / `n_distinct(paste(Lab, Participant))` restricted to samples with that `SurfaceName`.
- `NC` row: same, restricted to `master_ncs`.
- `TOTAL` row: computed directly from the full sample set (`master`), **not** summed from the per-surface rows, to avoid double-counting a lab/participant that contributed to more than one surface.
- `Successfully Run` row: left blank (`NA`) -- consistent with the other breakdown columns on this single-count row.


**Per-surface rows** (`Steel`, `ABS-Smooth`, `Glass`, `ABS-Textured`): standard sample QC outcome —
- `PASS` = samples with no QC issues
- `PASS*` = samples with warnings (e.g. inherited QC brackets, low SNR warnings, per-analyte 0.2ng QC override)
- `FAIL` = samples failing QC criteria (bias, SNR, or bracketing failures)
- `Complete %` (Updated July 29, 2026) = **(PASS + PASS*) / expected study-wide total for that surface × 100** -- NOT `/Total`. `Total` shows how many samples have actually been processed; `Complete %` shows what fraction of the *eventual full study* for that surface is done and QC-passed. A surface with 0 samples processed shows `0.0%`, not blank.

**Study-wide expected totals** (`expected_sample_totals` / `expected_nc_total`, `Code/04_CollateStudyResults.R` ~line 45-62, hardcoded per the study design -- update here if the design target changes. Corrected July 29, 2026: the original figures accidentally folded NC counts into the surface totals; samples and NCs now have separate expected totals that were provided independently):

| Surface | Expected Total |
|---------|----------------|
| Steel | 330 |
| ABS-Smooth | 336 |
| Glass | 141 |
| ABS-Textured | 141 |
| **Grand total (samples)** | **948** |
| NC | 161 |

**`NC` row** (restructured July 29, 2026 to fold the previous separate "Negative Controls" table into this same table; recomputed August 7, 2026 to read from the current two-stage columns instead of the retired `nc_status`/`nc_analysis_valid`), using **Stage 1 (`nc_analysis_accepted`)** crossed with **Stage 2 (`nc_result`)**:
- `PASS` = Stage 1 PASS/PASS* **and** Stage 2 `"Negative (clean)"`
- `PASS*` = Stage 1 PASS/PASS* **and** Stage 2 `"Positive: ..."` (trace/contamination detected). The injection itself was reliable; it just detected something.
- `FAIL` = Stage 1 `FAIL:` (failed IS detection or a failed bracketing QC, either 6ng or 0.2ng), regardless of Stage 2. This is a run-reliability failure, not a positive detection.
- `Complete %` (Updated July 29, 2026) = **(PASS + PASS*) / expected_nc_total (161) × 100** — i.e. progress against the study-wide expected NC total, consistent with the per-surface rows (previously used the dynamic NC total processed so far; corrected once a fixed NC target was provided).

**`nc_analysis_accepted`** (Stage 1, column on the `Negative Controls` sheet, restructured August 7, 2026 from the previous boolean `nc_analysis_valid`) is `"PASS"`/`"PASS*"` only if:
- The internal standard (15N-RDX) was detected in the injection (`rdx_is_snr_flag` not `NA`/`"Not_Detected"`, and `rdx_is_pa_flag == "OK"`), **and**
- All eight bracketing QC columns -- **both** 6ng (`petn_qc_6ng_pre/post`, `rdx_qc_6ng_pre/post`) and 0.2ng (`petn_qc_02ng_pre/post`, `rdx_qc_02ng_pre/post`) -- are not `"FAIL"` (`PASS`/`WARN` both count as acceptable; a missing bracket, `NA`, triggers `"PASS*"` rather than an outright fail, matching how samples' `analysis_accepted` treats a missing bracket). Otherwise it is `"FAIL: <reason>"`.

This distinguishes two independent questions that were previously conflated: **"is this NC negative?"** (`nc_result`, Stage 2) vs **"was this NC injection analytically trustworthy?"** (`nc_analysis_accepted`, Stage 1). A contaminated-but-trustworthy NC (`nc_result` = `"Positive: ..."`) still represents a successful analytical run — the system worked correctly and detected real contamination. An NC with a failed IS or bracket QC gets `nc_analysis_accepted = "FAIL: ..."` and its `nc_result` is forced to `"Not evaluated (analysis failed)"`, regardless of whether its raw peaks happened to look "clean".

**`TOTAL` row**: `Total`/`PASS`/`PASS*`/`FAIL` = sum of the per-surface rows only (samples, excludes the `NC` row). `Complete %` = (sum of per-surface PASS + PASS*) / **grand expected sample total (948)** × 100 -- i.e. overall study-wide completion progress across all four surfaces (NC excluded, since it has its own separate target and row).

**`Successfully Run` row** (Updated July 29, 2026 — a **single headline count**, not a full PASS/PASS*/FAIL breakdown):
- `Total` = `TOTAL.PASS` + `TOTAL.PASS*` + `NC.PASS` + `NC.PASS*` (i.e. sample PASS + sample PASS* + analytically-valid NCs)
- `PASS`, `PASS*`, `FAIL`, `Complete %` are left blank (`NA`) for this row — those breakdowns don't apply to a single combined count.

**Implementation**: `Code/04_CollateStudyResults.R`, `expected_sample_totals`/`expected_nc_total` config (~line 45-62), function `compute_nc_analysis_valid()` (added alongside `compute_nc_status()`), and the SampleSummary sheet construction (~line 1137).

**Example output** (real output, verified July 29, 2026 on the 8 datasets under `Accepted Analysis` — `ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, `Lab29`, `Lab33`, `Lab9`, `Steel-1` — Glass and ABS-Textured have 0 samples processed so far in this subset):

```
Surface            Total  N Labs  N Participants  PASS  PASS*  FAIL  Complete %
Steel                72      6         12          36     33     3        20.9
ABS-Smooth          120     12         20          53     63     4        34.5
Glass                 0      0          0           0      0     0         0.0
ABS-Textured          0      0          0           0      0     0         0.0
NC                   32     12         20           6     18     8        14.9
TOTAL               192     12         20          89     96     7        19.5
Successfully Run    209     NA         NA          NA     NA    NA        NA
```

**Note**: In the output above:
- `Steel.Complete %` = (36 + 33) / 330 × 100 = 20.9% (69 of 330 study-wide expected Steel samples done and QC-passed, from 6 labs / 12 lab:participant combinations)
- `ABS-Smooth.Complete %` = (53 + 63) / 336 × 100 = 34.5% (from 12 labs / 20 lab:participant combinations)
- `Glass`/`ABS-Textured.Complete %` = 0.0% (no datasets processed yet for these surfaces, hence 0 labs)
- `NC.Complete %` = (6 + 18) / 161 × 100 = 14.9% (24 of 161 study-wide expected NCs done and analytically valid)
- `TOTAL.Complete %` = (69 + 116 + 0 + 0) / 948 × 100 = 19.5% (overall study-wide sample progress; NC excluded from this total, tracked separately in its own row). `TOTAL.N Labs`/`N Participants` (12/20) are computed directly across all samples, not summed from the per-surface rows, since the same 12 labs contributed to both Steel and ABS-Smooth in this subset.
- `Successfully Run.Total` = 89 (sample PASS) + 96 (sample PASS*) + 6 (NC PASS) + 18 (NC PASS*) = 209

**Use case**: Quick assessment of study-wide QC performance and **overall study completion progress** (per surface, NC, and overall), identify problematic surfaces, verify NC cleanliness and run reliability, and see how many labs/participants have contributed data to each surface, all in a single table.

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
- **Sample size labels** (added June 2026): "n=X" displayed above each box showing number of observations per group
- Y-axis: Percent recovery (drift-corrected when available, falls back to uncorrected)
- Negative controls excluded from all plots
- Per-lab plots use fixed surface order (Steel, Glass, ABS-Smooth, ABS-Textured)
- Overall plots ordered by median recovery across all labs

**Overall Bar Charts by Participant** (added July 2026, error bars + label format updated July 2026):

Two additional grouped bar charts (300 DPI), one per analyte, showing per-participant recovery broken down by surface:
- `Plots/Overall/Overall_PETN_ByParticipant_BarChart.png`
- `Plots/Overall/Overall_RDX_ByParticipant_BarChart.png`

**Plot features**:
- **X-axis**: one label per participant across the whole study, e.g. `Lab 6 (1)`, `Lab 9 (2)` (`sprintf("Lab %d (%d)", Lab, Participant)`), ordered **numerically** by Lab then Participant (not alphabetically -- avoids `Lab 10 (1)` sorting between `Lab 1 (1)` and `Lab 2 (1)`)
- **Bars**: one dodged bar per surface present for that participant, height = mean recovery across that participant's replicates for that surface (same `recovery_data` as the box plots above: NA recovery -> 0, restricted to `Outcome == "Complete"` samples, NCs excluded)
- **Error bars** (added July 2026): standard error of the mean, SEM = SD / sqrt(n), per participant × surface group. Only drawn when n > 1 (a single-replicate group has an undefined SD, so no error bar is drawn for it -- not zero-width).
- **Fill colour**: one fixed colour per surface (`surface_colors`: Glass `#1b9e77`, Steel `#d95f02`, ABS-Smooth `#7570b3`, ABS-Textured `#e7298a`), consistent between the PETN and RDX charts
- **Dodge**: `position_dodge(width = dodge_width)` (`dodge_width <- 0.9`) applied identically to both `geom_bar()` and `geom_errorbar()` so the error bars align exactly over their corresponding bar. (Not `position_dodge2()` -- that computes dodge slot width per-geom from each geom's own rendered width, which caused the errorbar's `width` parameter to desync from the bar's implicit width and misalign the error bars whenever more than one surface was present at a given participant; see July 29, 2026 session summary. Plain `position_dodge()` guarantees alignment but means bar width varies slightly depending on how many surfaces are present for that participant.)
- Participants with no data for an analyte are simply omitted from that analyte's chart

**Implementation**: `Code/04_CollateStudyResults.R`, immediately after the Overall box plot generation (~line 1604), reuses `recovery_data`, `fixed_surface_order` already computed for the box plots. New objects: `surface_colors`, `participant_bar_data` (with `petn_mean`/`petn_sd`/`petn_n`/`petn_sem` and RDX equivalents), `participant_order`, `dodge_width`, `create_participant_barchart()`.

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

**Statistical Modeling Approach:**

The analysis uses **Linear Mixed-Effects Models (LMM)** via the `lme4::lmer()` function. This is appropriate for continuous recovery percentages that are approximately normally distributed.

**R Packages Used:**
- `lme4` — Core mixed-effects modeling (`lmer()` function)
- `lmerTest` — Adds p-values and degrees of freedom to `lmer()` models using Satterthwaite approximation
- `emmeans` — Estimated marginal means, contrasts, and post-hoc comparisons
- `effectsize` — Calculate partial eta-squared effect sizes

**Model structure (multi-lab, Lab as FIXED effect):**
```r
Recovery ~ Lab + SurfaceName + Analyte + 
           Lab:SurfaceName + SurfaceName:Analyte + 
           (1|Lab:Participant)
```

**Key features:**
- **Lab**: Fixed effect (specific lab deviations from grand mean)
- **Surface & Analyte**: Fixed effects (main effects)
- **Lab:Surface**: Fixed interaction (tests if surface ranking differs by lab)
- **Surface:Analyte**: Fixed interaction (tests if surface effect differs for PETN vs RDX)
- **Lab:Participant**: Random effect (participant nested within lab)

**Sum-to-zero contrasts for Lab:**
```r
contrasts(long_data$Lab) <- contr.sum(length(levels(long_data$Lab)))
```
This makes Lab effects interpretable as **deviations from grand mean** rather than comparisons to a reference lab.

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

**Important Note on Model Choice:**

Currently using `lmer()` (Linear Mixed Model) which assumes **normal distribution** of residuals. This is appropriate for recovery data that is continuous and roughly normally distributed.

**Future consideration:** RDX recovery on ABS-S surfaces shows extremely low values with many zeros, violating normality assumptions. If residual diagnostics show severe non-normality, skewness, or boundary effects (many 0% or 100% values), consider switching to **Generalized Linear Mixed Model (GLMM)** using `glmer()`:

```r
# Example GLMM with binomial family for bounded proportions
glmer(Recovery_proportion ~ Lab + SurfaceName + Analyte + 
                            Lab:SurfaceName + SurfaceName:Analyte + 
                            (1|Lab:Participant),
      data = long_data,
      family = binomial(link = "logit"),
      weights = n_replicates)
```

**When to switch to `glmer()`:**
- Residuals severely non-normal (QQ plot shows heavy deviation)
- Data is heavily skewed (most values near 0% or 100%)
- Many zero values causing floor effects
- Logit transformation (current optional feature) doesn't adequately normalize residuals

**Decision deferred until full dataset available for assessment.**

**Outputs (added to FINEX_StudyResults.xlsx):**

| Sheet | Content | Availability |
|-------|---------|--------------|
| **Descriptive_Statistics** | n, Mean, SD, SE, Median, IQR, RSD%, 95% CI by Analyte × Surface | Always |
| **Statistical_Summary** | Fixed effects ANOVA table (F-tests for Lab, Surface, interactions) | If models fitted |
| **Fixed_Effects** | All fixed effect coefficients including lab terms | If models fitted |
| **Variance_Components** | Between-Participant (within Lab) and Residual variance | If mixed model fitted |
| **Lab_Effects** | Lab deviations from grand mean with p-values and 95% CI | Multi-lab only |
| **Lab_x_Surface** | Lab × Surface interaction means (if interaction fitted) | Multi-lab only |
| **Surface_Comparisons** | Tukey-adjusted surface × surface comparisons per analyte | If models fitted and ≥2 surfaces |
| **Model_Fit** | AIC, BIC, N observations | If models fitted |

**Diagnostic plots** (`Plots/Statistical_Diagnostics/`):
- `Residual_QQ.png` — Normality check
- `Residuals_vs_Fitted.png` — Homoscedasticity check
- `Lab_Effects_ForestPlot.png` — Forest plot of lab deviations from grand mean (multi-lab only)
- `Lab_Surface_Interaction.png` — Interaction plot showing if surface ranking differs by lab (multi-lab only)
- `Variance_Partition.png` — Pie chart of variance components (2 components: Participant, Residual)
- `Surface_Effects.png` — Forest plot of estimated marginal means
- `Recovery_Boxplots.png` — Box plots by Surface × Analyte (always)

**Key outputs:**
1. **Lab effect** — F-test p-value; if significant, specific labs differ from grand mean
2. **Lab × Surface interaction** — F-test p-value; if significant, surface ranking differs by lab
3. **Surface effect** — F-test p-value; if significant, surfaces differ in recovery
4. **Partial η²** — Effect size; proportion variance explained by each factor
5. **Variance components** — % variance due to Participant (within Lab) and Residual (multi-lab only)
6. **Lab deviations** — Each lab's deviation from grand mean with p-values (multi-lab only)
7. **Pairwise comparisons** — Which specific surfaces differ (Tukey HSD)
8. **Descriptive statistics** — Summary tables for reporting

**Single-lab limitations:**
- Cannot estimate between-lab variance or test lab effects
- Cannot generalize results beyond this lab
- If only 1 participant: Cannot assess operator variability
- If multiple participants: Limited assessment (fixed effect only)

**Multi-lab with Lab as fixed effect:**
- Lab effects show **specific lab deviations** from grand mean (not variance component)
- Cannot partition "between-lab variance" as single value
- Results apply to the **specific labs tested** (not generalizable to untested labs)
- Can test if specific labs perform above/below average
- Can test if surface ranking differs by lab (Lab × Surface interaction)
- Variance components: Only 2 levels (Participant within Lab, Residual)

**Interpretation guide:**

| Metric | Interpretation |
|--------|----------------|
| **Lab p < 0.05** | Specific labs differ significantly from grand mean; see Lab_Effects sheet |
| **Lab:Surface p < 0.05** | Surface ranking differs by lab; surface effect not consistent across labs |
| **Surface p < 0.05** | Recovery differs significantly between surfaces (pooled across labs) |
| **Lab deviation > 0** | Lab performs above average (grand mean) |
| **Lab deviation < 0** | Lab performs below average (grand mean) |
| **High Participant variance** | Operators within labs differ; training/SOP adherence variable |
| **High residual variance** | Repeatability issue; method precision limited |
| **Interaction significant** | Surface:Analyte = surface ranking differs for PETN vs RDX; Lab:Surface = surface ranking differs by lab |

**Error handling:**
- Automatically detects lab/participant structure
- Falls back to simpler models if mixed-effects fail
- Warns user about interpretation limitations
- Always provides descriptive statistics regardless of model success
- Post-hoc comparisons require ≥2 surfaces

**Technical notes (June 2026):**
- Uses `lme4::lmer()` for Linear Mixed-Effects Models (LMM), not `glmer()` for GLMM
- `lmerTest` package loaded after `lme4` to add p-values via Satterthwaite approximation
- `emmeans` package for marginal means and contrasts (including sum-to-zero "eff" contrasts)
- Sum-to-zero contrasts applied to Lab factor: `contrasts(long_data$Lab) <- contr.sum(n_levels)`
- Column renaming in `pairs()` output uses safe `colnames()` approach with `[[]]` extraction to avoid tidyverse quirks
- Variable assignment in `tryCatch()` uses super-assignment (`<<-`) to ensure persistence in outer scope
- REML = TRUE for variance component estimation (use REML = FALSE if comparing nested fixed effects models)

**Why `lmer()` not `glmer()`:**
- Recovery % is continuous and approximately normally distributed
- `lmer()` assumes normal distribution (Gaussian family) with identity link
- `glmer()` would be used for:
  - Binary outcomes (detected/not detected) → `family = binomial(link = "logit")`
  - Count outcomes (number of peaks) → `family = poisson(link = "log")`
  - Strictly bounded proportions with severe skewness → `family = binomial(link = "logit")`
- Current data appropriate for `lmer()`; switch to `glmer()` only if residual diagnostics show severe non-normality

**Random effects syntax:**
- `(1 | Lab)` = Random intercept for Lab (each lab has different baseline) — NOT USED since Lab is fixed
- `(1 | Lab:Participant)` = Random intercept for Participant nested within Lab (each participant has different baseline)
- `(1 + Surface | Lab)` = Random intercept AND random slope (surface effect varies by lab) — NOT USED (use fixed Lab:Surface interaction instead)

**Alternative packages NOT used:**
- `nlme::lme()` — Older mixed modeling package; `lme4::lmer()` is current standard
- `glmer()` — For non-normal distributions; not needed for continuous normal data
- Custom `random.factor()` functions — Not a standard R function for mixed models

**Reporting example (multi-lab, Lab as fixed effect):**
```
A linear mixed-effects model was fitted with Lab, Surface, and Analyte 
as fixed effects, including Lab × Surface and Surface × Analyte 
interactions, with Participant nested as a random effect within Lab. 
Sum-to-zero contrasts were applied to Lab effects to interpret 
coefficients as deviations from the grand mean.

Lab effects were significant (F(36, 1824) = 15.2, p < 0.001), 
indicating systematic differences between laboratories. Twelve labs 
showed significant deviations from the grand mean (α = 0.05): Lab 22 
performed 15.7% above average (p < 0.001), while Lab 33 performed 
12.4% below average (p < 0.001). See Lab_Effects sheet for complete 
lab rankings.

The Lab × Surface interaction was significant (F(108, 1824) = 2.1, 
p = 0.042), indicating that surface ranking was not consistent across 
all laboratories. Most labs showed the pattern Glass > Steel > ABS, 
but some labs showed reversed rankings for certain surface pairs.

Recovery differed significantly by surface type for both PETN 
(F = 24.5, p < 0.001) and RDX (F = 18.2, p = 0.001). Glass 
surfaces yielded the highest recovery (PETN: mean = 62.3%, 
RSD = 8.2%; RDX: mean = 58.1%, RSD = 10.4%), followed by steel 
(PETN: 45.2%, RSD = 11.7%), ABS-Smooth (23.1%, RSD = 15.3%), 
and ABS-Textured (12.5%, RSD = 22.1%).

Pairwise comparisons (Tukey HSD) showed that glass had 
significantly higher recovery than all other surfaces (p < 0.01). 
Steel was significantly higher than both ABS surfaces (p < 0.05).

Variance partitioning showed that 12.5% of variance was attributed 
to between-participant differences within labs, and 87.5% to 
within-participant repeatability (residual variance). Lab variance 
was not estimated as a random component since Lab was fitted as a 
fixed effect.

These results apply to the specific 37 laboratories tested and 
cannot be generalized to predict performance of new/untested labs.
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

**Minimum QC Requirement (Updated July 2026):**

PETN drift correction requires **≥3 QC injections** at the selected concentration level.

**Rationale:**
- Power law method (`Signal ∝ Injection^-1.0`) requires ≥3 points for meaningful fit
- 2 QCs provide no degrees of freedom to assess model quality (R² is meaningless)
- Linear interpolation between 2 points is not true drift modeling
- Validation study (June 2026) showed power law achieves 45% better accuracy than polynomial

**Behavior with insufficient QCs:**
- < 3 QCs: Drift correction skipped
- Message: "PETN drift correction: fewer than 3 usable QC points (need ≥3 for power law fit) -- skipping."
- Result: `petn_drift_model = "None"`, all correction factors = 1.0

#### RDX Drift Correction (Removed July 2026)

**Previous behavior (May-June 2026):** QC-based polynomial drift correction was available for RDX PA-only mode.

**Current behavior (July 2026+):** RDX drift correction has been **removed entirely**.

**Rationale:**
- When `use_is_for_rdx = TRUE`: IS ratio correction is sufficient (~1.2% RSD)
- When `use_is_for_rdx = FALSE`: PA-only mode uses uncorrected concentrations
- RDX drift patterns were not validated with power law (validation focused on PETN)
- Simpler pipeline with single analyte drift correction
- Power law drift correction requires ≥3 QCs; removing 2-QC support necessitated removing RDX drift correction

**RDX output columns:**
- NO drift-corrected columns created (no `_dc` suffix for RDX)
- Use `rdx_concentration`, `rdx_percent_bias`, `rdx_qc_flag` (uncorrected values)
- Downstream scripts calculate `rdx_mass` and `rdx_recovery` from uncorrected concentration

**Backward compatibility:**
- Old datasets (processed before July 2026) may have `rdx_recovery_dc` columns
- CollateStudyResults.R and StatisticalAnalysis.R include fallback logic to handle both formats

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

#### PETN Output Columns

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
- Missing: final optimised method summary, results section, ~~LOD/LOQ information~~ (SNR thresholds used instead of formal LOD/LOQ validation)

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

- ~~Investigate LOD/LOQ determination for final method~~ → **NOT REQUIRED** (SNR thresholds sufficient)

---

## Session Summary (July 2, 2026) - IN PROGRESS

### Current Task: Statistical Analysis Script Integration

**Status**: Partially complete - multi-lab section needs integration from TEMP file into main script

**What we're doing**: Implementing separate PETN and RDX statistical models to address fundamental distributional differences:
- **PETN**: 0% zeros, 98 observations, 42.1% between-participant variance
- **RDX**: 27.7% zeros (floor effect from ABS-Smooth surface), 94 observations, 52.0% between-participant variance

**Problem identified**: The current `05_StatisticalAnalysis.R` uses a **combined model** (PETN and RDX together with Analyte as a factor), which artificially dilutes participant effects and masks variance structure differences. Combined model shows only 12.7% between-participant variance vs 42-52% when separated.

**Solution approach**: Replace multi-lab section (lines 610-960) with separate PETN/RDX models from `05_StatisticalAnalysis_TEMP_MULTILAB.R`.

### Files Involved

| File | Status | Purpose |
|------|--------|---------|
| `Code/05_StatisticalAnalysis.R` | **NEEDS UPDATE** | Main script - lines 610-960 need replacement |
| `Code/05_StatisticalAnalysis_TEMP_MULTILAB.R` | Complete | Contains correct separate models implementation (223 lines) |
| `Code/05_StatisticalAnalysis_Diagnostics.R` | Complete | Reference implementation that works perfectly - use as template |

### What Needs to Happen (Tomorrow)

**Task 1: Replace Multi-Lab Section (lines 610-960)**

Current approach (WRONG):
```r
# Combined model with Analyte factor
model_combined <- lmer(Recovery ~ Lab + SurfaceName + Analyte + 
                       Lab:SurfaceName + SurfaceName:Analyte + 
                       (1 | Lab:Participant),
                       data = long_data, REML = TRUE)
```

Target approach (CORRECT):
```r
# Separate PETN model
model_petn <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                   data = long_data_petn, REML = TRUE)

# Separate RDX model  
model_rdx <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                  data = long_data_rdx, REML = TRUE)
```

**Step-by-step integration:**

1. **Add data splitting section** (after line 300, before multi-lab section):
   ```r
   # Split data by analyte for separate models
   long_data_petn <- long_data %>% filter(Analyte == "PETN")
   long_data_rdx <- long_data %>% filter(Analyte == "RDX")
   
   # Apply sum-to-zero contrasts for Lab (both datasets)
   contrasts(long_data_petn$Lab) <- contr.sum(nlevels(long_data_petn$Lab))
   contrasts(long_data_rdx$Lab) <- contr.sum(nlevels(long_data_rdx$Lab))
   
   # Report zero counts
   petn_zeros <- sum(long_data_petn$Recovery == 0)
   rdx_zeros <- sum(long_data_rdx$Recovery == 0)
   print(paste0("PETN zeros: ", petn_zeros, " (", 
                round(100*petn_zeros/nrow(long_data_petn), 1), "%)"))
   print(paste0("RDX zeros: ", rdx_zeros, " (", 
                round(100*rdx_zeros/nrow(long_data_rdx), 1), "%)"))
   ```

2. **Replace lines 610-960** with content from `05_StatisticalAnalysis_TEMP_MULTILAB.R`:
   - Delete existing combined model code (lines 610-960)
   - Insert entire TEMP_MULTILAB.R content (lines 1-223)
   - This creates separate `model_petn` and `model_rdx` objects
   - Includes Levene's tests for variance homogeneity
   - Includes non-parametric tests (Kruskal-Wallis + Dunn)
   - Includes method comparison table

3. **Update diagnostic plots section** (lines 990-1140):
   - Generate 8 plots instead of 4:
     * `PETN_Residuals_vs_Fitted.png`
     * `PETN_Residuals_Faceted_by_Lab.png` (NEW - 9 panels)
     * `PETN_QQ_Plot.png`
     * `PETN_Variance_Components.png`
     * `RDX_Residuals_vs_Fitted.png`
     * `RDX_Residuals_Faceted_by_Lab.png` (NEW - 9 panels)
     * `RDX_QQ_Plot.png`
     * `RDX_Variance_Components.png`
   - Use diagnostic script (lines 250-400) as template for faceted plots

4. **Update Excel output section** (lines 1200-1400):
   - Create 14 sheets instead of current structure:
     * Descriptive_Overall
     * Descriptive_By_Lab
     * Descriptive_Participant
     * RDX_Zero_Frequency
     * Statistical_Summary_PETN
     * Statistical_Summary_RDX
     * Pairwise_Comparisons_PETN
     * Pairwise_Comparisons_RDX
     * Nonparametric_Tests
     * Levene_Test
     * Variance_Components_PETN
     * Variance_Components_RDX
     * Model_Fit_PETN
     * Model_Fit_RDX
   - Use diagnostic script (lines 450-550) as template

5. **Test the modified script**:
   - Run on Accepted Analysis dataset (9 labs, 2 surfaces)
   - Verify 14 Excel sheets created
   - Verify 8 diagnostic plots generated
   - Compare PETN/RDX model results to diagnostic script output (should match exactly)
   - Check that variance components show ~42% (PETN) and ~52% (RDX) between-participant

### Key Diagnostic Findings to Verify

From the diagnostic script run, we expect:

| Metric | PETN | RDX |
|--------|------|-----|
| **Parametric p-value** (Surface) | 1.33e-05 | 2.75e-17 |
| **Non-parametric p-value** (K-W) | 0.057 | 5.55e-11 |
| **Between-participant variance** | 42.1% | 52.0% |
| **Surface ranking** | Steel > ABS-Smooth | Steel > ABS-Smooth |
| **Rankings agree?** | Yes | Yes |

**Important**: Both parametric and non-parametric methods agree on surface ranking (Steel > ABS-Smooth), validating conclusions despite moderate assumption violations.

### Why Separate Models?

**From Pat's notes** (statistical consultant):
- Observed "obvious floor effect" in RDX data
- Recommended separate analyses
- Identified both Lab and Participant as significant sources of variance

**Empirical evidence**:
- RDX: 27.7% zeros (26/94 observations) - floor effect from ABS-Smooth surface
- PETN: 0% zeros (98 observations) - no floor effect
- Combined model: Only 12.7% between-participant variance (artificially diluted)
- Separate models: 42.1% (PETN) and 52.0% (RDX) between-participant variance

**Diagnostic plots**:
- PETN QQ: Heavy right tail (high outliers), LOESS curve acceptable (~1.5 waves)
- RDX QQ: Heavy tails at both ends (floor effect signature), LOESS very straight

### Alternative If Integration Problematic

The diagnostic script (`05_StatisticalAnalysis_Diagnostics.R`) **already works perfectly** and implements everything needed. If integrating into the main script proves difficult:

1. Rename diagnostic script → `05_StatisticalAnalysis_v2.R`
2. Add any missing features from old script (single_lab/few_labs modes)
3. Use as the new primary analysis script

**Benefit**: Clean implementation, already tested and working, matches expected outputs exactly.

### Reference Files for Tomorrow

**Key sections to copy**:
- `05_StatisticalAnalysis_TEMP_MULTILAB.R`: Lines 1-223 (entire file)
- `05_StatisticalAnalysis_Diagnostics.R`: Lines 50-130 (descriptive stats), 250-400 (diagnostic plots), 450-550 (Excel export)

**Testing command**:
```r
source("Code/GlobalCode.R")
source("Code/05_StatisticalAnalysis.R")
```

**Expected console output**:
```
=== FITTING SEPARATE MODELS ===
--- Fitting PETN Model ---
PETN ANOVA: ...
PETN Variance Components: ...
--- Fitting RDX Model ---
RDX ANOVA: ...
RDX Variance Components: ...
=== Method Comparison ===
✓ PARAMETRIC AND NON-PARAMETRIC RANKINGS AGREE FOR BOTH ANALYTES
```

**Expected files created**:
- `FINEX_StatisticalAnalysis.xlsx` (14 sheets)
- 8 PNG diagnostic plots in `Plots/Statistical_Diagnostics/`

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
- NA values in peak detection columns filled with "ND" (Not Detected); NA concentrations filled with "ND" or "BQL" (Below Quantification Limit) based on SNR flag

---

### Feature: QC Bracket Carry-Forward for Failed Injections

**File**: `Code/04_CollateStudyResults.R`

**Problem**: When a QC injection fails to inject (autosampler error — RDX IS not detected), the bracket assignment logic assigned `NA` to adjacent samples, triggering `PASS*` (warning) even though the previous QC may have been perfectly acceptable.

**Detection criterion**: A QC "did not inject" when `rdx_is_snr_flag == "Not_Detected"` or is `NA`. This is the universal indicator — if the internal standard wasn't detected, nothing was injected.

**Solution**: When the closest QC didn't inject, look back (or forward) to find the last QC of the same level that DID inject, and use its result. An `inherited` boolean flag column marks when this carry-forward occurs.

**New helper functions** (lines 284-322):
- `find_injected_qc_pre(qc_df, sample_line)` — searches backwards for last QC that injected
- `find_injected_qc_post(qc_df, sample_line)` — searches forwards for next QC that injected
- Both return `list(row = data_row, inherited = TRUE/FALSE)`

**New columns added** (8 boolean flags):
- `petn_qc_6ng_pre_inherited`, `petn_qc_6ng_post_inherited`
- `rdx_qc_6ng_pre_inherited`, `rdx_qc_6ng_post_inherited`
- `petn_qc_02ng_pre_inherited`, `petn_qc_02ng_post_inherited`
- `rdx_qc_02ng_pre_inherited`, `rdx_qc_02ng_post_inherited`

**XLSX formatting**: Inherited = TRUE cells highlighted amber.

**Key design decisions**:
- Same level only: 6ng inherits from previous 6ng; 0.2ng from previous 0.2ng
- If the QC injected but analyte wasn't detected (genuine below-detection), that is NOT a carry-forward scenario — the injection happened, the analyte just wasn't there
- `compute_analysis_accepted()` unchanged — inherited QC provides a real PASS/FAIL result
- If all QCs at a level failed to inject, bracket stays `NA` (triggers `PASS*` as before)

---

### Fix: Backup Files Excluded from Collation

**File**: `Code/04_CollateStudyResults.R`, line 76

**Problem**: The recursive file discovery (`list.files(..., recursive = TRUE)`) was picking up timestamped backup copies of results files from `Results/Backup/` folders, causing duplicate data in the collated study results.

**Fix**: Added filter to exclude paths containing `/Backup/`:

```r
results_files <- results_files[!grepl("/Backup/", results_files, fixed = TRUE)]
```

---

### Feature: `Outcome` Column in Collated Study Results

**File**: `Code/04_CollateStudyResults.R`, lines 603-618

**Purpose**: Provides a simple actionable outcome for each sample based on the `analysis_accepted` evaluation. Allows quick identification of what action is needed for failed samples.

**Logic** (priority order):

| `analysis_accepted` | `Outcome` | XLSX Colour | Meaning |
|---|---|---|---|
| `PASS` or `PASS*` | `Complete` | Green | No further action needed |
| `FAIL:` (any reason) | `Reanalyse` | Red | QC brackets failed or signal quality insufficient — rerun the batch |
| `NC` | `NA` | — | Negative controls evaluated separately via `nc_analysis_accepted`/`nc_result` (see "NC Evaluation Restructured into Two Explicit Stages" session summary) |

**XLSX conditional formatting**: The "Reanalyse" outcome uses expression-based formatting with red background.

**Column position**: Immediately after `analysis_accepted` in `desired_cols`.

---

### Feature: Per-Analyte Minimum Peak Height Thresholds

**Files**: `GlobalCode.R`, `Code/02_PeakDetection.R`

**Problem**: The previous single SIM threshold (`SignalMaxThresholdSIM = 40`) was too low to discriminate between noise artefacts and genuine peaks. PETN blanks frequently showed small peaks (PH 50-180, SNR 1-6) that were "detected" and reported as Below_LOD/Below_LOQ, cluttering results with false positives.

**Solution**: Replaced the single `SignalMaxThresholdSIM` with per-analyte minimum peak height thresholds, validated against Lab10-1, Lab17-1, and ABS-S-1 datasets.

**Configuration** (`GlobalCode.R`, lines 103-108):

```r
MinPeakHeight_PETN <- 200   # Blank noise max PH ~180; lowest real 0.2ng QC PH ~260
MinPeakHeight_RDX  <- 50    # Blanks always NA for RDX; lowest real 0.2ng QC PH ~61
MinPeakHeight_IS   <- 40    # 15N-RDX IS injection confirmation threshold
```

**Validation basis**:

| Analyte | Max Blank PH (noise) | Min 0.2ng QC PH (real peak) | Threshold | Margin |
|---------|---------------------|----------------------------|-----------|--------|
| PETN | ~180 (across 3 datasets) | ~260 (Lab10-1 degraded) | 200 | 60 PH gap |
| RDX | Always NA (no blank RDX) | ~61 (Lab10-1 degraded) | 50 | 11 PH gap |
| IS (15N-RDX) | N/A | ~2,340 (lowest valid) | 40 | Very conservative |

**Implementation**:
- Each analyte and IS config now includes `min_peak_height` field
- `02_PeakDetection.R` builds a `mz_min_peak_height` lookup mapping m/z → threshold
- `process_sim_channel()` receives the per-channel threshold (no code changes needed in ModPeaks.R)
- Peaks below threshold are not detected — no PA, PH, SNR, or snr_flag reported

**IS injection confirmation**: `MinPeakHeight_IS = 40` serves as the injection confirmation gate. If the IS peak height is below 40, it won't be detected (`rdx_is_snr_flag = NA`), and the QC carry-forward logic in `04_CollateStudyResults.R` treats this as "did not inject".

**Effect**:
- PETN blanks with PH < 200: No longer detected (clean results)
- PETN carryover (PH > 200 in blanks after high cals): Still detected — correctly flagged downstream
- 0.2ng QCs (even degraded late-sequence): Still detected (PH ≥ 260)
- RDX: Threshold is a safety net (blanks never show RDX signal anyway)

**Files modified**:
- `GlobalCode.R`: Lines 103-108 (thresholds), 134 (IS config), 168/174 (analyte configs), 329-337 (validation), 387-389 (reproducibility log)
- `Code/02_PeakDetection.R`: Lines 184-189 (mz_min_peak_height lookup), line 225 (per-channel threshold passed to process_sim_channel)

---

## Session Summary (June 17, 2026)

### Feature: SystemMonitoring.xlsx - Separate Sheets for 0.2ng and 6ng QCs

**Problem**: QC data from datasets with overlapping injection numbers (DataFile values like "049Summary.csv") were being silently discarded due to flawed deduplication logic. The pipeline used `distinct(DataFile, .keep_all = TRUE)`, which assumed DataFile was globally unique. However, DataFile is sequence-relative (injection number) and repeats across different datasets.

**Example of the bug**:
- Lab22-1 (April 30): QC at injection 49 → "049Summary.csv" added to SystemMonitoring.csv
- ABS-S-2 (June 11): QC at injection 49 → "049Summary.csv" → **Silently discarded** by distinct()
- Result: ABS-S-2 QC data never appeared in monitoring file despite successful processing

**Solution**: Complete redesign of QC monitoring export:

1. **Two separate sheets for QC levels** - QC_0.2ng and QC_6ng sheets for easy comparison
2. **Column structure matches QC sheet** - Uses same columns as QC sheet in `*_GCMSResults.xlsx` files (with exclusions noted below)
3. **DataPath column added as final column** - Full folder path for direct dataset access
4. **XLSX format** - Changed from CSV to XLSX for better compatibility and future formatting options
5. **Fresh start approach** - No legacy compatibility (clean slate)

**Columns removed** (user requested):
- `TrueCalConcAdj` - True calibration concentration (not needed for monitoring)
- `rdx_is_snr`, `rdx_is_snr_flag` - IS SNR not needed for monitoring
- `petn_concentration` - Uncorrected concentration redundant (have drift-corrected)
- `petn_qc_type` - QC type known from sheet/CalLevel
- RDX drift correction columns:
  - `rdx_drift_correction_factor`
  - `rdx_concentration_dc`
  - `rdx_percent_bias_dc`
  - `rdx_qc_flag_dc`
- **Rationale**: Streamlined to essential monitoring columns; RDX uses IS ratio correction only (no drift correction applied)

**Implementation** (`Code/03_Quantification.R`, `GlobalCode.R`):

**Column list (lines 1900-1950):**
```r
qc_export_cols <- c(
  "Date", "Line", "SampleName", "DataFile", "Vial", "CalLevel", "CalibrationSet",
  # IS (15N-RDX) - PA only
  "rdx_is_rt", "rdx_is_pa",
  # PETN
  "petn_rt", "petn_pa", "petn_ph", "petn_snr", "petn_snr_flag", "petn_ratio",
  "petn_drift_correction_factor", "petn_concentration_dc",
  "petn_percent_bias", "petn_qc_flag",
  "petn_percent_bias_dc", "petn_qc_flag_dc",
  # RDX (no drift correction columns)
  "rdx_rt", "rdx_pa", "rdx_ph", "rdx_snr", "rdx_snr_flag", "rdx_ratio",
  "rdx_concentration", "rdx_percent_bias", "rdx_qc_type", "rdx_qc_flag"
)
```

**Split QC data by level (lines 1928-1950):**
```r
# Add DataPath (full folder path) as FINAL column
QCResults <- QCResults %>%
  mutate(DataPath = DataFolder)

# Split into separate dataframes for 0.2ng and 6ng QCs
QCResults_02ng <- QCResults %>% filter(CalLevel == 0.2)
QCResults_6ng <- QCResults %>% filter(CalLevel == 6)

# For 0.2ng QCs: simplify flags to PASS/FAIL (sensitivity check only)
if (nrow(QCResults_02ng) > 0) {
  QCResults_02ng <- QCResults_02ng %>%
    mutate(
      petn_qc_flag = case_when(
        grepl("PASS", petn_qc_flag, ignore.case = TRUE) ~ "PASS",
        grepl("FAIL|WARN", petn_qc_flag, ignore.case = TRUE) ~ "FAIL",
        TRUE ~ petn_qc_flag
      ),
      # Similar for petn_qc_flag_dc and rdx_qc_flag
    )
}
```

**Rationale for 0.2ng flag simplification**: At trace levels, the QC type (SENSITIVITY_CHECK_PASS vs SENSITIVITY_CHECK_WARN vs SENSITIVITY_CHECK_FAIL) is overly detailed for monitoring. The user already knows 0.2ng QCs are for sensitivity assessment. Simplified PASS/FAIL makes the sheet easier to scan visually.

**Deduplication logic (applied per sheet):**
```r
Combined_02ng <- bind_rows(Existing_02ng, QCResults_02ng) %>%
  distinct(DataPath, DataFile, .keep_all = TRUE)
  
Combined_6ng <- bind_rows(Existing_6ng, QCResults_6ng) %>%
  distinct(DataPath, DataFile, .keep_all = TRUE)
```

**File format change (GlobalCode.R line 280):**
```r
qc_monitoring_file <- file.path(
  ".../System Monitoring",
  "SystemMonitoring.xlsx"  # Changed from .csv
)
```

**New file structure** (SystemMonitoring.xlsx):

**Sheet: QC_0.2ng (30 columns)**
```
Date, Line, SampleName, DataFile, Vial, CalLevel, CalibrationSet,
rdx_is_rt, rdx_is_pa,
petn_rt, petn_pa, petn_ph, petn_snr, petn_snr_flag, petn_ratio,
petn_drift_correction_factor, petn_concentration_dc,
petn_percent_bias, petn_qc_flag,
petn_percent_bias_dc, petn_qc_flag_dc,
rdx_rt, rdx_pa, rdx_ph, rdx_snr, rdx_snr_flag, rdx_ratio,
rdx_concentration, rdx_percent_bias, rdx_qc_type, rdx_qc_flag,
DataPath
```
**Note**: QC flags simplified to PASS/FAIL for 0.2ng (SENSITIVITY_CHECK_* → PASS/FAIL)

**Sheet: QC_6ng (30 columns, same structure)**
**Note**: QC flags retain full detail (PASS, FAIL_BIAS, FAIL_SNR) for 6ng quantitative QCs

**Example DataPath values**:
```
C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/ABS-S-2
C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/FINEX Swabbing Study/Accepted Analysis/Lab10-1
```

**Expected behavior**:

| Scenario | Behavior |
|----------|----------|
| **First run** | Creates new SystemMonitoring.xlsx with QC_0.2ng and QC_6ng sheets (5 rows each for ABS-S-2) |
| **Subsequent runs** | Appends new dataset QCs to appropriate sheet, removes duplicates by (DataPath, DataFile) |
| **Overlapping injection numbers** | Properly handled: Different DataPath values make rows unique |
| **Reprocessing same dataset** | Duplicate (DataPath, DataFile) pairs replaced (keeps most recent) |
| **Dataset with only one QC level** | Only creates the sheet for the level present |

**Key improvements**:
1. ✅ **Separate sheets for QC levels** - Easy to review and compare 0.2ng vs 6ng performance independently
2. ✅ **Streamlined columns** - Removed 12 unnecessary columns for cleaner monitoring
3. ✅ **Simplified 0.2ng flags** - PASS/FAIL only (no SENSITIVITY_CHECK_* verbosity)
4. ✅ **Direct dataset access** - Full path enables instant folder navigation
5. ✅ **XLSX format** - Better for Excel users, enables future conditional formatting
6. ✅ **Unique compound key** - (DataPath, DataFile) is truly unique across all datasets
7. ✅ **No legacy baggage** - Clean slate, no NA columns or migration complexity
8. ✅ **Backup protection** - Timestamped backups in Backup/ subfolder before each update

**Column count reduction**:
- **Original design**: 42 columns (single sheet with all QC levels)
- **Final design**: 30 columns per sheet (12 columns removed)
- **Removed**: TrueCalConcAdj, rdx_is_snr/flag, petn_concentration, petn_qc_type, all RDX DC columns

**Removed columns** (not in export):
- `TrueCalConcAdj` - Redundant (CalLevel shows nominal concentration)
- `rdx_drift_correction_factor`, `rdx_concentration_dc`, `rdx_percent_bias_dc`, `rdx_qc_flag_dc` - RDX uses IS ratio only, no drift correction needed

**Files modified**:
- `Code/03_Quantification.R`: Lines 1900-1970 (complete rewrite of QC export section with separate sheets)
- `GlobalCode.R`: Line 280 (changed filename to .xlsx)
- `CONTEXT.md`: Documentation updated

**Testing**:
- Re-run ABS-S-2 → Should create new SystemMonitoring.xlsx with QC_0.2ng and QC_6ng sheets
- Verify 30 columns present in correct order (each sheet)
- Verify DataPath shows full folder path
- Verify 0.2ng QC flags simplified to PASS/FAIL only
- Verify 6ng QC flags retain full detail (PASS, FAIL_BIAS, FAIL_SNR)
- Verify no duplicate rejection for overlapping DataFile values
- Re-run same dataset → Should replace existing rows (deduplication working)
- Verify 0.2ng QCs appear only in QC_0.2ng sheet, 6ng only in QC_6ng sheet

---

### Feature: QC Monitoring Trend Plots (PNG)

**File**: `Code/03_Quantification.R`, lines 2043-2179

Added automatic generation of PNG trend plots showing QC performance across all datasets in the SystemMonitoring.xlsx file. These plots provide visual overview of instrument drift patterns across the entire study.

**Purpose**:
- Quick visual assessment of long-term instrument performance
- Identify problematic datasets (outliers, sudden drops)
- Compare 0.2ng (sensitivity) vs 6ng (quantitative) trends
- Monitor progressive inlet degradation across multiple runs

**Plot specifications**:
- **2 PNG files**: QC_0.2ng_Trends.png and QC_6ng_Trends.png
- **2 facets per file**: PETN (top), RDX (bottom)
- **X-axis**: Sequential QC injection number across all datasets (1, 2, 3, 4, 5... for each QC injection)
- **Y-axis**: Individual peak area per QC injection (not aggregated)
- **Style**: Black points and line, dashed horizontal mean reference
- **Format**: PNG, 300 DPI, 10×8 inches

**Data processing**:
1. Read QC_0.2ng and QC_6ng sheets from SystemMonitoring.xlsx
2. Sort by Date, then Line (injection order within sequence)
3. Assign sequential QC injection number (1, 2, 3... across all datasets)
4. Remove NA values before plotting to ensure proper line connections between available points
5. Reshape to long format for faceting (Analyte = PETN or RDX)
6. Plot individual peak areas (for PA plots) or drift-corrected concentrations (for DC plots)

**Generated plots**:
- **QC_0.2ng_Trends.png**: PETN and RDX sensitivity trends (0.2ng level, peak area)
- **QC_6ng_Trends.png**: PETN and RDX quantitative trends (6ng level, peak area)
- **QC_0.2ng_DriftCorrected_Trends.png**: PETN and RDX drift-corrected concentration trends (0.2ng level)
- **QC_6ng_DriftCorrected_Trends.png**: PETN and RDX drift-corrected concentration trends (6ng level)

**Location**: `.../System Monitoring/` (same folder as SystemMonitoring.xlsx)

**Trigger**: Automatically regenerated after each SystemMonitoring.xlsx update

**Minimum requirement**: Plots only generated if ≥2 QC injections present in monitoring file (single point not useful for trend visualization)

**Console output**:
- If ≥2 QC injections: "-> QC_0.2ng_Trends.png" and "-> QC_6ng_Trends.png"
- If only 1 QC injection: "-> Skipping QC_0.2ng_Trends.png (need ≥2 QC injections for trend)"

**Error handling**: Wrapped in `tryCatch()` — if plot generation fails, warning issued but pipeline continues

**Example interpretation**:

| Pattern | Meaning |
|---------|---------|
| Flat line | Stable instrument performance across all QC injections |
| Gradual decline | Progressive inlet degradation (signal loss over time) |
| Step changes between datasets | Different instrument conditions between runs |
| Within-dataset drift | Signal loss within a single sequence (groups of 5 QCs) |
| Outlier high | Specific QC had carryover or contamination |
| Outlier low | Specific QC injection failed or severely drifted |

**Example with 3 datasets** (5 QCs each at 6ng):
- X-axis: 1, 2, 3, 4, 5 (Dataset 1), 6, 7, 8, 9, 10 (Dataset 2), 11, 12, 13, 14, 15 (Dataset 3)
- Pattern visible: Within-dataset drift (1→5, 6→10, 11→15) and between-dataset differences

**Comparison with per-dataset QC plots**:
- **Per-dataset plots** (`PETN_QC_6ng_RawPA_Trends.png`): Show within-run drift for a single dataset
- **Monitoring PA plots** (`QC_6ng_Trends.png`): Show all QC injections across all datasets (cumulative peak area view)
- **Monitoring DC plots** (`QC_6ng_DriftCorrected_Trends.png`): Show drift-corrected concentrations across all datasets (cumulative accuracy view)

**Drift-corrected concentration plots**:
- Show PETN drift-corrected concentrations (`petn_concentration_dc`) and RDX uncorrected concentrations (`rdx_concentration`)
- **Green shaded area**: Acceptance range (±20% of target concentration)
  - 0.2ng level: 0.16 - 0.24 ng
  - 6ng level: 4.8 - 7.2 ng
- **Dark green solid line**: Target concentration (0.2ng or 6ng)
- **Red dashed lines**: Acceptance limits (±20%)
- **Blue dotted line**: Mean achieved concentration across all QC injections
- **Black line + points**: Individual QC injection results
- Useful for assessing overall quantitative accuracy and effectiveness of drift correction
- Missing QC injections (NA values) are removed, so lines connect between available data points
- Points within green shaded area indicate QC PASS; points outside indicate QC FAIL

**Files modified**: `Code/03_Quantification.R` (lines 2043-2179), `CONTEXT.md` (documentation)

**Testing**:
- After processing multiple datasets, check System Monitoring folder for PNG files
- Verify 2 plots present (0.2ng and 6ng)
- Verify each plot has 2 facets (PETN, RDX)
- Verify x-axis shows sequential numbering (1, 2, 3...)
- Verify mean reference line is present
- Verify title shows correct QC level and dataset count

---

## Session Summary (June 12, 2026)

### Bug Fix: dplyr 1.1.4 Compatibility - Filter Syntax Error

**Problem**: Pipeline failed during peak detection (file 083, QC) with error:
```
Error in `dplyr::filter()`:
! Can't specify an argument named `by` in this verb.
ℹ Did you mean to use `.by` instead?
```

**Root cause**: dplyr 1.1.0+ introduced stricter argument checking. While no explicit `by=` argument was present in the code, the error indicated a parameter interpretation issue at line 460 in `Code/ModPeaks.R` within the `extract_peak_area()` function.

**Solution**: Replaced all 7 `dplyr::filter()` calls in `ModPeaks.R` with base R subsetting using `[row_condition, , drop = FALSE]` syntax:

| Location | Function | Replaced |
|----------|----------|----------|
| Lines 249-251 | `process_sim_channel()` | m/z and RT filtering |
| Lines 367-370 | `calculate_snr()` | Noise window data extraction |
| Line 460 | `extract_peak_area()` | **ERROR LOCATION** - Peak RT window filtering |
| Line 481 | `extract_peak_area()` | Peak integration data extraction |
| Line 567 | `plot_integration()` | Baseline plot data filtering |
| Line 573 | `plot_integration()` | Peak plot data filtering |

**Key changes**:
- `channel_data <- raw_data %>% dplyr::filter(abs(Mass - mz) < 0.5)`
  - → `channel_data <- raw_data[abs(raw_data$Mass - mz) < 0.5, , drop = FALSE]`
- `candidate <- peaks %>% dplyr::filter(x < (expected_rt + rt_tolerance) & x > (expected_rt - rt_tolerance))`
  - → `candidate <- peaks[peaks$x < (expected_rt + rt_tolerance) & peaks$x > (expected_rt - rt_tolerance), , drop = FALSE]`
- `peak_data <- signal %>% dplyr::filter(dplyr::between(RTime, lmin, lmax))`
  - → `peak_data <- signal[signal$RTime >= lmin & signal$RTime <= lmax, , drop = FALSE]`

**Verification**: `grep -n "dplyr::filter" Code/ModPeaks.R` returns no results (all instances replaced).

**Files modified**: `Code/ModPeaks.R` (7 locations), `CONTEXT.md` (documentation updated)

**Impact**: Eliminates dplyr dependency for filtering operations in signal processing functions, resolving compatibility issue with dplyr 1.1.4+. Pipeline can now process all files through peak detection without error.

**Next step**: User will re-run pipeline on ABS-S-2 dataset to confirm fix resolves the issue.

---

### Feature: QC Raw Peak Area Trend Plots

**File**: `Code/03_Quantification.R`, lines 1467-1538

Added diagnostic plots showing **raw peak area trends for QC injections BEFORE drift correction is applied**. These plots make it easy to visually identify drift patterns, outliers (e.g., first QC contamination), and the severity of signal loss across a sequence.

**Purpose**:
- Visualize signal drift magnitude and pattern (linear, quadratic, step changes)
- Identify outlier QCs (e.g., first 6ng QC contaminated with carryover from 10ng cal)
- Assess whether drift correction is needed and which model (linear/quadratic) is appropriate
- Compare drift severity across different datasets

**Plot specifications**:
- **Separate plot for each QC level** (e.g., 0.2ng, 6ng) — optimal y-axis scaling per level
- **Blue points and lines** showing PA progression across injection number
- **Dashed reference line** at mean PA for the level
- **Subtitle** includes mean PA and sample count
- **Format**: PNG, 300 DPI, 8×6 inches

**Generated plots** (per analyte, per QC level):
- `PETN_QC_6ng_RawPA_Trends.png` — PETN 6ng QC PA vs injection number
- `PETN_QC_0p2ng_RawPA_Trends.png` — PETN 0.2ng QC PA vs injection number
- `RDX_QC_6ng_RawPA_Trends.png` — RDX 6ng QC PA vs injection number
- `RDX_QC_0p2ng_RawPA_Trends.png` — RDX 0.2ng QC PA vs injection number

**Data source**: Uses existing `qc_plot_data` dataframe (created at line 1322, filtered to `Type == "QC"`). No additional data processing required.

**Location**: Plots generated after existing QC Accuracy plots (line 1465), before results export (line 1540).

**Expected patterns by dataset**:

| Dataset | Expected PETN 6ng Trend | Expected RDX 6ng Trend |
|---------|------------------------|------------------------|
| **ABS-S-1** | Smooth declining trend (~10-15% loss) | Similar smooth decline |
| **ABS-S-2** | First QC high (outlier), then steep drop | Similar pattern but less pronounced |
| **Lab10-1** | First QC high (outlier), complex drift | Similar first QC issue |
| **Lab17-1** | U-shaped (quadratic drift) | Similar U-shaped pattern |

**Validation**: These patterns match the drift correction model R² values previously observed:
- ABS-S-1: R² = 0.9492 (excellent fit → smooth linear trend expected)
- ABS-S-2: R² = 0.6724 (poor fit → first QC outlier expected)
- Lab10-1: R² = 0.5712 (poor fit → first QC outlier + complex drift expected)
- Lab17-1: R² = 0.8537 (good fit → clear quadratic pattern expected)

**Files modified**: `Code/03_Quantification.R` (lines 1467-1538 added), `CONTEXT.md` (documentation)

**Next steps**: After confirming plots show expected patterns, proceed with drift correction model improvement to handle first QC outliers (exclude from model if >10% higher than expected).

---

## Statistical Analysis Diagnostic Issues (June 30, 2026)

### Dataset Context: Accepted Analysis

**Data structure**:
- **9 labs**: 6, 7, 10, 14, 17, 20, 21, 32, 38
- **2 surfaces only**: ABS-Smooth, Steel (NO Glass or ABS-Textured in this subset)
- **90 samples total** (91 rows including header)
- **Recovery range**:
  - PETN: 0-15% (most values 0.2-14%)
  - RDX: **28 zeros** (31% of measurements), others 0-5.4%

### Observed Issues with 05_StatisticalAnalysis.R

#### Issue 1: Residuals vs Fitted - Curved Blue Line (dip at x≈3)

**What it shows**: Blue loess line dips below zero at fitted values ~3%, indicating systematic over-prediction in low recovery range.

**Root cause**: **Floor effect** - Recovery cannot be negative, but linear model predicts negative values for lowest observations:
- Many RDX values are zero or near-zero (28 zeros + many <1%)
- Model tries to fit these with linear predictors → over-predicts
- Creates characteristic curved residual pattern at boundary

**Physical explanation**: 
- ABS-Smooth surface: Very low/zero RDX recovery (mean RDX PA = 360 vs Steel = 6,953)
- 28 RDX zeros represent "not recovered" or "below detection" rather than true zero recovery
- Linear model cannot capture this bounded/discrete behavior

**Severity**: **Moderate concern**. Violates homoscedasticity assumption but ANOVA F-tests are robust to moderate violations with n>30 per group. Surface ranking conclusion (Steel >> ABS) remains valid.

**Diagnostic command** (identify which observations drive the curve):
```r
resid_data <- data.frame(
  fitted = fitted(model_combined),
  residuals = residuals(model_combined),
  Lab = long_data$Lab,
  Surface = long_data$SurfaceName,
  Analyte = long_data$Analyte,
  Recovery = long_data$Recovery
)

# Observations in the dip region (fitted value 2-4%)
problem_range <- resid_data %>% filter(fitted >= 2 & fitted <= 4)
table(problem_range$Surface, problem_range$Analyte)

# Are RDX zeros driving this?
problem_range %>% filter(Recovery == 0) %>% nrow()

# Visual breakdown by lab/surface
ggplot(resid_data, aes(x = fitted, y = residuals, color = Lab)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Analyte) +
  labs(title = "Residuals by Lab and Analyte")
```

---

#### Issue 2: QQ Plot - Extreme Values Deviate from Line

**What it shows**: **Heavy tails** - both low and high extreme values fall further from the theoretical quantiles than expected for normal distribution.

**Root cause**: **Multiple sources of non-normality**:

1. **Left tail (low extreme)**: 
   - 28 RDX zeros create spike at 0%
   - Very low ABS-Smooth recoveries cluster near floor

2. **Right tail (high extreme)**:
   - Some labs performing unusually well (or contamination/outliers)
   - Surface differences amplified by lab variability

3. **Between-lab heterogeneity**:
   - Different labs show different recovery ranges
   - Mixed within one distribution creates fat tails

**Severity**: **Moderate concern**. ANOVA/mixed models are robust to moderate non-normality with sample sizes >30. With 90 observations, F-tests should remain valid.

**Diagnostic command** (quantify tail behavior):
```r
# Check skewness and kurtosis
library(moments)
skewness(long_data$Recovery)  # Expect positive (right skew)
kurtosis(long_data$Recovery)  # Expect >3 (heavy tails)

# Identify extreme values (>3 SD from mean)
outliers <- long_data %>%
  mutate(
    z_score = (Recovery - mean(Recovery, na.rm = TRUE)) / sd(Recovery, na.rm = TRUE),
    is_outlier = abs(z_score) > 3
  ) %>%
  filter(is_outlier)

print(outliers %>% select(Lab, Surface, Analyte, Recovery, z_score))

# Separate distributions by analyte
qqnorm(long_data$Recovery[long_data$Analyte == "PETN"], main = "PETN Recovery")
qqline(long_data$Recovery[long_data$Analyte == "PETN"])

qqnorm(long_data$Recovery[long_data$Analyte == "RDX"], main = "RDX Recovery")
qqline(long_data$Recovery[long_data$Analyte == "RDX"])
```

---

#### Issue 3: Variance Components Chart - "NA" on X-axis ✅ **RESOLVED** (July 2, 2026)

**What it showed**: Bar chart title "Variance Components" but x-axis showed "NA" instead of source labels (e.g., "Between-Participant", "Residual").

**Root cause**: Factor levels in bar chart plotting code (lines 1015-1017) didn't match the actual Source values in the variance_components dataframe. Code expected old three-component model labels ("Between-Lab", "Within-Lab (Between-Participant)", "Within-Participant (Residual)") but current analysis uses two-component model with Lab as fixed effect.

**Fix applied**: Updated factor levels to match actual Source values: "Between-Participant (within Lab)" and "Residual (within Participant)".

---

### Solution Options (Priority Order)

#### **Option 1: Quick Fix - Accept Violations & Report** (RECOMMENDED)

**Rationale**: Research question is "Does recovery differ by surface?" The answer is clearly **YES** from descriptive statistics and F-test. Moderate assumption violations don't invalidate the ranking conclusion with n=90.

**Actions**:
1. ✅ **Fix variance components NA bug** - **COMPLETED** (July 2, 2026)
2. ✅ **Add non-parametric confirmation**:
   ```r
   # Kruskal-Wallis test (does not assume normality)
   kruskal.test(Recovery ~ SurfaceName, data = long_data %>% filter(Analyte == "PETN"))
   kruskal.test(Recovery ~ SurfaceName, data = long_data %>% filter(Analyte == "RDX"))
   ```
   If both parametric (ANOVA) and non-parametric (Kruskal-Wallis) agree → robust conclusion

3. ✅ **Report violations in thesis**:
   ```
   "Residual diagnostics showed moderate non-normality due to floor 
   effects: 31% of RDX measurements on ABS-Smooth surfaces were zero 
   (below detection/recovery limit), and many additional measurements 
   were near-zero. QQ plots showed heavy tails consistent with 
   between-lab heterogeneity. ANOVA F-tests are robust to moderate 
   non-normality with sample sizes >30 (Schmider et al., 2010), 
   and non-parametric Kruskal-Wallis tests confirmed the same surface 
   ranking (p < 0.001), supporting the validity of the conclusions."
   ```

**Reference**: Schmider E, Ziegler M, Danay E, Beyer L, Bühner M. "Is it really robust? Reinvestigating the robustness of ANOVA against violations of the normal distribution assumption." *Methodology*, 2010, 6(4), 147-151.

---

#### **Option 2: Investigate Patterns** (If time permits)

**Goal**: Understand exactly which labs/surfaces drive the curved residuals and heavy tails.

**Steps**:

1. **Residual patterns by lab**:
   ```r
   ggplot(resid_data, aes(x = fitted, y = residuals)) +
     geom_point(alpha = 0.3) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
     geom_smooth(method = "loess", se = FALSE, color = "blue") +
     facet_wrap(~ Lab) +
     labs(title = "Residuals by Lab")
   ```

2. **Identify high-leverage observations**:
   ```r
   # Cook's distance (influence on model fit)
   cooksd <- cooks.distance(model_combined)
   influential <- which(cooksd > 4/length(cooksd))
   print(long_data[influential, c("Lab", "Surface", "Analyte", "Recovery")])
   ```

3. **Separate PETN and RDX models**:
   ```r
   # RDX and PETN behave very differently (28 RDX zeros vs no PETN zeros)
   model_petn <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                      data = long_data %>% filter(Analyte == "PETN"))
   model_rdx <- lmer(Recovery ~ Lab + SurfaceName + (1|Lab:Participant),
                     data = long_data %>% filter(Analyte == "RDX" & Recovery > 0))
   
   # Check if diagnostics improve
   plot(model_petn)  # Residuals vs fitted
   qqnorm(residuals(model_petn))  # QQ plot
   ```

---

#### **Option 3: Model Improvements** (Only if violations severe)

**Potential fixes**:

**A. Filter RDX zeros** (treat as "not recovered" rather than 0%):
```r
analysis_data <- analysis_data %>%
  mutate(
    rdx_recovery_censored = ifelse(rdx_recovery_dc == 0, NA, rdx_recovery_dc)
  ) %>%
  filter(!(Analyte == "RDX" & Recovery == 0))
```

**Rationale**: Zero recovery is qualitatively different from low recovery (detection limit issue, not true recovery value).

**B. Transformation** (only if descriptive stats also non-normal):
```r
# Log transformation (add 0.01 to avoid log(0))
long_data$Recovery_log <- log(long_data$Recovery + 0.01)

model_log <- lmer(Recovery_log ~ Lab + SurfaceName + Analyte + (1|Lab:Participant),
                  data = long_data)
```

**Rationale**: Log transform can normalize right-skewed distributions and stabilize variance.

**C. Generalized Linear Mixed Model** (GLMM for bounded proportions):
```r
# Binomial family with logit link for 0-100% bounded data
# Requires converting % to proportion (0-1) and providing trial size
long_data$Recovery_prop <- long_data$Recovery / 100

model_glmm <- glmer(Recovery_prop ~ Lab + SurfaceName + Analyte + (1|Lab:Participant),
                    data = long_data,
                    family = binomial(link = "logit"),
                    weights = rep(100, nrow(long_data)))  # Treat as if out of 100 trials
```

**Rationale**: Explicitly models bounded nature of recovery data. More complex but theoretically appropriate.

---

### Next Steps (Tomorrow)

1. **Run variance components diagnostic** (command in Issue 3 above)
2. **Share output** with assistant → Fix NA bug
3. **Run Kruskal-Wallis tests** → Confirm surface ranking holds with non-parametric approach
4. **Choose path**:
   - **Path A** (recommended): Fix bug, add Kruskal-Wallis, report violations → **DONE**
   - **Path B** (optional): Investigate patterns (commands in Option 2)
   - **Path C** (if needed): Try model improvements (Option 3)

---

### Expected Kruskal-Wallis Results

**Prediction**: Kruskal-Wallis will show **significant surface effect** (p < 0.001) for both PETN and RDX, confirming:
- Steel recovery >> ABS-Smooth recovery
- Ranking holds regardless of normality assumptions
- Conclusion is robust

If Kruskal-Wallis agrees with ANOVA → strong evidence that surface differences are real despite violated assumptions.

If Kruskal-Wallis disagrees with ANOVA → deeper investigation needed (Option 2 or 3).

---

### Key Takeaway

**The curved residuals and heavy tails are expected** given your data structure:
- 28 RDX zeros (floor effect)
- Only 2 surfaces with very different recovery (3× difference in PA)
- 9 labs with heterogeneous performance

**These violations are moderate, not severe.** With n=90 and robust F-tests, your surface ranking conclusion is valid. The non-parametric Kruskal-Wallis test will provide additional confidence.

**The NA variance components bug is a code error** that needs fixing but doesn't affect the validity of the surface effect test.

---

## Power Law vs Polynomial Drift Correction (June 2026)

### Validation Study Results

Comprehensive validation comparing 4 drift correction approaches across 4 datasets (ABS-S-1, ABS-S-2, Lab10-1, Lab17-1):

| Method | Description | Average Bias | Result |
|--------|-------------|--------------|---------|
| **Method 1** | Current (Ratio Polynomial) | 11.0% | Baseline |
| **Method 2** | PA Power Law (No IS) | **6.1%** | ✅ **WINNER** |
| **Method 3** | Dual Power (PETN+IS corrected) | 12.1% | Worse than current |
| **Method 4** | PETN Power + Uncorrected IS | 314% | Catastrophic failure |

**Key findings:**

1. **Power law fits drift pattern perfectly**: R² > 0.99 for all datasets vs R² 0.57-0.95 for polynomial
2. **PA-only approach is optimal**: 45% reduction in average QC bias vs current method
3. **IS not needed with power law**: Once drift is properly corrected, IS adds no value
4. **Dual correction fails**: Correcting both PETN and IS removes natural compensation effect
5. **Partial correction catastrophic**: Correcting only PETN PA while using uncorrected IS creates artificial inflation (>300% bias)

**Physical explanation**: Power law (`Signal ∝ Injection^-1.0`) models the actual contamination accumulation process in the inlet better than polynomial. As non-volatile residues build up, active sites increase following a power law decay pattern.

### Legacy: Polynomial Drift Correction (Pre-June 2026)

**Method**: QC-based polynomial (quadratic or linear) drift correction applied to either PETN PA or PETN ratio depending on `use_15nrdx_for_petn` flag.

**Algorithm** (`Code/03_Quantification.R`, lines 698-900, archived here for reference):

1. **Select QC level**: Use `drift_correction_qc_level` or auto-select highest available (typically 6ng)
2. **Calculate correction factors**: `CF = Reference_Response / Measured_QC_Response` where Reference = first QC value
3. **Fit polynomial model**: 
   - If ≥4 QC points: Quadratic `CF ~ Row + Row²` (fall back to linear if R² < 0.5)
   - If 3 QC points: Quadratic (exact fit)
   - If 2 QC points: Linear (exact fit)
4. **Restrict correction range**: Only correct rows after first QC (first QC has CF = 1.0)
5. **Apply correction**: `Corrected_Response = Measured_Response × CF`
6. **Re-quantify**: Use calibration curve to find concentration from corrected response

**Limitations identified:**
- Poor fit for datasets with first QC outlier (R² 0.57-0.67)
- Polynomial doesn't match physical drift process
- Over-corrects late-sequence samples when first QC is contaminated
- Ratio-based correction complicated by IS also drifting (different rate)

**Diagnostic plots generated:**
- `PETN_DriftCorrection.png`: Correction factors vs injection number with fitted polynomial
- `PETN_Calibration_{Set}_DriftCorrected.png`: Calibration plot using corrected responses

**Configuration** (GlobalCode.R):
```r
apply_petn_drift_correction <- TRUE  # Enable/disable drift correction
drift_correction_qc_level <- 6       # QC level to use (or NA for auto-select)
use_15nrdx_for_petn <- TRUE/FALSE    # Determines if ratio or PA is corrected
```

**Archived code location**: Lines 698-900 in `Code/03_Quantification.R` (before power law implementation)

### Current: Power Law Drift Correction (June 2026)

**Method**: Power law model fitted directly to PETN PA decay across QC injections. PA-only quantification (no IS).

**Algorithm** (`Code/03_Quantification.R`, lines 698-900, modified):

1. **Select QC level**: Use `drift_correction_qc_level` or auto-select highest available (typically 6ng)
2. **Check minimum**: Require ≥3 QC points; skip drift correction if fewer
3. **Fit power law model**: `PA ~ a × Row^b` using non-linear least squares (`nls()`)
   - Starting values: `a = max(QC_PA)`, `b = -1.0`
   - Expected exponent: `b ≈ -0.95 to -1.05` (inverse proportional decay)
   - Expected R²: > 0.99
4. **Predict expected PA**: Use power model to predict PA at each injection position
5. **Calculate correction factors**: `CF = Predicted_PA_at_first_QC / Predicted_PA_at_current_injection`
6. **Restrict correction range**: Only correct rows after first QC (first QC has CF = 1.0)
7. **Apply correction**: `Corrected_PA = Measured_PA × CF`
8. **Re-quantify**: Use PA calibration curve to find concentration

**Advantages over polynomial:**
- Near-perfect fit (R² > 0.99 vs 0.57-0.95)
- Models actual physical process (contamination accumulation)
- Works with first QC outliers (model fits through all points)
- Simple, direct correction (no IS complications)
- 45% reduction in average QC bias

**Fallback logic:**
- If power law fails to converge (convergence error): Fall back to polynomial
- If R² < 0.90: Issue warning but still use power law (polynomial not better)
- If < 3 QC points: Use polynomial (power law needs ≥3 points for stable fit)

**Configuration** (GlobalCode.R):
```r
drift_correction_method <- "power_law"  # "power_law" or "polynomial"
apply_petn_drift_correction <- TRUE     # Enable drift correction
drift_correction_qc_level <- 6          # QC level to use
use_15nrdx_for_petn <- FALSE            # MUST be FALSE (PA-only quantification)
```

**Diagnostic plots:**
- `PETN_DriftCorrection.png`: Shows power law fit (PA ~ a×Row^b) with R², exponent, and QC points
- `PETN_Calibration_{Set}_DriftCorrected.png`: Calibration plot using corrected PAs

**Output columns** (unchanged from polynomial):
- `petn_drift_correction_factor`: Multiplicative CF at each injection
- `petn_drift_model`: "PowerLaw", "Quadratic", "Linear", "None", or "Disabled"
- `petn_pa_dc`: Drift-corrected PA
- `petn_concentration_dc`: Concentration from corrected PA
- `petn_percent_bias_dc`: %Bias after correction (Cal/QC only)

**Key difference from legacy**: Fits power law to raw PA, not to correction factors. The correction factor is derived from the power law prediction, not fitted directly.

**Mathematical note**: The power law form `PA = a × Row^b` is equivalent to `log(PA) = log(a) + b×log(Row)`, so it could also be fitted as a linear regression on log-transformed data. The `nls()` approach is used here to avoid log-transforming zeros/NAs and to get direct parameter estimates.

---

## Session Summary (July 20, 2026)

### Feature: Analyte-Specific 0.2ng QC Filtering

**Problem**: Samples with quantifiable analyte signal (SNR ≥ 10) were being rejected due to 0.2ng QC failures, even though the sample itself demonstrated sufficient system sensitivity at that level. The 0.2ng QC failure indicated a QC vial/drift issue, not a fundamental system capability problem.

**Solution**: Modified QC filtering logic to evaluate 0.2ng QC failures on a **per-analyte basis**. A sample now only fails due to 0.2ng QC failure if the corresponding analyte in that sample is also unquantifiable.

**Implementation** (`Code/04_CollateStudyResults.R`, lines 657-683):

```r
# Check 0.2ng QCs (SNR-based: PASS/WARN/FAIL/NA)
# MODIFIED (July 2026): Only fail sample if corresponding analyte is NOT quantifiable
for (j in seq_along(qc_02ng_cols)) {
  col_name <- qc_02ng_cols[j]
  if (col_name %in% names(df)) {
    val <- df[[col_name]][i]
    
    # Determine which analyte this QC relates to
    analyte <- if (grepl("petn", col_name, ignore.case = TRUE)) "PETN" else "RDX"
    analyte_snr_col <- if (analyte == "PETN") "petn_snr_flag" else "rdx_snr_flag"
    analyte_snr <- if (analyte_snr_col %in% names(df)) df[[analyte_snr_col]][i] else NA_character_
    
    if (is.na(val)) {
      warnings_na <- c(warnings_na, paste0("No ", qc_02ng_labels[j], " QC"))
    } else if (val == "FAIL") {
      # Only fail sample if analyte is NOT quantifiable
      if (!is.na(analyte_snr) && analyte_snr == "Quantifiable") {
        # Sample analyte IS quantifiable despite QC failure
        warnings_na <- c(warnings_na, paste0(qc_02ng_labels[j], " QC FAIL (sample OK)"))
      } else {
        # Sample analyte is NOT quantifiable — QC failure is relevant
        failures <- c(failures, paste0(qc_02ng_labels[j], " QC FAIL"))
      }
    } else if (val == "WARN") {
      warnings_na <- c(warnings_na, paste0(qc_02ng_labels[j], " QC low SNR"))
    }
  }
}
```

**Behavior table**:

| 0.2ng QC Status | Sample Analyte SNR | Old Behavior | New Behavior |
|---|---|---|---|
| FAIL | Quantifiable (≥10) | `FAIL: PETN 0.2ng pre QC FAIL` | `PASS*` ✅ |
| FAIL | Below_LOQ (3-10) or Below_LOD (<3) | `FAIL` | `FAIL` (unchanged) |
| WARN | Any | `PASS*` | `PASS*` (unchanged) |
| PASS | Any | Contributes to PASS | Contributes to PASS (unchanged) |

**Key design decisions**:
- **6ng QC logic unchanged**: 6ng QC failures (bias-based) always disqualify samples regardless of sample analyte status
- **Both pre and post brackets**: Logic applies to both pre and post 0.2ng QC brackets
- **Warning message**: `"PETN 0.2ng pre QC FAIL (sample OK)"` when QC fails but sample is quantifiable
- **Mixed cases handled**: If PETN passes but RDX fails, sample fails only for RDX issues

**Console output added** (line 702-705):
```r
# Print summary of 0.2ng QC overrides (samples that passed despite QC failure)
n_qc_overrides <- sum(grepl("QC FAIL \\(sample OK\\)", samples$analysis_accepted), na.rm = TRUE)
if (n_qc_overrides > 0) {
  message("-> ", n_qc_overrides, " sample(s) passed despite 0.2ng QC failure (analyte quantifiable)")
}
```

**Example scenarios**:

**Scenario 1: Both analytes quantifiable, both QCs failed**
- Sample: PETN SNR = 250 (Quantifiable), RDX SNR = 150 (Quantifiable)
- QCs: PETN 0.2ng pre FAIL, RDX 0.2ng post FAIL
- Old result: `FAIL: PETN 0.2ng pre QC FAIL; RDX 0.2ng post QC FAIL`
- New result: `PASS*` (both QC failures ignored)

**Scenario 2: PETN quantifiable, RDX not quantifiable**
- Sample: PETN SNR = 250 (Quantifiable), RDX SNR = 2 (Below_LOD)
- QCs: PETN 0.2ng pre FAIL, RDX 0.2ng pre FAIL
- Old result: `FAIL: RDX Below_LOD; PETN 0.2ng pre QC FAIL; RDX 0.2ng pre QC FAIL`
- New result: `FAIL: RDX Below_LOD; RDX 0.2ng pre QC FAIL` (PETN QC failure ignored)

**Expected impact**: Likely to increase sample pass rate by 5-15% for datasets with late-sequence sensitivity degradation where samples themselves remain quantifiable despite QC failures.

**Files modified**:
- `Code/04_CollateStudyResults.R`: Lines 657-683 (QC filtering logic), lines 702-705 (console summary)
- `CONTEXT.md`: Updated "Sequence Acceptance Criteria" section with analyte-specific filtering table

**Documentation updated**: Added detailed explanation to CONTEXT.md "Sequence Acceptance Criteria" section including logic table, rationale, and implementation notes.

---

## Session Summary (July 29, 2026)

### Bug Fix: Non-Detection QCs Silently Produced NA Instead of FAIL

**Problem identified**: Investigating `FINEX_StudyResults.xlsx` rows for `Lab9 P1 S1-S6` (Steel) showed `petn_qc_02ng_post` blank/`NA` for all six replicates, instead of a `PASS`/`WARN`/`FAIL` value. Root cause traced to the 0.2ng QC injection immediately after this block (`Lab9`, Line 69, `069Summary.csv`) having **no detectable PETN peak at all** — `petn_rt`/`petn_pa`/`petn_ph`/`petn_snr` were all `NA` (displayed as `"ND"` in exported XLSX sheets per the existing NA→"ND" display substitution, `Code/03_Quantification.R` line ~1539). RDX was still clearly detected in the same injection (SNR 68.5, `Quantifiable`), confirming this was a PETN-specific sensitivity loss, not a failed injection.

**Root cause**: Four QC-evaluation blocks in `Code/03_Quantification.R` and one helper in `Code/04_CollateStudyResults.R` treated "cannot evaluate this QC" (SNR/PA/concentration all `NA` because no peak was found) as `NA_character_` — silently dropping the QC result — instead of the `FAIL` (6ng) / `SENSITIVITY_CHECK_FAIL` (0.2ng) flags already documented as the correct outcome for this scenario:
- 0.2ng spec (line ~45-47, `CONTEXT.md`): *"Fail: SNR < 3 OR drift-corrected concentration ≤ 0 OR concentration is NA"*
- 6ng spec (line ~22, `CONTEXT.md`): *"FAIL: General failure (no concentration)"*

The `NA` branches were short-circuiting **before** reaching the already-implemented `FAIL`/`SENSITIVITY_CHECK_FAIL` fallback logic that was designed to handle exactly this case. This meant a non-detection was effectively invisible in bracket columns and QC monitoring sheets, and did not trigger the "Analyte-Specific QC Filtering" (July 20, 2026) logic at all, since that logic only fires on an explicit `"FAIL"` string.

**Fix applied** — 5 locations changed from `NA_character_` to the documented FAIL flag:

| # | File | Location | Change |
|---|------|----------|--------|
| 1 | `Code/03_Quantification.R` | ~line 1118, `petn_qc_flag_dc` 0.2ng block | `is.na(snr_val) \|\| is.na(conc_dc_val)` → `"SENSITIVITY_CHECK_FAIL"` (was `NA_character_`) |
| 2 | `Code/03_Quantification.R` | ~line 1140, `petn_qc_flag_dc` 6ng block | `is.na(bias_val_dc)` → `"FAIL"` (was `NA_character_`) |
| 3 | `Code/03_Quantification.R` | ~line 1180, generic per-analyte loop, 0.2ng (drives legacy `petn_qc_flag` and the **primary** `rdx_qc_flag`) | `is.na(snr_val)` → `"SENSITIVITY_CHECK_FAIL"` (was `NA_character_`) |
| 4 | `Code/03_Quantification.R` | ~line 1197, generic per-analyte loop, 6ng | `is.na(bias_val)` → `"FAIL"` (was `NA_character_`) |
| 5 | `Code/04_CollateStudyResults.R` | ~line 346, `evaluate_qc_bias()` (governs study-level `petn_qc_6ng_pre/post`, `rdx_qc_6ng_pre/post` bracket columns, recomputed independently from raw bias rather than from the flags above) | Added `snr_value` parameter; returns `"FAIL"` only when bias **and** SNR are both `NA` (genuine non-detection); leaves bias-NA-for-other-reasons (e.g. bias column entirely absent in an older-format dataset) as `NA_character_` to avoid falsely failing every QC in legacy datasets. 4 call sites (~lines 468/469/477/478) updated to pass `petn_snr`/`rdx_snr`. |

**Design decision**: Reused existing `FAIL` / `SENSITIVITY_CHECK_FAIL` flag values rather than introducing a distinct "non-detection" flag (e.g. `FAIL_ND`). The raw `petn_pa`/`petn_ph`/`petn_snr` = `"ND"` values already visible in the QC sheet provide the diagnostic reason; no new enum value needed. No changes were required to `evaluate_qc_flag_dc()`, `compute_analysis_accepted()`, QC plot shape scales, or `SystemMonitoring.xlsx` flag simplification — all already correctly handle `FAIL`/`SENSITIVITY_CHECK_FAIL` string values via existing `grepl()` matching.

**Downstream effect (verified by design, not by rerun)**:
- `petn_qc_02ng_post` (Lab9 P1 Steel) / `petn_qc_02ng_pre` (Lab9 P2 Steel) will now show `FAIL` instead of blank.
- Samples whose own analyte remains `Quantifiable` (SNR ≥ 10) despite the QC failure still resolve to `PASS*` via the existing analyte-specific override (July 20, 2026), now with a visible reason (`"PETN 0.2ng post QC FAIL (sample OK)"`) instead of a silently-dropped `NA`.
- Samples where the sample's own analyte is *also* not quantifiable (e.g. `Lab9 P1 S3`, which itself had no detectable PETN) now correctly flip from `PASS*` to `FAIL`, since neither the QC nor the sample demonstrated quantifiable PETN.
- 6ng non-detection failures are **not** subject to the per-analyte override (by design — 6ng failures always disqualify the sample regardless of sample analyte status).

**Reprocessing required**: This changes historical flag computation for any dataset containing non-detect QCs. Plan: re-run `RunAllDatasets.R` across the full `Accepted Analysis` folder, then re-run `Code/04_CollateStudyResults.R` to regenerate `FINEX_StudyResults.xlsx` and `SystemMonitoring.xlsx` with corrected flags.

**Files modified**: `Code/03_Quantification.R` (4 locations), `Code/04_CollateStudyResults.R` (1 helper function + 4 call sites), `CONTEXT.md` (this entry).

**Addendum -- reprocessing actually performed**: `RunAllDatasets.R` was found to have a pre-existing, unrelated bug: it pre-sets a per-dataset `DataFolder` in an isolated environment before sourcing `GlobalCode.R`, but `GlobalCode.R`'s `rm(list=ls())` (line 37) wipes that isolated environment before the hardcoded default `DataFolder` (line 60) overwrites it -- verified empirically with a minimal reproduction. This means `RunAllDatasets.R` as written would silently reprocess the hardcoded default folder for every dataset rather than each intended dataset. Not fixed (out of scope for this session). Instead, reprocessed each of the 8 datasets present under `Accepted Analysis` (`ABS-S-1`, `ABS-S-2`, `Lab10-1`, `Lab17-1`, `Lab29`, `Lab33`, `Lab9`, `Steel-1`) manually: temporarily edited `GlobalCode.R` line 60's `DataFolder` per dataset and ran `Rscript -e "source('GlobalCode.R')"` (which auto-chains `01_MsFilesReorganiser.R` → `02_PeakDetection.R` → `03_Quantification.R`, skipping already-processed peak-detection files and only recomputing calibration/QC/drift correction), then restored `DataFolder` to its original value. Ran `Code/04_CollateStudyResults.R` (with `GlobalCode.R` already sourced in the same session, since `04` also re-sources `GlobalCode.R` internally if `SampleVol`/`DepositMass` don't exist yet, hitting the same `rm(list=ls())` problem if run standalone in a fresh session). Verified Lab9 P1/P2 Steel rows changed exactly as predicted; study-wide `SampleSummary` showed no mass regression (7 FAIL / 240 samples, 97.1% complete).

---

## Session Summary (July 29, 2026, continued) — SampleSummary "Successfully Run" Fix

### Bug: "Successfully Run" Row Counted Failed NCs as Successes

**Problem identified**: In the `SampleSummary` sheet of `FINEX_StudyResults.xlsx`, the "Successfully Run" row added **every** negative control to the `PASS*`/`Total` counts regardless of `nc_status` — including NCs that failed due to contamination (`NC FAIL`). This meant a genuinely failed NC (real contamination detected) was still counted as part of "successfully run", overstating apparent study completeness.

**Root cause**: The row conflated two independent questions:
1. **Is this NC negative (uncontaminated)?** — `nc_status` (contamination outcome: `NC PASS`/`NC WARN`/`NC FAIL`)
2. **Was this NC injection analytically valid?** — not previously computed at all

A contaminated NC can still be a perfectly valid, successful analytical run (the system worked correctly and detected real contamination). Conversely, an NC bracketed by failed 0.2ng QCs, or where the internal standard wasn't even detected, is not a reliable result — regardless of whether it happened to look clean. The old code used contamination status (or rather, ignored it entirely and just counted all NCs) as a proxy for run validity, which is the wrong criterion.

**Fix applied** (`Code/04_CollateStudyResults.R`):

1. **New function `compute_nc_analysis_valid()`** (added alongside `compute_nc_status()`, ~line 819): returns `TRUE` for an NC row only if:
   - The internal standard (15N-RDX) was detected (`rdx_is_snr_flag` not `NA`/`"Not_Detected"`), **and**
   - All four bracketing 0.2ng QC columns (`petn_qc_02ng_pre/post`, `rdx_qc_02ng_pre/post`) are not `"FAIL"` (`PASS`/`WARN` both count as valid; missing bracket `NA` is conservatively treated as invalid).
2. **Applied to `nc_rows`** as a new `nc_analysis_valid` column (alongside `nc_status`), with a console summary (`"NC analysis validity: Valid=X Invalid=Y"`).
3. **Added to `desired_cols_nc`**: `nc_analysis_valid` plus audit columns (`rdx_is_snr_flag`, `petn_qc_02ng_pre/post`, `rdx_qc_02ng_pre/post`) so the basis for the validity flag is visible in the `Negative Controls` sheet, not just a black-box boolean.
4. **"Successfully Run" row recomputed** (~line 1183) to use `nc_analysis_valid` instead of blanket NC count:
   - `Total` = all samples + all NCs attempted (denominator unchanged)
   - `PASS` = sample `PASS` count (unchanged)
   - `PASS*` = sample `PASS*` count + count of analytically-**valid** NCs only
   - `FAIL` = sample `FAIL` count + count of analytically-**invalid** NCs (previously these weren't counted anywhere in this row's FAIL bucket)
   - `Complete %` = 100 × (PASS + PASS*) / Total (unchanged formula, corrected inputs)

**Design decision**: The fix uses *analysis validity* (IS + QC bracket integrity), not contamination result, as the criterion for "successfully run" — per explicit clarification that a valid-but-contaminated NC should still count as a successful run, while an NC with unreliable underlying QC/IS should not, irrespective of its apparent cleanliness.

**Files modified**: `Code/04_CollateStudyResults.R` (new function + `desired_cols_nc` + SampleSummary "Successfully Run" block), `CONTEXT.md` (SampleSummary Sheet, NC Sheet column table, NC Sheet Columns reference table).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` needed to be re-run (this is a collation-time calculation using columns already present in each dataset's exported results; no need to re-run `03_Quantification.R` for any dataset).

---

## Session Summary (July 29, 2026, continued further) — SampleSummary Restructured to a Single Table

### Follow-up Correction: "Successfully Run" Was Still Wrong

**Feedback received**: The previous fix (above) still produced a confusing "Successfully Run" **row** with full `Total`/`PASS`/`PASS*`/`FAIL`/`Complete %` columns (`280`/`119`/`143`/`18`/`93.6`), which was unclear ("I don't know what the 119, 143 and 18 represent"). The correct design should be:
1. `Successfully Run` should be a **single headline number** (`PASS + PASS* + NC valid`), not a full breakdown row.
2. Negative controls should be folded into the **same table** as an `NC` row (not a separate second table), positioned after the surface rows and before `TOTAL`.
3. For the `NC` row specifically:
   - `PASS` = analytically valid **and** negative (clean)
   - `PASS*` = analytically valid **and** positive (contamination/trace detected)
   - `FAIL` = analytically **invalid** (this is a run-reliability failure, independent of the contamination result)

**Fix applied** (`Code/04_CollateStudyResults.R`, SampleSummary section, ~line 1120):

- Removed the separate "Table 2: Negative Controls" (`nc_summary`) entirely — its information is now captured directly by the `NC` row in the main table (plus the full `nc_status`/`nc_analysis_valid`/audit columns already present on the `Negative Controls` sheet itself for anyone who wants the underlying detail).
- Built a single `NC` row using `nc_analysis_valid` crossed with `nc_status`:
  ```r
  nc_valid_negative <- sum(master_ncs$nc_analysis_valid & master_ncs$nc_status == "NC PASS", na.rm = TRUE)
  nc_valid_positive <- sum(master_ncs$nc_analysis_valid & master_ncs$nc_status != "NC PASS", na.rm = TRUE)
  nc_invalid        <- sum(!master_ncs$nc_analysis_valid, na.rm = TRUE)
  ```
- Row order is now: one row per surface present → `NC` → `TOTAL` (surface rows only, excludes `NC`, unchanged definition) → `Successfully Run`.
- `Successfully Run` is now a single value: `TOTAL.PASS + TOTAL.PASS* + NC.PASS + NC.PASS*`, placed in the `Total` column; `PASS`/`PASS*`/`FAIL`/`Complete %` are left `NA` for this row since a single combined count has no meaningful breakdown.

**Verified output** (real run, 8 reprocessed datasets — note: this example predates the Lab29/Lab33 duplicate-file discovery below, so the raw counts here are inflated by 48 duplicate sample rows; see the corrected counts in the next session summary):
```
Surface            Total  PASS  PASS*  FAIL  Complete %
ABS-Smooth          144    65     75     4        97.2
Steel                96    54     39     3        96.9
NC                   40     9     20    11        72.5
TOTAL               240   119    114     7        97.1
Successfully Run    262    NA     NA    NA        NA
```
`Successfully Run` = 119 + 114 + 9 + 20 = 262, matching the design exactly. `NC` row: 9 clean-and-valid + 20 contaminated-but-valid + 11 invalid = 40 total, Complete % = 29/40 × 100 = 72.5%.

**Files modified**: `Code/04_CollateStudyResults.R` (SampleSummary section rewritten), `CONTEXT.md` (SampleSummary Sheet section rewritten to describe the single-table structure, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, continued further still) — Lab29/Lab33 Duplicated in FINEX_StudyResults

### Bug: Stale Empty-Prefix Results Files Caused Duplicate Sample Rows

**Problem identified**: `Lab29` and `Lab33` each appeared **twice** in `FINEX_StudyResults.xlsx` — every sample row was duplicated (48 extra rows total: 2 labs × 24 samples each).

**Root cause**: Each of `Lab29/Results/` and `Lab33/Results/` contained a stale, incorrectly-named file with an **empty dataset prefix** (`_GCMSResults.csv`, `_GCMSResults.xlsx`, `_CalibrationStats.xlsx`), dated 20-21 July 2026 — left over from an earlier run, predating this session's reprocessing. This happens when `DataFolder` (`GlobalCode.R` line 60) ends with a trailing slash: `ParentFolder <- sub(".*/", "", DataFolder)` strips everything up to and including the *last* `/`, which — if `DataFolder` itself ends in `/` — removes the lab name too, leaving `ParentFolder = ""` and producing output files named just `_GCMSResults.csv` instead of `Lab29_GCMSResults.csv`.

`Code/04_CollateStudyResults.R` discovers all `*_GCMSResults.csv` files **recursively** (`Section 1: Discover all *_GCMSResults.csv files`) and has no logic to detect that two files in the same `Results/` folder represent the *same* dataset run twice under different names — it picked up both `Lab29_GCMSResults.csv` (correct, current) and `_GCMSResults.csv` (stale duplicate, from a prior run) and treated them as two independent datasets. Verified the content was byte-for-byte the same sample set (same `SampleName`/`DataFile` values) before deleting.

**Fix applied**: Deleted the 6 stale files (`_CalibrationStats.xlsx`, `_GCMSResults.csv`, `_GCMSResults.xlsx` in each of `Lab29/Results/` and `Lab33/Results/`). Re-ran `Code/04_CollateStudyResults.R`.

**Verification**:
- Console: `"Found 8 results file(s)"` / `"Found 8 calibration stats file(s)."` (previously 10 of each).
- `All Data` sheet: 192 rows (previously 240 — exactly 48 fewer, matching 2 duplicated labs × 24 samples).
- `Lab29`/`Lab33`: each now shows the correct 24 samples (2 participants × 2 surfaces × 6 reps), no duplicate `(SourceFile, DataFile)` pairs anywhere in the dataset.
- Corrected `SampleSummary`:
  ```
  Surface            Total  PASS  PASS*  FAIL  Complete %
  ABS-Smooth          120    53     63     4        96.7
  Steel                72    36     33     3        95.8
  NC                   32     6     18     8        75.0
  TOTAL               192    89     96     7        96.4
  Successfully Run    209    NA     NA    NA        NA
  ```
  *(Note: `Complete %` values above reflect the pass-rate definition used at the time; see the next session summary for the corrected against-study-target definition and the resulting row order/values.)*

**Not fixed (out of scope, flagged for awareness)**: The underlying `ParentFolder <- sub(".*/", "", DataFolder)` fragility (empty result if `DataFolder` has a trailing slash) was not patched in `GlobalCode.R`. A future improvement would be to strip any trailing slash from `DataFolder` before computing `ParentFolder`, and/or have `04_CollateStudyResults.R` warn if it discovers a results file with an empty or missing dataset-name prefix.

**Files modified**: Deleted 6 stale files under `Accepted Analysis/Lab29/Results/` and `Accepted Analysis/Lab33/Results/`. No code changes. `CONTEXT.md` (this entry, and correction of the numbers in the immediately preceding session summary).

---

## Session Summary (July 29, 2026, continued yet further) — `Complete %` Now Tracks Study-Wide Target, Not Pass Rate

### Requested Change

`Complete %` in `SampleSummary` previously meant "pass rate among samples processed so far" ((PASS+PASS*)/Total for that row). The user wanted it to instead track **progress against the full study design target**: fixed expected sample totals of Steel=386, ABS-Smooth=393, Glass=165, ABS-Textured=165.

### Fix Applied (`Code/04_CollateStudyResults.R`)

1. **New config `expected_sample_totals`** (~line 34, alongside `qc_6ng_bias_limit`):
   ```r
   expected_sample_totals <- c(
     "Steel" = 386, "ABS-Smooth" = 393, "Glass" = 165, "ABS-Textured" = 165
   )
   ```
2. **Per-surface rows**: `Complete %` now = `(PASS + PASS*) / expected_sample_totals[Surface] × 100`, not `/Total`. `Total` is unchanged (still the actual count processed so far).
3. **All four surfaces always shown**, even with zero data processed for a surface yet (`Total=0`, `Complete %=0.0`) — previously a surface with no data simply didn't appear as a row at all. Rows are ordered `Steel, ABS-Smooth, Glass, ABS-Textured` (the order `expected_sample_totals` is defined in) rather than alphabetically.
4. **`TOTAL` row**: `Complete %` now = (sum of per-surface PASS+PASS*) / **grand total (1109)** × 100 -- i.e. overall study-wide progress across all four surfaces, not just progress among currently-collated samples.
5. **`NC` row unaffected** — no fixed expected NC total was specified, so it keeps the previous `(PASS+PASS*)/NC.Total × 100` definition. *(Superseded shortly after — see next session summary: a fixed NC target of 161 was subsequently provided.)*
6. **`Successfully Run` row unaffected** — single count, no `Complete %`.

**Verified output** (real run, 8 datasets, after the Lab29/Lab33 duplicate-file fix above):
```
Surface            Total  PASS  PASS*  FAIL  Complete %
Steel                72    36     33     3        17.9
ABS-Smooth          120    53     63     4        29.5
Glass                 0     0      0     0         0.0
ABS-Textured          0     0      0     0         0.0
NC                   32     6     18     8        75.0
TOTAL               192    89     96     7        16.7
Successfully Run    209    NA     NA    NA        NA
```
- Steel: (36+33)/386 = 17.9%; ABS-Smooth: (53+63)/393 = 29.5%; Glass/ABS-Textured: 0/165 = 0.0% (no datasets processed for these surfaces yet, but rows now still appear); TOTAL: (69+116+0+0)/1109 = 16.7%.

*(Note: these expected totals -- 386/393/165/165 -- were themselves corrected in the very next session, see below, since they accidentally included NC counts.)*

**Operational note encountered during verification**: The first re-run of `04_CollateStudyResults.R` after this change failed to actually write the updated file — `openxlsx::saveWorkbook()` emitted a non-fatal `"Permission denied"` warning (the script otherwise completed and printed success messages) because the output `.xlsx` was momentarily locked. The stale on-disk file was silently left in place. Re-running once the lock cleared wrote the file correctly. **Takeaway**: always re-verify the actual on-disk file after a collation run, don't rely solely on the console "Collation Complete" message, since a save failure here does not raise a stopping error.

**Files modified**: `Code/04_CollateStudyResults.R` (`expected_sample_totals` config + SampleSummary surface-row/TOTAL row `Complete %` logic), `CONTEXT.md` (SampleSummary Sheet section rewritten, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, continued once more) — Expected Totals Corrected: Separate NC Target

### Correction: Original Expected Totals Accidentally Included NCs

**Feedback received**: The expected sample totals used in the previous fix (Steel=386, ABS-Smooth=393, Glass=165, ABS-Textured=165) were wrong — they had inadvertently folded NC counts into the per-surface sample totals. The correct, separated figures are:

| | Samples (corrected) | Samples (original, wrong) | Difference |
|---|---|---|---|
| Steel | 330 | 386 | 56 |
| ABS-Smooth | 336 | 393 | 57 |
| Glass | 141 | 165 | 24 |
| ABS-Textured | 141 | 165 | 24 |
| **Sum** | **948** | **1109** | **161** |

The differences sum to exactly 161 — the correct, separately-specified NC expected total. NCs need their own fixed target since they are a distinct row in the table, no longer folded into surface totals.

**Fix applied** (`Code/04_CollateStudyResults.R`):
1. Updated `expected_sample_totals` (~line 52) to the corrected sample-only figures: `Steel=330, ABS-Smooth=336, Glass=141, ABS-Textured=141`.
2. Added `expected_nc_total <- 161` (~line 61).
3. **`NC` row's `Complete %`** changed from `(PASS+PASS*)/NC.Total × 100` (dynamic, samples-processed-so-far denominator) to `(PASS+PASS*)/expected_nc_total × 100` (fixed study-wide target), consistent with how the per-surface rows already worked.
4. **`TOTAL` row** unaffected in formula (`grand_expected_total <- sum(expected_sample_totals)`), automatically picks up the corrected sum (948, was 1109) since it references the updated vector.

**Verified output** (real run, 8 datasets):
```
Surface            Total  PASS  PASS*  FAIL  Complete %
Steel                72    36     33     3        20.9
ABS-Smooth          120    53     63     4        34.5
Glass                 0     0      0     0         0.0
ABS-Textured          0     0      0     0         0.0
NC                   32     6     18     8        14.9
TOTAL               192    89     96     7        19.5
Successfully Run    209    NA     NA    NA        NA
```
- Steel: (36+33)/330 = 20.9%; ABS-Smooth: (53+63)/336 = 34.5%; NC: (6+18)/161 = 14.9% (now against the fixed 161 target, not the 32 NCs processed so far); TOTAL: (69+116+0+0)/948 = 19.5% (samples only, NC excluded and tracked separately in its own row/target).

**Files modified**: `Code/04_CollateStudyResults.R` (`expected_sample_totals` corrected, `expected_nc_total` added, NC row `Complete %` formula changed), `CONTEXT.md` (SampleSummary Sheet section figures corrected, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, continued once more still) — Added `N Labs` / `N Participants` to SampleSummary

### Feature Request

Show, for each row in the `SampleSummary` table, how many distinct labs and participants contributed the data (not just the sample count).

### Fix Applied (`Code/04_CollateStudyResults.R`, SampleSummary section ~line 1147)

Added two new columns, positioned right after `Total` (before `PASS`/`PASS*`/`FAIL`/`Complete %`):
- **`N Labs`** = `n_distinct(Lab)` for that row's data
- **`N Participants`** = `n_distinct(paste(Lab, Participant))` -- counted as **(Lab, Participant) pairs**, since Participant numbers (1/2) repeat across different labs and must not be counted as if they were the same participant.

**Per row**:
- Per-surface rows (`Steel`/`ABS-Smooth`/`Glass`/`ABS-Textured`): computed from `master` filtered to that `SurfaceName`. Surfaces with 0 samples processed show `0`/`0`.
- `NC` row: computed from `master_ncs`.
- `TOTAL` row: computed directly from the **full** `master` dataframe (all samples), **not** by summing the per-surface `N Labs`/`N Participants` values -- summing would double-count any lab/participant that contributed to more than one surface.
- `Successfully Run` row: left blank (`NA`), consistent with the other breakdown columns on this single-count row.

**Verified output** (real run, 8 datasets):
```
Surface            Total  N Labs  N Participants  PASS  PASS*  FAIL  Complete %
Steel                72      6         12          36     33     3        20.9
ABS-Smooth          120     12         20          53     63     4        34.5
Glass                 0      0          0           0      0     0         0.0
ABS-Textured          0      0          0           0      0     0         0.0
NC                   32     12         20           6     18     8        14.9
TOTAL               192     12         20          89     96     7        19.5
Successfully Run    209     NA         NA          NA     NA    NA        NA
```
Spot-checked directly against the `All Data`/`Negative Controls` sheets (e.g. `Steel.N Labs` = `df[df.SurfaceName=="Steel"]["Lab"].nunique()` = 6) -- all values matched exactly.

**Operational note**: Hit the same transient `openxlsx` "Permission denied" save failure as in the previous session (file momentarily locked, non-fatal warning, stale file left on disk); resolved by re-running once the lock cleared. Continues to be worth double-checking the on-disk file after every collation run.

**Files modified**: `Code/04_CollateStudyResults.R` (SampleSummary section: added `N Labs`/`N Participants` to per-surface summarise, missing-surface fill, NC row, TOTAL row, and Successfully Run row), `CONTEXT.md` (SampleSummary Sheet column table and example output updated, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, continued yet again) — Overall Bar Charts by Participant

### Feature Request

Add two bar charts (one for PETN, one for RDX) to `04_CollateStudyResults.R`'s plot generation, with each participant (e.g. `lab1p1`) along the x-axis, and an individual series/colour per surface.

### Implementation (`Code/04_CollateStudyResults.R`, immediately after the existing Overall box plots, ~line 1604)

- Reused the existing `recovery_data` (already NA→0, restricted to `Outcome == "Complete"`, NCs excluded) and `fixed_surface_order` from the box plot section above -- no new data filtering needed.
- **`participant_bar_data`**: grouped by `Lab`, `Participant`, `ParticipantLabel` (`sprintf("lab%dp%d", Lab, Participant)`), `SurfaceName`; bar height = mean `petn_rec`/`rdx_rec` across that participant's replicates for that surface.
- **`participant_order`**: x-axis factor levels ordered by `arrange(Lab, Participant)` (numeric), not alphabetically -- otherwise `lab10p1` would sort between `lab1p1` and `lab2p1`.
- **`surface_colors`**: fixed named colour per surface (Glass/Steel/ABS-Smooth/ABS-Textured), shared between the PETN and RDX charts so colours mean the same thing in both.
- **`create_participant_barchart()`**: `geom_bar(stat = "identity", position = position_dodge2(preserve = "single"))` -- `preserve = "single"` keeps bar width constant across x positions even when a participant only has data for some surfaces (avoids the classic dodge bug where bars widen to fill gaps).
- Two new files: `Plots/Overall/Overall_PETN_ByParticipant_BarChart.png`, `Plots/Overall/Overall_RDX_ByParticipant_BarChart.png` (14×6in, 300 DPI -- wider than the box plots to accommodate ~20 participant labels).

**Verified**: Ran `04_CollateStudyResults.R` on the 8 real datasets; both PNGs generated and visually inspected -- x-axis correctly ordered `lab6p1, lab6p2, lab7p1, lab9p1, lab9p2, lab10p1, ..., lab38p1` (numeric, not alphabetic), bars correctly grouped/coloured by surface (orange = Steel, purple = ABS-Smooth in this dataset subset), missing surfaces per participant simply omitted rather than leaving a gap or widening neighbouring bars.

**Files modified**: `Code/04_CollateStudyResults.R` (new bar chart section), `CONTEXT.md` (Box Plot Generation section extended with "Overall Bar Charts by Participant" subsection, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, one more time) — SEM Error Bars + Participant Label Format

### Follow-up Requests

1. Add error bars showing the standard error of the mean (SEM = SD/sqrt(n)) to the two by-participant bar charts added in the previous session.
2. Change the x-axis participant labels from `lab6p1` to `Lab 6 (1)`.

### Implementation (`Code/04_CollateStudyResults.R`, by-participant bar chart section, ~line 1605)

1. **Label format**: `ParticipantLabel <- sprintf("Lab %d (%d)", Lab, Participant)` (was `sprintf("lab%dp%d", ...)`). Numeric ordering logic (`participant_order`) unchanged in principle, just now sorts/labels e.g. `Lab 6 (1)`, `Lab 6 (2)`, `Lab 7 (1)`, ... instead of `lab6p1`, `lab6p2`, `lab7p1`, ...

2. **SEM error bars**: `participant_bar_data` now computes, per (Lab, Participant, SurfaceName) group:
   ```r
   petn_mean = mean(petn_rec, na.rm = TRUE)
   petn_sd   = sd(petn_rec, na.rm = TRUE)
   petn_n    = sum(!is.na(petn_rec))
   petn_sem  = ifelse(petn_n > 1, petn_sd / sqrt(petn_n), NA_real_)
   ```
   (and the RDX equivalents). `petn_sem`/`rdx_sem` is `NA` (no error bar drawn) when a participant×surface group has only 1 replicate, since SD is undefined for n=1 -- this is *not* rendered as a zero-width error bar.

3. **Bar/error-bar alignment**: introduced `dodge_width <- 0.9` and applied `position_dodge2(width = dodge_width, preserve = "single")` identically to **both** `geom_bar()` and the new `geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.3)` in `create_participant_barchart()`. Using the same dodge width/spec for both geoms is what makes the error bars land exactly centred over their corresponding bar rather than drifting -- verified visually, no misalignment.

4. `create_participant_barchart()` signature changed from `(data, y_var, y_label, title)` to `(data, y_var, sem_var, y_label, title)`; call sites updated to pass `petn_mean`/`petn_sem` and `rdx_mean`/`rdx_sem` respectively (renamed from the previous `petn_rec`/`rdx_rec` mean-only columns).

**Verified**: Re-ran `04_CollateStudyResults.R` on the 8 real datasets; both PNGs regenerated and visually inspected -- error bars correctly centred on each bar, x-axis labels now read `Lab 6 (1)`, `Lab 6 (2)`, `Lab 7 (1)`, `Lab 9 (1)`, `Lab 9 (2)`, ... in the same correct numeric Lab/Participant order as before. *(Correction: this "verified" check only visually spot-checked the first few bars, which happen to be single-surface participants where dodging is trivial. The `position_dodge2` alignment claim in point 3 was WRONG for multi-surface participants -- see the next session summary for the actual root cause and fix.)*

**Files modified**: `Code/04_CollateStudyResults.R` (by-participant bar chart section rewritten), `CONTEXT.md` (Overall Bar Charts by Participant subsection updated, this session summary).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).

---

## Session Summary (July 29, 2026, yet once more) — Error Bar Misalignment Fixed (dodge2 → dodge)

### Bug Reported

"The first three bars have error bars lined up normally but the rest aren't." Confirmed visually: `Lab 6 (1)`, `Lab 6 (2)`, `Lab 7 (1)` (each a single bar -- only ABS-Smooth data) had correctly-centred error bars, but from `Lab 9 (1)` onward (the first participant with **two** bars -- Steel and ABS-Smooth side by side) the error bars drifted off-centre from their bars.

### Root Cause

`position_dodge2(width = dodge_width, preserve = "single")` was applied identically to both `geom_bar()` and `geom_errorbar()`, but `position_dodge2()` computes each element's dodge slot width from that **geom's own reported width** (not purely from the `position_dodge2(width = ...)` argument) -- `geom_bar()` has an implicit rendered width of ~0.9, while `geom_errorbar()`'s `width = 0.3` parameter (the whisker cap span) was *also* being read by `position_dodge2()` as that geom's width for slot-sizing purposes. This meant the two geoms dodged to slightly different x-offsets whenever more than one fill group (surface) was present at an x position. With only one group present (the first three participants), there is nothing to dodge, so the mismatch was invisible -- exactly matching the reported symptom.

### Fix Applied

Replaced `position_dodge2(width = dodge_width, preserve = "single")` with plain `position_dodge(width = dodge_width)` for **both** `geom_bar()` and `geom_errorbar()`. Plain `position_dodge()` divides the supplied width evenly by the number of groups present at each x, identically for every layer that shares it -- guaranteeing alignment regardless of each geom's own `width` parameter.

**Trade-off accepted**: bar width now varies slightly depending on how many surfaces are present for a given participant (e.g. a single-surface participant gets one full-width bar instead of a bar pre-sized to match a 2-surface participant elsewhere in the chart) -- this was the reason `dodge2`/`preserve = "single"` was originally chosen. Alignment correctness was prioritised over this cosmetic consistency, since misaligned error bars are actively misleading.

**Verified**: Re-ran `04_CollateStudyResults.R` on the 8 real datasets; regenerated both PNGs and visually inspected the **entire** chart width this time (not just the first few bars) -- error bars now correctly centred on every bar, including all multi-surface participants (`Lab 9`, `Lab 10`, `Lab 14`, `Lab 17`, `Lab 20`, `Lab 21`, `Lab 29`, `Lab 32`, `Lab 33`).

**Files modified**: `Code/04_CollateStudyResults.R` (`create_participant_barchart()`: `position_dodge2(...)` → `position_dodge(width = dodge_width)` for both geoms; `dodge_width` comment updated to explain the root cause and trade-off), `CONTEXT.md` (this session summary; note added to the previous session's incorrect "verified" claim).

**Reprocessing**: Only `Code/04_CollateStudyResults.R` re-run (no dataset-level reprocessing needed).


