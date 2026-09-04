# Design of Experiments: Trace Explosives Recovery

## Session Summary (September 3, 2026) — Thesis Colour Palette Extended to Every Subproject in the Repo (GCMSQuantitation, UVSwabbing, Data Analysis, Extraction)

### Motivation

Direct continuation of the previous session's ASTRA-only colour palette work (Okabe-Ito qualitative palette + viridis for ordinal/high-cardinality data, `ASTRA/thesis_palette.R`). User asked for the same palette extended to **every** plotting script across the whole `TransferPersistenceRecoveryExplosives` repo, not just ASTRA. Explicitly asked for a staged plan before any changes, then for a full audit of every remaining subproject before any edits.

### Scope decided with the user before editing

- `ExplosivesInterpolScopusSearch`/`SamplingInterpolScopusSearch` (literature-review bibliometric figures) explicitly excluded from scope -- user confirmed these don't need to change.
- `Sartorius Balance`, `Reports`, `Solution Preparation MU` confirmed (by direct inspection, not just grep) to contain **no plotting code at all** -- out of scope, nothing to do.
- Remaining in scope: `GCMSQuantitation` (24 live plotting files), `UVSwabbing` (8), `Data Analysis/Chapter 3/ASTRA` (2), `Extraction` (2) -- 36 files total, audited in full (via parallel sub-agent exploration) before any edits, per the user's explicit request.
- Cross-chapter colour-reuse policy agreed with the user: **per-study/chapter uniqueness only** -- a colour may mean something different in an unrelated chapter/study (there are only 8 Okabe-Ito hues and dozens of distinct factors thesis-wide; full global uniqueness is not achievable), but never two different things within the same study's own closely-related figures (the original problem this whole effort started from).
- High-cardinality factors (>8 levels: `Lab`, `Dataset`, `Sample`) and genuinely-ordered factors (`Pressure` High/Normal/Low, `Concentration` 50/25/10%, `CalibrationSet`, extraction-step index `Ext 1-5`) -- confirmed with the user to use `scale_*_viridis_*()` rather than a qualitative palette, consistent with the file's own stated policy.
- Confirmed with the user to also fix genuine cross-file colour inconsistencies found during the audit (not just add colours where missing).

### `thesis_palette.R` relocated to the repo root

Moved from `ASTRA/thesis_palette.R` to `TransferPersistenceRecoveryExplosives/thesis_palette.R` (deleted the old ASTRA-local copy) so every subproject sources the exact same one file. Updated all 4 ASTRA scripts that already `source()`'d the old path (`main_study_analysis.R`, `pilot_analysis.R`, `plot_recovery_boxplot_actualpressure.R`, `plot_recovery_vs_pressure_unified.R`) to point at the new root location; updated the code-comment references (not `source()` calls, per the file's own "no source()" self-contained design rule) in `main_study_batch_process.R`/`time_based_batch_process_adaptive_DISTANCE.R` to say "repo-root thesis_palette.R" instead of "ASTRA/thesis_palette.R".

Added ~20 new `pal_*` vectors to the file, organised into clearly-labelled `SECTION:` blocks per subproject (GCMSQuantitation, UVSwabbing, Data Analysis, Extraction), each documenting which real file(s)/factor(s) it covers and any label-text aliasing needed (several scripts used slightly different literal strings, including embedded-newline variants, for what turned out to be the exact same underlying category -- e.g. `"Current(Ratio Poly)"`/`"Current (Ratio Poly)"`/`"Current Polynomial"`/`"Current"` all alias to the same colour in the new `pal_method`).

**GCMSQuantitation** (full detail in that repo's own `CONTEXT.md`): `pal_surface_gcms`, `pal_injection_role`, `pal_method`, `pal_analyte_vs_is`, `pal_significant`, `pal_sample_role`, `pal_ion_interpretation`, `pal_ion_candidate`, `pal_ion_classification`, `pal_method_group`, `pal_sequence_flag`, `pal_trace_baseline`. Applied across `GlobalCode.R` + 20 of 24 live files (2 had no colour-mapped factors at all; 1 already used Brewer Dark2 consistently and was left alone -- no inconsistency to fix).

**UVSwabbing** (full detail in that repo's own `CONTEXT.md`): `pal_image_stage`, `pal_pattern`, `pal_condition`, `pal_surface_uv`, `pal_method_uv`, `pal_source_surface_rep`. Applied across `00-GlobalCode.R` + all 8 live files. Fixed a genuine cross-file inconsistency (Blank/Before/After using different colours in `Diagnostic_ThresholdPlots.R` vs `UV_Recovery_Reprocessing.R`) and tightened `06-RecoveryAnalysis.R`'s fragile unnamed/positional `ColourPalette2` into a proper named alias.

**Data Analysis / Extraction** (no `CONTEXT.md` of their own -- documented here instead): `pal_mount_type` (Original/Modified swab mount, `OrigModSwabMountPressure.R`), `pal_swab_mount_angle` (45°/90°/70°, `SwabMountAngles.R`) -- both previously `scale_colour_brewer(palette="Set1")`, now the shared palette. `pal_test_std` (3-level test-standard condition) and `pal_swab` (Swab 1/2/3) in `Extraction/InitialExtractionTest.R`/`PlotExtractionRecovery.R`; the latter's `Extraction` (Ext 1-5) sequential index switched to `scale_fill_viridis_d(direction = -1)` -- `direction = -1` specifically because the underlying factor's own levels are deliberately reversed (`levels = 5:1`, for stacking order) and viridis's default direction would otherwise have come out backwards (Ext5 dark/Ext1 light) relative to the original dark=Ext1/light=Ext5 design.

### Two more pre-existing bugs found and fixed (both were blocking verification of this session's own work, both trivial)

- `Data Analysis/Chapter 3/ASTRA/SwabMountAngles.R`: line referenced an undefined `orig_data_avg` (a copy-paste artefact from the sibling `OrigModSwabMountPressure.R` file) which crashed the script *before it ever reached the plotting code* -- meaning this file's own colour change could never have been verified by execution otherwise. Fixed to reference the correct `data45_avg`.
- `UVSwabbing/UV_Recovery_Reprocessing.R`: `Diagnostic_RawCurves.png`'s own subtitle caption ("Blue=Blank, Red=Before swab, Green=After swab") would have gone stale/wrong the moment the underlying colours changed -- caught only by actually rendering the plot and comparing the legend against the caption text, not by reading the code alone. Fixed to match the new colours (see also the `PETN_pilot_interactions`-style viridis-direction catch from the previous ASTRA-only session -- same category of bug: a change that's correct in isolation but makes an existing caption/comment describing the OLD behaviour silently wrong).

One more pre-existing bug found but deliberately left UNFIXED (out of scope, disclosed instead): `UVSwabbing/Diagnostic_ThresholdPlots.R` contains `cat("=".join(rep("=", 60)), ...)` -- Python `.join()` syntax, invalid R -- which fails to parse and blocks the entire script (including its own plots) from ever running, regardless of this session's changes.

### Verification

All ~40 touched files across all 4 subprojects `parse()`-checked (39 pass; 1 known pre-existing unrelated failure, disclosed above). Executed end-to-end and visually confirmed correct rendering for: both Data Analysis scripts, 1 of 2 Extraction scripts (the other blocked by a missing input data file, unrelated to this change), 1 representative GCMSQuantitation diagnostic script, and 1 UVSwabbing script (which is also where the stale-caption bug above was caught). The remaining edited files were not individually executed this session -- many require specific real experimental datasets not populated in this local OneDrive checkout, and several are explicitly documented as "authoritative" scripts that reproduce already-submitted thesis results, which did not seem appropriate to re-run wholesale without checking first; correctness for those was instead verified by exact string-for-string comparison of every new `pal_*` key against the real label text/column values found in each script.

### Update (same day, continued) -- user asked to actually run everything remaining; this caught 2 real bugs a code-only review had missed

Went back and executed every remaining edited file where the underlying data was locally available (full detail in `GCMSQuantitation/CONTEXT.md` and `UVSwabbing/CONTEXT.md`, since each subproject's own dependency chains/gotchas are specific to it). Headline finding: **actually rendering the outputs caught two genuine functional bugs in this session's own edits that parse-checking and code review had both missed**:

1. `GCMSQuantitation/FINEX/05_StatisticalAnalysis.R`'s Lab forest plot: a new `scale_color_manual()` was added without noticing a **pre-existing** one further down the same plot's layer chain (positioned unusually, after `coord_flip()`) -- ggplot2 silently let the old one win, so the new colour was never actually applied despite the edit looking correct on its own. Caught only by rendering the PNG and seeing plain red instead of vermillion. This prompted a full systematic re-scan of every one of the ~30 edited files across the whole cross-project effort for this exact pattern (any two same-aesthetic scale calls within 40 lines, checked for an intervening `ggplot(` boundary) -- confirmed this was the only real instance.
2. The same file's Lab x Surface interaction plot: `scale_color_viridis_c()` (continuous) was the wrong choice for that specific plot's `Lab` column, which turned out to already be a factor there (confirmed via its use in `leveneTest(Recovery ~ Lab, ...)` immediately above) -- should have been `_d()`. The mismatch didn't error the whole script; it failed silently inside a `tryCatch`, leaving a stale/blank PNG on disk. Caught by noticing the `tryCatch`'s own printed warning ("Discrete value supplied to a continuous scale") while checking the log, not from the image itself. Both fixed, re-run, and visually re-confirmed correct.

Also fixed `UVSwabbing/Diagnostic_ThresholdPlots.R`'s previously-disclosed pre-existing Python-syntax bug (`"=".join(...)`) so it could actually be executed and its colours verified -- was left broken/unfixed in the first verification pass; fixed properly this time since the user confirmed wanting everything actually run.

Several more pre-existing, unrelated bugs were found while attempting to execute other files (data-shape mismatches, a missing upstream data-prep pipeline for UVSwabbing's numbered `04`-`07` scripts, one `select()` on a non-existent column) -- none of these were fixed, since they require actual data/logic investigation rather than a one-line typo fix; all disclosed in the relevant subproject's own `CONTEXT.md` rather than silently left unmentioned. One deliberate non-action: `GCMSQuantitation/FINEX/RunAllDatasets.R` (the only way to actually exercise `Code/02_PeakDetection.R`/`Code/03_Quantification.R`'s edited plots) was NOT run -- it reprocesses raw MS data for all 20 datasets (a documented past disk-space incident, ~10GB/dataset) and its default flags would skip already-processed datasets without even reaching the edited code anyway; judged disproportionate for a legend-colour change.

**Final execution tally across the whole cross-project effort**: GCMSQuantitation 13/21 edited files executed successfully with colours visually confirmed; UVSwabbing 2/8; Data Analysis 2/2; Extraction 1/2 (both now including the files originally left un-executed in the first pass). The remainder are either blocked by genuine pre-existing unrelated issues (disclosed, not fixed) or were judged disproportionately heavy to run for a colour-only change (disclosed, not run).

### Files Modified

`TransferPersistenceRecoveryExplosives/thesis_palette.R` (new location; ~20 new `pal_*` vectors added in per-subproject sections). `ASTRA/doe/main_study_analysis.R`, `ASTRA/doe/pilot_analysis.R`, `ASTRA/plot_recovery_boxplot_actualpressure.R`, `ASTRA/plot_recovery_vs_pressure_unified.R` (source path updated). `ASTRA/main_study_batch_process.R`, `ASTRA/time_based_batch_process_adaptive_DISTANCE.R` (comment references updated). `GCMSQuantitation/GlobalCode.R` + 21 files, including the 2 bug fixes in `FINEX/05_StatisticalAnalysis.R` (see `GCMSQuantitation/CONTEXT.md` for the full list). `UVSwabbing/00-GlobalCode.R` + 8 files, including the `Diagnostic_ThresholdPlots.R` bug fix (see `UVSwabbing/CONTEXT.md`). `Data Analysis/Chapter 3/ASTRA/OrigModSwabMountPressure.R`, `SwabMountAngles.R` (also: `orig_data_avg` bug fix). `Extraction/InitialExtractionTest.R`, `PlotExtractionRecovery.R`. Regenerated real output files across all 4 subprojects wherever execution succeeded (see each subproject's own `CONTEXT.md` for exact file lists). `CONTEXT.md` (this entry).

---

## Session Summary (September 2, 2026, continued) — Codebase Simplification Pass

### Motivation

The ASTRA scripts had accumulated into 16 "live" files across `ASTRA/` and `ASTRA/doe/`, many of them one-off companion scripts each explicitly documented as "additional, not a replacement" of an earlier one — a real, user-flagged mess, not just a perception. Went through every live script, asked the user what's actually still needed vs safe to retire/consolidate, and acted only on confirmed answers rather than guessing.

### 1. Experimental design scripts moved to `ASTRA/doe/ExperimentalDesign/`

`doe_explosives_recovery.R`, `pilot_study_design.R`, `pilot_design_batch_reorganizer.R`, `doe_nested_design.R` — all four confirmed (by reading their own headers) to be one-time design/randomisation/power-analysis generators, not part of the live analysis pipeline, and confirmed via `grep` that nothing else `source()`s them. User wants them kept as a methods-section record, not deleted. Moved, along with the design-output files that are **not** read by any live script (`design_matrix.csv`, `design_matrix_blocked.csv`, `main_study_negative_controls.csv`, `main_study_batch_checklist.csv`, `main_study_protocol.txt`, `batch_protocol.md`, `batch_timing_checklist.csv`, `power_sensitivity_summary.csv`, `power_analysis.png`, `pilot_design_batch_organized_original.csv`).

**Deliberately left in place in `ASTRA/doe/`** (not moved, despite also being design outputs): `pilot_design_nested.csv`, `pilot_design_batch_organized.csv`, `main_study_design_nested.csv`, `main_study_surface_use_tally.csv` — confirmed these are still read directly, every run, by `pilot_analysis.R`/`main_study_analysis.R`'s hardcoded paths. Moving them would require updating those paths in the live pipeline for no real benefit; see the new `ExperimentalDesign/README.md` for the full explanation of this split, so it doesn't look like an oversight later.

### 2. Three near-identical "unified pressure fit" scripts consolidated into one

`plot_recovery_vs_pressure_unified.R` (linear), `_quadratic.R`, `_logarithmic.R` in `ASTRA/` each independently loaded and joined the exact same Pilot+Main actual-pressure dataset, differing only in which curve shape was fit — confirmed by reading all three in full. User confirmed: consolidate. `plot_recovery_vs_pressure_unified.R` now does the data loading/joining and the AIC/BIC 4-model comparison (Linear/Quadratic/Logarithmic/Asymptotic — this part already existed in the logarithmic script) **once**, then produces all three existing named output figures in one run. Verified: re-ran end-to-end, exit code 0, all three PNGs regenerated with identical content to before. `_quadratic.R`/`_logarithmic.R` deleted (their functionality is now fully inside the consolidated script — nothing lost).

### 3. `rdx_batch_effect_with_main_study.R` — confirmed still open, left untouched

User confirmed this investigation (RDX recovery vs calendar date/batch) is still open, not concluded — kept live, no changes.

### 4. Pressure-model primary-analysis question — not yet decided, needs the categorical-vs-quadratic tradeoff explained first

User asked for `main_study_analysis_categorical_pressure.R` (built same day, see previous session entry) to be explained in plain terms before deciding whether it, the existing quadratic model, or the actual-pressure quadratic variant should be treated as the single primary reported model (vs keeping several as parallel analyses). Not yet resolved — explained to the user directly in conversation, decision pending.

### Files Modified

Moved 4 scripts + 10 data/doc files into new `ASTRA/doe/ExperimentalDesign/` (with a new `README.md` explaining the design-vs-live-pipeline file split). Rewrote `ASTRA/plot_recovery_vs_pressure_unified.R` (consolidated), deleted `ASTRA/plot_recovery_vs_pressure_unified_quadratic.R` and `_logarithmic.R`. `CONTEXT.md` (this entry).

### 5. Decision: categorical pressure made primary; quadratic fit retired to Archive/

User's decision after the categorical-vs-quadratic tradeoff was explained: **categorical is now the primary model in `main_study_analysis.R` itself** (not a separate companion script — folding it into the main script directly, per the earlier feedback that a standalone companion script was the wrong call for what's really a refinement of the same existing model, not a genuinely separate analysis).

**Archived** (new `ASTRA/doe/Archive/` folder):
- `main_study_analysis_QUADRATIC_VERSION_2026-09-02.R` — a full snapshot of `main_study_analysis.R` taken immediately before this edit, preserving the retired quadratic combined model for reference/reproducibility.
- `main_study_analysis_actual_pressure.R` — entirely a quadratic-model sensitivity check (actual vs nominal pressure); moot now that quadratic itself is retired.
- `main_study_analysis_categorical_pressure.R` — the standalone companion script from earlier today; fully superseded by folding its logic into `main_study_analysis.R` directly (see below).

**`main_study_analysis.R` itself restructured** (Section 5 rewritten, not a new file):
- `fit_combined_model()`: formula changed from `Surface_type * (Pressure_g + I(Pressure_g^2)) + Study + (1|SurfaceID_nested)` to `Surface_type * Pressure_f + Study + (1|SurfaceID_nested)` (`Pressure_f` = `Pressure_g` as a 5-level factor, added to `combined_data` in `build_pooled_dataset()`). Now also runs `emmeans()` pairwise (Tukey-adjusted) contrasts between every pair of pressure levels, within each `Surface_type` — the direct test of "is 100g different from 200g" that motivated this whole change.
- New `fit_surface_subset_model()` (Section 5b): fits Steel-only and ABS-only subset models, answering "does ABS show a pressure effect on its own?" directly — the other open question from the Aug 27 session. Called from the driver code for both analytes x both surfaces.
- `run_power_analysis()`'s `k_map` updated: `"Pressure_g"`/`"I(Pressure_g^2)"` (2 separate terms) → single `"Pressure_f"` term; `"Surface_type:I(Pressure_g^2)"` removed (no quadratic term exists any more).
- `run_apriori_power_check()` (Section 6b) needed **no changes** — it only uses `n_pressure_levels`/overall model noise SD, not specific term names.

**Verified**: syntax check passed; full end-to-end re-run (exit code 0) reproduces the same pairwise-contrast/subset-model results already found earlier today with the standalone script (e.g. RDX-on-ABS still significant on its own, p=0.017; PETN-on-ABS still not, p=0.136), confirming the merge preserved the underlying analysis exactly. `main_study_results.xlsx` and `main_study_analysis_summary.txt` both regenerated successfully this run (Excel was closed).

### 6. Two-segment ("hinge"/broken-stick) model investigated, confirmed, and formalised into `main_study_analysis.R` alongside categorical

User's own read of the data ("rises to a point then somewhat levels off/decreases") tested directly with a broken-stick model: two linear slopes joined at a breakpoint (100g, where the categorical model's own group means already peak), via `Recovery ~ Surface_type * (Pre100 + Post100) + Study + (1|SurfaceID_nested)` (`Pre100`/`Post100` parameterised so each segment's own slope — not just the *change* in slope — gets its own coefficient/SE/p-value directly).

**Result — confirms the hypothesis precisely, and outperforms both alternatives:**
- **Fits better by AIC than both linear and the (now-archived) quadratic model**, for both analytes (e.g. PETN: Linear 423.0, Quadratic 409.7, Hinge 406.1).
- **The slope genuinely changes at 100g** (highly significant everywhere the change itself was tested, p<0.001 to p=0.03).
- **Segment 2 (100g→300g) tested directly for its own significance** (not just "different from segment 1"): Steel PETN, Steel RDX, and ABS RDX all show a **statistically significant decline** (p=0.01-0.05); ABS PETN alone is **not significant — a genuine plateau**, not a decline, consistent with that combination's already-weak overall fit.
- **R² (Nakagawa, mixed models) comparison**: the hinge model (with per-surface slopes, 7 parameters) explains almost as much variance as the fully-flexible categorical model (10 parameters) — PETN marginal R²=0.525 vs 0.540; RDX marginal R²=0.493 vs 0.483 (hinge is *better* for RDX) — good parsimony, not just a lower AIC by accident. Honest caveat carried into the script's own comments: even the best models only explain ~50% of variance via fixed effects; the rest is real sample-to-sample scatter not captured by pressure/surface/study.

**Formalised directly into `main_study_analysis.R`** (per the user's explicit instruction — both categorical AND hinge in the one final script, not a separate companion file):
- New global `PRESSURE_BREAKPOINT <- 100` (USER CONFIGURATION section).
- `build_pooled_dataset()` now also builds `Pre100`/`Post100` columns alongside `Pressure_f`.
- New `report_r2()` helper (Nakagawa marginal/conditional R², `performance::r2()`) — added to **every** model function (categorical combined, categorical subset, hinge combined, hinge subset), not just the new hinge ones, so R² is reported consistently everywhere.
- New `fit_hinge_model()` (Section 5c, combined model with `Surface_type` interaction) and `fit_hinge_surface_subset_model()` (Section 5d, per-surface slope1/slope2 with individual significance + plain-English verdict) — structured to mirror `fit_combined_model()`/`fit_surface_subset_model()` exactly.
- `performance` added to `required_packages`.
- Driver code at the bottom calls both hinge functions for both analytes, alongside the existing categorical calls.

**Verified**: syntax check passed; full end-to-end re-run (exit code 0) reproduces the investigation's exact numbers (e.g. `>>> VERDICT (PETN, abs-only, post-100g slope): NOT significant -- consistent with a PLATEAU <<<`).

### Files Modified (this sub-session)

`ASTRA/doe/main_study_analysis.R` (added `PRESSURE_BREAKPOINT`, `Pre100`/`Post100`, `report_r2()`, `fit_hinge_model()`, `fit_hinge_surface_subset_model()`, R² calls added to the existing categorical functions, `performance` package added). `CONTEXT.md` (this entry).

### 7. Main Study plot audit -- every figure now traced to exactly one live script, no orphans, no redundant overlap

User asked which plots in `Main Study/` to keep. Audited all 8 (mapping each to whichever script, if any, currently regenerates it):

- **Live, kept as-is**: `PETN_RDX_main_preliminary.png` (`main_study_analysis.R`), the 3 `Recovery_vs_ActualPressure_Unified_*.png` (`plot_recovery_vs_pressure_unified.R`, already consolidated earlier this session).
- **Orphaned, found by this audit**: `Pressure_Categorical_Means_PilotMain.png` -- only ever produced by the now-archived `main_study_analysis_categorical_pressure.R`; nothing was regenerating it any more, and it predated the hinge model entirely.
- **Redundant with each other**: `Recovery_Boxplot_ActualPressure_PilotMain.png` (`plot_recovery_boxplot_actualpressure.R`) and `Recovery_vs_ActualPressure_MainStudy_PETN/RDX.png` (`plot_main_study_recovery_vs_pressure.R`) -- both visualise "how actual measured pressure varies within each nominal level", from two separate scripts.

**Actioned (all three, per user confirmation):**
1. **New `generate_categorical_hinge_plot()` added to `main_study_analysis.R`** (Section 5e), replacing the orphaned figure -- a single figure per analyte showing BOTH primary models together: categorical group means ± SE (points/error bars, no shape assumption) with the hinge model's own fitted two-segment line overlaid (dashed, evaluated at the 5 real pressure levels -- since the hinge model is piecewise linear with a breakpoint exactly at 100g, this reproduces the true fitted curve exactly, no interpolation error). Output: `Pressure_Categorical_Hinge_PilotMain.png`. Verified visually -- the dashed hinge line tracks the categorical means closely on both analytes/surfaces, confirming the two models agree.
2. **Orphaned `Pressure_Categorical_Means_PilotMain.png` moved to `Archive/`** (alongside its already-archived source script).
3. **`plot_main_study_recovery_vs_pressure.R` archived** (kept `plot_recovery_boxplot_actualpressure.R` instead -- it packs both nominal category *and* real position into one figure, plus outlier detection, which the other script doesn't have). Its two output figures + intermediate joined-data CSV moved to `Archive/` alongside it.

**Final state**: every figure in `Main Study/` now traces to exactly one currently-live script; zero orphans; zero redundant overlap.

### Files Modified (this sub-session, continued)

`ASTRA/doe/main_study_analysis.R` (added Section 5e `generate_categorical_hinge_plot()`, called from the driver code). Moved `plot_main_study_recovery_vs_pressure.R` + its 2 PNG outputs + 1 CSV, and the orphaned `Pressure_Categorical_Means_PilotMain.png`, into `ASTRA/doe/Archive/`. `CONTEXT.md` (this entry).

### 8. User flagged `PETN_RDX_main_preliminary.png` and `Recovery_Boxplot_ActualPressure_PilotMain.png` as visually near-identical -- preliminary plot retired; the real one extended to cover the samples it was silently dropping

Compared both directly: same box+point+mean-diamond structure, same facets, but the "preliminary" one used random jitter (no outlier detection, n=118, no pressure-trace dependency) while the actual-pressure one used real achieved-pressure positioning + 1.5xIQR outlier flagging (n=114 -- 4 fewer, because it silently DROPPED any sample lacking a pressure-trace match). Confirmed the original "preliminary, works with partial/lightweight data" justification for keeping a separate plot is moot now both studies are essentially complete.

**Decision: retire the preliminary plot; fix the actual-pressure one's silent drop instead of accepting it as an unavoidable tradeoff.**

- **`generate_preliminary_plot()` removed entirely from `main_study_analysis.R`** (Section 4 -- function body and driver call both deleted, not just commented out, to avoid dead code). Header comments updated throughout to stop referencing it. The now-orphaned `PETN_RDX_main_preliminary.png` moved to `Archive/`.
- **`plot_recovery_boxplot_actualpressure.R` fixed to stop dropping samples without a pressure-trace match** (previously `filter(!is.na(Mean_Pressure), ...)` silently excluded them -- confirmed exactly 4 such points exist, all `PILOT_*` ABS @ 200g, 2 samples x 2 analytes). Root cause of the drop: the script sourced its nominal pressure level from `Target` (a column that comes from the SAME pressure-trace join as `Mean_Pressure`, so it's NA in exactly the cases that needed a nominal level to fall back on) -- switched to `Pressure_g` (each study's own recovery-data column, always present) instead.
  - Missing-pressure points are now **included**, plotted at their nominal-pressure box centre (no real position to place them at) with a distinct **hollow triangle** marker (`scale_shape_manual`), vs. filled/hollow **circles** for points with a real position -- new "Pressure position" legend.
  - Handles the (found-to-be-real) edge case of >1 missing point in the same group: a small fixed nudge (based on a running count of missing rows within the group, not raw row-number, which would have miscentered it) spreads them apart so both remain visible rather than perfectly overlapping.
  - Outlier detection, IQR/mean calculations, and `n=` labels are unaffected -- computed over ALL points in a group regardless of pressure-match status, since a missing pressure record says nothing about whether the recovery value itself is unusual.
- **Verified**: syntax checks passed for both files; `main_study_analysis.R` re-ran end-to-end (exit code 0, no "PRELIMINARY DESCRIPTIVE PLOTS" section any more); `plot_recovery_boxplot_actualpressure.R` re-ran standalone (exit code 0, console confirms "4 point(s) have NO pressure-trace match", both hollow triangles visually confirmed at the correct position in the regenerated PNG).

### Files Modified (this sub-session, continued further)

`ASTRA/doe/main_study_analysis.R` (Section 4 / `generate_preliminary_plot()` removed, header comments updated). `ASTRA/plot_recovery_boxplot_actualpressure.R` (stopped dropping pressure-trace-unmatched samples; added triangle marker + `pressure_source` legend; switched nominal-pressure source from `Target` to `Pressure_g`). Moved the now-orphaned `PETN_RDX_main_preliminary.png` into `ASTRA/doe/Archive/`. `CONTEXT.md` (this entry).

### 9. Per-panel R²/p annotations added to the hinge plot (matching the Unified plots' convention)

User noticed `Pressure_Categorical_Hinge_PilotMain.png` had no R²/p-value annotations, unlike the Unified linear/quadratic/logarithmic plots (which print `n=.../R2=.../p=...` directly on each panel). Added the same idea to `generate_categorical_hinge_plot()` (Section 5e), per Surface_type x Analyte:

- **R²**: marginal (Nakagawa, fixed effects only -- same measure as `report_r2()` elsewhere in the script), from the same per-surface hinge model already being fit for the plotted line (no duplicate model fitting).
- **p**: a likelihood-ratio test comparing the hinge model (`Pre100 + Post100 + Study`) against a no-pressure null model (`Study` only, same random-effect structure) -- refit with `REML = FALSE` for a valid LRT, as required. This is the fairest analogue to the Unified plots' own overall-model p-value ("does this curve improve on nothing"), as opposed to the segment-2-specific p-value already reported in the text summary (Section 5d) -- the two answer different questions and are both legitimate, but this plot uses the "does pressure matter at all" framing to match the Unified plots' convention specifically, per the user's request.
- Rendered as italic, colour-matched text (blue=Steel, orange=ABS) in the top-left of each Analyte panel.
- Increased plot width (8 -> 10in) and reduced subtitle font size -- the added second subtitle line (explaining what R²/p mean here) was being clipped at the original width.

**Result** (for the record): PETN Steel R²=0.52, p=9.2e-06; PETN ABS R²=0.17, p=0.065 (borderline, consistent with ABS-PETN being the weakest signal of the 4 combinations throughout this investigation); RDX Steel R²=0.34, p=0.0006; RDX ABS R²=0.40, p=0.0013.

**Verified**: syntax check passed; end-to-end re-run (exit code 0); plot visually confirmed -- all 4 annotations render correctly positioned and colour-matched, subtitle no longer clipped.

### Files Modified (this sub-session, continued once more)

`ASTRA/doe/main_study_analysis.R` (Section 5e `generate_categorical_hinge_plot()`: added per-panel R²/LRT-p annotations, widened plot, shrunk subtitle font). `CONTEXT.md` (this entry).

### 10. Individual sample points added to the hinge plot (was means-only, unlike the Unified plots' raw-scatter style)

User flagged that `Pressure_Categorical_Hinge_PilotMain.png` showed only the categorical group means +/- SE, while the Unified linear/quadratic/logarithmic plots show every individual sample as a scatter point with the fitted curve overlaid -- making the two families of plots hard to visually cross-check against each other.

**Fixed**: added a `geom_point()` layer plotting every individual sample (from `long_data`, already available in the function), jittered + dodged by `Surface_type` (`position_jitterdodge()`, fixed seed for reproducibility) so Steel/ABS points at the same nominal pressure don't overlap, drawn small and faint (`size = 1.4, alpha = 0.35`) so they sit visually behind the large mean points/error bars/hinge line, which remain the primary signal. Subtitle updated to describe both layers.

**Side benefit, not the original goal**: the individual points immediately make visible several outliers that were invisible in the means-only version (e.g. a ~38% PETN Steel point at 200g, ~33% at 300g) -- consistent with (and a nice visual confirmation of) the outliers already flagged numerically in `Recovery_Boxplot_ActualPressure_PilotMain.png`'s own console summary.

**Verified**: syntax check passed; end-to-end re-run (exit code 0); plot visually confirmed -- individual points render correctly, faint enough not to obscure the means/line, and the R²/p annotations from item 9 remain legible.

### Files Modified (this sub-session, continued yet further)

`ASTRA/doe/main_study_analysis.R` (Section 5e `generate_categorical_hinge_plot()`: added individual-sample point layer, updated subtitle). `CONTEXT.md` (this entry).

### 11. Hinge plot styling revised -- solid points (not faded), diamond means (not circles)

User feedback on item 10's fix: the individual-sample points were too faded (`alpha = 0.35`), and asked for styling consistent with the existing conventions elsewhere in the repo -- solid/visible points like the Unified (quadratic etc.) plots, and the categorical means shown as diamonds (matching `plot_recovery_boxplot_actualpressure.R`'s own black-bordered diamond mean marker).

- Individual points: `alpha` 0.35 -> 0.85, `size` 1.4 -> 2.1, explicit `shape = 19` (solid circle) -- same visual weight as `plot_recovery_vs_pressure_unified.R`'s own points.
- Categorical means: changed from a plain coloured `geom_point()` to `shape = 23` (diamond), `fill = Surface_type` (coloured interior) with a fixed `color = "black"` border -- overriding the plot's inherited `color = Surface_type` mapping for this one layer, mirroring the boxplot script's own mean-diamond convention. Added a matching `scale_fill_manual()` (`guide = "none"` -- Surface is already shown via the existing colour legend, no need for a second one).
- Subtitle updated ("Circles = individual samples... | Diamonds/error bars = categorical group mean...").

**Verified**: syntax check passed; end-to-end re-run (exit code 0); plot visually confirmed -- points now solid and clearly visible, diamonds prominent with a black border, no duplicate legend appeared.

### Files Modified (this sub-session, final)

`ASTRA/doe/main_study_analysis.R` (Section 5e `generate_categorical_hinge_plot()`: point/diamond styling revised, `scale_fill_manual()` added, subtitle updated). `CONTEXT.md` (this entry).

### 12. Legend bug fix -- stray "a" glyph underneath the Surface legend keys

User spotted a faint letter "a" rendered underneath the circle/diamond/line glyphs in the Surface legend. Root cause: the item 9 R²/p annotation's `geom_text()` maps `color = Surface_type` but never set `show.legend = FALSE` -- ggplot2's default behaviour for a *text* geom included in a legend is to render its own key glyph as a literal lowercase "a" (a generic text-geom placeholder), which was being layered into the same Surface legend keys as the point/diamond/line geoms (all sharing the same `color` aesthetic).

**Fixed**: added `show.legend = FALSE` to that `geom_text()` call (the `n=` label `geom_text()` elsewhere in the same function already had this set correctly, which is how the original design intended every text layer to behave -- this one was simply missed when it was added in item 9).

**Verified**: end-to-end re-run (exit code 0); legend visually confirmed clean -- only the circle+line glyphs remain, no stray text artefact.

### 13. "Pressure" -> "Applied Mass" terminology finished across the remaining plot-facing text (display text only, not variable/column names)

Continuation of an in-progress rename (already partially done in `main_study_analysis.R`'s hinge plot: `x = "Nominal Applied Mass (g)"`, "no-mass-effect null model") -- the design has always measured this via mass on a balance (grams), not true pressure (kPa/Pa); `Pressure_g`/`Mean_Pressure` etc. are literally applied mass readings mislabelled "pressure" in plot-facing text specifically. Scope confirmed with the user: rendered plot text only (titles/axis labels/legends/subtitles, including factor levels that become legend entries) -- `Pressure_g`/`Mean_Pressure`/`Pressure_level`/`Target`/file names/`Pressure Traces` folder path and all other code identifiers deliberately left untouched (renaming those would be a much larger, higher-risk change touching CSVs and other scripts' hardcoded paths).

Brought the remaining 3 files into line:
- **`ASTRA/plot_recovery_boxplot_actualpressure.R`**: title `"Recovery by Pressure..."` -> `"Recovery by Applied Mass..."`; `x = "Nominal Pressure (g)"` -> `"Nominal Applied Mass (g)"`; subtitle's "actual achieved pressure"/"no pressure-trace record" -> "actual achieved mass"/"no mass-trace record"; `shape` legend title `"Pressure position"` -> `"Mass position"`; the `pressure_source` factor's own two level labels (which double as the shape legend's entries) `"Actual pressure recorded"`/`"No pressure record (nominal position)"` -> `"Actual mass recorded"`/`"No mass record (nominal position)"` (and the matching `scale_shape_manual()` keys).
- **`ASTRA/plot_recovery_vs_pressure_unified.R`** (all 3 fit blocks -- linear/quadratic/logarithmic): titles `"...vs Actual Swabbing Pressure..."` -> `"...vs Actual Applied Mass..."`; `x = "Actual mean swabbing pressure (g, stable regions)"` -> `"Actual mean applied mass (g, stable regions)"`; quadratic/logarithmic subtitles' `"Recovery ~ Pressure + Pressure^2"`/`"Recovery ~ log(Pressure)"` -> `"Recovery ~ Mass + Mass^2"`/`"Recovery ~ log(Mass)"`.
- **`ASTRA/doe/pilot_analysis.R`**: interaction plot titles `"- Surface x Pressure"`/`"- Pressure x Solvent"` -> `"- Surface x Applied Mass"`/`"- Applied Mass x Solvent"`; `color = "Pressure"` legend -> `"Applied Mass"`; the Applied-Mass-x-Solvent plot previously had no explicit `x =` at all (would have defaulted to ggplot's raw column name, `"Pressure_level"`, as the visible axis title) -- added `x = "Applied Mass"` explicitly rather than leaving that default.

Console-only `cat()` log text (not rendered on any plot) and code comments were deliberately left saying "pressure" throughout -- out of scope per the confirmed display-text-only boundary above.

**Verified**: `parse()`-checked all 3 edited files (no syntax errors); re-grepped all three for any remaining "Pressure" inside `labs()`/`title`/`subtitle`/`x =`/`y =`/`color =`/`shape =`/`scale_shape_manual()` -- zero matches remain. Not re-run end-to-end against real data this session (no code logic changed, only literal label strings).

### Files Modified

`ASTRA/plot_recovery_boxplot_actualpressure.R`, `ASTRA/plot_recovery_vs_pressure_unified.R`, `ASTRA/doe/pilot_analysis.R` (all three: plot-facing title/axis/legend/subtitle text only, per above). `CONTEXT.md` (this entry).

### 14. All 6 renamed-plot outputs actually regenerated (item 13 above only edited source, never re-ran it); `Pressure_Categorical_Hinge_PilotMain.png` further updated -- diamonds/points now positioned at ACTUAL achieved mass, not the nominal target

User asked directly whether the plots had been regenerated after item 13 -- they hadn't; item 13 only edited the `.R` source. Ran `plot_recovery_boxplot_actualpressure.R`, `plot_recovery_vs_pressure_unified.R` (all 3 fits), and `pilot_analysis.R` (full collation + stats pipeline) -- all exit code 0, all 6 PNGs confirmed regenerated by timestamp.

User then separately spotted that `Pressure_Categorical_Hinge_PilotMain.png` (`main_study_analysis.R`, not touched by item 13) still showed the OLD "Pressure (g)" label -- root cause was the same "edited but never re-run" gap: that script's own last edit (19:53) postdated the PNG's last write (19:45), so the "Nominal Applied Mass (g)" label from an earlier sub-session had genuinely never been rendered. Re-ran it once to confirm -- fixed.

**User's actual further request** (this item's main work): reposition `generate_categorical_hinge_plot()`'s diamonds (categorical group means) at the group's own mean ACTUAL achieved mass, and the individual sample points at each sample's own ACTUAL achieved mass -- instead of both being placed at the nominal design target (`Pressure_g` = 10/50/100/200/300). This plot had never joined in the ASTRA-measured `Mean_Pressure` at all (unlike `plot_recovery_boxplot_actualpressure.R`/`plot_recovery_vs_pressure_unified.R`, which already did).

**Implementation** (`main_study_analysis.R`):
- New `main_pressure_file`/`pilot_pressure_file` config paths (USER CONFIGURATION) pointing at each study's own `Pressure Traces/ProcessedData/batch_summary.csv` -- same files the two standalone plotting scripts already read.
- `build_pooled_dataset()`: after `combined` is built, both studies' `batch_summary.csv` files are stacked into one `Sample`/`Mean_Pressure` lookup (safe to do in one step, unlike those two scripts' per-study joins, since `RunID` is globally unique across `PILOT_`/`MAIN_` prefixes) and left-joined onto `combined` by `RunID <-> Sample`. Logged: "Actual-pressure-trace match: 57/59 combined rows matched (2 unmatched)". Explicitly scoped as **only** feeding this one plot -- the combined mixed-effects model itself is untouched, still fit against nominal `Pressure_g`/`Pressure_f`/`Pre100`/`Post100` (this request was about the figure, not the statistical model).
- `generate_categorical_hinge_plot()`:
  - `long_data` (individual points) gains `Mean_Pressure`; plotted at `aes(x = Mean_Pressure, ...)` (was `Pressure_g`), and the `position_jitterdodge()` used to spread out points at a shared nominal x is removed entirely -- with real, continuous, naturally-distinct x-positions, artificial jitter/dodge would misrepresent them (matches the existing convention in `plot_recovery_vs_pressure_unified.R`, which never jitters its own real-position points either). 4 sample x analyte points (2 physical pilot samples x 2 analytes, the same `PILOT_015`/`PILOT_020` ABS-200g-wet pair already known from item 8's boxplot fix to lack a pressure-trace match) have no real position and are excluded from this layer -- logged explicitly, and still counted in their group's `mean_rec`/`n` (only the x-position calculation skips them via `na.rm = TRUE`).
  - `means_data`: gains `mean_actual` (mean `Mean_Pressure` per `Surface_type x Analyte x Pressure_g` group, `na.rm = TRUE`) -- the diamond/error-bar/n-label layer's new x-position. `y` values (`mean_rec`/`se_rec`/`n`) are unchanged (still computed from `Recovery` alone), so this is a pure repositioning, not a change to any reported number.
  - Hinge line: the model itself still predicts from nominal `Pre100`/`Post100` (derived from the nominal `pressure_levels` grid, since that's how it was fit) -- but the line's own plotted x-coordinate (`PlotX`, new) is remapped per Surface_type to that level's own mean actual achieved mass (computed from `sub`, the same data the model was fit on), so the dashed line lands among the repositioned points/diamonds instead of at the nominal target. Falls back to the nominal value if a level has zero matched samples (not observed in practice).
  - `hinge_stats_df$x_pos` (R²/p annotation placement) similarly switched from `min(pressure_levels)` (nominal) to `min(long_data$Mean_Pressure, na.rm = TRUE)` (actual).
  - `geom_vline(xintercept = PRESSURE_BREAKPOINT, ...)` (the dotted 100g reference line) deliberately left as a literal nominal `x = 100` -- it marks the model's own design breakpoint, not a data point, so repositioning it wouldn't make sense the way it does for the diamonds/points/line.
  - Subtitle rewritten (3 lines instead of 2) to describe the new positions plainly and to note the x-axis breaks are nominal reference marks only; plot width widened 10in -> 11in and subtitle font shrunk 9 -> 8pt after the longer text first overflowed the right edge in testing.

**Verified**: end-to-end re-run (exit code 0, no errors -- only the same pre-existing singular-fit `performance::r2()` warnings already present before this change); console confirmed the join (57/59 matched) and the 4-point exclusion note; visually confirmed via the regenerated PNG -- diamonds for Steel vs ABS at the same nominal level are now visibly offset from each other (previously both sat exactly on the nominal gridline), the dashed hinge line still tracks through them correctly, and the 3-line subtitle now fits within the widened frame with no clipping.

### Files Modified

`ASTRA/doe/main_study_analysis.R` (`main_pressure_file`/`pilot_pressure_file` config vars; `build_pooled_dataset()` gains the actual-pressure join; `generate_categorical_hinge_plot()`: `long_data`/`means_data` gain `Mean_Pressure`/`mean_actual`, jitterdodge removed, hinge-line `PlotX` remapping, annotation `x_pos` switched to actual, subtitle/width/font adjustments). Regenerated (not just edited) all 4 live plotting scripts' outputs: `Recovery_Boxplot_ActualPressure_PilotMain.png`, `Recovery_vs_ActualPressure_Unified_PilotMain.png` (+ `_Quadratic`/`_Logarithmic`), `PETN_pilot_interactions.png`/`RDX_pilot_interactions.png` (+ the rest of `pilot_analysis.R`'s usual outputs, regenerated as an unavoidable side effect of it being one all-in-one script), `Pressure_Categorical_Hinge_PilotMain.png`. `CONTEXT.md` (this entry).

### 15. "Pressure" -> "Applied Mass" rename extended to the raw ASTRA trace-processing pipelines + a retired design template, plus a full regeneration run of both heavy batch pipelines

User asked to extend the item 13/14 rename to cover "all pressure plots", not just the DOE recovery-analysis figures. Did a repo-wide grep (excluding `archive/`/`Archive/`, per the existing convention that those are intentionally-frozen historical snapshots) for any remaining plot-facing "Pressure" text (`labs()`/`title=`/`x=`/`y=`/`color=`/`fill=`/`shape=`/base-R `main=`), which surfaced two more live scripts that had never been touched by the earlier rename:

- **`ASTRA/main_study_batch_process.R`** and **`ASTRA/time_based_batch_process_adaptive_DISTANCE.R`** (the raw ASTRA balance-trace processing pipelines for the Main Study and Pilot Study respectively -- near-identical twin scripts) -- 6 identical edits in each:
  - Base-R `plot(..., main = sprintf("%s: Full Pressure Trace (%d cycles detected)", ...))` -> `"...Full Applied Mass Trace..."` (time-based single-sample trace plot)
  - Same for the distance-based variant: `"...Full Pressure Trace - Distance Based..."` -> `"...Full Applied Mass Trace - Distance Based..."`
  - `ggplot` multi-sample comparison plot: `title = sprintf("%dg Samples - Full Pressure Traces (Distance-Based)", ...)` -> `"...Full Applied Mass Traces..."`; `y = "Pressure (g)"` -> `"Applied Mass (g)"` (appears in both the comparison plot and the per-sample stable-region-overlay plot, `replaceAll`'d)
  - `plot_batch_qc_overview()`: `title = "Batch QC Overview - Mean Pressure vs Target"` -> `"...Mean Applied Mass vs Target"`; `x = "Mean Pressure (stable regions, g)"` -> `"Mean Applied Mass (stable regions, g)"`
  - Left untouched (out of scope, consistent with items 13/14's own display-text-only boundary): `xlab`/`ylab` on the base-R trace plots already said `"Load (g)"`, not "Pressure" -- no change needed there; every `Pressure_g`/`Mean_Pressure`/`pressure_g` column/variable name; the `.../Pressure Traces/...` folder path; and all console-only `cat()` diagnostic text (e.g. "PRESSURE STATISTICS:", "Mean pressure (stable regions): ...").
- **`ASTRA/doe/ExperimentalDesign/doe_explosives_recovery.R`** (the retired original, never-executed 2-level design template, kept only as a methods-record per the Sept 2 "Codebase Simplification" session) -- its two interaction-plot titles, `"Surface_type x Pressure Interaction"`/`"Pressure x Solvent Interaction"` -> `"Surface_type x Applied Mass Interaction"`/`"Applied Mass x Solvent Interaction"`. The underlying `Pressure` data-column/`aes()` mapping in this template was left as-is (out of scope, matching the established variable-name boundary).

A repo-wide re-grep afterward (same exclusions) confirmed **zero** remaining plot-facing "Pressure" text anywhere live -- the two apparent hits it initially returned were both false positives from case-insensitive substring matching against code identifiers (`PRESSURE_BREAKPOINT`, `target_pressure`), not actual display strings.

**Regenerated both heavy pipelines end-to-end** (per explicit user confirmation, since -- unlike every other script touched in this rename -- these two reprocess every individual raw trace file and rewrite dozens of PNGs each, a much larger blast radius): `main_study_batch_process.R` (`RUN_MODE <- "batch"`, all 44 Main Study samples) and `time_based_batch_process_adaptive_DISTANCE.R` (all 32 Pilot Study samples). Both exit code 0, no real errors (only the pipeline's own normal per-cycle timing-prediction diagnostic lines and ggplot's routine "removed missing values" warnings, 45/32 respectively -- unrelated to this change). Visually confirmed via two regenerated PNGs (`batch_qc_overview.png`, `10g_samples_full_traces.png`) that the new "Applied Mass" title/axis text renders correctly.

### Files Modified

`ASTRA/main_study_batch_process.R`, `ASTRA/time_based_batch_process_adaptive_DISTANCE.R` (6 plot-facing label edits each, per above), `ASTRA/doe/ExperimentalDesign/doe_explosives_recovery.R` (2 interaction-plot titles). Regenerated every individual sample's trace/overlay PNG plus each study's own comparison-by-level and `batch_qc_overview.png` figures, in both `Main Study/Pressure Traces/ProcessedData/Figures/` and `Pilot Study/Pressure Traces/ProcessedData/Figures/`. `CONTEXT.md` (this entry).

### 16. Shared thesis-wide colour palette (`ASTRA/thesis_palette.R`, Okabe-Ito) -- new, single source of truth for every plot; every hardcoded colour across the live codebase replaced

User asked for a single consistent colour scheme to use across every chart in their thesis. Investigated the live codebase first: colours were scattered and inconsistent -- `Study` (Pilot/Main) and `Surface_type` (steel/abs) both used the exact same blue/orange hex pair (`#4477AA`/`#EE9944`) in different figures (so the same two colours carried two different meanings depending on which figure you were looking at), `wet`/`dry` used a completely unrelated blue/green (`#3182bd`/`#31a354`), and the raw-trace QC/cycle-gradient plots used their own separate ad hoc colours again -- no single source of truth anywhere.

**Palette discussion with the user** (talked through before implementing): recommended the Okabe-Ito 8-colour qualitative palette (Okabe & Ito, 2008; Wong, 2011, *Nature Methods*) -- colourblind-safe for both common forms of colour vision deficiency, distinguishable in greyscale -- with a fixed, non-overlapping colour pair assigned to each recurring NOMINAL factor (Surface_type, Study, Analyte, Solvent), so the same two colours never carry two different meanings again. User initially pushed back on the resulting blue/orange pairing (disliked it aesthetically); explored an alternative (Paul Tol's "muted" qualitative palette -- wine/teal/purple/olive/green/rose, no blue or orange anywhere) and a viridis-based option (rejected for nominal factors specifically, since viridis is a *sequential*/ordinal colormap -- using it for unordered categories is a recognised anti-pattern, multiple independent binary factors sampled from it end up looking like arbitrary slices of the same rainbow rather than genuinely distinct comparisons; demonstrated this directly with a rendered comparison swatch). After seeing a rendered swatch of the original full Okabe-Ito mapping again, user decided to go with it after all.

**Final semantic assignment** (`ASTRA/thesis_palette.R`, new file):
- `pal_surface_type`: steel = Blue `#0072B2`, abs = Orange `#E69F00`
- `pal_study`: Pilot = Reddish Purple `#CC79A7`, Main = Bluish Green `#009E73`
- `pal_analyte`: PETN = Sky Blue `#56B4E9`, RDX = Vermillion `#D55E00` (defined for consistency/future use -- no current live plot colours by Analyte, it's always shown via facets instead)
- `pal_solvent`: keyed under BOTH naming conventions used across scripts (`present`/`absent` in `pilot_analysis.R`'s `Solvent_level`, `wet`/`dry` in the raw-trace scripts' `wet_dry`) -> Blue `#0072B2` / Bluish Green `#009E73`
- `pal_mass_level`: pilot's original 2-level low/high design factor (`Pressure_level`, distinct from the main study's 5-level `Pressure_f`/`Pressure_g`, which isn't shown as a discrete colour anywhere) -> Yellow `#F0E442` / Black `#000000`
- `pal_qc_status`: PASS/WARN/FAIL traffic light -> Bluish Green/Orange/Vermillion -- deliberate, disclosed exception to the "never reuse a colour for a different meaning" rule, since status is always shown captioned (PASS/WARN/FAIL text) and never appears as a colour comparison side-by-side with Surface_type/Study/etc in the same figure
- Ordinal/continuous quantities (e.g. cycle number in the stable-region-overlay gradient) directed to use `scale_colour_viridis_c()`/`_d()` instead of a slice of the qualitative palette -- viridis is perceptually uniform and built for exactly this use case.
- `qc_pass_fail_labels()` helper: builds a 2-colour (pass/fail only, no warn) named vector for legend labels that are dynamically `sprintf()`-built at runtime (e.g. `plot_batch_qc_overview()`'s `"Within X% of Target"` text, where X isn't a compile-time constant).

**Applied across all 6 live plotting scripts**:
- `main_study_analysis.R`, `pilot_analysis.R`, `plot_recovery_boxplot_actualpressure.R`, `plot_recovery_vs_pressure_unified.R`: added `source(".../ASTRA/thesis_palette.R")` (absolute path, matching this repo's existing cross-file `source()` convention) alongside each script's own package loading, then replaced every hardcoded hex with the matching `pal_*` vector: `main_study_analysis.R`'s hinge plot (`pal_surface_type`), `pilot_analysis.R`'s 3 interaction plots (`pal_mass_level`, `pal_solvent` x2 -- these previously had NO explicit colour scale at all, relying on ggplot's own default hue picker), `plot_recovery_boxplot_actualpressure.R` + `plot_recovery_vs_pressure_unified.R`'s `Study` colour/fill scales (`pal_study`, including the boxplot script's `guide_legend(override.aes = ...)` fill list, which had to preserve the correct Main-before-Pilot alphabetical legend order rather than just swapping in the new hex codes positionally).
- `main_study_batch_process.R` / `time_based_batch_process_adaptive_DISTANCE.R`: **NOT** `source()`'d -- both scripts' own headers explicitly state "SINGLE-SCRIPT DESIGN: this file is fully self-contained... nothing should source() it either", a deliberate pre-existing design rule that was respected rather than overridden. Instead, the matching hex values were inlined literally (with a comment noting they match `pal_solvent`/`pal_qc_status` for provenance): `PLOTTING_PARAMS$color_wet/color_dry` -> `#0072B2`/`#009E73`; `plot_batch_qc_overview()`'s `Flag_Label` colours (`"steelblue"`/`"firebrick"`, an ad hoc 2-colour PASS/FAIL-style pair with no WARN tier) -> `#009E73`/`#D55E00`; the stable-region-overlay's `cycle_num` gradient (previously `scale_color_gradient(low = "#c6dbef", high = "#08519c")`, an ad hoc light-to-dark blue) -> `scale_color_viridis_c()`.
- **Bug caught during visual verification**: viridis's default direction is dark(low)->light(high), but the stable-overlay plot's own subtitle says "Color intensity: Light (early) -> Dark (late)" (Cycle # low = early) -- a straight swap would have silently made the caption describe the colours backwards. Fixed with `direction = -1` in both scripts' `scale_color_viridis_c()` call before regenerating, confirmed visually correct afterward (cycle 1-4 render pale yellow, cycle 16 renders dark purple, matching the caption).
- **Deliberately left unchanged**: `pilot_analysis.R`'s by-repeat barchart `Condition` fill (a 4-level composite of mass-level x solvent, `"50g/dry"`/`"50g/wet"`/`"200g/dry"`/`"200g/wet"`, currently a separate ColorBrewer-ish quartet) -- all 8 Okabe-Ito hues are already spoken for by the 4 factor pairs above, and this composite factor doesn't decompose cleanly into any single one of them; forcing a worse-fitting reuse was judged worse than leaving its own already-reasonable, already-colourblind-conscious palette alone. Flagged here explicitly rather than silently left inconsistent without explanation.

**Verified**: all 6 files `parse()`-checked after editing (no syntax errors); all 6 re-run end-to-end (`main_study_analysis.R`, `pilot_analysis.R` -- full collation + stats pipeline, `plot_recovery_boxplot_actualpressure.R`, `plot_recovery_vs_pressure_unified.R`, `main_study_batch_process.R`, `time_based_batch_process_adaptive_DISTANCE.R` -- both full batch re-processing runs, twice each, once before and once after the viridis-direction fix), all exit code 0, zero real errors (only the pipelines' own pre-existing, unrelated warnings: `performance::r2()`'s singular-fit notices, ggplot's routine "removed missing values", 45/32 harmless warnings respectively). Visually confirmed via 5 regenerated PNGs spanning every changed colour: `Pressure_Categorical_Hinge_PilotMain.png` (Surface_type blue/orange), `Recovery_Boxplot_ActualPressure_PilotMain.png` (Study purple/green), `PETN_pilot_interactions.png` (mass_level yellow/black, solvent blue/green), `batch_qc_overview.png` (QC status bluish-green/vermillion), `MAIN_001_stable_overlay.png` (viridis cycle gradient, correct direction).

### Files Modified

`ASTRA/thesis_palette.R` (new). `ASTRA/doe/main_study_analysis.R`, `ASTRA/doe/pilot_analysis.R`, `ASTRA/plot_recovery_boxplot_actualpressure.R`, `ASTRA/plot_recovery_vs_pressure_unified.R` (each: added `source()` of the new palette file, replaced hardcoded hex with `pal_*` vectors). `ASTRA/main_study_batch_process.R`, `ASTRA/time_based_batch_process_adaptive_DISTANCE.R` (each: matching hex values inlined literally, not `source()`'d, per their own self-contained design rule; cycle-gradient switched to `scale_color_viridis_c(direction = -1)`). Regenerated every plot produced by all 6 scripts (Pilot Study + Main Study, both `Pressure Traces/ProcessedData/Figures/` folders and the main `doe/`-analysis PNGs). `CONTEXT.md` (this entry).

---

## Session Summary (September 2, 2026) — Follow-up on MAIN_013, ABS NC Contamination Source, and the Other 4 Unevaluated NCs

### Motivation

Direct follow-up to the August 27, 2026 session's "Not done, flagged for follow-up" list (below): reanalyse `MAIN_013`, investigate the ABS NC contamination source, and resolve the other 4 unevaluated NCs. Cross-referenced with `RedTeam_Findings.md`'s FINEX-side item 20 work done the same week (the same underlying failure mode — a whole-injection instrument-response wobble that PETN's raw-PA quantification is exposed to but RDX's IS ratio cancels — turns out to explain `MAIN_013` too, see below).

### 1. `MAIN_013` — same root cause as FINEX's Steel 4/Outstanding samples 4 (RedTeam_Findings.md item 20), not a data-quality problem with this sample

`MAIN_013` (Line 95, Analysis1) fails on an isolated `petn_qc_6ng_post` FAIL. Traced to its bracketing 6ng PETN QCs: pre = Line 87 (PASS), post = **Line 99 (FAIL_BIAS, +51.2% bias)** — the *last* 6ng QC in the Analysis1 sequence. Investigated why Line 99 fails when the whole run otherwise declines smoothly (133,251→80,334→49,449→44,387→38,948→36,160→30,580 across Lines 15-87):

- Line 99's own PETN PA jumps *up* to 42,757 (vs. 30,580 at Line 87) — breaking the decline.
- **The internal standard's own raw PA at Line 99 also jumps up in the same direction and a similar relative magnitude** (7,456→9,949, +33%, vs. PETN's 30,580→42,757, +40%) — proving this is a shared, whole-injection instrument-response effect, not a PETN-specific integration failure (identical diagnostic signature to the FINEX Steel 4/Line 87 and Outstanding samples 4 findings — this is now the **third** independent real-data confirmation of the same mechanism).
- **RDX's own ratio-based bias at Line 99 is only −8.8%** (in fact *less* biased than its neighbours' −18% to −19%) — direct confirmation that RDX's `rdx_pa/rdx_is_pa` ratio cancels this wobble almost perfectly, while PETN's raw-PA-only quantification (item 8) has no such protection.

**Not yet reanalysed anywhere** — checked the "Oustanding samples 5" mixed FINEX/ASTRA dataset (the other reprocessed-elsewhere candidate) directly; it contains 9 other `MAIN_###` samples but not `MAIN_013`. Per the FINEX decision on the structurally-identical Steel 4/Outstanding samples 4 case (accept the failure, treat existing reanalysis as the fix — see `RedTeam_Findings.md` item 20), the consistent options here are: (a) physically reanalyse `MAIN_013` (a lab task, not something resolvable from these data files), or (b) accept the already-computed values (`PETN_Recovery_pct=12.63`, `RDX_Recovery_pct=10.86` — both internally plausible against neighbouring samples) with a documented caveat, consistent with how Steel 4/Outstanding samples 4 were handled. **Not decided here — flagged for the same user decision as item 20.**

### 2 & 3. ABS NC contamination source AND 3 of the 4 other unevaluated NCs — traced to a single bad low-level RDX calibration standard in Analysis1

Investigating the ABS NC contamination source and the Steel/ABS NC bracket failures converged on the same root cause, found by comparing each dataset's own 0.2ng-level RDX calibration point against (a) the rest of that dataset's own 0.2ng QCs and (b) simple proportional scaling from the 6ng calibrator:

| Dataset | 0.2ng Cal ratio (rdx_pa/rdx_is_pa) | 0.2ng QCs' own ratio (rest of run) | 6ng Cal ratio (for scale) |
|---|---|---|---|
| Analysis1 | **0.0445** (Line 6) | ~0.018–0.022 (Lines 25-97) | 0.511 (Line 9) |
| Analysis2 (healthy, for contrast) | 0.0208 (Line 6) | ~0.019–0.023 (Lines 25-93) | 0.575 (Line 9) |

**Analysis1's own lowest-level RDX calibration standard (Cal Line 6, nominal 0.2ng) reads at roughly double the ratio every other 0.2ng-level measurement in the same dataset shows** — and Analysis2's freshly-run equivalent calibrator (0.0208) matches Analysis1's *other* 0.2ng QCs almost exactly, not Analysis1's own Cal point. Simple proportional scaling from the 6ng calibrator (0.511 × 0.2/6 ≈ 0.017) also lands close to the ~0.018-0.022 range, not 0.0445. This is strong, convergent evidence that **Analysis1's 0.2ng RDX calibration standard itself is the anomalous point** (almost certainly a contaminated/mislabeled/degraded calibration injection), not a reflection of the dataset's true low-end response.

**Practical consequence**: this single bad calibration point drags the low end of Analysis1's fitted RDX quadratic curve upward, so every genuine 0.2ng-level RDX response for the rest of the run (Lines 25 onward) computes a concentration of **exactly 0** (`rdx_percent_bias = -100.0000000%`, to 7 decimal places, at every one of Lines 25/37/49/61/73/85) — a "floor at zero" `solve_concentration()` result (no positive root exists once the response falls below the curve's distorted lower bound), not a real non-detect. (Also confirms Analysis1 predates item 5's `rdx_extrapolated`/`petn_extrapolated` column — it has no `rdx_extrapolated` column at all, unlike Analysis2 — so this dataset hasn't been reprocessed with the current `03_Quantification.R` since that flag was added; reprocessing alone would surface the flag but would **not** fix the underlying bad calibrator.)

This one bug explains 3 separate open items at once:
- **`MAIN_SteelBatch1_NC`/`MAIN_SteelBatch2_NC`** ("RDX 0.2ng QC bracket FAILED or unavailable"): their bracketing 0.2ng RDX QCs floor to a computed concentration of 0 → `SENSITIVITY_CHECK_FAIL` → bracket FAIL. Not a problem with these NCs' own data.
- **The reported 0% RDX recovery for the 2 "positive" ABS NCs (`MAIN_ABSBatch1_NC`/`MAIN_ABSBatch2_NC`, also Analysis1)**: their `nc_result` = "RDX contaminated" is genuine and **independent of this bug** (driven purely by `rdx_snr_flag == "Quantifiable"`, i.e. a real detected peak, in `derive_nc_result()`'s tier-based logic — concentration value is irrelevant to that classification). But their reported `RDX_Recovery_pct = 0%` is almost certainly an **underestimate** caused by the same floor-at-zero bug — the true contamination level is probably a small positive value, not literally zero.
- (Does not explain the *source* of the contamination itself — that's a real lab/physical question (pre-contaminated ABS stock vs. handling/storage vs. equipment carryover) that can't be answered from these data files alone. One data-only observation that might help narrow it down: both positive ABS NCs are in Analysis1, both came back positive, and both evaluable Steel NCs elsewhere (Analysis1's own SteelBatch1/2 are unevaluated due to the bug above, but Analysis2's SteelBatch3/4/5 are clean) — worth checking against lab records whether the *specific physical ABS surfaces* used for Batch1/2 were also used/stored/handled differently from the Steel surfaces in the same batches, or reused from a prior contaminated run.)

**Recommended fix, implemented and tested (02/09/2026), then REVERTED — it made the problem worse, not better.** Added a genuine per-dataset calibration-point exclusion mechanism, mirroring the existing QC-exclusion pattern:
- `GlobalCode.R`: new `cal_exclude_lines_by_dataset` (keyed by `ParentFolder`, same convention as `drift_correction_exclude_qc_lines_by_dataset`) resolving to `cal_exclude_lines`, validated in `validate_setup()` and logged in `run_metadata.yaml`.
- `Code/03_Quantification.R`: the calibration-fitting loop now filters `cal_exclude_lines`-matched Line(s) out of `cal_fit` (after the existing Below_LOD filter, before the `lm()` call) — deliberately **not analyte-specific** per explicit user instruction (excludes the Line from both PETN's and RDX's fits together, even though the investigation below shows only RDX's response at Line 6 is actually anomalous — PETN's own raw PA there is perfectly consistent with its neighbouring Cal levels). Excluded row stays in `Combined`/`CalData`/`CalibrationSet` and the output — only removed from the fit itself. New `N_Excluded_Manual` column added alongside the existing `N_Excluded_LOD` in the `*_CalibrationStats.xlsx` export.
- **Verified the mechanism itself works exactly as designed**: re-ran `GlobalCode.R`→`02`→`03` for Analysis1 with `"Analysis1" = c(6)` set; console confirmed `"PETN calibration set 1: excluding 1 Cal point(s) from the fit (Line 6)"` and the same for RDX; Line 6 remained present in the output, just absent from the fitted curve. Exit code 0, no errors.
- **But the actual effect was the opposite of what was wanted**: with only Cal levels 2/4/6/8/10ng left to fit RDX's quadratic curve, the curve no longer extrapolates down to a 0.2ng-equivalent response *at all* — **every** 0.2ng-level RDX QC, including **Line 13, which correctly resolved to ~0.20ng before this change**, now floors to a computed concentration of exactly 0 (`rdx_extrapolated = TRUE`). Confirmed directly: before exclusion, Line 13 = 0.2011ng (bias +0.6%), Lines 25-85 = 0 (bias -100%), Line 97 = 0.322ng (bias +61%); after exclusion, **all 8** 0.2ng QCs floor to 0.
- **Conclusion: Cal Line 6, whatever its own merits, is functioning as this dataset's only usable low-end calibration anchor — removing it doesn't fix the flooring problem, it just extends it to cover the entire 0.2ng level instead of most of it.** The earlier "Analysis2's fresh 0.2ng Cal ratio (0.0208) closely matches Analysis1's own later 0.2ng QCs (~0.018-0.022), not Analysis1's own Cal point (0.0445)" evidence is still true and still suggests Line 6 itself reads anomalously high relative to the rest of the dataset's real 0.2ng-level behaviour — but "anomalous" doesn't mean "safe to discard" when there's no other low-end anchor to fall back on.
- **Reverted**: removed the `"Analysis1" = c(6)` entry from `cal_exclude_lines_by_dataset` (left empty, with a comment explaining why it was tried and rejected) and re-ran Analysis1 through `03_Quantification.R` again to restore the original (better) behaviour — confirmed byte-for-byte identical per-QC concentrations to the pre-exclusion state (Line 13 = 0.2011ng again, etc.). The exclusion **mechanism** itself is kept in the codebase (tested, working, currently unused — empty by default for every dataset), available for a genuinely fixable future case (e.g. a QC-style dip-then-recover Cal point with healthy calibration data on both sides, unlike this case where the bad point is the *only* data at that end of the range).
- **Practical consequence**: `SteelBatch1_NC`/`SteelBatch2_NC` remain unresolved, and `ABSBatch1_NC`/`ABSBatch2_NC`'s reported 0% RDX recovery remains a likely understatement of their real (still-positive) contamination. **Root cause is now understood but NOT fixed** — this dataset's RDX calibration design simply doesn't have a second, independent low-level standard to substitute in; a real fix would need either a genuinely different/additional low-level calibration point (impossible to add retroactively to already-acquired data) or accepting these specific NCs as analytically unresolvable via calibration, similar to the `MAIN_013`/`ABSBatch3_NC` "needs physical reanalysis" category above.

### Files Modified (this sub-investigation)

`GCMSQuantitation/GlobalCode.R` (new `cal_exclude_lines_by_dataset`/`cal_exclude_lines`, `validate_setup()` check, `run_metadata.yaml` logging — net effect on live datasets: none, since the list is empty), `GCMSQuantitation/Code/03_Quantification.R` (calibration-fit loop now honours `cal_exclude_lines`; `N_Excluded_Manual` column added to `cal_stats`/`*_CalibrationStats.xlsx`). Analysis1 was reprocessed twice (once with the exclusion, once without) — final on-disk state is unchanged from before this investigation (confirmed identical per-QC values); the intermediate "with exclusion" run's own backup is retained in `Analysis1/Results/Backup/2026-09-02_10-41-43_Analysis1_GCMSResults.csv` purely as an artefact of the normal backup-before-overwrite mechanism, not as a meaningful alternate result.

### 4. `MAIN_ABSBatch3_NC` — genuine, isolated injection-validity failure, not fixable

Fails on `"IS abnormally low (PA=105)"` — the internal-standard injection-validity check (`rdx_is_pa_flag`), a hard-coded short-circuit unrelated to any bracket or calibration mechanism. PA=105 is far below `MinPeakArea_IS` (1000) and even further below every genuine injection in this study (thousands to tens of thousands). This is a real, isolated problem with that one specific injection (partial injection, syringe issue, etc.) — not fixable via any exclusion list, since it's the row's *own* IS that's bad, not a neighbour's. Would need physical reanalysis, same category as `MAIN_013` above.

### 5. `MAIN_ABSBatch4_NC`/`MAIN_ABSBatch5_NC` — NOT fixed by the existing Line 39 exclusion; real (but marginal) cause found

`main_study_analysis.R` already has `qc_bracket_exclude <- list("/Analysis2/" = c(39))` (the already-documented item-23-equivalent bad injection — PETN PA=267/Below_LOQ, RDX PA=107 collapsing in lockstep, IS unaffected — see `RedTeam_Findings.md` item 23 for the FINEX-side twin of this exact injection). **Verified this exclusion is actually being applied** (re-ran `main_study_analysis.R`; console confirms `"excluding QC Line(s) 39 from bracket assignment"`) — but the NC fail count is unchanged (still 5 FAIL) either way, because **Line 39 was never the real cause of these two NCs' failure**.

Traced the actual cause: `MAIN_ABSBatch3_NC`/`4_NC`/`5_NC`/`SteelBatch3_NC` (Lines 17/19/21/23) sit between the 6ng RDX QC at **Line 15 (pre, bias = −19.3%, PASS)** and **Line 27 (post, bias = −20.9%, FAIL)** — Line 27 sits just **0.9 percentage points** outside the ±20% bias limit. This is a genuine but *marginal* QC performance early in the Analysis2 run (Lines 15 and 27 both sit close to the boundary, ~19-21% biased, before settling to a much cleaner ~1.5-3% for the rest of the run from Line 51 onward) — not a dramatic, obviously-anomalous single injection the way Line 33/39/99 elsewhere in this document are. `MAIN_ABSBatch3_NC` and `SteelBatch3_NC`'s own overall results aren't affected by this (the former already fails earlier on its own IS check; the latter's own RDX result doesn't need the 6ng bracket), but `MAIN_ABSBatch4_NC`/`5_NC`'s does.

**Not fixed — flagged as a judgement call, not a mechanical bug**: there's no clear evidence Line 27 (or Line 15) is an anomalous/failed injection the way the other excluded points in this document are (no lockstep IS collapse, no dip-then-recover shape) — it may simply be genuine early-run QC performance that happens to sit right at the pass/fail boundary. Worth a decision on whether ±20% is being read too strictly at the margin here, versus treating it as a real (if small) accuracy problem specific to the early part of this run.

### Decision (02/09/2026): physically reanalyse the affected samples

**User's decision: reanalyse the affected samples in the lab, rather than pursuing further code-side fixes.** Covers every item above that isn't already resolved by existing code:
- `MAIN_013` (item 1) — bracketed by a whole-injection instrument-response wobble at Line 99, not a real data-quality problem with the sample itself.
- `MAIN_SteelBatch1_NC`/`MAIN_SteelBatch2_NC` (item 2/3) — bracket failures caused by Analysis1's RDX 0.2ng calibration-curve flooring; confirmed not fixable by excluding the calibration point (see item 2/3 above — tried, made it worse, reverted).
- `MAIN_ABSBatch1_NC`/`MAIN_ABSBatch2_NC` (item 2/3) — genuine RDX-positive result, but reported 0% recovery is an artefact of the same calibration flooring; a reanalysis would give a trustworthy quantitative recovery figure instead of the current artificial 0%.
- `MAIN_ABSBatch3_NC` (item 4) — genuine, isolated IS injection failure, not fixable from the existing data.

`MAIN_ABSBatch4_NC`/`MAIN_ABSBatch5_NC` (item 5, the marginal Line 27 bias case) is **not** included in this decision — no reanalysis needed unless the user separately decides the ±20% boundary call for Line 27 should be revisited; left as an open judgement call, not actioned here.

**Status: reanalysis is a lab task, pending — not yet done.** No further code changes follow from this decision; the calibration-exclusion mechanism added and tested above remains in the codebase (unused, empty by default) in case a genuinely fixable calibration-point case turns up in a future dataset. Once the physical reanalysis results come back, they should be dropped into a new `AnalysisN` folder under `Main Study/GC Data/` and picked up automatically by `main_study_analysis.R`'s existing `select_best_attempt()` dedup logic (same mechanism already used for `MAIN_004/005/018/027/033/034/038/042/044`'s own re-analyses) — no code changes needed for that step either.

### 6. `main_study_analysis_categorical_pressure.R` (new, standalone) — resolves the Aug 27 "refit Pressure_g as categorical + test ABS curvature" follow-up

New self-contained companion script (mirrors `main_study_analysis_actual_pressure.R`'s own pattern — reads the already-collated `main_study_data_nested.csv`/`pilot_data_nested.csv` directly, rebuilds the identical pooled Pilot+Main dataset, does **not** touch `main_study_analysis.R` or its outputs). Refits `Recovery ~ Surface_type * Pressure_f + Study + (1|SurfaceID_nested)` with `Pressure_f` as a 5-level categorical factor (no quadratic-curve shape assumption at all), plus `emmeans()` pairwise (Tukey-adjusted) contrasts between every pair of pressure levels, both for the combined model and for Steel-only/ABS-only subsets. Output: `main_study_analysis_summary_CATEGORICAL_PRESSURE.txt` and `Pressure_Categorical_Means_PilotMain.png` (both in `Main Study/`, alongside but not overwriting the quadratic model's own outputs).

**Result — refines rather than simply confirms the Aug 27 finding:**

- **The categorical model's own EMMs peak at 100g, not 200g, for both analytes on Steel** (PETN: 100g=28.7% vs 200g=24.9%; RDX: 100g=28.1% vs 200g=24.2%) — directly matching the raw data (not the quadratic model's forced-symmetric-parabola EMM), confirming the Aug 27 correction was right to prefer the raw means over the quadratic curve's vertex.
- **But formally, 100g is NOT statistically distinguishable from 200g for either analyte** (Steel PETN Tukey p=0.889; Steel RDX p=0.874) — the categorical model's own point estimates favour 100g, but the sample size/variability here can't statistically pin down which of the two adjacent levels is the "true" peak. Correct statement for the thesis: *recovery is descriptively higher at 100g than 200g, but the two are not statistically distinguishable at this sample size* — not "the peak is significantly at 100g, not 200g."
- **What IS statistically solid for Steel (both analytes): the RISE from 10g is real.** 10g vs 100g and 10g vs 200g are both significant (p<0.05, both analytes) — i.e. "more pressure helps, up to a point" is a safe claim. The DECLINE at the top end (100g/200g vs 300g) is directionally present in the point estimates but **not statistically significant anywhere** (e.g. Steel PETN 100g vs 300g: p=0.121) — "rises then falls" should be stated as "rises significantly, then a non-significant decline" for Steel, not as a confirmed two-sided effect.
- **ABS curvature, tested directly for the first time (Part B, surface-only subset models) — genuinely different answers for the two analytes, correcting the Aug 27 session's informal "ABS doesn't show a clean pattern" framing:**
  - **PETN on ABS: NO significant pressure effect on its own** (p = 0.146) — confirms the Aug 27 informal read was right for PETN specifically.
  - **RDX on ABS: a SIGNIFICANT pressure effect on its own** (p = 0.017) — this is a **new finding**, not previously tested. ABS RDX's own EMMs (10g=7.1%, 50g=11.6%, 100g=15.5%, 200g=11.2%, 300g=10.0%) show the same qualitative rise-then-plateau shape as Steel, just smaller in magnitude — the significant pairwise contrast is specifically 10g vs 100g (p=0.008), the same "real rise, non-significant decline" pattern found for Steel above.
- **Practical correction to the Aug 27 text**: "for ABS specifically, the raw data doesn't show a clean version of that pattern" should be revised to **analyte-specific**: true for PETN, not true for RDX — RDX on ABS does show a real, statistically detectable pressure response, just a smaller one than Steel's (consistent with the significant `Surface_type:Pressure_f` interaction in the combined RDX-adjacent PETN model, though note the combined RDX model's own interaction term itself was *not* significant, p=0.29 — the ABS-only subset model is the more direct/sensitive test for this specific question than the interaction term in the pooled model).

### Files Modified

`ASTRA/doe/main_study_analysis_categorical_pressure.R` (new, standalone — does not modify `main_study_analysis.R` or any of its existing outputs). `CONTEXT.md` (this entry).

Re-ran `main_study_analysis.R` (no code changes made this session) to verify the existing Line 39 exclusion and refresh `main_study_data_nested.csv`/`.xlsx` (written successfully) — **`main_study_results.xlsx` failed to write** (`Permission denied`, `GC Data/main_study_results.xlsx` is currently locked, likely open in Excel) — needs a re-run with that file closed before the refreshed workbook is available. `CONTEXT.md` (this entry). No fixes implemented yet — every finding above is flagged for a decision (calibration-point exclusion mechanism, `MAIN_013`/`ABSBatch3_NC` reanalysis, and the Line 27 marginal-bias judgement call) before further code changes.

---

## Session Summary (August 27, 2026) — Main Study Full Run: One QC Failure, ABS Negative-Control Contamination Found, Pooled-Model Pressure Peak Shown to Be a Curve-Fitting/Study-Composition Artefact, Preliminary Boxplot X-Axis Fixed

### Motivation

Continuation of a red-team-style review session (see `GCMSQuantitation/CONTEXT.md` for the FINEX-side work done in parallel, and `RedTeam_Findings.md` for the numbered-item tracking). A drift-correction bug found in `Analysis2` (see GCMSQuantitation's CONTEXT.md entry, same date) was fixed first; this entry covers the subsequent full Main Study run and its statistical interpretation.

### 1. Full Main Study run: 43/44 real samples pass, but the negative controls tell a more concerning story

`main_study_results.xlsx` regenerated post-Analysis2-fix: 43 of 44 real samples PASS/PASS* (97.7% complete). The one failure, **MAIN_013** (ABS, 300g, EtOH present, `Analysis1`) — an isolated `petn_qc_6ng_post` FAIL, unrelated to the Analysis2 fix; RDX and the pre-bracket both PASS. Values are still computed (`PETN_Recovery_pct=12.63`, `RDX_Recovery_pct=10.86`) despite the FAIL flag, per the pipeline's usual "compute but flag" convention; `Outcome=Reanalyse` is the correct next step, not yet done.

**Negative controls are a real concern, not just a bookkeeping gap.** Of 10 NCs, only 5 gave a usable clean/contaminated verdict — the other 5 failed their own analysis-acceptance QC (2 Steel: RDX 0.2ng bracket, `Analysis1`; 1 ABS: IS abnormally low PA=105, `Analysis2`; 2 ABS: RDX 6ng bracket, `Analysis2`) and are "not evaluated." Of the 5 that *did* resolve: **all 3 evaluable Steel NCs came back "Negative (clean)"; both evaluable ABS NCs came back "Positive: PETN trace detected, RDX contaminated."** A 100%-vs-0% split on real (if small) evidence — worth investigating the source (pre-contaminated ABS stock? handling/storage? equipment carryover?) before treating ABS sample recovery values as background-free. Not yet investigated further.

### 2. The pooled quadratic model's "pressure peaks at 200g" claim does not match the raw data — traced to two compounding causes

Asked to interpret `main_study_analysis_summary.txt`'s combined (Pilot+Main) model. Initially reported the EMM table's apparent peak at 200g (Steel PETN: 7.70→16.18→23.58→**27.68**→17.51 across 10/50/100/200/300g) as "the" pressure optimum — **this was wrong**, caught when the raw boxplot (`PETN_RDX_main_preliminary.png`) visibly peaked at 100g instead.

Quantified directly (Steel, PETN): raw pooled mean at 100g is **29.96%**, the model's EMM is 23.58% (under by 6.4pp); raw pooled mean at 200g is 24.53%, the model's EMM is 27.68% (over by 3.15pp). Two independent causes, both real:
1. **Symmetric-parabola mismatch**: the quadratic model (`Pressure_g + Pressure_g²`) forces one smooth symmetric curve through all 5 points, but the real relationship rises sharply (10g→100g) then declines only gently (100g→300g) — an asymmetric shape a single quadratic term structurally can't represent. The least-squares fit compromises by flattening/shifting the peak rightward.
2. **Study-composition confound at 50g/200g**: those two levels are a 1/3-Main + 2/3-Pilot mix (Pilot's own original design levels), while 10/100/300g are 100% Main. Since `Study` has a real, significant effect, and the EMM is reported "averaged over the levels of Study" (an implicit 50/50 blend, not weighted by the actual 1-Main:2-Pilot composition), the 200g EMM gets pulled toward Main's higher values more than the raw pooled mean does.

**Correction issued to the user**: the statistically-confirmed finding is that curvature exists (`Pressure_g²` significant, p=2.3e-05 PETN / p=4.5e-05 RDX) — i.e. the relationship is non-monotonic, not "more pressure = more recovery." *Where* the peak sits is a separate question the model's own smoothed curve answers less reliably than the raw per-level means; the raw data says 100g, not 200g, for Steel. Recommended (not yet done): refit `Pressure_g` as a categorical factor + pairwise contrasts to formally test 100g vs 200g rather than relying on the parabola's vertex.

**Follow-up correction (n/replication)**: initially claimed 50g/200g were under-replicated (n=2/surface, Main-only). This was also wrong — `build_pooled_dataset()` pools in Pilot's own 4 reps/surface at exactly those two levels, giving a genuinely **balanced n=6/surface at all 5 levels** (verified directly: 43 Main QC-passed + 16 Pilot-pooled = 59, matching the summary file's reported n exactly). Retracted the "need more 50g/200g repeats" recommendation once checked against the actual pooling code rather than the Main-only `Summary_By_Pressure` sheet.

**ABS surface reconsidered separately, at the user's prompt**: the same raw-vs-EMM check for ABS shows an even weaker case for "rises then falls" — raw pooled means (PETN) go 8.98→12.60→**17.00**→**9.99**→13.85 across 10/50/100/200/300g, i.e. rise, peak at 100g, **drop below the 50g level at 200g**, then rise again at 300g (RDX shows the same shape). Not a clean parabola at all, and the whole range (~8-17%) is much smaller than Steel's (~8-30%) — closer to a flat relationship with sampling noise (n=6/cell) than a reliable curve. The significant `Surface_type:Pressure_g` interaction confirms ABS's response *differs* from Steel's, almost certainly because Steel's much larger effect dominates the pooled model's overall quadratic terms — but whether ABS shows any statistically detectable curvature *on its own* has not been tested (would need an ABS-only subset fit or a simple-effects contrast). Honest conclusion given to the user: "rises then falls" is well-supported for Steel; for ABS specifically, the raw data doesn't show a clean version of that pattern, and it shouldn't be assumed to hold there without testing it directly.

### 3. `main_study_analysis.R`'s preliminary boxplot fixed — numeric, not categorical, pressure axis

`generate_preliminary_plot()` (Section 4) converted `Pressure_g` to a factor before plotting, forcing all 5 nominal levels to equal categorical spacing regardless of their real numeric gaps (10→50 = 40g, 50→100 = 50g, 100→200 and 200→300 = 100g each) — visually distorting the shape of the pressure-recovery relationship (part of why the 200g-vs-100g peak discrepancy above was easy to miss at a glance). Fixed: `Pressure_g` now stays numeric, `geom_boxplot`/`geom_jitter` use an explicit `group = Pressure_g` aesthetic and gram-scale widths (15/5) to work correctly on a continuous x, and `scale_x_continuous(breaks = c(10,50,100,200,300))` keeps all levels labelled. Re-ran and confirmed visually. Committed (`34ce142`).

**Files modified**: `main_study_analysis.R` (item 3). No other files changed by ASTRA-side work this session — the drift-correction fix (item 4, see `GCMSQuantitation/CONTEXT.md`) was in `GlobalCode.R`, shared with FINEX.

**Not done, flagged for follow-up**: reanalyse `MAIN_013`; investigate the ABS NC contamination source; resolve the other 4 unevaluated NCs; refit `Pressure_g` as categorical + pairwise contrasts to properly locate the peak (Steel) and test whether ABS shows any curvature at all.

---

## Session Summary (August 25, 2026) — Actual (ASTRA-Measured) Pressure Model + RDX Batch/Date Effect Re-Tested With Main Study Data + Stock/Vial (`Standard`) Column Investigated

### Motivation

Follow-on work from a separate ASTRA-side session (see `ASTRA/README.md`'s own Changelog for the companion pressure-trace-processing work) that generated, for the first time, an ASTRA-measured **actual** achieved swabbing pressure (`Mean_Pressure`) for every Main Study sample (previously this only existed for the Pilot Study). This raised the obvious question `main_study_analysis.R`'s own combined model had never been able to ask: does using the *actual* achieved pressure instead of the *nominal/target* `Pressure_g` change the already-significant pressure-recovery relationship? A second, unrelated thread also came up mid-session: whether the already-known RDX-specific batch/date dependency (documented extensively elsewhere in this file, "RDX Batch-to-Batch Recovery Investigation" and its many follow-ups) still holds once the Main Study's own later dates are added to the timeline.

### 1. `main_study_analysis_actual_pressure.R` (new, standalone -- `main_study_analysis.R` left completely untouched)

Mirrors `main_study_analysis.R`'s Section 5/6/6b combined-model architecture exactly (`Recovery ~ Surface_type * (Pressure + Pressure^2) + Study + (1|SurfaceID_nested)`, same Type III ANOVA / `effectsize::eta_squared()` / `emmeans()` / retrospective + a priori/MDES power-analysis sections), but substitutes the ASTRA-measured `Mean_Pressure` for the nominal `Pressure_g` predictor. Deliberately does **not** re-run GC-MS collation (no `InjectionAcceptance.R` sourcing) -- reads the already-collated `main_study_data_nested.csv`/`pilot_data_nested.csv` directly and only adds the actual-pressure join (each study's own `Pressure Traces/ProcessedData/batch_summary.csv`, produced by the ASTRA repo's pressure-trace pipeline) on top. `CumulativeUseNumber` intentionally omitted (out of scope for this specific actual-vs-nominal comparison). Output: `main_study_analysis_summary_ACTUAL_PRESSURE.txt`, written alongside (not overwriting) the existing `main_study_analysis_summary.txt`, specifically so the two can be diffed/compared side by side.

**Result -- the core pressure effect is essentially unchanged, but two secondary effects shift meaningfully:**

| Effect | Nominal `Pressure_g` (existing, n=50 checkpoint) | Actual `Mean_Pressure` (new, full n=48-49) |
|---|---|---|
| PETN pressure linear/quadratic | F=16.14/12.47, p=0.0003/0.0011, η²=0.30/0.25 | F=16.11/12.08, p=0.0003/0.0014, η²=0.31/0.25 |
| RDX pressure linear/quadratic | F=20.99/18.65, p<0.0001/0.0001, η²=0.37/0.34 | F=19.66/17.27, p<0.0001/0.0002, η²=0.37/0.33 |
| **PETN Surface×Pressure** | **F=4.49, p=0.041 (significant)** | F=2.27, p=0.140 (not significant) |
| RDX Surface×Pressure | F=3.25, p=0.080 | F=2.11, p=0.155 |
| PETN Study | F=3.28, p=0.077 | F=1.61, p=0.212 |
| RDX Study | F=12.93, p=0.0008 | F=10.20, p=0.0027 (still significant) |

Interpretation: the main pressure dose-response finding is robust to which pressure measure is used (reassuring -- not an artefact of using idealised target values). PETN's Surface×Pressure interaction, however, loses significance once real (noisier, especially at the 10g/300g extremes) achieved pressure replaces the clean nominal target -- some of that interaction signal in the original analysis may have been an artefact of the idealised predictor. The `Study` (Pilot vs Main) effect weakens for both analytes (more for PETN) once actual pressure is controlled for directly, consistent with part of the original `Study` effect having actually been capturing systematic *achieved-pressure* differences between the two studies' execution, now absorbed into the pressure term instead. RDX's `Study` effect remains significant either way -- consistent with (and motivating) the batch/date investigation below. Power-analysis conclusions (MinN, MDES) are materially unchanged between the two variants.

### 2. `rdx_batch_effect_with_main_study.R` (new, standalone) -- Does Adding Main Study Data Extend the Pilot-Only RDX Batch/Date Finding?

Built to directly test whether the well-established within-Pilot RDX decline (see "RDX Batch-to-Batch Recovery Investigation" below) simply continues once the Main Study's own later calendar dates are added to the same timeline, using the same combined wet-Pilot(50g/200g)+Main dataset (with actual pressure) as above.

**Finding: NOT a simple continuation -- there is an apparent reset at the Pilot→Main boundary, not further decline.**

Raw RDX means by calendar date (both studies, pooled across pressure levels): 07-13 Pilot 24.0% → 07-30 Pilot 15.3% → 08-04 Pilot 7.8% (n=1) → 08-07 Pilot 10.9% → **08-14 Main 17.6%** → 08-21 Main 19.4%. RDX genuinely declines through the whole Pilot Study exactly as already documented, but then **rebounds** at the start of the Main Study rather than continuing to fall -- the opposite of what a pure "more elapsed calendar time = more decline" mechanism predicts (Main should have been *lower* than Pilot's 10.9% endpoint, not back up near its 24.0% starting point).

Confirmed statistically, not just visually, with two nested mixed models (controlling for actual pressure linear+quadratic and Surface_type throughout):
- A single continuous `ElapsedDays` term across the full 39-day combined span is **not significant on its own** (p=0.336) -- the naive "just keeps declining" story fails once Main data is added.
- Fitting `ElapsedDays` **together with** a discrete `Study` step term makes *both* significant: **ElapsedDays = -0.37 pp/day (p=0.028)** and **Study(Main) = +14.5pp (p=0.0004)**. I.e. RDX genuinely keeps declining continuously within a given time window, but something discrete reset the baseline upward specifically at the Pilot→Main transition.
- PETN shows neither effect at any point (ElapsedDays p=0.224, Study p=0.095 in the same joint model; PETN's models also repeatedly hit boundary/singular fits) -- the RDX-specific (not general-instrument) nature of the effect holds up with Main data included.

**Relationship to the existing "two-prep" investigation** (see "Is it the internal standard, or the analyte itself?" session below, which tested and *rejected* a Prep A/Prep B two-standard hypothesis *within* the Pilot Study alone): that test was at the wrong granularity to catch this. It never had the opportunity to test a reset specifically at the Pilot→Main *study* boundary, since the Main Study didn't exist yet at the time. This result reopens the fresh-stock/fresh-calibration-standard hypothesis, just one level up from what was previously checked and ruled out.

**Caveats**: small, sparse sample (n=49 combined, only 7 distinct calendar dates, one date with n=1); flagged explicitly as a lead worth checking against real lab records, not a settled conclusion.

### 3. `Standard` (Stock Prep.Vial) Column Added to `main_study_data_nested.xlsx` -- Directly Tests (and Does NOT Support) the "Fresh Main Study Stock" Hypothesis

User updated `main_study_data_nested.xlsx` with a new `Standard` column recording which physical spike-stock preparation and vial was used per sample (format `prep.vial`, e.g. `"3.1"` = 3rd preparation, 1st vial). This directly tests the fresh-stock-reset hypothesis proposed in #2 above, using real records instead of an assumption.

**Data found** (`Standard` values present in the Main Study only -- no equivalent column exists yet in `pilot_data_nested.xlsx`, see "Still open" below): three values in use, **not** a single fresh Main-Study-wide prep as originally guessed --

| Standard | n | Date(s) | Pressure levels used | Mean PETN | Mean RDX |
|---|---|---|---|---|---|
| 2.2 | 10 | Both 08-14 AND 08-21 | mostly 200/300g | 21.6% | 24.4% |
| 3.1 | 18 | 08-14 only | mostly 10/100/300g | 15.6% | 14.6% |
| 3.2 | 16 | 08-21 only | mostly 10/50/100g | 12.1% | 19.2% |

**Critical issue found**: `Standard` is **confounded with nominal pressure level** (not independently/randomly assigned across the design) -- `2.2` is disproportionately used on 200/300g samples (which already tend toward higher recovery for unrelated pressure-response reasons), while `3.1`/`3.2` cover more of the lower-pressure samples. The raw "2.2 has higher recovery" pattern in the table above could simply be reflecting *which pressures* it happened to be used on.

**Tested properly** (likelihood-ratio test: does adding `Standard` to a model that already controls for actual pressure linear+quadratic and Surface_type improve the fit?), Main Study data only (n=34-35, since `pilot_data_nested.xlsx` has no `Standard` values to include):
- **RDX: no improvement at all** (χ²=1.12, df=2, **p=0.572**) -- individual `Standard` coefficients far from significant (p=0.47, p=0.92 vs the `2.2` reference level).
- **PETN: no significant improvement** (χ²=3.84, df=2, **p=0.146** -- one borderline coefficient, `3.2` vs `2.2`: p=0.061).

**This is the opposite of what the fresh-stock-reset hypothesis (#2 above) predicted.** If a fresher prep genuinely boosted RDX recovery, the higher-numbered (nominally newer) `3.1`/`3.2` should show *higher* recovery than `2.2` -- instead `2.2` is numerically the *highest* of the three (though not statistically distinguishable from the others once pressure/surface are properly accounted for). **Conclusion: within the Main Study itself, there is no reliable evidence that stock/vial identity drives recovery differences beyond what pressure and surface already explain**, and the specific mechanistic story proposed in #2 (a fresh Main-Study-wide prep explaining the Pilot→Main rebound) is not supported by the real prep/vial records -- the `Study`-level rebound itself (from #2) remains real and statistically supported, but its cause is now back to being unexplained rather than attributed to stock freshness.

**Still open / next step**: `pilot_data_nested.xlsx` has no equivalent `Standard` column yet. To properly test whether the Pilot's own already-documented Prep A (07-10, 07-22)/Prep B (07-30, 08-04) split (see "Is it the internal standard, or the analyte itself?" session below) maps onto this same `prep.vial` numbering scheme -- i.e. whether Main's earliest-used `"2.2"` is literally a continuation of the Pilot's own physical Prep B, just its second vial -- the same `Standard`-style labelling would need to be added to the Pilot Study's own records too, if they exist (lab notebook or equivalent). Flagged to the user as an open question; not resolved this session.

### Files Created/Modified

`main_study_analysis_actual_pressure.R` (new), `main_study_analysis_summary_ACTUAL_PRESSURE.txt` (new, in `Main Study/`), `rdx_batch_effect_with_main_study.R` (new) -- all three self-contained, none of them modify or overwrite `main_study_analysis.R`, `pilot_analysis.R`, or any of their existing outputs. `main_study_data_nested.xlsx`'s new `Standard` column was added directly by the user (not by this session) and was only read/analysed here, not modified. `CONTEXT.md` (this entry).

---

## Session Summary (August 19, 2026, continued yet further) — RDX Drift Correction Determined Unnecessary (the Internal Standard Already Serves This Function; the Observed Effect Is Not a DC Issue)

### Determination

**User's explicit, authoritative determination**: RDX does **not** need -- and should **not** receive -- a post-hoc drift correction of the kind PETN has. This retracts/supersedes the "Recommended next step (not yet done): Build an RDX drift correction..." item from the RDX batch-effect investigation (below), which had proposed exactly that.

### Rationale

RDX in this pipeline is quantified via the **15N-RDX internal standard ratio** (`use_is_for_rdx = TRUE`, `GlobalCode.R`) -- `rdx_pa / rdx_is_pa`, not raw peak area alone. Correcting for instrument-response drift between injections/calendar dates (inlet degradation, ionization efficiency changes, injection-to-injection sensitivity variation) is the **entire purpose** of an internal standard: since the same known amount of 15N-RDX IS is spiked into every Cal/QC/Sample/NC injection, any shared multiplicative drift affecting the instrument's response cancels out in the ratio by construction. This is not a novel claim specific to this session -- it is the same conclusion this file's own "Filtering Study Results" and "Is it the internal standard, or the analyte itself?" sections already reached independently: the RDX/15N-RDX ratio held to ~1.2% RSD across 19 QC injections in the Method Dev dataset despite raw `rdx_pa`/`rdx_is_pa` both declining by ~49%, and the pilot's own analyte-vs-IS signal-chain check confirmed the IS stays comparatively stable while the analyte itself genuinely declines -- i.e. the ratio method was already independently verified to be "doing its job correctly."

**PETN is the genuinely different case, and the reason a drift correction exists for it at all**: PETN's own internal standard (NG) is frequently undetected or unreliable (documented extensively elsewhere in this file, e.g. "GC-MS Signal Loss During Sample Sequences", the Filtering Study Results' "NG is a suboptimal IS for PETN quantification" conclusion), forcing this pipeline to fall back to **raw peak-area-only quantification** for PETN (`use_15nrdx_for_petn = FALSE`/PA-only mode). With no functioning IS to divide out instrument drift, PETN needed an explicit substitute mechanism -- the QC-based power-law drift correction (`apply_petn_drift_correction`, `Code/03_Quantification.R`). RDX has never been in this situation: its own IS (15N-RDX) works, is reliably detected, and already performs the equivalent correction implicitly, every single injection, via the ratio itself.

**Correction to this file's own earlier framing** (the "New leading candidate: uncorrected RDX quantitation drift" session, below): that session's text stated "RDX has no drift correction applied in this pipeline (unlike PETN, which already gets one)... Any real instrument drift therefore flows straight into reported RDX recovery, uncorrected" -- this framing is now understood to be **incorrect**. It described the IS-ratio method as if it were an absence of correction, when the ratio method is itself the correction. The calibration-QC "6ng RDX %bias" trend that session documented (Analysis1 -0.3% -> Analysis7 -13.7%) is real and reproducible, but it is a bias observed **after** ratio-based quantification has already been applied to those very calibration standards (the ratio method is used identically for Cal/QC/Sample rows, not switched on only for samples) -- so it cannot be "uncorrected instrument drift flowing straight through," since the correction (division by the co-injected IS) has already happened by the time that bias is measured. Whatever is producing that residual calibration-standard-level trend, it is **not a drift-correction (DC) problem**, and building an RDX-side drift correction analogous to PETN's would not address it -- at best redundant (re-correcting an already-corrected ratio), at worst actively distorting an already-correctly-normalized value.

### What This Means for the RDX Batch/Date Effect Investigation

The RDX batch/date effect itself is **not retracted** by this determination -- it remains real and (per the reanalysis-selection-bias session) still statistically significant after that partial correction (p=0.027). What changes is which explanatory buckets remain viable:
- **"RDX quantitation drift from Analysis4 onward"** (previously listed under "Shows independent signal but doesn't (alone) explain the effect") is reclassified below -- the underlying calibration-standard-level trend is still real and worth understanding (see "Still open" below), but it is **not a fixable-via-drift-correction issue**, and is **not** evidence that reported RDX sample recovery values are running "uncorrected" the way PETN's once were.
- This pushes weight back toward the **genuine recovery/analyte-level explanations already investigated**: spike-stock degradation (already ruled out, single or two-prep), and the still-open physical/chemical possibilities (RDX-specific calibration/QC standard stock stability -- i.e. whether the *standard solution itself* partially degrades over the ~3-4 week pilot span, independent of instrument response; storage conditions during the extraction delay).

### Files Modified

`CONTEXT.md` (this entry; also directly annotated/corrected the "New leading candidate: uncorrected RDX quantitation drift", "Recommended next step (not yet done)", and "Updated status of all candidates" sections within the RDX batch-effect investigation below, rather than leaving the superseded framing to stand unqualified).

---

## Session Summary (August 19, 2026) — `main_study_analysis.R` Created (Collation, Pilot Pooling, Preliminary Plots, Non-Circular Power Analysis)

### Motivation

Direct follow-up to `doe_nested_design.R`'s own "Still To Do" list (below): the main study's first real GC-MS data (`Analysis1`, one batch's worth) had arrived, and the user wanted (1) a working analysis pipeline to turn it into recovery values and QC status exactly like the pilot's own `pilot_analysis.R`, (2) a quick "how's it looking so far" preliminary view usable with partial data, and (3) eventually the full pooled pilot+main combined model specified in CONTEXT.md's own spec (`Recovery ~ Surface_type * (Pressure_g + I(Pressure_g^2)) + Study + (1|SurfaceID_nested)`).

### `main_study_analysis.R` -- Implementation

New script, structured to mirror `pilot_analysis.R`'s own conventions as closely as possible (same shared `GCMSQuantitation/Code/InjectionAcceptance.R` QC-acceptance logic, same recovery-calculation constants, same `select_best_attempt()`/reanalysis-dedup logic copied verbatim, same conditional-formatting `add_sheet()` helper) rather than inventing a parallel approach:

1. **Collation** (`collate_gcms_results()`): discovers `Analysis*/Results/*_GCMSResults.csv` under a configurable `gc_data_dir` (confirmed by the user to be `.../ASTRA Swabbing/Main Study/GC Data`, exactly matching the path this script had already guessed), classifies rows via `^MAIN_(\d{3})$` (samples) and `^(?:MAIN_)?(Steel|ABS)Batch(\d+)[ _]?(?:NegCtrl|NC)$` (NCs, same flexible-regex convention as the pilot's own regex-drift fix), computes recovery with the identical `Below_LOD`/`Below_LOQ`/no-peak -> 0% legitimate-negative treatment as the pilot, and writes `main_study_data_nested.csv`/`.xlsx` + `main_study_results.xlsx` (All Data/Steel/ABS/Negative Controls/Summary_By_Pressure/SampleSummary/CalSummary sheets). Simpler than the pilot's equivalent in one respect: `main_study_design_nested.csv` already has `TestOrder` == the design's own `BatchNumber` and already includes NC rows in the same column structure as samples (per `doe_nested_design.R`'s own design decisions), so a single RunID-matched update loop handles both sample and NC rows -- no separate NC-row construction needed, unlike the pilot.
2. **Pilot pooling** (`build_pooled_dataset()`): pools the pilot's own wet-only 50g/200g rows (`pilot_data_nested.csv`, `Solvent_level=="present"`, `Pressure_g %in% c(50,200)`, QC-filtered) with the new main-study data, adding the `Study` (Pilot/Main) covariate specified in CONTEXT.md's own spec. `SurfaceID_nested` is deliberately `Surface_type:SurfaceID` (NOT including `Study`) so the pilot's own reused physical surfaces (Steel_01-04/ABS_01-04) are correctly treated as the SAME physical surface across both studies in the random-effects structure -- exactly the pooling logic `doe_nested_design.R`'s design was built around.
3. **`CumulativeUseNumber`** (approximate): built from a combined pilot+main "event log" (samples + NCs, both studies), ordered by Study (Pilot always before Main) -> BatchNumber/TestOrder -> NC-before-sample tiebreak within a batch (matching the documented "NCs taken at the START of their own batch's session" convention), with `CumulativeUseNumber = row_number()` per physical `SurfaceID`. Explicitly flagged in code comments as an approximation (a fully rigorous version would need the pilot's own two-tier NC-matching approach from `Diagnostics/investigate_rdx_batch_effect.R`, extended across both studies) -- cross-checked against `main_study_surface_use_tally.csv`'s own final `TotalUses` per surface as a sanity bound (never exceeded in practice).
4. **Combined mixed-effects model** (`fit_combined_model()`): fits the exact CONTEXT.md-specified formula per analyte, with a graceful "not enough data yet" fallback (needs >=10 observations, >=4 distinct `Pressure_g` levels, both surface types) rather than crashing `lmer()` on rank-deficient partial data. Includes the `CumulativeUseNumber` covariate check via a likelihood-ratio test (base model vs. +covariate), Type III ANOVA, `effectsize::eta_squared()`, EMMs at Pressure_g=10/50/100/200/300, and `isSingular()`/Shapiro-Wilk diagnostics -- same pattern as the pilot's own `analyze_analyte()`.

### Preliminary Plots -- Iterated Per Explicit Feedback

1. **Initial version**: scatter (jitter) of Recovery vs Pressure_g, faceted by `Surface_type`, one PNG per analyte (`PETN_main_preliminary.png`/`RDX_main_preliminary.png`), new main-study data only.
2. **Box plots requested instead of scatter**, **and** PETN/RDX combined into two facet columns rather than two separate files: rebuilt as a single `PETN_RDX_main_preliminary.png`, long-format-reshaped data, `facet_grid(Surface_type ~ Analyte)` (rows = Steel/ABS, columns = PETN/RDX), box + jittered points + red mean diamond + `n=` labels per cell.
3. **Pilot's 50g/200g data incorporated into this same plot** (not just the model): moved the pilot-pooling step (originally Section 4, after the plots) to BEFORE the preliminary-plot section, so the plot draws from `combined_data` (all 5 pressure levels: pilot contributes 50g/200g, main contributes 10/50/100/200/300g) instead of main-study-only data. Points coloured by `Study` (orange=Pilot, blue=Main) via `scale_colour_manual()` so provenance stays visually explicit; falls back gracefully to main-only data (grey subtitle note) if the pilot file isn't found.

### Real-Data Verification (`Analysis1`, 24 main samples + 4 NCs)

Ran end-to-end against the confirmed real path (`.../Main Study/GC Data`, `Analysis1` only, one batch's worth so far). Results: 24 main-study samples classified (1 extraneous "0.2ng RDX PETN" response-check row correctly excluded), 20 PASS / 4 FAIL (`PETN 6ng QC bracket FAILED or unavailable`) for samples, NC Stage 1: 2 PASS* / 2 FAIL. `main_study_data_nested.csv`/`.xlsx` and `main_study_results.xlsx` written successfully to the OneDrive Main Study folder. Pooled dataset: 16 pilot rows + 40 QC-passed main rows = 56 total, spanning all 5 pressure levels and both surface types -- enough to clear `fit_combined_model()`'s minimum-data floor, so `analysis_mode` was switched from the initial safe default (`"preliminary"`) to `"full"` to actually exercise the combined-model/power-analysis code paths against real data.

**Combined model results** (n=36 per analyte after restricting to non-NA recovery + Pressure_g): both `Pressure_g` and `I(Pressure_g^2)` highly significant for both analytes (p<0.0001), `Surface_type:Pressure_g` and `Surface_type:I(Pressure_g^2)` interactions significant for both (p<0.01 PETN, p<0.05 RDX) -- directly engaging the pilot's own previously-unresolved "does the Pressure shape differ by surface" question. `CumulativeUseNumber` likelihood-ratio test: not significant for either analyte (p=0.97 PETN, p=0.45 RDX) -- no evidence yet that physical surface wear needs to be controlled for, though this is from a small, partial sample. Residual and random-effect Shapiro-Wilk both comfortably non-significant (no normality concerns) for both analytes.

### Literature Check: Does the Rise-Then-Fall Pressure Curve Have Precedent?

User asked whether the observed dose-response shape (Steel recovery rising from 10g to ~100-200g, then declining by 300g) has any literature backing. Investigated via `webfetch` against DOIs already cited elsewhere in this codebase's own literature (`PhD_Work_Summary.md`'s "3 published wipe-sampling papers" behind this study's original pressure-level rationale) rather than guessing new sources:

- **Robinson, Sisco, Staymates & Lawrence (2018)**, *Anal. Methods* 10(2):204-213, DOI: 10.1039/C7AY02694C (full text read via the open-access NIST manuscript on PMC, PMC5986101) -- one of the same "3 papers" already cited for this study's original 50g/200g pressure rationale. Directly tested RDX collection efficiency vs. applied load (260-1060g) on ABS/leather/nylon: CE% rises sharply then plateaus ("applied loads greater than 460g provided little improvement, indicating a modest force will result in optimal collection"), with the explicit mechanism given being **re-deposition of collected analyte back onto the surface under excessive/prolonged intimate contact** -- and they demonstrate an actual measured *decline* (not just a plateau) for a related parameter (CE% dropped from 50.8% to 37.8% as wipe dwell distance increased from 5cm to 20cm), attributed to the same re-deposition mechanism.
- **Caveat given directly to the user**: within Robinson et al.'s own tested force range, no surface showed an actual decline (only rise-then-plateau) -- the falling part of this study's own curve (100g->300g on Steel) sits at a higher force / different surface (rigid steel, not ABS/leather/nylon) than they tested, so it's not directly contradicted by their data, but also not directly confirmed by it. Two other candidate papers already cited elsewhere in this codebase (Chaffee-Cipich et al. 2016 *Forensic Sci. Int.* "Contact between traps and surfaces..."; Novosselov et al. 2021 *Talanta*, the TESSA study) could not be verified via full text (paywalled, no OA copy found) -- flagged as worth chasing directly via institutional access rather than assumed.

### Power Analysis: Retrospective (Circular) vs. A Priori/MDES (Non-Circular)

User asked whether n=6/level/surface-type (the main study's own design target) would be sufficient for >=80% power, using the data collected so far.

**Section 6 -- per-effect retrospective power** (`run_power_analysis()`): mirrors the pilot's own established "PER-EFFECT POWER ANALYSIS" convention (optimistic = point-estimate Cohen's f from partial eta^2; conservative = Cohen's f from that effect's own 95% CI lower bound; `pwr::pwr.anova.test()` with `k` mapped per effect -- 5 for `Pressure_g`/`I(Pressure_g^2)`, 2 for `Surface_type`, 10 for the interactions). Real-data result: optimistic power comfortably clears 80% for the Pressure linear/quadratic terms and both interactions; conservative power is well below 80% for most effects (as low as 12% for RDX's `Surface_type:Pressure_g` interaction) -- i.e. genuinely inconclusive on the current small, partial sample.

**Follow-up question from the user**: this retrospective approach is circular (the dataset is judged against a criterion -- its own estimated effect size -- derived from that same dataset). Confirmed this is a real, well-documented problem (Hoenig & Heisey 2001, "The Abuse of Power", *Am. Statistician* 55(1):19-24: observed power is mathematically just a monotonic transform of the p-value, not new information -- significant results always look "adequately powered" retrospectively regardless of the true effect).

**Section 6b -- a priori/MDES power check** (`run_apriori_power_check()`, new, non-circular): resolves this exactly the way the pilot's own script already solved the identical problem once before (`pressure_literature_effect_pct`, an externally-anchored, non-data-derived target) -- new `meaningful_recovery_difference_pct <- 10` config constant (matching the pilot's own established value), combined with the ACTUAL residual+between-surface variance from the fitted model (legitimate to take from the data, since it's a nuisance/noise parameter, not the effect being judged) via a new `get_overall_sd()` helper (`VarCorr()`-based). Reports both directions: (1) power to detect the externally-set 10-percentage-point target at n=6/level, and (2) the Minimum Detectable Effect Size (MDES) at n=6/level, 80% power, given the noise actually observed -- explicitly instructed to be compared directly against the real observed effect+CI from Section 6, rather than collapsed into another circular yes/no verdict. Real-data result: ~7-point noise SD for both analytes -> only 34% power to detect a 10-point target at n=6, MDES ~17 points at n=6 -- compared directly against Steel's own observed ~25-point EMM range (10g->200g), which comfortably exceeds the MDES, giving an honest (if still small-sample) basis for "n=6 looks adequate for an effect of the size actually being seen, but would not reliably catch a substantially smaller one."

**Explicitly flagged limitation**: both Section 6 and 6b still use the `pwr::pwr.anova.test()` one-way-ANOVA approximation (treating the 5 pressure levels as `k` groups) as a stand-in for the actual quadratic mixed model -- a reasonable planning-level approximation (same convention as the pilot's own established approach) but not a full simulation-based power analysis of the real model. A more exact version would need the `simr` package (checked: not currently installed).

### Files Created/Modified

`main_study_analysis.R` (new -- collation, pilot pooling, `CumulativeUseNumber`, combined mixed-effects model, preliminary box plots, retrospective + a priori/MDES power analysis), `main_study_data_nested.csv`/`.xlsx` (new, written to `.../Main Study/`), `main_study_results.xlsx` (new, written to `.../Main Study/GC Data/`), `PETN_RDX_main_preliminary.png` (new, written to `.../Main Study/`), `main_study_analysis_summary.txt` (new, written to `.../Main Study/`, generated when `analysis_mode <- "full"`), `CONTEXT.md` (this entry).

### Still To Do

Re-run `main_study_analysis.R` as Batches 3-5 of the main study are collected (script is incremental/idempotent, safe to rerun any time); revisit the retrospective vs. a priori power comparison once the full n=6/level dataset is in, per the resolution documented above (compare the real observed effect+CI against the MDES, not a circular power percentage). `simr` is now installed (see below) but not yet wired into `main_study_analysis.R` for a genuine simulation-based power check of the real quadratic mixed model -- still using the `pwr.anova.test` one-way approximation.

---

## Session Summary (August 19, 2026, continued) — `main_study_protocol.txt` Drafted; `simr` Installed

### Motivation

Direct follow-ups from the session above: (1) the user asked what `main_study_protocol.txt` actually was, since it had only ever appeared as a name in a "still to do" list with no description; (2) the user asked to install `simr`, flagged in that same session as the package needed for a more rigorous, non-approximated power analysis of the actual quadratic mixed model.

### `main_study_protocol.txt` -- What It Is and What Was Drafted

Explained first: this is the main-study analogue of the pilot's own `batch_protocol.md` (read in full before drafting, to match its structure exactly) -- a printable, step-by-step LAB EXECUTION checklist, not an analysis script. Originally scoped in this file's own "Requirements for Full DOE Script" section (`main_study_protocol.txt - Detailed protocol`), written before `doe_nested_design.R` existed, and never revisited since.

Drafted as a close structural mirror of `batch_protocol.md` (materials checklist, pre-experiment setup/calibration, batch-by-batch spike->dry->test->clean sequences, QC checks, troubleshooting, timeline, safety notes, sign-off), but updated for the main study's actual design rather than the pilot's:
- **5 pressure levels** (10/50/100/200/300g) instead of 2 -- new calibration table with force (N) and pressure (kPa) computed by linearly scaling the pilot's own contact-area-derived 50g->6.7kPa / 200g->27kPa anchors (10g~1.3kPa, 100g~13.4kPa, 300g~40.2kPa); cross-checked the resulting force range (0.10N->2.94N, a 2.84N span) against `doe_nested_design.R`'s own previously-documented "10-300g range (2.84N)" figure -- matches exactly.
- **5 batches, variable surface count**: Batches 1-3 (all 12 physical surfaces: 8 reused + 4 new) vs. Batches 4-5 (only the 4 new surfaces, testing their own 50g/200g since the reused surfaces already have this from pilot pooling) -- pulled directly from `main_study_batch_checklist.csv` and `main_study_design_nested.csv`, not invented from scratch.
- **NCs already built into the design** (unlike the pilot, where they had to be hand-inserted) -- protocol instructs swabbing the NC's host surface FIRST within each batch's surface-type group, before that group's spiking begins, matching the already-documented "NCs taken at the START of their own batch's session" convention and the design file's own real execution order.
- **Batches 1-2 drafted as a COMPLETE historical record**, not a blank checklist: `main_study_design_nested.csv` already had real `Test_DateTime`/`Temp_C`/`Humidity_pct`/`Time Swabbed` values recorded for these two batches (executed 12-13 Aug 2026, per the previous session's own real-data verification against `Analysis1`) -- transcribed directly into per-batch tables rather than leaving them as unfilled blanks. **One data-quality issue caught while transcribing**: `MAIN_023`'s `Temp_C` field in the live CSV contains `"07:12"` (a time-like value, not a temperature) -- almost certainly a transposed data-entry error against an adjacent time column. Flagged explicitly in the protocol document itself (does not affect recovery calculation, only that one row's environmental-conditions record) rather than silently corrected, since the actual intended value isn't recoverable from the file alone.
- **Batches 3-5 drafted as pending fill-in checklists** (RunID/SurfaceID/Pressure_g/NC host already pre-filled from the fixed design, since those are already determined; only Swabbed time/Temp/RH/deviations left blank for actual execution).
- Reused-surface wear note added to both the surface-prep and troubleshooting sections, referencing the already-completed reuse-trend check (no degradation found up to 6 uses; this study pushes reused surfaces to ~7-8 total uses including NCs).

**Not independently verified against a second source** (unlike code changes, this is a lab-execution document with no automated test) -- the batch-by-batch sample tables were cross-checked by eye against both `main_study_batch_checklist.csv` and `main_study_design_nested.csv` directly (both read in full before drafting) to catch transcription errors; the calibration table's kPa figures were cross-checked against `doe_nested_design.R`'s own documented Newton range as described above.

### `simr` Installation

Installed via `install.packages("simr", repos="https://cloud.r-project.org")` -- pulled in `binom` (binary) and `RLRsim` (binary) as dependencies, then compiled `simr` itself from source (no binary available for this R version at the time) with no errors. Verified `library(simr)` loads cleanly, `packageVersion("simr")` reports `1.0.10`. Not yet wired into `main_study_analysis.R` -- Section 6/6b's power analysis still uses the `pwr::pwr.anova.test()` one-way-ANOVA approximation; a genuine `simr`-based simulation power check of the actual fitted `lmer()` model (with the target effect size set to an externally-anchored value, not the observed one, per the same non-circularity principle as Section 6b) remains a future task.

### Files Created/Modified

`main_study_protocol.txt` (new), `CONTEXT.md` (this entry). R environment: `simr` 1.0.10 + `binom`/`RLRsim` dependencies installed (no project files changed by this).

---

## Session Summary (August 11, 2026, continued) — Main Study Design: Multi-Level Pressure Dose-Response (`doe_nested_design.R` Created)

### Motivation

Moving on from the pilot to the full/main DOE design. The pilot's own `pilot_analysis_summary.txt` flagged Pressure's effect as genuinely unresolved (95% CI on Cohen's f touched zero for both analytes) -- a 2-level (50g/200g) contrast cannot distinguish "no effect" from "small effect, underpowered," nor characterise the *shape* of any real relationship. Raw pilot means also hinted at an unresolved possibility the pilot could never properly test: steel recovery trending up with pressure while ABS trends down (a `Surface_type:Pressure_level` interaction that was never significant at n=32 -- correctly identified mid-discussion as a genuinely open question, not something already "resolved" as an earlier assistant summary incorrectly implied).

### Design-Logic Discussion (before any code was written)

Worked through, in order:

1. **Why 2 levels is insufficient**: cannot distinguish linear/threshold/plateau/non-monotonic dose-response shapes.
2. **Proposed levels**: 10g, 50g, 100g, 200g, 300g (user-proposed, log-ish spacing -- denser at the low end, extends 100g beyond the pilot's previous "high"). Recommended treating `Pressure_g` as a **continuous covariate** (`Recovery ~ Surface_type * (Pressure_g + I(Pressure_g^2))`) rather than an ordered categorical factor, since the uneven spacing breaks `contr.poly()`'s equal-spacing assumption.
3. **Literature-informed effect size at the wider range**: rescaling the pilot's own "~6% absolute difference, 4%/Newton" literature anchor from the old 50-200g range (1.47N) to the new 10-300g range (2.84N) gives an expected target of **~11.6%**, roughly doubling Cohen's f (0.58 -> 1.08-1.12, "large" effect territory) -- meaning far fewer samples are needed than the pilot's own 2-level recommendation once the range is widened.
4. **Sample budget**: ~100 max, ideally less. Computed `pwr::pwr.anova.test`-based minimum n per level: as few as n=4/level under the optimistic literature-scaled effect, up to n=9-10/level if the true effect is only half that (a real possibility if the relationship plateaus rather than staying linear across the full range, which is exactly what this study is designed to check). Landed on **n=6/level** as a deliberate middle ground, not fully trusting the optimistic extrapolation.
5. **D-optimal / unequal-n discussion**: for a linear+quadratic model, weighting replicates toward the two extremes (most leverage for the slope) while keeping a moderate mid-point (curvature/lack-of-fit) is more efficient than uniform n. Combined with a second, independent reason: **50g and 200g are the pilot's own original levels** -- 16 existing wet-only samples (4 steel + 4 ABS at each) can be pooled in rather than re-collected. Computed the actual leverage gain (Σ(x-x̄)²): an extreme-weighted 28-sample/type design (8/3/6/3/8 across 10/50/100/200/300g) gave ~15% more leverage than a uniform 30-sample/type design, using fewer samples.
6. **User's simplification**: keep n=6 *everywhere*, but only collect 2 *new* samples at 50g/200g (relying on the 4 existing pilot reps to reach 6 combined) -- confirmed this is exactly why 2 new + 4 old = 6, matching the other levels' 6-new-only.
7. **Structural resolution**: unequal new-sample allocation (6/2/6/2/6) is incompatible with a strict "every surface tested at all 5 levels" design (that forces equal n per level by construction). Initially proposed a Group A/B incomplete-block split using brand-new surfaces; **user's better idea**: reuse the pilot's own still-available physical surfaces as the "backbone" instead. Since the 4 pilot surfaces/type already have 50g/200g data, testing them at the 3 new levels (10/100/300g) makes each contribute 1 sample to *all 5* levels automatically; adding just **2 brand-new surfaces/type tested at all 5 levels** brings every level to n=6 with no incomplete-block complexity at all. This uses only 4 new physical surfaces total (2 steel + 2 ABS) instead of 8-16.

### Surface-Reuse Trend Re-Check (before finalizing the reuse plan)

Reusing the pilot surfaces pushes them to 7 total uses (4 pilot conditions + 3 new pressure tests) -- one step beyond the 6-use maximum previously investigated. Before accepting this, re-ran the reuse-trend check with a critical fix: **`pilot_data_lab_notes.xlsx` was found to be stale**, still reflecting the pre-`select_best_attempt()`-fix (most-recent-attempt) recovery values for all 17 reanalysed samples (confirmed directly: `RDX_Recovery_pct` mismatches between the lab notes file and the live `pilot_data_nested.csv` for e.g. `PILOT_004`/`PILOT_032`). New diagnostic `Diagnostics/reuse_trend_check_corrected.R` patches in the corrected recovery values (from `pilot_data_nested.csv`, matched by `RunID`) before rebuilding the physical-surface usage history and testing the trend.

**Result**: no significant trend for RDX (p=0.14 alone / p=0.48 controlling for `BatchNumber`) or PETN (p=0.27); Spearman rho ~ -0.04 for both (essentially zero, wrong sign for degradation even if real). **Bonus finding**: the pilot's own design already tested some surfaces up to **6 total uses** (not just 4), because negative controls reuse the same physical surfaces -- so reusing pilot surfaces a 7th time is only one step beyond the already-validated range, not a much larger extrapolation. This directly enabled relaxing the original "conservative guess" of a ≤3-reuse limit.

### `doe_nested_design.R` -- Implementation

New script, generates the main study design from scratch each run (`set.seed(20260811)`):

- **5 pressure levels**: 10, 50, 100, 200, 300g. `shared_levels` (10/100/300, tested by every surface, Batches 1-3) vs `new_only_levels` (50/200, tested only by the 2 brand-new surfaces/type, Batches 4-5, since the reused pilot surfaces already have this data).
- **Reused pilot surfaces** (`Steel_01-04`, `ABS_01-04`): each gets 3 new rows (own random permutation of `shared_levels` assigned to Batch slots 1-3, mirroring the pilot's own per-surface `TestOrder` randomization).
- **New surfaces** (`Steel_05-06`, `ABS_05-06`): each gets 5 rows (3 shared levels randomized to Batches 1-3, plus 2 new-only levels randomized to Batches 4-5).
- **Negative controls**: 1 per surface type per batch (10 total), host surface drawn randomly pooling reused+new surfaces active that batch. **Capped at max 2 NC hits per physical surface** across the study -- added after an initial run randomly concentrated 4 of 5 ABS NCs onto a single surface (`ABS_05`, reaching 9 total uses); user chose the cap-at-2 option over re-rolling the seed or accepting it as-is. Re-running with the cap brought the max down to 8, spread across 4 surfaces instead of 1.
- **Physical surface use tally**: explicit diagnostic table (pilot uses + new sample uses + NC hits = total), flags any surface exceeding the validated 6-use range and recommends including `CumulativeUseNumber` as a covariate in the eventual `main_study_analysis.R`.
- **Existing pilot data pooling reference**: printed reminder of exactly which rows to pool (`pilot_data_nested.csv`, `Solvent_level=="present"`, `Pressure_g %in% c(50,200)`, 16 rows) plus an explicit caution to include a `Study` (Pilot vs Main) covariate in the combined model, given the already-documented RDX date-linked drift.
- **Validation checks**: sample counts, per-level new+pooled totals (confirms all 5 levels reach the target combined n=12, i.e. 6/surface type), per-surface new-test counts (3 for reused, 5 for new).

**Verified**: all checks pass -- 44 new samples (24 from 8 reused surfaces x 3 levels, 20 from 4 new surfaces x 5 levels), 10 NCs, every pressure level reaches combined n=12 after pooling.

### Output File Structure -- Iterated Per Explicit Feedback

Several rounds of refinement to match the pilot's own conventions exactly:

1. **Sample naming**: `SampleID` initially used a descriptive format (`M_Steel_06_300g`); corrected to exactly equal `RunID` (`MAIN_001`, `MAIN_002`, ...), matching the pilot's plain sequential GC-MS-facing identifier convention (`PILOT_001` etc.) -- descriptive info lives in separate columns instead. NC naming kept descriptive (`MAIN_SteelBatch1_NC`), matching the pilot's own NC convention (needed so NCs stay regex-distinguishable from regular samples in a future `main_study_analysis.R`).
2. **Column structure**: the *live* `pilot_design_nested.csv` had evolved since its original generation (user had manually added `Time Cleaned`/`Time Spiked`/`Time Swabbed`/`Date Extracted` columns during actual pilot execution, and dropped `PETN_Recovery_pct`/`RDX_Recovery_pct`/`Notes`) -- re-read the live file directly rather than relying on the original generation script, and matched `main_study_design_nested.csv` to this current 18-column structure exactly.
3. **Negative controls included directly**: unlike the pilot (where `pilot_design_nested.csv` never included NC rows at all -- they had to be written in by hand during execution, a manual step the user explicitly wanted to avoid this time), all 10 NC rows are included directly in `main_study_design_nested.csv` from the start, with design-specific columns (`Pressure_level`/`Pressure_g`/`Solvent_level`/`Solvent_detail`) left blank for them.
4. **NC `SurfaceNumber`**: initially `NA` for NC rows in `main_study_batch_checklist.csv`; fixed to show the *same* `SurfaceNumber` as the NC's host surface's own sample row (looked up by matching `SurfaceID` within the same `BatchNumber`+`Surface_type`), so the checklist directly states which numbered surface to swab for the NC.
5. **NC ordering**: initially interleaved by `SurfaceNumber`; fixed to list the NC **first** within each batch's surface-type group (before Surface 1), reflecting that NCs are physically collected before spiking begins for that group.
6. **File consolidation**: `main_study_design_nested.csv` (correct columns, nominal `SurfaceProcessingOrder`/`TestOrder` sort) and `main_study_batch_checklist.csv` (correct real execution order, reduced columns) had diverged into two incomplete files. Consolidated so `main_study_design_nested.csv` is now sorted in the **real execution order** (NC first per batch/surface-type group, then Surface 1-6 in randomized `TestSequence`) while keeping its full column set -- this is now the single file to use for lab notes, matching how the user actually used `pilot_design_nested.csv` itself during the pilot (as the live working document, not the separate batch-organized file). `main_study_batch_checklist.csv` retained as a simplified/printable subset with the same row order.

### Files Created/Modified

`doe_nested_design.R` (new), `main_study_design_nested.csv` (new -- primary lab-notes file, 54 rows: 44 samples + 10 NCs, real execution order), `main_study_negative_controls.csv` (new -- NC-only reference), `main_study_batch_checklist.csv` (new -- printable execution-order subset), `main_study_surface_use_tally.csv` (new -- reuse-count diagnostic), `Diagnostics/reuse_trend_check_corrected.R` (new -- corrected reuse-trend check, patches stale lab-notes recovery values before testing), `CONTEXT.md` (this entry, "To Create (Main Study)" list updated to reflect what's now built vs still pending).

### Still To Do

`main_study_analysis.R` (pool pilot's 50g/200g wet-only data with new results, `Study` covariate, `Recovery ~ Surface_type * (Pressure_g + I(Pressure_g^2)) + (1|SurfaceID_nested)`, check `CumulativeUseNumber` as a covariate given several surfaces now exceed the previously-tested 6-use range), `main_study_protocol.txt`, `main_study_data_nested.csv` (created by filling in recovery values after GC-MS), `main_study_results.xlsx`. **Update (August 19, 2026): `main_study_analysis.R`, `main_study_data_nested.csv`, and `main_study_results.xlsx` now created and verified against real `Analysis1` data -- see that session's own summary above. Only `main_study_protocol.txt` remains outstanding.**

---

## Session Summary (August 11, 2026) — `select_best_attempt()` Now Prefers the EARLIEST Accepted Attempt, Not the Most Recent (RDX Batch Effect Partially Explained by Reanalysis Selection Bias)

### Motivation

Direct follow-up to the RDX batch-to-batch investigation (see "RDX Batch-to-Batch Recovery Investigation" session below and its several follow-ups). While building a workbook to show PETN/RDX quant from every individual analysis attempt for the 17 pilot samples that were reanalysed more than once (`Diagnostics/export_multi_attempt_quant.R`, new this session), the user asked directly: **could the multiple-analyses/reanalysis-selection mechanism itself be explaining (or exaggerating) the RDX batch/date decline**, rather than it being a real recovery effect?

### Investigation 1: Does reanalysis selection explain the RDX batch effect?

Built a read-only diagnostic (`Diagnostics/does_reanalysis_explain_rdx_batch_effect.R`) that recomputed RDX recovery for all 17 reanalysed samples using each one's **earliest** attempt instead of the officially-selected (previously: most-recent-accepted) one, and re-ran the exact `BatchNumber`/`ElapsedDays` mixed models from `investigate_rdx_batch_effect.R`. Key findings:
- 16 of 17 reanalysed samples' official value was NOT their earliest attempt; the selected value averaged **2.7 percentage points lower** than the earliest attempt (SD 2.7pp) -- consistent with `select_best_attempt()`'s old "prefer most recent PASS" rule systematically landing on more RDX-drift-biased later GC-MS runs (RDX has no drift correction in this pipeline; its bias is documented elsewhere to worsen at later calendar dates).
- Analysis delay (selected GC-MS date minus swab date) correlates strongly with swab date itself (r=-0.95, p<1e-8): early-batch samples waited far longer for their selected run than late-batch samples.
- Substituting earliest-attempt values narrowed the Batch1→4 RDX mean gap by ~29% (6.97pp → 4.92pp) and pushed the continuous `ElapsedDays` trend from p=0.020 to a borderline p=0.058 -- i.e. reanalysis selection was adding a real but partial (~20-30%) inflation to the apparent decline, not the whole story.

### Investigation 2: Is "most recent PASS" even the right rule to begin with?

Extracted the real per-attempt `analysis_accepted` evaluation (sourcing the actual shared `GCMSQuantitation/Code/InjectionAcceptance.R` used by the production pipeline, read-only, via a new `Diagnostics/why_was_earlier_attempt_rejected.R`) for all 20 rejected/earlier attempts across the 17 reanalysed samples. Result: **zero** were rejected for an RDX-specific reason -- every single rejected attempt's own RDX 6ng/0.2ng QC brackets (pre and post) were `PASS`. Rejections were 100% either a PETN-specific QC bracket failure (11/20, RDX never even evaluated due to the pipeline's IS→PETN→RDX first-failure-only order, but its own bracket data confirms it would have passed) or pure date tie-break between two already-fully-accepted attempts (9/20, no quality difference at all). Conclusion: the old "most recent PASS wins" tie-break existed to serve PETN, and was incidentally picking the *more RDX-biased* of two equally-RDX-valid attempts every time, for reasons that had nothing to do with RDX.

### Fix Applied

Per explicit user decision (kept as a single whole-sample selection, deliberately NOT decoupled per analyte): `select_best_attempt()` (`pilot_analysis.R`, ~line 480) tie-break changed from `desc(Date)` (most recent wins) to `Date` (earliest wins), among attempts of equal quality (`quality_ok` -- i.e. among PASS/PASS* attempts, or among all attempts if none are accepted). One-line change (`arrange(SampleName_trimmed, desc(..quality_ok), desc(Date))` → `arrange(SampleName_trimmed, desc(..quality_ok), Date)`), applied identically to both pilot samples and negative controls (same shared function). Comments and `report_multiple_attempts()`'s console message updated to document the rationale.

**Verified** (isolated unit test, synthetic data, before touching any live file): sample with an old FAIL + two PASSes now correctly selects the *earlier* PASS, not the later one; sample with two PASSes selects the earliest; sample with no PASS at all still falls back to earliest overall (unchanged edge case).

### Live Re-Run and Verification (real data, full pipeline)

Backed up all live outputs first (`%TEMP%\opencode\EarliestAttemptFix_Backup_20260811_094103\`), then re-ran `pilot_analysis.R` (`analysis_mode = "full"`, live default) end-to-end. Console confirmed all 17 pilot samples + 2 NCs now select their earliest accepted attempt (message updated accordingly). Compared `pilot_analysis_summary.txt` before vs after:

| Metric | Before (most-recent-PASS) | After (earliest-PASS) |
|---|---|---|
| **RDX BatchNumber effect p-value** | **0.0131** (significant) | **0.0269** (still significant, ~2x weaker) |
| RDX batch means (1→2→3→4) | 9.61 → 7.30 → 6.91 → 2.64 | 9.60 → 7.63 → 7.97 → **4.23** (Batch 1 unchanged -- its reanalysed samples were 0% in every attempt; Batches 2-4 shifted up, Batch 4 by +60%) |
| PETN BatchNumber effect | p=0.9692 (n.s.) | p=0.9721 (n.s.) -- unchanged |
| RDX/PETN Surface_type, Solvent_level effects | all significant | all still significant, similar magnitude | 
| Pressure effect (both analytes) | n.s. | n.s. -- unchanged |
| Recommended main study design | Wet-only, Pressure-focused, 16+16 surfaces (64 samples) | Same design, 14+14 surfaces (56 samples) -- smaller n, same shape |
| Full-factorial alternative | 3+3 surfaces (24 samples) | 3+3 surfaces (24 samples) -- unchanged |

**No headline conclusion flipped** (every effect that was significant/non-significant before remains so after), but the RDX batch/date effect -- the specific thing under investigation -- weakened by roughly the amount Investigation 1 predicted, confirming reanalysis-selection bias was a real, partial (not sole) contributor to the previously-reported decline.

**New secondary finding, not present before**: PETN's residual normality check (Shapiro-Wilk) flipped from marginal-pass (p=0.057) to fail (p=0.0004), with a newly-flagged outlier (`P_ABS_04_T3`, not previously flagged). Does not change any effect's significance (ANOVA reasonably robust at n=32; PETN's effect p-values remain comfortably significant) but is a new diagnostic note worth checking in `PETN_pilot_diagnostics.png` in a future session.

**Files modified**: `pilot_analysis.R` (`select_best_attempt()` tie-break direction + comments), `pilot_data_nested.csv`/`.xlsx`, `ASTRA_PilotStudyResults.xlsx`, `pilot_analysis_summary.txt`, all pilot plots (all regenerated with the corrected selection logic, verified as above, left live). New diagnostic scripts added to `Diagnostics/`: `export_multi_attempt_quant.R` (per-attempt PETN/RDX quant workbook, `pilot_data_multi_attempt_quant.xlsx`), `does_reanalysis_explain_rdx_batch_effect.R`, `why_was_earlier_attempt_rejected.R` (both read-only investigations, not part of the regular pipeline). `CONTEXT.md` (this entry).

### Updated status for the ongoing RDX batch-effect investigation (supersedes the "Updated status of all candidates" table further below)

Reanalysis-selection bias (this session) is now a **confirmed, partial contributor** (~20-30% of the previously-reported decline) alongside the already-documented uncorrected RDX quantitation drift -- both point in the same direction and likely share the same root cause (later GC-MS runs are more RDX-biased, and reanalysis disproportionately lands late-batch samples in later runs). The batch effect itself, however, **remains statistically significant even after this fix** (p=0.027) -- still not fully explained, spike-stock/physical-recovery-decline hypotheses remain open per the existing investigation.

---

## Session Summary (August 10, 2026, continued x3) — `pilot_analysis.R` Now Sources Shared `GCMSQuantitation/Code/InjectionAcceptance.R` (Duplication Removed, NC-Leakage Bug Found and Fixed)

### Motivation

Companion change to the same-day GCMSQuantitation session ("Injection Acceptance Logic Consolidated into Shared `Code/InjectionAcceptance.R`", see that repo's `CONTEXT.md`). This file's `assign_qc_brackets()`/`compute_analysis_accepted()`/`compute_nc_analysis_accepted()`/`compute_nc_result()` were near-byte-identical hand-ported copies of GCMSQuantitation's FINEX pipeline, already silently diverging (this file had independently evolved to treat non-Quantifiable results as legitimate negatives before GCMSQuantitation did). Consolidated into one shared implementation rather than porting fixes by hand indefinitely.

**New cross-repo dependency introduced**: this script now `source()`s `C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Code/InjectionAcceptance.R` by absolute path. If GCMSQuantitation is ever moved/renamed, this line needs updating.

### Implementation

Added the `source()` call in `USER CONFIGURATION`, right after package loading. Removed the local nested `assign_qc_brackets()` (was defined *inside* `collate_gcms_results()`, unlike GCMSQuantitation's top-level version -- resolved fine via R's lexical scoping once the local definition was deleted) and the local `compute_analysis_accepted()`/`compute_nc_analysis_accepted()`/`compute_nc_result()`. Kept **completely untouched**: `select_best_attempt()`/`report_multiple_attempts()` (this study's own reanalysis-dedup feature, no GCMSQuantitation equivalent), the recovery-calculation constants and `petn_treat_as_zero`/`rdx_treat_as_zero` logic (Section 5, this study's own deliberate zero-recovery treatment for `Below_LOD`/`Below_LOQ`/`NA`), the design-matrix merge, the NC/pilot-sample naming regexes, and all DOE statistics (mixed models, ANOVA, power analysis) further down the script.

Call sequence now: `evaluate_blanks(all_data)` → `assign_qc_brackets(...)` → classify pilot/NC rows → combine into `combined_for_accept` → `compute_injection_acceptance(combined_for_accept)` (replaces the old `compute_analysis_accepted()` call; also computes `Outcome` in the same call, so the old separate `sapply()` Outcome block was removed) → split back into `pilot_rows`/`nc_rows` → `select_best_attempt()` on each (unchanged) → for NC rows, `nc_analysis_accepted <- nc_rows$analysis_accepted` (aliased) + `nc_result <- derive_nc_result(nc_rows$petn_result_tier, nc_rows$rdx_result_tier, nc_rows$nc_analysis_accepted)`. `desired_cols`/`desired_cols_nc`/`add_sheet()` gained `petn_blank_bracket`/`rdx_blank_bracket` (new carryover-risk audit columns, matching GCMSQuantitation).

### Bug found and fixed during verification: NC rows leaking into the factorial-design statistics

Removing the old `analysis_accepted == "NC"` sentinel (NC rows now get a real PASS/PASS*/FAIL like everyone else, since the unified `compute_injection_acceptance()` no longer special-cases them) broke an implicit assumption two places depended on:

1. **`generate_boxplots()`'s "all data" branch** explicitly checked `data_valid$analysis_accepted == "NC"` to drop NC rows before plotting -- this string is never produced any more, so NC rows would have started appearing in the `_alldata` plots again (the exact thing the August 6, 2026 "Removed Point Colour Coding and NC/Untested" session removed). Fixed: now checks `SampleType == "NC"` instead.
2. **The main QC-acceptance filter** (`pilot_data <- pilot_data %>% filter(is.na(analysis_accepted) | analysis_accepted %in% c("PASS","PASS*"))`) previously excluded NC rows as a side effect of them carrying the literal `"NC"` string (which matched neither branch of that filter). With that string gone, an NC that happens to be QC-accepted would silently survive this filter and flow into `analyze_analyte()`'s `data_clean`. `lmer()` itself would still have dropped it via `na.omit` (NC rows have `Pressure_level`/`Solvent_level = NA`, required by the model formula), so the **mixed model was never actually at risk** -- but `generate_boxplots()`/`generate_barchart()`/`generate_repeat_barchart()`'s `Condition`-label construction (`paste0(ifelse(Pressure_level=="low",...)...)`) has no equivalent automatic safeguard and would have produced a spurious `"NA/NA"` condition bucket on the plots. Fixed by adding an explicit `filter(is.na(SampleType) | SampleType != "NC")` immediately after the QC filter -- a single choke-point fix, so no individual downstream function needs its own NC-awareness.

Caught by comparing sample counts entering the box plots before/after (old: "32/32 samples have data"; first new attempt: same message but only after the fix was applied -- before the fix, the QC-filter step's own "Excluded N" count dropped from 9 to 1, which is what surfaced the issue for investigation).

### Verification (real data: 32 pilot samples + 9 NCs)

Tested using temporary copies with `analysis_mode` forced to `"boxplots_only"` first (safe, fast), then the live script's actual `analysis_mode <- "full"` default (per explicit request, understanding this exercises the same dataset the RDX batch-effect investigation elsewhere in this file depends on):

- **`boxplots_only` mode**: 32/32 pilot samples reach the box plots in both old and new code, exactly matching pre-refactor -- confirms the NC-leakage fix works.
- **Pilot-sample transitions**: 15 `PASS*`→`PASS` (same root cause as GCMSQuantitation FINEX: 0.2ng bracket no longer checked for Quantifiable-tier samples). **Zero regressions** (no FAIL↔anything, since all 32 pilot samples were already PASS/PASS* in both versions).
- **NC transitions**: several `FAIL`→`PASS*` (same causes as FINEX: no more auto-fail-on-non-Quantifiable, IS-LOW no longer a hard failure); NC Stage-2 labels changed in the same explainable way as FINEX (tier-based classification replacing the old `concentration==0`-checked-first heuristic).
- **`full` mode**: re-ran the complete statistical analysis (mixed-effects models, Type III ANOVA, effect sizes, batch-effect check, EMMs, power analysis, main-study recommendations). **`pilot_analysis_summary.txt` came back byte-for-byte identical to the pre-refactor version** (only the generation timestamp differs) -- expected and correct, since no pilot sample crossed the accept/fail boundary (all 32 were always PASS or PASS*, before and after; only their internal PASS-vs-PASS* label changed, which the model doesn't discriminate between). **Confirms the ongoing RDX batch-effect investigation and all other DOE analysis in this file are completely unaffected by this refactor.**

Live files (`pilot_data_nested.csv`/`.xlsx`, `ASTRA_PilotStudyResults.xlsx`, `pilot_analysis_summary.txt`, all plots) now reflect the verified refactored pipeline, left live per user decision. Pre-testing originals backed up at `%TEMP%\opencode\InjectionAcceptanceVerify_ASTRA\LiveState_Before_*` (temp, not committed).

### Files modified

`pilot_analysis.R` (`source()` added; `assign_qc_brackets()`/`compute_analysis_accepted()`/`compute_nc_analysis_accepted()`/`compute_nc_result()` removed; `generate_boxplots()`'s NC-drop logic and the main QC-filter step both fixed to use `SampleType` instead of the retired `"NC"` sentinel; `desired_cols`/`desired_cols_nc`/`add_sheet()` gained blank-bracket columns), `pilot_data_nested.csv`/`.xlsx`, `GC Data/ASTRA_PilotStudyResults.xlsx`, `pilot_analysis_summary.txt`, all pilot plots (all regenerated with the new logic, verified correct, left live), this file.

---



**Status: investigation in progress, not yet concluded.** This entry documents ad hoc analysis performed directly against `pilot_data_lab_notes.xlsx` and the raw `GC Data/Analysis*` files (temporary scripts, not yet folded into `Diagnostics/investigate_rdx_batch_effect.R` -- user asked to pick this up in a later session before consolidating). No permanent files were modified this session.

### Motivation

Following on from the RDX batch-effect investigation below, the user raised a direct objection to the leading "spike stock degradation" hypothesis: two separately-prepared PETN+RDX standards were used across the whole pilot (not one continuously degrading stock), so a real stock-driven effect should show up as an **obvious step change** at the swap point, not the gradual/accelerating decline previously observed.

### Discovery: nominal `BatchNumber` is not the same as calendar date

Re-checking the shape-of-decline analysis (which had used nominal `BatchNumber` as the categorical grouping variable) revealed it was confounded: nominal Batches 2 and 3 each contain samples run on **two different real dates**, because the two known delayed catch-up samples (`PILOT_026`, `PILOT_015`) are nominally Batch 2/3 but were physically run on 08-04 (same day as Batch 4). A spike-stock swap happens by calendar session, not by nominal batch label, so all shape/step analysis was redone grouped by true `EventDate` instead.

### Testing the two-prep hypothesis directly

User confirmed the exact switch: a fresh 200 ng/µL PETN+RDX standard was made on **07-30**; the two 08-04 catch-up samples used whichever standard was on hand that day (i.e. Prep B, same as Batch 4), not a leftover Prep-A aliquot. This gives an unambiguous, non-guessed split: **Prep A = 07-10 + 07-22, Prep B = 07-30 + 08-04**.

Fitting `StockPrep` (A vs B) as a factor in the same mixed model used throughout this investigation:
- **Not significant on its own: p = 0.56** (F(1,23)=0.35). AIC 169.3 vs the fully flexible 4-date categorical model's 157.1 -- StockPrep explains almost none of the date effect.
- **No rebound at the swap.** If spike-stock degradation were the driver, a fresh standard made on 07-30 should have pushed recovery back up toward the 07-10 baseline (~24% wet-sample mean). Instead the single 07-30 sample (13.4%) sits squarely inside the *already-declined* 07-22 distribution (6.7-23.9%, mostly 7-16%), not up near 24%.
- **Recovery keeps declining within each prep at essentially the same rate.** Fitting "days since that prep's own first use" as a continuous covariate is itself significant (p=0.0046, ≈-0.45 pp RDX/day) and the swap adds nothing on top of that trend -- the decline runs straight through the confirmed swap point as if it hadn't happened.

**Conclusion: the two-separately-prepared-standard hypothesis is falsified**, exactly per the user's own reasoning. Spike-stock degradation (single stock *or* two-stock) is now ruled out as an explanation for the RDX batch effect.

### Other candidates addressed this session (user-provided information, no new lab data needed)

- **Ethanol (extraction solvent) lot**: user confirmed unchanged throughout -- **ruled out** (no lot-change mechanism possible).
- **Ambient lab temperature**: never logged; user notes the lab is environmentally controlled and expects low variation. Cannot be formally tested without data -- **downgraded to low priority**, remains an unmeasured (probably small) residual confound.
- **Cotton swab pre-cleaning**: protocol itself unchanged (overnight acetone soak) throughout. User noted "the first batch was washed in Jan" (i.e. long storage between cleaning and use for the earliest swabs) -- checked directly against the lab notes' `Swab Batch` column and confirmed this maps exactly onto the already-tested `SwabBatch` factor (`SwabBatch=1` spans 07-10 through 07-30 uniformly, `SwabBatch=2` only appears 08-04; already tested, p=0.431, ruled out in the prior session). Since every `SwabBatch=1` swab was cleaned at the same time, its "staleness" cannot be disentangled from elapsed study time with the data available -- **dead end, no further test possible without new records**.

### New leading candidate: uncorrected RDX quantitation drift (real, but smaller than first estimated)

> **[CORRECTED August 19, 2026 -- see "RDX Drift Correction Determined Unnecessary" session at the top of this file]** This section's framing below is now understood to be wrong in one specific respect: RDX's IS-ratio quantification (`rdx_pa / rdx_is_pa`) is **already** the drift-correction mechanism for RDX -- describing it as "uncorrected" (as the original text below does) incorrectly treats the ratio method as an absence of correction. The calibration-QC bias trend documented below is real and reproducible, but it is measured **after** ratio-based quantification has already been applied (Cal/QC rows use the identical ratio method as samples) -- so it is not "uncorrected instrument drift flowing straight through." It is **not a DC (drift-correction) issue**, and no RDX-side drift correction should be built to address it. Left unedited below for the historical record of the investigation as it actually unfolded.

Checked the GC-MS's own accuracy using the **calibration QC standards** (independent known-concentration check-points run in every `Analysis*` sequence -- unrelated to pilot samples or the spike stock). At the well-above-LOD **6ng level** (the reliable check-point; the 0.2ng level was excluded from this summary after a correction below):

| Analysis run | GC-MS date | Mean 6ng RDX %bias |
|---|---|---|
| Analysis1 | 07-13 | -0.3% |
| Analysis3 | 07-30 | -1.8% |
| Analysis4 | 07-31 | **-7.4%** |
| Analysis5 | 08-03 | -10.9% |
| Analysis6 | 08-04 | -10.5% |
| Analysis7 | 08-07 | -13.7% (range -18.6 to -6.3%) |

**RDX has no drift correction applied in this pipeline** (unlike PETN, which already gets one) -- CONTEXT.md's "GC-MS Data Collation" section already documents this design choice (RDX relies on raw 15N-RDX IS-ratio quantification only). Any real instrument drift therefore flows straight into reported RDX recovery, uncorrected -- and plausibly explains why **only RDX**, never PETN, shows a batch/date effect anywhere in this investigation.

Cross-checking against the pilot samples: `AnalysisFolder` shows the 08-04 swab-date samples (the lowest-recovery date in the whole study) were quantified almost entirely through `Analysis7` -- the run with the worst drift. `EventDate == 2026-07-22` is the one date split across two different Analysis runs (`Analysis3`, near-zero bias, vs `Analysis7`, -13.7% bias), giving a clean within-date natural test: Analysis3-quantified wet samples average 15.3% vs Analysis7-quantified 10.5% -- the right direction, but badly underpowered (n=2 vs n=7, Welch t-test p=0.67) to prove anything on its own.

**Self-correction during this session**: an initial pass reported a much larger "-41.6% mean bias" for Analysis7, which was wrong -- it averaged the 6ng bias together with the 0.2ng QC's bias, and three of the five 0.2ng QC points in Analysis7 are genuine non-detections (-100%, a floor/LOD effect at a near-detection-limit standard, not a proportional accuracy drift). User caught this directly by asking which bias figures were being reported. Corrected figure (6ng only, above) is -13.7% mean, not -41.6%.

**Honest net assessment**: there is a real, reproducible ~7-19 percentage-point negative bias in RDX quantification from Analysis4 (07-31) onward, entirely independent of spike stock or samples -- a legitimate, previously-unidentified, previously-uncorrected confound. But on its own it is too small to fully explain the ~80% relative decline seen in wet-sample RDX recovery (24.0% -> 3.3% across the pilot) -- it is at most a **partial contributor**, not the whole explanation.

### Recommended next step (SUPERSEDED August 19, 2026 -- not to be done)

> **This recommendation was retracted -- see "RDX Drift Correction Determined Unnecessary" session at the top of this file.** Building an RDX drift correction analogous to PETN's is not applicable: RDX already corrects for instrument drift via its 15N-RDX IS ratio, by design. Left below for the historical record.

~~Build an RDX drift correction from the existing raw calibration-standard data (mirroring the procedure already used for PETN), apply it retroactively to all pilot samples' RDX concentrations, and re-run the `BatchNumber`/`EventDate` significance test on the corrected values. If the effect shrinks substantially, that would properly partition how much of the remaining ~80% decline is quantitation artifact vs a real recovery/spike-stock/physical effect still requiring explanation. User has asked to pick this up in a later session.~~

### Updated status of all candidates (supersedes the table in the entry below; itself further updated August 19, 2026 -- see note on the RDX quantitation drift line)

**Ruled out**: ABS-specific surface degradation, within-batch injection order, ASTRA platform/config change, swab lot change, drying time, physical surface wear/reuse count, **spike stock age/degradation (single OR two-prep -- newly ruled out this session)**, **ethanol lot (newly ruled out this session)**.

**Shows independent signal but doesn't (alone) explain the effect**: extraction delay (wrong direction), humidity. ~~**RDX quantitation drift from Analysis4 onward (newly identified this session -- real but insufficient in magnitude alone)**~~ -- **reclassified August 19, 2026**: the underlying calibration-standard-level trend is still real (see note above), but is not a drift-correction/quantitation-artifact issue for RDX specifically (the IS ratio already corrects for instrument drift) -- moved to "Still open" below as an unexplained calibration-standard-level observation, not a candidate explanation for the sample-level batch effect via an uncorrected-quantitation mechanism.

**Dead ends (cannot be tested further without new data)**: cotton swab pre-cleaning storage time (confounded with elapsed study time within `SwabBatch=1`), ambient temperature (never logged).

**Still open**: RDX-specific calibration/QC standard stock stability -- whether the *standard solution itself* partially degrades over the ~3-4 week pilot span (independent of instrument response), which could still explain the calibration-standard-level 6ng bias trend documented above even though it is **not** a drift-correction/instrument-response issue (see August 19, 2026 correction above) -- and storage conditions during the extraction delay.

**Files modified**: none (ad hoc analysis only, against existing `pilot_data_lab_notes.xlsx` and `GC Data/Analysis*/Results/*_GCMSResults.csv`; not yet folded into `Diagnostics/investigate_rdx_batch_effect.R` per user's request to pick this up later), `CONTEXT.md` (this entry).

---

## Session Summary (August 10, 2026, continued) — RDX Batch-to-Batch Recovery Investigation

### Motivation

While reviewing the `"full"`-mode statistical output (after the boundary-fit crash fix below), a significant `BatchNumber` effect on RDX recovery was noticed (p≈0.013-0.03 throughout this investigation, consistently) with **no equivalent effect on PETN** at any point. User provided a new lab-notes workbook, `pilot_data_lab_notes.xlsx` (same schema as `pilot_data_nested.csv` plus `Humidity_pct`, `Time Cleaned`/`Time Spiked`/`Time Swabbed`, `Date Extracted`, `Swab Batch`, `ASTRA Platform?`), and flagged two known anomalies: samples 15 and 26 had to be collected as an extra "ABS Batch 5"; partway through the pilot a new batch of swabs was used and the ASTRA platform configuration changed (config change + a platform added).

This became a full systematic investigation, consolidated into a re-runnable script: `Diagnostics/investigate_rdx_batch_effect.R` (see that script for exact, reproducible code -- every number below was verified to come out identically when re-run from source data).

### Data-quality corrections made along the way (before the causal investigation could proceed)

1. **NC-inclusive physical surface usage history required two rounds of correction.** NCs are not run on separate/spare surfaces -- they reuse the same 8 physical pilot surfaces (some surfaces used for NC twice, e.g. `ABS_02`/`ABS_03`/`Steel_02` each swabbed 6 times total including NCs, vs `ABS_04`/`Steel_01` never used for NC at all, only 4 times). Building a correct "true cumulative use number" per surface required:
   - Round 1 (wrong): defaulted same-day NC-vs-pilot ties to sort NC *last* that day. User corrected this directly: **NCs are taken at the START of their own batch's session**, not the end.
   - Round 2 (still incomplete): re-matching each NC to its own batch-mate via `(SurfaceID, EventDate, BatchNumber)` worked for 8 of 9 NCs, but silently defaulted the 9th (`"ABSBatch5 NC"`, on `ABS_01`) to sort *first that day* instead, because its `BatchNumber=5` label doesn't correspond to any real pilot `BatchNumber` (there is no "Batch 5" in the design -- "5" is a `TestOrder` marker for the delayed catch-up session, confirmed against `PILOT_026`'s own `TestOrder=5`). User caught this with a direct question ("have you changed the order of all NCs, not just the last ones?").
   - **Final, verified-correct approach**: two-tier matching -- `(SurfaceID, EventDate, BatchNumber)` first, `(SurfaceID, EventDate, TestOrder)` as fallback for the one orphan. All 9 NCs explicitly checked and confirmed correctly placed (e.g. `Steel_02`'s verified order: `PILOT_005 → PILOT_SteelBatch2_NC → PILOT_006 → SteelBatch3 NC → PILOT_007 → PILOT_008`).
2. **The two anomalous samples' TRUE chronological position is their surface's LAST use, not an early/mid one.** `PILOT_026` (nominally "Batch 2") is `ABS_01`'s 5th of 5 physical uses; `PILOT_015` (nominally "Batch 3") is `ABS_03`'s 6th of 6 -- because `ABS_01`/`ABS_03`'s regular Batch 4 tests happened *earlier the same day* (08-04) than these delayed catch-up redos. Nominal `BatchNumber` does not reflect true physical-use chronology for these two rows.

### Factors tested and ruled out (no meaningful signal, or don't survive combination with `BatchNumber`)

| # | Factor | Test | Result |
|---|---|---|---|
| 1 | ABS-specific surface degradation | `Surface_type × BatchNumber` interaction | p=0.812; raw means show **Steel** declining more than ABS (14.6%→4.1% vs 4.6%→1.1%) -- opposite of the hypothesis |
| 2 | Within-batch injection order (GC-MS drift within one batch's own randomized run) | Rank of injection `Line` *within the same physical GC-MS sequence* (Dataset), by batch | Pooled Spearman rho≈-0.13 to -0.16 (n.s.); full model p=0.938 |
| 3 | ASTRA platform/config change | `Platform` (No/Yes) in full model, alone and combined with `BatchNumber` | p=0.545 alone; p=0.954 with Batch in the model (Batch itself unchanged, p≈0.02-0.03) |
| 4 | New swab batch (physical swab lot) | `SwabBatch` (1/2) in full model | p=0.431 |
| 5 | Drying time (spiking→swabbing interval) | `DryingTime_min` in full model | p=0.868 (RDX), p=0.958 (PETN) -- no signal at all |
| 6 | Physical surface wear/reuse count, NC-inclusive, chronologically corrected (see above) | `CumulativeUseNumber` in full model, AIC vs Batch | rho=-0.10 to -0.13 (n.s.); AIC 169.6 vs Batch's 155.5; p=0.45-0.50 with Batch in the model |

### Factors that show their own independent signal, but don't explain the batch effect

- **Extraction delay** (`ExtractionDelay_days`): borderline "significant" (p=0.048) but in the **wrong direction** -- longer delay associated with *higher* recovery. AIC (169.7) far worse than Batch alone (155.5); almost certainly riding along with the Batch1/4=0-day vs Batch2/3=5.25-day split rather than a real causal effect.
- **Humidity** (`Humidity_pct`): real, independent negative association with recovery, clearest for PETN (raw correlation p=0.015; borderline p=0.053 for RDX in the full model). AIC (171.5) still far worse than Batch alone. Worth continuing to log and control for in the main study, but it is a separate finding, not an explanation for the RDX batch pattern.

### Shape of the decline

Neither linear, quadratic, nor a single step-change (tested at every batch boundary) comes close to matching a fully flexible categorical-batch fit on the raw % scale (AIC 155.5 vs 169-176 for every constrained shape). **On a log1p scale** (the natural scale for testing constant-%-per-day chemical decay), a simple linear decay is much closer to the flexible fit (AIC 74.6 vs 73.4) -- but the actual day-to-day relative decline rate *accelerates* over time (~1.6%/day day 0-12 → ~5.8%/day day 12-20 → ~12%/day day 20-25), which a true constant-rate exponential decay would not predict. Verdict: closer to an **accelerating proportional decay** (power-law-like/compounding) than to either flat-rate linear or constant-rate exponential loss. With only 4 distinct calendar dates in the data, the exact functional form cannot be resolved further from batch-level recovery data alone.

### Is it the internal standard, or the analyte itself?

User raised a valid and important objection to the "15N-RDX IS is degrading" hypothesis: if the same IS stock spikes both the real samples and that day's calibration standards/QCs, ratio-based quantification should self-correct for gradual IS degradation by design (that is the entire purpose of an internal standard). Checked directly against the raw signal chain by date:

```
Date        rdx_pa (analyte)   rdx_is_pa (IS)
07-10       2,735              29,206
07-22       2,896              33,388
07-30       1,280              24,680
08-04       636                22,591
```

`rdx_is_pa` (IS) stays comparatively stable (~23% peak-to-trough decline, even rises between the first two dates); `rdx_pa` (analyte) collapses (~78% decline, most pronounced in the last two dates). Splitting by `Solvent_level` to remove the dry-floor-effect confound confirms the same pattern holds within wet (signal-bearing) samples specifically: 6,171 → 4,334 → 4,053 → 1,069. **This is a real decline in the RDX analyte signal itself, not an IS-normalization artefact** -- the ratio method appears to be doing its job correctly. This redirects the leading hypothesis away from the 15N-RDX IS stock and toward the **RDX spike stock** (the combined 200 ng/µL PETN+RDX solution applied to the surface) degrading over the ~3-4 week pilot span, or a genuine physical recovery decline specific to RDX.

### Is the effect bigger than the noise?

Checked directly (wet samples, raw `rdx_pa`): within-group CV is high (38-45%), and one date (07-30) has only n=1 (no replication, so its own value can't be assessed for reliability). But the between-date spread (1,069 to 6,171, ~3× the pooled within-group SD of ~1,633) is real: one-way ANOVA on raw signal alone (unadjusted) gives F(3,12)=4.38, **p=0.027** -- independently corroborating the properly-adjusted mixed-model result (p≈0.02-0.03) via a completely different, simpler analytical route.

### Remaining candidates for follow-up (not yet testable from existing data/records)

- **Spike stock (PETN+RDX combined) age/degradation** -- leading hypothesis, given the analyte-vs-IS finding above. Check: was one preparation used across the whole pilot, or remade per batch?
- **RDX-specific calibration/QC standard stock stability** (separate from the spike stock) -- would bias reported concentration via calibration-curve drift rather than true recovery change.
- **Ambient lab temperature** -- not logged (only humidity was); could correlate with batch by coincidence across the ~3-4 week span.
- **Ethanol (extraction solvent) lot/water content** -- new bottle opened partway through, or moisture uptake over time.
- **Storage conditions during the extraction delay** -- fridge vs bench, capped vs open, light exposure (RDX has some light/thermal sensitivity); the delay *length* was tested and ruled out as the sole driver, but *how* samples were stored during that delay was not.
- **Cotton swab pre-cleaning (acetone soak) duration/thoroughness per batch** -- distinct from the swab *lot* change already tested.

**Files modified**: `Diagnostics/investigate_rdx_batch_effect.R` (new -- consolidated, re-runnable version of every check above; verified to reproduce every number in this entry exactly from source data), `CONTEXT.md` (this entry).

---

## Session Summary (August 10, 2026) — NC Evaluation Restructured to Two Explicit Stages + Boundary-Fit Crash Fix

### NC Evaluation Restructuring (ported from GCMSQuantitation's August 7, 2026 fix)

Following the same recommendations given for GCMSQuantitation's FINEX pipeline (`Code/04_CollateStudyResults.R`), `pilot_analysis.R`'s NC evaluation was restructured from a single un-gated `nc_status` (contamination classification) + boolean `nc_analysis_valid` pair into an explicit two-stage pattern mirroring pilot samples' own `analysis_accepted`/`Outcome`:

- **Stage 1 -- `nc_analysis_accepted`** (`compute_nc_analysis_accepted()`, replacing `compute_nc_analysis_valid()`): was this injection/run analytically trustworthy? Same criteria family as samples (IS validity + QC brackets), **now checking BOTH 6ng and 0.2ng QC brackets** (previously only 0.2ng was checked for NC validity here, unlike samples) -- minus the "own analyte must be Quantifiable" check and the per-analyte 0.2ng-FAIL override, neither of which apply to an NC. Returns `"PASS"`/`"PASS*"`/`"FAIL: <reason>"`.
- **Stage 2 -- `nc_result`** (`compute_nc_result()`, replacing `compute_nc_status()`): is it negative (clean) or positive (contamination/trace detected)? Same three-tier SNR/PH/concentration classification as before, relabelled `"Negative (clean)"` / `"Positive: ..."` to avoid clashing with Stage 1's own PASS/FAIL vocabulary, and **forced to `"Not evaluated (analysis failed)"` whenever Stage 1 fails** -- regardless of what the raw signal shows.

**Immediate real-world impact**: adding the 6ng bracket check to Stage 1 revealed that **all 4 `Analysis4` NCs now correctly fail Stage 1** -- their nearest 6ng QC bracket (Line 31, `FAIL_BIAS` at -20.09%) is the same known Analysis4 PETN drift-correction weakness documented in GCMSQuantitation's own `CONTEXT.md`. Previously invisible under the 0.2ng-only check (all 4 showed as "valid").

Downstream consumers updated to match: `select_best_attempt()`'s NC dedup criterion (now `nc_analysis_accepted %in% c("PASS","PASS*")`, sensitive to 6ng failures too), `desired_cols_nc` (6ng bracket columns added to the `Negative Controls` sheet, previously not shown at all), `add_sheet()` conditional formatting, and the `SampleSummary` `NC` row (recomputed from the new columns, same net PASS/PASS*/FAIL logic as before).

### Boundary (Singular) Fit Crash Fix

Running `"full"` analysis mode crashed partway through with `Error in shapiro.test(ranef_vals) : all 'x' values are identical`, alongside a `bobyqa` convergence warning. Root cause: RDX's mixed model hit a **boundary/singular fit** (between-surface variance component estimated at exactly zero, consistent with this pilot's previously-documented low ICC) -- every surface's random-effect BLUP shrinks to an identical value (0), and `shapiro.test()` hard-errors on a constant vector rather than returning a normal result.

**Fix** (`analyze_analyte()`):
1. Explicit `isSingular(model)` check immediately after fitting, printing a clear `[WARNING] Boundary (singular) fit: ...` explanation.
2. The random-effects Shapiro-Wilk test wrapped in `tryCatch`, printing `"Shapiro-Wilk (random effects): skipped -- random effect BLUPs are constant ..."` instead of crashing.

Verified against real data (temporary copy, `analysis_mode <- "full"`, live default untouched): PETN's model fit normally; RDX's hit the boundary fit exactly as expected, the new warning printed, the previously-fatal Shapiro-Wilk call was skipped gracefully, and the script reached `PILOT ANALYSIS COMPLETE` with `pilot_analysis_summary.txt` regenerated successfully end-to-end.

**Files modified**: `pilot_analysis.R` (`compute_nc_status()`/`compute_nc_analysis_valid()` replaced by `compute_nc_analysis_accepted()`/`compute_nc_result()`; `select_best_attempt()` NC criterion, `desired_cols_nc`, `add_sheet()` NC formatting, `SampleSummary` NC row all updated to match; `analyze_analyte()` gains `isSingular()` check + `tryCatch`-wrapped Shapiro-Wilk test), `CONTEXT.md` (this entry).

---

## Session Summary (August 7, 2026) — Negative Control Regex Fix (Analysis6 NCs Missing From Summary)

### Problem

User noticed fewer negative controls in `ASTRA_PilotStudyResults.xlsx`'s "Negative Controls"/`SampleSummary` sheets than expected and suspected the NC regex needed updating.

### Root Cause

`nc_pattern <- "^PILOT_(Steel|ABS)Batch(\\d+)_?(?:NegCtrl|NC)$"` (the pattern documented in the "GC-MS Data Collation" section below as already handling the `...NegCtrl` vs `..._NC` naming drift between Analysis1 and Analysis3) did not anticipate a **third** naming convention introduced in `Analysis6`: `"ABSBatch4 NC"` / `"ABSBatch5 NC"` -- missing the `PILOT_` prefix entirely, and using a space instead of an underscore (or no separator) before `NC`. Neither `IsPilotSample` nor `IsNegativeControl` matched these two rows, so they were silently swept into the `"Excluding N non-pilot sample row(s)"` bucket alongside genuine non-pilot rows (test standards, "Swab_Ext2", etc.) with no distinct warning.

Confirmed directly against every `SampleName` containing "NC"/"Neg"/"Ctrl" across `Analysis1`/`3`/`4`/`6`'s `*_GCMSResults.csv` files: 7 physical NC batches exist in total (Steel1/ABS1 -- duplicated across Analysis1 and Analysis4, deduplicated separately by `select_best_attempt()`, not related to this bug -- Steel2, ABS3, Steel4, and the two new ABS4/ABS5 from Analysis6), but only 5 were reaching `nc_rows` before this fix.

### Fix Applied

`nc_pattern` changed to:
```r
nc_pattern <- "^(?:PILOT_)?(Steel|ABS)Batch(\\d+)[ _]?(?:NegCtrl|NC)$"
```
- `PILOT_` prefix made optional (`(?:PILOT_)?`) rather than mandatory.
- The separator between the batch number and `NegCtrl`/`NC` widened from `_?` (none-or-underscore) to `[ _]?` (none, underscore, **or space**).

Verified against all 7 known real NC names (all 5 pre-existing variants + the 2 new Analysis6 names) plus 10 non-NC sample names (pilot `PILOT_0XX` samples, Test Std entries, Swab_Ext2 entries, QC-level labels) -- new pattern matches exactly the 7 NC names and none of the non-NC names, identical behaviour to the old pattern except for correctly picking up the 2 previously-missed rows.

### Verification (real data, live files regenerated)

Re-ran `pilot_analysis.R` (default `analysis_mode = "boxplots_only"`, no statistical-model code path touched): console now reports `"Negative controls found: 9"` (raw rows, before dedup) -> `"Negative controls appended: 7"` (after `select_best_attempt()` collapses the Analysis1/Analysis4 Batch1 duplicates) -- up from the previous 5 unique NCs. Directly inspected the regenerated `ASTRA_PilotStudyResults.xlsx` "Negative Controls" sheet: `ABSBatch4 NC` (Analysis6, `NC WARN: RDX trace detected`) and `ABSBatch5 NC` (Analysis6, `NC WARN: RDX trace detected`) now present alongside the 5 previously-showing rows. `SampleSummary`'s `NC` row Total increased from 5 to 7 accordingly (1 PASS / 6 WARN / 0 FAIL, 7/7 analytically valid).

**Files modified**: `pilot_analysis.R` (`nc_pattern` regex), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued x9) — Mean Diamond Colour Changed to Red

Changed the mean-recovery diamond marker in `generate_boxplots()` from black to red (`geom_point(data = means_df, ..., shape = 18, colour = "red")`) so it stands out clearly against the now-uniformly-black individual points on the "all data" plots (previously both were black, making the mean hard to distinguish at a glance). Applies to every plot `generate_boxplots()` produces (QC-filtered and all-data variants alike, since it's the same shared function). Verified by regenerating `PETN_pilot_boxplots_alldata.png` -- red diamonds clearly visible against black points in every facet/condition.

**Files modified**: `pilot_analysis.R` (`generate_boxplots()`: mean diamond `colour` changed from `"black"` to `"red"`), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued x8) — Simplified "All Data" Plot Titles

Removed "(incl. FAIL)" from the `title_suffix` used by all four "all data" plots (`generate_boxplots()`/`generate_repeat_barchart()` calls inside `generate_all_data_boxplots()`) -- titles now read simply `"{Analyte} Recovery by Condition — All Data"` / `"{Analyte} Recovery by Repeat and Condition — All Data"` instead of appending `"(incl. FAIL)"`. Verified by regenerating all 4 `_alldata` PNGs; confirmed clean title text with no errors.

**Files modified**: `pilot_analysis.R` (`generate_all_data_boxplots()`: 4 `title_suffix` strings updated), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued x7) — All-Data Box Plots: Removed Point Colour Coding and NC/Untested

### Change

Per explicit request, simplified the "all data" box plots (`generate_boxplots(..., show_points = TRUE)`, producing `*_boxplots_alldata.png`):
- **Removed QC-status colour coding** on the overlaid points -- `geom_jitter(aes(color = AcceptanceStatus), ...)` + `scale_color_manual(...)` (PASS=green/PASS\*=orange/FAIL=red/NC=blue/Untested=grey) replaced with a single uniform `colour = "black"`, no colour legend.
- **Removed NC and "Untested" rows entirely** (not just their colour) -- filtered out of `data_valid` before `Condition`/`means_df`/`counts_df` are even built, so these plots now show only PASS/PASS*/FAIL pilot samples (the actual 2^3 factorial design), matching every other plot in this script. The now-permanently-empty `"NC"` `Condition` level was also removed from the factor's level list (previously added via `if (SampleType == "NC") condition_raw <- "NC"`, now dead code since NC rows never reach that point).
- Added an empty-data guard (`if (nrow(data_valid) == 0) ... return(invisible(NULL))`) after the new filter, in case a future dataset ends up with only NC/Untested rows.
- Updated `title_suffix` in `generate_all_data_boxplots()` from `"All Data (incl. FAIL/NC)"` to `"All Data (incl. FAIL)"` to match.

**Note on scope**: this only affects the `show_points = TRUE` call path (the `_alldata` variants) -- the primary QC-filtered box plots never had colour-coded points or NC rows to begin with (NC is already excluded by the QC filter before `pilot_data` reaches `generate_boxplots()` in the non-alldata case), so they are unaffected.

**Verification**: ran the full script (default `analysis_mode = "boxplots_only"`); regenerated `PETN_pilot_boxplots_alldata.png`/`RDX_pilot_boxplots_alldata.png` with no errors. Visually confirmed: all points now plain black, no legend, all 8 (Surface x Condition) cells show their full `n=4` count with no NC category or grey "Untested" points anywhere on either plot.

**Files modified**: `pilot_analysis.R` (`generate_boxplots()`: NC/Untested filter added before Condition/means/counts, empty-data guard, colour-coded `geom_jitter`/`scale_color_manual` replaced with plain black points; `generate_all_data_boxplots()`: title text updated), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued x6) — "Wet-Only" Clarified as a Main-Study Design, Not Pilot Data-Mining; Literature-Anchored Recommendation

### Clarification

The previous session's suggestion ("check whether any remaining QC-bracket failures are wet, to grow the wet-only pilot subset") was a misreading of the user's intent. The actual ask: **for the *main study* (future data collection), collect wet-swab samples only and vary Pressure** -- i.e. exactly the `SCENARIO: WET-SWAB-ONLY` design already built, used as the primary forward-looking recommendation, not a retrospective pilot-data-recovery exercise.

### Problem With Using the Pilot's Own Wet-Only Estimate as "The" Recommendation

The wet-only pilot subset is only n=9 -- too small for its own retrospective Cohen's f estimate to be trustworthy (RDX's conservative, CI-lower-bound figure couldn't even be computed; effect could be ~0). Recommending a main-study sample size directly from that number would just relocate the retrospective-power problem rather than solve it.

### Fix: Externally-Anchored (Non-Circular) Target Effect

**New config variable** (`pilot_analysis.R`, `USER CONFIGURATION`): `pressure_literature_effect_pct <- 6` -- sourced from this same file's own "Pressure selection rationale" section (`CONTEXT.md`): *"Expected ~6% absolute CE difference based on literature (4%/Newton)"*, based on 3 published wipe-sampling papers -- **not** derived from this pilot's own data at all. Only the residual variance (`fit$overall_sd`, still necessarily pilot-derived -- unavoidable for any power calculation) scales it into a Cohen's f; the effect-size numerator itself is external.

`fit_pressure_only_model()` extended to also return the wet-only subset's `overall_sd`. New calculation: `cohens_f <- (pressure_literature_effect_pct / overall_sd) / sqrt(2)`, run through the same `find_min_n_for_power()` (n_conditions=2, n_fixed_effects=3) used elsewhere.

**Result** (real data): **PETN needs only 3 surfaces/type** (12 wet-only samples, 98.2% power) to detect a 6% Pressure difference; **RDX needs 10/type** (40 samples, 82.3% power) -- RDX's larger residual variance in the wet-only subset (9.38% vs PETN's 8.28%) drives it to need more replication for the same target effect. Taking the more conservative (RDX-driven) figure: **10 steel + 10 abs = 20 surfaces, 40 total samples** for the recommended main study.

### `FINAL RECOMMENDATIONS` Restructured

Previously presented "Option A" (full factorial) and "Option B" (wet-only) as coequal alternatives for the user to choose between. Restructured to properly reflect the evidence:
- **`RECOMMENDED MAIN STUDY`** (renamed from "Option B") is now presented first, using the literature-informed n (not the pilot's own noisy wet-only estimate) as its headline figure, with an explicit rationale paragraph explaining *why* (Solvent already large/precisely-estimated; Pressure's pooled effect is confounded by the dry floor effect and remains genuinely unresolved specifically under wet conditions).
- **`ALTERNATIVE -- FULL FACTORIAL MAIN STUDY`** (renamed from "Option A") is now explicitly labelled "Not recommended as the primary plan", kept only for completeness/comparison, with its own reasoning spelled out (spends new samples re-confirming an already-resolved Solvent effect while further diluting the still-open Pressure question with more dry data).
- Added an explicit "pooling with the pilot's existing 9 wet-only samples might reduce the new-sample requirement" note, pointing at the still-not-yet-built `doe_nested_design.R`/`main_study_analysis.R` combined-analysis plan already documented elsewhere in this file.

### Verification

Re-ran the full `"full"`-mode analysis (temporary copy, live script default untouched); confirmed the regenerated `pilot_analysis_summary.txt`'s `FINAL RECOMMENDATIONS` section reads as: recommended wet-only plan (10+10 surfaces, 40 samples, PETN 100%/RDX 82.3% power at that n) presented first with full rationale, full-factorial alternative (4+4 surfaces, 32 samples) presented second and explicitly deprioritised, and a clear `NEXT STEPS` list reflecting the recommended plan as the default action.

**Files modified**: `pilot_analysis.R` (`pressure_literature_effect_pct` config variable; `fit_pressure_only_model()` returns `overall_sd`; new literature-informed calculation in the `SCENARIO: WET-SWAB-ONLY` reporting loop; `FINAL RECOMMENDATIONS` section restructured), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued yet further) — PETN `Below_LOQ` Extended Too; Pressure's Apparent Effect Collapses With Fuller Data

### Extension to PETN

Per explicit request, extended the RDX `Below_LOQ` -> 0% treatment (previous session) to PETN as well, using the same evidence standard: checked every PETN `Below_LOQ` occurrence across the whole study, max 1.10% recovery (`PILOT_032`) -- negligible, same as RDX's 0.17% max. `pilot_analysis.R` Section 5 restructured so `petn_treat_as_zero`/`rdx_treat_as_zero` are checked identically (flag first, before the concentration-based branches), and `compute_analysis_accepted()`'s two per-analyte blocks made symmetric (both now treat `NA`/`Below_LOD`/`Below_LOQ` as a `PASS*` "legitimate negative", not just RDX).

**Result**: valid pilot samples **17 -> 23 (RDX fix) -> 25 (PETN fix)**, FAIL count 15 -> 9 -> 7. All 8 newly-rescued samples across both fixes turned out to be `Solvent_level == "absent"` (dry) -- confirmed directly, none wet. This makes physical sense (near-zero/negligible recovery is the expected signature of the dry condition specifically) and has a direct consequence for the per-effect power analysis below: it improves the Solvent-effect estimate and the full-dataset analysis, but leaves the **wet-only** scenario (n=9, unchanged) completely untouched, since no new wet data was added.

### Re-ran Full Power Analysis at n=25 (was n=17) -- Pressure's Apparent Effect Almost Entirely Disappears

Per explicit request ("see how the improvement in usable samples changes the proposed next steps"), reran `"full"` mode and compared the **PER-EFFECT POWER ANALYSIS** section before/after:

| | n=17 (before) | n=25 (after) |
|---|---|---|
| **PETN Pressure** Eta2_partial [95% CI] | 0.387 [0.000-1.000] | **0.002 [0.000-1.000]** |
| **PETN Pressure** Cohen's f (optimistic) | 0.794 | **0.049** |
| **PETN Pressure** min n for 80% power | 2/type (optimistic) | **cannot reach 80% power within 20/type** |
| **RDX Pressure** Eta2_partial [95% CI] | 0.051 [0.000-1.000] | **0.002 [0.000-1.000]** |
| **RDX Pressure** Cohen's f (optimistic) | 0.233 | **0.048** |
| **RDX Pressure** min n for 80% power | 18/type (optimistic) | **cannot reach 80% power within 20/type** |
| **PETN Solvent** Eta2_partial [95% CI] | 0.882 [0.409-1.000] | 0.793 [**0.581**-1.000] (tighter CI) |
| **PETN Solvent** Cohen's f (conservative) | 0.831 | 1.176 (stronger) |
| **RDX Solvent** Eta2_partial [95% CI] | 0.923 [0.630-1.000] | 0.917 [**0.827**-1.000] (tighter CI) |
| **RDX Solvent** Cohen's f (conservative) | 1.306 | 2.189 (stronger) |

**Pressure's apparent effect (in the full dataset) has almost entirely collapsed** for both analytes once the dry-condition samples that were previously wrongly excluded (hard-failed) are correctly included as genuine 0% data points. This is a direct, real-world confirmation of the retrospective-power caution flagged in the previous session: PETN Pressure's earlier-looking "large" effect (f=0.794, only 17 samples) was a small-sample artefact that mostly vanished with 8 more (admittedly all-dry) data points. **Solvent's effect, by contrast, got *stronger and more tightly estimated*** with the same additional data -- both its CI lower bounds narrowed upward, consistent with the new data being additional confirmatory dry-swab zeros that reinforce (rather than contradict) the existing Solvent signal.

**Important nuance -- this does NOT mean Pressure has no real effect, full stop.** All 8 newly-added samples are dry (absent), where recovery is expected to be near-zero regardless of pressure (a floor effect) -- adding them dilutes the FULL-dataset marginal Pressure estimate by construction, whether or not a real Pressure effect exists specifically *within* wet samples. The **wet-only scenario is the relevant test for that question, and it is completely unchanged** (still n=9, still Cohen's f = 0.163 PETN / 0.229 RDX, still "cannot determine a finite sample size from this pilot alone" for the conservative estimate). **The correct updated conclusion is: Pressure's marginal (dry+wet pooled) effect is now confidently ruled out as large, but its within-wet effect remains exactly as unresolved as before** -- more dry data cannot answer that question; only more wet data can.

### Updated Next-Step Recommendation

Given the above, the previous "Priority 4" framing (decide between Option A/full-factorial vs Option B/wet-only) should be revised:

1. **Do not conclude "Pressure doesn't matter, drop it from the main study"** purely from the full-dataset per-effect result -- that conclusion is confounded with the dry floor effect, not a genuine test of Pressure's relevance under realistic (wet) swabbing conditions.
2. **The wet-only subset (n=9) is the bottleneck**, not the dry/full dataset (which is now reasonably well resolved: Solvent's effect is large and precisely estimated, and PASS rate has already improved via QC fixes without any new lab work). Further QC-recovery fixes to *dry* samples (e.g. Priority 1's remaining 7 QC-bracket failures, if they happen to be dry) will not move this bottleneck.
3. **Recommend checking whether any of Priority 1's remaining 7 QC-bracket failures are `Solvent_level == "present"` (wet)** -- if so, resolving those specific ones (rather than Priority 1 as a whole) would directly grow the wet-only n and could genuinely resolve whether Pressure matters under wet conditions, which nothing else can currently do.
4. If no more wet pilot data can be recovered from existing failures, the wet-only Pressure question will have to be resolved by the **main study itself** rather than the pilot -- meaning Option A (full factorial, which inherently collects more wet data as part of testing all 4 conditions) is the safer choice over Option B (wet-only) specifically *because* Option B's own justification (that Pressure doesn't need much wet-specific replication) is exactly the thing this pilot cannot currently confirm.

**Files modified**: `pilot_analysis.R` (Section 5 `petn_treat_as_zero` added, restructured to match `rdx_treat_as_zero`; `compute_analysis_accepted()`'s PETN block extended to match RDX's), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026, continued further) — RDX `Below_LOQ` Now Also Counted as 0% Recovery

### Investigation

Following up on "Priority 2" from the previous session's recommendations (RDX `Below_LOQ` was responsible for 6 of the pilot's 15 QC failures -- far more than PETN's single `Below_LOQ` failure), pulled **every** `rdx_snr_flag == "Below_LOQ"` occurrence across **all** Analysis runs in the study (not just the 6 currently-failing pilot samples, to check the pattern holds generally, not just in the failing cases):

```
RDX Below_LOQ concentrations across the whole study: 0.000 - 0.0167 ng/uL  ->  0.00% - 0.17% recovery
```

Every single occurrence is negligible relative to the study's 10% "meaningful difference" threshold -- consistent with `Below_LOQ`'s own definition (SNR 3-10, i.e. barely above the noise floor by construction; a large concentration at that SNR would be a near-contradiction). This confirmed the user's hypothesis directly: treating RDX `Below_LOQ` as 0% recovery is a near-lossless simplification for this dataset, not merely a convenient approximation.

**Scope**: applied to RDX only, per explicit request. PETN also has 3 `Below_LOQ` occurrences in the study (max 1.1%, `PILOT_032`) where the same reasoning would plausibly apply, but this was **not** changed -- flagged separately for the user's own decision rather than applied unilaterally.

### Fix Applied (`pilot_analysis.R`)

1. **Recovery calculation** (Section 5): unlike `Below_LOD`/`NA` (which set `rdx_concentration` to `NA`), a `Below_LOQ` row already has a *real, non-NA, positive* quantified concentration (GCMSQuantitation still computes concentration for `Below_LOQ` -- "signal is present, just low precision"). So simply adding `"Below_LOQ"` to the existing NA-fallback logic would have no effect (the real small value would still win, since it's not `NA`). Restructured so the flag is checked **first**, before the concentration-based branches: `rdx_treat_as_zero <- rdx_snr_flag_col %in% c(NA_character_, "Below_LOD", "Below_LOQ")` now forces `RDX_Recovery_pct = 0` for all three flag states, overriding whatever small positive concentration was actually measured. PETN's equivalent logic (`petn_legit_negative`) is unchanged (`NA`/`Below_LOD` only).
2. **`compute_analysis_accepted()`**: RDX's per-analyte check extended from `is.na(rdx_snr) || rdx_snr == "Below_LOD"` to also include `"Below_LOQ"` -- adds to `warnings_na` (contributing `PASS*`, message `"RDX Below_LOQ (legitimate negative, recovery quanted at 0%)"`) instead of `failures`. PETN's equivalent check is unchanged, still hard-fails on `Below_LOQ` (e.g. `PILOT_018` remains `FAIL: PETN Below_LOQ`).

### Verification (real data, full pipeline re-run)

| | Before | After |
|---|---|---|
| Pilot samples FAIL | 15 | **9** |
| Pilot samples PASS/PASS* (valid for stats) | 17 | **23** |

All 6 previously-failing RDX `Below_LOQ` samples (`PILOT_016/017/021/024/027/028`) now show `analysis_accepted = PASS*`, `Outcome = Complete`, `RDX_Recovery_pct = 0` (confirmed exactly `0`, not the tiny real value) -- and, since all 6 also already had `petn_snr_flag = NA` (fixed in the earlier "legitimate non-detection" session), `PETN_Recovery_pct = 0` too, so these rows now contribute genuine 0%/0% data points to the statistics for both analytes rather than being dropped entirely. Remaining 9 FAILs: 8 are the QC-bracket-driven failures already flagged as "Priority 1" in the prior session's recommendations (unrelated to this fix), 1 is `PILOT_018`'s PETN `Below_LOQ` (out of scope, per above).

**Files modified**: `pilot_analysis.R` (Section 5 recovery calculation restructured for RDX; `compute_analysis_accepted()`'s RDX check extended), `CONTEXT.md` (this entry).

**Open question for a future session**: should PETN's own `Below_LOQ` cases (`PILOT_018`, `PILOT_032`, 1 NC) receive the same treatment? The same evidence (max 1.1% recovery across all PETN `Below_LOQ` occurrences in the study) would support it, but this was deliberately left for a separate decision rather than assumed.

---

## Session Summary (August 6, 2026, continued) — Power Analysis Overhaul: Fixed Floor Bug, Per-Effect Comparison, Wet-Only Scenario

### Background: Two Problems Surfaced by Direct Questioning

**Problem 1 (user's arithmetic check)**: asked whether "Main Study needs 4 surfaces/type" meant "Pilot(4) + Main(4) = 4 total, so 0 more needed" or "4 additional". Tracing the actual code showed **neither** interpretation was correct: `calculate_nested_power()` never referenced how many samples were already collected at all -- it answered a third, unrelated question ("would a hypothetical fresh dataset of size N, considered in total isolation, detect the target effect with 80% power?"). Since that N happened to numerically equal the pilot's own design size, and the effect size fed into the calculation was *estimated from the pilot itself*, this is a near-textbook case of **retrospective/observed power analysis** -- a well-documented statistical pitfall (Hoenig & Heisey, 2001, "The Abuse of Power", *The American Statistician* 55(1):19-24): an effect found significant in a sample will almost always show "adequate" power when tested against a study of that same sample's size, because the two calculations are not independent.

**Problem 2 (floor bug, found while investigating Problem 1)**: the search for "minimum surfaces per type" was hardcoded to `seq(4, max_surfaces_per_type, 2)` -- it **never tested n=2 or n=3**. So "4 is the minimum" only ever meant "4 is the smallest value the loop bothered to check", not a genuine minimum. (Confirmed materially wrong once fixed: the true minimum for this pilot's data turned out to be **3**, not 4.)

**Problem 3 (user's follow-up)**: asked to additionally treat Pressure and Solvent as separate questions, since Solvent visibly has a much larger effect (many dry-swab samples show ~0% recovery) -- specifically, whether a main study could be restricted to wet swabs only and focus replication on Pressure.

### Fixes Implemented (`pilot_analysis.R`, inside the same `sink()`-captured full-analysis block)

1. **`calculate_nested_power()` generalised**: now accepts `n_conditions`/`n_fixed_effects` (default 4/7, matching the original full 2^3 design) instead of hardcoding them -- needed to also evaluate a *reduced* 2^2 design (Solvent fixed at one level: `n_conditions=2`, `n_fixed_effects=3`).
2. **New `find_min_n_for_power()` helper**: single reusable minimum-n search, `min_n` defaults to **2** (not 4) -- fixes the floor bug everywhere it was previously duplicated (the main per-analyte loop and the `FINAL RECOMMENDATIONS` section both now call this instead of hand-rolled `sapply(seq(4, ...))` loops).
3. **New `PER-EFFECT POWER ANALYSIS` section**: for each analyte, extracts `Pressure_level`'s and `Solvent_level`'s *own* Cohen's f (already available in `analyze_analyte()`'s returned `effect_sizes` data frame) and runs `find_min_n_for_power()` for each separately -- directly answers "does Pressure need more replication than Solvent?" per analyte. Reports **two** figures per factor to partially guard against the retrospective-power problem above:
   - **Optimistic**: the point-estimate Cohen's f (what the pilot's ANOVA literally found)
   - **Conservative**: Cohen's f computed from the *lower bound* of that effect's 95% CI (`sqrt(max(CI_low,0) / (1-max(CI_low,0)))`) -- explicitly labelled as the more defensible planning figure. When `CI_low` is ~0 (common with this pilot's small n and wide CIs), reports plainly that "the effect could be as small as ~0... cannot determine a finite sample size from this pilot alone" rather than a misleadingly precise number.
4. **New `SCENARIO: WET-SWAB-ONLY MAIN STUDY` section**: new `fit_pressure_only_model()` helper subsets each analyte's own cleaned data (`analyze_analyte()` now also returns `data_clean` in its result list, needed here) to `Solvent_level == "present"` only, refits a **reduced** mixed model (`recovery ~ Surface_type * Pressure_level + (1|SurfaceID_nested)`, no Solvent term at all), extracts the wet-only Pressure effect + ICC, and runs the same optimistic/conservative `find_min_n_for_power()` treatment with `n_conditions=2`. Guards against too-small a wet-only subset (`n < 8` or fewer than 2 surface types present) and model-fit failure, both handled via `list(ok = FALSE, reason = ...)` rather than erroring.
5. **`FINAL RECOMMENDATIONS` restructured** into explicit **Option A (full factorial)** vs **Option B (wet-swab-only, Pressure-focused)**, each with its own surfaces/samples/power, plus an explicit **"IMPORTANT CAVEAT"** paragraph directly addressing Problem 1 (this is a standalone calculation, not additive to/subtracted from the pilot, and is a retrospective-power estimate).

### Verification (real data, `use_qc_filtered_data = TRUE`, 17 valid samples)

Ran the full `"full"`-mode analysis (temporary copy, live script's own `"boxplots_only"` default untouched) and inspected the regenerated `pilot_analysis_summary.txt` directly:

- **Floor bug confirmed fixed**: `OPTION A` now reports **3** surfaces/type (24 total samples, 100%/86.2% power for PETN/RDX) -- genuinely smaller than the old hardcoded-floor answer of 4, confirming the previous "minimum" was an artefact of where the search started, not a real minimum.
- **Per-effect comparison directly answered the user's question**: for **RDX**, Pressure's observed effect (Cohen's f = 0.233, a small-medium effect) requires **18 surfaces/type (144 total samples)** for 80% power -- dramatically more replication than Solvent (f = 3.450, only 2/type) needs. For **PETN**, Pressure's point estimate looks large (f = 0.794, only 2/type optimistically) but its 95% CI lower bound is 0 (effect could be genuinely absent) -- i.e. Pressure's true effect is far less certain than Solvent's even for PETN, where Solvent's *conservative* estimate (f = 0.831) still only needs 2/type. **Confirms the user's premise directly and quantifies it**: Solvent is unambiguously the larger, more reliably-estimated effect for both analytes; Pressure's replication requirement is either much larger (RDX) or much more uncertain (PETN).
- **Wet-only scenario surfaced an unexpected, informative wrinkle**: with only 9 valid wet-only samples per analyte (about half the already-small pilot), the wet-only Pressure effect estimates (Cohen's f = 0.163 PETN, 0.229 RDX) are *smaller* than the same effect estimated across the full dataset (0.794 PETN, 0.233 RDX) -- suggesting Pressure's apparent effect in the full-data analysis may be partly driven by the dry-swab floor effect (many near-zero recoveries) rather than a genuine Pressure-specific signal within wet samples. RDX's wet-only Pressure effect could not reach 80% power within the 20-surfaces/type search ceiling at all. This is exactly the kind of result a hasty "just focus on wet swabs" decision would have missed -- flagged in Option B's output as "Could not determine a sufficient n within the surface limit for one or both analytes."
- Confirmed all new sections are captured inside the existing `sink()` block (present in the regenerated `.txt` file, not just the console), and `analyze_analyte()`'s new `data_clean` return value did not break anything already consuming its result list.

**Files modified**: `pilot_analysis.R` (`analyze_analyte()` return value gains `data_clean`; `calculate_nested_power()` generalised; new `find_min_n_for_power()`, `fit_pressure_only_model()`; new `PER-EFFECT POWER ANALYSIS` and `SCENARIO: WET-SWAB-ONLY` sections; `FINAL RECOMMENDATIONS` restructured into Option A/B), `CONTEXT.md` (this entry).

---

## Session Summary (August 6, 2026) — Statistical Results Persisted to `pilot_analysis_summary.txt`

### Problem: "Full" Mode Results Were Console-Only, Never Saved

Investigated where a user's `"full"`-mode run's results had gone: found that the entire statistical analysis (variance components, ICC, Type III ANOVA, effect sizes/Cohen's f, batch-effect check, estimated marginal means + pairwise comparisons, assumption checks, power analysis, and the main study design recommendation) was **only ever printed to the console** via `cat()`/`print()` -- no `write.csv()`/`write_xlsx()`/text file of any kind captured it. Only the PNG plots (box/bar/repeat/interaction/diagnostic charts) and `pilot_data_nested.csv/.xlsx` were ever written to disk. This matches a gap already flagged (but never implemented) in this file's own "To Create" list: `pilot_analysis_summary.txt`. Anyone who ran `"full"` mode without manually copying the console output had no persistent record of the numeric results.

### Fix: `sink(..., split = TRUE)` Around the Entire Full-Analysis Section

**New output file**: `stats_summary_file <- file.path(output_dir, "pilot_analysis_summary.txt")`, defined in `USER CONFIGURATION` alongside `data_file`.

**Implementation** (`pilot_analysis.R`, wraps the `"ANALYZE BOTH ANALYTES"` section through the very end of the script -- this code path is already only reached when `analysis_mode == "full"`, since `"boxplots_only"` mode `stop()`s/`quit()`s earlier):
```r
sink_con <- file(stats_summary_file, open = "wt", encoding = "UTF-8")
sink(sink_con, split = TRUE)
tryCatch({
  ... all existing analyze_analyte() calls, power analysis, final recommendations ...
}, finally = {
  sink()
  close(sink_con)
})
```
- `split = TRUE` means output still appears on the console exactly as before, *while simultaneously* being written to the file -- chosen deliberately over redirecting silently, so the interactive experience is unchanged.
- Every single `cat()`/`print()` call already in this section (nothing manually re-transcribed into a separate report-building routine) ends up in the file verbatim -- guarantees completeness ("include everything you wrote") and that the file can never drift out of sync with what the console shows, by construction.
- `tryCatch(..., finally = ...)` guarantees `sink()`/`close()` run even if an error occurs partway through the analysis (e.g. a model fails to converge) -- prevents a mid-run failure from leaving the R session's stdout silently redirected to a half-written file for anything printed afterward.
- A header block (generation timestamp, `data_file`, `analysis_mode`, `use_qc_filtered_data` and its implication, PETN/RDX sample counts actually analyzed, and the power-analysis target parameters) is written as the first thing inside the sink -- makes the file self-documenting/self-contained rather than a bare console dump with no context.
- The file closes with a line pointing back to its own path (`"This statistical summary ... was saved to: ..."`), and one final `cat()` *outside* the sink block confirms the same path to the live console/caller.

### Incidental Fix: Pre-Existing Character-Encoding Corruption (Unicode Arrows/Checkmarks)

Discovered while reviewing the first generated summary file: special Unicode symbols used in several `cat()`/`labs(title=...)` calls (`→`, `×`, `✓`, `✗`, `⚠`, `←`) rendered as mojibake (e.g. `â†’`) -- confirmed via an old console-only log capture that this **predates this session's change entirely** (Windows `Rscript` was already mis-parsing these raw multi-byte UTF-8 literals in the source file at parse time, before sink was ever introduced -- likely a native-encoding/locale mismatch, not something fixable from within the script's own `cat()` calls). Since this directly hurt the readability of the new persistent summary file (and, it turns out, the interaction-plot PNG titles too -- same corrupted characters would have appeared there), replaced every instance with plain ASCII: `→`/`←` -> `->`/`<-`, `×` -> `x`, `✓`/`✗` -> `[OK]`/`[FAIL]`, `⚠` -> `[WARNING]`. Confirmed via a full-file scan (`iconv(..., to = "ASCII", sub = "BYTE")` comparison) that zero non-ASCII characters remain anywhere in the script.

### Verification

Ran the full `"full"`-mode analysis twice (temporary copies with `analysis_mode` substituted to `"full"`, live script's own default `"boxplots_only"` untouched) -- once before and once after the ASCII fix:
- Confirmed `pilot_analysis_summary.txt` (405 lines) is generated with zero errors, `sink()` correctly closes afterward (subsequent console output, e.g. the final `cat("Statistical summary saved to: ...")` outside the `tryCatch`, appears normally on-console, confirming stdout was properly restored).
- Content verified complete and in order: header metadata block -> PETN analysis (variance components, ANOVA, effect sizes, batch check, EMMs, pairwise comparisons, assumption checks) -> RDX analysis (same sections) -> power analysis for both analytes -> **FINAL RECOMMENDATIONS / MAIN STUDY DESIGN** (surfaces per type, expected power, timeline estimate, next steps) -> completion banner -> file-location footer.
- Confirmed all `->`/`<-`/`x`/`[OK]`/`[FAIL]`/`[WARNING]` replacements render correctly with no mojibake anywhere in the regenerated file.

**Files modified**: `pilot_analysis.R` (`stats_summary_file` config variable; `sink()`/`tryCatch(..., finally=...)` wrapping the full-analysis section with a new header block; ASCII replacements for all Unicode symbols throughout the file), `CONTEXT.md` (this entry).

---

## Session Summary (August 5, 2026, continued x5) — `use_qc_filtered_data` Toggle

### Feature: User-Configurable QC Filter for the Whole Statistical Analysis

**Request**: a flag/variable, settable at the top of the script, to run the *whole* statistical analysis (mixed-effects model, ANOVA, power analysis, plus the main QC-filtered box/bar/repeat charts) on either all data or only QC-accepted data.

**Implementation** (`pilot_analysis.R`):
- New config variable in the `USER CONFIGURATION` block (alongside `analysis_mode`): `use_qc_filtered_data <- TRUE` (default `TRUE`, i.e. unchanged from previous behaviour).
- The previously-unconditional "QC ACCEPTANCE FILTER" block (which narrows `pilot_data` to `analysis_accepted %in% c("PASS","PASS*")` before it flows into the `"boxplots_only"` charts and `analyze_analyte()`) is now gated by this flag:
  - `TRUE`: unchanged -- only QC-accepted samples included, same console messaging as before.
  - `FALSE`: the filter is skipped entirely; a new console message states this explicitly (`"QC FILTER: DISABLED ..."`). `pilot_data` retains every sample regardless of `analysis_accepted` (including `FAIL:` samples), which then flows into everything downstream that reads `pilot_data`.
- **Negative controls are excluded either way**, regardless of this flag -- NCs have no `Surface_type`/`Pressure_level`/`Solvent_level` (design-specific columns are blank for NC rows by design, see "Negative Controls Included in `pilot_data_nested`" section below), so `lmer()`'s default `na.action` drops them from the mixed model automatically, and `generate_boxplots()`/`generate_barchart()`/`generate_repeat_barchart()` already drop them for the same structural reason (no `Condition`/factorial `SurfaceID` to plot against). This flag only ever controls the QC PASS/PASS* vs FAIL boundary, not NC inclusion.
- The `"_alldata"` plots (`generate_all_data_boxplots()`, and the by-repeat `_alldata` charts added earlier this session) are **unaffected** by this flag -- they already always show every sample regardless, as a permanent QC-audit comparison view independent of both `analysis_mode` and this new setting.

**Verification**: tested both settings directly against the real pilot dataset (temporary copies with `analysis_mode <- "full"` substituted in, to reach the statistical-model code path; the shared script's own `analysis_mode` default of `"boxplots_only"` was not touched).
- `use_qc_filtered_data = TRUE` (default): console shows `"QC FILTER: Excluded 20 sample(s)..."`, both PETN/RDX mixed models fit on 17 valid samples -- **byte-for-byte the same as before this change**, confirming zero regression for anyone who doesn't touch the new flag.
- `use_qc_filtered_data = FALSE`: console shows `"QC FILTER: DISABLED (use_qc_filtered_data = FALSE) ..."`, both PETN/RDX mixed models fit successfully on all 32 non-NC pilot rows (37 total rows in `pilot_data_nested.csv` at the time of testing, minus 5 NCs dropped automatically by `lmer()`) -- confirming QC-failed samples are now included and NCs are still correctly excluded without any error.

**Files modified**: `pilot_analysis.R` (`use_qc_filtered_data` config variable; "QC ACCEPTANCE FILTER" block gated by it), `CONTEXT.md` (this entry).

---

## Session Summary (August 5, 2026, continued yet again) — By-Repeat Individual-Value Bar Charts

### Feature: Faceted Bar Charts of Individual Recovery Values by Repeat

**Request**: two faceted bar charts (one per analyte), faceted by surface, with a series per condition and each individual recovery value plotted (not aggregated to a mean) -- surface replicate ("repeat") number along the x-axis.

**Implementation** (`pilot_analysis.R`, new `generate_repeat_barchart()` function, defined immediately after `generate_barchart()`): companion to the existing `generate_barchart()` (which plots the *mean* recovery per condition ± SEM) -- this version plots every individual sample's own recovery value directly, no aggregation.

- **X-axis**: `Repeat` -- the physical surface replicate number (1-4), parsed from the numeric suffix of `SurfaceID` (e.g. `"Steel_02"` -> `2`, `"ABS_04"` -> `4`). In this design each of the 4 physical surfaces per type is tested under all 4 conditions exactly once, so the surface replicate *is* the natural per-condition repeat identifier -- there is no independent within-surface replication of the same condition to plot instead.
- **Fill/series**: `Condition` (`50g/dry`/`50g/wet`/`200g/dry`/`200g/wet`, dodged bars), same condition labelling and colour-blind-friendlyish manual palette (`#8da0cb`/`#66c2a5`/`#fc8d62`/`#e78ac3`) used consistently across both analytes.
- **Facets**: `Surface_type` (Steel | ABS), same `facet_wrap` + labeller pattern as `generate_boxplots()`/`generate_barchart()`.
- **Y-axis**: the raw `recovery_col` value per row directly (`geom_bar(stat = "identity")`, not a computed mean) -- one bar per (Repeat, Condition, Surface_type) combination.
- **Duplicate guard**: since `geom_bar(stat = "identity")` does not aggregate, more than one row sharing the same (Surface_type, Repeat, Condition) combination would silently draw overlapping bars rather than a single value. A `dplyr::count()` check emits a loud `warning()` if this ever occurs (e.g. a reanalysed sample not yet deduplicated) rather than producing a silently misleading plot -- did not fire in the real data (each of the 32 pilot samples occupies a unique (Surface_type, Repeat, Condition) cell by design).
- Negative controls (no `Pressure_level`/`Solvent_level`, hence no `Condition`, and no factorial `SurfaceID`) are dropped, matching `generate_barchart()`'s existing NC handling.
- Outputs: `{Analyte}_pilot_byrepeat_barchart.png` (8x4in, 300dpi), alongside the existing box plot/bar chart outputs.

**Wired in** at both existing call sites: the `"boxplots_only"` short-circuit block (on `pilot_data`) and inside `analyze_analyte()` (on `data_clean`, the QC-filtered per-analyte data used in `"full"` mode) -- same pattern as `generate_boxplots()`/`generate_barchart()` immediately above each call.

**Verification**: ran the full `pilot_analysis.R` against the real pilot dataset (idempotent, `tryCatch`-wrapped collation, safe to re-run). Both `PETN_pilot_byrepeat_barchart.png`/`RDX_pilot_byrepeat_barchart.png` generated successfully with no duplicate-combination warnings; visually confirmed correct faceting (Steel | ABS), correct dodged colour-per-condition bars, and repeat numbers 1-4 on the x-axis showing each individual sample's own recovery value (e.g. Steel repeat 1 shows a single tall `200g/wet` bar and a short `200g/dry` bar, matching the underlying per-surface data directly rather than an averaged value).

**Files modified**: `pilot_analysis.R` (new `generate_repeat_barchart()` function; calls added in the `"boxplots_only"` block and inside `analyze_analyte()`), `CONTEXT.md` (this entry).

### Follow-up: All-Data Variant + Sample-Size Labels

**Requests**: (1) also run the by-repeat charts on *all* data (QC filter ignored, mirrors `generate_all_data_boxplots()`); (2) show sample size on the plot.

**Sample-size labels**: added a `geom_text()` layer showing `"n=X"` above every bar -- `X` is the count of underlying rows contributing to that (Surface_type, Repeat, Condition) cell, computed via the same `group_by()`/`summarise()` used for the pre-existing `dup_check` warning. Normally `n=1` (this design has exactly one physical measurement per repeat x condition cell), but shown directly on the plot for transparency/consistency with the `n=` labels already used on the box plots and mean bar charts, and as a visible on-plot flag (alongside the console `warning()`) if a cell ever does contain >1 row. **Alignment note**: the label layer uses `position_dodge(width = 0.8)` -- identical to the bar layer's own dodge width/mechanism -- and is built from a `group_by(Surface_type, Repeat, Condition)` summary of the *same* `data_valid` the bars are drawn from, so both layers see an identical per-x group-presence pattern and dodge to the same offsets (this is precisely the alignment pitfall documented in GCMSQuantitation's July 29, 2026 "Error Bar Misalignment" session -- avoided here by construction rather than discovered after the fact). Increased top `ylim` headroom from `1.1x` to `1.18x` max value to leave room for the labels above the tallest bars.

**All-data variant**: `generate_all_data_boxplots()` (already called unconditionally, independent of `analysis_mode`, on the unfiltered `pilot_data_all`) now also calls `generate_repeat_barchart()` on the same unfiltered data, producing `{Analyte}_pilot_byrepeat_barchart_alldata.png` alongside the existing `_alldata` box plots. `generate_repeat_barchart()` itself needed no filtering logic changes -- it never referenced `analysis_accepted` in the first place, so which rows appear was already entirely controlled by which dataframe the caller passes in (exactly like `generate_boxplots()`/`generate_barchart()`). NC rows are still dropped inside the function itself regardless (no `Pressure_level`/`Solvent_level`/factorial `SurfaceID` to plot against), so "all data" here means "including QC FAIL", not NCs.

**Verification**: re-ran the full script against the real pilot dataset. All 4 by-repeat PNGs regenerated (QC-filtered + all-data, PETN + RDX), zero warnings. Visually confirmed: `n=1` labels correctly centred above every bar in both variants (including bars sitting right at y=0, e.g. a `Below_LOD` sample quanted at 0% recovery per the earlier August 5 fix, which still gets a visible `n=1` label at the axis line rather than being invisible); the all-data PETN chart correctly shows additional bars not present in the QC-filtered version (e.g. Steel repeat 1's `50g/dry`/`50g/wet` bars, previously excluded by the QC filter, now visible with their own `n=1` label) -- confirming the QC-filtered vs all-data distinction is working as intended.

**Files modified**: `pilot_analysis.R` (`generate_repeat_barchart()`: sample-size label layer, `ylim` headroom; `generate_all_data_boxplots()`: two new calls), `CONTEXT.md` (this entry).

---

## Session Summary (August 5, 2026, continued) — Extended to `NA`-Flag Non-Detections Too

### Follow-up Investigation: Which Samples Have Been Analysed Multiple Times?

Following the `Below_LOD` fix immediately below, cross-referenced every `*_GCMSResults.csv` across all 6 `Analysis*` folders to find every pilot sample/NC analysed more than once, and compared uncorrected vs. drift-corrected PETN concentration across attempts. Findings:

- **PILOT_006/019/030/004** each show good agreement between attempts' *uncorrected* `petn_concentration` (within ~10-25%), but whichever attempt happened to land in **Analysis2** (only 2 total 6ng QCs) got an explosive drift correction factor (8-12x, vs. a sane 1.3-1.9x in every other Analysis run) -- reproducing the exact degenerate-2-QC-fit over-correction risk documented in GCMSQuantitation's own Aug 5, 2026 session (PILOT_006 there too, `petn_concentration_dc = 11.4 ng` cited directly). `select_best_attempt()` (which prefers any `PASS`/`PASS*` attempt over `FAIL`, irrespective of which is more physically credible) is currently keeping the *over-corrected* Analysis2 attempt for PILOT_006 specifically, because that attempt's own 6ng QC brackets both trivially pass (a 2-point fit reproduces its own anchors by construction) while the more-trustworthy Analysis4 attempt's 6ng *post* bracket happens to fail due to an unrelated QC problem elsewhere in that sequence. Not fixed this session -- flagged to the user as a known issue with `select_best_attempt()`'s "prefer PASS over FAIL" rule not accounting for drift-correction-fit degeneracy; no action taken pending further direction.
- **PILOT_013/017/021/027** show `petn_snr_flag = NA` (no candidate PETN peak found at all) **consistently across every one of their 2-3 independent re-analyses** (different `Analysis` folders, different injection dates) -- strong evidence of a genuine, reproducible non-detection rather than a single bad injection. This directly motivated extending the `Below_LOD` fix below to also cover the `NA`-flag case (see next section).

### Extension: `NA` Flag (No Candidate Peak Found At All) Now Also Quanted at 0%

**Problem**: The `Below_LOD` fix (below) only handled the case where GCMSQuantitation explicitly labelled a row `"Below_LOD"` (a candidate peak was found but below threshold). It did not cover the stricter non-detection case where `petn_snr_flag`/`rdx_snr_flag` is `NA` itself (no candidate peak found within RT tolerance at all) -- for these rows, `PETN_Recovery_pct`/`RDX_Recovery_pct` remained `NA_real_` (silently dropped from stats, distinct from being 0%), even though `compute_analysis_accepted()` already didn't hard-fail on a bare `NA` flag (the `!is.na(petn_snr) && ...` guard skipped the check entirely) -- so these samples weren't failing, but weren't being counted as legitimate 0% recovery data points either, and their non-detection status wasn't visible anywhere in `analysis_accepted`.

**Decision**: per explicit user direction, extended the same "legitimate negative -> 0%" treatment to the `NA`-flag case, given the reproducibility confirmed above (PILOT_013/017/021/027).

**Fix applied** (`pilot_analysis.R`):
1. **Recovery calculation** (Section 5): replaced the `Below_LOD`-only check with `petn_snr_flag_col %in% c(NA_character_, "Below_LOD")` (R's `%in%` matches `NA` against a `NA` in the table, verified directly: `NA_character_ %in% c(NA_character_, "Below_LOD")` -> `TRUE`) -- both flag states now resolve to `0`, everything else (including a genuinely missing/absent `petn_snr_flag` column on an older dataset) still resolves to `NA_real_` as before. Hardened the column-existence guard to `rep(NA_character_, nrow(samples))` (a full-length vector) rather than a bare scalar `NA_character_`, avoiding a latent length-recycling footgun in the nested `ifelse()`.
2. **`compute_analysis_accepted()`** (Section 7): restructured the per-analyte check to `if (is.na(petn_snr) || petn_snr == "Below_LOD")` -> add to `warnings_na` (contributing to `PASS*`, with a distinct message for each sub-case: `"PETN not detected (no candidate peak found; legitimate non-detection, recovery quanted at 0%)"` vs `"PETN Below_LOD (legitimate non-detection, recovery quanted at 0%)"`) -- `else if (petn_snr != "Quantifiable")` -> `failures` (unchanged, still hard-fails `Below_LOQ` etc.). Same for RDX.

**Verification**: standalone test against a synthetic 5-row data frame covering all relevant cases (`Quantifiable`+real value, `Below_LOD`+NA, `NA` flag+NA, `Below_LOQ`+NA [a mismatched edge case], `Below_LOQ`+real small value) confirmed exactly the intended behaviour: rows 2 and 3 (`Below_LOD` and `NA` flag) both correctly resolve to `0` recovery; row 4 (`Below_LOQ` with no concentration, a combination that shouldn't occur in practice) correctly stays `NA`, not zeroed; row 5 (`Below_LOQ` with a real small positive value) correctly computes its actual non-zero recovery, confirming `Below_LOQ` is not swept up into the zero-recovery treatment. Confirmed via `Rscript -e "parse(...)"` that the full edited script still parses with no syntax errors. Not yet re-run against the live pilot dataset in this session.

**Files modified**: `pilot_analysis.R` (`collate_gcms_results()`: recovery calculation in Section 5, `compute_analysis_accepted()` in Section 7), `CONTEXT.md` (this entry).

---

## Session Summary (August 5, 2026) — "Below_LOD" Non-Detections No Longer Hard-Fail Samples

### Problem

`compute_analysis_accepted()` (in `pilot_analysis.R`'s `collate_gcms_results()`) hard-failed any sample whose `petn_snr_flag`/`rdx_snr_flag` was anything other than `"Quantifiable"` -- including `"Below_LOD"` (no detectable analyte signal at all). Since GCMSQuantitation deliberately sets `petn_concentration_dc`/`rdx_concentration` to `NA` for `Below_LOD` rows (`Code/03_Quantification.R`), this compounded into two separate problems for such samples:

1. **`analysis_accepted` = `FAIL: PETN Below_LOD`** → `Outcome = "Reanalyse"` → the sample was excluded entirely from the QC-filtered statistics (mixed-effects model, box plots, bar charts), even though a true non-detection under some experimental conditions (e.g. low pressure / dry swab) is a real, expected, meaningful result, not evidence of an analytical failure.
2. **`PETN_Recovery_pct`/`RDX_Recovery_pct` = `NA`** (not `0`) for these rows, because the recovery calculation directly propagated the `NA` concentration -- so even samples that *did* make it into the dataset elsewhere weren't correctly showing 0% recovery.

The surrounding QC brackets (6ng bias, 0.2ng sensitivity) and the IS-validity check already independently confirm whether the *injection itself* was reliable -- a `Below_LOD` analyte flag on top of a clean QC/IS picture is exactly what a genuine near-zero-recovery result looks like, not an injection problem.

### Fix Applied (`pilot_analysis.R`)

Scope was deliberately narrowed to `Below_LOD` only (per explicit decision) -- `Below_LOQ` (a real, if imprecise, positive detection with an already-computed non-zero concentration) continues to hard-fail the sample exactly as before; this was not judged to be a "legitimately negative" case.

1. **Recovery calculation** (Section 5, `collate_gcms_results()`): `PETN_Recovery_pct`/`RDX_Recovery_pct` now resolve to `0` (not `NA_real_`) when the underlying concentration is `NA` *and* the corresponding `petn_snr_flag`/`rdx_snr_flag == "Below_LOD"`. Any other reason concentration is `NA` (sample not yet processed, or a different flag) is unchanged, still `NA_real_`.
2. **`compute_analysis_accepted()`**: the per-analyte SNR check now branches on the flag value -- `"Below_LOD"` is added to `warnings_na` (contributing to `PASS*` rather than `FAIL`, message `"PETN Below_LOD (legitimate non-detection, recovery quanted at 0%)"`), while any other non-`"Quantifiable"` flag (`"Below_LOQ"`, etc.) is still added to `failures` exactly as before.

**Net effect**: a sample with `Below_LOD` on one or both analytes, whose QC brackets and IS check are otherwise clean, now resolves to `PASS*` / `Outcome = "Complete"` with `Recovery_pct = 0` for that analyte, and is correctly included in the QC-filtered statistics (mixed model, box/bar charts) as a genuine 0% data point instead of being silently dropped. A sample where the QC brackets/IS *also* fail (or where the flag is `Below_LOQ`) is unaffected by this change and still fails exactly as before.

**Not changed**: negative control evaluation (`compute_nc_status()`/`compute_nc_analysis_valid()`) already treated `concentration == 0`/`Below_LOD` as `"trace"`/clean rather than a hard failure, and was not touched. The 6ng/0.2ng QC bracket logic itself (`assign_qc_brackets()`) is unaffected -- this fix is scoped entirely to how a *sample's own* analyte flag feeds into `analysis_accepted`, not to QC evaluation.

**Verification**: confirmed via `Rscript -e "parse(...)"` that the edited script still parses with no syntax errors. Not yet re-run against the live pilot dataset in this session (no code changes were made to GCMSQuantitation, so no reprocessing of `Analysis*` folders is required -- re-running `pilot_analysis.R` alone against the existing `*_GCMSResults.csv` files is sufficient to pick up the fix).

**Files modified**: `pilot_analysis.R` (`collate_gcms_results()`: recovery calculation in Section 5, `compute_analysis_accepted()` in Section 7), `CONTEXT.md` (this entry).

---

## Overview

This directory contains scripts for planning and analyzing a Design of Experiments (DOE) study to optimize trace explosives recovery from surfaces.

**Study objective:** Determine the effects of surface type, pressure, and solvent on recovery of PETN and RDX from swabbed surfaces.

**Date created:** 2026-07-06  
**Status:** Pilot study ready for execution

---

## Experimental Design

### **Design Type: Split-Plot (Nested) Factorial**

**Whole-plot factor:**
- **Surface_type**: steel vs abs (BETWEEN surfaces - cannot change on same surface)

**Sub-plot factors (WITHIN surface):**
- **Pressure**: low (50g) vs high (200g)
- **Solvent**: absent (dry swab) vs present (100µL EtOH)

**Structure:**
- Full 2³ factorial design
- 8 treatment combinations total (2 × 2 × 2)
- Each surface tested under 4 conditions (Pressure × Solvent)
- Surface_type nested within physical surfaces

---

## Experimental Details

### **Contamination Protocol**
- **Analytes**: PETN + RDX (combined standard)
- **Concentration**: 200 ng/µL in ethanol
- **Volume**: 50 µL per contamination event
- **Total mass**: 10,000 ng (10 µg) per surface
- **Application**: Full surface swabbing with ASTRA device
- **Drying time**: 5-10 minutes

### **Swabbing Protocol**
- **Swab type**: Cotton Q-tip
- **Device**: ASTRA (automated swabbing device for repeatable pattern)
- **Pressure measurement**: Mass on balance (grams)
  - **Low pressure**: 50g (± 5g acceptable)
    - **Literature context**: ~6.7 kPa on Q-tip contact area
    - Below volunteer minimum in published studies (genuinely "light")
    - Tests recovery at minimal contact force
  - **High pressure**: 200g (± 10g acceptable)
    - **Literature context**: ~27 kPa on Q-tip contact area
    - Within literature "firm" pressure range (1000-2300g on large wipes)
    - Tests recovery at firm, deliberate contact force
  - **Pressure selection rationale**: 
    - 4× force ratio provides strong statistical contrast
    - 50g tests sub-optimal recovery (below volunteer forces)
    - 200g tests near-optimal recovery (firm contact)
    - Based on analysis of 3 published wipe-sampling papers
    - Expected ~6% absolute CE difference based on literature (4%/Newton)
- **Solvent conditions**:
  - **Absent**: Dry swab
  - **Present**: 100µL ethanol applied to swab tip before swabbing

### **Surface Cleaning (between tests)**
- Bleach solution
- Rinse with water
- Ethanol wipe
- Dry completely

### **Available Resources**
- **Steel surfaces**: 20 available
- **ABS surfaces**: 20 available
- **Surface reuse**: Up to 3 tests per surface (within limit)
- **GC-MS capacity**: 28 samples/day
- **Total budget**: ~150 samples maximum

---

## Study Phases

### **Phase 1: Pilot Study (32 samples)**

**Purpose:**
1. Estimate variance components (between-surface vs within-surface)
2. Calculate effect sizes for all factors
3. Determine required sample size for main study
4. Validate experimental protocol
5. Check assumptions (normality, homogeneity, outliers)

**Design:**
- **4 steel surfaces** (Steel_01 to Steel_04)
- **4 ABS surfaces** (ABS_01 to ABS_04)
- Each surface tested under all 4 conditions (Pressure × Solvent)
- **32 total samples**
- **Randomization structure**:
  - Test order randomized within each surface (which of 4 conditions tested first/second/third/fourth)
  - Surface processing order randomized overall
  - **Within-batch surface order randomized** (batch workflow uses grouped spiking)
    - Batch 1: All surfaces' TestOrder 1 (different surface order per batch)
    - Batch 2: All surfaces' TestOrder 2 (different surface order per batch)
    - Etc.
  - This prevents systematic bias from "first tested" vs "last tested" within batches

**Timeline:**
- Sample collection: Variable (1-4 days depending on ASTRA setup time per sample)
  - Estimated: ~3-4 hours total if ASTRA setup is quick
  - Realistic: May be 1-2 hours per batch if ASTRA requires setup between samples
  - Recommendation: Execute Batch 1 first to calibrate timing expectations
- GC-MS analysis: 2 days (16 samples/day)
- Data analysis: 1 day

**Batch Workflow (Recommended):**
- **Purpose**: Efficiency - spike multiple surfaces at once, minimizing total time
- **Structure**: 4 batches, 8 samples per batch
  - Batch 1: All surfaces' first test (TestOrder=1)
  - Batch 2: All surfaces' second test (TestOrder=2)
  - Batch 3: All surfaces' third test (TestOrder=3)
  - Batch 4: All surfaces' fourth test (TestOrder=4)
- **Within each batch**:
  - Spike all 4 steel surfaces → dry 5 min → test in randomized order
  - Spike all 4 ABS surfaces → dry 5 min → test in randomized order
  - Clean all 8 surfaces (15 min)
- **Advantages**:
  - Reduces total time from ~8 hours to ~3 hours
  - All surfaces spike simultaneously (no individual waiting)
  - Still maintains randomization within batches
- **Statistical note**: Batch effect is checkable and expected to be small

**Multi-Day Execution (Flexible):**
- **Pilot can be completed over 1-4 days** depending on actual ASTRA setup time
- **Recommended approach**: 
  - Execute Batch 1 first as a calibration run
  - Time the actual execution (ASTRA setup between samples may take longer than estimated)
  - Adjust schedule for Batches 2-4 based on real timing
- **Possible schedules**:
  - **Same day:** All 4 batches (~3-4 hours if ASTRA setup is quick)
  - **2 days:** Batches 1-2 on Day 1, Batches 3-4 on Day 2 (~1.5-2 hours per day)
  - **3-4 days:** 1-2 batches per day (if ASTRA setup is time-consuming)
- **Statistical considerations**:
  - ASTRA controls pressure automatically (no operator/day variability in force application)
  - Standard kept refrigerated, fresh aliquots spiked per batch (no concentration drift)
  - Within-batch surface randomization prevents time-order bias
  - Record Day/Date/Time per batch for analysis
  - Pilot analysis will check if batch/day effects are significant
  - Any day effects expected to be small compared to main treatment effects
- **Quality control between days** (if applicable):
  - Verify ASTRA device settings unchanged
  - Record environmental conditions per batch (Temp, RH)
  - Use same Q-tip lot throughout pilot
  - Keep detailed timing notes to inform main study planning

**Scripts:**
- `pilot_study_design.R` - Generates randomized pilot design matrix
- `pilot_design_batch_reorganizer.R` - Reorganizes for batch workflow with surface randomization
- `pilot_analysis.R` - **Single consolidated script** (see "GC-MS Data Collation" section below): auto-collates GC-MS results from GCMSQuantitation, applies QC pass/fail criteria, calculates recovery, updates `pilot_data_nested.csv`, produces `ASTRA_PilotStudyResults.xlsx`, then analyzes pilot data and calculates power for main study. Run this after GCMSQuantitation's `01`/`02`/`03` scripts have processed each Analysis run.

**Output files:**
- `pilot_design_nested.csv` - Experimental design matrix (original surface processing order)
- `pilot_design_batch_organized.csv` - Batch workflow design with randomized surface order
- `batch_timing_checklist.csv` - Simplified checklist for lab execution
- `batch_protocol.md` - Detailed step-by-step protocol for batch execution
- `pilot_data_nested.csv` / `pilot_data_nested.xlsx` - Completed data file (after GC-MS analysis), including negative controls (see below); `.xlsx` sibling has conditional formatting
- `ASTRA_PilotStudyResults.xlsx` - Collated multi-sheet study workbook (analogous to GCMSQuantitation's `FINEX_StudyResults.xlsx`)
- `PETN_pilot_boxplots.png` / `RDX_pilot_boxplots.png` - Box plots, QC-filtered (PASS/PASS* only)
- `PETN_pilot_barchart.png` / `RDX_pilot_barchart.png` - Bar charts (mean per condition, error bars = SEM), QC-filtered, same data as the box plots above -- see "Bar Charts with SEM Error Bars" section below
- `PETN_pilot_boxplots_alldata.png` / `RDX_pilot_boxplots_alldata.png` - Box plots of **all** data, ignoring QC acceptance filtering, colour-coded by status
- `PETN_pilot_main_effects.png` - Main effects plots for PETN
- `PETN_pilot_interactions.png` - Interaction plots for PETN
- `PETN_pilot_diagnostics.png` - Model diagnostics for PETN
- `RDX_pilot_*.png` - Same plots for RDX

---

### **Phase 2: Main Study (Sample size TBD from pilot)**

**Purpose:**
1. Confirm effects observed in pilot
2. Achieve 80%+ statistical power for detecting 10% recovery differences
3. Test interactions with adequate precision
4. Generate publication-quality results

**Design:** (To be determined after pilot analysis)

**Expected structure:**
- **8-12 steel surfaces** + **8-12 ABS surfaces** (depending on pilot ICC)
- Each surface tested under 4 conditions
- **64-96 total samples** (within 150 sample budget)
- Optional: Add replication of critical conditions if ICC is low

**Timeline estimate:**
- Sample collection: 3-4 days
- GC-MS analysis: 3-4 days
- Data analysis: 1-2 days

---

## Statistical Model

### **Nested Mixed-Effects Model**

```r
model <- lmer(Recovery ~ Surface_type * Pressure * Solvent + 
              (1|Surface_type:SurfaceID), 
              data = data)
```

**Fixed effects:**
- `Surface_type` - steel vs abs (whole-plot factor)
- `Pressure` - low vs high (sub-plot factor)
- `Solvent` - absent vs present (sub-plot factor)
- All 2-way interactions
- 3-way interaction (Surface_type × Pressure × Solvent)

**Random effects:**
- `(1|Surface_type:SurfaceID)` - Random intercept for each surface nested within surface type

**Why this structure?**
- Accounts for repeated measures on same surface (4 tests per surface)
- Properly estimates between-surface and within-surface variance
- Correct error terms for testing whole-plot vs sub-plot effects
- More powerful than treating all tests as independent

---

## Key Metrics from Pilot Study

The pilot analysis will provide:

### **1. Variance Components**
- **σ²_between**: Variance among surfaces of same type
- **σ²_within**: Variance among tests on same surface (residual)
- **ICC** (Intraclass Correlation): `σ²_between / (σ²_between + σ²_within)`

**ICC interpretation:**
- ICC < 0.10: Very low between-surface variability → focus on replication
- ICC 0.10-0.30: Low-moderate → balanced design
- ICC 0.30-0.50: Moderate-high → prioritize more surfaces over replication
- ICC > 0.50: Very high → strongly prioritize more surfaces

**Block variance reduction = ICC** (used in power calculations)

### **2. Effect Sizes**
For each factor and interaction:
- **Partial η²** (proportion of variance explained)
- **Cohen's f** = sqrt(η² / (1 - η²))
- **95% confidence intervals**

**Cohen's f interpretation:**
- Small: f = 0.10
- Medium: f = 0.25
- Large: f = 0.40

### **3. Estimated Marginal Means**
- Mean recovery for each factor level
- Pairwise comparisons with p-values
- Standard errors and confidence intervals

### **4. Power Analysis Results**
- Minimum surfaces per type needed for 80% power
- Expected power at different sample sizes
- Total samples required for main study

---

## Requirements for Full DOE Script

**File:** `doe_nested_design.R` (to be created after pilot)

### **Input Parameters (from pilot analysis):**

```r
# Empirical variance components
empirical_icc_petn <- [from pilot]       # e.g., 0.25
empirical_icc_rdx <- [from pilot]        # e.g., 0.28

# Empirical effect sizes (Cohen's f)
surface_effect_petn <- [from pilot]      # e.g., 0.45 (large)
pressure_effect_petn <- [from pilot]     # e.g., 0.30 (medium)
solvent_effect_petn <- [from pilot]      # e.g., 0.35 (medium)

surface_effect_rdx <- [from pilot]
pressure_effect_rdx <- [from pilot]
solvent_effect_rdx <- [from pilot]

# Sample size recommendation
recommended_surfaces_per_type <- [from pilot]  # e.g., 10

# Observed standard deviations
petn_overall_sd <- [from pilot]         # e.g., 12.5%
rdx_overall_sd <- [from pilot]          # e.g., 10.8%
```

### **Script Functionality:**

1. **User Configuration Section**
   ```r
   # Design parameters
   n_surfaces_per_type <- [recommended from pilot]
   n_conditions <- 4  # Fixed: Pressure × Solvent
   
   # Factor levels
   pressure_low <- 75
   pressure_high <- 250
   solvent_absent <- "dry"
   solvent_present <- "100uL_EtOH"
   
   # Optional replication
   replicate_conditions <- FALSE  # Set TRUE if ICC < 0.20
   conditions_to_replicate <- c(1, 4)  # Which conditions to replicate
   ```

2. **Design Generation**
   - Generate steel surface IDs (Steel_01 to Steel_[n])
   - Generate ABS surface IDs (ABS_01 to ABS_[n])
   - Create full factorial of sub-plot factors (4 conditions)
   - Assign randomized test order within each surface
   - Randomize overall surface processing order
   - Add optional replication if specified
   - Create run and sample identifiers

3. **Design Validation**
   - Check balance across all factor combinations
   - Verify proper nesting structure
   - Confirm randomization at all levels
   - Calculate total samples and compare to budget

4. **Power Confirmation**
   - Use empirical ICC from pilot
   - Use empirical effect sizes from pilot
   - Calculate expected power for main study design
   - Confirm ≥80% power for target effects
   - Display power for all main effects and interactions

5. **Protocol Documentation**
   - Generate detailed protocol document
   - Include exact factor levels from pilot
   - Reference any protocol refinements from pilot learnings
   - Specify quality control measures

6. **Output Files**
   ```
   main_study_design_nested.csv     - Full design matrix
   main_study_protocol.txt           - Detailed protocol
   main_study_power_analysis.png     - Power curves
   main_study_power_summary.csv      - Power table
   ```

### **Design Matrix Columns:**

```
RunID                  - Unique run identifier (MAIN_001, MAIN_002, ...)
SampleID               - Unique sample identifier (S_Steel_01_T1, ...)
SurfaceProcessingOrder - Order in which surfaces are processed (1-[2n])
SurfaceID              - Surface identifier (Steel_01, ABS_01, ...)
Surface_type           - steel or abs
TestOrder              - Test order within surface (1-4 or 1-6 if replicated)
Pressure_level         - low or high
Pressure_g             - 75 or 250
Solvent_level          - absent or present
Solvent_detail         - dry or 100uL_EtOH
ReplicateNumber        - 1 (or 2 if replicated)
PETN_Recovery_pct      - [to be filled in after GC-MS]
RDX_Recovery_pct       - [to be filled in after GC-MS]
Test_DateTime          - Date/time of test
Operator               - Operator initials
Temp_C                 - Room temperature
Humidity_pct           - Relative humidity
Notes                  - Any observations
```

### **Analysis Script:** `main_study_analysis.R`

**Functionality:**
1. Load main study data (`main_study_data_nested.csv`)
2. Fit nested mixed-effects models (same as pilot)
3. Combined analysis (pilot + main study) for maximum power
4. Type III ANOVA with correct error terms
5. Effect sizes with confidence intervals
6. Estimated marginal means and pairwise comparisons
7. Multiple comparisons correction (FDR or Bonferroni)
8. Publication-quality figures:
   - Main effects bar plots with error bars
   - Interaction line plots with confidence bands
   - Pareto chart of effect magnitudes
   - Forest plot of effect sizes
   - Model diagnostics (residuals, Q-Q, leverage)
9. Summary tables (ANOVA, means, comparisons)
10. Export results to Excel workbook (multi-sheet)

### **Key Differences from Pilot:**

1. **Combined analysis:**
   ```r
   # Merge pilot and main study data
   combined_data <- rbind(
     pilot_data %>% mutate(Study = "Pilot"),
     main_data %>% mutate(Study = "Main")
   )
   
   # Fit model with study as covariate (if needed)
   model <- lmer(Recovery ~ Surface_type * Pressure * Solvent + Study +
                 (1|Study:Surface_type:SurfaceID), data = combined_data)
   ```

2. **Multiple comparisons correction:**
   ```r
   # Adjust p-values for 7 effects tested
   anova_results$p_fdr <- p.adjust(anova_results$`Pr(>F)`, method = "fdr")
   ```

3. **Publication figures:**
   - Higher resolution (600 dpi)
   - Consistent color scheme
   - Clear labeling for journals
   - Effect size annotations

4. **Sensitivity analysis:**
   - Test model with/without 3-way interaction
   - Check robustness to outliers
   - Validate with different random effect structures

---

## Success Criteria

### **Pilot Study Success:**
- [ ] All 32 samples collected without protocol deviations
- [ ] GC-MS analysis complete with <10% missing values
- [ ] Model assumptions satisfied (normality, homogeneity)
- [ ] ICC calculated and within reasonable range (0.10-0.50)
- [ ] Effect sizes estimated with adequate precision
- [ ] Power analysis determines feasible main study size (<150 samples)

### **Main Study Success:**
- [ ] Achieve ≥80% power for 10% recovery differences
- [ ] All main effects and interactions tested
- [ ] Model assumptions satisfied
- [ ] Results confirm or refine pilot findings
- [ ] Publication-quality figures and tables generated
- [ ] Clear recommendations for optimal swabbing protocol

---

## Expected Outcomes

Based on typical trace explosives recovery studies, we expect:

**Main Effects:**
- **Surface_type**: Large effect (f > 0.40)
  - Steel typically 10-20% higher recovery than abs
  - Due to surface porosity and material properties

- **Solvent**: Medium-large effect (f = 0.25-0.40)
  - EtOH expected to increase recovery 5-15%
  - Helps dissolve and mobilize residues

- **Pressure**: Small-medium effect (f = 0.10-0.30)
  - Higher pressure improves contact
  - Effect may be smaller due to ASTRA standardization

**Interactions:**
- **Surface × Solvent**: Possible moderate interaction
  - Solvent may help more on porous abs surfaces
  - Effect sizes: f = 0.15-0.25

- **Pressure × Solvent**: Likely small or negligible
  - Both work through different mechanisms
  - Effect sizes: f < 0.15

- **3-way interaction**: Expected to be negligible
  - May drop from model if non-significant

**Variance Components:**
- **ICC**: Expected 0.20-0.35
  - Moderate between-surface variability
  - Suggests ~6-10 surfaces per type needed

---

## GC-MS Data Collation & QC Pass/Fail Criteria (Added July 30, 2026)

### Overview

`pilot_analysis.R` now begins with a full data-collation stage (function `collate_gcms_results()`, called via `tryCatch` so a missing/partial GC-MS run doesn't crash the rest of the script) before running any statistics. This replaced an earlier, more limited approach that only read a single hardcoded `Analysis1` results file and had no QC pass/fail logic at all.

**Workflow**: run GCMSQuantitation's `Code/01_MsFilesReorganiser.R` → `02_PeakDetection.R` → `03_Quantification.R` for each Analysis run (`Analysis1`, `Analysis2`, ...), then run `pilot_analysis.R` — it auto-discovers and collates everything.

### Data Discovery

- Scans `.../ASTRA Swabbing/Pilot Study/GC Data/` recursively for `Analysis*/Results/*_GCMSResults.csv` (excludes `/Backup/`). Naming is inconsistent between runs (`Analysis1` produced `_GCMSResults.csv`, `Analysis2` produced `Analysis2_GCMSResults.csv`) — the glob pattern handles both.
- Also collates every `*_CalibrationStats.xlsx` found, into a `CalSummary` sheet.
- Fully incremental/idempotent: re-run any time a new `Analysis3`, `Analysis4`, ... folder appears; no code changes needed.
- Rows are classified by `SampleName`:
  - Pilot samples: `^PILOT_\d{3}$` (matches the design matrix `RunID`)
  - Negative controls: `^(?:PILOT_)?(Steel|ABS)Batch(\d+)[ _]?(?:NegCtrl|NC)$` (updated August 7, 2026 -- handles `...NegCtrl`, `..._NC`, and the no-`PILOT_`-prefix/space-separator `"ABSBatch4 NC"` style seen in Analysis6; see "Negative Control Regex Fix" session summary above)
  - Anything else (e.g. `"Test Std 1 - 10ng"`, system-suitability standards) is discarded

### QC Pass/Fail Criteria (ported from GCMSQuantitation FINEX pipeline)

Logic ported near-verbatim from `GCMSQuantitation/Code/04_CollateStudyResults.R` (see that repo's `CONTEXT.md` for full rationale), adapted to this study's identifiers (RunID/SurfaceID/Surface_type/Pressure/Solvent/BatchNumber instead of Lab/Participant):

- **6ng QC brackets** (nearest QC before/after each sample, per Analysis run + injection `Line`): PASS if `|%bias| ≤ 20%`. **Note**: 20% mirrors the actual value in `GCMSQuantitation/GlobalCode.R`'s `qc_bias_limit`, not the ±15% figure quoted in that repo's own `CONTEXT.md` prose — the two have drifted apart; we deliberately matched the code, not the stale docs.
- **0.2ng QC brackets**: reads pre-computed `petn_qc_flag_dc`/`rdx_qc_flag` → maps `SENSITIVITY_CHECK_PASS/WARN/FAIL` → `PASS/WARN/FAIL`.
- **QC carry-forward**: if the nearest QC didn't inject (IS not detected), searches for the last/next QC that did.
- **`analysis_accepted`**: `PASS` / `PASS*` (QC bracket missing or WARN, or per-analyte 0.2ng override — a 0.2ng QC FAIL is downgraded to a warning if the sample's own analyte is still `Quantifiable`) / `FAIL: <reasons>`. Negative controls are exempted and get `analysis_accepted = "NC"`.
- **`Outcome`**: `Complete` (PASS/PASS*) or `Reanalyse` (FAIL); `NA` for NCs.
- **Negative control evaluation** (three-tier, ported from `compute_nc_status()`): `concentration_dc == 0` → trace/WARN; SNR = `Quantifiable` → contaminated/FAIL; peak height below threshold (200 PETN / 50 RDX) → trace/WARN; else clean/PASS.
- **`nc_analysis_valid`** (ported from `compute_nc_analysis_valid()`): `TRUE` only if the internal standard (15N-RDX) was detected AND all four bracketing 0.2ng QCs are not `FAIL`. Distinguishes "was this NC injection reliable?" from "was it clean?" — a contaminated-but-valid NC still counts as a successful run.

### Recovery Calculation

- Uses this study's own constants (NOT GCMSQuantitation's FINEX-specific `DepositMass`/`DepositConc`): 50 µL × 200 ng/µL = 10,000 ng deposited, 1000 µL extraction volume → ideal concentration = 10 ng/µL at 100% recovery.
- Computed for **every** `Sample`-type row (pilot samples AND negative controls) so NC contamination can be expressed on the same recovery-equivalent scale as real samples (mirrors GCMSQuantitation's FINEX NC sheet, which also reports `petn_recovery_dc`/`rdx_recovery` for negative controls).
- PETN uses drift-corrected concentration (`petn_concentration_dc`); RDX has no drift correction in this pipeline configuration (uses 15N-RDX IS ratio quantification instead, per `use_is_for_rdx = TRUE` in `GlobalCode.R`).

### Negative Controls Included in `pilot_data_nested` (analogous to FINEX)

Unlike GCMSQuantitation's FINEX pipeline (which keeps NCs in a separate sheet only, excluded from the main `FINEX_StudyResults.csv`), **this study's `pilot_data_nested.csv`/`.xlsx` includes NC rows directly**, appended after the 32 design-matrix rows:
- `SampleType` column added: `"Pilot"` (the 32 design rows) vs `"NC"` (appended rows)
- NC rows use `RunID`/`SampleID` = the NC's raw sample name (e.g. `PILOT_ABSBatch2_NC`); design-specific columns (SurfaceID, TestOrder, Pressure, Solvent) are left blank since NCs aren't part of the factorial design; `Surface_type`/`BatchNumber` are populated from the parsed NC name
- `nc_status` and `nc_analysis_valid` columns added
- `ASTRA_PilotStudyResults.xlsx`'s separate "Negative Controls" sheet also gained `PETN_Recovery_pct`/`RDX_Recovery_pct` columns for parity with the FINEX NC sheet structure

### Additional Columns

- **`GCMS_Analysis_Date`** — the GC-MS injection date (e.g. `20260713`), named distinctly from the existing `Test_DateTime` (lab swabbing date) to avoid confusion
- **`AnalysisFolder`** — full path to the specific Analysis run folder each sample came from (e.g. `.../GC Data/Analysis1`)
- Both `pilot_data_nested` and `ASTRA_PilotStudyResults.xlsx`'s "All Data"/"Negative Controls" sheets are **sorted by analysis date** (untested samples with no date sort last)

### Outputs

1. **`pilot_data_nested.csv`** (canonical, read by the statistics section below) — updated in place, original 32-row schema plus all the new columns above, plus appended NC rows.
2. **`pilot_data_nested.xlsx`** — new formatted sibling (CSV can't carry cell colours) with the same conditional formatting as below.
3. **`ASTRA_PilotStudyResults.xlsx`** (in `Pilot Study/GC Data/`, analogous to `FINEX_StudyResults.xlsx`'s location relative to `Accepted Analysis`) — sheets: **All Data**, **Steel**, **ABS**, **Negative Controls**, **Summary_By_Condition** (mean/SD/RSD by Surface_type × Pressure × Solvent, PASS/PASS* only), **SampleSummary** (counts + PASS/PASS*/FAIL by surface, "Complete %" vs the 32-sample pilot target), **CalSummary**.

**Conditional formatting** (green=PASS, amber=PASS*/WARN, red=FAIL) applied via a shared `add_sheet()` helper (defined once, reused for both workbooks) on: `analysis_accepted`, `Outcome`, all QC bracket columns, `petn_snr_flag`/`rdx_snr_flag`, `nc_status`, and `nc_analysis_valid` (TRUE=green/FALSE=red).

### QC Filter in Statistical Analysis (mirrors GCMSQuantitation's July 2026 bug fix)

Before fitting any mixed-effects models, `pilot_data` is filtered to `analysis_accepted %in% c("PASS", "PASS*")` (or `NA`, for untested rows) — mirroring the exact bug GCMSQuantitation's FINEX pipeline hit and fixed ("Critical QC Filtering Issue", July 3 2026 session): including QC-failed samples artificially dilutes variance components and effect sizes. NC rows (`analysis_accepted == "NC"`) are correctly excluded from statistics by this same filter.

### All-Data Box Plots (ignoring QC filters)

`generate_all_data_boxplots()` (calls the extended `generate_boxplots()` with `show_points = TRUE`) plots **every** sample with a recovery value — PASS, PASS*, FAIL, and NC alike — as individual jittered points colour-coded by QC status (green=PASS, orange=PASS*, red=FAIL, blue=NC, grey=Untested), overlaid on the same box-and-diamond-mean plot. Runs unconditionally (independent of `analysis_mode`), producing `PETN_pilot_boxplots_alldata.png`/`RDX_pilot_boxplots_alldata.png` alongside the QC-filtered plots. Negative controls (which have no Pressure/Solvent condition) get an explicit `"NC"` category on the x-axis rather than falling through to an unlabeled `"NA"` bucket.

### Bugs Found and Fixed This Session

1. **Recovery values silently dropped after adding NC support.** The design template (`pilot_design_nested.csv`) already has placeholder `PETN_Recovery_pct`/`RDX_Recovery_pct` columns (all `NA`). When recovery was computed on `samples` *before* the `left_join` onto the design matrix, the join created `.x`/`.y` suffixed duplicates instead of using the real values, and `pilot_rows$PETN_Recovery_pct` silently evaluated to nothing. **Fix**: drop the design template's placeholder recovery columns before joining (`select(-any_of(c("PETN_Recovery_pct","RDX_Recovery_pct")))`).
2. **`car::recode`/`dplyr::recode` namespace collision.** A redundant no-op `recode()` call (mapping `"abs"→"abs"`, `"steel"→"steel"`) broke when sourced from within `pilot_analysis.R`, because `car` (loaded for Type III ANOVA) masks `dplyr::recode()` with an incompatible signature. Fixed by deleting the no-op call entirely.
3. **`AcceptanceStatus` not found in `geom_jitter()`.** The colour-status column was added to `data_valid` *after* `ggplot(data_valid, ...)` had already captured its data snapshot (data frames are copied by value in R). Fixed by computing `AcceptanceStatus` before building the `ggplot` object.

### Findings From the Actual Pilot Data So Far (12 of 32 samples processed)

- **Analysis1**: PETN drift correction succeeded (power-law model, 4 QCs at 6ng — meets the ≥3 minimum). **Analysis2**: drift correction was skipped entirely — only 2 QCs at 6ng, below GCMSQuantitation's minimum of 3 for a power-law fit — so all Analysis2 PETN concentrations are uncorrected, and most Analysis2 samples fail the "PETN 6ng post QC FAIL" check. **Action for future batches**: ensure ≥3 QC injections at the 6ng level per Analysis run sequence design.
- **PETN has substantially more missing data than RDX** (e.g. 7 of 16 processed rows show no detectable PETN peak at all vs 1 of 16 for RDX). Root causes: PETN's higher minimum-peak-height threshold (200 vs RDX's 50 counts), PETN's greater thermal lability/inlet adsorption (nitrate ester vs RDX's nitramine), and PETN having no effective internal standard (unlike RDX's 15N-RDX, ~1.2% RSD) to compensate for signal drift — compounded by Analysis2's failed drift correction (above).
- Of the 4 negative controls processed so far, 3 are **analytically invalid** (`nc_analysis_valid = FALSE`): 2 due to failed PETN 0.2ng bracketing QCs, 1 due to the internal standard not being detected in that injection at all. Only 1 NC is both valid and shows real contamination (`NC FAIL: RDX contaminated`) — a valid run correctly detecting a real problem, as opposed to the other 3 which are unreliable results independent of how "clean" they looked.

---

## IS Peak Area Injection-Validity Check (Added July 31, 2026)

### Overview

Ported near-verbatim from GCMSQuantitation's July 31, 2026 fix (see that repo's `CONTEXT.md`, "Session Summary (July 31, 2026) — IS Peak Area Injection-Validity Check"), for the same reason it was needed there: `compute_analysis_accepted()`'s SNR checks only fail a sample when the flag is a non-NA value other than `"Quantifiable"` (e.g. `"Below_LOD"`). A completely failed injection (no peak found at all -> flag is `NA`, not `"Below_LOD"`) was silently passing as `PASS`/`Complete`, and there was no check at all on the internal standard (15N-RDX) for regular sample rows.

**Column consumed, not computed here**: `rdx_is_pa_flag` (`"OK"` / `"LOW"` / `"NOT_DETECTED"`) is computed upstream in `GCMSQuantitation/Code/03_Quantification.R` (against `MinPeakArea_IS = 1000` in `GlobalCode.R`) and simply read from the `*_GCMSResults.csv` files this script already loads — no new threshold logic needed in this repo. Since `all_data` is built via `bind_rows()` across all `Analysis*` datasets, the column exists (with `NA` for any dataset not yet reprocessed with the updated GCMSQuantitation code) as soon as **any** dataset has it.

### Changes Made (mirroring GCMSQuantitation exactly)

1. **`compute_analysis_accepted()`**: added an IS-validity check block (after the PETN/RDX SNR checks, before the QC bracket loop) that adds a hard failure — `"IS not detected (injection failure suspected)"` or `"IS abnormally low (PA=X)"` — independent of what the analyte peaks show. Guarded by `"rdx_is_pa_flag" %in% names(df)` so Analysis runs not yet reprocessed degrade gracefully (check simply skipped, not a crash).
2. **`compute_nc_analysis_valid()`**: `is_ok` now also requires `rdx_is_pa_flag == "OK"`, alongside the existing `rdx_is_snr_flag` detection check.
3. **`desired_cols`** (All Data/Steel/ABS sheets): added `rdx_is_pa_flag` right after `analysis_accepted`/`Outcome`.
4. **`desired_cols_nc`** (Negative Controls sheet): added `rdx_is_pa_flag` next to the existing `rdx_is_snr_flag` audit column.
5. **`pilot_data_nested` template columns**: added `rdx_is_pa_flag` to the "ensure columns exist" list, the per-row copy loop (guarded for datasets lacking the column), and the negative-control appended-rows `data.frame()`.
6. **`add_sheet()` conditional formatting**: added a block for `rdx_is_pa_flag` (green=`OK`, amber=`LOW`, red=`NOT_DETECTED`), same pattern as the existing `petn_snr_flag`/`rdx_snr_flag` block.

### Verification

Re-ran the full collation against the real data (`Analysis1` already reprocessed with the updated GCMSQuantitation pipeline at the time of this port; `Analysis2`/`Analysis3` reprocessed immediately afterward, same session):

- `Analysis1`: all 32 Cal/QC/Sample rows show `rdx_is_pa_flag = "OK"` — no IS injection failures present in this batch, so no new `analysis_accepted` failures were introduced (confirmed no regression).
- `Analysis2`: 16 `OK`, 1 `NOT_DETECTED` (`PILOT_ABSBatch3_NC`) — correctly surfaces as `nc_analysis_valid = FALSE` even though `nc_status = "NC PASS"` (looked clean but the run itself wasn't reliable — exactly the intended distinction).
- `Analysis3`: 22 `OK`, no failures.
- `pilot_data_nested.csv`/`.xlsx` and `ASTRA_PilotStudyResults.xlsx` all regenerated successfully with the new column visible and correctly formatted across all three datasets.

**Files modified**: `pilot_analysis.R` (`compute_analysis_accepted()`, `compute_nc_analysis_valid()`, `desired_cols`, `desired_cols_nc`, `add_sheet()`, `pilot_data_nested` template column handling). `GCMSQuantitation/Code/03_Quantification.R` was re-run against `Analysis2` and `Analysis3` (via `GlobalCode.R` with `DataFolder` pointed at each in turn) to populate `rdx_is_pa_flag` for those datasets too.

---

## Reanalysis / Duplicate-Sample Handling (Added July 31, 2026)

### Problem

When a sample fails QC, the natural workflow is to re-run it in a later `Analysis` folder (`Analysis4`, `Analysis5`, ...). Since `collate_gcms_results()` discovers and `bind_rows()`s **every** `Analysis*/Results/*_GCMSResults.csv` it finds, the same `RunID` (or the same negative control identity) then appears more than once in `all_data`/`pilot_rows`/`nc_rows`.

This was not handled at all previously:
- The `design_data` update loop (`for (i in seq_len(nrow(pilot_rows))) ...`) matches each row to its `RunID` in the 32-row design template and **overwrites** that row's values every time a match is found. With duplicate `RunID`s, whichever attempt happened to be processed **last** in `pilot_rows`'s row order silently won -- with no way to tell from the output that this had happened.
- That row order comes from `list.files(..., recursive = TRUE)`, which sorts **alphabetically**, not chronologically. `sort(c("Analysis1","Analysis2","Analysis10","Analysis9"))` -> `"Analysis1" "Analysis10" "Analysis2" "Analysis9"` -- i.e. once there are 10+ Analysis runs, "last in file order" stops meaning "most recent" and the wrong attempt could silently be selected.
- `all_data_sheet`/`nc_sheet` (the "All Data"/"Steel"/"ABS"/"Negative Controls" sheets in `ASTRA_PilotStudyResults.xlsx`) would show **both** attempts as separate rows with no indication either was superseded, and if both attempts happened to pass QC, `Summary_By_Condition`/`SampleSummary` would double-count that physical sample.

### Fix

Added a `select_best_attempt()` helper (defined once, used for both pilot samples and NCs, right before the acceptance-computation step) that collapses duplicates down to exactly one row per `SampleName_trimmed`:

```r
select_best_attempt <- function(df, quality_ok) {
  df$..quality_ok <- quality_ok
  df %>%
    group_by(SampleName_trimmed) %>%
    mutate(N_Attempts = n()) %>%
    ungroup() %>%
    arrange(SampleName_trimmed, desc(..quality_ok), desc(Date)) %>%
    group_by(SampleName_trimmed) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(MultipleAttempts = N_Attempts > 1) %>%
    select(-..quality_ok)
}
```

**Selection rule**: prefer any accepted attempt over any failed one (`analysis_accepted %in% c("PASS","PASS*")` for pilot samples; `nc_analysis_valid == TRUE` for NCs); among ties (all accepted, or all failed), the most recent attempt by actual GC-MS **`Date`** wins -- not file-discovery order, so this is correct regardless of how many Analysis folders exist or how they're named. If an *older* attempt passed and a *newer* one failed, the older passing attempt is kept (an old PASS beats a new FAIL) -- validated with a standalone test case before wiring into the main script.

**Applied to both `pilot_rows` (immediately after `analysis_accepted`/`Outcome` are computed) and `nc_rows` (immediately after `nc_status`/`nc_analysis_valid` are computed)**, each before their respective counts are printed and before either feeds into `design_data`/`all_data_sheet`/`nc_sheet` -- so every downstream consumer (the canonical `pilot_data_nested.csv`, the study workbook sheets, `Summary_By_Condition`, `SampleSummary`, and the statistical analysis section) sees only the selected attempt, eliminating the double-counting risk.

**New columns** `N_Attempts` (total analysis attempts found for that sample/NC) and `MultipleAttempts` (`N_Attempts > 1`) retain visibility that a reanalysis happened, even though only the winning attempt is kept. Added to `desired_cols`/`desired_cols_nc`, the `pilot_data_nested` template column list + copy loop + NC append rows, and `add_sheet()` conditional formatting (`MultipleAttempts == TRUE` highlighted amber, as a "heads up, this was reanalysed" flag rather than a pass/fail colour).

**`report_multiple_attempts()`** helper prints which `SampleName_trimmed` values had more than one attempt to the console during collation, so reanalysis is visible in the run log even without opening the spreadsheet.

### Verification

- Standalone test (`select_best_attempt()` on a synthetic 6-row/3-sample data frame with mixed PASS/FAIL attempts at different dates) confirmed: a sample with an old FAIL + new PASS selects the new PASS; a sample with two FAILs + one PASS at the *oldest* date still selects that PASS over the newer FAILs.
- Ran the real pipeline (no actual duplicate `RunID`s exist yet across `Analysis1`/`2`/`3`): output unchanged from before this fix (`N_Attempts = 1`, `MultipleAttempts = FALSE` for every current sample/NC) -- confirms no regression on the existing, non-duplicated dataset.

**Files modified**: `pilot_analysis.R` (`select_best_attempt()`/`report_multiple_attempts()` helpers, applied to `pilot_rows` and `nc_rows`; `desired_cols`, `desired_cols_nc`, `pilot_data_nested` template handling, `add_sheet()` conditional formatting).

---

## Bar Charts with SEM Error Bars (Added July 31, 2026)

### Overview

Companion visualisation to the existing box plots, for easier visual comparison of the 8 treatment conditions at a glance. New `generate_barchart()` function (defined immediately after `generate_boxplots()`), called wherever `generate_boxplots(pilot_data, ...)`/`generate_boxplots(data_clean, ...)` already is -- both the `"boxplots_only"` short-circuit path and the per-analyte `analyze_analyte()` function used in `"full"` mode -- so the bar chart is always produced alongside the box plot, using the same QC-filtered data (PASS/PASS* only).

**Design** (one bar chart per analyte, `{Analyte}_pilot_barchart.png`):
- **X-axis**: Condition (`50g/dry`, `50g/wet`, `200g/dry`, `200g/wet` -- Pressure x Solvent)
- **Facets**: `Surface_type` (Steel | ABS), same layout as the existing box plots
- **Bar height**: mean recovery (%) for that Surface_type x Condition group
- **Error bars**: standard error of the mean, SEM = SD / sqrt(n). Only drawn when n > 1 (a single-replicate group has an undefined SD, so no error bar is drawn -- not a zero-width one) -- same convention as GCMSQuantitation's by-participant bar charts (`Code/04_CollateStudyResults.R`)
- **`n=X` labels** above each bar showing the replicate count for that group
- Negative controls (no Pressure/Solvent level) are dropped rather than plotted as an "NA" bar

### Verification

Ran against the real pilot data (12 QC-passed samples with recovery values at the time of this change): both `PETN_pilot_barchart.png` and `RDX_pilot_barchart.png` generated correctly -- bars present for every Surface_type x Condition combination with at least 1 sample, error bars correctly shown for n>1 groups and correctly omitted for n=1 groups (e.g. ABS/200g-wet), `n=` labels positioned above each bar/error-bar.

**Files modified**: `pilot_analysis.R` (new `generate_barchart()` function; calls added in the `"boxplots_only"` block and inside `analyze_analyte()`).

---

## Box Plot Y-Axis Fixed to 0-50% + Sample Size Labels (Added August 2026)

### Change: Zoomed-In Fixed Axis and n= Labels for `generate_boxplots()`

**Problem**: `generate_boxplots()` used a hardcoded `coord_cartesian(ylim = c(0, 100))`. Real pilot data (checked directly against `pilot_data_nested.csv`, 39 rows) currently tops out around ~25% (PETN, max 25.17%) and ~29% (RDX, max 29.18%) -- so the 0-100% frame left roughly half the plot empty, compressing the actual box-and-whisker detail into the bottom quarter of the image. There was also no indication of how many replicates contributed to each box.

**Fix applied**:
1. **Y-axis capped at 0-50%** instead of 0-100% (`y_axis_max <- 50`, used in `coord_cartesian(ylim = c(0, y_axis_max))`) -- zooms in on the actual data range while leaving reasonable headroom above the highest values observed so far. `coord_cartesian()` only clips the rendered view; box statistics are still computed from the full underlying data.
2. **`n=` sample-size labels** added above each box, one per (Condition, Surface_type) group -- mirrors the equivalent labels in GCMSQuantitation's `Code/04_CollateStudyResults.R` `create_boxplot()`. New `counts_df` computed via `aggregate(..., FUN = length)` alongside the existing `means_df`, then plotted via `geom_text(data = counts_df, aes(x = Condition, y = label_y, label = paste0("n=", n)), ...)` at a fixed height near the top of the frame (`label_y <- y_axis_max * 0.96`).

Applies to every plot produced by `generate_boxplots()`: the QC-filtered `PETN_pilot_boxplots.png`/`RDX_pilot_boxplots.png` and the all-data variants (`*_boxplots_alldata.png`).

**Verification**: Extracted the exact updated function and ran it directly against the real `pilot_data_nested.csv` (39 rows, 19 PETN / 28 RDX with data). Confirmed both plots regenerated with the 0-50% frame, `n=X` labels correctly positioned above every box in both the ABS and Steel facets (matching the actual per-condition replicate counts, e.g. Steel 50g/dry n=4, ABS 200g/wet n=1), and the previously-compressed box/whisker detail now clearly visible across most of the plot height.

**Files modified**: `pilot_analysis.R` (`generate_boxplots()`, `counts_df` + axis/label changes), `CONTEXT.md` (this entry).

---


---

## Files in This Directory

### **Created (Pilot Phase):**
- `pilot_study_design.R` - Pilot design generation script
- `pilot_analysis.R` - Pilot data analysis script (collates GC-MS results, applies QC pass/fail criteria, computes recovery, then runs the statistical analysis -- see "GC-MS Data Collation" section above)
- `pilot_design_nested.csv` - Pilot design matrix
- `pilot_design_batch_organized.csv` - Batch workflow design matrix (source of `BatchNumber`)
- `doe_explosives_recovery.R` - Original (incorrect) design script (archived)
- `design_matrix.csv` - Original non-nested design (archived)
- `design_matrix_blocked.csv` - Original blocked design (archived)
- `power_analysis.png` - Original power analysis (archived)

### **Created (GC-MS Collation, July 30, 2026):**
- `pilot_data_nested.csv` / `.xlsx` - Recovery + QC acceptance data, including negative controls (see above)
- `ASTRA_PilotStudyResults.xlsx` - Collated multi-sheet study workbook (in `Pilot Study/GC Data/`)
- `PETN_pilot_boxplots_alldata.png` / `RDX_pilot_boxplots_alldata.png` - All-data box plots, QC filter ignored

### **Created (Pilot Full Analysis -- see "Statistical Results Persisted"/"Power Analysis Overhaul" sessions below):**
- `PETN_pilot_main_effects.png` / `RDX_pilot_main_effects.png` -- superseded by `PETN_pilot_interactions.png`/`RDX_pilot_interactions.png` and the by-condition box/bar charts (main-effects-only plots were never actually produced separately; interaction plots subsume them)
- `PETN_pilot_interactions.png` / `RDX_pilot_interactions.png` - Interaction plots from pilot
- `PETN_pilot_diagnostics.png` / `RDX_pilot_diagnostics.png` - Model diagnostics from pilot
- `pilot_analysis_summary.txt` - Full statistical summary (all sections captured via `sink()`)

### **To Create (Main Study):**
- Nothing outstanding at the file level -- `main_study_protocol.txt` drafted August 19, 2026 (see session summary), covering Batches 1-2 as a completed historical record and Batches 3-5 as pending checklists. Will need re-review/re-drafting of the Batch 3-5 sections once those batches are actually executed (fill in real Swabbed times/Temp/RH, mark complete).

**Created (August 11, 2026 -- see "Main Study Design: Multi-Level Pressure Dose-Response" session below):**
- `doe_nested_design.R` - Main study design generator (5 pressure levels, reused + new surfaces, randomized NC placement)
- `main_study_design_nested.csv` - Main study design matrix + lab notes template (54 rows: 44 samples + 10 NCs, real execution order)
- `main_study_negative_controls.csv` / `main_study_batch_checklist.csv` / `main_study_surface_use_tally.csv` - Supporting outputs (see session summary for each file's purpose)

**Created (August 19, 2026 -- see "`main_study_analysis.R` Created" session at the top of this file):**
- `main_study_analysis.R` - Main study analysis script: collation + QC acceptance (shared logic with the pilot), pilot 50g/200g pooling with `Study` covariate, `CumulativeUseNumber`, combined mixed-effects model, preliminary box plots, retrospective + non-circular a priori/MDES power analysis
- `main_study_data_nested.csv` / `.xlsx` - Main study data with recovery values + QC columns (incremental -- currently reflects `Analysis1` only; rerun as more batches are collected)
- `main_study_results.xlsx` - Collated multi-sheet study workbook (in `Main Study/GC Data/`)
- `PETN_RDX_main_preliminary.png` - Preliminary box plots, Recovery by Pressure_g, faceted Surface_type x Analyte, pilot+main pooled
- `main_study_analysis_summary.txt` - Full statistical summary (written when `analysis_mode <- "full"`)

---

## References and Resources

**Statistical methods:**
- Montgomery, D.C. (2017). *Design and Analysis of Experiments*. 9th ed.
- Oehlert, G.W. (2010). *A First Course in Design and Analysis of Experiments*.
- Bates, D. et al. (2015). "Fitting Linear Mixed-Effects Models Using lme4." *Journal of Statistical Software*.

**R packages:**
- `lme4`, `lmerTest` - Mixed-effects models
- `car` - Type III ANOVA
- `effectsize` - Effect size calculations
- `emmeans` - Estimated marginal means
- `pwr` - Power analysis
- `ggplot2` - Visualization

**Cohen's conventions:**
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*. 2nd ed.
- Small effect: f = 0.10, d = 0.20, η² = 0.01
- Medium effect: f = 0.25, d = 0.50, η² = 0.06
- Large effect: f = 0.40, d = 0.80, η² = 0.14

---

## Contact and Support

For questions about experimental design or statistical analysis:
- Review this CONTEXT.md file
- Check R script comments for detailed explanations
- Consult original design references above

---

**Last updated:** 2026-07-30 (GC-MS data collation, QC pass/fail criteria, and negative control handling added -- see "GC-MS Data Collation & QC Pass/Fail Criteria" section)  
**Next update:** After Batches 3-4 of the pilot study are processed / after pilot study completion
