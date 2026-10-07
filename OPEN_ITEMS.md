# Cross-Project Open Items

A single index of every genuinely open question, pending decision, or blocked task currently scattered across the various `CONTEXT.md` files in this repo. This file does **not** replace those entries — it points back to them. When an item here gets resolved, update its status below, but write the actual resolution detail in the originating `CONTEXT.md`'s session log (consistent with how every other decision in this repo is recorded).

Each item is tagged with one of:
- **[Lab work]** — needs physical bench/instrument time, not further analysis.
- **[Decision]** — a judgement call explicitly left open, usually deferred to the thesis author.
- **[Limitation]** — a known gap, deliberately deprioritised (not an oversight).
- **[Blocked]** — can't proceed in this environment (e.g. missing/unsynced data).
- **[Investigation]** — still actively being chased, no blocker, just not finished.

Last compiled: October 2026 (from `GCMSQuantitation/CONTEXT.md`, `ASTRA/doe/CONTEXT.md`, `UVSwabbing/CONTEXT.md`, `SartoriusBalance/CONTEXT.md`, `ASTRA/README.md`).

---

## GCMSQuantitation

See [`GCMSQuantitation/CONTEXT.md`](GCMSQuantitation/CONTEXT.md) for full detail on every item below (search for the session date/heading named).

- **[Limitation]** Generic "other significant peak" detection for blanks is hard-coded `FALSE` (`evaluate_blanks()`'s `blank_other_peak_flag`) — no logic scans the full chromatogram for a generic unexpected-peak signature. *Deferred, Aug 10 2026 session.*
- **[Decision]** `assign_blank_brackets()` treats a missing bracketing blank as "clean" rather than a hard FAIL, unlike QC brackets (which require both sides). Flagged as an assumption worth revisiting. *Aug 10 2026 session.*
- **[Investigation]** `tic_other_peak_cluster_tolerance` (5s) is a first-pass empirical value from only 2 example spikes — revisit once a larger peak-log population exists. *Aug 18 2026 session.*
- **[Investigation]** Two recurring Blank-dominant TIC clusters (RT~417.6s / ~421.6s) haven't been checked against `acqmeth.txt`/known column-bleed ranges to identify their source. *Aug 18 2026 session.*
- **[Limitation]** ASTRA has no equivalent of FINEX's recurring-"other peak" log (`04_CollateStudyResults.R` Section 13a) — explicitly out of originally agreed scope. *Aug 18 2026 session.*
- **[Decision]** No way to distinguish "this specific 6ng QC standard suffered unusually severe unprotected-inlet loss" from "a real accuracy problem affecting samples too" within the existing ±20% bias threshold. Flagged as worth a future design discussion (e.g. bracket-position-aware step-detection alert). *Aug 19 2026 session.*
- **[Limitation]** ICH Q2(R2) QC-replicate LOD/LOQ columns are populated for all 30 datasets as of the Oct 6 2026 reprocessing run — confirm this stays current if new datasets are added (low-risk, additive whenever convenient). *Oct 6 2026 session / `Diagnostics/Generic/ICH_LODLOQ_Output/LODLOQ_Investigation_Narrative.md`.*
- **[Limitation]** `ParentFolder <- sub(".*/", "", DataFolder)` in `GlobalCode.R` silently produces an empty dataset-name prefix if `DataFolder` has a trailing slash (root cause of the Lab29/Lab33 duplicate-file incident). No guard added. *Jul 29 2026 session.*
- **[Decision]** `DualMethodProcess.R`'s hardcoded `data_dir` no longer exists as a single folder (now split into "-1st half"/"-2nd half"); whether to merge the folders or skip the script was left to the user, unresolved. *Sept 4 2026 session.*
- **[Blocked]** `Diagnostics/FINEX/Dual_PA_Power_Validation.R` — `Error: Column 'predicted_petn_pa' doesn't exist`. Real pre-existing bug, not picked back up. *Sept 3-4 2026 sessions.*
- **[Blocked]** `Diagnostics/OtherStudies/mz52_Diagnostic.R` — data-shape mismatch (`differing number of rows: 1, 0`). Real pre-existing bug, not picked back up. *Sept 3-4 2026 sessions.*
- **[Decision]** Sample-prep recommendations from the old Lab14-Glass signal-loss diagnosis never confirmed as adopted into lab protocol: (1) remove wooden stick entirely before extraction, (2) centrifuge before filtering, (3) bracket every 3 samples instead of 7. (Other items from the same list were later adopted or superseded — see the file for detail.) *Part 3, static section.*
- **[Lab work]** `Reports/TMC4.txt` (thesis report draft) has an incomplete Standard Preparation section (2.3.1) and several unresolved reviewer-flagged inconsistencies (figure numbering, a stated "slower RDX decline" that contradicts the data, a 34%-vs-18% ratio-drop discrepancy, a wrong figure reference). *Part 3, "Report Progress (TMC4.txt)" section.*

---

## ASTRA (DOE / Main Study)

See [`ASTRA/doe/CONTEXT.md`](ASTRA/doe/CONTEXT.md) for full detail.

- **[Investigation]** ABS Negative-Control contamination source (pre-contaminated stock vs. handling/storage vs. equipment carryover) still unidentified, despite the Steel-vs-ABS split now being on firmer evidentiary footing. **Also tracked in `GCMSQuantitation/CONTEXT.md`'s Sept 8 2026 session** — see ASTRA's own CONTEXT.md for the full investigation; don't duplicate work across both files. *Sept 8 2026 session (carried over from Aug 27/Sept 2).*
- **[Investigation]** `rdx_batch_effect_with_main_study.R` — the RDX-recovery-vs-calendar-date/batch investigation remains unconcluded. *Sept 2 2026 session.*
- **[Lab work]** `MAIN_013` — isolated 6ng QC fail traced to an instrument wobble, not a sample defect. Reanalysis needed; not yet done. *Sept 2 2026 session.*
- **[Lab work]** `MAIN_SteelBatch1_NC` / `SteelBatch2_NC` — bracket failures from a bad RDX 0.2ng calibration point; confirmed not fixable by excluding that point (made it worse, reverted). Pending physical reanalysis. *Sept 2 2026 session.*
- **[Lab work]** `MAIN_ABSBatch1_NC` / `ABSBatch2_NC` — genuine RDX-positive result, but the reported 0% recovery is a calibration-flooring artefact, not a true zero. Pending reanalysis for a trustworthy figure. *Sept 2 2026 session.*
- **[Lab work]** `MAIN_ABSBatch3_NC` — genuine, isolated IS injection-validity failure (PA=105), not fixable from existing data. Pending reanalysis. *Sept 2 2026 session.*
- **[Decision]** `MAIN_ABSBatch4_NC` / `ABSBatch5_NC` — bracketed by a marginal (0.9 percentage-point) 6ng RDX QC bias boundary case. Explicitly left as an open judgement call: is ±20% being read too strictly at the margin? *Sept 2 2026 session.*
- **[Decision]** Pilot Study's `pilot_data_nested.xlsx` has no equivalent to Main Study's new `Standard` (stock prep. vial) column, needed to test whether Main's earliest-used prep is a literal continuation of Pilot's documented Prep B. Flagged to the user, unresolved. *Aug 25 2026 session.*
- **[Investigation]** Whether the RDX calibration/QC standard solution itself degrades over the ~3-4 week pilot span (independent of instrument response), and storage conditions during the extraction delay — untested. *Aug 19 2026 session, explicitly labelled "Still open."*
- **[Decision]** Whether PETN's own `Below_LOQ` cases should also be zero-treated, matching the fix already applied to RDX (`PILOT_018`/`PILOT_032`/1 NC would qualify). Deliberately left as a separate decision, not applied. *Aug 6 2026 session.*
- **[Decision]** `select_best_attempt()`'s "prefer PASS over FAIL" tie-break can select a degenerate 2-QC drift fit over a more-trustworthy 4-QC attempt that fails on an unrelated QC (PILOT_006/019/030/004). Not fixed, no action taken pending further direction. *Undated diagnostic entry.*
- **[Lab work]** `main_study_protocol.txt` Batches 3-5 are marked `PENDING` — RunID/SurfaceID/Pressure pre-filled, but Swabbed time/Temp/RH/deviations await actual batch execution. Protocol doc itself will need re-drafting once executed. *`ASTRA/doe/ExperimentalDesign/main_study_protocol.txt`.*
- **[Limitation]** `simr`-based genuine simulation power check was installed specifically for this purpose but never wired into `main_study_analysis.R` — Sections 6/6b still use `pwr::pwr.anova.test()`'s one-way-ANOVA approximation rather than simulating the actual quadratic/hinge mixed model. *Aug 19 2026 session, "Still To Do."*
- **[Decision]** Whether `pressure_literature_effect_pct`/`meaningful_recovery_difference_pct` need rescaling now that the recovery-efficiency correction changed Recovery%'s absolute scale. Per explicit user instruction, only a display bug was patched — the underlying rescaling question is still open. *Sept 10 2026 session (newest entry in the file).*

---

## UVSwabbing

See [`UVSwabbing/CONTEXT.md`](UVSwabbing/CONTEXT.md) for full detail.

- **[Investigation]** Is the UV powder homogeneous or subject to settling/separation? *"Open Questions" section, item 3.*
- **[Investigation]** What is the particle size distribution of UV powder? *"Open Questions" section, item 4.*
- **[Decision]** Would the positive calibration result (proportional recovery at 25% loading) translate to a positive UV-PETN correlation in a repeat swabbing study? This is the single most important unresolved question in this project — the original FINEX study's negative correlation was attributed to powder overloading, but that explanation has never been confirmed with a repeat study. *"Open Questions" item 6, same substance as the "Still Recommended" item below.*
- **[Decision]** "Still Recommended if repeating the study": use a darkroom/lighting box, add a fluorescence reference card, use single threshold k=20 at 25% powder with no alignment, and repeat the swabbing study to directly confirm the positive UV-PETN correlation above. None of these have been acted on.
- **[Blocked]** `UV_Calibration_Analysis.R`'s own input folder is not populated in this local OneDrive checkout — blocks execution/colour verification. Needs the raw ImageJ pipeline re-run against original images.
- **[Blocked]** The numbered `04`-`07` pipeline scripts fail (`04-BlankImageAnalysis.R`: `pivot_longer` on NULL) because `01`-`03`'s data-prep output folders are empty in this checkout. Only 3 of 8 scripts in this folder have ever been executed successfully with colours visually confirmed; the other 5 remain blocked. *Last updated Sept 4 2026.*

---

## SartoriusBalance

See [`SartoriusBalance/CONTEXT.md`](SartoriusBalance/CONTEXT.md) for full detail.

- **[Blocked]** The balance's SBI output text format depends on the "Standard 1/2/3" printout setting chosen in the task settings — not yet verified against the real parser. Run a short test and check the `raw_line` column before trusting a full logging run. This script has never been run against real hardware output.

---

## Aspirational / future enhancements (not tied to any open decision)

`ASTRA/README.md`'s "Future Enhancement Ideas" wishlist — real-time processing, pressure drift correction, ML-based cycle detection, a web viewer, automated reports, vectorizing known-slow row-by-row loops, and a `testthat` regression suite for `clean_and_read_csv()`/`detect_stable_by_threshold()`/`calculate_cumulative_distance()`. None of these are blocking anything; listed here only so they're not lost.
