# LOD/LOQ Investigation: SNR Convention, ICH Calibration-Curve Validation, and the QC-Replicate Fix

**Status: adopted and live in `Code/03_Quantification.R` (reporting/validation metric only — does not gate sample acceptance).**
**Date: October 2026.** Full session-by-session detail is in `CONTEXT.md`; this document is the consolidated narrative for citation in the thesis methods/validation section.

---

## 1. Motivation

The live pipeline's per-injection QC acceptance uses an SD-based signal-to-noise convention (`snr_detect <- 3`, `snr_quant <- 10`, `GlobalCode.R`/`Code/ModPeaks.R::calculate_snr()`): `SNR = peak_height / SD(noise_region)`. The traditional chromatography convention these numbers are usually quoted from (USP ⟨621⟩, Ph. Eur. 2.2.46) defines S/N as `2H/h`, where `h` is the full **peak-to-peak** noise range in a blank injection — a different, and empirically smaller, denominator than SD. ICH Q2(R2) §3.2.3 accepts S/N≈3:1/≥10:1 as one of three valid LOD/LOQ approaches but does not mandate which noise convention to use, leaving this genuinely ambiguous.

## 2. Empirical measurement: SD vs peak-to-peak

176 real noise-region measurements (both PETN and RDX channels, 44 blank injections each from two independent FINEX datasets) gave a consistent peak-to-peak/SD ratio of **3.73–3.90×** — close to the ~5× "rule of thumb" widely cited in the chromatography literature (e.g. for Gaussian noise, peak-to-peak ≈ 5.16×SD at 99% confidence; LabVeda, 2026; ASDL/Chemistry LibreTexts). This confirms the live SD-based thresholds are considerably more lenient than a traditional peak-to-peak reading — but by itself, this doesn't tell us whether the live thresholds are *wrong*, only that they're a different convention.

## 3. Independent validation: ICH's calibration-curve LOD/LOQ

Rather than argue about which noise convention is "correct," a completely independent, ICH-sanctioned method was computed: the calibration-curve approach (ICH Q2(R2) §3.2.3's third option), `LOD = 3.3σ/S`, `LOQ = 10σ/S`.

**Methodology decisions** (agreed before implementation):
- **σ**: residual standard error of the weighted quadratic calibration fit (Dolan JW, *Separation Science* 2026, "Chromatographic Measurements, Part 5").
- **S**: because every calibration curve in this pipeline is a weighted quadratic (not linear, as ICH's formula implicitly assumes), S was taken as the **local derivative** of the fitted curve at the lowest calibration standard's concentration — consistent with (a) Cuadros-Rodríguez L. et al., *Anal. Bioanal. Chem.* 2011, 401(9), 2881–2889 (adapting the same IUPAC/ICH methodology to a different nonlinear calibration shape), and (b) the delta method (JCGM 100:2008, GUM) — the same technique this codebase's own Item 19 PETN drift-correction uncertainty propagation already uses.

**Result** (60 calibration sets across all 30 datasets, both studies, R²=0.978–0.9999): the live SNR system calls results "Quantifiable" at concentrations roughly **10–20× below** where this independent method places the LOQ (median empirical floor 0.03–0.08 ng vs. ICH-derived LOQ 0.79–1.13 ng).

## 4. Real-data impact of tightening the thresholds (would it help?)

Row-level simulation (697 real sample rows, 149 QC injections/level/analyte, all 30 datasets):
- The **6ng (quantitative) QC level is essentially unaffected** (0/142 PETN, 1/143 RDX QCs would newly fail) — the backbone of the acceptance system doesn't depend on this ambiguity.
- The **0.2ng (sensitivity) QC level is heavily affected**, especially PETN (63.3% of currently-passing PETN 0.2ng QCs would newly fail).
- Real sample SNR tiers rarely flip, but **reported concentrations sitting below the ICH calibration-curve LOQ are far more common**: 24.6% of PETN and **50.5% of RDX** "Quantifiable" results already sit below it, independent of any threshold change.
- **Conclusion: tightening the operational SNR thresholds would not meaningfully address the real problem** (the below-LOQ rate), and risks a large, retroactive reclassification cascade concentrated in the 0.2ng/NC detectability logic. **Decision: `snr_detect`/`snr_quant` left unchanged.**

## 5. Surface breakdown — is this the known ABS/solvent issue?

Breaking the below-LOQ rate down by surface (885 rows matched, FINEX regex + ASTRA RunID join):

| | ABS-Smooth | ABS-Textured | Steel | Glass |
|---|---|---|---|---|
| PETN below-LOQ rate | 53.5% | 41.7% | 17.4% | 0.0% |
| RDX below-LOQ rate | 93.7% | 84.6% | **51.3%** | 0.0% |

The already-documented RDX/ABS acetonitrile-solvent compatibility issue is real and substantial (ABS is ~1.8× worse than Steel for RDX), and explains most of PETN's below-LOQ problem (71.9% of flagged PETN rows are ABS). **But it does not fully explain RDX**: Steel alone still has 51.3% of its own RDX results below the ICH LOQ, and Steel is not affected by the ABS-specific mechanism. A second, surface-independent contributor exists.

## 6. The adopted fix: QC-replicate-based LOD/LOQ

The full-range (0.2–10ng) quadratic curve has **zero within-level replication at the Cal-standard level itself** (`Code/03_Quantification.R`'s `CalSets <- split(CalData, rep(seq_len(nrow(CalData)/n_levels), each=n_levels))` — exactly 1 point per level per set). But every sequence also runs 5–6 **replicate 0.2ng QC injections** throughout, for routine QC — the same physical measurement as the 0.2ng Cal standard, never previously used to characterise low-end repeatability.

**Method** (ICH Q2(R2) §3.2.3's first approach, applied directly in concentration units): `LOD = 3.3×SD(replicate concentrations)`, `LOQ = 10×SD(replicate concentrations)` — no slope term needed, since the QC is at a known fixed concentration.

**Result**: median LOQ tightens from 0.844→0.223 ng (RDX, 2.0×) and 0.953→0.566 ng (PETN, 1.7×). Re-evaluating the same real-sample rows previously flagged below the global-curve LOQ: **43.1% of PETN's and 35.0% of RDX's flagged rows are rescued** (now above the tighter, better-justified LOQ) with zero new data or reanalysis.

**Why this is legitimate, not circular**: the back-calculated concentration for each replicate still passes through the same quadratic model, so any *systematic* bias in the curve shape affects all replicates equally. The SD across replicates isolates the *random* scatter at the concentration that actually matters for LOD/LOQ — a more direct measurement than inferring low-end noise from a curve dominated by residuals from much higher concentrations.

## 7. Could a different calibration fit help instead?

Tested 5 candidate models (quadratic/linear × three weighting schemes), judged by held-out prediction of the 0.2ng QC replicates (not R² on the same points the model was fit on):

| Analyte | Best model | Signed bias | Current production model |
|---|---|---|---|
| RDX | **Current (quadratic, 1/x weights)** | +4.7% | Already the best of 5 |
| PETN | Linear, 1/x weights | −44.7% | −64.7% (worst of 5) |

**RDX's current model is already optimal** — no fitting change helps. **PETN's bias could be reduced by ~a third by dropping the quadratic term** (−65%→−45%) — but **this change was deliberately not made**: PETN's quadratic shape is confirmed correct by the user and left as-is. Even the best PETN alternative leaves a large (−44.7%) systematic bias, consistent with PETN's already-documented lack of internal-standard correction being a deeper limitation than curve-fitting choice.

## 8. Exhaustive check — nothing further helps RDX

Two more avenues tested and rejected:
- **Augmenting the fit with the QC replicates directly** (not just using them as an independent SD estimate): no reliable improvement (median 1.13×, and the direction is inconsistent — 16/30 better, 14/30 worse). Confirms the global-curve-residual formula is structurally insensitive to extra low-end points.
- **Pooling calibration data across datasets**: not viable — RDX's own low-end curve shape (0.2ng/6ng response ratio) varies 33.4% CV across datasets, consistent with the separately-documented RDX calendar-drift finding. Pooling would blend genuinely different run-to-run behaviour into a misleading average.

**Conclusion**: the realistic, non-reanalysis option space for RDX is exhausted. The residual ~45% below-LOQ rate (even after the QC-replicate fix) should be documented as a genuine, intrinsic sensitivity/non-proportionality limitation of RDX quantification at trace levels, consistent with this project's own extensively-documented trace-level adsorption/signal-loss literature — not a fixable statistical or curve-fitting artefact.

## 9. Live pipeline integration

**Decision**: add the QC-replicate LOD/LOQ as an additional **reported** metric only — not a new acceptance/gating criterion. Operational SNR-based acceptance (which samples pass/fail) and the ICH-defensible validation LOD/LOQ (what to cite in the thesis) are kept deliberately separate.

**Implementation** (`Code/03_Quantification.R`, immediately before "Export final results," after both PETN drift correction and RDX concentration are fully finalised): computes `{Analyte}_LOD_QCReplicate_ng`/`{Analyte}_LOQ_QCReplicate_ng` per `CalibrationSet` from the already-computed `petn_concentration_dc`/`rdx_concentration` columns of the 0.2ng QC rows (≥4 usable replicates required, else `NA` — not fabricated), appends them to the already-built `cal_stats_wide` object, and re-writes `*_CalibrationStats.xlsx`. `FINEX/04_CollateStudyResults.R`'s `CalSummary` sheet aggregation updated to retain the new columns (previously would have been silently dropped by its hard-coded `select()`); ASTRA's `main_study_analysis.R`/`pilot_analysis.R` CalSummary-equivalents pick them up automatically (no hard select there).

**Verified**: tested against Lab9 (FINEX) and Analysis4 (ASTRA) — zero regression on any existing column (89/90 columns byte-identical before/after in both cases); new columns compute correctly (RDX LOD/LOQ for Lab9: 0.0470/0.1423 ng, matching the standalone diagnostic script exactly) and correctly return `NA` when fewer than 4 usable replicates exist (observed for Analysis4, which only has 3 QC replicates at 0.2ng).

**Not yet done**: a full reprocessing run across all 30 datasets to populate this column everywhere (currently populated only for datasets reprocessed after this change). Low risk, available as a follow-up whenever convenient — purely additive, same verification method as above.

---

## References

1. ICH Harmonised Guideline Q2(R2), *Validation of Analytical Procedures*, adopted 1 Nov 2023, §3.2.3.
2. United States Pharmacopeia, General Chapter ⟨621⟩ *Chromatography* (S/N = 2H/h definition).
3. European Pharmacopoeia 2.2.46, *Chromatographic Separation Techniques*.
4. Dolan JW. "Chromatographic Measurements, Part 4/5: Determining LOD and LOQ..." *Separation Science* (sepscience.com/hplc-solutions-125, -126).
5. Dolan JW. "The Role of the Signal-to-Noise Ratio in Precision and Accuracy." *LCGC Europe*, 2006.
6. Cuadros-Rodríguez L. et al. "An IUPAC-based approach to estimate the detection limit in co-extraction-based optical sensors for anions with sigmoidal response calibration curves." *Anal. Bioanal. Chem.* 2011, 401(9), 2881–2889. DOI: 10.1007/s00216-011-5366-8.
7. JCGM 100:2008, *Evaluation of measurement data — Guide to the expression of uncertainty in measurement (GUM)*, BIPM.
8. Analytical Sciences Digital Library. "Calculating S/N." *Chemistry LibreTexts*, 2023.
9. LabVeda Analytical. "Signal-to-Noise Ratio for LOD: USP, Ph. Eur. & ICH." 2026.

## Scripts and outputs (all in `GCMSQuantitation/Diagnostics/Generic/`)

- `ICH_CalibrationCurve_LODLOQ.R` → `ICH_LODLOQ_Output/ICH_LODLOQ_PerCalibrationSet.csv`, `ICH_LODLOQ_Summary.csv`, `ICH_LODLOQ_Comparison.png`
- `SNR_Threshold_Impact_Simulation.R` → `SNRThreshold_Impact_PerDataset.csv`, `SNRThreshold_Impact_QC_PerDataset.csv`
- `RDX_PETN_BelowLOQ_BySurface.R` → `BelowLOQ_BySurface_Detail.csv`
- `ReplicateQC_LowLevel_LODLOQ.R` → `ReplicateQC_vs_QuadCurve_LODLOQ.csv`
- `BelowLOQ_Rescue_Comparison.R` → `BelowLOQ_Rescue_Comparison.csv`
- `CalibrationModel_Comparison.R` → `CalibrationModel_Comparison_Detail.csv`
- `RDX_AugmentedCalibration_Test.R` → `RDX_AugmentedCalibration_Detail.csv`
