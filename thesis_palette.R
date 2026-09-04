# ===============================================================================
# THESIS COLOUR PALETTE -- shared, single source of truth for every plot
# across the WHOLE thesis repo (ASTRA, GCMSQuantitation, UVSwabbing, Data
# Analysis, Extraction). Source this file by absolute path (matching this
# repo's existing cross-file sourcing convention -- see pilot_analysis.R's
# own source() of GCMSQuantitation/Code/InjectionAcceptance.R) at the top of
# any script that builds a plot, instead of hardcoding hex codes locally.
#
# LOCATION: this file lives at the repo ROOT (not inside ASTRA/ any more),
# since it now serves every subproject, not just ASTRA.
#
# Rationale (recorded here so it doesn't need re-explaining in every script):
#   - Base palette: Okabe & Ito (2008) / Wong (2011, Nature Methods) 8-colour
#     qualitative palette -- colourblind-safe for the two most common forms
#     of colour vision deficiency (protanopia/deuteranopia), and distinguishable
#     in greyscale printing/photocopying.
#   - Each recurring NOMINAL (unordered) factor gets its own fixed, dedicated
#     pair/set of colours from this palette. With only 8 hues and dozens of
#     distinct factors across the whole thesis, FULL thesis-wide uniqueness
#     is not achievable -- the policy adopted here (confirmed with the user)
#     is PER-CHAPTER/STUDY uniqueness: colours are never reused for a
#     different meaning within the same study's own closely-related figures
#     (the specific problem that motivated this file -- Surface_type and
#     Study previously both used the same blue/orange hex codes in different
#     ASTRA figures), but reuse ACROSS unrelated chapters/studies (e.g. an
#     ASTRA colour also appearing in a GCMSQuantitation diagnostic chart) is
#     accepted as unavoidable and not a real ambiguity risk in practice
#     (nobody compares an ASTRA swabbing-pressure figure directly against a
#     GC-MS ion-correlation diagnostic). Within GCMSQuantitation specifically,
#     which has an unusually large number of small, secondary diagnostic-only
#     comparison factors (various drift-correction "Method" labels, ion/IS
#     m/z candidates, etc.), the same relaxed standard is applied one level
#     down: colours are kept unique across the STUDY'S OWN HEADLINE figures
#     (Surface/SurfaceName, injection role, PETN vs IS, Significant), but
#     minor diagnostic-only classification factors may reuse hues from each
#     other where a good-faith fully-unique assignment isn't practical.
#   - QC/status colours (PASS/WARN/FAIL) are a deliberate, self-contained
#     exception: kept as an intuitive traffic-light convention (green/amber/
#     red family, still from the same Okabe-Ito set) rather than forced to be
#     unique from every other factor, since status is always shown captioned
#     (PASS/WARN/FAIL text) and never appears as a colour-comparison
#     side-by-side with Surface_type/Study/etc in the same figure.
#   - ORDINAL/continuous quantities (e.g. cycle number in a gradient, or a
#     genuinely ordered factor like "High/Normal/Low" pressure, "10%/25%/50%"
#     concentration, or a high-cardinality identifier like Lab/Dataset/Sample
#     with too many levels for a small qualitative set to stay distinguishable)
#     should use viridis (ggplot2's built-in scale_colour_viridis_c()/_d()),
#     NOT a slice of the qualitative palette above -- viridis is perceptually
#     uniform and designed for exactly this use case, and remains the best
#     available option even for unordered high-cardinality IDs, since no
#     8-colour (or even 20-colour) qualitative set stays visually distinct
#     at that count either. Using a qualitative palette for ordered/
#     high-cardinality data (or vice versa) is a recognised
#     data-visualisation anti-pattern.
# ===============================================================================

# --- Base Okabe-Ito 8-colour qualitative palette ---
okabe_ito <- c(
  black          = "#000000",
  orange         = "#E69F00",
  sky_blue       = "#56B4E9",
  bluish_green   = "#009E73",
  yellow         = "#F0E442",
  blue           = "#0072B2",
  vermillion     = "#D55E00",
  reddish_purple = "#CC79A7"
)

# Neutral grey -- NOT part of the Okabe-Ito 8, used only for genuine
# "blank"/"other"/"catch-all"/baseline-absence categories that deliberately
# want to read as de-emphasised rather than as one of the 8 vivid hues
# (e.g. a diagnostic classification's "Other" bucket, or a "Blank" control).
neutral_grey <- "#999999"

# ===============================================================================
# SECTION: ASTRA (doe/ -- swabbing pressure/recovery study)
# ===============================================================================

# Surface_type: steel vs abs
pal_surface_type <- c(steel = okabe_ito[["blue"]], abs = okabe_ito[["orange"]])

# Study: Pilot vs Main
pal_study <- c(Pilot = okabe_ito[["reddish_purple"]], Main = okabe_ito[["bluish_green"]])

# Analyte: PETN vs RDX
pal_analyte <- c(PETN = okabe_ito[["sky_blue"]], RDX = okabe_ito[["vermillion"]])

# Solvent condition -- both naming conventions used across scripts
# (pilot_analysis.R's Solvent_level: "present"/"absent"; main_study_batch_process.R's
# wet_dry: "wet"/"dry") map onto the SAME two colours, so either script can
# key into this one vector directly with its own actual level strings.
pal_solvent <- c(
  present = okabe_ito[["blue"]],         wet = okabe_ito[["blue"]],
  absent  = okabe_ito[["bluish_green"]], dry = okabe_ito[["bluish_green"]]
)

# Applied-mass level (pilot's original 2-level low/high design factor,
# Pressure_level -- distinct from the main study's 5-level Pressure_f/
# Pressure_g, which isn't shown as a discrete colour anywhere)
pal_mass_level <- c(low = okabe_ito[["yellow"]], high = okabe_ito[["black"]])

# QC/status traffic light (deliberate exception -- see header comment)
pal_qc_status <- c(
  PASS = okabe_ito[["bluish_green"]],
  WARN = okabe_ito[["orange"]],
  FAIL = okabe_ito[["vermillion"]]
)

# ===============================================================================
# Helper: build a QC-style 2-colour (pass/fail only, no warn) named vector for
# a pair of DYNAMIC sprintf()-built labels (e.g. main_study_batch_process.R's
# plot_batch_qc_overview(), whose "Within X% of Target"/"Outside X% of Target"
# legend labels embed a runtime outlier_pct value, so can't be hardcoded here).
# ===============================================================================
qc_pass_fail_labels <- function(pass_label, fail_label) {
  setNames(c(pal_qc_status[["PASS"]], pal_qc_status[["FAIL"]]), c(pass_label, fail_label))
}

# ===============================================================================
# SECTION: GCMSQuantitation (FINEX multi-lab study + method-development
# diagnostics)
# ===============================================================================

# Surface/SurfaceName (FINEX 4-level swabbed surface material -- Glass, Steel,
# ABS-Smooth, ABS-Textured. Distinct from ASTRA's own 2-level Surface_type --
# different study, no naming collision since ASTRA never has "Glass" or the
# Smooth/Textured ABS split). Previously inconsistent: hardcoded near-Dark2
# hex in 04_CollateStudyResults.R, ggplot defaults in SequenceDiagnostics.R/
# 05_StatisticalAnalysis.R -- now the same 4 colours everywhere.
pal_surface_gcms <- c(
  Steel          = okabe_ito[["blue"]],
  Glass          = okabe_ito[["bluish_green"]],
  "ABS-Smooth"   = okabe_ito[["orange"]],
  "ABS-Textured" = okabe_ito[["vermillion"]]
)

# Injection role -- consolidates TWO previously-separate factors that turned
# out to describe the same underlying concept under different label text:
# Code/03_Quantification.R's "Group" (Standard/QC/Sample) and
# FINEX/04_CollateStudyResults.R's "Type" (Cal/QC/Sample/Blank). "Standard"
# and "Cal" both mean "calibration standard injection" -- aliased to the same
# colour here.
pal_injection_role <- c(
  Standard = okabe_ito[["blue"]],  Cal    = okabe_ito[["blue"]],
  QC       = okabe_ito[["orange"]],
  Sample   = okabe_ito[["bluish_green"]],
  Blank    = okabe_ito[["reddish_purple"]]
)

# Drift-correction "Method" comparison labels -- consolidates every distinct
# label string found across AllMethods_Comprehensive_Comparison.R,
# Dual_PA_Power_Validation.R, PowerCurve_Bias_Comparison.R,
# PowerCurve_Ratio_Validation.R, PowerCurve_vs_Polynomial_Analysis.R and
# Partial_Drift_Correction.R into ONE vector, so whichever subset of methods
# any individual chart compares, the same method always gets the same
# colour. Same-concept labels under different chart-specific short names are
# aliased to the same colour (documented per-line below).
pal_method <- c(
  # baseline/current method -- exact label text varies across scripts,
  # including a literal embedded newline variant used for compact plot labels
  "Current(Ratio Poly)"    = okabe_ito[["reddish_purple"]],
  "Current\n(Ratio Poly)"  = okabe_ito[["reddish_purple"]],
  "Current (Ratio Poly)"   = okabe_ito[["reddish_purple"]],
  "Current Polynomial"     = okabe_ito[["reddish_purple"]],
  "Current"                = okabe_ito[["reddish_purple"]],
  "Before Correction"      = okabe_ito[["reddish_purple"]],
  # no-internal-standard power-law family
  "PA Power(No IS)"        = okabe_ito[["blue"]],
  "PA Power\n(No IS)"      = okabe_ito[["blue"]],
  "PA Power (No IS)"       = okabe_ito[["blue"]],
  "Power(Row)"             = okabe_ito[["blue"]],
  "Power (Row)"            = okabe_ito[["blue"]],
  # position-based power-law variant
  "Power(Position)"        = okabe_ito[["sky_blue"]],
  "Power (Position)"       = okabe_ito[["sky_blue"]],
  "Power (Pos)"            = okabe_ito[["sky_blue"]],
  # dual-PA (uses both PETN + IS response) power-law family
  "Dual Power(PETN+IS)"    = okabe_ito[["bluish_green"]],
  "Dual Power\n(PETN+IS)"  = okabe_ito[["bluish_green"]],
  "Dual PA Power"          = okabe_ito[["bluish_green"]],
  # full raw-IS power-law variant / the newly-applied correction
  "PETN Power+Raw IS"      = okabe_ito[["orange"]],
  "PETN Power +\nRaw IS"   = okabe_ito[["orange"]],
  "After Correction"       = okabe_ito[["orange"]]
)

# PETN analyte vs its internal standard (IS) signal -- Dual_PA_Power_Validation.R.
# NOT the same concept as pal_analyte (PETN vs RDX) -- this compares an
# analyte's own signal to ITS internal standard's signal, so needs its own pair.
pal_analyte_vs_is <- c(
  PETN = okabe_ito[["sky_blue"]],    PETN_PA = okabe_ito[["sky_blue"]],
  IS   = okabe_ito[["vermillion"]],  IS_PA   = okabe_ito[["vermillion"]]
)

# Statistical significance flag (FINEX per-lab forest plot)
pal_significant <- c(No = okabe_ito[["black"]], Yes = okabe_ito[["vermillion"]])

# Sample/injection role (Calibrant/Blank/QC Sample) -- TIC_AllIons_Scan.R,
# mz52_Diagnostic.R. Was already consistent across both files (blue/gray50/
# orange via named colours) -- tightened to the exact Okabe-Ito hex here.
# "Blank" kept neutral grey (not a vivid hue) -- consistent with the
# "genuine absence/baseline control" convention used elsewhere in this file.
pal_sample_role <- c(
  Calibrant  = okabe_ito[["blue"]],
  Blank      = neutral_grey,
  "QC Sample" = okabe_ito[["orange"]]
)

# Ion correlation "interpretation" category (TIC_IonCorrelation_Diagnostic.R,
# 5-level). "Weak correlation" kept neutral grey (deliberately de-emphasised
# -- the whole point of this category is "not much going on here").
pal_ion_interpretation <- c(
  "RDX fragment/correlated" = okabe_ito[["vermillion"]],
  "IS fragment/correlated"  = okabe_ito[["blue"]],
  "Independent candidate"   = okabe_ito[["bluish_green"]],
  "Weak correlation"        = neutral_grey,
  "Moderate correlation"    = okabe_ito[["orange"]]
)

# Alternative-IS ion candidate (m/z 52 / m/z 122 / m/z 120 / m/z 53) --
# consolidates mz52_Analysis.R, mz52_Diagnostic.R, mz52_Quantification_Test.R,
# mz53_Quantification_Test.R's "ion"/"IS" factors (several different label
# text variants for the same underlying m/z values) into one vector.
pal_ion_candidate <- c(
  "m/z 52"    = okabe_ito[["bluish_green"]], "mz52"     = okabe_ito[["bluish_green"]], "m/z 52 IS"  = okabe_ito[["bluish_green"]],
  "m/z 122"   = okabe_ito[["vermillion"]],   "mz122"    = okabe_ito[["vermillion"]],   "m/z 122 IS" = okabe_ito[["vermillion"]],
  "mz120"     = okabe_ito[["black"]],
  "m/z 53 IS" = okabe_ito[["blue"]]
)

# GC method variant (DualMethodProcess.R -- V8 vs V4). NOTE: this specific
# script currently has an unrelated pre-existing bug (source()s a path that
# no longer exists, GCMSQuantitation/v2-NG/Code/ModPeaks.R -- the code moved
# to Archive/v2-NG_Deprecated/) and will not run until that's fixed
# separately; colour assignment recorded here regardless, for whenever it is.
pal_method_group <- c(V8 = okabe_ito[["blue"]], V4 = okabe_ito[["orange"]])

# Boolean sequence-boundary flag (Code/03_Quantification.R's IsFirstOfSequence,
# 4 identical occurrences -- "Within sequence" vs "First of new sequence").
# Matches pal_significant's black/vermillion No/Yes convention: ordinary case
# unmarked, notable/flagged case in vermillion.
pal_sequence_flag <- c("FALSE" = okabe_ito[["black"]], "TRUE" = okabe_ito[["vermillion"]])

# Fixed 2-line legend in Code/02_PeakDetection.R's TIC/baseline overlay plot
# (previously ggplot default hue).
pal_trace_baseline <- c("GC trace" = okabe_ito[["black"]], "baseline" = okabe_ito[["vermillion"]])

# TIC_AllIons_Scan.R's ion "classification" (7-level, near the 8-colour cap).
# "Other" kept neutral grey as a deliberately de-emphasised catch-all bucket
# (matches the existing script's own use of gray50/gray70 for this purpose).
pal_ion_classification <- c(
  "Excellent IS candidate"      = okabe_ito[["bluish_green"]],
  "Good IS candidate"           = okabe_ito[["sky_blue"]],
  "RDX-dependent (problem)"     = okabe_ito[["vermillion"]],
  "Ion enhancement effect"      = okabe_ito[["orange"]],
  "Matrix effect warning"       = okabe_ito[["reddish_purple"]],
  "Background only"             = okabe_ito[["black"]],
  "Other"                       = neutral_grey
)

# ===============================================================================
# SECTION: UVSwabbing
# ===============================================================================

# Image/curve stage -- consolidates THREE previously-inconsistent factors
# describing the same Blank -> Before -> After swabbing sequence, which had
# DIFFERENT colours in different files (Diagnostic_ThresholdPlots.R used
# blue/green/red; UV_Recovery_Reprocessing.R used blue/red/green3 -- Before
# and After were literally swapped between the two). Also covers
# 06-RecoveryAnalysis.R's "BlankArea"/"BeforeArea"/"AfterArea" column-name
# variant of the same three stages.
pal_image_stage <- c(
  Blank = okabe_ito[["blue"]],          BlankArea  = okabe_ito[["blue"]],
  Before = okabe_ito[["orange"]],       BeforeArea = okabe_ito[["orange"]],
  After = okabe_ito[["bluish_green"]],  AfterArea  = okabe_ito[["bluish_green"]]
)

# Swabbing pattern (06-RecoveryAnalysis.R, 07-PatternComparison.R -- already
# consistent between these two files, previously hardcoded Brewer-Set1-
# equivalent hex; tightened to Okabe-Ito hex here). Deliberately disjoint
# from pal_image_stage's blue/orange/bluish-green so the two headline
# UVSwabbing comparisons (stage vs pattern) stay visually distinct from
# each other.
pal_pattern <- c(
  "50g_BackandForth" = okabe_ito[["sky_blue"]],
  "50g_Snake"         = okabe_ito[["vermillion"]],
  "50g_Ratchet"        = okabe_ito[["reddish_purple"]]
)

# Calibration experiment condition (UV_Calibration_Analysis.R, 5-level:
# Blank/Before/1_swipe/2_swipes/3_swipes). Own independent narrative
# (calibration curve, not the recovery/pattern figures above) -- minor,
# good-faith-distinct assignment; some hue reuse from pal_pattern is
# accepted here (see file header policy note).
pal_condition <- c(
  Blank      = neutral_grey,
  Before     = okabe_ito[["reddish_purple"]],
  "1_swipe"  = okabe_ito[["sky_blue"]],
  "2_swipes" = okabe_ito[["bluish_green"]],
  "3_swipes" = okabe_ito[["vermillion"]]
)

# Surface identity A/B/C (UV_Calibration_Analysis.R Plot 3) -- distinct
# concept from pal_surface_type (ASTRA)/pal_surface_gcms (GCMS FINEX
# materials); this is just a 3-way replicate/surface-sample label.
pal_surface_uv <- c(A = okabe_ito[["blue"]], B = okabe_ito[["orange"]], C = okabe_ito[["bluish_green"]])

# Recovery-calculation method comparison (UV_Recovery_Reprocessing.R, 5-level)
pal_method_uv <- c(
  SingleThreshold  = okabe_ito[["blue"]],
  CrossingShift    = okabe_ito[["orange"]],
  AUC              = okabe_ito[["bluish_green"]],
  OptimalK         = okabe_ito[["vermillion"]],
  BlankNormalised  = okabe_ito[["reddish_purple"]]
)

# Surface x replicate identity (04-BlankImageAnalysis.R, 05-BeforeImageAnalysis.R,
# "source" column, 6-level: Surface{1,2,3}_Rep{1,2}). Paired by "family" --
# each Surface gets its own hue, Rep1/Rep2 of the same Surface share a
# related pair from that hue's neighbourhood in the Okabe-Ito wheel.
pal_source_surface_rep <- c(
  Surface1_Rep1 = okabe_ito[["blue"]],         Surface1_Rep2 = okabe_ito[["sky_blue"]],
  Surface2_Rep1 = okabe_ito[["orange"]],       Surface2_Rep2 = okabe_ito[["vermillion"]],
  Surface3_Rep1 = okabe_ito[["bluish_green"]], Surface3_Rep2 = okabe_ito[["yellow"]]
)

# ===============================================================================
# SECTION: Data Analysis (Chapter 3 / ASTRA device-design figures)
# ===============================================================================

# Original vs Modified swab mount (OrigModSwabMountPressure.R). Previously
# RColorBrewer Set1 -- switched to Okabe-Ito for consistency with the rest
# of the thesis.
pal_mount_type <- c(Original = okabe_ito[["blue"]], Modified = okabe_ito[["orange"]])

# Swab mount angle (SwabMountAngles.R, 3-level: 45 deg/90 deg/70 deg).
# Previously RColorBrewer Set1.
pal_swab_mount_angle <- c(
  "45\u00B0" = okabe_ito[["blue"]],
  "90\u00B0" = okabe_ito[["orange"]],
  "70\u00B0" = okabe_ito[["bluish_green"]]
)

# ===============================================================================
# SECTION: Extraction
# ===============================================================================

# Test standard condition (InitialExtractionTest.R, 3-level). The 3rd level
# ("100% recovery, no swab") has no wet/dry equivalent, so this is its OWN
# palette rather than reusing/extending pal_solvent -- but the wet/dry pair
# WITHIN it intentionally matches pal_solvent's existing blue/bluish-green
# convention, since it's the same physical wet-vs-dry-swab concept as ASTRA.
pal_test_std <- c(
  "100% recovery\n(no swab)" = okabe_ito[["reddish_purple"]],
  "Spiked swab\n(wet)"       = okabe_ito[["blue"]],
  "Spiked swab\n(dry)"       = okabe_ito[["bluish_green"]]
)

# Swab replicate identity (PlotExtractionRecovery.R, 3-level: Swab 1/2/3)
pal_swab <- c(
  "Swab 1" = okabe_ito[["blue"]],
  "Swab 2" = okabe_ito[["orange"]],
  "Swab 3" = okabe_ito[["bluish_green"]]
)

# NOTE: PlotExtractionRecovery.R's "Extraction" factor (Ext 1-5, a
# SEQUENTIAL/ordinal extraction-step index, not a nominal category) and
# GCMSQuantitation's "Lab"/"Dataset" and UVSwabbing's "Sample" (17+ level
# IDs) and "Pressure"/"Concentration" (genuinely ordered: High/Normal/Low,
# 50%/25%/10%) are all handled with scale_colour_viridis_d() directly in
# their own scripts instead of a pal_* vector here -- see this file's own
# header policy note on ordinal/high-cardinality data.
