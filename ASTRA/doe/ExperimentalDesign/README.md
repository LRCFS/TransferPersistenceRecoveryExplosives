# Experimental Design — Methods-Section Record

This folder contains the scripts and generated artefacts used to **create** the
Pilot and Main Study experimental designs (randomisation, batch assignment,
power analysis) before any data collection began. Moved here (02/09/2026) from
`ASTRA/doe/` as a simplification pass — these scripts have already done their
job and are not part of the live analysis pipeline; kept as a permanent record
for the thesis methods section, not because anything still runs them.

## Scripts (in chronological order of use)

1. **`doe_explosives_recovery.R`** — the original 2^3 factorial design +
   power analysis for the Pilot Study (Surface x Pressure x Solvent).
   Outputs: `design_matrix.csv`, `design_matrix_blocked.csv`,
   `power_analysis.png`, `power_sensitivity_summary.csv`.
2. **`pilot_study_design.R`** — the actual Pilot Study nested design
   generator (produces the live `pilot_design_nested.csv` — see note below).
3. **`pilot_design_batch_reorganizer.R`** — reorganises the pilot design into
   a lab-execution-friendly batch CSV. Outputs (this folder):
   `batch_protocol.md`, `batch_timing_checklist.csv`.
4. **`doe_nested_design.R`** — the Main Study's multi-level-pressure (5-level)
   nested design generator, extending the Pilot's 2-level design. Outputs
   (this folder): `main_study_protocol.txt`,
   plus the live `main_study_design_nested.csv`/`main_study_surface_use_tally.csv`
   (see note below).

## Important: some design outputs deliberately stayed in `ASTRA/doe/`, not here

Four design CSVs are **still read directly** by the live analysis scripts
(`pilot_analysis.R`, `main_study_analysis.R`, and its companions) every time
they run, so they were left in `ASTRA/doe/` rather than moved here — moving
them would have meant updating hardcoded paths in the active pipeline for no
real benefit:

- `pilot_design_nested.csv`, `pilot_design_batch_organized.csv` (read by `pilot_analysis.R`)
- `main_study_design_nested.csv`, `main_study_surface_use_tally.csv` (read by `main_study_analysis.R`)

If you want a single, fully self-contained methods-section record, these 4
files are the ones to copy alongside this folder's contents — just don't
*move* them out of `ASTRA/doe/`, since the live scripts would break.

## Other files moved here (design-generation outputs, not read by anything live)

`design_matrix.csv`, `design_matrix_blocked.csv`, `main_study_negative_controls.csv`,
`main_study_batch_checklist.csv`, `main_study_protocol.txt`, `batch_protocol.md`,
`batch_timing_checklist.csv`, `power_sensitivity_summary.csv`, `power_analysis.png`,
`pilot_design_batch_organized_original.csv`.
