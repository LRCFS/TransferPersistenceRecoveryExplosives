# TransferPersistenceRecoveryExplosives

PhD research repository: *Evaluation of Effectiveness of Recovery of Explosive Traces from Surfaces*. Covers surface-swabbing recovery studies (manual and automated/pressure-controlled), GC-MS method development and quantification, UV-powder surrogate validation, measurement-uncertainty calculations, and supporting scientometric literature reviews.

This is a collection of largely independent R projects (each with its own `.Rproj`), not a single integrated pipeline. There is no top-level build/run command — see each sub-project's own documentation below.

## Sub-projects

| Folder | Purpose | Docs |
|---|---|---|
| `ASTRA/` | Pressure-controlled automated swabbing device: pressure-trace analysis and the Design of Experiments (pilot + main study) for explosive-trace recovery vs. swab pressure | `ASTRA/README.md` (consolidated reference + changelog); `ASTRA/doe/CONTEXT.md` (DOE session log + design doc) |
| `GCMSQuantitation/` | Core GC-MS data-processing pipeline: peak detection, quantification, QC acceptance, and collation across the FINEX swabbing study and ASTRA pilot/main studies | `GCMSQuantitation/CONTEXT.md` (session log + design doc); `PhD_Work_Summary.md`; `Future_PhD_Research_Directions.md` |
| `UVSwabbing/` | Validation of UV fluorescent powder as a surrogate/training aid for explosive-trace swabbing recovery | `UVSwabbing/CONTEXT.md` |
| `SartoriusBalance/` | Serial-logging script for the balance used to record swab contact pressure on the ASTRA device | `SartoriusBalance/CONTEXT.md` |
| `DataAnalysis/` | Thesis Chapter 3 methodology analysis (manual swab mount angle/pressure) | — (see `GCMSQuantitation/CONTEXT.md` / `ASTRA/doe/CONTEXT.md` session entries that reference it) |
| `Extraction/` | Extraction and filtration efficiency measurement for PETN/RDX | — (see `GCMSQuantitation/CONTEXT.md` / `ASTRA/doe/CONTEXT.md` session entries that reference it) |
| `SolutionPreparationMU/` | Measurement-uncertainty propagation for calibration-standard solution preparation | — |
| `ExplosivesInterpolScopusSearch/` | Scientometric literature review: explosives-as-evidence trends (Scopus + INTERPOL IFSMS reports) | `ExplosivesInterpolScopusSearch/README.md` |
| `SamplingInterpolScopusSearch/` | Sibling scientometric review, same methodology, for drugs-as-evidence/sampling literature | `SamplingInterpolScopusSearch/README.md` |
| `ErrorCalc/` | Standalone error-calculation spreadsheet | — |
| `Reports/` | Thesis chapter drafts (extracted from Word), bibliography (`.ris`), and the text-extraction tooling used to read them | — |

## Documentation conventions

Most sub-projects keep a `CONTEXT.md` acting as both a running session log (dated entries, newest-first) and living reference documentation (design rationale, current pipeline behaviour). The two largest — `GCMSQuantitation/CONTEXT.md` and `ASTRA/doe/CONTEXT.md` — each start with a Table of Contents to help navigate between the session log and the static reference sections. `ASTRA/README.md` is the exception: a fully consolidated reference manual + changelog rather than a session log.

[`OPEN_ITEMS.md`](OPEN_ITEMS.md) (repo root) indexes every open question, pending decision, or blocked task currently scattered across the various `CONTEXT.md` files, grouped by project, with a pointer back to the full detail in each source file.

## Shared code

- `thesis_palette.R` (repo root) — shared colour-palette definitions, `source()`d by scripts across `GCMSQuantitation`, `UVSwabbing`, `ASTRA`, `DataAnalysis`, and `Extraction` for consistent thesis figure styling.

## Licence

Crown Copyright (2025), Dstl. Licensed under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/). See `LICENSE`.
