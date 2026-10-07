# Reports

Thesis chapter drafts (extracted from Word documents), bibliography exports, and a small property-reference table — plus the extraction tooling used to produce plain-text versions of the drafts for review.

## Thesis drafts

- `Chapter1Draft.txt` — Chapter 1 (Introduction: explosive properties, traces on surfaces, recovery, PhD aim), extracted from the source `.docx`.
- `TMC4.txt` — Chapter 2 (International Swabbing Study: experimental design, methodology, method development), extracted from the source `.docx`. Its review status and specific flagged issues (figure numbering, a couple of data-consistency points) are tracked in `GCMSQuantitation/CONTEXT.md`'s "Report Progress (TMC4.txt)" section rather than in this folder.
- `Text Extraction Tools/PhD_Summary.txt` — a mechanical concatenation of four source documents (PhD work summary, TMC4/Chapter 2, "TMC3", and Chapter 1), produced by the scripts below. **Note**: this file currently holds the only copy of the "TMC3" and "Chapter 2" sections anywhere in this repo — `Chapter1Draft.txt`/`TMC4.txt` above are separate, standalone exports of different chapters, not duplicates of the content inside this file.

## Reference data

- `table.csv` — Chapter 1's explosive-properties reference table (TNT/PETN/RDX/R-Salt/TATP: structure, formula, melting/decomposition temperature, vapour pressure, etc.), each cell annotated with a reference number.
- `*.ris` (`PETN`, `RDX`, `TNT`, `TATP`, `RSalt`, plus `_ICSC` variants) — EndNote/reference-manager citation exports backing the table above.

## Text Extraction Tools

Small utility scripts used to pull plain text out of the `.docx` thesis chapters (and concatenate them into `PhD_Summary.txt` above) so they can be read without opening Word: `extract_docx.ps1`, `create_summary.py`, `read_extracted.py`, `read_tmc4.py`.
