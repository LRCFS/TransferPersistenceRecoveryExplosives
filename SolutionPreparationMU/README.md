# Solution Preparation MU

Calculates combined relative standard uncertainty (`ur`) for solutions prepared by serial dilution, with per-analyte tracking (PETN, RDX). Automates the same calculation worked by hand in [`../ErrorCalc/Error Calc.xlsx`](../ErrorCalc/README.md).

## How it works

`Global Code.R` is the single script. Configure the three variables at the top of the file, then run it:

```r
input_file     <- "CalStocksNoIS.csv"     # which solution-hierarchy CSV to process
output_prefix  <- "CalStocksNoIS"         # outputs: <prefix>_report.txt / _results.csv / _hierarchy.png
coverage_final <- 2                       # coverage factor (k) for the final expanded uncertainty
equipment_file <- "EquipmentUncertainties.csv"
```

**Input CSV** (`input_file`): one row per equipment item or standard used in preparing each solution, with columns `solution`, `parent` (blank for root/stock solutions), `type` (`"equipment"` or `"standard"`), `name`, `label`, `volume_or_concentration`, plus `tolerance_or_error`/`coverage` for standard rows (equipment uncertainties are looked up automatically from `equipment_file` by matching `name`/`volume_or_concentration` against `equipmentType`/`volumeUsed`).

**Calculation**: for each equipment/standard row, `u = tolerance_or_error / coverage` and `ur = u / volume_or_concentration`. For each solution, these are combined by root-sum-square, propagating from parent solutions down through the dependency tree built from the `parent` column (the script detects circular dependencies and topologically sorts solutions before propagating). The final expanded relative uncertainty is `Ur = coverage_final * ur_combined`.

**Outputs** (named `<output_prefix>_*`): a console summary, a `.png` flowchart of the solution hierarchy (via `DiagrammeR`/Graphviz), a `.txt` audit report showing every uncertainty formula substituted with real numbers, and a `.csv` of the final per-solution, per-analyte results.

## Files in this folder

| File(s) | Purpose |
|---|---|
| `Global Code.R` | The calculator script (see above) |
| `EquipmentUncertainties.csv` / `.xlsx` | Lookup table of certified tolerances per equipment type + volume used |
| `CalStocksNoIS.csv`, `CalSingleStockIS.csv`, `CalMultiStockIS.csv` | Input solution-hierarchy definitions for three different calibration-stock preparation schemes |
| `example_cal_stocks.csv`, `example_10ng_stock.csv` | Worked example inputs |
| `*_report.txt`, `*_results.csv`, `*_hierarchy.png` | Generated outputs for each of the inputs above (already run; re-run `Global Code.R` with the matching `input_file`/`output_prefix` to regenerate) |
