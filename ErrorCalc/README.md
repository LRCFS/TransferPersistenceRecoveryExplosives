# Error Calc

Standalone spreadsheet (`Error Calc.xlsx`) that manually computes measurement-uncertainty propagation for two solution-preparation series: **"Cal Stocks"** and **"10ng Stock"**.

For each solution, every contributing equipment item (pipette, volumetric flask) is listed with its used volume and certified tolerance. These are converted to a standard uncertainty `u(Equipment)` (tolerance / coverage factor) and relative uncertainty `ur(Equipment)` (u / volume used), then combined (root-sum-square) up through the solution hierarchy (e.g. `Stock Solution A` → `Cal Solution 1-4` / `Working Solution 1-3`) to give each root stock's overall `u(solution)` / `ur(solution)`, calculated separately for the PETN and RDX standards.

This is a manual precursor to the same calculation later automated in [`../SolutionPreparationMU/`](../SolutionPreparationMU/Global%20Code.R) (same formulas: `u = tolerance/coverage`, `ur = u/volume`, combined by root-sum-square up a solution dependency tree) — kept here as the original worked example / cross-check rather than merged into that project.
