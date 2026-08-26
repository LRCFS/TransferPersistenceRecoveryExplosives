@echo off
REM ============================================================================
REM ASTRA Pressure Trace Analysis - Batch Processing Launcher
REM ============================================================================
REM
REM This script runs the current adaptive time+distance stable contact
REM detection pipeline on all pilot study samples.
REM
REM Author: ASTRA Analysis System
REM Date: 2026-07-28
REM Script version: v1.0-DISTANCE (single consolidated script)
REM ============================================================================

echo ============================================================================
echo ASTRA Pressure Trace Analysis - Batch Processing
echo ============================================================================
echo.
echo This will process all pilot study samples and generate:
echo   - Time-based and distance-based full trace plots per sample
echo   - Zoomed distance plots (passes 1-5) per sample
echo   - 25s window plots per sample
echo   - Detected cycles CSV and summary statistics per sample
echo   - Batch summary (CSV + Excel) and comparison plots
echo.
echo Processing time: ~5-10 minutes for full pilot study set
echo.
pause

echo.
echo Starting analysis...
echo.

"C:\Program Files\R\R-4.4.2\bin\Rscript.exe" "C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\ASTRA\time_based_batch_process_adaptive_DISTANCE.R"

echo.
echo ============================================================================
echo ANALYSIS COMPLETE!
echo ============================================================================
echo.
echo Results saved to:
echo   ...\Pilot Study\Pressure Traces\ProcessedData\
echo.
echo Check the following files:
echo   - batch_summary.csv / batch_summary.xlsx (main results)
echo   - PILOT_*\PILOT_*_full_trace_time.png (per-sample plots)
echo   - PILOT_*\PILOT_*_summary_statistics.txt (per-sample details)
echo.
pause
