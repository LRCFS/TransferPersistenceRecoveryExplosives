# =========================================================
# Test: PowerLaw vs Linear fit quality for ASTRA 6ng PETN QCs,
# excluding Analysis2 (terminal collapse, not drift) and the
# last QC of Analysis1 (row 59, the non-monotonic uptick -- a
# pattern outlier relative to the other 3 Analysis1 QCs).
# =========================================================
# Read-only analysis -- prints results, does not modify any pipeline files.

data <- list(
  Analysis1_full    = data.frame(Row = c(13, 27, 41, 59),
                                  PA  = c(104761.219984451, 64651.9581215719, 56215.1827558455, 66676.5953565429)),
  Analysis1_trimmed = data.frame(Row = c(13, 27, 41),
                                  PA  = c(104761.219984451, 64651.9581215719, 56215.1827558455)),
  Analysis3 = data.frame(Row = c(15, 27, 41),
                          PA  = c(104163.887107497, 64969.2676868348, 47500.8799570005)),
  Analysis4 = data.frame(Row = c(15, 31, 47, 57, 67),
                          PA  = c(88539.1121616264, 50066.1819600554, 43239.0401006875, 40002.3037771265, 51129.1264625948)),
  Analysis5 = data.frame(Row = c(17, 29, 41),
                          PA  = c(92733.3813310332, 63585.4960797321, 38059.1354206726))
)

fit_and_report <- function(name, df) {
  cat("\n=== ", name, " (n=", nrow(df), ") ===\n", sep = "")
  cat("Row:", df$Row, "\n")
  cat("PA :", round(df$PA, 0), "\n")

  # Linear fit
  lin <- lm(PA ~ Row, data = df)
  r2_lin <- summary(lin)$r.squared

  # Power law fit (same functional form used in the pipeline)
  pow <- tryCatch(
    nls(PA ~ a * Row^b, data = df,
        start = list(a = max(df$PA), b = -1.0),
        control = nls.control(maxiter = 200, warnOnly = TRUE)),
    error = function(e) NULL
  )

  if (!is.null(pow)) {
    resid_p <- residuals(pow)
    ss_res <- sum(resid_p^2)
    ss_tot <- sum((df$PA - mean(df$PA))^2)
    r2_pow <- 1 - ss_res / ss_tot
    coefs <- coef(pow)
    cat(sprintf("PowerLaw: a=%.1f, b=%.4f, R2=%.4f\n", coefs["a"], coefs["b"], r2_pow))
    cat("  Residuals (PowerLaw):", round(resid_p, 0), "\n")
  } else {
    r2_pow <- NA
    cat("PowerLaw: fit failed to converge\n")
  }

  cat(sprintf("Linear:   R2=%.4f\n", r2_lin))
  cat("  Residuals (Linear):  ", round(residuals(lin), 0), "\n")

  # Monotonicity check: does PA strictly decrease? (Power law fundamentally
  # cannot capture a late-sequence increase)
  diffs <- diff(df$PA)
  monotonic_decreasing <- all(diffs <= 0)
  cat("Strictly monotonic decreasing:", monotonic_decreasing,
      "| diffs:", round(diffs, 0), "\n")

  invisible(list(r2_pow = r2_pow, r2_lin = r2_lin, monotonic = monotonic_decreasing))
}

results <- list()
for (nm in names(data)) {
  results[[nm]] <- fit_and_report(nm, data[[nm]])
}

cat("\n\n=== SUMMARY ===\n")
summary_df <- data.frame(
  Run = names(results),
  R2_PowerLaw = sapply(results, function(x) round(x$r2_pow, 4)),
  R2_Linear   = sapply(results, function(x) round(x$r2_lin, 4)),
  Monotonic_Decreasing = sapply(results, function(x) x$monotonic)
)
print(summary_df, row.names = FALSE)
