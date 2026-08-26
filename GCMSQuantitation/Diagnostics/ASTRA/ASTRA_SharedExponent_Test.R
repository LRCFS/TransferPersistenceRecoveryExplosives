# =========================================================
# Test: is a single shared PowerLaw exponent (b) consistent across
# Analysis1 (trimmed), Analysis3, and Analysis4 (trimmed), or does each
# run genuinely need its own b?
# =========================================================
# Read-only analysis -- prints results only, does not touch pipeline files.

combined <- data.frame(
  Run = c(rep("Analysis1", 3), rep("Analysis3", 3), rep("Analysis4", 4)),
  Row = c(13, 27, 41,
          15, 27, 41,
          15, 31, 47, 57),
  PA  = c(104761.219984451, 64651.9581215719, 56215.1827558455,
          104163.887107497, 64969.2676868348, 47500.8799570005,
          88539.1121616264, 50066.1819600554, 43239.0401006875, 40002.3037771265)
)
combined$Run <- factor(combined$Run)

n_total <- nrow(combined)

# ---- Full model: separate a AND b per run (3 independent fits) ----
runs <- levels(combined$Run)
rss_full <- 0
k_full <- 0
cat("=== Full model: independent a and b per run ===\n")
indiv_fits <- list()
for (r in runs) {
  d <- combined[combined$Run == r, ]
  fit <- nls(PA ~ a * Row^b, data = d,
             start = list(a = max(d$PA), b = -1.0),
             control = nls.control(warnOnly = TRUE, maxiter = 200))
  indiv_fits[[r]] <- fit
  rss_r <- sum(residuals(fit)^2)
  rss_full <- rss_full + rss_r
  k_full <- k_full + 2
  cat(sprintf("  %s: a=%.1f, b=%.4f, RSS=%.0f\n", r, coef(fit)["a"], coef(fit)["b"], rss_r))
}
df_full <- n_total - k_full
cat(sprintf("Total RSS_full = %.0f, k_full = %d, df_full = %d\n\n", rss_full, k_full, df_full))

# ---- Reduced model: separate a per run, single SHARED b ----
cat("=== Reduced model: separate a per run, single shared b ===\n")
shared_fit <- nls(
  PA ~ a[Run] * Row^b,
  data = combined,
  start = list(a = setNames(rep(80000, length(runs)), NULL), b = -0.6),
  algorithm = "port"
)
print(summary(shared_fit))
rss_reduced <- sum(residuals(shared_fit)^2)
k_reduced <- length(runs) + 1  # one a per run + one shared b
df_reduced <- n_total - k_reduced
cat(sprintf("\nRSS_reduced = %.0f, k_reduced = %d, df_reduced = %d\n", rss_reduced, k_reduced, df_reduced))
cat(sprintf("Shared b estimate: %.4f (SE %.4f)\n",
            coef(shared_fit)["b"], summary(shared_fit)$coefficients["b", "Std. Error"]))

# ---- Nested F-test: does allowing separate b per run significantly
#      improve the fit over a single shared b? ----
cat("\n=== F-test: separate b (full) vs shared b (reduced) ===\n")
df1 <- df_reduced - df_full   # extra parameters in full model
df2 <- df_full
F_stat <- ((rss_reduced - rss_full) / df1) / (rss_full / df2)
p_val <- pf(F_stat, df1, df2, lower.tail = FALSE)
cat(sprintf("F(%d, %d) = %.3f, p = %.4f\n", df1, df2, F_stat, p_val))
cat(if (p_val < 0.05) {
  "=> Significant: separate b per run fits meaningfully better (shared exponent rejected)\n"
} else {
  "=> Not significant: no evidence separate b per run improves fit over a single shared exponent\n"
})

# ---- AIC comparison (RSS-based, valid for comparing nested LS models with equal n) ----
aic <- function(rss, n, k) n * log(rss / n) + 2 * k
aic_full    <- aic(rss_full, n_total, k_full)
aic_reduced <- aic(rss_reduced, n_total, k_reduced)
cat(sprintf("\nAIC (separate b): %.2f\nAIC (shared b):    %.2f\n", aic_full, aic_reduced))
cat(if (aic_reduced < aic_full) "=> Shared-b model preferred by AIC (simpler, near-equal fit)\n"
    else "=> Separate-b model preferred by AIC\n")

# ---- Individual b estimates for reference ----
cat("\n=== Individual b estimates (for reference) ===\n")
for (r in runs) {
  cat(sprintf("  %s: b = %.4f\n", r, coef(indiv_fits[[r]])["b"]))
}
