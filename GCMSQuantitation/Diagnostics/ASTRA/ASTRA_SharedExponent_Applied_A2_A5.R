# =========================================================
# Test: does the pooled exponent (b = -0.6555, estimated from
# Analysis1/3/4 trimmed) improve on standalone fits for Analysis2
# and Analysis5 if only the amplitude (a) is estimated per-run?
# =========================================================
# Read-only analysis -- prints results only, does not touch pipeline files.

b_shared <- -0.6555

fit_amplitude_only <- function(name, df, b_fixed) {
  cat("\n=== ", name, " -- amplitude-only fit, b fixed at ", round(b_fixed, 4), " ===\n", sep = "")
  cat("Row:", df$Row, "\n")
  cat("PA :", round(df$PA, 1), "\n")

  # Closed-form least-squares amplitude for fixed b:
  # minimize sum((PA - a*Row^b)^2) w.r.t a  =>  a = sum(PA * Row^b) / sum(Row^(2b))
  a_hat <- sum(df$PA * df$Row^b_fixed) / sum(df$Row^(2 * b_fixed))
  predicted <- a_hat * df$Row^b_fixed
  resid <- df$PA - predicted
  ss_res <- sum(resid^2)
  ss_tot <- sum((df$PA - mean(df$PA))^2)
  r2 <- 1 - ss_res / ss_tot

  cat(sprintf("a_hat = %.1f\n", a_hat))
  cat("Predicted:", round(predicted, 1), "\n")
  cat("Residuals:", round(resid, 1), "\n")
  cat(sprintf("R2 (fixed b, amplitude-only) = %.4f\n", r2))

  # Observed decline ratio vs what fixed-b power law implies between first/last point
  obs_ratio <- df$PA[nrow(df)] / df$PA[1]
  implied_ratio <- (df$Row[nrow(df)] / df$Row[1])^b_fixed
  cat(sprintf("Observed decline ratio (last/first PA): %.4f\n", obs_ratio))
  cat(sprintf("Implied decline ratio under shared-b power law: %.4f\n", implied_ratio))

  invisible(list(r2 = r2, resid = resid))
}

# ---- Analysis2 (2 QCs, the terminal collapse) ----
df_a2 <- data.frame(Row = c(15, 29), PA = c(106130.993644954, 4264.65619871407))
res_a2 <- fit_amplitude_only("Analysis2", df_a2, b_shared)

# For comparison: Analysis2's own best-fit b (2 points -> exact fit, trivial R2=1)
own_a2 <- nls(PA ~ a * Row^b, data = df_a2,
              start = list(a = max(df_a2$PA), b = -1.0),
              control = nls.control(warnOnly = TRUE, maxiter = 200))
cat(sprintf("\n[Reference] Analysis2 own-b power law (2 pts, exact fit): b = %.4f (a=%.1f)\n",
            coef(own_a2)["b"], coef(own_a2)["a"]))

# ---- Analysis5 (3 QCs) ----
df_a5 <- data.frame(Row = c(17, 29, 41), PA = c(92733.3813310332, 63585.4960797321, 38059.1354206726))
res_a5 <- fit_amplitude_only("Analysis5", df_a5, b_shared)

# Comparison table: shared-b amplitude-only vs run's own full 2-param PowerLaw vs Linear
cat("\n\n=== SUMMARY: R2 comparison ===\n")
own_pow_a5 <- nls(PA ~ a * Row^b, data = df_a5, start = list(a = max(df_a5$PA), b = -1.0),
                   control = nls.control(warnOnly = TRUE))
r2_own_pow_a5 <- 1 - sum(residuals(own_pow_a5)^2) / sum((df_a5$PA - mean(df_a5$PA))^2)
lin_a5 <- lm(PA ~ Row, data = df_a5)
r2_lin_a5 <- summary(lin_a5)$r.squared

summary_tbl <- data.frame(
  Run = c("Analysis2", "Analysis5", "Analysis5", "Analysis5"),
  Model = c("Shared-b (-0.656) amplitude-only",
            "Shared-b (-0.656) amplitude-only",
            "Own full PowerLaw (a & b both fit)",
            "Linear"),
  R2 = c(res_a2$r2, res_a5$r2, r2_own_pow_a5, r2_lin_a5)
)
print(summary_tbl, row.names = FALSE)
