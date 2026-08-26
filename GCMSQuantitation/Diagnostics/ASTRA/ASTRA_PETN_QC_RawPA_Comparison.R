# =========================================================
# ASTRA Pilot Study (Analysis1-5): PETN QC Peak Area -- PowerLaw Fits
# =========================================================
#
# Purpose: For each of the 5 ASTRA pilot runs, fit a PowerLaw decay model
# (Response ~ a * Row^b) to the raw (uncorrected) PETN QC peak areas, at
# both the 6ng and 0.2ng QC levels, and plot the fitted curve against the
# raw QC points -- to visually assess how well a PowerLaw shape describes
# each run's actual drift pattern.
#
# Output: Diagnostics/ASTRA_PETN_QC_PowerLawFit_ByRun.png
#   - 2 facets (6ng, 0.2ng), one colour per run (Analysis1-5)
#   - Points: raw QC petn_pa values
#   - Lines: fitted PowerLaw curve (Response = a * Row^b), evaluated only
#     within that run's own observed Row range (no extrapolation)
#
# Data source: each run's own {Analysis}_GCMSResults.csv (already
# processed, drift-uncorrected petn_pa column), read directly -- no
# reprocessing, no GlobalCode.R sourcing needed.

library(dplyr)
library(ggplot2)
library(readr)

astra_root <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/GC Data"
diagnostics_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/GCMSQuantitation/Diagnostics/ASTRA"

runs <- paste0("Analysis", 1:5)

qc_data <- lapply(runs, function(run) {
  csv_path <- file.path(astra_root, run, "Results", paste0(run, "_GCMSResults.csv"))
  if (!file.exists(csv_path)) {
    warning("Missing: ", csv_path)
    return(NULL)
  }
  df <- read_csv(csv_path, show_col_types = FALSE)
  df %>%
    filter(Type == "QC", CalLevel %in% c(0.2, 6), !is.na(petn_pa)) %>%
    arrange(CalLevel, Row) %>%
    mutate(Run = run) %>%
    select(Run, CalLevel, Line, Row, petn_pa)
}) %>% bind_rows()

qc_data <- qc_data %>%
  mutate(
    CalLevelLabel = factor(
      paste0(CalLevel, " ng"),
      levels = c("6 ng", "0.2 ng")
    )
  )

message("Loaded ", nrow(qc_data), " QC rows across ", length(runs), " runs.")
print(qc_data %>% count(Run, CalLevelLabel))

# ---- Fit PowerLaw per Run x CalLevel group, build smooth curve + R2 ----
fit_one_group <- function(run, cal_level_label, d) {
  d <- d %>% arrange(Row)
  n <- nrow(d)

  if (n < 2) {
    return(list(curve = NULL, r2 = NA_real_, a = NA_real_, b = NA_real_))
  }

  fit <- tryCatch(
    nls(petn_pa ~ a * Row^b, data = d,
        start = list(a = max(d$petn_pa), b = -1.0),
        control = nls.control(maxiter = 200, warnOnly = TRUE)),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(list(curve = NULL, r2 = NA_real_, a = NA_real_, b = NA_real_))
  }

  coefs <- coef(fit)
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((d$petn_pa - mean(d$petn_pa))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  row_grid <- seq(min(d$Row), max(d$Row), length.out = 100)
  curve <- data.frame(
    Run = run,
    CalLevelLabel = cal_level_label,
    Row = row_grid,
    Fitted = coefs["a"] * row_grid^coefs["b"]
  )

  list(curve = curve, r2 = r2, a = coefs["a"], b = coefs["b"])
}

groups <- qc_data %>% distinct(Run, CalLevelLabel)

fit_results <- lapply(seq_len(nrow(groups)), function(i) {
  run <- groups$Run[i]
  cal_level_label <- groups$CalLevelLabel[i]
  d <- qc_data %>% filter(Run == run, CalLevelLabel == cal_level_label)
  fit_one_group(run, cal_level_label, d)
})

curve_df <- bind_rows(lapply(fit_results, `[[`, "curve"))

fit_stats <- data.frame(
  Run = groups$Run,
  CalLevelLabel = groups$CalLevelLabel,
  R2 = sapply(fit_results, `[[`, "r2"),
  a  = sapply(fit_results, `[[`, "a"),
  b  = sapply(fit_results, `[[`, "b")
)
message("\nPowerLaw fit summary (a * Row^b):")
print(fit_stats %>% mutate(R2 = round(R2, 4), a = round(a, 1), b = round(b, 4)),
      row.names = FALSE)

# ---- Plot: raw QC points + fitted PowerLaw curves, faceted by CalLevel ----
p <- ggplot() +
  geom_line(data = curve_df,
            aes(x = Row, y = Fitted, colour = Run),
            linewidth = 1) +
  geom_point(data = qc_data,
             aes(x = Row, y = petn_pa, colour = Run),
             size = 2.8) +
  facet_wrap(~ CalLevelLabel, scales = "free", ncol = 1) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  ) +
  labs(
    title = "ASTRA Pilot Study: PETN QC Peak Area -- PowerLaw Fits by Run",
    subtitle = "Points = raw uncorrected petn_pa; lines = fitted PowerLaw curve (a \u00D7 Row^b) per run, within its own observed Row range",
    x = "Injection Row (sequence position)",
    y = "PETN Peak Area (raw, uncorrected)",
    colour = "Run"
  )

out_path <- file.path(diagnostics_dir, "ASTRA_PETN_QC_PowerLawFit_ByRun.png")
ggsave(out_path, p, width = 10, height = 9, dpi = 300)
message("\nSaved: ", out_path)
