#===============================================================================
# DESIGN OF EXPERIMENTS: TRACE EXPLOSIVES RECOVERY
# 
# Full factorial 2^3 design examining effects of:
#   - Surface (steel vs. abs)
#   - Pressure (low vs. high)
#   - Solvent presence (absent vs. present)
#
# Features:
#   - Complete block design (within-surface blocking) for higher power
#   - Toggle between blocked and non-blocked designs
#   - Power analysis with sensitivity analysis for multiple effect sizes
#
# Author: Generated for explosives trace analysis research
# Date: 2025-06-02
# Updated: 2025-06-02 - Added blocked design with within-surface blocking
#===============================================================================

#===============================================================================
# USER CONFIGURATION - MODIFY THESE PARAMETERS
#===============================================================================

# Design type selection
use_blocking <- FALSE           # Set to TRUE for blocked design, FALSE for standard

# Blocked design parameters (used if use_blocking = TRUE)
n_surfaces <- 12              # Number of physical surface samples (blocks)
n_conditions <- 8             # Total factorial conditions per surface (don't change)
block_variance_reduction <- 0.30  # Expected variance reduction from blocking (0.20-0.50)

# Non-blocked design parameters (used if use_blocking = FALSE)
n_replicates <- 11            # Replicates per condition (non-blocked only)

# Common parameters
alpha_level <- 0.05           # Significance level for statistical tests

# Effect sizes for sensitivity analysis (Cohen's f conventions)
effect_sizes <- c(0.10, 0.25, 0.40)  # Small, medium, large
effect_labels <- c("Small (f = 0.10)", "Medium (f = 0.25)", "Large (f = 0.40)")

# Output directory (change if needed)
output_dir <- "C:/Users/A Bruce - User/Documents/TransferPersistenceRecoveryExplosives/ASTRA/doe"

#===============================================================================
# PACKAGE INSTALLATION AND LOADING
#===============================================================================

# Install packages if not available
required_packages <- c("FrF2", "DoE.base", "ggplot2", "pwr", "gridExtra", "car", "lme4", "lmerTest", "MuMIn")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Set seed for reproducibility
set.seed(2025)

#===============================================================================
# DESIGN GENERATION
#===============================================================================

if (use_blocking) {
  
  cat("\n========================================\n")
  cat("GENERATING BLOCKED EXPERIMENTAL DESIGN\n")
  cat("========================================\n\n")
  
  # Create full factorial conditions (8)
  conditions <- expand.grid(
    Surface_type = c("steel", "abs"),
    Pressure = c("low", "high"),
    Solvent = c("absent", "present")
  )
  
  # Replicate for each surface block
  design_list <- list()
  for (surf in 1:n_surfaces) {
    surf_data <- conditions
    surf_data$SurfaceID <- paste0("Surface_", sprintf("%02d", surf))
    surf_data$TestOrder <- sample(1:nrow(conditions))
    design_list[[surf]] <- surf_data
  }
  
  design_blocked <- do.call(rbind, design_list)
  
  # Add run identifiers
  design_blocked$RunID <- sprintf("RUN_%03d", 1:nrow(design_blocked))
  
  # Reorder columns
  design_blocked <- design_blocked[, c("RunID", "SurfaceID", "TestOrder", 
                                        "Surface_type", "Pressure", "Solvent")]
  
  # Add placeholder for recovery
  design_blocked$Recovery <- NA
  
  # Sort by SurfaceID and TestOrder for export
  design_blocked <- design_blocked[order(design_blocked$SurfaceID, design_blocked$TestOrder), ]
  
  # Shuffie rows to randomize which surface is tested first overall
  surface_order <- sample(unique(design_blocked$SurfaceID))
  design_blocked$SurfaceID <- factor(design_blocked$SurfaceID, levels = surface_order)
  design_blocked <- design_blocked[order(design_blocked$SurfaceID, design_blocked$TestOrder), ]
  
  # Display summary
  cat("Blocked design structure:\n")
  cat("  - Number of surfaces (blocks):", n_surfaces, "\n")
  cat("  - Conditions per surface:", n_conditions, "\n")
  cat("  - Total runs:", nrow(design_blocked), "\n")
  cat("  - Block variance reduction:", block_variance_reduction*100, "%\n\n")
  
  cat("Design matrix preview (first 15 rows):\n")
  print(head(design_blocked, 15))
  cat("\n")
  
  # Save design
  design_file <- file.path(output_dir, "design_matrix_blocked.csv")
  write.csv(design_blocked, design_file, row.names = FALSE)
  cat("Design matrix saved to:", design_file, "\n\n")
  
} else {
  
  cat("\n========================================\n")
  cat("GENERATING STANDARD EXPERIMENTAL DESIGN\n")
  cat("========================================\n\n")
  
  # Create full factorial 2^3 design
  factorial_design <- FrF2(
    nruns = 8,
    nfactors = 3,
    factor.names = list(
      Surface_type = c("smooth", "textured"),
      Pressure = c("low", "high"),
      Solvent = c("absent", "present")
    ),
    randomize = TRUE,
    seed = 2025
  )
  
  # Convert to data frame
  design_df <- as.data.frame(factorial_design)
  
  # Replicate design by n_replicates
  design_replicated <- design_df[rep(1:nrow(design_df), each = n_replicates), ]
  
  # Add run identifiers
  design_replicated$RunID <- sprintf("RUN_%03d", 1:nrow(design_replicated))
  design_replicated$SampleID <- sprintf("SMP_%03d", 1:nrow(design_replicated))
  
  # Randomize the full run order
  design_replicated$Block <- rep(1:n_replicates, times = 8)
  design_replicated <- design_replicated[sample(1:nrow(design_replicated)), ]
  
  # Rename columns for clarity
  names(design_replicated) <- c("Surface_type", "Pressure", "Solvent", "RunID", "SampleID", "Block")
  
  # Reorder columns
  design_replicated <- design_replicated[, c("RunID", "SampleID", "Block", "Surface_type", "Pressure", "Solvent")]
  
  # Add placeholder for recovery data
  design_replicated$Recovery <- NA
  
  # Display design summary
  cat("Design structure:\n")
  cat("  - Factors: Surface_type, Pressure, Solvent\n")
  cat("  - Levels per factor: 2\n")
  cat("  - Total conditions: 8\n")
  cat("  - Replicates per condition:", n_replicates, "\n")
  cat("  - Total runs:", nrow(design_replicated), "\n\n")
  
  # Show design matrix preview
  cat("Design matrix preview (first 10 rows):\n")
  print(head(design_replicated, 10))
  cat("\n")
  
  # Save design matrix to CSV
  design_file <- file.path(output_dir, "design_matrix.csv")
  write.csv(design_replicated, design_file, row.names = FALSE)
  cat("Design matrix saved to:", design_file, "\n\n")
}

#===============================================================================
# POWER ANALYSIS
#===============================================================================

if (use_blocking) {
  
  cat("\n========================================\n")
  cat("POWER ANALYSIS FOR BLOCKED DESIGN\n")
  cat("========================================\n\n")
  
  # Calculate parameters for blocked design
  total_runs <- n_surfaces * n_conditions
  n_effects <- 7
  df_effect <- 1
  df_blocks <- n_surfaces - 1
  df_error_blocked <- total_runs - n_effects - df_blocks - 1
  
  # Effective variance reduction
  effective_variance <- 1 - block_variance_reduction
  
  cat("Power analysis for blocked design:\n")
  cat("  - Number of surfaces (blocks):", n_surfaces, "\n")
  cat("  - Total runs:", total_runs, "\n")
  cat("  - Degrees of freedom (model):", n_effects, "\n")
  cat("  - Degrees of freedom (blocks):", df_blocks, "\n")
  cat("  - Degrees of freedom (error):", df_error_blocked, "\n")
  cat("  - Block variance reduction:", block_variance_reduction*100, "%\n")
  cat("  - Significance level:", alpha_level, "\n\n")
  
  cat("Power for each effect size (adjusted for blocking):\n")
  cat("---------------------------------------------------------------\n")
  
  achieved_powers <- numeric(length(effect_sizes))
  for (i in seq_along(effect_sizes)) {
    f2_orig <- effect_sizes[i]^2
    f2_adjusted <- f2_orig / effective_variance
    
    achieved_powers[i] <- pwr.f2.test(
      u = df_effect,
      v = df_error_blocked,
      f2 = f2_adjusted,
      sig.level = alpha_level
    )$power
    
    cat(sprintf("  %s: Power = %.3f\n", effect_labels[i], achieved_powers[i]))
  }
  cat("\n")
  
} else {
  
  cat("\n========================================\n")
  cat("POWER ANALYSIS\n")
  cat("========================================\n\n")
  
  # Calculate degrees of freedom
  n_total <- 8 * n_replicates
  df_effect <- 1    # Testing one effect at a time (most conservative)
  df_error <- n_total - 8
  
  #Display power for each effect size
  cat("Power analysis for current settings (n =", n_replicates, "replicates):\n")
  cat("  - Total observations:", n_total, "\n")
  cat("  - Degrees of freedom (error):", df_error, "\n")
  cat("  - Significance level:", alpha_level, "\n")
  cat("---------------------------------------------------------------\n")
  
  achieved_powers <- numeric(length(effect_sizes))
  for (i in seq_along(effect_sizes)) {
    f2 <- effect_sizes[i]^2
    achieved_powers[i] <- pwr.f2.test(
      u = df_effect,
      v = df_error,
      f2 = f2,
      sig.level = alpha_level
    )$power
    cat(sprintf("  %s: Power = %.3f\n", effect_labels[i], achieved_powers[i]))
  }
  cat("\n")
}

# Generate power curves
if (use_blocking) {
  
  # Power curves for blocked design: vary number of surfaces
  surface_range <- 4:20
  
  power_curves <- data.frame()
  for (j in seq_along(effect_sizes)) {
    f2_orig <- effect_sizes[j]^2
    f2_adjusted <- f2_orig / effective_variance
    
    for (k in seq_along(surface_range)) {
      n_surf_temp <- surface_range[k]
      total_temp <- n_surf_temp * n_conditions
      df_blocks_temp <- n_surf_temp - 1
      df_error_temp <- total_temp - n_effects - df_blocks_temp - 1
      
      if (df_error_temp > 0) {
        power_curves <- rbind(power_curves, data.frame(
          Surfaces = n_surf_temp,
          Power = pwr.f2.test(
            u = df_effect,
            v = df_error_temp,
            f2 = f2_adjusted,
            sig.level = alpha_level
          )$power,
          Effect_Size = effect_labels[j]
        ))
      }
    }
  }
  
  # Ensure Effect_Size is a factor with proper ordering
  power_curves$Effect_Size <- factor(power_curves$Effect_Size, levels = effect_labels)
  
  # Plot power curves for blocked design
  power_plot <- ggplot(power_curves, aes(x = Surfaces, y = Power, color = Effect_Size, linetype = Effect_Size)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = c(0.80, 0.90, 0.95), linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_vline(xintercept = n_surfaces, linetype = "solid", color = "red", size = 1.2, alpha = 0.8) +
    annotate("text", x = 20.5, y = c(0.80, 0.90, 0.95), 
             label = c("0.80", "0.90", "0.95"), size = 3.5, color = "gray50", hjust = 0) +
    scale_color_manual(values = c("Small (f = 0.10)" = "#E41A1C", 
                                   "Medium (f = 0.25)" = "#377EB8", 
                                   "Large (f = 0.40)" = "#4DAF4A")) +
    scale_linetype_manual(values = c("Small (f = 0.10)" = "dotted", 
                                      "Medium (f = 0.25)" = "dashed", 
                                      "Large (f = 0.40)" = "solid")) +
    scale_y_continuous(limits = c(0.05, 1.02), breaks = seq(0.2, 1, 0.2)) +
    scale_x_continuous(breaks = seq(4, 20, 2), limits = c(3, 21.5)) +
    labs(
      title = "Statistical Power vs. Number of Surfaces (Blocked Design)",
      subtitle = paste0("Each surface tested under 8 conditions, ", 
                        block_variance_reduction*100, "% variance reduction"),
      x = "Number of Surface Samples (Blocks)",
      y = "Statistical Power",
      color = "Effect Size",
      linetype = "Effect Size",
      caption = paste0("Red line: current setting (", n_surfaces, " surfaces). Dashed horizontal: power thresholds.")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0, color = "gray50"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10)
    )
  
} else {
  
  # Power curves for non-blocked design: vary number of replicates
  rep_range <- 2:15
  
  power_curves <- data.frame()
  for (j in seq_along(effect_sizes)) {
    f2 <- effect_sizes[j]^2
    for (i in seq_along(rep_range)) {
      n_temp <- 8 * rep_range[i]
      df_error_temp <- n_temp - 8
      power_curves <- rbind(power_curves, data.frame(
        Replicates = rep_range[i],
        Power = pwr.f2.test(
          u = df_effect,
          v = df_error_temp,
          f2 = f2,
          sig.level = alpha_level
        )$power,
        Effect_Size = effect_labels[j]
      ))
    }
  }
  
  # Ensure Effect_Size is a factor with proper ordering
  power_curves$Effect_Size <- factor(power_curves$Effect_Size, levels = effect_labels)
  
  # Plot power curves
  power_plot <- ggplot(power_curves, aes(x = Replicates, y = Power, color = Effect_Size, linetype = Effect_Size)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = c(0.80, 0.90, 0.95), linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_vline(xintercept = n_replicates, linetype = "solid", color = "red", size = 1.2, alpha = 0.8) +
    annotate("text", x = 15.3, y = c(0.80, 0.90, 0.95), 
             label = c("0.80", "0.90", "0.95"), size = 3.5, color = "gray50", hjust = 0) +
    scale_color_manual(values = c("Small (f = 0.10)" = "#E41A1C", 
                                   "Medium (f = 0.25)" = "#377EB8", 
                                   "Large (f = 0.40)" = "#4DAF4A")) +
    scale_linetype_manual(values = c("Small (f = 0.10)" = "dotted", 
                                      "Medium (f = 0.25)" = "dashed", 
                                      "Large (f = 0.40)" = "solid")) +
    scale_y_continuous(limits = c(0.05, 1.02), breaks = seq(0.2, 1, 0.2)) +
    scale_x_continuous(breaks = seq(2, 15, 1), limits = c(1.5, 16)) +
    labs(
      title = "Statistical Power vs. Number of Replicates",
      subtitle = paste0("Sensitivity analysis for α = ", alpha_level, " (n_current = ", n_replicates, ")"),
      x = "Replicates per Condition",
      y = "Statistical Power",
      color = "Effect Size",
      linetype = "Effect Size",
      caption = paste0("Red line: current setting (n=", n_replicates, "). Dashed horizontal: power thresholds.")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0, color = "gray50"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10)
    )
}

# Save power plot
power_file <- file.path(output_dir, "power_analysis.png")
ggsave(power_file, power_plot, width = 10, height = 6, dpi = 300)
cat("Power analysis plot saved to:", power_file, "\n\n")

# Save summary table
summary_table <- data.frame(
  Effect_Size = effect_labels,
  Cohen_f = effect_sizes,
  f_squared = effect_sizes^2,
  Power_at_current_setting = achieved_powers
)
summary_file <- file.path(output_dir, "power_sensitivity_summary.csv")
write.csv(summary_table, summary_file, row.names = FALSE)
cat("Power summary table saved to:", summary_file, "\n\n")

# Calculate minimum surfaces/replicates needed
if (use_blocking) {
  cat("\nMinimum surfaces needed to achieve power thresholds:\n")
  cat("--------------------------------------------------------\n")
  for (j in seq_along(effect_sizes)) {
    cat(effect_labels[j], ":\n")
    f2_orig <- effect_sizes[j]^2
    f2_adjusted <- f2_orig / effective_variance
    for (threshold in c(0.80, 0.90, 0.95)) {
      min_surf <- NA
      for (n_surf_test in 4:25) {
        df_blocks_test <- n_surf_test - 1
        df_error_test <- (n_surf_test * n_conditions) - n_effects - df_blocks_test - 1
        if (df_error_test > 0) {
          pwr_test <- pwr.f2.test(u = df_effect, v = df_error_test, f2 = f2_adjusted, sig.level = alpha_level)$power
          if (pwr_test >= threshold) {
            min_surf <- n_surf_test
            break
          }
        }
      }
      if (!is.na(min_surf)) {
        cat(sprintf("    Power ≥ %.2f: %d surfaces (%d total runs)\n", threshold, min_surf, min_surf * n_conditions))
      } else {
        cat(sprintf("    Power ≥ %.2f: Not achieved with ≤ 25 surfaces\n", threshold))
      }
    }
    cat("\n")
  }
} else {
  cat("\nMinimum replicates needed to achieve power thresholds:\n")
  cat("--------------------------------------------------------\n")
  for (j in seq_along(effect_sizes)) {
    cat(effect_labels[j], ":\n")
    f2 <- effect_sizes[j]^2
    for (threshold in c(0.80, 0.90, 0.95)) {
      min_n <- NA
      for (n_test in 2:30) {
        df_test <- (8 * n_test) - 8
        pwr_test <- pwr.f2.test(u = df_effect, v = df_test, f2 = f2, sig.level = alpha_level)$power
        if (pwr_test >= threshold) {
          min_n <- n_test
          break
        }
      }
      if (!is.na(min_n)) {
        cat(sprintf("    Power ≥ %.2f: n = %d replicates (%d total runs)\n", threshold, min_n, min_n * 8))
      } else {
        cat(sprintf("    Power ≥ %.2f: Not achieved with n ≤ 30\n", threshold))
      }
    }
    cat("\n")
  }
}

#===============================================================================
# ANALYSIS TEMPLATE - RUN AFTER DATA COLLECTION
#===============================================================================

cat("\n========================================\n")
cat("ANALYSIS TEMPLATE\n")
cat("========================================\n\n")

if (use_blocking) {
  cat("After collecting data, add recovery values to the CSV file,\n")
  cat("then run the analysis code below.\n\n")
  
  cat("Template for blocked design analysis:\n")
  cat("---------------------------------------------------------------\n")
  cat("# Load required packages\n")
  cat("library(lme4)\n")
  cat("library(lmerTest)  # Adds p-values\n\n")
  
  cat("# Read data with recovery values\n")
  cat("data <- read.csv(file.path(output_dir, 'design_matrix_blocked.csv'))\n\n")
  
  cat("# Fit mixed-effects model (Surface as random effect)\n")
  cat("model <- lmer(Recovery ~ Surface_type * Pressure * Solvent + \n")
  cat("               (1|SurfaceID), data = data)\n\n")
  
  cat("# View ANOVA table (Type III)\n")
  cat("anova(model)\n\n")
  
  cat("# View fixed effects and p-values\n")
  cat("summary(model)\n\n")
  
  cat("# Diagnostic plots\n")
  cat("plot(model)  # Residuals vs fitted\n")
  cat("qqnorm(resid(model))  # Normality check\n")
  cat("qqline(resid(model))\n\n")
  
  cat("# Effect sizes (marginal R-squared)\n")
  cat("library(MuMIn)\n")
  cat("r.squaredGLMM(model)  # Returns R2m (marginal) and R2c (conditional)\n\n")
  
} else {
  cat("After collecting data, add recovery values to the CSV file,\n")
  cat("then run the analysis section below.\n\n")
  
  cat("Template for analysis (uncomment and run after data collection):\n")
  cat("---------------------------------------------------------------\n")
  cat("# Read data with recovery values\n")
  cat("data <- read.csv(file.path(output_dir, 'design_matrix.csv'))\n\n")
  cat("# Fit full factorial model\n")
  cat("model <- lm(Recovery ~ Surface_type * Pressure * Solvent, data = data)\n\n")
  cat("# ANOVA table (Type III)\n")
  cat("library(car)\n")
  cat("anova_table <- Anova(model, type = 3)\n")
  cat("print(anova_table)\n\n")
  cat("# Effect sizes (partial eta-squared)\n")
  cat("effect_sizes <- summary.lm(model)\n")
  cat("print(effect_sizes)\n\n")
  cat("# Diagnostic plots\n")
  cat("par(mfrow = c(2, 2))\n")
  cat("plot(model)\n")
  cat("par(mfrow = c(1, 1))\n\n")
  cat("# Generate visualizations...\n\n")
}

#===============================================================================
# MAIN EFFECTS PLOTS TEMPLATE
#===============================================================================

main_effects_template <- function(data, use_blocking = FALSE) {
  # Calculate means for each factor level
  if (use_blocking) {
    surface_means <- aggregate(Recovery ~ Surface_type, data = data, FUN = mean)
    pressure_means <- aggregate(Recovery ~ Pressure, data = data, FUN = mean) 
    solvent_means <- aggregate(Recovery ~ Solvent, data = data, FUN = mean)
    
    plot_data <- rbind(
      data.frame(Factor = "Surface_type", Level = surface_means$Surface_type, Recovery = surface_means$Recovery),
      data.frame(Factor = "Pressure", Level = pressure_means$Pressure, Recovery = pressure_means$Recovery),
      data.frame(Factor = "Solvent", Level = solvent_means$Solvent, Recovery = solvent_means$Recovery)
    )
  } else {
    surface_means <- aggregate(Recovery ~ Surface_type, data = data, FUN = mean)
    pressure_means <- aggregate(Recovery ~ Pressure, data = data, FUN = mean) 
    solvent_means <- aggregate(Recovery ~ Solvent, data = data, FUN = mean)
    
    plot_data <- rbind(
      data.frame(Factor = "Surface_type", Level = surface_means$Surface_type, Recovery = surface_means$Recovery),
      data.frame(Factor = "Pressure", Level = pressure_means$Pressure, Recovery = pressure_means$Recovery),
      data.frame(Factor = "Solvent", Level = solvent_means$Solvent, Recovery = solvent_means$Recovery)
    )
  }
  
  ggplot(plot_data, aes(x = Level, y = Recovery, fill = Level)) +
    geom_bar(stat = "identity", width = 0.6) +
    facet_wrap(~ Factor, scales = "free_x") +
    labs(title = "Main Effects Plot", y = "Mean Recovery") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"))
}

#===============================================================================
# INTERACTION PLOTS TEMPLATE
#===============================================================================

interaction_plots_template <- function(data) {
  # Two-way interaction: Surface_type x Pressure
  p1 <- ggplot(data, aes(x = Surface_type, y = Recovery, color = Pressure, group = Pressure)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    labs(title = "Surface_type x Applied Mass Interaction", y = "Mean Recovery") +
    theme_bw()
  
  # Two-way interaction: Surface_type x Solvent
  p2 <- ggplot(data, aes(x = Surface_type, y = Recovery, color = Solvent, group = Solvent)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    labs(title = "Surface_type x Solvent Interaction", y = "Mean Recovery") +
    theme_bw()
  
  # Two-way interaction: Pressure x Solvent
  p3 <- ggplot(data, aes(x = Pressure, y = Recovery, color = Solvent, group = Solvent)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    labs(title = "Applied Mass x Solvent Interaction", y = "Mean Recovery") +
    theme_bw()
  
  # Arrange plots
  grid.arrange(p1, p2, p3, ncol = 3)
}

#===============================================================================
# PARETO CHART TEMPLATE
#===============================================================================

pareto_chart_template <- function(model, use_blocking = FALSE) {
  if (use_blocking) {
    # For lmer models, use lmerTest summary
    coef_summary <- summary(model)$coefficients
  } else {
    # For lm models
    coef_summary <- summary(model)$coefficients
  }
  
  # Calculate standardized effects (absolute t-values)
  effects_df <- data.frame(
    Effect = rownames(coef_summary)[-1],  # Exclude intercept
    t_value = abs(coef_summary[-1, "t value"])
  )
  
  # Order by magnitude
  effects_df <- effects_df[order(effects_df$t_value, decreasing = TRUE), ]
  effects_df$Effect <- factor(effects_df$Effect, levels = rev(effects_df$Effect))
  
  ggplot(effects_df, aes(x = Effect, y = t_value, fill = t_value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Pareto Chart of Standardized Effects",
         y = "Absolute t-value",
         x = "Effect") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"))
}

#===============================================================================
# COMPLETION SUMMARY
#===============================================================================

cat("\n========================================\n")
cat("DESIGN GENERATION COMPLETE\n")
cat("========================================\n\n")

cat("Design type:", ifelse(use_blocking, "Complete Block Design (with surface blocking)", 
                          "Full Factorial (Non-Blocked)"), "\n\n")

cat("Files generated:\n")
cat("  1.", design_file, "\n")
cat("  2.", power_file, "\n")
cat("  3.", summary_file, "\n\n")

if (use_blocking) {
  cat("Summary for blocked design (", n_surfaces, " surfaces):\n")
  cat("---------------------------------------------------------------\n")
  cat("  - Total runs:", n_surfaces * n_conditions, "\n")
  cat("  - Block variance reduction:", block_variance_reduction*100, "%\n\n")
} else {
  cat("Summary for n =", n_replicates, "replicates per condition:\n")
  cat("---------------------------------------------------------------\n")
  cat("  - Total runs:", n_replicates * n_conditions, "\n\n")
}

for (i in seq_along(effect_sizes)) {
  cat(sprintf("  %s: Power = %.3f\n", effect_labels[i], achieved_powers[i]))
}
cat("\n")

cat("Next steps:\n")
if (use_blocking) {
  cat("  1. Review power_sensitivity_summary.csv to verify design adequacy\n")
  cat("  2. Import design_matrix_blocked.csv into your lab notebook\n")
  cat("  3. Test each surface under all 8 conditions in randomized order\n")
  cat("  4. Clean surfaces between tests as needed\n")
  cat("  5. Record recovery values in the Recovery column\n")
  cat("  6. Use mixed-effects model (lmer) for analysis\n\n")
} else {
  cat("  1. Review power_sensitivity_summary.csv to verify design adequacy\n")
  cat("  2. Import design_matrix.csv into your lab notebook\n")
  cat("  3. Run experiments in the randomized order shown\n")
  cat("  4. Record recovery values in the Recovery column\n")
  cat("  5. Re-run analysis sections with your data\n\n")
}

cat("To modify design:\n")
if (use_blocking) {
  cat("  - Change 'use_blocking' to FALSE for non-blocked design\n")
  cat("  - Edit 'n_surfaces' to change number of surface samples\n")
  cat("  - Adjust 'block_variance_reduction' based on pilot data\n")
} else {
  cat("  - Change 'use_blocking' to TRUE for blocked design (higher power)\n")
  cat("  - Edit 'n_replicates' to change replicates per condition\n")
}
cat("  - Re-run script to regenerate design and power analysis\n\n")
