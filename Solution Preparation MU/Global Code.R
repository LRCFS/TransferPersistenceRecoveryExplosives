# =============================================================================
# Solution Preparation Measurement Uncertainty Calculator
# =============================================================================
#
# Calculates combined relative standard uncertainty (ur) for a set of
# solutions prepared by serial dilution, with per-analyte tracking.
#
# Input CSV columns:
#   solution                - Name of the solution being prepared
#   parent                  - Parent solution name (blank for root/stock solutions)
#   type                    - "equipment" or "standard"
#   name                    - Equipment type (must match equipmentType in equipment
#                             file, e.g. "pipette", "volumetric flask") or standard
#                             name (e.g. "PETN")
#   label                   - Human-readable label for formula output
#   volume_or_concentration - Volume (uL) for equipment; concentration (ug/mL) for standards
#   tolerance_or_error      - (Standards only) Certified error from certificate
#   coverage                - (Standards only) Coverage factor from certificate
#
# Equipment uncertainties (tolerance_or_error, coverage) are automatically
# looked up from the equipment file by matching (name, volume_or_concentration)
# to (equipmentType, volumeUsed). The script will stop with an error if any
# equipment volume is not found in the equipment file.
#
# Output:
#   Console  - Brief summary table
#   .png     - Flowchart diagram of solution preparation hierarchy
#   .txt     - Audit report with uncertainty formulas and results
#   .csv     - Machine-readable results table
# =============================================================================

# ---- Configuration -----------------------------------------------------------
# Set the input file and output prefix here before running

input_file    <- "CalStocksNoIS.csv"
output_prefix <- "CalStocksNoIS"       # outputs: <prefix>_report.txt, <prefix>_results.csv
coverage_final <- 2                         # coverage factor for expanding the final combined ur
equipment_file <- "EquipmentUncertainties.csv"  # equipment uncertainty lookup table

# ------------------------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(sprintf("Input file not found: %s", input_file))
}
if (!file.exists(equipment_file)) {
  stop(sprintf("Equipment uncertainties file not found: %s", equipment_file))
}

output_report <- paste0(output_prefix, "_report.txt")
output_csv    <- paste0(output_prefix, "_results.csv")

# ---- Read and Validate Equipment Uncertainties --------------------------------

equip_db <- read.csv(equipment_file, stringsAsFactors = FALSE, strip.white = TRUE,
                     check.names = FALSE)

equip_required_cols <- c("equipmentType", "volumeUsed", "combined error (abs)", "coverage")
equip_missing_cols <- setdiff(equip_required_cols, colnames(equip_db))
if (length(equip_missing_cols) > 0) {
  stop(sprintf("Equipment file missing required columns: %s",
               paste(equip_missing_cols, collapse = ", ")))
}

# Forward-fill equipmentType (blank rows inherit from the row above)
for (i in seq_len(nrow(equip_db))) {
  equip_db$equipmentType[i] <- trimws(equip_db$equipmentType[i])
  if (i > 1 && equip_db$equipmentType[i] == "") {
    equip_db$equipmentType[i] <- equip_db$equipmentType[i - 1]
  }
}

# Validate no blank equipmentType remains
if (any(equip_db$equipmentType == "")) {
  stop("Equipment file has rows with blank equipmentType that could not be forward-filled.")
}

# Validate no duplicate (equipmentType, volumeUsed) pairs
equip_keys <- paste(equip_db$equipmentType, equip_db$volumeUsed, sep = "|")
dup_keys <- equip_keys[duplicated(equip_keys)]
if (length(dup_keys) > 0) {
  stop(sprintf("Duplicate (equipmentType, volumeUsed) entries in equipment file: %s",
               paste(unique(dup_keys), collapse = ", ")))
}

cat(sprintf("Equipment uncertainties loaded: %d entries from %s\n", nrow(equip_db), equipment_file))

# ---- Read and Validate Input -------------------------------------------------

df <- read.csv(input_file, stringsAsFactors = FALSE, strip.white = TRUE)

# Core columns required for all rows
required_cols <- c("solution", "parent", "type", "name", "label",
                   "volume_or_concentration")

missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

# Ensure tolerance_or_error and coverage columns exist (may be blank for equipment)
if (!"tolerance_or_error" %in% colnames(df)) {
  df$tolerance_or_error <- NA_real_
}
if (!"coverage" %in% colnames(df)) {
  df$coverage <- NA_real_
}

# Trim whitespace from character columns
for (col in c("solution", "parent", "type", "name", "label")) {
  df[[col]] <- trimws(df[[col]])
}

# Replace empty parent strings with NA
df$parent[df$parent == ""] <- NA

# Validate type values
valid_types <- c("equipment", "standard")
bad_types <- unique(df$type[!df$type %in% valid_types])
if (length(bad_types) > 0) {
  stop(sprintf("Invalid type values: %s. Must be 'equipment' or 'standard'.",
               paste(bad_types, collapse = ", ")))
}

# Validate volume_or_concentration for all rows
if (any(is.na(df$volume_or_concentration))) {
  bad_rows <- which(is.na(df$volume_or_concentration))
  stop(sprintf("Missing values in 'volume_or_concentration' at rows: %s",
               paste(bad_rows, collapse = ", ")))
}
if (any(df$volume_or_concentration <= 0)) {
  bad_rows <- which(df$volume_or_concentration <= 0)
  stop(sprintf("Non-positive values in 'volume_or_concentration' at rows: %s",
               paste(bad_rows, collapse = ", ")))
}

# Validate tolerance_or_error and coverage for standard rows only
std_idx <- which(df$type == "standard")
for (col in c("tolerance_or_error", "coverage")) {
  if (any(is.na(df[[col]][std_idx]))) {
    bad_rows <- std_idx[is.na(df[[col]][std_idx])]
    stop(sprintf("Missing values in '%s' for standard rows: %s", col,
                 paste(bad_rows, collapse = ", ")))
  }
  if (any(df[[col]][std_idx] <= 0)) {
    bad_rows <- std_idx[df[[col]][std_idx] <= 0]
    stop(sprintf("Non-positive values in '%s' for standard rows: %s", col,
                 paste(bad_rows, collapse = ", ")))
  }
}

# Validate parent references
all_solutions <- unique(df$solution)
parents_referenced <- unique(df$parent[!is.na(df$parent)])
bad_parents <- setdiff(parents_referenced, all_solutions)
if (length(bad_parents) > 0) {
  stop(sprintf("Parent solutions referenced but not defined: %s",
               paste(bad_parents, collapse = ", ")))
}

# Validate that standards only appear in root solutions
standard_rows <- df[df$type == "standard", ]
if (any(!is.na(standard_rows$parent))) {
  bad_sols <- unique(standard_rows$solution[!is.na(standard_rows$parent)])
  stop(sprintf(
    "Standards can only be defined in root solutions (no parent). Found standards in: %s",
    paste(bad_sols, collapse = ", ")))
}

cat("Input validated successfully.\n")
cat(sprintf("  Solutions: %d\n", length(all_solutions)))
cat(sprintf("  Rows: %d\n", nrow(df)))
cat("\n")

# ---- Equipment Uncertainty Lookup --------------------------------------------

equip_idx <- which(df$type == "equipment")

# Build lookup key for each equipment row: (name, volume_or_concentration)
# Match against (equipmentType, volumeUsed) in equip_db
unmatched <- character(0)

for (i in equip_idx) {
  eq_name   <- df$name[i]
  eq_volume <- df$volume_or_concentration[i]

  match_idx <- which(equip_db$equipmentType == eq_name &
                     equip_db$volumeUsed == eq_volume)

  if (length(match_idx) == 0) {
    unmatched <- c(unmatched,
      sprintf("  Row %d: solution='%s', name='%s', volume=%s",
              i, df$solution[i], eq_name, eq_volume))
  } else {
    df$tolerance_or_error[i] <- equip_db$`combined error (abs)`[match_idx[1]]
    df$coverage[i]           <- equip_db$coverage[match_idx[1]]
  }
}

if (length(unmatched) > 0) {
  stop(sprintf(
    paste("Equipment volume not found in %s.",
          "No matching (equipmentType, volumeUsed) entry for:\n%s",
          "\nCheck that 'name' matches 'equipmentType' exactly and the volume is listed."),
    equipment_file, paste(unmatched, collapse = "\n")))
}

cat(sprintf("Equipment uncertainties matched for %d equipment rows.\n\n", length(equip_idx)))

# ---- Row-Level Calculations --------------------------------------------------

df$u  <- df$tolerance_or_error / df$coverage
df$ur <- df$u / df$volume_or_concentration

# ---- Build Dependency Tree ---------------------------------------------------

# Get unique solution info
solution_info <- data.frame(
  solution = all_solutions,
  parent   = sapply(all_solutions, function(s) {
    p <- unique(df$parent[df$solution == s])
    p <- p[!is.na(p)]
    if (length(p) == 0) return(NA_character_)
    if (length(p) > 1) stop(sprintf("Solution '%s' has multiple parents: %s",
                                     s, paste(p, collapse = ", ")))
    return(p)
  }),
  stringsAsFactors = FALSE
)
rownames(solution_info) <- NULL

# Detect circular dependencies
detect_cycle <- function(sol, visited, chain) {
  if (sol %in% chain) {
    stop(sprintf("Circular dependency detected: %s",
                 paste(c(chain, sol), collapse = " -> ")))
  }
  if (sol %in% visited) return(visited)
  chain <- c(chain, sol)
  parent <- solution_info$parent[solution_info$solution == sol]
  if (!is.na(parent)) {
    visited <- detect_cycle(parent, visited, chain)
  }
  return(c(visited, sol))
}

visited <- character(0)
for (sol in all_solutions) {
  visited <- detect_cycle(sol, visited, character(0))
}

# Topological sort: parents before children
topo_order <- character(0)
topo_visited <- character(0)

topo_sort <- function(sol) {
  if (sol %in% topo_visited) return()
  parent <- solution_info$parent[solution_info$solution == sol]
  if (!is.na(parent)) {
    topo_sort(parent)
  }
  topo_visited <<- c(topo_visited, sol)
  topo_order  <<- c(topo_order, sol)
}

for (sol in all_solutions) {
  topo_sort(sol)
}

# Identify root solutions
root_solutions <- solution_info$solution[is.na(solution_info$parent)]

# Identify which analytes exist (from standard rows in root solutions)
all_analytes <- unique(df$name[df$type == "standard"])

# Build map: for each solution, trace back to root to find which analytes it carries
get_analytes <- function(sol) {
  parent <- solution_info$parent[solution_info$solution == sol]
  if (is.na(parent)) {
    # Root solution: analytes come from its standard rows
    return(unique(df$name[df$solution == sol & df$type == "standard"]))
  } else {
    return(get_analytes(parent))
  }
}

solution_analytes <- lapply(all_solutions, get_analytes)
names(solution_analytes) <- all_solutions

# ---- Uncertainty Propagation -------------------------------------------------

# Storage for combined ur results: list of named vectors per solution
# Each solution maps to a named numeric vector: analyte -> ur_combined
ur_combined <- list()

for (sol in topo_order) {
  parent <- solution_info$parent[solution_info$solution == sol]
  sol_rows <- df[df$solution == sol, ]
  equip_rows <- sol_rows[sol_rows$type == "equipment", ]
  std_rows   <- sol_rows[sol_rows$type == "standard", ]

  # Sum of squared ur for all equipment in this solution
  equip_ur2_sum <- sum(equip_rows$ur^2)

  analytes <- solution_analytes[[sol]]
  result <- numeric(0)

  if (is.na(parent)) {
    # Root solution
    for (analyte in analytes) {
      std_ur <- std_rows$ur[std_rows$name == analyte]
      if (length(std_ur) != 1) {
        stop(sprintf("Expected exactly 1 standard row for '%s' in '%s', found %d",
                     analyte, sol, length(std_ur)))
      }
      result[analyte] <- sqrt(std_ur^2 + equip_ur2_sum)
    }

  } else {
    # Child solution
    parent_ur <- ur_combined[[parent]]
    for (analyte in analytes) {
      parent_analyte_ur <- parent_ur[analyte]
      result[analyte] <- sqrt(parent_analyte_ur^2 + equip_ur2_sum)
    }
  }

  ur_combined[[sol]] <- result
}

# ---- Compute Expanded Relative Uncertainty -----------------------------------

# Ur = k * ur (expanded), Ur_pct = Ur * 100 (as percentage)
Ur_expanded <- list()
Ur_pct      <- list()

for (sol in topo_order) {
  Ur_expanded[[sol]] <- coverage_final * ur_combined[[sol]]
  Ur_pct[[sol]]      <- Ur_expanded[[sol]] * 100
}

# ---- Build Results Data Frame ------------------------------------------------

results <- data.frame(
  solution     = character(0),
  analyte      = character(0),
  ur_combined  = numeric(0),
  Ur_combined  = numeric(0),
  Ur_percent   = numeric(0),
  stringsAsFactors = FALSE
)

for (sol in topo_order) {
  ur_vals <- ur_combined[[sol]]
  Ur_vals <- Ur_expanded[[sol]]
  Ur_pct_vals <- Ur_pct[[sol]]
  analytes <- names(ur_vals)
  for (a in analytes) {
    results <- rbind(results, data.frame(
      solution    = sol,
      analyte     = a,
      ur_combined = ur_vals[a],
      Ur_combined = Ur_vals[a],
      Ur_percent  = Ur_pct_vals[a],
      stringsAsFactors = FALSE
    ))
  }
}
rownames(results) <- NULL

# ---- Build Report Text -------------------------------------------------------

report_lines <- character(0)

add_line <- function(...) {
  report_lines <<- c(report_lines, sprintf(...))
}

add_blank <- function() {
  report_lines <<- c(report_lines, "")
}

# -- Section A: Solution Preparation Hierarchy (Flowchart) ---------------------

add_line("==============================================================================")
add_line("  SOLUTION PREPARATION MEASUREMENT UNCERTAINTY REPORT")
add_line("==============================================================================")
add_blank()
add_line("Input file: %s", input_file)
add_line("Equipment uncertainties file: %s", equipment_file)
add_line("Date: %s", Sys.time())
add_line("Expansion coverage factor (k): %g", coverage_final)
add_blank()
add_line("Solution hierarchy diagram saved to: %s",
         paste0(output_prefix, "_hierarchy.png"))
add_blank()

# Build a Graphviz DOT flowchart using DiagrammeR
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}
if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
  install.packages("DiagrammeRsvg")
}
if (!requireNamespace("rsvg", quietly = TRUE)) {
  install.packages("rsvg")
}

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

get_children <- function(sol) {
  solution_info$solution[!is.na(solution_info$parent) & solution_info$parent == sol]
}

# Build node definitions
dot_nodes <- character(0)
dot_edges <- character(0)

for (sol in topo_order) {
  sol_rows <- df[df$solution == sol, ]
  equip_rows <- sol_rows[sol_rows$type == "equipment", ]
  std_rows   <- sol_rows[sol_rows$type == "standard", ]
  parent <- solution_info$parent[solution_info$solution == sol]

  # Build label with solution name, standards, equipment, and ur result
  label_parts <- paste0("<b>", sol, "</b>")

  if (nrow(std_rows) > 0) {
    std_lines <- sprintf("%s (%s ug/mL)", std_rows$label, std_rows$volume_or_concentration)
    label_parts <- c(label_parts, "",
                     "<i>Standards:</i>",
                     std_lines)
  }

  if (nrow(equip_rows) > 0) {
    equip_lines <- sprintf("%s (%s uL)", equip_rows$label, equip_rows$volume_or_concentration)
    label_parts <- c(label_parts, "",
                     "<i>Equipment:</i>",
                     equip_lines)
  }

  # Add U' results as expanded relative uncertainty percentages
  Ur_pct_vals <- Ur_pct[[sol]]
  analytes <- solution_analytes[[sol]]
  Ur_lines <- sprintf("U'(%s) = %s%%", analytes,
                       formatC(Ur_pct_vals[analytes], format = "f", digits = 2))
  label_parts <- c(label_parts, "", Ur_lines)

  # Join with HTML line breaks
  node_label <- paste(label_parts, collapse = "<br/>")

  # Node colour: root = light blue, children = light green
  fill_colour <- if (is.na(parent)) "#D6EAF8" else "#D5F5E3"
  border_colour <- if (is.na(parent)) "#2E86C1" else "#28B463"

  # Sanitise solution name into a valid DOT node ID
  node_id <- gsub("[^A-Za-z0-9]", "_", sol)

  dot_nodes <- c(dot_nodes, sprintf(
    '  %s [label=<%s>, fillcolor="%s", color="%s"]',
    node_id, node_label, fill_colour, border_colour
  ))

  # Edge from parent
  if (!is.na(parent)) {
    parent_id <- gsub("[^A-Za-z0-9]", "_", parent)
    dot_edges <- c(dot_edges, sprintf("  %s -> %s", parent_id, node_id))
  }
}

dot_code <- paste(c(
  "digraph solution_hierarchy {",
  "  graph [rankdir=TB, fontname=Helvetica, nodesep=0.4, ranksep=0.6]",
  "  node [shape=box, style=\"filled,rounded\", fontname=Helvetica, fontsize=10, margin=\"0.2,0.15\"]",
  "  edge [color=\"#555555\", penwidth=1.5]",
  "",
  dot_nodes,
  "",
  dot_edges,
  "}"
), collapse = "\n")

# Render and save as PNG
hierarchy_graph <- grViz(dot_code)
hierarchy_svg <- export_svg(hierarchy_graph)
hierarchy_png <- paste0(output_prefix, "_hierarchy.png")
rsvg_png(charToRaw(hierarchy_svg), file = hierarchy_png, width = 1400)
cat(sprintf("Hierarchy diagram saved to: %s\n", hierarchy_png))

# -- Section B: Uncertainty Formulas -------------------------------------------

add_line("==============================================================================")
add_line("  SECTION B: Uncertainty Formulas")
add_line("==============================================================================")
add_blank()

fmt_ur <- function(x) formatC(x, format = "f", digits = 6)
fmt_u  <- function(x) formatC(x, format = "f", digits = 4)
fmt_val <- function(x) formatC(x, format = "f", digits = 4)
fmt_pct <- function(x) formatC(x, format = "f", digits = 2)

for (sol in topo_order) {
  parent <- solution_info$parent[solution_info$solution == sol]
  sol_rows <- df[df$solution == sol, ]
  equip_rows <- sol_rows[sol_rows$type == "equipment", ]
  std_rows   <- sol_rows[sol_rows$type == "standard", ]
  analytes <- solution_analytes[[sol]]

  if (is.na(parent)) {
    add_line("%s", sol)
  } else {
    add_line("%s  (from %s)", sol, parent)
  }
  add_line(paste(rep("-", 78), collapse = ""))
  add_blank()

  # Row-level calculations
  add_line("  Component uncertainties:")
  add_blank()

  for (i in seq_len(nrow(sol_rows))) {
    row <- sol_rows[i, ]
    if (row$type == "equipment") {
      add_line("    u(%s) = tolerance / coverage = %s / %s = %s",
               row$label, fmt_val(row$tolerance_or_error), fmt_val(row$coverage), fmt_u(row$u))
      add_line("    ur(%s) = u / volume = %s / %s = %s",
               row$label, fmt_u(row$u), fmt_val(row$volume_or_concentration), fmt_ur(row$ur))
    } else {
      add_line("    u(%s) = error / coverage = %s / %s = %s",
               row$label, fmt_val(row$tolerance_or_error), fmt_val(row$coverage), fmt_u(row$u))
      add_line("    ur(%s) = u / concentration = %s / %s = %s",
               row$label, fmt_u(row$u), fmt_val(row$volume_or_concentration), fmt_ur(row$ur))
    }
    add_blank()
  }

  # Combined formulas per analyte
  add_line("  Combined uncertainties:")
  add_blank()

  for (analyte in analytes) {
    # Build the formula terms
    terms_labels <- character(0)
    terms_values <- character(0)
    terms_numeric <- numeric(0)

    if (is.na(parent)) {
      # Standard term
      std_ur_val <- std_rows$ur[std_rows$name == analyte]
      std_label  <- std_rows$label[std_rows$name == analyte]
      terms_labels  <- c(terms_labels, sprintf("ur(%s)", std_label))
      terms_values  <- c(terms_values, fmt_ur(std_ur_val))
      terms_numeric <- c(terms_numeric, std_ur_val)
    } else {
      # Parent term
      parent_ur_val <- ur_combined[[parent]][analyte]
      terms_labels  <- c(terms_labels, sprintf("ur(%s, %s)", parent, analyte))
      terms_values  <- c(terms_values, fmt_ur(parent_ur_val))
      terms_numeric <- c(terms_numeric, parent_ur_val)
    }

    # Equipment terms
    for (i in seq_len(nrow(equip_rows))) {
      terms_labels  <- c(terms_labels, sprintf("ur(%s)", equip_rows$label[i]))
      terms_values  <- c(terms_values, fmt_ur(equip_rows$ur[i]))
      terms_numeric <- c(terms_numeric, equip_rows$ur[i])
    }

    # Formula line
    formula_str <- paste(sprintf("%s^2", terms_labels), collapse = " + ")
    values_str  <- paste(sprintf("%s^2", terms_values), collapse = " + ")
    result_val  <- ur_combined[[sol]][analyte]

    add_line("    ur(%s, %s) = sqrt( %s )", sol, analyte, formula_str)
    add_line("    %s= sqrt( %s )",
             strrep(" ", nchar(sprintf("ur(%s, %s) ", sol, analyte))),
             values_str)
    add_line("    %s= %s",
             strrep(" ", nchar(sprintf("ur(%s, %s) ", sol, analyte))),
             fmt_ur(result_val))
    add_line("    U'(%s, %s) = k * ur = %s * %s = %s  (%s%%)",
             sol, analyte,
             fmt_val(coverage_final), fmt_ur(result_val),
             fmt_ur(Ur_expanded[[sol]][analyte]),
             fmt_pct(Ur_pct[[sol]][analyte]))
    add_blank()
  }

  add_blank()
}

# -- Section C: Summary Table --------------------------------------------------

add_line("==============================================================================")
add_line("  SECTION C: Summary Table")
add_line("==============================================================================")
add_blank()

# Determine column widths
sol_width <- max(nchar("Solution"), nchar(results$solution))
ana_width <- max(nchar("Analyte"), nchar(results$analyte))
ur_width  <- 12
Ur_width  <- 12
pct_width <- 8

header <- sprintf("  %-*s  %-*s  %*s  %*s  %*s", sol_width, "Solution",
                  ana_width, "Analyte", ur_width, "ur_combined",
                  Ur_width, "Ur_combined", pct_width, "Ur_%")
add_line("%s", header)
add_line("  %s", paste(rep("-", nchar(header) - 2), collapse = ""))

for (i in seq_len(nrow(results))) {
  add_line("  %-*s  %-*s  %*s  %*s  %*s",
           sol_width, results$solution[i],
           ana_width, results$analyte[i],
           ur_width, fmt_ur(results$ur_combined[i]),
           Ur_width, fmt_ur(results$Ur_combined[i]),
           pct_width, sprintf("%s%%", fmt_pct(results$Ur_percent[i])))
}

add_blank()
add_line("==============================================================================")

# ---- Write Outputs -----------------------------------------------------------

# Console summary
cat("Results Summary:\n")
cat(sprintf("  %-30s  %-10s  %12s  %12s  %8s\n", "Solution", "Analyte", "ur_combined", "Ur_combined", "Ur_%"))
cat(sprintf("  %s\n", paste(rep("-", 78), collapse = "")))
for (i in seq_len(nrow(results))) {
  cat(sprintf("  %-30s  %-10s  %12s  %12s  %8s\n",
              results$solution[i], results$analyte[i],
              fmt_ur(results$ur_combined[i]),
              fmt_ur(results$Ur_combined[i]),
              sprintf("%s%%", fmt_pct(results$Ur_percent[i]))))
}
cat("\n")

# Write report
writeLines(report_lines, output_report)
cat(sprintf("Report written to: %s\n", output_report))

# Write CSV
write.csv(results, output_csv, row.names = FALSE)
cat(sprintf("Results written to: %s\n", output_csv))

cat("\nDone.\n")
