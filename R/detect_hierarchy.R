#' Detect a standard or grouped forecasting hierarchy
#'
#' Selects an approach compatible with FinnTS hierarchy construction without
#' changing input data, running preprocessing, or saving artifacts.
#'
#' @param input_data A local data frame containing the combo columns. Repeated
#'   observations are allowed. Collect Spark data frames before calling.
#' @param combo_variables Nonempty character vector of unique combo column names.
#' @param target_variable Target column name, required only when
#'   `combo_cleanup_date` is supplied. Must contain numeric values; missing
#'   targets are allowed but infinite values are not.
#' @param combo_cleanup_date Optional `Date` scalar. When supplied, apply the
#'   same inactive-series cleanup as [prep_data()]: retain series whose target
#'   sum between this date and `hist_end_date`, inclusive, is nonzero.
#' @param hist_end_date `Date` scalar required for cleanup. Future target values
#'   do not contribute to the cleanup sum.
#'
#' @details Character and factor combo boundaries are trimmed using the same
#'   normalization as [prep_data()]. The caller's data is unchanged. Missing,
#'   blank, nonfinite, or unsupported combo labels, ambiguous `"--"`-joined
#'   combo identities, invalid cleanup inputs, and an empty retained population
#'   produce errors.
#'
#'   Levels are ordered by increasing distinct count, with supplied column order
#'   breaking ties, matching the engine. A standard hierarchy requires every
#'   child label to determine exactly one parent at each adjacent level and the
#'   finest level to identify every bottom-level tuple. Crossed dimensions or
#'   reused child labels select a grouped hierarchy. One retained series selects
#'   `"bottoms_up"` because the HTS constructors require multivariate input.
#'
#'   Detection describes observed relationships, not unobserved business
#'   relationships. Use the same input population and cleanup settings that
#'   will be passed to [prep_data()]. This function does not override explicit
#'   preprocessing choices. Agent setup shares the structural analysis but
#'   preserves its single-column bottoms-up policy and saved-version contracts.
#'
#' @return A list with `forecast_approach` (`"standard_hierarchy"`,
#'   `"grouped_hierarchy"`, or `"bottoms_up"`), `hierarchy_order`, named distinct
#'   `counts`, retained bottom-series count `total_ts`, a `conflicts` data frame
#'   (`parent`, `child`, `child_label`, `parent_count`), and a readable `reason`.
#'   Diagnostics are returned in memory only.
#' @export
#' @examples
#' data <- data.frame(
#'   Region = c("North", "North", "South"),
#'   Site = c("A", "B", "C")
#' )
#' decision <- detect_hierarchy(data, c("Region", "Site"))
#' decision$forecast_approach
#'
#' data$Date <- as.Date("2026-08-01")
#' data$Revenue <- c(10, 0, 20)
#' detect_hierarchy(
#'   data, c("Region", "Site"), target_variable = "Revenue",
#'   combo_cleanup_date = as.Date("2025-08-01"),
#'   hist_end_date = as.Date("2026-08-01")
#' )
detect_hierarchy <- function(input_data,
                             combo_variables,
                             target_variable = NULL,
                             combo_cleanup_date = NULL,
                             hist_end_date = NULL) {
  if (!is.data.frame(input_data) || anyDuplicated(names(input_data))) {
    stop("Hierarchy detection requires a local data frame with unique column names.", call. = FALSE)
  }
  if (!is.character(combo_variables) || length(combo_variables) == 0L ||
    anyNA(combo_variables) || any(!nzchar(trimws(combo_variables))) ||
    anyDuplicated(combo_variables)) {
    stop("Supply a nonempty, unique vector of combo column names.", call. = FALSE)
  }
  missing <- setdiff(combo_variables, names(input_data))
  if (length(missing)) {
    stop("Missing combo columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if ("Date" %in% combo_variables) {
    stop("'Date' cannot be a combo variable; it is reserved for the time stamp.", call. = FALSE)
  }
  for (variable in combo_variables) {
    values <- input_data[[variable]]
    if (!(is.character(values) || is.factor(values) || is.numeric(values) ||
      is.logical(values)) || !is.null(dim(values)) ||
      (is.object(values) && !is.factor(values))) {
      stop("Unsupported combo column type: ", variable, call. = FALSE)
    }
    if (anyNA(values) || any(!nzchar(stringr::str_trim(as.character(values)))) ||
      (is.numeric(values) && any(!is.finite(values)))) {
      stop("Missing, blank, or nonfinite combo labels in: ", variable, call. = FALSE)
    }
  }
  input_data <- normalize_combo_values(input_data, combo_variables)
  combos <- dplyr::distinct(input_data[, combo_variables, drop = FALSE])
  combo_ids <- do.call(paste, c(combos, sep = "--"))
  if (anyDuplicated(combo_ids)) {
    stop("Different combo tuples produce the same '--'-joined engine identity.", call. = FALSE)
  }

  if (!is.null(combo_cleanup_date)) {
    if (!inherits(combo_cleanup_date, "Date") || length(combo_cleanup_date) != 1L ||
      is.na(combo_cleanup_date) || !is.finite(as.numeric(combo_cleanup_date)) ||
      !inherits(hist_end_date, "Date") || length(hist_end_date) != 1L ||
      is.na(hist_end_date) || !is.finite(as.numeric(hist_end_date)) ||
      combo_cleanup_date > hist_end_date) {
      stop("Cleanup requires finite Date scalars with combo_cleanup_date <= hist_end_date.", call. = FALSE)
    }
    if (!is.character(target_variable) || length(target_variable) != 1L ||
      is.na(target_variable) || !target_variable %in% names(input_data) ||
      target_variable %in% c(combo_variables, "Date")) {
      stop("Cleanup requires a target_variable naming a non-combo, non-Date column.", call. = FALSE)
    }
    target <- input_data[[target_variable]]
    if (!is.numeric(target) || !is.null(dim(target)) || any(is.infinite(target))) {
      stop("Cleanup target values must be numeric and cannot be infinite.", call. = FALSE)
    }
    if (!inherits(input_data$Date, "Date") || anyNA(input_data$Date) ||
      any(!is.finite(as.numeric(input_data$Date)))) {
      stop("Cleanup requires a 'Date' column containing finite, nonmissing Date values.", call. = FALSE)
    }
    cleanup_data <- data.frame(
      Combo = do.call(paste, c(input_data[, combo_variables, drop = FALSE], sep = "--")),
      Date = input_data$Date, Target = target
    )
    retained <- combo_cleanup_fn(cleanup_data, combo_cleanup_date, hist_end_date)
    combos <- combos[combo_ids %in% retained$Combo, , drop = FALSE]
  }
  analyze_hierarchy(combos, combo_variables)
}

# Analyze local combo tuples without normalization, cleanup, or artifact I/O.
# Callers own validation and population selection. Return detector diagnostics;
# optionally append all ordered pair tests in the legacy Agent expand.grid order.
# Missing legacy labels remain labels; empty populations fail explicitly.
analyze_hierarchy <- function(input_data, combo_variables, include_pair_tests = FALSE) {
  combos <- dplyr::distinct(input_data[, combo_variables, drop = FALSE])
  if (!nrow(combos)) {
    stop("No retained time series remain for hierarchy detection.", call. = FALSE)
  }
  counts <- vapply(combos, dplyr::n_distinct, integer(1))
  # Stable ties must match standard-node construction and preprocessing order.
  hierarchy_order <- combo_variables[order(counts, seq_along(counts))]
  conflicts <- data.frame(
    parent = character(), child = character(), child_label = character(),
    parent_count = integer(), stringsAsFactors = FALSE
  )
  for (i in seq_len(length(hierarchy_order) - 1L)) {
    parent <- hierarchy_order[i]
    child <- hierarchy_order[i + 1L]
    pairs <- dplyr::distinct(combos[, c(parent, child), drop = FALSE])
    child_labels <- unique(pairs[[child]])
    parent_counts <- tabulate(match(pairs[[child]], child_labels), nbins = length(child_labels))
    bad <- parent_counts > 1L
    if (any(bad)) {
      conflicts <- rbind(conflicts, data.frame(
        parent = parent, child = child, child_label = as.character(child_labels[bad]),
        parent_count = parent_counts[bad], stringsAsFactors = FALSE
      ))
    }
  }
  standard <- !nrow(conflicts) && counts[[hierarchy_order[length(hierarchy_order)]]] == nrow(combos)
  if (nrow(combos) == 1L) {
    approach <- "bottoms_up"
    reason <- "Only one retained series; no aggregation is needed."
  } else if (standard) {
    approach <- "standard_hierarchy"
    reason <- "Every child has one parent in engine order."
  } else {
    approach <- "grouped_hierarchy"
    reason <- "Hierarchy columns are crossed or child labels are reused across parents."
  }
  result <- list(
    forecast_approach = approach, hierarchy_order = hierarchy_order,
    counts = counts, total_ts = nrow(combos), conflicts = conflicts, reason = reason
  )
  if (include_pair_tests) {
    pairs <- expand.grid(from = combo_variables, to = combo_variables,
      stringsAsFactors = FALSE)
    pairs <- pairs[pairs$from != pairs$to, , drop = FALSE]
    # Classify child-to-parent uniqueness without changing legacy diagnostic names.
    tests <- vapply(seq_len(nrow(pairs)), function(i) {
      labels <- dplyr::distinct(combos[, c(pairs$from[i], pairs$to[i]), drop = FALSE])
      if (anyDuplicated(labels[[pairs$to[i]]])) "many-to-many" else "one-to-many"
    }, character(1))
    result$pair_tests <- stats::setNames(tests, paste0(pairs$from, "->", pairs$to))
  }
  result
}
