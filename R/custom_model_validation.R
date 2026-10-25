# Normalize explicit formula-history counts and observed cadence lags without inferring the business rule.
# Zero formula-history need is allowed, but workflow history has a separate positive minimum.
# Return canonical integral counts or a bounded field error; never invent history or alter accepted examples.
custom_author_history_requirements <- function(requirements) {
  fields <- c("minimum_rows_per_series", "required_lag_periods", if ("workflow_minimum_rows_per_series" %in% names(requirements)) "workflow_minimum_rows_per_series")
  tryCatch(custom_author_fields(requirements, fields, "contract"), error = function(error) custom_author_abort("contract",
    "history_requirements must contain minimum_rows_per_series and required_lag_periods.",
    code = "invalid_history_requirements",
    field = "history_requirements"
  ))
  minimum <- requirements$minimum_rows_per_series
  if (!is.numeric(minimum) || length(minimum) != 1L || !is.finite(minimum) || minimum < (0) || minimum > 2000 || minimum !=
    floor(minimum)) {
    custom_author_abort("contract", "minimum_rows_per_series must be an integer from one through 2000.",
      code = "invalid_history_requirements",
      field = "history_requirements.minimum_rows_per_series"
    )
  }
  lags <- requirements$required_lag_periods
  if (is.list(lags) && !length(lags))
    lags <- integer()
  if (is.list(lags) && all(vapply(lags, function(value) is.numeric(value) && length(value) == 1L, logical(1))))
    lags <- unlist(lags, use.names = FALSE)
  if (!is.numeric(lags) || length(lags) > 2000L || any(!is.finite(lags)) || anyDuplicated(lags) || any(lags < 1 | lags >
    2000 | lags != floor(lags))) {
    custom_author_abort("contract", "required_lag_periods must be unique positive integer cadence offsets through 2000, or an empty array.",
      code = "invalid_history_requirements", field = "history_requirements.required_lag_periods"
    )
  }
  result <- list(minimum_rows_per_series = as.integer(minimum), required_lag_periods = sort(as.integer(lags)))
  {
    result$workflow_minimum_rows_per_series <- as.integer(max(1, minimum, lags))
    if (!is.null(requirements$workflow_minimum_rows_per_series) && !identical(
      requirements$workflow_minimum_rows_per_series,
      result$workflow_minimum_rows_per_series
    )) {
      custom_author_abort("contract", "Workflow history count changed.", code = "invalid_history_requirements", field = "history_requirements")
    }
  }
  result
}

# Derive bounded structural repair facts from a rejected history declaration.
# Generated examples use scaffold counts; optional caller examples and metadata
# use their already provider-visible numerical cases. Error cases may deliberately
# violate history requirements and are excluded from caller count feedback.
# No observed training values, candidate execution or oracle changes occur.
# Missing/invalid declarations or unsupported protocols yield no optional facts.
# Lag references remain forecast-date-relative. Impossible offsets receive a
# package-owned explanation distinguishing fit-window counts from future lookups;
# the helper never decides which interpretation the business rule intended.
custom_author_history_feedback <- function(proposal, protocol, examples = NULL, metadata = NULL) {
  if ((is.null(protocol$scaffold) && is.null(examples)))
    return(NULL)
  tryCatch(
    {
      requirements <- custom_author_history_requirements(proposal$history_requirements)
      records <- list()
      if (!is.null(examples)) {
        for (example in examples) {
          if (!is.null(example$expected_error))
            next
          history <- custom_author_table(example$history)
          for (combo in unique(history$Combo)) {
            records[[length(records) + 1L]] <- list(scenario = example$name, combo = combo, history_rows = as.integer(sum(history$Combo ==
              combo)))
          }
        }
        horizon <- as.integer(metadata$forecast_horizon)
      } else {
        for (scenario in protocol$scaffold$scenarios) {
          matching <- Filter(function(example) is.list(example) && identical(example$id, scenario$id), proposal$examples)
          for (combo in scenario$combos) {
            series <- if (length(matching) == 1L)
              Filter(function(entry) is.list(entry) && identical(entry$combo, combo), matching[[1L]]$series)
            else list()
            records[[length(records) + 1L]] <- list(scenario = scenario$id, combo = combo, history_rows = if (length(series) ==
              1L) length(series[[1L]]$history$Target) else 0L)
          }
        }
        horizon <- as.integer(protocol$scaffold$forecast_horizon)
      }
      if (length(horizon) != 1L || is.na(horizon) || horizon < 1L)
        return(NULL)
      unobserved <- requirements$required_lag_periods[requirements$required_lag_periods < horizon]
      list(
        minimum_history_rows = as.integer(max(c(
          requirements$workflow_minimum_rows_per_series, requirements$minimum_rows_per_series,
          requirements$required_lag_periods
        ))), required_lag_periods = requirements$required_lag_periods, forecast_horizon = horizon,
        observed_history_possible = !length(unobserved), examples = records, lag_reference = "forecast_date", unobserved_lag_periods = unobserved,
        guidance = paste(
          if (length(unobserved)) "Adding older history cannot satisfy declared lags that reach the forecast window." else "Supply enough chronological history for the declared counts and observed forecast-relative lookups.",
          if (!is.null(examples)) "Caller examples and expected outcomes are fixed. Correct only an unaccepted history declaration that misstates the rule; never lower genuine history needs merely to pass. Counts exclude intentional error cases.",
          "For a statistic calculated once from the last N actual observations at fit time, use minimum_rows_per_series = N and required_lag_periods = [].",
          "Use nonempty required_lag_periods only for observed lookups relative to each forecast date. Correct only unaccepted declarations that misrepresent the rule; never change fixed answers."
        )
      )
    },
    error = function(error) NULL
  )
}

# Check fixed example tables against the explicit history declaration before a
# new contract is accepted. Every forecast/series lookup must be observed. This
# inspects dates/counts only, never fills data, changes expected answers, runs a
# candidate or proves that the declaration matches arbitrary source semantics.
# New expected-error fixtures can intentionally violate formula-history needs;
# they retain nonempty keyed tables and are validated by exact error behavior.
custom_author_check_history <- function(examples, requirements, date_type) {
  requirements <- custom_author_history_requirements(requirements)
  if (!custom_author_text(date_type) || !date_type %in% c("day", "week", "month", "quarter", "year")) {
    custom_author_abort("contract", "History requirements need a supported captured cadence.",
      code = "invalid_history_requirements",
      field = "history_requirements"
    )
  }
  for (example in examples) {
    if (!is.null(example$expected_error))
      next
    for (combo in unique(example$new_data$Combo)) {
      history_dates <- example$history$Date[example$history$Combo == combo]
      minimum <- requirements$workflow_minimum_rows_per_series %||% requirements$minimum_rows_per_series
      if (length(history_dates) < minimum) {
        custom_author_abort("examples", paste(
          "Example history has", length(history_dates), "rows; at least", minimum,
          "are required per series."
        ), code = "insufficient_example_history", field = "examples.history")
      }
      lags <- requirements$required_lag_periods
      if (length(lags)) {
        forecast_dates <- example$new_data$Date[example$new_data$Combo == combo]
        for (index in seq_along(forecast_dates)) {
          required <- seq(forecast_dates[index], by = paste("-1", date_type), length.out = max(lags) + 1L)[lags +
            1L]
          if (any(!required %in% history_dates)) {
            custom_author_abort("examples", "Example history does not contain every declared lag date for every forecast row.",
              code = "insufficient_example_history", field = "examples.history"
            )
          }
        }
      }
    }
  }
  invisible(NULL)
}

# Define synthetic grids independently of candidate source or business data.
# The package owns cadence, cutoff, scenario identity, mode coverage and row keys;
# providers supply bounded values and arithmetic only. No random draws or I/O.
custom_author_example_scaffold <- function(metadata, model_type, predictors = character()) {
  if (!custom_author_text(metadata$date_type) || !metadata$date_type %in% c("day", "week", "month", "quarter", "year") ||
    !is.numeric(metadata$forecast_horizon) || length(metadata$forecast_horizon) != 1L || !is.finite(metadata$forecast_horizon) ||
    metadata$forecast_horizon < 1 || metadata$forecast_horizon != floor(metadata$forecast_horizon) || metadata$forecast_horizon >
    2000 || !is.character(model_type) || !length(model_type) || anyNA(model_type) || anyDuplicated(model_type) || any(!model_type %in%
    c("local", "global")) || !is.character(predictors) || anyNA(predictors) || anyDuplicated(predictors)) {
    custom_author_abort("examples", "Invalid example scaffold metadata.")
  }
  modes <- intersect(c("local", "global"), model_type)
  scenarios <- unlist(lapply(modes, function(mode) lapply(c(FALSE, TRUE), function(edge) {
    list(id = paste0(mode, if (edge) "_edge" else "_basic"), model_type = mode, edge_case = edge, combos = if (mode ==
      "local") "example_a" else c("example_a", "example_b"))
  })), recursive = FALSE)
  list(
    date_type = metadata$date_type, forecast_horizon = as.integer(metadata$forecast_horizon), cutoff = switch(metadata$date_type,
      day = "2020-02-28",
      week = "2020-12-28",
      month = "2020-12-01",
      quarter = "2020-10-01",
      year = "2020-01-01"
    ), predictors = unname(predictors),
    scenarios = unname(scenarios)
  )
}

# Materialize current typed value arrays onto fixed synthetic date/series grids without executing candidate source.
# Require complete scenario/series coverage, nonempty keyed data, declared predictor types and exact horizon lengths.
# Normalize numeric columns to double; preserve independent expected values and exact declared error outcomes.
# Return current example records or bounded field errors. No recycling, imputation or oracle generation occurs.
custom_author_scaffold_examples <- function(values, scaffold, model_type, predictors, predictor_types = NULL,
                                            historical_only_predictors = character()) {
  reject <- function(field, reason) custom_author_abort("examples", reason, code = "invalid_examples_shape", field = field)
  fields <- function(value, required, field) {
    tryCatch(custom_author_fields(value, required, "examples"), error = function(error) {
      reject(field, paste(field, "must contain exactly its required fields."))
    })
  }
  custom_author_fields(
    scaffold, c("date_type", "forecast_horizon", "cutoff", "predictors", "scenarios"),
    "examples"
  )
  available_modes <- unique(vapply(scaffold$scenarios, `[[`, character(1), "model_type"))
  if (!identical(scaffold, custom_author_example_scaffold(scaffold, available_modes, scaffold$predictors)) || !all(model_type %in%
    available_modes) || !identical(predictors, scaffold$predictors)) {
    custom_author_abort("examples", "Frozen example scaffold changed.")
  }
  scenarios <- Filter(function(scenario) scenario$model_type %in% model_type, scaffold$scenarios)
  ids <- vapply(scenarios, `[[`, character(1), "id")
  if (!is.list(values) || !is.null(names(values)) || (FALSE) || ((length(values) < length(scenarios) || length(values) >
    8L)))
    reject("examples", "Invalid example scenario coverage.")
  supplied <- vapply(values, function(value) {
    fields(value, c("id", "series", "rationale", c("outcome", "error_phase", "error_code")), "examples")
    if ((!custom_author_text(value$outcome) || !value$outcome %in% c("predictions", "error") || !is.character(value$error_phase) ||
      length(value$error_phase) != 1L || !is.character(value$error_code) || length(value$error_code) != 1L || (value$outcome ==
      "predictions" && (!identical(value$error_phase, "none") || !identical(value$error_code, ""))) || (value$outcome ==
      "error" && (!value$error_phase %in% c("fit", "predict") || !grepl("^[a-z][a-z0-9_]{0,79}$", value$error_code)))))
      reject("examples.outcome", "Invalid declared example outcome.")
    if (!custom_author_text(value$id) || !custom_author_text(value$rationale))
      reject("examples.id", "Invalid example scenario.")
    value$id
  }, character(1))
  {
    extras <- setdiff(supplied, ids)
    allowed <- unlist(lapply(model_type, function(mode) paste0(mode, "_error_", seq_len(6L))), use.names = FALSE)
    if (!all(ids %in% supplied) || !all(extras %in% allowed))
      reject("examples.id", "Invalid example scenario coverage.")
    for (id in extras) {
      mode <- sub("_error_[1-6]$", "", id)
      entry <- values[[match(id, supplied)]]
      if (!identical(entry$outcome, "error"))
        reject("examples.outcome", "Additional scenarios must declare errors.")
      scenarios[[length(scenarios) + 1L]] <- list(id = id, model_type = mode, edge_case = TRUE, combos = if (mode ==
        "local") "example_a" else c("example_a", "example_b"))
    }
    ids <- vapply(scenarios, `[[`, character(1), "id")
  }
  if (anyDuplicated(supplied) || !setequal(supplied, ids))
    reject("examples.id", "Invalid example scenario coverage.")
  values <- values[match(ids, supplied)]
  array <- function(value, numeric = FALSE, field = "examples") {
    if (is.list(value) && length(value) && all(vapply(
      value, function(entry) is.atomic(entry) && length(entry) == 1L,
      logical(1)
    ))) {
      value <- unlist(value, use.names = FALSE)
    }
    if (!is.atomic(value) || is.object(value) || !is.null(dim(value)) || !length(value) || length(value) > 2000L || anyNA(value) ||
      (numeric && !is.numeric(value)) || (is.numeric(value) && any(!is.finite(value)))) {
      reject(field, "Invalid finite example value array or length.")
    }
    custom_run_passive(value)
    unname(value)
  }
  cutoff <- as.Date(scaffold$cutoff)
  future_dates <- seq(cutoff, by = scaffold$date_type, length.out = scaffold$forecast_horizon + 1L)[-1L]
  result <- lapply(seq_along(scenarios), function(index) {
    scenario <- scenarios[[index]]
    value <- values[[index]]
    error_case <- identical(value$outcome, "error")
    if (error_case && !scenario$edge_case)
      reject("examples.outcome", "Basic scenarios must supply numerical controls.")
    if (!is.list(value$series) || !is.null(names(value$series)) || length(value$series) != length(scenario$combos)) {
      reject("examples.series", "examples.series must be an array covering the scenario series.")
    }
    combos <- vapply(value$series, function(series) {
      fields(series, c("combo", "history", "future", "expected"), "examples.series")
      if (!custom_author_text(series$combo))
        reject("examples.series", "Invalid scenario series identity.")
      series$combo
    }, character(1))
    if (anyDuplicated(combos) || !setequal(combos, scenario$combos))
      reject("examples.series", "Invalid scenario series coverage.")
    tables <- lapply(value$series[match(scenario$combos, combos)], function(series) {
      fields(series$history, c("Target", predictors), "examples.history")
      future_predictors <- setdiff(predictors, historical_only_predictors)
      if (!is.list(series$future) || any(!names(series$future) %in% predictors))
        reject("examples.future", "Unknown future driver field.")
      fields(series$future[intersect(names(series$future), future_predictors)], future_predictors, "examples.future")
      target <- array(series$history$Target, TRUE, "examples.history")
      expected <- if (error_case) {
        if (length(series$expected))
          reject("examples.expected", "Expected errors must not contain numerical predictions.")
        numeric()
      } else array(series$expected, TRUE, "examples.expected")
      {
        target <- as.numeric(target)
        expected <- as.numeric(expected)
      }
      if (!error_case && length(expected) != length(future_dates))
        reject("examples.expected", "Expected array length must match the horizon.")
      history <- data.frame(
        Date = rev(seq(cutoff, by = paste("-1", scaffold$date_type), length.out = length(target))),
        Combo = series$combo, Target = target, stringsAsFactors = FALSE
      )
      request <- data.frame(Date = future_dates, Combo = series$combo, stringsAsFactors = FALSE)
      for (predictor in predictors) {
        observed <- array(series$history[[predictor]], field = "examples.history")
        future <- if (predictor %in% future_predictors)
          array(series$future[[predictor]], field = "examples.future") else NULL
        if (length(observed) != length(target))
          reject("examples.history", "Predictor array length must match its history grid.")
        if (predictor %in% future_predictors && length(future) != length(future_dates))
          reject("examples.future", "Predictor array length must match its future grid.")
        if (!is.null(predictor_types)) {
          matches_type <- switch(predictor_types[[predictor]],
            number = is.numeric,
            boolean = is.logical,
            string = is.character
          )
          if (is.null(matches_type) || !matches_type(observed))
            reject("examples.history", "Predictor values must match their captured type.")
          if (predictor %in% future_predictors && !matches_type(future))
            reject("examples.future", "Predictor values must match their captured type.")
        }
        if (is.numeric(observed) && is.numeric(future)) {
          observed <- as.numeric(observed)
          future <- as.numeric(future)
        }
        history[[predictor]] <- observed
        if (predictor %in% future_predictors) request[[predictor]] <- future
      }
      list(history = history, new_data = request, expected = if (error_case) NULL else data.frame(
        Date = future_dates,
        Combo = series$combo, .pred = expected
      ))
    })
    combined <- lapply(c("history", "new_data", "expected"), function(field) do.call(rbind, lapply(tables, `[[`, field)))
    if (any(vapply(combined, function(table) if (is.null(table)) 0L else nrow(table), integer(1)) > 2000L)) {
      reject("examples", "Example table length exceeds 2000 rows.")
    }
    result <- list(
      name = scenario$id, model_type = scenario$model_type, history = combined[[1L]], new_data = combined[[2L]],
      expected = combined[[3L]], tolerance = 1e-08, edge_case = scenario$edge_case, rationale = value$rationale
    )
    {
      result$outcome <- value$outcome
      result["expected_error"] <- list(if (error_case) list(phase = value$error_phase, code = value$error_code) else NULL)
    }
    result
  })
  unname(result)
}

#' Template for Custom Model Validation Examples
#'
#' Return a small example table in the format [create_custom_model()] expects
#' for `validation_examples`. Each `example` value groups one worked case:
#' history rows carry observed target values, future rows leave the target
#' missing and carry the independently calculated `expected` forecast. Replace
#' the illustrative values with cases calculated from your own business rule.
#'
#' @param forecast_horizon Number of future rows per example. Must match the
#'   forecast horizon used by [create_custom_model()].
#' @param date_type Date cadence: `"year"`, `"quarter"`, `"month"`, `"week"`
#'   or `"day"`. Must match the cadence used by [create_custom_model()].
#' @return A data frame with columns `example`, `Date`, `Target`, `expected`
#'   and `how`. The template holds one normal example (`last_value`) and one
#'   edge case (`edge_zero_history`) for an illustrative "repeat the last
#'   observed value" rule.
#' @details The table may also contain:
#'   \itemize{
#'     \item Series columns named like the forecast's `combo_variables`. Omit
#'       them for one-series examples. An example with two or more series
#'       validates global logic; one series validates local logic.
#'     \item Driver columns named like `external_regressors`, filled on every
#'       row, including future rows.
#'     \item `edge_case` (logical). Example names starting with `edge_` are
#'       edge cases automatically.
#'     \item `expected_error`, written as `"fit:code"` or `"predict:code"`, for
#'       an edge case whose model must stop with that error code. Leave
#'       `expected` missing on its future rows.
#'     \item `how`, a short description of how the expected values were
#'       calculated, and `tolerance`, an absolute tolerance up to 0.01.
#'   }
#'   The target column may be named `Target` or like the forecast's
#'   `target_variable`. Supply two to eight examples including at least one
#'   edge case. Examples are visible to the LLM; use nonconfidential values.
#' @seealso [create_custom_model()]
#' @examples
#' custom_model_examples_template()
#' custom_model_examples_template(forecast_horizon = 2, date_type = "quarter")
#' @export
custom_model_examples_template <- function(forecast_horizon = 3, date_type = "month") {
  if (!is.numeric(forecast_horizon) || length(forecast_horizon) != 1L || !is.finite(forecast_horizon) ||
    forecast_horizon < 1 || forecast_horizon != floor(forecast_horizon) || forecast_horizon > 24) {
    stop("forecast_horizon must be a whole number from 1 through 24.", call. = FALSE)
  }
  if (!custom_author_text(date_type) || !date_type %in% c("year", "quarter", "month", "week", "day")) {
    stop("date_type must be year, quarter, month, week or day.", call. = FALSE)
  }
  dates <- seq(as.Date("2024-01-01"), by = date_type, length.out = 3L + forecast_horizon)
  future <- rep(NA_real_, forecast_horizon)
  case <- function(name, history, how) {
    data.frame(
      example = name, Date = dates, Target = c(history, future),
      expected = c(rep(NA_real_, 3L), rep(history[[3L]], forecast_horizon)), how = how,
      stringsAsFactors = FALSE
    )
  }
  rbind(
    case("last_value", c(100, 110, 120), "Repeat the last observed value."),
    case("edge_zero_history", c(0, 0, 0), "All-zero history repeats zero.")
  )
}

# Convert the caller's validation-example table into the internal example list.
# The table holds one row per example, series and date: history rows carry the
# target and leave `expected` missing; future rows leave the target missing and
# carry `expected` (or nothing for an `expected_error` edge case). Series columns
# use the resolved combo_variables (absent means one series); drivers use the
# resolved external_regressors. Local/global is inferred from the series count
# and must match the effective model_type exactly. Errors name the example,
# table row and date. Set-level and horizon checks remain in custom_author_examples().
custom_examples_from_table <- function(table, target_variable, combo_variables, external_regressors, model_type,
                                       historical_only_predictors = character(), defer_future = FALSE) {
  if (!is.data.frame(table) || !nrow(table)) {
    custom_author_abort("examples", "validation_examples must be a data frame; start from custom_model_examples_template().")
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  rownames(table) <- NULL
  columns <- names(table)
  target <- if (custom_author_text(target_variable) && target_variable %in% columns) target_variable else "Target"
  required <- c("example", "Date", target, "expected")
  missing_columns <- setdiff(required, columns)
  if (length(missing_columns)) {
    custom_author_abort("examples", paste0(
      "validation_examples is missing column(s) ", paste(missing_columns, collapse = ", "),
      "; required columns are example, Date, ", target, " and expected."
    ))
  }
  combo_variables <- setdiff(as.character(combo_variables), c(".finnts_single_series", "Date", target))
  series <- intersect(combo_variables, columns)
  if (length(series) && length(series) != length(combo_variables)) {
    custom_author_abort("examples", paste0(
      "validation_examples must contain all series columns (", paste(combo_variables, collapse = ", "),
      ") or none of them."
    ))
  }
  drivers <- as.character(external_regressors)
  optional <- c("edge_case", "expected_error", "how", "tolerance")
  unknown <- setdiff(columns, c(required, series, drivers, optional))
  if (length(unknown)) {
    custom_author_abort("examples", paste0(
      "validation_examples has unexpected column(s) ", paste(unknown, collapse = ", "),
      "; use only example, Date, ", target, ", expected, series columns, driver columns, edge_case, expected_error, how and tolerance."
    ))
  }
  where <- function(row) {
    paste0("example '", table$example[[row]], "', row ", row, ", date ", format(table$Date[[row]]))
  }
  if (anyNA(table$example) || !all(vapply(as.character(table$example), custom_author_text, logical(1)))) {
    custom_author_abort("examples", "Every row needs a nonblank example name.")
  }
  table$example <- as.character(table$example)
  if (is.character(table$Date)) {
    if (anyNA(table$Date) || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", table$Date))) {
      custom_author_abort("examples", "Date values must be Date objects or ISO dates (YYYY-MM-DD).")
    }
    table$Date <- as.Date(table$Date)
  }
  if (!inherits(table$Date, "Date") || anyNA(table$Date)) {
    custom_author_abort("examples", "Date values must be Date objects or ISO dates (YYYY-MM-DD).")
  }
  for (column in c(target, "expected")) {
    if (!is.numeric(table[[column]]) && !all(is.na(table[[column]]))) {
      custom_author_abort("examples", paste0("Column ", column, " must be numeric."))
    }
    table[[column]] <- as.numeric(table[[column]])
  }
  observed <- !is.na(table[[target]])
  answered <- !is.na(table$expected)
  for (row in which(observed & answered)) {
    custom_author_abort("examples", paste0(
      where(row), ": a row is either history (", target, " filled, expected missing) or future (",
      target, " missing, expected filled), not both."
    ))
  }
  for (row in which(observed & !is.finite(table[[target]]) | answered & !is.finite(table$expected))) {
    custom_author_abort("examples", paste0(where(row), ": values must be finite numbers."))
  }
  if (length(series)) {
    if (anyNA(table[series])) {
      custom_author_abort("examples", "Series columns must not contain missing values.")
    }
    normalized <- normalize_combo_values(table, series)
    combo <- tidyr::unite(normalized, ".example_combo", tidyselect::all_of(series), sep = "--", remove = FALSE)$.example_combo
  } else {
    combo <- rep("example", nrow(table))
  }
  duplicated_rows <- which(duplicated(data.frame(table$example, combo, table$Date)))
  for (row in duplicated_rows) {
    custom_author_abort("examples", paste0(where(row), ": duplicate example, series and date."))
  }
  setting <- function(rows, column) {
    if (!column %in% columns) {
      return(NULL)
    }
    values <- unique(table[[column]][rows][!is.na(table[[column]][rows])])
    if (length(values) > 1L) {
      custom_author_abort("examples", paste0(
        "example '", table$example[[rows[[1L]]]], "': ", column, " must have one value for the whole example."
      ))
    }
    if (length(values)) values[[1L]] else NULL
  }
  names_in_order <- unique(table$example)
  examples <- lapply(names_in_order, function(name) {
    rows <- which(table$example == name)
    error_text <- setting(rows, "expected_error")
    expected_error <- NULL
    if (!is.null(error_text)) {
      parts <- strsplit(as.character(error_text), ":", fixed = TRUE)[[1L]]
      if (length(parts) != 2L || !trimws(parts[[1L]]) %in% c("fit", "predict") ||
        !grepl("^[a-z][a-z0-9_]{0,79}$", trimws(parts[[2L]]))) {
        custom_author_abort("examples", paste0(
          "example '", name, "': expected_error must look like 'fit:code' or 'predict:code' with a lowercase snake_case code."
        ))
      }
      expected_error <- list(phase = trimws(parts[[1L]]), code = trimws(parts[[2L]]))
    }
    edge_value <- setting(rows, "edge_case")
    if (!is.null(edge_value) && !is.logical(edge_value)) {
      custom_author_abort("examples", paste0("example '", name, "': edge_case must be TRUE or FALSE."))
    }
    edge_case <- isTRUE(edge_value) || startsWith(name, "edge_") || !is.null(expected_error)
    if (!is.null(expected_error) && identical(edge_value, FALSE)) {
      custom_author_abort("examples", paste0("example '", name, "': an expected_error example is always an edge case."))
    }
    history_rows <- rows[observed[rows]]
    future_rows <- rows[!observed[rows]]
    if (!length(history_rows) || !length(future_rows)) {
      custom_author_abort("examples", paste0(
        "example '", name, "' needs history rows (", target, " filled) and future rows (", target, " missing)."
      ))
    }
    if (is.null(expected_error)) {
      for (row in future_rows[!answered[future_rows]]) {
        custom_author_abort("examples", paste0(
          where(row), ": future rows need an expected value (or set expected_error for an error edge case)."
        ))
      }
    } else {
      for (row in future_rows[answered[future_rows]]) {
        custom_author_abort("examples", paste0(where(row), ": expected_error examples leave expected missing."))
      }
    }
    for (key in unique(combo[rows])) {
      last_history <- max(table$Date[history_rows][combo[history_rows] == key], as.Date(-Inf))
      early <- future_rows[combo[future_rows] == key & table$Date[future_rows] <= last_history]
      if (length(early) || !any(combo[history_rows] == key) || !any(combo[future_rows] == key)) {
        row <- c(early, rows[combo[rows] == key])[[1L]]
        custom_author_abort("examples", paste0(
          where(row), ": every series needs history rows followed by later future rows."
        ))
      }
    }
    type <- if (length(unique(combo[rows])) > 1L) "global" else "local"
    for (driver in drivers) {
      check_rows <- if (defer_future || driver %in% historical_only_predictors) history_rows else rows
      if (!driver %in% columns || anyNA(table[[driver]][check_rows])) {
        custom_author_abort("examples", paste0(
          "example '", name, "': driver column ", driver, " must be filled on every row, including future rows."
        ))
      }
      if (defer_future && !driver %in% historical_only_predictors) {
        future_values <- table[[driver]][future_rows]
        if ((is.numeric(future_values) && any(!is.na(future_values) & !is.finite(future_values))) ||
          (anyNA(future_values) && !all(is.na(future_values)))) {
          custom_author_abort("examples", paste0(
            "example '", name, "': driver column ", driver, " must be filled on every row, including future rows."
          ))
        }
      }
    }
    keep <- c(drivers)
    history <- data.frame(Date = table$Date[history_rows], Combo = combo[history_rows], Target = table[[target]][history_rows], stringsAsFactors = FALSE)
    history[keep] <- table[history_rows, keep, drop = FALSE]
    new_data <- data.frame(Date = table$Date[future_rows], Combo = combo[future_rows], stringsAsFactors = FALSE)
    future_keep <- setdiff(keep, historical_only_predictors)
    new_data[future_keep] <- table[future_rows, future_keep, drop = FALSE]
    if (defer_future) {
      for (driver in future_keep) {
        if (anyNA(new_data[[driver]]) || (is.numeric(new_data[[driver]]) && any(!is.finite(new_data[[driver]]))))
          new_data[[driver]] <- NULL
      }
    }
    history <- custom_author_table(history)
    new_data <- custom_author_table(new_data)
    expected <- NULL
    if (is.null(expected_error)) {
      expected <- data.frame(Date = table$Date[future_rows], Combo = combo[future_rows], .pred = table$expected[future_rows], stringsAsFactors = FALSE)
      expected <- custom_author_table(expected)[c("Date", "Combo", ".pred")]
      rownames(expected) <- NULL
    }
    rownames(history) <- NULL
    rownames(new_data) <- NULL
    tolerance <- setting(rows, "tolerance") %||% 1e-08
    if (!is.numeric(tolerance) || !is.finite(tolerance) || tolerance < 0 || tolerance > 0.01) {
      custom_author_abort("examples", paste0("example '", name, "': tolerance must be a number from 0 through 0.01."))
    }
    rationale <- setting(rows, "how")
    if (is.null(rationale)) {
      rationale <- if (is.null(expected_error)) "Caller-supplied independent expected values." else "Caller-supplied expected business error."
    }
    if (!custom_author_text(as.character(rationale))) {
      custom_author_abort("examples", paste0("example '", name, "': how must be nonblank text."))
    }
    list(
      name = name, model_type = type, history = history, new_data = new_data, expected = expected,
      tolerance = as.numeric(tolerance), edge_case = edge_case, rationale = as.character(rationale),
      outcome = if (is.null(expected_error)) "predictions" else "error", expected_error = expected_error
    )
  })
  if (length(examples) < 2L || length(examples) > 8L) {
    custom_author_abort("examples", "validation_examples needs two to eight examples, including at least one edge case.")
  }
  if (!any(vapply(examples, function(example) example$edge_case, logical(1)))) {
    custom_author_abort("examples", "validation_examples needs at least one edge case: name it with an 'edge_' prefix or set edge_case = TRUE.")
  }
  types <- unique(vapply(examples, function(example) example$model_type, character(1)))
  declared <- model_type %||% "local"
  if (length(setdiff(types, declared))) {
    custom_author_abort("examples", paste0(
      "validation_examples include ", paste(setdiff(types, declared), collapse = " and "),
      " examples (", if ("global" %in% setdiff(types, declared)) "two or more series" else "one series",
      ") but model_type is ", paste(declared, collapse = "/"), "; add it to model_type or change the examples."
    ))
  }
  if (length(setdiff(declared, types))) {
    custom_author_abort("examples", paste0(
      "model_type includes ", paste(setdiff(declared, types), collapse = " and "), " but no example covers it; ",
      "local examples use one series and global examples use two or more series."
    ))
  }
  unname(examples)
}

# Convert a reviewed example table from plain data.frame or JSON row records.
# Dates are strict ISO dates, identities are character, numeric values finite.
# Optional exclude removes declared historical-only columns before missingness checks.
# No code or arbitrary classes are accepted; returns a stable local data.frame.
custom_author_table <- function(value, exclude = NULL) {
  if (is.list(value) && !is.data.frame(value)) {
    custom_run_passive(value)
    value <- dplyr::bind_rows(value)
  }
  if (!is.data.frame(value) || !nrow(value) || nrow(value) > 2000L || anyDuplicated(names(value)) || !all(c("Date", "Combo") %in%
    names(value))) {
    custom_author_abort("examples", "Expected a nonempty Date/Combo table with unique columns (at most 2000 rows).")
  }
  value <- as.data.frame(value, stringsAsFactors = FALSE)
  if (length(exclude)) {
    value <- value[setdiff(names(value), exclude)]
  }
  if (is.character(value$Date)) {
    if (anyNA(value$Date) || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value$Date)))
      custom_author_abort("examples", "Dates must be ISO dates.")
    value$Date <- as.Date(value$Date)
  }
  if (!inherits(value$Date, "Date") || !is.character(value$Combo) || anyNA(value) || any(!nzchar(value$Combo)) || anyDuplicated(value[c(
    "Combo",
    "Date"
  )])) {
    custom_author_abort("examples", "Example identities must be unique and nonmissing.")
  }
  for (column in value) {
    if (inherits(column, "Date")) {
      if (any(!is.finite(as.numeric(column))))
        custom_author_abort("examples", "Invalid dates.")
    } else {
      custom_run_passive(column)
      if (!is.atomic(column))
        custom_author_abort("examples", "Example columns must be atomic.")
    }
  }
  value[order(value$Combo, value$Date, method = "radix"), , drop = FALSE]
}

# Validate and canonicalize caller or generated example tables without changing their numerical answers or tolerances.
# Require declared mode coverage, complete horizons, finite typed inputs and a numerical control per mode.
# Error cases retain exact phase/code declarations and no numerical output. Return independent current-format examples.
custom_author_examples <- function(examples, definition, metadata) {
  if (!is.list(examples) || length(examples) < 2L || length(examples) > 8L)
    custom_author_abort("examples", "Provide two to eight worked examples including an edge case.")
  result <- lapply(seq_along(examples), function(example_index) {
    example <- examples[[example_index]]
    if (!"outcome" %in% names(example)) {
      example$outcome <- "predictions"
      example["expected_error"] <- list(NULL)
    }
    fields <- c("name", "model_type", "history", "new_data", "expected", "tolerance", "edge_case", "rationale", c(
      "outcome",
      "expected_error"
    ))
    if (!is.list(example) || length(example) != length(fields) || !setequal(names(example), fields) || anyDuplicated(names(example)) ||
      !custom_author_text(example$name) || !custom_author_text(example$rationale) || !custom_author_text(example$model_type) ||
      !is.logical(example$edge_case) || length(example$edge_case) != 1L || is.na(example$edge_case) || !is.numeric(example$tolerance) ||
      length(example$tolerance) != 1L || !is.finite(example$tolerance) || example$tolerance < 0 || example$tolerance >
      0.01) {
      custom_author_abort("examples", "Invalid worked-example metadata or tolerance (0 through 0.01).")
    }
    if (!example$model_type %in% definition$model_type) {
      custom_author_abort("examples", "Worked-example model_type must belong to the declared model modes.", code = "example_mode")
    }
    example$history <- custom_author_table(example$history)
    example$new_data <- custom_author_table(
      example$new_data,
      definition$requirements$historical_only_predictors
    )
    error_case <- identical(example$outcome, "error")
    if ((!custom_author_text(example$outcome) || !example$outcome %in% c("predictions", "error") || (!error_case && !is.null(example$expected_error))))
      custom_author_error_contract("invalid_error_declaration", field = "examples.outcome",
        model_type = example$model_type, example_index = as.integer(example_index), stage = "examples")
    if (error_case) {
      error <- example$expected_error
      if (!is.null(example$expected) || !isTRUE(example$edge_case) || !is.list(error) || length(error) != 2L || !setequal(
        names(error),
        c("phase", "code")
      ) || !custom_author_text(error$phase) || !error$phase %in% c("fit", "predict") || !custom_author_text(error$code) ||
        !grepl("^[a-z][a-z0-9_]{0,79}$", error$code)) {
        custom_author_error_contract("invalid_error_declaration", field = "examples.expected_error",
          model_type = example$model_type, example_index = as.integer(example_index), stage = "examples")
      }
      example$expected_error <- error[c("phase", "code")]
    } else example$expected <- custom_author_table(example$expected)
    {
      for (column in names(example$history)) {
        if (is.numeric(example$history[[column]]) && !inherits(example$history[[column]], "Date")) {
          example$history[[column]] <- as.numeric(example$history[[column]])
          if (column %in% names(example$new_data) && is.numeric(example$new_data[[column]])) {
            example$new_data[[column]] <- as.numeric(example$new_data[[column]])
          }
        }
      }
    }
    predictors <- custom_run_predictors(definition, example$history)
    future_predictors <- custom_model_phase_predictors(definition, "predict")
    if (!is.numeric(example$history$Target) || any(!is.finite(example$history$Target)) || "Target" %in% names(example$new_data) ||
      !all(future_predictors %in% names(example$new_data)) || (!error_case && (!setequal(names(example$expected), c(
      "Combo",
      "Date", ".pred"
    )) || !is.numeric(example$expected$.pred) || !identical(
      example$expected[c("Combo", "Date")],
      example$new_data[c("Combo", "Date")]
    )))) {
      custom_author_abort("examples", "History, request and expected prediction columns must match the declared rule.")
    }
    combos <- unique(example$history$Combo)
    if ((example$model_type == "local" && length(combos) != 1L) || (example$model_type == "global" && length(combos) <
      2L) || !setequal(combos, unique(example$new_data$Combo)))
      custom_author_abort("examples", "Worked examples do not cover the declared series mode.")
    custom_model_data(example$new_data, definition, list(model_type = example$model_type), "predict")
    for (combo in combos) {
      cutoff <- max(example$history$Date[example$history$Combo == combo])
      expected_dates <- seq(cutoff, by = metadata$date_type, length.out = metadata$forecast_horizon + 1L)[-1L]
      if (!identical(example$new_data$Date[example$new_data$Combo == combo], expected_dates)) {
        custom_author_abort("examples", "Worked example must cover the full declared horizon after its history.",
          code = "example_horizon"
        )
      }
    }
    example
  })
  if (anyDuplicated(vapply(result, function(example) example$name, character(1))) || !setequal(vapply(
    result, function(example) example$model_type,
    character(1)
  ), definition$model_type) || !any(vapply(result, function(example) example$edge_case, logical(1)))) {
    custom_author_abort("examples", "Examples need unique names, every declared mode and an edge case.")
  }
  unname(result)
}

# Validate the complete raw panel before sampling, padding or provider requests.
# Bounds are inclusive period labels. An omitted end is inferred only when every
# series has the same last finite target; this cannot certify business close status.
# Every series must cover the common historical window and declared drivers must
# cover the next horizon. Returns resolved dates/source and internal combo labels;
# inferred bounds retain the input Date representation. Cadence comparisons use
# exact day values, independent of integer versus double Date storage.
# values are not filled, forecasts/vintages are not inferred, and no I/O occurs.
custom_author_preflight <- function(
    input_data, combo_variables, target_variable, date_type, forecast_horizon, external_regressors,
    hist_start_date, hist_end_date, defer_future = FALSE) {
  if (anyNA(input_data[c(combo_variables, "Date")]) || any(!is.finite(as.numeric(input_data$Date)))) {
    custom_author_abort("input", "Series identifiers and dates must be complete and finite.")
  }
  combos <- tidyr::unite(input_data, ".author_combo", tidyselect::all_of(combo_variables), sep = "--", remove = FALSE)$.author_combo
  rows <- split(seq_len(nrow(input_data)), combos)
  cutoff_source <- if (is.null(hist_end_date))
    "inferred"
  else "explicit"
  target <- input_data[[target_variable]]
  if (is.null(hist_end_date)) {
    last_actual <- vapply(rows, function(index) {
      observed <- input_data$Date[index[is.finite(target[index])]]
      if (!length(observed))
        custom_author_abort("input", "Every series requires observed historical targets; future-only series are unsupported.")
      as.numeric(max(observed))
    }, numeric(1))
    if (length(unique(last_actual)) != 1L) {
      custom_author_abort("input", "Cannot infer a shared hist_end_date from uneven series histories; supply a common completed historical window.")
    }
    hist_end_date <- input_data$Date[[match(last_actual[[1L]], as.numeric(input_data$Date))]]
  }
  if (is.null(hist_start_date))
    hist_start_date <- min(input_data$Date)
  if (!inherits(hist_start_date, "Date") || !inherits(hist_end_date, "Date") || length(hist_start_date) != 1L || length(hist_end_date) !=
    1L || any(!is.finite(as.numeric(c(hist_start_date, hist_end_date)))) || hist_start_date > hist_end_date) {
    custom_author_abort("input", "Invalid historical bounds.")
  }
  historical <- input_data$Date >= hist_start_date & input_data$Date <= hist_end_date
  dates <- sort(unique(input_data$Date[historical]))
  if (!length(dates) || dates[[1L]] != hist_start_date || tail(dates, 1L) != hist_end_date || !identical(as.numeric(dates), as.numeric(seq(hist_start_date,
    by = date_type, length.out = length(dates)
  )))) {
    custom_author_abort("input", "Historical dates must cover an uninterrupted cadence aligned with hist_start_date and hist_end_date.")
  }
  future_dates <- seq(hist_end_date, by = date_type, length.out = forecast_horizon + 1L)[-1L]
  future_valid <- stats::setNames(rep(TRUE, length(external_regressors)), external_regressors)
  for (index in rows) {
    history_rows <- index[historical[index]]
    if (!identical(sort(input_data$Date[history_rows]), dates)) {
      custom_author_abort("input", "Every series must cover the complete historical window; choose common bounds instead of padding missing periods.")
    }
    if (any(!is.finite(target[history_rows]))) {
      custom_author_abort("input", "Historical target values must be observed and finite through hist_end_date; missing actuals cannot become zeros.")
    }
    future_rows <- index[input_data$Date[index] > hist_end_date & input_data$Date[index] <= max(future_dates)]
    complete <- identical(as.numeric(sort(input_data$Date[future_rows])), as.numeric(future_dates))
    if (!defer_future && length(external_regressors) && !complete) {
      custom_author_abort("input", "Future drivers must cover every forecast date for every series after hist_end_date.")
    }
    for (predictor in external_regressors) {
      values <- input_data[[predictor]]
      if (!is.null(dim(values)) || !(is.numeric(values) || is.logical(values) || is.character(values) || is.factor(values))) {
        custom_author_abort("input", "Declared drivers must have supported scalar numeric, logical or categorical values.")
      }
      observed <- values[history_rows]
      if (anyNA(observed) || (is.numeric(observed) && any(!is.finite(observed)))) {
        custom_author_abort("input", "Authoring requires complete finite observed driver history.")
      }
      future <- values[future_rows]
      valid <- complete && !anyNA(future) && !(is.numeric(future) && any(!is.finite(future)))
      future_valid[[predictor]] <- future_valid[[predictor]] && valid
      if (!defer_future && !valid) {
        custom_author_abort("input", "Future driver values must be complete and finite for the entire forecast horizon.")
      }
    }
  }
  list(
    hist_start_date = hist_start_date, hist_end_date = hist_end_date, cutoff_source = cutoff_source, combos = combos,
    series_checked = length(rows), future_valid = future_valid
  )
}

# Resolve only unambiguous raw-input metadata without provider calls or I/O.
# Single-series omission requires unique dates and adds a private constant key to
# a copy and no varying undeclared columns that could identify multiple series.
# Cadence inference requires at least three historical dates in every series,
# matching exactly one shared complete grid. Explicit values retain normal
# validation; target, horizon, drivers and completed-business cutoff are not guessed.
custom_author_input_metadata <- function(
    input_data, combo_variables, target_variable, date_type, hist_start_date, hist_end_date,
    external_regressors = NULL) {
  if (!is.data.frame(input_data) || !nrow(input_data) || !"Date" %in% names(input_data) || !inherits(input_data$Date, "Date") ||
    anyNA(input_data$Date) || any(!is.finite(as.numeric(input_data$Date)))) {
    custom_author_abort("input", "Raw input requires a valid Date column.")
  }
  if (is.null(combo_variables)) {
    if (anyDuplicated(input_data$Date))
      custom_author_abort("input", "Duplicate dates require explicit combo_variables; series grouping is never guessed.")
    undeclared <- setdiff(names(input_data), c("Date", target_variable, external_regressors))
    if (any(vapply(input_data[undeclared], function(column) length(unique(column)) > 1L, logical(1)))) {
      custom_author_abort("input", "Varying undeclared columns require explicit combo_variables or external_regressors; series grouping is ambiguous.")
    }
    key <- ".finnts_single_series"
    while (key %in% names(input_data)) key <- paste0(key, "_")
    input_data <- as.data.frame(input_data)
    input_data[[key]] <- "series"
    combo_variables <- key
  }
  if (is.null(date_type)) {
    if (!custom_author_text(target_variable) || !target_variable %in% names(input_data) || !is.numeric(input_data[[target_variable]])) {
      custom_author_abort("input", "Raw input requires an explicit numeric target_variable.")
    }
    if (!is.character(combo_variables) || !length(combo_variables) || anyNA(combo_variables) || anyDuplicated(combo_variables) ||
      !all(combo_variables %in% names(input_data)))
      custom_author_abort("input", "Invalid combo_variables for cadence inference.")
    for (bound in list(hist_start_date, hist_end_date)) {
      if (!is.null(bound) && (!inherits(bound, "Date") || length(bound) != 1L || is.na(bound) || !is.finite(as.numeric(bound)))) {
        custom_author_abort("input", "Historical bounds must be finite scalar Date values.")
      }
    }
    groups <- dplyr::group_indices(dplyr::group_by(input_data, dplyr::across(dplyr::all_of(combo_variables))))
    grids <- lapply(split(seq_len(nrow(input_data)), groups), function(rows) {
      dates <- sort(unique(input_data$Date[rows][is.finite(input_data[[target_variable]][rows])]))
      if (!is.null(hist_start_date))
        dates <- dates[dates >= hist_start_date]
      if (!is.null(hist_end_date))
        dates <- dates[dates <= hist_end_date]
      dates
    })
    candidates <- Filter(function(cadence) {
      all(vapply(grids, function(dates) {
        if (length(dates) < 3L) return(FALSE)
        expected <- tryCatch(seq(dates[1L], by = cadence, length.out = length(dates)), error = function(error) NULL)
        identical(dates, expected)
      }, logical(1)))
    }, c("day", "week", "month", "quarter", "year"))
    if (length(candidates) != 1L)
      custom_author_abort("input", "Cannot infer an unambiguous complete cadence; supply date_type explicitly.")
    date_type <- candidates[[1L]]
  }
  list(input_data = input_data, combo_variables = combo_variables, date_type = date_type)
}

# Summarize one series' observed history for representative global sampling.
# Takes the series label and its historical targets (every row counts toward
# the budget; non-finite values are ignored in statistics); returns a one-row
# data frame of row count, mean absolute scale, zero share, negative flag and
# standard deviation. Summaries stay local and never enter provider metadata.
custom_author_series_summary <- function(label, target) {
  rows <- length(target)
  target <- target[is.finite(target)]
  if (!length(target)) target <- 0
  data.frame(
    label = label, key = hash_data(label), rows = rows, scale = mean(abs(target)),
    zero_share = mean(target == 0), negative = any(target < 0),
    spread = if (length(target) > 1L) stats::sd(target) else 0, stringsAsFactors = FALSE
  )
}

# Choose a deterministic representative global authoring sample under a total
# historical row budget and a series budget. `summary` has one row per series
# from custom_author_series_summary(); `levels` optionally holds aligned combo
# variable values (used only when there are two or more variables, since a
# single variable's levels are the series themselves). `budget` caps total
# history rows; `series_budget` caps sampled series so authoring preparation,
# whose cost grows roughly linearly with series count, stays near 30-45 seconds.
# Edge series are mandatory: largest/smallest scale, most zeros, any negatives,
# flattest and shortest, ties broken by hash (at most six, below any valid
# series budget). Level coverage is then best-effort in hash order, and the rest
# of both budgets fills in hash order. Mandatory rows above the row budget abort
# with counts. Returns selected labels in hash order plus count-only disclosure.
custom_author_global_sample <- function(summary, levels = NULL, budget = 10000L, series_budget = 100L) {
  order_by <- function(value) order(value, summary$key, method = "radix")
  edges <- list(
    largest_scale = order_by(-summary$scale)[1L], smallest_scale = order_by(summary$scale)[1L],
    most_zeros = if (any(summary$zero_share > 0)) order_by(-summary$zero_share)[1L],
    negative = if (any(summary$negative)) order_by(!summary$negative)[1L],
    flattest = order_by(summary$spread)[1L], shortest = order_by(summary$rows)[1L]
  )
  edges <- edges[lengths(edges) > 0L]
  chosen <- unique(unlist(edges, use.names = FALSE))
  used <- sum(summary$rows[chosen])
  if (used > budget) {
    custom_author_abort("input", sprintf(
      "Representative global edge series need %d historical rows, above the %d-row authoring budget; shorten history with hist_start_date.",
      as.integer(used), as.integer(budget)
    ))
  }
  hash_order <- order(summary$key, method = "radix")
  level_series <- integer()
  uncovered <- 0L
  if (is.data.frame(levels) && ncol(levels) > 1L) {
    seen <- lapply(levels, function(values) unique(values[chosen]))
    for (index in setdiff(hash_order, chosen)) {
      new_level <- any(vapply(names(levels), function(column) !levels[[column]][[index]] %in% seen[[column]], logical(1)))
      if (new_level && length(chosen) < series_budget && used + summary$rows[[index]] <= budget) {
        chosen <- c(chosen, index)
        level_series <- c(level_series, index)
        used <- used + summary$rows[[index]]
        for (column in names(levels)) seen[[column]] <- union(seen[[column]], levels[[column]][[index]])
      }
    }
    uncovered <- sum(vapply(names(levels), function(column) length(setdiff(unique(levels[[column]]), seen[[column]])), integer(1)))
  }
  fill <- integer()
  for (index in setdiff(hash_order, chosen)) {
    if (length(chosen) + length(fill) >= series_budget) break
    if (used + summary$rows[[index]] <= budget) {
      fill <- c(fill, index)
      used <- used + summary$rows[[index]]
    }
  }
  chosen <- c(chosen, fill)
  list(
    selected = summary$label[chosen[order(summary$key[chosen], method = "radix")]],
    disclosure = list(
      strategy = "representative_hash", row_budget = as.integer(budget), series_budget = as.integer(series_budget), edge_series = length(unique(unlist(edges))),
      edge_reasons = names(edges), level_series = length(level_series), uncovered_levels = as.integer(uncovered),
      hash_fill_series = length(fill)
    )
  )
}

# Read or prepare one bounded representative sample without changing caller
# objects, options, RNG or an existing run. Unknown series are discovered once;
# exact selected C1 artifacts are reused across all authoring/repair attempts.
# Raw preparation writes only a unique temporary root. Returns local rows plus
# provider-safe schema/cadence/count metadata; no business values enter metadata.
# Effective driver names and historical bounds come from arguments/already-read
# run metadata, never additional discovery, and support automatic policy evidence.
# Available packages must resolve to real metadata, excluding check-only dummies.
# Explicit hierarchy inputs retain every derived node of the raw leaf sample,
# or every node of an existing prepared hierarchy, under the same total row cap.
# Raw driver columns retain their observed values/types outside generic feature
# engineering, then join historical rows by Combo/Date. Missing driver history
# errors rather than being imputed; future driver/target rows never enter this join.
# Full raw-panel preflight precedes sampling and temporary preparation; resolved
# cutoffs are validation metadata, never fixed constraints on later model fits.
# Prepared driver panels are checked one artifact at a time before retaining a
# three-series sample. Reuse those reads;
# prepared inputs cannot establish whether prior preparation filled raw gaps.
# Omitted raw grouping/cadence uses deterministic metadata resolution. Prepared
# forecast approach may be inherited by reusing its exact log read once.
# Explicit global calls choose a deterministic representative sample under the
# same 10000-history-row budget and at most 100 series: edge series (scale, zeros, negatives, flat,
# shortest) are mandatory, multi-variable combo levels are covered when they
# fit, and the rest fills in hash order. Full-panel preflight still covers every
# series; count-only sampling disclosure enters metadata. Local sampling keeps
# the first three series in hash order.
custom_author_data <- function(
    input_data, run_info, combo_variables, target_variable, date_type, forecast_horizon, external_regressors,
    hist_start_date, hist_end_date, forecast_approach = "bottoms_up", global_sample = FALSE, defer_future = FALSE) {
  original_options <- options()
  on.exit(options(original_options), add = TRUE)
  withr::local_preserve_seed()
  prepared_log <- NULL
  if (is.null(forecast_approach)) {
    if (!is.null(input_data))
      forecast_approach <- "bottoms_up"
    else {
      if (!is.list(run_info) || !is.null(run_info$storage_object))
        custom_author_abort("input", "Prepared input requires local/mounted run_info.")
      prepared_log <- read_exact_artifact(run_info, local_artifact_path(run_info, "logs", extension = "csv"))
      forecast_approach <- prepared_log$forecast_approach
      if (!custom_author_text(forecast_approach) || !forecast_approach %in% c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")) {
        custom_author_abort("input", "Prepared forecast_approach is invalid.")
      }
    }
  }
  hierarchical <- forecast_approach != "bottoms_up"
  hierarchy_data <- NULL
  driver_data <- NULL
  prepared_sample <- NULL
  sample_disclosure <- NULL
  future_valid <- NULL
  if (!is.null(input_data)) {
    resolved <- custom_author_input_metadata(
      input_data, combo_variables, target_variable, date_type, hist_start_date,
      hist_end_date, external_regressors
    )
    input_data <- resolved$input_data
    combo_variables <- resolved$combo_variables
    date_type <- resolved$date_type
    if (!is.data.frame(input_data) || !nrow(input_data) || !length(combo_variables) || !custom_author_text(target_variable) ||
      !custom_author_text(date_type) || !date_type %in% c("year", "quarter", "month", "week", "day") || !is.numeric(forecast_horizon) ||
      length(forecast_horizon) != 1L || !is.finite(forecast_horizon) || forecast_horizon < 1 || forecast_horizon !=
      floor(forecast_horizon)) {
      custom_author_abort("input", "Raw input requires a data frame and explicit valid Finn metadata.")
    }
    input_data <- normalize_combo_values(input_data, combo_variables)
    check_input_data(input_data, combo_variables, target_variable, external_regressors, date_type, 1, NULL)
    preflight <- custom_author_preflight(
      input_data, combo_variables, target_variable, date_type, forecast_horizon, external_regressors,
      hist_start_date, hist_end_date, defer_future = defer_future
    )
    future_valid <- preflight$future_valid
    hist_start_date <- preflight$hist_start_date
    hist_end_date <- preflight$hist_end_date
    if (hierarchical) {
      if (length(external_regressors))
        custom_author_abort("input", "Hierarchy authoring does not support external regressors.")
      custom_run_hierarchy_panel(input_data, combo_variables, target_variable, date_type, hist_start_date, hist_end_date)
    }
    combos <- preflight$combos
    labels <- unique(combos)
    keys <- vapply(labels, hash_data, character(1))
    in_history <- input_data$Date >= hist_start_date & input_data$Date <= hist_end_date
    if (global_sample) {
      history_target <- split(input_data[[target_variable]][in_history], factor(combos[in_history], levels = labels))
      summary <- do.call(rbind, Map(custom_author_series_summary, labels, history_target))
      first_rows <- match(labels, combos)
      sample <- custom_author_global_sample(summary, input_data[first_rows, combo_variables, drop = FALSE])
      selected <- sample$selected
      sample_disclosure <- sample$disclosure
    } else {
      selected <- labels[head(order(keys, method = "radix"), 3L)]
    }
    selected_rows <- combos %in% selected & in_history
    sampled <- input_data[selected_rows, , drop = FALSE]
    if (nrow(sampled) > 10000L)
      custom_author_abort("input", "Selected complete histories exceed 10000 rows; supply a smaller subset.")
    if (length(external_regressors)) {
      driver_data <- data.frame(Combo = combos[selected_rows], Date = sampled$Date, stringsAsFactors = FALSE)
      driver_data[external_regressors] <- sampled[external_regressors]
    }
    temporary <- tempfile("finnts-authoring-")
    info <- set_run_info(
      project_name = "custom-authoring", run_name = "sample", path = temporary, data_output = "csv",
      object_output = "rds", add_unique_id = FALSE
    )
    prep_data(info, sampled, combo_variables, target_variable, date_type, forecast_horizon,
      external_regressors = NULL,
      hist_start_date = hist_start_date, hist_end_date = hist_end_date, recipes_to_run = "C1", stationary = FALSE,
      box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE, forecast_approach = forecast_approach
    )
    paths <- local_artifact_path(info, "prep_data", "-C1", vapply(selected, hash_data, character(1)))
    total_series <- length(labels)
    if (hierarchical) {
      log <- read_exact_artifact(info, local_artifact_path(info, "logs", extension = "csv"))
      hierarchy_data <- custom_run_hierarchy_context(info, custom_run_context(info, log, allow_hierarchy = TRUE),
        log,
        return_data = TRUE, max_rows = 10000
      )
      paths <- local_artifact_path(info, "prep_data", "-C1", vapply(
        hierarchy_data$topology$hts_combos, hash_data,
        character(1)
      ))
    }
  } else {
    info <- run_info
    if (!is.list(info) || !is.null(info$storage_object))
      custom_author_abort("input", "Prepared input requires local/mounted run_info.")
    if (hierarchical)
      local_artifact_files(local_artifact_path(info, "logs", extension = "csv"))
    log <- if (is.null(prepared_log))
      read_exact_artifact(info, local_artifact_path(info, "logs", extension = "csv"))
    else prepared_log
    context <- custom_run_context(info, log, allow_hierarchy = hierarchical)
    if (!identical(context$forecast_approach, forecast_approach))
      custom_author_abort("input", "forecast_approach conflicts with the prepared run.")
    actual <- list(
      combo_variables = strsplit(log$combo_variables, "---", fixed = TRUE)[[1]], target_variable = log$target_variable,
      date_type = log$date_type, forecast_horizon = as.numeric(log$forecast_horizon), external_regressors = if (is.na(log$external_regressors)) NULL else strsplit(log$external_regressors,
        "---",
        fixed = TRUE
      )[[1]], hist_start_date = as.Date(log$hist_start_date), hist_end_date = as.Date(log$hist_end_date)
    )
    supplied <- list(
      combo_variables = combo_variables, target_variable = target_variable, date_type = date_type, forecast_horizon = forecast_horizon,
      external_regressors = external_regressors, hist_start_date = hist_start_date, hist_end_date = hist_end_date
    )
    for (field in names(supplied)) {
      if (!is.null(supplied[[field]]) && !isTRUE(all.equal(supplied[[field]], actual[[field]]))) {
        custom_author_abort("input", "Supplied metadata conflicts with the prepared run.")
      }
    }
    date_type <- actual$date_type
    forecast_horizon <- actual$forecast_horizon
    hist_end_date <- actual$hist_end_date
    if (hierarchical) {
      if (length(info$combo))
        custom_author_abort("input", "Hierarchy authoring requires the complete prepared run, not a combo subset.")
      custom_run_hierarchy_controls(context, actual$external_regressors)
      hierarchy_data <- custom_run_hierarchy_context(info, context, log, return_data = TRUE, max_rows = 10000)
      paths <- local_artifact_path(info, "prep_data", "-C1", vapply(
        hierarchy_data$topology$hts_combos, hash_data,
        character(1)
      ))
      total_series <- length(hierarchy_data$topology$original_combos)
    } else {
      if (length(info$combo))
        paths <- local_artifact_path(info, "prep_data", "-C1", info$combo)
      else paths <- sort(as.character(local_artifact_inventory(info, "prep_data", "-*-C1")), method = "radix")
      total_series <- length(paths)
      if (length(actual$external_regressors) || global_sample) {
        prepared_sample <- list()
        summaries <- list()
        sampled_rows <- 0L
        for (index in seq_along(paths)) {
          path <- as.character(paths[[index]])
          panel <- read_exact_artifact(info, path)
          if (!all(c("Date", "Combo", "Target", actual$external_regressors) %in% names(panel))) {
            custom_author_abort("input", "Prepared driver data must retain its declared original columns and complete future values.")
          }
          checked <- custom_author_preflight(
            panel, "Combo", "Target", date_type, forecast_horizon, actual$external_regressors,
            actual$hist_start_date, hist_end_date, defer_future = defer_future
          )
          if (is.null(future_valid)) future_valid <- checked$future_valid else
            future_valid <- future_valid & checked$future_valid
          if (global_sample) {
            prepared_sample[[index]] <- panel
            summaries[[index]] <- custom_author_series_summary(panel$Combo[[1L]], panel$Target[panel$Date <= hist_end_date])
          } else if (index <= 3L) {
            sampled_rows <- sampled_rows + sum(panel$Date <= hist_end_date)
            if (sampled_rows > 10000L)
              custom_author_abort("input", "Selected complete histories exceed 10000 rows; supply a smaller explicit cohort.")
            prepared_sample[[index]] <- panel
          }
        }
        if (global_sample) {
          summary <- do.call(rbind, summaries)
          parts <- strsplit(summary$label, "--", fixed = TRUE)
          levels <- if (length(actual$combo_variables) > 1L && all(lengths(parts) == length(actual$combo_variables))) {
            stats::setNames(as.data.frame(do.call(rbind, parts), stringsAsFactors = FALSE), actual$combo_variables)
          }
          sample <- custom_author_global_sample(summary, levels)
          sample_disclosure <- sample$disclosure
          keep <- match(sample$selected, summary$label)
          prepared_sample <- prepared_sample[keep]
          paths <- paths[keep]
        }
      }
      if (!global_sample)
        paths <- head(paths, 3L)
    }
  }
  if (!length(paths))
    custom_author_abort("data", "No prepared C1 series are available.")
  data <- if (hierarchical)
    hierarchy_data$data
  else if (!is.null(prepared_sample))
    dplyr::bind_rows(prepared_sample)
  else read_exact_artifact(info, paths)
  if (!all(c("Date", "Combo", "Target") %in% names(data)) || !inherits(data$Date, "Date"))
    custom_author_abort("data", "Prepared C1 data has invalid identity columns.")
  data <- as.data.frame(data[data$Date <= hist_end_date, ], stringsAsFactors = FALSE)
  if (nrow(data) > 10000L)
    custom_author_abort("data", "Selected complete histories exceed 10000 rows; supply a smaller subset.")
  if (!nrow(data) || anyNA(data$Target) || any(!is.finite(data$Target)) || anyDuplicated(data[c("Combo", "Date")])) {
    custom_author_abort("data", "Authoring requires finite observed history and unique series/date keys.")
  }
  if (!is.null(driver_data)) {
    data <- dplyr::left_join(dplyr::select(data, -tidyselect::any_of(external_regressors)), driver_data, by = c(
      "Combo",
      "Date"
    ))
    for (predictor in external_regressors) {
      values <- data[[predictor]]
      if (anyNA(values) || (is.numeric(values) && any(!is.finite(values)))) {
        custom_author_abort("data", "Authoring requires complete finite observed driver history.")
      }
    }
  }
  data <- data[order(data$Combo, data$Date, method = "radix"), , drop = FALSE]
  rownames(data) <- NULL
  schema <- lapply(data, function(column) paste(class(column), collapse = "/"))
  declarations <- utils::packageDescription("finnts", fields = c("Depends", "Imports", "Suggests"))
  packages <- trimws(sub("\\s*\\(.*\\)$", "", trimws(unlist(strsplit(paste(unlist(declarations), collapse = ","), ",")))))
  installed <- rownames(utils::installed.packages())
  packages <- intersect(
    unique(c("finnts", packages, rownames(utils::installed.packages(priority = c("base", "recommended"))))),
    installed
  )
  packages <- packages[vapply(packages, function(package) nzchar(system.file("DESCRIPTION", package = package)), logical(1))]
  result <- list(data = data, metadata = list(
    date_type = date_type, forecast_horizon = as.numeric(forecast_horizon), schema = schema,
    sampled_series = length(unique(data$Combo)), total_series = total_series, sampled_rows = nrow(data), date_start = as.character(min(data$Date)),
    date_end = as.character(max(data$Date)), available_packages = sort(packages, method = "radix")
  ))
  if (!is.null(sample_disclosure))
    result$metadata$global_sample <- sample_disclosure
  if (hierarchical) {
    result$metadata$forecast_approach <- forecast_approach
    result$metadata$hierarchy <- list(
      sampled_leaves = length(hierarchy_data$topology$original_combos), node_count = length(hierarchy_data$topology$hts_combos),
      total_leaves = total_series
    )
  }
  result$metadata$input_context <- list(
    external_regressors = if (is.null(input_data)) actual$external_regressors else external_regressors,
    hist_start_date = as.character(if (is.null(input_data)) actual$hist_start_date else hist_start_date), hist_end_date = as.character(hist_end_date)
  )
  result$metadata$cutoff_source <- if (is.null(input_data))
    "prepared_run"
  else preflight$cutoff_source
  # This call-local validation summary is not part of saved metadata or prompts.
  if (defer_future) result$future_valid <- future_valid
  result$metadata$resolved_inputs <- list(
    combo_variables = if (is.null(input_data)) actual$combo_variables else combo_variables,
    target_variable = if (is.null(input_data)) actual$target_variable else target_variable, date_type = date_type, forecast_horizon = as.numeric(forecast_horizon),
    forecast_approach = forecast_approach
  )
  result
}

# Identify advisory LLM-generated numerical expectations from their provenance.
# Caller examples remain mandatory; execution and coded-error checks never weaken.
custom_author_llm_examples_advisory <- function(contract) {
  identical(contract$expectation_provenance, "llm_proposed_unverified")
}

# Execute trusted current candidates against independent examples, chronological holdouts and RDS round-trips.
# Caller numerical expectations and required properties are hard checks; generated numbers and suggested properties are diagnostic.
# Capture only bounded candidate errors, exact rule codes and safe source-referenced identifiers; surrounding I/O failures propagate.
# Return measured current-format evidence. No subprocess, provider request, oracle rewrite or execution timeout is introduced.
custom_author_child <- function(definition, examples, data, contract = NULL) {
  started <- proc.time()[["elapsed"]]
  metadata <- data$metadata
  properties <- c("row_order", contract$validation_properties)
  mandatory <- c("row_order", contract$required_properties)
  diagnostics <- list()
  verified <- stats::setNames(as.list(rep(0L, length(properties))), properties)
  custom_model_current_definition(definition)
  for (package in definition$packages) {
    if (!custom_model_package_available(package))
      return(list(passed = FALSE, code = "missing_dependency"))
  }
  # Catch candidate execution only and return bounded phase-specific failures; surrounding I/O remains outside this handler.
  candidate <- function(expression, phase, mode, example_index) {
    tryCatch(force(expression), error = function(error) {
      message <- conditionMessage(error)
      reason <- if (inherits(error, "finnts_custom_date_error") && is.character(error$code) && length(error$code) ==
        1L && !is.na(error$code) && error$code %in% c("date_key_type", "date_key_duplicate", "date_shift_invalid"))
        error$code
      else if ((inherits(error, "vctrs_error_cast_lossy") || inherits(error, "vctrs_error_incompatible_type")))
        "input_type_mismatch"
      else if (inherits(error, "finnts_custom_rule_error"))
        "rule_error"
      else if (identical(message, "non-numeric argument to binary operator"))
        "non_numeric_operand"
      else if (identical(message, "Custom model prediction must return exact unique row IDs and finite numeric .pred values."))
        "prediction_shape"
      else if (message %in% c("Custom model fitted state contains unsupported nonportable state.", "Custom model fitted state contains a nonportable environment."))
        "nonportable_state"
      else "candidate_error"
      unbound_symbol <- if (identical(reason, "candidate_error"))
        custom_author_unbound_symbol(error, definition$source)
      else NULL
      if (!is.null(unbound_symbol))
        reason <- "unbound_symbol"
      failure <- custom_author_failure(definition, phase, reason, example_index, mode, error, rule_error = if (inherits(
        error,
        "finnts_custom_rule_error"
      ))
        error$code
      else NULL, unbound_symbol = unbound_symbol)
      stop(structure(list(message = failure$message, call = NULL, failure = failure), class = c(
        "finnts_custom_candidate_failure",
        "error", "condition"
      )))
    })
  }
  # Fit and predict one independent case, then verify serialization and its required or diagnostic properties.
  evaluate <- function(history, request, mode, phase, example_index = NULL, check_properties = TRUE, tolerance = 1e-08) {
    workflow <- custom_run_workflows(definition, history, metadata)
    workflow <- workflow$Model_Workflow[[match(mode, workflow$Model_Type)]]
    fitted <- candidate(generics::fit(workflow, history), paste0(phase, "_fit"), mode, example_index)
    predictions <- candidate(stats::predict(fitted, request)$.pred, paste0(phase, "_predict"), mode, example_index)
    path <- tempfile(fileext = ".rds")
    saveRDS(fitted, path)
    reloaded <- readRDS(path)
    restored <- candidate(stats::predict(reloaded, request)$.pred, paste0(phase, "_roundtrip"), mode, example_index)
    if (!identical(predictions, restored)) {
      failure <- custom_author_failure(
        definition, paste0(phase, "_roundtrip"), "serialization_mismatch", example_index,
        mode
      )
      stop(structure(list(message = failure$message, call = NULL, failure = failure), class = c(
        "finnts_custom_candidate_failure",
        "error", "condition"
      )))
    }
    if (check_properties && phase == "example") {
      # Compare a property with numerical tolerance; reject required violations and count suggested violations separately.
      compare <- function(actual, expected, property) {
        allowance <- tolerance + 8 * .Machine$double.eps * pmax(1, abs(expected))
        if (length(actual) != length(expected) || any(!is.finite(actual)) || any(abs(actual - expected) > allowance)) {
          if (!property %in% mandatory) {
            diagnostics[[length(diagnostics) + 1L]] <<- list(
              property = property, example_index = example_index,
              model_type = mode, reason = "property_mismatch"
            )
            return(invisible(FALSE))
          }
          failure <- custom_author_failure(definition, "example_compare", "property_mismatch", example_index, mode,
            property = property
          )
          stop(structure(list(message = paste(failure$message, property), call = NULL, failure = failure), class = c(
            "finnts_custom_candidate_failure",
            "error", "condition"
          )))
        }
        verified[[property]] <<- verified[[property]] + 1L
      }
      order <- rev(seq_len(nrow(request)))
      reordered <- candidate(
        stats::predict(fitted, request[order, , drop = FALSE])$.pred, "example_predict", mode,
        example_index
      )
      compare(reordered, predictions[order], "row_order")
      for (property in setdiff(properties, "row_order")) {
        changed_history <- history
        changed_request <- request
        expected <- predictions
        if (property == "constant_forecast") {
          expected <- predictions[match(request$Combo, request$Combo)]
          compare(predictions, expected, property)
          next
        }
        if (property == "independent_series") {
          selected <- request$Combo == request$Combo[[1]]
          other <- history$Combo != request$Combo[[1]]
          changed_history$Target[other] <- history$Target[other] * 2 + 1
        }
        if (property == "target_scale_equivariant") {
          changed_history$Target <- history$Target * 2
          expected <- predictions * 2
        }
        if (property == "fixed_window") {
          old <- lapply(split(history, history$Combo), function(series) {
            first <- series[which.min(series$Date), , drop = FALSE]
            first$Date <- seq(first$Date, by = paste("-1", metadata$date_type), length.out = 2L)[[2]]
            first$Target <- first$Target * 2 + 1
            first
          })
          changed_history <- rbind(do.call(rbind, old), history)
        }
        if (property == "relative_calendar") {
          # Translate dates by four calendar years for the declared relative-calendar property.
          shift <- function(dates) {
            calendar <- as.POSIXlt(dates)
            calendar$year <- calendar$year + 4L
            as.Date(calendar)
          }
          changed_history$Date <- shift(history$Date)
          changed_request$Date <- shift(request$Date)
        }
        actual <- tryCatch(evaluate(changed_history, changed_request, mode, phase, example_index,
          check_properties = FALSE,
          tolerance = tolerance
        ), finnts_custom_candidate_failure = identity)
        if (inherits(actual, "finnts_custom_candidate_failure")) {
          if (property %in% mandatory || identical(actual$failure$reason, "input_type_mismatch"))
            stop(actual)
          diagnostics[[length(diagnostics) + 1L]] <<- list(
            property = property, example_index = example_index, model_type = mode,
            reason = "evaluation_error"
          )
          next
        }
        if (property == "independent_series")
          compare(actual[selected], expected[selected], property)
        else compare(actual, expected, property)
      }
    }
    predictions
  }
  result <- tryCatch(
    {
      mismatch <- NULL
      example_results <- lapply(seq_along(examples), function(index) {
        example <- examples[[index]]
        if (!is.null(example$expected_error)) {
          failure <- tryCatch(
            {
              evaluate(example$history, example$new_data, example$model_type, "example", as.integer(index), check_properties = FALSE)
              NULL
            },
            finnts_custom_candidate_failure = function(error) error$failure
          )
          expected <- example$expected_error
          if (is.null(failure) || !identical(failure$phase, paste0("example_", expected$phase)) || !identical(
            failure$rule_error,
            expected$code
          )) {
            if (!is.null(failure) && identical(failure$reason, "input_type_mismatch")) {
              stop(structure(list(message = failure$message, call = NULL, failure = failure), class = c(
                "finnts_custom_candidate_failure",
                "error", "condition"
              )))
            }
            mismatch <- custom_author_failure(definition, paste0("example_", expected$phase), if (is.null(failure))
              "expected_error_not_raised"
            else "wrong_expected_error", as.integer(index), example$model_type)
            stop(structure(list(message = mismatch$message, call = NULL, failure = mismatch), class = c(
              "finnts_custom_candidate_failure",
              "error", "condition"
            )))
          }
          return(list(
            name = example$name, model_type = example$model_type, outcome = "error", error_phase = expected$phase,
            error_code = expected$code
          ))
        }
        predictions <- evaluate(example$history, example$new_data, example$model_type, "example", as.integer(index),
          tolerance = example$tolerance
        )
        bad <- which(abs(predictions - example$expected$.pred) > example$tolerance)
        if (length(bad) && is.null(mismatch)) {
          comparisons <- lapply(head(bad, 4L), function(row) list(
            row = as.integer(row), Date = as.character(example$new_data$Date[[row]]),
            Combo = as.character(example$new_data$Combo[[row]]), expected = example$expected$.pred[[row]], actual = predictions[[row]],
            tolerance = example$tolerance
          ))
          mismatch <<- custom_author_failure(definition, "example_compare", "intent_mismatch", as.integer(index), example$model_type,
            comparisons = comparisons
          )
        }
        list(
          name = example$name, model_type = example$model_type, maximum_error = max(abs(predictions - example$expected$.pred)),
          tolerance = example$tolerance
        )
      })
      if (!custom_author_llm_examples_advisory(contract) && any(vapply(example_results, function(result) isTRUE(result$maximum_error >
        result$tolerance), logical(1)))) {
        return(list(passed = FALSE, code = "intent_mismatch", failure = mismatch))
      }
      holdouts <- list()
      for (mode in definition$model_type) {
        datasets <- if (mode == "local")
          unname(split(data$data, data$data$Combo))
        else list(data$data)
        if (mode == "global" && length(unique(data$data$Combo)) < 2L)
          return(list(passed = FALSE, code = "insufficient_global_series", failure = custom_author_failure(definition,
            "holdout_data", "insufficient_global_series",
            model_type = mode
          )))
        for (dataset in datasets) {
          dates <- sort(unique(dataset$Date))
          horizon <- metadata$forecast_horizon
          if (length(dates) <= horizon + 1L)
            return(list(passed = FALSE, code = "insufficient_history", failure = custom_author_failure(definition,
              "holdout_data", "insufficient_history",
              model_type = mode
            )))
          cutoff <- dates[[length(dates) - horizon]]
          history <- dataset[dataset$Date <= cutoff, , drop = FALSE]
          assessment <- dataset[dataset$Date > cutoff, , drop = FALSE]
          predictors <- custom_run_predictors(definition, history)
          request <- assessment[predictors]
          if (any(table(request$Combo) != horizon))
            return(list(passed = FALSE, code = "incomplete_holdout", failure = custom_author_failure(definition, "holdout_data",
              "incomplete_holdout",
              model_type = mode
            )))
          predictions <- evaluate(history, request, mode, "holdout")
          errors <- abs(predictions - assessment$Target)
          denominator <- sum(abs(assessment$Target))
          holdouts[[length(holdouts) + 1L]] <- list(
            model_type = mode, rows = length(predictions), training_rows = nrow(history),
            cutoff = as.character(cutoff), mae = mean(errors), wmape = if (denominator == 0) "unavailable_zero_denominator" else sum(errors) / denominator
          )
        }
      }
      report <- list(
        passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(examples,
          algo = "sha256", serializeVersion = 2
        ), examples = example_results, holdouts = holdouts, serialization = TRUE,
        r_version = as.character(getRversion()), package_version = as.character(utils::packageVersion("finnts")), elapsed = proc.time()[["elapsed"]] -
          started
      )
      report$verification <- list(
        provenance = contract$expectation_provenance, properties = verified,
        expected_errors = as.integer(sum(vapply(examples, function(example) !is.null(example$expected_error), logical(1))))
      )
      report$verification$required_properties <- contract$required_properties
      report$verification$diagnostics <- diagnostics
      report
    },
    finnts_custom_candidate_failure = function(error) list(passed = FALSE, code = "candidate_validation_error", failure = error$failure)
  )
  result
}

# Check selected namespace availability and exports only on the consented path.
# No candidate body runs. Namespace loading may invoke .onLoad, so passive draft
# inspection never calls this helper. Arbitrary load errors are not disclosed.
custom_author_namespace_check <- function(definition, policy) {
  references <- custom_author_source_guard(definition$source, policy$available, references = TRUE)
  packages <- unique(vapply(references, `[[`, character(1), "namespace"))
  loadable <- stats::setNames(vapply(packages, function(package) {
    tryCatch(isTRUE(custom_model_package_available(package)), error = function(error) FALSE)
  }, logical(1)), packages)
  rejected <- list()
  for (reference in references) {
    reason <- if (!loadable[[reference$namespace]])
      "unavailable_namespace"
    else if (!reference$member %in% getNamespaceExports(reference$namespace))
      "unexported_member"
    else NULL
    if (!is.null(reason)) {
      reference$reason <- reason
      rejected[[length(rejected) + 1L]] <- reference
    }
  }
  if (!length(rejected))
    return(NULL)
  custom_author_failure(definition, "source_guard", "source_dependency", dependency = custom_author_dependency(
    rejected,
    policy$available
  ))
}

# Validate one exact authorized candidate in the current session and restore ordinary R process settings on return/error.
# Check package availability and exports before candidate execution; evaluate typed examples and holdouts with the fixed contract.
# Return measured evidence or bounded failure. The elapsed budget is checked after return and cannot interrupt stuck code.
custom_author_validate <- function(definition, examples, data, timeout, package_policy = NULL, contract = NULL) {
  started <- proc.time()[["elapsed"]]
  original_options <- options()
  original_directory <- getwd()
  original_libraries <- .libPaths()
  on.exit(
    {
      added_options <- setdiff(names(options()), names(original_options))
      options(stats::setNames(rep(list(NULL), length(added_options)), added_options))
      options(original_options)
      setwd(original_directory)
      .libPaths(original_libraries)
    },
    add = TRUE
  )
  withr::local_preserve_seed()
  custom_model_current_definition(definition)
  examples <- custom_author_examples(examples, definition, data$metadata)
  if (!is.null(package_policy)) {
    failure <- custom_author_namespace_check(definition, package_policy)
    if (!is.null(failure))
      return(list(passed = FALSE, code = "candidate_validation_error", failure = failure))
  }
  for (package in definition$packages) {
    if (!custom_model_package_available(package))
      custom_author_abort("environment", "A declared package is unavailable; no packages were installed.")
  }
  report <- custom_author_child(definition, examples, data, contract)
  if (proc.time()[["elapsed"]] - started > timeout) {
    return(list(passed = FALSE, code = "validation_timeout", failure = custom_author_failure(
      definition, "validation_budget",
      "validation_timeout"
    )))
  }
  if (!is.null(report$failure))
    validate_custom_author_failure(report$failure, definition)
  report
}

# Validate measured current-format evidence against exact model, contract, examples and property/provenance identities.
# Convert only bounded metrics into passive check summaries; required comparisons and hierarchy coverage cannot be omitted.
# Generated numerical mismatches remain warning evidence. Automatic defaults must use the single current catalog.
# No source execution, warning emission, provider request or storage write occurs during this check.
custom_author_checks <- function(
    report, definition, contract_id, examples, metadata = NULL, approval_mode = "manual", automatic_defaults = NULL,
    contract = NULL) {
  if (!custom_author_text(approval_mode) || !approval_mode %in% c("manual", "automatic"))
    custom_author_abort("validation", "Unknown approval policy.")
  custom_author_fields(report, c(
    "passed", "code", "version_id", "examples_id", "examples", "holdouts", "serialization",
    "r_version", "package_version", "elapsed", "verification"
  ), "validation")
  if (!identical(report$passed, TRUE) || !identical(report$code, "passed") || !identical(report$version_id, definition$version_id) ||
    !identical(report$serialization, TRUE) || !length(report$examples) || !length(report$holdouts) || !is.numeric(report$elapsed) ||
    report$elapsed < 0) {
    custom_author_abort("validation", "Invalid measured validation report.")
  }
  # Accept one finite nonnegative scalar metric, not empty or vector evidence.
  metric <- function(value) is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0
  if (!metric(report$elapsed) || !custom_author_text(report$r_version) || !custom_author_text(report$package_version) ||
    !custom_author_text(report$examples_id) || !grepl("^[a-f0-9]{64}$", report$examples_id)) {
    custom_author_abort("validation", "Malformed runtime report.")
  }
  if (length(report$examples) != length(examples) || !identical(report$examples_id, digest::digest(examples,
    algo = "sha256",
    serializeVersion = 2
  ))) {
    custom_author_abort("validation", "Measured report does not cover the confirmed examples.")
  }
  checks <- list(
    contract = paste(
      if (approval_mode == "automatic") "Automatically authorized contract" else "Confirmed intent",
      contract_id
    ), examples = paste(
      if (approval_mode == "automatic") "Fixed validation examples" else "Confirmed examples",
      report$examples_id
    ), runtime = paste("R", report$r_version, "finnts", report$package_version), serialization = "Fitted RDS round-trip predictions matched.",
    elapsed = paste("Validation seconds", round(report$elapsed, 3))
  )
  verification <- report$verification
  custom_author_fields(verification, c("provenance", "properties", "expected_errors", c(
    "required_properties",
    "diagnostics", NULL
  )), "validation")
  expected_properties <- c("row_order", contract$validation_properties)
  count <- sum(vapply(examples, function(example) !is.null(example$expected_error), logical(1)))
  if (!identical(verification$provenance, contract$expectation_provenance) ||
    !setequal(names(verification$properties), expected_properties) || !all(vapply(verification$properties, function(value) is.integer(value) &&
    length(value) == 1L && !is.na(value) && value >= (0L), logical(1))) || !identical(verification$expected_errors, as.integer(count)))
    custom_author_abort("validation", "Measured behavioral evidence is incomplete or changed.")
  mandatory <- c("row_order", contract$required_properties)
  if (!identical(verification$required_properties, contract$required_properties) || !all(vapply(
    verification$properties[mandatory],
    function(value) !is.null(value) && value > 0L, logical(1)
  )) || !is.list(verification$diagnostics) || length(verification$diagnostics) >
    40L) {
    custom_author_abort("validation", "Property authority or measured required checks changed.")
  }
  for (diagnostic in verification$diagnostics) {
    custom_author_fields(diagnostic, c("property", "example_index", "model_type", "reason"), "validation")
    if (!custom_author_text(diagnostic$property) || !diagnostic$property %in% setdiff(expected_properties, mandatory) ||
      !is.integer(diagnostic$example_index) || length(diagnostic$example_index) != 1L || is.na(diagnostic$example_index) ||
      !diagnostic$example_index %in% seq_along(examples) || !identical(diagnostic$model_type, examples[[diagnostic$example_index]]$model_type) ||
      !custom_author_text(diagnostic$reason) || !diagnostic$reason %in% c("property_mismatch", "evaluation_error")) {
      custom_author_abort("validation", "Invalid diagnostic property evidence.")
    }
  }
  numerical_cases <- sum(vapply(examples, function(example) is.null(example$expected_error), logical(1)))
  for (property in expected_properties) {
    failed <- sum(vapply(verification$diagnostics, function(item) identical(item$property, property), logical(1)))
    if (verification$properties[[property]] + failed != numerical_cases)
      custom_author_abort("validation", "Measured property coverage changed.")
  }
  checks$property_diagnostics <- paste(length(verification$diagnostics), "suggested-property checks did not pass; required checks passed.")
  checks$expectation_provenance <- paste(verification$provenance, "- numerical expectations are not independently mathematically verified.")
  checks$behavioral_evidence <- as.character(jsonlite::toJSON(verification, auto_unbox = TRUE))
  if (!is.null(metadata$input_context$hist_end_date)) {
    checks$authoring_cutoff <- paste(metadata$input_context$hist_end_date, paste0(
      "(", metadata$cutoff_source %||% "saved",
      ")"
    ), "for authoring validation; later fits derive their own cutoff.")
  }
  if (!is.null(metadata$resolved_inputs)) {
    custom_run_passive(metadata$resolved_inputs)
    checks$resolved_inputs <- as.character(jsonlite::toJSON(metadata$resolved_inputs, auto_unbox = TRUE, null = "null"))
  }
  if (length(definition$requirements$predictors)) {
    checks$driver_conditioning <- "Holdout metrics are conditional on supplied driver values; historical plan vintages and publication lags are not verified."
  }
  if (approval_mode == "automatic") {
    custom_author_fields(automatic_defaults, c("defaults_used", "effective"), "validation")
    checks$automatic_defaults <- jsonlite::toJSON(automatic_defaults, auto_unbox = TRUE, null = "null")
    checks$automatic_defaults <- as.character(checks$automatic_defaults)
  }
  if (!is.null(definition$requirements$hierarchy)) {
    nodes <- metadata$hierarchy$node_count
    if (!metric(nodes) || nodes < 1 || nodes != floor(nodes) || length(report$holdouts) != nodes) {
      custom_author_abort("validation", "Measured report omits required hierarchy node holdouts.")
    }
    checks$hierarchy <- paste("Validated", nodes, "independent node holdouts; final forecasts require normal FinnTS reconciliation.")
  }
  for (index in seq_along(report$examples)) {
    result <- report$examples[[index]]
    if (!is.null(examples[[index]]$expected_error)) {
      custom_author_fields(result, c("name", "model_type", "outcome", "error_phase", "error_code"), "validation")
      expected <- examples[[index]]$expected_error
      if (!identical(result$name, examples[[index]]$name) || !identical(result$model_type, examples[[index]]$model_type) ||
        !identical(result$outcome, "error") || !identical(result$error_phase, expected$phase) || !identical(
        result$error_code,
        expected$code
      )) {
        custom_author_abort("validation", "Measured error case differs from its fixed expected outcome.")
      }
      checks[[paste0("example_", index)]] <- paste(
        result$model_type, "observed expected", result$error_phase, "error",
        result$error_code
      )
      next
    }
    custom_author_fields(result, c("name", "model_type", "maximum_error", "tolerance"), "validation")
    if (!identical(result[c("name", "model_type", "tolerance")], examples[[index]][c("name", "model_type", "tolerance")])) {
      custom_author_abort("validation", "Measured report altered the confirmed examples.")
    }
    if (!custom_author_text(result$name) || !custom_author_text(result$model_type) || !result$model_type %in% definition$model_type ||
      !metric(result$maximum_error) || !metric(result$tolerance) || result$tolerance > 0.01 || result$maximum_error <
      0 || (!custom_author_llm_examples_advisory(contract) && result$maximum_error > result$tolerance))
      custom_author_abort("validation", "Intent comparison failed.")
    checks[[paste0("example_", index)]] <- paste(
      result$model_type, "maximum error", result$maximum_error, "tolerance",
      result$tolerance
    )
  }
  if (custom_author_llm_examples_advisory(contract)) {
    mismatches <- which(vapply(report$examples, function(result) isTRUE(result$maximum_error > result$tolerance), logical(1)))
    if (length(mismatches))
      checks$llm_example_warning <- paste(
        length(mismatches), "LLM-generated numerical example(s) did not match: indices",
        paste(mismatches, collapse = ", "), ". No numerical rewrite was requested for those expectations. Supply validation_examples with independently calculated expected outputs to verify your business rule."
      )
  }
  for (index in seq_along(report$holdouts)) {
    result <- report$holdouts[[index]]
    custom_author_fields(result, c("model_type", "rows", "training_rows", "cutoff", "mae", "wmape"), "validation")
    if (!custom_author_text(result$model_type) || !result$model_type %in% definition$model_type || !metric(result$rows) ||
      result$rows < 1 || result$rows != floor(result$rows) || !metric(result$training_rows) || result$training_rows <
      2 || result$training_rows != floor(result$training_rows) || !custom_author_text(result$cutoff) || !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
      result$cutoff
    ) || !metric(result$mae) || !(metric(result$wmape) || identical(result$wmape, "unavailable_zero_denominator"))) {
      custom_author_abort("validation", "Malformed holdout report.")
    }
    checks[[paste0("holdout_", index)]] <- paste(
      result$model_type, "rows", result$rows, "training rows", result$training_rows,
      "cutoff", result$cutoff, "MAE", result$mae, "WMAPE", result$wmape
    )
  }
  if (!setequal(vapply(report$examples, function(result) result$model_type, character(1)), definition$model_type) || !setequal(vapply(
    report$holdouts,
    function(result) result$model_type, character(1)
  ), definition$model_type)) {
    custom_author_abort("validation", "Measured report omits a declared mode.")
  }
  checks
}
