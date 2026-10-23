#' Validate Prepared Input Data
#'
#' Checks a prepared input table against the rules [prep_data()] enforces and
#' reports how preparation will treat the data, without starting a run or
#' writing anything to disk.
#'
#' Input rules `R1`-`R8`, `R10` and `R11` reuse the checks in [prep_data()],
#' so a failure here carries the same message `prep_data()` would raise. `R9`
#' is stricter than `prep_data()`: it also checks that `date_type` matches the
#' spacing of the dates. Preparation checks
#' (`P1`-`P8`) describe what `prep_data()` and the train/test split planning
#' will do with the data, such as future rows, padded gaps, missing values,
#' series removed by `combo_cleanup_date` and short histories. A check fails
#' only when the data cannot be prepared correctly; behavior that is applied
#' automatically is returned with status `"report"`.
#'
#' Rules:
#' * `R1` combo variables, the target and external regressors exist.
#' * `R2` `Date` is not a combo variable, the target or an external regressor.
#' * `R3` the date column is named `Date` and has class Date.
#' * `R4` combo variables are not date columns.
#' * `R5` the target is numeric.
#' * `R6` combo values are not blank after trimming whitespace.
#' * `R7` each combo and `Date` pair appears once.
#' * `R8` month, quarter and year dates fall on one day of the month.
#' * `R9` `date_type` is valid and is not finer or coarser than the data.
#' * `R10` `fiscal_year_start` is a number from 1 to 12.
#' * `R11` Spark tables run with Spark parallel processing.
#' * `P1` history end date and the rows treated as future periods.
#' * `P2` external regressors with future values and their coverage.
#' * `P3` missing periods that preparation pads.
#' * `P4` missing, NaN and infinite target values.
#' * `P5` weekly dates share a weekday; quarterly and yearly dates share a
#'   period month.
#' * `P6` series removed by `combo_cleanup_date`.
#' * `P7` total or subtotal series mixed with the series they sum.
#' * `P8` history supports the forecast horizon and back-test splits.
#'
#' Preparation checks run only when every input rule passes, and a check that
#' depends on a failed check is skipped. Spark tables are collected locally
#' after `R11` is checked.
#'
#' @inheritParams prep_data
#' @param forecast_approach Forecast approach, as passed to [prep_data()].
#'   It is validated but does not change the checks: every approach expects
#'   bottom-level series, so totals mixed with their parts fail `P7` for all.
#' @param back_test_scenarios Number of back-test scenarios, as passed to
#'   [prep_models()]. Default of NULL chooses them from the history length.
#' @param back_test_spacing Periods between back-test scenarios, as passed to
#'   [prep_models()]. Default of NULL uses the date type default.
#'
#' @return A list with:
#'   * `passed`: TRUE when no check failed.
#'   * `checks`: tibble with one row per rule and columns `Rule`,
#'     `Description`, `Status` (`"pass"`, `"fail"`, `"report"` or
#'     `"skipped"`) and `Message`.
#'   * `summary`: list with the history start and end dates, row, series,
#'     history and future row counts, `date_type` and `forecast_horizon`.
#'   * `details`: list of tibbles and vectors behind the preparation checks:
#'     `future_regressors`, `future_regressor_coverage`, `gaps`,
#'     `target_values`, `cleanup_combos`, `total_like_values`,
#'     `aggregate_levels`, `splits` and `short_history_combos`.
#' @examples
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(Date >= "2010-01-01")
#'
#' result <- validate_prepared_input(
#'   input_data = data_tbl,
#'   combo_variables = "id",
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3
#' )
#'
#' result$passed
#' result$checks
#' @export
validate_prepared_input <- function(input_data,
                                    combo_variables,
                                    target_variable,
                                    date_type,
                                    forecast_horizon,
                                    external_regressors = NULL,
                                    hist_start_date = NULL,
                                    hist_end_date = NULL,
                                    combo_cleanup_date = NULL,
                                    fiscal_year_start = 1,
                                    clean_missing_values = TRUE,
                                    forecast_approach = "bottoms_up",
                                    parallel_processing = NULL,
                                    back_test_scenarios = NULL,
                                    back_test_spacing = NULL) {
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame", "tbl_spark"))
  check_input_type("combo_variables", combo_variables, "character")
  check_input_type("target_variable", target_variable, "character")
  check_input_type("date_type", date_type, "character")
  check_input_type("forecast_horizon", forecast_horizon, "numeric")
  check_input_type("external_regressors", external_regressors, c("character", "NULL"))
  check_input_type("hist_start_date", hist_start_date, c("Date", "NULL"))
  check_input_type("hist_end_date", hist_end_date, c("Date", "NULL"))
  check_input_type("combo_cleanup_date", combo_cleanup_date, c("Date", "NULL"))
  check_input_type("clean_missing_values", clean_missing_values, "logical")
  check_input_type("forecast_approach", forecast_approach, "character", c("bottoms_up", "grouped_hierarchy", "standard_hierarchy"))
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("back_test_scenarios", back_test_scenarios, c("numeric", "NULL"))
  check_input_type("back_test_spacing", back_test_spacing, c("numeric", "NULL"))

  state <- new.env(parent = emptyenv())
  state$checks <- list()
  state$details <- list(
    future_regressors = character(),
    future_regressor_coverage = NULL,
    gaps = NULL,
    target_values = NULL,
    cleanup_combos = character(),
    total_like_values = NULL,
    aggregate_levels = NULL,
    splits = NULL,
    short_history_combos = character()
  )

  run_rule <- function(rule, requires, fn) {
    blocked <- requires[vapply(requires, function(r) !state$checks[[r]]$Status %in% c("pass", "report"), logical(1))]
    if (length(blocked) > 0) {
      state$checks[[rule]] <- validation_check_row(
        rule, "skipped",
        paste0("Skipped because ", paste(blocked, collapse = ", "), " did not pass.")
      )
      return(invisible(NULL))
    }
    result <- tryCatch(fn(), error = function(e) list(status = "fail", message = conditionMessage(e)))
    if (is.null(result)) result <- list(status = "pass", message = "Passed.")
    for (name in names(result$details)) state$details[[name]] <- result$details[[name]]
    state$checks[[rule]] <- validation_check_row(rule, result$status, result$message)
    invisible(NULL)
  }

  run_rule("R11", character(), function() {
    check_input_parallel_type(input_data, parallel_processing)
  })

  data <- tibble::as_tibble(dplyr::collect(input_data))

  run_rule("R1", character(), function() {
    check_input_columns(data, combo_variables, target_variable, external_regressors)
  })

  data <- normalize_combo_values(
    input_data = data,
    combo_variables = combo_variables
  )

  run_rule("R2", character(), function() {
    check_input_reserved_date(combo_variables, target_variable, external_regressors)
  })
  run_rule("R3", character(), function() check_input_date_column(data))
  run_rule("R4", "R1", function() check_input_combo_types(data, combo_variables))
  run_rule("R5", "R1", function() check_input_target_numeric(data, target_variable))
  run_rule("R6", c("R1", "R3"), function() check_input_blank_combos(data, combo_variables))
  run_rule("R7", c("R1", "R3"), function() check_input_duplicates(data, combo_variables))
  run_rule("R9", c("R1", "R3"), function() validation_check_frequency(data, combo_variables, date_type))
  run_rule("R8", c("R3", "R9"), function() check_input_date_alignment(data, date_type))
  run_rule("R10", character(), function() check_input_fiscal_year_start(fiscal_year_start))

  input_rules <- paste0("R", 1:11)
  bounds <- NULL
  if (all(vapply(input_rules, function(r) identical(state$checks[[r]]$Status, "pass"), logical(1)))) {
    data$Combo <- do.call(paste, c(unname(as.list(data[combo_variables])), sep = "--"))
    data$Target <- data[[target_variable]]
    bounds <- resolve_hist_date_bounds(data, hist_start_date, hist_end_date)
    date_grid <- tryCatch(
      validation_date_grid(bounds$hist_start_date, bounds$hist_end_date, date_type, forecast_horizon),
      error = function(e) e
    )

    run_rule("P1", character(), function() {
      validation_check_history(data, bounds)
    })
    run_rule("P2", "P1", function() {
      validation_check_future_regressors(data, external_regressors, bounds, date_grid, forecast_horizon)
    })
    run_rule("P3", "P1", function() {
      validation_check_gaps(data, bounds, date_type, clean_missing_values)
    })
    run_rule("P4", "P1", function() {
      validation_check_target_values(data, bounds, clean_missing_values)
    })
    run_rule("P5", character(), function() validation_check_grid(data, date_type))
    run_rule("P6", "P1", function() {
      validation_check_cleanup(data, combo_cleanup_date, bounds)
    })
    run_rule("P7", "P1", function() {
      # Only a P6 report leaves some series; when P6 fails, check them all.
      removed <- if (identical(state$checks$P6$Status, "report")) state$details$cleanup_combos else character()
      validation_check_totals(data, combo_variables, bounds, removed)
    })
    run_rule("P8", c("P1", "P5"), function() {
      validation_check_splits(
        data, bounds, date_type, date_grid, forecast_horizon,
        back_test_scenarios, back_test_spacing
      )
    })
  } else {
    for (rule in paste0("P", 1:8)) {
      state$checks[[rule]] <- validation_check_row(
        rule, "skipped", "Skipped because an input rule did not pass."
      )
    }
  }

  rule_order <- c(input_rules, paste0("P", 1:8))
  checks <- dplyr::bind_rows(state$checks[rule_order])

  summary <- list(
    hist_start_date = if (is.null(bounds)) NULL else bounds$hist_start_date,
    hist_end_date = if (is.null(bounds)) NULL else bounds$hist_end_date,
    n_rows = nrow(data),
    n_series = if ("Combo" %in% names(data)) length(unique(data$Combo)) else NA_integer_,
    n_history_rows = if (is.null(bounds)) NA_integer_ else sum(data$Date <= bounds$hist_end_date),
    n_future_rows = if (is.null(bounds)) NA_integer_ else sum(data$Date > bounds$hist_end_date),
    date_type = date_type,
    forecast_horizon = forecast_horizon
  )

  list(
    passed = !any(checks$Status == "fail"),
    checks = checks,
    summary = summary,
    details = state$details
  )
}

#' Build one validation check row
#'
#' @param rule rule identifier such as `"R1"` or `"P3"`
#' @param status one of `"pass"`, `"fail"`, `"report"` or `"skipped"`
#' @param message plain-language result message
#'
#' @return one-row tibble with Rule, Description, Status and Message
#' @noRd
validation_check_row <- function(rule, status, message) {
  descriptions <- c(
    R1 = "Required columns exist",
    R2 = "Date is not used as a combo variable, target or regressor",
    R3 = "Date column is named Date and has class Date",
    R4 = "Combo variables are not dates",
    R5 = "Target is numeric",
    R6 = "Combo values are not blank after trimming",
    R7 = "Each combo and Date pair appears once",
    R8 = "Period dates fall on the same day of the month",
    R9 = "date_type is valid and matches the data frequency",
    R10 = "fiscal_year_start is a number from 1 to 12",
    R11 = "Spark tables use Spark parallel processing",
    P1 = "History end date and future rows",
    P2 = "Future external regressor values",
    P3 = "Missing periods padded during preparation",
    P4 = "Missing and non-finite target values",
    P5 = "Dates align to the date_type grid",
    P6 = "Series removed by combo_cleanup_date",
    P7 = "No total series mixed with the series they sum",
    P8 = "History supports the forecast horizon and back tests"
  )
  tibble::tibble(
    Rule = rule,
    Description = unname(descriptions[[rule]]),
    Status = status,
    Message = message
  )
}

#' Count whole periods between consecutive dates
#'
#' @param dates sorted unique Date vector
#' @param date_type one of day, week, month, quarter or year
#'
#' @return numeric vector of steps measured in `date_type` periods
#' @noRd
validation_period_steps <- function(dates,
                                    date_type) {
  if (length(dates) < 2) {
    return(numeric())
  }
  if (date_type %in% c("day", "week")) {
    days <- as.numeric(diff(dates))
    return(if (date_type == "day") days else days / 7)
  }
  month_index <- as.numeric(format(dates, "%Y")) * 12 + as.numeric(format(dates, "%m"))
  months <- diff(month_index)
  switch(date_type,
    month = months,
    quarter = months / 3,
    year = months / 12
  )
}

#' Check that date_type is valid and matches the data frequency
#'
#' Fails when a Date is missing, dates are closer than one period or not whole
#' periods apart, or dates are never exactly one period apart within any series.
#'
#' @param data input data with combo columns and Date
#' @param combo_variables combo variable names
#' @param date_type declared date type
#'
#' @return list with status and message
#' @noRd
validation_check_frequency <- function(data,
                                       combo_variables,
                                       date_type) {
  valid_types <- c("day", "week", "month", "quarter", "year")
  if (length(date_type) != 1 || !date_type %in% valid_types) {
    stop("date_type must be one of: ", paste(valid_types, collapse = ", "), ".", call. = FALSE)
  }
  if (anyNA(data$Date)) {
    stop(sum(is.na(data$Date)), " rows have a missing Date value.", call. = FALSE)
  }

  combo_key <- do.call(paste, c(unname(as.list(data[combo_variables])), sep = "--"))
  steps <- unlist(lapply(
    split(data$Date, combo_key),
    function(dates) validation_period_steps(sort(unique(dates)), date_type)
  ), use.names = FALSE)

  if (length(steps) == 0) {
    return(list(status = "pass", message = "Each series has one date, so the frequency cannot be compared."))
  }

  off_grid <- steps < 1 - 1e-8 | abs(steps - round(steps)) > 1e-8
  if (any(off_grid)) {
    stop(
      "dates are closer than one ", date_type, " or not a whole number of ",
      date_type, "s apart in ", sum(off_grid), " of ", length(steps),
      " steps. Check that date_type matches the data.",
      call. = FALSE
    )
  }

  single_steps <- sum(abs(steps - 1) < 1e-8)
  if (single_steps == 0) {
    stop(
      "no series has consecutive dates one ", date_type, " apart; the smallest step is ",
      min(steps), " ", date_type, "s. The data looks coarser than '", date_type, "'.",
      call. = FALSE
    )
  }

  list(
    status = "pass",
    message = paste0(single_steps, " of ", length(steps), " date steps are exactly one ", date_type, ".")
  )
}

#' Build the padded date grid used for split planning
#'
#' Mirrors the per-series grid [prep_data()] writes: history padded from the
#' start to the end date plus `forecast_horizon` future periods.
#'
#' @param hist_start_date first historical date
#' @param hist_end_date last historical date
#' @param date_type date type
#' @param forecast_horizon number of future periods
#'
#' @return tibble with a Date column
#' @noRd
validation_date_grid <- function(hist_start_date,
                                 hist_end_date,
                                 date_type,
                                 forecast_horizon) {
  suppressMessages(
    tibble::tibble(Date = sort(unique(c(hist_start_date, hist_end_date)))) %>%
      timetk::pad_by_time(Date, .by = date_type) %>%
      timetk::future_frame(Date, .length_out = forecast_horizon, .bind_data = TRUE)
  )
}

#' Report the history window and future rows
#'
#' @param data validated input with Date and Target
#' @param bounds list from `resolve_hist_date_bounds()`
#'
#' @return list with status and message
#' @noRd
validation_check_history <- function(data,
                                     bounds) {
  if (bounds$hist_start_date > bounds$hist_end_date) {
    stop("hist_start_date ", bounds$hist_start_date, " is after hist_end_date ", bounds$hist_end_date, ".", call. = FALSE)
  }
  if (!any(data$Date <= bounds$hist_end_date)) {
    stop("no rows fall on or before hist_end_date ", bounds$hist_end_date, ".", call. = FALSE)
  }

  future <- data$Date > bounds$hist_end_date
  history_text <- paste0("History runs from ", bounds$hist_start_date, " to ", bounds$hist_end_date, ".")
  if (!any(future)) {
    return(list(status = "pass", message = paste(history_text, "No rows fall after the history end date.")))
  }

  list(
    status = "report",
    message = paste0(
      history_text, " ", sum(future), " rows fall after it and are treated as future periods; ",
      sum(!is.na(data$Target[future])), " of their target values are present and will be ignored."
    )
  )
}

#' Report external regressors with future values and their coverage
#'
#' @param data validated input with Combo and Date
#' @param external_regressors external regressor names or NULL
#' @param bounds list from `resolve_hist_date_bounds()`
#' @param date_grid tibble from `validation_date_grid()` or an error condition
#' @param forecast_horizon number of future periods
#'
#' @return list with status, message and details
#' @noRd
validation_check_future_regressors <- function(data,
                                               external_regressors,
                                               bounds,
                                               date_grid,
                                               forecast_horizon) {
  if (length(external_regressors) == 0) {
    return(list(status = "pass", message = "No external regressors."))
  }

  future_xregs <- global_future_regressors(data, external_regressors, bounds$hist_end_date)
  if (is.null(future_xregs)) {
    return(list(
      status = "pass",
      message = paste0(
        "No external regressor has values after ", bounds$hist_end_date,
        "; all are used as historical features only."
      ),
      details = list(future_regressors = character())
    ))
  }

  coverage <- NULL
  if (!inherits(date_grid, "error")) {
    future_dates <- date_grid$Date[date_grid$Date > bounds$hist_end_date]
    combos <- sort(unique(data$Combo))
    coverage <- dplyr::bind_rows(lapply(future_xregs, function(xreg) {
      present <- data[data$Date %in% future_dates & !is.na(data[[xreg]]), c("Combo", "Date")]
      counts <- table(factor(unique(present)$Combo, levels = combos))
      tibble::tibble(
        Regressor = xreg,
        Combo = combos,
        Future_Values = as.integer(counts),
        Missing_Future_Periods = as.integer(length(future_dates) - counts)
      )
    }))
  }

  missing <- if (is.null(coverage)) 0 else sum(coverage$Missing_Future_Periods > 0)
  list(
    status = "report",
    message = paste0(
      "Future values after ", bounds$hist_end_date, " are used for: ",
      paste(future_xregs, collapse = ", "), ".",
      if (missing > 0) {
        paste0(
          " ", missing, " regressor and series pairs lack values for part of the ",
          forecast_horizon, "-period horizon; preparation imputes or zero-fills them."
        )
      } else {
        ""
      }
    ),
    details = list(future_regressors = future_xregs, future_regressor_coverage = coverage)
  )
}

#' Report missing periods that preparation pads
#'
#' Pads each series on the same grid [prep_data()] uses and counts periods
#' added before the first date and inside or after the series history.
#'
#' @param data validated input with Combo and Date
#' @param bounds list from `resolve_hist_date_bounds()`
#' @param date_type date type
#' @param clean_missing_values logical; whether missing values are imputed
#'
#' @return list with status, message and details
#' @noRd
validation_check_gaps <- function(data,
                                  bounds,
                                  date_type,
                                  clean_missing_values) {
  history <- unique(data[data$Date <= bounds$hist_end_date, c("Combo", "Date")])
  padded <- suppressMessages(
    history %>%
      dplyr::group_by(Combo) %>%
      timetk::pad_by_time(Date,
        .by = date_type,
        .start_date = bounds$hist_start_date,
        .end_date = bounds$hist_end_date
      ) %>%
      dplyr::ungroup()
  )

  first_dates <- tapply(history$Date, history$Combo, min)
  added <- dplyr::anti_join(padded, history, by = c("Combo", "Date"))
  added_first <- as.Date(first_dates[added$Combo], origin = "1970-01-01")
  combos <- sort(unique(history$Combo))
  gaps <- tibble::tibble(
    Combo = combos,
    Leading_Periods = as.integer(table(factor(added$Combo[added$Date < added_first], levels = combos))),
    Missing_Periods = as.integer(table(factor(added$Combo[added$Date >= added_first], levels = combos)))
  )
  gaps <- gaps[gaps$Leading_Periods > 0 | gaps$Missing_Periods > 0, ]

  if (nrow(gaps) == 0) {
    return(list(status = "pass", message = "No missing periods.", details = list(gaps = gaps)))
  }

  list(
    status = "report",
    message = paste0(
      nrow(gaps), " series have ", sum(gaps$Missing_Periods), " missing periods inside or after their history (",
      if (clean_missing_values) "imputed" else "set to 0", ") and ",
      sum(gaps$Leading_Periods), " periods before their first date (set to 0)."
    ),
    details = list(gaps = gaps)
  )
}

#' Report missing, NaN and infinite target values
#'
#' @param data validated input with Combo, Date and Target
#' @param bounds list from `resolve_hist_date_bounds()`
#' @param clean_missing_values logical; whether missing values are imputed
#'
#' @return list with status, message and details
#' @noRd
validation_check_target_values <- function(data,
                                           bounds,
                                           clean_missing_values) {
  history <- data[data$Date <= bounds$hist_end_date, c("Combo", "Target")]
  is_nan <- is.nan(history$Target)
  is_na <- is.na(history$Target) & !is_nan
  is_inf <- is.infinite(history$Target)
  combos <- sort(unique(history$Combo))
  values <- tibble::tibble(
    Combo = combos,
    Missing = as.integer(table(factor(history$Combo[is_na], levels = combos))),
    NaN_Values = as.integer(table(factor(history$Combo[is_nan], levels = combos))),
    Infinite = as.integer(table(factor(history$Combo[is_inf], levels = combos)))
  )
  values <- values[values$Missing + values$NaN_Values + values$Infinite > 0, ]

  if (nrow(values) == 0) {
    return(list(status = "pass", message = "No missing or non-finite target values.", details = list(target_values = values)))
  }

  list(
    status = "report",
    message = paste0(
      sum(values$Missing), " missing target values (",
      if (clean_missing_values) "imputed" else "set to 0", ") and ",
      sum(values$NaN_Values) + sum(values$Infinite), " NaN or infinite values (set to 0) in ",
      nrow(values), " series."
    ),
    details = list(target_values = values)
  )
}

#' Check that dates align to the date_type grid
#'
#' @param data validated input with Date
#' @param date_type date type
#'
#' @return list with status and message
#' @noRd
validation_check_grid <- function(data,
                                  date_type) {
  dates <- unique(data$Date)
  if (date_type == "week") {
    weekdays <- unique(format(dates, "%u"))
    if (length(weekdays) > 1) {
      stop(
        "weekly dates fall on ", length(weekdays), " different weekdays; use one weekday for every series.",
        call. = FALSE
      )
    }
  } else if (date_type == "quarter") {
    offsets <- unique((as.numeric(format(dates, "%m")) - 1) %% 3)
    if (length(offsets) > 1) {
      stop("quarterly dates do not use the same month of each quarter.", call. = FALSE)
    }
  } else if (date_type == "year") {
    months <- unique(format(dates, "%m"))
    if (length(months) > 1) {
      stop("yearly dates fall in ", length(months), " different months; use one month for every year.", call. = FALSE)
    }
  }
  NULL
}

#' Report series removed by combo_cleanup_date
#'
#' @param data validated input with Combo, Date and Target
#' @param combo_cleanup_date Date or NULL
#' @param bounds list from `resolve_hist_date_bounds()`
#'
#' @return list with status, message and details
#' @noRd
validation_check_cleanup <- function(data,
                                     combo_cleanup_date,
                                     bounds) {
  if (is.null(combo_cleanup_date)) {
    return(list(status = "pass", message = "combo_cleanup_date is not set; no series are removed."))
  }

  combos <- unique(data$Combo)
  kept <- unique(combo_cleanup_fn(
    data[, c("Combo", "Date", "Target")],
    combo_cleanup_date,
    bounds$hist_end_date
  )$Combo)
  removed <- sort(setdiff(combos, kept))

  if (length(kept) == 0) {
    return(list(
      status = "fail",
      message = paste0(
        "combo_cleanup_date ", combo_cleanup_date,
        " removes every series because none has a non-zero total after it."
      ),
      details = list(cleanup_combos = removed)
    ))
  }
  if (length(removed) == 0) {
    return(list(status = "pass", message = "No series are removed.", details = list(cleanup_combos = removed)))
  }

  list(
    status = "report",
    message = paste0(
      length(removed), " of ", length(combos), " series have a zero total after ",
      combo_cleanup_date, " and will be removed."
    ),
    details = list(cleanup_combos = removed)
  )
}

#' Find combo levels that equal the sum of their sibling levels
#'
#' For each combo variable, compares every level with the sum of the other
#' levels that share the remaining combo values and date. A level is an
#' aggregate when it matches on at least `min_share` of at least three
#' comparable dates, so one missing leaf row does not hide a total. A row is
#' comparable only when its value or its sibling sum is non-zero beyond
#' `tolerance`: in sparse data every level trivially "equals" an all-zero
#' sibling sum, which would otherwise flag ordinary series as totals.
#' Removed combos still contribute to sibling sums, but their rows cannot
#' supply aggregate evidence, even when a kept combo shares the same level.
#'
#' @param data history rows with combo columns, Date and Target; also requires
#'   Combo when `removed_combos` is nonempty
#' @param combo_variables combo variable names
#' @param tolerance relative tolerance for equality; also the absolute
#'   magnitude below which a value and its sibling sum both count as zero
#' @param min_share minimum share of comparable dates that must match
#' @param removed_combos exact Combo identities excluded as aggregate candidates
#'
#' @return tibble with Variable and Level for aggregate levels
#' @noRd
validation_aggregate_levels <- function(data,
                                        combo_variables,
                                        tolerance = 1e-6,
                                        min_share = 0.9,
                                        removed_combos = character()) {
  data <- data[!is.na(data$Target) & is.finite(data$Target), ]
  found <- list()
  for (variable in combo_variables) {
    others <- setdiff(combo_variables, variable)
    group_key <- do.call(paste, c(unname(as.list(data[c(others, "Date")])), sep = "\r"))
    group_total <- stats::ave(data$Target, group_key, FUN = sum)
    group_size <- stats::ave(rep(1, nrow(data)), group_key, FUN = sum)
    sibling_sum <- group_total - data$Target
    comparable <- pmax(abs(data$Target), abs(sibling_sum)) > tolerance
    eligible <- group_size >= 3 & comparable
    if (length(removed_combos)) {
      eligible <- eligible & !data$Combo %in% removed_combos
    }
    matches <- abs(data$Target - sibling_sum) <= tolerance * pmax(1, abs(data$Target))
    if (!any(eligible)) next
    level <- as.character(data[[variable]])[eligible]
    row_count <- tapply(rep(1, length(level)), level, sum)
    match_share <- tapply(matches[eligible], level, mean)
    any_nonzero <- tapply(data$Target[eligible] != 0, level, any)
    hits <- names(row_count)[row_count >= 3 & match_share >= min_share & any_nonzero]
    if (length(hits) > 0) {
      found[[length(found) + 1]] <- tibble::tibble(Variable = variable, Level = sort(hits))
    }
  }
  if (length(found) == 0) {
    return(tibble::tibble(Variable = character(), Level = character()))
  }
  dplyr::bind_rows(found)
}

#' Check for total series mixed with the series they sum
#'
#' Fails when a combo level equals the sum of its sibling levels on nearly
#' every comparable date; reports values whose names look like totals. All
#' history contributes sibling values, so a removed part cannot hide a total.
#' Only exact combos kept by `combo_cleanup_date` supply candidate evidence
#' and total-like names.
#'
#' @param data validated input with Combo, combo columns, Date and Target
#' @param combo_variables combo variable names
#' @param bounds list from `resolve_hist_date_bounds()`
#' @param removed_combos Combo values that `combo_cleanup_date` removes
#'
#' @return list with status, message and details
#' @noRd
validation_check_totals <- function(data,
                                    combo_variables,
                                    bounds,
                                    removed_combos = character()) {
  total_pattern <- "^(grand[ _-]?)?(sub[ _-]?)?totals?$|^all$|[ _-](sub[ _-]?)?totals?$"
  kept <- data[!data$Combo %in% removed_combos, ]
  total_like <- dplyr::bind_rows(lapply(combo_variables, function(variable) {
    values <- unique(as.character(kept[[variable]]))
    values <- values[!is.na(values) & grepl(total_pattern, values, ignore.case = TRUE)]
    tibble::tibble(Variable = rep(variable, length(values)), Level = values)
  }))

  aggregates <- validation_aggregate_levels(
    data[data$Date <= bounds$hist_end_date, ],
    combo_variables,
    removed_combos = removed_combos
  )
  details <- list(total_like_values = total_like, aggregate_levels = aggregates)

  if (nrow(aggregates) > 0) {
    return(list(
      status = "fail",
      message = paste0(
        "These combo values equal the sum of the other values and look like totals mixed with the series they sum: ",
        paste0(aggregates$Variable, " = '", aggregates$Level, "'", collapse = ", "),
        ". Remove total and subtotal rows before forecasting."
      ),
      details = details
    ))
  }
  if (nrow(total_like) > 0) {
    return(list(
      status = "report",
      message = paste0(
        "These combo values are named like totals but do not sum the other values: ",
        paste0(total_like$Variable, " = '", total_like$Level, "'", collapse = ", "),
        ". Confirm they are real series."
      ),
      details = details
    ))
  }
  list(status = "pass", message = "No total series found.", details = details)
}

#' Check that history supports the horizon and back-test splits
#'
#' Plans splits with the same helper [prep_models()] uses and reports series
#' that start after the earliest back-test training window ends.
#'
#' @param data validated input with Combo, Date and Target
#' @param bounds list from `resolve_hist_date_bounds()`
#' @param date_type date type
#' @param date_grid tibble from `validation_date_grid()` or an error condition
#' @param forecast_horizon number of future periods
#' @param back_test_scenarios back test scenario override or NULL
#' @param back_test_spacing back test spacing override or NULL
#'
#' @return list with status, message and details
#' @noRd
validation_check_splits <- function(data,
                                    bounds,
                                    date_type,
                                    date_grid,
                                    forecast_horizon,
                                    back_test_scenarios,
                                    back_test_spacing) {
  if (forecast_horizon < 1) {
    stop("forecast_horizon must be at least 1.", call. = FALSE)
  }
  if (inherits(date_grid, "error")) {
    stop("could not build the forecast date grid: ", conditionMessage(date_grid), call. = FALSE)
  }

  splits <- tryCatch(
    suppressMessages(plan_train_test_splits(
      date_grid,
      bounds$hist_end_date,
      forecast_horizon,
      back_test_scenarios,
      get_back_test_spacing(back_test_spacing, date_type)
    )),
    error = function(e) {
      periods <- sum(date_grid$Date <= bounds$hist_end_date)
      stop(
        "the history has ", periods, " periods, too few for a forecast_horizon of ",
        forecast_horizon, " plus back tests (", conditionMessage(e),
        "). Add more history or shorten forecast_horizon.",
        call. = FALSE
      )
    }
  )

  back_tests <- splits[splits$Run_Type == "Back_Test", ]
  earliest_train_end <- min(back_tests$Train_End)
  history <- data[data$Date <= bounds$hist_end_date & !is.na(data$Target), c("Combo", "Date")]
  first_dates <- tapply(history$Date, history$Combo, min)
  short <- sort(names(first_dates)[first_dates > as.numeric(earliest_train_end)])

  message <- paste0(
    nrow(back_tests), " back tests and ", sum(splits$Run_Type == "Validation"),
    " validation folds; the earliest back test trains through ", earliest_train_end, "."
  )
  details <- list(splits = splits, short_history_combos = short)
  if (length(short) == 0) {
    return(list(status = "pass", message = message, details = details))
  }

  list(
    status = "report",
    message = paste0(
      message, " ", length(short), " series start after that date, so their earliest back tests train only on padded zeros."
    ),
    details = details
  )
}
