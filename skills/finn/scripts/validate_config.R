# Validate a Finn run config against its data and render the settings review.
# Usage:
#   Rscript validate_config.R --project=<name> [--config=<config.json>] [--save=true]
#       [--run_type=standard|iterate|update]
#       [--batch_size=<planned concurrent runs>] [--num_cores=<worker ceiling>]
# With --config, the file is merged over defaults; --save=true stores the
# result as <project>/config.json, which run_forecast.R uses by default.
# --run_type picks the run the review is for; by default it follows the
# project's run history (finn_next_run()). data.run names the run type, why,
# the other option, and the run_forecast.R arguments to launch it.
# data.settings_review is the full settings review. When
# --config differs from the saved config.json, data.settings_changes lists only
# the changed settings.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Ordered settings shown in the review for one run mode
#'
#' Orders core inputs first, then inputs shared by both modes,
#' mode-only inputs, then how the file is read. Operational keys such as
#' `seed`, `parallel`, and the data path are not shown.
#'
#' @param mode Config mode, `"standard"` or `"agent"`.
#' @return Character vector of setting names.
validate_review_fields <- function(mode) {
  core <- c("target_variable", "combo_variables", "date_type", "hist_end_date", "forecast_horizon")
  shared <- c("external_regressors", "hist_start_date", "combo_cleanup_date", "back_test_scenarios",
    "back_test_spacing", "negative_forecast", "run_global_models", "run_local_models")
  only <- if (identical(mode, "agent")) {
    c("allow_hierarchical_forecast", "max_iter", "weighted_mape_goal", "llm")
  } else {
    c("forecast_approach", "models_to_run", "models_not_to_run", "run_ensemble_models", "recipes_to_run",
      "pca", "average_models", "max_model_average", "feature_selection", "clean_outliers", "clean_missing_values")
  }
  c(core, shared, only, "date_column", "date_format", "decimal_mark", "fiscal_year_start")
}

#' Review categories and the settings in each, in display order
#'
#' Covers every setting either mode can show (see [validate_review_fields()]),
#' and each setting belongs to exactly one category. "Forecast overview" feeds
#' the at-a-glance table; the other categories group the remaining settings.
#' Display order only: saved configs and launches are unaffected.
#'
#' @return Named list of character vectors, category title to setting names.
validate_setting_categories <- function() {
  list(
    "Forecast overview" = c("target_variable", "combo_variables", "date_type", "hist_end_date", "forecast_horizon",
      "external_regressors"),
    "How hard the Agent searches" = c("max_iter", "weighted_mape_goal", "llm"),
    "History & series" = c("hist_start_date", "combo_cleanup_date"),
    "Model selection" = c("models_to_run", "models_not_to_run", "run_local_models", "run_global_models",
      "run_ensemble_models", "average_models", "max_model_average"),
    "Accuracy testing" = c("back_test_scenarios", "back_test_spacing"),
    "Forecast rules" = c("forecast_approach", "allow_hierarchical_forecast", "negative_forecast"),
    "Data preparation & features" = c("clean_outliers", "clean_missing_values", "recipes_to_run", "pca", "feature_selection"),
    "How your file is read" = c("date_column", "date_format", "decimal_mark", "fiscal_year_start")
  )
}

#' Review categories for one run mode
#'
#' @param mode Config mode, `"standard"` or `"agent"`.
#' @return Named list like [validate_setting_categories()], limited to the
#'   mode's review settings, without empty categories.
validate_setting_groups <- function(mode) {
  fields <- validate_review_fields(mode)
  groups <- lapply(validate_setting_categories(), function(keys) keys[keys %in% fields])
  groups[lengths(groups) > 0]
}

#' Review settings for one run mode in category display order
#'
#' @param mode Config mode, `"standard"` or `"agent"`.
#' @return Character vector of setting names.
validate_setting_order <- function(mode) {
  unlist(validate_setting_groups(mode), use.names = FALSE)
}

#' Plain-language labels shown beside each setting name
#'
#' @return Named character vector, setting name to label, covering every
#'   setting in [validate_setting_categories()].
validate_setting_labels <- function() {
  c(
    target_variable = "What we're forecasting", combo_variables = "Broken out by", date_type = "Date grain",
    hist_end_date = "History used", forecast_horizon = "Forecast period", external_regressors = "Drivers",
    max_iter = "Max iterations", weighted_mape_goal = "Accuracy goal", llm = "AI model",
    hist_start_date = "First period of history", combo_cleanup_date = "Drop inactive series",
    models_to_run = "Models to try", models_not_to_run = "Models to skip", run_local_models = "Per-series models",
    run_global_models = "Cross-series models", run_ensemble_models = "Ensemble models",
    average_models = "Average the top models", max_model_average = "Most models in an average",
    back_test_scenarios = "Back-test rounds", back_test_spacing = "Back-test spacing",
    forecast_approach = "Hierarchy handling", allow_hierarchical_forecast = "Let the Agent try hierarchies",
    negative_forecast = "Allow negative forecasts", clean_outliers = "Fix outliers",
    clean_missing_values = "Fill missing values", recipes_to_run = "Feature recipes",
    pca = "Principal component analysis", feature_selection = "Feature selection",
    date_column = "Date column", date_format = "Date format", decimal_mark = "Decimal mark",
    fiscal_year_start = "Fiscal year starts"
  )
}

#' Setting label with its config name, for table rows
#'
#' @param key Setting name from [validate_setting_labels()].
#' @return Text such as "Back-test rounds (`back_test_scenarios`)".
validate_setting_name <- function(key) {
  sprintf("%s (`%s`)", validate_setting_labels()[[key]], key)
}

#' Short meaning shown after a setting value
#'
#' Says what `null` means for settings where it is automatic or empty, and
#' shows `weighted_mape_goal` as a percentage.
#'
#' @param key Setting name.
#' @param value Setting value from the config.
#' @return Text such as " (automatic)", or "" when there is nothing to add.
validate_value_note <- function(key, value) {
  note <- if (is.null(value) || !length(value)) {
    switch(key,
      external_regressors = "none", hist_start_date = "earliest available", combo_cleanup_date = "keep all series",
      back_test_scenarios = "automatic", back_test_spacing = "automatic", models_to_run = "Finn's default models",
      models_not_to_run = "none", run_global_models = "Finn decides", run_ensemble_models = "automatic",
      recipes_to_run = "all", pca = "automatic", date_format = "auto-detect", decimal_mark = "auto-detect",
      NULL
    )
  } else if (identical(key, "weighted_mape_goal")) {
    goal <- suppressWarnings(as.numeric(value[[1]]))
    if (!is.na(goal)) sprintf("%s%% average error", format(goal * 100, digits = 6))
  }
  if (is.null(note)) "" else sprintf(" (%s)", note)
}

#' Value cell for a settings table: the config.json value plus its meaning
#'
#' @param key Setting name.
#' @param value Setting value from the config.
#' @return Markdown cell text such as "`null` (automatic)".
validate_value_cell <- function(key, value) {
  sprintf("`%s`%s", validate_escape_cell(validate_json_value(key, value)), validate_value_note(key, value))
}

#' Value of one review setting from a config
#'
#' @param cfg Merged config.
#' @param key Setting name from [validate_review_fields()]; Agent settings are
#'   read from `cfg$agent`.
#' @return The stored value, or NULL.
validate_field_value <- function(cfg, key) {
  if (key %in% c("allow_hierarchical_forecast", "max_iter", "weighted_mape_goal", "llm")) cfg$agent[[key]] else cfg[[key]]
}

#' Format a setting value as it is written in config.json
#'
#' List settings are always shown as JSON arrays, even with one item, so the
#' review matches what the user would see in the config file.
#'
#' @param key Setting name.
#' @param value Setting value.
#' @return Single JSON string, for example `["Region","Product"]`, `true`, or `null`.
validate_json_value <- function(key, value) {
  if (is.null(value) || length(value) == 0) {
    return("null")
  }
  array_key <- key %in% c("combo_variables", "external_regressors", "models_to_run", "models_not_to_run", "recipes_to_run")
  if (array_key) value <- unlist(value)
  as.character(jsonlite::toJSON(value, auto_unbox = !array_key, null = "null", digits = NA))
}

#' Make text safe for one markdown table cell
#'
#' @param x Text.
#' @return Text with pipes escaped and line breaks replaced by spaces.
validate_escape_cell <- function(x) {
  gsub("\n", " ", gsub("|", "\\|", x, fixed = TRUE), fixed = TRUE)
}

#' What `hist_end_date = null` resolves to
#'
#' Standard and Agent runs both end history at the latest Date in the input
#' file, including rows that only carry driver values: the worker passes the
#' full data and a null cutoff to `prep_data()` and `set_agent_info_custom()`.
#' When that differs from the last period with actuals, the review offers the
#' last actuals as an explicit cutoff.
#'
#' @param facts Data facts from [finn_check_config()].
#' @return Sentence(s), or NULL when the facts do not say.
validate_auto_hist_end <- function(facts) {
  latest <- facts$date_max
  actual <- facts$last_actual_date
  if (is.null(latest)) {
    return(if (!is.null(actual)) sprintf("Auto uses `%s`.", actual))
  }
  if (!is.null(actual) && !identical(as.character(actual), as.character(latest))) {
    return(sprintf(paste(
      "Auto uses `%s`, the latest date in the file, including rows that only carry driver values.",
      "To end history at the last actuals instead, set it to `%s`."
    ), latest, actual))
  }
  sprintf("Auto uses `%s`, the latest date in the file.", latest)
}

#' Plain-language meaning of one review setting
#'
#' Explains each setting and adds context for automatic values,
#' such as the date `hist_end_date = null` resolves to (see
#' [validate_auto_hist_end()]).
#'
#' @param key Setting name.
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()].
#' @param run Result of [validate_run_info()], or NULL. Adds how to continue
#'   the last Agent search when `max_iter` or `weighted_mape_goal` is shown
#'   for an Agent iterate run that can continue the current Agent version.
#' @return Explanation sentence(s).
validate_field_explanation <- function(key, cfg, facts = list(), run = NULL) {
  text <- switch(key,
    target_variable = "The column in your file that Finn forecasts.",
    combo_variables = "The columns that split your data into separate time series; Finn forecasts each combination.",
    date_type = "The period length used for history, back tests, and forecasts.",
    hist_end_date = "The last period treated as actuals; the forecast starts after it, or `null` for the latest date in the file.",
    forecast_horizon = "How many periods Finn forecasts after `hist_end_date`.",
    external_regressors = paste("Columns from your file that help explain changes in the target, or `null` for none.",
      "Custom models need complete history for all declared drivers, but future values only for",
      "prediction-required drivers in their approved contract. Built-ins retain their past/future feature rules."),
    hist_start_date = "The first period Finn learns from. Trim older history if early data is unreliable, or use `null` for all history.",
    combo_cleanup_date = "Removes series with no values after this date, such as discontinued products, or `null` to keep every series.",
    back_test_scenarios = "How many times Finn hides recent history and re-forecasts it to measure accuracy, or `null` to size it automatically.",
    back_test_spacing = "The number of periods between back-test rounds, or `null` for the default for your date grain.",
    negative_forecast = "When `false`, negative forecast values become 0. Use `true` for values that can be negative, such as margin or variance.",
    run_global_models = "Trains models that learn across all series together, which can help with many short or similar series, or `null` to let Finn decide.",
    run_local_models = "Trains a separate model for each series.",
    forecast_approach = "`bottoms_up` forecasts each series on its own; `standard_hierarchy` and `grouped_hierarchy` reconcile forecasts so totals add up across levels.",
    models_to_run = "The model types to try, or `null` for Finn's default models.",
    models_not_to_run = "Model types to leave out, or `null` to leave none out.",
    run_ensemble_models = "Whether Finn also trains ensemble models that combine other models' forecasts; separate from simple averaging.",
    recipes_to_run = "The feature recipes, or `null` to use all supported recipes.",
    pca = "Controls principal component analysis, or `null` for automatic behavior.",
    average_models = "Whether Finn also tries simple averages of its top models.",
    max_model_average = "The most models combined in one average.",
    feature_selection = "Controls driver feature selection.",
    clean_outliers = "Whether unusual spikes or dips in history are replaced before training, so one-off events don't distort the forecast.",
    clean_missing_values = "Whether missing values in history are filled in before training.",
    allow_hierarchical_forecast = "Lets the Agent try hierarchical forecasts that add up across series.",
    max_iter = paste("The total search iterations for this Agent version, per search (the global search and each series' search),",
      "not extra iterations. More iterations can improve accuracy but take longer and use more AI model requests."),
    weighted_mape_goal = "The target weighted MAPE as a decimal, such as `0.03` for 3%; the Agent can stop searching once it is reached.",
    llm = "The AI provider and model the Agent uses.",
    date_column = "The input column that holds dates.",
    date_format = "How dates are read, as an R date format, or `null` to auto-detect.",
    decimal_mark = "The decimal mark for numbers stored as text, or `null` to auto-detect.",
    fiscal_year_start = "The month number (1-12) the fiscal year starts; used for fiscal and seasonal features.",
    ""
  )
  value <- validate_field_value(cfg, key)
  if (is.null(value)) {
    auto <- switch(key,
      hist_end_date = validate_auto_hist_end(facts),
      run_ensemble_models = if (identical(cfg$mode, "standard")) "Auto may enable ensembles; set it to `false` unless you asked for ensemble models.",
      date_format = if (!is.null(facts$date_format)) sprintf("Auto-detected `%s`.", facts$date_format),
      decimal_mark = if (!is.null(facts$decimal_mark)) sprintf("Auto-detected `%s`.", facts$decimal_mark),
      NULL
    )
    if (!is.null(auto)) text <- paste(text, auto)
  } else if (identical(key, "run_ensemble_models") && identical(cfg$mode, "standard")) {
    text <- paste(text, if (isTRUE(value)) {
      "Ensembles are on, which adds training time."
    } else {
      "Kept `false` for standard runs unless you ask for ensembles."
    })
  } else if (identical(key, "max_iter") && !is.null(run$prior_max_iter)) {
    lower <- isTRUE(suppressWarnings(as.numeric(value) < as.numeric(run$prior_max_iter)))
    text <- paste(text, if (lower) {
      sprintf(paste(
        "Your last Agent search ran up to `%s`; a lower value can't undo iterations, so nothing new would run.",
        "Set it above `%s` to continue that Agent version."
      ), run$prior_max_iter, run$prior_max_iter)
    } else {
      sprintf(
        "Your last Agent search ran up to `%s`; a higher value continues that Agent version and runs only the extra iterations.",
        run$prior_max_iter
      )
    })
  } else if (identical(key, "weighted_mape_goal") && !is.null(run$prior_weighted_mape_goal)) {
    text <- paste(text, sprintf(paste(
      "Your last Agent search used `%s`. A stricter (lower) goal continues that Agent version and reopens series whose best",
      "model is global and miss it; a looser goal alone runs nothing new."
    ), run$prior_weighted_mape_goal))
  }
  text
}

#' Other run type to offer for a project with no forecasts yet
#'
#' Uses the size-based advice from [finn_recommend_settings()]: the Agent is
#' offered for small data, discouraged for very large data, and Standard is
#' offered as the faster choice for Agent configs.
#'
#' @param run_type Chosen run type, "standard" or "iterate".
#' @param rec Result of [finn_recommend_settings()], or NULL.
#' @return List with `run_type`, `label`, `reason`, and `launch`.
validate_new_project_alternative <- function(run_type, rec = NULL) {
  tier <- rec$tier %||% "small"
  if (identical(run_type, "standard")) {
    reason <- switch(tier,
      very_large = "Not recommended at this size: the Agent searches every series over several iterations, which can take days.",
      large = "Searches settings per series for better accuracy, but at this size it takes several times longer than standard and uses many Copilot requests.",
      rec$suggested_mode_reason %||% "Searches settings per series for better accuracy. Slower, and it uses Copilot requests."
    )
    return(list(run_type = "iterate", label = finn_run_type_label("iterate"), reason = reason, launch = "--mode=iterate"))
  }
  list(run_type = "standard", label = finn_run_type_label("standard"),
    reason = rec$suggested_mode_reason %||% "Faster and needs no AI provider; it picks the best of the selected models without the Agent's search.",
    launch = "--mode=standard")
}

#' Run type the review proposes and how it was chosen
#'
#' Combines the history-based recommendation from [finn_next_run()] with the
#' run type the user asked for. When they differ, the recommendation becomes
#' the other option so the user still sees it.
#'
#' @param next_run Result of [finn_next_run()].
#' @param run_type Run type to propose: "standard", "iterate", or "update".
#' @param cfg Merged config, used for the run type hint.
#' @param rec Result of [finn_recommend_settings()], or NULL.
#' @return List with `run_type`, `label`, `hint`, `situation`, `why`,
#'   `alternative`, `launch`, `recommended_run_type`, `prior_max_iter` and
#'   `prior_weighted_mape_goal` (set only when an iterate run can continue the
#'   current Agent version, that is when [finn_next_run()] returned a
#'   `continuation`), and `continuation_kind` for an iterate run on a current
#'   Agent version: the `kind` from [finn_iterate_continuation()], or
#'   `"last_update"` when the latest Agent forecast was an update, which no
#'   iterate run can continue.
validate_run_info <- function(next_run, run_type, cfg, rec = NULL) {
  recommended <- next_run$run_type
  situation <- next_run$situation
  if (identical(run_type, recommended)) {
    why <- next_run$why
    alternative <- if (identical(situation, "new_project")) validate_new_project_alternative(run_type, rec) else next_run$alternative
    launch <- next_run$launch
  } else {
    why <- NULL
    alternative <- list(run_type = recommended, label = paste(finn_run_type_label(recommended), "(recommended)"),
      reason = next_run$why %||% "", launch = next_run$launch)
    if (identical(situation, "new_project")) {
      alternative$reason <- validate_new_project_alternative(run_type, rec)$reason
      alternative$label <- finn_run_type_label(recommended)
    }
    launch <- if (identical(run_type, "iterate") && identical(situation, "agent_refresh")) {
      "--mode=iterate --new=true --confirm=true"
    } else {
      paste0("--mode=", run_type)
    }
  }
  iterate_current <- identical(situation, "agent_current") && identical(run_type, "iterate")
  cont <- if (iterate_current) next_run$continuation
  list(run_type = run_type, label = finn_run_type_label(run_type), hint = finn_run_type_hint(run_type, cfg),
    situation = situation, why = why, alternative = alternative, launch = launch,
    recommended_run_type = recommended,
    prior_max_iter = if (!is.null(cont)) next_run$prior_max_iter,
    prior_weighted_mape_goal = if (!is.null(cont)) next_run$prior_weighted_mape_goal,
    continuation_kind = if (iterate_current) {
      cont$kind %||% if (identical(next_run$last_runs$agent$mode, "update")) "last_update"
    })
}

#' Caller instruction for the other option in a review
#'
#' An option with a different run type is reviewed by rerunning this script
#' with that `--run_type`. An option with the same run type (a new Agent
#' version while iterating) would only repeat the same review, so its
#' instruction names its own launch flags and requires the user's explicit
#' confirmation instead.
#'
#' @param run Run info from [validate_run_info()].
#' @param run_type Run type under review.
#' @return One instruction string, or NULL when the review has no other option.
validate_alternative_action <- function(run, run_type) {
  alt <- run$alternative
  if (is.null(alt)) {
    return(NULL)
  }
  if (identical(alt$run_type, run_type)) {
    return(sprintf(
      "If the user prefers %s, it is the same run type, so do not rerun this script. Get their explicit confirmation, then after the config is saved start it with run_forecast.R --project=<name> %s.",
      alt$label, alt$launch %||% paste0("--mode=", alt$run_type)
    ))
  }
  sprintf("If the user prefers %s, rerun this script with --run_type=%s and show the new review before launching.", alt$label, alt$run_type)
}

#' Format a count for prose, with thousands separators
#'
#' @param x Number.
#' @return Text such as "1,200".
validate_number <- function(x) {
  format(as.numeric(unlist(x)[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Join words for a sentence: "A", "A and B", or "A, B and C"
#'
#' @param x Character vector.
#' @return Single string.
validate_join_words <- function(x) {
  x <- as.character(unlist(x))
  if (length(x) <= 1) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[[length(x)]])
}

#' Fiscal-year start month from a config
#'
#' @param cfg Merged config.
#' @return Integer month 1-12; 1 (calendar years) when unset or invalid.
validate_fys <- function(cfg) {
  fys <- suppressWarnings(as.integer(unlist(cfg$fiscal_year_start %||% 1L))[1])
  if (is.na(fys) || !fys %in% 1:12) 1L else fys
}

#' Fiscal year a date falls in, named by its ending calendar year
#'
#' @param date Date.
#' @param fys Fiscal-year start month (1-12); 1 means calendar years.
#' @return Integer year, for example 2024 for July 2023 when `fys` is 7.
validate_fiscal_year <- function(date, fys) {
  year <- as.integer(format(date, "%Y"))
  if (fys == 1) year else year + as.integer(as.integer(format(date, "%m")) >= fys)
}

#' Parse one date for display
#'
#' @param x Date or `YYYY-MM-DD` text, or NULL.
#' @return A Date, or NULL when `x` is missing or not a date.
validate_as_date <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  d <- tryCatch(as.Date(as.character(unlist(x)[[1]])), error = function(e) as.Date(NA))
  if (is.na(d)) NULL else d
}

#' Whether a date type is one finnts supports
#'
#' @param date_type Value to check.
#' @return TRUE for "day", "week", "month", "quarter", or "year".
validate_known_grain <- function(date_type) {
  isTRUE(date_type %in% c("day", "week", "month", "quarter", "year"))
}

#' Date a number of periods after another
#'
#' @param date Date.
#' @param date_type finnts date type.
#' @param n Whole number of periods, 0 or more.
#' @return Date, or NULL for an unknown date type.
validate_shift_period <- function(date, date_type, n) {
  if (!validate_known_grain(date_type)) {
    return(NULL)
  }
  seq(date, by = date_type, length.out = n + 1)[[n + 1]]
}

#' Plain label for one period
#'
#' Months read "Jan 2023", weeks and days "Jan 2, 2023", and years "2023".
#' Quarters are fiscal ("Q1 FY24", fiscal years named by their ending year)
#' when the fiscal year starts in a calendar-quarter month, calendar
#' ("Q2 2023") for January starts, and otherwise the quarter's first month.
#' Month names are English in every locale.
#'
#' @param date Date.
#' @param date_type finnts date type.
#' @param fys Fiscal-year start month (1-12).
#' @return Label text; the ISO date for an unknown date type.
validate_period_label <- function(date, date_type, fys = 1) {
  if (!validate_known_grain(date_type)) {
    return(as.character(date))
  }
  month <- as.integer(format(date, "%m"))
  year <- format(date, "%Y")
  month_year <- sprintf("%s %s", month.abb[[month]], year)
  switch(date_type,
    quarter = if ((fys - 1) %% 3 != 0) {
      month_year
    } else if (fys == 1) {
      sprintf("Q%d %s", (month - 1L) %/% 3L + 1L, year)
    } else {
      sprintf("Q%d FY%02d", (month - fys) %% 12L %/% 3L + 1L, validate_fiscal_year(date, fys) %% 100L)
    },
    year = year,
    week = ,
    day = sprintf("%s %d, %s", month.abb[[month]], as.integer(format(date, "%d")), year),
    month_year
  )
}

#' Span label for a run of periods
#'
#' @param start,end Dates of the first and last period.
#' @param date_type finnts date type.
#' @param fys Fiscal-year start month (1-12).
#' @return "Jan 2023 to Dec 2023", or one label when both are the same period.
validate_span_label <- function(start, end, date_type, fys = 1) {
  first <- validate_period_label(start, date_type, fys)
  last <- validate_period_label(end, date_type, fys)
  if (identical(first, last)) first else paste(first, "to", last)
}

#' Period count with its unit
#'
#' @param n Number of periods.
#' @param date_type finnts date type; unknown types read "periods".
#' @return Text such as "1 month" or "8 quarters".
validate_count_label <- function(n, date_type) {
  unit <- if (validate_known_grain(date_type)) date_type else "period"
  one <- isTRUE(suppressWarnings(as.numeric(unlist(n)[1])) == 1)
  sprintf("%s %s%s", format(unlist(n)[1]), unit, if (one) "" else "s")
}

#' First and last forecast period after history ends
#'
#' @param hist_end Last history period (Date or text), or NULL.
#' @param date_type finnts date type.
#' @param horizon Forecast horizon in periods.
#' @return List with `start` and `end` Dates, or NULL when any input is unknown.
validate_forecast_window <- function(hist_end, date_type, horizon) {
  end <- validate_as_date(hist_end)
  h <- suppressWarnings(as.integer(unlist(horizon))[1])
  if (is.null(end) || is.na(h) || h < 1 || !validate_known_grain(date_type)) {
    return(NULL)
  }
  list(start = validate_shift_period(end, date_type, 1), end = validate_shift_period(end, date_type, h))
}

#' Fiscal-year note for a forecast ending on a date
#'
#' @param end Date of the last forecast period.
#' @param date_type finnts date type.
#' @param fys Fiscal-year start month (1-12).
#' @return ", through the end of FY24" when `end` is its fiscal year's last
#'   period, ", ends in FY24" otherwise, or "" for calendar fiscal years
#'   (`fys` 1) and yearly data.
validate_fiscal_suffix <- function(end, date_type, fys) {
  if (fys == 1 || identical(date_type, "year")) {
    return("")
  }
  fy <- validate_fiscal_year(end, fys)
  last <- validate_fiscal_year(validate_shift_period(end, date_type, 1), fys) > fy
  sprintf(if (last) ", through the end of FY%02d" else ", ends in FY%02d", fy %% 100L)
}

#' Date history ends at for this review
#'
#' Standard and Agent runs resolve an unset `hist_end_date` the same way: to
#' the latest date in the file (see [validate_auto_hist_end()]).
#'
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()].
#' @return `hist_end_date` when set; otherwise the latest date in the file,
#'   or the last period with actuals when the file's dates are unknown. Text,
#'   or NULL when unknown.
validate_resolved_hist_end <- function(cfg, facts) {
  end <- cfg$hist_end_date %||% facts$date_max %||% facts$last_actual_date
  if (is.null(end) || !length(end)) NULL else as.character(unlist(end)[[1]])
}

#' Display-only history facts for the review
#'
#' Counts the distinct periods in the history window, from the first period
#' used (`hist_start_date`, or the first actual) through `hist_end`. The
#' window includes rows that only carry driver values, because runs train on
#' every period up to the cutoff. Also counts the new actuals after the last
#' run's history end. Never blocks the review: an unreadable input returns an
#' empty list.
#'
#' @param data Checked data frame with a `Date` column, or NULL.
#' @param cfg Merged config.
#' @param hist_end History end for this review, from [validate_resolved_hist_end()].
#' @param prior_end History end of the last run, or NULL.
#' @return List with `history_start` (text) and `history_periods` (integer)
#'   when history is known, and `new_actuals` (`count`, `first`, `last`) when
#'   periods with actuals follow `prior_end`.
validate_history_facts <- function(data, cfg, hist_end, prior_end = NULL) {
  tryCatch({
    dates <- as.Date(data$Date)
    actual <- sort(unique(dates[!is.na(dates) & !is.na(data[[cfg$target_variable]])]))
    periods <- sort(unique(dates[!is.na(dates)]))
    end <- validate_as_date(hist_end)
    out <- list()
    if (length(actual) && !is.null(end)) {
      start <- max(actual[[1]], validate_as_date(cfg$hist_start_date) %||% actual[[1]])
      used <- periods[periods >= start & periods <= end]
      if (length(used)) out <- list(history_start = as.character(used[[1]]), history_periods = length(used))
      prior <- validate_as_date(prior_end)
      new <- if (!is.null(prior)) actual[actual > prior & actual <= end] else actual[0]
      if (length(new)) {
        out$new_actuals <- list(count = length(new), first = as.character(new[[1]]), last = as.character(new[[length(new)]]))
      }
    }
    out
  }, error = function(e) list())
}

#' Last finished run to compare the review with
#'
#' Standard reviews compare with the last finished standard run; Agent iterate
#' and update reviews compare with the last finished Agent forecast. A first
#' Agent run on a standard-only project has nothing to compare with.
#'
#' @param next_run Result of [finn_next_run()].
#' @param run_type Run type under review.
#' @return The matching `last_runs` entry with its config merged over the
#'   defaults, or NULL when there is none or its config cannot be read.
validate_last_run <- function(next_run, run_type) {
  last <- next_run$last_runs[[if (identical(run_type, "standard")) "standard" else "agent"]]
  if (!is.list(last) || !is.list(last$config)) {
    return(NULL)
  }
  last$config <- tryCatch(finn_merge_config(last$config), error = function(e) NULL)
  if (is.null(last$config)) NULL else last
}

#' Plain date of a run, such as "Oct 3, 2026"
#'
#' @param created_at Run creation time in ISO 8601, or NULL.
#' @return Label text, or "" when the time cannot be read.
validate_run_date_label <- function(created_at) {
  d <- validate_as_date(substr(as.character(created_at %||% ""), 1, 10))
  if (is.null(d)) "" else validate_period_label(d, "day")
}

#' Whether an Agent iterate review starts a new Agent version
#'
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return TRUE when the launch arguments start a new Agent version.
validate_new_version <- function(run, run_type) {
  identical(run_type, "iterate") && grepl("--new=true", run$launch %||% "", fixed = TRUE)
}

#' Whether an Agent iterate review is for the current Agent version
#'
#' True for an iterate run on a project whose finished Agent version has no
#' newer actuals (situation `agent_current`) and a known
#' `run$continuation_kind`, unless the launch starts a new version. Whether
#' it actually continues depends on that kind.
#'
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return Logical.
validate_current_version <- function(run, run_type) {
  identical(run_type, "iterate") && identical(run$situation, "agent_current") && !is.null(run$continuation_kind) &&
    !validate_new_version(run, run_type)
}

#' Whether an Agent iterate review continues the current Agent version
#'
#' Other changed settings, or a latest Agent forecast that was an update,
#' mean the run needs a new Agent version instead.
#'
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return TRUE when only the search budget or goal changes on that version.
validate_continuing <- function(run, run_type) {
  validate_current_version(run, run_type) && !isTRUE(run$continuation_kind %in% c("other_changes", "last_update"))
}

#' Forecast type cell for the at-a-glance table
#'
#' @param cfg Merged config.
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return Markdown text.
validate_forecast_type_text <- function(cfg, run, run_type) {
  iterations <- format(unlist(cfg$agent$max_iter %||% 3))
  if (identical(run_type, "standard")) {
    return("**Standard**")
  }
  if (identical(run_type, "update")) {
    return("**Agent update** with your new actuals")
  }
  if (validate_new_version(run, run_type)) {
    "**Agent iterate**, new Agent version (the search starts over)"
  } else if (validate_current_version(run, run_type) && identical(run$continuation_kind, "last_update")) {
    "**Agent iterate**, needs a new Agent version because your last Agent forecast was an update"
  } else if (validate_current_version(run, run_type) && !validate_continuing(run, run_type)) {
    "**Agent iterate**, needs a new Agent version because other settings changed"
  } else if (validate_continuing(run, run_type)) {
    sprintf("**Agent iterate**, continues your current Agent version up to %s iterations", iterations)
  } else {
    sprintf("**Agent iterate**, up to %s iterations", iterations)
  }
}

#' Forecast type of the last run, for the Last run column
#'
#' @param last Result of [validate_last_run()].
#' @return Text such as "Agent iterate (version 1, up to 3 iterations)".
validate_last_type_text <- function(last) {
  version <- if (!is.null(last$agent_version)) format(unlist(last$agent_version))
  mode <- last$mode %||% "unknown"
  if (identical(mode, "standard")) {
    "Standard"
  } else if (identical(mode, "update")) {
    paste0("Agent update", if (!is.null(version)) sprintf(" (version %s)", version) else "")
  } else if (identical(mode, "iterate")) {
    sprintf("Agent iterate (%sup to %s iterations)", if (!is.null(version)) sprintf("version %s, ", version) else "",
      format(unlist(last$config$agent$max_iter %||% 3)))
  } else {
    "-"
  }
}

#' At-a-glance table of the forecast's core settings
#'
#' One plain-language row each for the target, series columns (with the series
#' count), date grain, history used, forecast period (with a fiscal-year note),
#' drivers, enrolled custom models, and forecast type, each with its exact
#' config value. With a last run, a "Last run" column reads "same" for rows
#' that did not change; the forecast type row always names the last run's type.
#' Last run cells use that run's own date grain and fiscal year. Its history
#' cutoff and forecast window are shown only when it saved `hist_end_date`;
#' otherwise finnts ended history at that file's latest date, which is not
#' saved, so the cells show its last actuals and horizon length and never
#' read "same". Derived facts are display-only.
#'
#' @param cfg Merged config.
#' @param facts Review facts: data facts plus [validate_history_facts()].
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @param last Result of [validate_last_run()], or NULL.
#' @return Markdown string.
validate_overview_section <- function(cfg, facts, run, run_type, last = NULL) {
  prev <- last$config
  fys <- validate_fys(cfg)
  date_type <- cfg$date_type
  labels <- validate_setting_labels()
  value_of <- function(conf, key) validate_json_value(key, validate_field_value(conf, key))
  setting <- function(key) sprintf("`%s` = `%s`", key, validate_escape_cell(value_of(cfg, key)))
  same <- function(key) !is.null(prev) && identical(value_of(cfg, key), value_of(prev, key))
  names_text <- function(x) validate_escape_cell(paste(unlist(x), collapse = ", "))
  grain <- function(x) {
    grains <- c(day = "Daily", week = "Weekly", month = "Monthly", quarter = "Quarterly", year = "Yearly")
    if (validate_known_grain(x)) grains[[x]] else names_text(x)
  }
  rows <- list()
  # Adds one row; `previous` is the Last run text shown when the row changed.
  add <- function(label, plan, setting_text, previous = "", unchanged = FALSE) {
    rows[[length(rows) + 1]] <<- list(label = label, plan = plan, setting = setting_text,
      previous = if (unchanged) "same" else if (nzchar(previous)) previous else "-")
  }

  add(labels[["target_variable"]], sprintf("**%s**", names_text(cfg$target_variable)), setting("target_variable"),
    names_text(prev$target_variable), same("target_variable"))
  series <- if (!is.null(facts$n_series)) sprintf(" (%s series)", validate_number(facts$n_series)) else ""
  add(labels[["combo_variables"]], sprintf("**%s**%s", names_text(cfg$combo_variables), series), setting("combo_variables"),
    names_text(prev$combo_variables), same("combo_variables"))
  add(labels[["date_type"]], sprintf("**%s**", grain(date_type)), setting("date_type"),
    if (!is.null(prev)) grain(prev$date_type) else "", same("date_type"))

  end <- validate_resolved_hist_end(cfg, facts)
  end_date <- validate_as_date(end)
  auto <- is.null(cfg$hist_end_date)
  start_date <- validate_as_date(facts$history_start)
  history <- if (!is.null(start_date) && !is.null(end_date) && !is.null(facts$history_periods)) {
    sprintf("**%s** (%s%s)", validate_span_label(start_date, end_date, date_type, fys),
      validate_count_label(facts$history_periods, date_type), if (auto) ", automatic" else "")
  } else if (!is.null(end_date)) {
    sprintf("**through %s**%s", validate_period_label(end_date, date_type, fys), if (auto) " (automatic)" else "")
  } else {
    "**automatic**"
  }
  # The last run's own grain and fiscal year label its cells. Its cutoff is known only when it saved a
  # hist_end_date; otherwise finnts ended history at that file's latest date, which is not saved, so
  # `last$end` (its last actuals) must not stand in for it.
  prev_type <- prev$date_type %||% date_type
  prev_fys <- if (!is.null(prev)) validate_fys(prev) else fys
  last_end <- validate_as_date(last$end)
  cutoff <- validate_as_date(prev$hist_end_date)
  prev_start <- validate_as_date(prev$hist_start_date)
  same_end <- !is.null(cutoff) && identical(end, as.character(cutoff)) && same("date_type")
  add(labels[["hist_end_date"]], history, setting("hist_end_date"),
    if (!is.null(cutoff) || !is.null(last_end)) {
      paste0(if (!is.null(prev_start)) sprintf("from %s ", validate_period_label(prev_start, prev_type, prev_fys)) else "",
        if (!is.null(cutoff)) "through " else "actuals through ",
        validate_period_label(cutoff %||% last_end, prev_type, prev_fys))
    } else {
      ""
    },
    same_end && same("hist_start_date"))

  horizon <- validate_count_label(cfg$forecast_horizon %||% "?", date_type)
  window <- validate_forecast_window(end, date_type, cfg$forecast_horizon)
  forecast <- if (!is.null(window)) {
    sprintf("**%s** (%s%s)", validate_span_label(window$start, window$end, date_type, fys), horizon,
      validate_fiscal_suffix(window$end, date_type, fys))
  } else {
    sprintf("**%s** after history ends", horizon)
  }
  last_window <- if (!is.null(cutoff)) validate_forecast_window(cutoff, prev_type, prev$forecast_horizon)
  add(labels[["forecast_horizon"]], forecast, setting("forecast_horizon"),
    if (!is.null(last_window)) {
      sprintf("%s (%s)", validate_span_label(last_window$start, last_window$end, prev_type, prev_fys),
        validate_count_label(prev$forecast_horizon, prev_type))
    } else if (!is.null(prev)) {
      validate_count_label(prev$forecast_horizon %||% "?", prev_type)
    } else {
      ""
    },
    same_end && same("forecast_horizon"))

  drivers <- unlist(cfg$external_regressors)
  add(labels[["external_regressors"]], if (length(drivers)) sprintf("**%s** (from your file)", names_text(drivers)) else "**None**",
    setting("external_regressors"),
    if (is.null(prev)) "" else if (length(unlist(prev$external_regressors))) names_text(prev$external_regressors) else "none",
    same("external_regressors"))

  custom <- finn_custom_labels(cfg)
  prev_custom <- if (!is.null(prev)) finn_custom_labels(prev) else character()
  if (length(custom) || length(prev_custom)) {
    role <- if (identical(run_type, "standard")) "they compete with Finn's models" else "the Agent tries them alongside its own models"
    add("Your custom models", if (length(custom)) sprintf("%s: %s", paste(sprintf("**%s**", custom), collapse = ", "), role) else "**None**",
      setting("custom_models"), finn_custom_label_text(prev_custom), same("custom_models"))
  }
  rows[[length(rows) + 1]] <- list(label = "Forecast type", plan = validate_forecast_type_text(cfg, run, run_type),
    setting = sprintf("`mode` = `%s`", validate_json_value("mode", cfg$mode)),
    previous = if (!is.null(last)) validate_last_type_text(last) else "-")

  if (is.null(last)) {
    header <- c("| | Plan | Setting |", "|---|---|---|")
    lines <- vapply(rows, function(r) sprintf("| %s | %s | %s |", r$label, r$plan, r$setting), character(1))
  } else {
    header <- c(sprintf("| | %s | Last run | Setting |", if (identical(run_type, "update")) "This update" else "This run"),
      "|---|---|---|---|")
    lines <- vapply(rows, function(r) sprintf("| %s | %s | %s | %s |", r$label, r$plan, r$previous, r$setting), character(1))
  }
  paste(c("### Your forecast at a glance", "", header, lines), collapse = "\n")
}

#' One-line data check for the settings review
#'
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()], optionally with `n_columns`.
#' @param rec Result of [finn_recommend_settings()], or NULL, for the data size.
#' @param warnings Non-blocking warnings, counted in the validation result.
#' @param notes Notes about how the file was read, listed below the line.
#' @return Markdown string.
validate_data_check <- function(cfg, facts, rec = NULL, warnings = character(), notes = character()) {
  counts <- c(
    if (!is.null(facts$n_rows)) sprintf("%s rows", validate_number(facts$n_rows)),
    if (!is.null(facts$n_columns)) sprintf("%s columns", validate_number(facts$n_columns)),
    if (!is.null(facts$n_series)) sprintf("%s series", validate_number(facts$n_series))
  )
  size <- if (!is.null(rec$tier)) {
    c(small = "small (up to 100 series)", large = "large (100+ series)", very_large = "very large (1,000+ series)")[[rec$tier]]
  }
  date_order <- cfg$date_format %||% facts$date_format
  parts <- c(
    if (length(counts)) paste(counts, collapse = ", "),
    if (!is.null(facts$date_min)) sprintf("dates `%s` to `%s`", facts$date_min, facts$date_max),
    if (!is.null(facts$last_actual_date) && !identical(facts$last_actual_date, facts$date_max)) {
      sprintf("last period with actuals `%s` (later rows only carry driver values)", facts$last_actual_date)
    },
    if (length(date_order) && nzchar(date_order)) sprintf("date order `%s`", date_order),
    if (!is.null(cfg$date_type)) sprintf("cadence `%s`", cfg$date_type),
    if (!is.null(size)) sprintf("data size %s", size),
    sprintf("validation passed%s", if (length(warnings)) {
      sprintf(" with %d warning%s", length(warnings), if (length(warnings) == 1) "" else "s")
    } else {
      ""
    })
  )
  lines <- sprintf("**Data check:** %s.", paste(parts, collapse = "; "))
  if (length(notes)) lines <- c(lines, "", paste0("- ", vapply(notes, validate_escape_cell, character(1), USE.NAMES = FALSE)))
  paste(lines, collapse = "\n")
}

#' Heads-up block for warnings and notices
#'
#' @param items Warning and notice sentences.
#' @return "**Heads up:** ..." for one item, a bulleted block for several, or
#'   NULL when there are none.
validate_heads_up <- function(items) {
  items <- vapply(as.character(unlist(items)), validate_escape_cell, character(1), USE.NAMES = FALSE)
  if (!length(items)) {
    return(NULL)
  }
  if (length(items) == 1) {
    return(sprintf("**Heads up:** %s", items))
  }
  paste(c("**Heads up**", "", paste0("- ", items)), collapse = "\n")
}

#' Rule for changing settings on the current Agent version
#'
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return One paragraph for an Agent iterate on the current Agent version,
#'   else NULL.
validate_version_rule <- function(run, run_type) {
  if (!validate_current_version(run, run_type)) {
    return(NULL)
  }
  if (identical(run$continuation_kind, "last_update")) {
    return(paste("Your last Agent forecast was an update, so this Agent version can't search further.",
      "Any iterate run, including one with more **Max iterations**, needs a new Agent version, which restarts the search",
      "and needs your confirmation."))
  }
  if (!validate_continuing(run, run_type)) {
    return(paste("Only **Max iterations** and **Accuracy goal** can change on your current Agent version.",
      "Other settings changed, so this run needs a new Agent version, which restarts the search and needs your confirmation."))
  }
  sprintf(paste("This run continues your current Agent version, so only **Max iterations** (set it above %s, your last search's total,",
    "to search longer) and **Accuracy goal** can change. Changing any other setting needs a new Agent version, which restarts the search."),
  format(unlist(run$prior_max_iter)))
}

#' Settings to review and the defaults table
#'
#' Splits the settings outside the at-a-glance table by category. A setting is
#' listed for review when it differs from Finn's default or is one the user
#' always confirms: `run_ensemble_models` for standard runs (the ensemble
#' policy), `max_iter` and `weighted_mape_goal` for Agent iterate, and `llm`
#' for every Agent run. Everything else goes to one compact defaults table.
#' An Agent iterate on the current Agent version opens the review with
#' [validate_version_rule()].
#'
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()].
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return List with `review` and `defaults` markdown strings, each NULL when empty.
validate_settings_sections <- function(cfg, facts, run, run_type) {
  defaults <- finn_merge_config(list())
  always <- c(
    if (identical(run_type, "standard")) "run_ensemble_models",
    if (identical(run_type, "iterate")) c("max_iter", "weighted_mape_goal"),
    if (!identical(run_type, "standard")) "llm"
  )
  overview <- validate_setting_categories()[["Forecast overview"]]
  groups <- validate_setting_groups(cfg$mode)
  review <- character()
  default_rows <- character()
  for (title in names(groups)) {
    rows <- character()
    for (key in setdiff(groups[[title]], overview)) {
      value <- validate_field_value(cfg, key)
      cell <- validate_value_cell(key, value)
      differs <- !identical(validate_json_value(key, value), validate_json_value(key, validate_field_value(defaults, key)))
      if (key %in% always || differs) {
        rows <- c(rows, sprintf("| %s | %s | %s |", validate_setting_name(key), cell,
          validate_escape_cell(validate_field_explanation(key, cfg, facts, run))))
      } else {
        default_rows <- c(default_rows, sprintf("| %s | %s | %s |", title, validate_setting_name(key), cell))
      }
    }
    if (length(rows)) review <- c(review, sprintf("#### %s", title), "", "| Setting | Value | What it means |", "|---|---|---|", rows, "")
  }
  rule <- validate_version_rule(run, run_type)
  list(
    review = if (length(review)) {
      paste(c("### Settings to review", "", if (!is.null(rule)) c(rule, ""), review[-length(review)]), collapse = "\n")
    },
    defaults = if (length(default_rows)) {
      paste(c("### Everything else uses Finn's defaults", "", "No action needed. Ask about any of these if you're curious.", "",
        "| Category | Setting | Value |", "|---|---|---|", default_rows), collapse = "\n")
    }
  )
}

#' Example replies that get more from a run
#'
#' Suggests adding drivers and forecasting through the end of the fiscal year
#' that holds the period after the forecast (not for Agent updates or a
#' continued Agent version, where those changes need a new version), a longer
#' Agent search for iterate runs (above the last search's total when the
#' version continues; not when the latest Agent forecast was an update, so
#' more iterations cannot continue that version), and asking where
#' custom models helped for Agent runs. SKILL.md maps each reply to its config
#' setting.
#'
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()].
#' @param run Run info from [validate_run_info()], or NULL.
#' @param run_type Run type under review.
#' @return Character vector of tips, possibly empty.
validate_review_tips <- function(cfg, facts, run, run_type) {
  tips <- character()
  target <- paste(unlist(cfg$target_variable), collapse = "")
  if (!identical(run_type, "update") && !validate_continuing(run, run_type)) {
    drivers <- unlist(cfg$external_regressors)
    tips <- c(tips, if (length(drivers)) {
      sprintf("You're using %s as %s. If another column in your file moves with `%s`, say `also use <column> as a driver`.",
        validate_join_words(drivers), if (length(drivers) == 1) "a driver" else "drivers", target)
    } else {
      sprintf("Add a driver: if another column in your file moves with `%s`, say `use <column> as a driver`.", target)
    })
    window <- validate_forecast_window(validate_resolved_hist_end(cfg, facts), cfg$date_type, cfg$forecast_horizon)
    if (!is.null(window) && !identical(cfg$date_type, "year")) {
      # The end of the fiscal year holding the next period: finishes this year, or adds the next one.
      fys <- validate_fys(cfg)
      tips <- c(tips, sprintf("Need a longer outlook? Say `forecast through %s %d`.",
        month.abb[[if (fys == 1) 12L else fys - 1L]], validate_fiscal_year(validate_shift_period(window$end, cfg$date_type, 1), fys)))
    }
  }
  if (identical(run_type, "iterate") && !identical(run$continuation_kind, "last_update")) {
    # On a continued version, more iterations only help above the last search's total.
    budgets <- suppressWarnings(as.numeric(c(unlist(cfg$agent$max_iter %||% 3)[1],
      if (validate_continuing(run, run_type)) unlist(run$prior_max_iter)[1])))
    budgets <- budgets[!is.na(budgets)]
    if (length(budgets)) {
      tips <- c(tips, sprintf("Want the Agent to search longer? Say `use %s iterations`. Each extra iteration adds run time and AI model requests.",
        format(max(budgets) + 2)))
    }
  }
  if (!identical(run_type, "standard") && length(finn_custom_labels(cfg))) {
    tips <- c(tips, "After the run, ask which models Finn picked for each series to see where your custom models helped.")
  }
  tips
}

#' Settings review for user confirmation
#'
#' Renders, in order: an intro naming the run type, `Your forecast at a glance`
#' (with a Last run column for existing projects), new actuals since the last
#' run, a one-line data check, a heads-up block, `Your custom models` when
#' custom models are enrolled, `Settings to review` by category, `Suggestions`,
#' `Ways to get more from this run`, `On this computer`, `Everything else uses
#' Finn's defaults`, and a fixed footer. Every review setting renders exactly
#' once. Values use config.json notation. Display only: nothing is saved.
#'
#' @param cfg Merged, validated config.
#' @param facts Data facts from [finn_check_config()], optionally with
#'   `n_columns` and the [validate_history_facts()] fields; `custom_models`
#'   lists enrolled custom models.
#' @param plan Parallel plan from [finn_plan_with_others()].
#' @param rec Result of [finn_recommend_settings()], or NULL.
#' @param warnings Non-blocking warnings to list.
#' @param notes Notes about how the file was read.
#' @param run Result of [validate_run_info()], or NULL to name the run type
#'   from the config mode only.
#' @param last_run Result of [validate_last_run()], or NULL when the project
#'   has no finished run of the same type.
#' @return Single markdown string.
validate_settings_review <- function(cfg, facts, plan, rec = NULL, warnings = character(), notes = character(), run = NULL,
                                     last_run = NULL) {
  run_type <- run$run_type %||% if (identical(cfg$mode, "agent")) "iterate" else "standard"
  label <- finn_run_type_label(run_type)
  intro <- if (is.null(run) || identical(run$situation, "new_project")) {
    sprintf("Here are the proposed **%s** forecast settings for project `%s`.", label, cfg$project_name)
  } else {
    sprintf("Here are the proposed settings for the next **%s** forecast for existing project `%s`.", label, cfg$project_name)
  }
  sections <- paste(c(intro, "",
    sprintf("- **Run type:** %s. %s", label, run$hint %||% finn_run_type_hint(run_type, cfg)),
    if (!is.null(run$why) && nzchar(run$why)) sprintf("- **Why:** %s", run$why),
    if (!is.null(run$alternative)) {
      sprintf("- **Other option:** %s. %s Tell me if you prefer it.", run$alternative$label, run$alternative$reason)
    }
  ), collapse = "\n")
  sections <- c(sections, validate_overview_section(cfg, facts, run, run_type, last_run))

  new <- facts$new_actuals
  if (!is.null(last_run) && !is.null(new$count)) {
    when <- validate_run_date_label(last_run$created_at)
    first <- validate_as_date(new$first)
    final <- validate_as_date(new$last)
    span <- if (!is.null(first) && !is.null(final)) {
      sprintf(" (%s)", validate_span_label(first, final, cfg$date_type, validate_fys(cfg)))
    } else {
      ""
    }
    sections <- c(sections, sprintf("**New since your last run%s:** %s more %s of actuals%s.",
      if (nzchar(when)) sprintf(" (%s)", when) else "", format(new$count),
      sub("^\\S+ ", "", validate_count_label(new$count, cfg$date_type)), span))
  }
  sections <- c(sections, validate_data_check(cfg, facts, rec, warnings, notes))

  cutoff <- is.null(cfg$hist_end_date) && !is.null(facts$last_actual_date) &&
    !is.null(facts$date_max) && !identical(as.character(facts$last_actual_date), as.character(facts$date_max))
  sections <- c(sections, validate_heads_up(c(
    if (cutoff) paste("`hist_end_date` is `null`.", validate_auto_hist_end(facts)),
    warnings,
    if (validate_new_version(run, run_type)) {
      paste("This run starts a **new Agent version**. The search starts over instead of building on earlier runs,",
        "so it takes about as long as a first Agent run. Earlier versions and their files stay saved.")
    }
  )))
  sections <- c(sections, validate_custom_models_section(facts$custom_models, run_type))
  settings <- validate_settings_sections(cfg, facts, run, run_type)
  sections <- c(sections, settings$review)

  suggestions <- c(
    vapply(rec$recommendations %||% list(), function(r) {
      sprintf("- `%s`: `%s`. %s", r$key, validate_escape_cell(validate_json_value(r$key, r$value)), validate_escape_cell(r$reason))
    }, character(1)),
    if (length(rec$notes)) paste0("- ", vapply(rec$notes, validate_escape_cell, character(1), USE.NAMES = FALSE))
  )
  if (length(suggestions)) {
    sections <- c(sections, paste(c("### Suggestions", "",
      "These are optional; nothing changes unless you accept them.", "", suggestions), collapse = "\n"))
  }
  tips <- validate_review_tips(cfg, facts, run, run_type)
  if (length(tips)) sections <- c(sections, paste(c("### Ways to get more from this run", "", paste0("- ", tips)), collapse = "\n"))

  computer <- c(
    if (!is.null(rec$estimated_runtime)) sprintf("- **Estimated run time:** %s (a rough guide, not a promise)", rec$estimated_runtime),
    if (!is.null(plan$reason)) sprintf("- **Computer use:** %s", plan$reason)
  )
  if (length(computer)) sections <- c(sections, paste(c("### On this computer", "", computer), collapse = "\n"))
  sections <- c(sections, settings$defaults)

  sections <- c(sections, paste(
    "The data passed validation and these settings are ready to run on this computer.",
    "Reply `run` to start the forecast, or tell me which input value you want to change."
  ))
  paste(sections, collapse = "\n\n")
}

#' Custom models section of the settings review
#'
#' Lists each enrolled model with its version, type (with what local or
#' global means), the file columns it uses, and its rule, then which of its
#' drivers are historical-only or need future values.
#'
#' @param models Summaries from [finn_check_custom_models()].
#' @param run_type Run type under review.
#' @return Markdown string, or NULL when no custom models are enrolled.
validate_custom_models_section <- function(models, run_type = "standard") {
  if (!length(models)) {
    return(NULL)
  }
  short <- function(x) {
    x <- trimws(gsub("\\s+", " ", paste(x %||% "", collapse = " ")))
    if (nchar(x) > 200) paste0(substr(x, 1, 197), "...") else x
  }
  list_text <- function(x) if (length(unlist(x))) paste(unlist(x), collapse = ", ") else "none"
  # Model type with a plain meaning when it is only local or only global.
  type_text <- function(x) {
    types <- as.character(unlist(x))
    meaning <- c(local = "one model per series", global = "learns across series")
    if (length(types) == 1 && types %in% names(meaning)) sprintf("%s (%s)", types, meaning[[types]]) else list_text(types)
  }
  rows <- vapply(models, function(m) {
    sprintf("| `%s` | `%s` | %s | %s | %s |", m$name, m$version, validate_escape_cell(type_text(m$model_type)),
      validate_escape_cell(list_text(m$drivers)), validate_escape_cell(short(m$interpretation)))
  }, character(1))
  note <- if (identical(run_type, "standard")) {
    "Saved, approved custom models from this project. They train and compete with Finn's models in this run."
  } else {
    paste("Saved, approved custom models from this project. The Agent tries them alongside its own models;",
      "later iterate and update runs of this Agent version must use the same ones.")
  }
  availability <- vapply(models, function(m) {
    sprintf("- `%s`: Historical-only: %s. Future-required: %s.", m$name,
      list_text(m$historical_only_drivers), list_text(m$future_required_drivers %||% m$drivers))
  }, character(1))
  paste(c("### Your custom models", "", note, "",
    "| Model | Version | Type | Uses from your file | What it does |", "|---|---|---|---|---|", rows,
    "", availability), collapse = "\n")
}

#' Revision view listing only the settings that changed
#'
#' Renders a `Changes` table with plain labels, previous and new values
#' (including the enrolled custom models) in the review's category order,
#' a heads-up block for current warnings, and a footer offering the full
#' configuration.
#'
#' @param previous Merged config the user reviewed before (the saved config).
#' @param cfg Merged, validated proposed config.
#' @param facts Data facts from [finn_check_config()].
#' @param warnings Non-blocking warnings for the proposed config.
#' @return Single markdown string, or NULL when the run mode changed and the
#'   full review must be shown instead.
validate_settings_changes <- function(previous, cfg, facts = list(), warnings = character()) {
  if (!identical(previous$mode, cfg$mode)) {
    return(NULL)
  }
  rows <- character()
  for (key in validate_setting_order(cfg$mode)) {
    old <- validate_json_value(key, validate_field_value(previous, key))
    new <- validate_json_value(key, validate_field_value(cfg, key))
    if (!identical(old, new)) {
      rows <- c(rows, sprintf("| %s | `%s` | `%s` | %s |", validate_setting_name(key), validate_escape_cell(old),
        validate_escape_cell(new), validate_escape_cell(validate_field_explanation(key, cfg, facts))))
    }
  }
  custom_old <- finn_custom_labels(previous)
  custom_new <- finn_custom_labels(cfg)
  if (!identical(custom_old, custom_new)) {
    rows <- c(rows, sprintf("| Your custom models (`custom_models`) | %s | %s | %s |", validate_escape_cell(finn_custom_label_text(custom_old)),
      validate_escape_cell(finn_custom_label_text(custom_new)),
      "Saved custom models this project's runs train alongside Finn's models (`name@version`)."))
  }
  sections <- if (length(rows)) {
    c("Updated the settings.", paste(c("### Changes", "",
      "| Setting | Previous | New value | What it means |", "|---|---|---|---|", rows), collapse = "\n"))
  } else {
    "No settings changed. The previously reviewed settings remain ready to run."
  }
  sections <- c(sections, validate_heads_up(warnings))
  sections <- c(sections, paste(
    "The updated settings are ready to run on this computer. Reply `run` to start the forecast,",
    "tell me another input value to change, or say `show full configuration` to review every input."
  ))
  paste(sections, collapse = "\n\n")
}

#' Saved project config to compare a proposed config against
#'
#' @param paths Project paths.
#' @param config_file Config file being validated.
#' @return Merged saved config, or NULL when `config_file` is the saved config,
#'   no config is saved yet, or the saved config cannot be read.
validate_previous_config <- function(paths, config_file) {
  saved <- file.path(paths$dir, "config.json")
  if (!file.exists(saved) || identical(finn_norm(config_file), finn_norm(saved))) {
    return(NULL)
  }
  tryCatch(finn_merge_config(finn_read_project_config(paths, saved)), error = function(e) NULL)
}

finn_main(function(args) {
  paths <- finn_project_from_args(args)
  config_file <- finn_arg(args, "config") %||% file.path(paths$dir, "config.json")
  cfg <- finn_read_project_config(paths, config_file)
  checked <- finn_load_checked(cfg, root = paths$root)
  cfg <- checked$config
  data <- list(config_file = finn_norm(config_file), errors = checked$errors, warnings = checked$warnings,
    notes = checked$notes, facts = checked$facts)
  if (length(checked$errors)) {
    return(finn_result(FALSE, "invalid", "The config has problems to fix before running.", data,
      c("Explain each error to the user in plain words and fix the config with their input.")))
  }
  next_run <- finn_next_run(paths, cfg, checked$facts)
  run_type <- finn_arg(args, "run_type") %||% next_run$run_type
  if (!run_type %in% c("standard", "iterate", "update")) stop("--run_type must be standard, iterate, or update.", call. = FALSE)
  run_mode <- if (identical(run_type, "standard")) "standard" else "agent"
  if (!identical(cfg$mode, run_mode)) {
    cfg$mode <- run_mode
    data$errors <- finn_check_config(cfg, checked$data, paths$root)$errors
    if (length(data$errors)) {
      return(finn_result(FALSE, "invalid", sprintf("The config has problems to fix before running %s.", finn_run_type_label(run_type)), data,
        c("Explain each error to the user in plain words and fix the config with their input.")))
    }
  }
  if (identical(run_type, "update")) {
    if (is.null(next_run$last_agent_version)) {
      data$warnings <- c(data$warnings, "Agent update needs a finished Agent forecast to build on, and this project has none yet. Run Agent iterate first.")
    } else if (!next_run$situation %in% c("agent_refresh", "resume")) {
      data$warnings <- c(data$warnings, sprintf("The file has no actuals newer than your last Agent forecast (`%s`), so there is nothing new to update with.",
        next_run$last_end %||% "unknown"))
    }
    changed <- next_run$custom_models_changed
    if (!is.null(changed) && !identical(next_run$situation, "resume")) {
      data$warnings <- c(data$warnings, sprintf(paste(
        "Agent update must use the same custom models as your last Agent forecast (%s), but this config enrolls %s.",
        "Restore them with custom_model.R --action=enroll or --action=unenroll, or start a new Agent version instead."
      ), finn_custom_label_text(changed$previous), finn_custom_label_text(changed$new)))
    }
  }
  cont <- next_run$continuation
  iterate_current <- identical(run_type, "iterate") && identical(next_run$situation, "agent_current") && is.list(cont)
  settings_changes <- next_run$settings_changes
  if (iterate_current && !identical(cont$kind, "other_changes")) {
    # Budget and goal changes are the supported way to continue this version.
    settings_changes <- settings_changes[!grepl("^(max_iter|weighted_mape_goal) ", settings_changes)]
  }
  if (run_type %in% c("update", "iterate") && length(settings_changes) &&
    next_run$situation %in% c("agent_refresh", "agent_current")) {
    data$warnings <- c(data$warnings, paste0(
      sprintf("These settings differ from your last Agent forecast (version %s): ", next_run$last_agent_version %||% "?"),
      paste(sprintf("`%s`", settings_changes), collapse = ", "),
      if (identical(run_type, "update")) {
        ". The update will ask you to confirm them; for big changes a new Agent iterate version may fit better."
      } else if (iterate_current && identical(cont$kind, "other_changes")) {
        ". Only `max_iter` and `weighted_mape_goal` can change on the same Agent version, so this needs a new Agent version, which needs your confirmation."
      } else {
        "."
      }
    ))
  }
  if (iterate_current && identical(cont$kind, "lower_max_iter")) {
    data$warnings <- c(data$warnings, sprintf(paste(
      "`max_iter` (`%s`) is lower than the `%s` your last Agent search already ran, so nothing new would run.",
      "Set it above `%s` to continue that Agent version."
    ), format(cont$max_iter$new), format(cont$max_iter$previous), format(cont$max_iter$previous)))
  }
  global <- cfg$run_global_models %||% finn_default_global_models(cfg$date_type)
  claimed <- finn_claimed_resources(paths$root, exclude_project = paths$name)
  sizing <- finn_plan_with_others(checked$facts$n_series, finn_data_gb(checked$data), global, claimed,
    sequential = identical(cfg$parallel, "none"),
    batch_size = suppressWarnings(as.numeric(finn_arg(args, "batch_size", "1"))),
    num_cores = if (is.null(args$num_cores)) NULL else suppressWarnings(as.numeric(args$num_cores))
  )
  plan <- sizing$plan
  if (!identical(run_type, "standard")) plan <- finn_custom_agent_plan(plan, cfg, finn_data_gb(checked$data), checked$facts$n_series)
  if (!sizing$fits) {
    plan$reason <- paste(plan$reason, "Other Finn runs are using this computer now, so this run cannot start until one finishes.")
  }
  data$parallel <- plan
  data$warnings <- c(data$warnings, plan$warnings)
  rec <- finn_recommend_settings(cfg, checked$facts, root = paths$root, workers = plan$num_cores)
  if (identical(run_type, "update")) rec$estimated_runtime <- finn_estimate_runtime(as.numeric(checked$facts$n_series %||% 0), "update", plan$num_cores)$text
  rec$recommendations <- c(next_run$recommendations, rec$recommendations)
  data$recommendations <- rec
  run <- validate_run_info(next_run, run_type, cfg, rec)
  data$run <- run
  last_run <- validate_last_run(next_run, run_type)
  review_facts <- c(checked$facts, list(n_columns = if (!is.null(checked$data)) ncol(checked$data)),
    validate_history_facts(checked$data, cfg, validate_resolved_hist_end(cfg, checked$facts), last_run$end))
  data$settings_review <- validate_settings_review(cfg, review_facts, plan, rec, data$warnings, data$notes, run, last_run)
  previous <- validate_previous_config(paths, config_file)
  if (!is.null(previous)) data$settings_changes <- validate_settings_changes(previous, cfg, review_facts, data$warnings)
  if (finn_bool(finn_arg(args, "save"))) {
    target <- file.path(paths$dir, "config.json")
    finn_save_project_config(paths, cfg, target)
    data$saved_to <- finn_norm(target)
  }
  finn_result(TRUE, "ok", "Config is valid.", data,
    c(if (is.null(data$settings_changes)) {
      "Show data.settings_review to the user exactly as written; do not reorder, reformat, or paraphrase it. Get a yes (for example `run`) before starting the run."
    } else {
      "The user is revising settings: show data.settings_changes exactly as written (data.settings_review if they ask for the full configuration). Get a yes (for example `run`) before starting the run."
    },
      if (length(rec$recommendations)) "Offer each item in recommendations with its reason. Apply only the ones the user accepts by writing a draft config outside the project and rerunning with --config=<draft> to show the changes. Never refuse to run with the user's own settings.",
      if (isTRUE(rec$test_subset_first)) "Suggest a quick test first on about 50 series: prepare_data.R --file=<data> --sample_series=50 --combo_variables=<cols> --out_dir=<folder>, then a separate test project from that file.",
      validate_alternative_action(run, run_type),
      if (is.null(data$saved_to)) sprintf("Rerun with --save=true%s to store this config for run_forecast.R.", if (is.null(finn_arg(args, "run_type"))) "" else paste0(" --run_type=", run_type)),
      sprintf("After the user agrees and the config is saved, start the run with run_forecast.R --project=<name> %s.", run$launch),
      if (!sizing$fits) "Tell the user other forecasts are using this computer; check run_status.R --all=true and start this run after one finishes."))
})
