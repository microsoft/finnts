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

#' Plain-language meaning of one review setting
#'
#' Explains each setting and adds context for automatic values,
#' such as the date `hist_end_date = null` resolves to.
#'
#' @param key Setting name.
#' @param cfg Merged config.
#' @param facts Data facts from [finn_check_config()].
#' @param run Result of [validate_run_info()], or NULL. Adds how to continue
#'   the last Agent search when `max_iter` is shown for an Agent iterate run.
#' @return Explanation sentence(s).
validate_field_explanation <- function(key, cfg, facts = list(), run = NULL) {
  text <- switch(key,
    target_variable = "The input column Finn forecasts.",
    combo_variables = "The columns that define each independent time series.",
    date_type = "The period granularity used for training, backtesting, and forecasting.",
    hist_end_date = "The last period treated as historical actuals; forecasting begins after it, or `null` for the last period with actuals.",
    forecast_horizon = "The number of periods forecast after `hist_end_date`.",
    external_regressors = "Input-file columns used as forecast drivers, or `null` for none. Drivers need values for every forecast period.",
    hist_start_date = "The first historical period used for training, or `null` for the earliest available period.",
    combo_cleanup_date = "Removes series with no values after this date, or `null` to keep them.",
    back_test_scenarios = "The number of historical forecast folds used to measure model accuracy, or `null` for automatic sizing.",
    back_test_spacing = "The number of periods between backtest folds, or `null` for the cadence default.",
    negative_forecast = "Controls whether forecast values may be below zero.",
    run_global_models = "Controls whether models learn across all time series together, or `null` to let Finn decide.",
    run_local_models = "Controls whether models are trained separately for each time series.",
    forecast_approach = "Controls whether forecasts run independently from the bottom level (`bottoms_up`) or add up through a hierarchy (`standard_hierarchy`, `grouped_hierarchy`).",
    models_to_run = "The models to evaluate, or `null` for Finn's default models.",
    models_not_to_run = "Models to leave out, or `null` to leave none out.",
    run_ensemble_models = "Controls whether ensemble models are trained; separate from simple averaging.",
    recipes_to_run = "The feature recipes, or `null` to use all supported recipes.",
    pca = "Controls principal component analysis, or `null` for automatic behavior.",
    average_models = "Controls whether top model forecasts are averaged (simple averaging).",
    max_model_average = "The maximum number of models included in a model average.",
    feature_selection = "Controls driver feature selection.",
    clean_outliers = "Controls whether detected target outliers are replaced.",
    clean_missing_values = "Controls whether missing target values are imputed.",
    allow_hierarchical_forecast = "Lets the Agent try hierarchical forecasts that add up across series.",
    max_iter = "The maximum number of Agent optimization iterations.",
    weighted_mape_goal = "The target weighted MAPE expressed as a decimal, such as `0.03` for 3%; the Agent can stop early once it is reached.",
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
      hist_end_date = if (!is.null(facts$last_actual_date)) sprintf("Auto uses `%s`.", facts$last_actual_date),
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
    text <- paste(text, sprintf(
      "Your last Agent search ran up to `%s`; a higher value continues that Agent version and runs only the extra iterations.",
      run$prior_max_iter
    ))
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
#'   `alternative`, `launch`, `recommended_run_type`, and `prior_max_iter`
#'   (set only when an iterate run can continue the current Agent version).
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
  list(run_type = run_type, label = finn_run_type_label(run_type), hint = finn_run_type_hint(run_type, cfg),
    situation = situation, why = why, alternative = alternative, launch = launch,
    recommended_run_type = recommended,
    prior_max_iter = if (identical(situation, "agent_current") && identical(run_type, "iterate")) next_run$prior_max_iter)
}

#' Settings review for user confirmation
#'
#' Renders an intro naming the run type, `Data check`, a `Forecast configuration`
#' table, `Warnings`, `Suggestions`, `On this computer`, and a fixed footer.
#' Values use config.json notation.
#'
#' @param cfg Merged, validated config.
#' @param facts Data facts from [finn_check_config()], optionally with
#'   `n_columns`.
#' @param plan Parallel plan from [finn_plan_with_others()].
#' @param rec Result of [finn_recommend_settings()], or NULL.
#' @param warnings Non-blocking warnings to list.
#' @param notes Notes about how the file was read.
#' @param run Result of [validate_run_info()], or NULL to name the run type
#'   from the config mode only.
#' @return Single markdown string.
validate_settings_review <- function(cfg, facts, plan, rec = NULL, warnings = character(), notes = character(), run = NULL) {
  bullet <- function(label, value) if (!is.null(value) && length(value) && nzchar(value)) sprintf("- **%s:** `%s`", label, value)
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

  size_label <- if (!is.null(rec$tier)) {
    c(small = "small (up to 100 series)", large = "large (100+ series)", very_large = "very large (1,000+ series)")[[rec$tier]]
  }
  check <- c(
    "### Data check", "",
    bullet("Rows", facts$n_rows),
    bullet("Columns", facts$n_columns),
    bullet("Series", facts$n_series),
    if (!is.null(facts$date_min)) bullet("Date range", sprintf("%s to %s", facts$date_min, facts$date_max)),
    if (!is.null(facts$last_actual_date) && !identical(facts$last_actual_date, facts$date_max)) {
      sprintf("- **Last period with actuals:** `%s` (later rows only carry driver values)", facts$last_actual_date)
    },
    bullet("Inferred date order", cfg$date_format %||% facts$date_format),
    bullet("Detected cadence", cfg$date_type),
    if (!is.null(size_label)) sprintf("- **Data size:** %s", size_label),
    bullet("Validation", if (length(warnings)) sprintf("passed with %d warning%s", length(warnings), if (length(warnings) == 1) "" else "s") else "passed"),
    if (length(notes)) paste0("- ", vapply(notes, validate_escape_cell, character(1), USE.NAMES = FALSE))
  )
  sections <- c(sections, paste(check, collapse = "\n"))

  rows <- vapply(validate_review_fields(cfg$mode), function(key) {
    sprintf("| `%s` | `%s` | %s |", key,
      validate_escape_cell(validate_json_value(key, validate_field_value(cfg, key))),
      validate_escape_cell(validate_field_explanation(key, cfg, facts, run)))
  }, character(1), USE.NAMES = FALSE)
  sections <- c(sections, paste(c("### Forecast configuration", "",
    "| Input | Recommended value | What it means for your forecast |", "|---|---|---|", rows), collapse = "\n"))

  if (length(warnings)) {
    sections <- c(sections, paste(c("### Warnings", "", paste0("- ", vapply(warnings, validate_escape_cell, character(1), USE.NAMES = FALSE))), collapse = "\n"))
  }

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

  computer <- c(
    if (!is.null(rec$estimated_runtime)) sprintf("- **Estimated run time:** %s (a rough guide, not a promise)", rec$estimated_runtime),
    if (!is.null(plan$reason)) sprintf("- **Computer use:** %s", plan$reason)
  )
  if (length(computer)) sections <- c(sections, paste(c("### On this computer", "", computer), collapse = "\n"))

  sections <- c(sections, paste(
    "The data passed validation and these settings are ready to run on this computer.",
    "Reply `run` to start the forecast, or tell me which input value you want to change."
  ))
  paste(sections, collapse = "\n\n")
}

#' Revision view listing only the settings that changed
#'
#' Renders a `Changes` table with previous and new values, current warnings,
#' and a footer offering the full configuration.
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
  for (key in validate_review_fields(cfg$mode)) {
    old <- validate_json_value(key, validate_field_value(previous, key))
    new <- validate_json_value(key, validate_field_value(cfg, key))
    if (!identical(old, new)) {
      rows <- c(rows, sprintf("| `%s` | `%s` | `%s` | %s |", key, validate_escape_cell(old), validate_escape_cell(new),
        validate_escape_cell(validate_field_explanation(key, cfg, facts))))
    }
  }
  sections <- if (length(rows)) {
    c("Updated the settings.", paste(c("### Changes", "",
      "| Input | Previous | New value | What it means for your forecast |", "|---|---|---|---|", rows), collapse = "\n"))
  } else {
    "No settings changed. The previously reviewed settings remain ready to run."
  }
  if (length(warnings)) {
    sections <- c(sections, paste(c("### Warnings", "", paste0("- ", vapply(warnings, validate_escape_cell, character(1), USE.NAMES = FALSE))), collapse = "\n"))
  }
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
  }
  if (run_type %in% c("update", "iterate") && length(next_run$settings_changes) &&
    next_run$situation %in% c("agent_refresh", "agent_current")) {
    data$warnings <- c(data$warnings, paste0(
      sprintf("These settings differ from your last Agent forecast (version %s): ", next_run$last_agent_version %||% "?"),
      paste(sprintf("`%s`", next_run$settings_changes), collapse = ", "),
      if (identical(run_type, "update")) ". The update will ask you to confirm them; for big changes a new Agent iterate version may fit better." else "."
    ))
  }
  global <- cfg$run_global_models %||% finn_default_global_models(cfg$date_type)
  claimed <- finn_claimed_resources(paths$root, exclude_project = paths$name)
  sizing <- finn_plan_with_others(checked$facts$n_series, finn_data_gb(checked$data), global, claimed,
    sequential = identical(cfg$parallel, "none"),
    batch_size = suppressWarnings(as.numeric(finn_arg(args, "batch_size", "1"))),
    num_cores = if (is.null(args$num_cores)) NULL else suppressWarnings(as.numeric(args$num_cores))
  )
  plan <- sizing$plan
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
  review_facts <- c(checked$facts, list(n_columns = if (!is.null(checked$data)) ncol(checked$data)))
  data$settings_review <- validate_settings_review(cfg, review_facts, plan, rec, data$warnings, data$notes, run)
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
      if (!is.null(run$alternative)) sprintf("If the user prefers %s, rerun this script with --run_type=%s and show the new review before launching.", run$alternative$label, run$alternative$run_type),
      if (is.null(data$saved_to)) sprintf("Rerun with --save=true%s to store this config for run_forecast.R.", if (is.null(finn_arg(args, "run_type"))) "" else paste0(" --run_type=", run_type)),
      sprintf("After the user agrees and the config is saved, start the run with run_forecast.R --project=<name> %s.", run$launch),
      if (!sizing$fits) "Tell the user other forecasts are using this computer; check run_status.R --all=true and start this run after one finishes."))
})
