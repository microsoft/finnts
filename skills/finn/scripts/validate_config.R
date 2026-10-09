# Validate a Finn run config against its data and show a settings card.
# Usage:
#   Rscript validate_config.R --project=<name> [--config=<config.json>] [--save=true]
# With --config, the file is merged over defaults; --save=true stores the
# result as <project>/config.json, which run_forecast.R uses by default.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Plain-language settings card for user confirmation
#'
#' @param cfg Normalized config.
#' @param facts Data facts from [finn_check_config()].
#' @param plan Parallel plan from [finn_plan_with_others()].
#' @return Character vector of card lines.
validate_settings_card <- function(cfg, facts, plan) {
  fmt <- function(x) if (is.null(x) || length(x) == 0) "auto" else paste(unlist(x), collapse = ", ")
  lines <- c(
    sprintf("Project: %s (%s run)", cfg$project_name, cfg$mode),
    sprintf("Data: %s rows, %s series, %s to %s", facts$n_rows %||% "?", facts$n_series %||% "?", facts$date_min %||% "?", facts$date_max %||% "?"),
    sprintf("Forecasting: %s by %s, %s %ss ahead", cfg$target_variable, fmt(cfg$combo_variables), cfg$forecast_horizon, cfg$date_type),
    sprintf("Drivers (external regressors): %s", if (length(cfg$external_regressors)) fmt(cfg$external_regressors) else "none"),
    sprintf("Models: %s; global models %s; local models %s",
      if (length(cfg$models_to_run)) fmt(cfg$models_to_run) else "Finn defaults",
      if (is.null(cfg$run_global_models)) "auto" else cfg$run_global_models,
      cfg$run_local_models),
    sprintf("Back testing: %s scenarios, spacing %s", fmt(cfg$back_test_scenarios), fmt(cfg$back_test_spacing)),
    sprintf("Negative forecasts allowed: %s; outlier cleaning: %s", cfg$negative_forecast, cfg$clean_outliers),
    sprintf("Reading the file: dates %s; decimal mark '%s'; fiscal year starts in month %s",
      cfg$date_format %||% facts$date_format %||% "auto-detected", cfg$decimal_mark %||% facts$decimal_mark %||% "auto-detected",
      cfg$fiscal_year_start %||% "1 (January, default)"),
    if (!is.null(facts$last_actual_date) && !identical(facts$last_actual_date, facts$date_max)) {
      sprintf("Last month/period with actuals: %s (later rows only carry driver values)", facts$last_actual_date)
    },
    sprintf("Computer use: %s", plan$reason)
  )
  if (identical(cfg$mode, "agent")) {
    lines <- c(lines, sprintf("Agent: LLM %s (%s), up to %s iterations, accuracy goal %s%% WMAPE",
      cfg$agent$llm$provider, cfg$agent$llm$model %||% "default", cfg$agent$max_iter, 100 * as.numeric(cfg$agent$weighted_mape_goal)))
  }
  lines
}

#' Settings-card lines describing size-based recommendations
#'
#' @param rec Result of [finn_recommend_settings()].
#' @return Character vector of card lines (possibly empty).
validate_recommendation_lines <- function(rec) {
  fmt <- function(v) paste(unlist(v), collapse = ", ")
  label <- c(small = "small", large = "large (100+ series)", very_large = "very large (1,000+ series)")[[rec$tier]]
  c(
    sprintf("Data size: %s; estimated run time %s", label, rec$estimated_runtime),
    vapply(rec$recommendations, function(r) sprintf("Suggested for this size: %s = %s", r$key, fmt(r$value)), character(1)),
    if (!is.null(rec$suggested_mode_reason)) sprintf("Run type advice: %s", rec$suggested_mode_reason)
  )
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
  global <- cfg$run_global_models %||% finn_default_global_models(cfg$date_type)
  claimed <- finn_claimed_resources(paths$root, exclude_project = paths$name)
  sizing <- finn_plan_with_others(checked$facts$n_series, finn_data_gb(checked$data), global, claimed,
    sequential = identical(cfg$parallel, "none")
  )
  plan <- sizing$plan
  if (!sizing$fits) {
    plan$reason <- paste(plan$reason, "Other Finn runs are using this computer now, so this run cannot start until one finishes.")
  }
  data$parallel <- plan
  rec <- finn_recommend_settings(cfg, checked$facts, root = paths$root, workers = plan$num_cores)
  data$recommendations <- rec
  data$settings_card <- c(validate_settings_card(cfg, checked$facts, plan), validate_recommendation_lines(rec))
  if (finn_bool(finn_arg(args, "save"))) {
    target <- file.path(paths$dir, "config.json")
    finn_save_project_config(paths, cfg, target)
    data$saved_to <- finn_norm(target)
  }
  finn_result(TRUE, "ok", "Config is valid.", data,
    c("Show the settings card and warnings to the user and get a yes before starting the run.",
      if (length(rec$recommendations)) "Offer each item in recommendations with its reason. Apply only the ones the user accepts by editing the config, then rerun with --save=true. Never refuse to run with the user's own settings.",
      if (isTRUE(rec$test_subset_first)) "Suggest a quick test first on about 50 series: prepare_data.R --file=<data> --sample_series=50 --combo_variables=<cols> --out_dir=<folder>, then a separate test project from that file.",
      if (!identical(rec$suggested_mode, cfg$mode)) sprintf("Recommend a %s run for the whole data set and explain why (suggested_mode_reason); keep the user's choice if they insist.", rec$suggested_mode),
      if (is.null(data$saved_to)) "Rerun with --save=true to store this config for run_forecast.R.",
      if (!sizing$fits) "Tell the user other forecasts are using this computer; check run_status.R --all=true and start this run after one finishes."))
})
