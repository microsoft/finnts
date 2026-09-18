# Calculate original-unit metrics for explicit grouping columns. WMAPE and
# percentage bias remain NA when actuals have a zero absolute denominator.
acr_validation_metrics <- function(data, groups) {
  finnts:::acr_scott_validation_metrics(data, groups)
}

# Save a representative series plot with actual history and labeled holdout
# forecasts. Opens/closes only its own PNG graphics device; never changes input.
acr_validation_plot <- function(history, predictions, combo, path, cutoff) {
  history <- history[history$Combo == combo, , drop = FALSE]
  predictions <- predictions[predictions$Combo == combo, , drop = FALSE]
  grDevices::png(path, width = 1400, height = 800, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  history <- history[order(history$Date), ]
  graphics::plot(history$Date, history$Actual, type = "l", lwd = 2,
    xlab = "Month", ylab = "Configured target units", main = combo,
    ylim = range(c(history$Actual, predictions$Forecast), finite = TRUE))
  models <- sort(unique(predictions$Model))
  colors <- c("#B44335", "#247F70", "#71549C", "#C18A1E")
  for (index in seq_along(models)) {
    values <- predictions[predictions$Model == models[index], ]
    values <- values[order(values$Date), ]
    graphics::lines(values$Date, values$Forecast, col = colors[index], lwd = 2, lty = 2)
  }
  graphics::abline(v = as.numeric(cutoff), col = "grey50", lty = 3)
  graphics::legend("topleft", legend = c("Actual", models),
    col = c("black", colors[seq_along(models)]), lty = c(1, rep(2, length(models))), bty = "n")
}

# Run an offline user-data evaluation from an explicit configuration list.
# Requires local input_path, new output_dir, date_column, target_variable,
# combo_variables and options; holdout/horizon default to 12. The latest actual
# months are excluded from all fitting. Writes only to the confirmed new output
# directory; input remains unchanged. Failures never trigger policy changes.
acr_validate_custom <- function(configuration) {
  required <- c("input_path", "output_dir", "date_column", "target_variable", "combo_variables", "options")
  if (!all(required %in% names(configuration))) stop("Validation configuration is missing required fields.")
  input_path <- normalizePath(configuration$input_path, mustWork = TRUE)
  output_dir <- normalizePath(configuration$output_dir, mustWork = FALSE)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory must be new or empty; validation never overwrites an earlier run.")
  }
  if (identical(tolower(input_path), tolower(output_dir))) stop("Input and output paths must differ.")
  source_hash <- tools::md5sum(input_path)
  data <- switch(tolower(tools::file_ext(input_path)),
    csv = utils::read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE),
    rds = readRDS(input_path), stop("Provide a local CSV or RDS input file."))
  data <- as.data.frame(data)
  columns <- c(configuration$date_column, configuration$target_variable, configuration$combo_variables)
  if (!all(columns %in% names(data))) stop("Configured columns are absent from the input.")
  if (configuration$date_column != "Date" && "Date" %in% names(data)) stop("Ambiguous existing Date column.")
  names(data)[names(data) == configuration$date_column] <- "Date"
  if (!inherits(data$Date, "Date")) data$Date <- as.Date(data$Date)
  if (anyNA(data$Date)) stop("Dates must use unambiguous ISO dates or Date objects.")
  target <- configuration$target_variable
  options <- finnts:::acr_scott_options(configuration$options, configuration$combo_variables)
  data <- finnts:::normalize_combo_values(data, configuration$combo_variables)
  if (!is.numeric(data[[target]])) stop("The target must be numeric.")
  actual_dates <- sort(unique(data$Date[!is.na(data[[target]])]))
  holdout <- if (is.null(configuration$holdout)) 12L else configuration$holdout
  horizon <- if (is.null(configuration$horizon)) 12L else configuration$horizon
  if (length(holdout) != 1L || !is.finite(holdout) || holdout < 1 || holdout > 12 || holdout != as.integer(holdout) ||
      length(horizon) != 1L || !is.finite(horizon) || horizon < 1 || horizon > 12 || horizon != as.integer(horizon)) {
    stop("Holdout and future horizon must be integers from 1 through 12.")
  }
  if (length(actual_dates) < holdout + 19L) stop("Insufficient history for a holdout and internal validation.")
  hist_end <- max(actual_dates)
  cutoff <- actual_dates[length(actual_dates) - holdout]
  finnts:::acr_scott_prepare(data, configuration$combo_variables, target, options,
    min(actual_dates), hist_end, horizon, "month", FALSE, FALSE, FALSE, FALSE,
    "bottoms_up", NULL, "R1")
  keyed <- tidyr::unite(data, "Combo", tidyselect::all_of(configuration$combo_variables), sep = "--", remove = FALSE)
  historical <- keyed[keyed$Date <= hist_end, , drop = FALSE]
  historical$Actual <- historical[[target]]
  evaluation <- historical[historical$Date > cutoff, c("Combo", "Date", "Actual"), drop = FALSE]
  if (anyDuplicated(evaluation[c("Combo", "Date")])) stop("Holdout rows are not unique.")
  training_input <- data[data$Date <= hist_end, , drop = FALSE]
  training_input[training_input$Date > cutoff, target] <- NA_real_
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  quality <- historical %>% dplyr::group_by(Combo) %>% dplyr::summarise(
    Months = dplyr::n(), FirstDate = min(Date), LastDate = max(Date),
    ZeroMonths = sum(Actual == 0), .groups = "drop")
  utils::write.csv(quality, file.path(output_dir, "input-quality.csv"), row.names = FALSE)
  saveRDS(configuration, file.path(output_dir, "validation-config.rds"))
  start <- proc.time()[["elapsed"]]
  run <- finnts::set_run_info(project_name = "acr-scott-custom-validation", run_name = "holdout",
    path = file.path(output_dir, "finn-run"), add_unique_id = FALSE)
  finnts::forecast_time_series(run_info = run, input_data = training_input,
    combo_variables = configuration$combo_variables, target_variable = target,
    date_type = "month", forecast_horizon = as.numeric(holdout), hist_end_date = cutoff,
    hist_start_date = min(actual_dates), models_to_run = c("acr-scott-custom", "snaive", "arima", "ets"),
    back_test_scenarios = 1, recipes_to_run = "R1", stationary = FALSE,
    box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE,
    acr_scott_custom_options = options, run_global_models = TRUE, run_local_models = TRUE,
    run_ensemble_models = FALSE, average_models = FALSE, return_data = FALSE)
  forecasts <- finnts::get_forecast_data(run)
  predictions <- forecasts[forecasts$Run_Type == "Future_Forecast" &
    forecasts$Model_Name %in% c("acr-scott-custom", "snaive", "arima", "ets"),
    c("Combo", "Date", "Model_Name", "Forecast"), drop = FALSE]
  names(predictions)[names(predictions) == "Model_Name"] <- "Model"
  predictions <- unique(predictions)
  if (nrow(predictions) != nrow(evaluation) * 4L || any(!is.finite(predictions$Forecast))) {
    stop("One or more comparison models did not produce a complete finite holdout forecast; inspect saved run artifacts.")
  }
  predictions <- merge(predictions, evaluation, by = c("Combo", "Date"), all.x = TRUE)
  if (anyNA(predictions$Actual)) stop("Forecast and held-out actual identities do not match.")
  peer_map <- unique(historical[c("Combo", options$pool_by)])
  predictions <- merge(predictions, peer_map, by = "Combo", all.x = TRUE)
  series_metrics <- acr_validation_metrics(predictions, c("Model", "Combo"))
  peer_metrics <- acr_validation_metrics(predictions, c("Model", options$pool_by))
  portfolio <- predictions %>% dplyr::group_by(Model, Date) %>%
    dplyr::summarise(Actual = sum(Actual), Forecast = sum(Forecast), .groups = "drop")
  portfolio_metrics <- acr_validation_metrics(portfolio, "Model")
  utils::write.csv(predictions, file.path(output_dir, "holdout-predictions.csv"), row.names = FALSE)
  utils::write.csv(series_metrics, file.path(output_dir, "series-metrics.csv"), row.names = FALSE)
  utils::write.csv(peer_metrics, file.path(output_dir, "peer-metrics.csv"), row.names = FALSE)
  utils::write.csv(portfolio_metrics, file.path(output_dir, "portfolio-total-metrics.csv"), row.names = FALSE)
  models <- finnts::get_trained_models(run)
  workflow <- models$Model_Fit[[which(models$Model_Name == "acr-scott-custom")]]
  future_rows <- finnts:::acr_scott_restore_metadata(finnts::get_prepped_data(run, recipe = "R1"), options)
  future_rows <- future_rows[future_rows$Date > cutoff, , drop = FALSE]
  explanation <- finnts::explain_acr_scott_custom(workflow, future_rows)
  utils::write.csv(explanation, file.path(output_dir, "holdout-rule-explanations.csv"), row.names = FALSE)
  saved_path <- file.path(output_dir, "holdout-workflow.rds")
  saveRDS(workflow, saved_path)
  stopifnot(identical(predict(workflow, future_rows), predict(readRDS(saved_path), future_rows)))
  full_data <- historical
  full_data$Target <- full_data[[target]]
  full_data$.acr_scott_observed <- 1L
  recipe <- workflows::extract_recipe(workflow, estimated = FALSE)
  full_data <- finnts:::adjust_column_types(full_data, recipe)
  refitted <- generics::fit(workflow, full_data)
  metadata <- unique(full_data[c("Combo", options$pool_by, options$profile_column)])
  next_dates <- seq.Date(hist_end, by = "month", length.out = horizon + 1L)[-1L]
  next_rows <- merge(metadata, data.frame(Date = next_dates))
  next_rows$.acr_scott_observed <- 0L
  next_rows <- finnts:::adjust_column_types(next_rows, recipe)
  if (!is.null(options$exposure)) {
    supplied <- keyed[keyed$Date > hist_end, c("Combo", "Date", options$exposure), drop = FALSE]
    next_rows <- merge(next_rows, supplied, by = c("Combo", "Date"), all.x = TRUE)
  }
  final_forecast <- finnts::explain_acr_scott_custom(refitted, next_rows)
  utils::write.csv(final_forecast, file.path(output_dir, "future-forecast-and-rules.csv"), row.names = FALSE)
  saveRDS(refitted, file.path(output_dir, "refitted-workflow.rds"))
  selected <- unique(explanation[c("Combo", "Profile")])
  selected <- selected[!duplicated(selected$Profile), , drop = FALSE]
  for (index in seq_len(min(nrow(selected), 4L))) {
    acr_validation_plot(historical, predictions, selected$Combo[index],
      file.path(output_dir, paste0("series-", index, ".png")), cutoff)
  }
  elapsed <- proc.time()[["elapsed"]] - start
  writeLines(c(
    "# acr-scott-custom validation", "",
    paste("Holdout starts after", cutoff, "and ends", hist_end),
    paste("Target mode:", options$target_mode), paste("Pool by:", paste(options$pool_by, collapse = ", ")),
    paste("Runtime seconds:", round(elapsed, 2)),
    "The final holdout was excluded from fitting and peer-reference estimation.",
    "Series/peer errors and portfolio-total errors are separate; portfolio aggregation may cancel errors.",
    "Undefined zero-denominator WMAPE is recorded as NA. No model was forced to win.",
    "Adjusted-day availability at historical origins requires the user's confirmation.",
    "Review the representative plots and holdout-rule-explanations before accepting business behavior.",
    "The refitted future forecast uses all historical actuals; it is not a second holdout evaluation.",
    "RDS prediction reload parity passed. These outputs do not imply a clean R CMD check."),
    file.path(output_dir, "validation-report.md"))
  stopifnot(identical(unname(source_hash), unname(tools::md5sum(input_path))))
  invisible(list(run_info = run, series_metrics = series_metrics,
                 peer_metrics = peer_metrics, portfolio_metrics = portfolio_metrics,
                 future = final_forecast, output_dir = output_dir, elapsed = elapsed))
}

# CLI execution accepts one local RDS configuration. Sourcing this file defines
# functions only, allowing deterministic tests without reading business data.
if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) != 1L) stop("Usage: Rscript tools/validate-acr-scott-custom.R <configuration.rds>")
  library(finnts)
  library(dplyr)
  acr_validate_custom(readRDS(arguments[[1]]))
}