make_restart_average_predictions <- function(info) {
  path <- locate_single_models_file(info)
  rows <- read_fcst_file(path)
  actuals <- ifelse(rows$Train_Test_ID == 1,
    100 + as.integer(format(as.Date(rows$Date), "%m")), rows$Target)
  rows$Forecast <- actuals * ifelse(rows$Model_Name == "meanf", 1.1, 0.9)
  write_fcst_file(rows, path)
  rows
}

test_that("unfinished Best_Model flags are repaired by rerunning selection and averages", {
  evaluations <- 0L
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    train_models = function(...) stop("retry must not train models"),
    fit_models = function(...) stop("retry must not fit models"),
    select_series_forecasts = function(...) {
      evaluations <<- evaluations + 1L
      original_selector(...)
    }
  )
  info <- make_best_models_fixture()
  rows <- make_restart_average_predictions(info)
  rows$Best_Model <- "No"
  write_fcst_file(rows, locate_single_models_file(info))
  result <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  expect_identical(evaluations, 2L)
  expect_setequal(strsplit(result$selections$Synthetic$selected_id, "_", fixed = TRUE)[[1]],
    unique(rows$Model_ID))
  average <- read_selection_file(info, "forecasts", "-average_models", "Synthetic")
  expect_true(all(average$Best_Model == "Yes"))
  expect_true(all(read_fcst_file(locate_single_models_file(info))$Best_Model == "No"))
  published <- get_forecast_data(info)
  expect_identical(unique(published$Model_ID[published$Best_Model == "Yes"]),
    result$selections$Synthetic$selected_id)
})

test_that("a completed saved average survives a retry before completion logging", {
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    train_models = function(...) stop("retry must not train models")
  )
  info <- make_best_models_fixture()
  info$combo <- hash_data("Synthetic")
  make_restart_average_predictions(info)
  initial <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  log <- read_selection_file(info, "logs")
  log$weighted_mape <- NA_real_
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  local_mocked_bindings(select_series_forecasts = function(...) stop("a complete saved winner must not be reselected"))
  result <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  expect_identical(result$selections$Synthetic$selected_id, initial$selections$Synthetic$selected_id)
  expect_equal(agent_selection_summary(result)$weighted_mape, 0)
  expect_length(result$rejected_combos, 0L)
})

test_that("a finite completion log cannot hide invalid saved winner flags", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  for (defect in c("all_no", "missing_flag", "multiple_winners")) {
    info <- make_best_models_fixture()
    make_restart_average_predictions(info)
    initial <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
    average <- read_selection_file(info, "forecasts", "-average_models", "Synthetic")
    rows <- read_fcst_file(locate_single_models_file(info))
    if (defect == "multiple_winners") {
      rows$Best_Model[rows$Model_Name == "meanf"] <- "Yes"
      write_fcst_file(rows, locate_single_models_file(info))
    } else {
      average$Best_Model <- if (defect == "all_no") "No" else NA_character_
      write_data(average, combo = "Synthetic", run_info = info,
        output_type = "data", folder = "forecasts", suffix = "-average_models")
    }
    result <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
    expect_identical(result$selections$Synthetic$selected_id, initial$selections$Synthetic$selected_id,
      info = defect)
    saved <- dplyr::bind_rows(read_fcst_file(locate_single_models_file(info)),
      read_selection_file(info, "forecasts", "-average_models", "Synthetic"))
    expect_false(anyNA(saved$Best_Model), info = defect)
    expect_identical(unique(saved$Model_ID[saved$Best_Model == "Yes"]),
      initial$selections$Synthetic$selected_id, info = defect)
  }
})

test_that("average file existence cannot suppress a complete retry result", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  make_restart_average_predictions(info)
  initial <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  log <- read_selection_file(info, "logs")
  log$weighted_mape <- NA_real_
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  local_mocked_bindings(select_series_forecasts = function(...) stop("complete series must not be reselected"))
  result <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  expect_true(is.list(result) && "Synthetic" %in% names(result$selections))
  if (is.list(result)) {
    expect_identical(result$selections$Synthetic$selected_id, initial$selections$Synthetic$selected_id)
  }
})

test_that("multi-series retries retain completed and newly finalized results", {
  evaluated <- character()
  original_selector <- select_series_forecasts
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  rows <- make_restart_average_predictions(info)
  first <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  history <- read_selection_file(info, "prep_data", "-R1", "Synthetic")
  history$Combo <- "Other"
  write_data(history, combo = "Other", run_info = info,
    output_type = "data", folder = "prep_data", suffix = "-R1")
  rows$Combo <- "Other"
  rows$Combo_ID <- "Other"
  write_data(rows, combo = "Other", run_info = info,
    output_type = "data", folder = "forecasts", suffix = "-single_models")
  log <- read_selection_file(info, "logs")
  log$weighted_mape <- NA_real_
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  local_mocked_bindings(
    select_series_forecasts = function(predictions, ...) {
      evaluated <<- c(evaluated, unique(predictions$Combo))
      original_selector(predictions, ...)
    },
    train_models = function(...) stop("retry must not train models")
  )
  result <- final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  expect_setequal(names(result$selections), c("Synthetic", "Other"))
  expect_identical(result$selections$Synthetic$selected_id, first$selections$Synthetic$selected_id)
  expect_identical(evaluated, c("Other", "Other"))
  expect_setequal(unique(get_forecast_data(info)$Combo), c("Synthetic", "Other"))
  expect_equal(agent_selection_summary(result)$weighted_mape, 0)
})

test_that("saved average reuse requires its original component predictions", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  make_restart_average_predictions(info)
  final_models(info, average_models = TRUE, weekly_to_daily = FALSE)
  rows <- read_fcst_file(locate_single_models_file(info))
  rows <- rows[rows$Model_Name != "meanf", ]
  write_fcst_file(rows, locate_single_models_file(info))
  expect_error(final_models(info, average_models = TRUE, weekly_to_daily = FALSE),
    "component predictions are missing")
})

test_that("prediction read errors are not treated as absent model families", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  original_reader <- read_file
  local_mocked_bindings(read_file = function(run_info, path = NULL, file_list = NULL, ...) {
    requested <- if (is.null(file_list)) path else file_list
    if (any(grepl("-single_models", requested, fixed = TRUE))) stop("prediction storage unavailable")
    original_reader(run_info, path = path, file_list = file_list, ...)
  })
  expect_error(final_models(info, weekly_to_daily = FALSE), "prediction storage unavailable")
})

test_that("partial weekly hierarchy retries repair one source without changing delivered values", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  fixture <- make_hierarchical_selection_artifacts(date_type = "week", horizon = 3L,
    backtest_scenarios = 1L)
  info <- fixture$run_info
  initial <- final_models(info, average_models = FALSE, weekly_to_daily = TRUE)
  expected <- get_forecast_data(info)
  source_combo <- fixture$metadata$hts_combos[1]
  source <- read_selection_file(info, "forecasts", "-single_models", source_combo)
  source$Best_Model <- NA_character_
  source <- convert_weekly_to_daily(source, "week", TRUE)
  write_data(source, combo = source_combo, run_info = info,
    output_type = "data", folder = "forecasts", suffix = "-single_models")
  evaluated <- character()
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    train_models = function(...) stop("retry must not train models"),
    select_series_forecasts = function(predictions, ...) {
      evaluated <<- c(evaluated, unique(predictions$Combo))
      original_selector(predictions, ...)
    }
  )
  repaired <- final_models(info, average_models = FALSE, weekly_to_daily = TRUE)
  expect_setequal(names(repaired$selections), fixture$metadata$original_combos)
  expect_identical(evaluated, rep(source_combo, 2L))
  expect_equal(agent_selection_summary(repaired)$weighted_mape,
    agent_selection_summary(initial)$weighted_mape, tolerance = 1e-7)
  delivered <- get_forecast_data(info)
  columns <- c("Combo", "Model_ID", "Train_Test_ID", "Date", "Date_Day", "Forecast", "Target", "Best_Model")
  expect_equal(dplyr::arrange(delivered[, columns], Combo, Model_ID, Train_Test_ID, Date, Date_Day),
    dplyr::arrange(expected[, columns], Combo, Model_ID, Train_Test_ID, Date, Date_Day), tolerance = 1e-7)
  expect_false(any(grepl("\\.[xy]$", names(delivered))))
  local_mocked_bindings(
    select_series_forecasts = function(...) stop("complete hierarchy must not be reselected"),
    reconcile_hierarchical_data = function(...) stop("unchanged hierarchy must not be reconciled again")
  )
  resumed <- final_models(info, average_models = FALSE, weekly_to_daily = TRUE)
  expect_equal(agent_selection_summary(resumed), agent_selection_summary(repaired), tolerance = 1e-7)
})

test_that("explicitly completed rejections are not evaluated again on retry", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  info$allow_quality_rejection <- TRUE
  rows <- read_fcst_file(locate_single_models_file(info))
  rows$Forecast <- Inf
  write_fcst_file(rows, locate_single_models_file(info))
  initial <- final_models(info, weekly_to_daily = FALSE)
  log <- record_agent_selection_attempt(read_selection_file(info, "logs"), initial,
    list(agent_version = 1, forecast_approach = "bottoms_up"))
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  local_mocked_bindings(select_series_forecasts = function(...) stop("completed rejection must not be reassessed"))
  result <- final_models(info, weekly_to_daily = FALSE)
  expect_identical(result$rejected_combos, "Synthetic")
  expect_identical(agent_selection_summary(result)$status, "rejected")
  expect_true(is.na(read_selection_file(info, "logs")$weighted_mape))
})