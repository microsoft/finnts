make_global_update_selection_fixture <- function() {
  path <- withr::local_tempdir(pattern = "finnts-global-selection-", .local_envir = parent.frame())
  previous <- set_run_info(project_name = paste0("global-selection_", hash_data("all")),
    run_name = "previous", path = path, data_output = "csv", add_unique_id = FALSE)
  updated <- previous
  updated$run_name <- "updated"
  fixture <- make_selection_case(actuals = rep(100, 36),
    futures = list(template = rep(100, 6)), errors = c(template = 0))
  series <- fixture$context
  series$history <- fixture$history
  splits <- series$train_test_split
  models <- c("xgboost", "chronos2", "timegpt")
  model_ids <- paste(models, "global", "R1", sep = "--")
  template <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  source <- dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
    dplyr::bind_rows(lapply(c("first", "second"), function(combo) {
      rows <- template
      rows$Combo <- combo
      rows$Combo_ID <- "All-Data"
      rows$Model_Name <- models[model_index]
      rows$Model_Type <- "global"
      rows$Recipe_ID <- "R1"
      rows$Model_ID <- model_ids[model_index]
      rows$Hyperparameter_ID <- 1L
      rows$Run_Type <- splits$Run_Type[match(rows$Train_Test_ID, splits$Train_Test_ID)]
      rows$Forecast <- c(98, if (combo == "first") 102 else 100, 103)[model_index]
      rows$Best_Model <- if (combo == "second" && model_index == 2L) "Yes" else "No"
      rows
    }))
  }))
  source <- create_prediction_intervals(source, splits)
  average_id <- paste(sort(model_ids[1:2]), collapse = "_")
  average <- source[source$Combo == "first" & source$Model_ID == model_ids[1], ]
  average$Model_ID <- average_id
  average$Model_Name <- NA_character_
  average$Model_Type <- "local"
  average$Recipe_ID <- "simple_average"
  average$Forecast <- 100
  average$Best_Model <- "Yes"
  average <- create_prediction_intervals(average, splits)
  for (combo in c("first", "second")) {
    write_data(source[source$Combo == combo, ], combo = combo, run_info = previous,
      output_type = "data", folder = "forecasts", suffix = "-global_models")
  }
  write_data(average, combo = "first", run_info = previous,
    output_type = "data", folder = "forecasts", suffix = "-average_models")
  fitted <- dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
    make_fitted_selection_models(source[source$Model_ID == model_ids[model_index], ], models[model_index])
  }))
  trained <- fitted[, c("Combo_ID", "Model_Name", "Model_Type", "Recipe_ID", "Model_Fit")]
  trained$Model_ID <- model_ids
  write_data(trained, combo = "All-Data", run_info = previous,
    output_type = "object", folder = "models", suffix = "-single_models")
  log <- read_selection_file(previous, "logs")
  settings <- list(models_to_run = paste(models, collapse = "---"), recipes_to_run = "R1",
    external_regressors = NA_character_, lag_periods = NA_character_, rolling_window_periods = NA_character_,
    seasonal_period = 12, forecast_approach = "bottoms_up", date_type = "month", negative_forecast = FALSE,
    box_cox = FALSE, stationary = FALSE, feature_selection = FALSE, global_model_recipes = "R1",
    average_models = TRUE, max_model_average = 3, weekly_to_daily = FALSE, pca = FALSE,
    num_hyperparameters = 1, forecast_horizon = 6, multistep_horizon = FALSE,
    run_global_models = TRUE, run_local_models = FALSE, run_ensemble_models = FALSE,
    clean_missing_values = TRUE, clean_outliers = FALSE, hist_end_date = max(fixture$history$Date))
  for (setting in names(settings)) log[[setting]] <- settings[[setting]]
  write_data(log, combo = NULL, run_info = previous, output_type = "log", folder = "logs", suffix = NULL)
  log$run_name <- updated$run_name
  write_data(log, combo = NULL, run_info = updated, output_type = "log", folder = "logs", suffix = NULL)
  list(previous = previous, updated = updated, log = log, source = source, fitted = fitted,
    model_ids = model_ids, series = series, splits = splits,
    winners = c(first = average_id, second = model_ids[2]),
    input = dplyr::bind_rows(lapply(c("first", "second"), function(combo) {
      dplyr::mutate(fixture$history, Combo = combo)
    })),
    agent = list(run_id = "updated", agent_version = 2, forecast_horizon = 6,
      project_info = list(project_name = "global-selection", path = path, data_output = "csv",
        object_output = "rds", combo_variables = "Series", date_type = "month", weekly_to_daily = FALSE)),
    best = data.frame(combo = c("first", "second"), model_type = "global",
      best_run_name = previous$run_name, weighted_mape = 0.2))
}

local_global_update_selection_mocks <- function(fixture, .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$fits <- list()
  state$logged <- NULL
  state$transform <- function(fitted, retune) fitted
  original_reader <- read_file
  testthat::local_mocked_bindings(
    get_run_info = function(project_name, run_name, ...) {
      log <- fixture$log
      log$project_name <- project_name
      log$run_name <- run_name
      log
    },
    validate_prev_run_log = function(log) log,
    list_files = function(storage_object, path, ...) {
      if (!grepl("/input_data/", path, fixed = TRUE)) stop("saved selection must use exact artifact reads")
      "input.csv"
    },
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      if (identical(file_list, "input.csv")) return(fixture$input)
      original_reader(run_info, path = path, file_list = file_list, ...)
    },
    set_run_info = function(...) fixture$updated,
    prep_data = function(...) NULL,
    prep_models = function(...) NULL,
    get_prepped_models = function(...) tibble::tibble(Type = c("Train_Test_Splits", "Model_Hyperparameters"),
      Data = list(fixture$splits, data.frame(Hyperparameter_ID = 1L))),
    fit_models = function(trained_models_tbl, retune_hyperparameters, ...) {
      state$fits[[length(state$fits) + 1L]] <- trained_models_tbl$Model_ID
      fitted <- fixture$fitted[match(trained_models_tbl$Model_ID, fixture$model_ids), ]
      state$transform(fitted, retune_hyperparameters)
    },
    read_series_history = function(...) fixture$series,
    validate_run_outputs = function(...) TRUE,
    log_best_run = function(run_info, ...) { state$logged <- run_info$forecast_selection; "logged" },
    .package = "finnts", .env = .env
  )
  state
}

test_that("global updates refit selected components and retain each saved winner", {
  fixture <- make_global_update_selection_fixture()
  state <- local_global_update_selection_mocks(fixture)
  result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  expect_identical(result$status, "done")
  expect_length(state$fits, 1L)
  expect_setequal(state$fits[[1]], fixture$model_ids[1:2])
  expect_identical(state$logged$selections$first$selected_id, fixture$winners[["first"]])
  expect_identical(state$logged$selections$second$selected_id, fixture$winners[["second"]])
  rows <- read_candidate_forecasts(fixture$updated, c("first", "second"), fixture$log)
  chosen <- rows[rows$Best_Model == "Yes", ]
  expect_equal(chosen$Forecast, rep(100, nrow(chosen)))
  expect_false(any(rows$Model_ID == fixture$model_ids[3]))
})

test_that("an unselected series prediction cannot invalidate another global winner", {
  fixture <- make_global_update_selection_fixture()
  state <- local_global_update_selection_mocks(fixture)
  state$transform <- function(fitted, retune) {
    model_index <- which(fitted$Model_Name == "xgboost")
    rows <- fitted$Forecast_Tbl[[model_index]]
    rows$Forecast[rows$Combo == "second"] <- Inf
    fitted$Forecast_Tbl[[model_index]] <- rows
    fitted
  }
  result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  expect_length(result$quality_rejected_combos, 0L)
  expect_setequal(names(state$logged$selections), c("first", "second"))
  rows <- read_candidate_forecasts(fixture$updated, c("first", "second"), fixture$log)
  expect_true(all(is.finite(rows$Forecast)))
  expect_identical(unique(rows$Model_ID[rows$Combo == "second"]), fixture$winners[["second"]])
})

test_that("global retuning retains saved subsets and rejects only required failures", {
  fixture <- make_global_update_selection_fixture()
  fixture$best$weighted_mape <- 0
  state <- local_global_update_selection_mocks(fixture)
  state$transform <- function(fitted, retune) {
    fitted$Forecast_Tbl <- lapply(fitted$Forecast_Tbl, function(rows) {
      if (retune) {
        rows$Forecast[rows$Combo == "first" & rows$Train_Test_ID == 1L] <- Inf
      } else rows$Forecast <- rows$Forecast * 1.02
      rows
    })
    fitted
  }
  result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  expect_length(state$fits, 2L)
  expect_true(all(vapply(state$fits, function(ids) setequal(ids, fixture$model_ids[1:2]), logical(1))))
  expect_identical(result$quality_rejected_combos, hash_data("first"))
  expect_identical(names(state$logged$selections), "second")
  expect_identical(state$logged$selections$second$selected_id, fixture$winners[["second"]])
  rows <- read_candidate_forecasts(fixture$updated, "second", fixture$log)
  expect_equal(rows$Forecast, rep(100, nrow(rows)))
})

test_that("selected global component coverage cannot be repaired by its average", {
  lapply(c("missing_row", "duplicate_row", "missing_component"), function(defect) {
    fixture <- make_global_update_selection_fixture()
    state <- local_global_update_selection_mocks(fixture)
    state$transform <- function(fitted, retune) {
      model_index <- which(fitted$Model_Name == "xgboost")
      rows <- fitted$Forecast_Tbl[[model_index]]
      affected <- which(rows$Combo == "first")
      if (defect == "missing_row") rows <- rows[-affected[1], ]
      if (defect == "duplicate_row") rows <- dplyr::bind_rows(rows, rows[affected[1], ])
      if (defect == "missing_component") rows <- rows[-affected, ]
      fitted$Forecast_Tbl[[model_index]] <- rows
      fitted
    }
    result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
    expect_identical(result$quality_rejected_combos, hash_data("first"), info = defect)
    expect_identical(names(state$logged$selections), "second", info = defect)
  })
})

test_that("saved global winner evidence and fitted models are required", {
  fixture <- make_global_update_selection_fixture()
  state <- local_global_update_selection_mocks(fixture)
  average <- read_selection_file(fixture$previous, "forecasts", "-average_models", "first")
  average$Best_Model <- "No"
  write_data(average, combo = "first", run_info = fixture$previous,
    output_type = "data", folder = "forecasts", suffix = "-average_models")
  expect_error(update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123),
    "Saved global winner is missing or ambiguous")
  expect_length(state$fits, 0L)
  average$Best_Model <- "Yes"
  write_data(average, combo = "first", run_info = fixture$previous,
    output_type = "data", folder = "forecasts", suffix = "-average_models")
  source <- read_selection_file(fixture$previous, "forecasts", "-global_models", "first")
  source$Best_Model[source$Model_Name == "xgboost"] <- "Yes"
  write_data(source, combo = "first", run_info = fixture$previous,
    output_type = "data", folder = "forecasts", suffix = "-global_models")
  expect_error(update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123),
    "Saved global winner is missing or ambiguous")
  expect_length(state$fits, 0L)
  source$Best_Model <- "No"
  write_data(source, combo = "first", run_info = fixture$previous,
    output_type = "data", folder = "forecasts", suffix = "-global_models")
  model_path <- fs::path(fixture$previous$path, "models", paste0(hash_data(fixture$previous$project_name), "-",
    hash_data(fixture$previous$run_name), "-", hash_data("All-Data"), "-single_models.rds"))
  trained <- read_file(fixture$previous, file_list = model_path)
  write_data(trained[trained$Model_Name != "xgboost", ], combo = "All-Data", run_info = fixture$previous,
    output_type = "object", folder = "models", suffix = "-single_models")
  expect_error(update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123),
    "Saved selected model fits are missing or ambiguous")
  expect_length(state$fits, 0L)
})

test_that("global selected mappings survive persistence and input ordering", {
  fixture <- make_global_update_selection_fixture()
  state <- local_global_update_selection_mocks(fixture)
  fixture$best <- fixture$best[2:1, ]
  update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  restored <- read_global_update_selection(fixture$updated, fixture$log, c("first", "second"))
  expect_identical(restored$selected_ids, fixture$winners)
  expect_setequal(restored$components$first, fixture$model_ids[1:2])
  expect_identical(restored$components$second, fixture$model_ids[2])
  rows <- read_candidate_forecasts(fixture$updated, c("first", "second"), fixture$log)
  expect_false(anyDuplicated(rows[, c("Combo", "Model_ID", "Train_Test_ID", "Date")]) > 0)
  expect_true(all(vapply(state$logged$selections, function(selection) !is.na(selection$selected_id), logical(1))))
})

test_that("hierarchical global updates preserve heterogeneous source winners before solving", {
  fixture <- make_reconciled_selection_fixture()
  base <- fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]
  fitted <- make_fitted_selection_models(base)
  saved <- adjust_forecast(fitted, fixture$project_info, "standard_hierarchy", FALSE)
  average_id <- unique(saved$Model_ID[saved$Recipe_ID == "simple_average"])
  winners <- stats::setNames(rep(c("xgboost--global--R1", average_id),
    length.out = length(fixture$metadata$hts_combos)), fixture$metadata$hts_combos)
  saved$Best_Model <- ifelse(saved$Model_ID == unname(winners[saved$Combo]), "Yes", "No")
  contexts <- stats::setNames(lapply(fixture$metadata$hts_combos, function(combo) {
    context <- fixture$contexts[[1]]
    context$history$Target <- base$Forecast[base$Combo == combo & base$Train_Test_ID == 1L][1]
    context
  }), fixture$metadata$hts_combos)
  solver_input <- NULL
  local_mocked_bindings(
    read_selection_hierarchy = function(...) fixture$metadata,
    read_candidate_forecasts = function(..., reconciled = TRUE) {
      expect_false(reconciled)
      saved
    },
    read_series_history = function(run_info, combo, ...) contexts[[combo]],
    reconcile = function(initial_fcst, ...) {
      solver_input <<- initial_fcst
      bottoms <- utils::tail(fixture$metadata$hts_combos, length(fixture$metadata$original_combos))
      rows <- initial_fcst[initial_fcst$Combo %in% bottoms, ]
      rows$Combo <- fixture$metadata$original_combos[match(rows$Combo, bottoms)]
      rows$Model_ID <- "Best-Model"
      rows
    }
  )
  log <- data.frame(forecast_approach = "standard_hierarchy", date_type = "month", negative_forecast = FALSE)
  mapping <- read_global_update_selection(fixture$project_info, log, fixture$metadata$original_combos)
  expect_identical(mapping$selected_ids, winners)
  assembled <- adjust_forecast(fitted, fixture$project_info, "standard_hierarchy", FALSE,
    selected_models = mapping)
  result <- assess_update_forecasts(assembled, fixture$project_info, log, fixture$splits,
    expected_components = mapping$components, combos = fixture$metadata$original_combos)
  expect_length(result$quality_rejected_combos, 0L)
  expect_setequal(unique(solver_input$Combo), fixture$metadata$hts_combos)
  expect_true(all(solver_input$Best_Model == "Yes"))
  expect_identical(solver_input$Model_ID, unname(winners[solver_input$Combo]))
  expect_setequal(names(result$selections), fixture$metadata$original_combos)
})

test_that("uniform global and legacy local assembly retain their component arithmetic", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.02))
  forecasts <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  forecasts$Combo <- "series"
  forecasts$Run_Type <- fixture$context$train_test_split$Run_Type[
    match(forecasts$Train_Test_ID, fixture$context$train_test_split$Train_Test_ID)]
  fitted <- make_fitted_selection_models(forecasts)
  legacy <- adjust_forecast(fitted, list(), "bottoms_up", FALSE)
  average_id <- unique(legacy$Model_ID[legacy$Best_Model == "Yes"])
  mapping <- list(selected_ids = c(series = average_id),
    components = list(series = strsplit(average_id, "_", fixed = TRUE)[[1]]))
  mapped <- adjust_forecast(fitted[2:1, ], list(), "bottoms_up", FALSE, selected_models = mapping)
  columns <- c("Combo", "Model_ID", "Train_Test_ID", "Date", "Forecast", "Target", "Best_Model")
  expect_equal(dplyr::arrange(mapped[, columns], Combo, Model_ID, Train_Test_ID, Date),
    dplyr::arrange(legacy[, columns], Combo, Model_ID, Train_Test_ID, Date))
  fitted$Model_Type <- "local"
  fitted$Combo_ID <- "series"
  local_average <- adjust_forecast(fitted, list(), "bottoms_up", FALSE)
  expect_true(all(local_average$Best_Model[local_average$Recipe_ID == "simple_average"] == "Yes"))
  local_single <- adjust_forecast(fitted[1, ], list(), "bottoms_up", FALSE)
  expect_true(all(local_single$Best_Model == "Yes"))
  columns <- c("Combo", "Train_Test_ID", "Date", "Forecast")
  expect_equal(dplyr::arrange(local_single[, columns], Combo, Train_Test_ID, Date),
    dplyr::arrange(tibble::as_tibble(fitted$Forecast_Tbl[[1]][, columns]), Combo, Train_Test_ID, Date))
})