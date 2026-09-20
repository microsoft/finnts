# Construct selected local/global predictions and small real serialized fits for
# update tests. Cadence and signed/zero actuals exercise reporting invariants;
# expensive engines are mocked and the temporary artifacts belong to the caller.
make_global_update_selection_fixture <- function(date_type = "month", weekly_to_daily = FALSE,
                                                 zero_targets = FALSE, global = TRUE,
                                                 signed_targets = FALSE) {
  path <- withr::local_tempdir(pattern = "finnts-global-selection-", .local_envir = parent.frame())
  combos <- if (global) c("first", "second") else "first"
  combo_id <- if (global) "All-Data" else "first"
  model_type <- if (global) "global" else "local"
  previous <- set_run_info(project_name = paste0("global-selection_", hash_data(if (global) "all" else "first")),
    run_name = "previous", path = path, data_output = "csv", add_unique_id = FALSE)
  updated <- previous
  updated$run_name <- "updated"
  actuals <- rep(100, 36)
  if (zero_targets) actuals[c(33, 35)] <- 0
  if (signed_targets) actuals[32] <- -100
  fixture <- make_selection_case(actuals = actuals,
    futures = list(template = rep(100, 6)), errors = c(template = 0), date_type = date_type)
  series <- fixture$context
  series$history <- fixture$history
  splits <- series$train_test_split
  write_data(splits, combo = NULL, run_info = previous, output_type = "data",
    folder = "prep_models", suffix = "-train_test_split")
  models <- c("xgboost", "chronos2", "timegpt")
  model_ids <- paste(models, model_type, "R1", sep = "--")
  template <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::group_by(Train_Test_ID) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup()
  source <- dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
    dplyr::bind_rows(lapply(combos, function(combo) {
      rows <- template
      rows$Combo <- combo
      rows$Combo_ID <- combo_id
      rows$Model_Name <- models[model_index]
      rows$Model_Type <- model_type
      rows$Recipe_ID <- "R1"
      rows$Model_ID <- model_ids[model_index]
      rows$Hyperparameter_ID <- 1L
      rows$Run_Type <- splits$Run_Type[match(rows$Train_Test_ID, splits$Train_Test_ID)]
      rows$Forecast <- c(98, if (combo == "first") if (global) 102 else 104 else 100, 103)[model_index]
      observed <- !is.na(rows$Target)
      rows$Forecast[observed] <- rows$Forecast[observed] * rows$Target[observed] / 100
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
  average$Forecast <- (average$Forecast +
    source$Forecast[source$Combo == "first" & source$Model_ID == model_ids[2]]) / 2
  average$Best_Model <- "Yes"
  average <- create_prediction_intervals(average, splits)
  for (combo in combos) {
    saved_source <- convert_weekly_to_daily(source[source$Combo == combo, ], date_type, weekly_to_daily)
    write_data(saved_source, combo = combo, run_info = previous,
      output_type = "data", folder = "forecasts", suffix = if (global) "-global_models" else "-single_models")
  }
  write_data(convert_weekly_to_daily(average, date_type, weekly_to_daily), combo = "first", run_info = previous,
    output_type = "data", folder = "forecasts", suffix = "-average_models")
  fitted <- dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
    make_fitted_selection_models(source[source$Model_ID == model_ids[model_index], ], models[model_index])
  }))
  fitted$Combo_ID <- combo_id
  fitted$Model_Type <- model_type
  fitted$Model_Fit <- rep(list(stats::lm(mpg ~ wt, data = mtcars)), nrow(fitted))
  trained <- fitted[, c("Combo_ID", "Model_Name", "Model_Type", "Recipe_ID", "Model_Fit")]
  trained$Model_ID <- model_ids
  write_data(trained, combo = combo_id, run_info = previous,
    output_type = "object", folder = "models", suffix = "-single_models")
  log <- read_selection_file(previous, "logs")
  settings <- list(models_to_run = paste(models, collapse = "---"), recipes_to_run = "R1",
    external_regressors = NA_character_, lag_periods = NA_character_, rolling_window_periods = NA_character_,
    seasonal_period = 12, forecast_approach = "bottoms_up", date_type = date_type, negative_forecast = signed_targets,
    box_cox = FALSE, stationary = FALSE, feature_selection = FALSE, global_model_recipes = "R1",
    average_models = TRUE, max_model_average = 3, weekly_to_daily = weekly_to_daily, pca = FALSE,
    num_hyperparameters = 1, forecast_horizon = 6, multistep_horizon = FALSE,
    run_global_models = global, run_local_models = !global, run_ensemble_models = FALSE,
    clean_missing_values = TRUE, clean_outliers = FALSE, hist_end_date = max(fixture$history$Date))
  for (setting in names(settings)) log[[setting]] <- settings[[setting]]
  write_data(log, combo = NULL, run_info = previous, output_type = "log", folder = "logs", suffix = NULL)
  log$run_name <- updated$run_name
  write_data(log, combo = NULL, run_info = updated, output_type = "log", folder = "logs", suffix = NULL)
  list(previous = previous, updated = updated, log = log, source = source, fitted = fitted,
    model_ids = model_ids, series = series, splits = splits,
    winners = c(first = average_id, second = model_ids[2])[combos],
    input = dplyr::bind_rows(lapply(combos, function(combo) {
      dplyr::mutate(fixture$history, Combo = combo)
    })),
    agent = list(run_id = "updated", agent_version = 2, forecast_horizon = 6,
      project_info = list(project_name = "global-selection", path = path, data_output = "csv",
        object_output = "rds", combo_variables = "Series", date_type = date_type, weekly_to_daily = weekly_to_daily)),
    best = data.frame(combo = combos, model_type = model_type,
      best_run_name = previous$run_name, weighted_mape = 0.2))
}

local_global_update_selection_mocks <- function(fixture, .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$fits <- list()
  state$logged <- NULL
  state$metric <- NULL
  state$retunes <- logical()
  state$reads <- character()
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
      state$reads <- c(state$reads, path, file_list)
      if (identical(file_list, "input.csv") || any(grepl("/input_data/", file_list, fixed = TRUE))) {
        return(fixture$input)
      }
      original_reader(run_info, path = path, file_list = file_list, ...)
    },
    set_run_info = function(...) fixture$updated,
    prep_data = function(...) NULL,
    prep_models = function(...) NULL,
    get_prepped_models = function(...) tibble::tibble(Type = c("Train_Test_Splits", "Model_Hyperparameters"),
      Data = list(fixture$splits, data.frame(Hyperparameter_ID = 1L))),
    fit_models = function(trained_models_tbl, retune_hyperparameters, ...) {
      state$fits[[length(state$fits) + 1L]] <- trained_models_tbl$Model_ID
      state$retunes <- c(state$retunes, retune_hyperparameters)
      fitted <- fixture$fitted[match(trained_models_tbl$Model_ID, fixture$model_ids), ]
      state$transform(fitted, retune_hyperparameters)
    },
    read_series_history = function(...) fixture$series,
    validate_run_outputs = function(...) TRUE,
    log_best_run = function(run_info, weighted_mape, ...) {
      state$logged <- run_info$forecast_selection
      state$metric <- weighted_mape
      "logged"
    },
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
  expect_true(attr(state$metric, "selection_ok"))
  expect_named(attr(state$metric, "model_accuracy"),
    c("model_avg_wmape", "model_median_wmape", "model_std_wmape"))
  expect_equal(as.numeric(state$metric), 0)
  expect_equal(attr(state$metric, "forecast_accuracy"),
    list(weighted_mape = 0, by_series = c(first = 0, second = 0)))
  rows <- read_candidate_forecasts(fixture$updated, c("first", "second"), fixture$log)
  chosen <- rows[rows$Best_Model == "Yes", ]
  expect_equal(chosen$Forecast, rep(100, nrow(chosen)))
  expect_false(any(rows$Model_ID == fixture$model_ids[3]))
})

test_that("weekly updates retain native aggregate and completed per-series accuracy", {
  original_logger <- log_best_run
  original_converter <- convert_weekly_to_daily
  cases <- list(
    list(global = TRUE, daily = TRUE, signed = FALSE, retune = FALSE),
    list(global = FALSE, daily = TRUE, signed = TRUE, retune = FALSE),
    list(global = FALSE, daily = FALSE, signed = FALSE, retune = TRUE),
    list(global = TRUE, daily = TRUE, signed = TRUE, retune = TRUE)
  )
  for (case in cases) local({
    fixture <- make_global_update_selection_fixture("week", case$daily, zero_targets = TRUE,
      global = case$global, signed_targets = case$signed)
    fixture$best$weighted_mape <- if (case$retune) 0 else if (case$global) 0.001 else 0.011
    state <- local_global_update_selection_mocks(fixture)
    state$conversions <- 0L
    local_mocked_bindings(
      log_best_run = function(agent_info, run_info, weighted_mape, ...) {
        state$metric <- weighted_mape
        original_logger(agent_info, run_info, weighted_mape, ...)
      },
      convert_weekly_to_daily = function(...) {
        state$conversions <- state$conversions + 1L
        original_converter(...)
      }
    )

    result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)

    expect_identical(result$status, "done")
    expect_identical(state$retunes, if (case$retune) c(FALSE, TRUE) else FALSE)
    expect_identical(state$conversions, 1L)
    expect_false(any(grepl("/forecasts/", state$reads, fixed = TRUE) &
      grepl(hash_data(fixture$updated$run_name), state$reads, fixed = TRUE)))
    native <- if (case$global) 0.0005 else 0.0105
    expect_equal(as.numeric(state$metric), native)
    expect_equal(read_selection_file(fixture$updated, "logs")$weighted_mape, native)
    parent <- fixture$agent$project_info
    parent$run_name <- fixture$agent$run_id
    saved_before <- list()
    rows_before <- list()
    for (combo in fixture$best$combo) {
      rows <- read_selection_file(fixture$updated, "forecasts",
        if (combo == "first") "-average_models" else "-global_models", combo)
      rows_before[[combo]] <- rows
      expect_identical("Date_Day" %in% names(rows), case$daily)
      expect_false("Run_Type" %in% names(rows))
      expect_equal(nrow(rows), 12L * if (case$daily) 7L else 1L)
      rows$Run_Type <- fixture$splits$Run_Type[match(rows$Train_Test_ID, fixture$splits$Train_Test_ID)]
      expected <- round(calc_wmape(rows), 4)
      expect_equal(expected, if (!case$daily) native else if (case$global) 0.0035 else 0.0135)
      saved <- read_selection_file(parent, "logs", "-agent_best_run", combo)
      saved_before[[combo]] <- saved
      expect_equal(saved$weighted_mape, expected)
      if (case$global) {
        expect_equal(saved$model_avg_wmape, native)
        expect_equal(saved$model_median_wmape, native)
        expect_equal(saved$model_std_wmape, 0)
      } else {
        models <- read_selection_file(fixture$updated, "forecasts", "-single_models", combo)
        models$Run_Type <- fixture$splits$Run_Type[match(models$Train_Test_ID, fixture$splits$Train_Test_ID)]
        models$Best_Model <- "Yes"
        accuracy <- vapply(split(models, models$Model_ID), calc_wmape, numeric(1))
        expect_equal(saved$model_avg_wmape, mean(accuracy))
        expect_equal(saved$model_median_wmape, stats::median(accuracy))
        expect_equal(saved$model_std_wmape, stats::sd(accuracy))
        expect_gt(saved$model_std_wmape, 0)
      }
    }

    repeated <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)

    expect_identical(repeated$status, "done")
    expect_identical(state$retunes, if (case$retune) c(FALSE, TRUE) else FALSE)
    expect_identical(state$conversions, 1L)
    for (combo in fixture$best$combo) {
      expect_equal(read_selection_file(parent, "logs", "-agent_best_run", combo), saved_before[[combo]])
      expect_equal(read_selection_file(fixture$updated, "forecasts",
        if (combo == "first") "-average_models" else "-global_models", combo), rows_before[[combo]])
    }
  })
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

test_that("workers finish interrupted logging without refitting saved outputs", {
  for (global in c(FALSE, TRUE)) local({
    fixture <- make_global_update_selection_fixture("week", TRUE, global = global)
    original_logger <- log_best_run
    state <- local_global_update_selection_mocks(fixture)
    state$interrupt <- TRUE
    state$before_metric <- NULL
    local_mocked_bindings(log_best_run = function(run_info, weighted_mape, ...) {
      if (state$interrupt) {
        state$before_metric <- weighted_mape
        stop("logging interrupted", call. = FALSE)
      }
      expect_equal(weighted_mape, state$before_metric)
      original_logger(run_info = run_info, weighted_mape = weighted_mape, ...)
    })
    expect_error(update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123),
      "logging interrupted")
    paths <- c(fs::dir_ls(fs::path(fixture$updated$path, "models")),
      fs::dir_ls(fs::path(fixture$updated$path, "forecasts")))
    before <- tools::md5sum(paths)
    state$interrupt <- FALSE
    result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
    expect_identical(result$status, "done")
    expect_length(state$fits, 1L)
    expect_identical(tools::md5sum(paths), before)
    parent <- fixture$agent$project_info
    parent$run_name <- fixture$agent$run_id
    for (combo in fixture$best$combo) {
      saved <- read_selection_file(parent, "logs", "-agent_best_run", combo)
      expect_identical(saved$best_run_name, fixture$updated$run_name)
      expect_equal(saved$weighted_mape,
        round(unname(attr(state$before_metric, "forecast_accuracy")$by_series[combo]), 4))
      expect_false("rebuild_update_models" %in% names(saved))
    }
  })
})

test_that("a worker refits a corrupt current result in its existing paths", {
  fixture <- make_global_update_selection_fixture(global = FALSE)
  state <- local_global_update_selection_mocks(fixture)
  update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  model_path <- local_artifact_path(fixture$updated, "models", "-single_models", hash_data("first"), "rds")
  writeBin(charToRaw("broken fitted model"), model_path)
  result <- update_forecast_combo(fixture$agent, fixture$best, NULL, 1, FALSE, 123)
  expect_identical(result$status, "done")
  expect_length(state$fits, 2L)
  expect_true(all(!vapply(readRDS(model_path)$Model_Fit, is.null, logical(1))))
})

test_that("a damaged accepted default reaches training without preparation changes", {
  fixture <- make_global_update_selection_fixture(global = FALSE)
  state <- local_global_update_selection_mocks(fixture)
  agent <- fixture$agent
  agent$default_reforecast <- TRUE
  log <- fixture$log
  log$default_reforecast_status <- "accepted"
  write_data(log, combo = NULL, run_info = fixture$updated, output_type = "log",
    folder = "logs", suffix = NULL)
  inputs <- list(models_to_run = "meanf", external_regressors = "NULL",
    clean_missing_values = TRUE, clean_outliers = FALSE, stationary = FALSE,
    negative_forecast = FALSE, forecast_approach = "bottoms_up", lag_periods = "NULL",
    rolling_window_periods = "NULL", recipes_to_run = "R1", multistep_horizon = FALSE,
    seasonal_period = "NULL", feature_selection = FALSE)
  local_mocked_bindings(
    read_local_artifacts = function(...) fixture$input,
    assess_agent_run = function(...) stop("damaged output cannot be restored"),
    train_models = function(run_info, ...) {
      expect_true(isTRUE(run_info$rebuild_update_models))
      stop("training reached", call. = FALSE)
    }
  )
  expect_error(submit_fcst_run(agent, inputs, hash_data("first"), "default",
    num_cores = 1), "training reached")
  expect_false("rebuild_update_models" %in% names(read_selection_file(fixture$updated, "logs")))
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