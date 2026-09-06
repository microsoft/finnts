test_that("quality-rejected updates bypass only the ordinary failure limit", {
  local_mocked_bindings(resolve_combo_hashes = function(agent_info, combos) combos)
  combos <- paste0("combo-", 1:30)
  previous <- data.frame(combo = combos)
  expect_setequal(check_update_failures(list(), previous, combos, character(), character(),
    global_quality_rejected = combos), combos)
  expect_error(check_update_failures(list(), previous, combos, combos[1:11], character(),
    global_quality_rejected = combos), "exceeds")
  expect_setequal(check_update_failures(list(), previous, combos, combos[1], character(),
    global_quality_rejected = c(combos[1:2], "removed")), combos[1:2])
})

test_that("update assessment preserves healthy series and rejects a bad sibling", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.03))
  series <- fixture$context
  series$history <- fixture$history
  local_mocked_bindings(read_series_history = function(...) series)
  rows <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::mutate(Combo = "healthy", Best_Model = "Yes", Recipe_ID = "R1")
  bad <- rows %>% dplyr::mutate(Combo = "bad", Forecast = ifelse(Train_Test_ID == 1, 1e8, Forecast))
  result <- assess_update_forecasts(dplyr::bind_rows(rows, bad), list(), list(), fixture$context$train_test_split)
  expect_identical(unique(result$forecasts$Combo), "healthy")
  expect_identical(result$quality_rejected_combos, hash_data("bad"))
})

test_that("update forecast assembly leaves reconciliation until after quality assessment", {
  fixture <- make_reconciled_selection_fixture()
  predictions <- fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]
  fitted <- make_fitted_selection_models(predictions)
  solver_calls <- 0L
  local_mocked_bindings(reconcile = function(initial_fcst, ...) {
    solver_calls <<- solver_calls + 1L
    initial_fcst
  })
  result <- adjust_forecast(fitted, fixture$project_info, "standard_hierarchy", FALSE)
  expect_identical(solver_calls, 0L)
  expect_setequal(unique(result$Combo), fixture$metadata$hts_combos)
  expect_setequal(unique(result$Recipe_ID), c("R1", "simple_average"))
  expect_true(all(result$Best_Model[result$Recipe_ID == "simple_average"] == "Yes"))
})

test_that("hierarchical update quality is checked before a single selected-only reconciliation", {
  fixture <- make_reconciled_selection_fixture()
  raw <- adjust_forecast(make_fitted_selection_models(
    fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]), fixture$project_info,
    "standard_hierarchy", FALSE)
  contexts <- stats::setNames(lapply(fixture$metadata$hts_combos, function(combo) {
    context <- fixture$contexts[[1]]
    context$history$Target <- raw$Forecast[raw$Combo == combo & raw$Train_Test_ID == 1][1]
    context
  }), fixture$metadata$hts_combos)
  events <- character()
  original_selector <- select_series_forecasts
  original_reconcile <- reconcile
  local_mocked_bindings(
    read_selection_hierarchy = function(...) fixture$metadata,
    read_series_history = function(run_info, combo, ...) contexts[[combo]],
    read_file = function(run_info, path, return_type = "df", ...) {
      if (return_type == "object") fixture$metadata else fixture$history
    },
    select_series_forecasts = function(...) { events <<- c(events, "assess"); original_selector(...) },
    reconcile = function(initial_fcst, ...) {
      events <<- c(events, "reconcile")
      expect_true(all(initial_fcst$Best_Model == "Yes"))
      original_reconcile(initial_fcst, ...)
    }
  )
  log <- data.frame(forecast_approach = "standard_hierarchy", date_type = "month", negative_forecast = FALSE)
  result <- assess_update_forecasts(raw, fixture$project_info, log, fixture$splits)
  expect_identical(events, c(rep("assess", length(contexts)), "reconcile"))
  expect_setequal(unique(result$forecasts$Combo), fixture$metadata$original_combos)
  expect_true(all(result$forecasts$Best_Model == "Yes"))
  expect_identical(names(result$source_selections), fixture$metadata$hts_combos)

  raw$Forecast[raw$Combo == fixture$metadata$hts_combos[1] &
    raw$Recipe_ID == "R1" & raw$Train_Test_ID == 1] <- Inf
  rejected <- assess_update_forecasts(raw, fixture$project_info, log, fixture$splits)
  expect_equal(nrow(rejected$forecasts), 0)
  expect_setequal(rejected$quality_rejected_combos,
    vapply(fixture$metadata$original_combos, hash_data, character(1), USE.NAMES = FALSE))
  expect_equal(sum(events == "reconcile"), 1)
  missing <- raw[raw$Combo != fixture$metadata$hts_combos[1], ]
  expect_equal(nrow(assess_update_forecasts(missing, fixture$project_info, log, fixture$splits)$forecasts), 0)
  expect_equal(sum(events == "reconcile"), 1)
})

test_that("hierarchical refit and retune preserve solver settings before final logging", {
  fixture <- make_reconciled_selection_fixture()
  base <- fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]
  fitted <- make_fitted_selection_models(base)
  contexts <- stats::setNames(lapply(fixture$metadata$hts_combos, function(combo) {
    context <- fixture$contexts[[1]]
    context$history$Target <- base$Forecast[base$Combo == combo & base$Train_Test_ID == 1][1]
    context
  }), fixture$metadata$hts_combos)
  previous <- data.frame(models_to_run = "xgboost---chronos2", external_regressors = NA_character_,
    lag_periods = NA_character_, rolling_window_periods = NA_character_, seasonal_period = 12,
    forecast_approach = "standard_hierarchy", date_type = "month", negative_forecast = FALSE,
    box_cox = FALSE, stationary = FALSE, feature_selection = FALSE, global_model_recipes = "R1",
    average_models = TRUE, max_model_average = 3L, weekly_to_daily = FALSE)
  events <- character()
  settings <- list()
  writes <- list()
  logged <- NULL
  invalid_retune <- FALSE
  original_selector <- select_series_forecasts
  original_reconcile <- reconcile
  local_mocked_bindings(
    get_run_info = function(...) previous,
    validate_prev_run_log = function(log) log,
    list_files = function(...) "input.csv",
    read_file = function(run_info, file_list = NULL, path = NULL, return_type = "df", ...) {
      if (return_type == "object") return(fixture$metadata)
      if (!is.null(path)) return(fixture$history)
      if (any(grepl("/models/", file_list, fixed = TRUE))) {
        return(data.frame(Model_ID = c("xgboost--global--R1", "chronos2--global--R1")))
      }
      fixture$history
    },
    set_run_info = function(...) fixture$project_info,
    prep_data = function(...) NULL,
    prep_models = function(...) NULL,
    get_prepped_models = function(...) tibble::tibble(Type = c("Train_Test_Splits", "Model_Hyperparameters"),
      Data = list(fixture$splits, data.frame(Hyperparameter_ID = 1L))),
    fit_models = function(retune_hyperparameters, ...) {
      events <<- c(events, if (retune_hyperparameters) "retune" else "refit")
      updated <- fitted
      if (retune_hyperparameters && invalid_retune) {
        updated$Forecast_Tbl <- lapply(updated$Forecast_Tbl, function(rows) {
          rows$Forecast[rows$Combo == fixture$metadata$hts_combos[1] & rows$Train_Test_ID == 1L] <- Inf
          rows
        })
      }
      updated
    },
    read_selection_file = function(...) previous[, names(previous) != "negative_forecast", drop = FALSE],
    read_selection_hierarchy = function(...) fixture$metadata,
    read_candidate_forecasts = function(...) {
      adjust_forecast(fitted, fixture$project_info, "standard_hierarchy", FALSE)
    },
    read_series_history = function(run_info, combo, ...) contexts[[combo]],
    select_series_forecasts = function(...) { events <<- c(events, "assess"); original_selector(...) },
    reconcile = function(initial_fcst, run_info, forecast_approach, negative_forecast) {
      events <<- c(events, "reconcile")
      settings[[length(settings) + 1L]] <<- negative_forecast
      expect_true(all(initial_fcst$Best_Model == "Yes"))
      original_reconcile(initial_fcst, run_info, forecast_approach, negative_forecast)
    },
    write_data = function(x, combo, suffix, ...) { writes[[length(writes) + 1L]] <<- list(data = x, combo = combo, suffix = suffix) },
    validate_run_outputs = function(...) TRUE,
    log_best_run = function(run_info, ...) { logged <<- run_info$forecast_selection }
  )
  agent <- list(project_info = fixture$project_info, run_id = "updated", forecast_horizon = 6)
  selected <- data.frame(combo = fixture$metadata$original_combos, model_type = "global",
    best_run_name = "previous", weighted_mape = 0.01)
  expect_no_error(update_forecast_combo(agent, selected, NULL, 1, FALSE, 123))
  expect_identical(events, c("refit", rep("assess", length(contexts)), "reconcile",
    "retune", rep("assess", length(contexts)), "reconcile"))
  expect_identical(settings, list(FALSE, FALSE))
  expect_setequal(names(logged$source_selections), fixture$metadata$hts_combos)
  expect_setequal(names(logged$selections), fixture$metadata$original_combos)
  expect_equal(sum(vapply(writes, function(write) identical(write$suffix, "-reconciled"), logical(1))), 1L)
  expect_equal(sum(vapply(writes, function(write) identical(write$suffix, "-global_models"), logical(1))), length(contexts))

  events <- character()
  settings <- list()
  writes <- list()
  logged <- NULL
  invalid_retune <- TRUE
  rejected <- update_forecast_combo(agent, selected, NULL, 1, FALSE, 123)
  expect_identical(events, c("refit", rep("assess", length(contexts)), "reconcile",
    "retune", rep("assess", length(contexts))))
  expect_identical(settings, list(FALSE))
  expect_length(writes, 0L)
  expect_null(logged)
  expect_setequal(rejected$quality_rejected_combos,
    vapply(fixture$metadata$original_combos, hash_data, character(1), USE.NAMES = FALSE))
})

test_that("update assessment rejects an invalid component even if its average looks valid", {
  fixture <- make_selection_case()
  series <- fixture$context
  series$history <- fixture$history
  local_mocked_bindings(read_series_history = function(...) series)
  rows <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::mutate(Combo = "Synthetic", Best_Model = ifelse(Model_ID == "safe", "Yes", "No"),
      Recipe_ID = ifelse(Model_ID == "safe", "simple_average", "R1"),
      Forecast = ifelse(Model_ID == "accurate" & Train_Test_ID == 1, NA_real_, Forecast))
  result <- assess_update_forecasts(rows, list(), list(), fixture$context$train_test_split)
  expect_equal(nrow(result$forecasts), 0)
  expect_identical(result$quality_rejected_combos, hash_data("Synthetic"))
})

test_that("quality failures are not retried as infrastructure failures", {
  calls <- 0L
  local_mocked_bindings(
    submit_fcst_run = function(...) {
      calls <<- calls + 1L
      rlang::abort("no acceptable candidate", class = "finnts_forecast_selection_rejected")
    },
    wait_before_retry = function(...) stop("must not wait")
  )
  expect_error(execute_node(list(fn = "submit_fcst_run", max_retry = 3, retry_mode = "plain"),
    list(args = list(), results = list(), attempts = list()), NULL),
    class = "finnts_forecast_selection_rejected")
  expect_identical(calls, 1L)
})

test_that("final selection returns rejected evaluations without publishing a best model", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  run_info <- make_best_models_fixture()
  path <- locate_single_models_file(run_info)
  rows <- read_fcst_file(path)
  rows$Forecast[rows$Train_Test_ID == 1] <- Inf
  write_fcst_file(rows, path)
  run_info$allow_quality_rejection <- TRUE
  result <- final_models(run_info)
  expect_identical(result$rejected_combos, "Synthetic")
  expect_true(all(read_fcst_file(path)$Best_Model == "No"))
  expect_error(get_forecast_data(run_info), "missing a best model")
})

test_that("Agent run comparison chooses lower accuracy error and keeps a fixed anchor", {
  fixture <- make_selection_case()
  first <- fixture
  first$context$candidate_ids <- "accurate"
  second <- fixture
  second$context$candidate_ids <- "safe"
  results <- list(
    list(selections = list(series = do.call(select_forecast_candidate, first))),
    list(selections = list(series = do.call(select_forecast_candidate, second)))
  )
  expect_identical(rank_agent_run_selections(results)$series$series, 1L)
  expect_identical(rank_agent_run_selections(results)$best_run_index, 1L)
  rejected <- results[[1]]
  rejected$selections$series$selected_id <- NA_character_
  expect_false(agent_selection_summary(rejected)$acceptable)
  expect_identical(agent_selection_summary(rejected)$status, "rejected")
  expect_identical(rank_agent_run_selections(c(results, list(rejected)))$series$series, 1L)
})

test_that("seasonal preferences remain in selection rather than iteration comparison", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  fixture <- make_selection_case(100 + rep(10 * profile, 4),
    futures = list(diluted = 100 + 5 * profile, intact = 100 + 10 * profile),
    errors = c(diluted = 0.0815, intact = 0.083))
  results <- lapply(c("diluted", "intact"), function(model_id) {
    candidate <- fixture
    candidate$context$candidate_ids <- model_id
    list(selections = list(series = do.call(select_forecast_candidate, candidate)))
  })
  summary <- agent_selection_summary(results[[1]])
  expect_true(summary$acceptable)
  expect_equal(summary$seasonal_fidelity, 0.45, tolerance = 1e-10)
  expect_identical(rank_agent_run_selections(results)$series$series, 1L)
  expect_identical(rank_agent_run_selections(results)$best_run_index, 1L)

  series <- fixture$context
  series$history <- fixture$history
  local_mocked_bindings(read_series_history = function(...) series)
  rows <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::filter(Model_ID == "diluted") %>%
    dplyr::mutate(Combo = "series", Best_Model = "Yes", Recipe_ID = "R1")
  reused <- assess_update_forecasts(rows, list(), list(), fixture$context$train_test_split)
  expect_length(reused$quality_rejected_combos, 0L)
  expect_equal(nrow(reused$forecasts), nrow(rows))

  first <- list(selections = results[[2]]$selections,
    source_selections = list(root = results[[1]]$selections$series, leaf = results[[2]]$selections$series))
  first$selections$series$rankings$WMAPE <- 0.02
  second <- first
  second$source_selections$root <- results[[2]]$selections$series
  second$selections$series$rankings$WMAPE <- 0.022
  summary <- agent_selection_summary(first, check_quality = TRUE)
  expect_equal(summary$weighted_mape, 0.02)
  expect_equal(summary$seasonal_fidelity, 0.45, tolerance = 1e-10)
  expect_true(summary$acceptable)
  expect_identical(rank_agent_run_selections(list(first, second))$series$series, 1L)
  expect_identical(rank_agent_run_selections(list(first, second))$best_run_index, 1L)
  first$source_selections$root$rankings$Seasonal_Fidelity <- NA_real_
  expect_identical(agent_selection_summary(first, check_quality = TRUE)$seasonal_fidelity, NA_real_)
  expect_identical(rank_agent_run_selections(list(first, second))$best_run_index, 1L)
  results[[1]]$selections$series$rankings$Seasonal_Fidelity <- NULL
  expect_identical(agent_selection_summary(results[[1]])$seasonal_fidelity, NA_real_)
  expect_identical(rank_agent_run_selections(results)$best_run_index, 1L)
})

test_that("selected Agent results use existing log files only", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  info$forecast_selection <- final_models(info)
  agent <- list(project_info = info, run_id = "agent-parent", agent_version = 1,
    forecast_approach = "bottoms_up")
  result <- log_best_run(agent, info, 0.1, combo = hash_data("Synthetic"), check_best_run = FALSE)
  expect_identical(result$status, "evaluated")
  saved_log <- read_selection_file(info, "logs")
  expect_identical(saved_log$selection_status, "evaluated")
  parent <- info
  parent$run_name <- agent$run_id
  best <- read_selection_file(parent, "logs", "-agent_best_run", "Synthetic")
  expect_identical(best$best_run_name, info$run_name)
  expect_true(is.finite(best$weighted_mape))
  expect_false(any(grepl("quality|selection", list.files(info$path, recursive = TRUE))))
})

test_that("rejected Agent attempts keep finite bookkeeping and consume history slots", {
  fixture <- make_selection_case()
  fixture$forecasts$Forecast <- Inf
  rejected <- do.call(select_forecast_candidate, fixture)
  run_log <- data.frame(run_name = "attempt", models_to_run = "arima")
  agent <- list(agent_version = 2, forecast_approach = "bottoms_up")
  recorded <- record_agent_selection_attempt(run_log, list(selections = list(series = rejected)), agent)
  expect_identical(recorded$selection_status, "rejected")
  expect_true(is.na(recorded$weighted_mape))
  expect_equal(new_reason_history(recorded, agent_version = 2)$total_runs, 1)
})

test_that("partial global runs cannot win by omitting a difficult series", {
  fixture <- make_selection_case()
  selection <- do.call(select_forecast_candidate, fixture)
  rejected <- selection
  rejected$selected_id <- NA_character_
  results <- list(
    list(selections = list(first = selection, second = rejected)),
    list(selections = list(first = selection, second = selection))
  )
  ranked <- rank_agent_run_selections(results)
  expect_identical(ranked$best_run_index, 2L)
  expect_identical(ranked$series$first, 1L)
  expect_identical(ranked$series$second, 2L)
})

test_that("hierarchical resume restores accuracy without source-quality assessment", {
  fixture <- make_reconciled_selection_fixture()
  base <- fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]
  base$Best_Model <- "Yes"
  contexts <- stats::setNames(lapply(fixture$metadata$hts_combos, function(combo) {
    level <- base$Forecast[base$Combo == combo & base$Train_Test_ID == 1][1]
    context <- fixture$contexts[[1]]
    context$history$Target <- level
    context
  }), fixture$metadata$hts_combos)
  delivered <- base[base$Combo %in% fixture$run_inputs$combo, ]
  delivered$Combo <- fixture$metadata$original_combos[match(delivered$Combo, fixture$run_inputs$combo)]
  delivered$Model_ID <- "Best-Model"
  delivered$Forecast <- ifelse(delivered$Train_Test_ID == 1, 1e8, delivered$Target * 1.02)
  assessed_combos <- character()
  local_mocked_bindings(
    list_files = function(...) stop("must not list"),
    read_selection_hierarchy = function(...) fixture$metadata,
    read_selection_file = function(...) fixture$splits,
    read_candidate_forecasts = function(..., reconciled = TRUE) {
      if (!reconciled) stop("must not reload source forecasts for quality assessment")
      delivered
    },
    read_series_history = function(run_info, combo, ...) contexts[[combo]],
    select_series_forecasts = function(predictions, ...) {
      assessed_combos <<- c(assessed_combos, unique(predictions$Combo))
      stop("must not repeat future-quality selection")
    }
  )
  log <- data.frame(forecast_approach = "standard_hierarchy", date_type = "month")
  result <- assess_agent_run(fixture$project_info, log, fixture$metadata$original_combos)
  expect_true(agent_selection_summary(result)$acceptable)
  expect_equal(agent_selection_summary(result)$weighted_mape, 0.02)
  expect_identical(names(result$selections), fixture$metadata$original_combos)
  expect_length(result$source_selections, 0L)
  expect_length(assessed_combos, 0L)

  root <- base$Combo == fixture$metadata$hts_combos[1] & base$Train_Test_ID == 1
  base$Forecast[root] <- base$Forecast[root] * 3
  concerned <- assess_agent_run(fixture$project_info, log, fixture$metadata$original_combos)
  expect_true(agent_selection_summary(concerned)$acceptable)
  expect_true(is.na(agent_selection_summary(concerned)$risk))
  expect_equal(agent_selection_summary(concerned)$weighted_mape, 0.02)
  expect_equal(concerned$selections, result$selections)
})

test_that("fresh and resumed hierarchical artifacts preserve heterogeneous source winners", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    fixture <- make_hierarchical_selection_artifacts(approach)
    result <- final_models(fixture$run_info, average_models = FALSE, weekly_to_daily = FALSE)
    log <- read_selection_file(fixture$run_info, "logs")
    expect_identical(names(result$selections), fixture$metadata$original_combos)
    expect_setequal(names(result$source_selections), fixture$metadata$hts_combos)
    expect_setequal(vapply(result$source_selections, `[[`, character(1), "selected_id"),
      c("arima--local--R1", "ets--local--R1"))
    source <- read_candidate_forecasts(fixture$run_info, fixture$metadata$hts_combos, log, reconciled = FALSE)
    chosen <- source[source$Best_Model == "Yes", ]
    chosen$Run_Type <- fixture$splits$Run_Type[match(chosen$Train_Test_ID, fixture$splits$Train_Test_ID)]
    expected <- reconcile_pre_selection_case(fixture, list(forecasts = chosen))
    delivered <- read_candidate_forecasts(fixture$run_info, fixture$metadata$original_combos, log)
    keys <- c("Combo", "Train_Test_ID", "Date", "Forecast")
    expect_equal(dplyr::arrange(delivered[, keys], Combo, Train_Test_ID, Date),
      dplyr::arrange(expected[, keys], Combo, Train_Test_ID, Date), tolerance = 1e-7)
    resumed <- assess_agent_run(fixture$run_info, log, fixture$metadata$original_combos)
    expect_equal(agent_selection_summary(resumed), agent_selection_summary(result), tolerance = 1e-7)
    delivered$Forecast[delivered$Train_Test_ID == 1] <- 1e8
    write_data(delivered, combo = "Best-Model", run_info = fixture$run_info, output_type = "data",
      folder = "forecasts", suffix = "-reconciled")
    reloaded <- assess_agent_run(fixture$run_info, log, fixture$metadata$original_combos)
    expect_equal(agent_selection_summary(reloaded), agent_selection_summary(resumed))
  }
})

test_that("weekly hierarchy resume retains accuracy and native outputs after daily expansion", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  fixture <- make_hierarchical_selection_artifacts(date_type = "week", shape = "seasonal",
    horizon = 6L, backtest_scenarios = 1L)
  result <- final_models(fixture$run_info, average_models = FALSE, weekly_to_daily = FALSE)
  log <- read_selection_file(fixture$run_info, "logs")
  resumed <- assess_agent_run(fixture$run_info, log, fixture$metadata$original_combos)
  expect_equal(agent_selection_summary(resumed), agent_selection_summary(result), tolerance = 1e-7)
  expect_true(is.na(agent_selection_summary(resumed)$seasonal_fidelity))
  source <- read_candidate_forecasts(fixture$run_info, fixture$metadata$hts_combos, log, reconciled = FALSE)
  source <- source[source$Best_Model == "Yes", ]
  expect_setequal(unique(source$Combo), fixture$metadata$hts_combos)
  expect_equal(nrow(source), length(fixture$metadata$hts_combos) * fixture$horizon * nrow(fixture$splits))
  for (combo in fixture$metadata$hts_combos) {
    expanded <- convert_weekly_to_daily(source[source$Combo == combo, ], "week", TRUE)
    write_data(expanded, combo = combo, run_info = fixture$run_info, output_type = "data",
      folder = "forecasts", suffix = "-single_models")
  }
  delivered <- read_candidate_forecasts(fixture$run_info, fixture$metadata$original_combos, log)
  delivered <- convert_weekly_to_daily(delivered, "week", TRUE)
  write_data(delivered, combo = "Best-Model", run_info = fixture$run_info, output_type = "data",
    folder = "forecasts", suffix = "-reconciled")
  log$weekly_to_daily <- TRUE
  write_data(log, combo = NULL, run_info = fixture$run_info, output_type = "log", folder = "logs", suffix = NULL)
  log <- read_selection_file(fixture$run_info, "logs")
  expanded_resumed <- assess_agent_run(fixture$run_info, log, fixture$metadata$original_combos)
  expect_equal(expanded_resumed$source_selections, resumed$source_selections, tolerance = 1e-7)
  expect_equal(expanded_resumed$selections, resumed$selections, tolerance = 1e-7)
  expect_equal(agent_selection_summary(expanded_resumed), agent_selection_summary(resumed), tolerance = 1e-7)
  native_source <- read_candidate_forecasts(fixture$run_info, fixture$metadata$hts_combos, log, reconciled = FALSE)
  columns <- c("Combo", "Model_ID", "Train_Test_ID", "Date", "Forecast", "Target", "Best_Model")
  expect_equal(dplyr::arrange(native_source[, columns], Combo, Model_ID, Train_Test_ID, Date),
    dplyr::arrange(source[, columns], Combo, Model_ID, Train_Test_ID, Date), tolerance = 1e-7)
  delivered$Forecast[delivered$Train_Test_ID == 1L] <- 1e8
  write_data(delivered, combo = "Best-Model", run_info = fixture$run_info, output_type = "data",
    folder = "forecasts", suffix = "-reconciled")
  altered_future <- assess_agent_run(fixture$run_info, log, fixture$metadata$original_combos)
  expect_equal(agent_selection_summary(altered_future), agent_selection_summary(expanded_resumed))
})

test_that("default reforecast quality failure is attempted once through the graph", {
  calls <- 0L
  local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    read_file = function(...) tibble::tibble(),
    submit_fcst_run = function(...) {
      calls <<- calls + 1L
      rlang::abort("replacement is unusable", class = "finnts_forecast_selection_rejected")
    },
    wait_before_retry = function(...) stop("quality failures must not wait"),
    cancel_parallel = function(...) NULL
  )
  agent <- list(run_id = "run", project_info = list(project_name = "project", path = tempdir()))
  context <- list(args = list(agent_info = agent, new_combos = character(), failed_combos = "series-hash",
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1, seed = 1),
    results = list(), attempts = list())
  expect_error(execute_node(list(fn = "forecast_new_combos", max_retry = 2, retry_mode = "plain"), context, NULL),
    class = "finnts_forecast_selection_rejected")
  expect_identical(calls, 1L)
})

test_that("Agent iteration continues until a complete forecast meets the accuracy goal", {
  state <- new.env(parent = emptyenv())
  state$submissions <- 0L
  state$refreshes <- 0L
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(...) chat
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "prompt",
    reason_inputs = function(...) list(models_to_run = "arima"),
    submit_fcst_run = function(...) { state$submissions <- state$submissions + 1L; list() },
    get_fcst_output = function(...) data.frame(),
    calculate_fcst_metrics = function(...) {
      structure(if (state$submissions > 1) 0.01 else Inf, selection_ok = state$submissions > 1)
    },
    log_best_run = function(...) "logged",
    load_reason_history = function(...) { state$refreshes <- state$refreshes + 1L; list(total_runs = state$submissions) },
    finalize_run = function(...) "finalized"
  )
  result <- fcst_agent_workflow(list(llm = chat, agent_version = 1), combo = "series",
    weighted_mape_goal = 0.03, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1, max_iter = 2, previous_run_results = "No Previous Runs")
  expect_identical(state$submissions, 2L)
  expect_identical(state$refreshes, 1L)
  expect_identical(result$node, "stop")
})

test_that("a default replacement cannot publish success with soft quality concerns", {
  fixture <- make_selection_case(futures = list(only = rep(200, 6)), errors = c(only = 0.01))
  selection <- do.call(select_forecast_candidate, fixture)
  logged <- 0L
  submitted <- 0L
  local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    read_file = function(...) tibble::tibble(),
    submit_fcst_run = function(...) {
      submitted <<- submitted + 1L
      list(forecast_selection = list(selections = list(series = selection)))
    },
    get_fcst_output = function(...) data.frame(),
    log_best_run = function(...) { logged <<- logged + 1L },
    cancel_parallel = function(...) NULL
  )
  agent <- list(run_id = "run", project_info = list(project_name = "project", path = tempdir()))
  expect_error(forecast_new_combos(agent, character(), "series-hash", NULL, FALSE, 1, 1),
    class = "finnts_forecast_selection_rejected")
  expect_identical(submitted, 1L)
  expect_identical(logged, 0L)
})

test_that("an explicitly rejected best-run record does not skip default recovery", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.03))
  selection <- do.call(select_forecast_candidate, fixture)
  submitted <- 0L
  logged <- 0L
  local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    read_file = function(...) data.frame(combo = "series", best_run_name = "reused"),
    submit_fcst_run = function(agent_info, timestamp, ...) {
      expect_true(isTRUE(agent_info$default_reforecast))
      expect_identical(timestamp, "default")
      submitted <<- submitted + 1L
      list(forecast_selection = list(selections = list(series = selection)))
    },
    get_fcst_output = function(...) data.frame(),
    log_best_run = function(...) { logged <<- logged + 1L }
  )
  agent <- list(run_id = "run", quality_rejected_combos = hash_data("series"),
    project_info = list(project_name = "project", path = tempdir()))
  forecast_new_combos(agent, character(), hash_data("series"), NULL, FALSE, 1, 1)
  expect_identical(submitted, 1L)
  expect_identical(logged, 1L)
})

test_that("the update graph never refits because reconciliation rejects its input", {
  recovered <- character()
  reconciliations <- 0L
  final_writes <- 0L
  combos <- c("stored-a", "stored-b")
  hashes <- vapply(combos, hash_data, character(1), USE.NAMES = FALSE)
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    initial_checks = function(...) list(prev_best_runs_tbl = data.frame(combo = combos),
      current_run_combos = hashes, new_combos = character()),
    update_global_models = function(...) list(failed_combos = character(), quality_rejected_combos = character()),
    update_local_models = function(...) list(failed_combos = character(), quality_rejected_combos = character()),
    forecast_new_combos = function(agent_info, new_combos, failed_combos, ...) {
      recovered <<- c(recovered, failed_combos)
      "done"
    },
    save_best_agent_run = function(...) "done",
    analyze_results = function(...) 0,
    reconcile_agent_forecast = function(...) {
      reconciliations <<- reconciliations + 1L
      if (reconciliations == 1L) {
        rlang::abort("invalid reconciliation input", class = "finnts_forecast_selection_rejected", combo = "stored-b")
      }
      "done"
    },
    summarize_hierarchy = function(...) "done",
    save_agent_forecast = function(...) { final_writes <<- final_writes + 1L },
    summarize_models = function(...) "done",
    eda_agent_workflow = function(...) "done",
    wait_before_retry = function(...) stop("quality rejection must not enter retry waiting")
  )
  project <- list(project_name = "project", path = tempdir(), combo_variables = "ID")
  agent <- list(run_id = "run", forecast_approach = "standard_hierarchy", project_info = project)
  expect_error(update_fcst_agent_workflow(agent, project, NULL, FALSE, 1,
    max_iter = 1, allow_iterate_forecast = FALSE), class = "finnts_forecast_selection_rejected")
  expect_identical(recovered, character())
  expect_identical(reconciliations, 1L)
  expect_identical(final_writes, 0L)
})

test_that("reconciliation storage errors never initiate model fitting", {
  submissions <- 0L
  local_mocked_bindings(
    check_agent_info = function(...) NULL,
    get_best_agent_run = function(...) stop("storage unavailable"),
    forecast_new_combos = function(...) { submissions <<- submissions + 1L }
  )
  expect_error(reconcile_agent_forecast(list(), list()), "storage unavailable")
  expect_identical(submissions, 0L)
})

test_that("a rejected default run cannot be fitted again on restart", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.03))
  selection <- do.call(select_forecast_candidate, fixture)
  prepared <- 0L
  local_mocked_bindings(
    list_files = function(...) "input.csv",
    read_file = function(...) data.frame(Combo = "series", Date = fixture$history$Date, Target = 100),
    set_run_info = function(...) list(project_name = "project", run_name = "default", path = tempdir()),
    read_selection_file = function(...) data.frame(default_reforecast_status = "rejected"),
    prep_data = function(...) { prepared <<- prepared + 1L },
    prep_models = function(...) NULL,
    train_models = function(...) NULL,
    final_models = function(...) list(selections = list(series = selection)),
    validate_run_outputs = function(...) NULL
  )
  agent <- list(run_id = "run", default_reforecast = TRUE,
    project_info = list(project_name = "project", path = tempdir(), data_output = "csv"))
  expect_error(submit_fcst_run(agent, list(models_to_run = "meanf"), hash_data("series"), "default"),
    class = "finnts_forecast_selection_rejected")
  expect_identical(prepared, 0L)
})

test_that("an already-used default best run cannot be recovered a second time", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.03))
  selection <- do.call(select_forecast_candidate, fixture)
  submissions <- 0L
  local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    read_file = function(...) data.frame(combo = "series", best_run_name = "default", default_reforecast_status = "accepted"),
    submit_fcst_run = function(...) {
      submissions <<- submissions + 1L
      list(forecast_selection = list(selections = list(series = selection)))
    },
    get_fcst_output = function(...) data.frame(),
    log_best_run = function(...) NULL
  )
  agent <- list(run_id = "run", quality_rejected_combos = hash_data("series"),
    project_info = list(project_name = "project", path = tempdir()))
  expect_error(forecast_new_combos(agent, character(), hash_data("series"), NULL, FALSE, 1, 1),
    class = "finnts_forecast_selection_rejected")
  expect_identical(submissions, 0L)
})

test_that("reconciliation rejects nonfinite input before reading or filling", {
  local_mocked_bindings(read_file = function(...) stop("must reject before storage"))
  for (value in c(Inf, -Inf, NA_real_, NaN)) {
    expect_error(reconcile(data.frame(Combo = "series", Forecast = value), list(), "standard_hierarchy", FALSE),
      class = "finnts_forecast_selection_rejected")
  }
})

test_that("Agent forecast loading does not require an ineligible average artifact", {
  fixture <- make_selection_case(futures = list(only = rep(100, 6)), errors = c(only = 0.03))
  rows <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::mutate(Combo = "series", Combo_ID = Combo, Hyperparameter_ID = 1,
      Best_Model = "Yes", Model_Name = "meanf", Model_Type = "local", Recipe_ID = "R1")
  reads <- character()
  local_mocked_bindings(
    check_agent_info = function(...) NULL,
    load_best_agent_run = function(...) data.frame(combo = "series", model_type = "local",
      best_run_name = "selected", recipes_to_run = "R1", models_to_run = "meanf---snaive", average_models = TRUE),
    list_files = function(...) stop("must not list"),
    read_file = function(run_info, file_list = NULL, path = NULL, ...) {
      if (!is.null(path)) stop("expected exact file_list")
      reads <<- c(reads, file_list)
      if (any(grepl("-average_models", file_list, fixed = TRUE))) stop("no eligible average exists")
      if (any(grepl("-train_test_split", file_list, fixed = TRUE))) fixture$context$train_test_split else rows
    }
  )
  agent <- list(run_id = "run", forecast_approach = "bottoms_up",
    project_info = list(project_name = "project", path = tempdir(), data_output = "csv", combo_variables = "Series"))
  result <- load_agent_forecast(agent)
  expect_identical(unique(result$Model_ID), "only")
  expect_length(reads, 2L)
  expect_true(all(result$Best_Model == "Yes"))
})

test_that("an unpublished quality rejection is not treated as a completed forecast", {
  result <- rejected_agent_selection(c("first", "second"), "incomplete hierarchy")
  expect_identical(agent_selection_summary(result)$status, "rejected")
  expect_equal(nrow(get_fcst_output(list(forecast_selection = result))), 0)
  metric <- calculate_fcst_metrics(list(forecast_selection = result), data.frame())
  expect_false(attr(metric, "selection_ok"))
})