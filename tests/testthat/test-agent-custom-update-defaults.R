test_that("a public update replays saved custom winners and genuinely fits new-series defaults", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  local_custom_default_grid()
  args <- fixture$args
  added <- args$input_data
  added$series <- "new-series"
  added$value <- 80
  args$input_data <- rbind(args$input_data, added)
  current <- do.call(set_agent_info, args)
  original_submit <- finnts:::submit_fcst_run
  original_prep <- finnts:::prep_models
  submissions <- list()
  # Retain every default engine and genuine fit/prediction, with one tuning
  # candidate per engine. Observe actual dispatch, not fabricated forecasts.
  local_mocked_bindings(
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"),
    agent_custom_foundations = function(...) "",
    summarize_models = function(...) invisible(NULL),
    prep_models = function(..., num_hyperparameters = 20) {
      original_prep(..., num_hyperparameters = 1)
    },
    submit_fcst_run = function(agent_info, inputs, combo, ...) {
      expect_null(agent_info$custom_agent_contract)
      expect_null(agent_info$custom_agent_update)
      submissions[[combo]] <<- inputs
      original_submit(agent_info, inputs, combo, ...)
    }, .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(names(submissions), hash_data("new-series"))
  expect_identical(submissions[[1]]$models_to_run,
    c("replay_mean", "arima", "ets", "naive", "snaive", "tbats", "stlm-arima", "xgboost", "glmnet"))
  expect_identical(submissions[[1]]$recipes_to_run, c("R1", "C1"))
  actual <- get_agent_forecast(current)
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  expect_equal(unique(actual$Forecast[actual$Combo == "first" &
    actual$Model_ID == custom_id & actual$Train_Test_ID == 1]), 101)
  expect_equal(unique(actual$Forecast[actual$Combo == "new-series" &
    actual$Model_ID == custom_id & actual$Train_Test_ID == 1]), 80)
  expect_setequal(get_best_agent_run(current)$combo, c("first", "new-series"))
  expect_no_error(audit_agent_custom_best(load_agent_custom_state(current), get_best_agent_run(current)))
  local_mocked_bindings(train_models = function(...) stop("Unexpected completed-default refit"),
    fit_models = function(...) stop("Unexpected completed-replay refit"), .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_equal(get_agent_forecast(current), actual)
})
