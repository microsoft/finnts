test_that("Agent first and later submissions keep custom drivers out of built-in settings", {
  args <- custom_driver_agent(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  contract <- info$custom_agent_contract
  for (xregs in c("NULL", "Builtin")) {
    inputs <- validate_agent_custom_proposal(jsonlite::fromJSON(
      custom_agent_response("driver_rule---meanf", external_regressors = xregs)),
      contract, hash_data("series1"))
    expect_identical(inputs$external_regressors, xregs)
    run <- submit_fcst_run(info, inputs, hash_data("series1"), paste0("drivers-", xregs), num_cores = 1)
    forecast <- get_forecast_data(run)
    custom <- forecast[which(forecast$Model_Name == "driver_rule" & forecast$Train_Test_ID == 1), ]
    expect_equal(custom$Forecast, 2 * (35:36))
    r1 <- read_exact_artifact(run, local_artifact_path(run, "prep_data", "-R1",
      hash_data("series1"), extension = "csv"))
    expect_false("Driver" %in% names(r1))
    expect_identical("Builtin" %in% names(r1), xregs == "Builtin")
    expect_no_error(audit_agent_custom_child(info, run, hash_data("series1")))
  }
  before <- contract$envelopes$driver_rule
  expect_identical(before, args$custom_models$driver_rule)
  args$input_data$Driver[[1]] <- 999
  expect_error(do.call(set_agent_info, args), "(Custom|C1).*changed|custom.*source")
  args$input_data$Driver <- NULL
  expect_error(do.call(set_agent_info, args), "driver_rule.*Driver")
})

test_that("public first iteration supplies custom-only drivers without enabling built-in xregs", {
  args <- custom_driver_agent(withr::local_tempdir())
  args$custom_models$unit_growth <- historical_driver_model()
  args$input_data$Units <- c(rep(110, 24), NA, NA)
  args$input_data$Price <- c(rep(10, 24), 12, 13)
  args$llm <- custom_agent_chat(custom_agent_response("driver_rule---unit_growth"))
  args$external_regressors <- NULL
  local_custom_update_runtime()
  info <- do.call(set_agent_info, args)
  local_mocked_bindings(get_eda_data = function(...) data.frame(done = TRUE),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  forecasts <- get_agent_forecast(info)
  custom <- forecasts[which(forecasts$Model_Name == "driver_rule" & forecasts$Train_Test_ID == 1), ]
  expect_equal(custom$Forecast, 2 * (35:36))
  historical <- forecasts[which(forecasts$Model_Name == "unit_growth" & forecasts$Train_Test_ID == 1), ]
  expect_equal(historical$Forecast, c(1452, 1730.3), tolerance = 1e-8)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expect_identical(get_agent_forecast(info), forecasts)
  expect_identical(load_agent_custom_state(info)$custom_agent_contract$envelopes$unit_growth,
    args$custom_models$unit_growth)
  expect_null(info$external_regressors)
})

test_that("failed-series recovery dispatches C1 without enabling built-in xregs", {
  args <- custom_driver_agent(withr::local_tempdir())
  args$input_data$Driver <- 101:126
  args$input_data$value <- c(2 * (101:124), NA, NA)
  local_custom_update_runtime()
  info <- do.call(set_agent_info, args)
  combo <- hash_data("series1")
  failed <- check_update_failures(info, data.frame(combo = "series1"), combo,
    character(), combo)
  expect_identical(failed, combo)
  submitted <- NULL
  # Stop at the real dispatch boundary: genuine default fitting is covered by
  # the public update regression, and no successful forecast is fabricated here.
  local_mocked_bindings(get_foundation_model_suffix = function(...) stop("Unselected foundation probe"),
    agent_custom_foundations = function(...) "",
    submit_fcst_run = function(agent_info, inputs, combo, ...) {
      submitted <<- inputs
      restored <- load_agent_custom_state(agent_info)
      expect_identical(restored$custom_agent_contract$envelopes$driver_rule,
        args$custom_models$driver_rule)
      stop("Observed failed-series dispatch")
    }, .package = "finnts")
  expect_error(forecast_new_combos(info, new_combos = character(), failed_combos = failed,
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1, seed = 123),
    "Observed failed-series dispatch")
  expect_identical(submitted$external_regressors, "NULL")
  expect_setequal(submitted$models_to_run,
    c("driver_rule", "arima", "ets", "naive", "snaive", "tbats", "stlm-arima", "xgboost", "glmnet"))
  expect_setequal(submitted$recipes_to_run, c("R1", "C1"))
})
