test_that("saved custom updates and new-series defaults preserve model-owned drivers", {
  args <- custom_driver_agent(withr::local_tempdir())
  args$input_data$Driver <- 101:126
  args$input_data$value <- c(2 * (101:124), NA, NA)
  local_custom_update_runtime()
  local_custom_default_grid()
  original_prep <- finnts:::prep_models
  local_mocked_bindings(get_eda_data = function(...) data.frame(done = TRUE),
    get_foundation_model_suffix = function(...) "",
    prep_models = function(..., num_hyperparameters = 20)
      original_prep(..., num_hyperparameters = 1), .package = "finnts")
  previous <- do.call(set_agent_info, args)
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  local_mocked_bindings(summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$overwrite <- TRUE
  added <- args$input_data
  added$id <- "new-series"
  added$value <- added$value * 0.75
  args$input_data <- rbind(args$input_data, added)
  current <- do.call(set_agent_info, args)
  original_submit <- finnts:::submit_fcst_run
  submitted <- list()
  local_mocked_bindings(submit_fcst_run = function(agent_info, inputs, combo, ...) {
    submitted[[combo]] <<- inputs
    original_submit(agent_info, inputs, combo, ...)
  }, .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(submitted[[hash_data("new-series")]]$external_regressors, "NULL")
  forecasts <- get_agent_forecast(current)
  for (combo in c("series1", "new-series")) {
    rows <- forecasts[which(forecasts$Combo == combo & forecasts$Model_Name == "driver_rule" &
      forecasts$Train_Test_ID == 1), ]
    expect_equal(rows$Forecast, (if (combo == "series1") 2 else 1.5) * (125:126))
  }
  expect_no_error(audit_agent_custom_best(load_agent_custom_state(current), get_best_agent_run(current)))
})
