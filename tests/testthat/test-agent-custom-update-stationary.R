test_that("custom update refits keep raw C1 history after a stationary predecessor", {
  local_custom_update_runtime()
  path <- withr::local_tempdir()
  # A noisy upward trend is differenced by the stationarity step, so a refit
  # that wrongly undifferenced raw C1 history would change the custom mean.
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 25)
  values <- 100 + 5 * seq_along(dates) + 3 * sin(seq_along(dates))
  data <- data.frame(Date = dates[1:24], series = "first", value = values[1:24])
  args <- list(project_info = set_project_info(
    project_name = "custom-update-stationary", path = path,
    combo_variables = "series", target_variable = "value", date_type = "month"),
    llm = custom_agent_chat(custom_agent_response("replay_mean---meanf", stationary = TRUE)),
    input_data = data, forecast_horizon = 1, negative_forecast = TRUE,
    run_global_models = FALSE, run_local_models = TRUE, back_test_scenarios = 1,
    custom_models = list(replay_mean = custom_update_model()))
  previous <- load_agent_custom_state(do.call(set_agent_info, args))
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  best <- get_best_agent_run(previous)
  expect_true(isTRUE(as.logical(best$stationary)))
  child <- previous$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("first"))
  child$run_name <- best$best_run_name[[1]]
  expect_false(is.na(read_selection_file(child, "prep_data", "-orig_combo_info")$Diff_Value1[[1]]))

  args$input_data <- rbind(data, data.frame(Date = dates[25], series = "first", value = values[25]))
  args$overwrite <- TRUE
  current <- do.call(set_agent_info, args)
  local_mocked_bindings(
    forecast_new_combos = function(...) stop("Unexpected default fallback"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  forecast <- get_agent_forecast(current)
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  expect_equal(unique(forecast$Forecast[forecast$Model_ID == custom_id & forecast$Train_Test_ID == 1]),
    mean(values), tolerance = 1e-8)
})
