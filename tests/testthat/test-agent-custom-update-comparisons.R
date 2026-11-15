test_that("custom updates always refit enrolled custom models beside a built-in winner", {
  fixture <- custom_update_fixture(builtin_winner = TRUE)
  args <- fixture$args
  current <- fixture$current
  forecast <- fixture$forecast
  local_custom_update_runtime()
  winner <- "meanf--local--R1"
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  expect_identical(read_agent_custom_update(current, TRUE)$winners$first$components, winner)
  expect_setequal(unique(forecast$Model_ID), c(winner, custom_id))
  expect_identical(unique(forecast$Model_ID[forecast$Best_Model == "Yes"]), winner)
  expect_true(all(forecast$Best_Model[forecast$Model_ID == custom_id] == "No"))
  expect_equal(unique(forecast$Forecast[forecast$Model_ID == custom_id & forecast$Train_Test_ID == 1]), 101)
  # The comparison is never averaged into the winner or its model-pool spread;
  # one winning model has no spread, whereas a refit comparison would add one.
  expect_false(any(forecast$Recipe_ID == "simple_average"))
  best <- get_best_agent_run(current)
  expect_true(is.na(best$model_std_wmape))
  expect_equal(as.numeric(best$model_avg_wmape), as.numeric(best$weighted_mape), tolerance = 1e-4)
  expect_setequal(strsplit(best$models_to_run, "---", fixed = TRUE)[[1]], c("meanf", "replay_mean"))

  local({
    local_mocked_bindings(
      fit_models = function(...) stop("Unexpected update refit"),
      summarize_models = function(...) invisible(NULL), .package = "finnts")
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_identical(tools::md5sum(names(before)), before)
  })

  local_mocked_bindings(
    forecast_new_combos = function(...) stop("Unexpected default fallback"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$input_data <- rbind(args$input_data, data.frame(Date = as.Date("2022-02-01"), series = "first", value = 101))
  next_run <- do.call(set_agent_info, args)
  update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
  chained <- get_agent_forecast(next_run)
  expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
  expect_setequal(unique(chained$Model_ID), c(winner, custom_id))
  expect_identical(unique(chained$Model_ID[chained$Best_Model == "Yes"]), winner)
  expect_equal(unique(chained$Forecast[chained$Model_ID == custom_id & chained$Train_Test_ID == 1]), 101)
})
