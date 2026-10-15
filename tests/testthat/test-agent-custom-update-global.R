test_that("global custom replay preserves saved component subsets from one shared run", {
  fixture <- custom_update_fixture(TRUE)
  args <- fixture$args
  current <- fixture$current
  local_custom_update_runtime()
  expect_length(unique(get_best_agent_run(fixture$previous)$best_run_name), 1L)
  ids <- paste0("custom-", vapply(args$custom_models, function(model)
    model$definition$version_id, character(1)), "--global--C1")
  forecast <- get_agent_forecast(current)
  expect_equal(unique(forecast$Forecast[forecast$Train_Test_ID == 1]), 101)
  expect_setequal(unique(forecast$Model_ID[forecast$Combo == "first"]), c(ids, paste(sort(ids), collapse = "_")))
  expect_identical(unique(forecast$Model_ID[forecast$Combo == "second"]), ids[[1]])
  expect_length(unique(get_best_agent_run(current)$best_run_name), 1L)
  expect_false(any(grepl("^meanf--", forecast$Model_ID)))
  local_mocked_bindings(
    forecast_new_combos = function(...) stop("Unexpected global fallback"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$input_data <- rbind(args$input_data, data.frame(
    Date = as.Date("2022-02-01"), series = c("first", "second"), value = 101))
  next_run <- do.call(set_agent_info, args)
  update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
  expect_length(unique(get_best_agent_run(next_run)$best_run_name), 1L)
})
