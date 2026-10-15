test_that("global-only custom iteration preserves one pinned global run and history", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$run_local_models <- FALSE
  tracker <- new.env(parent = emptyenv())
  args$llm <- custom_agent_chat(custom_agent_response(), tracker)
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"),
    .package = "finnts"
  )
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  forecast <- get_agent_forecast(info)
  best <- get_best_agent_run(info)
  expect_equal(unique(forecast$Forecast), 150)
  expect_identical(unique(best$model_type), "global")
  expect_length(unique(best$best_run_name), 1L)
  expect_identical(unique(best$custom_agent_contract_id), info$custom_agent_contract_id)
  history <- load_run_results(info)
  expect_identical(history$models_to_run, "business_mean")
  expect_identical(history$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_identical(history$model_avg_wmape, history$model_median_wmape)
  expect_equal(history$model_std_wmape, 0)
  summaries <- get_summarized_models(info)
  expect_gt(nrow(summaries), 0L)
  expect_false(any(summaries$section == "error"))
  testthat::local_mocked_bindings(
    train_models = function(...) stop("Unexpected global refit"),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  expect_equal(tracker$calls, 1)
  parent <- info$project_info
  parent$project_name <- paste0(parent$project_name, "_", hash_data("all"))
  parent$run_name <- best$best_run_name[[1]]
  log <- read_selection_file(parent, "logs")
  log$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(log, NULL, parent, "log", "logs")
  expect_error(load_run_results(info), "history.*identity")
  expect_error(iterate_forecast(info, max_iter = 1, num_cores = 1), "parent identity")
})
