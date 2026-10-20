test_that("public custom iteration publishes summaries and restarts without new fitting", {
  args <- custom_agent_inputs(withr::local_tempdir())
  tracker <- new.env(parent = emptyenv())
  args$llm <- custom_agent_chat(custom_agent_response(), tracker)
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  original_workflow <- fcst_agent_workflow
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    fcst_agent_workflow = function(agent_info, ...) {
      expect_null(agent_info$custom_agent_contract)
      expect_null(agent_info$custom_agent_update)
      original_workflow(agent_info, ...)
    },
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"), .package = "finnts"
  )
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  forecast <- get_agent_forecast(info)
  summaries <- get_summarized_models(info)
  expect_true(nrow(forecast) > 0)
  expect_equal(unique(forecast$Forecast), 100)
  expect_true(any(summaries$name == "version_id"))
  expect_false(any(summaries$section == "error"))
  expect_true(any(grepl("{literal_contract_text}", summaries$value, fixed = TRUE)))
  expect_equal(tracker$calls, 1)
  expect_length(args$llm$turns, 0L)
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  testthat::local_mocked_bindings(train_models = function(...) stop("Unexpected refit"), .package = "finnts")
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expect_equal(tracker$calls, 1)
  expect_equal(get_agent_forecast(info), forecast)
  expect_identical(tools::md5sum(files), hashes)
  corrupted <- get_best_agent_run(info)
  corrupted$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(corrupted, "series1", agent_custom_parent_info(info), "log", "logs", "-agent_best_run")
  expect_error(iterate_forecast(info, max_iter = 1, num_cores = 1), "winner parent identity")
})

test_that("partial custom local restart does not repeat the global phase", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  info <- do.call(set_agent_info, args)
  state <- new.env(parent = emptyenv())
  state$best <- data.frame(
    combo = c("series1", "series2"), model_type = c("local", "global"), weighted_mape = c(0.2, 0.2),
    run_complete = c(TRUE, FALSE), max_iterations = c(2, 0)
  )
  state$calls <- character()
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_best_agent_run = function(...) state$best,
    load_run_results = function(...) "No Previous Runs", fcst_agent_workflow = function(agent_info, combo, ...) {
      if (is.null(combo))
        stop("Unexpected repeated global phase")
      state$calls <- c(state$calls, combo)
      state$best$model_type[[2]] <- "local"
      state$best$run_complete[[2]] <- TRUE
      state$best$max_iterations[[2]] <- 2
      invisible(NULL)
    }, save_best_agent_run = function(...) invisible(NULL), save_agent_forecast = function(...) invisible(NULL), summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )
  iterate_forecast(info, max_iter = 2, weighted_mape_goal = 0.01, num_cores = 1)
  expect_identical(state$calls, hash_data("series2"))
  iterate_forecast(info, max_iter = 2, weighted_mape_goal = 0.01, num_cores = 1)
  expect_identical(state$calls, hash_data("series2"))
})
