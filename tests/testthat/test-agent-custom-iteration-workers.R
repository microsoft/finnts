test_that("installed custom Agent forecasts dispatch to two independent real workers", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Real Agent worker dispatch is verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  observations <- withr::local_tempdir()
  payload_observations <- withr::local_tempdir()
  args$llm <- custom_agent_chat(custom_agent_response(), observation_path = observations)
  info <- do.call(set_agent_info, args)
  original_parallel <- finnts:::par_start
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    par_start = function(...) {
      parallel_info <- original_parallel(...)
      if (!is.null(parallel_info$cl))
        parallel::clusterCall(parallel_info$cl, function(expected_namespace, destination) {
          stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")), normalizePath(expected_namespace)))
          original <- finnts:::fcst_agent_workflow
          testthat::local_mocked_bindings(fcst_agent_workflow = function(agent_info, ...) {
            saveRDS(list(pid = Sys.getpid(), lean = is.null(agent_info$custom_agent_contract) &&
              is.null(agent_info$custom_agent_update) && is.null(agent_info$custom_enrollment_cache) &&
              is.null(agent_info$selection_cache)), file.path(destination, paste0(Sys.getpid(), ".rds")))
            original(agent_info, ...)
          }, .package = "finnts", .env = globalenv())
          testthat::local_mocked_bindings(load_eda_results = function(...) "Offline EDA", .package = "finnts", .env = globalenv())
          invisible(NULL)
        }, namespace, payload_observations)
      parallel_info
    }, .package = "finnts"
  )
  sequential_args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  sequential_args$llm <- custom_agent_chat(custom_agent_response())
  sequential <- do.call(set_agent_info, sequential_args)
  iterate_forecast(sequential, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expected_forecast <- get_agent_forecast(sequential)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, parallel_processing = "local_machine", num_cores = 2)
  forecast <- get_agent_forecast(info)
  expected <- ifelse(forecast$Combo == "series1", 100, 200)
  expect_equal(forecast$Forecast, expected)
  columns <- c("Combo", "Date", "Model_ID", "Train_Test_ID", "Forecast", "Best_Model")
  expect_equal(dplyr::arrange(forecast[, columns], Combo, Date, Train_Test_ID), dplyr::arrange(
    expected_forecast[, columns],
    Combo, Date, Train_Test_ID
  ))
  expect_identical(unique(forecast$Model_ID), paste0(
    "custom-", args$custom_models$business_mean$definition$version_id,
    "--local--C1"
  ))
  sessions <- lapply(list.files(observations, full.names = TRUE), readRDS)
  expect_length(sessions, 2L)
  expect_length(unique(vapply(sessions, function(session) session$pid, integer(1))), 2L)
  expect_true(all(vapply(sessions, function(session) session$previous_turns == 0L, logical(1))))
  expect_false(Sys.getpid() %in% vapply(sessions, function(session) session$pid, integer(1)))
  expect_false(any(get_summarized_models(info)$section == "error"))
  expect_length(args$llm$turns, 0L)
  payloads <- lapply(list.files(payload_observations, full.names = TRUE), readRDS)
  expect_length(payloads, 2L)
  expect_true(all(vapply(payloads, function(payload) payload$lean, logical(1))))
})

test_that("installed custom Agent submission runs custom backtest folds with inner_parallel", {
  skip_on_cran()
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Inner parallel workers are verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  clusters <- 0L
  original_parallel <- finnts:::par_start
  testthat::local_mocked_bindings(par_start = function(...) {
    parallel_info <- original_parallel(...)
    if (!is.null(parallel_info$cl)) clusters <<- clusters + 1L
    parallel_info
  }, .package = "finnts")
  # Submit one mixed custom/built-in child run and return its forecasts after
  # checking that the parent contract identity and child audit are unchanged.
  submit <- function(inner_parallel, num_cores) {
    args <- custom_agent_inputs(withr::local_tempdir())
    info <- load_agent_custom_state(do.call(set_agent_info, args))
    proposal <- validate_agent_custom_proposal(
      jsonlite::fromJSON(custom_agent_response("business_mean---meanf")), info$custom_agent_contract, "combo"
    )
    run <- submit_fcst_run(info, proposal, hash_data("series1"), "fixed",
      inner_parallel = inner_parallel, num_cores = num_cores)
    expect_identical(read_selection_file(run, "logs")$custom_agent_contract_id, info$custom_agent_contract_id)
    expect_no_error(audit_agent_custom_child(info, run, hash_data("series1")))
    list(forecast = get_forecast_data(run), model_id = paste0("custom-", args$custom_models$business_mean$definition$version_id, "--local--C1"))
  }
  sequential <- submit(FALSE, 1)
  expect_identical(clusters, 0L)
  parallel <- submit(TRUE, 2)
  expect_gt(clusters, 0L)
  columns <- c("Combo", "Date", "Model_ID", "Train_Test_ID", "Forecast", "Best_Model")
  arrange <- function(forecast) dplyr::arrange(forecast[, columns], Model_ID, Train_Test_ID, Date)
  expect_equal(arrange(parallel$forecast), arrange(sequential$forecast))
  custom <- parallel$forecast[parallel$forecast$Model_ID == parallel$model_id, ]
  expect_gt(nrow(custom), 0L)
  expect_true(all(custom$Forecast == 100))
})