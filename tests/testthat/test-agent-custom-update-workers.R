test_that("installed mixed custom updates preserve choices on two real workers", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Real custom update dispatch is verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  path <- withr::local_tempdir()
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2),
    series = rep(c("first", "second"), each = 24), value = 100)
  args <- list(project_info = set_project_info(
    project_name = "worker-custom-update", path = path, combo_variables = "series",
    target_variable = "value", date_type = "month"), llm = custom_update_chat("replay_mean---meanf"),
    input_data = data, forecast_horizon = 1, negative_forecast = TRUE,
    run_global_models = FALSE, back_test_scenarios = 1,
    custom_models = list(replay_mean = custom_update_model()))
  local_custom_update_runtime()
  previous <- load_agent_custom_state(do.call(set_agent_info, args))
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  best <- get_best_agent_run(previous)
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  average_id <- paste(sort(c(custom_id, "meanf--local--R1")), collapse = "_")
  for (combo in c("first", "second")) {
    child <- previous$project_info
    child$project_name <- paste0(child$project_name, "_", hash_data(combo))
    child$run_name <- best$best_run_name[best$combo == combo]
    rows <- read_selection_file(child, "forecasts", "-single_models", combo)
    rows$Best_Model <- if (combo == "first") "No" else
      ifelse(rows$Model_ID == "meanf--local--R1", "Yes", "No")
    write_data(rows, combo, child, "data", "forecasts", "-single_models")
    average <- read_selection_file(child, "forecasts", "-average_models", combo)
    average$Best_Model <- if (combo == "first") "Yes" else "No"
    write_data(average, combo, child, "data", "forecasts", "-average_models")
  }
  sequential_path <- file.path(withr::local_tempdir(), "sequential")
  fs::dir_copy(path, sequential_path)
  args$input_data <- rbind(data, data.frame(Date = as.Date("2022-01-01"),
    series = c("first", "second"), value = 125))
  args$overwrite <- TRUE
  args$forecast_horizon <- 2L
  sequential_args <- args
  sequential_args$project_info$path <- sequential_path
  sequential <- do.call(set_agent_info, sequential_args)
  local({
    original_fit <- finnts:::fit_models
    retuned <- FALSE
    local_mocked_bindings(fit_models = function(run_info, ..., retune_hyperparameters) {
      result <- original_fit(run_info, ..., retune_hyperparameters = retune_hyperparameters)
      if (retune_hyperparameters && !is.null(run_info$custom_fixed_results)) {
        retuned <<- TRUE
        expect_identical(result[result$Model_Name == "replay_mean", ], run_info$custom_fixed_results)
      }
      result
    }, .package = "finnts")
    update_forecast(sequential, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_true(retuned)
  })
  expected <- get_agent_forecast(sequential)
  current <- do.call(set_agent_info, args)
  observations <- withr::local_tempdir()
  original_parallel <- finnts:::par_start
  local_mocked_bindings(par_start = function(...) {
    parallel_info <- original_parallel(...)
    if (!is.null(parallel_info$cl))
      parallel::clusterCall(parallel_info$cl, function(expected_namespace, destination) {
        stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")), normalizePath(expected_namespace)))
        original <- finnts:::update_forecast_combo
        testthat::local_mocked_bindings(update_forecast_combo = function(agent_info, ...) {
          saveRDS(list(pid = Sys.getpid(), no_chat = is.null(agent_info$llm),
            no_models = !any(vapply(agent_info, is.data.frame, logical(1))) &&
              is.null(agent_info$custom_agent_contract) && is.null(agent_info$custom_agent_update)),
            file.path(destination, paste0("update-", Sys.getpid(), ".rds")))
          original(agent_info, ...)
        }, .package = "finnts", .env = globalenv())
        invisible(NULL)
      }, namespace, observations)
    parallel_info
  }, .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, parallel_processing = "local_machine", num_cores = 2)
  actual <- get_agent_forecast(current)
  columns <- c("Combo", "Date", "Model_ID", "Train_Test_ID", "Forecast", "Best_Model")
  expect_equal(dplyr::arrange(actual[, columns], Combo, Date, Model_ID, Train_Test_ID),
    dplyr::arrange(expected[, columns], Combo, Date, Model_ID, Train_Test_ID))
  future <- actual[actual$Train_Test_ID == 1, ]
  expect_true(all(table(future$Combo[future$Best_Model == "Yes"]) == 2L))
  builtin_mean <- mean(c(rep(100, 11), 125))
  expect_equal(unique(future$Forecast[future$Model_ID == custom_id]), 101)
  expect_equal(unique(future$Forecast[future$Model_ID == "meanf--local--R1"]), builtin_mean)
  expect_equal(unique(future$Forecast[future$Model_ID == average_id]), mean(c(101, builtin_mean)))
  expect_identical(unique(actual$Model_ID[actual$Combo == "first" & actual$Best_Model == "Yes"]), average_id)
  expect_identical(unique(actual$Model_ID[actual$Combo == "second"]), "meanf--local--R1")
  workers <- lapply(list.files(observations, full.names = TRUE), readRDS)
  expect_length(workers, 2L)
  expect_length(unique(vapply(workers, function(worker) worker$pid, integer(1))), 2L)
  expect_false(Sys.getpid() %in% vapply(workers, function(worker) worker$pid, integer(1)))
  expect_true(all(vapply(workers, function(worker) worker$no_chat && worker$no_models, logical(1))))
  expect_length(args$llm$turns, 0L)
  expect_false(any(get_summarized_models(current)$section == "error"))
  local({
    prepared <- prepare_agent_custom_update(current, 123)
    metadata <- load_update_runs(current)
    local_mocked_bindings(load_update_runs = function(...) metadata[metadata$combo == "second", , drop = FALSE], .package = "finnts")
    routed <- initial_agent_custom_update(prepared)
    expect_identical(routed$prev_best_runs_tbl$combo, "first")
  })
})
