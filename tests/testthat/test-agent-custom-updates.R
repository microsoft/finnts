# Synthetic approved mean definition for offline replay tests. Source/evidence
# are fixed and no provider or authoring approval is inferred by this fixture.
# The current runtime owns chronological complete-horizon execution.
custom_update_model <- function(name = "replay_mean") {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the historical mean.", "Use the analysis mean; {literal_update_text} is descriptive text.",
    c("local", "global"), c(fit = "function(data, context, parameters) mean(data$Target)", predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=rep(object,nrow(new_data)))"),
    list(predictors = character(), recipes = "R1", target_scale = "original", date_types = "month", forecast_horizon = c(
      1,
      2
    ), missing_data = "error")
  )
  structure(list(schema_version = 1L, definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent synthetic mean fixture")
  ), approval = list(
    version_id = definition$version_id,
    intent_confirmed = TRUE, allow_code = TRUE
  )), class = "finnts_custom_model")
}

# Offline Chat template with independent clone histories. It supplies one fixed
# complete initial proposal; update replay must not ask it to select new logic.
custom_update_chat <- function(models = "replay_mean") {
  session <- new.env(parent = emptyenv())
  class(session) <- "Chat"
  session$turns <- list()
  session$clone <- function(deep = FALSE) custom_update_chat(models)
  session$set_system_prompt <- function(prompt) session
  session$set_turns <- function(turns) {
    session$turns <- turns
    session
  }
  session$chat <- function(prompt, ...) {
    session$turns <- c(session$turns, list(prompt))
    jsonlite::toJSON(
      list(
        models_to_run = models, external_regressors = "NULL", clean_missing_values = FALSE, clean_outliers = FALSE,
        forecast_approach = "bottoms_up", stationary = FALSE, feature_selection = FALSE, multistep_horizon = FALSE, seasonal_period = "NULL",
        recipes_to_run = "R1", lag_periods = "NULL", rolling_window_periods = "NULL", reasoning = "Use the approved fixed mean."
      ),
      auto_unbox = TRUE
    )
  }
  session
}

# Write a small completed offline EDA artifact without changing runtime EDA code.
# The caller owns its temporary project; no model source or real data is sent out.
custom_update_eda <- function(agent_info, ...) {
  write_data(
    data.frame(Combo = "All", Analysis_Type = "Offline", Metric = "fixture", Value = "complete"), NULL, agent_custom_parent_info(agent_info),
    "data", "final_output", "-eda"
  )
  invisible(NULL)
}

test_that("public custom update replays the exact approved version on new data", {
  args <- list(
    project_info = set_project_info(
      project_name = "custom-update", path = withr::local_tempdir(), combo_variables = "series",
      target_variable = "value", date_type = "month"
    ), llm = custom_update_chat(), input_data = data.frame(Date = seq(as.Date("2020-01-01"),
      by = "month", length.out = 24
    ), series = "first", value = 100), forecast_horizon = 1, negative_forecast = TRUE, run_global_models = FALSE,
    back_test_scenarios = 1, custom_models = list(replay_mean = custom_update_model()), models_to_run = "replay_mean"
  )
  local_mocked_bindings(eda_agent_workflow = custom_update_eda, load_eda_results = function(...) "Offline EDA", .package = "finnts")
  previous <- do.call(set_agent_info, args)
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expect_equal(unique(get_agent_forecast(previous)$Forecast), 100)
  args$input_data <- rbind(args$input_data, data.frame(Date = as.Date("2022-01-01"), series = "first", value = 125))
  args$overwrite <- TRUE
  current <- do.call(set_agent_info, args)
  local_mocked_bindings(
    create_custom_model = function(...) stop("Unexpected source regeneration"), forecast_new_combos = function(...) stop("Unexpected default fallback"),
    .package = "finnts"
  )
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  forecast <- get_agent_forecast(current)
  expect_equal(unique(forecast$Forecast[forecast$Train_Test_ID == 1]), 101)
  expect_identical(unique(forecast$Model_ID), paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--R1"))
  expect_identical(read_agent_custom_record(current, TRUE)$envelopes, previous$custom_agent_contract$envelopes)
  expect_length(args$llm$turns, 0L)
  local({
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    local_mocked_bindings(fit_models = function(...) stop("Unexpected update refit"), .package = "finnts")
    update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_equal(get_agent_forecast(current), forecast)
    expect_identical(tools::md5sum(names(before)), before)
  })
  record <- read_agent_custom_update(current, TRUE)
  expect_identical(record$predecessor$run_id, previous$run_id)
  expect_identical(record$winners$first$components, unique(forecast$Model_ID))
  local({
    prepared <- prepare_agent_custom_update(current, 123)
    best <- get_best_agent_run(current)
    source <- dplyr::bind_rows(lapply(record$winners, function(row) row[setdiff(names(row), c("components", "selected_id"))]))
    original_read <- finnts:::read_selection_file
    calls <- 0L
    local_mocked_bindings(load_update_runs = function(...) tibble::tibble(), read_selection_file = function(run_info,
                                                                                                            folder, suffix = NULL, combo = NULL, optional = FALSE, ...) {
      if (identical(suffix, "-agent_best_run") && identical(run_info$run_name, current$run_id) && optional && calls ==
        0L) {
        calls <<- calls + 1L
        return(tibble::tibble())
      }
      original_read(run_info, folder, suffix, combo, optional, ...)
    }, fit_models = function(...) stop("Unexpected interrupted-tail refit"), .package = "finnts")
    artifacts <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
    artifacts <- artifacts[grepl("/(models|forecasts)/", gsub("\\\\", "/", artifacts))]
    before <- tools::md5sum(artifacts)
    update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_equal(calls, 1L)
    expect_identical(tools::md5sum(artifacts), before)
    expect_equal(get_best_agent_run(current), best)
    expect_error(
      check_update_failures(prepared, source, hash_data("first"), character(), character(), local_quality_rejected = hash_data("first")),
      "no fallback"
    )
    local_mocked_bindings(update_forecast_combo = function(...) stop("Deliberate custom source failure"), .package = "finnts")
    expect_error(update_local_models(prepared, source, NULL, FALSE, 1, 123), "Deliberate custom source failure")
  })
  expect_error(update_forecast(current), "allow_iterate_forecast")
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE, inner_parallel = TRUE), "inner_parallel")
  local({
    changed <- current
    changed$hist_start_date <- as.Date("2020-02-01")
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(changed, allow_iterate_forecast = FALSE), "provenance|inputs changed")
    expect_identical(tools::md5sum(names(before)), before)
  })
  local({
    next_args <- args
    next_args$input_data <- rbind(next_args$input_data, data.frame(Date = as.Date("2022-02-01"), series = "first", value = 101))
    next_run <- do.call(set_agent_info, next_args)
    update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
    expect_equal(unique(get_agent_forecast(next_run)$Forecast[get_agent_forecast(next_run)$Train_Test_ID == 1]), 101)
  })
  local({
    input_path <- local_artifact_path(agent_custom_parent_info(current), "input_data", combo = hash_data("first"))
    original <- read_file(current$project_info, file_list = input_path)
    altered <- original
    altered$Target[[1]] <- 101
    write_data(altered, "first", agent_custom_parent_info(current), "data", "input_data")
    on.exit(write_data(original, "first", agent_custom_parent_info(current), "data", "input_data"))
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "inputs changed")
    expect_identical(tools::md5sum(names(before)), before)
  })
  local({
    best <- get_best_agent_run(current)
    child <- current$project_info
    child$project_name <- paste0(child$project_name, "_", hash_data("first"))
    child$run_name <- best$best_run_name[[1]]
    fit_path <- local_artifact_path(child, "models", "-single_models", hash_data("first"), "rds")
    original <- readRDS(fit_path)
    saveRDS(NULL, fit_path)
    on.exit(saveRDS(original, fit_path))
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "damaged|fitted outputs")
    expect_identical(tools::md5sum(names(before)), before)
  })
  local({
    best <- get_best_agent_run(current)
    child <- current$project_info
    child$project_name <- paste0(child$project_name, "_", hash_data("first"))
    child$run_name <- best$best_run_name[[1]]
    fit_path <- local_artifact_path(child, "models", "-single_models", hash_data("first"), "rds")
    models <- readRDS(fit_path)
    altered <- models
    spec <- workflows::extract_spec_parsnip(altered$Model_Fit[[1]])
    spec$eng_args$allow_code <- rlang::new_quosure(FALSE, emptyenv())
    altered$Model_Fit[[1]]$fit$actions$model$spec <- spec
    expect_error(
      validate_agent_custom_update_workflows(altered, custom_run_load(child, read_selection_file(child, "logs"))),
      "specification changed"
    )
  })
  local({
    other <- args
    other$custom_models$replay_mean$validation$checks$arithmetic <- "Different approved evidence"
    changed <- do.call(set_agent_info, other)
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(changed, allow_iterate_forecast = FALSE), "same enrollment")
    expect_identical(tools::md5sum(names(before)), before)
  })
  for (change in c("horizon", "series", "mode", "source")) local({
    other <- args
    if (change == "horizon")
      other$forecast_horizon <- 2
    if (change == "series")
      other$input_data$series <- "renamed"
    if (change == "mode")
      other$run_global_models <- TRUE
    if (change == "source") {
      other$custom_models <- list(other_mean = custom_update_model("other_mean"))
      other$models_to_run <- "other_mean"
    }
    changed <- do.call(set_agent_info, other)
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(changed, allow_iterate_forecast = FALSE), "same enrollment|series membership")
    expect_identical(tools::md5sum(names(before)), before)
  })
  path <- local_artifact_path(agent_custom_parent_info(current), "logs", "-agent-custom-update", extension = "rds")
  saveRDS(NULL, path)
  before <- tools::md5sum(path)
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "provenance identity")
  expect_identical(tools::md5sum(path), before)
})

test_that("global custom replay preserves saved component subsets from one shared run", {
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2), series = rep(c(
    "first",
    "second"
  ), each = 24), value = 100)
  args <- list(
    project_info = set_project_info(
      project_name = "global-custom-update", path = withr::local_tempdir(), combo_variables = "series",
      target_variable = "value", date_type = "month"
    ), llm = custom_update_chat("replay_mean---other_mean"), input_data = data,
    forecast_horizon = 1, negative_forecast = TRUE, run_global_models = TRUE, run_local_models = FALSE, back_test_scenarios = 1,
    custom_models = list(replay_mean = custom_update_model(), other_mean = custom_update_model("other_mean")), models_to_run = c(
      "replay_mean",
      "other_mean", "meanf"
    )
  )
  local_mocked_bindings(eda_agent_workflow = custom_update_eda, load_eda_results = function(...) "Offline EDA", .package = "finnts")
  previous <- do.call(set_agent_info, args)
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  best <- get_best_agent_run(previous)
  expect_length(unique(best$best_run_name), 1L)
  child <- previous$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("all"))
  child$run_name <- best$best_run_name[[1]]
  ids <- paste0("custom-", vapply(args$custom_models, function(model) model$definition$version_id, character(1)), "--global--R1")
  for (combo in c("first", "second")) {
    rows <- read_selection_file(child, "forecasts", "-global_models", combo)
    rows$Best_Model <- if (combo == "first")
      "No"
    else ifelse(rows$Model_ID == ids[[1]], "Yes", "No")
    write_data(rows, combo, child, "data", "forecasts", "-global_models")
    average <- rows[rows$Model_ID == ids[[1]], , drop = FALSE]
    average$Model_ID <- paste(sort(ids), collapse = "_")
    average$Model_Name <- NA_character_
    average$Model_Type <- "local"
    average$Recipe_ID <- "simple_average"
    average$Best_Model <- if (combo == "first")
      "Yes"
    else "No"
    write_data(average, combo, child, "data", "forecasts", "-average_models")
  }
  args$input_data <- rbind(data, data.frame(Date = as.Date("2022-01-01"), series = c("first", "second"), value = 125))
  args$overwrite <- TRUE
  current <- do.call(set_agent_info, args)
  local_mocked_bindings(forecast_new_combos = function(...) stop("Unexpected global fallback"), .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  forecast <- get_agent_forecast(current)
  expect_equal(unique(forecast$Forecast[forecast$Train_Test_ID == 1]), 101)
  expect_setequal(unique(forecast$Model_ID[forecast$Combo == "first"]), c(ids, paste(sort(ids), collapse = "_")))
  expect_identical(unique(forecast$Model_ID[forecast$Combo == "second"]), ids[[1]])
  expect_length(unique(get_best_agent_run(current)$best_run_name), 1L)
  expect_false(any(grepl("^meanf--", forecast$Model_ID)))
  local({
    prepared <- prepare_agent_custom_update(current, 123)
    best <- get_best_agent_run(current)
    original_load <- finnts:::load_update_runs
    local_mocked_bindings(
      load_update_runs = function(...) original_load(...)[1, , drop = FALSE], fit_models = function(...) stop("Unexpected shared refit"),
      .package = "finnts"
    )
    routed <- initial_agent_custom_update(prepared)
    expect_setequal(routed$prev_best_runs_tbl$combo, c("first", "second"))
    update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_equal(get_agent_forecast(current), forecast)
    wrong <- best
    wrong$combo[[1]] <- "foreign-series"
    expect_error(audit_agent_custom_best(prepared, wrong), "winner choice")
  })
  next_args <- args
  next_args$input_data <- rbind(args$input_data, data.frame(
    Date = as.Date("2022-02-01"), series = c("first", "second"),
    value = 101
  ))
  next_run <- do.call(set_agent_info, next_args)
  update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
  expect_length(unique(get_best_agent_run(next_run)$best_run_name), 1L)
})

test_that("installed mixed custom updates preserve choices on two real workers", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Real custom update dispatch is verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  path <- withr::local_tempdir()
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2), series = rep(c(
    "first",
    "second"
  ), each = 24), value = 100)
  args <- list(
    project_info = set_project_info(
      project_name = "worker-custom-update", path = path, combo_variables = "series",
      target_variable = "value", date_type = "month"
    ), llm = custom_update_chat("replay_mean---meanf"), input_data = data,
    forecast_horizon = 1, negative_forecast = TRUE, run_global_models = FALSE, back_test_scenarios = 1, custom_models = list(replay_mean = custom_update_model()),
    models_to_run = c("replay_mean", "meanf")
  )
  local_mocked_bindings(eda_agent_workflow = custom_update_eda, load_eda_results = function(...) "Offline EDA", .package = "finnts")
  previous <- do.call(set_agent_info, args)
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  best <- get_best_agent_run(previous)
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--R1")
  average_id <- paste(sort(c(custom_id, "meanf--local--R1")), collapse = "_")
  for (combo in c("first", "second")) {
    child <- previous$project_info
    child$project_name <- paste0(child$project_name, "_", hash_data(combo))
    child$run_name <- best$best_run_name[best$combo == combo]
    rows <- read_selection_file(child, "forecasts", "-single_models", combo)
    rows$Best_Model <- if (combo == "first")
      "No"
    else ifelse(rows$Model_ID == "meanf--local--R1", "Yes", "No")
    write_data(rows, combo, child, "data", "forecasts", "-single_models")
    average <- read_selection_file(child, "forecasts", "-average_models", combo)
    average$Best_Model <- if (combo == "first")
      "Yes"
    else "No"
    write_data(average, combo, child, "data", "forecasts", "-average_models")
  }
  sequential_path <- file.path(withr::local_tempdir(), "sequential")
  fs::dir_copy(path, sequential_path)
  args$input_data <- rbind(data, data.frame(Date = as.Date("2022-01-01"), series = c("first", "second"), value = 125))
  args$overwrite <- TRUE
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
          saveRDS(list(pid = Sys.getpid(), no_chat = is.null(agent_info$llm), no_models = !any(vapply(
            agent_info,
            is.data.frame, logical(1)
          ))), file.path(destination, paste0("update-", Sys.getpid(), ".rds")))
          original(agent_info, ...)
        }, .package = "finnts", .env = globalenv())
        invisible(NULL)
      }, namespace, observations)
    parallel_info
  }, .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, parallel_processing = "local_machine", num_cores = 2)
  actual <- get_agent_forecast(current)
  columns <- c("Combo", "Date", "Model_ID", "Train_Test_ID", "Forecast", "Best_Model")
  expect_equal(dplyr::arrange(actual[, columns], Combo, Date, Model_ID, Train_Test_ID), dplyr::arrange(
    expected[, columns],
    Combo, Date, Model_ID, Train_Test_ID
  ))
  future <- actual[actual$Train_Test_ID == 1, ]
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
    original_load <- finnts:::load_update_runs
    metadata <- original_load(current)
    local_mocked_bindings(load_update_runs = function(...) metadata[metadata$combo == "second", , drop = FALSE], .package = "finnts")
    routed <- initial_agent_custom_update(prepared)
    expect_identical(routed$prev_best_runs_tbl$combo, "first")
  })
})

test_that("finalization preserves numeric-looking identities through both CSV rewrites", {
  for (mode in c("local", "global")) for (run_id in c("40607e5321822937", "4282d17137126405", "ordinary-run")) local({
    info <- list(project_info = list(
      project_name = "finalize-identity", path = withr::local_tempdir(), storage_object = NULL,
      data_output = "csv", object_output = "rds"
    ), run_id = run_id, max_iter = 3)
    parent <- info$project_info
    parent$run_name <- run_id
    row <- data.frame(
      combo = "00017", agent_run_id = run_id, best_run_name = "4282d17137126405", model_type = mode,
      weighted_mape = 0.125, max_iterations = 0, run_complete = FALSE
    )
    write_data(row, "00017", parent, "log", "logs", "-agent_best_run")
    path <- local_artifact_path(parent, "logs", "-agent_best_run", hash_data("00017"), "csv")
    before <- read_exact_artifact(parent, path, character_columns = c("combo", "agent_run_id", "best_run_name"))
    expect_identical(before$agent_run_id, run_id)
    finalize_run(info, combo = if (mode == "local")
      hash_data("00017")
    else NULL)
    after <- read_exact_artifact(parent, path, character_columns = c("combo", "agent_run_id", "best_run_name"))
    expect_identical(after$combo, "00017")
    expect_identical(after$agent_run_id, run_id)
    expect_identical(after$best_run_name, "4282d17137126405")
    expect_true(after$run_complete)
    expect_equal(after$max_iterations, 3)
    expect_equal(after$weighted_mape, 0.125)
    published <- load_best_agent_run(info)
    expect_equal(nrow(published), 1L)
    expect_identical(published$agent_run_id, run_id)
  })
})
