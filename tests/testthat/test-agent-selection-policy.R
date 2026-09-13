make_agent_policy_result <- function(wmape, risk = 0, fidelity = NA_real_) {
  list(selections = list(series = list(
    selected_id = "chosen",
    rankings = tibble::tibble(
      Model_ID = "chosen", Eligible = TRUE, WMAPE = wmape,
      Log_Weight = log(100), Risk = risk, Violations = as.integer(risk > 0),
      Reasons = list(if (risk > 0) "level_shift" else character()),
      Seasonal_Fidelity = fidelity
    )
  )))
}

test_that("iteration winners are compared by accuracy without quality tie-breaks", {
  for (scores in list(c(2, 0), c(0, 0))) {
    results <- list(make_agent_policy_result(0.08, scores[1], 0.5),
      make_agent_policy_result(0.083, scores[2], 0))
    ranked <- rank_agent_run_selections(results)
    expect_identical(ranked$series$series, 1L)
    expect_identical(ranked$best_run_index, 1L)
  }
})

test_that("equal iteration accuracy retains the earlier selected winner", {
  results <- list(make_agent_policy_result(0.08, 2, 0.5),
    make_agent_policy_result(0.08, 0, 0))
  ranked <- rank_agent_run_selections(results)
  expect_identical(ranked$series$series, 1L)
  expect_identical(ranked$best_run_index, 1L)
})

test_that("Agent metrics require completeness but not a second soft-quality pass", {
  result <- make_agent_policy_result(0.01, 2, 0.5)
  metric <- calculate_fcst_metrics(list(forecast_selection = result), data.frame())
  expect_equal(as.numeric(metric), 0.01)
  expect_true(attr(metric, "selection_ok"))
  result$selections["missing"] <- list(NULL)
  metric <- calculate_fcst_metrics(list(forecast_selection = result), data.frame())
  expect_identical(as.numeric(metric), Inf)
  expect_false(attr(metric, "selection_ok"))
})

test_that("completed forecast restoration never evaluates future quality", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  selected <- final_models(info, weekly_to_daily = FALSE)
  log <- read_selection_file(info, "logs")
  local_mocked_bindings(
    select_series_forecasts = function(...) stop("must not repeat future-quality selection"),
    forecast_path_risk = function(...) stop("must not assess saved future paths")
  )
  restored <- assess_agent_run(info, log, "Synthetic")
  expect_identical(restored$selections$Synthetic$selected_id,
    selected$selections$Synthetic$selected_id)
  expect_equal(agent_selection_summary(restored)$weighted_mape, log$weighted_mape)
})

test_that("normal iteration stops on a complete accurate winner with soft concerns", {
  state <- new.env(parent = emptyenv())
  state$submissions <- 0L
  state$refreshes <- 0L
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(...) chat
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "prompt",
    reason_inputs = function(...) list(models_to_run = "arima"),
    submit_fcst_run = function(...) {
      state$submissions <- state$submissions + 1L
      list(forecast_selection = make_agent_policy_result(0.01, 2, 0.5))
    },
    get_fcst_output = function(...) data.frame(),
    log_best_run = function(...) "logged",
    load_reason_history = function(...) {
      state$refreshes <- state$refreshes + 1L
      list(total_runs = state$submissions)
    },
    finalize_run = function(...) "finalized"
  )
  result <- fcst_agent_workflow(list(llm = chat, agent_version = 1), combo = "series",
    weighted_mape_goal = 0.03, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1, max_iter = 2, previous_run_results = "No Previous Runs")
  expect_identical(state$submissions, 1L)
  expect_identical(state$refreshes, 0L)
  expect_identical(result$node, "stop")
})

test_that("run history compares recorded accuracy without restoring forecasts", {
  reads <- 0L
  logs <- data.frame(
    project_name = paste0("project_", hash_data("all")),
    run_name = paste0("agent_run_", hash_data("all"), "_", 1:4),
    created = paste0("2026-01-01 00:00:0", 1:4),
    agent_version = c(1, 1, 1, 0), agent_forecast_approach = "bottoms_up",
    weighted_mape = c(0.08, 0.083, 0.001, 0.0001),
    selection_status = c("evaluated", "evaluated", "partial", "evaluated")
  )
  local_mocked_bindings(
    get_run_info = function(...) { reads <<- reads + 1L; logs },
    agent_selection_pool = function(...) stop("history must not restore old forecast assessments"),
    assess_agent_run = function(...) stop("history must use its recorded accuracy"),
    read_series_history = function(...) stop("history must not reload prepared recipes")
  )
  agent <- list(agent_version = 1, forecast_approach = "bottoms_up",
    selection_combos = c("first", "second"),
    project_info = list(project_name = "project", path = tempdir()))
  result <- load_run_results(agent)
  expect_equal(result$weighted_mape[result$best_run == "yes"], 0.08)
  expect_true(all(result$best_run[result$selection_status == "partial"] == "no"))
  expect_true(all(result$best_run[result$agent_version == 0] == "no"))
  expect_identical(reads, 1L)
})

test_that("best-run logging compares the saved incumbent without forecast rereads", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  info$forecast_selection <- final_models(info, weekly_to_daily = FALSE)
  agent <- list(project_info = info, run_id = "parent", agent_version = 1,
    forecast_approach = "bottoms_up")
  log_best_run(agent, info, 0.1, combo = hash_data("Synthetic"), check_best_run = FALSE)
  later <- info
  later$run_name <- "later"
  later$forecast_selection <- make_agent_policy_result(0.12, 0)
  names(later$forecast_selection$selections) <- "Synthetic"
  later_log <- read_selection_file(info, "logs")
  later_log$run_name <- later$run_name
  write_data(later_log, combo = NULL, run_info = later,
    output_type = "log", folder = "logs", suffix = NULL)
  local_mocked_bindings(
    load_run_results = function(...) stop("incumbent comparison must not rescan run history"),
    agent_selection_pool = function(...) stop("incumbent comparison must not reload forecasts"),
    assess_agent_run = function(...) stop("incumbent comparison must not reassess forecasts"),
    read_series_history = function(...) stop("incumbent comparison must not reload recipes")
  )
  parent <- info
  parent$run_name <- agent$run_id
  log_best_run(agent, later, 0.12, combo = hash_data("Synthetic"))
  saved <- read_selection_file(parent, "logs", "-agent_best_run", "Synthetic")
  expect_identical(saved$best_run_name, info$run_name)
  later$forecast_selection$selections$Synthetic$rankings$WMAPE <- 0.085
  log_best_run(agent, later, 0.085, combo = hash_data("Synthetic"))
  saved <- read_selection_file(parent, "logs", "-agent_best_run", "Synthetic")
  expect_identical(saved$best_run_name, later$run_name)
  expect_equal(saved$weighted_mape, 0.085)
})

test_that("only unfinished default acceptance restores one-time quality evidence", {
  quality_calls <- 0L
  status <- NULL
  current <- make_agent_policy_result(0.03)
  current$rejected_combos <- character()
  restored <- current
  restored$selections$series$rankings$Risk <- NA_real_
  restored$selections$series$rankings$Violations <- NA_integer_
  info <- list(project_name = "project", run_name = "default", path = tempdir(), data_output = "csv")
  local_mocked_bindings(
    list_files = function(...) stop("known input must not require directory discovery"),
    read_local_artifacts = function(run_info, file_list, ...) {
      expect_length(file_list, 1L)
      expect_false(grepl("*", file_list, fixed = TRUE))
      data.frame(Combo = "series", Date = as.Date("2024-01-01") + 0:5, Target = 100)
    },
    read_file = function(...) data.frame(Combo = "series", Date = as.Date("2024-01-01") + 0:5, Target = 100),
    set_run_info = function(...) info,
    read_selection_file = function(...) data.frame(forecast_approach = "bottoms_up", date_type = "month"),
    prep_data = function(...) NULL,
    prep_models = function(...) NULL,
    train_models = function(...) NULL,
    final_models = function(...) restored,
    assess_agent_run = function(..., check_quality = FALSE) {
      expect_true(check_quality)
      quality_calls <<- quality_calls + 1L
      current
    },
    write_data = function(x, ...) { status <<- x$default_reforecast_status },
    validate_run_outputs = function(...) TRUE
  )
  agent <- list(run_id = "run", agent_version = 1, default_reforecast = TRUE,
    project_info = list(project_name = "project", path = tempdir(), data_output = "csv"))
  result <- submit_fcst_run(agent, list(models_to_run = "meanf"), hash_data("series"), "default")
  expect_identical(quality_calls, 1L)
  expect_identical(status, "accepted")
  expect_true(agent_selection_summary(result$forecast_selection, check_quality = TRUE)$acceptable)
  agent$default_reforecast <- FALSE
  quality_calls <- 0L
  status <- NULL
  submit_fcst_run(agent, list(models_to_run = "meanf"), hash_data("series"), "iteration")
  expect_identical(quality_calls, 0L)
  expect_null(status)
})

make_selected_agent_log_fixture <- function(path, series = c("North--Revenue", "South--Revenue"),
                                            global = TRUE) {
  info <- list(project_name = "selected-run-logging", run_name = "iteration-1",
    path = path, data_output = "csv", object_output = "rds", storage_object = NULL)
  info$forecast_selection <- list(selections = stats::setNames(lapply(seq_along(series), function(index) {
    make_agent_policy_result(0.08 + index / 100)$selections$series
  }), series))
  info$selection_combos <- series
  log <- data.frame(project_name = info$project_name, run_name = info$run_name,
    path = path, data_output = "csv", object_output = "rds",
    run_global_models = global, weighted_mape = 0.5)
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs")
  agent <- list(project_info = info, run_id = "parent-run", agent_version = 2,
    forecast_approach = "bottoms_up", selection_combos = series)
  parent <- info
  parent$run_name <- agent$run_id
  list(info = info, agent = agent, parent = parent, log = log)
}

test_that("global iteration promotion never mixes per-series winners", {
  cases <- list(
    list(wmape = c(0.08, 0.13), winner = "iteration-1", saved_wmape = c(0.1, 0.1)),
    list(wmape = c(0.06, 0.11), winner = "iteration-2", saved_wmape = c(0.06, 0.11)),
    list(wmape = c(0.09, 0.11), winner = "iteration-1", saved_wmape = c(0.1, 0.1))
  )
  for (case in cases) local({
    fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
    series <- names(fixture$info$forecast_selection$selections)
    fixture$info$forecast_selection$selections <- lapply(
      fixture$info$forecast_selection$selections, function(selection) {
        selection$rankings$WMAPE <- 0.1
        selection
      }
    )
    local_mocked_bindings(
      list_files = function(...) stop("global promotion must not enumerate artifacts"),
      get_run_info = function(...) stop("global promotion must not rescan run history"),
      forecast_path_risk = function(...) stop("global promotion must not reassess future paths")
    )
    log_best_run(fixture$agent, fixture$info, 0.1, check_best_run = FALSE)
    later <- fixture$info
    later$run_name <- "iteration-2"
    for (index in seq_along(series)) {
      later$forecast_selection$selections[[index]]$rankings$WMAPE <- case$wmape[index]
    }
    later$forecast_selection$selections <- later$forecast_selection$selections[2:1]
    later_log <- fixture$log
    later_log$run_name <- later$run_name
    write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")

    log_best_run(fixture$agent, later, mean(case$wmape))

    saved <- dplyr::bind_rows(lapply(series, function(combo) {
      read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
    }))
    expect_identical(unique(saved$best_run_name), case$winner)
    expect_equal(saved$weighted_mape, case$saved_wmape)
    log_best_run(fixture$agent, later, mean(case$wmape))
    repeated <- dplyr::bind_rows(lapply(series, function(combo) {
      read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
    }))
    expect_equal(repeated, saved)
    third <- later
    third$run_name <- "iteration-3"
    for (index in seq_along(series)) {
      third$forecast_selection$selections[[series[index]]]$rankings$WMAPE <- c(0.03, 0.12)[index]
    }
    third_log <- fixture$log
    third_log$run_name <- third$run_name
    write_data(third_log, combo = NULL, run_info = third, output_type = "log", folder = "logs")

    log_best_run(fixture$agent, third, 0.075)

    promoted <- dplyr::bind_rows(lapply(series, function(combo) {
      read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
    }))
    expect_identical(unique(promoted$best_run_name), third$run_name)
    expect_equal(promoted$weighted_mape, c(0.03, 0.12))
  })
})

test_that("incomplete global iterations cannot promote an available subset", {
  for (mode in c("missing", "reported-missing", "rejected", "forced")) local({
    fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
    series <- names(fixture$info$forecast_selection$selections)
    log_best_run(fixture$agent, fixture$info, 0.095, check_best_run = FALSE)
    later <- fixture$info
    later$run_name <- "partial-iteration"
    later$forecast_selection$selections[[1]]$rankings$WMAPE <- 0.01
    if (mode %in% c("missing", "reported-missing")) {
      later$forecast_selection$selections <- later$forecast_selection$selections[1]
      if (mode == "reported-missing") later$selection_combos <- series[1]
    } else {
      later$forecast_selection$selections[[2]]$selected_id <- NA_character_
    }
    later_log <- fixture$log
    later_log$run_name <- later$run_name
    write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")
    local_mocked_bindings(list_files = function(...) stop("partial promotion must not enumerate"))

    expect_no_error(log_best_run(fixture$agent, later, Inf, check_best_run = mode != "forced"))

    saved <- dplyr::bind_rows(lapply(series, function(combo) {
      read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
    }))
    expect_identical(unique(saved$best_run_name), fixture$info$run_name)
    log <- read_selection_file(later, "logs")
    expect_identical(log$selection_status, "partial")
    expect_true(is.na(log$weighted_mape))
  })
})

test_that("global promotion preserves a superior local winner", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  series <- names(fixture$info$forecast_selection$selections)
  log_best_run(fixture$agent, fixture$info, 0.095, check_best_run = FALSE)
  local_winner <- read_selection_file(fixture$parent, "logs", "-agent_best_run", series[1])
  local_winner$model_type <- "local"
  local_winner$best_run_name <- "local-winner"
  local_winner$weighted_mape <- 0.02
  write_data(local_winner, combo = series[1], run_info = fixture$parent,
    output_type = "log", folder = "logs", suffix = "-agent_best_run")
  later <- fixture$info
  later$run_name <- "new-global-winner"
  later$forecast_selection$selections[[1]]$rankings$WMAPE <- 0.05
  later$forecast_selection$selections[[2]]$rankings$WMAPE <- 0.11
  later_log <- fixture$log
  later_log$run_name <- later$run_name
  write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")

  log_best_run(fixture$agent, later, 0.08)

  saved <- dplyr::bind_rows(lapply(series, function(combo) {
    read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
  }))
  expect_identical(saved$best_run_name, c("local-winner", "new-global-winner"))
  expect_identical(saved$model_type, c("local", "global"))
  expect_equal(saved$weighted_mape, c(0.02, 0.11))
})

test_that("mixed global iterations cannot be loaded or finalized", {
  mixed <- data.frame(combo = c("North", "South"), model_type = "global",
    best_run_name = c("iteration-1", "iteration-2"), agent_version = 2,
    weighted_mape = c(0.1, 0.08))
  agent <- list(project_info = list(project_name = "project", path = tempdir(), data_output = "csv"),
    run_id = "parent", agent_version = 2, max_iter = 3, forecast_approach = "bottoms_up")
  writes <- 0L
  forecasts <- 0L
  local_mocked_bindings(
    check_agent_info = function(...) NULL,
    list_files = function(...) "existing-metadata.csv",
    read_file = function(...) mixed,
    write_data = function(...) { writes <<- writes + 1L },
    get_total_combos = function(...) vapply(mixed$combo, hash_data, character(1)),
    read_selected_agent_forecasts = function(...) {
      forecasts <<- forecasts + 1L
      stop("mixed forecasts reached the prediction reader")
    }
  )
  expect_error(load_best_agent_run(agent), "single global iteration")
  expect_error(finalize_run(agent), "single global iteration")
  expect_error(save_best_agent_run(agent), "single global iteration")
  expect_error(load_agent_forecast(agent), "single global iteration")
  expect_identical(writes, 0L)
  expect_identical(forecasts, 0L)
})

test_that("global updates reject mixed iterations before any refit", {
  mixed <- data.frame(combo = c("North", "South"), model_type = "global",
    best_run_name = c("iteration-1", "iteration-2"), agent_version = 2)
  fits <- 0L
  local_mocked_bindings(update_forecast_combo = function(...) {
    fits <<- fits + 1L
    list(status = "done")
  })
  expect_error(update_global_models(list(project_info = list()), mixed, NULL, FALSE, 1, 123),
    "single global iteration")
  expect_identical(fits, 0L)
  mixed$best_run_name <- "iteration-1"
  result <- update_global_models(list(project_info = list()), mixed, NULL, FALSE, 1, 123)
  expect_identical(fits, 1L)
  expect_length(result$failed_combos, 0L)
})

test_that("selection-backed logging preserves genuine local model-pool statistics", {
  for (global in c(FALSE, TRUE)) local({
    fixture <- make_selected_agent_log_fixture(withr::local_tempdir(), "series", global = global)
    fixture$info$forecast_selection$selections$series$rankings$WMAPE <- 0.1
    forecasts <- data.frame(
      Combo = "series", Model_ID = rep(c("arima", "glmnet", "xgboost", "average"), each = 2),
      Recipe_ID = rep(c("R1", "R1", "R1", "simple_average"), each = 2),
      Run_Type = "Back_Test", Target = rep(c(100, 300), 4)
    )
    forecasts$Forecast <- forecasts$Target * (1 + rep(c(0.1, 0.2, 0.3, 0.001), each = 2))
    future <- forecasts
    future$Run_Type <- "Future_Forecast"
    future$Forecast <- 1000000
    forecasts <- dplyr::bind_rows(forecasts, future)
    local_mocked_bindings(
      list_files = function(...) stop("model statistics must not enumerate artifacts"),
      read_series_history = function(...) stop("model statistics must use loaded backtests"),
      forecast_path_risk = function(...) stop("model statistics must not reassess future paths")
    )

    metric <- calculate_fcst_metrics(fixture$info, forecasts)
    log_best_run(fixture$agent, fixture$info, metric, check_best_run = FALSE)

    current <- read_selection_file(fixture$info, "logs")
    saved <- read_selection_file(fixture$parent, "logs", "-agent_best_run", "series")
    expected <- if (global) c(0.1, 0.1, 0) else c(0.2, 0.2, 0.1)
    for (log in list(current, saved)) {
      expect_equal(unname(unlist(log[, c("model_avg_wmape", "model_median_wmape", "model_std_wmape")])),
        expected)
    }
  })
})

test_that("model-pool statistics preserve signed targets and unavailable values", {
  forecasts <- data.frame(Model_ID = rep(c("arima", "glmnet"), each = 3),
    Recipe_ID = "R1", Run_Type = "Back_Test", Model_Type = "local",
    Target = rep(c(0, -100, 300), 2))
  adjusted <- ifelse(forecasts$Target == 0, 0.1, forecasts$Target)
  forecasts$Forecast <- adjusted * (1 + rep(c(0.1, 0.2), each = 3))
  expect_equal(agent_model_accuracy(forecasts),
    list(model_avg_wmape = 0.15, model_median_wmape = 0.15, model_std_wmape = stats::sd(c(0.1, 0.2))))
  expect_equal(agent_model_accuracy(forecasts[forecasts$Model_ID == "arima", ]),
    list(model_avg_wmape = 0.1, model_median_wmape = 0.1, model_std_wmape = NA_real_))
  unavailable <- list(model_avg_wmape = NA_real_, model_median_wmape = NA_real_, model_std_wmape = NA_real_)
  global <- forecasts
  global$Model_Type <- "global"
  expect_identical(agent_model_accuracy(global), unavailable)
  forecasts$Forecast[1] <- Inf
  expect_identical(agent_model_accuracy(forecasts), unavailable)
  forecasts$Target[] <- NA_real_
  expect_identical(agent_model_accuracy(forecasts), unavailable)
  expect_identical(agent_model_accuracy(data.frame()), unavailable)
})

test_that("Agent decisions and saved winners retain original rounded precision", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir(), "series", global = FALSE)
  fixture$info$forecast_selection$selections$series$rankings$WMAPE <- 0.100049
  metric <- calculate_fcst_metrics(fixture$info, data.frame())
  expect_identical(as.numeric(metric), 0.1)
  log_best_run(fixture$agent, fixture$info, metric, check_best_run = FALSE)
  current <- read_selection_file(fixture$info, "logs")
  saved <- read_selection_file(fixture$parent, "logs", "-agent_best_run", "series")
  expect_equal(current$weighted_mape, 0.1)
  expect_equal(saved$weighted_mape, 0.1)
  later <- fixture$info
  later$run_name <- "rounded-tie"
  later$forecast_selection$selections$series$rankings$WMAPE <- 0.100041
  later_log <- fixture$log
  later_log$run_name <- later$run_name
  write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")

  log_best_run(fixture$agent, later, calculate_fcst_metrics(later, data.frame()))

  saved <- read_selection_file(fixture$parent, "logs", "-agent_best_run", "series")
  expect_identical(saved$best_run_name, fixture$info$run_name)
})

test_that("the workflow does not stop below a goal only because of unrounded digits", {
  state <- new.env(parent = emptyenv())
  state$submissions <- 0L
  state$refreshes <- 0L
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(...) chat
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "prompt",
    reason_inputs = function(...) list(models_to_run = "arima"),
    submit_fcst_run = function(...) {
      state$submissions <- state$submissions + 1L
      list(forecast_selection = make_agent_policy_result(if (state$submissions == 1L) 0.09996 else 0.08))
    },
    get_fcst_output = function(...) data.frame(),
    log_best_run = function(...) "logged",
    load_reason_history = function(...) {
      state$refreshes <- state$refreshes + 1L
      list(total_runs = state$submissions)
    },
    finalize_run = function(...) "finalized"
  )

  result <- fcst_agent_workflow(list(llm = chat, agent_version = 1), combo = "series",
    weighted_mape_goal = 0.1, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1, max_iter = 2, previous_run_results = "No Previous Runs")

  expect_identical(state$submissions, 2L)
  expect_identical(state$refreshes, 1L)
  expect_identical(result$node, "stop")
})

test_that("global promotion uses the workflow's existing iteration history", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  series <- names(fixture$info$forecast_selection$selections)
  log_best_run(fixture$agent, fixture$info, 0.095, check_best_run = FALSE)
  for (index in seq_along(series)) {
    previous <- read_selection_file(fixture$parent, "logs", "-agent_best_run", series[index])
    previous$model_type <- "local"
    previous$best_run_name <- paste0("local-", index)
    previous$weighted_mape <- c(0.07, 0.2)[index]
    write_data(previous, combo = series[index], run_info = fixture$parent,
      output_type = "log", folder = "logs", suffix = "-agent_best_run")
  }
  later <- fixture$info
  later$run_name <- "worse-global-iteration"
  later$forecast_selection$selections[[1]]$rankings$WMAPE <- 0.08
  later$forecast_selection$selections[[2]]$rankings$WMAPE <- 0.09
  later_log <- fixture$log
  later_log$run_name <- later$run_name
  write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")
  history <- data.frame(run_name = "previous-global-iteration", agent_version = 2,
    weighted_mape = 0.05, model_avg_wmape = 0.05, selection_status = "evaluated")
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(...) chat
  fixture$agent$llm <- chat
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    resolve_agent_global_forecast_approaches = function(...) "bottoms_up",
    iterate_forecast_system_prompt = function(...) "prompt",
    reason_inputs = function(...) list(models_to_run = "meanf"),
    submit_fcst_run = function(...) later,
    get_fcst_output = function(...) data.frame(),
    get_run_info = function(...) stop("promotion must reuse the loaded iteration history"),
    list_files = function(...) stop("promotion must not enumerate artifacts"),
    finalize_run = function(...) "finalized"
  )

  fcst_agent_workflow(fixture$agent, combo = NULL, weighted_mape_goal = 0.01,
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1,
    max_iter = 1, previous_run_results = history)

  saved <- dplyr::bind_rows(lapply(series, function(combo) {
    read_selection_file(fixture$parent, "logs", "-agent_best_run", combo)
  }))
  expect_identical(saved$best_run_name, c("local-1", "local-2"))
  expect_identical(saved$model_type, rep("local", 2))
})

test_that("an incomplete global result cannot satisfy the workflow accuracy goal", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  log_best_run(fixture$agent, fixture$info, 0.095, check_best_run = FALSE)
  partial <- fixture$info
  partial$run_name <- "partial-goal"
  partial$forecast_selection$selections <- partial$forecast_selection$selections[1]
  partial$forecast_selection$selections[[1]]$rankings$WMAPE <- 0.001
  partial$selection_combos <- names(partial$forecast_selection$selections)
  partial_log <- fixture$log
  partial_log$run_name <- partial$run_name
  write_data(partial_log, combo = NULL, run_info = partial, output_type = "log", folder = "logs")
  submissions <- 0L
  refreshes <- 0L
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(...) chat
  fixture$agent$llm <- chat
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    resolve_agent_global_forecast_approaches = function(...) "bottoms_up",
    iterate_forecast_system_prompt = function(...) "prompt",
    reason_inputs = function(...) list(models_to_run = "meanf"),
    submit_fcst_run = function(...) {
      submissions <<- submissions + 1L
      partial
    },
    get_fcst_output = function(...) data.frame(),
    load_reason_history = function(...) {
      refreshes <<- refreshes + 1L
      new_reason_history("No Previous Runs", fixture$agent$agent_version)
    },
    finalize_run = function(...) "finalized"
  )

  result <- fcst_agent_workflow(fixture$agent, combo = NULL, weighted_mape_goal = 0.01,
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1,
    max_iter = 2, previous_run_results = "No Previous Runs")

  expect_identical(submissions, 2L)
  expect_identical(refreshes, 1L)
  expect_identical(result$completion_reason, "quality_rejected")
  saved <- load_best_agent_run(fixture$agent)
  expect_identical(unique(saved$best_run_name), fixture$info$run_name)
})

test_that("interrupted global promotion cannot be marked complete", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  series <- names(fixture$info$forecast_selection$selections)
  log_best_run(fixture$agent, fixture$info, 0.095, check_best_run = FALSE)
  later <- fixture$info
  later$run_name <- "interrupted-iteration"
  later$forecast_selection$selections[[1]]$rankings$WMAPE <- 0.05
  later$forecast_selection$selections[[2]]$rankings$WMAPE <- 0.06
  later_log <- fixture$log
  later_log$run_name <- later$run_name
  write_data(later_log, combo = NULL, run_info = later, output_type = "log", folder = "logs")
  original_write <- write_data
  local_mocked_bindings(write_data = function(...) {
    arguments <- list(...)
    if (identical(arguments$combo, series[2]) && identical(arguments$suffix, "-agent_best_run")) {
      return(invisible(NULL))
    }
    do.call(original_write, arguments)
  })

  expect_error(log_best_run(fixture$agent, later, 0.055), "does not match the selected iteration")

  current <- read_selection_file(later, "logs")
  expect_false("selection_status" %in% names(current))
  expect_error(load_best_agent_run(fixture$agent), "single global iteration")
  expect_error(finalize_run(fixture$agent), "single global iteration")
})

test_that("selected Agent logging verifies every exact file without enumeration", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  series <- names(fixture$info$forecast_selection$selections)
  expected_paths <- vapply(series, function(combo) {
    as.character(fs::path(fixture$parent$path, "logs", paste0(
      hash_data(fixture$parent$project_name), "-", hash_data(fixture$parent$run_name),
      "-", hash_data(combo), "-agent_best_run.csv")))
  }, character(1), USE.NAMES = FALSE)
  reads <- character()
  original_read <- read_file
  local_mocked_bindings(
    list_files = function(...) stop("selected logger must not enumerate files"),
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      expect_null(path)
      expect_length(file_list, 1L)
      reads <<- c(reads, as.character(file_list))
      original_read(run_info, file_list = file_list, ...)
    }
  )
  result <- log_best_run(fixture$agent, fixture$info, 0, check_best_run = FALSE)
  expect_identical(result, list(status = "evaluated", selected_combos = series))
  expect_length(reads, 3L)
  expect_identical(utils::tail(reads, 2), expected_paths)
  for (index in seq_along(series)) {
    saved <- original_read(fixture$parent, file_list = expected_paths[index])
    expect_identical(saved$combo, series[index])
    expect_identical(saved$best_run_name, fixture$info$run_name)
    expect_equal(saved$weighted_mape,
      fixture$info$forecast_selection$selections[[index]]$rankings$WMAPE)
  }
})

test_that("unrelated best-run files cannot hide a missing selected record", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir())
  write_data(fixture$log, combo = "Unrelated", run_info = fixture$parent,
    output_type = "log", folder = "logs", suffix = "-agent_best_run")
  missing_path <- as.character(fs::path(fixture$parent$path, "logs", paste0(
    hash_data(fixture$parent$project_name), "-", hash_data(fixture$parent$run_name),
    "-", hash_data("South--Revenue"), "-agent_best_run.csv")))
  original_write <- write_data
  original_read <- read_file
  local_mocked_bindings(
    write_data = function(...) {
      arguments <- list(...)
      if (identical(arguments$combo, "South--Revenue") &&
          identical(arguments$suffix, "-agent_best_run")) return(invisible(NULL))
      do.call(original_write, arguments)
    },
    read_file = function(run_info, file_list = NULL, ...) {
      if (identical(as.character(file_list), missing_path)) return(tibble::tibble())
      original_read(run_info, file_list = file_list, ...)
    }
  )
  expect_error(log_best_run(fixture$agent, fixture$info, 0, check_best_run = FALSE),
    "The exact forecast artifact is empty or unreadable")
  expect_false(file.exists(missing_path))
  current <- read_selection_file(fixture$info, "logs")
  expect_false("selection_status" %in% names(current))
  expect_equal(current$weighted_mape, fixture$log$weighted_mape)
})

test_that("local selected Agent logging verifies all written records directly", {
  for (series_count in c(1L, 2L)) local({
    series <- c("North--Revenue", "South--Revenue")[seq_len(series_count)]
    fixture <- make_selected_agent_log_fixture(withr::local_tempdir(), series, global = FALSE)
    verified <- character()
    original_read <- read_selection_file
    local_mocked_bindings(
      list_files = function(...) stop("selected logger must not enumerate files"),
      read_selection_file = function(run_info, folder, suffix = NULL, combo = NULL,
                                      optional = FALSE, cache = NULL) {
        if (identical(suffix, "-agent_best_run")) {
          expect_false(optional)
          expect_null(cache)
          verified <<- c(verified, combo)
        }
        original_read(run_info, folder, suffix, combo, optional, cache)
      }
    )
    result <- log_best_run(fixture$agent, fixture$info, 0,
      combo = hash_data(series[1]), check_best_run = FALSE)
    expect_identical(verified, series)
    expect_identical(result, list(status = "evaluated", selected_combos = series))
    for (combo in series) {
      saved <- original_read(fixture$parent, "logs", "-agent_best_run", combo)
      expect_identical(saved$model_type, "local")
    }
  })
})

test_that("selected record read failures prevent final attempt logging", {
  for (failure_mode in c("empty", "unreadable", "provider")) local({
    fixture <- make_selected_agent_log_fixture(withr::local_tempdir(), "North--Revenue")
    original_read <- read_file
    local_mocked_bindings(
      list_files = function(...) stop("selected logger must not enumerate files"),
      read_file = function(run_info, file_list = NULL, ...) {
        if (length(file_list) == 1L && endsWith(file_list, "-agent_best_run.csv")) {
          return(switch(failure_mode,
            empty = tibble::tibble(), unreadable = NULL,
            provider = rlang::abort("selected record storage unavailable", class = "selected_record_read_error")))
        }
        original_read(run_info, file_list = file_list, ...)
      }
    )
    expected <- if (failure_mode == "provider") "selected record storage unavailable" else
      "The exact forecast artifact is empty or unreadable"
    error <- expect_error(log_best_run(fixture$agent, fixture$info, 0, check_best_run = FALSE), expected)
    if (failure_mode == "provider") expect_s3_class(error, "selected_record_read_error")
    current <- read_selection_file(fixture$info, "logs")
    expect_false("selection_status" %in% names(current))
    expect_equal(current$weighted_mape, fixture$log$weighted_mape)
  })
})

test_that("local selected logging verifies only new winners and preserves incumbent rules", {
  fixture <- make_selected_agent_log_fixture(withr::local_tempdir(),
    c("retained", "improved", "rejected"), global = FALSE)
  fixture$info$forecast_selection$selections$rejected$selected_id <- NA_character_
  previous <- data.frame(best_run_name = "incumbent", agent_version = 2, weighted_mape = 0.09)
  write_data(previous, combo = "retained", run_info = fixture$parent,
    output_type = "log", folder = "logs", suffix = "-agent_best_run")
  previous$weighted_mape <- 0.2
  write_data(previous, combo = "improved", run_info = fixture$parent,
    output_type = "log", folder = "logs", suffix = "-agent_best_run")
  reads <- list()
  written <- character()
  original_read <- read_selection_file
  original_write <- write_data
  local_mocked_bindings(
    list_files = function(...) stop("selected logger must not enumerate files"),
    read_selection_file = function(run_info, folder, suffix = NULL, combo = NULL,
                                    optional = FALSE, cache = NULL) {
      if (identical(suffix, "-agent_best_run")) {
        reads[[length(reads) + 1L]] <<- list(series = combo, optional = optional)
      }
      original_read(run_info, folder, suffix, combo, optional, cache)
    },
    write_data = function(...) {
      arguments <- list(...)
      if (identical(arguments$suffix, "-agent_best_run")) written <<- c(written, arguments$combo)
      do.call(original_write, arguments)
    }
  )
  result <- log_best_run(fixture$agent, fixture$info, 0)
  expect_identical(result, list(status = "partial", selected_combos = c("retained", "improved")))
  expect_identical(written, "improved")
  expect_identical(reads, list(list(series = "retained", optional = TRUE),
    list(series = "improved", optional = TRUE), list(series = "improved", optional = FALSE)))
  saved <- original_read(fixture$parent, "logs", "-agent_best_run", "retained")
  expect_identical(saved$best_run_name, "incumbent")
  expect_equal(saved$weighted_mape, 0.09)
  saved <- original_read(fixture$parent, "logs", "-agent_best_run", "improved")
  expect_identical(saved$best_run_name, fixture$info$run_name)
  expect_equal(saved$weighted_mape, 0.1)

  reads <- list()
  written <- character()
  expect_identical(log_best_run(fixture$agent, fixture$info, 0), result)
  expect_length(written, 0L)
  expect_identical(reads, list(list(series = "retained", optional = TRUE),
    list(series = "improved", optional = TRUE)))

  fixture$agent$agent_version <- 3
  reads <- list()
  expect_identical(log_best_run(fixture$agent, fixture$info, 0), result)
  expect_identical(written, c("retained", "improved"))
  expect_identical(reads, list(list(series = "retained", optional = TRUE),
    list(series = "improved", optional = TRUE), list(series = "retained", optional = FALSE),
    list(series = "improved", optional = FALSE)))
  saved <- original_read(fixture$parent, "logs", "-agent_best_run", "retained")
  expect_identical(saved$best_run_name, fixture$info$run_name)
  expect_equal(saved$agent_version, 3)

  fixture$info$forecast_selection$selections <- lapply(fixture$info$forecast_selection$selections, function(selection) {
    selection$selected_id <- NA_character_
    selection
  })
  reads <- list()
  written <- character()
  expect_identical(log_best_run(fixture$agent, fixture$info, 0),
    list(status = "rejected", selected_combos = character()))
  expect_length(reads, 0L)
  expect_length(written, 0L)
  current <- original_read(fixture$info, "logs")
  expect_identical(current$selection_status, "rejected")
  expect_true(is.na(current$weighted_mape))
})