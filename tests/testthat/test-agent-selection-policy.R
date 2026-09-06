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
    list_files = function(...) "input.csv",
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