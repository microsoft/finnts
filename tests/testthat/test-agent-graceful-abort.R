make_graceful_abort_agent_info <- function(run_global_models = TRUE,
                                           run_local_models = TRUE) {
  list(
    project_info = list(
      project_name = "graceful_abort",
      path = tempdir(),
      data_output = "csv",
      object_output = "rds",
      storage_object = NULL,
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    run_id = "agent-run",
    agent_version = 2,
    forecast_horizon = 6,
    hist_end_date = as.Date("2026-06-01"),
    external_regressors = "Driver_A",
    forecast_approach = "bottoms_up",
    negative_forecast = FALSE,
    run_global_models = run_global_models,
    run_local_models = run_local_models
  )
}

make_agent_response <- function(...) {
  values <- utils::modifyList(
    list(
      models_to_run = "arima---ets",
      external_regressors = "NULL",
      clean_missing_values = "FALSE",
      clean_outliers = "FALSE",
      forecast_approach = "bottoms_up",
      stationary = "FALSE",
      feature_selection = "FALSE",
      multistep_horizon = "FALSE",
      seasonal_period = "NULL",
      recipes_to_run = "R1",
      lag_periods = "NULL",
      rolling_window_periods = "NULL",
      reasoning = "deterministic test response"
    ),
    list(...)
  )
  jsonlite::toJSON(values, auto_unbox = TRUE)
}

make_queued_fake_chat <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$responses <- responses
  state$call_count <- 0L
  state$prompts <- character(0)
  state$system_prompt <- NULL

  state$chat <- function(prompt, ...) {
    state$call_count <- state$call_count + 1L
    state$prompts <- c(state$prompts, as.character(prompt))
    response <- state$responses[[min(state$call_count, length(state$responses))]]
    if (inherits(response, "condition")) {
      stop(response)
    }
    response
  }
  state$clone <- function(deep = FALSE) state
  state$set_system_prompt <- function(prompt) {
    state$system_prompt <- prompt
    state
  }
  state$set_turns <- function(turns) state
  state
}

minimal_reason_history <- function(...) {
  defaults <- list(
    agent_version = 2,
    run_number = 1,
    best_run = "yes",
    weighted_mape = 0.2,
    model_avg_wmape = 0.2,
    model_median_wmape = 0.2,
    model_std_wmape = 0,
    models_to_run = "arima---ets",
    external_regressors = NA_character_,
    clean_missing_values = FALSE,
    clean_outliers = FALSE,
    forecast_approach = "bottoms_up",
    stationary = FALSE,
    feature_selection = FALSE,
    multistep_horizon = FALSE,
    seasonal_period = NA_character_,
    recipes_to_run = "R1",
    lag_periods = NA_character_,
    rolling_window_periods = NA_character_
  )
  as.data.frame(utils::modifyList(defaults, list(...)), stringsAsFactors = FALSE)
}

test_that("reason history separates current runs from previous-version replay context", {
  history <- dplyr::bind_rows(
    minimal_reason_history(
      agent_version = 1,
      run_number = 1,
      models_to_run = "arima",
      weighted_mape = 0.15
    ),
    minimal_reason_history(
      agent_version = 1,
      run_number = 2,
      models_to_run = "ets",
      weighted_mape = 0.12
    ),
    minimal_reason_history(
      agent_version = 2,
      run_number = 3,
      models_to_run = "glmnet",
      weighted_mape = 0.2
    )
  )

  snapshot <- new_reason_history(history, agent_version = 2)

  expect_identical(snapshot$total_runs, 1L)
  expect_equal(snapshot$current_run_results$agent_version, 2)
  expect_equal(snapshot$previous_version_results$agent_version, c(1, 1))
  expect_equal(snapshot$previous_run_results, snapshot$current_run_results)
})

test_that("previous-version replay renders only nullable defaults as NULL", {
  history <- dplyr::bind_rows(
    minimal_reason_history(
      agent_version = 1,
      run_number = 1,
      model_std_wmape = NA_real_,
      recipes_to_run = NA_character_
    ),
    minimal_reason_history(
      agent_version = 2,
      run_number = 2,
      recipes_to_run = NA_character_
    )
  )
  history$pca <- NA
  original_history <- history
  nullable_fields <- c(
    "external_regressors",
    "recipes_to_run",
    "lag_periods",
    "rolling_window_periods",
    "seasonal_period"
  )

  expect_no_warning(
    snapshot <- new_reason_history(history, agent_version = 2)
  )

  expect_identical(
    unname(unlist(
      snapshot$previous_version_results[1, nullable_fields],
      use.names = FALSE
    )),
    rep("NULL", length(nullable_fields))
  )
  expect_true(is.na(snapshot$previous_version_results$model_std_wmape[[1]]))
  expect_true(is.na(snapshot$previous_version_results$pca[[1]]))
  expect_true(all(vapply(
    snapshot$current_run_results[nullable_fields],
    function(value) is.na(value[[1]]),
    logical(1)
  )))
  expect_identical(history, original_history)
})

test_that("previous-version replay sanitizes seasonal periods containing one", {
  invalid_values <- c(
    "1",
    "1---12",
    "12---1",
    "1---3---12",
    "3---1---12",
    "3---12---1"
  )
  invalid_history <- dplyr::bind_rows(lapply(
    seq_along(invalid_values),
    function(index) {
      minimal_reason_history(
        agent_version = 1,
        run_number = index,
        models_to_run = "stlm-arima",
        seasonal_period = invalid_values[[index]]
      )
    }
  ))
  original_history <- invalid_history
  warnings <- character(0)

  snapshot <- withCallingHandlers(
    new_reason_history(invalid_history, agent_version = 2),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  if (length(warnings) > 0) {
    expect_match(warnings[[1]], "invalid seasonal_period", fixed = TRUE)
    expect_match(warnings[[1]], "Using 'NULL'", fixed = TRUE)
    expect_match(warnings[[1]], "1---3---12", fixed = TRUE)
  }
  expect_identical(
    snapshot$previous_version_results$seasonal_period,
    rep("NULL", length(invalid_values))
  )
  expect_identical(invalid_history, original_history)

  valid_values <- c("2", "2---12", "2---6---12")
  valid_history <- dplyr::bind_rows(lapply(
    seq_along(valid_values),
    function(index) {
      minimal_reason_history(
        agent_version = 1,
        run_number = index,
        models_to_run = "stlm-arima",
        seasonal_period = valid_values[[index]]
      )
    }
  ))

  expect_no_warning(
    valid_snapshot <- new_reason_history(valid_history, agent_version = 2)
  )
  expect_identical(
    valid_snapshot$previous_version_results$seasonal_period,
    valid_values
  )
})

test_that("first new-version run replays legacy seasonal defaults safely", {
  invalid_values <- c(
    "1",
    "1---12",
    "12---1",
    "1---3---12",
    "3---1---12",
    "3---12---1"
  )
  case_state <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "system prompt",
    make_pipe_table = function(data) {
      paste0("SEASONAL_PERIOD=", data$seasonal_period[[1]])
    },
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    submit_fcst_run = function(...) {
      arguments <- list(...)
      case_state$submit_count <- case_state$submit_count + 1L
      case_state$inputs <- arguments$inputs
      list(run = case_state$submit_count)
    },
    get_fcst_output = function(...) data.frame(),
    calculate_fcst_metrics = function(...) 0.01,
    log_best_run = function(...) "logged",
    finalize_run = function(completion_reason = "completed", ...) {
      case_state$completion_reason <- completion_reason
      if (identical(completion_reason, "reasoning_exhausted")) {
        stop("Error in finalize_run(). No best run file found for fresh agent version.")
      }
      "finalized"
    },
    .package = "finnts"
  )

  for (seasonal_period in invalid_values) {
    case_state$submit_count <- 0L
    case_state$completion_reason <- NULL
    case_state$inputs <- NULL
    chat <- make_queued_fake_chat("")
    chat_state <- chat
    chat$chat <- local({
      invalid_value <- seasonal_period
      state <- chat_state
      function(prompt, ...) {
        state$call_count <- state$call_count + 1L
        state$prompts <- c(state$prompts, as.character(prompt))
        replay_value <- if (grepl(
          paste0("SEASONAL_PERIOD=", invalid_value),
          prompt,
          fixed = TRUE
        )) {
          invalid_value
        } else {
          "NULL"
        }
        make_agent_response(
          models_to_run = "stlm-arima",
          seasonal_period = replay_value
        )
      }
    })
    agent_info <- make_graceful_abort_agent_info()
    agent_info$llm <- chat
    history <- minimal_reason_history(
      agent_version = 1,
      models_to_run = "stlm-arima",
      seasonal_period = seasonal_period
    )
    warnings <- character(0)

    outcome <- tryCatch(
      withCallingHandlers(
        fcst_agent_workflow(
          agent_info = agent_info,
          combo = "combo-hash",
          weighted_mape_goal = 0.05,
          parallel_processing = NULL,
          inner_parallel = FALSE,
          num_cores = 1,
          max_iter = 1,
          seed = 123,
          previous_run_results = history,
          fallback_available = FALSE
        ),
        warning = function(warning) {
          warnings <<- c(warnings, conditionMessage(warning))
          invokeRestart("muffleWarning")
        }
      ),
      error = identity
    )

    expect_false(inherits(outcome, "error"), info = seasonal_period)
    if (inherits(outcome, "error")) {
      next
    }
    expect_equal(length(warnings), 1L, info = seasonal_period)
    expect_identical(chat$call_count, 1L, info = seasonal_period)
    expect_identical(case_state$submit_count, 1L, info = seasonal_period)
    expect_identical(
      case_state$inputs$seasonal_period,
      "NULL",
      info = seasonal_period
    )
    expect_null(
      null_converter(case_state$inputs$seasonal_period),
      info = seasonal_period
    )
    expect_identical(
      case_state$completion_reason,
      "completed",
      info = seasonal_period
    )
    expect_identical(outcome$node, "stop", info = seasonal_period)
  }
})

test_that("reason prompt keeps prior versions for replay but counts current version only", {
  history <- dplyr::bind_rows(
    minimal_reason_history(
      agent_version = 1,
      run_number = 1,
      models_to_run = "arima",
      weighted_mape = 0.12
    ),
    minimal_reason_history(
      agent_version = 2,
      run_number = 2,
      models_to_run = "ets",
      weighted_mape = 0.2
    )
  )
  snapshot <- new_reason_history(history, agent_version = 2)
  chat <- make_queued_fake_chat(make_agent_response(models_to_run = "glmnet"))
  agent_info <- make_graceful_abort_agent_info()
  agent_info$llm <- chat

  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  reason_inputs(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05,
    previous_run_results = snapshot$current_run_results,
    previous_version_results = snapshot$previous_version_results,
    total_runs = snapshot$total_runs
  )

  expect_match(chat$prompts[[1]], "run count : 1", fixed = TRUE)
  expect_match(chat$prompts[[1]], "CURRENT AGENT VERSION RUN RESULTS", fixed = TRUE)
  expect_match(chat$prompts[[1]], "PREVIOUS AGENT VERSION RESULTS", fixed = TRUE)
  expect_match(
    chat$prompts[[1]],
    "best weighted MAPE from current-version runs",
    fixed = TRUE
  )
  expect_match(
    chat$prompts[[1]],
    "best run number from current-version runs",
    fixed = TRUE
  )
  expect_match(chat$prompts[[1]], "arima", fixed = TRUE)
  expect_match(chat$prompts[[1]], "ets", fixed = TRUE)
})

test_that("reason proposal failures carry typed metadata", {
  invalid <- tryCatch(
    abort_invalid_agent_proposal(
      field = "models_to_run",
      value = "unknown",
      reason = "model is unavailable"
    ),
    error = identity
  )
  exhausted <- tryCatch(
    abort_exhausted_agent_search(
      field = "seasonal_period",
      reason = "change budget exhausted"
    ),
    error = identity
  )

  expect_s3_class(invalid, "finnts_reason_proposal_invalid")
  expect_identical(invalid$field, "models_to_run")
  expect_identical(invalid$proposed_value, "unknown")
  expect_s3_class(exhausted, "finnts_reason_search_exhausted")
  expect_true(is_graceful_reason_failure(invalid))
  expect_true(is_graceful_reason_failure(exhausted))
  expect_false(is_graceful_reason_failure(simpleError("storage failed")))
})

test_that("all LLM-controlled fields are validated before submission", {
  agent_info <- make_graceful_abort_agent_info()
  valid <- validate_agent_proposal(
    jsonlite::fromJSON(make_agent_response()),
    agent_info = agent_info,
    combo = "combo-hash",
    available_models = c("arima", "ets", "stlm-arima", "tbats")
  )

  expect_equal(valid$models_to_run, c("arima", "ets"))
  expect_identical(valid$external_regressors, "NULL")
  expect_identical(valid$clean_missing_values, FALSE)
  expect_identical(valid$forecast_approach, "bottoms_up")

  invalid_cases <- list(
    models_to_run = list(models_to_run = "unknown"),
    external_regressors = list(external_regressors = "Driver_B"),
    recipes_to_run = list(recipes_to_run = "R3"),
    forecast_approach = list(forecast_approach = "unknown"),
    stationary = list(stationary = "maybe"),
    lag_periods = list(lag_periods = "3---Inf"),
    rolling_window_periods = list(rolling_window_periods = "3---text"),
    seasonal_without_model = list(
      models_to_run = "arima---ets",
      seasonal_period = "12---6"
    )
  )

  for (case_name in names(invalid_cases)) {
    response <- do.call(make_agent_response, invalid_cases[[case_name]])
    error <- tryCatch(
      validate_agent_proposal(
        jsonlite::fromJSON(response),
        agent_info = agent_info,
        combo = if (case_name == "forecast_approach") NULL else "combo-hash",
        available_models = c("arima", "ets", "stlm-arima", "tbats")
      ),
      error = identity
    )
    expect_s3_class(error, "finnts_reason_proposal_invalid")
  }

  global_recipe_error <- tryCatch(
    validate_agent_proposal(
      jsonlite::fromJSON(make_agent_response(
        models_to_run = "xgboost",
        recipes_to_run = "R2"
      )),
      agent_info = agent_info,
      combo = NULL,
      available_models = "xgboost"
    ),
    error = identity
  )
  expect_s3_class(global_recipe_error, "finnts_reason_proposal_invalid")
  expect_match(conditionMessage(global_recipe_error), "must prepare R1", fixed = TRUE)
})

test_that("canonical change budgets allow three new settings and reject a fourth", {
  defaults <- c(12, 6, 3)
  prior <- c(NA, "2---6---12", "4---6---12", "6---8---12")

  expect_no_error(validate_agent_setting_change_budget(
    previous_values = prior,
    proposed_value = "12---6---2",
    default_value = defaults,
    field = "seasonal_period"
  ))
  expect_no_error(validate_agent_setting_change_budget(
    previous_values = prior,
    proposed_value = "12---6---3",
    default_value = defaults,
    field = "seasonal_period"
  ))

  error <- tryCatch(
    validate_agent_setting_change_budget(
      previous_values = prior,
      proposed_value = "10---12",
      default_value = defaults,
      field = "seasonal_period"
    ),
    error = identity
  )
  expect_s3_class(error, "finnts_reason_search_exhausted")

  budget_cases <- list(
    lag_periods = c(1, 2, 3, 6, 9, 12),
    rolling_window_periods = c(3, 6, 9, 12)
  )
  for (field in names(budget_cases)) {
    defaults <- budget_cases[[field]]
    prior <- c(
      NA,
      paste(c(2, 4), collapse = "---"),
      paste(c(3, 6), collapse = "---"),
      paste(c(4, 8), collapse = "---")
    )
    expect_no_error(validate_agent_setting_change_budget(
      previous_values = prior,
      proposed_value = paste(rev(c(2, 4)), collapse = "---"),
      default_value = defaults,
      field = field
    ))
    expect_no_error(validate_agent_setting_change_budget(
      previous_values = prior,
      proposed_value = paste(defaults, collapse = "---"),
      default_value = defaults,
      field = field
    ))
    error <- tryCatch(
      validate_agent_setting_change_budget(
        previous_values = prior,
        proposed_value = "10---20",
        default_value = defaults,
        field = field
      ),
      error = identity
    )
    expect_s3_class(error, "finnts_reason_search_exhausted")
    expect_identical(error$field, field)
  }
})

test_that("reason prompt reports the canonical current-version seasonal budget", {
  history <- dplyr::bind_rows(
    minimal_reason_history(
      run_number = 1,
      models_to_run = "stlm-arima",
      seasonal_period = "2---6---12"
    ),
    minimal_reason_history(
      run_number = 2,
      models_to_run = "stlm-ets",
      seasonal_period = "4---6---12"
    ),
    minimal_reason_history(
      run_number = 3,
      models_to_run = "tbats",
      seasonal_period = "6---8---12"
    )
  )
  chat <- make_queued_fake_chat(make_agent_response(
    models_to_run = "xgboost",
    seasonal_period = "NULL"
  ))
  agent_info <- make_graceful_abort_agent_info()
  agent_info$llm <- chat

  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  reason_inputs(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05,
    previous_run_results = history,
    previous_version_results = "No Previous Runs",
    total_runs = 3L
  )

  expect_match(
    chat$prompts[[1]],
    "seasonal_changes_allowed : FALSE",
    fixed = TRUE
  )
})

test_that("reason prompt canonicalizes lag and rolling change budgets", {
  history <- dplyr::bind_rows(
    minimal_reason_history(
      run_number = 1,
      lag_periods = "2---4",
      rolling_window_periods = "2---4"
    ),
    minimal_reason_history(
      run_number = 2,
      lag_periods = "4---2",
      rolling_window_periods = "4---2"
    ),
    minimal_reason_history(
      run_number = 3,
      lag_periods = "3---6",
      rolling_window_periods = "3---6"
    )
  )
  chat <- make_queued_fake_chat(make_agent_response(models_to_run = "glmnet"))
  agent_info <- make_graceful_abort_agent_info()
  agent_info$llm <- chat

  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  reason_inputs(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05,
    previous_run_results = history,
    previous_version_results = "No Previous Runs",
    total_runs = 3L
  )

  expect_match(chat$prompts[[1]], "lag_changes_allowed : TRUE", fixed = TRUE)
  expect_match(chat$prompts[[1]], "rolling_changes_allowed : TRUE", fixed = TRUE)
})

test_that("reason inputs do not cap external regressor configurations", {
  drivers <- paste0("Driver_", LETTERS[1:10])
  history <- dplyr::bind_rows(lapply(seq_along(drivers), function(index) {
    minimal_reason_history(
      run_number = index,
      external_regressors = drivers[[index]]
    )
  }))
  chat <- make_queued_fake_chat(make_agent_response(
    models_to_run = "glmnet",
    external_regressors = "Driver_A---Driver_B",
    feature_selection = "TRUE"
  ))
  agent_info <- make_graceful_abort_agent_info()
  agent_info$external_regressors <- drivers
  agent_info$llm <- chat

  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  expect_no_error(
    reason_inputs(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05,
      previous_run_results = history,
      previous_version_results = "No Previous Runs",
      total_runs = 10L
    )
  )

  expect_false(grepl("external_regressor_changes_allowed", chat$prompts[[1]], fixed = TRUE))
})

test_that("local system prompt describes seasonal budget semantics", {
  agent_info <- make_graceful_abort_agent_info()

  testthat::local_mocked_bindings(
    load_eda_results = function(...) "No EDA needed for this test.",
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  prompt <- iterate_forecast_system_prompt(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05
  )

  expect_match(
    prompt,
    "at most 3 distinct non-default seasonal_period configurations within the current agent version",
    fixed = TRUE
  )
  expect_match(
    prompt,
    "Reusing a previously tested configuration or selecting \"NULL\" does not consume another change",
    fixed = TRUE
  )
})

test_that("system prompts do not cap external regressor configurations", {
  agent_info <- make_graceful_abort_agent_info()

  testthat::local_mocked_bindings(
    load_eda_results = function(...) "No EDA needed for this test.",
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  for (combo in list(NULL, "combo-hash")) {
    prompt <- iterate_forecast_system_prompt(
      agent_info = agent_info,
      combo = combo,
      weighted_mape_goal = 0.05
    )

    expect_match(
      prompt,
      "same set of parameters as a run in the current agent version",
      fixed = TRUE
    )
    expect_match(
      prompt,
      "add ONLY ONE new external regressor variable per run",
      fixed = TRUE
    )
    expect_false(grepl("external_regressors parameter a total of", prompt, fixed = TRUE))
    expect_false(grepl("external_regressor_changes_allowed", prompt, fixed = TRUE))
    expect_false(grepl("lag_chages_allowed", prompt, fixed = TRUE))
  }
})

test_that("malformed JSON retries and explicit abort bypasses submission", {
  agent_info <- make_graceful_abort_agent_info()
  malformed_chat <- make_queued_fake_chat(c(
    "not json",
    make_agent_response(models_to_run = "glmnet")
  ))
  agent_info$llm <- malformed_chat

  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    .package = "finnts"
  )

  result <- execute_node(
    list(fn = "reason_inputs", max_retry = 3, retry_mode = "plain"),
    list(
      args = list(
        agent_info = agent_info,
        combo = "combo-hash",
        weighted_mape_goal = 0.05,
        last_error = NULL,
        previous_run_results = minimal_reason_history(),
        previous_version_results = "No Previous Runs",
        total_runs = 1L
      ),
      results = list(),
      attempts = list()
    ),
    malformed_chat
  )
  expect_true(result$ok)
  expect_identical(malformed_chat$call_count, 2L)
  expect_match(malformed_chat$prompts[[2]], "Invalid proposed response", fixed = TRUE)

  abort_chat <- make_queued_fake_chat('{"abort":"TRUE","reasoning":"no improvement remains"}')
  agent_info$llm <- abort_chat
  abort_result <- reason_inputs(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05,
    previous_run_results = minimal_reason_history(),
    previous_version_results = "No Previous Runs",
    total_runs = 1L
  )
  expect_identical(abort_result$abort, "TRUE")
  expect_identical(abort_result$reasoning, "no improvement remains")
})

test_that("available agent models respect scope and foundation availability", {
  local_without_foundation <- get_available_agent_models("combo-hash", "")
  local_with_foundation <- get_available_agent_models(
    "combo-hash",
    "---chronos2---chronos-bolt-base"
  )
  global_with_foundation <- get_available_agent_models(
    NULL,
    "---chronos2---chronos-bolt-base"
  )

  expect_false("chronos2" %in% local_without_foundation)
  expect_true(all(c("chronos2", "chronos-bolt-base") %in% local_with_foundation))
  expect_true("chronos2" %in% global_with_foundation)
  expect_false("chronos-bolt-base" %in% global_with_foundation)
  expect_true("xgboost" %in% global_with_foundation)
  expect_false("arima" %in% global_with_foundation)
})

test_that("reasoning retries reuse supplied history without storage reads", {
  agent_info <- make_graceful_abort_agent_info()
  chat <- make_queued_fake_chat(c(
    make_agent_response(seasonal_period = "1"),
    make_agent_response(seasonal_period = "12---6", models_to_run = "stlm-arima")
  ))
  agent_info$llm <- chat
  history <- minimal_reason_history()
  list_calls <- 0L

  testthat::local_mocked_bindings(
    load_run_results = function(...) {
      list_calls <<- list_calls + 1L
      stop("history must come from the supplied snapshot")
    },
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    .package = "finnts"
  )

  context <- list(
    args = list(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05,
      last_error = NULL,
      previous_run_results = history,
      total_runs = 1L
    ),
    results = list(),
    attempts = list()
  )
  result <- execute_node(
    list(fn = "reason_inputs", max_retry = 3, retry_mode = "plain"),
    context,
    chat
  )

  expect_true(result$ok)
  expect_identical(chat$call_count, 2L)
  expect_identical(list_calls, 0L)
  expect_equal(result$ctx$results$reason_inputs$seasonal_period, c(12, 6))
})

test_that("system failures remain hard and never become graceful", {
  error <- simpleError("provider unavailable")
  expect_false(is_graceful_reason_failure(error))

  agent_info <- make_graceful_abort_agent_info()
  chat <- make_queued_fake_chat(list(error))
  agent_info$llm <- chat

  testthat::local_mocked_bindings(
    wait_before_retry = function(...) invisible(NULL),
    .package = "finnts"
  )

  expect_error(
    execute_node(
      list(fn = "reason_inputs", max_retry = 0, retry_mode = "plain"),
      list(
        args = list(
          agent_info = agent_info,
          combo = "combo-hash",
          weighted_mape_goal = 0.05,
          last_error = NULL,
          previous_run_results = minimal_reason_history(),
          total_runs = 1L
        ),
        results = list(),
        attempts = list()
      ),
      chat
    ),
    "Tool 'reason_inputs' failed after 1 attempt.*provider unavailable"
  )
})

test_that("execute_node remains storage-free across exhausted proposals", {
  if (!exists("wait_before_retry", envir = asNamespace("finnts"), inherits = FALSE)) {
    fail("wait_before_retry() is required so deterministic exhaustion tests do not sleep")
    return()
  }

  agent_info <- make_graceful_abort_agent_info()
  chat <- make_queued_fake_chat(rep(
    make_agent_response(seasonal_period = "1"),
    4
  ))
  agent_info$llm <- chat
  storage_calls <- 0L

  testthat::local_mocked_bindings(
    load_run_results = function(...) {
      storage_calls <<- storage_calls + 1L
      stop("unexpected history read")
    },
    list_files = function(...) {
      storage_calls <<- storage_calls + 1L
      stop("execute_node must not list storage")
    },
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    .package = "finnts"
  )

  result <- execute_node(
    list(fn = "reason_inputs", max_retry = 3, retry_mode = "plain"),
    list(
      args = list(
        agent_info = agent_info,
        combo = "combo-hash",
        weighted_mape_goal = 0.05,
        last_error = NULL,
        previous_run_results = minimal_reason_history(),
        total_runs = 1L
      ),
      results = list(),
      attempts = list()
    ),
    chat
  )

  expect_true(result$ok)
  expect_identical(chat$call_count, 4L)
  expect_identical(storage_calls, 0L)
  expect_identical(result$ctx$results$reason_inputs$abort, "TRUE")
})

test_that("history refreshes once after completion and never between corrections", {
  chat <- make_queued_fake_chat(c(
    make_agent_response(seasonal_period = "1"),
    make_agent_response(models_to_run = "arima"),
    make_agent_response(models_to_run = "ets")
  ))
  agent_info <- make_graceful_abort_agent_info()
  agent_info$llm <- chat
  refresh_count <- 0L
  submit_count <- 0L

  testthat::local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "system prompt",
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    load_reason_history = function(...) {
      refresh_count <<- refresh_count + 1L
      list(previous_run_results = minimal_reason_history(
        run_number = refresh_count + 1,
        best_run = "yes",
        models_to_run = "arima"
      ), total_runs = refresh_count + 1L)
    },
    submit_fcst_run = function(...) {
      submit_count <<- submit_count + 1L
      list(run = submit_count)
    },
    get_fcst_output = function(...) data.frame(),
    calculate_fcst_metrics = function(...) 0.5,
    log_best_run = function(...) "logged",
    finalize_run = function(...) "finalized",
    .package = "finnts"
  )

  result <- fcst_agent_workflow(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    inner_parallel = FALSE,
    num_cores = 1,
    max_iter = 2,
    seed = 123,
    previous_run_results = "No Previous Runs",
    fallback_available = FALSE
  )

  expect_identical(submit_count, 2L)
  expect_identical(refresh_count, 1L)
  expect_identical(chat$call_count, 3L)
  expect_match(chat$prompts[[3]], "arima", fixed = TRUE)
  expect_match(chat$prompts[[3]], "run count : 2", fixed = TRUE)
  expect_identical(result$node, "stop")
})

test_that("global and local graphs route exhausted fake responses to finalization", {
  cases <- list(
    global = list(combo = NULL, fallback = TRUE),
    local = list(combo = "combo-hash", fallback = FALSE)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    chat <- make_queued_fake_chat(rep(
      make_agent_response(models_to_run = "unknown"),
      4
    ))
    agent_info <- make_graceful_abort_agent_info()
    agent_info$llm <- chat
    calls <- new.env(parent = emptyenv())
    calls$submit <- 0L
    calls$finalize <- list()

    testthat::local_mocked_bindings(
      new_llm_session = function(llm) llm,
      iterate_forecast_system_prompt = function(...) "system prompt",
      get_foundation_model_suffix = function() "",
      wait_before_retry = function(...) invisible(NULL),
      submit_fcst_run = function(...) {
        calls$submit <- calls$submit + 1L
        stop("invalid proposals must not be submitted")
      },
      finalize_run = function(...) {
        calls$finalize <- list(...)
        "finalized"
      },
      .package = "finnts"
    )

    result <- fcst_agent_workflow(
      agent_info = agent_info,
      combo = case$combo,
      weighted_mape_goal = 0.05,
      parallel_processing = NULL,
      inner_parallel = FALSE,
      num_cores = 1,
      max_iter = 2,
      seed = 123,
      previous_run_results = minimal_reason_history(),
      fallback_available = case$fallback
    )

    expect_identical(result$node, "stop")
    expect_identical(chat$call_count, 4L)
    expect_identical(calls$submit, 0L)
    expect_identical(calls$finalize$completion_reason, "reasoning_exhausted")
    expect_identical(calls$finalize$fallback_available, case$fallback)
  }
})

run_mocked_iterate_mode <- function(run_global_models,
                                    run_local_models,
                                    best_run_tbl) {
  calls <- new.env(parent = emptyenv())
  calls$scopes <- character(0)
  calls$list_count <- 0L
  agent_info <- make_graceful_abort_agent_info(
    run_global_models = run_global_models,
    run_local_models = run_local_models
  )
  agent_info$llm <- structure(list(), class = "Chat")

  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_eda_data = function(...) data.frame(done = TRUE),
    list_files = function(...) {
      calls$list_count <- calls$list_count + 1L
      "input.csv"
    },
    read_file = function(...) data.frame(Combo = c("A", "B")),
    load_run_results = function(...) "No Previous Runs",
    load_best_agent_run = function(...) best_run_tbl,
    par_start = function(...) list(
      cl = NULL,
      packages = character(0),
      foreach_operator = foreach::`%do%`
    ),
    par_end = function(...) invisible(NULL),
    fcst_agent_workflow = function(agent_info, combo, ...) {
      calls$scopes <- c(calls$scopes, if (is.null(combo)) "global" else paste0("local:", combo))
      list(status = "completed")
    },
    save_best_agent_run = function(...) invisible(NULL),
    save_agent_forecast = function(...) invisible(NULL),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )

  iterate_forecast(
    agent_info = agent_info,
    max_iter = 1,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )
  calls
}

test_that("iterate routing covers global-only local-only and combined modes", {
  global_best <- data.frame(
    combo = c("A", "B"),
    model_type = "global",
    weighted_mape = c(0.1, 0.1),
    run_complete = FALSE,
    max_iterations = 0
  )
  local_best <- transform(global_best,
    model_type = "local",
    weighted_mape = c(0.2, 0.2)
  )
  combined_best <- transform(global_best,
    weighted_mape = c(0.01, 0.2)
  )

  global_only <- run_mocked_iterate_mode(TRUE, FALSE, global_best)
  local_only <- run_mocked_iterate_mode(FALSE, TRUE, local_best)
  combined <- run_mocked_iterate_mode(TRUE, TRUE, combined_best)
  combined_fallback <- run_mocked_iterate_mode(
    TRUE,
    TRUE,
    data.frame()
  )

  expect_identical(global_only$scopes, "global")
  expect_setequal(local_only$scopes, c(
    paste0("local:", hash_data("A")),
    paste0("local:", hash_data("B"))
  ))
  expect_identical(combined$scopes[[1]], "global")
  expect_true(paste0("local:", hash_data("B")) %in% combined$scopes)
  expect_false(paste0("local:", hash_data("A")) %in% combined$scopes)
  expect_identical(combined_fallback$scopes[[1]], "global")
  expect_setequal(
    combined_fallback$scopes[-1],
    c(
      paste0("local:", hash_data("A")),
      paste0("local:", hash_data("B"))
    )
  )
})

test_that("combined mode exhausts the global graph before real local graph fallback", {
  agent_info <- make_graceful_abort_agent_info(
    run_global_models = TRUE,
    run_local_models = TRUE
  )
  agent_info$llm <- structure(list(), class = "Chat")
  calls <- new.env(parent = emptyenv())
  calls$sessions <- list()
  calls$submit <- 0L
  calls$finalize <- list()

  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_eda_data = function(...) data.frame(done = TRUE),
    list_files = function(...) "input.csv",
    read_file = function(...) data.frame(Combo = c("A", "B")),
    load_run_results = function(...) "No Previous Runs",
    load_best_agent_run = function(...) data.frame(),
    par_start = function(...) list(
      cl = NULL,
      packages = character(0),
      foreach_operator = foreach::`%do%`
    ),
    par_end = function(...) invisible(NULL),
    new_llm_session = function(...) {
      session <- make_queued_fake_chat(rep(
        make_agent_response(models_to_run = "unknown"),
        4
      ))
      calls$sessions[[length(calls$sessions) + 1L]] <- session
      session
    },
    iterate_forecast_system_prompt = function(...) "system prompt",
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    submit_fcst_run = function(...) {
      calls$submit <- calls$submit + 1L
      stop("invalid proposals must not be submitted")
    },
    finalize_run = function(combo = NULL, fallback_available = FALSE, ...) {
      calls$finalize[[length(calls$finalize) + 1L]] <- list(
        combo = combo,
        fallback_available = fallback_available
      )
      if (is.null(combo)) {
        return(list(status = "skipped", continue_to_local = TRUE))
      }
      "finalized"
    },
    save_best_agent_run = function(...) invisible(NULL),
    save_agent_forecast = function(...) invisible(NULL),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )

  iterate_forecast(
    agent_info = agent_info,
    max_iter = 1,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(length(calls$sessions), 3L)
  expect_true(all(vapply(calls$sessions, function(chat) chat$call_count == 4L, logical(1))))
  expect_identical(calls$submit, 0L)
  expect_identical(length(calls$finalize), 3L)
  expect_null(calls$finalize[[1]]$combo)
  expect_true(calls$finalize[[1]]$fallback_available)
  expect_setequal(
    vapply(calls$finalize[-1], function(call) call$combo, character(1)),
    c(hash_data("A"), hash_data("B"))
  )
  expect_false(any(vapply(calls$finalize[-1], function(call) call$fallback_available, logical(1))))
})

test_that("one hundred exhausted local nodes do not list storage", {
  if (!exists("wait_before_retry", envir = asNamespace("finnts"), inherits = FALSE)) {
    fail("wait_before_retry() is required so the stress test stays deterministic")
    return()
  }

  storage_calls <- 0L
  testthat::local_mocked_bindings(
    load_run_results = function(...) {
      storage_calls <<- storage_calls + 1L
      stop("unexpected history read")
    },
    list_files = function(...) {
      storage_calls <<- storage_calls + 1L
      stop("unexpected wildcard listing")
    },
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    .package = "finnts"
  )

  for (index in seq_len(100)) {
    agent_info <- make_graceful_abort_agent_info()
    chat <- make_queued_fake_chat(make_agent_response(
      models_to_run = paste0("unknown-", index)
    ))
    agent_info$llm <- chat
    node <- list(
      fn = "reason_inputs",
      max_retry = 0,
      retry_mode = "plain"
    )
    result <- execute_node(
      node,
      list(
        args = list(
          agent_info = agent_info,
          combo = paste0("combo-", index),
          weighted_mape_goal = 0.05,
          last_error = NULL,
          previous_run_results = minimal_reason_history(),
          total_runs = 1L
        ),
        results = list(),
        attempts = list()
      ),
      chat
    )
    expect_true(result$ok)
  }

  expect_identical(storage_calls, 0L)
})
