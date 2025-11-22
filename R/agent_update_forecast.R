#' Update Forecast with Latest Data and Inputs
#'
#' This function updates the forecast agent with the latest data and inputs.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param weighted_mape_goal Weighted MAPE goal the agent is trying to achieve for each time series
#' @param allow_iterate_forecast Logical indicating if the forecast iteration
#'   should be allowed if poor performance is detected, meaning >40% of
#'   time series with >20% worse weighted MAPE than previous agent run
#' @param max_iter Numeric indicating the maximum number of iterations
#'   if iterate_forecast is ran
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   forecasts each individual time series one after another. 'local_machine'
#'   leverages all cores on current machine Finn is running on. 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks or
#'   Azure Synapse.
#' @param inner_parallel Run components of forecast process inside a specific
#'   time series in parallel. Can only be used if parallel_processing is
#'   set to NULL or 'spark'.
#' @param num_cores Number of cores to run when parallel processing is set up.
#'   Used when running parallel computations on local machine or within Azure.
#'   Default of NULL uses total amount of cores on machine minus one. Can't be
#'   greater than number of cores on machine minus 1.
#' @param seed Set seed for random number generator. Numeric value.
#'
#' @return Nothing
#' @examples
#' \dontrun{
#' # load example data
#' hist_data <- timetk::m4_monthly %>%
#'   dplyr::filter(date >= "2013-01-01") %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id))
#'
#' # set up Finn project
#' project <- set_project_info(
#'   project_name = "Demo_Project",
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month"
#' )
#'
#' # set up LLM
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6,
#'   hist_end_date = as.Date("2014-12-01")
#' )
#'
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#' )
#'
#' # update the forecast with latest data and inputs
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6,
#'   hist_end_date = as.Date("2014-12-01"),
#'   overwrite = TRUE # required to update the agent for latest data and inputs
#' )
#'
#' update_forecast(
#'   agent_info = agent_info,
#'   weighted_mape_goal = 0.03
#' )
#' }
#' @export
update_forecast <- function(agent_info,
                            weighted_mape_goal = 0.1,
                            allow_iterate_forecast = FALSE,
                            max_iter = 3,
                            parallel_processing = NULL,
                            inner_parallel = FALSE,
                            num_cores = NULL,
                            seed = 123) {
  message("[agent] Starting Forecast Update Process")

  # formatting checks
  check_agent_info(agent_info)

  check_parallel_processing(
    run_info = agent_info$project_info,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel
  )

  check_input_type("max_iter", max_iter, "numeric")
  check_input_type("weighted_mape_goal", weighted_mape_goal, "numeric")

  # agent info adjustments
  if (agent_info$forecast_approach != "bottoms_up") {
    agent_info$project_info$combo_variables <- "ID"
  }

  # run the workflow
  results <- update_fcst_agent_workflow(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    max_iter = max_iter,
    allow_iterate_forecast = allow_iterate_forecast,
    weighted_mape_goal = weighted_mape_goal,
    seed = seed
  )

  message("[agent] Forecast Update Process Complete")
}

#' Update Forecast Agent Workflow
#'
#' This function defines the workflow for updating the forecast agent.
#'
#' @param agent_info A list containing the agent information.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Numeric indicating the number of cores to use for parallel processing.
#' @param max_iter Numeric indicating the maximum number of iterations for the workflow.
#' @param allow_iterate_forecast Logical indicating if the forecast iteration should be allowed.
#' @param weighted_mape_goal Numeric indicating the goal for the weighted MAPE.
#' @param seed Numeric seed for reproducibility.
#'
# @return A list containing the results of the workflow.
#' @noRd
update_fcst_agent_workflow <- function(agent_info,
                                       parallel_processing,
                                       inner_parallel,
                                       num_cores,
                                       max_iter = 3,
                                       allow_iterate_forecast = TRUE,
                                       weighted_mape_goal = 0.1,
                                       seed = 123) {
  # create a fresh session for the reasoning LLM
  if (!is.null(agent_info$reason_llm)) {
    agent_info$reason_llm <- agent_info$reason_llm$clone()
  }

  # construct the workflow
  workflow <- list(
    start = list(
      fn = "initial_checks",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info
      ),
      branch = function(ctx) {
        # extract the results from the current node
        results <- ctx$results$initial_checks
        forecast_approach <- ctx$agent_info$forecast_approach

        # check if initial checks passed
        if (is.data.frame(results)) {
          return(list(ctx = ctx, `next` = "update_global_models"))
        } else if (results == "no updates required" & forecast_approach == "bottoms_up") {
          # if no updates required, stop the workflow
          cli::cli_alert_info("No model updates required, stopping workflow.")
          return(list(ctx = ctx, `next` = "stop"))
        } else if (results == "no updates required" & forecast_approach != "bottoms_up") {
          # if no updates required, stop the workflow
          cli::cli_alert_info("No model updates required, moving to reconcile forecast.")
          return(list(ctx = ctx, `next` = "reconcile_agent_forecast"))
        } else {
          stop("Error in initial checks.", call. = FALSE)
        }
      }
    ),
    update_global_models = list(
      fn = "update_global_models",
      `next` = "update_local_models",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        previous_best_run_tbl = "{results$initial_checks}",
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores,
        seed = seed
      )
    ),
    update_local_models = list(
      fn = "update_local_models",
      `next` = "save_best_agent_run",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        previous_best_run_tbl = "{results$initial_checks}",
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores,
        seed = seed
      )
    ),
    save_best_agent_run = list(
      fn = "save_best_agent_run",
      `next` = "analyze_results",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info
      )
    ),
    analyze_results = list(
      fn = "analyze_results",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info
      ),
      branch = function(ctx) {
        # extract the results from the current node
        results <- ctx$results
        perc_worse <- ctx$results$analyze_results
        forecast_approach <- ctx$agent_info$forecast_approach

        # check if forecast iteration should be ran again
        if (perc_worse >= 40 & allow_iterate_forecast) {
          cli::cli_alert_info("Poor performance detected: Running iterate_forecast() to improve results.")
          return(list(ctx = ctx, `next` = "iterate_forecast"))
        } else if (forecast_approach != "bottoms_up") {
          return(list(ctx = ctx, `next` = "reconcile_agent_forecast"))
        } else {
          return(list(ctx = ctx, `next` = "save_agent_forecast"))
        }
      }
    ),
    iterate_forecast = list(
      fn = "iterate_forecast",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        weighted_mape_goal = weighted_mape_goal,
        max_iter = max_iter,
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores,
        seed = seed
      ),
      branch = function(ctx) {
        # get forecast approach and determine next step
        forecast_approach <- ctx$agent_info$forecast_approach

        if (forecast_approach == "bottoms_up") {
          return(list(ctx = ctx, `next` = "save_agent_forecast"))
        } else {
          return(list(ctx = ctx, `next` = "reconcile_agent_forecast"))
        }
      }
    ),
    reconcile_agent_forecast = list(
      fn = "reconcile_agent_forecast",
      `next` = "save_hierarchy_summary",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        project_info = agent_info$project_info,
        parallel_processing = parallel_processing,
        num_cores = num_cores
      )
    ),
    save_hierarchy_summary = list(
      fn = "summarize_hierarchy",
      `next` = "save_agent_forecast",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info
      )
    ),
    save_agent_forecast = list(
      fn = "save_agent_forecast",
      `next` = "save_model_summaries",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        parallel_processing = parallel_processing,
        num_cores = num_cores
      )
    ),
    save_model_summaries = list(
      fn = "summarize_models",
      `next` = "eda_agent_workflow",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        parallel_processing = parallel_processing,
        num_cores = num_cores
      )
    ),
    eda_agent_workflow = list(
      fn = "eda_agent_workflow",
      `next` = "stop",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        parallel_processing = parallel_processing,
        num_cores = num_cores
      )
    ),
    stop = list(fn = NULL)
  )

  init_ctx <- list(
    node = "start",
    iter = 0, # iteration counter
    max_iter = max_iter, # loop limit
    results = list(), # where each tool's output will be stored
    attempts = list(), # retry bookkeeping for execute_node()
    agent_info = agent_info # agent information
  )

  # run the graph
  run_graph(agent_info$driver_llm, workflow, init_ctx)
}

#' Initial Checks for Update Forecast
#'
#' This function performs initial checks on the agent information and prepares the necessary data for the update process.
#'
#' @param agent_info A list containing the agent information.
#'
#' @return A data frame containing the previous best run information or a message indicating no updates are required.
#' @noRd
initial_checks <- function(agent_info) {
  # get metadata
  project_info <- agent_info$project_info

  # formatting checks
  check_agent_info(agent_info = agent_info)

  if (!agent_info$overwrite) {
    stop("Error in agent_info(). Set overwrite = TRUE to update the agent with latest data and inputs.",
      call. = FALSE
    )
  }

  # check if forecast update already run for current agent version
  current_agent_run_tbl <- load_best_agent_run(agent_info)

  if (nrow(current_agent_run_tbl) > 0) {
    finished_combos <- current_agent_run_tbl %>%
      dplyr::rowwise() %>%
      dplyr::mutate(combo_hash = hash_data(combo)) %>%
      dplyr::pull(combo_hash) %>%
      unique()

    hash_combo_list <- get_total_combos(agent_info)

    unfinished_combos <- setdiff(hash_combo_list, finished_combos)

    if (length(unfinished_combos) == 0) {
      # stop workflow if no updates required
      return("no updates required")
    }
  } else {
    unfinished_combos <- NULL
  }

  # load all agent runs
  agent_runs_list <- list_files(
    project_info$storage_object,
    paste0(
      agent_info$project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      "*agent_run.csv"
    )
  )

  if (length(agent_runs_list) <= 1) {
    stop("Error in update_forecast(). No previous agent runs found.",
      call. = FALSE
    )
  }

  # load previous agent run results (excluding current run)
  prev_agent_run_tbl <- read_file(
    run_info = agent_info$project_info,
    file_list = agent_runs_list,
    return_type = "df"
  ) %>%
    dplyr::arrange(dplyr::desc(agent_version)) %>%
    dplyr::filter(agent_version < agent_info$agent_version)

  # loop through each agent version, until finding the last completed run with real results
  for (version in prev_agent_run_tbl$agent_version) {
    # create version specific agent info
    temp_agent_tbl <- prev_agent_run_tbl %>%
      dplyr::filter(agent_version == version)

    temp_agent_info <- list(
      agent_version = temp_agent_tbl$agent_version,
      project_info = agent_info$project_info,
      run_id = temp_agent_tbl$run_id[1],
      storage_object = project_info$storage_object,
      path = project_info$path,
      forecast_approach = temp_agent_tbl$forecast_approach,
      driver_llm = agent_info$driver_llm,
      reason_llm = agent_info$reason_llm,
      forecast_horizon = temp_agent_tbl$forecast_horizon,
      external_regressors = temp_agent_tbl$external_regressors,
      hist_end_date = temp_agent_tbl$hist_end_date,
      back_test_scenarios = temp_agent_tbl$back_test_scenarios,
      back_test_spacing = temp_agent_tbl$back_test_spacing,
      combo_cleanup_date = temp_agent_tbl$combo_cleanup_date,
      overwrite = agent_info$overwrite
    )

    # get best run results for version
    temp_run_results <- load_best_agent_run(agent_info = temp_agent_info)

    # get number of time series from previous agent run
    temp_combo_list <- get_total_combos(agent_info = temp_agent_info)

    if (nrow(temp_run_results) > 0) {
      if (length(unique(temp_run_results$combo)) == length(temp_combo_list)) {
        prev_agent_info <- temp_agent_info
        prev_best_run_results <- temp_run_results
        break # exit loop if found a previous agent run with real results
      } else {
        next # continue to next version
      }
    }
  }

  # get time series from previous agent run
  prev_run_combos <- get_total_combos(agent_info = prev_agent_info)

  # get number of time series from current agent run
  current_run_combos <- get_total_combos(agent_info = agent_info)

  # check if number of time series has changed
  if (length(prev_run_combos) < length(current_run_combos)) {
    stop("Error in update_forecast(). The number of time series has grown since last complted agent run, please remove new time series.",
      call. = FALSE
    )
  } else {
    new_combos <- setdiff(current_run_combos, prev_run_combos)

    if (length(new_combos) > 0) {
      stop("Error in update_forecast(). The following time series have been added since last completed agent run: ",
        paste(new_combos, collapse = ", "), ". Please remove time series.",
        call. = FALSE
      )
    }
  }

  # get best runs from previous agent run and filter on combos that haven't been updated for this version
  prev_best_runs_tbl <- get_best_agent_run(agent_info = prev_agent_info)

  if (!is.null(unfinished_combos)) {
    prev_best_runs_tbl <- prev_best_runs_tbl %>%
      dplyr::rowwise() %>%
      dplyr::mutate(combo_hash = hash_data(combo)) %>%
      dplyr::filter(combo_hash %in% unfinished_combos) %>%
      dplyr::select(-combo_hash) %>%
      dplyr::ungroup()
  }

  if (nrow(prev_best_runs_tbl) == 0) {
    stop("Error in update_forecast(). No best runs found in previous agent run.",
      call. = FALSE
    )
  }

  return(prev_best_runs_tbl)
}

#' Update Global Models
#'
#' This function updates the global models based on the previous best runs.
#'
#' @param agent_info A list containing the agent information.
#' @param previous_best_run_tbl A data frame containing the previous best run information.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Numeric indicating the number of cores to use for parallel processing.
#' @param seed Numeric seed for reproducibility.
#'
# @return A character string indicating the completion of the update process.
#' @noRd
update_global_models <- function(agent_info,
                                 previous_best_run_tbl,
                                 parallel_processing,
                                 inner_parallel,
                                 num_cores,
                                 seed) {
  # get metadata
  project_info <- agent_info$project_info

  # check if global models are required to run
  previous_best_run_global_tbl <- previous_best_run_tbl %>%
    dplyr::filter(model_type == "global")

  if (nrow(previous_best_run_global_tbl) == 0) {
    cli::cli_alert_info("No global models to update, skipping...")
    return("No global models to update, skipping...")
  }

  # start forecast update process
  if (identical(parallel_processing, "spark") & identical(inner_parallel, TRUE)) {
    results <- callr::r(
      function(agent_info, previous_best_run_global_tbl, parallel_processing, inner_parallel, num_cores, seed, libs) {
        .libPaths(libs)
        # make sure the child has no leftover backend
        try(doParallel::stopImplicitCluster(), silent = TRUE)
        foreach::registerDoSEQ()

        # ensure namespace is fully initialized
        ns <- loadNamespace("finnts", lib.loc = libs)
        attachNamespace(ns) # similar effect to library()

        # grab function from namespace
        fn <- getFromNamespace("update_forecast_combo", "finnts")

        # call function
        fn(
          agent_info = agent_info,
          prev_best_run_tbl = previous_best_run_global_tbl,
          parallel_processing = parallel_processing,
          num_cores = num_cores,
          inner_parallel = inner_parallel,
          seed = seed
        )
      },
      args = list(agent_info, previous_best_run_global_tbl, parallel_processing, inner_parallel, num_cores, seed, .libPaths()),
      libpath = .libPaths(),
      stdout = "|", stderr = "|", show = TRUE, supervise = TRUE
    )
  } else {
    results <- update_forecast_combo(
      agent_info = agent_info,
      prev_best_run_tbl = previous_best_run_global_tbl,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      inner_parallel = inner_parallel,
      seed = seed
    )
  }

  # clean up any clusters
  try(doParallel::stopImplicitCluster(), silent = TRUE)
  try(foreach::registerDoSEQ(), silent = TRUE)

  return("Finished Global Model Update")
}

#' Update Local Models
#'
#' This function updates the local models based on the previous best runs.
#'
#' @param agent_info A list containing the agent information.
#' @param previous_best_run_tbl A data frame containing the previous best run information.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Numeric indicating the number of cores to use for parallel processing.
#' @param seed Numeric seed for reproducibility.
#'
#' @return Nothing
#' @noRd
update_local_models <- function(agent_info,
                                previous_best_run_tbl,
                                parallel_processing,
                                inner_parallel,
                                num_cores,
                                seed) {
  # get metadata
  project_info <- agent_info$project_info

  # check if local models are required to run
  previous_best_run_local_tbl <- previous_best_run_tbl %>%
    dplyr::filter(model_type == "local")

  if (nrow(previous_best_run_local_tbl) == 0) {
    cli::cli_alert_info("No local models to update, skipping...")
    return("No local models to update, skipping...")
  }

  prev_run_id <- unique(previous_best_run_local_tbl$agent_run_id)[[1]]

  # check if forecast update already ran for current agent version
  current_best_run_local_tbl <- load_best_agent_run(agent_info)

  if (nrow(current_best_run_local_tbl) > 0) {
    current_best_run_local_tbl <- current_best_run_local_tbl %>%
      dplyr::filter(model_type == "local")
  }

  if (nrow(current_best_run_local_tbl) > 0) {
    finished_combos <- unique(current_best_run_local_tbl$combo)

    total_combos <- unique(previous_best_run_local_tbl$combo)

    previous_best_run_local_tbl <- previous_best_run_local_tbl %>%
      dplyr::filter(combo %in% setdiff(total_combos, finished_combos))

    if (nrow(previous_best_run_local_tbl) == 0) {
      # stop if no updates required
      return("no updates required")
    }
  } else {
    # do nothing
  }

  # final list to iterate through
  local_combo_list <- unique(previous_best_run_local_tbl$combo)

  # agent adjustments to prevent serialization issues
  agent_info_lean <- agent_info
  agent_info_lean$driver_llm <- NULL
  agent_info_lean$reason_llm <- NULL

  # start forecast update process
  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(local_combo_list)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  on.exit(par_end(cl), add = TRUE)

  combo_tbl <- tryCatch(
    {
      foreach::foreach(
        combo = local_combo_list,
        .packages = packages,
        .errorhandling = "stop",
        .inorder = FALSE,
        .multicombine = TRUE
      ) %op%
        {
          # get the previous best run for combo
          prev_run <- read_file(agent_info_lean$project_info,
            file_list = paste0(
              agent_info_lean$project_info$path, "/logs/",
              hash_data(agent_info_lean$project_info$project_name), "-",
              hash_data(prev_run_id), "-",
              hash_data(combo), "-agent_best_run.csv"
            ) %>%
              fs::path_tidy()
          )

          # run update forecast for combo
          results <- tryCatch(
            {
              update_forecast_combo(
                agent_info = agent_info_lean,
                prev_best_run_tbl = prev_run,
                parallel_processing = NULL,
                num_cores = num_cores,
                inner_parallel = inner_parallel,
                seed = seed
              )
            },
            error = function(e) {
              stop(sprintf(
                "Combo '%s' failed. Error Message: %s.",
                combo, e
              ))
            }
          )

          return(data.frame(Combo = hash_data(combo)))
        } %>%
        base::suppressPackageStartupMessages()
    },
    error = function(e) {
      # hard abort: cancel Spark jobs + stop PSOCK workers immediately
      cancel_parallel(par_info)

      # still propagate the error so execute_node() can trigger failover/retry
      stop(e)
    }
  )

  return("Finished Local Model Update")
}

#' Analyze Results of Agent Run
#'
#' This function analyzes the results of the agent run by comparing the latest best runs with previous best runs.
#'
#' @param agent_info A list containing the agent information.
#'
#' @return A numeric value representing the percentage of time series that had poor performance.
#' @noRd
analyze_results <- function(agent_info) {
  # get metadata
  project_info <- agent_info$project_info

  # load all agent runs
  agent_runs_list <- list_files(
    project_info$storage_object,
    paste0(
      agent_info$project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      "*agent_run.csv"
    )
  )

  if (length(agent_runs_list) <= 1) {
    stop("Error in update_forecast(). No previous agent runs found.",
      call. = FALSE
    )
  }

  # calculate total time series that should have ran
  total_combos <- get_total_combos(agent_info) %>%
    length()

  # load previous agent run results (excluding current run)
  prev_agent_run_tbl <- read_file(
    run_info = agent_info$project_info,
    file_list = agent_runs_list,
    return_type = "df"
  ) %>%
    dplyr::arrange(dplyr::desc(agent_version)) %>%
    dplyr::filter(agent_version < agent_info$agent_version)

  # get previous 3 completed agent runs
  counter <- 0
  previous_best_run_tbl <- tibble::tibble()
  for (version in prev_agent_run_tbl$agent_version) {
    if (counter >= 3) {
      break
    }
    # create version specific agent info
    temp_agent_tbl <- prev_agent_run_tbl %>%
      dplyr::filter(agent_version == version)

    temp_agent_info <- list(
      project_info = agent_info$project_info,
      run_id = temp_agent_tbl$run_id[1],
      storage_object = project_info$storage_object,
      path = project_info$path
    )

    # get best run results for version
    temp_run_results <- load_best_agent_run(agent_info = temp_agent_info)

    if (nrow(temp_run_results) > 0) {
      if (nrow(temp_run_results) == total_combos) { # ensure version finished successfully
        previous_best_run_tbl <- plyr::rbind.fill(previous_best_run_tbl, temp_run_results)
        counter <- counter + 1
      }
    }
  }

  # calc average weighted mape of previous best runs
  previous_best_run_tbl <- previous_best_run_tbl %>%
    dplyr::group_by(combo) %>%
    dplyr::summarise(weighted_mape = mean(weighted_mape, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # get latest best run tbl
  latest_best_runs_tbl <- get_best_agent_run(agent_info = agent_info)

  # formatting checks
  if (nrow(latest_best_runs_tbl) != nrow(previous_best_run_tbl)) {
    stop("Error in update_forecast(). The number of best runs has changed since last completed agent run, please check the results.",
      call. = FALSE
    )
  }

  # join with previous best run tbl
  best_run_compare_tbl <- latest_best_runs_tbl %>%
    dplyr::select(combo, weighted_mape) %>%
    dplyr::rename(latest_weighted_mape = weighted_mape) %>%
    dplyr::left_join(
      previous_best_run_tbl %>%
        dplyr::select(combo, weighted_mape) %>%
        dplyr::rename(previous_weighted_mape = weighted_mape),
      by = "combo"
    ) %>%
    dplyr::mutate(
      improvement_pct = ((previous_weighted_mape - latest_weighted_mape) / previous_weighted_mape) * 100
    ) %>%
    dplyr::mutate(performance_flag = ifelse(improvement_pct < -20, TRUE, FALSE))

  # calc percentage of time series that had poor performance
  poor_performance_pct <- best_run_compare_tbl %>%
    dplyr::filter(performance_flag == TRUE) %>%
    nrow() / nrow(best_run_compare_tbl) * 100

  return(poor_performance_pct)
}

#' Reconcile Agent Forecast
#'
#' This function reconciles the agent forecast based on the best model runs and prepares the final forecast table.
#'
#' @param agent_info A list containing the agent information.
#' @param project_info A list containing the project information.
#' @param parallel_processing A character value indicating the type of parallel processing to use.
#' @param num_cores Numeric indicating the number of cores to use for parallel processing.
#'
#' @return Nothing
#' @noRd
reconcile_agent_forecast <- function(agent_info,
                                     project_info,
                                     parallel_processing = NULL,
                                     num_cores = NULL) {
  # formatting checks
  check_agent_info(agent_info = agent_info)
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))

  # load best run settings
  run_inputs <- get_best_agent_run(
    agent_info = agent_info
  )

  if (TRUE %in% unique(run_inputs$negative_forecast)) {
    negative_forecast <- TRUE
  } else {
    negative_forecast <- FALSE
  }

  # load example train test info
  single_run <- run_inputs %>%
    dplyr::slice(1)

  if (single_run$model_type == "global") {
    single_combo <- "all"
  } else {
    single_combo <- single_run$combo
  }

  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    hash_data(single_combo)
  )

  run_name <- single_run$best_run_name

  train_test_file <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/prep_models/*", hash_data(project_name), "-",
      hash_data(run_name), "-train_test_split.", project_info$data_output
    )
  )

  if (length(train_test_file) < 1) {
    stop("Error in reconcile_agent_forecast(). No train test split file found for the best run.",
      call. = FALSE
    )
  }

  model_train_test_tbl <- read_file(
    run_info = project_info,
    file_list = train_test_file[1],
    return_type = "df"
  )

  # load hierarchical forecast
  hts_fcst_tbl <- load_agent_forecast(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores
  ) %>%
    dplyr::filter(Best_Model == "Yes")

  # reconcile the forecast
  project_info$run_name <- agent_info$run_id

  final_fcst_tbl <- reconcile(
    initial_fcst = hts_fcst_tbl,
    run_info = project_info,
    forecast_approach = agent_info$forecast_approach,
    negative_forecast = negative_forecast
  ) %>%
    create_prediction_intervals(model_train_test_tbl) %>%
    convert_weekly_to_daily(project_info$date_type, project_info$weekly_to_daily) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
    dplyr::left_join(
      model_train_test_tbl %>%
        dplyr::select(Run_Type, Train_Test_ID),
      by = "Train_Test_ID"
    ) %>%
    dplyr::relocate(Run_Type, .before = Train_Test_ID) %>%
    dplyr::select(-Combo_ID, -Hyperparameter_ID) %>%
    dplyr::relocate(Combo) %>%
    dplyr::arrange(Combo, dplyr::desc(Best_Model), Model_ID, Train_Test_ID, Date) %>%
    tidyr::separate(
      col = Combo,
      into = project_info$combo_variables,
      remove = FALSE,
      sep = "--"
    ) %>%
    base::suppressWarnings()

  write_data(
    x = final_fcst_tbl,
    combo = "Best-Model",
    run_info = project_info,
    output_type = "data",
    folder = "forecasts",
    suffix = "-reconciled"
  )
}

#' Update Forecast for a Combo
#'
#' This function updates the forecast for a specific combo based on the previous best run.
#'
#' @param agent_info A list containing the agent information.
#' @param prev_best_run_tbl A data frame containing the previous best run information.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param num_cores Numeric indicating the number of cores to use for parallel processing.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param seed Numeric seed for reproducibility.
#'
#' @return A data frame containing the updated forecast results.
#' @noRd
update_forecast_combo <- function(agent_info,
                                  prev_best_run_tbl,
                                  parallel_processing,
                                  num_cores,
                                  inner_parallel,
                                  seed) {
  # get metadata
  project_info <- agent_info$project_info

  if (unique(prev_best_run_tbl$model_type) == "global") {
    combo <- "All-Data"
    combo_value <- "*"
  } else {
    combo <- prev_best_run_tbl$combo[1]
    combo_value <- hash_data(combo)
  }

  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(combo == "All-Data", hash_data("all"), combo_value)
  )

  combo_list <- unique(prev_best_run_tbl$combo)

  prev_best_wmape <- mean(prev_best_run_tbl$weighted_mape, na.rm = TRUE)

  cli::cli_alert_info("Updating Forecast for {combo}")

  # get run info of previous best run
  prev_run_info <- list(
    project_name = project_name,
    run_name = prev_best_run_tbl$best_run_name[1],
    storage_object = project_info$storage_object,
    path = project_info$path,
    data_output = project_info$data_output,
    object_output = project_info$object_output
  )

  prev_run_log_tbl <- get_run_info(
    project_name = project_name,
    run_name = prev_best_run_tbl$best_run_name[1],
    storage_object = project_info$storage_object,
    path = project_info$path
  )

  # get best model list from previous run
  if (unique(prev_best_run_tbl$model_type) == "global") {
    # global model only runs xgboost with R1 recipe
    model_id_list <- "xgboost--global--R1"
  } else {
    # get previous forecast for local model
    # read the single models forecast file
    single_models_fcst <- read_file(
      run_info = prev_run_info,
      file_list = paste0(
        prev_run_info$path, "/forecasts/",
        hash_data(prev_run_info$project_name), "-",
        hash_data(prev_run_info$run_name), "-",
        hash_data(combo), "-single_models.",
        prev_run_info$data_output
      ) %>% fs::path_tidy(),
      return_type = "df"
    )
    
    # read the average models forecast file
    avg_models_fcst <- tryCatch(
      {
        read_file(
          run_info = prev_run_info,
          file_list = paste0(
            prev_run_info$path, "/forecasts/",
            hash_data(prev_run_info$project_name), "-",
            hash_data(prev_run_info$run_name), "-",
            hash_data(combo), "-average_models.",
            prev_run_info$data_output
          ) %>% fs::path_tidy(),
          return_type = "df"
        )
      },
      error = function(e) {
        tibble::tibble()
      }
    )
            
    # combine forecasts
    prev_fcst_tbl <- dplyr::bind_rows(single_models_fcst, avg_models_fcst) %>%
      dplyr::filter(Best_Model == "Yes")
    
    if (nrow(prev_fcst_tbl) == 0) {
      stop("Error in update_forecast(). No best model forecasts found from previous run for combo: ", combo,
           call. = FALSE
      )
    }
    
    model_id_list <- prev_fcst_tbl %>%
      dplyr::pull(Model_ID) %>%
      unique() %>%
      strsplit(split = "_") %>%
      unlist()
  }

  prev_best_model_list <- model_id_list %>%
    stringr::str_replace("--.*$", "") %>%
    unique()

  # get best model recipes from previous run
  prev_best_recipes <- sub(".*--", "", model_id_list) %>%
    unique()

  # get trained models from previous run
  # read the trained models file and filter by model IDs
  trained_models_tbl <- tryCatch(
    {
      read_file(
        run_info = prev_run_info,
        file_list = paste0(
          prev_run_info$path, "/models/",
          hash_data(prev_run_info$project_name), "-",
          hash_data(prev_run_info$run_name), "-",
          hash_data(combo), "-single_models.",
          prev_run_info$object_output
        ) %>% fs::path_tidy(),
        return_type = "df"
      ) %>%
        dplyr::filter(Model_ID %in% model_id_list)
    },
    error = function(e) {
      stop("Error in update_forecast(). No trained models found from previous run at: ", file_path,
           call. = FALSE
      )
    }
  )
  
  # verify models were found
  if (nrow(trained_models_tbl) == 0) {
    stop("Error in update_forecast(). No trained models matching the model IDs found in previous run.",
         call. = FALSE
    )
  }

  # get external regressor info from previous run
  external_regressors <- adjust_inputs(prev_run_log_tbl$external_regressors)

  if (!is.null(external_regressors)) {
    # check that regressors from previous run are still present in the new run, if any are missing throw an error
    missing_xregs <- setdiff(external_regressors, agent_info$external_regressors)
    if (length(missing_xregs) > 0) {
      stop("Error in update_forecast(). The following external regressors are missing from the new run: ",
        paste(missing_xregs, collapse = ", "), ". Please add them back to the agent inputs.",
        call. = FALSE
      )
    }
  }

  # adjust parallel processing for combos
  if (!is.null(parallel_processing)) {
    if (combo != "All-Data" & parallel_processing == "spark") {
      # turn off parallel processing when running single combo
      parallel_processing <- NULL
      prep_parallel <- NULL
    } else if (combo == "All-Data" & parallel_processing == "spark") {
      # local parallel process instead of spark on global models
      parallel_processing <- NULL
      prep_parallel <- "local_machine"
    } else {
      prep_parallel <- parallel_processing
    }
  } else {
    prep_parallel <- parallel_processing
  }

  # get input data for new run
  if (combo == "All-Data") {
    input_data <- read_file(
      run_info = project_info,
      file_list = list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, ".", project_info$data_output
        )
      ),
      return_type = "df"
    )
  } else {
    input_data <- read_file(
      run_info = project_info,
      file_list = paste0(
        project_info$path, "/input_data/", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, ".", project_info$data_output
      ) %>% fs::path_tidy(),
      return_type = "df"
    )
  }

  # create unique run name
  run_name <- paste0(
    "agent_",
    agent_info$run_id, "_",
    ifelse(combo == "All-Data", hash_data("all"), combo_value)
  )

  # create new run
  new_run_info <- set_run_info(
    project_name = project_name,
    run_name = run_name,
    storage_object = project_info$storage_object,
    path = project_info$path,
    data_output = project_info$data_output,
    object_output = project_info$object_output,
    add_unique_id = FALSE
  )

  # clean and prepare data for training
  prep_data(
    run_info = new_run_info,
    input_data = input_data,
    combo_variables = project_info$combo_variables,
    target_variable = "Target",
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    external_regressors = external_regressors,
    hist_start_date = agent_info$hist_start_date,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = agent_info$combo_cleanup_date,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = prev_run_log_tbl$clean_missing_values,
    clean_outliers = prev_run_log_tbl$clean_outliers,
    box_cox = prev_run_log_tbl$box_cox,
    stationary = prev_run_log_tbl$stationary,
    forecast_approach = prev_run_log_tbl$forecast_approach,
    parallel_processing = prep_parallel,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = adjust_inputs(prev_run_log_tbl$lag_periods, convert_numeric = TRUE),
    rolling_window_periods = adjust_inputs(prev_run_log_tbl$rolling_window_periods, convert_numeric = TRUE),
    recipes_to_run = prev_best_recipes,
    multistep_horizon = prev_run_log_tbl$multistep_horizon
  )

  if (prev_run_log_tbl$box_cox || prev_run_log_tbl$stationary) {
    combo_info_tbl <- read_file(new_run_info,
      file_list = paste0(
        new_run_info$path,
        "/prep_data/", hash_data(new_run_info$project_name), "-", hash_data(new_run_info$run_name),
        "-orig_combo_info.", new_run_info$data_output
      ) %>% fs::path_tidy(),
      return_type = "df"
    )

    if (combo != "All-Data") {
      combo_info_tbl <- combo_info_tbl %>%
        dplyr::filter(Combo == combo)
    }
  } else {
    combo_info_tbl <- tibble::tibble()
  }

  # prep models and train/test splits
  prep_models(
    run_info = new_run_info,
    back_test_scenarios = agent_info$back_test_scenarios,
    back_test_spacing = agent_info$back_test_spacing,
    models_to_run = prev_best_model_list,
    models_not_to_run = NULL,
    run_ensemble_models = FALSE,
    pca = prev_run_log_tbl$pca,
    num_hyperparameters = as.numeric(prev_run_log_tbl$num_hyperparameters),
    seasonal_period = adjust_inputs(prev_run_log_tbl$seasonal_period, convert_numeric = TRUE),
    seed = seed
  )

  prepped_model_tbl <- get_prepped_models(new_run_info)

  model_train_test_tbl <- prepped_model_tbl %>%
    dplyr::filter(Type == "Train_Test_Splits") %>%
    dplyr::select(Data) %>%
    tidyr::unnest(Data)

  model_hyperparameter_tbl <- prepped_model_tbl %>%
    dplyr::filter(Type == "Model_Hyperparameters") %>%
    dplyr::select(Data) %>%
    tidyr::unnest(Data)

  rm(prepped_model_tbl)

  # refit each model on previously selected hyperparameters
  cli::cli_progress_step("Refitting Models with Previously Selected Hyperparameters")

  final_model_tbl <- fit_models(
    run_info = new_run_info,
    combo = combo,
    combo_info_tbl = combo_info_tbl,
    trained_models_tbl = trained_models_tbl,
    model_train_test_tbl = model_train_test_tbl,
    model_hyperparameter_tbl = model_hyperparameter_tbl,
    prev_run_log_tbl = prev_run_log_tbl,
    forecast_horizon = agent_info$forecast_horizon,
    retune_hyperparameters = FALSE,
    num_cores = num_cores,
    inner_parallel = inner_parallel,
    seed = seed
  )

  # get forecast and calculate wmape
  final_fcst_tbl <- final_model_tbl %>%
    adjust_forecast(
      run_info = new_run_info,
      forecast_approach = prev_run_log_tbl$forecast_approach,
      negative_forecast = prev_run_log_tbl$negative_forecast
    )

  final_wmape <- final_fcst_tbl %>%
    dplyr::filter(Combo %in% combo_list) %>%
    calc_wmape()

  # retune hyperparameters if +10% worse than previous best
  if (final_wmape > (prev_best_wmape * 1.1)) {
    cli::cli_progress_step("Retuning Model Hyperparameters")

    rm(final_model_tbl)
    rm(final_fcst_tbl)
    rm(final_wmape)

    final_model_tbl <- fit_models(
      run_info = new_run_info,
      combo = combo,
      combo_info_tbl = combo_info_tbl,
      trained_models_tbl = trained_models_tbl,
      model_train_test_tbl = model_train_test_tbl,
      model_hyperparameter_tbl = model_hyperparameter_tbl,
      prev_run_log_tbl = prev_run_log_tbl,
      forecast_horizon = agent_info$forecast_horizon,
      retune_hyperparameters = TRUE,
      num_cores = num_cores,
      inner_parallel = inner_parallel,
      seed = seed
    )

    final_fcst_tbl <- final_model_tbl %>%
      adjust_forecast(
        run_info = new_run_info,
        forecast_approach = prev_run_log_tbl$forecast_approach,
        negative_forecast = prev_run_log_tbl$negative_forecast
      )

    final_wmape <- final_fcst_tbl %>%
      dplyr::filter(Combo %in% combo_list) %>%
      calc_wmape()
  }

  # finalize results
  final_fcst_tbl <- final_fcst_tbl %>%
    create_prediction_intervals(model_train_test_tbl)

  final_model_tbl <- final_model_tbl %>%
    tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
    dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Model_Fit)

  # write final outputs
  cli::cli_progress_step("Logging Forecast Results")

  combo_id <- unique(final_model_tbl$Combo_ID)

  write_data(
    x = final_model_tbl,
    combo = combo_id,
    run_info = new_run_info,
    output_type = "object",
    folder = "models",
    suffix = "-single_models"
  )

  if ("simple_average" %in% unique(final_fcst_tbl$Recipe_ID)) {
    write_data(
      x = final_fcst_tbl %>%
        dplyr::filter(Recipe_ID != "simple_average") %>%
        convert_weekly_to_daily(project_info$date_type, prev_run_log_tbl$weekly_to_daily),
      combo = combo_id,
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-single_models"
    )

    write_data(
      x = final_fcst_tbl %>%
        dplyr::filter(Recipe_ID == "simple_average") %>%
        convert_weekly_to_daily(project_info$date_type, prev_run_log_tbl$weekly_to_daily),
      combo = combo_id,
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-average_models"
    )
  } else if (combo == "All-Data" & prev_run_log_tbl$forecast_approach != "bottoms_up") {
    write_data(
      x = final_fcst_tbl %>%
        dplyr::filter(Combo %in% combo_list) %>%
        convert_weekly_to_daily(project_info$date_type, prev_run_log_tbl$weekly_to_daily),
      combo = "Best-Model",
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-reconciled"
    )
  } else {
    if (combo == "All-Data") {
      for (combo_name in combo_list) {
        write_data(
          x = final_fcst_tbl %>%
            dplyr::filter(Combo == combo_name) %>%
            convert_weekly_to_daily(project_info$date_type, prev_run_log_tbl$weekly_to_daily),
          combo = combo_name,
          run_info = new_run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-global_models"
        )
      }
    } else {
      write_data(
        x = final_fcst_tbl %>%
          convert_weekly_to_daily(project_info$date_type, prev_run_log_tbl$weekly_to_daily),
        combo = combo_id,
        run_info = new_run_info,
        output_type = "data",
        folder = "forecasts",
        suffix = "-single_models"
      )
    }
  }

  # log run
  log_wmape <- final_fcst_tbl %>%
    dplyr::mutate(Combo = "Placeholder") %>% # calc wmape for a single combo or all combos
    calc_wmape()

  new_log_tbl <- get_run_info(
    project_name = project_name,
    run_name = new_run_info$run_name,
    storage_object = project_info$storage_object,
    path = project_info$path
  ) %>%
    dplyr::mutate(
      run_global_models = ifelse(combo == "All-Data", TRUE, FALSE),
      run_local_models = ifelse(combo != "All-Data", TRUE, FALSE),
      global_model_recipes = prev_run_log_tbl$global_model_recipes,
      feature_selection = prev_run_log_tbl$feature_selection,
      seed = seed,
      negative_forecast = prev_run_log_tbl$negative_forecast,
      weekly_to_daily = prev_run_log_tbl$weekly_to_daily,
      inner_parallel = inner_parallel,
      average_models = prev_run_log_tbl$average_models,
      max_model_average = prev_run_log_tbl$max_model_average,
      weighted_mape = log_wmape
    )

  write_data(
    x = new_log_tbl,
    combo = NULL,
    run_info = new_run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  final_log_results <- log_best_run(
    agent_info = agent_info,
    run_info = new_run_info,
    weighted_mape = log_wmape,
    combo = if (combo == "All-Data") {
      NULL
    } else {
      combo
    }
  )

  cli::cli_progress_done("Update Forecast Complete for {combo}")

  return("done")
}

#' Fit models based on previous run information and hyperparameters
#'
#' This function fits models based on the provided run information, combo, and previous run log. It handles model adjustments, feature selection, and hyperparameter tuning if necessary.
#'
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#' @param combo A string indicating the combo type (e.g., "All-Data" or specific combo).
#' @param combo_info_tbl A data frame containing information about the combo, including any necessary adjustments for column types.
#' @param trained_models_tbl A data frame containing the trained models to be fitted.
#' @param model_train_test_tbl A data frame containing the train-test split information for the models.
#' @param model_hyperparameter_tbl A data frame containing the hyperparameters for the models.
#' @param prev_run_log_tbl A data frame containing the previous run log information, including multistep horizon and forecast horizon.
#' @param forecast_horizon An integer indicating the forecast horizon to be used for the models.
#' @param retune_hyperparameters A boolean indicating whether to retune hyperparameters if the initial fit does not meet performance criteria.
#' @param num_cores An integer indicating the number of cores to be used for parallel processing.
#' @param inner_parallel A boolean indicating whether to use inner parallel processing.
#' @param seed An integer seed for reproducibility.
#'
#' @return A data frame containing the fitted models with their respective model IDs, names, types, recipes, and fitted model objects.
#' @noRd
fit_models <- function(run_info,
                       combo,
                       combo_info_tbl,
                       trained_models_tbl,
                       model_train_test_tbl,
                       model_hyperparameter_tbl,
                       prev_run_log_tbl,
                       forecast_horizon,
                       retune_hyperparameters,
                       num_cores,
                       inner_parallel,
                       seed = 123) {
  
  # get recipe info
  if("R1" %in% unique(trained_models_tbl$Recipe_ID)) {
    if(combo == "All-Data") {
      r1_tbl <- get_prepped_data(
        run_info = run_info,
        recipe = "R1"
      )
    } else {
      r1_tbl <- read_file(
        run_info = run_info,
        file_list = paste0(
          run_info$path, "/prep_data/",
          hash_data(run_info$project_name), "-",
          hash_data(run_info$run_name), "-",
          hash_data(combo), "-R1.",
          run_info$data_output
        ) %>% fs::path_tidy(),
        return_type = "df"
      )
    }
  }

  if("R2" %in% unique(trained_models_tbl$Recipe_ID)) {
    r2_tbl <- read_file(
      run_info = run_info,
      file_list = paste0(
        run_info$path, "/prep_data/",
        hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name), "-",
        hash_data(combo), "-R2.",
        run_info$data_output
      ) %>% fs::path_tidy(),
      return_type = "df"
    )
  }
  
  # train each model
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = if (inner_parallel) {
      "local_machine"
    } else {
      NULL
    },
    num_cores = num_cores,
    task_length = num_cores
  )

  inner_cl <- par_info$cl
  inner_packages <- par_info$packages

  model_tbl <- foreach::foreach(
    model_run = trained_models_tbl %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE),
    .combine = "rbind",
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %do% {
    # get initial run info
    model <- model_run %>%
      dplyr::pull(Model_Name)

    data_prep_recipe <- model_run %>%
      dplyr::pull(Recipe_ID)

    prep_data <- switch(
      data_prep_recipe,
      "R1" = r1_tbl,
      "R2" = r2_tbl
    )
      
    workflow <- model_run$Model_Fit[[1]]

    # convert trained workflow to empty workflow to prevent memory issues
    workflow_recipe <- workflows::extract_recipe(workflow, estimated = FALSE) # unprepped
    workflow_spec <- workflows::extract_spec_parsnip(workflow) # model spec
    workflow <- workflows::workflow() %>%
      workflows::add_recipe(workflow_recipe) %>%
      workflows::add_model(workflow_spec)

    # adjust models based on data
    if (nrow(prep_data) > 500 & model == "xgboost") {
      # update xgboost model to use 'hist' tree method to speed up training
      workflow <- workflows::update_model(
        workflow,
        workflows::extract_spec_parsnip(workflow) %>%
          parsnip::set_args(tree_method = "hist")
      )
    }

    if (combo == "All-Data") {
      # adjust column types to match original data
      prep_data <- adjust_column_types(
        prep_data,
        workflows::extract_recipe(workflow, estimated = FALSE)
      )
    }

    # adjust workflow if forecast horizon changes for multistep model
    if (prev_run_log_tbl$multistep_horizon & data_prep_recipe == "R1" & model %in% list_multistep_models() & as.numeric(prev_run_log_tbl$forecast_horizon) != forecast_horizon) {
      updated_model_spec <- workflow %>%
        workflows::extract_spec_parsnip() %>%
        update(
          forecast_horizon = forecast_horizon,
          lag_periods = get_lag_periods(
            lag_periods = NULL,
            date_type = prev_run_log_tbl$date_type,
            forecast_horizon = forecast_horizon,
            multistep_horizon = TRUE
          )
        )

      workflow <- workflows::update_model(workflow, updated_model_spec)
    }

    # handle missing features or updating features based on changes in forecast horizon
    workflow_cols <- workflow %>%
      workflows::extract_recipe(estimated = FALSE) %>%
      summary() %>%
      dplyr::pull(variable)

    if ("Target_Original" %in% colnames(prep_data)) {
      missing_cols <- setdiff(workflow_cols, colnames(prep_data %>% dplyr::select(-Target_Original)))
    } else {
      missing_cols <- setdiff(workflow_cols, colnames(prep_data))
    }

    if (length(missing_cols) > 0 ||
      (as.numeric(prev_run_log_tbl$forecast_horizon) != forecast_horizon & model %in% list_multistep_models())) {
      # rerun feature selection if needed
      if (prev_run_log_tbl$feature_selection) {
        fs_list <- prep_data %>%
          run_feature_selection(
            run_info = run_info,
            train_test_data = model_train_test_tbl,
            parallel_processing = if (inner_parallel) {
              "local_machine"
            } else {
              NULL
            },
            date_type = prev_run_log_tbl$date_type,
            fast = FALSE,
            forecast_horizon = forecast_horizon,
            external_regressors = adjust_inputs(prev_run_log_tbl$external_regressors),
            multistep_horizon = prev_run_log_tbl$multistep_horizon
          )

        updated_model_spec <- workflow %>%
          workflows::extract_spec_parsnip()

        if (prev_run_log_tbl$multistep_horizon) {
          updated_model_spec <- updated_model_spec %>%
            update(selected_features = fs_list)
        }
      } else {
        if ("Target_Original" %in% colnames(prep_data)) {
          fs_list <- prep_data %>%
            dplyr::select(-Target, -Target_Original) %>%
            colnames()
        } else {
          fs_list <- prep_data %>%
            dplyr::select(-Target) %>%
            colnames()
        }

        updated_model_spec <- workflow %>%
          workflows::extract_spec_parsnip()

        if (prev_run_log_tbl$multistep_horizon) {
          updated_model_spec <- updated_model_spec %>%
            update(selected_features = NULL)
        }
      }

      if (prev_run_log_tbl$multistep_horizon & data_prep_recipe == "R1" & model %in% list_multistep_models()) {
        final_features_list <- unique(unlist(fs_list, use.names = FALSE))
        final_features_list <- (unique(c(final_features_list, "Date", "Date_index.num")))

        updated_recipe <- workflow %>%
          workflows::extract_recipe(estimated = FALSE) %>%
          recipes::remove_role(tidyselect::everything(), old_role = "predictor") %>%
          recipes::update_role(tidyselect::any_of(final_features_list), new_role = "predictor") %>%
          recipes::update_role(-tidyselect::any_of(c(final_features_list, "Target")), new_role = "ignore") %>% # ignore missing features
          base::suppressWarnings()

        workflow <- workflow %>%
          workflows::update_model(updated_model_spec) %>%
          workflows::update_recipe(updated_recipe)
      } else {
        if (prev_run_log_tbl$feature_selection & prev_run_log_tbl$multistep_horizon) {
          final_features_list <- fs_list[[paste0("model_lag_", as.numeric(prev_run_log_tbl$forecast_horizon))]]
          final_features_list <- (unique(c(final_features_list, "Date", "Date_index.num")))
        } else {
          final_features_list <- (unique(c(fs_list, "Date", "Date_index.num")))
        }

        updated_recipe <- workflow %>%
          workflows::extract_recipe(estimated = FALSE) %>%
          recipes::remove_role(tidyselect::everything(), old_role = "predictor") %>%
          recipes::update_role(tidyselect::any_of(final_features_list), new_role = "predictor") %>%
          recipes::update_role(-tidyselect::any_of(c(final_features_list, "Target")), new_role = "ignore") %>% # ignore missing features
          base::suppressWarnings()

        workflow <- workflow %>%
          workflows::update_recipe(updated_recipe)
      }

      # add missing columns as NA values to prep_data table, which will be ignored by the recipe
      for (col in missing_cols) {
        prep_data[[col]] <- NA_real_
      }
    }

    hyperparameters <- model_hyperparameter_tbl %>%
      dplyr::filter(
        Model == model,
        Recipe == data_prep_recipe
      ) %>%
      dplyr::select(Hyperparameter_Combo, Hyperparameters) %>%
      tidyr::unnest(Hyperparameters)

    if (prev_run_log_tbl$stationary & !(model %in% list_multivariate_models())) {
      # undifference the data for a univariate model
      prep_data <- prep_data %>%
        undifference_recipe(
          combo_info_tbl,
          model_train_test_tbl %>% dplyr::slice(1) %>% dplyr::pull(Train_End)
        )
    }

    # tune hyperparameters
    if (retune_hyperparameters) {
      set.seed(seed)

      tune_results <- tune::tune_grid(
        object = workflow,
        resamples = create_splits(prep_data, model_train_test_tbl %>% dplyr::filter(Run_Type == "Validation")),
        grid = hyperparameters %>% dplyr::select(-Hyperparameter_Combo),
        control = tune::control_grid(
          allow_par = inner_parallel,
          pkgs = c(inner_packages, "finnts"),
          parallel_over = "everything"
        )
      ) %>%
        base::suppressMessages() %>%
        base::suppressWarnings()

      best_param <- tune::select_best(tune_results, metric = "rmse")

      if (length(colnames(best_param)) == 1) {
        hyperparameter_id <- 1
      } else {
        hyperparameter_id <- hyperparameters %>%
          dplyr::inner_join(best_param) %>%
          dplyr::select(Hyperparameter_Combo) %>%
          dplyr::pull() %>%
          base::suppressMessages()
      }

      finalized_workflow <- tune::finalize_workflow(workflow, best_param)
    } else {
      hyperparameter_id <- 1
      finalized_workflow <- workflow
    }

    # fit model on entire input data
    set.seed(seed)

    wflow_fit <- generics::fit(finalized_workflow, prep_data %>% tidyr::drop_na(Target)) %>%
      base::suppressMessages()

    # refit on all train test splits
    set.seed(seed)

    refit_tbl <- tune::fit_resamples(
      object = finalized_workflow,
      resamples = create_splits(prep_data, model_train_test_tbl),
      metrics = NULL,
      control = tune::control_resamples(
        allow_par = inner_parallel,
        save_pred = TRUE,
        pkgs = c(inner_packages, "finnts"),
        parallel_over = "everything"
      )
    ) %>%
      tune::collect_predictions() %>%
      base::suppressMessages() %>%
      base::suppressWarnings()

    # finalize forecast
    final_fcst <- refit_tbl %>%
      dplyr::rename(
        Forecast = .pred,
        Train_Test_ID = id
      ) %>%
      dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
      dplyr::left_join(
        prep_data %>%
          dplyr::mutate(.row = dplyr::row_number()) %>%
          dplyr::select(Combo, Date, .row),
        by = ".row"
      ) %>%
      dplyr::mutate(Hyperparameter_ID = hyperparameter_id) %>%
      dplyr::select(-.row, -.config) %>%
      dplyr::left_join(
        model_train_test_tbl %>%
          dplyr::select(Train_Test_ID, Run_Type),
        by = "Train_Test_ID"
      )

    # check for future forecast
    if (as.numeric(min(unique(final_fcst$Train_Test_ID))) != 1) {
      stop("model is missing future forecast")
    }

    # undo differencing transformation
    if (prev_run_log_tbl$stationary & model %in% list_multivariate_models()) {
      if (combo == "All-Data") {
        final_fcst <- final_fcst %>%
          dplyr::group_by(Combo) %>%
          dplyr::group_split() %>%
          purrr::map(function(x) {
            combo <- unique(x$Combo)

            final_fcst_return <- x %>%
              undifference_forecast(
                prep_data %>% dplyr::filter(Combo == combo),
                combo_info_tbl %>% dplyr::filter(Combo == combo)
              )

            return(final_fcst_return)
          }) %>%
          dplyr::bind_rows()
      } else {
        final_fcst <- final_fcst %>%
          undifference_forecast(
            prep_data,
            combo_info_tbl
          )
      }
    }

    # undo box-cox transformation
    if (prev_run_log_tbl$box_cox) {
      if (combo == "All-Data") {
        final_fcst <- final_fcst %>%
          dplyr::group_by(Combo) %>%
          dplyr::group_split() %>%
          purrr::map(function(x) {
            combo <- unique(x$Combo)

            lambda <- combo_info_tbl %>%
              dplyr::filter(Combo == combo) %>%
              dplyr::select(Box_Cox_Lambda) %>%
              dplyr::pull(Box_Cox_Lambda)

            if (!is.na(lambda)) {
              final_fcst_return <- x %>%
                dplyr::mutate(
                  Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
                  Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
                )
              if ("Target_Original" %in% colnames(x)) {
                final_fcst_return <- final_fcst_return %>%
                  dplyr::mutate(
                    Target_Original = timetk::box_cox_inv_vec(Target_Original, lambda = lambda)
                  )
              }
            } else {
              final_fcst_return <- x
            }

            return(final_fcst_return)
          }) %>%
          dplyr::bind_rows()
      } else {
        lambda <- combo_info_tbl$Box_Cox_Lambda

        if (!is.na(lambda)) {
          final_fcst <- final_fcst %>%
            dplyr::mutate(
              Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
              Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
            )
        }
        if ("Target_Original" %in% colnames(final_fcst)) {
          final_fcst <- final_fcst %>%
            dplyr::mutate(
              Target_Original = timetk::box_cox_inv_vec(Target_Original, lambda = lambda)
            )
        }
      }
    }

    # negative forecast adjustment
    final_fcst <- final_fcst %>%
      negative_fcst_adj(prev_run_log_tbl$negative_forecast)

    # return the forecast
    combo_id <- ifelse(combo == "All-Data", "All-Data", unique(final_fcst$Combo))

    final_return_tbl <- tibble::tibble(
      Combo_ID = combo_id,
      Model_Name = model,
      Model_Type = ifelse(combo_id == "All-Data", "global", "local"),
      Recipe_ID = data_prep_recipe,
      Forecast_Tbl = list(final_fcst),
      Model_Fit = list(wflow_fit)
    )

    return(final_return_tbl)
  }

  par_end(inner_cl)
  try(doParallel::stopImplicitCluster(), silent = TRUE)
  foreach::registerDoSEQ()

  # ensure at least one model ran successfully
  if (is.null(model_tbl)) {
    stop("All models failed to train")
  }

  return(model_tbl)
}

#' Adjust Forecast After Model Fitting
#'
#' This function adjusts the forecast table after model fitting, averaging forecasts if multiple models are present, and reconciling the forecast if hierarchical time series methods are used.
#'
#' @param model_tbl A tibble containing the model fitting results.
#' @param run_info A list containing run information such as project name, run name, etc.
#' @param forecast_approach The approach used for forecasting, either "standard_hierarchy" or "grouped_hierarchy".
#' @param negative_forecast Logical indicating if negative forecasts are allowed.
#'
#' @return A tibble containing the adjusted forecast table.
#' @noRd
adjust_forecast <- function(model_tbl,
                            run_info,
                            forecast_approach,
                            negative_forecast) {
  # check if forecasts should be averaged
  if (nrow(model_tbl) > 1) {
    simple_average <- TRUE
  } else {
    simple_average <- FALSE
  }

  # extract out the forecast table
  forecast_tbl <- model_tbl %>%
    dplyr::select(-Model_Fit) %>%
    tidyr::unnest(Forecast_Tbl) %>%
    dplyr::arrange(Train_Test_ID) %>%
    tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
    dplyr::group_by(Combo, Model_ID, Train_Test_ID) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup()

  if (simple_average) {
    # average the forecasts
    avg_forecast_tbl <- forecast_tbl %>%
      dplyr::group_by(Combo_ID, Combo, Run_Type, Train_Test_ID, Date, Horizon) %>%
      dplyr::summarise(
        Forecast = mean(Forecast, na.rm = TRUE),
        Target = mean(Target, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Model_ID = paste(sort(unique(forecast_tbl$Model_ID)), collapse = "_"),
        Model_Name = NA,
        Model_Type = "local",
        Recipe_ID = "simple_average",
        Hyperparameter_ID = NA,
        Best_Model = "Yes"
      )

    final_fcst_tbl <- forecast_tbl %>%
      dplyr::mutate(Best_Model = "No") %>%
      dplyr::bind_rows(avg_forecast_tbl)
  } else {
    final_fcst_tbl <- forecast_tbl %>%
      dplyr::mutate(Best_Model = "Yes")
  }

  # reconcile forecast if hts
  if (unique(final_fcst_tbl$Combo_ID) == "All-Data" & forecast_approach != "bottoms_up") {
    final_fcst_tbl <- final_fcst_tbl %>%
      reconcile(
        run_info = run_info,
        forecast_approach = forecast_approach,
        negative_forecast = negative_forecast
      )
  }

  return(final_fcst_tbl)
}

#' reconcile the forecast
#'
#' This function reconciles the initial forecast table using hierarchical time series methods.
#'
#' @param initial_fcst The initial forecast table to reconcile.
#' @param run_info The run information list containing project and run details.
#' @param forecast_approach The approach used for forecasting, either "standard_hierarchy" or "grouped_hierarchy".
#' @param negative_forecast Logical indicating if negative forecasts are allowed.
#'
#' @return A reconciled forecast table.
#' @noRd
reconcile <- function(initial_fcst,
                      run_info,
                      forecast_approach,
                      negative_forecast) {
  hts_list <- read_file(run_info,
    path = paste0("/prep_data/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), "-hts_info.", run_info$object_output),
    return_type = "object"
  )

  hist_tbl <- read_file(run_info,
    path = paste0("/prep_data/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), "-hts_data.", run_info$data_output)
  ) %>%
    dplyr::select(Combo, Date, Target)

  hts_nodes <- hts_list$nodes
  original_combo_list <- hts_list$original_combos
  hts_combo_list <- hts_list$hts_combos

  tryCatch(
    {
      run_type_tbl <- initial_fcst %>%
        dplyr::select(Train_Test_ID, Run_Type) %>%
        dplyr::distinct()

      forecast_tbl <- initial_fcst %>%
        dplyr::select(Date, Train_Test_ID, Combo, Forecast) %>%
        tidyr::pivot_wider(names_from = Combo, values_from = Forecast)

      forecast_tbl[is.na(forecast_tbl)] <- 0

      date_tbl <- forecast_tbl %>%
        dplyr::select(Date, Train_Test_ID)

      ts <- forecast_tbl %>%
        tibble::as_tibble() %>%
        dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
        stats::ts()

      residual_multiplier <- 10 # shrink extra large residuals to prevent recon issues

      residuals_tbl <- initial_fcst %>%
        dplyr::filter(Run_Type == "Back_Test") %>%
        dplyr::mutate(
          Forecast_Adj = ifelse((abs(Target) + 1) * residual_multiplier < abs(Forecast), (Target + 1) * residual_multiplier, Forecast), # prevent hts recon issues
          Residual = Target - Forecast_Adj
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Residual = ifelse(Residual == 0, 0.0001, Residual)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Combo, Date, Train_Test_ID, Residual) %>%
        tidyr::pivot_wider(names_from = Combo, values_from = Residual) %>%
        tibble::as_tibble() %>%
        dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
        as.matrix()

      if (forecast_approach == "standard_hierarchy") {
        ts_combined <- data.frame(hts::combinef(ts,
          nodes = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
          keep = "bottom", nonnegative = !negative_forecast
        ))
        colnames(ts_combined) <- original_combo_list
      } else if (forecast_approach == "grouped_hierarchy") {
        ts_combined <- data.frame(hts::combinef(ts,
          groups = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
          keep = "bottom", nonnegative = !negative_forecast
        ))
        colnames(ts_combined) <- original_combo_list
      }
    },
    error = function(e) {
      stop("The 'Best-Model' was not able to be properly reconciled.",
        call. = FALSE
      )
    }
  )

  # final transformations before writing to disk
  reconciled_tbl <- ts_combined %>%
    tibble::add_column(
      Train_Test_ID = date_tbl$Train_Test_ID,
      .before = 1
    ) %>%
    tibble::add_column(
      Date = date_tbl$Date,
      .before = 1
    ) %>%
    tidyr::pivot_longer(!c(Date, Train_Test_ID),
      names_to = "Combo",
      values_to = "Forecast"
    ) %>%
    dplyr::left_join(hist_tbl, by = c("Date", "Combo")) %>%
    dplyr::left_join(
      run_type_tbl,
      by = "Train_Test_ID"
    ) %>%
    dplyr::rename(Combo_ID = Combo) %>%
    dplyr::mutate(
      Model_ID = "Best-Model",
      Best_Model = ifelse(Model_ID == "Best-Model", "Yes", "No"),
      Combo = Combo_ID
    ) %>%
    dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Hyperparameter_ID = NA) %>%
    dplyr::select(Combo_ID, Model_ID, Run_Type, Train_Test_ID, Hyperparameter_ID, Best_Model, Combo, Horizon, Date, Target, Forecast) %>%
    tidyr::separate(
      col = Model_ID,
      into = c("Model_Name", "Model_Type", "Recipe_ID"),
      remove = FALSE,
      sep = "--"
    ) %>%
    suppressWarnings()

  return(reconciled_tbl)
}

#' Calculate weighted Mean Absolute Percentage Error (WMAPE)
#'
#' This function calculates the weighted Mean Absolute Percentage Error (WMAPE) for a given forecast table.
#'
#' @param forecast_tbl A data frame containing forecast results with columns for Run_Type, Best_Model, Combo, Forecast, and Target.
#'
#' @return A numeric value representing the weighted MAPE.
#' @noRd
calc_wmape <- function(forecast_tbl) {
  forecast_tbl %>%
    dplyr::filter(
      Run_Type == "Back_Test",
      Best_Model == "Yes"
    ) %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target)
    ) %>%
    dplyr::group_by(Combo) %>%
    dplyr::mutate(
      MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4),
      Total = sum(abs(Target), na.rm = TRUE),
      Weight = (MAPE * abs(Target)) / Total
    ) %>%
    dplyr::summarise(weighted_mape = sum(Weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::pull(weighted_mape) %>%
    mean(na.rm = TRUE)
}

#' Adjust inputs for model fitting
#'
#' This function adjusts inputs for model fitting, converting character strings to numeric vectors if specified.
#'
#' @param x The input to adjust, can be a character string or numeric vector.
#' @param convert_numeric Logical, if TRUE converts character strings to numeric vectors.
#'
#' @return A list of adjusted inputs or NULL if input is NA.
#' @noRd
adjust_inputs <- function(x,
                          convert_numeric = FALSE) {
  if (is.na(x)) {
    NULL
  } else if (is.character(x)) {
    # split string by --- and return as list
    if (convert_numeric) {
      as.numeric(strsplit(x, "---")[[1]])
    } else {
      strsplit(x, "---")[[1]]
    }
  } else {
    if (convert_numeric) {
      as.numeric(x)
    } else {
      x
    }
  }
}
