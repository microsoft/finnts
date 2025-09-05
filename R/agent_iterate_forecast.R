#' Run the Finn Agent Forecast Iteration Process
#' 
#' This function orchestrates the forecast iteration process for a Finn agent, including exploratory data analysis,
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param max_iter Maximum number of iterations for forecast optimization.
#' @param weighted_mape_goal Weighted MAPE goal the agent is trying to achieve for each time series
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
#' @return NULL
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
#'   )
#'   
#' # set up LLM 
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'   
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#'  )
#'  
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#'  )
#' }
#' @export
iterate_forecast <- function(agent_info,
                             max_iter = 3,
                             weighted_mape_goal = 0.03,
                             parallel_processing = NULL,
                             inner_parallel = FALSE,
                             num_cores = NULL,
                             seed = 123) {
  message("[agent] Starting Forecast Iteration Process")

  # formatting checks
  check_agent_info(agent_info = agent_info)
  check_input_type("max_iter", max_iter, "numeric")
  check_input_type("weighted_mape_goal", weighted_mape_goal, "numeric")
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("inner_parallel", inner_parallel, "logical")
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))

  # get project info
  project_info <- agent_info$project_info

  # agent info adjustments
  if (agent_info$forecast_approach != "bottoms_up") {
    agent_info$project_info$combo_variables <- "ID"
  }

  # register tools
  register_eda_tools(agent_info)
  register_fcst_tools(agent_info)

  # run exploratory data analysis
  eda_results <- eda_agent_workflow(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores
  )

  # get total number of time series
  combo_list <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "*.", project_info$data_output
      )
    ),
    return_type = "df"
  ) %>%
    dplyr::pull(Combo) %>%
    unique()

  # optimize global models
  if (length(combo_list) > 1) {
    message("[agent] Starting Global Model Iteration Workflow")

    # adjust max iterations based on previous runs
    previous_runs <- load_run_results(
      agent_info = agent_info,
      combo = NULL
    )

    if (!tibble::is_tibble(previous_runs)) {
      max_iter_adj <- max_iter
    } else {
      prev_run_count <- previous_runs %>%
        dplyr::filter(agent_version == agent_info$agent_version) %>%
        nrow()

      max_iter_adj <- max_iter - prev_run_count
    }

    if (max_iter_adj > 0) {
      if (max_iter_adj < max_iter) {
        cli::cli_alert_info("Adjusting max iterations down due to previously completed iterations.")
      }

      # run the global model optimization
      fcst_results <- fcst_agent_workflow(
        agent_info = agent_info,
        combo = NULL,
        weighted_mape_goal = weighted_mape_goal,
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores,
        max_iter = max_iter_adj,
        seed = seed
      )
    } else {
      cli::cli_alert_info("Max iterations already met. Skipping global model optimization.")
    }

    # filter out which time series met the mape goal after global models
    local_combo_list <- get_best_agent_run(agent_info = agent_info) %>%
      dplyr::filter(weighted_mape > weighted_mape_goal) %>%
      dplyr::pull(combo) %>%
      unique()
  } else {
    message("[agent] Only one time series found. Skipping global model optimization.")
    local_combo_list <- combo_list
  }

  # optimize local models
  if (length(local_combo_list) == 0) {
    message("[agent] All time series met the MAPE goal after global models. Skipping local model optimization.")
  } else {
    message("[agent] Starting Local Model Iteration Workflow")

    # adjustments for parallel processing
    if (!is.null(parallel_processing)) {
      if (!is.null(agent_info$reason_llm)) {
        llm_info <- agent_info$reason_llm$get_provider()@api_key
      } else {
        llm_info <- agent_info$driver_llm$get_provider()@api_key
      }
    } else {
      llm_info <- NULL
    }

    # parallel setup
    par_info <- par_start(
      run_info            = project_info,
      parallel_processing = parallel_processing,
      num_cores           = num_cores,
      task_length         = length(local_combo_list)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # foreach over each combo file
    local_models <- foreach::foreach(
      x               = local_combo_list,
      .packages       = packages,
      .errorhandling  = "stop",
      .inorder        = FALSE,
      .multicombine   = TRUE
    ) %op%
      {
        message("[agent] Running local model optimization for combo: ", x)

        # ensure functions are available in the local environment
        if (inner_parallel) {
          run_graph <- run_graph
          execute_node <- execute_node
          reason_inputs <- reason_inputs
          submit_fcst_run <- submit_fcst_run
          get_fcst_output <- get_fcst_output
          calculate_fcst_metrics <- calculate_fcst_metrics
          log_best_run <- log_best_run
          load_eda_results <- load_eda_results
          make_pipe_table <- make_pipe_table
          load_run_results <- load_run_results
          get_total_run_count <- get_total_run_count
          agent_info <- agent_info
        }

        # rebuild llms when running on parallel workers
        if (!is.null(parallel_processing)) {
          # driver LLM
          driver_llm <- agent_info$driver_llm
          driver_provider <- driver_llm$get_provider()
          driver_provider@api_key <- llm_info
          driver_llm <- ellmer:::Chat$new(driver_provider)

          agent_info$driver_llm <- driver_llm

          # reason LLM
          if (!is.null(agent_info$reason_llm)) {
            reason_llm <- agent_info$reason_llm
            reason_provider <- reason_llm$get_provider()
            reason_provider@api_key <- llm_info
            reason_llm <- ellmer:::Chat$new(reason_provider)

            agent_info$reason_llm <- reason_llm
          }

          # re-register tools
          register_fcst_tools(agent_info)
        }

        # adjust max iterations based on previous runs
        previous_runs <- load_run_results(
          agent_info = agent_info,
          combo = hash_data(x)
        )

        if (!tibble::is_tibble(previous_runs)) {
          max_iter_adj <- max_iter
        } else {
          prev_run_count <- previous_runs %>%
            dplyr::filter(agent_version == agent_info$agent_version) %>%
            nrow()

          max_iter_adj <- max_iter - prev_run_count
        }

        if (max_iter_adj > 0) {
          if (max_iter_adj < max_iter) {
            cli::cli_alert_info("Adjusting max iterations down due to previously completed iterations.")
          }

          # run the local model workflow
          fcst_results <- fcst_agent_workflow(
            agent_info = agent_info,
            combo = hash_data(x),
            weighted_mape_goal = weighted_mape_goal,
            parallel_processing = NULL,
            inner_parallel = inner_parallel,
            num_cores = num_cores,
            max_iter = max_iter_adj,
            seed = seed
          )
        } else {
          cli::cli_alert_info("Max iterations already met. Skipping local model optimization.")
        }

        return(data.frame(Combo = x))
      } %>% base::suppressPackageStartupMessages()

    par_end(cl)
  }

  # reconcile hierarchical forecast
  if (agent_info$forecast_approach != "bottoms_up") {
    message("[agent] Reconciling Hierarchical Forecast")

    reconcile_agent_forecast(
      agent_info = agent_info,
      project_info = project_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores
    )
  }

  message("[agent] Forecast Iteration Process Complete")
}

#' Get the final best forecast for an agent
#' 
#' This function retrieves the final forecast for a Finn agent after the forecast iteration process is complete.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param parallel_processing Default of NULL runs no parallel processing and
#'  loads each time series forecast one after another. 'local_machine' leverages
#'  all cores on current machine Finn is running on. 'spark' runs time series
#'  in parallel on a spark cluster in Azure Databricks or Azure Synapse.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#'
#' @return A tibble containing the final forecast for the agent.
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
#'   )
#'   
#' # set up LLM 
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'   
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#'  )
#'  
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#'  )
#' 
#' # get the final forecast for the agent
#' final_forecast <- get_agent_forecast(agent_info = agent_info)
#' }
#' @export
get_agent_forecast <- function(agent_info,
                               parallel_processing = NULL,
                               num_cores = NULL) {
  # formatting checks
  check_agent_info(agent_info = agent_info)
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))

  # check for reconciled forecast
  if (agent_info$forecast_approach != "bottoms_up" & length(agent_info$project_info$combo_variables) > 1) {
    project_info <- agent_info$project_info
    project_info$run_name <- agent_info$run_id

    fcst_tbl <- read_file(
      run_info = project_info,
      path = paste0(
        "/forecasts/", hash_data(project_info$project_name), "-", hash_data(project_info$run_name),
        "-", hash_data("Best-Model"), "-reconciled.", project_info$data_output
      )
    )

    return(fcst_tbl)
  }

  # get the best run for the agent
  best_run_tbl <- get_best_agent_run(agent_info)

  model_type_list <- best_run_tbl %>%
    dplyr::pull(model_type) %>%
    unique()

  # load global model forecasts
  if ("global" %in% model_type_list) {
    global_combos <- best_run_tbl %>%
      dplyr::filter(model_type == "global") %>%
      dplyr::pull(combo) %>%
      unique()

    global_run_name <- best_run_tbl %>%
      dplyr::filter(model_type == "global") %>%
      dplyr::pull(best_run_name) %>%
      unique()

    run_info <- agent_info$project_info
    run_info$project_name <- paste0(
      agent_info$project_info$project_name,
      "_",
      hash_data("all")
    )
    run_info$run_name <- global_run_name

    global_fcst_tbl <- get_forecast_data(run_info = run_info) %>%
      dplyr::filter(Combo %in% global_combos)
  } else {
    global_fcst_tbl <- tibble::tibble()
  }

  # load local model forecasts
  if ("local" %in% model_type_list) {
    local_run_tbl <- best_run_tbl %>%
      dplyr::filter(model_type == "local")

    par_info <- par_start(
      run_info = agent_info$project_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = nrow(local_run_tbl)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # submit tasks
    local_fcst_tbl <- foreach::foreach(
      x = local_run_tbl %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .packages = packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op%
      {
        run_info <- agent_info$project_info
        run_info$project_name <- paste0(
          agent_info$project_info$project_name,
          "_",
          hash_data(x$combo)
        )
        run_info$run_name <- x$best_run_name

        temp_local_fcst_tbl <- get_forecast_data(run_info = run_info)

        return(temp_local_fcst_tbl)
      } %>%
      base::suppressPackageStartupMessages()

    par_end(cl)
  } else {
    local_fcst_tbl <- tibble::tibble()
  }

  return(global_fcst_tbl %>% dplyr::bind_rows(local_fcst_tbl))
}

#' Get the best run for an agent
#'
#' This function retrieves the best run information for a Finn agent after the forecast iteration process is complete.
#' 
#' @param agent_info Agent info from `set_agent_info()`
#' @param full_run_info A logical indicating whether to load all input settings
#'  from each run into the final output table
#' @param parallel_processing Default of NULL runs no parallel processing and
#'  loads each time series forecast one after another. 'local_machine' leverages
#'  all cores on current machine Finn is running on. 'spark' runs time series
#'  in parallel on a spark cluster in Azure Databricks or Azure Synapse.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#'
#' @return A tibble containing the best run information for the agent.
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
#'   )
#'   
#' # set up LLM 
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'   
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#'  )
#'  
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#'  )
#' 
#' # get the best run information for the agent
#' best_run_info <- get_best_agent_run(agent_info = agent_info, full_run_info = TRUE)
#' }
#' @export
get_best_agent_run <- function(agent_info,
                               full_run_info = FALSE,
                               parallel_processing = NULL,
                               num_cores = NULL) {
  # metadata
  project_info <- agent_info$project_info

  # get the best run for the agent
  combo_best_run_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "*-agent_best_run.", project_info$data_output
    )
  )

  if (length(combo_best_run_list) == 0) {
    best_run_tbl <- tibble::tibble()
  } else {
    best_run_tbl <- read_file(
      run_info = project_info,
      file_list = combo_best_run_list,
      return_type = "df"
    )
  }

  if (!full_run_info) {
    return(best_run_tbl)
  }

  # read in all run setting data
  model_type_list <- best_run_tbl %>%
    dplyr::pull(model_type) %>%
    unique()

  # load global model run
  if ("global" %in% model_type_list) {
    global_run_name <- best_run_tbl %>%
      dplyr::filter(model_type == "global") %>%
      dplyr::pull(best_run_name) %>%
      unique()

    global_run_info <- get_run_info(
      project_name = paste0(agent_info$project_info$project_name, "_", hash_data("all")),
      run_name = global_run_name,
      storage_object = project_info$storage_object,
      path = project_info$path
    ) %>%
      dplyr::select(-project_name, -path, -data_output, -object_output, -weighted_mape)

    global_best_run_tbl <- best_run_tbl %>%
      dplyr::filter(model_type == "global") %>%
      dplyr::left_join(global_run_info, by = dplyr::join_by(best_run_name == run_name))
  } else {
    global_best_run_tbl <- tibble::tibble()
  }

  # load local model runs
  if ("local" %in% model_type_list) {
    local_run_tbl <- best_run_tbl %>%
      dplyr::filter(model_type == "local")
    
    local_run_combo_list <- local_run_tbl %>%
      dplyr::pull(combo)
    
    # agent adjustments to prevent serialization issues
    agent_info_lean <- agent_info
    agent_info_lean$driver_llm <- NULL
    agent_info_lean$reason_llm <- NULL
    
    # run parallel process
    par_info <- par_start(
      run_info = agent_info$project_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = length(local_run_combo_list)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # submit tasks
    local_best_run_tbl <- foreach::foreach(
      combo = local_run_combo_list,
      .packages = packages,
      .errorhandling = "stop",
      .combine = "rbind",
      .inorder = FALSE,
      .multicombine = TRUE
    ) %op%
      {
        temp_local_run_tbl <- read_file(
          run_info = agent_info_lean$project_info,
          file_list = paste0(
            agent_info_lean$project_info$path, "/logs/", 
            hash_data(agent_info_lean$project_info$project_name), "-",
            hash_data(agent_info_lean$run_id), "-",
            hash_data(combo), "-",
            "agent_best_run.csv"
          ) %>% fs::path_tidy()
        )
        
        if(nrow(temp_local_run_tbl) == 0) {
          stop("Can't find previous best run for time series.")
        }

        temp_local_run_info <- get_run_info(
          project_name = paste0(agent_info_lean$project_info$project_name, "_", hash_data(combo)),
          run_name = temp_local_run_tbl$best_run_name,
          storage_object = agent_info_lean$project_info$storage_object,
          path = agent_info_lean$project_info$path
        ) %>%
          dplyr::select(-project_name, -path, -data_output, -object_output, -weighted_mape)

        temp_local_run_tbl <- temp_local_run_tbl %>%
          dplyr::left_join(temp_local_run_info, by = dplyr::join_by(best_run_name == run_name))
        
        return(temp_local_run_tbl)
      } %>%
      base::suppressPackageStartupMessages()

    par_end(cl)
  } else {
    local_best_run_tbl <- tibble::tibble()
  }

  final_run_tbl <- global_best_run_tbl %>%
    dplyr::bind_rows(local_best_run_tbl)

  return(final_run_tbl)
}

#' Forecast Agent Workflow
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param weighted_mape_goal A numeric value representing the goal for the weighted MAPE.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#' @param max_iter Maximum number of iterations for the workflow. Default is 3.
#' @param seed Random seed for reproducibility. Default is 123.
#'
#' @return A list containing the results of the workflow.
#' @noRd
fcst_agent_workflow <- function(agent_info,
                                combo,
                                weighted_mape_goal,
                                parallel_processing,
                                inner_parallel,
                                num_cores,
                                max_iter = 3,
                                seed = 123) {
  # create a fresh session for the reasoning LLM
  if (!is.null(agent_info$reason_llm)) {
    agent_info$reason_llm <- agent_info$reason_llm$clone()
  }
  
  # add LLM system prompt for EDA info
  agent_info$reason_llm <- agent_info$reason_llm$set_system_prompt(iterate_forecast_system_prompt(agent_info = agent_info,
                                                                                                  combo = combo,
                                                                                                  weighted_mape_goal = weighted_mape_goal))

  # create a timestamp for the run
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

  # construct the workflow
  workflow <- list(
    start = list(
      fn = "reason_inputs",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        combo = combo,
        weighted_mape_goal = weighted_mape_goal,
        last_error = NULL
      ),
      branch = function(ctx) {
        # extract the results from the current node
        results <- ctx$results

        # check if the LLM aborted the run
        if ("abort" %in% names(results$reason_inputs) && results$reason_inputs$abort == "TRUE") {
          return(list(ctx = ctx, `next` = "stop"))
        } else {
          return(list(ctx = ctx, `next` = "submit_fcst_run"))
        }
      }
    ),
    submit_fcst_run = list(
      fn = "submit_fcst_run",
      `next` = "get_fcst_output",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info          = agent_info,
        inputs              = "{results$reason_inputs}",
        combo               = combo,
        timestamp           = paste0(timestamp, "_", "{ctx$iter}"),
        parallel_processing = parallel_processing,
        inner_parallel      = inner_parallel,
        num_cores           = num_cores,
        seed                = seed
      )
    ),
    get_fcst_output = list(
      fn = "get_fcst_output",
      `next` = "calculate_fcst_metrics",
      retry_mode = "plain",
      max_retry = 3,
      args = list(run_info = "{results$submit_fcst_run}")
    ),
    calculate_fcst_metrics = list(
      fn = "calculate_fcst_metrics",
      `next` = "log_best_run",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        run_info  = "{results$submit_fcst_run}",
        fcst_tbl  = "{results$get_fcst_output}"
      )
    ),
    log_best_run = list(
      fn = "log_best_run",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        run_info = "{results$submit_fcst_run}",
        weighted_mape = "{results$calculate_fcst_metrics}",
        combo = combo
      ),
      branch = function(ctx) {
        # test if max run iterations have been reached
        ctx$iter <- ctx$iter + 1
        max_runs_reached <- ctx$iter >= ctx$max_iter

        cli::cli_alert_info(
          "Forecast Iteration {ctx$iter}/{ctx$max_iter} Complete"
        )

        # check if the weighted MAPE is below the goal
        weighted_mape <- ctx$results$calculate_fcst_metrics

        if (weighted_mape < weighted_mape_goal) {
          cli::cli_alert_success(
            "Weighted MAPE goal of {round(weighted_mape_goal * 100, 2)}% achieved! Latest weighted MAPE is {round(weighted_mape * 100, 2)}%. Stopping iterations."
          )
          wmape_goal_reached <- TRUE
        } else if (max_runs_reached) {
          cli::cli_alert_info(
            "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Stopping iterations as max runs is reached."
          )
          wmape_goal_reached <- FALSE
        } else {
          cli::cli_alert_info(
            "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Continuing to next iteration."
          )
          wmape_goal_reached <- FALSE
        }

        # determine next node based on conditions
        if (wmape_goal_reached || max_runs_reached) {
          next_node <- "stop"
        } else {
          next_node <- "start"
        }

        return(list(ctx = ctx, `next` = next_node))
      }
    ),
    stop = list(fn = NULL)
  )

  init_ctx <- list(
    node      = "start",
    iter      = 0, # iteration counter
    max_iter  = max_iter, # loop limit
    results   = list(), # where each tool's output will be stored
    attempts  = list() # retry bookkeeping for execute_node()
  )

  # run the graph
  run_graph(agent_info$driver_llm, workflow, init_ctx)
}

#' Register Finn forecasting tools for the agent
#'
#' @param agent_info A list containing agent information including driver LLM and project info.
#'
#' @return NULL
#' @noRd
register_fcst_tools <- function(agent_info) {
  # workflows
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "fcst_agent_workflow",
    .description = "Run the Finn forecasting agent workflow",
    .fun = fcst_agent_workflow
  ))

  # individual tools
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "reason_inputs",
    .description = "Reason about the best inputs for Finn forecast run",
    .fun = reason_inputs
  ))

  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "submit_fcst_run",
    .description = "Submit a Finn forecasting run with the given inputs",
    .fun = submit_fcst_run
  ))

  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "get_fcst_output",
    .description = "Get the forecast output from a Finn run",
    .fun = get_fcst_output
  ))

  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "calculate_fcst_metrics",
    .description = "Calculate back test accuracy metrics from the forecast output",
    .fun = calculate_fcst_metrics
  ))

  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "log_best_run",
    .description = "Log the best run from a Finn forecasting run",
    .fun = log_best_run
  ))
}

#' Generate Finn run inputs for the reasoning LLM
#'
#' @param agent_info A list containing agent information including LLMs, project info, and other parameters.
#' @param combo A character string representing the combo variables, or NULL for global model.
#' @param weighted_mape_goal A numeric value representing the weighted MAPE goal.
#' @param last_error A character string representing the last error message, or NULL if no errors.
#'
#' @return A list containing the LLM response and the parsed JSON object.
#' @noRd
reason_inputs <- function(agent_info,
                          combo = NULL,
                          weighted_mape_goal,
                          last_error = NULL) {
  # get metadata
  if (!is.null(agent_info$reason_llm)) {
    llm <- agent_info$reason_llm
  } else {
    llm <- agent_info$driver_llm
  }

  project_info <- agent_info$project_info

  # load previous run results
  previous_run_results <- load_run_results(agent_info = agent_info, combo = combo)
  total_runs <- get_total_run_count(agent_info, combo = combo)

  if (is.data.frame(previous_run_results)) {
    best_mape <- previous_run_results %>%
      dplyr::filter(best_run == "yes") %>%
      dplyr::pull(weighted_mape) %>%
      unique(round(4))

    best_run <- previous_run_results %>%
      dplyr::filter(best_run == "yes") %>%
      dplyr::pull(run_number)

    lag_period_changes <- previous_run_results %>%
      dplyr::select(lag_periods) %>%
      tidyr::drop_na(lag_periods) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      length()

    lag_changes_allowed <- lag_period_changes < 3

    rolling_window_changes <- previous_run_results %>%
      dplyr::select(rolling_window_periods) %>%
      tidyr::drop_na(rolling_window_periods) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      length()

    rolling_changes_allowed <- rolling_window_changes < 3
  } else {
    best_mape <- "NA"
    best_run <- "NA"
    lag_changes_allowed <- TRUE
    rolling_changes_allowed <- TRUE
  }

  # create final prompt
  final_prompt <- glue::glue(
    '
      -----LATEST CONTEXT-----
      Leverage all previous and current info provided to recommend the best inputs to create the most accurate forecast as possible. 

      -----LATEST METADATA-----
      - run count : <<run_count>>
      - best weighted MAPE from previous runs : <<best_mape>>
      - best run number from previous runs : <<best_run>>
      - lag_changes_allowed: <<lag_changes>>
      - rolling_changes_allowed changes: <<rolling_changes>>

      -----PREVIOUS RUN RESULTS-----
      <<run_results>>

      -----LAST ERROR-----
      <<last_error>>

      -----END OUTPUT-----',
    .open = "<<", .close = ">>",
    run_results = make_pipe_table(previous_run_results),
    run_count = total_runs,
    best_mape = best_mape,
    best_run = best_run,
    lag_changes = lag_changes_allowed,
    rolling_changes = rolling_changes_allowed,
    last_error = ifelse(is.null(last_error), "No errors in previous input recommendation.", last_error),
    agent_version = agent_info$agent_version
  )

  # send prompt to LLM
  response <- llm$chat(final_prompt, echo = FALSE)

  # extract out json from response and convert to list
  input_list <- extract_json_object(response)

  # check if the response is an abort schema
  if ("abort" %in% names(input_list) && input_list$abort == "TRUE") {
    cli::cli_alert_info("LLM has aborted the run. Reason: {input_list$reasoning}")
    return(input_list)
  }

  # force specific inputs if single time series
  if (!is.null(combo)) {
    input_list$forecast_approach <- "bottoms_up"
  }

  # check if all required fields are present
  required_fields <- c(
    "models_to_run", "external_regressors", "clean_missing_values",
    "clean_outliers", "negative_forecast", "forecast_approach",
    "stationary", "feature_selection", "multistep_horizon",
    "seasonal_period", "recipes_to_run", "lag_periods",
    "rolling_window_periods", "reasoning"
  )

  missing_fields <- setdiff(required_fields, names(input_list))

  # warn if any required fields are missing
  if (length(missing_fields) > 0 & !is.null(combo)) {
    cli::cli_alert_warning(
      "Missing fields in LLM response: {paste(missing_fields, collapse = ', ')}. Using default values."
    )
  }

  # fill in missing fields with default values
  if (is.null(combo)) {
    default_values <- list(
      models_to_run = "xgboost",
      external_regressors = "NULL",
      clean_missing_values = TRUE,
      clean_outliers = FALSE,
      negative_forecast = FALSE,
      forecast_approach = "bottoms_up",
      stationary = TRUE,
      feature_selection = TRUE,
      multistep_horizon = TRUE,
      seasonal_period = "NULL",
      recipes_to_run = "R1",
      lag_periods = "NULL",
      rolling_window_periods = "NULL",
      reasoning = "no reasoning provided"
    )
  } else {
    default_values <- list(
      models_to_run = "arima---ets---tbats---stlm-arima---xgboost---glmnet",
      external_regressors = "NULL",
      clean_missing_values = TRUE,
      clean_outliers = FALSE,
      negative_forecast = FALSE,
      forecast_approach = "bottoms_up",
      stationary = FALSE,
      feature_selection = TRUE,
      multistep_horizon = FALSE,
      seasonal_period = "NULL",
      recipes_to_run = "R1",
      lag_periods = "NULL",
      rolling_window_periods = "NULL",
      reasoning = "no reasoning provided"
    )
  }

  for (field in required_fields) {
    if (!(field %in% names(input_list))) {
      input_list[[field]] <- default_values[[field]]
    }
  }

  # format the list to ensure correct types
  input_list$models_to_run <- strsplit(input_list$models_to_run, "---")[[1]]
  input_list$external_regressors <- if (input_list$external_regressors == "NULL") {
    "NULL"
  } else {
    strsplit(input_list$external_regressors, "---")[[1]]
  }
  input_list$clean_missing_values <- as.logical(input_list$clean_missing_values)
  input_list$clean_outliers <- as.logical(input_list$clean_outliers)
  input_list$negative_forecast <- as.logical(input_list$negative_forecast)
  input_list$forecast_approach <- as.character(input_list$forecast_approach)
  input_list$stationary <- as.logical(input_list$stationary)
  input_list$feature_selection <- as.logical(input_list$feature_selection)
  input_list$multistep_horizon <- as.logical(input_list$multistep_horizon)
  input_list$seasonal_period <- if (input_list$seasonal_period == "NULL") {
    "NULL"
  } else {
    as.numeric(strsplit(input_list$seasonal_period, "---")[[1]])
  }
  input_list$recipes_to_run <- if (input_list$recipes_to_run == "NULL") {
    "NULL"
  } else {
    strsplit(input_list$recipes_to_run, "---")[[1]]
  }
  input_list$lag_periods <- if (input_list$lag_periods == "NULL") {
    "NULL"
  } else {
    as.numeric(strsplit(input_list$lag_periods, "---")[[1]])
  }
  input_list$rolling_window_periods <- if (input_list$rolling_window_periods == "NULL") {
    "NULL"
  } else {
    as.numeric(strsplit(input_list$rolling_window_periods, "---")[[1]])
  }

  # make sure xregs look correct
  if (length(input_list$external_regressors) > 1) {
    # check to see if there are any regressors that are not in the project info
    if (any(!input_list$external_regressors %in% agent_info$external_regressors)) {
      stop(
        sprintf(
          "External regressors %s are not in the agent info.",
          paste(setdiff(input_list$external_regressors, agent_info$external_regressors), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else if (input_list$external_regressors != "NULL") {
    # if there is only one regressor, check if it is NULL or in the project info
    if (!(input_list$external_regressors %in% c("NULL", agent_info$external_regressors))) {
      stop(
        sprintf(
          "External regressor %s is not in the agent info.",
          input_list$external_regressors
        ),
        call. = FALSE
      )
    }
  }

  # check if these inputs were used before in previous runs
  previous_runs_tbl <- load_run_results(agent_info = agent_info, combo = combo)

  if (is.data.frame(previous_runs_tbl)) {
    check_inputs <- input_list
    check_inputs$agent_version <- agent_info$agent_version
    check_inputs$models_to_run <- collapse_or_na(check_inputs$models_to_run)
    check_inputs$external_regressors <- collapse_or_na(check_inputs$external_regressors)
    check_inputs$recipes_to_run <- collapse_or_na(check_inputs$recipes_to_run)
    check_inputs$lag_periods <- collapse_or_na(check_inputs$lag_periods)
    check_inputs$rolling_window_periods <- collapse_or_na(check_inputs$rolling_window_periods)
    check_inputs$seasonal_period <- collapse_or_na(check_inputs$seasonal_period)
    check_inputs <- check_inputs[names(check_inputs) %in% names(previous_runs_tbl)]

    cols_to_fix <- c(
      "models_to_run",
      "external_regressors",
      "recipes_to_run",
      "lag_periods",
      "rolling_window_periods",
      "seasonal_period"
    )

    previous_runs_tbl <- previous_runs_tbl %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(intersect(names(previous_runs_tbl), cols_to_fix)),
          base::as.character
        )
      )

    if (does_param_set_exist(check_inputs, previous_runs_tbl)) {
      cli::cli_alert_info("The proposed input parameters have already been used in a previous run.")

      stop(
        sprintf(
          "Duplicate parameter set detected. Please modify the inputs or ABORT. Inputs proposed:\n%s",
          jsonlite::toJSON(check_inputs, auto_unbox = TRUE)
        ),
        call. = FALSE
      )
    }

    # check if the proposed inputs violate the lag and rolling window change rules
    if (length(unique(c(previous_runs_tbl$lag_periods, check_inputs$lag_periods))) > 4) {
      cli::cli_alert_info("Cannot propose more than 3 unique lag period changes across all runs.")
      stop("Cannot propose more than 3 unique lag period changes across all runs. Stop modifying lag_periods or ABORT.",
        call. = FALSE
      )
    }

    if (length(unique(c(previous_runs_tbl$rolling_window_periods, check_inputs$rolling_window_periods))) > 4) {
      cli::cli_alert_info("Cannot propose more than 3 unique rolling window period changes across all runs.")
      stop("Cannot propose more than 3 unique rolling window period changes across all runs. Stop modifying rolling_window_periods or ABORT.",
        call. = FALSE
      )
    }
  }

  return(input_list)
}

#' Submit a Finn forecasting run
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param inputs A list of inputs for the forecasting run.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param timestamp A timestamp for the run, used to create a unique run name.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#' @param seed An integer seed for reproducibility. Default is 123.
#'
#' @return A list containing the run information including project name, run name, storage object, path, data output, and object output.
#' @noRd
submit_fcst_run <- function(agent_info,
                            inputs,
                            combo,
                            timestamp,
                            parallel_processing = NULL,
                            inner_parallel = FALSE,
                            num_cores = NULL,
                            seed = 123) {
  cli::cli_alert_info(
    "Starting Finn forecasting run with inputs: {jsonlite::toJSON(inputs, auto_unbox = TRUE)}"
  )

  # get metadata
  project_info <- agent_info$project_info
  back_test_scenarios <- agent_info$back_test_scenarios
  back_test_spacing <- agent_info$back_test_spacing

  # read all combos or just one
  if (is.null(combo)) {
    combo_value <- "*"
    global_models <- TRUE
    local_models <- FALSE
  } else {
    combo_value <- combo
    global_models <- FALSE
    local_models <- TRUE
  }

  # adjust parallel processing for combos
  if (!is.null(parallel_processing)) {
    if (!is.null(combo) & parallel_processing == "spark") {
      # turn off parallel processing when running single combo
      parallel_processing <- NULL
      prep_parallel <- NULL
    } else if (is.null(combo) & parallel_processing == "spark") {
      # local parallel process instead of spark on global models
      parallel_processing <- NULL
      prep_parallel <- "local_machine"
    } else {
      prep_parallel <- parallel_processing
    }
  } else {
    prep_parallel <- parallel_processing
  }

  # get input data
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

  # create unique run name and project name
  run_name <- paste0(
    "agent_",
    agent_info$run_id, "_",
    ifelse(is.null(combo), hash_data("all"), combo_value), "_",
    timestamp
  )

  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(is.null(combo), hash_data("all"), combo_value)
  )

  # kick off Finn run
  run_info <- set_run_info(
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
    run_info = run_info,
    input_data = input_data,
    combo_variables = project_info$combo_variables,
    target_variable = "Target",
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    external_regressors = null_converter(inputs$external_regressors),
    hist_start_date = agent_info$hist_start_date,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = agent_info$combo_cleanup_date,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = inputs$clean_missing_values,
    clean_outliers = inputs$clean_outliers,
    box_cox = FALSE,
    stationary = inputs$stationary,
    forecast_approach = inputs$forecast_approach,
    parallel_processing = prep_parallel,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = null_converter(inputs$lag_periods),
    rolling_window_periods = null_converter(inputs$rolling_window_periods),
    recipes_to_run = null_converter(inputs$recipes_to_run),
    multistep_horizon = inputs$multistep_horizon
  )

  # prepare models for training
  prep_models(
    run_info = run_info,
    back_test_scenarios = back_test_scenarios,
    back_test_spacing = back_test_spacing,
    models_to_run = null_converter(inputs$models_to_run),
    models_not_to_run = NULL,
    run_ensemble_models = FALSE,
    pca = NULL,
    num_hyperparameters = 10,
    seasonal_period = null_converter(inputs$seasonal_period)
  )

  # train models
  train_models(
    run_info = run_info,
    run_global_models = global_models,
    run_local_models = local_models,
    global_model_recipes = c("R1"),
    feature_selection = inputs$feature_selection,
    negative_forecast = inputs$negative_forecast,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    seed = seed
  )

  # evaluate models
  final_models(
    run_info = run_info,
    average_models = TRUE,
    max_model_average = 3,
    weekly_to_daily = project_info$weekly_to_daily,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores
  )

  return(run_info)
}

#' Get forecast output from a Finn run
#'
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#'
#' @return A tibble containing the forecast output.
#' @noRd
get_fcst_output <- function(run_info) {
  fcst_tbl <- get_forecast_data(run_info)

  return(fcst_tbl)
}

#' Calculate forecast metrics from a Finn run
#'
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#' @param fcst_tbl A tibble containing the forecast output.
#'
# @return A numeric value representing the weighted MAPE of the forecast.
#' @noRd
calculate_fcst_metrics <- function(run_info,
                                   fcst_tbl) {
  # get weighted mape from run logging
  run_log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  weighted_mape <- run_log_df$weighted_mape

  if (is.null(weighted_mape)) {
    stop("No weighted MAPE found in run log. Ensure the run was completed successfully.", call. = FALSE)
  }

  return(weighted_mape)
}

#' Log the best run results
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#' @param weighted_mape A numeric value representing the weighted MAPE of the forecast.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#'
#' @return NULL
#' @noRd
log_best_run <- function(agent_info,
                         run_info,
                         weighted_mape,
                         combo = NULL) {
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # update the run log file with additional model accuracy information
  if (!is.null(combo)) {
    model_back_test_tbl <- get_forecast_data(run_info = run_info) %>%
      dplyr::filter(
        Run_Type == "Back_Test",
        Recipe_ID != "simple_average"
      )

    # calculate weighted mape by Model_ID
    model_wmape_tbl <- model_back_test_tbl %>%
      dplyr::mutate(
        Target = ifelse(Target == 0, 0.1, Target)
      ) %>%
      dplyr::group_by(Model_ID) %>%
      dplyr::mutate(
        MAPE = round(abs((Forecast - Target) / Target), digits = 4),
        Total = sum(Target, na.rm = TRUE),
        Weight = (MAPE * Target) / Total
      ) %>%
      dplyr::summarise(weighted_mape = sum(Weight, na.rm = TRUE)) %>%
      dplyr::ungroup()

    avg_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      mean()

    median_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      median()

    std_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      sd()

    # load log file
    log_df <- get_run_info(
      project_name = run_info$project_name,
      run_name = run_info$run_name,
      storage_object = run_info$storage_object,
      path = run_info$path
    ) %>%
      dplyr::mutate(
        model_avg_wmape = avg_wmape,
        model_median_wmape = median_wmape,
        model_std_wmape = std_wmape,
        agent_version = as.numeric(agent_info$agent_version),
        agent_forecast_approach = agent_info$forecast_approach
      )

    # save the log file
    write_data(
      x = log_df,
      combo = NULL,
      run_info = run_info,
      output_type = "log",
      folder = "logs",
      suffix = NULL
    )
  } else {
    # load log file
    log_df <- get_run_info(
      project_name = run_info$project_name,
      run_name = run_info$run_name,
      storage_object = run_info$storage_object,
      path = run_info$path
    ) %>%
      dplyr::mutate(
        model_avg_wmape = weighted_mape,
        model_median_wmape = weighted_mape,
        model_std_wmape = 0,
        agent_version = as.numeric(agent_info$agent_version),
        agent_forecast_approach = agent_info$forecast_approach
      )

    # save the log file
    write_data(
      x = log_df,
      combo = NULL,
      run_info = run_info,
      output_type = "log",
      folder = "logs",
      suffix = NULL
    )
  }

  # check if previous best run exists and is more accurate
  previous_runs <- load_run_results(agent_info = agent_info, combo = combo)

  if (is.data.frame(previous_runs) && nrow(previous_runs) > 1) {
    # see if latest run was the best run
    last_run <- previous_runs %>%
      dplyr::filter(run_number == max(run_number))

    if (last_run$best_run == "yes") {
      log_results <- TRUE
    } else {
      log_results <- FALSE
    }
  } else {
    log_results <- TRUE
  }

  # log results if the new run is better or if no previous runs exist
  if (log_results) {
    if (is.null(combo)) {
      combo <- "all"
    }

    # get back test data
    back_test_tbl <- get_forecast_data(run_info = run_info) %>%
      dplyr::filter(
        Best_Model == "Yes",
        Run_Type == "Back_Test"
      )

    combo_list <- unique(back_test_tbl$Combo)

    # for each combo, filter the back test data, calculate the weighted mape and save to logs
    for (combo_name in combo_list) {
      # filter the back test data for the current combo
      combo_data <- back_test_tbl %>%
        dplyr::filter(Combo == combo_name)

      # calculate mape and weight it by value of target colum
      wmape <- combo_data %>%
        dplyr::mutate(
          Target = ifelse(Target == 0, 0.1, Target)
        ) %>%
        dplyr::mutate(
          MAPE = round(abs((Forecast - Target) / Target), digits = 4),
          Total = sum(Target, na.rm = TRUE),
          Weight = (MAPE * Target) / Total
        ) %>%
        dplyr::pull(Weight) %>%
        sum(na.rm = TRUE) %>%
        round(digits = 4)

      # check if previous log file exists, if so check if mape is better
      prev_log_df <- tryCatch(
        read_file(project_info,
          file_list = paste0(
            project_info$path,
            "/logs/", hash_data(project_info$project_name), "-", hash_data(agent_info$run_id),
            "-", hash_data(combo_name), "-agent_best_run.csv"
          ) %>% fs::path_tidy(),
          return_type = "df"
        ),
        error = function(e) {
          tibble::tibble()
        }
      ) %>%
        base::suppressWarnings()

      if (nrow(prev_log_df) > 0) {
        prev_wmape <- as.numeric(prev_log_df$weighted_mape)
        prev_model_type <- prev_log_df$model_type
      } else {
        prev_wmape <- Inf # if no previous log, assume previous was worse
        prev_model_type <- "global" # assume global model if no previous log
      }

      if (wmape < prev_wmape || (combo == "all" & prev_model_type == "global")) { # always log the wmape for global models with no local best run, but check for local models

        # log the best run
        log_df <- tibble::tibble(
          project_name = agent_info$project_info$project_name,
          agent_run_id = agent_info$run_id,
          best_run_name = run_info$run_name,
          model_type = ifelse(combo == "all", "global", "local"),
          combo = combo_name,
          weighted_mape = wmape
        )

        write_data(
          x = log_df,
          combo = combo_name,
          run_info = project_info,
          output_type = "log",
          folder = "logs",
          suffix = "-agent_best_run"
        )
      } else {
        next # don't log if the previous run was better
      }
    }
  }
  return("Run logged successfully.")
}

#' Load previous run results for the agent
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#'
#' @return A tibble containing the previous run results or a message indicating no previous runs.
#' @noRd
load_run_results <- function(agent_info,
                             combo = NULL) {
  # determine the combo value and columns to return
  if (is.null(combo)) {
    combo_value <- hash_data("all")

    column_list <- c(
      "agent_version", "run_number", "best_run", "weighted_mape",
      "external_regressors", "clean_missing_values", "clean_outliers",
      "stationary", "forecast_approach",
      "lag_periods", "rolling_window_periods",
      "multistep_horizon", "feature_selection",
      "negative_forecast"
    )
  } else {
    combo_value <- combo

    column_list <- c(
      "agent_version", "run_number", "best_run", "weighted_mape",
      "external_regressors", "clean_missing_values", "clean_outliers",
      "stationary", "box_cox", "forecast_approach",
      "lag_periods", "rolling_window_periods", "recipes_to_run",
      "multistep_horizon", "models_to_run", "pca",
      "seasonal_period", "num_hyperparameters", "feature_selection",
      "negative_forecast"
    )
  }

  # get previous runs
  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(is.null(combo), hash_data("all"), combo_value)
  )

  previous_runs <- get_run_info(
    project_name = project_name,
    run_name = NULL,
    storage_object = agent_info$project_info$storage_object,
    path = agent_info$project_info$path
  )

  # filter previous runs based on the combo value and select relevant columns
  if ("run_name" %in% names(previous_runs) & "weighted_mape" %in% names(previous_runs)) {
    pattern <- sprintf("^agent_[^_]+_%s.*", combo_value)

    previous_runs_formatted <- previous_runs %>%
      dplyr::filter(stringr::str_detect(run_name, pattern)) %>%
      dplyr::mutate(created = lubridate::ymd_hms(created, tz = "UTC")) %>%
      dplyr::arrange(created) %>%
      dplyr::filter(!is.na(weighted_mape)) %>%
      dplyr::filter(!is.na(agent_version)) %>%
      dplyr::filter(agent_forecast_approach == agent_info$forecast_approach) %>%
      dplyr::mutate(agent_run_id = stringr::str_extract(run_name, "agent_([^_]+)")) %>%
      dplyr::mutate(
        run_number = dplyr::row_number()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(agent_version, run_number, weighted_mape)

    # earliest row with *global* minimum weighted_mape for latest agent version
    earliest_min <- previous_runs_formatted %>%
      dplyr::filter(agent_version == max(agent_version)) %>%
      dplyr::filter(weighted_mape == min(weighted_mape)) %>% # all global-mins
      dplyr::slice(1) %>% # earliest one
      suppressWarnings()

    best_idx <- earliest_min$run_number
    best_wmape <- earliest_min$weighted_mape
    best_model <- earliest_min$model_avg_wmape

    # look **after** that for runs whose weighted_mape is within +-10 %
    # of the initial best and pick the *lowest* model_avg_wmape overall
    if ("model_avg_wmape" %in% names(previous_runs_formatted)) {
      if (nrow(previous_runs_formatted) > 1) {
        cand <- previous_runs_formatted %>%
          dplyr::filter(
            run_number > best_idx, # later runs only
            abs(weighted_mape - best_wmape) <= best_wmape * 0.10 # within +-10 %
          )

        if (nrow(cand)) {
          cand <- cand %>%
            dplyr::filter(model_avg_wmape == min(model_avg_wmape)) %>% # lowest avg
            dplyr::slice(1) # earliest tie
          if (cand$model_avg_wmape < best_model) { # strictly better than current
            best_idx <- cand$run_number
          }
        }
      }
    }

    # flag the chosen best run
    previous_runs_formatted <- previous_runs_formatted %>%
      dplyr::mutate(
        best_run = dplyr::if_else(run_number == best_idx, "yes", "no")
      )

    if (nrow(previous_runs_formatted) == 0) {
      run_output <- "No Previous Runs"
    } else if ("model_avg_wmape" %in% names(previous_runs_formatted)) {
      run_output <- previous_runs_formatted %>%
        dplyr::select(tidyselect::all_of(c(column_list, "model_avg_wmape", "model_median_wmape", "model_std_wmape"))) %>%
        dplyr::relocate(agent_version, run_number, best_run, weighted_mape, model_avg_wmape, model_median_wmape, model_std_wmape)
    } else {
      run_output <- previous_runs_formatted %>%
        dplyr::select(tidyselect::all_of(column_list)) %>%
        dplyr::relocate(agent_version, run_number, best_run, weighted_mape)
    }
  } else {
    run_output <- "No Previous Runs"
  }

  return(run_output)
}

#' Get the total run count for the agent
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#'
#' @return A numeric value representing the total run count for the agent.
#' @noRd
get_total_run_count <- function(agent_info,
                                combo = NULL) {
  # determine the combo value
  if (is.null(combo)) {
    combo_value <- hash_data("all")
  } else {
    combo_value <- combo
  }

  # get total runs
  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(is.null(combo), hash_data("all"), combo_value)
  )

  total_runs <- get_run_info(
    project_name = project_name,
    run_name = NULL,
    storage_object = agent_info$project_info$storage_object,
    path = agent_info$project_info$path
  )

  # filter total runs based on the combo value
  if ("run_name" %in% names(total_runs) & "weighted_mape" %in% names(total_runs)) {
    total_runs <- total_runs %>%
      dplyr::filter(stringr::str_starts(run_name, paste0("agent_", agent_info$run_id, "_", combo_value))) %>%
      dplyr::filter(!is.na(weighted_mape)) %>%
      dplyr::filter(!is.na(agent_version))
  } else {
    total_runs <- tibble::tibble()
  }

  return(nrow(total_runs))
}

#' Collapse a vector into a string with "---" as separator, or return NA if the vector is "NULL"
#'
#' @param x A character vector to collapse.
#'
#' @return A character string with elements collapsed by "---", or NA if the input is "NULL".
#' @noRd
collapse_or_na <- function(x) {
  if (length(x) == 1 && identical(x, "NULL")) {
    NA_character_
  } else {
    paste(x, collapse = "---")
  }
}

#' Apply column types from a template dataframe to a target dataframe
#'
#' @param target_df The target dataframe to apply types to.
#' @param template_df The template dataframe containing the desired column types.
#' @param drop_extra Logical indicating whether to drop columns in the target that are not in the template.
#' @param reorder Logical indicating whether to reorder columns in the target to match the template.
#'
# @return The target dataframe with applied column types.
#' @noRd
apply_column_types <- function(target_df,
                               template_df,
                               drop_extra = FALSE,
                               reorder = FALSE) {
  # get common columns between template and target
  common_cols <- intersect(names(template_df), names(target_df))

  # function to cast a vector to the type of a template column
  cast_col <- function(vec, template) {
    if (inherits(template, "factor")) {
      factor(vec, levels = levels(template))
    } else if (inherits(template, "Date")) {
      as.Date(vec)
    } else if (inherits(template, "POSIXct")) {
      as.POSIXct(vec, tz = attr(template, "tzone") %||% "UTC")
    } else if (inherits(template, "POSIXlt")) {
      as.POSIXlt(vec, tz = attr(template, "tzone") %||% "UTC")
    } else if (is.integer(template)) {
      as.integer(vec)
    } else if (is.numeric(template)) {
      as.numeric(vec)
    } else if (is.logical(template)) {
      as.logical(vec)
    } else if (is.character(template)) {
      as.character(vec)
    } else {
      vec # leave as is for other classes (lists, df columns, etc.)
    }
  }

  # coerce all common columns
  for (col in common_cols) {
    target_df[[col]] <- cast_col(target_df[[col]], template_df[[col]])
  }

  # drop or reorder columns if requested
  if (drop_extra) {
    target_df <- target_df[common_cols]
  } else if (reorder) {
    target_df <- dplyr::relocate(target_df, tidyselect::all_of(common_cols),
      .after = dplyr::last_col()
    ) %>%
      dplyr::relocate(tidyselect::all_of(common_cols), .before = 1)
  }

  return(target_df)
}


#' Check if a parameter set exists in a previous run results dataframe
#'
#' @param x A list or atomic vector containing the parameter set to check.
#' @param df A dataframe containing previous run results with named columns.
#'
#' @return TRUE if the parameter set exists in the dataframe, FALSE otherwise.
#' @noRd
does_param_set_exist <- function(x, df) {
  # ensure x is a named list or atomic vector
  stopifnot(
    is.list(x) || is.atomic(x),
    !is.null(names(x)),
    all(names(x) %in% names(df))
  )

  # transform x into a 1-row tibble with matching column types
  probe <- x %>%
    tibble::as_tibble() %>% # 1-row tibble with same names as df
    apply_column_types(template_df = df) # ensure types match

  # check if the probe matches any row in df
  final_df <- df %>%
    dplyr::select(tidyselect::all_of(names(probe))) # select only matching cols

  dplyr::semi_join(final_df, probe, by = names(probe)) %>% # keep rows that match
    nrow() > 0 # TRUE if >=1 match
}

#' Extract JSON object from raw text
#'
#' @param raw_text A character string containing the raw text with JSON object.
#'
#' @return A parsed JSON object as a list.
#' @noRd
extract_json_object <- function(raw_text) {
  # format check
  if (!is.character(raw_text) || length(raw_text) != 1) {
    stop("`raw_text` must be a single character string.", call. = FALSE)
  }

  # locate the first {...} block
  json_block <- raw_text %>%
    stringr::str_remove_all("(?s)```json\\s*|```") %>% # strip code-fences
    stringr::str_extract("(?s)\\{.*?\\}") # non-greedy first brace

  if (is.na(json_block) || json_block == "") {
    stop("No JSON object found in `raw_text`.", call. = FALSE)
  }

  # parse JSON with error trap
  result <- tryCatch(
    jsonlite::fromJSON(json_block),
    error = function(e) {
      stop(
        sprintf("Failed to parse JSON object: %s", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  return(result)
}

#' Convert NULL string to NULL value
#'
#' @param x A character string that may be "NULL" or a vector.
#'
#' @return NULL if "NULL", otherwise returns the input vector.
#' @noRd
null_converter <- function(x) {
  if (length(x) > 1) {
    return(x)
  } else if (x == "NULL") {
    return(NULL)
  } else {
    return(x)
  }
}

#' Create the system prompt for the forecasting agent
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param weighted_mape_goal A numeric value representing the target weighted MAPE goal for the agent.
#'
#' @return A character string containing the system prompt for the agent.
#' @noRd
iterate_forecast_system_prompt <- function(agent_info,
                                           combo = NULL,
                                           weighted_mape_goal) {
  
  # get metadata
  project_info <- agent_info$project_info
  combo_str <- paste(project_info$combo_variables, collapse = "---")
  xregs_str <- paste(agent_info$external_regressors, collapse = "---")
  xregs_length <- length(agent_info$external_regressors)
  
  # get NULL defaults
  lag_default <- get_lag_periods(
    lag_periods = NULL,
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    multistep_horizon = TRUE,
    feature_engineering = TRUE
  ) %>%
    paste(collapse = "---")
  
  rolling_default <- get_rolling_window_periods(
    rolling_window_periods = NULL,
    date_type = project_info$date_type
  ) %>%
    paste(collapse = "---")
  
  recipe_default <- get_recipes_to_run(
    recipes_to_run = NULL,
    date_type = project_info$date_type
  ) %>%
    paste(collapse = "---")
  
  seasonal_period_default <- get_seasonal_periods(date_type = project_info$date_type) %>%
    paste(collapse = "---")
  
  # load EDA results
  eda_results <- load_eda_results(agent_info = agent_info, combo = combo)
  
  # create final prompt
  if (is.null(combo)) {
    # global model prompt
    final_prompt <- glue::glue(
      '
      -----CONTEXT-----
      You are an autonomous time-series forecasting agent. Your goal is to choose
      Finn-API parameters that **lower weighted MAPE** versus all prior runs.

      -----INITIAL METADATA-----
      - combos : <<combo>>
      - target : <<target>>
      - date type : <<dtype>>
      - hist end date : <<hist_end>>
      - forecast horizon : <<horizon>>
      - potential external regressors : <<xregs>>
      - weighted MAPE goal : <<weighted_mape_goal>>

      -----Exploratory Data Analysis-----
      <<eda>>

      -----RULES (MUST / MUST NOT)-----
      1.  YOU MUST output exactly one JSON object matching the schema below.
      2.  YOU MUST include a "reasoning" field with <= 250 words.
      3.  RUN CHANGE RULES
          3-A.  IF changes made in the previous run reduced the weighted mape compared to the best run, keep them, otherwise revert back to previous best run.
          3-B.  AFTER the first run (run_count > 0), YOU MUST change at most ONE parameter per new run.
          3-C.  Reverting back to a previous best run AND changing ONE parameter from that best run counts as ONE parameter change.
          3-D.  You MUST NOT recommend the same set of parameters as a previous run. If you do you MUST ABORT.
      4.  IF data is not stationary then set stationary="TRUE".
      5.  IF EDA shows strong autocorrelation on periods less than the forecast horizon AND forecast horizon > 1 then set multistep_horizon="TRUE".
      6.  NEGATIVE FORECAST RULES
          6-A.  IF EDA shows significant amount of negative values then set negative_forecast="TRUE".
          6-B.  IF no negative values are present then set negative_forecast="FALSE".
      7. HIERARCHICAL RULES
          7-A. IF run_count == 0 then set forecast_approach="bottoms_up"
          7-B. IF run_count > 0 AND hiearchy type != "none" AND *Step A is complete* then set forecast_approach="standard_hierarchy" or "grouped_hierarchy" depending on EDA results.
          7-C. IF hiearchy type == "none" then set forecast_approach="bottoms_up".
          7-D. You MUST NOT use "standard_hierarchy" or "grouped_hierarchy" if the hierarchy type is "none".
          7-E. You MUST NOT use "standard_hierarchy" if the hierarchy type is grouped.
          7-F. You MUST NOT use "grouped_hierarchy" if the hierarchy type is standard.
      8. MISSING VALUES RULES
          8-A. IF missing values are present AND run_count == 0 then set clean_missing_values="FALSE"
          8-B. IF missing values are present AND run_count > 0 AND *Step B is complete* then set clean_missing_values="TRUE"
          8-C. ALWAYS use "FALSE" if no missing values are detected
      9. OUTLIER RULES
          9-A. IF outliers are present AND run_count == 0 then set clean_outliers="FALSE"
          9-B. IF outliers are present AND run_count > 0 AND *Step C is complete* then set clean_outliers="TRUE"
          9-C. ALWAYS use "FALSE" if no outliers are detected
      10. EXTERNAL REGRESSOR RULES
          10-A. When choosing external_regressors, YOU MUST only select from the external regressors listed in the metadata. Separate multiple regressors with "---".
          10-B. IF adding external regressors AND run_count == 0 then set external_regressors="NULL"
          10-C  IF adding external regressors AND run_count > 0 AND *Step D is complete*, add ONLY ONE new external regressor variable per run.
          10-D. ALWAYS use "NULL" if no external regressors are needed.
          10-E. ALWAYS start with the most promising external regressors based on distance correlation results.
          10-F. ALWAYS set feature_selection="TRUE" if any external regressors are used.
          10-G. IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
          10-H. ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
          10-I. You are ONLY ALLOWED to change the external_regressors parameter a total of <<xregs_length>> times across all runs.
          10-J. IF using multiple external_regressors, you MUST NOT change the ordering of them compared to previous runs.
      11. FEATURE LAG RULES
          11-A. IF run_count == 0 then set lag_periods = "NULL"
          11-B. IF run_count > 0 AND *Step E is complete* -> use ACF and PCF results from EDA to select lag_periods.
          11-C. IF selecting lag periods to use, ALWAYS combine them using "---" separator.
          11-D. IF selecting lag periods less than the forecast horizon, ALWAYS set multistep_horizon="TRUE".
          11-E. IF lag_chages_allowed == FALSE, you MUST NOT make any new lag_periods changes.
          11-F. A value of "NULL" means that lags are defaulted to <<lag_default>>. Note that these default lags assume multistep_horizon="TRUE". If not the lags will be greater than or equal to forecast horizon.
          11-G. If using multiple lag_periods, you MUST NOT change the ordering of them compared to previous runs.
      12. ROLLING WINDOW LAGS
          12-A. IF run_count == 0 then set rolling_window_periods = "NULL"
          12-B. IF run_count > 0 AND *Step F is complete* then set rolling_window_periods = "NULL" or a list of periods separated by "---".
          12-C. IF rolling_changes_allowed == FALSE, you MUST NOT make any new rolling_window_periods changes.
          12-D. A value of "NULL" means that rolling window lags are defaulted to <<rolling_default>>.
          12-E. If using multiple rolling_window_periods, you MUST NOT change the ordering of them compared to previous runs.
      13. PREVIOUS VERSION REPLAY RULES
          13-A. IF run_count == 0 AND one or more *earlier agent versions* exist before current agent version of <<agent_version>>,
          YOU MUST first try the **full input set** from the best run of the most-recent previous agent version.
          13-B. IF that set has already been tried in the current version AND run_count < 4, choose the next best run, and so on.
          13-C. AFTER following 13-A AND 13-B, you MUST follow the decision tree below to propose a new set of parameters.
          13-D. Using the full input set from a previous run counts as ONE parameter change.
      14. An example value of "NULL|var1---var2" means YOU MUST either
          include "NULL" or a list of variables separated by "---". NOT both.
      15. DECISION TREE RULES
          15-A. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
          15-B. YOU MUST stop at the first step where a rule applies that has not been tried.
          15-C. YOU MUST exhaust all ideas on a step before moving to the next step in the decision tree.
          15-D. YOU MUST learn from previous agent_version runs and EDA results to avoid repeating params that have not worked in the past.
      16. ABORT RULES
          16-A. AFTER completing the decision tree of options, ABORT IF you cannot propose a set that you believe will beat the weighted mape goal based on EDA results and weighted_mape from previous runs.
          16-B. IF you ABORT, output the *abort-schema* instead of the normal parameter schema.

      -----ORDER OF OPERATIONS DECISION TREE (Rule 15)-----
      Step A (Previous Version Replay - Rule 13)
      -> Step B (Hierarchy - Rule 7)
      -> Step C (Missing Values - Rule 8)
      -> Step D (Outliers - Rule 9)
      -> Step E (External Regressors - Rule 10)
      -> Step F (Feature Lags - Rule 11)
      -> Step G (Rolling Window Lags - Rule 12)

      -----OUTPUT FORMAT-----
      <scratchpad>
      ...your chain-of-thought, work through the decision tree, cite Rules #,
      compare input recommendation to previous runs to prevent duplicates or violate parameter change constraints...
      </scratchpad>
      ```json
      // ---- normal schema (when you propose a new run) ----
      {
        "external_regressors"   : "NULL|var1---var2",
        "clean_missing_values"  : "TRUE|FALSE",
        "clean_outliers"        : "TRUE|FALSE",
        "negative_forecast"     : "TRUE|FALSE",
        "forecast_approach"     : "bottoms_up|standard_hierarchy|grouped_hierarchy",
        "stationary"            : "TRUE|FALSE",
        "feature_selection"     : "TRUE|FALSE",
        "multistep_horizon"     : "TRUE|FALSE",
        "lag_periods"           : "NULL|1---2---3",
        "rolling_window_periods" : "NULL|1---2---3",
        "reasoning"             : "...<=250 words..."
      }
      // ---- abort schema (Rule 16) ----
      {
        "abort"     : "TRUE",
        "reasoning" : "...<=250 words explaining why no further improvement is likely..."
      }
      ```
      -----END OUTPUT-----',
      .open = "<<", .close = ">>",
      combo = combo_str,
      target = project_info$target_variable,
      dtype = project_info$date_type,
      hist_end = agent_info$hist_end_date,
      horizon = agent_info$forecast_horizon,
      xregs = xregs_str,
      eda = eda_results,
      weighted_mape_goal = weighted_mape_goal,
      lag_default = lag_default,
      rolling_default = rolling_default,
      agent_version = agent_info$agent_version
    )
  } else {
    # local model prompt
    final_prompt <- glue::glue(
      '
      -----CONTEXT-----
      You are an autonomous time-series forecasting agent. Your goal is to choose
      Finn-API parameters that **lower weighted MAPE** versus all prior runs.

      -----METADATA-----
      - combos : <<combo>>
      - target : <<target>>
      - date type : <<dtype>>
      - hist end date : <<hist_end>>
      - forecast horizon : <<horizon>>
      - potential external regressors : <<xregs>>
      - weighted MAPE goal : <<weighted_mape_goal>>

      -----Exploratory Data Analysis-----
      <<eda>>

      -----RULES (MUST / MUST NOT)-----
      1.  YOU MUST output exactly one JSON object matching the schema below.
      2.  YOU MUST include a "reasoning" field with <= 250 words.
      3.  RUN CHANGE RULES
          3-A.  IF changes made in the previous run reduced the weighted mape compared to the best run, keep them, otherwise revert back to previous best run.
          3-B.  AFTER the first run (run_count > 0), YOU MUST change at most ONE parameter per new run.
          3-C.  Reverting back to a previous best run AND changing ONE parameter from that best run counts as ONE parameter change.
          3-D.  You MUST NOT recommend the same set of parameters as a previous run. If you do you MUST ABORT.
      4.  IF data is not stationary -> stationary="TRUE".
      5.  IF EDA shows strong autocorrelation on periods less than the forecast horizon AND forecast horizon > 1 -> multistep_horizon="TRUE".
      6.  NEGATIVE FORECAST RULES
          8-A.  IF EDA shows significant amount of negative values -> negative_forecast="TRUE".
          8-B.  IF no negative values are present -> negative_forecast="FALSE".
      7.  MISSING VALUES RULES
          10-A. IF missing values are present AND run_count == 0 AND *Step A is complete* -> clean_missing_values="FALSE"
          10-B. IF missing values are present AND run_count > 0 -> clean_missing_values="TRUE"
          10-C. ALWAYS use "FALSE" if no missing values are detected
      8.  OUTLIER RULES
          11-A. IF outliers are present AND run_count == 0 -> clean_outliers="FALSE"
          11-C. ALWAYS use "FALSE" if no outliers are detected
      9.  SEASONAL PERIOD RULES
          9-A. IF run_count == 0 -> seasonal_period = "NULL"
          9-B. IF run_count > 0 AND *Step C is complete* -> seasonal_period = "NULL" or a list of periods separated by "---". Use EDA results to select seasonal periods.
          9-C. You MUST NOT change the seasonal_period parameter more than *3 times* across all runs. IF you do you MUST ABORT.
          9-D. A value of "NULL" means that seasonal periods are defaulted to <<seasonal_period_default>>.
          9-E. There can only be at most 3 seasonal periods, separated by "---". If you select more than 3, you MUST ABORT.
          9-F. Seasonal period inputs ONLY apply to these models: "stlm-arima", "stlm-ets", "tbats". IF you are not using these models, you MUST NOT select any seasonal periods.
      10. FULL MODEL SWEEP RULES
          10-A. IF run_count == 0 -> models_to_run = "arima---meanf---snaive---stlm-arima---tbats---xgboost"
          10-B. IF run_count > 0 AND *Step D is complete* -> models_to_run = "arima---ets---meanf---nnetar---prophet---snaive---stlm-arima---tbats---theta---cubist---glmnet---xgboost"
          10-C. AFTER applying rule 10-B, if wmape goal was not met -> models_to_run = "arima---croston---ets---meanf---nnetar---prophet---snaive---stlm-arima---stlm-ets---tbats---theta---cubist---mars---glmnet---svm-poly---svm-rbf---xgboost"
      11. EXTERNAL REGRESSOR RULES
          11-A. When choosing external_regressors, YOU MUST only select from the external regressors listed in the metadata. Separate multiple regressors with "---".
          11-B. IF adding external regressors AND run_count == 0 -> external_regressors="NULL"
          11-C  IF adding external regressors AND run_count > 0 AND *Step E is complete*, add ONLY ONE new external regressor variable per run.
          11-D. ALWAYS use "NULL" if no external regressors are needed.
          11-E. ALWAYS start with the most promising external regressors based on distance correlation results.
          11-F. ALWAYS set feature_selection="TRUE" if any external regressors are used. Changing feature_selection AND external_regressors counts as ONE parameter change.
          11-G. IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
          11-H. ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
          11-I. You are ONLY ALLOWED to change the external_regressors parameter a total of <<xregs_length>> times across all runs.
          11-J. IF using multiple external_regressors, you MUST NOT change the ordering of them compared to previous runs.
      12. FEATURE LAG RULES
          12-A. IF run_count == 0 -> lag_periods = "NULL"
          12-B. IF run_count > 0 AND *Step F is complete* -> use ACF and PCF results from EDA to select lag_periods.
          12-C. IF selecting lag periods to use, ALWAYS combine them using "---" separator.
          12-D. IF selecting lag periods less than the forecast horizon, ALWAYS set multistep_horizon="TRUE".
          12-E. IF lag_changes_allowed == FALSE, you MUST NOT make any new lag_periods changes.
          12-F. A value of "NULL" means that lags are defaulted to <<lag_default>>. Note that these default lags assume multistep_horizon="TRUE". If not the lags will be greater than or equal to forecast horizon.
          12-G. If using multiple lag_periods, you MUST NOT change the ordering of them compared to previous runs.
      13. ROLLING WINDOW LAGS
          13-A. IF run_count == 0 -> rolling_window_periods = "NULL"
          13-B. IF run_count > 0 AND *Step G is complete* -> rolling_window_periods = "NULL" or a list of periods separated by "---".
          13-C. IF rolling_changes_allowed == FALSE, you MUST NOT make any new rolling_window_periods changes.
          13-D. A value of "NULL" means that rolling window lags are dedaulted to <<rolling_default>>.
          13-E. If using multiple rolling_window_periods, you MUST NOT change the ordering of them compared to previous runs.
      14. RECIPE RULES
          14-A. IF run_count == 0 -> recipes_to_run = "R1"
          14-B. IF run_count > 0 AND *Step H is complete* -> recipes_to_run = "NULL" or "R1"
          14-C. A value of "NULL" means that recipes are defaulted to <<recipe_default>>.
      15. PREVIOUS VERSION REPLAY RULES
          15-A. IF run_count == 0 AND one or more *earlier agent versions* exist before current agent version of <<agent_version>>,
          YOU MUST first try the **full input set** from the best run of the most-recent previous agent version.
          15-B. IF that set has already been tried in the current version AND run_count < 4, choose the next best run, and so on.
          15-C. AFTER following 15-A AND 15-B, you MUST follow the decision tree below to propose a new set of parameters.
          15-D. Using the full input set from a previous run counts as ONE parameter change.
      16. An example value of "NULL|var1---var2" means YOU MUST either
          include "NULL" or a list of variables separated by "---". NOT both.
      17. DECISION TREE RULES
          17-A. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
          17-B. YOU MUST stop at the first step where a rule applies that has not been tried.
          17-C. YOU MUST exhaust all ideas on a step before moving to the next step in the decision tree.
          17-D. YOU MUST learn from previous agent_version runs and EDA results to avoid repeating params that have not worked in the past.
      18. ABORT RULES
          18-A. AFTER completing the decision tree of options, ABORT IF you cannot propose a set that you believe will beat the weighted mape goal based on EDA results and weighted_mape from previous runs.
          18-B. IF you ABORT, output the *abort-schema* instead of the normal parameter schema.

      -----ORDER OF OPERATIONS DECISION TREE (Rule 17)-----
      Step A (Previous Version Replay - Rule 15)
      -> Step B (Missing Values - Rule 7)
      -> Step C (Outliers - Rule 8)
      -> Step D (Seasonal Period - Rule 9)
      -> Step E (Full Model Sweep - Rule 10)
      -> Step F (External Regressors - Rule 11)
      -> Step G (Feature Lags - Rule 12)
      -> Step H (Rolling Window Lags - Rule 13)
      -> Step I (Recipes - Rule 14)

      -----OUTPUT FORMAT-----
      <scratchpad>
      ...your chain-of-thought, work through the decision tree, cite Rules #,
      compare input recommendation to previous runs to prevent duplicates or violate parameter change constraints...
      </scratchpad>
      ```json
      // ---- normal schema (when you propose a new run) ----
      {
        "external_regressors"   : "NULL|var1---var2",
        "clean_missing_values"  : "TRUE|FALSE",
        "clean_outliers"        : "TRUE|FALSE",
        "negative_forecast"     : "TRUE|FALSE",
        "seasonal_period"       : "NULL|1---2---3",
        "models_to_run"         : "arima---ets---tbats---snaive---stlm-arima---xgboost",
        "stationary"            : "TRUE|FALSE",
        "feature_selection"     : "TRUE|FALSE",
        "multistep_horizon"     : "TRUE|FALSE",
        "lag_periods"           : "NULL|1---2---3",
        "rolling_window_periods": "NULL|1---2---3",
        "recipes_to_run"        : "R1|NULL",
        "reasoning"             : "...<=250 words..."
      }
      // ---- abort schema (Rule 18) ----
      {
        "abort"     : "TRUE",
        "reasoning" : "...<=250 words explaining why no further improvement is likely..."
      }
      ```
      -----END OUTPUT-----',
      .open = "<<", .close = ">>",
      combo = combo_str,
      target = project_info$target_variable,
      dtype = project_info$date_type,
      hist_end = agent_info$hist_end_date,
      horizon = agent_info$forecast_horizon,
      xregs = xregs_str,
      eda = eda_results,
      weighted_mape_goal = weighted_mape_goal,
      xregs_length = xregs_length,
      lag_default = lag_default,
      rolling_default = rolling_default,
      recipe_default = recipe_default,
      seasonal_period_default = seasonal_period_default,
      agent_version = agent_info$agent_version
    )
  }
  
  return(final_prompt)
}
