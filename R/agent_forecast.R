
extract_json_object <- function(raw_text) {
  # sanity check 
  if (!is.character(raw_text) || length(raw_text) != 1) {
    stop("`raw_text` must be a single character string.", call. = FALSE)
  }
  
  # locate the first {...} block 
  json_block <- raw_text %>% 
    stringr::str_remove_all("(?s)```json\\s*|```") %>%   # strip code-fences
    stringr::str_extract("(?s)\\{.*?\\}")                # non-greedy first brace
  
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
  
  result
}

null_converter <- function(x) {
  if(length(x) > 1) {
    return(x)
  } else if(x == "NULL") {
    return(NULL)
  } else {
    return(x)
  }
}

reason_inputs <- function(agent_info, 
                          combo = NULL) {
  
  # get metadata
  if(!is.null(agent_info$reason_llm)) {
    llm <- agent_info$reason_llm
  } else {
    llm <- agent_info$driver_llm
  }
  
  project_info <- agent_info$project_info
  combo_str   <- paste(project_info$combo_variables,   collapse = "---")
  drivers_str <- paste(agent_info$external_regressors, collapse = "---")
  
  eda_results <- load_eda_results(agent_info = agent_info, combo = combo)
  previous_run_results <- load_run_results(agent_info = agent_info, combo = combo)
  total_runs <- get_total_run_count(agent_info, combo = combo)

  # create final prompt
  final_prompt <- glue::glue(
  '
  -----CONTEXT-----
  You are an autonomous time-series forecasting agent. Your goal is to choose
  Finn-API parameters that **lower weighted MAPE** versus all prior runs.
  
  -----METADATA-----
  • combos                : <<combo>>
  • target                : <<target>>
  • date_type             : <<dtype>>
  • hist_end_date         : <<hist_end>>
  • forecast_horizon      : <<horizon>>
  • external_regressors   : <<drivers>>
  • run_count             : <<run_count>>
  
  -----Exploratory Data Analysis-----
  <<eda>>
  
  -----PREVIOUS RUN RESULTS-----
  <<run_results>>
  
  -----RULES (MUST / MUST NOT)-----
  1.  YOU MUST output exactly one JSON object matching the schema below.
  2.  YOU MUST include a "reasoning" field with ≤ 250 words.
  3.  YOU MUST NOT repeat a parameter set used in any previous run.
  4.  AFTER the first run, YOU MUST change at most ONE parameter per new run
  5.  IF changes made in the previous run reduced the wMAPE compared to the best run, keep them, otherwise revert them.
  6.  IF data is not stationary → stationary="TRUE".
  7.  IF EDA shows strong autocorrelation on periods less than the forecast horizon → multistep_horizon="TRUE".
  8.  IF hiearchy type != "none", test "bottoms_up" first, then "standard_hierarchy" or "grouped_hiearchy" depending on EDA results. IF hierarchy type == "none" → forecast_approach="bottoms_up".
  9.  IF outliers are present AND run_count == 0 → clean_outliers="FALSE" first, then after try "TRUE".
  10. DRIVER RULES
      10-A. IF adding external regressors AND run_count == 0 → external_regressors="NULL" first, then after add ONLY ONE new regressor variable per run. 
      10-B. ALWAYS use "NULL" if no external regressors are needed.
      10-C. ALWAYS start with the most promising external regressors based on distance correlation results.  
      10-D. ALWAYS set feature_selection="TRUE" if any external regressors are used.
      10-E. IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
      10-F. ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
  11. IF a param change in any previous run did not reduce the forecast error, NEVER use it again in another run. 
  12. ABORT IF you cannot propose a set that you believe will beat the wMAPE goal based on EDA results and weighted_mape from previous runs.
  13. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
  14. An example value of "NULL|var1---var2" means YOU MUST either 
      include "NULL" or a list of variables separated by "---". NOT both.
  15. When choosing external_regressors, YOU MUST only select from the 
      external regressors listed in the metadata. Separate multiple
      regressors with "---". If no external regressors are needed,
      YOU MUST use "NULL".
     
  -----ORDER OF OPERATIONS DECISION TREE-----
  Step A (Hierarchy - Rule 8) 
  → Step B (Outliers - Rule 9) 
  → Step C (Drivers - Rule 11)
  (Stop at the first step where a rule applies that hasn’t been tried.)
  
  -----OUTPUT FORMAT-----
  <scratchpad>
  …your chain-of-thought, cite Rules #…
  </scratchpad>
  ```json
  {
    "models_to_run"         : "xgboost",
    "external_regressors"   : "NULL|var1---var2",
    "clean_missing_values"  : "TRUE|FALSE",
    "clean_outliers"        : "TRUE|FALSE",
    "negative_forecast"     : "TRUE|FALSE",
    "forecast_approach"     : "bottoms_up|standard_hierarchy|grouped_hierarchy",
    "stationary"            : "TRUE|FALSE",
    "feature_selection"     : "TRUE|FALSE",
    "multistep_horizon"     : "TRUE|FALSE",
    "seasonal_period"       : "NULL|12---3",
    "recipes_to_run"        : "NULL",
    "reasoning"             : "… ≤250 words …"
  }
  ```
  -----END OUTPUT-----',
  .open = '<<', .close = '>>',
  combo = combo_str,
  target = project_info$target_variable,
  dtype = project_info$date_type,
  hist_end= agent_info$hist_end_date,
  horizon = agent_info$forecast_horizon,
  drivers = drivers_str,
  eda = eda_results, 
  run_results = previous_run_results, 
  run_count = total_runs
  )

  # send prompt to LLM
  response <- llm$chat(final_prompt, echo = FALSE)
  
  # extract out json from response and convert to list
  input_list <- extract_json_object(response)
  
  # return(input_list)
  
  # check if all required fields are present
  required_fields <- c(
    "models_to_run", "external_regressors", "clean_missing_values",
    "clean_outliers", "negative_forecast", "forecast_approach",
    "stationary", "feature_selection", "multistep_horizon",
    "seasonal_period", "recipes_to_run", "reasoning"
  )
  
  missing_fields <- setdiff(required_fields, names(input_list))
  
  if (length(missing_fields) > 0) {
    stop(
      sprintf("Missing required fields in LLM response: %s", 
              paste(missing_fields, collapse = ", ")),
      call. = FALSE
    )
  }
  
  # format the list to ensure correct types
  input_list$models_to_run <- strsplit(input_list$models_to_run, "---")[[1]]
  input_list$external_regressors <- if(input_list$external_regressors == "NULL") {"NULL"} else {strsplit(input_list$external_regressors, "---")[[1]]}
  input_list$clean_missing_values <- as.logical(input_list$clean_missing_values)
  input_list$clean_outliers <- as.logical(input_list$clean_outliers)
  input_list$negative_forecast <- as.logical(input_list$negative_forecast)
  input_list$forecast_approach <- as.character(input_list$forecast_approach)
  input_list$stationary <- as.logical(input_list$stationary)
  input_list$feature_selection <- as.logical(input_list$feature_selection)
  input_list$multistep_horizon <- as.logical(input_list$multistep_horizon)
  input_list$seasonal_period <- if(input_list$seasonal_period == "NULL") {"NULL"} else {as.numeric(strsplit(input_list$seasonal_period, "---")[[1]])}
  input_list$recipes_to_run <- if(input_list$recipes_to_run == "NULL") {"NULL"} else {strsplit(input_list$recipes_to_run, "---")[[1]]}
  
  # final checks
  if(length(input_list$external_regressors) > 1) {
    
    # check to see if there are any regressors that are not in the project info
    if (any(!input_list$external_regressors %in% agent_info$external_regressors)) {
      stop(
        sprintf("External regressors %s are not in the agent info.",
                paste(setdiff(input_list$external_regressors, agent_info$external_regressors), collapse = ", ")),
        call. = FALSE
      )
    }
  } else if (input_list$external_regressors != "NULL") {
    # if there is only one regressor, check if it is NULL or in the project info
    if (!(input_list$external_regressors %in% c("NULL", agent_info$external_regressors))) {
      stop(
        sprintf("External regressor %s is not in the agent info.",
                input_list$external_regressors),
        call. = FALSE
      )
    }
  }

  return(input_list)
}

submit_fcst_run <- function(agent_info,
                            inputs, 
                            combo = NULL, 
                            parallel_processing = NULL, 
                            inner_parallel = FALSE, 
                            num_cores = NULL) {

  cli::cli_alert_info(
    "Starting Finn forecasting run with inputs: {jsonlite::toJSON(inputs, auto_unbox = TRUE)}"
  )
  
  # get project info
  project_info <- agent_info$project_info
  
  # read all combos or just one
  if(is.null(combo)) {
    combo_value <- "*"
  } else {
    combo_value <- combo
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
  
  # create unique run name
  run_name <- paste0(
    "agent_",
    agent_info$run_id, "_",
    ifelse(is.null(combo), hash_data("all"), combo_value), "_",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  )
  
  # kick off Finn run
  run_info <- set_run_info(project_name = project_info$project_name,
                           run_name = run_name,
                           storage_object = project_info$storage_object,
                           path = project_info$path,
                           data_output = project_info$data_output,
                           object_output = project_info$object_output,
                           add_unique_id = FALSE)
  
  # clean and prepare data for training
  prep_data(
    run_info = run_info,
    input_data = input_data,
    combo_variables = project_info$combo_variables,
    target_variable = "Target",
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    external_regressors = null_converter(inputs$external_regressors),
    hist_start_date = NULL,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = NULL,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = inputs$clean_missing_values,
    clean_outliers = inputs$clean_outliers,
    box_cox = FALSE,
    stationary = inputs$stationary,
    forecast_approach = inputs$forecast_approach,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = NULL,
    rolling_window_periods = NULL,
    recipes_to_run = null_converter(inputs$recipes_to_run),
    multistep_horizon = inputs$multistep_horizon
  )

  # prepare models for training
  prep_models(
    run_info = run_info,
    back_test_scenarios = NULL,
    back_test_spacing = NULL,
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
    run_global_models = TRUE,
    run_local_models = FALSE,
    global_model_recipes = c("R1"),
    feature_selection = inputs$feature_selection,
    negative_forecast = inputs$negative_forecast,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores
  )
  
  # evaluate models
  final_models(
    run_info = run_info,
    average_models = TRUE,
    max_model_average = 3,
    weekly_to_daily = TRUE,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores
  )
  
  return(run_info)
}

get_fcst_output <- function(run_info) {
  fcst_tbl <- get_forecast_data(run_info)
  
  return(fcst_tbl)
}

calculate_fcst_metrics <- function(run_info, 
                                   fcst_tbl) {
  
  # get weighted mape from run logging
  run_log_df <- read_file(run_info,
                          path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
                          return_type = "df")
  
  weighted_mape <- run_log_df$weighted_mape
  
  if (is.null(weighted_mape)) {
    stop("No weighted MAPE found in run log. Ensure the run was completed successfully.", call. = FALSE)
  }
  
  cli::cli_alert_info(
    "The weighted MAPE for this run is {round(weighted_mape*100, 2)}%."
  )
}

fcst_agent_workflow <- function(agent_info,
                                combo = NULL,
                                parallel_processing,
                                inner_parallel,
                                num_cores,
                                max_iter = 3
) {
  
  message("[agent] 📈 Starting forecast iteration workflow")
  
  # 1. construct the workflow 
  workflow <- list(
    start = list(
      fn   = "reason_inputs",
      `next` = "submit_fcst_run",
      max_retry = 0,
      args = list(agent_info = agent_info, 
                  combo = combo)
    ),
    
    submit_fcst_run = list(
      fn   = "submit_fcst_run",
      `next` = "get_fcst_output",
      max_retry = 0,
      args = list(
        agent_info          = agent_info,
        inputs              = "{results$reason_inputs}",
        combo               = NULL,
        parallel_processing = parallel_processing,
        inner_parallel      = inner_parallel,
        num_cores           = num_cores
      )
    ),
    
    get_fcst_output = list(
      fn   = "get_fcst_output",
      `next` = "calculate_fcst_metrics",
      max_retry = 0,
      args = list(run_info = "{results$submit_fcst_run}")
    ),
    
    calculate_fcst_metrics = list(
      fn   = "calculate_fcst_metrics",
      `next` = NULL,                     # handled by branch() instead
      max_retry = 0,
      args = list(
        run_info  = "{results$submit_fcst_run}",
        fcst_tbl  = "{results$get_fcst_output}"
      ),
      # 2. branch decides where to go next 
      branch = function(ctx) {
        
        ctx$iter <- ctx$iter + 1
        next_node <- if (ctx$iter < ctx$max_iter) "start" else "stop"
        
        cli::cli_alert_info(
          "Forecast Iteration {ctx$iter}/{ctx$max_iter} Completed"
        )
        
        return(list(ctx = ctx, `next` = next_node))
      }
    ),
    
    stop = list(fn = NULL)
  )
  
  # 3. initial context 
  init_ctx <- list(
    node      = "start",
    iter      = 0,          # iteration counter
    max_iter  = max_iter,   # loop limit
    results   = list(),     # where each tool’s output will be stored
    attempts  = list()      # retry bookkeeping for execute_node()
  )
  
  # 4. run the graph 
  run_graph(agent_info$driver_llm, workflow, init_ctx)
}


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
}

load_run_results <- function(agent_info, 
                             combo = NULL) {
  
  if(is.null(combo)) {
    combo_value <- hash_data("all")
  } else {
    combo_value <- combo
  }
  
  previous_runs <- get_run_info(project_name = agent_info$project_info$project_name,
               run_name = NULL,
               storage_object = agent_info$project_info$storage_object,
               path = agent_info$project_info$path)
  
  if("run_name" %in% names(previous_runs)) {
    
    previous_runs <- previous_runs %>%
      dplyr::filter(stringr::str_starts(run_name, paste0("agent_", agent_info$run_id, "_", combo_value))) %>%
        dplyr::mutate(created = lubridate::ymd_hms(created, tz = "UTC")) %>%
        dplyr::arrange(created) %>%
        dplyr::select(external_regressors, clean_missing_values, clean_outliers, 
                      stationary, box_cox, forecast_approach, 
                      lag_periods, rolling_window_periods, recipes_to_run, 
                      multistep_horizon, models_to_run, pca, 
                      seasonal_period, num_hyperparameters, feature_selection, 
                      negative_forecast, weighted_mape) %>%
        dplyr::filter(!is.na(weighted_mape)) %>%
        dplyr::mutate(run_number = dplyr::row_number())
    
    if (nrow(previous_runs) == 0) {
      run_output <- "No Previous Runs"
    } else {
      run_output <- make_pipe_table(previous_runs)
    }
  } else {
    run_output <- "No Previous Runs"
  }
  
  return(run_output)
}

get_total_run_count <- function(agent_info, 
                                combo = NULL) {
  if(is.null(combo)) {
    combo_value <- hash_data("all")
  } else {
    combo_value <- combo
  }
  
  total_runs <- get_run_info(project_name = agent_info$project_info$project_name,
                             run_name = NULL,
                             storage_object = agent_info$project_info$storage_object,
                             path = agent_info$project_info$path)
  
  if("run_name" %in% names(total_runs)) {
    total_runs <- total_runs %>%
      dplyr::filter(stringr::str_starts(run_name, paste0("agent_", agent_info$run_id, "_", combo_value)))
  } else {
    total_runs <- tibble::tibble()
  }

  return(nrow(total_runs))
}
