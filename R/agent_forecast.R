
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
  
  # get metadata and EDA results
  project_info <- agent_info$project_info
  llm <- agent_info$driver_llm
  combo_str   <- paste(project_info$combo_variables,   collapse = "---")
  drivers_str <- paste(agent_info$external_regressors, collapse = "---")
  
  
  eda_results <- load_eda_results(agent_info, 
                                  combo = combo)

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
  
  -----Exploratory Data Analysis-----
  <<eda>>
  
  -----RULES (MUST / MUST NOT)-----
  1. YOU MUST output exactly one JSON object matching the schema below.  
  2. YOU MUST include a "reasoning" field with ≤ 250 words.
  3. An example value of "NULL|var1---var2" means YOU MUST either 
     include "NULL" or a list of variables separated by "---". NOT both.
  4. When choosing external_regressors, YOU MUST only select from the 
     external regressors listed in the metadata. Separate multiple
     regressors with "---". If no external regressors are needed,
     YOU MUST use "NULL".
  
  -----OUTPUT FORMAT-----
  <scratchpad>
  …your chain-of-thought, cite Rules #…
  </scratchpad>
  ```json
  {
    "models_to_run"         : "arima---xgboost",
    "external_regressors"   : "NULL|var1---var2",
    "clean_missing_values"  : "TRUE|FALSE",
    "clean_outliers"        : "TRUE|FALSE",
    "negative_forecast"     : "TRUE|FALSE",
    "forecast_approach"     : "bottoms_up|standard_hierarchy|grouped_hierarchy",
    "stationary"            : "TRUE|FALSE",
    "feature_selection"     : "TRUE|FALSE",
    "multistep_horizon"     : "TRUE|FALSE",
    "seasonal_period"       : "NULL|12---3",
    "recipes_to_run"        : "NULL|R1",
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
  eda = eda_results
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
  if(input_list$external_regressors != "NULL") {
    
    # check to see if there are any regressors that are not in the project info
    if (any(!input_list$external_regressors %in% agent_info$external_regressors)) {
      stop(
        sprintf("External regressors %s are not in the agent info.",
                paste(setdiff(input_list$external_regressors, agent_info$external_regressors), collapse = ", ")),
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
  
  # kick off Finn run
  run_info <- set_run_info(project_name = project_info$project_name,
                           run_name = "finn_fcst",
                           storage_object = project_info$storage_object,
                           path = project_info$path,
                           data_output = project_info$data_output,
                           object_output = project_info$object_output,
                           add_unique_id = TRUE)
  
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
                                parallel_processing, 
                                inner_parallel, 
                                num_cores) {
  
  message("[agent] 📈 Starting forecast iteration workflow")
  
  # construct workflow
  workflow <- list(
    start = list(
      fn = "reason_inputs", `next` = "submit_fcst_run", max_retry = 0, 
      args = list("agent_info" = agent_info, 
                  "combo" = NULL)
    ),
    submit_fcst_run = list(
      fn = "submit_fcst_run", `next` = "get_fcst_output", max_retry = 0, 
      args = list("agent_info" = agent_info, 
                  "inputs" = "{results$reason_inputs}", 
                  "combo" = NULL, 
                  "parallel_processing" = parallel_processing, 
                  "inner_parallel" = inner_parallel, 
                  "num_cores" = num_cores)
    ),
    get_fcst_output = list(
      fn = "get_fcst_output", `next` = "calculate_fcst_metrics", max_retry = 0, 
      args = list("run_info" = "{results$submit_fcst_run}")
    ),
    calculate_fcst_metrics = list(
      fn = "calculate_fcst_metrics", `next` = "stop", max_retry = 0, 
      args = list("run_info" = "{results$submit_fcst_run}",
                  "fcst_tbl" = "{results$get_fcst_output}")
    ),
    stop  = list(fn = NULL)
  )
  
  # call the agent graph
  results <- run_graph(agent_info$driver_llm, workflow)
  
  return(results)
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


