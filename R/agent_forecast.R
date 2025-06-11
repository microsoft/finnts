
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
                          combo = NULL, 
                          weighted_mape_goal, 
                          last_error = NULL) {
  
  # get metadata
  if(!is.null(agent_info$reason_llm)) {
    llm <- agent_info$reason_llm
  } else {
    llm <- agent_info$driver_llm
  }
  
  project_info <- agent_info$project_info
  combo_str   <- paste(project_info$combo_variables,   collapse = "---")
  xregs_str <- paste(agent_info$external_regressors, collapse = "---")
  xregs_length <- length(agent_info$external_regressors)
  
  eda_results <- load_eda_results(agent_info = agent_info, combo = combo)
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
    
    rolling_window_changes <- previous_run_results %>%
      dplyr::select(rolling_window_periods) %>%
      tidyr::drop_na(rolling_window_periods) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      length()
    
  } else {
    best_mape <- "NA"
    best_run <- "NA"
    lag_period_changes <- 0
    rolling_window_changes <- 0
  }

  # create final prompt
  final_prompt <- glue::glue(
  '
  -----CONTEXT-----
  You are an autonomous time-series forecasting agent. Your goal is to choose
  Finn-API parameters that **lower weighted MAPE** versus all prior runs.
  
  -----METADATA-----
  • combos : <<combo>>
  • target : <<target>>
  • date type : <<dtype>>
  • hist end date : <<hist_end>>
  • forecast horizon : <<horizon>>
  • potential external regressors : <<xregs>>
  • run count : <<run_count>>
  • weighted MAPE goal : <<weighted_mape_goal>>
  • best weighted MAPE from previous runs : <<best_mape>>
  • best run number from previous runs : <<best_run>>
  • lag_period changes: <<lag_changes>>
  • rolling_window_period changes: <<rolling_changes>>
  
  -----Exploratory Data Analysis-----
  <<eda>>
  
  -----PREVIOUS RUN RESULTS-----
  <<run_results>>
  
  -----LAST ERROR-----
  <<last_error>>
  
  -----RULES (MUST / MUST NOT)-----
  1.  YOU MUST output exactly one JSON object matching the schema below.
  2.  YOU MUST include a "reasoning" field with ≤ 250 words.
  3.  RUN CHANGE RULES
      3-A.  IF changes made in the previous run reduced the weighted mape compared to the best run, keep them, otherwise revert back to previous best run.
      3-B.  AFTER the first run (run_count > 0), YOU MUST change at most ONE parameter per new run. 
      3-C.  Reverting back to a previous best run AND changing ONE parameter from that best run counts as ONE parameter change. 
      3-D.  You MUST NOT recommend the same set of parameters as a previous run. If you do you MUST ABORT. 
  6.  IF data is not stationary → stationary="TRUE".
  7.  IF EDA shows strong autocorrelation on periods less than the forecast horizon → multistep_horizon="TRUE".
  8.  NEGATIVE FORECAST RULES
      8-A.  IF EDA shows significant amount of negative values → negative_forecast="TRUE". 
      8-B.  IF no negative values are present → negative_forecast="FALSE".
  9. HIERARCHICAL RULES
      9-A. IF run_count == 0 → forecast_approach="bottoms_up"
      9-B. IF run_count > 0 AND hiearchy type != "none" → forecast_approach="standard_hierarchy" or "grouped_hierarchy" depending on EDA results.
      9-C. IF hiearchy type == "none" → forecast_approach="bottoms_up".
      9-D. You MUST NOT use "standard_hierarchy" or "grouped_hierarchy" if the hierarchy type is "none".
      9-E. You MUST NOT use "standard_hierarchy" if the hierarchy type is grouped. 
      9-F. You MUST NOT use "grouped_hierarchy" if the hierarchy type is standard.
  10. MISSING VALUES RULES
      10-A. IF missing values are present AND run_count == 0 → clean_missing_values="FALSE"
      10-B. IF missing values are present AND run_count > 0 AND *Step A is complete* → clean_missing_values="TRUE"
      10-C. ALWAYS use "FALSE" if no missing values are detected
  11. OUTLIER RULES
      11-A. IF outliers are present AND run_count == 0 → clean_outliers="FALSE"
      11-B. IF outliers are present AND run_count > 0 AND *Step B is complete* → clean_outliers="TRUE"
      11-C. ALWAYS use "FALSE" if no outliers are detected
  12. EXTERNAL REGRESSOR RULES
      12-A. When choosing external_regressors, YOU MUST only select from the external regressors listed in the metadata. Separate multiple regressors with "---".
      12-B. IF adding external regressors AND run_count == 0 → external_regressors="NULL"
      12-C  IF adding external regressors AND run_count > 0 AND *Step C is complete*, add ONLY ONE new external regressor variable per run. 
      12-D. ALWAYS use "NULL" if no external regressors are needed.
      12-E. ALWAYS start with the most promising external regressors based on distance correlation results.  
      12-F. ALWAYS set feature_selection="TRUE" if any external regressors are used.
      12-G. IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
      12-H. ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
      12-I. You are ONLY ALLOWED to change the external_regressors parameter a total of <<xregs_length>> times across all runs.
  13. FEATURE LAG RULES
      13-A. IF run_count == 0 → lag_periods = "NULL"
      13-B. IF run_count > 0 AND *Step D is complete* → use ACF and PCF results from EDA to select lag_periods. 
      13-C. IF selecting lag periods to use, ALWAYS combine them using "---" separator.
      13-D. IF selecting lag periods less than the forecast horizon, ALWAYS set multistep_horizon="TRUE".
      13-E. You MUST NOT change the lag_periods parameter more than *3 times* across all runs. IF you do you MUST ABORT. 
      13-F. A value of "NULL" means that lags are automatically selected using fixed logic, not that you are not using lags.
  14. ROLLING WINDOW LAGS
      14-A. IF run_count == 0 → rolling_window_periods = "NULL"
      14-B. IF run_count > 0 AND *Step E is complete* → rolling_window_periods = "NULL" or a list of periods separated by "---". 
      14-C. You MUST NOT change the rolling_window_periods parameter more than *3 times* across all runs. IF you do you MUST ABORT. 
      14-D. A value of "NULL" means that rolling window lags are automatically selected using fixed logic, not that you are not using rolling window calculations.
  15. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
  16. An example value of "NULL|var1---var2" means YOU MUST either 
      include "NULL" or a list of variables separated by "---". NOT both.
  17. DECISION TREE RULES
      17-A. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
      17-B. YOU MUST stop at the first step where a rule applies that hasn’t been tried.
  18. ABORT RULES
      18-A. AFTER completing the decision tree of options, ABORT IF you cannot propose a set that you believe will beat the weighted mape goal based on EDA results and weighted_mape from previous runs.
      18-B. IF you ABORT, output the *abort-schema* instead of the normal parameter schema.
     
  -----ORDER OF OPERATIONS DECISION TREE (Rule 17)-----
  Step A (Hierarchy - Rule 9) 
  → Step B (Missing Values - Rule 10)
  → Step C (Outliers - Rule 11) 
  → Step D (External Regressors - Rule 12)
  → Step E (Feature Lags - Rule 13)
  → Step F (Rolling Window Lags - Rule 14)
  
  -----OUTPUT FORMAT-----
  <scratchpad>
  …your chain-of-thought, work through the decision tree, cite Rules #, 
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
    "reasoning"             : "… ≤250 words …"
  }
  // ---- abort schema (Rule 18) ----
  {
    "abort"     : "TRUE",
    "reasoning" : "… ≤250 words explaining why no further improvement is likely …"
  }
  ```
  -----END OUTPUT-----',
  .open = '<<', .close = '>>',
  combo = combo_str,
  target = project_info$target_variable,
  dtype = project_info$date_type,
  hist_end= agent_info$hist_end_date,
  horizon = agent_info$forecast_horizon,
  xregs = xregs_str,
  eda = eda_results, 
  run_results = make_pipe_table(previous_run_results), 
  run_count = total_runs, 
  weighted_mape_goal = weighted_mape_goal, 
  best_mape = best_mape,
  best_run = best_run, 
  xregs_length = xregs_length, 
  lag_changes = lag_period_changes,
  rolling_changes = rolling_window_changes, 
  last_error = ifelse(is.null(last_error), "No errors in previous input recommendation.", last_error)
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
  if(is.null(combo)) {
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
      models_to_run = "NULL",
      external_regressors = "NULL",
      clean_missing_values = TRUE,
      clean_outliers = FALSE,
      negative_forecast = FALSE,
      forecast_approach = "bottoms_up",
      stationary = FALSE,
      feature_selection = TRUE,
      multistep_horizon = FALSE,
      seasonal_period = "NULL",
      recipes_to_run = "NULL",
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
  input_list$lag_periods <- if(input_list$lag_periods == "NULL") {"NULL"} else {as.numeric(strsplit(input_list$lag_periods, "---")[[1]])}
  input_list$rolling_window_periods <- if(input_list$rolling_window_periods == "NULL") {"NULL"} else {as.numeric(strsplit(input_list$rolling_window_periods, "---")[[1]])}
  
  # make sure xregs look correct
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
  
  # check if these inputs were used before in previous runs
  previous_runs_tbl <- load_run_results(agent_info = agent_info, combo = combo)

  if(is.data.frame(previous_runs_tbl)) {
    check_inputs <- input_list
    check_inputs$models_to_run          <- collapse_or_na(check_inputs$models_to_run)
    check_inputs$external_regressors    <- collapse_or_na(check_inputs$external_regressors)
    check_inputs$recipes_to_run         <- collapse_or_na(check_inputs$recipes_to_run)
    check_inputs$lag_periods            <- collapse_or_na(check_inputs$lag_periods)
    check_inputs$rolling_window_periods <- collapse_or_na(check_inputs$rolling_window_periods)
    check_inputs <- check_inputs[names(check_inputs) %in% names(previous_runs_tbl)]
    
    cols_to_fix <- c(
      "models_to_run",
      "external_regressors",
      "recipes_to_run",
      "lag_periods",
      "rolling_window_periods"
    )
    
    previous_runs_tbl <- previous_runs_tbl %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(intersect(names(previous_runs_tbl), cols_to_fix)),
          base::as.character
        )
      )
    
    if(does_param_set_exist(check_inputs, previous_runs_tbl)) {
      cli::cli_alert_info("The proposed input parameters have already been used in a previous run.")
      stop("The proposed input parameters have already been used in a previous run. Please modify the inputs to ensure they are unique.",
        call. = FALSE
      )
    }
    
    if(length(unique(c(previous_runs_tbl$lag_periods, check_inputs$lag_periods))) > 4) {
      cli::cli_alert_info("Cannot propose more than 3 unique lag period changes across all runs.")
      stop("Cannot propose more than 3 unique lag period changes across all runs. Stop modifying lag_periods.",
        call. = FALSE
      )
    }
    
    if(length(unique(c(previous_runs_tbl$rolling_window_periods, check_inputs$rolling_window_periods)))  > 4) {
      cli::cli_alert_info("Cannot propose more than 3 unique rolling window period changes across all runs.")
      stop("Cannot propose more than 3 unique rolling window period changes across all runs. Stop modifying rolling_window_periods.",
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
    lag_periods = null_converter(inputs$lag_periods),
    rolling_window_periods = null_converter(inputs$rolling_window_periods),
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
    num_hyperparameters = 2,
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
  
  return(weighted_mape)
}

log_best_run <- function(agent_info, 
                         run_info, 
                         weighted_mape, 
                         combo = NULL) {
  
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
  print(log_results)
  # log results if the new run is better or if no previous runs exist
  if(log_results) {
    
    if (is.null(combo)) {
      combo <- "all"
    }
    
    project_info <- agent_info$project_info
    project_info$run_name <- agent_info$run_id
    
    
    # get back test data
    back_test_tbl <- get_forecast_data(run_info = run_info) %>%
      dplyr::filter(Best_Model == "Yes", 
                    Run_Type == "Back_Test")
    
    combo_list <- unique(back_test_tbl$Combo)
    
    # for each combo, filter the back test data, calculate the weighted mape and save to logs
    for (combo_name in combo_list) {
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
        sum() %>%
        round(digits = 4)
      
      # check if previous log file exists, if so check if mape is better
      prev_log_df <- tryCatch(
        read_file(project_info,
                  path = paste0("logs/", hash_data(project_name), "-", hash_data(agent$run_id), 
                                "-",  hash_data(combo_name), "-agent_best_run.csv"),
                  return_type = "df"
        ),
        error = function(e) {
          tibble::tibble()
        }
      ) %>%
        base::suppressWarnings()
      
      if(nrow(prev_log_df) > 0) {
        prev_wmape <- as.numeric(prev_log_df$weighted_mape)
      } else {
        prev_wmape <- Inf # if no previous log, assume previous was worse
      }
      
      if(wmape < prev_wmape || combo == "all") { # always log the wmape for global models, but check for local models
        print(combo_name)
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

fcst_agent_workflow <- function(agent_info,
                                combo = NULL,
                                weighted_mape_goal, 
                                parallel_processing,
                                inner_parallel,
                                num_cores,
                                max_iter = 3
) {
  
  message("[agent] 📈 Starting Forecast Iteration Workflow")
  
  # 1. construct the workflow 
  workflow <- list(
    start = list(
      fn   = "reason_inputs",
      `next` = "submit_fcst_run",
      retry_mode = "plain",  
      max_retry = 3,
      args = list(agent_info = agent_info, 
                  combo = combo, 
                  weighted_mape_goal = weighted_mape_goal, 
                  last_error = NULL), 
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
      fn   = "submit_fcst_run",
      `next` = "get_fcst_output",
      retry_mode = "plain",
      max_retry = 3,
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
      retry_mode = "plain",  
      max_retry = 3,
      args = list(run_info = "{results$submit_fcst_run}")
    ),
    
    calculate_fcst_metrics = list(
      fn   = "calculate_fcst_metrics",
      `next` = "log_best_run",
      retry_mode = "plain",  
      max_retry = 3,
      args = list(
        run_info  = "{results$submit_fcst_run}",
        fcst_tbl  = "{results$get_fcst_output}"
      )#,
      # branch = function(ctx) {
      #   
      #   # test if max run iterations have been reached
      #   ctx$iter <- ctx$iter + 1
      #   max_runs_reached <- ctx$iter >= ctx$max_iter
      #   
      #   cli::cli_alert_info(
      #     "Forecast Iteration {ctx$iter}/{ctx$max_iter} Complete"
      #   )
      #   
      #   # check if the weighted MAPE is below the goal
      #   weighted_mape <- ctx$results$calculate_fcst_metrics
      #   
      #   if (weighted_mape < weighted_mape_goal) {
      #     cli::cli_alert_success(
      #       "Weighted MAPE goal of {round(weighted_mape_goal * 100, 2)}% achieved! Latest weighted MAPE is {round(weighted_mape * 100, 2)}%. Stopping iterations."
      #     )
      #     wmape_goal_reached <- TRUE
      #   } else if (max_runs_reached) {
      #     cli::cli_alert_info(
      #       "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Stopping iterations as max runs is reached."
      #     )
      #     wmape_goal_reached <- FALSE
      #   } else {
      #     cli::cli_alert_info(
      #       "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Continuing to next iteration."
      #     )
      #     wmape_goal_reached <- FALSE
      #   }
      #   
      #   # check if weighted mape is better than previous runs
      #   if(!wmape_goal_reached) {
      #     previous_runs <- load_run_results(agent_info = agent_info, combo = combo)
      #     
      #     if (is.data.frame(previous_runs) && nrow(previous_runs) > 0) {
      #       best_run_mape <- previous_runs %>%
      #         dplyr::filter(best_run == "yes") %>%
      #         dplyr::pull(weighted_mape) %>%
      #         unique()
      #       
      #       if (weighted_mape < best_run_mape) {
      #         cli::cli_alert_success(
      #           "New run has a better weighted MAPE of {round(weighted_mape * 100, 2)}% than the best previous run of {round(best_run_mape * 100, 2)}%."
      #         )
      #       }
      #       
      #       log_run <- TRUE
      #       
      #     } else {
      #       log_run <- FALSE
      #     }
      #   } else {
      #     log_run <- FALSE
      #   }
      #   
      #   # determine next node based on conditions
      #   if(wmape_goal_reached || log_run ) {
      #     next_node <- "log_best_run"
      #   } else if (max_runs_reached) {
      #     next_node <- "stop"
      #   }
      #   else {
      #     next_node <- "start"
      #   }
      #   
      #   return(list(ctx = ctx, `next` = next_node, 
      #               stop = wmape_goal_reached || max_runs_reached))
      # }
    ),
    
    log_best_run = list(
      fn   = "log_best_run",
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
        if(wmape_goal_reached || max_runs_reached) {
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
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "log_best_run",
    .description = "Log the best run from a Finn forecasting run",
    .fun = log_best_run
  ))
}

load_run_results <- function(agent_info, 
                             combo = NULL) {
  
  if(is.null(combo)) {
    combo_value <- hash_data("all")
    
    column_list <- c("external_regressors", "clean_missing_values", "clean_outliers", 
                      "stationary", "forecast_approach", 
                      "lag_periods", "rolling_window_periods",
                      "multistep_horizon", "feature_selection", 
                      "negative_forecast", "weighted_mape")
  } else {
    combo_value <- combo
    
    column_list <- c("external_regressors", "clean_missing_values", "clean_outliers", 
                     "stationary", "box_cox", "forecast_approach", 
                     "lag_periods", "rolling_window_periods", "recipes_to_run", 
                     "multistep_horizon", "models_to_run", "pca", 
                     "seasonal_period", "num_hyperparameters", "feature_selection", 
                     "negative_forecast", "weighted_mape")
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
        dplyr::select(tidyselect::all_of(column_list)) %>%
        dplyr::filter(!is.na(weighted_mape)) %>%
        dplyr::mutate(run_number = dplyr::row_number()) %>%
      dplyr::mutate(
        best_run = dplyr::if_else(
          weighted_mape == min(weighted_mape) &
            cumsum(weighted_mape == min(weighted_mape)) == 1,
          "yes",                       # first (earliest) min-MAPE row
          "no"                         # all others
        )
      )

    if (nrow(previous_runs) == 0) {
      run_output <- "No Previous Runs"
    } else {
      run_output <- previous_runs
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

# helper: collapse vector unless it is the literal "NULL"
collapse_or_na <- function(x) {
  if (length(x) == 1 && identical(x, "NULL")) {
    NA_character_
  } else {
    paste(x, collapse = "---")
  }
}

apply_column_types <- function(target_df,
                               template_df,
                               drop_extra = FALSE,
                               reorder     = FALSE) {
  
  common_cols <- intersect(names(template_df), names(target_df))
  
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
      vec   # leave as is for other classes (lists, df columns, etc.)
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
                                 .after = dplyr::last_col()) %>%
      dplyr::relocate(tidyselect::all_of(common_cols), .before = 1)
  }
  
  return(target_df)
}


does_param_set_exist <- function(x, df) {
  
  stopifnot(is.list(x) || is.atomic(x),                 # basic sanity
            !is.null(names(x)),                         # must be named
            all(names(x) %in% names(df)))               # cols present

  probe <- x %>%
    tibble::as_tibble() %>% # 1-row tibble with same names as df
    apply_column_types(template_df = df) # ensure types match
  
  final_df <- df %>%
    dplyr::select(tidyselect::all_of(names(probe)))  # select only matching cols
  
  dplyr::semi_join(final_df, probe, by = names(probe)) %>%    # keep rows that match
    nrow() > 0                                          # TRUE if ≥1 match
}
