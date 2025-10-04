#' Ask Questions About Finn Agent Forecast Results
#'
#' This function allows users to ask questions about their Finn AI Agent
#' forecast results and get answers based on the outputs from iterate_forecast()
#' or update_forecast(). It uses an LLM-driven workflow to generate and execute
#' R code to answer questions.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param question A character string containing the question to ask about the forecast
#'
#' @return A character string containing the answer to the question
#' @examples
#' \dontrun{
#' # After running iterate_forecast() or update_forecast()
#'
#' # Ask about exploratory data analysis
#' answer <- ask_agent(
#'   agent_info = agent_info,
#'   question = "Were there any missing values in the data?"
#' )
#'
#' # Ask about forecast accuracy
#' answer <- ask_agent(
#'   agent_info = agent_info,
#'   question = "What is the average weighted MAPE across all time series?"
#' )
#'
#' # Ask about models used
#' answer <- ask_agent(
#'   agent_info = agent_info,
#'   question = "Which models were used for the forecast?"
#' )
#'
#' # Ask about specific time series
#' answer <- ask_agent(
#'   agent_info = agent_info,
#'   question = "What is the forecast for product XYZ for the next 3 months?"
#' )
#' }
#' @export
ask_agent <- function(agent_info,
                      question) {
  # Input validation
  check_agent_info(agent_info)
  check_input_type("question", question, "character")

  # Get LLM from agent_info
  if (is.null(agent_info$driver_llm)) {
    stop("No driver_llm found in agent_info. Please ensure agent_info was created with set_agent_info() and includes a driver_llm.",
      call. = FALSE
    )
  }

  # Create and run the workflow
  result <- ask_agent_workflow(
    agent_info = agent_info,
    question = question
  )

  # Display the formatted answer
  display_answer(result$answer)

  return(invisible(result$answer))
}

#' Ask Agent Workflow
#'
#' @param agent_info Agent info object
#' @param question The user's question
#'
#' @return List with answer and collected data
#' @noRd
ask_agent_workflow <- function(agent_info,
                               question) {
  # Clone the LLM for this workflow
  agent_info$driver_llm <- agent_info$driver_llm$clone()

  # Set system prompt for Q&A workflow
  system_prompt <- glue::glue(
    "You are an R programming assistant that answers questions about Finn forecast results.
    You generate and execute R code to analyze forecast data and model information.

    Project Information:
    - Project Name: {agent_info$project_info$project_name}
    - Target Variable: {agent_info$project_info$target_variable}
    - Date Type: {agent_info$project_info$date_type}
    - Combo Variables: {paste(agent_info$project_info$combo_variables, collapse = ', ')}
    - Forecast Horizon: {agent_info$forecast_horizon}
    - External Regressors: {ifelse(is.null(agent_info$external_regressors), 'None',
                                   paste(agent_info$external_regressors, collapse = ', '))}

    Available data sources:
    - get_agent_forecast(agent_info): Returns a df of the final forecast data with the following columns:
      - Combo: individual time series identifier, which is the combination of all combo variables, separated by '--'
      - Model_ID: THE PRIMARY MODEL IDENTIFIER - unique identifier of the specific model(s) trained.
        * For single models: combination of Model_Name, Model_Type, and Recipe_ID (e.g., 'arima--local--R1')
        * For ensemble/average models: lists all averaged models separated by '_' (e.g., 'arima--local--R1_ets--local--R1_prophet--local--R1')
        * ALWAYS USE Model_ID to identify which model(s) were used
      - Model_Name: name of the model type (e.g., 'arima', 'ets', 'cubist')
        * IMPORTANT: When Model_Name is NA, this indicates a SIMPLE AVERAGE model - check Model_ID for the actual models used
      - Model_Type: how the model was trained ('local' for individual time series, 'global' for all time series)
      - Recipe_ID: unique identifier of the recipe used for the model (e.g., 'R1', 'R2', 'simple_average')
      - Run_Type: distinguishes between 'Back_Test' and 'Future_Forecast'
      - Train_Test_ID: identifies each fold of time series cross-validation (e.g., 1 for future forecast, 2 for the first back test fold, etc.)
      - Best_Model: indicates if this model was selected as the best model for the time series (Yes, No)
      - Horizon: forecast horizon step (1, 2, ..., n)
      - Date: timestamp of the forecast
      - Target: actual value, only available for back test periods
      - Forecast: forecasted value
      - lo_95: lower bound of the 95% prediction interval (future forecasts only)
      - hi_95: upper bound of the 95% prediction interval (future forecasts only)
      - lo_80: lower bound of the 80% prediction interval (future forecasts only)
      - hi_80: upper bound of the 80% prediction interval (future forecasts only)
    - get_best_agent_run(agent_info): Returns a df of the best agent run metadata inputs for each time series with the following columns:
      - combo: individual time series identifier, which is the combination of all combo variables, separated by '--'
      - model_type: how the model was trained, local for individual time series, global for all time series
      - weighted_mape: weighted mean absolute percentage error across all back test periods
      - clean_missing_values: indicates if missing values were cleaned (TRUE, FALSE)
      - clean_outliers: indicates if outliers were cleaned (TRUE, FALSE)
      - stationary: indicates if the time series was made stationary (TRUE, FALSE)
      - box_cox: indicates if a Box-Cox transformation was applied (TRUE, FALSE)
      - fourier_periods: number of Fourier terms used (NA uses default values)
      - lag_periods: lag periods used in feature engineering recipe, combined as a string using '---' (e.g., '1---12---24'), value of NA means default lags were used
      - rolling_window_periods: rolling window periods used in feature engineering recipe, combined as a string using '---' (e.g., '3---7---30'), value of NA means default windows were used
      - recipes_to_run: recipes used in the model (e.g., 'R1', 'R2')
      - multistep_horizon: indicates if multistep horizon forecasting was used (TRUE, FALSE)
      - models_to_run: models trained and evaluated, combined as a string using '---' (e.g., 'arima---ets---cubist'), value of NA means all models were used
      - pca: indicates if PCA was applied in the recipe (TRUE, FALSE)
      - seasonal_period: seasonal periods used in models that can handle multiple seasonalities, combined as a string using '---' (e.g., '12---24'), value of NA means default seasonality was used
      - back_test_scenarios: number of folds used in time series cross-validation, value of NA means default number of folds was used
      - back_test_spacing: spacing between back test folds, value of NA means default spacing was used
      - feature_selection: indicates if feature selection was applied in the recipe (TRUE, FALSE)
      - negative_forecast: indicates if negative forecasts were allowed (TRUE, FALSE)
      - average_models: indicates if an average ensemble model was created (TRUE, FALSE)
    - get_eda_data(agent_info): Returns a data frame containing all exploratory data analysis results with columns:
      - Combo: time series identifier ('All' for aggregate metrics across all series, or specific combo like 'Product_A--Region_1')
      - Analysis_Type: type of EDA analysis performed
      - Metric: specific metric name within each analysis type
      - Value: numeric or character value of the metric

      Analysis types and their metrics include:
      - Data_Profile: Total_Rows, Number_Series, Min_Rows_Per_Series, Max_Rows_Per_Series, Avg_Rows_Per_Series, Negative_Count, Negative_Percent, Start_Date, End_Date
      - ACF: Lag_0, Lag_1, Lag_2, etc. (autocorrelation values at different lags)
      - PACF: Lag_0, Lag_1, Lag_2, etc. (partial autocorrelation values at different lags)
      - Stationarity: is_stationary (TRUE/FALSE indicating if series is stationary based on ADF and KPSS tests)
      - Missing_Data: total_rows, missing_count, missing_pct, longest_gap
      - Outliers: total_rows, outlier_count, outlier_pct, first_outlier_dt, last_outlier_dt
      - Additional_Seasonality: Lag_X values indicating seasonal patterns beyond primary seasonality
      - Hierarchy: hierarchy_type (none, standard, or grouped)
      - External_Regressor_Distance_Correlation: Regressor_Lag_X values showing distance correlation between regressors and target

      To filter for specific analysis or combo:
      eda_data %>% dplyr::filter(Analysis_Type == 'ACF', Combo == 'Product_A--Region_1')

    ALWAYS use the dplyr package for data manipulation.
    Be precise and efficient in your code generation."
  )

  agent_info$driver_llm <- agent_info$driver_llm$set_system_prompt(system_prompt)

  # Store workflow parameters in agent_info for tool access
  agent_info$workflow_params <- list(
    question = question
  )

  # Construct the workflow
  workflow <- list(
    start = list(
      fn = "create_analysis_plan",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        question = question
      ),
      branch = function(ctx) {
        # Check if planning was successful
        results <- ctx$results$create_analysis_plan

        if (is.null(results) || length(results) == 0) {
          cli::cli_alert_warning("No analysis steps planned")
          return(list(ctx = ctx, `next` = "generate_final_answer"))
        } else {
          # Initialize execution tracking
          ctx$analysis_plan <- results
          ctx$step_index <- 1
          ctx$analysis_results <- list()
          return(list(ctx = ctx, `next` = "execute_analysis_step"))
        }
      }
    ),
    execute_analysis_step = list(
      fn = "execute_analysis_step",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        analysis_plan = "{ctx$analysis_plan}",
        step_index = "{ctx$step_index}",
        previous_results = "{ctx$analysis_results}",
        last_error = NULL
      ),
      branch = function(ctx) {
        # Store the result from the last step execution
        if (!is.null(ctx$results$execute_analysis_step)) {
          step_name <- paste0("step_", ctx$step_index)
          ctx$analysis_results[[step_name]] <- ctx$results$execute_analysis_step
        }

        # Check if there are more steps to execute
        ctx$step_index <- ctx$step_index + 1

        if (ctx$step_index <= length(ctx$analysis_plan)) {
          cli::cli_alert_info(paste0("Executing step ", ctx$step_index, "/", length(ctx$analysis_plan)))
          return(list(ctx = ctx, `next` = "execute_analysis_step"))
        } else {
          cli::cli_alert_success("All analysis steps completed, generating answer")
          return(list(ctx = ctx, `next` = "generate_final_answer"))
        }
      }
    ),
    generate_final_answer = list(
      fn = "generate_final_answer",
      `next` = "stop",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        agent_info = agent_info,
        question = question,
        analysis_results = "{ctx$analysis_results}"
      )
    ),
    stop = list(fn = NULL)
  )

  # Initialize context
  init_ctx <- list(
    node = "start",
    results = list(), # where each tool's output will be stored
    attempts = list(), # retry bookkeeping for execute_node()
    analysis_plan = list(), # full plan of analysis steps
    step_index = 1, # current step index
    analysis_results = list(), # collected results from each step
    agent_info = agent_info, # pass agent_info to all tools
    question = question # pass question to all tools
  )

  # Run the workflow
  final_ctx <- run_graph(agent_info$driver_llm, workflow, init_ctx)

  # Return the final answer and analysis results
  return(list(
    answer = final_ctx$results$generate_final_answer,
    results = final_ctx$analysis_results,
    plan = final_ctx$analysis_plan
  ))
}

#' Create a plan of R analysis steps to answer the question
#'
#' @param agent_info Agent info object
#' @param question The user's question
#'
#' @return List of analysis steps
#' @noRd
create_analysis_plan <- function(agent_info, question) {
  llm <- agent_info$driver_llm

  planning_prompt <- glue::glue(
    "Create a plan to answer this question using R code: '{question}'

    Available data sources:

    1. get_agent_forecast(agent_info):
       USE FOR: Future predictions, confidence intervals, back-test results, actual vs forecast comparisons, identifying which models were used
       Returns columns: Combo, Date, Forecast, Target (actuals), Run_Type, Train_Test_ID, Best_Model,
                       Model_ID, Model_Name, Recipe_ID, Horizon, lo_95, hi_95, lo_80, hi_80

    2. get_best_agent_run(agent_info):
       USE FOR: model configurations, feature engineering settings
       Returns columns: combo, weighted_mape, model_type, models_to_run, recipes_to_run,
                       clean_missing_values, clean_outliers, stationary, box_cox, fourier_periods,
                       lag_periods, rolling_window_periods, pca, feature_selection, etc.

    3. get_eda_data(agent_info):
       USE FOR: data quality issues, time series characteristics, seasonality analysis, stationarity tests
       Returns a data frame with columns: Combo, Analysis_Type, Metric, Value
       - Filter by Analysis_Type to get specific EDA results (e.g., 'ACF', 'PACF', 'Stationarity', 'Missing_Data', 'Outliers', etc.)
       - Filter by Combo to get results for specific time series
       - Value column contains the metric values (numeric or character)

    4. previous step results:
       USE FOR: Working with results from earlier steps in the analysis
       Set data_source to \"none\" or \"previous\" when you need to use results from a prior step

    Decision Rules:
    - Questions about accuracy/WMAPE/errors -> use get_agent_forecast() for detailed metrics or get_best_agent_run() for summary WMAPE
    - Questions about which specific models were used -> use get_agent_forecast() and analyze Model_ID column
    - Questions about forecasts/predictions/future values -> use get_agent_forecast()
    - Questions about models used -> check if asking about all models that were ran (get_best_agent_run) or the best model (get_agent_forecast)
    - Questions about feature engineering/transformations -> use get_best_agent_run()
    - Questions about data quality/patterns/seasonality -> use get_eda_data()
    - Questions about stationarity/ACF/PACF -> use get_eda_data() with Analysis_Type filter
    - Questions about outliers in the data -> use get_eda_data() with Analysis_Type == 'Outliers'
    - Questions about missing data patterns -> use get_eda_data() with Analysis_Type == 'Missing_Data'
    - Questions needing both forecast values AND run settings -> use BOTH sources in separate steps

    Keywords to Data Source Mapping:
    - WMAPE, MAPE, accuracy, error, performance -> get_agent_forecast()
    - forecast, prediction, future, back test, next month/year -> get_agent_forecast()
    - confidence interval, prediction interval -> get_agent_forecast()
    - outliers, missing values, transformations -> get_best_agent_run() for settings, get_eda_data() for actual counts
    - best model -> get_agent_forecast()
    - data quality, seasonality, stationarity, ACF, PACF -> get_eda_data()
    - time series characteristics, patterns -> get_eda_data()
    - external regressor correlations -> get_eda_data() with Analysis_Type == 'External_Regressor_Distance_Correlation'

    Return a JSON array of analysis steps. Each step should have:
    - description: What this step does
    - data_source: Which function to call for data (\"get_agent_forecast\", \"get_best_agent_run\", \"get_eda_data\", \"none\", or \"previous\")
    - analysis: Brief description of the R code analysis to perform
    - output_name: Variable name to store this step's result (e.g., 'accuracy_data', 'forecast_data') - THIS WILL BE AVAILABLE IN SUBSEQUENT STEPS

    Examples:
    For 'What is the average WMAPE across all time series?':
    [{{
      \"description\": \"Get accuracy metrics from best agent runs\",
      \"data_source\": \"get_best_agent_run\",
      \"analysis\": \"Calculate mean of weighted_mape column\",
      \"output_name\": \"avg_wmape\"
    }}]

    For 'Which models were used for each time series?':
    [{{
      \"description\": \"Get forecast data with model information\",
      \"data_source\": \"get_agent_forecast\",
      \"analysis\": \"Filter for Best_Model == 'Yes' and select distinct Combo and Model_ID combinations\",
      \"output_name\": \"models_used\"
    }}]

    For 'Analyze forecast bias and recommend adjustments':
    [{{
      \"description\": \"Get back-test results\",
      \"data_source\": \"get_agent_forecast\",
      \"analysis\": \"Filter for Run_Type == 'Back_Test'\",
      \"output_name\": \"backtest_data\"
    }},
    {{
      \"description\": \"Calculate errors\",
      \"data_source\": \"none\",
      \"analysis\": \"Using backtest_data, calculate error = Forecast - Target and pct_error = (Forecast - Target) / Target * 100\",
      \"output_name\": \"error_data\"
    }},
    {{
      \"description\": \"Summarize bias by series\",
      \"data_source\": \"none\",
      \"analysis\": \"Using error_data, group by Combo and calculate mean percentage error\",
      \"output_name\": \"series_bias\"
    }}]

    DO NOT call any tools. Return ONLY the JSON array."
  )

  response <- llm$chat(planning_prompt, echo = FALSE)

  # Extract JSON from response
  if (inherits(response, "list") && !is.null(response$content)) {
    plan_text <- response$content
  } else {
    plan_text <- as.character(response)
  }

  # Parse JSON
  plan_text <- gsub("```json|```", "", plan_text)
  plan_text <- trimws(plan_text)

  tryCatch(
    {
      plan <- jsonlite::fromJSON(plan_text, simplifyVector = FALSE)

      return(plan)
    },
    error = function(e) {
      cli::cli_alert_warning("Could not parse plan, using default")
      return(list(list(
        description = "Get forecast summary",
        data_source = "get_agent_forecast",
        analysis = "Summarize forecast data",
        question = question
      )))
    }
  )
}

#' Execute an analysis step by generating and running R code
#'
#' @param agent_info Agent info object
#' @param analysis_plan The full analysis plan
#' @param step_index Current step index
#' @param previous_results Results from previous steps
#' @param last_error Last error message (if any)
#'
#' @return Result of the R code execution
#' @noRd
execute_analysis_step <- function(agent_info,
                                  analysis_plan,
                                  step_index,
                                  previous_results = list(),
                                  last_error = NULL) {
  if (step_index > length(analysis_plan)) {
    return(NULL)
  }

  current_step <- analysis_plan[[step_index]]
  llm <- agent_info$driver_llm

  cli::cli_progress_step("Executing: {current_step$description}")

  # Build an explicit first-line data source call the LLM must use
  ds_call <- tryCatch(
    {
      src <- as.character(current_step$data_source %||% "get_agent_forecast")
      if (identical(src, "get_best_agent_run")) {
        # Often we want full_run_info for accuracy fields
        "get_best_agent_run(agent_info)"
      } else if (identical(src, "get_agent_forecast")) {
        "get_agent_forecast(agent_info)"
      } else if (identical(src, "get_eda_data")) {
        "get_eda_data(agent_info)"
      } else if (identical(src, "none") || identical(src, "previous")) {
        # Working with previous results
        NULL
      } else {
        # Fallback: treat as a function name taking the standard args
        sprintf("%s(agent_info)", src)
      }
    },
    error = function(...) {
      "get_agent_forecast(agent_info)"
    }
  )

  # Build context about available previous results
  previous_context <- ""
  if (length(previous_results) > 0 && step_index > 1) {
    # Map step names to output names from the plan
    available_objects <- c()
    for (i in 1:(step_index - 1)) {
      if (i <= length(analysis_plan) && !is.null(previous_results[[paste0("step_", i)]])) {
        output_name <- analysis_plan[[i]]$output_name
        available_objects <- c(available_objects, paste0(output_name, " (from step ", i, ")"))
      }
    }
    if (length(available_objects) > 0) {
      previous_context <- paste0(
        "\n\nAvailable objects from previous steps:\n",
        paste("- ", available_objects, collapse = "\n")
      )
    }
  }

  # Generate R code for this step
  code_prompt <- glue::glue(
    "You are writing R code that will be executed inside an R environment that ALREADY contains:
    - agent_info (list-like)  [DO NOT create or modify it]
    - functions: get_agent_forecast(), get_best_agent_run(), get_eda_data()
    {previous_context}

    Task: {current_step$analysis}
    Step description: {current_step$description}
    Expected output name: {current_step$output_name}
    Last error: {ifelse(is.null(last_error), 'None', last_error)}

    HARD RULES:
    - NEVER write placeholder values like \"your_agent_info\".
    - NEVER assign to agent_info.
    - Do NOT call library(); always attach the package to the function using ::.
    - ONLY USE these specific R libraries: dplyr, feasts, foreach, generics, glue, gtools,
      lubridate, plyr, purrr, rlang, stringr, tibble, tidyr, tidyselect, timetk
    - If last error is not none, it contains the error message from the last attempt to run R code, YOU MUST fix the code accordingly
    - NEVER call any tools, just generate the R code

    Data Loading Rules:
    {ifelse(!is.null(ds_call), paste0('- FIRST LINE MUST load data exactly like:\n      data <- ', ds_call), paste0('- This step uses data from a previous step named \"', ifelse(step_index > 1 && (step_index - 1) <= length(analysis_plan), analysis_plan[[step_index - 1]]$output_name, 'previous_result'), '\"\n- Access it directly by its name (it\\'s already in the environment)'))}

    - Use dplyr verbs for manipulation.
    - Put your final output in a variable named result.
    - Output ONLY raw R code (no backticks, no prose).

    Special notes:
    - For get_eda_data(): Returns a data frame with columns: Combo, Analysis_Type, Metric, Value
    - For get_agent_forecast():
      * Model_ID is the PRIMARY model identifier - ALWAYS include it in your results
      * When Model_Name is NA, it indicates an ensemble/average model
      * Model_ID for ensembles contains multiple models separated by '_'
    - When calculating metrics like MAPE, ensure you group by BOTH Combo AND Model_ID to maintain model information"
  )

  code_response <- llm$chat(code_prompt, echo = FALSE)

  # Extract raw code
  r_code <- if (inherits(code_response, "list") && !is.null(code_response$content)) {
    code_response$content
  } else {
    as.character(code_response)
  }

  # Clean up and apply guardrails against placeholders / reassignments
  r_code <- gsub("```r|```R|```", "", r_code)
  r_code <- trimws(r_code)
  # Strip any illegal reassignments to pre-bound variables
  r_code <- gsub("(?m)^\\s*(agent_info)\\s*<-.*$", "", r_code, perl = TRUE)
  # Replace any explicit placeholders that slipped through
  r_code <- gsub('"your_agent_info"|\'your_agent_info\'', "agent_info", r_code)
  # Normalize spacing
  r_code <- gsub("\n{3,}", "\n\n", r_code)

  # Execute the R code in the prepared environment
  result <- execute_r_code(
    code = r_code,
    agent_info = agent_info,
    previous_results = previous_results,
    analysis_plan = analysis_plan,
    step_index = step_index
  )

  if (is.null(result)) {
    stop("code execution failed, stopping workflow.", call. = FALSE)
  }

  cli::cli_progress_done()
  return(result)
}

#' Execute R code safely
#'
#' @param code R code to execute
#' @param agent_info Agent info object
#' @param previous_results Previous step results
#' @param analysis_plan The full analysis plan (optional)
#' @param step_index Current step index (optional)
#'
#' @return Result of code execution
#' @noRd
execute_r_code <- function(code,
                           agent_info,
                           previous_results = list(),
                           analysis_plan = NULL,
                           step_index = NULL) {
  # Create execution environment with necessary objects
  exec_env <- new.env(parent = globalenv())
  exec_env$agent_info <- agent_info
  exec_env$get_agent_forecast <- get_agent_forecast
  exec_env$get_best_agent_run <- get_best_agent_run
  exec_env$get_eda_data <- get_eda_data

  # Add previous results to environment with their proper names
  if (!is.null(analysis_plan) && !is.null(step_index) && length(previous_results) > 0) {
    # Map step_X results to their output_name from the plan
    for (i in 1:(step_index - 1)) {
      step_name <- paste0("step_", i)
      if (step_name %in% names(previous_results) && i <= length(analysis_plan)) {
        output_name <- analysis_plan[[i]]$output_name
        if (!is.null(output_name)) {
          exec_env[[output_name]] <- previous_results[[step_name]]
        }
      }
    }
  } else {
    # Fallback: add all previous results as-is
    for (name in names(previous_results)) {
      exec_env[[name]] <- previous_results[[name]]
    }
  }

  # Load required packages in the environment
  eval(quote(suppressPackageStartupMessages(requireNamespace("dplyr"))), envir = exec_env)
  eval(quote(suppressPackageStartupMessages(requireNamespace("tidyr"))), envir = exec_env)

  cli::cli_alert_info("Executing R code...")

  # Execute the code
  result <- tryCatch(
    {
      # Parse and evaluate the code
      parsed_code <- parse(text = code)
      for (expr in parsed_code) {
        eval(expr, envir = exec_env)
      }

      # Get the result variable if it exists
      if (exists("result", envir = exec_env)) {
        exec_env$result
      } else {
        # Return the last evaluated expression
        eval(parsed_code[length(parsed_code)], envir = exec_env)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Error executing code: {e$message}")
      cli::cli_alert_info("Attempted code:\n{code}")
      stop(paste0("Error executing code: ", e$message), call. = FALSE)
      NULL
    }
  )

  return(result)
}

#' Generate final answer based on analysis results
#'
#' @param agent_info Agent info object
#' @param question The user's question
#' @param analysis_results Results from all analysis steps
#'
#' @return Character string with the answer
#' @noRd
generate_final_answer <- function(agent_info, question, analysis_results) {
  llm <- agent_info$driver_llm

  # Format analysis results for context
  context_parts <- list()

  for (name in names(analysis_results)) {
    result <- analysis_results[[name]]

    if (is.data.frame(result)) {
      # Format data frames as tables
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result:\n",
        utils::capture.output(print(head(result, 20))) %>%
          paste(collapse = "\n")
      )
    } else if (is.list(result)) {
      # Format lists
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result:\n",
        utils::capture.output(utils::str(result)) %>%
          paste(collapse = "\n")
      )
    } else if (is.numeric(result) || is.character(result)) {
      # Format simple values
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result: ",
        paste(result, collapse = ", ")
      )
    } else {
      # Default formatting
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result:\n",
        utils::capture.output(print(result)) %>%
          paste(collapse = "\n")
      )
    }
  }

  full_context <- paste(context_parts, collapse = "\n")

  # Create answer prompt
  answer_prompt <- glue::glue(
    "Based on the following analysis results, provide a clear answer to this question: {question}

    Analysis Results:
    {full_context}

    Instructions:
    - Provide a clear, concise answer using plain text.
    - Assume the user has no technical background. So avoid jargon and explain concepts simply.
    - Reference specific numbers and findings from the analysis.
    - Format numbers appropriately (e.g., percentages, decimals)
    - If the analysis shows a table, describe the key findings
    - No markdown formatting"
  )

  cli::cli_progress_step("Generating answer...")
  response <- llm$chat(answer_prompt, echo = FALSE)
  cli::cli_progress_done()

  # Extract answer text
  if (inherits(response, "list") && !is.null(response$content)) {
    answer_text <- response$content
  } else {
    answer_text <- as.character(response)
  }

  return(answer_text)
}

#' Display the formatted answer
#'
#' @param answer The answer text to display
#' @noRd
display_answer <- function(answer) {
  cli::cli_h3("Answer:")

  # Clean any residual markdown
  answer <- clean_markdown(answer)

  # Display the answer
  cat(answer, "\n")
}

#' Clean markdown formatting from text
#'
#' @param text Character string potentially containing markdown
#' @return Character string with markdown removed
#' @noRd
clean_markdown <- function(text) {
  # Remove markdown bold
  text <- gsub("\\*\\*(.+?)\\*\\*", "\\1", text)
  text <- gsub("__(.+?)__", "\\1", text)

  # Remove markdown italic
  text <- gsub("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", "\\1", text, perl = TRUE)
  text <- gsub("(?<!_)_([^_]+?)_(?!_)", "\\1", text, perl = TRUE)

  # Remove markdown headers
  text <- gsub("^#{1,6}\\s+", "", text, perl = TRUE, useBytes = FALSE)

  # Remove markdown code blocks
  text <- gsub("```[^`]*```", "", text)
  text <- gsub("`([^`]+)`", "\\1", text)

  # Clean up extra whitespace
  text <- gsub("\\n{3,}", "\n\n", text)
  text <- trimws(text)

  return(text)
}
