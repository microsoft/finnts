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
#' # Ask about feature importance
#' answer <- ask_agent(
#'   agent_info = agent_info,
#'   question = "What are the top 5 most important features in the xgboost model?"
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
    - Forecast Approach: {agent_info$forecast_approach}

    Hierarchical Forecasting Approach Guide:
    The forecast approach indicates how time series are aggregated and forecasted:

    1. bottoms_up: No hierarchical aggregation
       - Each time series is forecasted independently at its lowest granularity
       - No aggregation or reconciliation is performed
       - get_hierarchy_summary() is NOT available for bottoms_up approach

    2. standard_hierarchy: Nested hierarchical structure (like a pyramid)
       - Combo variables can be aggregated sequentially into each other
       - Example: City → Country → Continent → Total
         * Bottom: Kansas City, Seattle, Mexico City (individual cities)
         * Level 1: United States, Mexico (countries, sum of their cities)
         * Level 2: North America (continent, sum of countries)
         * Total: Grand total (sum of all continents)
       - Each level aggregates ALL series from the level below
       - Models are trained at each hierarchy level independently
       - Forecasts are then 'reconciled' back down to the bottom level using optimization
       - Hierarchy_Level_Type values: 'Total', 'Level 1', 'Level 2', ..., 'Bottom'
       - Parent_Level shows the immediate level above (Total → Level 1 → Level 2 → Bottom)

    3. grouped_hierarchy: Cross-cutting aggregations (multiple ways to slice data)
       - Combo variables can be aggregated in multiple different ways
       - Example: Country × Segment × Product
         * Bottom: US--Enterprise--Coffee, US--PublicSector--Coffee, Mexico--Enterprise--Tea
         * Country level: US (sum across all segments and products), Mexico
         * Segment level: Enterprise (sum across all countries and products), PublicSector
         * Product level: Coffee (sum across all countries and segments), Tea
         * Total: Grand total (sum of everything)
       - Same series can appear in multiple aggregation paths
       - Models are trained at each aggregation level (Total, each grouping variable, Bottom)
       - Forecasts are reconciled back to bottom level using optimization
       - Hierarchy_Level_Type values: 'Total', grouping variable names (e.g., 'Country', 'Segment', 'Product'), 'Bottom'
       - Parent_Level is typically 'Total' for grouping levels, and 'Multiple' for bottom (since bottom series can roll up multiple ways)

    Key Concepts for Hierarchical Forecasting:
    - RECONCILIATION: After models are trained at each hierarchy level, forecasts are mathematically adjusted
      so that when you sum up bottom-level forecasts, they equal the forecast at higher levels
    - COHERENCY: Reconciled forecasts maintain the hierarchical structure (bottom forecasts sum to top forecast)
    - Best Model Selection: Each hierarchy level gets its own best model, then all models are reconciled
    - Use get_hierarchy_summary() to understand which bottom series contribute to each aggregate level

    When explaining hierarchical forecasts to users:
    - Standard hierarchy: Emphasize the nested, pyramid-like structure
    - Grouped hierarchy: Emphasize multiple ways to aggregate the same data
    - Always mention that forecasts are reconciled to ensure mathematical consistency
    - Explain that different hierarchy levels may use different best models

    Feature Naming Convention Guide:
    When analyzing variable importance, interpret feature names as follows:
    - 'Target_lagN': Lagged values of the target variable (e.g., Target_lag1 = 1 period ago)
    - 'Target_lagN_rollM_Stat': Rolling window statistics (e.g., Target_lag1_roll12_Sum = sum of last 12 values starting from lag 1)
      * Common statistics: Sum, Avg, StdDev, Min, Max
    - 'Date_month.lbl_Month': One-hot encoded month indicators
    - 'Date_sinN_KM' / 'Date_cosN_KM': Fourier seasonal features (N = period, M = harmonic order)
    - 'Date_diff': Time difference between observations
    - 'regressor_lagN': Lagged external regressor values
    - 'regressor_squared_lagN': Squared transformation of lagged regressor
    - 'regressor_log_lagN': Log transformation of lagged regressor
    - 'regressor_cubed_lagN': Cubed transformation of lagged regressor

    When explaining feature importance:
    1. Group related features (e.g., all lag features, all seasonal features, all external regressors)
    2. Explain what the feature represents in business context
    3. Interpret why high importance makes sense given the data patterns
    4. Distinguish between different types of seasonal encoding (month indicators vs Fourier terms)
    5. Explain the practical meaning of rolling window features, polynomial transformations, and lagged regressors

    Available data sources:
    - get_agent_forecast(agent_info): Returns a df of the final forecast data with the following columns:
      - Combo: individual time series identifier, which is the combination of all combo variables, separated by '--'
      - Model_ID: THE PRIMARY MODEL IDENTIFIER - unique identifier of the specific model(s) trained.
        * For single models: combination of Model_Name, Model_Type, and Recipe_ID (e.g., 'arima--local--R1')
        * For ensemble/average models: lists all averaged models separated by '_' (e.g., 'arima--local--R1_ets--local--R1_prophet--local--R1')
        * For hierarchical forecasts: 'Best-Model' indicates a reconciled forecast that combines predictions from multiple hierarchy levels
          - When Model_ID == 'Best-Model', use get_hierarchy_summary() to understand the hierarchical structure
          - Then use get_summarized_models() with Best_Model == 'Yes' to see the actual models trained at each hierarchy level
          - Each hierarchy level (Total, Level 1, Bottom, etc.) may have used different best models
        * ALWAYS USE Model_ID to identify which model(s) were used
        * IMPORTANT: To analyze individual models within an average, split Model_ID by '_' to get component Model_IDs
      - Model_Name: name of the model type (e.g., 'arima', 'ets', 'cubist')
        * IMPORTANT: When Model_Name is NA, this indicates a SIMPLE AVERAGE model - check Model_ID for the actual models used
        * To get details about an average model, split the Model_ID and look up each component in get_summarized_models()
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
      - External_Regressor_Distance_Correlation: Regressor_Lag_X values showing distance correlation between regressors and target

      To filter for specific analysis or combo:
      eda_data %>% dplyr::filter(Analysis_Type == 'ACF', Combo == 'Product_A--Region_1')
    - get_summarized_models(agent_info): Returns a df of trained model details for each time series with the following columns:
      - Combo: time series combination identifier
      - Model_ID: unique model identifier (format: model_name--model_type--recipe)
      - Model_Name: name of the forecasting model (e.g., 'arima', 'ets', 'xgboost')
      - Model_Type: whether the model is 'local' (trained per combo) or 'global' (trained across all combos)
      - Best_Model: 'Yes' if this model is the best model for this time series, 'No' otherwise
        * When the best model is a simple average (e.g., 'arima--local--R1_ets--local--R1'), each component model is flagged as 'Yes'
        * Use Best_Model == 'Yes' to filter to only the best performing models
      - section: category of information (e.g., 'predictor', 'outcome', 'recipe_step', 'model_arg', 'engine_param', 'coefficient', 'importance', 'diagnostic')
      - name: specific name of the parameter/feature/metric
      - value: the value as a character string

      Section types and their content:
      - predictor: input variables used by the model (e.g., 'Date', 'Date_year', 'Date_month', lag features, Fourier terms, rolling statistics)
      - outcome: target variable being predicted (typically 'Target')
      - recipe_step: preprocessing steps applied, numbered sequentially (e.g., '1', '2', '3') with step descriptions as values
        * Examples: 'Remove zero variance predictors [step_zv]', 'One-hot encode categorical variables [step_dummy]'
      - model_arg: parsnip model arguments (e.g., 'seasonal_period', 'non_seasonal_ar', 'learn_rate', 'trees', 'tree_depth')
      - engine_param: actual fitted model parameters from the underlying engine
        * ARIMA: 'aic', 'bic', 'aicc', 'loglik', 'sigma2', 'order_str', 'seasonal_order_str', 'nobs', 'ljung_box_*'
        * XGBoost: 'objective', 'colsample_bytree', 'model_type'
        * TBATS: 'alpha', 'lambda', 'likelihood', 'state_space_dim', 'variance'
        * General: 'nobs', 'residuals.mean', 'residuals.sd'
      - coefficient: model coefficients (e.g., 'intercept', 'intercept.se')
        * Applies to only arima, armimax, arima-boost, and glmnet models
      - importance: feature importance scores for multivariate models
        * Values are importance scores, scaled to 100 for the most important feature
      - diagnostic: model fit statistics (e.g., 'AIC', 'BIC', 'RMSE', 'ljung_box_p_value', residual tests)
        * Note: Some diagnostics may appear in engine_param section depending on model type

      Key Model Characteristics to Explain:
      - Univariate models (ARIMA, ETS, Prophet, NNETAR, etc.): Use only historical values of the target variable
      - Multivariate models (XGBoost, Cubist, MARS, GLMNet, SVM, etc.): Can use additional input variables (features) beyond just historical values
      - Local models: Trained separately for each time series (more customized)
      - Global models: Trained across all time series together (learns common patterns)

      Use cases:
      - Understanding exact model specifications (ARIMA orders, ETS components, hyperparameters)
      - Analyzing coefficients and their significance
      - Identifying important features/predictors
      - Reviewing model diagnostics and fit quality
      - Comparing model architectures across time series
      - Examining preprocessing steps applied to each model

      Usage examples:
      - Get best models only: model_summary %>% dplyr::filter(Best_Model == 'Yes')
      - Get feature importance for best XGBoost models: model_summary %>% dplyr::filter(Best_Model == 'Yes', Model_Name == 'xgboost', section == 'importance')
      - Get ARIMA coefficients: model_summary %>% dplyr::filter(section == 'coefficient', Model_Name == 'arima')
      - Get all preprocessing steps: model_summary %>% dplyr::filter(section == 'recipe_step')
    - get_hierarchy_summary(agent_info): Returns a df mapping hierarchical time series to bottom-level series (only available when forecast approach != 'bottoms_up') with the following columns:
      - Hierarchy_Combo: The aggregated hierarchy level combo name (e.g., 'Total', 'North America', 'United States--Enterprise'), maps to Combo in get_summarized_models() get_eda_data() and get_best_agent_run()
      - Hierarchy_Level_Type: The type of hierarchical level
        * For standard hierarchy: 'Total', 'Level 1', 'Level 2', etc., or 'Bottom'
        * For grouped hierarchy: grouping variable name (based on combo variables) or 'Total', 'Bottom'
      - Bottom_Combo: Individual bottom-level series name that rolls up to this hierarchy level, maps to Combo in get_agent_forecast()
      - Is_Bottom: Logical (TRUE/FALSE) indicating if this is a bottom-level series (Hierarchy_Combo == Bottom_Combo)
      - Parent_Level: The hierarchical level above this one (NA for Total level)

      Use cases:
      - Understanding hierarchical structure and relationships between aggregated and bottom-level series
      - Finding which bottom series contribute to a specific aggregate level
      - Understanding results from get_eda_data() get_best_agent_run() and get_summarized_models() at different hierarchy levels

      IMPORTANT: This data source is ONLY available when hierarchical forecasting is enabled (forecast approach is 'standard_hierarchy' or 'grouped_hierarchy').
      If forecast approach is 'bottoms_up', this function will error.

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
    "You will create a plan to answer this question: '{question}'

    FOLLOW THIS REASONING PROCESS:

    STEP 1: UNDERSTAND THE QUESTION
    - What is the user asking for? (forecast values, accuracy metrics, model details, data quality, etc.)
    - What is the specific scope? (all time series, specific combo, specific time period, etc.)
    - What type of answer format is expected? (single number, comparison, list, explanation, etc.)
    
    Step 2: Review Project Information
    - What project details are relevant? (target variable, combo variables, forecast horizon, external regressors, forecast approach)
    - Are there any hierarchical structures to consider? (forecast approach == standard_hierarchy or grouped_hierarchy)

    STEP 3: IDENTIFY REQUIRED DATA SOURCES
    - Which data source(s) contain the information needed?
    - Do I need multiple sources to answer completely?
    - What specific columns or filters are needed from each source?

    STEP 4: DETERMINE ANALYSIS SEQUENCE
    - What order should I retrieve data in?
    - Are there dependencies (e.g., need to identify models before getting their specs)?
    - Can I combine operations or do I need separate steps?

    STEP 5: PLAN FOR EDGE CASES
    - What if the best model is a simple average (need to split Model_IDs)?
    - What if this is a hierarchical forecast with forecast approach of standard_hierarchy or grouped_hierarchy (need get_hierarchy_summary)?
    - What if numeric columns are stored as character (need to convert)?

    Now, based on this reasoning, create your analysis plan.

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

    4. get_summarized_models(agent_info):
       USE FOR: Detailed model specifications, hyperparameters, coefficients, feature importance, model diagnostics
       Returns a data frame with columns: Combo, Model_ID, Model_Name, Model_Type, Best_Model, model_class, engine, section, name, value
       - Filter by section to get specific types of information:
         * 'predictor': input variables/features used (Date features, lags, Fourier terms, etc.)
         * 'outcome': target variable
         * 'recipe_step': preprocessing steps (numbered 1, 2, 3, etc.)
         * 'model_arg': parsnip model arguments (learn_rate, trees, seasonal_period, etc.)
         * 'engine_param': fitted parameters (ARIMA orders, AIC, BIC, lambda, nobs, etc.)
         * 'coefficient': model coefficients (intercept, regressor coefficients)
         * 'importance': feature importance scores (for XGBoost, Cubist, etc.)
         * 'diagnostic': model fit statistics (may overlap with engine_param)
       - Filter by Best_Model == 'Yes' to get only the best models for each time series
       - Filter by Model_Name or Model_ID to get specific model types or instances
       - Filter by Combo to get model details for specific time series
       - IMPORTANT: value column is character type - convert to numeric when needed: as.numeric(value)

    5. get_hierarchy_summary(agent_info):
       USE FOR: Understanding hierarchical structure, mapping between aggregated and bottom-level series
       Returns a data frame with columns: Hierarchy_Combo, Hierarchy_Level_Type, Bottom_Combo, Is_Bottom, Parent_Level
       - Only available when forecast approach is 'standard_hierarchy' or 'grouped_hierarchy'
       - Use to find which bottom series contribute to an aggregate level
       - Use to analyze forecasts across different hierarchy levels
       - Use to understand parent-child relationships in the hierarchy

    6. previous step results:
       USE FOR: Working with results from earlier steps in the analysis
       Set data_source to \"none\" or \"previous\" when you need to use results from a prior step

    Decision Rules:
    - Questions about accuracy/WMAPE/errors -> use get_agent_forecast() for detailed metrics or get_best_agent_run() for summary WMAPE
    - Questions about which specific models were used -> use get_agent_forecast() and analyze Model_ID column
      * If Model_Name is NA, the best model is a simple average - split Model_ID by '_' to get components
      * If Model_ID == 'Best-Model', this is a hierarchical reconciled forecast:
        1. Use get_hierarchy_summary() to understand the hierarchical structure
        2. Use get_summarized_models() with Best_Model == 'Yes' to see models at each hierarchy level
      * Use get_summarized_models() with the component Model_IDs to get details about each model in the average
    - Questions about forecasts/predictions/future values -> use get_agent_forecast()
    - Questions about models used -> check if asking about all models that were ran (get_best_agent_run) or the best model (get_agent_forecast)
      * If the best model is a simple average, you'll need BOTH get_agent_forecast() AND get_summarized_models()
    - Questions about feature engineering/transformations -> use get_best_agent_run()
    - Questions about data quality/patterns/seasonality -> use get_eda_data()
    - Questions about stationarity/ACF/PACF -> use get_eda_data() with Analysis_Type filter
    - Questions about outliers in the data -> use get_eda_data() with Analysis_Type == 'Outliers'
    - Questions about missing data patterns -> use get_eda_data() with Analysis_Type == 'Missing_Data'
    - Questions about model specifications/parameters -> use get_summarized_models()
      * Combine with Best_Model == 'Yes' to get importance for best models only
    - Questions about model hyperparameters -> use get_summarized_models() with section == 'engine_param' or 'model_arg'
    - Questions about model coefficients -> use get_summarized_models() with section == 'coefficient'
    - Questions about feature importance -> use get_summarized_models() with section == 'importance'
      * Combine with Best_Model == 'Yes' to get importance for best models only
    - Questions about model diagnostics (AIC, BIC, RMSE) -> use get_summarized_models() with section == 'engine_param'
    - Questions about hierarchical structure/relationships -> use get_hierarchy_summary()
    - Questions about which series roll up to an aggregate level -> use get_hierarchy_summary()
    - Questions comparing forecasts across hierarchy levels -> use get_hierarchy_summary() and get_agent_forecast() together
    - Questions needing both forecast values AND run settings -> use BOTH get_agent_forecast() and get_best_agent_run() sources in separate steps
    - Questions needing both model settings AND detailed specifications -> use get_best_agent_run() AND get_summarized_models() sources in separate steps

    Keywords to Data Source Mapping:
    - WMAPE, MAPE, accuracy, error, performance -> get_agent_forecast()
    - forecast, prediction, future, back test, next month/year -> get_agent_forecast()
    - confidence interval, prediction interval -> get_agent_forecast()
    - outliers, missing values, transformations -> get_best_agent_run() for settings, get_eda_data() for actual counts
    - best model -> get_agent_forecast() for which model was best, get_summarized_models() with Best_Model == 'Yes' for details
      * No need to manually split Model_IDs - Best_Model flag handles simple averages automatically
    - simple average, ensemble, averaged models, model combination -> get_agent_forecast() to identify, then get_summarized_models() with split Model_IDs
    - data quality, seasonality, stationarity, ACF, PACF -> get_eda_data()
    - time series characteristics, patterns -> get_eda_data() for summary metrics, get_agent_forecast() for custom stats on forecasts
    - external regressor correlations -> get_eda_data() with Analysis_Type == 'External_Regressor_Distance_Correlation'
    - model hyperparameters -> get_summarized_models() with section == 'engine_param' or 'model_arg'
    - model coefficients -> get_summarized_models() with section == 'coefficient'
    - hyperparameters, tuning parameters, model settings -> get_summarized_models() with section in ('model_arg', 'engine_param')
    - feature importance, variable importance, predictor importance -> get_summarized_models() with section == 'importance'
    - which features/predictors were used -> get_summarized_models() with section == 'predictor'
    - preprocessing steps, recipe steps -> get_summarized_models() with section == 'recipe_step'
    - hierarchy, hierarchical, aggregation, roll up, parent, child, top level, bottom level -> get_hierarchy_summary() (only if hierarchical forecast)

    Return a JSON array of analysis steps. Each step should have:
    - description: What this step does
    - data_source: Which function to call for data (\"get_agent_forecast\", \"get_best_agent_run\", \"get_eda_data\", \"get_summarized_models\", \"none\", or \"previous\")
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

    For 'What are the top 5 most important features in the XGBoost models?':
    [{{
      \"description\": \"Get feature importance from XGBoost models\",
      \"data_source\": \"get_summarized_models\",
      \"analysis\": \"Filter for Model_Name == 'xgboost' and section == 'importance', convert value to numeric, arrange by Combo and desc(value), and take top 5 per Combo\",
      \"output_name\": \"top_features\"
    }}]

    For 'Analyze forecast bias and recommend adjustments':
    [{{
      \"description\": \"Get back-test results\",
      \"data_source\": \"get_agent_forecast\",
      \"analysis\": \"Filter for Run_Type == 'Back_Test' and Best_Model == 'Yes'\",
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

    For 'Explain how the best model works':
    [{{
      \"description\": \"Get best model information\",
      \"data_source\": \"get_agent_forecast\",
      \"analysis\": \"Filter for Best_Model == 'Yes' and select distinct Combo, Model_ID, Model_Name\",
      \"output_name\": \"best_models\"
    }},
    {{
      \"description\": \"Check if best models are simple averages and split Model_IDs\",
      \"data_source\": \"none\",
      \"analysis\": \"For each row in best_models, check if Model_Name is NA. If so, split Model_ID by '_' to get component model IDs. Create a data frame with Combo and individual Model_IDs\",
      \"output_name\": \"component_models\"
    }},
    {{
      \"description\": \"Get detailed specifications for component models\",
      \"data_source\": \"get_summarized_models\",
      \"analysis\": \"Filter for Model_IDs in component_models and get key specifications from sections: model_arg, engine_param, coefficient, importance\",
      \"output_name\": \"model_specs\"
    }}]

    Return ONLY the JSON array."
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
      print(plan)
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
      } else if (identical(src, "get_summarized_models")) {
        "get_summarized_models(agent_info)"
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
    - functions: get_agent_forecast(), get_best_agent_run(), get_eda_data(), get_summarized_models()
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
      * To analyze components of an average model: stringr::str_split(Model_ID, '_')[[1]]
      * Then filter get_summarized_models() results using the component Model_IDs
    - For get_summarized_models():
      * Returns detailed model information organized by section (predictor, outcome, recipe_step, model_arg, engine_param, coefficient, importance, diagnostic)
      * Filter by section to get specific types of information
      * The 'value' column may be numeric or character depending on the parameter
      * For numeric analysis, convert value column: as.numeric(value)
      * Model_ID matches the Model_ID in get_agent_forecast() for joining
      * When analyzing simple averages, filter for multiple Model_IDs using: Model_ID %in% c('model1', 'model2', 'model3')
      * Filter Best_Model == 'Yes' to get details for only the best models
    - When working with variable importance data, use these helper patterns for feature categorization:
      importance_data %>%
        dplyr::mutate(
          feature_category = dplyr::case_when(
            stringr::str_detect(name, '^Target_lag\\\\d+$') ~ 'Simple Lags',
            stringr::str_detect(name, '^Target_lag.*_roll.*_(Sum|Avg|StdDev|Min|Max)') ~ 'Rolling Windows',
            stringr::str_detect(name, 'Date_month\\\\.lbl_') ~ 'Month Indicators',
            stringr::str_detect(name, 'Date_(sin|cos)\\\\d+_K\\\\d+') ~ 'Fourier Seasonality',
            stringr::str_detect(name, 'Date_') ~ 'Date Features',
            stringr::str_detect(name, '_squared_lag') ~ 'Squared Transforms',
            stringr::str_detect(name, '_log_lag') ~ 'Log Transforms',
            stringr::str_detect(name, '_cubed_lag') ~ 'Cubed Transforms',
            TRUE ~ 'External Regressors'
          )
        )

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
  cli::cli_code(code)
  # Create execution environment with necessary objects
  exec_env <- new.env(parent = globalenv())
  exec_env$agent_info <- agent_info
  exec_env$get_agent_forecast <- get_agent_forecast
  exec_env$get_best_agent_run <- get_best_agent_run
  exec_env$get_eda_data <- get_eda_data
  exec_env$get_summarized_models <- get_summarized_models

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
        utils::capture.output(print(head(result, 100))) %>%
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

  # Create answer prompt with structured reasoning
  answer_prompt <- glue::glue(
    "You will answer this question: '{question}'

    Based on these analysis results:
    -----Analysis Results-----
    {full_context}

    FOLLOW THIS REASONING PROCESS TO CONSTRUCT YOUR ANSWER:

    STEP 1: SYNTHESIZE THE DATA
    - What are the key findings from the analysis results?
    - Are there any patterns, trends, or notable observations?
    - What numbers or facts are most relevant to the question?

    STEP 2: DETERMINE THE CORE ANSWER
    - What is the direct, concise answer to the user's question?
    - Can I answer with a single statement or do I need multiple points?
    - What is the most important information the user needs to know?

    STEP 3: IDENTIFY SUPPORTING EVIDENCE
    - What specific numbers support my answer?
    - Are there any comparisons or context that would help?
    - What details would make this answer more actionable?

    STEP 4: TRANSLATE TO BUSINESS LANGUAGE
    - What technical terms need to be explained?
    - How can I make model details accessible to non-technical users?
    - What business implications should I highlight?

    STEP 5: STRUCTURE THE RESPONSE
    - Lead with the direct answer
    - Follow with supporting evidence and numbers
    - Add context or explanation where needed
    - End with implications or recommendations if relevant

    Now, construct your answer following these guidelines:

    -----RULES FOR FINANCE-FRIENDLY EXPLANATIONS-----

    AUDIENCE: Assume the reader is a finance professional with NO data science background.

    STRUCTURE YOUR ANSWER:
    1. Start with the direct answer (1-2 sentences)
    2. Provide supporting evidence with specific numbers
    3. If discussing models, explain WHAT they do, not HOW they work
    4. End with business implications if relevant

    WHEN EXPLAINING MODELS:
    - ALWAYS use the plain-language model descriptions from the system prompt
    - NEVER use technical terms like 'hyperparameters', 'tuning', 'cross-validation' without explanation
    - If mentioning a simple average, say: 'combines predictions from X different models' instead of 'ensemble'
    - For model selection, focus on WHY it was chosen (accuracy, reliability) not technical specs
    - If describing model components (ARIMA orders, hyperparameters):
      * Only mention if directly asked
      * Translate to business meaning (e.g., 'captures 12-month seasonal pattern' not 'seasonal_period = 12')

    When explaining feature importance:
    1. Start with a summary (e.g., 'The top 3 features contribute X% of total importance')
    2. Group features by category (lags, rolling windows, seasonal, external regressors)
    3. Explain what each important feature represents in plain language
    4. Connect importance to business context (e.g., 'September is important because it's peak season')
    5. If discussing specific features, decode the naming:
       - Target_lag1_roll12_Sum = 'Sum of target values over the last 12 months, starting 1 month ago'
       - driver1_squared_lag6 = 'Squared value of driver1 from 6 periods ago'

    TECHNICAL TERM TRANSLATIONS (use these automatically):
    - 'WMAPE' or 'weighted MAPE' -> 'weighted average prediction error' (explain: lower is better, 5% means typically off by 5%)
    - 'MAPE' -> 'average prediction error'
    - 'Back test' -> 'historical testing period'
    - 'Forecast horizon' -> 'number of periods predicted into the future'
    - 'Feature engineering' -> 'data preparation and variable creation'
    - 'Lag features' -> 'using past values as predictors'
    - 'Rolling window' -> 'moving averages calculated over time'
    - 'Fourier terms' -> 'mathematical components that capture seasonal patterns'
    - 'Box-Cox transformation' -> 'mathematical adjustment to stabilize variance'
    - 'Stationarity' -> 'removing trends to focus on patterns'
    - 'Recipe' -> 'data preparation workflow'
    - 'Local model' -> 'model trained specifically for this time series'
    - 'Global model' -> 'model trained across multiple time series'

    NUMBER FORMATTING:
    - Percentages: Always include % symbol, round to 2 decimals (e.g., 4.52%)
    - Large numbers: Use commas as thousands separators (e.g., 1,234.56)
    - WMAPE/MAPE: Express as percentage, explain scale (e.g., '3.2% error means forecasts are typically within 3.2% of actual values')

    AVOID THESE MISTAKES:
    - DON'T say 'the model with the lowest WMAPE' -> SAY 'the most accurate model'
    - DON'T list technical parameters without context
    - DON'T assume knowledge of statistical concepts
    - DON'T use acronyms without defining them first
    - DON'T say 'Recipe R1' -> SAY 'feature engineering recipe #1'

    FORMAT:
    - Use plain text only (NO markdown, NO bullet points)
    - Use clear paragraph breaks for readability
    - Put the most important information first
    - If showing multiple models, group by time series and prioritize the best model

    EXAMPLE GOOD ANSWER:
    'The most accurate model for this time series is an ARIMA model, which achieved a 3.2% weighted average prediction error. This means the forecasts are typically within 3.2% of the actual values. ARIMA is a statistical model that uses historical patterns and trends to predict future values, and in this case, it was trained specifically for this individual time series (local model). The model captures a 12-month seasonal pattern and uses the past 3 months of data to make predictions.'

    EXAMPLE BAD ANSWER:
    'The best model is arima--local--R1 with WMAPE of 0.032. It has seasonal_period=12, lag_periods=1---3, and uses step_zv and step_dummy in the recipe.'
    "
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
