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

#' Generate Tool Specification (Single Source of Truth)
#'
#' Generates a detailed tool specification string describing available functions,
#' their purposes, and hierarchy/feature rules for Finn Agent workflows.
#'
#' @param agent_info Agent info object. Used to tailor the tool specification if needed.
#' @return Character string containing the tool specification for the agent.
#' @noRd
get_tool_spec <- function(agent_info) {
  glue::glue(
    "TOOL SPECIFICATION

Functions (return data frames unless noted):

1. get_agent_forecast(agent_info):
   Use for: future values, backtests, intervals, which model(s) were used.

2. get_best_agent_run(agent_info):
   Use for: run settings, WMAPE summary.

3. get_eda_data(agent_info):
   Use for: data quality, ACF/PACF, stationarity, missing/outliers, seasonality.

4. get_summarized_models(agent_info):
   Use for: specs, coefficients, importance, diagnostics.

5. get_hierarchy_summary(agent_info): only if forecast_approach != 'bottoms_up'
   Use for: mapping hierarchy levels to bottom series.

HIERARCHY RULES:
- bottoms_up: no hierarchy. Don't call get_hierarchy_summary().
- standard_hierarchy / grouped_hierarchy: use get_hierarchy_summary() to map
  hierarchy levels <-> bottom combos. EDA/best_run/model summary at hierarchy level;
  forecasts at bottom level; reconciled forecasts appear under Model_ID == \"Best-Model\".

FEATURE NAME DECODING:
- Target_lagN: lagged target values
- Target_lagN_rollM_Sum/Avg/StdDev/Min/Max: rolling window statistics
- Date_month.lbl_*: month indicators
- Date_sinP_KH, Date_cosP_KH: Fourier seasonal terms
- Date_diff: time differences
- regressor_lag/squared_lag/log_lag/cubed_lag: transformed regressors

KEYWORD -> SOURCE HINTS:
- WMAPE/accuracy/error -> get_agent_forecast or get_best_agent_run (summary)
- forecast/prediction/intervals/back test -> get_agent_forecast
- model used/specs/importance/diagnostics -> get_summarized_models (+ Best_Model filter)
- data quality/stationarity/ACF/PACF/outliers/missing -> get_eda_data
- hierarchy/aggregation/rollup/parent/child -> get_hierarchy_summary

JOIN MAP:
- get_agent_forecast$Combo <-> get_hierarchy_summary$Bottom_Combo
- get_eda_data$Combo <-> get_hierarchy_summary$Hierarchy_Combo
- get_best_agent_run$combo <-> get_hierarchy_summary$Hierarchy_Combo
- get_summarized_models$Combo <-> get_hierarchy_summary$Hierarchy_Combo"
  )
}

#' Generate Column Cards (Selective Metadata)
#'
#' @param data_sources Character vector of data source names to include
#' @return Character string with column metadata
#' @noRd
get_column_cards <- function(data_sources) {
  cards <- list(
    get_agent_forecast = "
get_agent_forecast KEY COLUMNS:
- Combo - str - bottom-level series id - dimension - joins: get_hierarchy_summary$Bottom_Combo - GOTCHA: not a hierarchy level name
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- Date - date - period timestamp - dimension - GOTCHA: respect project granularity
- Run_Type - enum {Back_Test, Future_Forecast} - flag - GOTCHA: Target is NA for Future_Forecast
- Train_Test_ID - int - backtest fold id - dimension - GOTCHA: 1 = future block; >=2 = historical tests
- Horizon - int - step-ahead index (1..n) - dimension - GOTCHA: don't mix horizons when summarizing
- Target - num - actuals (backtest only) - measure - GOTCHA: NA in future, compute errors only where present
- Forecast - num - prediction - measure
- lo_95/hi_95; lo_80/hi_80 - num - prediction intervals - measure - GOTCHA: often only present for future
- Best_Model - enum {Yes, No} - flag - GOTCHA: summarize on Best_Model=='Yes' unless need all candidates
- Model_ID - str - primary model key (ensembles joined by '_'; 'Best-Model'=reconciled hierarchy) - key - joins: get_summarized_models$Model_ID - GOTCHA: prefer Model_ID over Model_Name for joins
- Model_Name - str|NA - human label (NA=simple average) - label - GOTCHA: only split Model_ID if truly need components
- Model_Type - enum {local, global} - flag - local=per series; global=all series
- Recipe_ID - str - feature recipe id - label",
    get_best_agent_run = "
get_best_agent_run KEY COLUMNS:
- combo - str - series id - dimension - joins: get_hierarchy_summary$Hierarchy_Combo, get_eda_data$Combo, get_summarized_models$Combo - GOTCHA: not same as bottom Combo
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- weighted_mape - num - summary error over backtests (lower better) - measure - GOTCHA: prefer this for headline accuracy
- model_type - enum {local, global} - flag
- clean_missing_values/clean_outliers/stationary/box_cox/pca/feature_selection/negative_forecast/average_models - bool - flags
- fourier_periods/seasonal_period - str - seasonality terms - parameter - GOTCHA: NA=defaults
- lag_periods/rolling_window_periods - str - hyphen-delimited lists (e.g., '1---12---24') - parameter - GOTCHA: parse before numeric use
- recipes_to_run/models_to_run - str - hyphen-delimited candidates - parameter - GOTCHA: NA='all'",
    get_eda_data = "
get_eda_data KEY COLUMNS:
- Combo - str - series id - dimension - joins: get_hierarchy_summary$Hierarchy_Combo, get_best_agent_run$combo, get_summarized_models$Combo
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- Analysis_Type - enum {Data_Profile, ACF, PACF, Stationarity, Missing_Data, Outliers, Additional_Seasonality, External_Regressor_Distance_Correlation} - selector
- Metric - str - metric key based on analysis type - dimension
  * Data_Profile: Data_Profile: Total_Rows, Number_Series, Min_Rows_Per_Series, Max_Rows_Per_Series, Avg_Rows_Per_Series, Negative_Count, Negative_Percent, Start_Date, End_Date
  * ACF: Lag_0, Lag_1, Lag_2, etc. (autocorrelation values at different lags)
  * PACF: Lag_0, Lag_1, Lag_2, etc. (partial autocorrelation values at different lags)
  * Stationarity: is_stationary (TRUE/FALSE indicating if series is stationary based on ADF and KPSS tests)
  * Missing_Data: total_rows, missing_count, missing_pct, longest_gap
  * Outliers: total_rows, outlier_count, outlier_pct, first_outlier_dt, last_outlier_dt
  * Additional_Seasonality: Lag_X values indicating seasonal patterns beyond primary seasonality
  * External_Regressor_Distance_Correlation: Regressor_Lag_X values showing distance correlation between regressors and target
- Value - num|str - metric value - measure - GOTCHA: coerce to numeric when needed",
    get_summarized_models = "
get_summarized_models KEY COLUMNS:
- Combo - str - series id - dimension - joins: get_hierarchy_summary$Hierarchy_Combo, get_best_agent_run$combo, get_eda_data$Combo
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- Model_ID - str - primary model key - key - joins: get_agent_forecast$Model_ID
- Model_Name - str - model family - label
- Model_Type - enum {local, global} - flag
- Best_Model - enum {Yes, No} - flag - GOTCHA: for simple averages, each component may be flagged Yes
- section - enum {predictor, outcome, recipe_step, model_arg, engine_param, coefficient, importance, diagnostic} - selector
- name - str - item within section - dimension
- value - str|num - item value (often numeric stored as text) - measure - GOTCHA: convert with as.numeric carefully",
    get_hierarchy_summary = "
get_hierarchy_summary KEY COLUMNS:
- Hierarchy_Combo - str - hierarchy level id - key - joins: EDA/best_run/model_summary$Combo
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- Hierarchy_Level_Type - str - level type label (Total, Level 1, Country, Product, Bottom) - dimension
- Bottom_Combo - str - bottom series id - key - joins: get_agent_forecast$Combo
  * Combo format: values joined by '--' (e.g., 'US--Enterprise--Coffee')
  * To filter by component: use tidyr::separate() or stringr::str_split() to extract individual combo variables
  * Component order matches combo_variables from agent_info$project_info
- Is_Bottom - bool - true if bottom row - flag
- Parent_Level - str|NA - immediate parent - dimension"
  )

  # Filter to requested sources
  relevant_cards <- cards[intersect(names(cards), data_sources)]

  if (length(relevant_cards) == 0) {
    return("")
  }

  paste0(
    "\n\nCOLUMN CARDS (use for joins, filters, aggregations):",
    paste(relevant_cards, collapse = "\n")
  )
}

#' Get Column Sanity Checklist
#'
#' @return Character string with common pitfalls
#' @noRd
get_column_sanity_checklist <- function() {
  "
COLUMN SANITY CHECKS:
- If Run_Type=='Future_Forecast', don't use Target (it's NA)
- If computing errors, filter Run_Type=='Back_Test' AND !is.na(Target)
- For summaries, use Best_Model=='Yes' unless need all candidates
- Intervals (lo_*/hi_*) typically present only for future periods
- Don't mix hierarchy and bottom metrics without get_hierarchy_summary() mapping
- Use Model_ID (not Model_Name) as join key to get_summarized_models
- When Model_Name is NA, it's a simple average - Model_ID shows components"
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

  # Get tool specification once
  tool_spec <- get_tool_spec(agent_info)

  # Set system prompt for Q&A workflow (SHORT, ROLE-FIRST)
  system_prompt <- glue::glue(
    "ROLE
You are an R analyst that answers questions about Finn forecast results by planning small analyses, running safe R code against provided functions, and writing a concise finance-friendly answer.

MISSION
Given (a) the user question and (b) ToolSpec, plan the minimum steps needed, execute them, and explain results clearly.

CONSTRAINTS
- Never modify agent_info. Use provided functions only.
- Prefer dplyr/tidyr pipelines; qualify packages with ::
- Be exact and efficient; avoid unnecessary steps.

TOOLS
{tool_spec}

STYLE
- Be precise and concrete.
- If hierarchy is enabled (see ToolSpec), map hierarchy levels <-> bottom combos via get_hierarchy_summary() before mixing sources.
- When unsure about a mapping, state the limitation and use the safest interpretation.

OUTPUTS
- Planning: JSON that matches the provided schema.
- Code: raw R only, ends with result <- ...
- Final answer: plain text for finance readers (no markdown)."
  )

  agent_info$driver_llm <- agent_info$driver_llm$set_system_prompt(system_prompt)

  # Store workflow parameters in agent_info for tool access
  agent_info$workflow_params <- list(
    question = question,
    tool_spec = tool_spec
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

  # Determine hierarchy note
  hierarchy_note <- if (agent_info$forecast_approach != "bottoms_up") {
    "HIERARCHICAL FORECAST: Use get_hierarchy_summary() first when accessing EDA/best_run/model summary or mapping hierarchy levels to bottom combos."
  } else {
    "This is a bottoms_up forecast - no hierarchical mapping needed."
  }

  planning_prompt <- glue::glue(
    "You will create a minimal analysis plan to answer:
QUESTION: \"{question}\"

Context:
- Forecast approach: {agent_info$forecast_approach}
- Project: {agent_info$project_info$project_name}
- Target: {agent_info$project_info$target_variable}
- Combo variables: {paste(agent_info$project_info$combo_variables, collapse = ', ')}
- External regressors: {ifelse(is.null(agent_info$external_regressors), 'None', paste(agent_info$external_regressors, collapse = ', '))}
- Forecast horizon: {agent_info$forecast_horizon}

{hierarchy_note}

ToolSpec is available (see system prompt). Use it to choose sources.

Column Cards:
{get_column_cards(c('get_agent_forecast','get_best_agent_run','get_eda_data','get_summarized_models','get_hierarchy_summary'))}

Checklist:
1) Identify scope (series/timeframe/metric).
2) Pick the fewest data sources to answer.
3) If hierarchy != bottoms_up and you touch EDA/best_run/model summary, include get_hierarchy_summary() first.
4) Name outputs you will reuse (snake_case).
5) Prefer Best_Model == \"Yes\" unless you need all candidates.
6) If anwering questions about more than one series, think through possible filters and groupings to summarize the data.

Return ONLY JSON that conforms to this schema:

BEGIN_JSON_SCHEMA
{{
  \"type\": \"array\",
  \"items\": {{
    \"type\": \"object\",
    \"required\": [\"description\",\"data_source\",\"analysis\",\"output_name\"],
    \"properties\": {{
      \"description\": {{\"type\":\"string\", \"maxLength\": 180}},
      \"data_source\": {{\"type\":\"string\", \"enum\":[\"get_agent_forecast\",\"get_best_agent_run\",\"get_eda_data\",\"get_summarized_models\",\"get_hierarchy_summary\",\"none\",\"previous\"]}},
      \"analysis\": {{\"type\":\"string\", \"maxLength\": 280}},
      \"output_name\": {{\"type\":\"string\", \"pattern\":\"^[a-z][a-z0-9_]*$\"}}
    }}
  }},
  \"minItems\": 1,
  \"maxItems\": 6
}}
END_JSON_SCHEMA

EXAMPLES (resolve placeholders like <combo>, <N> from user question):

Q: What is the overall weighted MAPE?
[{{\"description\":\"Compute overall weighted MAPE across all hierarchy levels\",\"data_source\":\"get_best_agent_run\",\"analysis\":\"Calculate mean(weighted_mape, na.rm=true); also compute median for robustness\",\"output_name\":\"overall_wmape\"}}]

Q: Which model was selected as the best for each combo?
[{{\"description\":\"List best models per combo\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Best_Model=='Yes'; select distinct Combo, Model_ID, Model_Name, Model_Type\",\"output_name\":\"best_models_by_combo\"}}]

Q: Give the next 3 periods of forecast (with 80% and 95% intervals) for <combos>.
[{{\"description\":\"Get 3-step future forecasts for specified combos with intervals\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Combo in <combos> and Run_Type=='Future_Forecast' and Horizon<=3; select Combo, Date, Horizon, Forecast, lo_80, hi_80, lo_95, hi_95\",\"output_name\":\"combo_forecast_3h\"}}]

Q: Which bottom series roll up to <hierarchy_combo>, and what are their next-period forecasts?
[{{\"description\":\"Map requested hierarchy level to its bottom series\",\"data_source\":\"get_hierarchy_summary\",\"analysis\":\"Filter Hierarchy_Combo==<hierarchy_combo>; keep Bottom_Combo\",\"output_name\":\"children_map\"}},{{\"description\":\"Fetch next-step forecasts for mapped bottom series\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Combo in children_map$Bottom_Combo and Run_Type=='Future_Forecast' and Horizon==1; select Combo, Date, Forecast, lo_80, hi_80, lo_95, hi_95\",\"output_name\":\"child_h1_forecasts\"}}]

Q: What are the top 5 most important features in the best XGBoost models (per series)?
[{{\"description\":\"Top 5 importance features for best XGBoost models by series\",\"data_source\":\"get_summarized_models\",\"analysis\":\"Filter Best_Model=='Yes' and Model_Name=='xgboost' and section=='importance'; coerce value to numeric; group by Combo; take top 5 by value\",\"output_name\":\"top5_xgb_features\"}}]

Q: Where do we have missing data and outliers? Provide a compact summary by hierarchy level.
[{{\"description\":\"Summarize missing data and outliers from EDA\",\"data_source\":\"get_eda_data\",\"analysis\":\"Filter Analysis_Type in ('Missing_Data','Outliers'); coerce Value numeric when applicable; reshape to concise per-Combo summary\",\"output_name\":\"missing_outlier_summary\"}}]

Q: Is there systematic bias in backtests? Report mean percentage error (MPE) per series and overall.
[{{\"description\":\"Load backtest rows for best models\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Run_Type=='Back_Test' and Best_Model=='Yes' and !is.na(Target)\",\"output_name\":\"bt\"}},{{\"description\":\"Compute MPE by series and overall\",\"data_source\":\"previous\",\"analysis\":\"From bt, compute pct_error=(Forecast-Target)/Target*100; group by Combo and Model_ID for mean; also compute overall mean\",\"output_name\":\"bias_summary\"}}]

Q: Why was the best model chosen for <combo>? Compare candidate models by backtest WMAPE and rank them.
[{{\"description\":\"Load backtest rows for the specified combo across all candidate models\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Combo==<combo> and Run_Type=='Back_Test' and !is.na(Target)\",\"output_name\":\"bt_combo\"}},{{\"description\":\"Compute WMAPE per model and rank ascending\",\"data_source\":\"previous\",\"analysis\":\"wmape=sum(abs(Forecast-Target))/sum(Target)*100 by Model_ID; sort ascending; flag the lowest as best\",\"output_name\":\"model_wmape_rank\"}}]

Q: Show key diagnostics (AIC, BIC, RMSE) for best models at each hierarchy level.
[{{\"description\":\"Collect AIC, BIC, RMSE for best models at each level\",\"data_source\":\"get_summarized_models\",\"analysis\":\"Filter Best_Model=='Yes' and section in ('engine_param','diagnostic') and name in ('AIC','BIC','RMSE'); coerce value numeric; reshape to one row per Combo\",\"output_name\":\"best_model_diagnostics\"}}]

Q: Which bottom series contribute most to next period's total forecast? Return top <N> with shares.
[{{\"description\":\"Get bottom series for Total level\",\"data_source\":\"get_hierarchy_summary\",\"analysis\":\"Filter Hierarchy_Combo=='Total'; keep Bottom_Combo\",\"output_name\":\"total_children\"}},{{\"description\":\"Get next-step forecasts for those bottom series\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Combo in total_children$Bottom_Combo and Run_Type=='Future_Forecast' and Horizon==1; keep Combo and Forecast\",\"output_name\":\"h1_forecasts\"}},{{\"description\":\"Compute contribution shares and return top N\",\"data_source\":\"previous\",\"analysis\":\"share=Forecast/sum(Forecast); sort desc; take top <N>; include cumulative share\",\"output_name\":\"top_contributors\"}}]

Q: What is the forecast for all US series for the next period?
[{{\"description\":\"Get forecasts and split Combo to filter by Country\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Run_Type=='Future_Forecast' and Horizon==1; use tidyr::separate(Combo, into=agent_info$project_info$combo_variables, sep='--', remove=FALSE) to split; filter Country=='US'; select Combo, Date, Forecast, lo_80, hi_80\",\"output_name\":\"us_h1_forecasts\"}}]

Q: Which segments were the most challenging to forecast and why?
[{{\"description\":\"Get backtest errors for best models\",\"data_source\":\"get_agent_forecast\",\"analysis\":\"Filter Run_Type=='Back_Test' and Best_Model=='Yes' and !is.na(Target); compute abs_pct_error=abs((Forecast-Target)/Target)*100 by row\",\"output_name\":\"bt_errors\"}},{{\"description\":\"Split Combo and aggregate errors by Segment\",\"data_source\":\"previous\",\"analysis\":\"Use tidyr::separate(Combo, into=agent_info$project_info$combo_variables, sep='--', remove=FALSE); group by Segment; compute mean_ape, median_ape, std_ape; rank desc by mean_ape\",\"output_name\":\"segment_errors\"}},{{\"description\":\"Get EDA flags for top 3 challenging segments\",\"data_source\":\"get_eda_data\",\"analysis\":\"Filter Analysis_Type in ('Missing_Data','Outliers','Stationarity'); split Combo to extract Segment; filter Segment in top 3 from segment_errors; summarize issue counts\",\"output_name\":\"segment_issues\"}}]

Return ONLY the JSON array."
  )

  response <- llm$chat(planning_prompt, echo = FALSE)

  # Extract JSON from response
  if (inherits(response, "list") && !is.null(response$content)) {
    plan_text <- response$content
  } else {
    plan_text <- as.character(response)
  }

  # Parse JSON - look for content between BEGIN_JSON_SCHEMA and END_JSON_SCHEMA markers or clean up
  plan_text <- gsub("BEGIN_JSON_SCHEMA.*?END_JSON_SCHEMA", "", plan_text, perl = TRUE)
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
        output_name = "forecast_summary"
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

  # Build data source call
  ds_call <- tryCatch(
    {
      src <- as.character(current_step$data_source %||% "get_agent_forecast")
      if (identical(src, "get_best_agent_run")) {
        "get_best_agent_run(agent_info)"
      } else if (identical(src, "get_agent_forecast")) {
        "get_agent_forecast(agent_info)"
      } else if (identical(src, "get_eda_data")) {
        "get_eda_data(agent_info)"
      } else if (identical(src, "get_summarized_models")) {
        "get_summarized_models(agent_info)"
      } else if (identical(src, "get_hierarchy_summary")) {
        "get_hierarchy_summary(agent_info)"
      } else if (identical(src, "none") || identical(src, "previous")) {
        NULL
      } else {
        sprintf("%s(agent_info)", src)
      }
    },
    error = function(...) {
      "get_agent_forecast(agent_info)"
    }
  )

  # Collect data sources used in current and previous steps for column cards
  data_sources_used <- c()
  if (!is.null(current_step$data_source) &&
    !current_step$data_source %in% c("none", "previous")) {
    data_sources_used <- c(data_sources_used, current_step$data_source)
  }
  # Add sources from previous steps that might be referenced
  if (length(previous_results) > 0 && step_index > 1) {
    for (i in 1:(step_index - 1)) {
      if (i <= length(analysis_plan)) {
        prev_source <- analysis_plan[[i]]$data_source
        if (!is.null(prev_source) && !prev_source %in% c("none", "previous")) {
          data_sources_used <- c(data_sources_used, prev_source)
        }
      }
    }
  }
  data_sources_used <- unique(data_sources_used)

  # Get column cards for relevant data sources
  column_cards <- get_column_cards(data_sources_used)
  column_sanity <- get_column_sanity_checklist()

  # Build context about available previous results
  prev_output <- NULL
  if (length(previous_results) > 0 && step_index > 1) {
    for (i in 1:(step_index - 1)) {
      if (i <= length(analysis_plan) && !is.null(previous_results[[paste0("step_", i)]])) {
        output_name <- analysis_plan[[i]]$output_name
        if (i == step_index - 1) {
          prev_output <- output_name
        }
      }
    }
  }

  # Build data loading section
  data_loading_section <- if (!is.null(ds_call)) {
    paste0("FIRST LINE MUST BE:\ndata <- ", ds_call)
  } else {
    paste0(
      'This step uses a previous object named "',
      ifelse(!is.null(prev_output), prev_output, "previous_result"),
      '". Access it directly.'
    )
  }

  # Build function list based on forecast approach
  function_list <- if (agent_info$forecast_approach != "bottoms_up") {
    "get_agent_forecast(), get_best_agent_run(), get_eda_data(), get_summarized_models(), get_hierarchy_summary()"
  } else {
    "get_agent_forecast(), get_best_agent_run(), get_eda_data(), get_summarized_models()"
  }

  # Generate R code for this step
  code_prompt <- glue::glue(
    "You are writing R code to execute one plan step.

Environment already contains:
- agent_info (do not modify)
- functions: {function_list}
- Any previous step outputs named per the plan.

Task: {current_step$analysis}
Step: {current_step$description}
Expected output object: {current_step$output_name}
Last error (if any): {ifelse(is.null(last_error), 'None', last_error)}

Rules (concise):
- Use only namespaced verbs (e.g., dplyr::, tidyr::). No library().
- End with result <- {current_step$output_name} or directly assign result <- ...
- Keep code idempotent and vectorized; no interactive calls.

Data loading (exactly one of):
{data_loading_section}

Hints:
- For feature importance: convert numeric columns as needed with suppressWarnings(as.numeric(value)).
- When grouping metrics, keep both Combo and Model_ID.
{column_cards}
{column_sanity}

Output format:
- Return RAW R code only. No comments, no prose, no backticks.

Optional helper for feature categorization:
importance %>%
  dplyr::mutate(feature_category = dplyr::case_when(
    stringr::str_detect(name, '^Target_lag\\\\d+$') ~ 'Simple Lags',
    stringr::str_detect(name, '^Target_lag.*_roll.*_(Sum|Avg|StdDev|Min|Max)') ~ 'Rolling Windows',
    stringr::str_detect(name, 'Date_month\\\\.lbl_') ~ 'Month Indicators',
    stringr::str_detect(name, 'Date_(sin|cos)\\\\d+_K\\\\d+') ~ 'Fourier Seasonality',
    stringr::str_detect(name, 'Date_') ~ 'Date Features',
    stringr::str_detect(name, '_squared_lag') ~ 'Squared Transforms',
    stringr::str_detect(name, '_log_lag') ~ 'Log Transforms',
    stringr::str_detect(name, '_cubed_lag') ~ 'Cubed Transforms',
    TRUE ~ 'External Regressors'
  ))"
  )

  code_response <- llm$chat(code_prompt, echo = FALSE)

  # Extract raw code
  r_code <- if (inherits(code_response, "list") && !is.null(code_response$content)) {
    code_response$content
  } else {
    as.character(code_response)
  }

  # Clean up code
  r_code <- gsub("```r|```R|```", "", r_code)
  r_code <- trimws(r_code)
  # Strip any illegal reassignments to pre-bound variables
  r_code <- gsub("(?m)^\\s*(agent_info)\\s*<-.*$", "", r_code, perl = TRUE)
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
  exec_env$get_summarized_models <- get_summarized_models

  # Only add get_hierarchy_summary if hierarchical
  if (agent_info$forecast_approach != "bottoms_up") {
    exec_env$get_hierarchy_summary <- get_hierarchy_summary
  }

  # Add previous results to environment with their proper names
  if (!is.null(analysis_plan) && !is.null(step_index) && length(previous_results) > 0) {
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
      parsed_code <- parse(text = code)
      for (expr in parsed_code) {
        eval(expr, envir = exec_env)
      }

      if (exists("result", envir = exec_env)) {
        exec_env$result
      } else {
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

#' Summarize analysis results for answer generation
#'
#' @param analysis_results Results from all analysis steps
#' @param max_rows Maximum rows to include per data frame
#'
#' @return Character string with summarized results
#' @noRd
summarize_analysis_results <- function(analysis_results, max_rows = 20) {
  context_parts <- list()

  for (name in names(analysis_results)) {
    result <- analysis_results[[name]]

    if (is.data.frame(result)) {
      # Cap at max_rows and provide summary stats
      n_rows <- nrow(result)
      display_rows <- min(n_rows, max_rows)

      summary_text <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result (", n_rows, " rows total, showing ", display_rows, "):\n",
        utils::capture.output(print(head(data.frame(result), display_rows))) %>%
          paste(collapse = "\n")
      )

      if (n_rows > max_rows) {
        summary_text <- paste0(summary_text, "\n[... ", n_rows - max_rows, " more rows omitted]")
      }

      context_parts[[name]] <- summary_text
    } else if (is.list(result)) {
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result:\n",
        utils::capture.output(utils::str(result, max.level = 2)) %>%
          paste(collapse = "\n")
      )
    } else if (is.numeric(result) || is.character(result)) {
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result: ",
        paste(result, collapse = ", ")
      )
    } else {
      context_parts[[name]] <- paste0(
        "\nAnalysis ", gsub("step_", "", name), " result:\n",
        utils::capture.output(print(result)) %>%
          paste(collapse = "\n")
      )
    }
  }

  return(paste(context_parts, collapse = "\n"))
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

  # Summarize analysis results (cap token usage)
  full_context <- summarize_analysis_results(analysis_results, max_rows = 20)
  # Create answer prompt (LEAN FINANCE STYLE)
  answer_prompt <- glue::glue(
    "You will answer:
\"{question}\"

Use only the material below. If something isn't present, say so plainly.

-----INPUT (summarized analysis results)-----
{full_context}
--------------------------------------------

Audience: finance professionals (non-data scientists).
Style: plain text with simple lists allowed, short paragraphs.

Checklist:
1) Start with the direct answer in 1-2 sentences.
2) Support with the key numbers (include % with two decimals; use commas for thousands).
3) If models are discussed, explain WHAT they capture in business terms (seasonality, trend, recency), not technical parameters.
4) If hierarchy is involved, name the level(s) you're summarizing.
5) If technical terms are used, explain them briefly in simple language.
6) Answer in a way that makes sense to a corporate finance reader.

Term translations (use inline, don't over-explain):
- WMAPE -> \"weighted average prediction error\" (lower is better).
- Back test -> \"historical testing period\".
- Forecast horizon -> \"number of periods predicted ahead\".

Formatting:
- 6-10 sentences total (or fewer sentences with a short list if appropriate).
- Use simple bullet lists (- or *) ONLY when listing multiple items (e.g., series names, models, features).
- NO bold, NO italics, NO headers, NO code blocks, NO tables.
- Be specific; cite the exact series and dates you used.

Return plain text with simple lists only."
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

  # Clean any residual markdown (except lists)
  answer <- clean_markdown(answer, keep_lists = TRUE)

  # Display the answer
  cat(answer, "\n")
}

#' Clean markdown formatting from text
#'
#' @param text Character string potentially containing markdown
#' @param keep_lists Logical, if TRUE keeps bullet lists
#' @return Character string with markdown removed
#' @noRd
clean_markdown <- function(text, keep_lists = FALSE) {
  # Remove markdown bold
  text <- gsub("\\*\\*(.+?)\\*\\*", "\\1", text)
  text <- gsub("__(.+?)__", "\\1", text)

  # Remove markdown italic (but preserve list markers if keep_lists = TRUE)
  if (keep_lists) {
    # Only remove italics that are NOT list markers at start of line
    text <- gsub("(?<!^)(?<!\\n)\\*([^*]+?)\\*", "\\1", text, perl = TRUE)
    text <- gsub("(?<!^)(?<!\\n)_([^_]+?)_", "\\1", text, perl = TRUE)
  } else {
    text <- gsub("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", "\\1", text, perl = TRUE)
    text <- gsub("(?<!_)_([^_]+?)_(?!_)", "\\1", text, perl = TRUE)
  }

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
