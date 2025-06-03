
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
  • local_driver_variables: <<drivers>>
  
  -----Exploratory Data Analysis-----
  <<eda>>
  
  -----RULES (MUST / MUST NOT)-----
  1. YOU MUST output exactly one JSON object matching the schema below.  
  2. YOU MUST include a "reasoning" field with ≤ 250 words.
  
  -----OUTPUT FORMAT-----
  <scratchpad>
  …your chain-of-thought, cite Rules #…
  </scratchpad>
  ```json
  {
    "models"                : "arima---ets",
    "clean_missing_values"  : "TRUE|FALSE",
    "clean_outliers"        : "TRUE|FALSE",
    "negative_forecast"     : "TRUE|FALSE",
    "forecast_approach"     : "bottoms_up|standard_hierarchy|grouped_hierarchy",
    "stationary"            : "TRUE|FALSE",
    "feature_selection"     : "TRUE|FALSE",
    "multistep_horizon"     : "TRUE|FALSE",
    "local_driver_variables": "NULL|var1---var2",
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
  
  print(cat(final_prompt))
  
  # send prompt to LLM
  response <- llm$chat(final_prompt, echo = FALSE)
  
  print(cat(response))
  # return(response)
}