library(devtools)



#load the package
load_all()

data_tbl <- timetk::m4_monthly %>%
  dplyr::rename(Date = date) %>%
  dplyr::mutate(id = as.character(id))  %>%
  dplyr::filter(Date > as.Date("1995-01-01"))

data_tbl  


run_info <- set_run_info()

prep_data(run_info,
  input_data = data_tbl,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 12,
  recipes_to_run = "R1"
)

prep_models(run_info,
  # models_to_run = c("arimax", "meanf","xgboost", "arima-boost"),
  models_to_run = c("meanf"),
)

train_models(run_info,
  run_local_models = TRUE
)

final_models(run_info,
  average_models = TRUE 
)

fcst_tbl <- get_forecast_data(run_info)

View(fcst_tbl)