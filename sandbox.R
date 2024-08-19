library(devtools)
library(profvis)

# Start profiling
profvis({

#load the package
load_all()

data_tbl <- timetk::m4_monthly %>%
  dplyr::rename(Date = date) %>%
  dplyr::mutate(id = as.character(id))  %>%
  dplyr::filter(Date > as.Date("1995-01-01"))

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
  models_to_run = c("arimax", "meanf","xgboost", "arima-boost"),
  # models_to_run = c("meanf"),
)

train_models(run_info,
  run_local_models = TRUE
)

final_models(run_info,
  average_models = TRUE
)

fcst_tbl <- get_forecast_data(run_info)

# Plotting code...
library(ggplot2)
library(dplyr)

fcst_back_test <- fcst_tbl %>% filter(Run_Type == "Back_Test")
fcst_future_forecast <- fcst_tbl %>% filter(Run_Type == "Future_Forecast")

actuals <- fcst_back_test %>%
  group_by(Combo, Model_ID, Date) %>%
  summarize(Target = mean(Target)) %>%
  ungroup()

plot_fcst <- function(data, actuals, combo_id) {
  ggplot(data = data %>% filter(paste0(Combo, "_", id) == combo_id)) +
    geom_line(aes(x = Date, y = Forecast), color = "blue", linetype = "dashed") +
    geom_ribbon(aes(x = Date, ymin = lo_80, ymax = hi_80), fill = "blue", alpha = 0.3) +
    geom_ribbon(aes(x = Date, ymin = lo_95, ymax = hi_95), fill = "blue", alpha = 0.1) +
    geom_line(data = actuals %>% filter(paste0(Combo, "_", id) == combo_id),
              aes(x = Date, y = Target), color = "red") +
    labs(title = paste("Forecast vs Actuals for", combo_id),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    ) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"), aspect.ratio = 0.5)
}

combo_ids <- unique(paste0(fcst_tbl$Combo, "_", fcst_tbl$Model_ID))

plots <- lapply(combo_ids, function(combo_id) {
  plot_fcst(fcst_future_forecast, actuals, combo_id)
})

print(plots[[1]])

}) # End profiling
View(fcst_tbl)
