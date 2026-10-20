# Approved synthetic proportional rule. Its independent oracle is a constant
# target/driver ratio, evaluated against supplied future driver values.
custom_driver_model <- function(name = "driver_rule", driver = "Driver") {
  definition <- finnts:::new_custom_model_definition(
    name, "Multiply the supplied driver by the historical target/driver ratio.",
    "Fit the mean target/driver ratio on analysis rows only.", c("local", "global"),
    c(fit = "function(data, context, parameters) mean(data$Target / data[[parameters$driver]])",
      predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=object * new_data[[context$driver]])"),
    list(predictors = driver, recipes = "C1", target_scale = "original",
      date_types = "month", forecast_horizon = c(1, 2), missing_data = "error"),
    fixed_parameters = list(driver = driver))
  definition$source[["predict"]] <- paste0(
    "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=object * new_data[['",
    driver, "']])")
  definition$version_id <- finnts:::custom_model_definition_digest(definition)
  structure(list(definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent proportional-rule arithmetic")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE,
      allow_code = TRUE)), class = "finnts_custom_model")
}

# Two years of history plus two supplied future rows. Drivers have distinct
# values, and only Builtin is selected for ordinary recipes.
custom_driver_data <- function(series = 1L) {
  data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 26), series),
    id = rep(paste0("series", seq_len(series)), each = 26),
    value = rep(c(2 * (11:34), NA, NA), series),
    Driver = rep(11:36, series), Other = rep((11:36)^2, series),
    Builtin = rep(101:126, series))
}

# Standard preparation arguments, without fitting or artifact fabrication.
# Callers can replace the data/xregs to probe independent C1 availability.
custom_driver_prep <- function(info, data = custom_driver_data(), xregs = "Builtin") {
  list(run_info = info, input_data = data, combo_variables = "id",
    target_variable = "value", date_type = "month", forecast_horizon = 2,
    hist_end_date = as.Date("2021-12-01"), external_regressors = xregs,
    recipes_to_run = c("R1", "R2", "C1"), stationary = FALSE, box_cox = FALSE,
    clean_missing_values = FALSE, clean_outliers = FALSE)
}

# Public offline Agent setup with a custom-only driver and an unrelated built-in
# candidate driver. The Chat returns fixed proposals without network access.
custom_driver_agent <- function(path, series = 1L) {
  args <- custom_agent_inputs(path, series)
  args$input_data <- custom_driver_data(series)
  args$forecast_horizon <- 2
  args$hist_end_date <- as.Date("2021-12-01")
  args$external_regressors <- "Builtin"
  args$custom_models <- list(driver_rule = custom_driver_model())
  args$llm <- custom_agent_chat(custom_agent_response("driver_rule"))
  args
}
