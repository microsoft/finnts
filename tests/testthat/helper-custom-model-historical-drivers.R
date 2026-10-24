# Build a deterministic history-growth rule with independently specified future
# price inputs. The fit stores only the last observed units and a fixed growth.
historical_driver_definition <- function(modes = "local", historical = TRUE) {
  requirements <- list(predictors = c("Units", "Price"), recipes = "C1",
    target_scale = "original", date_types = "month", forecast_horizon = 2,
    missing_data = "error", predictor_types = list(Units = "number", Price = "number"))
  if (historical) requirements$historical_only_predictors <- "Units"
  new_custom_model_definition("unit_growth", "Grow units ten percent and multiply by supplied future Price.",
    "Units are historical-only; forecast units internally at ten percent per month.",
    modes, c(fit = paste0("function(data, context, parameters) { ",
      "lapply(split(data, data$Combo), function(x) x$Units[[nrow(x)]] ) }"),
      predict = paste0("function(object, new_data, context) { ",
        "units <- vapply(new_data$Combo, function(id) object[[id]], numeric(1)); ",
        "data.frame(.finnts_row = new_data$.finnts_row, ",
        ".pred = units * 1.1^context$forecast_step * new_data$Price) }")),
    requirements)
}

# Return two observed rows and a two-period request with a literal oracle.
historical_driver_panel <- function(global = FALSE) {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
    Combo = "A", Target = c(1000, 1100), Units = c(100, 110), Price = c(10, 10))
  future <- data.frame(Date = as.Date(c("2020-03-01", "2020-04-01")),
    Combo = "A", Price = c(12, 13))
  expected <- c(1452, 1730.3)
  if (global) {
    peer <- history
    peer$Combo <- "B"
    peer$Units <- peer$Units * 2
    peer$Target <- peer$Target * 2
    request <- future
    request$Combo <- "B"
    history <- rbind(history, peer)
    future <- rbind(future, request)
    expected <- c(expected, 2904, 3460.6)
  }
  list(history = history, future = future, expected = expected)
}

# Approved envelope for Agent/standard callers; validation evidence is passive
# and pinned to this deterministic definition, with no provider execution.
historical_driver_model <- function() {
  definition <- historical_driver_definition(c("local", "global"))
  structure(list(definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent units-growth oracle.")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE,
      allow_code = TRUE)), class = "finnts_custom_model")
}

# Four independent small finance oracles. Literal expectations do not execute
# candidate source; each rule explicitly specifies any internal extrapolation.
historical_finance_case <- function(family = "integrated", global = FALSE) {
  historical <- switch(family, customer_growth = "Active_Customers",
    unit_trend = "Units_Sold", productivity = c("Active_Customers", "Units_Sold"),
    integrated = c("Active_Customers", "Units_Sold"))
  future <- switch(family, customer_growth = character(), unit_trend = "Price_Index",
    productivity = character(), integrated = c("Price_Index", "Marketing_Spend", "Promo_Flag"))
  predictors <- c(historical, future)
  history <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    Combo = "A", Target = rep(1000, 12), Active_Customers = 89:100,
    Units_Sold = seq(90, 200, by = 10), Price_Index = 10,
    Marketing_Spend = 100, Promo_Flag = 0)
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01")), Combo = "A",
    Price_Index = c(12, 13), Marketing_Spend = c(100, 200), Promo_Flag = c(0, 1))
  expected <- switch(family, customer_growth = c(1100, 1210), unit_trend = c(2520, 2860),
    productivity = c(2000, 2000), integrated = c(2904, 4089.8))
  expression <- switch(family,
    customer_growth = "customers * 1.1^context$forecast_step * 10",
    unit_trend = "(units + 10 * context$forecast_step) * new_data$Price_Index",
    productivity = "customers * productivity * 10",
    integrated = "customers * 1.1^context$forecast_step * productivity * new_data$Price_Index * (1 + new_data$Marketing_Spend/1000 + new_data$Promo_Flag/10)")
  instructions <- switch(family,
    customer_growth = "Active_Customers is historical-only. Forecast customers internally from the last observation at 10 percent compounded per month, then multiply by 10.",
    unit_trend = "Units_Sold is historical-only. Add 10 units per month internally to the last observed units and multiply by supplied future Price_Index.",
    productivity = "Active_Customers and Units_Sold are historical-only. Fit productivity as last units divided by last customers; explicitly hold customers at the last observation and multiply by productivity and 10.",
    integrated = "Active_Customers and Units_Sold are historical-only. Fit productivity as last units divided by last customers; grow customers internally at 10 percent compounded monthly, multiply by productivity, supplied future Price_Index, and (1 + Marketing_Spend/1000 + Promo_Flag/10).")
  fit <- paste0("groups <- split(data, data$Combo); lapply(groups, function(x) { n <- nrow(x); ",
    "list(customers = ", if ("Active_Customers" %in% historical) "x$Active_Customers[[n]]" else "0",
    ", units = ", if ("Units_Sold" %in% historical) "x$Units_Sold[[n]]" else "0",
    ", productivity = ", if (family %in% c("productivity", "integrated"))
      "x$Units_Sold[[n]]/x$Active_Customers[[n]]" else "0", ") })")
  predict <- paste0("customers <- vapply(new_data$Combo, function(id) object[[id]]$customers, numeric(1)); ",
    "units <- vapply(new_data$Combo, function(id) object[[id]]$units, numeric(1)); ",
    "productivity <- vapply(new_data$Combo, function(id) object[[id]]$productivity, numeric(1)); ",
    expression)
  history <- history[c("Date", "Combo", "Target", predictors)]
  request <- request[c("Date", "Combo", future)]
  if (global) {
    peer <- history
    peer$Combo <- "B"
    other <- request
    other$Combo <- "B"
    history <- rbind(history, peer)
    request <- rbind(request, other)
    expected <- rep(expected, 2)
  }
  list(history = history, future = request, expected = expected,
    historical = historical, predictors = predictors, instructions = instructions,
    fit = fit, predict = predict, mode = if (global) "global" else "local")
}

# Build fixed caller examples on observed data; future history-only cells are
# explicitly missing in the public table, never fabricated from source output.
historical_finance_examples <- function(case) {
  example <- list(name = "ordinary", model_type = case$mode, history = case$history,
    new_data = case$future, expected = transform(case$future, .pred = case$expected)[c("Date", "Combo", ".pred")],
    tolerance = 1e-8, edge_case = FALSE, rationale = "Independent literal finance oracle.",
    outcome = "predictions", expected_error = NULL)
  for (driver in case$historical) example$new_data[[driver]] <- NA_real_
  edge <- example
  edge$name <- "edge"
  edge$edge_case <- TRUE
  list(example, edge)
}
