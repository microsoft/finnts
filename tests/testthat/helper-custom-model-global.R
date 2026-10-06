# Reproducible asymmetric monthly panels for offline global-model tests. All
# values are synthetic; the caller's RNG state is restored and no source runs.
# History and future have complete keys, including repeated dates across series.
custom_global_panel <- function(series = 3L, months = 72L, seed = 9241L, cutoff = as.Date("2023-11-01"), horizon = 3L) {
  withr::local_preserve_seed()
  set.seed(seed)
  dates <- seq(seq(cutoff, by = "-1 month", length.out = months)[months], by = "month", length.out = months + horizon)
  identity <- rep(seq_len(series), each = length(dates))
  position <- rep(seq_along(dates), series)
  data <- data.frame(Date = rep(dates, series), Combo = paste0("series-", identity), Target = 40 + 9 * identity + 0.3 *
    position + 4 * as.integer(format(rep(dates, series), "%m")))
  count <- nrow(data)
  data$Units <- 100 + 20 * identity + stats::runif(count, 0, 80)
  data$Unit_Price <- stats::runif(count, 3, 7)
  data$Discount_Rate <- stats::runif(count, 0.02, 0.22)
  data$Promo_Flag <- as.numeric(stats::runif(count) > 0.65)
  data$Marketing_Spend <- stats::runif(count, 0, 200)
  data$Holiday_Days <- as.numeric(sample(0:3, count, replace = TRUE))
  data$Stores <- as.numeric(3 + identity + sample(0:6, count, replace = TRUE))
  data$Trading_Days <- as.numeric(lubridate::days_in_month(data$Date)) - 4 - data$Holiday_Days
  data$Competitor_Price <- stats::runif(count, 3.5, 6.5)
  data$FX_Rate <- stats::runif(count, 0.8, 1.2)
  data$Region <- c("North", "South", "West")[(identity - 1L) %% 3L + 1L]
  data$Backlog <- stats::runif(count, 0, 80)
  data$Capacity_Revenue <- 250 + 50 * identity
  data$Portfolio_Total <- series * 200
  list(history = data[data$Date <= cutoff, ], future = data[data$Date > cutoff, setdiff(names(data), "Target")], context = list(
    model_type = "global",
    recipe_id = "R1", date_type = "month", forecast_horizon = horizon, target_scale = "original"
  ))
}

# Return the exact test rule names and declared input columns. This is a test
# coverage inventory, not a production recipe selector or authoring catalog.
custom_global_drivers <- function() {
  list(
    pooled_mean = character(), weighted = character(), seasonal = character(), ratio = "Units", linear = "Units", shrinkage = c(
      "Region",
      "Stores", "Trading_Days"
    ), interactions = c("Units", "Unit_Price", "Discount_Rate", "Promo_Flag", "Marketing_Spend"),
    calendar = c("Units", "Unit_Price", "Discount_Rate", "Promo_Flag", "Marketing_Spend", "Holiday_Days"), log_fiscal = c(
      "Units",
      "Unit_Price", "Discount_Rate", "Promo_Flag", "Marketing_Spend", "Competitor_Price", "FX_Rate", "Region"
    ), allocation = c(
      "Units",
      "Unit_Price", "Discount_Rate", "Promo_Flag", "Marketing_Spend", "Holiday_Days", "FX_Rate", "Backlog", "Capacity_Revenue",
      "Portfolio_Total"
    )
  )
}

# Project a reusable panel onto one rule's declared columns without changing its
# dates, values or RNG. Future targets remain absent, including in recipe tests.
custom_global_case <- function(rule, panel = custom_global_panel()) {
  columns <- c("Date", "Combo", custom_global_drivers()[[rule]])
  panel$history <- panel$history[c(columns, "Target")]
  panel$future <- panel$future[columns]
  panel
}

# Test-only candidate feature builder, embedded as source rather than called by
# the reference. stats::model.matrix creates fixed explicit contrasts; origin,
# levels and column ordering are fit state, not recomputed from future rows.
custom_global_source_features <- function(data, origin, levels, rule, phase) {
  if (rule == "linear")
    return(stats::model.matrix(~Units, data))
  if (rule == "interactions")
    return(stats::model.matrix(~ Units + Unit_Price + Discount_Rate + Promo_Flag + Marketing_Spend + Units:Unit_Price +
      Promo_Flag:Discount_Rate, data))
  month <- as.integer(format(data$Date, "%m"))
  trend <- 12L * (as.integer(format(data$Date, "%Y")) - as.integer(format(origin, "%Y"))) + month - as.integer(format(
    origin,
    "%m"
  ))
  if (rule == "log_fiscal") {
    price <- data$Unit_Price * (1 - data$Discount_Rate)
    if (any(data$Units <= 0 | price <= 0 | data$Competitor_Price <= 0 | data$FX_Rate <= 0 | data$Marketing_Spend < 0)) {
      finntsRuleError(paste0("log_domain_", phase))
    }
    region <- as.character(data$Region)
    if (anyNA(region) || any(!region %in% levels))
      finntsRuleError("unknown_region")
    fiscal <- (month - 7L) %% 12L + 1L
    frame <- data.frame(
      units = log(data$Units), price = log(price), competitor = log(data$Competitor_Price), marketing = log1p(data$Marketing_Spend),
      fx = log(data$FX_Rate), promo = data$Promo_Flag, region = factor(region, levels = levels), trend = trend, sine = sin(2 *
        pi * fiscal / 12), cosine = cos(2 * pi * fiscal / 12)
    )
    return(stats::model.matrix(~ units + price + competitor + marketing + fx + promo + region + trend + sine + cosine +
      promo:price, frame, contrasts.arg = list(region = stats::contr.treatment(levels))))
  }
  month_start <- as.Date(format(data$Date, "%Y-%m-01"))
  next_month <- as.Date(format(month_start + 32, "%Y-%m-01"))
  days <- as.numeric(next_month - month_start)
  frame <- data.frame(
    units = data$Units / days, price = data$Unit_Price, discount = data$Discount_Rate, promo = data$Promo_Flag,
    marketing = data$Marketing_Spend / days, holiday = data$Holiday_Days, month = factor(month, levels = 1:12), trend = trend
  )
  design <- stats::model.matrix(~ units + price + discount + promo + marketing + holiday + month + trend, frame, contrasts.arg = list(month = stats::contr.treatment(12)))
  if (rule == "allocation")
    design <- cbind(design, fx = data$FX_Rate, backlog = data$Backlog / days)
  design
}

# Test-only iterative water filling for the candidate. The separate oracle uses
# a scalar root solver. Invalid budgets, zero positive weights and infeasibility
# raise exact rule codes; zero-budget and exact-capacity boundaries are explicit.
custom_global_source_allocate <- function(scores, capacity, totals) {
  if (length(unique(totals)) != 1L || any(totals < 0) || any(capacity < 0))
    finntsRuleError("invalid_budget")
  budget <- totals[[1L]]
  if (budget == 0)
    return(rep(0, length(scores)))
  active <- scores > 0
  if (!any(active))
    finntsRuleError("zero_allocation_weight")
  if (sum(capacity[active]) < budget)
    finntsRuleError("infeasible_capacity")
  result <- numeric(length(scores))
  repeat {
    remaining <- budget - sum(result)
    shares <- remaining * scores[active] / sum(scores[active])
    bound <- shares >= capacity[active]
    indices <- which(active)
    if (!any(bound)) {
      result[indices] <- shares
      break
    }
    result[indices[bound]] <- capacity[indices[bound]]
    active[indices[bound]] <- FALSE
    if (!any(active))
      break
  }
  result
}

# Build test-only candidate source, never a production model catalog or substitute
# for LLM output. Expected answers are calculated separately below. The real
# assembler freezes portable helper source and the real engine executes it.
custom_global_definition <- function(rule = "pooled_mean") {
  window <- switch(rule,
    weighted = 3L,
    ratio = 12L,
    linear = 24L,
    shrinkage = 12L,
    interactions = 36L,
    calendar = 48L,
    log_fiscal = 60L,
    allocation = 48L,
    0L
  )
  helpers <- list()
  bodies <- switch(rule,
    pooled_mean = list(fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))"),
    weighted = list(fit_body = "sum(tapply(data$Target, as.character(data$Date), mean) * c(0.2, 0.3, 0.5))", predict_body = "rep(object, nrow(new_data))"),
    ratio = list(
      fit_body = "if (sum(data$Units) == 0) finntsRuleError('zero_denominator'); sum(data$Target) / sum(data$Units)",
      predict_body = "object * new_data$Units"
    ),
    shrinkage = list(fit_body = paste("exposure <- data$Stores * data$Trading_Days",
      "if (sum(exposure) == 0) finntsRuleError('zero_denominator')", "rate <- sum(data$Target) / sum(exposure)", "(tapply(data$Target, as.character(data$Region), sum) + 300 * rate) / (tapply(exposure, as.character(data$Region), sum) + 300)",
      sep = "\n"
    ), predict_body = "as.numeric(object[as.character(new_data$Region)]) * new_data$Stores * new_data$Trading_Days"),
    seasonal = list(fit_body = "data", predict_body = paste("vapply(seq_len(nrow(new_data)), function(index) {", "dates <- finntsShiftDate(rep(new_data$Date[index], 3L), c(-12L, -24L, -36L), 'month')",
      "mean(object$Target[object$Date %in% dates]) }, numeric(1))",
      sep = "\n"
    )),
    NULL
  )
  if (rule %in% c("linear", "interactions", "calendar", "log_fiscal", "allocation")) {
    helpers <- list(list(name = "globalFeatures", code = paste(deparse(custom_global_source_features), collapse = "\n")))
    bodies <- list(fit_body = paste("origin <- min(data$Date); levels <- sort(unique(as.character(data$Region)))", "design <- globalFeatures(data, origin, levels, parameters$rule, 'fit')",
      "response <- data$Target", "if (parameters$rule == 'log_fiscal') { if (any(response <= 0)) finntsRuleError('log_domain_fit'); response <- log(response) }",
      "if (parameters$rule %in% c('calendar', 'allocation')) response <- response / as.numeric(as.Date(format(as.Date(format(data$Date, '%Y-%m-01')) + 32, '%Y-%m-01')) - as.Date(format(data$Date, '%Y-%m-01')))",
      "fitted <- stats::lm.fit(design, response)", "if (fitted$rank != ncol(design)) finntsRuleError('rank_deficient_design')",
      "list(coefficients = fitted$coefficients, columns = colnames(design), origin = origin, levels = levels, rule = parameters$rule, cohort = sort(unique(data$Combo)))",
      sep = "\n"
    ), predict_body = paste("design <- globalFeatures(new_data, object$origin, object$levels, object$rule, 'predict')",
      "stopifnot(identical(colnames(design), object$columns))", "values <- as.numeric(design %*% object$coefficients)",
      "if (object$rule == 'log_fiscal') values <- exp(values)", "if (object$rule %in% c('calendar', 'allocation')) values <- values * as.numeric(as.Date(format(as.Date(format(new_data$Date, '%Y-%m-01')) + 32, '%Y-%m-01')) - as.Date(format(new_data$Date, '%Y-%m-01')))",
      "values",
      sep = "\n"
    ))
    if (rule == "allocation") {
      helpers[[2L]] <- list(name = "globalAllocate", code = paste(deparse(custom_global_source_allocate), collapse = "\n"))
      bodies$predict_body <- paste(bodies$predict_body, "if (!setequal(new_data$Combo, object$cohort)) stop('incomplete_cohort')",
        "values <- pmax(0, values)", "for (date in unique(as.character(new_data$Date))) { rows <- which(as.character(new_data$Date) == date); values[rows] <- globalAllocate(values[rows], new_data$Capacity_Revenue[rows], new_data$Portfolio_Total[rows]) }",
        "values",
        sep = "\n"
      )
    }
  }
  if (is.null(bodies))
    stop("Unknown global test rule.")
  if (window > 0L)
    bodies$fit_body <- paste("data <- data[data$Date > finntsShiftDate(context$cutoff, -parameters$window, 'month'), , drop = FALSE]",
      bodies$fit_body,
      sep = "\n"
    )
  assembled <- custom_author_assemble(c(bodies, list(helpers = helpers)))
  source <- stats::setNames(vapply(assembled$source, function(entry) entry$code, character(1)), vapply(
    assembled$source,
    function(entry) entry$name, character(1)
  ))
  new_custom_model_definition(paste0("test_", rule), "Apply the fixed independent global test rule.", "Share historical information across every included series.",
    "global", source, list(
      predictors = c("Date", "Combo", custom_global_drivers()[[rule]]), recipes = "R1", target_scale = "original",
      date_types = "month", forecast_horizon = 1:12, missing_data = "error", runtime = list(
        version = 2L, prediction_scope = "complete_horizon",
        cohort = if (rule == "allocation") "all" else "series", group_columns = character()
      )
    ),
    fixed_parameters = list(
      rule = rule,
      window = window
    ), packages = "stats"
  )
}

# Independent numerical references use ordinary grouped arithmetic and calendar
# labels, not candidate helpers, source, fitted coefficients or model predictions.
# Results are aligned to the caller's future rows; no inputs or oracles mutate.
custom_global_reference <- function(rule, history, future) {
  if (rule == "pooled_mean")
    return(rep(sum(history$Target) / nrow(history), nrow(future)))
  if (rule == "seasonal") {
    averages <- aggregate(history$Target, list(year = as.integer(format(history$Date, "%Y")), month = as.integer(format(
      history$Date,
      "%m"
    ))), mean)
    return(vapply(seq_len(nrow(future)), function(index) {
      years <- as.integer(format(future$Date[index], "%Y")) - 1:3
      month <- as.integer(format(future$Date[index], "%m"))
      sum(averages$x[averages$year %in% years & averages$month == month]) / 3
    }, numeric(1)))
  }
  windows <- c(
    weighted = 3L, ratio = 12L, linear = 24L, shrinkage = 12L, interactions = 36L, calendar = 48L, log_fiscal = 60L,
    allocation = 48L
  )
  month_index <- as.integer(format(history$Date, "%Y")) * 12L + as.integer(format(history$Date, "%m"))
  retained <- history[month_index > max(month_index) - windows[[rule]], ]
  if (rule == "weighted") {
    means <- aggregate(retained$Target, list(date = retained$Date), mean)$x
    return(rep((2 * means[1L] + 3 * means[2L] + 5 * means[3L]) / 10, nrow(future)))
  }
  if (rule == "ratio")
    return(sum(retained$Target) / sum(retained$Units) * future$Units)
  if (rule == "shrinkage") {
    total_exposure <- sum(retained$Stores * retained$Trading_Days)
    global_rate <- sum(retained$Target) / total_exposure
    return(vapply(seq_len(nrow(future)), function(index) {
      rows <- as.character(retained$Region) == as.character(future$Region[index])
      rate <- (sum(retained$Target[rows]) + 300 * global_rate) / (sum(retained$Stores[rows] * retained$Trading_Days[rows]) +
        300)
      rate * future$Stores[index] * future$Trading_Days[index]
    }, numeric(1)))
  }
  regions <- sort(unique(as.character(retained$Region)))
  origin <- min(retained$Date)
  design <- custom_global_reference_design(rule, retained, origin, regions)
  response <- retained$Target
  if (rule == "log_fiscal")
    response <- log(response)
  if (rule %in% c("calendar", "allocation"))
    response <- response / as.numeric(lubridate::days_in_month(retained$Date))
  coefficients <- qr.solve(design, response)
  values <- as.numeric(custom_global_reference_design(rule, future, origin, regions) %*% coefficients)
  if (rule == "log_fiscal")
    values <- exp(values)
  if (rule %in% c("calendar", "allocation"))
    values <- values * as.numeric(lubridate::days_in_month(future$Date))
  if (rule == "allocation") {
    for (date in unique(as.character(future$Date))) {
      rows <- which(as.character(future$Date) == date)
      values[rows] <- custom_global_reference_allocate(pmax(0, values[rows]), future$Capacity_Revenue[rows], future$Portfolio_Total[rows])
    }
  }
  values
}

# Independent explicit regression matrices: no model.matrix, candidate feature
# helper or candidate state. January/reference region is omitted, July starts the
# fiscal year, and the origin is fixed from retained training dates.
custom_global_reference_design <- function(rule, data, origin, regions) {
  if (rule == "linear")
    return(cbind(1, data$Units))
  if (rule == "interactions")
    return(cbind(1, as.matrix(data[custom_global_drivers()$interactions]), data$Units * data$Unit_Price, data$Promo_Flag *
      data$Discount_Rate))
  month <- as.integer(format(data$Date, "%m"))
  trend <- as.integer(format(data$Date, "%Y")) * 12L + month - (as.integer(format(origin, "%Y")) * 12L + as.integer(format(
    origin,
    "%m"
  )))
  if (rule == "log_fiscal") {
    fiscal <- (month + 5L) %% 12L + 1L
    return(cbind(
      1, log(data$Units), log(data$Unit_Price * (1 - data$Discount_Rate)), log(data$Competitor_Price), log1p(data$Marketing_Spend),
      log(data$FX_Rate), data$Promo_Flag, vapply(regions[-1L], function(region) as.numeric(as.character(data$Region) ==
        region), numeric(nrow(data))), trend, sin(pi * fiscal / 6), cos(pi * fiscal / 6), data$Promo_Flag * log(data$Unit_Price *
        (1 - data$Discount_Rate))
    ))
  }
  days <- as.numeric(lubridate::days_in_month(data$Date))
  design <- cbind(
    1, data$Units / days, data$Unit_Price, data$Discount_Rate, data$Promo_Flag, data$Marketing_Spend / days,
    data$Holiday_Days, vapply(2:12, function(level) as.numeric(month == level), numeric(nrow(data))), trend
  )
  if (rule == "allocation")
    design <- cbind(design, data$FX_Rate, data$Backlog / days)
  design
}

# Independently solve capped proportional allocation using a monotone root, not
# the candidate's iterative redistribution. Throws on invalid/unsatisfiable input;
# neither lowers budgets nor invents fallback weights.
custom_global_reference_allocate <- function(scores, capacity, total) {
  if (length(unique(total)) != 1L || any(total < 0) || any(capacity < 0))
    stop("invalid_budget")
  budget <- total[[1L]]
  if (budget == 0)
    return(rep(0, length(scores)))
  active <- scores > 0
  if (!any(active))
    stop("zero_allocation_weight")
  if (sum(capacity[active]) < budget)
    stop("infeasible_capacity")
  if (sum(capacity[active]) == budget)
    return(ifelse(active, capacity, 0))
  multiplier <- stats::uniroot(function(value) sum(pmin(capacity, value * scores)) - budget,
    interval = c(0, max(capacity[active] / scores[active])),
    tol = 1e-12
  )$root
  pmin(capacity, multiplier * scores)
}

# Fit a trusted test-only definition through the real engine with explicit
# consent. Only history supplies outcomes; future targets cannot enter the fit.
custom_global_fit <- function(definition, panel) {
  custom_model_fit_impl(panel$history[setdiff(names(panel$history), "Target")], panel$history$Target, definition, panel$context,
    allow_code = TRUE
  )
}
