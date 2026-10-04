# Create a standalone inert definition for runtime tests. Source is stored as
# text and any edited source must receive a new content identity before use.
runtime_definition <- function(model_type = "local") {
  new_custom_model_definition(
    name = "runtime_rule",
    instructions = "Use the prescribed historical mean.",
    interpretation = "Fit a mean using analysis rows only.",
    model_type = model_type,
    source = c(
      fit = "function(data, context, parameters) mean(data$Target)",
      predict = paste0("function(object, new_data, context) data.frame(",
        ".finnts_row = new_data$.finnts_row, .pred = rep(object, nrow(new_data)))")
    ),
    requirements = list(predictors = c("Date", "Combo"), recipes = "R1",
      target_scale = "original", date_types = "month", forecast_horizon = 1:12,
      missing_data = "Reject missing required observations."),
    packages = "stats"
  )
}

# Return a small monthly series with numeric outcomes and stable identity keys.
runtime_history <- function() {
  data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "first", Target = seq_len(24))
}

# Build a trusted fixture workflow with explicit representation metadata. This
# helper belongs to this test file so installed-worker tests are standalone.
runtime_workflow <- function(definition = runtime_definition(), history = runtime_history(),
                             recipe = recipes::recipe(Target ~ ., data = history),
                             model_type = "local", recipe_id = "R1", allow_code = TRUE) {
  custom_model_workflow(definition, recipe, model_type = model_type,
    recipe_id = recipe_id, date_type = "month", forecast_horizon = 2,
    target_scale = "original", allow_code = allow_code)
}

# Materialize only the package-owned portable calendar helpers for focused tests.
# The same source entries are frozen into authored model definitions.
runtime_date_functions <- function() {
  functions <- new.env(parent = baseenv())
  for (entry in custom_author_date_helpers("calendar_v1")) {
    assign(entry$name, eval(parse(text = entry$code), envir = functions), envir = functions)
  }
  functions
}

test_that("date helpers preserve classes and make calendar rollover explicit", {
  helpers <- runtime_date_functions()
  shift <- helpers$finntsShiftDate
  expect_identical(shift(as.Date("2024-02-28"), 1, "day"), as.Date("2024-02-29"))
  expect_identical(shift(as.Date("2024-02-28"), 1, "week"), as.Date("2024-03-06"))
  expect_identical(shift(as.Date(c("2024-03-01", "2023-12-01")), c(-3, 2), "month"), as.Date(c("2023-12-01", "2024-02-01")))
  expect_identical(shift(as.Date("2024-01-01"), -1, "quarter"), as.Date("2023-10-01"))
  expect_identical(shift(as.Date("2024-02-29"), -4, "year"), as.Date("2020-02-29"))
  for (cadence in c("month", "quarter", "year")) {
    date <- as.Date(if (cadence == "year") "2024-02-29" else "2024-01-31")
    expect_error(shift(date, 1, cadence), class = "finnts_custom_date_error")
    expected <- switch(cadence, month = "2024-02-29", quarter = "2024-04-30", year = "2025-02-28")
    expect_identical(shift(date, 1, cadence, "last_day"), as.Date(expected))
  }
  expect_identical(shift(as.Date(character()), 1, "month"), as.Date(character()))
  expect_error(shift(as.numeric(as.Date("2024-01-01")), 1, "month"), class = "finnts_custom_date_error")
  expect_error(shift(as.Date("2024-01-01"), 0.5, "month"), class = "finnts_custom_date_error")
  expect_error(shift(as.Date("2024-01-01"), 1:2, "month"), class = "finnts_custom_date_error")
  expect_error(shift(as.Date("2024-01-01"), 1, "fiscal"), class = "finnts_custom_date_error")
})

test_that("date matching preserves request order and isolates series", {
  lookup <- runtime_date_functions()$finntsMatchDate
  dates <- as.Date(c("2024-01-01", "2024-02-01"))
  expect_identical(lookup(dates[2:1], dates), 2:1)
  expect_identical(lookup(as.Date("2025-01-01"), dates), NA_integer_)
  expect_identical(lookup(dates[c(2, 1)], rep(dates, 2), c("second", "first"), rep(c("first", "second"), each = 2)), c(4L, 1L))
  expect_error(lookup(as.numeric(dates), dates), class = "finnts_custom_date_error")
  expect_error(lookup(dates, as.character(dates)), class = "finnts_custom_date_error")
  expect_error(lookup(dates, rep(dates, 2)), class = "finnts_custom_date_error")
  expect_error(lookup(dates, dates, "first", rep("first", 2)), class = "finnts_custom_date_error")
  expect_identical(lookup(as.Date(character()), dates), integer())
})

test_that("date helper source is frozen and cannot replace saved legacy wrappers", {
  proposal <- list(fit_body = "data", predict_body = "rep(0, nrow(new_data))", helpers = list())
  legacy <- custom_author_assemble(proposal, rule_errors = TRUE)
  current <- custom_author_assemble(proposal, rule_errors = TRUE, date_helpers = "calendar_v1")
  expect_length(legacy$source, 4L)
  expect_length(current$source, 7L)
  expect_identical(custom_author_assemble(legacy, assembled = TRUE, rule_errors = TRUE), legacy)
  expect_identical(custom_author_assemble(current, assembled = TRUE, rule_errors = TRUE, date_helpers = "calendar_v1"), current)
  corrupt <- current
  corrupt$source[[6]]$code <- "function(...) NULL"
  expect_error(custom_author_assemble(corrupt, assembled = TRUE, rule_errors = TRUE, date_helpers = "calendar_v1"), "wrapper changed")
  proposal$helpers <- list(list(name = "finntsShiftDate", code = "function(...) NULL"))
  expect_error(custom_author_assemble(proposal, date_helpers = "calendar_v1"), "reserved")
  expect_error(custom_author_date_helpers("unknown"), "Unknown date helper")
})

# Assemble a seasonal rule using only the shared date helpers. Literal test
# expectations below remain independent of this source's lookup calculation.
runtime_date_definition <- function() {
  contract <- runtime_definition(c("local", "global"))
  contract$authoring_protocol <- 6L
  contract$date_helpers <- "calendar_v1"
  contract$package_policy <- list(mode = "base_only", available = "base")
  contract$requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  custom_author_definition(contract, list(status = "candidate", conflict = "", fit_body = "data",
    predict_body = paste0("indices <- lapply(c(-12, -24, -36), function(offset) ",
      "finntsMatchDate(finntsShiftDate(new_data$Date, offset, 'month'), object$Date, new_data$Combo, object$Combo)); ",
      "Reduce('+', lapply(indices, function(rows) object$Target[rows])) / 3"), helpers = list()))
}

test_that("shared date helpers support seasonal forecasts across series and later cutoffs", {
  definition <- runtime_date_definition()
  before <- serialize(definition, NULL)
  for (origin in c("2020-01-01", "2021-01-01")) for (mode in c("local", "global")) {
    dates <- seq(as.Date(origin), by = "month", length.out = 38)
    history <- data.frame(Date = dates[1:36], Combo = "first", Target = seq_len(36))
    request <- data.frame(Date = dates[37:38], Combo = "first")
    expected <- c(13, 14)
    if (mode == "global") {
      history <- rbind(history, transform(history, Combo = "second", Target = -2 * Target))
      request <- rbind(request, transform(request, Combo = "second"))
      expected <- c(expected, -26, -28)
    }
    history <- history[rev(seq_len(nrow(history))), ]
    request <- request[rev(seq_len(nrow(request))), ]
    fitted <- generics::fit(runtime_workflow(definition, history, model_type = mode), history)
    expect_equal(predict(fitted, request)$.pred, rev(expected))
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(fitted, path)
    expect_identical(predict(readRDS(path), request), predict(fitted, request))
    expect_identical(serialize(definition, NULL), before)
  }
})

test_that("installed calendar helpers survive fresh processes and PSOCK workers", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")), "Calendar workers require the installed namespace")
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 38)
  history <- data.frame(Date = dates[1:36], Combo = "first", Target = seq_len(36))
  request <- data.frame(Date = dates[c(38, 37)], Combo = "first")
  workflow <- runtime_workflow(runtime_date_definition(), history)
  fitted <- generics::fit(workflow, history)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  # Each worker resolves the pinned package and uses the saved canonical source.
  worker <- function(path, workflow, history, request, expected_path) {
    stopifnot(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")) == normalizePath(expected_path))
    list(restored = stats::predict(readRDS(path), request)$.pred,
      refitted = stats::predict(generics::fit(workflow, history), request)$.pred)
  }
  environment(worker) <- baseenv()
  args <- list(path = path, workflow = workflow, history = history, request = request, expected_path = namespace_path)
  separate <- callr::r(worker, args = args, libpath = .libPaths())
  expect_equal(separate$restored, c(14, 13))
  expect_identical(separate$restored, separate$refitted)
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  cluster <- parallel::makePSOCKcluster(1L)
  withr::defer(parallel::stopCluster(cluster))
  expect_identical(do.call(parallel::clusterCall, c(list(cl = cluster, fun = worker), args))[[1]], separate)
})

test_that("workflow construction is inert and leaves built-in models unchanged", {
  definition <- runtime_definition()
  definition$source[] <- "function(...) stop('source executed during construction')"
  definition$version_id <- custom_model_definition_digest(definition)
  history <- runtime_history()
  catalog <- list_models()
  workflow <- custom_model_workflow(definition,
    recipes::recipe(Target ~ Date + Combo, data = history),
    model_type = "local", recipe_id = "R1", date_type = "month",
    forecast_horizon = 2, target_scale = "original")

  expect_s3_class(workflow, "workflow")
  expect_identical(list_models(), catalog)
  expect_false("runtime_rule" %in% list_models())
})

# Recompute identity after a deliberate source or contract edit in a test.
runtime_rehash <- function(definition) {
  definition$version_id <- custom_model_definition_digest(definition)
  definition
}

# Make request rows strictly after the training cutoff, preserving supplied order.
runtime_future <- function() {
  data.frame(Date = as.Date(c("2022-02-01", "2022-01-01")), Combo = "first")
}

test_that("trusted fitting and prediction use the analysis data only", {
  history <- runtime_history()
  fitted <- generics::fit(runtime_workflow(), data = history)
  expect_equal(predict(fitted, runtime_future())$.pred, rep(12.5, 2))
  leaked <- runtime_future()
  leaked$Target <- c(1e9, -1e9)
  expect_equal(predict(fitted, leaked)$.pred, rep(12.5, 2))
  engine <- workflows::extract_fit_parsnip(fitted)$fit
  expect_equal(engine$context$cutoff, max(history$Date))
  expect_false(any(c("run_info", "llm", "storage_object") %in% names(engine$context)))
})

test_that("versioned temporal runtime owns ordering and horizon indices", {
  legacy <- runtime_definition(c("local", "global"))
  requirements <- legacy$requirements
  requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  definition <- new_custom_model_definition("temporal_rule", "Extend each series using its dated forecast steps.",
    "Use the last actual plus the horizon index.", c("local", "global"),
    c(fit = paste0("function(data, context, parameters) { stopifnot(identical(order(data$Combo, data$Date), seq_len(nrow(data)))); ",
      "list(last = lapply(split(data$Target, data$Combo), function(values) values[length(values)])) }"),
      predict = paste0("function(object, new_data, context) { stopifnot(identical(order(new_data$Combo, new_data$Date), seq_len(nrow(new_data)))); ",
        "data.frame(.finnts_row = new_data$.finnts_row, .pred = vapply(as.character(new_data$Combo), ",
        "function(combo) object$last[[combo]], numeric(1)) + context$forecast_step) }")),
    requirements)
  history <- runtime_history()
  second <- transform(history, Combo = "second", Target = Target * 10)
  second$Date <- seq(as.Date("2020-02-01"), by = "month", length.out = nrow(second))
  training <- rbind(history, second)
  training <- training[rev(seq_len(nrow(training))), ]
  fitted <- generics::fit(runtime_workflow(definition, training, model_type = "global"), training)
  future <- data.frame(Date = as.Date(c("2022-03-01", "2022-01-01", "2022-02-01", "2022-02-01")),
    Combo = c("second", "first", "second", "first"))
  expect_identical(predict(fitted, future)$.pred, c(242, 25, 241, 26))
  expect_equal(predict(fitted, future[future$Combo == "first", ])$.pred, c(25, 26))
  expect_error(predict(fitted, future[1, , drop = FALSE]), "complete forecast horizon")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  expect_identical(predict(readRDS(path), future), predict(fitted, future))
  expect_identical(legacy$schema_version, 1L)
  expect_null(legacy$requirements$runtime)
})

test_that("temporal runtime handles cadence boundaries and later refits", {
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    requirements <- runtime_definition()$requirements
    requirements$date_types <- cadence
    requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
    definition <- new_custom_model_definition("cadence_steps", "Use dated steps from the current fit.", "Return the future step index.", "local",
      c(fit = "function(data, context, parameters) list(cutoff = context$cutoff)",
        predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=as.numeric(context$forecast_step))"), requirements)
    before <- serialize(definition, NULL)
    origins <- if (cadence == "day") c("2019-12-25", "2024-02-24") else c("2019-08-01", "2024-02-01")
    for (origin in origins) {
      dates <- seq(as.Date(origin), by = cadence, length.out = 8L)
      history <- data.frame(Date = dates[1:5], Combo = "series", Target = seq_len(5))
      context <- list(model_type = "local", recipe_id = "R1", date_type = cadence, forecast_horizon = 3, target_scale = "original")
      fit <- custom_model_fit_impl(history[c("Date", "Combo")], history$Target, definition, context, TRUE)
      request <- data.frame(Date = dates[c(8, 6, 7)], Combo = "series")
      expect_identical(custom_model_predict_impl(fit, request), c(3, 1, 2))
      expect_error(custom_model_predict_impl(fit, request[1:2, ]), "complete forecast horizon")
      expect_identical(serialize(definition, NULL), before)
    }
  }
})

test_that("temporal execution supports recursive date-dependent driver rules", {
  requirements <- runtime_definition()$requirements
  requirements$predictors <- c("Date", "Combo", "Adjustment")
  requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  definition <- new_custom_model_definition("recursive_calendar", "Double January balances before adding the adjustment; otherwise subtract it with a zero floor.",
    "Carry the previous prediction into the next month.", "local",
    c(fit = "function(data, context, parameters) data$Target[nrow(data)]",
      predict = paste0("function(object, new_data, context) { values <- numeric(nrow(new_data)); previous <- object; ",
        "for (index in seq_len(nrow(new_data))) { previous <- if (format(new_data$Date[index], '%m') == '01') ",
        "2 * previous + new_data$Adjustment[index] else max(0, previous - new_data$Adjustment[index]); values[index] <- previous }; ",
        "data.frame(.finnts_row=new_data$.finnts_row, .pred=values) }")), requirements)
  history <- data.frame(Date = as.Date(c("2023-11-01", "2023-10-01")), Combo = "series", Adjustment = 0, Target = c(2, 90))
  context <- list(model_type = "local", recipe_id = "R1", date_type = "month", forecast_horizon = 3, target_scale = "original")
  fit <- custom_model_fit_impl(history[c("Date", "Combo", "Adjustment")], history$Target, definition, context, TRUE)
  request <- data.frame(Date = as.Date(c("2024-02-01", "2023-12-01", "2024-01-01")), Combo = "series", Adjustment = c(20, 1, 3))
  expect_equal(custom_model_predict_impl(fit, request), c(0, 1, 5))
  expect_equal(custom_model_predict_impl(fit, request[3:1, ]), c(5, 1, 0))
  expect_error(custom_model_predict_impl(fit, request[c(1, 3), ]), "complete forecast horizon")
  expect_error(custom_model_predict_impl(fit, transform(request, Combo = "unseen")), "without training history")
  expect_error(custom_model_predict_impl(fit, request[c(1, 1, 3), ]), "duplicate row keys")
})

test_that("execution requires explicit consent and an unchanged definition", {
  history <- runtime_history()
  expect_error(generics::fit(runtime_workflow(allow_code = FALSE), history), "allow_code")
  expect_error(runtime_workflow(allow_code = "yes"), "allow_code")
  definition <- runtime_definition()
  definition$source[["fit"]] <- "function(...) stop('must not run')"
  expect_error(runtime_workflow(definition), "version_id")
  definition$version_id <- NULL
  expect_error(runtime_workflow(definition), "version_id")
  fitted <- generics::fit(runtime_workflow(), history)
  engine <- workflows::extract_fit_parsnip(fitted)$fit
  engine$definition$fixed_parameters <- list(changed = TRUE)
  expect_error(custom_model_predict_impl(engine, runtime_future()), "version_id")
  engine <- workflows::extract_fit_parsnip(fitted)$fit
  engine$allow_code <- FALSE
  expect_error(custom_model_predict_impl(engine, runtime_future()), "allow_code")
})

test_that("representation metadata and protected recipe fields are enforced", {
  definition <- runtime_definition()
  history <- runtime_history()
  recipe <- recipes::recipe(Target ~ ., data = history)
  args <- list(definition = definition, recipe = recipe, model_type = "local",
    recipe_id = "R1", date_type = "month", forecast_horizon = 2,
    target_scale = "original", allow_code = TRUE)
  for (change in list(list(model_type = "global"), list(recipe_id = "R2"),
    list(date_type = "day"), list(forecast_horizon = 13), list(target_scale = "prepared"))) {
    invalid <- utils::modifyList(args, change)
    expect_error(do.call(custom_model_workflow, invalid), names(change))
  }
  expect_error(runtime_workflow(recipe = recipes::prep(recipe)), "unprepped")
  expect_error(runtime_workflow(recipe = recipes::step_log(recipe, Target)), "protected")
  expect_error(runtime_workflow(recipe = recipes::step_rm(recipe, Combo)), "protected|required")
  expect_error(runtime_workflow(recipe = recipes::step_dummy(recipe, Combo)), "protected")
  expect_error(runtime_workflow(recipe = recipes::step_mutate(recipe, Target = Target * 2)), "step")
  missing <- history
  missing$Combo <- NULL
  expect_error(runtime_workflow(history = missing), "Combo")
  second <- history
  second$Combo <- "second"
  expect_error(generics::fit(runtime_workflow(), rbind(history, second)), "local")
  expect_error(predict(generics::fit(runtime_workflow(), history), history), "horizon|cutoff")
})

test_that("runtime errors and unavailable dependencies fail before fallback", {
  history <- runtime_history()
  definition <- runtime_definition()
  definition$packages <- c("stats", "nixtlar")
  definition$source[["fit"]] <- "function(...) stop('should not execute')"
  definition <- runtime_rehash(definition)
  original <- custom_model_package_available
  local_mocked_bindings(custom_model_package_available = function(package) {
    if (identical(package, "nixtlar")) FALSE else original(package)
  }, .package = "finnts")
  expect_error(generics::fit(runtime_workflow(definition), history), "nixtlar")
  broken <- runtime_definition()
  broken$source[["fit"]] <- "function(...) stop('deliberate model error')"
  expect_error(generics::fit(runtime_workflow(runtime_rehash(broken)), history), "deliberate model error")
})

test_that("prediction identities are realigned and malformed results rejected", {
  definition <- runtime_definition()
  definition$source[["predict"]] <- paste0("function(object, new_data, context) { ",
    "stopifnot(!'Target' %in% names(new_data)); data.frame(",
    ".finnts_row = rev(new_data$.finnts_row), .pred = rev(new_data$.finnts_row * 10)) }")
  fitted <- generics::fit(runtime_workflow(runtime_rehash(definition)), runtime_history())
  expect_equal(predict(fitted, runtime_future())$.pred, c(10, 20))
  for (result in c("data.frame(.finnts_row = c(1, 1), .pred = c(1, 2))",
    "data.frame(.finnts_row = c(1, 3), .pred = c(1, 2))",
    "data.frame(.finnts_row = 1, .pred = 1)",
    "data.frame(.finnts_row = 1:3, .pred = 1:3)",
    "data.frame(.finnts_row = 1:2, .pred = c(NA_real_, 2))",
    "data.frame(.finnts_row = 1:2, .pred = c(Inf, 2))",
    "data.frame(.finnts_row = 1:2, .pred = c('a', 'b'))", "c(1, 2)")) {
    definition$source[["predict"]] <- paste("function(object, new_data, context)", result)
    fitted <- generics::fit(runtime_workflow(runtime_rehash(definition)), runtime_history())
    expect_error(predict(fitted, runtime_future()), "prediction|pred|row")
  }
})

# Return a reviewed seasonal-growth implementation. Calendar matching, not row
# position, defines annual comparisons and prior-season predictions.
runtime_seasonal_definition <- function() {
  definition <- runtime_definition(c("local", "global"))
  definition$fixed_parameters <- list(window = 3L)
  definition$source <- c(
    previous_year = paste0("function(dates) as.Date(paste0(as.integer(format(dates, '%Y')) - 1L, ",
      "format(dates, '-%m-%d')))") ,
    fit = paste0("function(data, context, parameters) { rates <- lapply(split(data, data$Combo), function(series) { ",
      "series <- series[order(series$Date), ]; recent <- utils::tail(series, parameters$window); ",
      "baseline <- series$Target[match(previous_year(recent$Date), series$Date)]; ",
      "if (anyNA(baseline) || any(baseline == 0)) stop('required seasonal history unavailable'); ",
      "recent$Target / baseline - 1 }); ",
      "list(history = data, growth = stats::median(unlist(rates))) }") ,
    predict = paste0("function(object, new_data, context) { history <- object$history; ",
      "keys <- paste(history$Combo, history$Date); baseline <- history$Target[match(",
      "paste(new_data$Combo, previous_year(new_data$Date)), keys)]; ",
      "data.frame(.finnts_row = new_data$.finnts_row, .pred = baseline * (1 + object$growth)) }")
  )
  definition$packages <- c("stats", "utils")
  runtime_rehash(definition)
}

# Histories encode exactly .1/.2/.3 and .4/.5/.6 annual growth respectively.
runtime_seasonal_history <- function() {
  first <- data.frame(Date = seq(as.Date("2024-01-01"), by = "month", length.out = 15),
    Combo = "first", Target = c(rep(100, 12), 110, 120, 130))
  second <- first
  second$Combo <- "second"
  second$Target <- c(rep(200, 12), 280, 300, 320)
  rbind(first, second)
}

test_that("local and pooled global seasonal arithmetic matches independent expectations", {
  history <- runtime_seasonal_history()
  definition <- runtime_seasonal_definition()
  request <- data.frame(Date = as.Date(c("2025-04-01", "2025-04-01")), Combo = c("second", "first"))
  local <- generics::fit(runtime_workflow(definition, history, model_type = "local"), history[history$Combo == "first", ])
  expect_equal(predict(local, request[2, ])$.pred, 120)
  global <- generics::fit(runtime_workflow(definition, history, model_type = "global"), history)
  expect_equal(predict(global, request)$.pred, c(270, 135))
  expect_equal(workflows::extract_fit_parsnip(global)$fit$context$cutoff, as.Date("2025-03-01"))
})

test_that("chronological resampling never passes assessment Target into prediction", {
  history <- runtime_history()
  definition <- runtime_definition()
  definition$source[["fit"]] <- paste0("function(data, context, parameters) { ",
    "stopifnot(context$cutoff == max(data$Date)); mean(data$Target) }")
  definition$source[["predict"]] <- paste0("function(object, new_data, context) { ",
    "stopifnot(!'Target' %in% names(new_data)); data.frame(",
    ".finnts_row = new_data$.finnts_row, .pred = rep(object, nrow(new_data))) }")
  definition <- runtime_rehash(definition)
  predictions <- list()
  for (assessment in list(c(23, 24), c(1e9, -1e9))) {
    data <- history
    data$Target[23:24] <- assessment
    split <- rsample::make_splits(list(analysis = 1:22, assessment = 23:24), data)
    folds <- rsample::manual_rset(list(split), "Fold1")
    result <- tune::fit_resamples(runtime_workflow(definition, data), resamples = folds,
      control = tune::control_resamples(save_pred = TRUE, allow_par = FALSE))
    predictions[[length(predictions) + 1L]] <- tune::collect_predictions(result)$.pred
    expect_equal(predictions[[length(predictions)]], rep(11.5, 2))
  }
  expect_identical(predictions[[1]], predictions[[2]])
})

test_that("temporal resampling executes complete horizons but preserves scoring rows", {
  history <- runtime_history()
  history$Driver <- seq_len(nrow(history))
  history$Target[23:24] <- c(1e9, -1e9)
  definition <- runtime_definition()
  definition$schema_version <- 3L
  definition$requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  definition$requirements$predictors <- c("Date", "Combo", "Driver")
  definition$source[["predict"]] <- paste0("function(object, new_data, context) { stopifnot(!'Target' %in% names(new_data)); ",
    "data.frame(.finnts_row=new_data$.finnts_row, .pred=rep(object + sum(new_data$Driver), nrow(new_data))) }")
  definition <- runtime_rehash(definition)
  fold <- rsample::make_splits(list(analysis = 1:22, assessment = 23L), history)
  splits <- rsample::manual_rset(list(fold), "short")
  before <- serialize(splits, NULL)
  workflow <- runtime_workflow(definition, history)
  control <- tune::control_resamples(save_pred = TRUE, allow_par = FALSE)
  result <- custom_run_fit_resamples(workflow, history, splits, control)
  expect_equal(result$.row, 23L)
  expect_equal(result$.pred, 58.5)
  expect_identical(serialize(splits, NULL), before)
  expect_null(custom_run_check_predictions(result, history, splits))
  expect_error(custom_run_fit_resamples(workflow, history[-24, ], splits, control), "complete forecast horizon")
})

test_that("required driver preprocessing and explicit R2 horizon data work", {
  history <- runtime_history()
  history$Driver <- seq_len(nrow(history))
  history$Horizon <- 1L
  definition <- runtime_definition()
  definition$requirements$recipes <- "R2"
  definition$requirements$predictors <- c("Date", "Combo", "Driver", "Horizon")
  definition <- runtime_rehash(definition)
  recipe <- recipes::recipe(Target ~ ., data = history) %>%
    recipes::step_normalize(Driver)
  workflow <- runtime_workflow(definition, history, recipe, recipe_id = "R2")
  fitted <- generics::fit(workflow, history)
  future <- runtime_future()
  future$Driver <- c(26, 25)
  future$Horizon <- c(2, 1)
  expect_equal(predict(fitted, future)$.pred, c(12.5, 12.5))
  broken <- future
  broken$Horizon <- 99L
  expect_error(predict(fitted, broken), "Horizon")
  expect_error(runtime_workflow(definition, history,
    recipes::step_rm(recipes::recipe(Target ~ ., data = history), Driver), recipe_id = "R2"), "required")
  model <- workflows::extract_fit_parsnip(fitted)$fit
  broken$Horizon <- c(1, 1)
  broken$Driver <- NULL
  expect_error(custom_model_predict_impl(model, broken), "required")
})

test_that("training state is independent across fits and missing history fails clearly", {
  definition <- runtime_seasonal_definition()
  history <- runtime_seasonal_history()
  workflow <- runtime_workflow(definition, history)
  first <- history[history$Combo == "first", ]
  original <- generics::fit(workflow, first)
  changed <- history
  changed$Target[changed$Combo == "second"] <- 1e9
  repeated <- generics::fit(workflow, changed[changed$Combo == "first", ])
  request <- data.frame(Date = as.Date("2025-04-01"), Combo = "first")
  expect_identical(predict(original, request), predict(repeated, request))
  first$Target[1] <- 0
  expect_error(generics::fit(workflow, first), "seasonal history")
  expect_error(generics::fit(workflow, first[13:15, ]), "seasonal history")
  request$Combo <- "new"
  expect_error(custom_model_predict_impl(workflows::extract_fit_parsnip(original)$fit, request), "Combo")
})

test_that("portable fitted models roundtrip without global session bindings", {
  history <- runtime_history()
  history$Driver <- history$Target * 2
  definition <- runtime_definition()
  definition$requirements$predictors <- c("Date", "Combo", "Driver")
  definition$source[["fit"]] <- paste0("function(data, context, parameters) ",
    "stats::lm(stats::reformulate('Driver', 'Target', env = baseenv()), data = data)")
  definition$source[["predict"]] <- paste0("function(object, new_data, context) data.frame(",
    ".finnts_row = new_data$.finnts_row, .pred = as.numeric(stats::predict(object, new_data)))")
  workflow <- runtime_workflow(runtime_rehash(definition), history)
  workflow_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(workflow, workflow_path)
  fitted <- generics::fit(readRDS(workflow_path), history)
  request <- runtime_future()
  request$Driver <- c(52, 50)
  expect_equal(predict(fitted, request)$.pred, c(26, 25))
  fitted_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, fitted_path)
  expect_identical(predict(readRDS(fitted_path), request), predict(fitted, request))
  definition <- runtime_definition()
  definition$source[["fit"]] <- "function(...) new.env()"
  expect_error(generics::fit(runtime_workflow(runtime_rehash(definition)), runtime_history()), "nonportable")
  definition$source[["fit"]] <- "function(...) finnts_hidden_session_value"
  withr::local_options(list(finnts_unused = TRUE))
  expect_error(generics::fit(runtime_workflow(runtime_rehash(definition)), runtime_history()), "finnts_hidden_session_value")
})

test_that("fit hooks reject invalid outcomes and source is not evaluated before guards", {
  definition <- runtime_definition()
  definition$source[["fit"]] <- "function(...) { options(finnts_runtime_executed = TRUE); stop('executed') }"
  definition <- runtime_rehash(definition)
  withr::local_options(list(finnts_runtime_executed = FALSE))
  context <- list(model_type = "local", recipe_id = "R1", date_type = "month",
    forecast_horizon = 2, target_scale = "original")
  data <- runtime_history()
  predictors <- data[c("Date", "Combo")]
  expect_error(custom_model_fit_impl(predictors, data$Target, definition, context), "allow_code")
  expect_error(custom_model_fit_impl(predictors, rep(NA_real_, nrow(data)), definition, context, TRUE), "Target")
  context$run_info <- list(path = "not allowed")
  expect_error(custom_model_fit_impl(predictors, data$Target, definition, context, TRUE), "context")
  expect_false(getOption("finnts_runtime_executed"))
})

test_that("installed temporal models reuse source in a fresh process", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")), "Temporal reuse is checked from an installed namespace")
  definition <- runtime_definition()
  definition$schema_version <- 3L
  definition$requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  definition$source[["predict"]] <- "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=object + context$forecast_step)"
  definition <- runtime_rehash(definition)
  workflow <- runtime_workflow(definition)
  fitted <- generics::fit(workflow, runtime_history())
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  # Load the pinned namespace before S3 dispatch; reuse only the saved workflow.
  worker <- function(path, request, expected_path) {
    stopifnot(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")) == normalizePath(expected_path))
    stats::predict(readRDS(path), request)$.pred
  }
  environment(worker) <- baseenv()
  args <- list(path = path, request = runtime_future(), expected_path = namespace_path)
  result <- callr::r(worker, args = args, libpath = .libPaths())
  expect_equal(result, c(14.5, 13.5))
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  cluster <- parallel::makePSOCKcluster(1L)
  withr::defer(parallel::stopCluster(cluster))
  expect_identical(do.call(parallel::clusterCall, c(list(cl = cluster, fun = worker), args))[[1]], result)
})

test_that("installed runtime works in fresh processes and PSOCK workers", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  if (!file.exists(file.path(namespace_path, "Meta", "package.rds"))) {
    skip("Fresh-worker dispatch is checked from the installed package namespace")
  }
  history <- runtime_history()
  future <- runtime_future()
  workflow <- runtime_workflow()
  fitted <- generics::fit(workflow, history)
  # The callback captures neither test fixtures nor the test environment; each
  # process resolves the already-installed package and fits its own instance.
  worker <- function(workflow, fitted, history, future, expected_path) {
    stopifnot(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")) ==
      normalizePath(expected_path))
    fresh <- generics::fit(workflow, history)
    list(fresh = stats::predict(fresh, future)$.pred,
      restored = stats::predict(fitted, future)$.pred)
  }
  environment(worker) <- baseenv()
  args <- list(workflow = workflow, fitted = fitted, history = history,
    future = future, expected_path = namespace_path)
  separate <- callr::r(worker, args = args, libpath = .libPaths())
  expect_equal(separate$fresh, c(12.5, 12.5))
  expect_identical(separate$fresh, separate$restored)
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  cluster <- parallel::makePSOCKcluster(2L)
  withr::defer(parallel::stopCluster(cluster))
  results <- do.call(parallel::clusterCall, c(list(cl = cluster, fun = worker), args))
  expect_length(results, 2)
  for (result in results) {
    expect_equal(result$fresh, c(12.5, 12.5))
    expect_identical(result$fresh, result$restored)
  }
})

test_that("reference responses satisfy contracts and execute their stated predictions", {
  metadata <- list(date_type = "month", forecast_horizon = 99,
    schema = list(amount = "numeric", enabled = "logical", segment = "factor"))
  directory <- withr::local_tempdir()
  for (version in 1:6) {
    for (modes in list("local", "global", c("local", "global"))) {
      for (supplied in c(FALSE, TRUE)) {
        protocol <- if (version == 1L) NULL else custom_author_protocol(metadata, modes,
          names(metadata$schema), "base_only", supplied, version)
        payload <- list(metadata = metadata, protocol = protocol,
          name = if (supplied) "caller_name" else NULL, model_type = if (supplied) modes else NULL,
          contract = list(model_type = modes), approval_mode = if (supplied) "automatic" else "manual")
        contracts <- custom_author_prompt_examples("contract", payload)
        expect_length(contracts, 2L)
        request <- contracts[[1]]$request
        response <- contracts[[1]]$response
        expect_setequal(names(response), custom_author_contract_fields(protocol, request$name,
          request$model_type, supplied))
        expect_length(contracts[[2]]$response$questions, 1L)
        expect_identical(contracts[[2]]$response$parameters_json, "{}")
        proposal <- response
        proposal$defaults_used <- NULL
        contract <- custom_author_contract(proposal, request$instructions, request$metadata,
          request$name, request$model_type, request$validation_examples, request$protocol)
        expect_identical(contract$model_type, modes)
        expect_equal(contract$examples[[1]]$expected$.pred,
          rep(20 * seq_along(unique(contract$examples[[1]]$history$Combo)), each = 2))
        expect_true(all(vapply(contract$examples, function(example) nrow(example$new_data) ==
          2L * length(unique(example$history$Combo)), logical(1))))
        references <- custom_author_prompt_examples("source", payload)
        expect_length(references, if (version == 1L) 1L else 2L)
        for (reference in references) {
          definition <- custom_author_definition(reference$request$contract, reference$response)
          execution <- reference$execution
          context <- execution$context[c("model_type", "recipe_id", "date_type", "forecast_horizon", "target_scale")]
          predictors <- c("Date", "Combo", definition$requirements$predictors)
          fitted <- custom_model_fit_impl(execution$history[predictors], execution$history$Target,
            definition, context, allow_code = TRUE)
          expect_equal(fitted$state, execution$fitted_state)
          expect_equal(custom_model_predict_impl(fitted, execution$new_data), execution$predictions)
          path <- tempfile(tmpdir = directory, fileext = ".rds")
          saveRDS(fitted, path)
          expect_identical(custom_model_predict_impl(readRDS(path), execution$new_data),
            custom_model_predict_impl(fitted, execution$new_data))
          for (example in reference$request$contract$examples) {
            context$model_type <- example$model_type
            fitted <- custom_model_fit_impl(example$history[predictors], example$history$Target,
              definition, context, allow_code = TRUE)
            expect_equal(custom_model_predict_impl(fitted, example$new_data), example$expected$.pred)
          }
        }
      }
    }
  }
})
