test_that("global reference fixtures detect pooling and calendar regressions", {
  for (rule in names(custom_global_drivers())) for (series in c(2L, 3L, 6L)) {
    for (cutoff in c("2023-11-01", "2024-06-01")) {
      panel <- custom_global_case(rule, custom_global_panel(series = series, cutoff = as.Date(cutoff)))
      definition <- custom_global_definition(rule)
      unchanged <- serialize(list(panel, definition), NULL)
      expected <- custom_global_reference(rule, panel$history, panel$future)
      fitted <- custom_global_fit(definition, panel)
      expect_equal(custom_model_predict_impl(fitted, panel$future), expected)
      shuffled <- panel$future[rev(seq_len(nrow(panel$future))), ]
      expect_equal(custom_model_predict_impl(fitted, shuffled), rev(expected))
      reordered <- panel
      reordered$history <- panel$history[rev(seq_len(nrow(panel$history))), ]
      expect_equal(custom_model_predict_impl(custom_global_fit(definition, reordered), panel$future), expected)
      path <- withr::local_tempfile(fileext = ".rds")
      saveRDS(fitted, path)
      expect_identical(custom_model_predict_impl(readRDS(path), panel$future), custom_model_predict_impl(fitted, panel$future))
      changed <- panel
      peer <- paste0("series-", series)
      peer_rows <- which(changed$history$Combo == peer)
      changed$history$Target[peer_rows] <- changed$history$Target[peer_rows] * (2 + 0.3 * sin(seq_along(peer_rows)))
      affected <- custom_model_predict_impl(custom_global_fit(definition, changed), panel$future)
      expect_equal(affected, custom_global_reference(rule, changed$history, panel$future))
      expect_true(any(abs(affected[panel$future$Combo != peer] - expected[panel$future$Combo != peer]) > 1e-08), info = paste(
        rule,
        series, cutoff, "must respond in another series; binding caps may fix individual rows"
      ))
      expect_error(custom_model_predict_impl(fitted, panel$future[-1L, ]), "complete forecast horizon")
      expect_error(custom_model_predict_impl(fitted, panel$future[c(1L, seq_len(nrow(panel$future))), ]), "duplicate row keys")
      expect_identical(serialize(list(panel, definition), NULL), unchanged)
    }
  }
})

test_that("current pooled date selection retains peers without defining an aggregation", {
  current <- custom_author_date_helpers()
  expect_length(current, 4L)
  environment <- new.env(parent = baseenv())
  for (entry in current) assign(entry$name, eval(parse(text = entry$code), environment), environment)
  dates <- as.Date(c("2024-02-29", "2024-03-31"))
  history <- rep(dates, 3L)
  combos <- rep(c("first", "second", "third"), each = 2L)
  expect_identical(environment$finntsRowsDate(dates[2:1], history, combos), list(c(2L, 4L, 6L), c(1L, 3L, 5L)))
  expect_identical(environment$finntsRowsDate(as.Date("2024-01-01"), history, combos), list(integer()))
  expect_identical(environment$finntsRowsDate(as.Date(character()), history, combos), list())
  expect_error(environment$finntsRowsDate(as.numeric(dates), history, combos), class = "finnts_custom_date_error")
  expect_error(environment$finntsRowsDate(dates, c(history, history[1L]), c(combos, "first")), class = "finnts_custom_date_error")
  expect_error(environment$finntsMatchDate(dates, history), class = "finnts_custom_date_error")
  proposal <- list(fit_body = "data", predict_body = "rep(0, nrow(new_data))", helpers = list())
  assembled <- custom_author_assemble(proposal)
  expect_length(assembled$source, 8L)
  expect_identical(custom_author_assemble(assembled, assembled = TRUE), assembled)
  assembled$source[[8L]]$code <- "function(...) list()"
  expect_error(custom_author_assemble(assembled, assembled = TRUE), "wrapper")
  for (defect in c("date_key_type", "date_key_duplicate", "date_shift_invalid")) {
    definition <- custom_global_definition("pooled_mean")
    definition$source[["fit"]] <- paste0("function(data, context, parameters) finntsDateError('", defect, "')")
    definition$version_id <- custom_model_definition_digest(definition)
    panel <- custom_global_case("pooled_mean")
    example <- custom_model_example(panel$history, panel$future, rep(0, nrow(panel$future)), model_type = "global")
    report <- custom_author_child(definition, list(example), list(data = panel$history, metadata = panel$context))
    expect_identical(report$failure$reason, defect)
    expect_identical(report$failure$phase, "example_fit")
  }
})

test_that("global references reject plausible local and first-date-only substitutes", {
  panel <- custom_global_panel()
  for (rule in c("pooled_mean", "seasonal")) {
    expected <- custom_global_reference(rule, panel$history, panel$future)
    first_only <- panel$history[panel$history$Combo == "series-1", ]
    wrong <- custom_global_reference(rule, first_only, panel$future)
    expect_false(isTRUE(all.equal(wrong, expected)))
  }
  dates <- seq(as.Date("2021-01-01"), by = "month", length.out = 36L)
  history <- data.frame(Date = rep(dates, 2L), Combo = rep(c("first", "second"), each = 36L), Target = c(
    101:112, 201:212,
    301:312, 2 * c(101:112, 201:212, 301:312)
  ))
  future <- data.frame(Date = as.Date(c("2024-01-01", "2024-02-01")), Combo = "first")
  expect_equal(custom_global_reference("seasonal", history, future), c(301.5, 303))
})

test_that("global fixtures preserve seeds and distinguish independent arithmetic", {
  withr::local_seed(398L)
  before <- .Random.seed
  first <- custom_global_panel(seed = 17L)
  expect_identical(.Random.seed, before)
  expect_identical(custom_global_panel(seed = 17L), first)
  expect_false(identical(custom_global_panel(seed = 18L), first))
  expect_equal(custom_global_reference_allocate(c(1, 2, 3), c(10, 100, 100), rep(120, 3)), c(10, 44, 66))
  expect_equal(custom_global_reference_allocate(c(1, 2, 3), c(10, 40, 70), rep(120, 3)), c(10, 40, 70))
  for (seed in c(71L, 1049L)) for (rule in c("interactions", "calendar", "log_fiscal", "allocation")) {
    panel <- custom_global_case(rule, custom_global_panel(series = 4L, seed = seed, cutoff = as.Date("2024-01-01")))
    definition <- custom_global_definition(rule)
    expected <- custom_global_reference(rule, panel$history, panel$future)
    fitted <- custom_global_fit(definition, panel)
    expect_equal(custom_model_predict_impl(fitted, panel$future), expected, tolerance = 1e-09, info = paste(rule, seed))
    polluted <- panel$future
    polluted$Target <- rep(c(1e+12, -1e+12), length.out = nrow(polluted))
    expect_identical(custom_model_predict_impl(fitted, polluted), custom_model_predict_impl(fitted, panel$future))
    if (rule == "calendar") {
      wrong <- fitted
      wrong$state$origin <- wrong$state$origin + 365
      expect_false(isTRUE(all.equal(custom_model_predict_impl(wrong, panel$future), expected)))
    }
  }
})

test_that("global fitted features retain category labels origin and column order", {
  for (rule in c("calendar", "log_fiscal")) {
    panel <- custom_global_case(rule, custom_global_panel(cutoff = as.Date("2024-01-01")))
    definition <- custom_global_definition(rule)
    expected <- custom_global_reference(rule, panel$history, panel$future)
    fitted <- custom_global_fit(definition, panel)
    expect_type(fitted$state$columns, "character")
    expect_identical(names(fitted$state$coefficients), fitted$state$columns)
    expect_s3_class(fitted$state$origin, "Date")
    expect_silent(custom_model_state(fitted$state))
    expect_true(any(format(panel$future$Date, "%Y-%m-%d") == "2024-02-01"))
    if (rule == "log_fiscal") {
      panel$history$Region <- factor(panel$history$Region, levels = c("West", "North", "South"))
      panel$future$Region <- factor(panel$future$Region, levels = c("South", "West", "North"))
      expect_identical(fitted$state$levels, c("North", "South", "West"))
    }
    workflow <- custom_model_workflow(definition, recipes::recipe(Target ~ ., panel$history),
      model_type = "global",
      recipe_id = "R1", date_type = "month", forecast_horizon = 3, target_scale = "original", allow_code = TRUE
    )
    trained <- generics::fit(workflow, panel$history)
    expect_equal(predict(trained, panel$future)$.pred, expected, tolerance = 1e-09)
    expect_equal(custom_model_predict_impl(custom_global_fit(definition, panel), panel$future), expected, tolerance = 1e-09)
    selected <- panel$future$Combo == "series-1"
    expect_equal(custom_model_predict_impl(fitted, panel$future[selected, ]), expected[selected])
  }
})

test_that("global rules reject invalid domains and rank without invented fallbacks", {
  for (rule in c("ratio", "shrinkage", "linear", "interactions", "calendar", "log_fiscal", "allocation")) {
    panel <- custom_global_case(rule)
    definition <- custom_global_definition(rule)
    bad <- panel
    if (rule == "ratio")
      bad$history$Units <- 0
    else if (rule == "shrinkage")
      bad$history$Stores <- 0
    else bad$history$Units <- 0
    error <- tryCatch(custom_global_fit(definition, bad), error = identity)
    expect_s3_class(error, "finnts_custom_rule_error")
    expected <- if (rule %in% c("ratio", "shrinkage"))
      "zero_denominator"
    else if (rule == "log_fiscal")
      "log_domain_fit"
    else "rank_deficient_design"
    expect_identical(error$code, expected)
    expect_type(custom_model_predict_impl(custom_global_fit(definition, panel), panel$future), "double")
  }
  panel <- custom_global_case("log_fiscal")
  fitted <- custom_global_fit(custom_global_definition("log_fiscal"), panel)
  for (field in c("Units", "Unit_Price", "Competitor_Price", "FX_Rate", "Marketing_Spend", "Discount_Rate")) {
    future <- panel$future
    future[[field]][1L] <- if (field == "Marketing_Spend")
      -1
    else if (field == "Discount_Rate")
      1
    else 0
    error <- tryCatch(custom_model_predict_impl(fitted, future), error = identity)
    expect_s3_class(error, "finnts_custom_rule_error")
    expect_identical(error$code, "log_domain_predict")
  }
  for (region in c("unseen", NA_character_)) {
    future <- panel$future
    future$Region[1L] <- region
    error <- tryCatch(custom_model_predict_impl(fitted, future), error = identity)
    expect_identical(error$code, "unknown_region")
  }
  panel$history$Target[1L] <- -1
  panel$history$Target[nrow(panel$history)] <- 0
  error <- tryCatch(custom_global_fit(custom_global_definition("log_fiscal"), panel), error = identity)
  expect_identical(error$code, "log_domain_fit")
})

test_that("portfolio allocations preserve budgets caps and complete cohorts", {
  panel <- custom_global_case("allocation")
  definition <- custom_global_definition("allocation")
  fitted <- custom_global_fit(definition, panel)
  future <- panel$future
  future$Capacity_Revenue[future$Combo == "series-1"] <- 10
  prediction <- custom_model_predict_impl(fitted, future)
  expect_equal(prediction, custom_global_reference("allocation", panel$history, future), tolerance = 1e-09)
  expect_true(all(prediction >= 0))
  expect_true(all(prediction <= future$Capacity_Revenue + 1e-09))
  expect_equal(as.numeric(tapply(prediction, future$Date, sum)), rep(600, 3))
  expect_equal(prediction[future$Combo == "series-1"], rep(10, 3))
  expect_error(custom_model_predict_impl(fitted, future[future$Combo != "series-3", ]), "incomplete_cohort")
  zero <- future
  zero$Portfolio_Total <- 0
  expect_equal(custom_model_predict_impl(fitted, zero), rep(0, nrow(zero)))
  for (defect in c("negative_budget", "inconsistent_budget", "negative_capacity", "insufficient_capacity", "zero_weight")) {
    invalid <- future
    object <- fitted
    if (defect == "negative_budget")
      invalid$Portfolio_Total <- -1
    if (defect == "inconsistent_budget")
      invalid$Portfolio_Total[1L] <- 50
    if (defect == "negative_capacity")
      invalid$Capacity_Revenue[1L] <- -1
    if (defect == "insufficient_capacity")
      invalid$Capacity_Revenue <- 0
    if (defect == "zero_weight") {
      history <- panel
      history$history$Target <- 0
      object <- custom_global_fit(definition, history)
    }
    error <- tryCatch(custom_model_predict_impl(object, invalid), error = identity)
    expect_s3_class(error, "finnts_custom_rule_error")
    expect_identical(error$code, switch(defect,
      insufficient_capacity = "infeasible_capacity",
      zero_weight = "zero_allocation_weight",
      "invalid_budget"
    ))
  }
})

test_that("versioned global cohorts reject missing peers before source execution", {
  for (scope in c("series", "groups", "all")) {
    panel <- custom_global_case("shrinkage", custom_global_panel(series = 6L))
    definition <- custom_global_definition("shrinkage")
    definition$requirements$runtime <- list(version = 2L, prediction_scope = "complete_horizon", cohort = scope, group_columns = if (scope ==
      "groups") "Region" else character())
    definition$version_id <- custom_model_definition_digest(definition)
    fitted <- custom_global_fit(definition, panel)
    expected <- custom_global_reference("shrinkage", panel$history, panel$future)
    expect_equal(custom_model_predict_impl(fitted, panel$future), expected)
    selected <- if (scope == "groups")
      panel$future$Region == "North"
    else panel$future$Combo == "series-1"
    if (scope == "all")
      expect_error(custom_model_predict_impl(fitted, panel$future[selected, ]), "incomplete_cohort")
    else expect_equal(custom_model_predict_impl(fitted, panel$future[selected, ]), expected[selected])
    if (scope == "groups") {
      altered <- panel$future
      altered$Region <- "Different"
      expect_error(custom_model_predict_impl(fitted, altered), "group membership")
      bad <- panel
      bad$history$Region[1L] <- "Different"
      expect_error(custom_global_fit(definition, bad), "constant within")
      reordered <- panel$future
      reordered$Region <- factor(reordered$Region, levels = c("West", "South", "North"))
      expect_equal(custom_model_predict_impl(fitted, reordered), expected)
    }
    if (scope != "series") {
      fitted$definition$source[["predict"]] <- "function(object, new_data, context) stop('CANDIDATE_EXECUTED')"
      fitted$definition$version_id <- custom_model_definition_digest(fitted$definition)
      expect_error(custom_model_predict_impl(fitted, panel$future[panel$future$Combo == "series-1", ]), "incomplete_cohort")
    }
  }
  definition <- custom_global_definition("pooled_mean")
  before <- serialize(definition, NULL)
  panel <- custom_global_case("pooled_mean")
  fitted <- custom_global_fit(definition, panel)
  expect_setequal(fitted$context$prediction_cohort$Combo, panel$history$Combo)
  expect_length(custom_model_predict_impl(fitted, panel$future[panel$future$Combo == "series-1", ]), 3L)
  expect_identical(serialize(definition, NULL), before)
  definition$requirements$runtime$version <- 1L
  expect_error(custom_model_definition_digest(definition), "recreate")
})

test_that("global resampling expands cohort execution without changing scoring rows", {
  panel <- custom_global_case("allocation")
  definition <- custom_global_definition("allocation")
  definition$requirements$runtime <- list(version = 2L, prediction_scope = "complete_horizon", cohort = "all", group_columns = character())
  definition$version_id <- custom_model_definition_digest(definition)
  future <- panel$future
  future$Target <- 1e+10 + seq_len(nrow(future))
  data <- rbind(panel$history, future[names(panel$history)])
  scored <- nrow(panel$history) + 1L
  split <- rsample::make_splits(list(analysis = seq_len(nrow(panel$history)), assessment = scored), data)
  splits <- rsample::manual_rset(list(split), "global_short")
  unchanged <- serialize(splits, NULL)
  workflow <- custom_model_workflow(definition, recipes::recipe(Target ~ ., data),
    model_type = "global", recipe_id = "R1",
    date_type = "month", forecast_horizon = 3, target_scale = "original", allow_code = TRUE
  )
  result <- custom_run_fit_resamples(workflow, data, splits, tune::control_resamples(save_pred = TRUE, allow_par = FALSE))
  expect_identical(result$.row, scored)
  expect_equal(result$.pred, custom_global_reference("allocation", panel$history, panel$future)[1L], tolerance = 1e-09)
  expect_identical(serialize(splits, NULL), unchanged)
  expect_error(custom_run_fit_resamples(workflow, data[-nrow(data), ], splits, tune::control_resamples(
    save_pred = TRUE,
    allow_par = FALSE
  )), "complete forecast horizon")
})

test_that("global authoring retains the supplied cohort within the existing row cap", {
  panel <- custom_global_panel(series = 6L, months = 12L)
  columns <- c("Date", "Combo", "Target", "Region", "Units")
  future <- panel$future
  future$Target <- NA_real_
  raw <- rbind(panel$history[columns], future[columns])
  before <- serialize(raw, NULL)
  global <- custom_author_data(raw, NULL, "Combo", "Target", "month", 3, c("Region", "Units"), NULL, max(panel$history$Date),
    complete_global = TRUE
  )
  expect_identical(global$metadata$sampled_series, 6L)
  expect_identical(global$metadata$total_series, 6L)
  expect_equal(nrow(global$data), nrow(panel$history))
  expect_setequal(global$data$Region, panel$history$Region)
  expect_setequal(global$data$Combo, panel$history$Combo)
  expect_identical(serialize(raw, NULL), before)
  legacy <- custom_author_data(raw, NULL, "Combo", "Target", "month", 3, c("Region", "Units"), NULL, max(panel$history$Date))
  expect_identical(legacy$metadata$sampled_series, 3L)
  large <- custom_global_panel(series = 140L, months = 72L)
  future <- large$future
  future$Target <- NA_real_
  raw <- rbind(large$history[columns], future[columns])
  local_mocked_bindings(prep_data = function(...) stop("Unexpected preparation before row-cap rejection"), .package = "finnts")
  expect_error(custom_author_data(raw, NULL, "Combo", "Target", "month", 3, c("Region", "Units"), NULL, max(large$history$Date),
    complete_global = TRUE
  ), "10000 rows")
})

test_that("global calendar and allocation state survives installed worker replay", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")), "Global workers require the installed namespace")
  rules <- c("seasonal", "calendar", "log_fiscal", "allocation")
  cases <- lapply(rules, custom_global_case)
  fitted <- Map(function(rule, panel) {
    definition <- custom_global_definition(rule)
    definition$requirements$runtime <- list(version = 2L, prediction_scope = "complete_horizon", cohort = if (rule ==
      "allocation") "all" else "series", group_columns = character())
    entries <- custom_author_date_helpers()
    definition$source[["finntsRowsDate"]] <- entries[[4L]]$code
    if (rule == "seasonal")
      definition$source[["finntsPredictBody"]] <- paste0(
        "function(object, new_data, context) { ", "vapply(seq_len(nrow(new_data)), function(index) { dates <- finntsShiftDate(rep(new_data$Date[index],3L),c(-12L,-24L,-36L),'month'); ",
        "mean(object$Target[unlist(finntsRowsDate(dates, object$Date, object$Combo))]) },numeric(1)) }"
      )
    definition$version_id <- custom_model_definition_digest(definition)
    custom_global_fit(definition, panel)
  }, rules, cases)
  expected <- Map(function(rule, panel) custom_global_reference(rule, panel$history, panel$future), rules, cases)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  worker <- function(path, cases, expected_path) {
    stopifnot(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")) == normalizePath(expected_path))
    Map(function(object, panel) finnts::custom_model_predict_impl(object, panel$future), readRDS(path), cases)
  }
  environment(worker) <- baseenv()
  args <- list(path = path, cases = cases, expected_path = namespace_path)
  separate <- callr::r(worker, args = args, libpath = .libPaths())
  expect_equal(separate, expected, tolerance = 1e-09)
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  cluster <- parallel::makePSOCKcluster(1L)
  withr::defer(parallel::stopCluster(cluster))
  expect_identical(do.call(parallel::clusterCall, c(list(cl = cluster, fun = worker), args))[[1L]], separate)
})
