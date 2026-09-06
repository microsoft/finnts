test_that("outer reconciliation publishes the selected mixture without post-quality switching", {
  fixture <- make_reconciled_selection_fixture()
  fixture$project_info$combo_variables <- "Series"
  fixture$project_info$weekly_to_daily <- FALSE
  fixture$agent_info$project_info <- fixture$project_info
  fixture$agent_info$run_id <- "outer"
  fixture$run_inputs$negative_forecast <- FALSE
  fixture$forecasts$Model_ID <- ifelse(fixture$forecasts$Best_Model == "Yes",
    ifelse(match(fixture$forecasts$Combo, fixture$metadata$hts_combos) %% 2L == 0L,
      "arima--local--R1", "ets--local--R1"), "meanf--local--R1")
  reconciliations <- list()
  evaluations <- 0L
  published <- NULL
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    check_agent_info = function(...) NULL,
    get_best_agent_run = function(...) fixture$run_inputs,
    load_agent_forecast = function(...) fixture$forecasts,
    list_files = function(...) "split.csv",
    read_file = function(...) fixture$splits,
    read_selection_hierarchy = function(...) fixture$metadata,
    read_selection_file = function(...) data.frame(forecast_approach = "bottoms_up"),
    read_series_history = function(run_info, combo, ...) {
      fixture$contexts[[match(combo, fixture$run_inputs$combo)]]
    },
    reconcile = function(initial_fcst, ...) {
      reconciliations[[length(reconciliations) + 1L]] <<- initial_fcst
      rows <- initial_fcst[initial_fcst$Combo %in% fixture$run_inputs$combo, ]
      rows$Combo <- fixture$metadata$original_combos[match(rows$Combo, fixture$run_inputs$combo)]
      rows$Combo_ID <- rows$Combo
      rows$Model_ID <- "Best-Model"
      rows$Best_Model <- "Yes"
      rows$Hyperparameter_ID <- NA_real_
      rows$Run_Type <- NULL
      rows
    },
    select_series_forecasts = function(...) {
      evaluations <<- evaluations + 1L
      original_selector(...)
    },
    create_prediction_intervals = function(data, ...) data,
    convert_weekly_to_daily = function(data, ...) data,
    write_data = function(x, ...) { published <<- x }
  )
  reconcile_agent_forecast(fixture$agent_info, fixture$project_info)
  expect_length(reconciliations, 1L)
  expect_setequal(unique(reconciliations[[1]]$Model_ID), c("arima--local--R1", "ets--local--R1"))
  expect_identical(evaluations, 0L)
  future <- published[published$Train_Test_ID == 1, ]
  expect_equal(future$Forecast, 3 * unname(fixture$values[future$Combo]))
  expect_true(all(published$Best_Model == "Yes"))
})

test_that("standard reconciliation never promotes a uniform alternative after selection", {
  fixture <- make_reconciled_selection_fixture()
  accurate <- fixture$forecasts[fixture$forecasts$Model_ID == "accurate", ]
  safe <- fixture$forecasts[fixture$forecasts$Model_ID == "safe", ]
  forecasts <- dplyr::bind_rows(lapply(c("arima", "ets", "meanf"), function(model) {
    rows <- if (model == "meanf") safe else accurate
    rows$Model_Name <- model
    rows$Model_Type <- "local"
    rows$Recipe_ID <- "R1"
    rows$Model_ID <- paste(model, "local", "R1", sep = "--")
    chosen <- ifelse(match(rows$Combo, fixture$metadata$hts_combos) %% 2L == 0L, "arima", "ets")
    rows$Best_Model <- ifelse(model == chosen, "Yes", "No")
    rows$Run_Type <- NULL
    rows
  }))
  stored <- new.env(parent = emptyenv())
  evaluations <- 0L
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    list_files = function(...) character(),
    read_file = function(run_info, path, return_type = "df", ...) {
      if (return_type == "object") return(fixture$metadata)
      if (grepl("train_test_split", path, fixed = TRUE)) return(fixture$splits)
      if (grepl("hts_data", path, fixed = TRUE)) return(fixture$history)
      forecasts
    },
    read_selection_file = function(run_info, folder, combo = NULL, ...) {
      if (folder == "logs") data.frame(forecast_approach = "standard_hierarchy") else stored[[combo]]
    },
    select_series_forecasts = function(...) { evaluations <<- evaluations + 1L; original_selector(...) },
    write_data = function(x, combo, ...) { stored[[combo]] <- x }
  )
  info <- fixture$project_info
  info$path <- tempdir()
  reconcile_hierarchical_data(info, NULL, "standard_hierarchy", FALSE, FALSE, "month", 1)
  expect_identical(evaluations, 0L)
  expect_true(exists("meanf--local--R1", stored, inherits = FALSE))
  best <- stored[["Best-Model"]]
  future <- best[best$Train_Test_ID == 1, ]
  expect_equal(future$Forecast, 3 * unname(fixture$values[future$Combo]), tolerance = 1e-7)
  expect_true(all(best$Best_Model == "Yes"))
})

test_that("accuracy-only comparison avoids quality evaluation without changing its forecasts", {
  fixture <- make_pre_reconciliation_case("clean", "all", shape = "flat")
  quality_calls <- 0L
  original_selector <- select_series_forecasts
  local_mocked_bindings(select_series_forecasts = function(...) {
    quality_calls <<- quality_calls + 1L
    original_selector(...)
  })
  baseline <- select_pre_reconciliation_inputs(fixture, quality = FALSE)
  expect_identical(quality_calls, 0L)
  for (combo in names(fixture$contexts)) {
    rows <- fixture$forecasts[fixture$forecasts$Combo == combo, ]
    model_ids <- sort(unique(rows$Model_ID))
    template <- rows[rows$Model_ID == model_ids[1], ]
    template <- template[order(template$Train_Test_ID, template$Date), ]
    forecasts <- vapply(model_ids, function(model_id) {
      model <- rows[rows$Model_ID == model_id, ]
      model$Forecast[order(model$Train_Test_ID, model$Date)]
    }, numeric(nrow(template)))
    components <- c(as.list(seq_along(model_ids)),
      utils::combn(seq_along(model_ids), 2, simplify = FALSE), list(seq_along(model_ids)))
    candidate_ids <- vapply(components, function(indices) paste(model_ids[indices], collapse = "_"), character(1))
    candidates <- vapply(components, function(indices) {
      rowMeans(forecasts[, indices, drop = FALSE])
    }, numeric(nrow(template)))
    backtest <- template$Train_Test_ID %in% fixture$splits$Train_Test_ID[fixture$splits$Run_Type == "Back_Test"]
    history <- fixture$contexts[[combo]]$history
    actuals <- history$Target[match(template$Date[backtest], history$Date)]
    actuals[actuals == 0] <- 0.1
    errors <- apply(candidates[backtest, , drop = FALSE], 2, function(prediction) {
      stats::weighted.mean(round(abs((prediction - actuals) / abs(actuals)), 4), abs(actuals))
    })
    winner <- order(errors, candidate_ids)[1]
    selected <- baseline$forecasts[baseline$forecasts$Combo == combo, ]
    selected <- selected[order(selected$Train_Test_ID, selected$Date), ]
    expect_identical(unique(selected$Model_ID), candidate_ids[winner])
    expect_equal(selected$Train_Test_ID, template$Train_Test_ID)
    expect_equal(selected$Date, template$Date)
    expect_equal(selected$Forecast, candidates[, winner], tolerance = 1e-12)
    expect_equal(forecast_backtest_accuracy(history, selected[backtest, ])$WMAPE,
      errors[winner], tolerance = 1e-12)
  }
  malformed <- fixture
  malformed$forecasts$Model_ID[1] <- ""
  expect_error(select_pre_reconciliation_inputs(malformed, quality = FALSE),
    "Candidate identities must be nonmissing")
  malformed <- fixture
  malformed$contexts[[1]]$history$Date[2] <- malformed$contexts[[1]]$history$Date[1]
  expect_error(select_pre_reconciliation_inputs(malformed, quality = FALSE),
    "unique dated historical actuals")
})

test_that("preselection reduces pathological bottom forecasts through real hts", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    for (pathology in c("magnitude", "phase", "trend")) {
      result <- measure_pre_selection_case(make_pre_reconciliation_case(pathology, "all", approach))
      expect_true(result$Meets_Expectation, info = paste(approach, pathology))
      expect_gt(result$Before_Error, 0.01)
      expect_lt(result$After_Error, result$Before_Error / 2)
      expect_true(result$Preselection_Accuracy_OK)
    }
  }
})

test_that("seasonal amplitude preference avoids flattened reconciled forecasts", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    result <- measure_pre_selection_case(make_pre_reconciliation_case("flatten", "all", approach))
    expect_true(result$Meets_Expectation)
    expect_equal(result$Before_Error, 0.06439506, tolerance = 1e-7)
    expect_lt(result$After_Error, 1e-6)
    expect_lt(result$After_Amplitude, 1e-6)
    expect_equal(result$After_Phase, 1, tolerance = 1e-7)
    expect_true(result$Preselection_Accuracy_OK)
  }
})

test_that("supported short seasonal selection improves real hts output", {
  result <- measure_pre_selection_case(make_pre_reconciliation_case("phase", "all", horizon = 3L))
  expect_true(result$Meets_Expectation)
  expect_gt(result$Before_Error, 0.1)
  expect_lt(result$After_Error, result$Before_Error / 2)
  expect_gt(result$After_Phase, 0)
  expect_true(result$Preselection_Accuracy_OK)
})

test_that("preselection fixes finite shocks without damaging uninjected siblings", {
  result <- measure_pre_selection_case(make_pre_reconciliation_case("level", "leaf"))
  expect_gt(result$Before_Endpoint, 1)
  expect_gt(result$Before_Sibling, 0)
  expect_lt(result$After_Endpoint, 1e-7)
  expect_lt(result$After_Sibling, 1e-7)
})

test_that("short evidence and accuracy allowance remain explicit selection limits", {
  short <- make_pre_reconciliation_case("phase", "all", history_cycles = 1L)
  result <- measure_pre_selection_case(short)
  expect_false(result$Meets_Expectation)
  expect_equal(result$After_Error, result$Before_Error, tolerance = 1e-8)
  expect_lt(result$After_Phase, 0)

  fixture <- make_pre_reconciliation_case("level", "all", shape = "flat")
  for (combo in names(fixture$contexts)) {
    rows <- fixture$forecasts$Combo == combo
    futures <- rows & fixture$forecasts$Train_Test_ID == 1L
    safe_ids <- unique(fixture$forecasts$Model_ID[futures & fixture$forecasts$Forecast < 2 * fixture$contexts[[combo]]$history$Target[1]])
    safer <- rows & fixture$forecasts$Model_ID %in% safe_ids & fixture$forecasts$Train_Test_ID != 1L
    fixture$forecasts$Forecast[safer] <- fixture$forecasts$Target[safer] * 1.10
  }
  selected <- select_pre_reconciliation_inputs(fixture, max_average = 1L)
  expect_true(all(vapply(selected$selections, function(selection) {
    selection$rankings$Violations[match(selection$selected_id, selection$rankings$Model_ID)] > 0
  }, logical(1))))
  score <- score_pre_selection_case(fixture, reconcile_pre_selection_case(fixture, selected))
  expect_gt(score[["relative_error"]], 1)
})

test_that("invalid base keys and values are rejected before hts", {
  fixture <- make_pre_reconciliation_case("clean", "all", shape = "flat")
  first_combo <- fixture$metadata$hts_combos[1]
  candidates <- unique(fixture$forecasts$Model_ID)
  missing <- fixture$forecasts$Combo == first_combo & fixture$forecasts$Model_ID == candidates[1] &
    fixture$forecasts$Train_Test_ID == 4L
  fixture$forecasts <- fixture$forecasts[!missing, ]
  invalid <- fixture$forecasts$Combo == first_combo & fixture$forecasts$Model_ID == candidates[2] &
    fixture$forecasts$Train_Test_ID == 1L
  fixture$forecasts$Forecast[invalid] <- NA_real_
  selected <- select_pre_reconciliation_inputs(fixture)
  expect_identical(selected$selections[[first_combo]]$selected_id, candidates[3])
  expect_true(all(is.finite(selected$forecasts$Forecast)))
  fixture$forecasts$Forecast[fixture$forecasts$Combo == first_combo] <- Inf
  expect_error(select_pre_reconciliation_inputs(fixture), class = "finnts_forecast_selection_rejected")
})