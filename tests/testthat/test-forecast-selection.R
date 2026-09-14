test_that("final_models rejects an explosive accuracy winner and its averages", {
  local_mocked_bindings(
    par_start = function(...) {
      list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
    }
  )
  run_info <- make_best_models_fixture()
  forecast_path <- locate_single_models_file(run_info)
  predictions <- read_fcst_file(forecast_path) %>%
    dplyr::mutate(
      Forecast = dplyr::case_when(
        Train_Test_ID == 1 & Model_Name == "meanf" ~ 100000 * (100 + Horizon),
        Train_Test_ID == 1 ~ 100 + Horizon,
        Model_Name == "meanf" ~ Target * 1.01,
        TRUE ~ Target * 1.08
      )
    )
  write_fcst_file(predictions, forecast_path)

  final_models(run_info, average_models = TRUE)

  forecasts <- get_forecast_data(run_info)
  selected <- forecasts %>% dplyr::filter(Best_Model == "Yes")
  expect_identical(unique(selected$Model_ID), "snaive--local--R1")
  expect_true(all(selected$Forecast[selected$Run_Type == "Future_Forecast"] < 1000))
  rejected <- forecasts %>% dplyr::filter(Model_Name == "meanf")
  expect_gt(nrow(rejected), 0)
  expect_true(all(rejected$Best_Model == "No"))
})

test_that("the saved nonwinning average uses forecast quality as well as accuracy", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  run_info <- make_best_models_fixture()
  forecast_path <- locate_single_models_file(run_info)
  template <- read_fcst_file(forecast_path) %>% dplyr::filter(Model_Name == "meanf")
  models <- c("meanf", "snaive", "naive")
  errors <- c(0.01, 0.012, 0.014)
  predictions <- dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
    template %>% dplyr::mutate(
      Model_Name = models[model_index],
      Model_ID = paste(Model_Name, Model_Type, Recipe_ID, sep = "--"),
      Forecast = ifelse(Train_Test_ID == 1,
        (100 + Horizon) * ifelse(Model_Name == "snaive", 3, 1),
        Target * (1 + errors[model_index]))
    )
  }))
  write_fcst_file(predictions, forecast_path)

  final_models(run_info, average_models = TRUE)

  average <- read_selection_file(run_info, "forecasts", "-average_models", "Synthetic")
  individuals <- read_fcst_file(forecast_path)
  expect_identical(unique(individuals$Model_ID[individuals$Best_Model == "Yes"]), "meanf--local--R1")
  expect_setequal(strsplit(unique(average$Model_ID), "_", fixed = TRUE)[[1]],
    c("meanf--local--R1", "naive--local--R1"))
  expect_true(all(average$Best_Model == "No"))
  expect_equal(average$Forecast[average$Train_Test_ID == 1], 101:103)
})

test_that("sliding historical trend medians preserve direct-window risk scores", {
  original_runmed <- stats::runmed
  lapply(c(4L, 5L, 6L, 11L, 12L, 24L), function(horizon) {
    expect_identical(stats::runmed, original_runmed)
    actuals <- 100 + seq_len(48) + 4 * sin(seq_len(48))
    fixture <- make_selection_case(actuals = actuals,
      futures = list(only = rep(150, horizon)), errors = c(only = 0.03))
    expected <- do.call(select_forecast_candidate, fixture)
    reference <- forecast_reference(fixture$history, horizon, fixture$context)
    values <- reference$values
    forecast <- fixture$forecasts$Forecast / reference$normalization
    if (is.finite(reference$seasonal_strength) && reference$seasonal_strength >= 0.6) {
      phases <- ((seq_along(values) - length(values) - 1) %% reference$period) + 1
      values <- values - reference$profile[phases]
      forecast <- forecast - rep(reference$profile, length.out = horizon)
    }
    slopes <- vapply(seq_len(length(values) - horizon + 1L), function(start) {
      stats::median(diff(values[seq.int(start, length.out = horizon)]))
    }, numeric(1))
    slope_scale <- max(stats::mad(slopes), 0.05 * reference$scale / horizon)
    direct_risk <- max(0, abs(stats::median(diff(forecast)) - stats::median(slopes)) /
      (6 * slope_scale) - 1)
    expect_equal(expected$rankings$Checks[[1]][["trend"]], direct_risk, tolerance = 1e-12)
    reference_calls <- 0L
    local_mocked_bindings(runmed = function(values, k, ...) {
      reference_calls <<- reference_calls + 1L
      half <- (k - 1L) / 2L
      result <- rep(NA_real_, length(values))
      positions <- seq.int(half + 1L, length(values) - half)
      result[positions] <- vapply(positions, function(position) {
        stats::median(values[seq.int(position - half, position + half)])
      }, numeric(1))
      result
    }, .package = "stats")
    expect_equal(do.call(select_forecast_candidate, fixture), expected, tolerance = 1e-12)
    expect_identical(reference_calls, as.integer(horizon %% 2L == 0L))
  })
  expect_identical(stats::runmed, original_runmed)
})

test_that("catalogue benchmark covers supported recipes and every pair and triple", {
  evaluated <- list()
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    par_start = function(...) {
      list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
    },
    select_series_forecasts = function(predictions, ...) {
      evaluated[[length(evaluated) + 1L]] <<- predictions
      original_selector(predictions, ...)
    }
  )
  catalogue <- selection_benchmark_catalogue()
  expect_equal(nrow(catalogue), 41L)
  expect_false(anyDuplicated(catalogue$Model_ID) > 0)
  expect_setequal(catalogue$Model_Name[catalogue$Recipe_ID == "R2"], list_r2_models())
  expect_equal(choose(nrow(catalogue), 2) + choose(nrow(catalogue), 3), 11480)
  category <- ifelse(catalogue$Recipe_ID == "ensemble", "ensemble",
    ifelse(catalogue$Model_Type == "global", "global", catalogue$Recipe_ID))
  small_catalogue <- catalogue[match(c("R1", "R2", "global", "ensemble"), category), ]
  expect_equal(nrow(small_catalogue), 4L)
  expect_false(anyNA(small_catalogue$Model_ID))
  fixture <- make_catalogue_selection_fixture("smoke", small_catalogue, distinct_forecasts = TRUE)
  result <- final_models(fixture$run_info, average_models = TRUE, max_model_average = 3,
    weekly_to_daily = FALSE, parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1)
  rankings <- result$selections$Synthetic$rankings
  expect_length(evaluated, 2L)
  expect_equal(nrow(rankings), 4 + choose(4, 2) + choose(4, 3))
  expect_true(all(rankings$Eligible))
  model_ids <- sort(small_catalogue$Model_ID)
  combinations <- c(utils::combn(model_ids, 2, simplify = FALSE), utils::combn(model_ids, 3, simplify = FALSE))
  average_ids <- vapply(combinations, paste, character(1), collapse = "_")
  expect_setequal(rankings$Model_ID, c(model_ids, average_ids))
  expect_setequal(unique(evaluated[[2]]$Model_ID), c(model_ids, average_ids))
  direct_averages <- stats::setNames(lapply(seq_along(combinations), function(index) {
    components <- lapply(combinations[[index]], function(model_id) {
      rows <- fixture$forecasts[fixture$forecasts$Model_ID == model_id, ]
      rows[order(rows$Train_Test_ID, rows$Date), ]
    })
    measured <- evaluated[[2]][evaluated[[2]]$Model_ID == average_ids[index], ]
    measured <- measured[order(measured$Train_Test_ID, measured$Date), ]
    expected <- Reduce(`+`, lapply(components, `[[`, "Forecast")) / length(components)
    expect_equal(measured$Train_Test_ID, components[[1]]$Train_Test_ID)
    expect_equal(measured$Date, components[[1]]$Date)
    expect_equal(measured$Forecast, expected, tolerance = 1e-10)
    expected
  }), average_ids)
  average <- read_selection_file(fixture$run_info, "forecasts", "-average_models", "Synthetic")
  expect_equal(average$Forecast[order(average$Train_Test_ID, average$Date)],
    direct_averages[[unique(average$Model_ID)]], tolerance = 1e-10)
  expect_true(all(average$Best_Model == "No"))
})

test_that("prepared history restores original actuals from R1 and R2", {
  actuals <- 100 + seq_len(36) + 5 * sin(seq_len(36))
  actuals[15] <- 400
  cleaned <- actuals
  cleaned[15] <- 115
  for (recipe in c("R1", "R2")) {
    for (difference_order in 0:2) {
      for (box_cox in c(FALSE, TRUE)) {
        fixture <- make_selection_history_recipe(
          actuals, cleaned, difference_order, box_cox, recipe
        )
        reconstructed <- normalize_series_history(
          fixture$data, fixture$hist_end_date, recipe,
          fixture$combo_info, stationary = difference_order > 0,
          box_cox = box_cox
        )
        expect_equal(reconstructed$history, fixture$expected,
          tolerance = 1e-7,
          info = paste(recipe, difference_order, box_cox)
        )
      }
    }
  }
})

test_that("history uses Target only when Target_Original is absent", {
  fixture <- make_selection_history_recipe(101:124)
  fixture$data$Target_Original[4] <- NA_real_
  original <- normalize_series_history(fixture$data, fixture$hist_end_date)
  expect_true(is.na(original$history$Target[4]))
  fixture$data$Target_Original <- NULL
  fallback <- normalize_series_history(fixture$data, fixture$hist_end_date)
  expect_equal(fallback$history$Target, 101:124)
})

test_that("history reconstruction preserves the second original value after cleaning", {
  actuals <- as.numeric(101:136)
  actuals[2] <- 300
  cleaned <- actuals
  cleaned[2] <- 102
  fixture <- make_selection_history_recipe(actuals, cleaned, difference_order = 2)
  reconstructed <- normalize_series_history(
    fixture$data, fixture$hist_end_date, "R1",
    fixture$combo_info, stationary = TRUE
  )
  expect_equal(reconstructed$history, fixture$expected)
})

test_that("history normalization validates dates and R2 origins", {
  fixture <- make_selection_history_recipe(101:112, recipe = "R2")
  reversed <- fixture$data[rev(seq_len(nrow(fixture$data))), ]
  expect_equal(
    normalize_series_history(reversed, fixture$hist_end_date, "R2")$history,
    fixture$expected
  )
  duplicate <- dplyr::bind_rows(fixture$data, fixture$data[1, ])
  expect_error(normalize_series_history(duplicate, fixture$hist_end_date, "R2"), "one observation per date")
  fixture$data$Origin[2] <- 100
  expect_error(normalize_series_history(fixture$data, fixture$hist_end_date, "R2"), "inconsistent")
})

test_that("history reads exact artifacts once and never lists directories", {
  run_info <- make_best_models_fixture()
  exact_log <- file.path(run_info$path, "logs", paste0(
    hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"
  ))
  run_log <- read_file(run_info, file_list = exact_log)
  original_reader <- read_file
  reads <- character()
  local_mocked_bindings(
    list_files = function(...) stop("Directory listing is forbidden"),
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      expect_null(path)
      expect_length(file_list, 1)
      expect_false(grepl("*", file_list, fixed = TRUE))
      reads <<- c(reads, file_list)
      original_reader(run_info, file_list = file_list, ...)
    }
  )
  cache <- new.env(parent = emptyenv())
  first <- read_series_history(run_info, "Synthetic", run_log, cache)
  second <- read_series_history(run_info, "Synthetic", run_log, cache)
  expect_identical(first, second)
  expect_length(reads, 1)
  expect_match(reads, "-R1\\.csv$")
})

test_that("selection prefers a safer forecast inside the accuracy allowance", {
  fixture <- make_selection_case()
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "safe")
  expect_gt(result$rankings$Risk[result$rankings$Model_ID == "accurate"], 0)
  fixture$backtests$Forecast[fixture$backtests$Model_ID == "safe"] <- 108.6
  expect_identical(do.call(select_forecast_candidate, fixture)$selected_id, "accurate")
})

test_that("the accuracy allowance uses absolute and relative limits", {
  for (best_error in c(0, 0.02, 0.08, 0.20)) {
    allowance <- max(0.005, best_error * 0.05)
    fixture <- make_selection_case(errors = c(accurate = best_error, safe = best_error + allowance))
    expect_identical(do.call(select_forecast_candidate, fixture)$selected_id, "safe")
    fixture$backtests$Forecast[fixture$backtests$Model_ID == "safe"] <- 100 * (1 + best_error + allowance + 0.001)
    expect_identical(do.call(select_forecast_candidate, fixture)$selected_id, "accurate")
  }
})

test_that("one and two seasonal cycles remain selectable", {
  for (cycles in 1:2) {
    seasonal <- rep(c(80, 90, 100, 110, 120, 130, 120, 110, 100, 90, 80, 70), cycles)
    fixture <- make_selection_case(
      actuals = seasonal, futures = list(seasonal = seasonal[1:12]),
      errors = c(seasonal = 0.03)
    )
    result <- do.call(select_forecast_candidate, fixture)
    expect_identical(result$selected_id, "seasonal")
    expect_true(result$rankings$Eligible)
    expect_equal(result$rankings$Violations, 0L)
  }
})

test_that("strong repeated seasonal phase reversal receives a soft concern", {
  profile <- 100 + 20 * sin(2 * pi * (1:12) / 12)
  fixture <- make_selection_case(
    actuals = rep(profile, 3), futures = list(reversed = 200 - profile, seasonal = profile),
    errors = c(reversed = 0.04, seasonal = 0.043)
  )
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "seasonal")
  expect_true(all(result$rankings$Eligible))
  expect_true("seasonal_phase" %in% unlist(result$rankings$Reasons))
})

test_that("seasonal amplitude preference distinguishes eligible near-accuracy candidates", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  fixture <- make_selection_case(100 + rep(10 * profile, 4),
    futures = list(diluted = 100 + 5 * profile, intact = 100 + 10 * profile),
    errors = c(diluted = 0.0815, intact = 0.083))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "intact")
  expect_true(all(result$rankings$Eligible))
  expect_true(all(result$rankings$Risk == 0))
  expect_true(all(result$rankings$Violations == 0L))
  expect_equal(result$rankings$Seasonal_Fidelity[match(c("diluted", "intact"), result$rankings$Model_ID)],
    c(0.45, 0), tolerance = 1e-10)
  for (unit_scale in c(1e-100, 1e100)) {
    scaled <- fixture
    scaled$history$Target <- scaled$history$Target * unit_scale
    for (table in c("backtests", "forecasts")) {
      scaled[[table]]$Forecast <- scaled[[table]]$Forecast * unit_scale
      scaled[[table]]$Target <- scaled[[table]]$Target * unit_scale
      scaled[[table]] <- scaled[[table]][rev(seq_len(nrow(scaled[[table]]))), ]
    }
    selected <- do.call(select_forecast_candidate, scaled)
    expect_identical(selected$selected_id, result$selected_id)
    expect_equal(selected$rankings$Seasonal_Fidelity, result$rankings$Seasonal_Fidelity, tolerance = 1e-10)
  }
})

test_that("seasonal preference respects risk accuracy and unassessed ties", {
  rankings <- tibble::tibble(Model_ID = c("accuracy", "seasonal"), Eligible = TRUE,
    WMAPE = c(0.08, 0.083), Risk = 0, Violations = 0L,
    Reasons = list(character(), character()), Seasonal_Fidelity = c(0.5, 0))
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "seasonal")
  rankings$Risk[2] <- 0.1
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$Risk[2] <- 0
  rankings$Violations[2] <- 1L
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$Violations[2] <- 0L
  rankings$WMAPE[2] <- 0.086
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$WMAPE[2] <- 0.083
  rankings$Seasonal_Fidelity[1] <- NA_real_
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$Seasonal_Fidelity <- c(0.5, NA_real_)
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$Seasonal_Fidelity <- NULL
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "accuracy")
  rankings$Seasonal_Fidelity <- c(0, 0)
  rankings$WMAPE <- c(0.08, 0.08)
  expect_identical(rank_forecast_candidates(rankings[2:1, ])$selected_id, "accuracy")
})

test_that("historical amplitude variation permits continued seasonal damping", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  actuals <- 100 + unlist(lapply(c(20, 16, 12), function(amplitude) amplitude * profile))
  fixture <- make_selection_case(actuals,
    futures = list(continued = 100 + 10 * profile, fixed = 100 + 16 * profile),
    errors = c(continued = 0.08, fixed = 0.0801))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "continued")
  expect_true(all(result$rankings$Eligible))
  expect_true(all(result$rankings$Violations == 0L))
  expect_equal(result$rankings$Seasonal_Fidelity, c(0, 0), tolerance = 1e-10)
})

test_that("seasonal preference uses each cadence and the smallest configured period", {
  settings <- list(
    list(cadence = "day", period = 7L, configured = NULL),
    list(cadence = "week", period = 52L, configured = NULL),
    list(cadence = "month", period = 12L, configured = NULL),
    list(cadence = "quarter", period = 4L, configured = NULL),
    list(cadence = "week", period = 12L, configured = "52---12---24")
  )
  for (setting in settings) {
    profile <- cos(2 * pi * (seq_len(setting$period) - 0.5) / setting$period)
    fixture <- make_selection_case(100 + rep(10 * profile, 3),
      futures = list(diluted = 100 + 5 * profile, intact = 100 + 10 * profile),
      errors = c(diluted = 0.08, intact = 0.083), date_type = setting$cadence, period = setting$configured)
    expect_equal(forecast_seasonal_period(fixture$context), setting$period)
    result <- do.call(select_forecast_candidate, fixture)
    expect_identical(result$selected_id, "intact")
    expect_true(all(result$rankings$Eligible))
    expect_true(all(is.finite(result$rankings$Seasonal_Fidelity)))
  }
  fixture <- make_selection_case(date_type = "year")
  expect_equal(forecast_seasonal_period(fixture$context), 1L)
  expect_true(all(is.na(do.call(select_forecast_candidate, fixture)$rankings$Seasonal_Fidelity)))
})

test_that("short seasonal forecasts do not reward reversal or flat cancellation", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  fixture <- make_selection_case(100 + rep(10 * profile, 4),
    futures = list(reversed = 100 - 10 * profile[1:3], cancelled = rep(100, 3),
      intact = 100 + 10 * profile[1:3]),
    errors = c(reversed = 0.08, cancelled = 0.0815, intact = 0.083))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "intact")
  expect_true(all(result$rankings$Eligible))
  reversed <- result$rankings[result$rankings$Model_ID == "reversed", ]
  expect_gt(reversed$Risk, 0)
  expect_true("seasonal_phase" %in% reversed$Reasons[[1]])
  expect_equal(result$rankings$Seasonal_Fidelity[match(c("cancelled", "intact"), result$rankings$Model_ID)],
    c(0.95, 0), tolerance = 1e-10)
  expect_equal(result$rankings$Violations[result$rankings$Model_ID == "cancelled"], 0L)
})

test_that("short seasonal assessment requires historical and phase evidence", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  for (setting in list(c(1L, 3L), c(4L, 1L), c(4L, 2L))) {
    horizon <- setting[2]
    fixture <- make_selection_case(100 + rep(10 * profile, setting[1]),
      futures = list(reversed = 100 - 10 * profile[seq_len(horizon)],
        intact = 100 + 10 * profile[seq_len(horizon)]), errors = c(reversed = 0.08, intact = 0.083))
    result <- do.call(select_forecast_candidate, fixture)
    expect_identical(result$selected_id, "reversed")
    expect_true(all(result$rankings$Eligible))
    expect_true(all(is.na(result$rankings$Seasonal_Fidelity)))
    expect_true(all(is.na(vapply(result$rankings$Checks, `[[`, numeric(1), "seasonality"))))
  }
  for (actuals in list(rep(100, 48), 100 + rep(c(0, 0, 0, 5, 10, 15, 15, 10, 5, 0, 0, 0), 4))) {
    fixture <- make_selection_case(actuals, futures = list(only = rep(100, 3)), errors = c(only = 0.03))
    result <- do.call(select_forecast_candidate, fixture)
    expect_true(result$rankings$Eligible)
    expect_true(is.na(result$rankings$Seasonal_Fidelity))
    expect_true(is.na(result$rankings$Checks[[1]][["seasonality"]]))
  }
})

test_that("short seasonal comparisons preserve a supported historical trend", {
  profile <- cos(2 * pi * (seq_len(12) - 0.5) / 12)
  fixture <- make_selection_case(100 + seq_len(48) + rep(10 * profile, 4),
    futures = list(only = 100 + 49:51 + 10 * profile[1:3]), errors = c(only = 0.03))
  result <- do.call(select_forecast_candidate, fixture)
  expect_true(result$rankings$Eligible)
  expect_equal(result$rankings$Violations, 0L)
  expect_equal(result$rankings$Seasonal_Fidelity, 0, tolerance = 1e-10)
})

test_that("nonfinite forecasts survive cleanup and cannot win", {
  invalid <- c(Inf, -Inf, NaN, NA_real_)
  input <- data.frame(Forecast = c(invalid, -3, 0, 2))
  for (allow_negative in c(FALSE, TRUE)) {
    result <- negative_fcst_adj(input, allow_negative)
    expect_identical(result$Forecast[1:4], invalid)
    expect_equal(result$Forecast[5:7], c(if (allow_negative) -3 else 0, 0, 2))
  }
  for (value in invalid) {
    fixture <- make_selection_case()
    fixture$forecasts$Forecast[1] <- value
    result <- do.call(select_forecast_candidate, fixture)
    expect_identical(result$selected_id, "safe")
    expect_false(result$rankings$Eligible[result$rankings$Model_ID == "accurate"])
  }
})

test_that("invalid ensemble inputs cannot be hidden by missing-value filling", {
  fixture <- make_selection_case()
  fixture$forecasts$Forecast[1] <- NA_real_
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  context <- fixture$context
  context$history <- fixture$history
  screened <- screen_ensemble_inputs(predictions, context, context$train_test_split)
  expect_identical(unique(screened$Model_ID), "safe")
  expect_true(all(is.finite(screened$Forecast)))
})

test_that("hard-rejected extra keys do not change valid ensemble inputs", {
  fixture <- make_selection_case(
    futures = list(first = rep(100, 6), second = rep(100, 6), rejected = rep(100, 6)),
    errors = c(first = 0.02, second = 0.03, rejected = 0.01)
  )
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  context <- fixture$context
  context$history <- fixture$history
  control <- predictions[predictions$Model_ID != "rejected", , drop = FALSE]
  expect_identical(screen_ensemble_inputs(control, context, context$train_test_split), control)

  for (split_id in c(1L, 2L)) {
    extra <- predictions[which(predictions$Model_ID == "rejected" &
      predictions$Train_Test_ID == split_id)[1], , drop = FALSE]
    extra$Date <- max(predictions$Date[predictions$Train_Test_ID == split_id]) + 1
    damaged <- dplyr::bind_rows(predictions, extra)
    selection <- select_series_forecasts(damaged, context, context$train_test_split)
    expect_setequal(selection$rankings$Model_ID[selection$rankings$Eligible], c("first", "second"))

    for (reversed in c(FALSE, TRUE)) {
      rows <- if (reversed) damaged[rev(seq_len(nrow(damaged))), , drop = FALSE] else damaged
      expected <- rows[rows$Model_ID != "rejected", , drop = FALSE]
      screened <- screen_ensemble_inputs(rows, context, context$train_test_split)
      expect_identical(screened, expected, info = paste("split", split_id, "reversed", reversed))
    }
  }
})

test_that("ensemble screening retains hard eligibility and empty-pool behavior", {
  fixture <- make_selection_case()
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  context <- fixture$context
  context$history <- fixture$history
  expect_identical(screen_ensemble_inputs(predictions, context, context$train_test_split), predictions)

  for (split_id in c(1L, 2L)) {
    affected <- which(predictions$Model_ID == "accurate" & predictions$Train_Test_ID == split_id)[1]
    for (defect in c("missing", "duplicate", "nonfinite")) {
      rows <- predictions
      if (defect == "missing") rows <- rows[-affected, , drop = FALSE]
      if (defect == "duplicate") rows <- dplyr::bind_rows(rows, rows[affected, , drop = FALSE])
      if (defect == "nonfinite") rows$Forecast[affected] <- Inf
      expect_identical(screen_ensemble_inputs(rows, context, context$train_test_split),
        rows[rows$Model_ID == "safe", , drop = FALSE], info = paste("split", split_id, defect))
    }
  }

  predictions$Forecast <- Inf
  expect_identical(screen_ensemble_inputs(predictions, context, context$train_test_split),
    predictions[0, , drop = FALSE])
})

test_that("ensemble screening preserves validation rows and checks their coverage", {
  fixture <- make_selection_case()
  context <- fixture$context
  context$history <- fixture$history
  validation_split <- context$train_test_split[2, , drop = FALSE]
  validation_split$Train_Test_ID <- 3L
  validation_split$Run_Type <- "Validation"
  splits <- dplyr::bind_rows(context$train_test_split, validation_split)
  validation <- fixture$backtests
  validation$Train_Test_ID <- 3L
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts, validation)
  expect_identical(screen_ensemble_inputs(predictions, context, splits), predictions)

  affected <- which(predictions$Model_ID == "accurate" & predictions$Train_Test_ID == 3L)[1]
  for (defect in c("missing", "duplicate", "nonfinite")) {
    rows <- predictions
    if (defect == "missing") rows <- rows[-affected, , drop = FALSE]
    if (defect == "duplicate") rows <- dplyr::bind_rows(rows, rows[affected, , drop = FALSE])
    if (defect == "nonfinite") rows$Forecast[affected] <- NA_real_
    expect_identical(screen_ensemble_inputs(rows, context, splits),
      rows[rows$Model_ID == "safe", , drop = FALSE], info = defect)
  }
})

test_that("selection validates coverage and supports sole or rejected candidates", {
  fixture <- make_selection_case()
  fixture$forecasts <- fixture$forecasts[-1, ]
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "safe")
  expect_true("incomplete_forecast" %in% unlist(result$rankings$Reasons))
  fixture$forecasts$Forecast <- Inf
  expect_true(is.na(do.call(select_forecast_candidate, fixture)$selected_id))
  fixture <- make_selection_case(futures = list(only = rep(100, 2)), errors = c(only = 0))
  expect_identical(do.call(select_forecast_candidate, fixture)$selected_id, "only")
})

test_that("selection handles data shapes across all supported cadences", {
  shapes <- list(
    zero = rep(0, 24), constant = rep(100, 24), signed = rep(c(-80, -100, -90, -110), 6),
    sparse = rep(c(0, 0, 100, 0), 6), trending = 100 + 1:24,
    noisy = 100 + rep(c(-5, 9, -2, 7), 6)
  )
  for (date_type in c("day", "week", "month", "quarter", "year")) {
    for (shape in shapes) {
      fixture <- make_selection_case(shape,
        futures = list(only = rep(stats::median(shape), 3)),
        errors = c(only = 0.03), date_type = date_type
      )
      result <- do.call(select_forecast_candidate, fixture)
      expect_identical(result$selected_id, "only")
      expect_true(all(is.finite(result$rankings$Risk)))
    }
  }
})

test_that("selection is invariant to row order, units and future target values", {
  fixture <- make_selection_case()
  expected <- do.call(select_forecast_candidate, fixture)$selected_id
  for (scale in c(1e-100, 1, 1e100)) {
    scaled <- fixture
    scaled$history$Target <- scaled$history$Target * scale
    for (table in c("backtests", "forecasts")) {
      scaled[[table]]$Target <- scaled[[table]]$Target * scale
      scaled[[table]]$Forecast <- scaled[[table]]$Forecast * scale
      scaled[[table]] <- scaled[[table]][rev(seq_len(nrow(scaled[[table]]))), ]
    }
    scaled$forecasts$Target <- -1e200
    expect_identical(do.call(select_forecast_candidate, scaled)$selected_id, expected)
  }
})

test_that("a replacement selector is called and its structural contract checked", {
  fixture <- make_selection_case()
  series_data <- fixture$context
  series_data$history <- fixture$history
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  replacement <- function(history, backtests, forecasts, context) {
    result <- select_forecast_candidate(history, backtests, forecasts, context)
    result$selected_id <- "accurate"
    result
  }
  expect_identical(
    select_series_forecasts(predictions, series_data, series_data$train_test_split, selector = replacement)$selected_id,
    "accurate"
  )
  expect_error(select_series_forecasts(predictions, series_data, series_data$train_test_split,
    selector = function(...) list(selected_id = "absent", rankings = data.frame())), "invalid selection contract")
})

test_that("R2-only history uses one exact recipe read", {
  fixture <- make_selection_history_recipe(101:124, recipe = "R2")
  reads <- character()
  local_mocked_bindings(
    list_files = function(...) stop("must not list"),
    read_file = function(run_info, file_list, ...) {
      reads <<- c(reads, file_list)
      expect_match(file_list, "-R2\\.csv$")
      fixture$data
    }
  )
  result <- read_series_history(list(project_name = "project", run_name = "run", path = tempdir(), data_output = "csv"),
    "Synthetic", data.frame(hist_end_date = fixture$hist_end_date, recipes_to_run = "R2", date_type = "month"))
  expect_equal(result$history, fixture$expected)
  expect_length(reads, 1)
})

test_that("weekly collapse preserves invalid values from every expanded day", {
  fixture <- make_selection_case(actuals = rep(100, 104),
    futures = list(accurate = rep(100, 3), safe = rep(100, 3)),
    errors = c(accurate = 0.02, safe = 0.023), date_type = "week")
  native <- dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
    dplyr::mutate(Combo = "Synthetic")
  expanded <- native[rep(seq_len(nrow(native)), each = 7L), ]
  expanded$Date_Day <- expanded$Date + rep(0:6, nrow(native))
  expanded$Forecast <- expanded$Forecast / 7
  expanded$Target <- expanded$Target / 7
  expect_identical(native_forecast_rows(native, "week"), native)
  expect_equal(tibble::as_tibble(native_forecast_rows(expanded, "week")), tibble::as_tibble(native))
  outcomes <- list()
  for (value in c(Inf, -Inf, NaN, NA_real_)) {
    for (day in c(0L, 3L, 6L)) {
      invalid <- expanded
      affected <- invalid$Model_ID == "accurate" & invalid$Train_Test_ID == 1L &
        invalid$Date_Day == min(fixture$forecasts$Date) + day
      invalid$Forecast[affected] <- value
      for (reversed in c(FALSE, TRUE)) {
        rows <- if (reversed) invalid[rev(seq_len(nrow(invalid))), ] else invalid
        collapsed <- native_forecast_rows(rows, "week")
        context <- fixture$context
        context$history <- fixture$history
        selected <- select_series_forecasts(collapsed, context, context$train_test_split)
        screened <- screen_ensemble_inputs(collapsed, context, context$train_test_split)
        outcomes[[length(outcomes) + 1L]] <- data.frame(
          Invalid_Preserved = any(!is.finite(collapsed$Forecast[collapsed$Model_ID == "accurate"])),
          Selected = selected$selected_id,
          Accurate_Eligible = selected$rankings$Eligible[selected$rankings$Model_ID == "accurate"],
          Safe_Eligible = selected$rankings$Eligible[selected$rankings$Model_ID == "safe"],
          Invalid_Screened = !"accurate" %in% screened$Model_ID)
      }
    }
  }
  outcomes <- dplyr::bind_rows(outcomes)
  expect_equal(nrow(outcomes), 24L)
  expect_true(all(outcomes$Invalid_Preserved))
  expect_true(all(outcomes$Selected == "safe"))
  expect_false(any(outcomes$Accurate_Eligible))
  expect_true(all(outcomes$Safe_Eligible))
  expect_true(all(outcomes$Invalid_Screened))
})

test_that("invalid predictions remain invalid after artifact serialization", {
  data <- data.frame(Forecast = c(1, Inf, -Inf, NaN, NA_real_))
  csv <- withr::local_tempfile(fileext = ".csv")
  write_fcst_file(negative_fcst_adj(data, FALSE), csv)
  restored <- read_fcst_file(csv)
  expect_identical(is.finite(restored$Forecast), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(restored$Forecast[2:3], c(Inf, -Inf))
  if (requireNamespace("arrow", quietly = TRUE)) {
    parquet <- withr::local_tempfile(fileext = ".parquet")
    write_fcst_file(negative_fcst_adj(data, FALSE), parquet)
    restored <- read_fcst_file(parquet)
    expect_identical(is.finite(restored$Forecast), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  }
})

test_that("cutoff replay rejects explosions without rejecting usable reference paths", {
  shapes <- list(
    stable = rep(100, 60), trend = 100 + seq_len(60),
    seasonal = 100 + 20 * sin(2 * pi * seq_len(60) / 12),
    shift = c(rep(100, 30), rep(140, 30)), sparse = rep(c(0, 0, 0, 100, 0, 0), 10)
  )
  outcomes <- list()
  for (shape in names(shapes)) {
    for (cutoff in c(12, 24, 36, 48)) {
      history <- shapes[[shape]][seq_len(cutoff)]
      reference <- rep(utils::tail(history, min(12, cutoff)), length.out = 6)
      fixture <- make_selection_case(history,
        futures = list(explosive = rep(1e8, 6), reference = reference),
        errors = c(explosive = 0.01, reference = 0.04))
      selected <- do.call(select_forecast_candidate, fixture)
      holdout <- shapes[[shape]][cutoff + 1:6]
      outcomes[[length(outcomes) + 1L]] <- data.frame(
        Shape = shape, Cutoff = cutoff, Selected = selected$selected_id,
        Rejected_Reference = !selected$rankings$Eligible[selected$rankings$Model_ID == "reference"],
        Holdout_Error = sum(abs(reference - holdout)) / max(sum(abs(holdout)), 0.1)
      )
    }
  }
  replay <- dplyr::bind_rows(outcomes)
  expect_equal(nrow(replay), 20)
  expect_true(all(replay$Selected == "reference"))
  expect_false(any(replay$Rejected_Reference))
  expect_true(all(is.finite(replay$Holdout_Error)))
})

test_that("finished selection reruns compare only their unchanged input settings", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  info <- make_best_models_fixture()
  final_models(info)
  first <- get_forecast_data(info)
  expect_no_error(final_models(info))
  expect_equal(get_forecast_data(info), first)
  expect_error(final_models(info, weekly_to_daily = FALSE), "Inputs have recently changed")
})