make_daily_multistep_fixture <- function() {
  dates <- seq(as.Date("2023-07-01"), as.Date("2026-06-30"), by = "day")
  day <- seq_along(dates)
  target <- 1000 + 0.2 * day + 80 * sin(2 * pi * day / 7) + 25 * cos(2 * pi * day / 30)
  target[c(180, 550, 900)] <- target[c(180, 550, 900)] * 3

  tibble::tibble(
    KPI = "Synthetic KPI",
    EG_Business_Unit = "Synthetic Unit",
    Date = dates,
    Target = target
  )
}

run_daily_multistep_case <- function(clean_outliers,
                                     hist_start_date,
                                     lag_periods,
                                     rolling_window_periods,
                                     models_to_run) {
  run_path <- tempfile("finnts-multistep-daily-")
  dir.create(run_path, recursive = TRUE)

  run_info <- set_run_info(
    project_name = "synthetic_multistep_daily",
    run_name = paste0(
      if (clean_outliers) "outliers" else "raw",
      if (is.null(lag_periods)) "_auto" else "_explicit"
    ),
    path = run_path,
    add_unique_id = FALSE
  )

  prep_data(
    run_info = run_info,
    input_data = make_daily_multistep_fixture(),
    combo_variables = c("KPI", "EG_Business_Unit"),
    target_variable = "Target",
    date_type = "day",
    forecast_horizon = 92,
    hist_start_date = hist_start_date,
    hist_end_date = as.Date("2026-06-30"),
    combo_cleanup_date = as.Date("2023-07-01"),
    fiscal_year_start = 7,
    clean_missing_values = FALSE,
    clean_outliers = clean_outliers,
    stationary = TRUE,
    box_cox = FALSE,
    forecast_approach = "bottoms_up",
    lag_periods = lag_periods,
    rolling_window_periods = rolling_window_periods,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    models_to_run = models_to_run,
    run_ensemble_models = FALSE,
    pca = TRUE,
    num_hyperparameters = 1,
    back_test_scenarios = 1,
    back_test_spacing = 90,
    seed = 123
  )

  train_models(
    run_info = run_info,
    run_global_models = FALSE,
    run_local_models = TRUE,
    inner_parallel = FALSE,
    seed = 123,
    debug = FALSE
  )

  output <- read_unselected_forecast_data(run_info)

  list(
    forecast = output$forecast,
    trained = get_trained_models(run_info),
    prepared = get_prepped_data(run_info, recipe = "R1"),
    splits = output$splits
  )
}

expect_complete_multistep_output <- function(result, expected_models) {
  forecast <- result$forecast %>%
    dplyr::filter(Model_Name %in% expected_models)

  expect_setequal(unique(result$trained$Model_Name), expected_models)
  expect_setequal(unique(forecast$Model_Name), expected_models)
  expect_true(all(c("Future_Forecast", "Back_Test", "Validation") %in% unique(forecast$Run_Type)))
  expect_true(all(is.finite(forecast$Forecast)))

  expected_training_rows <- sum(!is.na(result$prepared$Target))
  for (model_index in seq_len(nrow(result$trained))) {
    workflow <- result$trained$Model_Fit[[model_index]]
    spec <- workflows::extract_spec_parsnip(workflow)
    lag_periods <- rlang::eval_tidy(spec$args$lag_periods)
    expected_lags <- finnts:::get_multi_lags(lag_periods, forecast_horizon = 92)
    fitted_model <- workflow$fit$fit$fit

    expect_equal(
      names(fitted_model$models),
      paste0("model_lag_", expected_lags),
      info = result$trained$Model_Name[[model_index]]
    )
    expect_equal(
      nrow(fitted_model$data),
      expected_training_rows,
      info = result$trained$Model_Name[[model_index]]
    )
  }

  split_expectations <- result$splits %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Expected_Dates = list(result$prepared$Date[
        result$prepared$Date > Train_End & result$prepared$Date <= Test_End
      ])
    ) %>%
    dplyr::ungroup()

  for (model_name in expected_models) {
    for (split_id in split_expectations$Train_Test_ID) {
      expected_dates <- split_expectations %>%
        dplyr::filter(Train_Test_ID == split_id) %>%
        dplyr::pull(Expected_Dates) %>%
        .[[1]]

      actual <- forecast %>%
        dplyr::filter(Model_Name == model_name, Train_Test_ID == split_id) %>%
        dplyr::arrange(Horizon)

      expect_equal(nrow(actual), length(expected_dates))
      expect_equal(actual$Date, expected_dates)
      expect_equal(actual$Horizon, seq_along(expected_dates))
      expect_false(anyDuplicated(actual$Date) > 0)
      expect_true(all(diff(actual$Date) > 0))
    }
  }
}

test_that("daily multistep models preserve every assessment row", {
  # Exercise every adapter once on the most complex preprocessing path, then
  # use one representative adapter for the shared automatic/raw path.
  settings <- list(
    list(
      clean_outliers = TRUE,
      hist_start_date = as.Date("2025-01-01"),
      lags = c(7, 28),
      rolls = c(7, 14),
      models = c("cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost")
    ),
    list(
      clean_outliers = FALSE,
      hist_start_date = as.Date("2025-01-01"),
      lags = NULL,
      rolls = NULL,
      models = "glmnet"
    )
  )

  for (setting in settings) {
    result <- run_daily_multistep_case(
      clean_outliers = setting$clean_outliers,
      hist_start_date = setting$hist_start_date,
      lag_periods = setting$lags,
      rolling_window_periods = setting$rolls,
      models_to_run = setting$models
    )
    expect_complete_multistep_output(result, expected_models = setting$models)
  }
})
