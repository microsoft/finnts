make_multistep_date_type_fixture <- function(date_type, date_by, periods) {
  dates <- seq(as.Date("2000-01-01"), by = date_by, length.out = periods)
  period_index <- seq_along(dates)

  tibble::tibble(
    Series = paste("Synthetic", date_type),
    Date = dates,
    Target = 500 + 0.5 * period_index + 20 * sin(2 * pi * period_index / 4)
  )
}

run_multistep_date_type_case <- function(date_type, date_by, periods) {
  run_path <- tempfile(paste0("finnts-multistep-", date_type, "-"))
  dir.create(run_path, recursive = TRUE)
  on.exit(unlink(run_path, recursive = TRUE), add = TRUE)

  run_info <- set_run_info(
    project_name = "synthetic_multistep_date_types",
    run_name = date_type,
    path = run_path,
    add_unique_id = FALSE
  )

  prep_data(
    run_info = run_info,
    input_data = make_multistep_date_type_fixture(date_type, date_by, periods),
    combo_variables = "Series",
    target_variable = "Target",
    date_type = date_type,
    forecast_horizon = 3,
    clean_missing_values = FALSE,
    clean_outliers = FALSE,
    stationary = FALSE,
    box_cox = FALSE,
    lag_periods = c(1, 2, 4),
    rolling_window_periods = c(2, 3),
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    models_to_run = "glmnet",
    run_ensemble_models = FALSE,
    pca = FALSE,
    num_hyperparameters = 1,
    back_test_scenarios = 1,
    back_test_spacing = 3,
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
    forecast = output$forecast %>%
      dplyr::filter(Model_Name == "glmnet"),
    prepared = get_prepped_data(run_info, recipe = "R1"),
    splits = output$splits,
    fitted_model = get_trained_models(run_info)$Model_Fit[[1]]$fit$fit$fit
  )
}

test_that("multistep training and forecasts support every date frequency", {
  cases <- tibble::tribble(
    ~date_type, ~date_by, ~periods,
    "day", "day", 450,
    "week", "week", 160,
    "month", "month", 72,
    "quarter", "3 months", 40,
    "year", "year", 24
  )

  for (case_index in seq_len(nrow(cases))) {
    date_type <- cases$date_type[[case_index]]
    result <- run_multistep_date_type_case(
      date_type = date_type,
      date_by = cases$date_by[[case_index]],
      periods = cases$periods[[case_index]]
    )

    expect_equal(
      names(result$fitted_model$models),
      c("model_lag_1", "model_lag_2", "model_lag_4"),
      info = date_type
    )
    expect_equal(
      nrow(result$fitted_model$data),
      sum(!is.na(result$prepared$Target)),
      info = date_type
    )
    expected_training_rows <- nrow(result$fitted_model$data)
    expect_equal(
      unname(vapply(
        result$fitted_model$models,
        function(model) model$fit$nobs,
        integer(1)
      )),
      rep(expected_training_rows, 3),
      info = date_type
    )

    expected_feature_lags <- list(
      model_lag_1 = c(1, 2, 4),
      model_lag_2 = c(2, 4),
      model_lag_4 = 4
    )
    for (model_name in names(expected_feature_lags)) {
      engine_features <- rownames(result$fitted_model$models[[model_name]]$fit$beta)
      actual_feature_lags <- engine_features %>%
        stringr::str_extract("^Target_lag[0-9]+") %>%
        stringr::str_extract("[0-9]+") %>%
        as.numeric() %>%
        stats::na.omit() %>%
        unique() %>%
        sort()

      expect_equal(
        unname(actual_feature_lags),
        expected_feature_lags[[model_name]],
        info = paste(date_type, model_name)
      )
    }
    expect_true(all(is.finite(result$forecast$Forecast)), info = date_type)

    for (split_index in seq_len(nrow(result$splits))) {
      split <- result$splits[split_index, ]
      expected_dates <- result$prepared$Date[
        result$prepared$Date > split$Train_End &
          result$prepared$Date <= split$Test_End
      ]
      actual <- result$forecast %>%
        dplyr::filter(Train_Test_ID == split$Train_Test_ID) %>%
        dplyr::arrange(Horizon)

      expect_equal(actual$Date, expected_dates, info = date_type)
      expect_equal(actual$Horizon, seq_along(expected_dates), info = date_type)
      expect_false(anyDuplicated(actual$Date) > 0, info = date_type)
    }
  }
})