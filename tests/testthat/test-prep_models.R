seasonal_period_test_data <- function() {
  periods <- seq_len(36)

  tibble::tibble(
    Date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = length(periods)
    ),
    id = "A",
    value = 100 + 10 * sin(2 * pi * periods / 4) + periods / 10
  )
}

workflow_seasonal_periods <- function(workflow) {
  args <- workflows::extract_spec_parsnip(workflow)$args

  args[paste0("seasonal_period_", 1:3)] %>%
    lapply(rlang::eval_tidy) %>%
    unlist(use.names = FALSE) %>%
    as.numeric()
}

test_that("custom seasonal periods reach supported model workflows", {
  run_path <- withr::local_tempdir()
  run_info <- set_run_info(
    project_name = "custom_seasonal_periods",
    run_name = "test",
    path = run_path,
    add_unique_id = FALSE
  )

  prep_data(
    run_info = run_info,
    input_data = seasonal_period_test_data(),
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    recipes_to_run = "R1"
  )

  custom_periods <- list(4L, 2)
  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = c("stlm-arima", "stlm-ets", "tbats"),
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    seasonal_period = custom_periods
  )

  prepared_models <- get_prepped_models(run_info)
  workflow_tbl <- prepared_models$Data[[
    which(prepared_models$Type == "Model_Workflows")
  ]]
  training_data <- get_prepped_data(run_info, recipe = "R1") %>%
    dplyr::filter(!is.na(Target))

  expect_setequal(
    workflow_tbl$Model_Name,
    c("stlm-arima", "stlm-ets", "tbats")
  )

  for (index in seq_len(nrow(workflow_tbl))) {
    expect_equal(
      workflow_seasonal_periods(workflow_tbl$Model_Workflow[[index]]),
      c(4, 2),
      info = workflow_tbl$Model_Name[[index]]
    )
    expect_no_error(
      generics::fit(workflow_tbl$Model_Workflow[[index]], training_data)
    )
  }

  run_log <- get_run_info(
    project_name = "custom_seasonal_periods",
    run_name = "test",
    path = run_path
  )
  expect_equal(run_log$seasonal_period, "4---2")
})

test_that("NULL seasonal periods remain missing in the run log", {
  run_path <- withr::local_tempdir()
  run_info <- set_run_info(
    project_name = "default_seasonal_periods",
    run_name = "test",
    path = run_path,
    add_unique_id = FALSE
  )

  prep_data(
    run_info = run_info,
    input_data = seasonal_period_test_data(),
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    recipes_to_run = "R1"
  )
  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "tbats",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    seasonal_period = NULL
  )

  run_log <- get_run_info(
    project_name = "default_seasonal_periods",
    run_name = "test",
    path = run_path
  )
  expect_true(is.na(run_log$seasonal_period))
})

test_that("invalid custom seasonal periods fail before model training", {
  expect_equal(validate_seasonal_period(c(4L, 2L)), c(4, 2))
  expect_equal(validate_seasonal_period(list(4, 2L)), c(4, 2))

  expect_error(validate_seasonal_period(numeric()), "between 1 and 3")
  expect_error(validate_seasonal_period(c(12, 6, 4, 2)), "between 1 and 3")
  expect_error(validate_seasonal_period(c(4, NA_real_)), "finite")
  expect_error(validate_seasonal_period(c(4, Inf)), "finite")
  for (seasonal_period in list(1, c(1, 12), c(12, 1), c(12, 3, 1))) {
    expect_error(
      validate_seasonal_period(seasonal_period),
      "greater than 1",
      info = paste(seasonal_period, collapse = "---")
    )
  }
  expect_error(validate_seasonal_period(c(4, 4)), "unique")
  expect_error(validate_seasonal_period(list(c(4, 2))), "numeric vector")
})

test_that("default seasonal periods are valid at every cadence", {
  for (date_type in c("year", "quarter", "month", "week", "day")) {
    seasonal_periods <- get_seasonal_periods(date_type)

    expect_true(all(seasonal_periods > 1), info = date_type)
    expect_identical(
      validate_seasonal_period(seasonal_periods),
      as.numeric(seasonal_periods),
      info = date_type
    )
  }
})

test_that("yearly defaults fit supported seasonal workflows", {
  periods <- seq_len(24)
  train_data <- tibble::tibble(
    Date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = length(periods)
    ),
    Combo = "A",
    Target = 100 + periods + 5 * sin(2 * pi * periods / 2)
  )
  seasonal_periods <- get_seasonal_periods("year")
  workflows <- list(
    `stlm-arima` = finnts:::stlm_arima(train_data, seasonal_periods),
    `stlm-ets` = finnts:::stlm_ets(train_data, seasonal_periods),
    tbats = finnts:::tbats(train_data, seasonal_periods)
  )

  for (model_name in names(workflows)) {
    expect_no_error(generics::fit(workflows[[model_name]], train_data))
  }
})
