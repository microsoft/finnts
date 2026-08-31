test_that("outlier summaries handle entirely missing dates without infinities", {
  outlier_scan <- tibble::tibble(
    Combo = c("A", "B"),
    total_rows = c(8, 7),
    outlier_count = c(NA_real_, NA_real_),
    outlier_pct = c(NA_real_, NA_real_),
    first_outlier_dt = c(NA, NA),
    last_outlier_dt = c(NA, NA)
  )

  expect_no_warning(summary <- summarize_outlier_scan(outlier_scan))

  expect_equal(summary$total_rows, 15)
  expect_equal(summary$outlier_count, 0)
  expect_equal(summary$outlier_pct, 0)
  expect_true(is.na(summary$first_outlier_dt))
  expect_true(is.na(summary$last_outlier_dt))
  expect_equal(
    format_eda_result(summary$first_outlier_dt, "None observed"),
    "None observed"
  )
})

test_that("outlier summaries retain valid date boundaries", {
  outlier_scan <- tibble::tibble(
    total_rows = c(8, 8),
    outlier_count = c(1, 2),
    first_outlier_dt = as.Date(c("2025-04-01", "2024-07-01")),
    last_outlier_dt = as.Date(c("2025-04-01", "2026-01-01"))
  )

  summary <- summarize_outlier_scan(outlier_scan)

  expect_equal(summary$outlier_count, 3)
  expect_equal(summary$outlier_pct, 18.75)
  expect_equal(summary$first_outlier_dt, "2024-07-01")
  expect_equal(summary$last_outlier_dt, "2026-01-01")
})

test_that("external regressor summaries omit groups without finite correlations", {
  xreg_scan <- tibble::tibble(
    Combo = c("A", "B", "A", "B", "A"),
    Regressor = c("Non_PO_Mix", "Non_PO_Mix", "Non_PO_Mix", "Non_PO_Mix", "PO_Mix"),
    Lag = c(4, 4, 0, 0, 1),
    dCor = c(NA_real_, NA_real_, 0.3, 0.5, Inf)
  )

  expect_no_warning(summary <- summarize_xreg_scan(xreg_scan))

  expect_equal(nrow(summary), 1)
  expect_equal(summary$Regressor, "Non_PO_Mix")
  expect_equal(summary$Lag, 0)
  expect_equal(summary$Avg_dCor, 0.4)
  expect_equal(summary$Median_dCor, 0.4)
  expect_equal(summary$Max_dCor, 0.5)
  expect_true(all(is.finite(unlist(summary[c("Avg_dCor", "Median_dCor", "Max_dCor")]))))

  unavailable <- summarize_xreg_scan(dplyr::filter(xreg_scan, .data$Lag == 4))
  expect_equal(nrow(unavailable), 0)
})

test_that("global EDA prompt handles no outliers and unavailable correlations", {
  testthat::skip_if_not_installed("knitr")

  output_path <- withr::local_tempdir()
  project_info <- list(
    project_name = "eda_summary_test",
    run_name = "agent_run",
    storage_object = NULL,
    path = output_path,
    data_output = "csv",
    object_output = "rds",
    combo_variables = "ID",
    target_variable = "Target",
    date_type = "quarter",
    fiscal_year_start = 1,
    weekly_to_daily = TRUE
  )
  combo_names <- c("A", "B")

  write_data(
    x = list(
      total_rows = 16,
      n_series = 2,
      rows_min = 8,
      rows_max = 8,
      rows_avg = 8,
      neg_count = 0,
      neg_pct = 0,
      date_start = "2024-07-01",
      date_end = "2026-04-01"
    ),
    combo = NULL,
    run_info = project_info,
    output_type = "object",
    folder = "eda",
    suffix = "-data_profile"
  )
  write_data(
    x = list(hierarchy = "none"),
    combo = NULL,
    run_info = project_info,
    output_type = "object",
    folder = "eda",
    suffix = "-hierarchy"
  )

  for (combo_name in combo_names) {
    write_data(
      x = tibble::tibble(Combo = combo_name, Lag = 1, Value = 0.5),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-acf"
    )
    write_data(
      x = tibble::tibble(Combo = combo_name, Lag = 1, Value = 0.4),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-pacf"
    )
    write_data(
      x = tibble::tibble(
        Combo = combo_name,
        stationary_adf = FALSE,
        stationary_kpss = FALSE
      ),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-stationarity"
    )
    write_data(
      x = tibble::tibble(
        Combo = combo_name,
        total_rows = 8,
        missing_count = 0,
        missing_pct = 0,
        longest_gap = 0
      ),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-missing"
    )
    write_data(
      x = tibble::tibble(
        Combo = combo_name,
        total_rows = 8,
        outlier_count = NA_real_,
        outlier_pct = NA_real_,
        first_outlier_dt = as.Date(NA),
        last_outlier_dt = as.Date(NA)
      ),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-outliers"
    )
    write_data(
      x = tibble::tibble(Combo = combo_name, Lag = 4, Value = 0.3),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-add_season"
    )
    write_data(
      x = tibble::tibble(
        Combo = combo_name,
        Regressor = "Non_PO_Mix",
        Lag = c(0, 4),
        dCor = c(0.4, NA_real_)
      ),
      combo = combo_name,
      run_info = project_info,
      output_type = "data",
      folder = "eda",
      suffix = "-xreg_scan"
    )
  }

  agent_info <- list(
    project_info = project_info,
    run_id = "agent_run",
    hist_end_date = as.Date("2026-04-01"),
    external_regressors = "Non_PO_Mix"
  )

  expect_no_warning(prompt <- load_eda_results(agent_info = agent_info))

  expect_match(prompt, "First Outlier Date: None observed", fixed = TRUE)
  expect_match(prompt, "Last Outlier Date: None observed", fixed = TRUE)
  expect_false(grepl("Inf|-Inf|NaN", prompt))
  expect_match(prompt, "Non_PO_Mix", fixed = TRUE)
})
