test_that("prep_data grouped_hierarchy with mixed xreg future coverage has no NAs", {
  # Synthetic 2×2 combo grid: Region × Segment
  # 24 months history ending 2024-06-01, 3 future months
  hist_dates <- seq.Date(as.Date("2022-07-01"), as.Date("2024-06-01"), by = "month")
  future_dates <- seq.Date(as.Date("2024-07-01"), by = "month", length.out = 3)
  all_dates <- c(hist_dates, future_dates)

  combos <- tidyr::expand_grid(
    Region = c("East", "West"),
    Segment = c("Consumer", "Commercial")
  )

  set.seed(42)
  data_tbl <- combos %>%
    dplyr::slice(rep(seq_len(dplyr::n()), each = length(all_dates))) %>%
    dplyr::mutate(
      Date = rep(all_dates, nrow(combos)),
      Revenue = dplyr::if_else(
        Date <= as.Date("2024-06-01"),
        rep(runif(length(all_dates), 100, 500), nrow(combos)) + rnorm(dplyr::n(), 0, 10),
        NA_real_
      )
    )

  # Regressor_A: future values for ALL combos (fully covered)
  data_tbl <- data_tbl %>%
    dplyr::mutate(
      Regressor_A = runif(dplyr::n(), 10, 50)
    )

  # Regressor_B: future values for ONLY East combos
  # West combos get NA for future dates (simulates partial coverage)
  data_tbl <- data_tbl %>%
    dplyr::mutate(
      Regressor_B = dplyr::case_when(
        Date <= as.Date("2024-06-01") ~ runif(dplyr::n(), 5, 20),
        Region == "East" ~ runif(dplyr::n(), 5, 20),
        TRUE ~ NA_real_
      )
    )

  run_info <- set_run_info()

  # This is the scenario that previously caused NAs in recipe output:
  # per-combo xregs_future_list would differ (East has both regressors with
  # future values, West only has Regressor_A), causing inconsistent columns
  # when recipes are combined for the global "All-Data" model.
  prep_data(
    run_info = run_info,
    input_data = data_tbl,
    combo_variables = c("Region", "Segment"),
    target_variable = "Revenue",
    date_type = "month",
    forecast_horizon = 3,
    external_regressors = c("Regressor_A", "Regressor_B"),
    hist_end_date = as.Date("2024-06-01"),
    forecast_approach = "grouped_hierarchy",
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prepped <- get_prepped_data(run_info, recipe = "R1")

  # basic sanity

  expect_gt(nrow(prepped), 0)

  # all hierarchy combos should be present
  combo_list <- unique(prepped$Combo)
  expect_true("Total" %in% combo_list)
  expect_true(any(grepl("^Region_", combo_list)))
  expect_true(any(grepl("^Segment_", combo_list)))

  # key check: no NAs in any numeric feature column
  # Target is intentionally NA for future rows; _original columns may have
  # intentional NAs for future periods (foundation model raw xregs)
  feature_cols <- prepped %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(-tidyselect::any_of(c("Target", "Target_Original"))) %>%
    dplyr::select(-tidyselect::matches("_original$")) %>%
    colnames()

  for (col in feature_cols) {
    na_count <- sum(is.na(prepped[[col]]))
    expect_equal(na_count, 0,
      info = paste("Column", col, "has", na_count, "NAs")
    )
  }

  # schema consistency: every combo should have the same numeric feature columns
  # (combo variable columns like Region/Segment differ by hierarchy level)
  combos_in_data <- unique(prepped$Combo)
  col_sets <- lapply(combos_in_data, function(cb) {
    prepped %>%
      dplyr::filter(Combo == cb) %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  })
  for (i in seq_along(col_sets)) {
    expect_equal(sort(col_sets[[i]]), sort(col_sets[[1]]),
      info = paste("Numeric column mismatch for combo:", combos_in_data[i])
    )
  }

  # historical regressor-derived features should not be all zeros
  hist_prepped <- prepped %>%
    dplyr::filter(Date <= as.Date("2024-06-01"))

  reg_a_cols <- grep("^Regressor_A", colnames(hist_prepped), value = TRUE)
  reg_a_cols <- reg_a_cols[!grepl("(_lag|_roll|_original)", reg_a_cols)]
  for (col in reg_a_cols) {
    expect_false(all(hist_prepped[[col]] == 0),
      info = paste("Historical", col, "should not be all zeros")
    )
  }

  reg_b_cols <- grep("^Regressor_B", colnames(hist_prepped), value = TRUE)
  reg_b_cols <- reg_b_cols[!grepl("(_lag|_roll|_original)", reg_b_cols)]
  for (col in reg_b_cols) {
    expect_false(all(hist_prepped[[col]] == 0),
      info = paste("Historical", col, "should not be all zeros")
    )
  }
})

test_that("clean_outliers keeps future Target_Original NA so future Target is not zero", {
  # Regression: the recipe's blanket NA->0 fill previously reset future
  # Target_Original to 0. create_splits() then copied that into the future
  # assessment Target, producing future Target = 0 for every model whenever
  # clean_outliers = TRUE, even for series with no historical zeros.
  hist_dates <- seq.Date(as.Date("2023-07-01"), as.Date("2026-06-01"), by = "month")
  set.seed(7)
  data_tbl <- tibble::tibble(
    id = "A",
    Date = hist_dates,
    value = runif(length(hist_dates), 3e5, 2e6)
  )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data_tbl,
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 6,
    hist_end_date = as.Date("2026-06-01"),
    clean_outliers = TRUE,
    recipes_to_run = "R1"
  )

  prepped <- get_prepped_data(run_info, recipe = "R1")
  future_rows <- prepped %>%
    dplyr::filter(Date > as.Date("2026-06-01"))

  expect_gt(nrow(future_rows), 0)
  expect_true("Target_Original" %in% colnames(future_rows))
  expect_true(all(is.na(future_rows$Target)))
  expect_true(all(is.na(future_rows$Target_Original)))

  # create_splits must not copy zeros into the future assessment Target
  splits <- tibble::tibble(
    Train_Test_ID = 1,
    Train_End = as.Date("2026-06-01"),
    Test_End = max(prepped$Date)
  )
  resamples <- create_splits(prepped, splits)
  future_assessment <- rsample::assessment(resamples$splits[[1]])
  expect_true(all(is.na(future_assessment$Target)))
})
