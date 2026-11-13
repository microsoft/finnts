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
  # clean_outliers = TRUE, even for series with no historical zeros. The fix
  # applies to both the R1 and R2 feature-engineering recipes.
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
    recipes_to_run = c("R1", "R2")
  )

  # both recipes must keep future Target and Target_Original as NA
  for (recipe in c("R1", "R2")) {
    prepped <- get_prepped_data(run_info, recipe = recipe)
    future_rows <- prepped %>%
      dplyr::filter(Date > as.Date("2026-06-01"))

    expect_gt(nrow(future_rows), 0)
    expect_true("Target_Original" %in% colnames(future_rows), info = recipe)
    expect_true(all(is.na(future_rows$Target)), info = recipe)
    expect_true(all(is.na(future_rows$Target_Original)), info = recipe)
  }

  # create_splits must not copy zeros into the future assessment Target
  prepped_r1 <- get_prepped_data(run_info, recipe = "R1")
  splits <- tibble::tibble(
    Train_Test_ID = 1,
    Train_End = as.Date("2026-06-01"),
    Test_End = max(prepped_r1$Date)
  )
  resamples <- create_splits(prepped_r1, splits)
  future_assessment <- rsample::assessment(resamples$splits[[1]])
  expect_true(all(is.na(future_assessment$Target)))
})

# Stand-in for sparklyr::spark_apply() in prep_data() tests: computes the
# grouped worker's result with spark_apply_grouped_result() and wraps it in a
# lazy stand-in that only dplyr::collect() turns into a data frame, like a
# Spark table. Stores the `columns` argument in `calls`.
prep_data_emulate_spark_apply <- function(calls) {
  function(x, f, columns = NULL, group_by = NULL, context = NULL, ...) {
    calls$columns <- columns
    structure(list(data = spark_apply_grouped_result(x, f, columns, group_by, context)),
      class = "prep_data_fake_sdf")
  }
}

# Run prep_data() on two trending monthly series with the given settings,
# sequentially or through the emulated Spark branch, in its own temporary
# folder. Returns the saved "-orig_combo_info" table sorted by Combo, with the
# emulated spark_apply() `columns` argument as attribute "spark_columns".
prep_data_orig_combo_info <- function(spark, data_output, box_cox, stationary, clean_outliers) {
  dates <- seq.Date(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month")
  set.seed(11)
  data_tbl <- tibble::tibble(
    id = rep(c("A", "B"), each = length(dates)),
    Date = rep(dates, 2),
    value = c(100 + 8 * seq_along(dates), 500 + 15 * seq_along(dates)) + stats::rnorm(2 * length(dates), 0, 3)
  )
  path <- tempfile("prep-spark-")
  run_info <- set_run_info(path = path, data_output = data_output)
  calls <- new.env()

  if (spark) {
    testthat::skip_if_not_installed("sparklyr")
    vctrs::s3_register("dplyr::collect", "prep_data_fake_sdf", function(x, ...) x$data)
    before <- ls(.GlobalEnv, all.names = TRUE)
    withr::defer(rm(list = setdiff(ls(.GlobalEnv, all.names = TRUE), before), envir = .GlobalEnv))
    assign("sc", structure(list(), class = "spark_connection"), envir = .GlobalEnv)
    testthat::local_mocked_bindings(
      check_parallel_processing = function(...) invisible(NULL),
      adjust_df = function(input_data, return_type = "df") input_data
    )
    testthat::local_mocked_bindings(spark_apply = prep_data_emulate_spark_apply(calls), .package = "sparklyr")
  }

  prep_data(
    run_info = run_info, input_data = data_tbl, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 3, recipes_to_run = "R1", box_cox = box_cox,
    stationary = stationary, clean_outliers = clean_outliers,
    parallel_processing = if (spark) "spark" else NULL
  )

  file <- list.files(file.path(path, "prep_data"), pattern = "-orig_combo_info", full.names = TRUE)
  info <- if (data_output == "parquet") as.data.frame(arrow::read_parquet(file)) else utils::read.csv(file)
  info <- info[order(info$Combo), , drop = FALSE]
  rownames(info) <- NULL
  structure(info, spark_columns = calls$columns)
}

test_that("Spark prep_data saves the same transformation table as the local path", {
  settings <- list(
    list(data_output = "csv", box_cox = TRUE, stationary = TRUE, clean_outliers = FALSE),
    list(data_output = "parquet", box_cox = FALSE, stationary = TRUE, clean_outliers = TRUE)
  )
  for (setting in settings) {
    label <- paste(names(setting), unlist(setting), sep = "=", collapse = ", ")
    local <- do.call(prep_data_orig_combo_info, c(list(spark = FALSE), setting))
    spark <- do.call(prep_data_orig_combo_info, c(list(spark = TRUE), setting))

    spark_columns <- attr(spark, "spark_columns")
    expect_identical(names(spark_columns), names(local), info = label)
    expect_identical(spark_columns$Combo, "character", info = label)
    expect_identical(names(spark), names(local), info = label)
    numeric_cols <- setdiff(names(local), c("Combo", "Combo_Hash"))
    as_numbers <- function(x) dplyr::mutate(x, dplyr::across(dplyr::all_of(numeric_cols), as.numeric))
    expect_equal(as_numbers(structure(spark, spark_columns = NULL)), as_numbers(local), info = label)
  }
})
