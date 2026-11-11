# Return dials parameter sets for the five ensemble model specs, finalized
# against a small feature table the way ensemble_models() finalizes them
# (svm-rbf is used unfinalized). Output: named list of parameter sets.
ensemble_grid_parameter_sets <- function() {
  features <- data.frame(a = 1:20, b = seq(0, 1, length.out = 20), c = (1:20)^2, Target = 1:20)
  specs <- list(
    cubist = parsnip::cubist_rules(committees = tune::tune(), neighbors = tune::tune(), max_rules = tune::tune()) %>%
      parsnip::set_engine("Cubist"),
    glmnet = parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
      parsnip::set_engine("glmnet"),
    `svm-poly` = parsnip::svm_poly(
      mode = "regression", cost = tune::tune(), degree = tune::tune(),
      scale_factor = tune::tune(), margin = tune::tune()
    ) %>% parsnip::set_engine("kernlab"),
    `svm-rbf` = parsnip::svm_rbf(mode = "regression", cost = tune::tune(), rbf_sigma = tune::tune(), margin = tune::tune()) %>%
      parsnip::set_engine("kernlab"),
    xgboost = parsnip::boost_tree(
      mode = "regression", trees = tune::tune(), tree_depth = tune::tune(), learn_rate = tune::tune(),
      loss_reduction = tune::tune(), mtry = tune::tune(), min_n = tune::tune()
    ) %>% parsnip::set_engine("xgboost")
  )
  lapply(stats::setNames(names(specs), names(specs)), function(model) {
    parameters <- workflows::extract_parameter_set_dials(specs[[model]])
    if (model == "svm-rbf") parameters else dials::finalize(parameters, features, force = FALSE)
  })
}

# Reference rolling-window features from the deprecated timetk implementation,
# with its purrr::cross_df() deprecation warning muffled. Inputs mirror
# augment_rolling_windows(); returns timetk's augmented data frame.
timetk_rolling_windows <- function(data, columns, periods, .f, names) {
  suppressWarnings(timetk::tk_augment_slidify(
    data, tidyselect::any_of(columns),
    .f = .f, .period = periods, .partial = TRUE, .align = "right", .names = names
  ))
}

# Name rolling-window columns in finnts' period-major order. Inputs: lag
# columns, periods and statistic suffix. Returns a character vector.
rolling_window_names <- function(columns, periods, statistic) {
  unlist(lapply(periods, function(period) paste0(columns, "_roll", period, "_", statistic)))
}

# Build a small multi-frequency prep fixture with missing targets, a negative
# regressor and an existing rolling column. Returns a data frame.
rolling_window_fixture <- function(by = "month", n = 30) {
  dates <- seq(as.Date("2018-01-01"), by = by, length.out = n)
  index <- seq_along(dates)
  data <- data.frame(
    Combo = "A", Date = dates, Target = 100 + index + 5 * sin(index),
    xreg1 = cos(index) * 10, Target_lag1 = c(NA, 100 + index[-n]),
    Target_lag3 = c(rep(NA, 3), 100 + index[seq_len(n - 3)])
  )
  data$Target[c(5, n - 1, n)] <- NA
  data$Target_lag1[c(4, 5)] <- NA
  data
}

test_that("ensemble grids match the deprecated latin hypercube for two or more points", {
  skip_if_not("grid_latin_hypercube" %in% getNamespaceExports("dials"), "dials no longer provides the reference grid")
  parameter_sets <- ensemble_grid_parameter_sets()
  for (model in names(parameter_sets)) {
    for (size in c(2, 5, 10)) {
      set.seed(123)
      expected <- suppressWarnings(dials::grid_latin_hypercube(parameter_sets[[model]], size = size))
      set.seed(123)
      actual <- rlang::with_options(
        lifecycle_verbosity = "error",
        ensemble_latin_hypercube_grid(parameter_sets[[model]], size)
      )
      expect_identical(actual, expected, label = paste(model, "grid of size", size))
    }
  }
})

test_that("single-point ensemble grids are reproducible and within parameter ranges", {
  parameters <- ensemble_grid_parameter_sets()$glmnet
  set.seed(123)
  first <- rlang::with_options(lifecycle_verbosity = "error", ensemble_latin_hypercube_grid(parameters, 1))
  set.seed(123)
  second <- ensemble_latin_hypercube_grid(parameters, 1)

  expect_identical(first, second)
  expect_equal(nrow(first), 1)
  expect_setequal(names(first), c("penalty", "mixture"))
  expect_true(first$mixture >= 0 && first$mixture <= 1)
  expect_true(first$penalty > 0)
})

test_that("rolling windows match timetk for every statistic, period and missing pattern", {
  skip_if_not(exists("cross_df", envir = asNamespace("purrr")), "purrr no longer provides the timetk reference")
  columns <- c("Target_lag1", "Target_lag3")
  periods <- c(3, 6, 12)
  statistics <- list(
    Avg = ~ mean(.x, na.rm = TRUE), Sum = ~ sum(.x, na.rm = TRUE), StdDev = ~ sd(.x, na.rm = TRUE)
  )

  for (data in list(rolling_window_fixture(), tibble::as_tibble(rolling_window_fixture("week", 10)))) {
    for (statistic in names(statistics)) {
      window_names <- rolling_window_names(columns, periods, statistic)
      expected <- timetk_rolling_windows(data, columns, periods, statistics[[statistic]], window_names)
      actual <- rlang::with_options(
        lifecycle_verbosity = "error",
        augment_rolling_windows(data, columns, periods, statistics[[statistic]], window_names)
      )
      expect_identical(actual, expected, label = paste(statistic, class(data)[1], nrow(data), "rows"))
    }
  }
})

test_that("rolling windows overwrite existing columns and ignore absent inputs like timetk", {
  skip_if_not(exists("cross_df", envir = asNamespace("purrr")), "purrr no longer provides the timetk reference")
  data <- rolling_window_fixture()
  data$Target_lag1_roll3_Avg <- -1
  columns <- c("Target_lag1", "Target_lag99")
  window_names <- rolling_window_names("Target_lag1", 3, "Avg")

  expected <- timetk_rolling_windows(data, columns, 3, ~ mean(.x, na.rm = TRUE), window_names)
  actual <- augment_rolling_windows(data, columns, 3, ~ mean(.x, na.rm = TRUE), window_names)

  expect_identical(actual, expected)
  expect_identical(colnames(actual), colnames(data))
})

test_that("R1 and R2 feature recipes are unchanged and free of deprecated timetk calls", {
  skip_if_not(exists("cross_df", envir = asNamespace("purrr")), "purrr no longer provides the timetk reference")
  cases <- list(
    list(by = "month", n = 30, date_type = "month", lags = c(1, 3), windows = c(3, 6), fourier = c(3, 6), horizon = 3),
    list(by = "week", n = 60, date_type = "week", lags = c(1, 4), windows = c(4, 8), fourier = c(4, 13), horizon = 4),
    list(by = "day", n = 60, date_type = "day", lags = c(7, 14), windows = c(7, 14), fourier = c(7, 30), horizon = 7)
  )

  for (case in cases) {
    data <- rolling_window_fixture(case$by, case$n)[, c("Combo", "Date", "Target", "xreg1")]
    hist_end <- data$Date[case$n - 2]
    run_recipes <- function() {
      list(
        R1 = multivariate_prep_recipe_1(data, "xreg1", NULL, case$fourier, case$lags, case$windows, hist_end, case$date_type),
        R2 = multivariate_prep_recipe_2(data, "xreg1", NULL, case$fourier, case$lags, case$windows, case$date_type, case$horizon, hist_end)
      )
    }
    expected <- testthat::with_mocked_bindings(run_recipes(), augment_rolling_windows = timetk_rolling_windows)
    actual <- rlang::with_options(lifecycle_verbosity = "error", run_recipes())

    expect_identical(actual$R1, expected$R1, label = paste(case$date_type, "R1"))
    expect_identical(actual$R2, expected$R2, label = paste(case$date_type, "R2"))
  }
})
