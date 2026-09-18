test_that("Scott standard rules preserve a hand-calculated seasonal blend", {
  result <- acr_scott_growth(
    profile = "standard", mom = c(0.08, 0.10, 0.12),
    quality = c(1.8, 2.2, 3), bases = c(60, 70, 80), last_rate = 120,
    median_month = 0.03, sd_month = 0.1, median_all = 0.03,
    sd_all = 0.1, recent = 0.04, previous_median = 0.03
  )
  expect_identical(result$rule, "standard_2")
  expect_equal(result$growth, (0.6 * 0.08 + 0.4 * 0.03) * (1.06 * 80 / 120))
})

test_that("missing history is not selected as reliable zero growth", {
  result <- acr_scott_growth(
    profile = "standard", mom = c(NA, 0.08, 0.12),
    quality = c(NA, 0.5, 3), bases = c(NA, 70, 80), last_rate = 120,
    median_month = 0.03, sd_month = 0.1, median_all = 0.03,
    sd_all = 0.1, recent = 0.04, previous_median = 0.03
  )
  expect_equal(result$analogue, 2L)
  expect_equal(result$growth, (0.6 * 0.08 + 0.4 * 0.03) * (1.06 * 80 / 120))
})

test_that("high-growth missing-history rule and calendar policies are explicit", {
  result <- acr_scott_growth(
    profile = "high_growth", mom = rep(NA_real_, 3),
    quality = rep(NA_real_, 3), bases = rep(NA_real_, 3), last_rate = 120,
    median_month = 0.03, sd_month = 0.1, median_all = 0.03,
    sd_all = 0.1, recent = 0.04, previous_median = 0.03
  )
  expect_identical(result$rule, "high_growth_1")
  expect_equal(result$growth, (0.6 * 0.04 + 0.4 * 0.03) * 0.78)
  expect_equal(acr_scott_calendar(-0.085, 7, "scott_main"), -0.1)
  expect_equal(acr_scott_calendar(0.1, 12, "scott_main"), 0.085)
  expect_equal(acr_scott_calendar(-0.085, 12, "scott_pricing"), -0.1)
  expect_equal(acr_scott_calendar(0.1, 12, "none"), 0.1)
})

test_that("reference trends extrapolate after the actual training window", {
  reference <- acr_scott_reference_fit(0.03 + 0.01 * log(seq_len(12)), 0.02)
  expect_s3_class(reference$model, "lm")
  expect_equal(acr_scott_reference_predict(reference, 1:12),
               0.03 + 0.01 * log(13:24), tolerance = 1e-12)
  fallback <- acr_scott_reference_fit(c(NA, rep(0.02, 11)), 0.04)
  expect_null(fallback$model)
  expect_equal(acr_scott_reference_predict(fallback, 1:12), rep(0.04, 12))
})

test_that("zero dispersion and calendar month keys are well-defined", {
  expect_equal(acr_scott_quality(c(0.03, 0.04, 0.02, NA), 0.03, 0),
               c(0, Inf, -Inf, NA))
  expect_equal(diff(acr_scott_month(as.Date(c("2024-12-01", "2025-01-01")))), 1)
})

test_that("fitted panels pool references and preserve prediction identity", {
  data <- acr_scott_fixture()
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  fit <- acr_scott_custom_model_fit_impl(data[setdiff(names(data), "Target")], data$Target, options)
  expect_s3_class(fit, "acr_scott_custom_fit")
  expect_length(fit$pools, 2L)
  expect_length(fit$series, 6L)
  future <- acr_scott_future_fixture(data)
  prediction <- acr_scott_custom_model_predict_impl(fit, future)
  expect_length(prediction, nrow(future))
  expect_true(all(is.finite(prediction) & prediction >= 0))
  before <- serialize(fit, NULL)
  reverse <- rev(seq_len(nrow(future)))
  expect_equal(acr_scott_custom_model_predict_impl(fit, future[reverse, ]), prediction[reverse])
  expect_identical(serialize(fit, NULL), before)
  future$Target <- 1e12
  expect_identical(acr_scott_custom_model_predict_impl(fit, future), prediction)
  model_path <- tempfile(fileext = ".rds")
  saveRDS(fit, model_path)
  expect_identical(acr_scott_custom_model_predict_impl(readRDS(model_path), future), prediction)
  explanation <- explain_acr_scott_custom(fit, future)
  expect_equal(explanation$Forecast, prediction)
  expect_identical(explanation$Combo, future$Combo)
  expect_true(all(c("Rule", "Pool", "RecentGrowth", "GrowthBeforeCap", "GrowthAfterCap") %in% names(explanation)))
})

test_that("unrelated peers and future outcomes do not change a group's forecasts", {
  data <- acr_scott_fixture()
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  fit <- acr_scott_custom_model_fit_impl(data, data$Target, options)
  changed <- data
  changed$Target[changed$Product == "B"] <- changed$Target[changed$Product == "B"] * 10
  other <- acr_scott_custom_model_fit_impl(changed, changed$Target, options)
  future <- subset(acr_scott_future_fixture(data), Product == "A")
  expect_identical(acr_scott_custom_model_predict_impl(fit, future),
                   acr_scott_custom_model_predict_impl(other, future))
})

test_that("panel validation rejects gaps, negatives, unknown combos and sparse pools", {
  data <- acr_scott_fixture()
  options <- list(pool_by = "Product", target_mode = "daily_rate")
  expect_error(acr_scott_custom_model_fit_impl(data[-20, ], data$Target[-20], options), "consecutive")
  negative <- data$Target
  negative[1] <- -1
  expect_error(acr_scott_custom_model_fit_impl(data, negative, options), "nonnegative")
  expect_error(acr_scott_custom_model_fit_impl(data, data$Target, list(pool_by = "Missing")), "pool_by")
  sparse <- subset(data, Market != "M3")
  expect_error(acr_scott_custom_model_fit_impl(sparse, sparse$Target, options), "Insufficient peer")
  options$insufficient_pool <- "series"
  fit <- acr_scott_custom_model_fit_impl(sparse, sparse$Target, options)
  expect_true(any(vapply(fit$series, function(series) series$pool_fallback == "series", logical(1))))
  future <- acr_scott_future_fixture(sparse)
  future$Combo[1] <- "unseen"
  expect_error(acr_scott_custom_model_predict_impl(fit, future), "Unknown")
})

test_that("calendar exposure and all-zero series have consistent units", {
  data <- acr_scott_fixture(61L)
  data$Target <- 0
  options <- list(pool_by = "Product")
  fit <- acr_scott_custom_model_fit_impl(data, data$Target, options)
  future <- acr_scott_future_fixture(data, 2L)
  result <- explain_acr_scott_custom(fit, future)
  expect_true(all(result$Forecast == 0))
  expect_true(all(result$Exposure[format(result$Date, "%m") == "02"] == 29))
})

test_that("parsnip workflow preserves structural predictors and reloads", {
  data <- acr_scott_fixture()
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  specification <- acr_scott_custom_model(options = options) %>%
    parsnip::set_engine("acr_scott_custom")
  expect_identical(rlang::quo_get_env(specification$args$options), baseenv())
  workflow <- workflows::workflow() %>%
    workflows::add_model(specification) %>%
    workflows::add_recipe(recipes::recipe(Target ~ Date + Combo + Product, data = data))
  fitted <- generics::fit(workflow, data)
  future <- acr_scott_future_fixture(data)
  prediction <- predict(fitted, future)
  expect_equal(prediction$.pred, explain_acr_scott_custom(fitted, future)$Forecast)
  expect_s3_class(workflows::extract_fit_parsnip(fitted)$fit, "acr_scott_custom_fit")
  restored <- unserialize(serialize(fitted, NULL))
  expect_identical(predict(restored, future), prediction)
  updated <- stats::update(specification, options = list(pool_by = "Product", min_pool_series = 2L))
  expect_equal(rlang::eval_tidy(updated$args$options)$min_pool_series, 2L)
  expect_identical(rlang::quo_get_env(updated$args$options), baseenv())
})

test_that("ordered pooling fallbacks and six-growth window are explicit", {
  data <- acr_scott_fixture()
  options <- list(pool_by = c("Product", "Market"), pool_fallbacks = list("Product"),
                  target_mode = "daily_rate", calendar_policy = "none")
  fitted <- acr_scott_custom_model_fit_impl(data, data$Target, options)
  expect_true(all(vapply(fitted$series, function(series) series$pool_fallback == "level_2", logical(1))))
  future <- acr_scott_future_fixture(data, 1L)
  explained <- explain_acr_scott_custom(fitted, future)
  own <- fitted$series[[future$Combo[1]]]$history
  expect_equal(explained$RecentGrowth[1], stats::median(tail(own$growth, 6)))
  expect_error(acr_scott_options(list(pool_by = "Product", pool_fallbacks = list("Market"))), "coarser")
})

test_that("pool options round-trip with ordered levels and arbitrary dimensions", {
  options <- acr_scott_options(list(pool_by = c("Market", "Product"),
                                    pool_fallbacks = list("Product")))
  log <- tibble::tibble(acr_scott_custom_options = as.character(jsonlite::toJSON(
    options, auto_unbox = TRUE, null = "null")))
  expect_identical(acr_scott_log_options(log), options)
  data <- acr_scott_fixture()
  names(data)[names(data) == "Product"] <- "Service Category"
  fit <- acr_scott_custom_model_fit_impl(data, data$Target,
    list(pool_by = "Service Category", target_mode = "daily_rate"))
  expect_length(fit$pools, 2L)
})

test_that("supplied exposure preserves unit scaling and rejects missing intervening months", {
  data <- acr_scott_fixture()
  data$AdjustedDays <- 20
  options <- list(pool_by = "Product", exposure = "AdjustedDays", calendar_policy = "none")
  fitted <- acr_scott_custom_model_fit_impl(data, data$Target * 20, options)
  rate_fit <- acr_scott_custom_model_fit_impl(data, data$Target,
    list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none"))
  future <- acr_scott_future_fixture(data)
  future$AdjustedDays <- 25
  explanation <- explain_acr_scott_custom(fitted, future)
  expect_equal(explanation$Forecast, acr_scott_custom_model_predict_impl(rate_fit, future) * 25)
  expect_true(all(c("Weights", "Damping", "OldestGrowth", "MiddleGrowth", "LatestGrowth",
                    "OldestQuality", "MiddleQuality", "LatestQuality") %in% names(explanation)))
  expect_true(all(nzchar(explanation$Weights)))
  expect_error(explain_acr_scott_custom(fitted, future[future$Date != min(future$Date), ]), "intervening")
  future$AdjustedDays[1] <- 0
  expect_error(explain_acr_scott_custom(fitted, future), "positive")
})

test_that("leading unavailable rows are not fitted as actual zero observations", {
  data <- acr_scott_fixture()
  data$.acr_scott_observed <- 1L
  early <- data$Combo == "series-1" & data$Date < as.Date("2019-07-01")
  data$.acr_scott_observed[early] <- 0L
  data$Target[early] <- 0
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  with_padding <- acr_scott_custom_model_fit_impl(data, data$Target, options)
  without_padding <- acr_scott_custom_model_fit_impl(data[!early, ], data$Target[!early], options)
  future <- acr_scott_future_fixture(data)
  expect_identical(acr_scott_custom_model_predict_impl(with_padding, future),
                   acr_scott_custom_model_predict_impl(without_padding, future))
})

test_that("future outcomes and peer changes are excluded from an earlier fold fit", {
  data <- acr_scott_fixture()
  split_table <- data.frame(Train_End = as.Date("2022-12-01"),
                             Test_End = as.Date("2023-03-01"), Train_Test_ID = 1)
  changed <- data
  changed$Target[changed$Date > split_table$Train_End] <- 1e12
  analysis <- rsample::analysis(create_splits(data, split_table)$splits[[1]])
  analysis_changed <- rsample::analysis(create_splits(changed, split_table)$splits[[1]])
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  first <- acr_scott_custom_model_fit_impl(analysis, analysis$Target, options)
  second <- acr_scott_custom_model_fit_impl(analysis_changed, analysis_changed$Target, options)
  future <- acr_scott_future_fixture(analysis)
  expect_identical(acr_scott_custom_model_predict_impl(first, future),
                   acr_scott_custom_model_predict_impl(second, future))
  expect_equal(first$cutoff, acr_scott_month(split_table$Train_End))
})

test_that("fit rejects unsafe identifier columns and overflowing rates", {
  data <- acr_scott_fixture()
  data$Product[1] <- ""
  expect_error(acr_scott_custom_model_fit_impl(data, data$Target,
                 list(pool_by = "Product")), "blank")
  data <- acr_scott_fixture()
  expect_error(acr_scott_options(list(pool_by = "Target"), c("Target", "Market")), "reserved")
  data$Days <- .Machine$double.xmin
  expect_error(acr_scott_custom_model_fit_impl(data, rep(.Machine$double.xmax, nrow(data)),
                 list(pool_by = "Product", exposure = "Days")), "finite.*rate|rate.*finite")
})

test_that("R1 CSV metadata retains numeric-looking character peer identifiers", {
  dates <- as.Date(c("2024-01-01", "2024-02-01"))
  metadata <- data.frame(Combo = "001--2", Date = dates, Product = "001",
                         Market = 2L, .acr_scott_observed = 1L)
  options <- acr_scott_options(list(pool_by = "Product"))
  data <- data.frame(Combo = "001--2", Date = dates, Target = 100)
  attached <- acr_scott_attach_metadata(data, metadata, "001--2", c("Product", "Market"), options)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(attached, path, row.names = FALSE)
  stored <- utils::read.csv(path, check.names = FALSE)
  restored <- acr_scott_restore_metadata(stored, options)
  expect_identical(restored$Product, rep("001", 2))
  expect_identical(restored$Market, rep(2L, 2))
})

test_that("manual validation metrics keep zero denominators undefined", {
  input <- data.frame(Model = "custom", Combo = c("A", "A"),
                       Date = as.Date(c("2024-01-01", "2024-02-01")),
                       Actual = c(100, 200), Forecast = c(110, 180))
  result <- acr_scott_validation_metrics(input, c("Model", "Combo"))
  expect_equal(result$WMAPE, 0.1)
  expect_equal(result$MAE, 15)
  expect_equal(result$Bias, -5)
  input$Actual <- 0
  expect_true(is.na(acr_scott_validation_metrics(input, "Model")$WMAPE))
})

test_that("month-end labels and forecast subsets retain the same monthly path", {
  data <- acr_scott_fixture()
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  fit <- acr_scott_custom_model_fit_impl(data, data$Target, options)
  future <- acr_scott_future_fixture(data)
  prediction <- acr_scott_custom_model_predict_impl(fit, future)
  future$Date <- as.Date(lubridate::ceiling_date(future$Date, "month") - 1)
  expect_equal(acr_scott_custom_model_predict_impl(fit, future), prediction)
  selected <- c(2L, 7L, 13L)
  expect_equal(acr_scott_custom_model_predict_impl(fit, future[selected, ]), prediction[selected])
})

test_that("missing damping bases use the documented standard median fallback", {
  result <- acr_scott_growth(profile = "standard", mom = c(0.1, NA, NA),
    quality = c(1.8, NA, NA), bases = c(50, NA, NA), last_rate = 100,
    median_month = 0.04, sd_month = 0.1, median_all = 0.03,
    sd_all = 0.1, recent = 0.04, previous_median = 0.03)
  expect_identical(result$rule, "standard_11")
  expect_equal(result$growth, 0.03)
})

test_that("profile and exposure source columns cannot silently change semantics", {
  data <- acr_scott_fixture()
  data$Profile <- "standard"
  future <- acr_scott_future_fixture(data)
  future$Target <- NA_real_
  future$Profile <- "high_growth"
  source <- rbind(data, future[names(data)])
  expect_error(acr_scott_prepare(source, c("Product", "Market"), "Target",
    list(pool_by = "Product", profile_column = "Profile", target_mode = "daily_rate"),
    min(data$Date), max(data$Date), 3, "month", FALSE, FALSE, FALSE, FALSE,
    "bottoms_up", NULL, "R1"), "profile|Profile")
  data$Revenue <- data$Target
  data$Target <- NULL
  expect_error(acr_scott_prepare(data, c("Product", "Market"), "Revenue",
    list(pool_by = "Product", exposure = "Revenue"), min(data$Date), max(data$Date),
    3, "month", FALSE, FALSE, FALSE, FALSE, "bottoms_up", NULL, "R1"), "target")
})