# The global undifferencing that train_models() and fit_models() ran before
# undifference_global_forecast(): group_split() by Combo, then one call of the
# original per-split loop per series with that series' recipe and combo rows.
# Inputs and output match undifference_global_forecast().
reference_global_undifference <- function(final_fcst, prep_data, combo_info) {
  final_fcst %>%
    dplyr::group_by(Combo) %>%
    dplyr::group_split() %>%
    purrr::map(function(x) {
      combo <- unique(x$Combo)
      undifference_forecast_loop(
        x,
        prep_data %>% dplyr::filter(Combo == combo),
        combo_info %>% dplyr::filter(Combo == combo)
      )
    }) %>%
    dplyr::bind_rows()
}

# Several differenced series in one global table, in train_models() shapes.
# Combo names mix case and punctuation so group order follows dplyr's C-locale
# sort; one series is not differenced. Rows are interleaved across series.
# Returns list(forecast, recipe, combo_info).
global_undifference_case <- function() {
  combos <- c("b", "B", "a_1", "A--x", "c")
  orders <- c(1, 2, 0, 1, 2)
  index <- seq_len(36)
  parts <- lapply(seq_along(combos), function(i) {
    actuals <- 50 * i + 2 * index + 0.1 * i * index^2 + 6 * sin(index / i)
    cleaned <- actuals
    cleaned[5 + i] <- cleaned[5 + i] + 20
    fixture <- make_selection_history_recipe(actuals, cleaned, orders[[i]])
    recipe <- fixture$data
    recipe$Combo <- combos[[i]]
    recipe$Target_lag3 <- dplyr::lag(recipe$Target, 3)
    fixture$combo_info$Combo <- combos[[i]]
    dates <- recipe$Date
    forecast <- tibble::tibble(
      Train_Test_ID = rep(c(1, 2, 3), each = 3),
      Forecast = i + c(37:39, 34:36, 31:33) / 10,
      Target = c(rep(NA_real_, 3), i + c(1.5, -2, 4, 0.5, 2, NA)),
      Combo = combos[[i]],
      Date = dates[c(37:39, 34:36, 31:33)],
      Hyperparameter_ID = 1
    )
    list(forecast = forecast, recipe = recipe, combo_info = fixture$combo_info)
  })
  forecast <- dplyr::bind_rows(lapply(parts, `[[`, "forecast"))
  list(
    forecast = forecast[order(rep_len(1:4, nrow(forecast)), seq_len(nrow(forecast))), ],
    recipe = dplyr::bind_rows(lapply(rev(parts), `[[`, "recipe")),
    combo_info = dplyr::bind_rows(lapply(parts[c(3, 1, 5, 2, 4)], `[[`, "combo_info"))
  )
}

test_that("global undifferencing matches the per-series group_split loop", {
  case <- global_undifference_case()
  expected <- reference_global_undifference(case$forecast, case$recipe, case$combo_info)
  expect_identical(
    undifference_global_forecast(case$forecast, case$recipe, case$combo_info),
    expected
  )
  expect_identical(unique(expected$Combo), c("A--x", "B", "a_1", "b", "c"))
})

# One monthly series with an lm workflow that has no tuning parameters, plus
# three validation splits ending at months 24, 27 and 30. Input: 36 targets.
# Returns list(data, splits, workflow).
untuned_case <- function(target) {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 36)
  data <- tibble::tibble(Combo = "A", Date = dates, x = seq_along(dates), Target = target)
  list(
    data = data,
    splits = tibble::tibble(
      Run_Type = "Validation", Train_Test_ID = c(4, 5, 6),
      Train_End = dates[c(24, 27, 30)], Test_End = dates[c(27, 30, 33)]
    ),
    workflow = workflows::workflow() %>%
      workflows::add_recipe(recipes::recipe(Target ~ x, data = data)) %>%
      workflows::add_model(parsnip::linear_reg() %>% parsnip::set_engine("lm"))
  )
}

test_that("untuned validation check stops at the first split that fits", {
  fit_resamples <- tune::fit_resamples
  calls <- 0
  testthat::local_mocked_bindings(fit_resamples = function(...) {
    calls <<- calls + 1
    fit_resamples(...)
  }, .package = "tune")

  case <- untuned_case(100 + seq_len(36))
  expect_no_error(check_untuned_validation(case$workflow, case$data, case$splits, "lm", "finnts"))
  expect_equal(calls, 1)

  # the first split's training rows have no target, so only the second fits
  target <- 100 + seq_len(36)
  target[1:24] <- NA
  case <- untuned_case(target)
  calls <- 0
  expect_no_error(check_untuned_validation(case$workflow, case$data, case$splits, "lm", "finnts"))
  expect_equal(calls, 2)
})

test_that("untuned validation failure reports the reason tune_grid() reported", {
  case <- untuned_case(rep(NA_real_, 36))
  grid <- tibble::tibble(Hyperparameter_Combo = 1, Hyperparameters = list(tibble::tibble())) %>%
    tidyr::unnest(Hyperparameters) %>%
    dplyr::select(-Hyperparameter_Combo)
  expected <- tryCatch(
    select_best_rmse(
      tune::tune_grid(case$workflow, resamples = create_splits(case$data, case$splits), grid = grid) %>%
        suppressMessages() %>%
        suppressWarnings(),
      "lm"
    ),
    error = conditionMessage
  )
  expect_match(expected, "Model 'lm' failed on every validation split: ", fixed = TRUE)
  expect_error(
    check_untuned_validation(case$workflow, case$data, case$splits, "lm", "finnts"),
    expected,
    fixed = TRUE
  )
})

test_that("only multistep xgboost workflows are marked as discarded fits", {
  data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    Target = as.numeric(1:12), Target_lag1 = as.numeric(0:11)
  )
  recipe <- recipes::recipe(Target ~ ., data = data)
  multistep <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(
      xgboost_multistep(trees = 5, lag_periods = 1, forecast_horizon = 1) %>%
        parsnip::set_engine("xgboost_multistep_horizon")
    )
  flagged <- discarded_fit_workflow(multistep)
  expect_true(rlang::eval_tidy(workflows::extract_spec_parsnip(flagged)$eng_args$finnts_discarded_fit))
  expect_identical(workflows::extract_preprocessor(flagged), recipe)
  expect_null(workflows::extract_spec_parsnip(multistep)$eng_args$finnts_discarded_fit)

  standard <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(parsnip::boost_tree(trees = 5) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression"))
  expect_identical(discarded_fit_workflow(standard), standard)
})

# Prepare one stationary, outlier-cleaned multistep run of two monthly series
# once, then copy its logs and prepared artifacts into an isolated run root so
# every training arm starts from identical inputs. Models: multistep xgboost
# (two tuned candidates, also global) and snaive (no parameters).
# Input: destination path. Returns run info pointing there.
speedup_run <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    if (is.null(cache$info)) {
      info <- set_run_info(
        project_name = "train-speedups", run_name = "test",
        path = withr::local_tempdir(.local_envir = cache),
        data_output = "csv", object_output = "rds", add_unique_id = FALSE
      )
      index <- seq_len(36)
      series_a <- 200 + 4 * index + 0.15 * index^2 + 12 * sin(2 * pi * index / 12)
      series_a[15] <- series_a[15] + 120
      series_b <- 500 - 3 * index + 20 * cos(2 * pi * index / 12) + (index %% 5) * 4
      prep_data(info,
        data.frame(
          Date = rep(seq(as.Date("2018-01-01"), by = "month", length.out = 36), 2),
          id = rep(c("A", "B"), each = 36), value = c(series_a, series_b)
        ),
        combo_variables = "id", target_variable = "value", date_type = "month",
        forecast_horizon = 2, recipes_to_run = "R1", multistep_horizon = TRUE,
        stationary = TRUE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = TRUE
      )
      prep_models(info,
        models_to_run = c("xgboost", "snaive"), run_ensemble_models = FALSE,
        back_test_scenarios = 1, back_test_spacing = 1, num_hyperparameters = 2, pca = FALSE, seed = 123
      )
      cache$info <- info
    }
    info <- cache$info
    info$path <- path
    for (folder in c("logs", "prep_data", "prep_models")) {
      fs::dir_copy(fs::path(cache$info$path, folder), fs::path(path, folder))
    }
    fs::dir_create(fs::path(path, c("models", "forecasts")))
    info
  }
})

# Train a copied speed-up run with local and global models, then refit series A
# with retuning as forecast updates do. With original = TRUE, mocks restore the
# pre-speed-up paths: full tune_grid() for models without parameters, no
# discarded-fit flag, tune's default metrics and the original undifferencing
# loops. Returns the saved forecasts (sorted, read attributes removed), saved
# and refit model tables, the combo info, and how often the evaluation-free
# trainer and untuned validation check ran.
speedup_outputs <- function(original) {
  info <- speedup_run(withr::local_tempdir())
  used <- c(trainer = 0, untuned = 0)
  if (original) {
    testthat::local_mocked_bindings(
      has_tuning_parameters = function(workflow) TRUE,
      discarded_fit_workflow = function(workflow) workflow,
      model_tuning_metrics = function() NULL,
      undifference_forecast = function(...) undifference_forecast_loop(...),
      undifference_global_forecast = reference_global_undifference
    )
  } else {
    trainer <- xgboost_discarded_impl()
    check <- check_untuned_validation
    testthat::local_mocked_bindings(
      xgboost_discarded_impl = function() {
        used[["trainer"]] <<- used[["trainer"]] + 1
        trainer
      },
      check_untuned_validation = function(...) {
        used[["untuned"]] <<- used[["untuned"]] + 1
        check(...)
      }
    )
  }

  train_models(info, run_global_models = TRUE, run_local_models = TRUE, seed = 123)
  forecast <- read_file(info,
    path = paste0(
      "/forecasts/*", hash_data(info$project_name), "-", hash_data(info$run_name), "*_models.",
      info$data_output
    ),
    return_type = "df"
  ) %>%
    dplyr::arrange(Combo, Model_ID, Train_Test_ID, Date)
  attr(forecast, "problems") <- NULL

  models <- get_trained_models(info) %>% dplyr::arrange(Model_ID, Combo_ID)
  prepared <- get_prepped_models(info)
  splits <- prepared %>%
    dplyr::filter(Type == "Train_Test_Splits") %>%
    dplyr::select(Data) %>%
    tidyr::unnest(Data)
  parameters <- prepared %>%
    dplyr::filter(Type == "Model_Hyperparameters") %>%
    dplyr::select(Data) %>%
    tidyr::unnest(Data)
  combo_info <- read_file(info,
    file_list = paste0(
      info$path, "/prep_data/", hash_data(info$project_name), "-", hash_data(info$run_name),
      "-orig_combo_info.", info$data_output
    ) %>% fs::path_tidy(),
    return_type = "df"
  )
  log <- read_selection_file(info, "logs")
  refit <- fit_models(info, "A", combo_info %>% dplyr::filter(Combo == "A"),
    models %>% dplyr::filter(Combo_ID == "A"), splits, parameters, log, 2, TRUE, 1, FALSE) %>%
    dplyr::arrange(Model_Name)
  list(forecast = forecast, models = models, refit = refit, combo_info = combo_info, used = used)
}

# Serialized boosters of every multistep xgboost fit in a model table, so saved
# fits can be compared exactly across training arms.
saved_xgboost_boosters <- function(model_tbl) {
  lapply(model_tbl$Model_Fit[model_tbl$Model_Name == "xgboost"], function(fit) {
    lapply(workflows::extract_fit_engine(fit)$models, xgboost::xgb.save.raw)
  })
}

test_that("training speed-ups keep trained and refit outputs identical", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  original <- speedup_outputs(original = TRUE)
  current <- speedup_outputs(original = FALSE)

  expect_true(all(!is.na(current$combo_info$Diff_Value1)))
  expect_gt(current$used[["trainer"]], 0)
  expect_gt(current$used[["untuned"]], 0)
  expect_setequal(unique(current$models$Model_ID), c(
    "xgboost--local--R1", "snaive--local--R1", "xgboost--global--R1"
  ))

  expect_identical(current$forecast, original$forecast)
  expect_identical(current$models$Model_ID, original$models$Model_ID)
  expect_identical(saved_xgboost_boosters(current$models), saved_xgboost_boosters(original$models))
  for (fit in current$models$Model_Fit) {
    expect_null(workflows::extract_spec_parsnip(fit)$eng_args$finnts_discarded_fit)
  }

  expect_identical(
    current$refit[c("Combo_ID", "Model_Name", "Model_Type", "Recipe_ID", "Forecast_Tbl")],
    original$refit[c("Combo_ID", "Model_Name", "Model_Type", "Recipe_ID", "Forecast_Tbl")]
  )
  expect_identical(saved_xgboost_boosters(current$refit), saved_xgboost_boosters(original$refit))
})

test_that("installed inner-parallel discarded xgboost fits match sequential training", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  skip_if_fewer_workers(2)
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(
    file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Inner PSOCK workers are verified from the installed namespace"
  )
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))

  # Local models trained with or without inner workers; returns sorted forecasts.
  train_local <- function(inner_parallel) {
    info <- speedup_run(withr::local_tempdir())
    train_models(info, run_local_models = TRUE, run_global_models = FALSE,
      inner_parallel = inner_parallel, num_cores = 2, seed = 123)
    forecast <- read_unselected_forecast_data(info)$forecast
    forecast[order(forecast$Combo, forecast$Model_ID, forecast$Train_Test_ID, forecast$Date), ]
  }
  sequential <- train_local(inner_parallel = FALSE)
  parallel <- train_local(inner_parallel = TRUE)

  expect_true("xgboost--local--R1" %in% sequential$Model_ID)
  rownames(sequential) <- NULL
  rownames(parallel) <- NULL
  expect_equal(parallel, sequential)
})
