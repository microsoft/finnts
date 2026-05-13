# tests/testthat/test-best_models.R
#
# Verifies that final_models() does not let a model with incomplete back test
# fold coverage win Best_Model. Simulates the scenario where one model's HTTP
# call (e.g. a foundation model) silently fails on some folds: tune::fit_resamples
# drops those folds from the per-model output, and historically the surviving
# model could win Best_Model on a smaller, easier sample than its competitors.

# helpers tied to a fresh run_info for each test
locate_single_models_file <- function(run_info) {
  forecasts_dir <- file.path(run_info$path, "forecasts")
  run_hash <- hash_data(run_info$run_name)
  pattern <- paste0(
    "^",
    hash_data(run_info$project_name), "-",
    run_hash, "-.*-single_models\\.(csv|parquet)$"
  )
  files <- list.files(forecasts_dir, pattern = pattern, full.names = TRUE)
  if (length(files) != 1) {
    stop(
      "expected exactly 1 single_models file for run ", run_info$run_name,
      " but found ", length(files)
    )
  }
  files
}

read_fcst_file <- function(path) {
  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    arrow::read_parquet(path)
  } else {
    suppressMessages(vroom::vroom(path, show_col_types = FALSE))
  }
}

write_fcst_file <- function(x, path) {
  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    arrow::write_parquet(x, path)
  } else {
    vroom::vroom_write(x, path, delim = ",")
  }
}

train_two_models <- function() {
  data <- modeltime::m750 %>%
    dplyr::rename(Date = date) %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::filter(Date >= "2012-01-01")

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3
  )

  prep_models(
    run_info = run_info,
    models_to_run = c("meanf", "snaive"),
    back_test_scenarios = 4
  )

  train_models(
    run_info = run_info,
    run_global_models = FALSE
  )

  run_info
}

test_that("final_models excludes models with partial back test coverage from Best_Model selection", {
  skip_on_cran()

  run_info <- train_two_models()
  single_models_path <- locate_single_models_file(run_info)

  fcst_tbl <- read_fcst_file(single_models_path)

  # baseline sanity: both models present, both with full fold coverage
  baseline_fold_counts <- fcst_tbl %>%
    dplyr::group_by(Model_ID) %>%
    dplyr::summarise(folds = dplyr::n_distinct(Train_Test_ID), .groups = "drop")
  expect_equal(nrow(baseline_fold_counts), 2)
  expect_true(all(baseline_fold_counts$folds == max(baseline_fold_counts$folds)))

  # corrupt snaive by removing two of its back test folds (Train_Test_ID 2 and 3),
  # simulating silent fold drops from a flaky model. keep Train_Test_ID 1
  # (future forecast) and at least one back test fold.
  snaive_id <- baseline_fold_counts$Model_ID[grepl("snaive", baseline_fold_counts$Model_ID)]
  meanf_id <- baseline_fold_counts$Model_ID[grepl("meanf", baseline_fold_counts$Model_ID)]
  expect_length(snaive_id, 1)
  expect_length(meanf_id, 1)

  corrupted_tbl <- fcst_tbl %>%
    dplyr::filter(!(Model_ID == snaive_id & Train_Test_ID %in% c(2, 3)))

  corrupted_counts <- corrupted_tbl %>%
    dplyr::group_by(Model_ID) %>%
    dplyr::summarise(folds = dplyr::n_distinct(Train_Test_ID), .groups = "drop")
  expect_lt(
    corrupted_counts$folds[corrupted_counts$Model_ID == snaive_id],
    corrupted_counts$folds[corrupted_counts$Model_ID == meanf_id]
  )

  write_fcst_file(corrupted_tbl, single_models_path)

  # final_models should warn about snaive's incomplete coverage and pick meanf
  # as the only Best_Model candidate.
  expect_message(
    final_models(
      run_info = run_info,
      average_models = FALSE
    ),
    regexp = "snaive.*back test folds",
    fixed = FALSE
  )

  result_tbl <- read_fcst_file(single_models_path)
  expect_true("Best_Model" %in% colnames(result_tbl))

  best_model_ids <- result_tbl %>%
    dplyr::filter(Best_Model == "Yes") %>%
    dplyr::pull(Model_ID) %>%
    unique()

  expect_length(best_model_ids, 1)
  expect_equal(best_model_ids, meanf_id)

  # the partial model's rows are still in the output, just not flagged as best
  snaive_rows <- result_tbl %>% dplyr::filter(Model_ID == snaive_id)
  expect_gt(nrow(snaive_rows), 0)
  expect_true(all(snaive_rows$Best_Model == "No"))
})

test_that("final_models errors when no model has complete back test coverage", {
  skip_on_cran()

  run_info <- train_two_models()
  single_models_path <- locate_single_models_file(run_info)

  fcst_tbl <- read_fcst_file(single_models_path)

  # drop a specific back test fold (Train_Test_ID == 2) from every model so
  # no model has complete coverage. Train_Test_ID 1 (future) and the rest of
  # the back test folds remain, so the existing "missing future forecast"
  # guard does not fire.
  corrupted_tbl <- fcst_tbl %>%
    dplyr::filter(Train_Test_ID != 2)

  remaining_counts <- corrupted_tbl %>%
    dplyr::group_by(Model_ID) %>%
    dplyr::summarise(folds = dplyr::n_distinct(Train_Test_ID), .groups = "drop")
  expect_true(all(remaining_counts$folds < max(fcst_tbl$Train_Test_ID)))

  write_fcst_file(corrupted_tbl, single_models_path)

  expect_error(
    suppressMessages(
      final_models(
        run_info = run_info,
        average_models = FALSE
      )
    ),
    regexp = "no models produced complete back test coverage"
  )
})

test_that("Best_Model wMAPE is computed from complete-fold models only", {
  skip_on_cran()

  # helper: rolling weighted MAPE per Model_ID over given back test rows.
  # mirrors the formula used inside final_models() so the test asserts the
  # exact metric that drives selection.
  rolling_wmape <- function(df) {
    df %>%
      dplyr::filter(Train_Test_ID != 1) %>%
      dplyr::mutate(
        Target = ifelse(Target == 0, 0.1, Target),
        MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4)
      ) %>%
      dplyr::group_by(Model_ID) %>%
      dplyr::mutate(
        Combo_Total = sum(abs(Target), na.rm = TRUE),
        weighted_MAPE = (abs(Target) / Combo_Total) * MAPE
      ) %>%
      dplyr::summarise(
        Rolling_MAPE = sum(weighted_MAPE, na.rm = TRUE),
        .groups = "drop"
      )
  }

  run_info <- train_two_models()
  single_models_path <- locate_single_models_file(run_info)
  fcst_tbl <- read_fcst_file(single_models_path)

  model_ids <- unique(fcst_tbl$Model_ID)
  expect_length(model_ids, 2)

  # natural per-fold MAPE so we can pick the "saboteur": the model whose two
  # worst back test folds, if dropped, would push its rolling wMAPE below the
  # competitor's complete wMAPE. without the fix, that saboteur would win on
  # a smaller, easier sample.
  per_fold_mape <- fcst_tbl %>%
    dplyr::filter(Train_Test_ID != 1) %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target),
      MAPE = abs((Forecast - Target) / abs(Target))
    ) %>%
    dplyr::group_by(Model_ID, Train_Test_ID) %>%
    dplyr::summarise(fold_MAPE = mean(MAPE, na.rm = TRUE), .groups = "drop")

  natural_wmape <- rolling_wmape(fcst_tbl)

  # try each model as the partial one and pick the configuration that creates
  # the bug condition: partial.wMAPE_on_remaining < complete.wMAPE_natural.
  pick_saboteur <- function() {
    for (candidate in model_ids) {
      competitor <- setdiff(model_ids, candidate)
      worst_folds <- per_fold_mape %>%
        dplyr::filter(Model_ID == candidate) %>%
        dplyr::arrange(dplyr::desc(fold_MAPE)) %>%
        dplyr::slice(1:2) %>%
        dplyr::pull(Train_Test_ID)

      partial_tbl <- fcst_tbl %>%
        dplyr::filter(!(Model_ID == candidate & Train_Test_ID %in% worst_folds))
      partial_wmape <- rolling_wmape(partial_tbl) %>%
        dplyr::filter(Model_ID == candidate) %>%
        dplyr::pull(Rolling_MAPE)
      complete_wmape <- natural_wmape %>%
        dplyr::filter(Model_ID == competitor) %>%
        dplyr::pull(Rolling_MAPE)

      if (length(partial_wmape) == 1 && partial_wmape < complete_wmape) {
        return(list(
          partial_id = candidate,
          complete_id = competitor,
          dropped_folds = worst_folds,
          partial_wmape = partial_wmape,
          complete_wmape = complete_wmape
        ))
      }
    }
    NULL
  }

  saboteur <- pick_saboteur()
  skip_if(is.null(saboteur), "neither model creates the bug condition on this dataset")

  # sanity: the bug condition really exists -- partial would beat complete
  # on naive (non-fix) wMAPE comparison.
  expect_lt(saboteur$partial_wmape, saboteur$complete_wmape)

  corrupted_tbl <- fcst_tbl %>%
    dplyr::filter(
      !(Model_ID == saboteur$partial_id & Train_Test_ID %in% saboteur$dropped_folds)
    )
  write_fcst_file(corrupted_tbl, single_models_path)

  suppressMessages(
    final_models(run_info = run_info, average_models = FALSE)
  )

  result_tbl <- read_fcst_file(single_models_path)

  best_model_ids <- result_tbl %>%
    dplyr::filter(Best_Model == "Yes") %>%
    dplyr::pull(Model_ID) %>%
    unique()

  # the complete model wins despite having the WORSE wMAPE, because the
  # partial competitor was excluded from selection entirely.
  expect_length(best_model_ids, 1)
  expect_equal(best_model_ids, saboteur$complete_id)

  # double-check: rebuild rolling wMAPE from the post-run forecast file
  # (back test rows only) and confirm the winner's value equals its natural
  # complete-fold wMAPE -- i.e. selection metric was NOT contaminated by
  # the partial model's inflated-looking number.
  back_test_rows <- result_tbl %>%
    dplyr::filter(!is.na(Target)) %>%
    dplyr::filter(Model_ID %in% c(saboteur$complete_id, saboteur$partial_id))

  rebuilt_wmape <- back_test_rows %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target),
      MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4)
    ) %>%
    dplyr::group_by(Model_ID) %>%
    dplyr::mutate(
      Combo_Total = sum(abs(Target), na.rm = TRUE),
      weighted_MAPE = (abs(Target) / Combo_Total) * MAPE
    ) %>%
    dplyr::summarise(
      Rolling_MAPE = sum(weighted_MAPE, na.rm = TRUE),
      .groups = "drop"
    )

  winner_wmape <- rebuilt_wmape %>%
    dplyr::filter(Model_ID == saboteur$complete_id) %>%
    dplyr::pull(Rolling_MAPE)
  expect_equal(winner_wmape, saboteur$complete_wmape, tolerance = 1e-3)
})
