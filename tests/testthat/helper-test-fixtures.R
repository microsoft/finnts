read_unselected_forecast_data <- function(run_info) {
  splits <- get_prepped_models(run_info) %>%
    dplyr::filter(Type == "Train_Test_Splits") %>%
    dplyr::pull(Data) %>%
    .[[1]]
  forecast_path <- paste0(
    "/forecasts/*",
    hash_data(run_info$project_name), "-",
    hash_data(run_info$run_name),
    "*-single_models.", run_info$data_output
  )

  forecast <- finnts:::read_file(
    run_info,
    path = forecast_path,
    return_type = "df"
  ) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
    dplyr::left_join(
      splits %>% dplyr::select(Run_Type, Train_Test_ID),
      by = "Train_Test_ID"
    )

  list(forecast = forecast, splits = splits)
}

validate_summary_output <- function(summary_tbl, model_name) {
  testthat::expect_true(tibble::is_tibble(summary_tbl) || is.data.frame(summary_tbl))
  testthat::expect_true(nrow(summary_tbl) >= 1)

  expected_cols <- c("model_class", "engine", "section", "name", "value")
  testthat::expect_true(all(expected_cols %in% colnames(summary_tbl)), info = model_name)
  testthat::expect_true(is.character(summary_tbl$section), info = model_name)
  testthat::expect_true(is.character(summary_tbl$name), info = model_name)
  testthat::expect_true(is.character(summary_tbl$value), info = model_name)
  testthat::expect_false(anyNA(summary_tbl$section), info = model_name)
  testthat::expect_false(anyNA(summary_tbl$name), info = model_name)

  valid_sections <- c(
    "predictor", "outcome", "recipe_step", "model_arg",
    "engine_param", "coefficient", "importance", "xreg_coefficient"
  )
  testthat::expect_true(all(summary_tbl$section %in% valid_sections), info = model_name)
  testthat::expect_true("outcome" %in% summary_tbl$section, info = model_name)
}

expect_importance_output <- function(summary_tbl, model_name) {
  testthat::skip_if_not_installed("vip", minimum_version = "0.5.0")

  importance <- summary_tbl %>%
    dplyr::filter(section == "importance")
  importance_values <- suppressWarnings(as.numeric(importance$value))

  testthat::expect_true(nrow(importance) > 0, info = model_name)
  testthat::expect_true(all(nzchar(importance$name)), info = model_name)
  testthat::expect_true(
    length(importance_values) > 0 && all(is.finite(importance_values)),
    info = model_name
  )
}

get_model_workflow <- function(trained_tbl, model_name) {
  row <- trained_tbl %>%
    dplyr::filter(Model_Name == model_name) %>%
    dplyr::slice(1)
  if (nrow(row) == 0) {
    stop(paste0("No trained model found with name '", model_name, "'"))
  }
  row$Model_Fit[[1]]
}