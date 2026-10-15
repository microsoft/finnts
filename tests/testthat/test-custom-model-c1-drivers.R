test_that("C1 retains independent drivers while R recipes use only selected xregs", {
  info <- set_run_info("c1-drivers", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  args <- custom_driver_prep(info, custom_driver_data(2L))
  do.call(prep_data, args)
  path <- function(recipe) finnts:::local_artifact_path(info, "prep_data", paste0("-", recipe),
    finnts:::hash_data("series1"), extension = "csv")
  c1 <- finnts:::read_exact_artifact(info, path("C1"))
  expect_true(all(c("Driver", "Other", "Builtin") %in% names(c1)))
  expect_equal(c1$Driver, 11:36)
  for (recipe in c("R1", "R2")) {
    built_in <- finnts:::read_exact_artifact(info, path(recipe))
    expect_false(any(c("Driver", "Other") %in% names(built_in)))
    expect_true("Builtin" %in% names(built_in))
  }
  models <- list(driver_rule = custom_driver_model(),
    other_rule = custom_driver_model("other_rule", "Other"))
  prep_models(info, models_to_run = c(names(models), "meanf"), custom_models = models,
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  workflows <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info,
    "prep_models", "-model_workflows", extension = "rds"), return_type = "object")
  for (name in names(models)) {
    rows <- workflows[workflows$Model_Name == name, ]
    for (workflow in rows$Model_Workflow) {
      recipe <- workflows::extract_recipe(workflow, estimated = FALSE)
      expect_setequal(recipe$var_info$variable,
        c("Target", "Date", "Combo", models[[name]]$definition$requirements$predictors))
    }
  }
  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  for (suffix in c("-single_models", "-global_models")) {
    forecasts <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info,
      "forecasts", suffix, finnts:::hash_data("series1"), extension = "csv"))
    custom <- forecasts[forecasts$Model_Name == "driver_rule" & forecasts$Train_Test_ID == 1, ]
    expect_equal(custom$Forecast, 2 * (35:36))
  }
  expect_no_error(do.call(prep_data, args))
  args$input_data$Driver[[1]] <- 999
  expect_error(do.call(prep_data, args), "C1.*(changed|source)|custom.*(changed|source)")
})

test_that("Spark preparation executes C1 with independent columns and backend-neutral identity", {
  info <- set_run_info("c1-spark-drivers", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  args <- custom_driver_prep(info)
  args$parallel_processing <- "spark"
  args$recipes_to_run <- c("R1", "C1")
  local_mocked_bindings(check_parallel_processing = function(...) invisible(NULL),
    adjust_df = function(df, ...) df, .package = "finnts")
  contexts <- list()
  # Execute the production callback on each genuine panel. Restore its global
  # context bindings at test exit; no Spark cluster or storage emulation is used.
  local_mocked_bindings(spark_apply = function(x, f, group_by, context, ...) {
    contexts[[length(contexts) + 1L]] <<- context
    if ("hash_panel" %in% names(context)) expect_identical(environment(f), baseenv())
    do.call(rlang::local_bindings, c(context,
      list(.env = .GlobalEnv, .frame = environment())))
    panels <- split(as.data.frame(x), x$Combo)
    dplyr::bind_rows(lapply(panels, function(panel) f(panel, context)))
  }, .package = "sparklyr")
  do.call(prep_data, args)
  c1 <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info,
    "prep_data", "-C1", finnts:::hash_data("series1"), extension = "csv"))
  expect_equal(c1$Driver, 11:36)
  r1 <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info,
    "prep_data", "-R1", finnts:::hash_data("series1"), extension = "csv"))
  expect_false("Driver" %in% names(r1))
  expect_true("Builtin" %in% names(r1))
  expect_true(all(c("Driver", "Other") %in% contexts[[1]]$c1_columns))
  expect_identical(contexts[[1]]$custom_source_join, finnts:::custom_source_join)
  expect_false(any(c("custom_models", "custom_agent_contract") %in% names(contexts[[1]])))
  panel <- data.frame(Combo = "series1", Date = args$input_data$Date,
    Target = args$input_data$value, Driver = args$input_data$Driver)
  spark_panel <- panel
  class(spark_panel) <- c("tbl_spark", class(panel))
  expect_identical(finnts:::custom_source_identity(spark_panel),
    finnts:::custom_source_identity(panel[nrow(panel):1, rev(names(panel))]))
  expect_identical(environment(contexts[[2]]$hash_panel), baseenv())
})

test_that("partial C1 preparation pins its source before any recipe is written", {
  info <- set_run_info("c1-partial", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  args <- custom_driver_prep(info)
  original <- finnts:::write_data
  local_mocked_bindings(write_data = function(x, combo, run_info, output_type, folder, ...) {
    if (identical(folder, "prep_data")) stop("Interrupted recipe write")
    original(x, combo, run_info, output_type, folder, ...)
  }, .package = "finnts")
  expect_error(do.call(prep_data, args), "Interrupted recipe write")
  args$input_data$Driver[[1]] <- 999
  expect_error(do.call(prep_data, args), "C1.*source data changed")
})

test_that("C1 requires complete future drivers and excludes forbidden input columns", {
  info <- set_run_info("c1-driver-missing", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  data <- custom_driver_data()
  data$Driver[[26]] <- NA_real_
  data$Train_Test_ID <- 9
  data$Target_Original <- 999
  data$unsupported <- I(rep(list(1:2), nrow(data)))
  do.call(prep_data, custom_driver_prep(info, data, NULL))
  c1 <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info,
    "prep_data", "-C1", finnts:::hash_data("series1"), extension = "csv"))
  expect_false(any(c("Train_Test_ID", "Target_Original", "unsupported") %in% names(c1)))
  expect_true(is.na(c1$Driver[[26]]))
  imputed <- finnts:::custom_view_build(c1, "Driver", character(), TRUE,
    "month", as.Date("2021-12-01"))
  expect_true(is.na(imputed$Driver[[26]]))
  model <- custom_driver_model()
  expect_error(prep_models(info, models_to_run = "driver_rule",
    custom_models = list(driver_rule = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1), "driver_rule.*Driver.*series1.*2022-02-01")
})
