test_that("installed custom training matches sequential execution on two PSOCK workers", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")), "Actual custom training workers are verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  sequential <- standard_run(withr::local_tempdir())
  parallel <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  for (info in list(sequential, parallel)) {
    prep_models(info,
      models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
      back_test_scenarios = 1, num_hyperparameters = 1
    )
  }
  train_models(sequential, run_global_models = TRUE, negative_forecast = TRUE)
  train_models(parallel, run_global_models = TRUE, negative_forecast = TRUE, parallel_processing = "local_machine", num_cores = 2)
  for (suffix in c("-single_models", "-global_models")) {
    combos <- vapply(c("A", "B"), finnts:::hash_data, character(1))
    expected <- finnts:::read_exact_artifact(sequential, finnts:::local_artifact_path(
      sequential, "forecasts", suffix,
      combos
    ))
    actual <- finnts:::read_exact_artifact(parallel, finnts:::local_artifact_path(parallel, "forecasts", suffix, combos))
    expect_equal(actual, expected)
  }
})
