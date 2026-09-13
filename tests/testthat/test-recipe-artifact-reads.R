test_that("known combo recipes load directly and exclude other combos", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_log(run_info, c("R2", "R1"))
  expected_r1 <- artifact_test_recipe(run_info, "A", "R1", 10)
  expected_r2 <- artifact_test_recipe(run_info, "A", "R2", 20)
  artifact_test_recipe(run_info, "B", "R1", 100)
  tracker <- local_artifact_spies()

  result <- get_recipe_data(run_info, combo = hash_data("A"))

  expect_setequal(result$Recipe, c("R1", "R2"))
  expect_equal(result$Data[[match("R1", result$Recipe)]], expected_r1)
  expect_equal(result$Data[[match("R2", result$Recipe)]], expected_r2)
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 3L)
  expect_true(all(vapply(tracker$reads, function(read) {
    is.null(read$path) && length(read$file_list) == 1L
  }, logical(1))))
  paths <- c(
    artifact_test_path(run_info, "logs", extension = "csv"),
    artifact_test_path(run_info, "prep_data", "A", "-R1"),
    artifact_test_path(run_info, "prep_data", "A", "-R2")
  )
  expect_setequal(tracker$metadata_paths, as.character(paths))
  expect_equal(anyDuplicated(tracker$metadata_paths), 0L)
  expect_setequal(tracker$payload_paths, as.character(paths))
  expect_equal(anyDuplicated(tracker$payload_paths), 0L)
  expect_equal(tracker$directory_listings, 0L)
})

test_that("known combo recipe defaults follow the cadence", {
  for (date_type in c("day", "week", "month", "quarter", "year")) {
    run_info <- artifact_test_run(withr::local_tempdir())
    artifact_test_log(run_info, NULL, date_type)
    recipes <- get_recipes_to_run(NULL, date_type)
    for (recipe in recipes) artifact_test_recipe(run_info, "A", recipe)
    tracker <- local_artifact_spies()

    result <- get_recipe_data(run_info, combo = hash_data("A"))

    expect_setequal(result$Recipe, recipes)
    expect_equal(tracker$listings, 0L, info = date_type)
  }
})

test_that("R2-only recipes preserve already hashed Unicode combo identifiers", {
  combo <- paste0("North ", intToUtf8(233), "--Retail")
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data(combo)
  artifact_test_log(run_info, "R2")
  expected <- artifact_test_recipe(run_info, combo, "R2")
  tracker <- local_artifact_spies()

  result <- get_recipe_data(run_info, combo = run_info$combo)

  expect_identical(result$Recipe, "R2")
  expect_equal(result$Data[[1]], expected)
  expect_equal(tracker$listings, 0L)
})

test_that("missing configured recipes remain required artifacts", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_log(run_info, c("R1", "R2"))
  artifact_test_recipe(run_info, "A", "R1")
  tracker <- local_artifact_spies()

  expect_error(
    get_recipe_data(run_info, combo = hash_data("A")),
    "Missing required Finn artifact.*R2"
  )
  expect_equal(tracker$listings, 0L)
})

test_that("All-Data recipe reads reuse one discovery result", {
  run_info <- artifact_test_run(withr::local_tempdir())
  for (combo in c("A", "B")) {
    for (recipe in c("R1", "R2")) artifact_test_recipe(run_info, combo, recipe)
  }
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_recipe_data(run_info, combo = "All-Data")

  expect_setequal(result$Recipe, c("R1", "R2"))
  for (data in result$Data) expect_setequal(data$Combo, c("A", "B"))
  expect_equal(tracker$listings, 1L)
  expect_length(tracker$reads, 2L)
  expect_true(all(vapply(tracker$reads, function(read) {
    is.null(read$path) && length(read$file_list) == 2L
  }, logical(1))))
})

test_that("supplied recipe metadata avoids another run log read", {
  run_info <- artifact_test_run(withr::local_tempdir())
  expected <- artifact_test_recipe(run_info, "A", "R1")
  tracker <- local_artifact_spies()

  result <- get_recipe_data(run_info, hash_data("A"), recipes = "R1")

  expect_equal(result$Data[[1]], expected)
  expect_length(tracker$reads, 1L)
})

test_that("training shares recipe metadata with every local worker", {
  run_info <- set_run_info(
    project_name = "artifact_training", run_name = "test",
    path = withr::local_tempdir(), add_unique_id = FALSE
  )
  data <- tibble::tibble(
    id = rep(c("A", "B"), each = 36),
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36), 2),
    value = rep(100 + seq_len(36), 2)
  )
  prep_data(run_info, data, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 2, recipes_to_run = "R1"
  )
  prep_models(run_info, models_to_run = "meanf", back_test_scenarios = 1,
    num_hyperparameters = 1, run_ensemble_models = FALSE
  )
  tracker <- local_artifact_spies(max_listings = 3L)

  train_models(run_info, run_global_models = FALSE, run_local_models = TRUE)

  log_reads <- vapply(tracker$reads, function(read) {
    any(grepl("(^|/)logs/", paste0(read$path, read$file_list)))
  }, logical(1))
  expect_equal(sum(log_reads), 1L)
  expect_equal(tracker$listings, 3L)
  for (combo in c("A", "B")) {
    expect_true(fs::file_exists(artifact_test_path(
      run_info, "models", combo, "-single_models", run_info$object_output
    )))
  }
})