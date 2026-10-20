# Build a version-bound category rule whose labels must survive CSV inference.
# The expected prediction comes directly from the category lookup, not source.
category_custom_model <- function() {
  model <- custom_driver_model("category_rule", "Category")
  definition <- model$definition
  definition$requirements$predictor_types <- list(Category = "string")
  definition$source <- c(
    fit = "function(data, context, parameters) { stopifnot(is.character(data$Category) || is.factor(data$Category)); 1 }",
    predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=as.numeric(as.character(new_data$Category)))")
  definition$version_id <- custom_model_definition_digest(definition)
  model$definition <- definition
  model$validation$version_id <- model$approval$version_id <- definition$version_id
  model
}

test_that("C1 CSV preparation and training preserve declared category strings", {
  info <- set_run_info("c1-types", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  data <- custom_driver_data(2L)
  data$Category <- rep(c("100", "200", "300"), length.out = nrow(data))
  do.call(prep_data, custom_driver_prep(info, data))
  model <- category_custom_model()
  prep_models(info, models_to_run = "category_rule", custom_models = list(category_rule = model),
    back_test_scenarios = 1, num_hyperparameters = 1, run_ensemble_models = FALSE)
  train_models(info, run_local_models = TRUE, run_global_models = TRUE)
  custom <- custom_run_read(info, TRUE)
  rows <- get_recipe_data(info, combo = hash_data("series1"), recipes = "C1",
    character_columns = custom_run_character_columns(custom))
  expect_identical(rows$Data[[1]]$Category, data$Category[data$id == "series1"])
  expect_type(rows$Data[[1]]$Driver, "double")
})

test_that("C1 text parsing preserves exact labels and leaves native types alone", {
  info <- set_run_info("c1-labels", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  data <- data.frame(Combo = "007", Category = c("00100", "12345678901234567890", "300"),
    Driver = c(1.5, 2.5, 3.5), Flag = c(TRUE, FALSE, TRUE))
  path <- fs::path(info$path, "labels.csv")
  vroom::vroom_write(data, path, delim = ",")
  columns <- custom_run_character_columns(list(pool = list(custom = list(category_custom_model()$definition))))
  expect_identical(columns, c("Combo", "Category"))
  actual <- read_file(info, file_list = path, character_columns = columns)
  expect_identical(actual$Category, data$Category)
  expect_identical(actual$Combo, data$Combo)
  expect_equal(actual$Driver, data$Driver)
  expect_identical(actual$Flag, data$Flag)
  native <- fs::path(info$path, "labels.rds")
  saveRDS(data, native)
  expect_identical(read_file(info, file_list = native, character_columns = columns), data)
  first <- category_custom_model()$definition
  second <- first
  second$requirements$predictor_types$Category <- "number"
  expect_error(custom_run_character_columns(list(pool = list(custom = list(first, second)))), "Conflicting.*Category")
})
