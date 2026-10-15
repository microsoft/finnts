# Construct synthetic, version-bound trusted-caller attestations for offline
# arithmetic tests. No authoring provider or public approval API is used.
standard_custom_model <- function(name = "business_mean", modes = c("local", "global"), offset = 0) {
  definition <- finnts:::new_custom_model_definition(name, "Use the historical mean plus a fixed offset.", "Fit the mean of analysis targets only.",
    modes,
    source = c(fit = paste0(
      "function(data, context, parameters) { ", "stopifnot(max(data$Date) == context$cutoff); ",
      "mean(data$Target) + parameters$offset }"
    ), predict = paste0(
      "function(object, new_data, context) { ", "stopifnot(!'Target' %in% names(new_data)); data.frame(",
      ".finnts_row = new_data$.finnts_row, .pred = rep(object, nrow(new_data))) }"
    )), requirements = list(
      predictors = character(),
      recipes = "C1", target_scale = "original", date_types = "month", forecast_horizon = 1:3, missing_data = "error"
    ),
    fixed_parameters = list(offset = offset)
  )
  structure(list(definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Synthetic fixture checked independently")
  ), approval = list(
    version_id = definition$version_id,
    intent_confirmed = TRUE, allow_code = TRUE
  )), class = "finnts_custom_model")
}

standard_input <- data.frame(
  Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2),
  id = rep(c("A", "B"), each = 24), value = c(rep(100, 24), rep(200, 24))
)

# Build a genuine preparation template lazily during the first measured test,
# then copy only unchanged logs/data into each caller's isolated temporary root.
# This invocation-local cache never contains fitted models or forecast results.
standard_run <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    if (is.null(cache$info)) {
      info <- set_run_info(
        project_name = "custom-standard", run_name = "test",
        path = withr::local_tempdir(.local_envir = cache),
        data_output = "csv", object_output = "rds", add_unique_id = FALSE
      )
      prep_data(info, standard_input,
        combo_variables = "id", target_variable = "value",
        date_type = "month", forecast_horizon = 2, recipes_to_run = c("R1", "C1"), stationary = FALSE,
        box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE
      )
      cache$info <- info
      cache$hashes <- tools::md5sum(list.files(info$path, recursive = TRUE, full.names = TRUE))
    }
    expect_identical(tools::md5sum(names(cache$hashes)), cache$hashes)
    info <- cache$info
    info$path <- path
    for (folder in c("logs", "prep_data")) {
      fs::dir_copy(fs::path(cache$info$path, folder), fs::path(path, folder))
    }
    fs::dir_create(fs::path(path, c("models", "forecasts", "prep_models")))
    info
  }
})
