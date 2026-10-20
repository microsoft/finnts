# C1 recipe: a user-selectable, original-scale prep_data recipe used only by
# custom models. Fixtures use synthetic trusted-caller attestations.
c1_custom_model <- function(name = "c1_mean", modes = "local", offset = 0) {
  definition <- finnts:::new_custom_model_definition(name, "Use the historical mean plus a fixed offset.", "Fit the mean of analysis targets only.",
    modes,
    source = c(fit = "function(data, context, parameters) { mean(data$Target) + parameters$offset }", predict = paste0(
      "function(object, new_data, context) { data.frame(",
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

c1_dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
c1_input <- data.frame(
  Date = rep(c1_dates, 2),
  id = rep(c("A", "B"), each = 24), value = c(rep(100, 24), rep(200, 24))
)

c1_info <- function(project = "c1-recipe") {
  set_run_info(
    project_name = project, run_name = "test", path = withr::local_tempdir(.local_envir = parent.frame()),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE
  )
}

c1_prep <- function(info, recipes = c("R1", "C1"), input = c1_input, ...) {
  args <- utils::modifyList(list(
    run_info = info, input_data = input, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 2, recipes_to_run = recipes, stationary = FALSE,
    box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE
  ), list(...))
  do.call(prep_data, args)
  fs::dir_create(fs::path(info$path, c("models", "forecasts", "prep_models")))
  invisible(info)
}

c1_files <- function(info) list.files(fs::path(info$path, "prep_data"))

c1_read <- function(info, folder, suffix, combos = c("A", "B")) {
  finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, folder, suffix,
    vapply(combos, finnts:::hash_data, character(1)),
    extension = "csv"
  ))
}

test_that("check_recipes_to_run accepts C1 combinations and rejects invalid values", {
  for (ok in list(NULL, "C1", "R1", c("R1", "C1"), c("all", "C1"), c("R2", "C1"), list("R1", "C1"))) {
    expect_null(finnts:::check_recipes_to_run(ok))
  }
  for (bad in list(c("C1", "C1"), NA_character_, c("R1", NA), "R3", "c1", character(), 1, TRUE, list("R1", 2))) {
    expect_error(finnts:::check_recipes_to_run(bad), "R1, R2, C1, all")
  }
})

test_that("recipe expansion keeps R recipes first and adds C1 only when requested", {
  expand <- finnts:::expand_recipes_to_run
  expect_identical(expand(NULL, "month"), c("R1", "R2"))
  expect_identical(expand(NULL, "week"), "R1")
  expect_identical(expand(character(), "day"), "R1")
  expect_identical(expand(NA_character_, "month"), c("R1", "R2"))
  expect_identical(expand("all", "week"), c("R1", "R2"))
  expect_identical(expand("C1", "month"), "C1")
  expect_identical(expand(c("C1", "R1"), "month"), c("R1", "C1"))
  expect_identical(expand(c("R2", "C1"), "month"), c("R2", "C1"))
  expect_identical(expand(c("all", "C1"), "day"), c("R1", "R2", "C1"))
  expect_identical(expand(list("R2", "R1"), "month"), c("R1", "R2"))

  add <- finnts:::add_custom_recipe
  expect_identical(add(NULL, "month"), c("R1", "R2", "C1"))
  expect_identical(add(NULL, "week"), c("R1", "C1"))
  expect_identical(add("C1", "month"), "C1")
  expect_identical(add("R1", "month"), c("R1", "C1"))
  expect_identical(add(c("C1", "R2"), "month"), c("R2", "C1"))
  expect_identical(add("all", "week"), c("R1", "R2", "C1"))

  expect_identical(finnts:::logged_recipe_ids(data.frame(recipes_to_run = "R1---C1", date_type = "month")), c("R1", "C1"))
  expect_identical(finnts:::logged_recipe_ids(data.frame(recipes_to_run = "C1", date_type = "month")), "C1")
  expect_identical(finnts:::logged_recipe_ids(data.frame(recipes_to_run = NA, date_type = "week")), "R1")
})

test_that("custom_view_build keeps the original scale and NA future values", {
  df <- tibble::tibble(
    Combo = "A", Date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
    Target = c(10, NA, Inf, 40, 50, 60), x_hist = c(1, NA, 3, 4, NA, NA), x_future = c(1, 2, NaN, 4, NA, 6)
  )
  end <- as.Date("2020-04-01")
  view <- finnts:::custom_view_build(df, c("x_hist", "x_future"), "x_future", FALSE, "month", end)
  expect_identical(names(view), c("Combo", "Date", "Target", "x_hist", "x_future"))
  expect_equal(view$Target, c(10, 0, 0, 40, NA, NA))
  expect_equal(view$x_hist, c(1, 0, 3, 4, NA, NA))
  expect_equal(view$x_future, c(1, 2, 0, 4, 0, 6))

  imputed <- finnts:::custom_view_build(df, NULL, NULL, TRUE, "month", end)
  expect_identical(names(imputed), c("Combo", "Date", "Target"))
  expect_true(all(is.finite(imputed$Target[1:4])))
  expect_true(all(is.na(imputed$Target[5:6])))
  expect_false(isTRUE(all.equal(imputed$Target[2], 0)))
})

test_that("prep_data writes C1 only when requested and keeps C1 untransformed", {
  info <- c1_prep(c1_info(), recipes = "R1")
  expect_false(any(grepl("-C1\\.csv$", c1_files(info))))
  expect_length(finnts:::prepared_recipe_combos(info, c("R1", "C1")), 0L)
  expect_length(finnts:::prepared_recipe_combos(info, "R1"), 2L)

  trend <- c1_input
  trend$value <- c(seq(10, by = 3, length.out = 24), seq(500, by = -7, length.out = 24))
  trend$value[5] <- 1000
  info <- c1_prep(c1_info("c1-transform"),
    recipes = c("R1", "C1"), input = trend, stationary = TRUE, box_cox = TRUE, clean_outliers = TRUE
  )
  files <- c1_files(info)
  expect_identical(sum(grepl("-C1\\.csv$", files)), 2L)
  expect_identical(sum(grepl("-R1\\.csv$", files)), 2L)
  expect_setequal(
    finnts:::prepared_recipe_combos(info, c("R1", "C1")),
    vapply(c("A", "B"), finnts:::hash_data, character(1), USE.NAMES = FALSE)
  )
  c1 <- c1_read(info, "prep_data", "-C1")
  history <- c1[as.Date(c1$Date) <= max(c1_dates), ]
  expected <- trend$value[match(paste(history$Combo, as.Date(history$Date)), paste(trend$id, trend$Date))]
  expect_equal(history$Target, expected)
  expect_true(all(is.na(c1$Target[as.Date(c1$Date) > max(c1_dates)])))
  r1 <- c1_read(info, "prep_data", "-R1")
  r1_history <- r1[as.Date(r1$Date) <= max(c1_dates), ]
  expect_false(isTRUE(all.equal(sort(r1_history$Target), sort(history$Target))))

  only <- c1_prep(c1_info("c1-only"), recipes = "C1")
  expect_true(all(grepl("-C1\\.csv$", c1_files(only))))
  expect_length(finnts:::prepared_recipe_combos(only, "C1"), 2L)
  expect_length(finnts:::prepared_recipe_combos(only, c("R1", "C1")), 0L)
})

test_that("recipe discovery lists C1 only for custom training or C1-only runs", {
  info <- c1_prep(c1_info())
  default <- finnts:::recipe_artifact_files(info)
  expect_length(default, 2L)
  expect_false(any(grepl("-C1\\.csv$", default)))
  with_custom <- finnts:::recipe_artifact_files(info, include_custom = TRUE)
  expect_length(with_custom, 4L)
  expect_identical(sum(grepl("-C1\\.csv$", with_custom)), 2L)
  expect_false(any(grepl("-C1\\.csv$", finnts:::preparation_recipe_files(info))))

  only <- c1_prep(c1_info("c1-discovery"), recipes = "C1")
  expect_length(finnts:::recipe_artifact_files(only), 0L)
  fallback <- finnts:::preparation_recipe_files(only)
  expect_length(fallback, 2L)
  expect_true(all(grepl("-C1\\.csv$", fallback)))
})

test_that("C1 without custom models warns and C1-only runs reject built-in models", {
  info <- c1_prep(c1_info())
  expect_warning(
    prep_models(info, models_to_run = "meanf", run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1),
    "C1 is ignored"
  )
  workflows <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "prep_models", "-model_workflows",
    extension = "rds"
  ), return_type = "object")
  expect_false("C1" %in% workflows$Model_Recipe)

  only <- c1_prep(c1_info("c1-only-builtin"), recipes = "C1")
  model <- c1_custom_model()
  expect_error(
    prep_models(only, models_to_run = c("c1_mean", "meanf"), custom_models = list(c1_mean = model), run_ensemble_models = FALSE),
    "need recipe R1"
  )
  expect_null(finnts:::custom_run_read(only))
  expect_error(
    prep_models(only, models_to_run = "meanf", run_ensemble_models = FALSE),
    "need recipe R1"
  )

  r1 <- c1_prep(c1_info("c1-missing"), recipes = "R1")
  expect_error(
    prep_models(r1, models_to_run = "c1_mean", custom_models = list(c1_mean = model), run_ensemble_models = FALSE),
    "need C1"
  )
})

test_that("custom models stay on C1 while built-in models use transformed R recipes", {
  trend <- c1_input
  trend$value <- c(seq(10, by = 3, length.out = 24), seq(500, by = -7, length.out = 24))
  info <- c1_prep(c1_info("c1-hybrid"), recipes = c("R1", "C1"), input = trend, stationary = TRUE, box_cox = TRUE)
  model <- c1_custom_model()
  prep_models(info,
    models_to_run = c("c1_mean", "meanf", "snaive"), custom_models = list(c1_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  workflows <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "prep_models", "-model_workflows",
    extension = "rds"
  ), return_type = "object")
  expect_identical(unique(workflows$Model_Recipe[workflows$Model_Name == "c1_mean"]), "C1")
  expect_false("C1" %in% workflows$Model_Recipe[workflows$Model_Name != "c1_mean"])
  train_models(info, run_global_models = FALSE, negative_forecast = TRUE)
  local <- c1_read(info, "forecasts", "-single_models")
  custom <- local[local$Model_Name == "c1_mean", ]
  expect_gt(nrow(custom), 0L)
  expect_identical(unique(custom$Recipe_ID), "C1")
  expect_setequal(c("meanf", "snaive"), unique(local$Model_Name[local$Recipe_ID == "R1"]))
  history <- trend[trend$Date <= max(c1_dates), ]
  future <- custom[custom$Train_Test_ID == 1, ]
  expect_equal(
    future$Forecast,
    as.numeric(tapply(history$value, history$id, mean)[future$Combo])
  )
  final_models(info, average_models = TRUE)
  result <- get_forecast_data(info)
  expect_true(all(c("c1_mean", "meanf", "snaive") %in% result$Model_Name))
  expect_true(all(is.finite(result$Forecast)))
  expect_identical(sort(unique(result$Combo[result$Best_Model == "Yes"])), c("A", "B"))
})

test_that("model averages may combine custom and built-in members", {
  info <- c1_prep(c1_info("c1-average"))
  models <- list(low_rule = c1_custom_model("low_rule", offset = -10))
  prep_models(info,
    models_to_run = c("low_rule", "meanf"), custom_models = models, run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  train_models(info, run_global_models = FALSE, negative_forecast = TRUE)
  final_models(info, average_models = TRUE)
  result <- get_forecast_data(info)
  average <- result[result$Recipe_ID == "simple_average", ]
  expect_gt(nrow(average), 0L)
  components <- unique(unlist(strsplit(unique(average$Model_ID), "_", fixed = TRUE)))
  expect_true(any(grepl("^custom-.*--local--C1$", components)))
  expect_true(any(grepl("^meanf--local--R1$", components)))
  # meanf is exact and low_rule is 10 low, so the 50/50 average is 5 low.
  expect_equal(average$Forecast, ifelse(average$Combo == "A", 95, 195))
})

test_that("ensemble targets use original-scale history only when C1 predictions are present", {
  target <- tibble::tibble(
    Combo = "A", Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Train_Test_ID = c(2, 2, 1), Target = c(0.5, 0.6, NA)
  )
  series <- list(history = tibble::tibble(Date = as.Date(c("2020-01-01", "2020-02-01")), Target = c(100, 120)))
  expect_identical(finnts:::ensemble_target_scale(target, data.frame(Recipe_ID = "R1"), series), target)
  scaled <- finnts:::ensemble_target_scale(target, data.frame(Recipe_ID = c("R1", "C1")), series)
  expect_equal(scaled$Target, c(100, 120, NA))
  expect_identical(scaled[c("Combo", "Date", "Train_Test_ID")], target[c("Combo", "Date", "Train_Test_ID")])
})

test_that("ensembles train on custom forecasts alongside built-in models", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  set.seed(7)
  dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 48)
  input <- data.frame(
    Date = rep(dates, 2), id = rep(c("A", "B"), each = 48),
    value = c(100 + seq_along(dates) * 2 + stats::rnorm(48, 0, 4), 300 - seq_along(dates) + stats::rnorm(48, 0, 4))
  )
  info <- c1_prep(c1_info("c1-ensemble"), input = input, forecast_horizon = 2)
  prep_models(info,
    models_to_run = c("c1_mean", "meanf", "glmnet"), custom_models = list(c1_mean = c1_custom_model()),
    run_ensemble_models = TRUE, back_test_scenarios = 3, num_hyperparameters = 1
  )
  train_models(info, run_global_models = FALSE, negative_forecast = TRUE)
  ensemble_models(info)
  fits <- list.files(fs::path(info$path, "models"), pattern = "ensemble_models\\.rds$", full.names = TRUE)
  expect_gt(length(fits), 0L)
  fit <- readRDS(fits[[1]])$Model_Fit[[1]]
  predictors <- names(workflows::extract_mold(fit)$predictors)
  expect_true(any(grepl("c1_mean-C1-Local", predictors, fixed = TRUE)))
  expect_true(any(grepl("meanf-R1-Local", predictors, fixed = TRUE)))
  final_models(info, average_models = TRUE)
  result <- get_forecast_data(info)
  expect_true("ensemble" %in% result$Recipe_ID)
  expect_true(all(is.finite(result$Forecast)))
  expect_identical(sort(unique(result$Combo[result$Best_Model == "Yes"])), c("A", "B"))
})

test_that("custom training runs with inner_parallel like other univariate models", {
  skip_on_cran()
  skip_if_not(
    file.exists(file.path(getNamespaceInfo(asNamespace("finnts"), "path"), "Meta", "package.rds")),
    "PSOCK workers need an installed finnts namespace"
  )
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  info <- c1_prep(c1_info("c1-inner"))
  prep_models(info,
    models_to_run = c("c1_mean", "meanf"), custom_models = list(c1_mean = c1_custom_model()), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  train_models(info, run_global_models = FALSE, negative_forecast = TRUE, inner_parallel = TRUE, num_cores = 2)
  local <- c1_read(info, "forecasts", "-single_models")
  custom <- local[local$Model_Name == "c1_mean", ]
  expect_equal(custom$Forecast, ifelse(custom$Combo == "A", 100, 200))
})
