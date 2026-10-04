test_that("legacy hierarchical updates recover actual fitted components once", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    fixture <- make_update_chain_case(withr::local_tempdir(), approach = approach, legacy = TRUE)
    fixture$log$models_to_run <- "xgboost---chronos2---timegpt"
    expect_warning(mapping <- with_mocked_bindings(
      read_global_update_selection(fixture$info, fixture$log, fixture$hierarchy$original_combos),
      list_files = function(...) stop("selection recovery must use exact paths")), "Legacy global update")
    expect_identical(names(mapping$selected_ids), fixture$hierarchy$hts_combos)
    expect_true(all(mapping$selected_ids == paste(sort(fixture$ids), collapse = "_")))
    expect_true(all(vapply(mapping$components, setequal, logical(1), fixture$ids)))
    expect_false(any(grepl("timegpt", mapping$selected_ids)))
    assembled <- adjust_forecast(fixture$fitted, fixture$info, approach, FALSE, mapping)
    chosen <- assembled[assembled$Best_Model == "Yes", ]
    expect_setequal(chosen$Combo, fixture$hierarchy$hts_combos)
    for (combo in fixture$hierarchy$hts_combos) {
      expect_equal(chosen$Forecast[chosen$Combo == combo],
        rep(fixture$contexts[[combo]]$history$Target[1], sum(chosen$Combo == combo)))
    }
  }
})

test_that("legacy recovery supports one fit and multiple model-recipe components", {
  cases <- list(list(models = "xgboost", recipes = "R2"),
    list(models = c("xgboost", "xgboost"), recipes = c("R1", "R2")),
    list(models = c("xgboost", "chronos2", "timegpt"), recipes = rep("R1", 3)))
  for (settings in cases) {
    fixture <- do.call(make_update_chain_case,
      c(list(path = withr::local_tempdir(), legacy = TRUE), settings))
    expect_warning(mapping <- read_global_update_selection(fixture$info, fixture$log,
      fixture$hierarchy$original_combos), "Legacy global update")
    expect_length(mapping$components, length(fixture$hierarchy$hts_combos))
    expect_true(all(vapply(mapping$components, setequal, logical(1), fixture$ids)))
    expect_true(all(mapping$selected_ids == paste(sort(fixture$ids), collapse = "_")))
  }
})

test_that("modern heterogeneous source choices remain authoritative", {
  fixture <- make_update_chain_case(withr::local_tempdir())
  mapping <- expect_no_warning(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos))
  expect_identical(mapping$selected_ids, fixture$winners)
  expect_gt(length(unique(mapping$selected_ids)), 1L)
  assembled <- adjust_forecast(fixture$fitted[2:1, ], fixture$info,
    fixture$log$forecast_approach, FALSE, mapping)
  chosen <- assembled[assembled$Best_Model == "Yes", ]
  expect_identical(chosen$Model_ID, unname(fixture$winners[chosen$Combo]))
})

test_that("partial modern source artifacts cannot trigger legacy recovery", {
  fixture <- make_update_chain_case(withr::local_tempdir(), omit_sources = "Total")
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), "Saved global winner is missing or ambiguous",
    class = "finnts_update_artifact_error")
})

test_that("corruption and provider failures are not absence", {
  fixture <- make_update_chain_case(withr::local_tempdir(), legacy = TRUE)
  path <- local_artifact_path(fixture$info, "forecasts", "-global_models", hash_data("Total"))
  writeLines("not an RDS", path)
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), class = "finnts_update_artifact_error")
  local_mocked_bindings(read_exact_artifact = function(...) {
    rlang::abort("storage access denied", class = "chain_storage_error")
  })
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), class = "chain_storage_error")
})

test_that("legacy and modern HTS runs publish reusable multi-generation updates", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    for (legacy in c(FALSE, TRUE)) {
      path <- withr::local_tempdir()
      previous <- make_update_chain_case(path, approach = approach, legacy = legacy)
      expected <- if (legacy) stats::setNames(rep(paste(sort(previous$ids), collapse = "_"),
        length(previous$winners)), names(previous$winners)) else previous$winners
      for (generation in 1:3) {
        current <- make_update_chain_case(path, paste0("update-", generation), approach, legacy = TRUE)
        if (legacy && generation == 1L) {
          expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
        } else step <- expect_no_warning(run_update_chain_step(previous, current))
        expect_identical(step$result$status, "done")
        expect_length(step$result$quality_rejected_combos, 0)
        expect_length(step$fits, 1)
        expect_setequal(step$fits[[1]], previous$ids)
        expect_identical(step$model_reads, 1L)
        saved <- read_update_result(current$info, current$hierarchy$original_combos,
          TRUE, 6, approach, current$splits, "R1")
        expect_type(saved, "list")
        expect_setequal(saved$forecasts$Combo, current$hierarchy$original_combos)
        actual <- expect_no_warning(read_global_update_selection(current$info,
          read_selection_file(current$info, "logs"), current$hierarchy$original_combos))
        expect_identical(actual$selected_ids, expected)
        parent <- step$agent$project_info
        parent$run_name <- step$agent$run_id
        for (combo in current$hierarchy$original_combos) {
          metadata <- read_selection_file(parent, "logs", "-agent_best_run", combo)
          expect_identical(metadata$best_run_name, current$info$run_name)
          expect_identical(metadata$model_type, "global")
        }
        previous <- current
      }
    }
  }
})

test_that("a dropped HTS source write cannot be promoted as a complete update", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path)
  current <- make_update_chain_case(path, "interrupted", legacy = TRUE)
  expect_error(run_update_chain_step(previous, current, drop_source = "Total"),
    "source|complete|Cannot log")
  parent <- current$info
  parent$project_name <- "chain"
  parent$run_name <- "parent-interrupted"
  for (combo in current$hierarchy$original_combos) {
    expect_equal(nrow(read_selection_file(parent, "logs", "-agent_best_run", combo, optional = TRUE)), 0)
  }
})

test_that("restart compares per-node choices instead of just the shared model pool", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path)
  current <- make_update_chain_case(path, "wrong-selections", uniform = TRUE)
  step <- run_update_chain_step(previous, current)
  expect_length(step$fits, 1)
  mapping <- read_global_update_selection(current$info,
    read_selection_file(current$info, "logs"), current$hierarchy$original_combos)
  expect_identical(mapping$selected_ids, previous$winners)
  resumed <- expect_no_warning(run_update_chain_step(previous, current))
  expect_length(resumed$fits, 0)
  expect_identical(resumed$result$status, "done")
})

test_that("legacy averaging is recorded even when historical averaging was disabled", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, legacy = TRUE)
  previous$log$average_models <- FALSE
  previous$log$max_model_average <- 1
  write_data(previous$log, NULL, previous$info, "log", "logs")
  current <- make_update_chain_case(path, "averaging-enabled", legacy = TRUE)
  expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
  expect_identical(step$result$status, "done")
  log <- read_selection_file(current$info, "logs")
  expect_true(log$average_models)
  expect_gte(log$max_model_average, 2)
  mapping <- expect_no_warning(read_global_update_selection(current$info, log, current$hierarchy$original_combos))
  expect_true(all(lengths(mapping$components) == 2L))
})

test_that("legacy recovery rejects invalid fits and existing empty source artifacts", {
  fixture <- make_update_chain_case(withr::local_tempdir(), legacy = TRUE)
  for (defect in c("duplicate", "null_fit", "malformed_id", "wrong_type", "unlogged_recipe", "empty")) {
    models <- fixture$models
    if (defect == "duplicate") models <- rbind(models, models[1, ])
    if (defect == "null_fit") models$Model_Fit[1] <- list(NULL)
    if (defect == "malformed_id") models$Model_ID[1] <- "xgboost"
    if (defect == "wrong_type") models$Model_Type[1] <- "local"
    if (defect == "unlogged_recipe") {
      models$Recipe_ID[1] <- "R2"
      models$Model_ID[1] <- "xgboost--global--R2"
    }
    if (defect == "empty") models <- models[0, ]
    write_data(models, "All-Data", fixture$info, "object", "models", "-single_models")
    expect_error(read_global_update_selection(fixture$info, fixture$log,
      fixture$hierarchy$original_combos), class = "finnts_update_artifact_error", info = defect)
  }
  write_data(fixture$models, "All-Data", fixture$info, "object", "models", "-single_models")
  write_data(fixture$source[0, ], "Total", fixture$info, "data", "forecasts", "-average_models")
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), "No candidate forecasts")
  fixture$log$average_models <- FALSE
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), "No candidate forecasts")
})

test_that("legacy global subsets do not require other local winners' reconciled rows", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, legacy = TRUE)
  covered <- previous$hierarchy$original_combos[1:2]
  reconciled <- read_selection_file(previous$info, "forecasts", "-reconciled", "Best-Model")
  write_data(reconciled[reconciled$Combo %in% covered, ], "Best-Model",
    previous$info, "data", "forecasts", "-reconciled")
  current <- make_update_chain_case(path, "subset-update", legacy = TRUE)
  expect_warning(step <- run_update_chain_step(previous, current, combos = covered), "Legacy global update")
  expect_identical(step$result$status, "done")
  expect_setequal(read_selection_file(current$info, "forecasts", "-reconciled", "Best-Model")$Combo, covered)
  expect_setequal(names(read_global_update_selection(current$info,
    read_selection_file(current$info, "logs"), covered)$selected_ids), previous$hierarchy$hts_combos)
  expect_error(read_global_update_selection(previous$info, previous$log, previous$hierarchy$original_combos),
    "every requested global series")
})

test_that("publication rejects an interrupted stale-average overwrite", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path)
  current <- make_update_chain_case(path, "stale-average", uniform = TRUE)
  expect_error(run_update_chain_step(previous, current, drop_source = "A", drop_suffix = "-average_models"),
    "Cannot log an incomplete global update")
})

test_that("local selected averages survive iteration followed by repeated updates", {
  path <- withr::local_tempdir()
  members <- data.frame(ID = "local-series")
  previous <- make_update_chain_case(path, approach = "bottoms_up", members = members,
    models = c("arima", "ets"), recipes = c("R1", "R1"), model_type = "local",
    uniform = TRUE, data_output = "csv")
  initial <- finalize_update_chain_iteration(previous)
  expected <- initial$selections[["local-series"]]$selected_id
  expect_setequal(strsplit(expected, "_", fixed = TRUE)[[1]], previous$ids)
  for (generation in 1:2) {
    current <- make_update_chain_case(path, paste0("local-update-", generation), "bottoms_up",
      members = members, models = c("arima", "ets"), recipes = c("R1", "R1"),
      model_type = "local", legacy = TRUE, data_output = "csv")
    step <- expect_no_warning(run_update_chain_step(previous, current))
    expect_identical(step$result$status, "done")
    result <- read_update_result(current$info, "local-series", FALSE, 6,
      splits = current$splits, recipes = "R1")
    expect_type(result, "list")
    expect_identical(unique(result$forecasts$Model_ID[result$forecasts$Best_Model == "Yes"]), expected)
    previous <- current
  }
})

test_that("modern component identities reject trailing separators", {
  fixture <- make_update_chain_case(withr::local_tempdir())
  combo <- fixture$hierarchy$hts_combos[2]
  original <- read_selection_file(fixture$info, "forecasts", "-global_models", combo)
  for (suffix in c("_", "--")) {
    rows <- original
    rows$Model_ID[rows$Best_Model == "Yes"] <- paste0(rows$Model_ID[rows$Best_Model == "Yes"], suffix)
    write_data(rows, combo, fixture$info, "data", "forecasts", "-global_models")
    expect_error(read_global_update_selection(fixture$info, fixture$log,
      fixture$hierarchy$original_combos), "invalid component identities")
  }
})

test_that("a corrupt optional unselected average cannot certify a reusable result", {
  fixture <- make_update_chain_case(withr::local_tempdir())
  combo <- fixture$hierarchy$hts_combos[2]
  expect_type(read_update_result(fixture$info, fixture$hierarchy$original_combos, TRUE,
    6, fixture$log$forecast_approach, fixture$splits, "R1"), "list")
  path <- local_artifact_path(fixture$info, "forecasts", "-average_models", hash_data(combo))
  writeLines("corrupt average", path)
  expect_null(read_update_result(fixture$info, fixture$hierarchy$original_combos, TRUE,
    6, fixture$log$forecast_approach, fixture$splits, "R1"))
})

test_that("parquet chained updates retain readable empty unselected averages", {
  skip_if_not_installed("arrow")
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, models = "xgboost", recipes = "R1",
    legacy = TRUE, data_output = "parquet")
  current <- make_update_chain_case(path, "parquet-update", models = "xgboost", recipes = "R1",
    legacy = TRUE, data_output = "parquet")
  expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
  expect_identical(step$result$status, "done")
  expect_type(read_update_result(current$info, current$hierarchy$original_combos, TRUE,
    6, current$log$forecast_approach, current$splits, "R1"), "list")
  mapping <- expect_no_warning(read_global_update_selection(current$info,
    read_selection_file(current$info, "logs"), current$hierarchy$original_combos))
  expect_true(all(mapping$selected_ids == previous$ids))
})

test_that("malformed modern fit schemas retain the hard artifact error class", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path)
  current <- make_update_chain_case(path, "invalid-model-schema", legacy = TRUE)
  write_data(data.frame(Model_Name = "xgboost"), "All-Data", previous$info, "object", "models", "-single_models")
  expect_error(run_update_chain_step(previous, current), class = "finnts_update_artifact_error")
})

test_that("an average without its source components cannot disguise partial artifacts", {
  fixture <- make_update_chain_case(withr::local_tempdir(), omit_sources = "Total")
  rows <- fixture$source[fixture$source$Combo == "Total" & fixture$source$Recipe_ID == "simple_average", ]
  write_data(rows, "Total", fixture$info, "data", "forecasts", "-average_models")
  expect_error(read_global_update_selection(fixture$info, fixture$log,
    fixture$hierarchy$original_combos), "source component forecasts are missing")
})
