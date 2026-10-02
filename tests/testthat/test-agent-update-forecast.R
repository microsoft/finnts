make_update_agent_run_table <- function(forecast_approach = "bottoms_up") {
  data.frame(
    agent_version = 5:1,
    run_id = paste0("run-", 5:1),
    forecast_approach = forecast_approach,
    forecast_horizon = 6,
    external_regressors = NA_character_,
    hist_end_date = as.Date("2026-07-01"),
    back_test_scenarios = 1,
    back_test_spacing = 1,
    combo_cleanup_date = as.Date(NA),
    run_global_models = TRUE,
    run_local_models = TRUE,
    stringsAsFactors = FALSE
  )
}

make_update_agent_info <- function(forecast_approach = "bottoms_up") {
  list(
    agent_version = 5,
    run_id = "run-5",
    overwrite = TRUE,
    forecast_approach = forecast_approach,
    forecast_horizon = 6,
    run_global_models = TRUE,
    run_local_models = TRUE,
    llm = NULL,
    project_info = list(
      project_name = "project",
      path = "run-logging",
      storage_object = NULL,
      data_output = "csv"
    )
  )
}

make_update_run_metadata <- function(run_id,
                                     combos = c("combo-a", "combo-b"),
                                     weighted_mape = 0.1) {
  data.frame(
    agent_run_id = run_id,
    combo = combos,
    model_type = "local",
    best_run_name = paste0("agent_", run_id, "_", combos),
    weighted_mape = weighted_mape,
    stringsAsFactors = FALSE
  )
}

make_update_forecast <- function(combos = c("combo-a", "combo-b"),
                                 model_ids = paste0("model-", combos),
                                 best_model = "Yes") {
  data.frame(
    Combo = combos,
    Model_ID = model_ids,
    Best_Model = best_model,
    stringsAsFactors = FALSE
  )
}

make_update_model_summary <- function(combos = c("combo-a", "combo-b"),
                                      model_ids = paste0("model-", combos)) {
  data.frame(
    Combo = combos,
    Model_ID = model_ids,
    Best_Model = "Yes",
    section = "engine_param",
    name = "model",
    value = model_ids,
    stringsAsFactors = FALSE
  )
}

make_update_eda <- function() {
  data.frame(
    Combo = "All",
    Analysis_Type = "Data_Profile",
    Metric = "Number_Series",
    Value = "2",
    stringsAsFactors = FALSE
  )
}

make_update_hierarchy_summary <- function() {
  data.frame(
    Hierarchy_Combo = c("Total", "combo-a", "combo-b"),
    Hierarchy_Level_Type = c("Total", "Bottom", "Bottom"),
    Bottom_Combo = c("combo-a", "combo-a", "combo-b"),
    Is_Bottom = c(FALSE, TRUE, TRUE),
    Parent_Level = c(NA, "Total", "Total"),
    stringsAsFactors = FALSE
  )
}

make_complete_final_outputs <- function(run_id,
                                        combos = c("combo-a", "combo-b"),
                                        weighted_mape = 0.1,
                                        forecast_approach = "bottoms_up") {
  outputs <- list(
    run_metadata = make_update_run_metadata(
      run_id,
      combos = combos,
      weighted_mape = weighted_mape
    ),
    forecast = make_update_forecast(combos = combos),
    model_summary = make_update_model_summary(combos = combos),
    eda = make_update_eda()
  )

  if (forecast_approach != "bottoms_up") {
    outputs$hierarchy_summary <- make_update_hierarchy_summary()
  }

  outputs
}

# Write nonempty final outputs and version logs under the caller's temporary
# path. Identifiers remain opaque text in the source data, while metrics and
# versions are numeric. Returns real-file Agent inputs and expected metadata;
# no model training, provider calls, or artifact deletion is performed.
make_csv_update_fixture <- function(path, run_id, combo = "00042") {
  agent_info <- make_update_agent_info()
  agent_info$project_info$path <- path
  agent_info$run_id <- "1000000000000000"
  agent_runs <- make_update_agent_run_table()[1:2, ]
  agent_runs$run_id <- c(agent_info$run_id, run_id)
  previous <- agent_info
  previous$run_id <- run_id
  previous$agent_version <- agent_runs$agent_version[[2]]
  outputs <- make_complete_final_outputs(run_id, combo, weighted_mape = 0.125)
  outputs$run_metadata$best_run_name <- "00007"
  fs::dir_create(fs::path(path, c("final_output", "logs")))
  for (suffix in names(outputs)) {
    utils::write.csv(outputs[[suffix]], fs::path(path, "final_output", paste0(
      hash_data(agent_info$project_info$project_name), "-", hash_data(run_id),
      "-", suffix, ".csv"
    )), row.names = FALSE)
  }
  for (index in seq_len(nrow(agent_runs))) {
    utils::write.csv(agent_runs[index, ], fs::path(path, "logs", paste0(
      hash_data(agent_info$project_info$project_name), "-",
      hash_data(agent_runs$run_id[[index]]), "-agent_run.csv"
    )), row.names = FALSE)
  }
  list(agent_info = agent_info, previous = previous, agent_runs = agent_runs,
    metadata = outputs$run_metadata, combo = combo)
}

test_that("real CSV completion metadata preserves opaque Agent identifiers", {
  run_ids <- c("4282d17137126405", "1e10", "9007199254740993",
    "0012345678901234", "19d7e8f9db2fb790")
  for (run_id in run_ids) {
    fixture <- make_csv_update_fixture(withr::local_tempdir(), run_id)
    metadata <- load_final_agent_run_metadata(fixture$previous)
    identifiers <- c("agent_run_id", "combo", "best_run_name")
    expect_identical(lapply(metadata[identifiers], as.character),
      lapply(fixture$metadata[identifiers], as.character))
    expect_true(all(vapply(metadata[identifiers], is.character, logical(1))))
    expect_identical(metadata$weighted_mape, 0.125)
    expect_length(find_completed_previous_agent_runs(fixture$agent_info,
      fixture$agent_runs[2, ]), 1L)
  }
})

test_that("CSV fallback preserves final metadata identifiers and metrics", {
  fixture <- make_csv_update_fixture(withr::local_tempdir(), "0012345678901234")
  original_vroom <- vroom::vroom
  testthat::local_mocked_bindings(
    vroom = function(file, ...) {
      if (any(startsWith(as.character(file), fixture$previous$project_info$path))) {
        stop("force fixture CSV fallback")
      }
      original_vroom(file, ...)
    }, .package = "vroom"
  )
  metadata <- load_final_agent_run_metadata(fixture$previous)
  identifiers <- c("agent_run_id", "combo", "best_run_name")
  expect_identical(lapply(metadata[identifiers], as.character),
    lapply(fixture$metadata[identifiers], as.character))
  expect_true(all(vapply(metadata[identifiers], is.character, logical(1))))
  expect_identical(metadata$weighted_mape, 0.125)
  expect_length(find_completed_previous_agent_runs(fixture$agent_info,
    fixture$agent_runs[2, ]), 1L)
})

test_that("saving final Agent metadata preserves the intermediate CSV identity", {
  for (backend in c("vroom", "fallback")) {
    for (run_id in c("4282d17137126405", "1e10", "9007199254740993",
      "0012345678901234", "19d7e8f9db2fb790")) local({
      fixture <- make_csv_update_fixture(withr::local_tempdir(), run_id)
      log_info <- fixture$previous$project_info
      log_info$run_name <- fixture$previous$run_id
      utils::write.csv(fixture$metadata, local_artifact_path(log_info, "logs",
        "-agent_best_run", hash_data(fixture$combo), extension = "csv"), row.names = FALSE)
      original_vroom <- vroom::vroom
      if (identical(backend, "fallback")) {
        testthat::local_mocked_bindings(
          vroom = function(file, ...) {
            if (any(startsWith(as.character(file), log_info$path))) stop("force fixture CSV fallback")
            original_vroom(file, ...)
          }, .package = "vroom"
        )
      }
      testthat::local_mocked_bindings(
        check_agent_info = function(...) invisible(NULL),
        get_total_combos = function(...) hash_data(fixture$combo),
        .package = "finnts"
      )
      save_best_agent_run(fixture$previous)
      metadata <- load_final_agent_run_metadata(fixture$previous)
      expect_identical(metadata$agent_run_id, fixture$previous$run_id)
      expect_identical(metadata$combo, fixture$combo)
      expect_identical(metadata$best_run_name, fixture$metadata$best_run_name)
      expect_identical(metadata$weighted_mape, 0.125)
      expect_length(find_completed_previous_agent_runs(fixture$agent_info,
        fixture$agent_runs[2, ]), 1L)
    })
  }
})

test_that("current update metadata preserves identifiers with one base CSV read", {
  fixture <- make_csv_update_fixture(withr::local_tempdir(), "0012345678901234")
  log_info <- fixture$previous$project_info
  log_info$run_name <- fixture$previous$run_id
  path <- local_artifact_path(log_info, "logs", "-agent_best_run", hash_data(fixture$combo))
  utils::write.csv(fixture$metadata, path, row.names = FALSE)
  before <- tools::md5sum(path)
  original_reader <- read_update_csv
  reads <- 0L
  testthat::local_mocked_bindings(
    read_update_csv = function(path, ...) {
      reads <<- reads + 1L
      original_reader(path, ...)
    },
    .package = "finnts"
  )
  testthat::local_mocked_bindings(
    vroom = function(...) stop("current metadata must use its single-connection base reader"),
    .package = "vroom"
  )
  metadata <- load_update_runs(fixture$previous)
  expect_identical(metadata$agent_run_id, fixture$previous$run_id)
  expect_identical(metadata$combo, fixture$combo)
  expect_identical(metadata$best_run_name, fixture$metadata$best_run_name)
  expect_identical(metadata$weighted_mape, 0.125)
  expect_equal(reads, 1L)
  expect_identical(tools::md5sum(path), before)
})

test_that("exact CSV reads forward identity types without extra storage operations", {
  for (provider in c("local", "staged")) {
    for (backend in c("vroom", "fallback")) local({
      directory <- withr::local_tempdir()
      source_path <- fs::path(directory, "metadata.csv")
      expected <- data.frame(agent_run_id = "0012345678901234", weighted_mape = 0.125)
      utils::write.csv(expected, source_path, row.names = FALSE)
      info <- list(path = directory, storage_object = NULL)
      path <- source_path
      if (provider == "staged") {
        info$storage_object <- structure(list(), class = "blob_container")
        path <- "logs/metadata.csv"
      }
      downloads <- 0L
      reads <- 0L
      original_vroom <- vroom::vroom
      testthat::local_mocked_bindings(
        download_exact_artifact = function(storage_object, path, destination, allow_missing) {
          downloads <<- downloads + 1L
          fs::file_copy(source_path, destination)
          TRUE
        },
        list_files = function(...) stop("exact reader enumerated storage"),
        .package = "finnts"
      )
      testthat::local_mocked_bindings(
        vroom = function(file, ...) {
          reads <<- reads + 1L
          if (backend == "fallback") stop("force fixture CSV fallback")
          original_vroom(file, ...)
        }, .package = "vroom"
      )
      result <- read_exact_artifact(info, path, character_columns = "agent_run_id")
      expect_identical(result$agent_run_id, expected$agent_run_id)
      expect_identical(result$weighted_mape, expected$weighted_mape)
      expect_equal(downloads, as.integer(provider == "staged"))
      expect_equal(reads, 1L)
    })
  }
})

test_that("identity schemas do not conceal missing or failed exact reads", {
  info <- list(path = withr::local_tempdir(), storage_object = NULL)
  missing <- fs::path(info$path, "missing.csv")
  expect_null(read_exact_artifact(info, missing, allow_missing = TRUE,
    character_columns = "agent_run_id"))
  expect_error(read_local_artifacts(info, missing, character_columns = "agent_run_id"),
    "Missing required Finn artifact")
  info$storage_object <- structure(list(), class = "blob_container")
  provider_error <- structure(list(message = "access denied", call = NULL),
    class = c("http_403", "error", "condition"))
  testthat::local_mocked_bindings(
    download_exact_artifact = function(...) stop(provider_error),
    list_files = function(...) stop("exact reader enumerated storage"),
    .package = "finnts"
  )
  expect_error(read_exact_artifact(info, "logs/metadata.csv", allow_missing = TRUE,
    character_columns = "agent_run_id"), class = "http_403")
})

test_that("already rewritten corrupt Agent metadata remains rejected without mutation", {
  fixture <- make_csv_update_fixture(withr::local_tempdir(), "4282d17137126405")
  info <- fixture$previous$project_info
  info$run_name <- fixture$previous$run_id
  path <- local_artifact_path(info, "final_output", "-run_metadata")
  corrupt <- fixture$metadata
  corrupt$agent_run_id <- "4.282e-304"
  utils::write.csv(corrupt, path, row.names = FALSE)
  before <- tools::md5sum(path)
  expect_null(load_completed_agent_run_outputs(fixture$previous))
  expect_identical(tools::md5sum(path), before)
})

test_that("CSV identity schemas allow absent columns without hiding read warnings", {
  path <- fs::path(withr::local_tempdir(), "metadata.csv")
  utils::write.csv(data.frame(combo = "00042", weighted_mape = 0.125),
    path, row.names = FALSE)
  info <- list(path = dirname(path), storage_object = NULL)
  columns <- c("combo", "agent_run_id", "best_run_name")
  expect_warning(metadata <- read_exact_artifact(info, path,
    character_columns = columns), NA)
  expect_identical(metadata$combo, "00042")
  expect_identical(metadata$weighted_mape, 0.125)
  expect_false("agent_run_id" %in% names(metadata))
  expect_false(validate_completed_run_metadata(metadata, "expected-owner"))
  expect_warning(current <- read_update_csv(path, character_columns = columns), NA)
  expect_identical(current$combo, "00042")
  expect_identical(current$weighted_mape, 0.125)
  original_vroom <- vroom::vroom
  local({
    testthat::local_mocked_bindings(
      vroom = function(...) stop("force fixture CSV fallback"), .package = "vroom"
    )
    expect_warning(fallback <- read_exact_artifact(info, path,
      character_columns = columns), NA)
    expect_identical(fallback$combo, "00042")
    expect_identical(fallback$weighted_mape, 0.125)
    original_csv <- utils::read.csv
    testthat::local_mocked_bindings(
      read.csv = function(file, ...) {
        warning("Base CSV parse warning remains visible", call. = FALSE)
        original_csv(file, ...)
      }, .package = "utils"
    )
    expect_warning(read_exact_artifact(info, path, character_columns = columns),
      "Base CSV parse warning remains visible")
    expect_warning(read_update_csv(path, character_columns = columns),
      "Base CSV parse warning remains visible")
  })
  testthat::local_mocked_bindings(
    vroom = function(file, ...) {
      warning("CSV parse warning remains visible", call. = FALSE)
      original_vroom(file, ...)
    }, .package = "vroom"
  )
  expect_warning(read_exact_artifact(info, path, character_columns = "combo"),
    "CSV parse warning remains visible")
})

test_that("real Agent CSV logs preserve predecessor identity in both readers", {
  for (backend in c("vroom", "fallback")) {
    for (run_id in c("4282d17137126405", "0012345678901234")) local({
      fixture <- make_csv_update_fixture(withr::local_tempdir(), run_id)
      original_vroom <- vroom::vroom
      if (identical(backend, "fallback")) {
        testthat::local_mocked_bindings(
          vroom = function(file, ...) {
            if (any(startsWith(as.character(file), fixture$previous$project_info$path))) {
              stop("force fixture CSV fallback")
            }
            original_vroom(file, ...)
          }, .package = "vroom"
        )
      }
      testthat::local_mocked_bindings(
        check_agent_info = function(...) invisible(NULL),
        load_update_runs = function(...) tibble::tibble(),
        get_total_combos = function(...) hash_data(fixture$combo),
        get_best_agent_run = function(...) fixture$metadata,
        .package = "finnts"
      )
      result <- initial_checks(fixture$agent_info)
      expect_identical(result$prev_best_runs_tbl$agent_run_id, run_id)
      expect_identical(result$prev_best_runs_tbl$combo, fixture$combo)
      expect_identical(result$prev_best_runs_tbl$weighted_mape, 0.125)
      expect_equal(analyze_results(fixture$agent_info), 0)
    })
  }
})

test_that("CSV character columns use one payload read and retain other types", {
  for (backend in c("vroom", "fallback")) local({
    path <- fs::path(withr::local_tempdir(), "metadata.csv")
    expected <- data.frame(run_id = "0012345678901234", agent_version = 4,
      weighted_mape = 0.125, hist_end_date = as.Date("2026-07-01"))
    utils::write.csv(expected, path, row.names = FALSE)
    info <- list(path = dirname(path), storage_object = NULL)
    original_vroom <- vroom::vroom
    original_csv <- utils::read.csv
    calls <- c(vroom = 0L, fallback = 0L)
    testthat::local_mocked_bindings(
      vroom = function(file, ...) {
        calls[["vroom"]] <<- calls[["vroom"]] + 1L
        if (identical(backend, "fallback")) stop("force fixture CSV fallback")
        original_vroom(file, ...)
      }, .package = "vroom"
    )
    testthat::local_mocked_bindings(
      read.csv = function(file, ...) {
        calls[["fallback"]] <<- calls[["fallback"]] + 1L
        original_csv(file, ...)
      }, .package = "utils"
    )
    testthat::local_mocked_bindings(
      list_files = function(...) stop("exact-path read enumerated storage"),
      .package = "finnts"
    )
    result <- read_file(info, file_list = path, character_columns = "run_id")
    expect_identical(result$run_id, expected$run_id)
    expect_true(is.numeric(result$agent_version))
    expect_identical(result$weighted_mape, expected$weighted_mape)
    expect_identical(calls, c(vroom = 1L, fallback = as.integer(backend == "fallback")))
    untyped <- read_file(info, file_list = path)
    expect_identical(result[setdiff(names(result), "run_id")],
      untyped[setdiff(names(untyped), "run_id")])
    rds_path <- sub("csv$", "rds", path)
    saveRDS(expected, rds_path)
    expect_equal(read_file(info, file_list = rds_path, character_columns = "run_id"),
      expected)
  })
})

# Run startup against deterministic version metadata and optional real current
# model/forecast artifacts. Returns startup routing; provider errors are injected
# only for the requested final artifact. Temporary paths belong to the caller.
run_initial_checks_case <- function(final_outputs,
                                    intermediate_tables = NULL,
                                    read_error = NULL,
                                    read_counts = NULL,
                                    forecast_approach = "bottoms_up",
                                    combos_by_run = NULL,
                                    artifact_path = NULL) {
  agent_info <- make_update_agent_info(forecast_approach)
  if (!is.null(artifact_path)) {
    agent_info$project_info$path <- artifact_path
    agent_info$project_info$object_output <- "rds"
    agent_info$project_info$date_type <- "month"
  }
  original_reader <- read_file
  agent_runs <- make_update_agent_run_table(forecast_approach)
  run_ids <- agent_runs$run_id
  run_files <- paste0(run_ids, "-agent_run.csv")

  if (is.null(combos_by_run)) {
    combos_by_run <- stats::setNames(
      lapply(run_ids, function(...) c("combo-a", "combo-b")),
      run_ids
    )
  }

  if (is.null(intermediate_tables)) {
    intermediate_tables <- stats::setNames(
      lapply(run_ids, make_update_run_metadata),
      run_ids
    )
    intermediate_tables[[agent_info$run_id]] <- tibble::tibble()
  }

  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    final_agent_artifact_exists = function(...) TRUE,
    hash_data = function(x) x,
    list_files = function(...) run_files,
    read_file = function(run_info,
                         path = NULL,
                         file_list = NULL,
                         return_type = "df",
                         schema = NULL,
                         allow_missing = FALSE,
                         strict = FALSE,
                         character_columns = NULL) {
      artifact <- if (is.null(file_list)) path else file_list
      if (!is.null(artifact_path) && any(file.exists(artifact))) {
        return(original_reader(run_info, path = path, file_list = file_list,
          return_type = return_type, allow_missing = allow_missing, strict = strict))
      }
      if (!is.null(file_list)) {
        return(agent_runs)
      }

      matched_run <- run_ids[vapply(
        run_ids,
        function(run_id) grepl(run_id, path, fixed = TRUE),
        logical(1)
      )]
      if (length(matched_run) == 0) {
        return(tibble::tibble())
      }

      run_id <- matched_run[[1]]
      artifacts <- c(
        "run_metadata", "forecast", "model_summary", "eda",
        "hierarchy_summary"
      )
      matched_artifact <- artifacts[vapply(
        artifacts,
        function(artifact) grepl(paste0("-", artifact, "."), path, fixed = TRUE),
        logical(1)
      )]
      if (length(matched_artifact) == 0) {
        return(tibble::tibble())
      }
      artifact <- matched_artifact[[1]]
      read_key <- paste(run_id, artifact, sep = ":")

      if (!is.null(read_counts)) {
        read_counts[[read_key]] <- (read_counts[[read_key]] %||% 0L) + 1L
      }
      if (!is.null(read_error) &&
        identical(run_id, read_error$run_id) &&
        identical(artifact, read_error$artifact)) {
        stop(read_error$message, call. = FALSE)
      }

      result <- final_outputs[[run_id]][[artifact]]
      if (is.null(result)) {
        return(tibble::tibble())
      }
      result
    },
    load_update_runs = function(agent_info) {
      result <- intermediate_tables[[agent_info$run_id]]
      if (is.null(result)) {
        return(tibble::tibble())
      }
      result
    },
    get_total_combos = function(agent_info) combos_by_run[[agent_info$run_id]],
    .package = "finnts"
  )

  initial_checks(agent_info)
}

# Build current local model/forecast files for the identity-hash startup harness.
# corrupt lists series whose model stream is malformed. Returns the temp path,
# scoped to the calling test; no predecessor or preparation files are changed.
make_current_update_files <- function(corrupt = character()) {
  path <- withr::local_tempdir(.local_envir = parent.frame())
  fs::dir_create(fs::path(path, c("models", "forecasts", "logs", "prep_data", "prep_models")))
  model_id <- "lm--local--R1"
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  for (combo in c("combo-a", "combo-b")) {
    prefix <- paste0("project_", combo, "-agent_run-5_", combo, "-", combo)
    model_path <- fs::path(path, "models", paste0(prefix, "-single_models.rds"))
    if (!combo %in% corrupt) {
      saveRDS(tibble::tibble(Combo_ID = combo, Model_ID = model_id,
        Model_Name = "linear_reg", Model_Type = "local", Recipe_ID = "R1",
        Model_Fit = list(fit)), model_path)
    } else {
      writeBin(charToRaw("not a serialized model"), model_path)
    }
    forecasts <- data.frame(Combo = combo, Model_ID = model_id,
      Model_Name = "linear_reg", Model_Type = "local", Recipe_ID = "R1",
      Train_Test_ID = rep(c(1, 2), each = 6),
      Date = rep(seq(as.Date("2026-08-01"), by = "month", length.out = 6), 2),
      Target = rep(c(NA_real_, 100), each = 6), Forecast = 100,
      Best_Model = "Yes")
    utils::write.csv(forecasts,
      fs::path(path, "forecasts", paste0(prefix, "-single_models.csv")), row.names = FALSE)
    run_prefix <- paste0("project_", combo, "-agent_run-5_", combo)
    utils::write.csv(data.frame(date_type = "month", recipes_to_run = "R1",
      hist_end_date = as.Date("2027-01-01")),
      fs::path(path, "logs", paste0(run_prefix, ".csv")), row.names = FALSE)
    utils::write.csv(data.frame(Combo = combo, Date = unique(forecasts$Date), Target = 100),
      fs::path(path, "prep_data", paste0(prefix, "-R1.csv")), row.names = FALSE)
    utils::write.csv(data.frame(Train_Test_ID = c(1, 2), Run_Type = c("Future_Forecast", "Back_Test"),
      Train_End = as.Date("2026-07-01"), Test_End = as.Date("2027-01-01")),
      fs::path(path, "prep_models", paste0(run_prefix, "-train_test_split.csv")), row.names = FALSE)
  }
  path
}

test_that("current completion metadata cannot hide a corrupt fitted model", {
  path <- make_current_update_files(corrupt = "combo-b")
  runs <- make_update_agent_run_table()$run_id
  metadata <- stats::setNames(lapply(runs, make_update_run_metadata), runs)
  result <- run_initial_checks_case(
    final_outputs = list("run-4" = make_complete_final_outputs("run-4")),
    intermediate_tables = metadata, artifact_path = path)
  expect_false(identical(result, "no updates required"))
  if (is.list(result)) {
    expect_equal(result$prev_best_runs_tbl$combo, "combo-b")
  }
})

test_that("initial_checks skips a canceled immediate predecessor", {
  result <- run_initial_checks_case(list(
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-3", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("incomplete current forecasts remain eligible for refitting", {
  path <- make_current_update_files()
  forecast_path <- fs::path(path, "forecasts",
    "project_combo-b-agent_run-5_combo-b-combo-b-single_models.csv")
  rows <- utils::read.csv(forecast_path)
  utils::write.csv(rows[-1, ], forecast_path, row.names = FALSE)
  runs <- make_update_agent_run_table()$run_id
  result <- run_initial_checks_case(
    final_outputs = list("run-4" = make_complete_final_outputs("run-4")),
    intermediate_tables = stats::setNames(lapply(runs, make_update_run_metadata), runs),
    artifact_path = path)
  expect_false(identical(result, "no updates required"))
  if (is.list(result)) expect_equal(result$prev_best_runs_tbl$combo, "combo-b")
})

test_that("completion requires backtests and valid forecast dates", {
  path <- make_current_update_files()
  rows <- utils::read.csv(fs::path(path, "forecasts",
    "project_combo-a-agent_run-5_combo-a-combo-a-single_models.csv"))
  models <- readRDS(fs::path(path, "models",
    "project_combo-a-agent_run-5_combo-a-combo-a-single_models.rds"))
  expect_true(valid_update_forecasts(rows, models, 6))
  expect_false(valid_update_forecasts(rows[rows$Train_Test_ID == 1, ], models, 6))
  rows$Date[1] <- "invalid-date"
  expect_false(valid_update_forecasts(rows, models, 6))
})

test_that("provider read errors cannot be classified as corrupt models", {
  info <- list(storage_object = structure(list(), class = "blob_container"))
  local_mocked_bindings(download_exact_artifact = function(...) {
    rlang::abort("error reading from connection", class = "http_503")
  })
  expect_error(read_update_artifact(info, "models/current.rds"), class = "http_503")
})

test_that("downloaded malformed model content is recoverable without hiding provider errors", {
  info <- list(storage_object = structure(list(), class = "blob_container"))
  local_mocked_bindings(download_exact_artifact = function(storage_object, path, destination, allow_missing) {
    writeBin(charToRaw("broken model"), destination)
    TRUE
  })
  expect_null(read_update_artifact(info, "models/current.rds"))
})

test_that("update wrappers preserve failures from current completion reads", {
  results <- list()
  for (global in c(FALSE, TRUE)) for (failure in c("log", "completion", "empty", "conflict")) local({
    agent <- make_update_agent_info()
    agent$project_info$storage_object <- structure(list(), class = "blob_container")
    info <- agent$project_info
    info$run_name <- "current-fit"
    previous <- make_update_run_metadata("run-4", "west")
    previous$models_to_run <- "lm"
    previous$model_type <- if (global) "global" else "local"
    original_reader <- read_file
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      par_end = function(...) NULL,
      read_file = function(run_info, ...) {
        if (!is.null(run_info$storage_object)) return(previous)
        original_reader(run_info, ...)
      },
      read_update_result = function(...) list(models = data.frame(Model_ID = "lm")),
      download_exact_artifact = function(storage_object, path, destination, allow_missing) {
        completion <- grepl("-agent_best_run.csv", path, fixed = TRUE)
        if ((failure == "log" && !completion) || (failure == "completion" && completion)) {
          rlang::abort("completion storage unavailable", class = "http_503")
        }
        rows <- if (completion) data.frame(agent_run_id = agent$run_id, combo = "west",
          best_run_name = "conflicting-fit", model_type = previous$model_type, weighted_mape = 0.1) else
          data.frame(weighted_mape = 0.1)
        if (completion && failure == "empty") rows <- rows[0, ]
        utils::write.csv(rows, destination, row.names = FALSE)
        TRUE
      },
      update_forecast_combo = function(...) {
        resume_update_result(agent, info, "west", global, data.frame(), "lm")
      }
    )
    wrapper <- if (global) update_global_models else update_local_models
    results[[paste(global, failure)]] <<- tryCatch(
      wrapper(agent, previous, NULL, FALSE, 1, 123), error = identity)
  })
  expect_true(all(vapply(results, inherits, logical(1), "finnts_update_artifact_error")))
  provider <- results[grepl("log$|completion$", names(results))]
  expect_true(all(vapply(provider, inherits, logical(1), "http_503")))
  expect_true(all(vapply(provider, function(error) inherits(error$parent, "http_503"), logical(1))))
  for (global in c(FALSE, TRUE)) {
    expect_match(conditionMessage(results[[paste(global, "empty")]]), "empty or unreadable")
    expect_match(conditionMessage(results[[paste(global, "conflict")]]), "conflicts with the saved update")
  }
})

test_that("absent or damaged current run logs request refitting", {
  info <- list(project_name = "project", run_name = "current", path = withr::local_tempdir(),
    data_output = "csv", storage_object = NULL)
  fs::dir_create(fs::path(info$path, "logs"))
  agent <- make_update_agent_info()
  local_mocked_bindings(read_update_result = function(...) list(models = data.frame(Model_ID = "lm")))
  missing <- tryCatch(resume_update_result(agent, info, "west", FALSE, data.frame(), "lm"), error = identity)
  writeLines(character(), local_artifact_path(info, "logs", extension = "csv"))
  damaged <- tryCatch(resume_update_result(agent, info, "west", FALSE, data.frame(), "lm"), error = identity)
  expect_identical(list(missing, damaged), list(FALSE, FALSE))
})

test_that("local dispatch serializes only its existing free-variable context", {
  agent <- make_update_agent_info()
  previous <- make_update_run_metadata("run-4", c("east", "west"))
  previous$models_to_run <- "lm"
  lean <- agent
  lean$llm <- NULL
  captured <- NULL
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(),
      foreach_operator = function(iterator, expression) {
        expression <- substitute(expression)
        environment <- parent.frame()
        symbols <- intersect(all.vars(expression), ls(envir = environment, all.names = TRUE))
        globals <- mget(symbols, envir = environment, inherits = FALSE)
        captured <<- list(expression = expression, globals = globals, iterator_names = iterator$argnames)
        list(data.frame(Combo = "east"), data.frame(Combo = "west"))
      }),
    par_end = function(...) NULL
  )
  update_local_models(agent, previous, NULL, FALSE, 1, 123)
  expected <- list(agent_info_lean = lean, project_info = agent$project_info,
    prev_run_id = "run-4", num_cores = 1, inner_parallel = FALSE, seed = 123)
  expect_setequal(names(captured$globals), names(expected))
  expect_equal(length(serialize(captured[c("expression", "globals")], NULL)),
    length(serialize(list(expression = captured$expression,
      globals = expected[names(captured$globals)]), NULL)))
  expect_identical(captured$iterator_names, "combo")
})

test_that("mixed update dispatch keeps grouped globals and lean local workers", {
  agent <- make_update_agent_info()
  previous <- make_update_run_metadata("run-4", c("north", "south", "east", "west"))
  previous$model_type <- c("global", "global", "local", "local")
  previous$best_run_name[1:2] <- "one-global-run"
  previous$models_to_run <- "lm"
  calls <- list()
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL,
    hash_data = function(value) value,
    read_file = function(run_info, file_list, ...) {
      combo <- if (grepl("-east-agent_best_run", file_list)) "east" else "west"
      previous[previous$combo == combo, ]
    },
    update_forecast_combo = function(agent_info, prev_best_run_tbl, ...) {
      calls[[length(calls) + 1L]] <<- list(combos = prev_best_run_tbl$combo,
        context = names(agent_info), bytes = length(serialize(agent_info, NULL)))
      list(status = "done", quality_rejected_combos = character())
    }
  )
  update_global_models(agent, previous, NULL, FALSE, 1, 123)
  update_local_models(agent, previous, NULL, FALSE, 1, 123)
  expect_length(calls, 3L)
  expect_identical(calls[[1]]$combos, c("north", "south"))
  lean <- agent
  lean$llm <- NULL
  for (index in 2:3) {
    expect_length(calls[[index]]$combos, 1L)
    expect_identical(calls[[index]]$context, names(lean))
    expect_equal(calls[[index]]$bytes, length(serialize(lean, NULL)))
  }
})

test_that("partial global completion retains the entire shared update group", {
  path <- make_current_update_files()
  info <- list(project_name = "project_all", run_name = "current-global", path = path,
    data_output = "csv", object_output = "rds")
  local_models <- readRDS(fs::path(path, "models",
    "project_combo-a-agent_run-5_combo-a-combo-a-single_models.rds"))
  local_models$Model_ID <- "lm--global--R1"
  local_models$Model_Type <- "global"
  rows <- utils::read.csv(fs::path(path, "forecasts",
    "project_combo-a-agent_run-5_combo-a-combo-a-single_models.csv"))
  rows$Model_ID <- "lm--global--R1"
  rows$Model_Type <- "global"
  saveRDS(local_models, fs::path(path, "models", "project_all-current-global-All-Data-single_models.rds"))
  utils::write.csv(rows, fs::path(path, "forecasts", "project_all-current-global-combo-a-global_models.csv"),
    row.names = FALSE)
  fs::file_copy(fs::path(path, "logs", "project_combo-a-agent_run-5_combo-a.csv"),
    fs::path(path, "logs", "project_all-current-global.csv"))
  fs::file_copy(fs::path(path, "prep_models", "project_combo-a-agent_run-5_combo-a-train_test_split.csv"),
    fs::path(path, "prep_models", "project_all-current-global-train_test_split.csv"))
  fs::file_copy(fs::path(path, "prep_data", "project_combo-a-agent_run-5_combo-a-combo-a-R1.csv"),
    fs::path(path, "prep_data", "project_all-current-global-combo-a-R1.csv"))
  current <- make_update_run_metadata("run-5", "combo-a")
  current$model_type <- "global"
  current$best_run_name <- "current-global"
  previous <- make_complete_final_outputs("run-4")
  previous$run_metadata$model_type <- "global"
  previous$run_metadata$best_run_name <- "previous-global"
  result <- run_initial_checks_case(list("run-4" = previous),
    intermediate_tables = list("run-5" = current), artifact_path = path)
  expect_setequal(result$prev_best_runs_tbl$combo, c("combo-a", "combo-b"))
})

test_that("update wrappers propagate storage failures instead of default fitting", {
  agent <- make_update_agent_info()
  previous <- make_update_run_metadata("run-4", "west")
  previous$models_to_run <- "lm"
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL,
    read_file = function(...) previous,
    update_forecast_combo = function(...) {
      rlang::abort("storage unavailable", class = "finnts_update_artifact_error")
    }
  )
  expect_error(update_local_models(agent, previous, NULL, FALSE, 1, 123),
    class = "finnts_update_artifact_error")
  previous$model_type <- "global"
  expect_error(update_global_models(agent, previous, NULL, FALSE, 1, 123),
    class = "finnts_update_artifact_error")
})

test_that("initial_checks selects a finalized immediate predecessor", {
  result <- run_initial_checks_case(list(
    "run-4" = make_complete_final_outputs("run-4"),
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks excludes removed bottom-up combos from update routing", {
  run_ids <- paste0("run-", 5:1)
  combos_by_run <- stats::setNames(
    lapply(run_ids, function(...) c("combo-a", "combo-b")),
    run_ids
  )
  combos_by_run[["run-5"]] <- c("combo-a", "combo-b", "combo-new")
  combos_by_run[["run-4"]] <- c(
    "combo-a", "combo-b", "combo-removed-local", "combo-removed-global"
  )

  run4_outputs <- make_complete_final_outputs(
    "run-4",
    combos = combos_by_run[["run-4"]]
  )
  run4_outputs$run_metadata$model_type <- c(
    "local", "global", "local", "global"
  )

  result <- run_initial_checks_case(
    final_outputs = list("run-4" = run4_outputs),
    combos_by_run = combos_by_run
  )

  expect_setequal(result$prev_best_runs_tbl$combo, c("combo-a", "combo-b"))
  expect_identical(result$new_combos, "combo-new")
})

test_that("initial_checks preserves hierarchy aggregates but drops removed bottoms", {
  run_ids <- paste0("run-", 5:1)
  combos_by_run <- stats::setNames(
    lapply(run_ids, function(...) c("combo-a", "combo-b")),
    run_ids
  )
  combos_by_run[["run-4"]] <- c("combo-a", "combo-removed")

  run4_outputs <- make_complete_final_outputs(
    "run-4",
    combos = c("Total", "combo-a", "combo-removed"),
    forecast_approach = "standard_hierarchy"
  )
  run4_outputs$run_metadata$model_type <- c("global", "local", "global")
  run4_outputs$hierarchy_summary <- data.frame(
    Hierarchy_Combo = c("Total", "Total", "combo-a", "combo-removed"),
    Hierarchy_Level_Type = c("Total", "Total", "Bottom", "Bottom"),
    Bottom_Combo = c("combo-a", "combo-removed", "combo-a", "combo-removed"),
    Is_Bottom = c(FALSE, FALSE, TRUE, TRUE),
    Parent_Level = c(NA, NA, "Total", "Total"),
    stringsAsFactors = FALSE
  )

  result <- run_initial_checks_case(
    final_outputs = list("run-4" = run4_outputs),
    forecast_approach = "standard_hierarchy",
    combos_by_run = combos_by_run
  )

  expect_setequal(result$prev_best_runs_tbl$combo, c("Total", "combo-a"))
  expect_identical(result$new_combos, "combo-b")
})

test_that("update failure fallback keeps only current input combos", {
  agent_info <- make_update_agent_info("standard_hierarchy")

  testthat::local_mocked_bindings(
    resolve_combo_hashes = function(agent_info, combo_hashes) combo_hashes,
    .package = "finnts"
  )

  result <- check_update_failures(
    agent_info = agent_info,
    previous_best_run_tbl = data.frame(
      combo = c("Total", "combo-a", "combo-removed")
    ),
    current_run_combos = c("combo-a", "combo-b"),
    global_failed_combos = c("Total", "combo-a", "combo-removed"),
    local_failed_combos = character(0)
  )

  expect_identical(result, "combo-a")
})

test_that("initial_checks retry stops after all current combos finish", {
  path <- make_current_update_files()
  result <- run_initial_checks_case(
    final_outputs = list(),
    intermediate_tables = list(
      "run-5" = make_update_run_metadata("run-5")
    ),
    artifact_path = path
  )

  expect_identical(result, "no updates required")
})

test_that("initial_checks accepts nonempty metadata without matching input combos", {
  run4_outputs <- make_complete_final_outputs("run-4")
  run4_outputs$run_metadata <- make_update_run_metadata(
    "run-4",
    combos = "combo-a"
  )
  result <- run_initial_checks_case(list(
    "run-4" = run4_outputs,
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks reports when no finalized predecessor exists", {
  expect_error(
    run_initial_checks_case(list()),
    paste0(
      "No completed previous agent run found.*",
      "at least one prior agent version finished successfully"
    )
  )
})

test_that("initial_checks propagates final metadata provider failures", {
  expect_error(
    run_initial_checks_case(
      final_outputs = list(
        "run-3" = make_complete_final_outputs("run-3")
      ),
      read_error = list(
        run_id = "run-4",
        artifact = "run_metadata",
        message = "storage provider unavailable"
      )
    ),
    "storage provider unavailable"
  )
})

test_that("initial_checks reads selected final metadata once", {
  read_counts <- new.env(parent = emptyenv())

  result <- run_initial_checks_case(
    final_outputs = list(
      "run-4" = make_complete_final_outputs("run-4")
    ),
    read_counts = read_counts
  )

  expect_s3_class(result$prev_best_runs_tbl, "data.frame")
  expect_identical(read_counts[["run-4:run_metadata"]], 1L)
  expect_identical(read_counts[["run-4:forecast"]], 1L)
  expect_identical(read_counts[["run-4:model_summary"]], 1L)
  expect_identical(read_counts[["run-4:eda"]], 1L)
})

test_that("initial_checks skips missing or empty required final outputs", {
  cases <- list(
    missing_forecast = list(artifact = "forecast", value = NULL),
    empty_model_summary = list(artifact = "model_summary", value = tibble::tibble()),
    missing_eda = list(artifact = "eda", value = NULL)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    run4_outputs <- make_complete_final_outputs("run-4")
    run4_outputs[case$artifact] <- list(case$value)

    result <- run_initial_checks_case(list(
      "run-4" = run4_outputs,
      "run-3" = make_complete_final_outputs("run-3")
    ))

    expect_true(
      all(grepl("run-3", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)),
      info = case_name
    )
  }
})

test_that("initial_checks does not compare forecast contents with metadata", {
  run4_outputs <- make_complete_final_outputs("run-4")
  run4_outputs$forecast <- data.frame(output = "available")

  result <- run_initial_checks_case(list(
    "run-4" = run4_outputs,
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks accepts artifact-local best model IDs", {
  run4_outputs <- make_complete_final_outputs("run-4")
  run4_outputs$forecast <- make_update_forecast(
    model_ids = c("Best-Model", "model-combo-b")
  )
  run4_outputs$model_summary <- make_update_model_summary(
    model_ids = c("underlying-model-a", "model-combo-b")
  )

  result <- run_initial_checks_case(list(
    "run-4" = run4_outputs,
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks does not compare model-summary contents with metadata", {
  run4_outputs <- make_complete_final_outputs("run-4")
  run4_outputs$model_summary <- data.frame(output = "available")

  result <- run_initial_checks_case(list(
    "run-4" = run4_outputs,
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks conditionally requires hierarchy output", {
  hierarchical_run4 <- make_complete_final_outputs(
    "run-4",
    forecast_approach = "standard_hierarchy"
  )
  hierarchical_run4$hierarchy_summary <- NULL

  hierarchical_result <- run_initial_checks_case(
    final_outputs = list(
      "run-4" = hierarchical_run4,
      "run-3" = make_complete_final_outputs(
        "run-3",
        forecast_approach = "standard_hierarchy"
      )
    ),
    forecast_approach = "standard_hierarchy"
  )
  bottom_up_result <- run_initial_checks_case(list(
    "run-4" = make_complete_final_outputs("run-4")
  ))

  expect_true(all(grepl(
    "run-3",
    hierarchical_result$prev_best_runs_tbl$best_run_name,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    "run-4",
    bottom_up_result$prev_best_runs_tbl$best_run_name,
    fixed = TRUE
  )))
})

test_that("initial_checks accepts producer-shaped hierarchical final outputs", {
  for (forecast_approach in c("standard_hierarchy", "grouped_hierarchy")) {
    run4_outputs <- make_complete_final_outputs(
      "run-4",
      forecast_approach = forecast_approach
    )
    run4_outputs$run_metadata <- make_update_run_metadata(
      "run-4",
      combos = c("Total", "combo-a", "combo-b")
    )
    run4_outputs$forecast <- make_update_forecast(
      combos = c("combo-a", "combo-b")
    )
    run4_outputs$model_summary <- data.frame(output = "available")
    run4_outputs$eda <- data.frame(output = "available")
    run4_outputs$hierarchy_summary <- data.frame(output = "available")

    result <- run_initial_checks_case(
      final_outputs = list(
        "run-4" = run4_outputs,
        "run-3" = make_complete_final_outputs(
          "run-3",
          forecast_approach = forecast_approach
        )
      ),
      forecast_approach = forecast_approach
    )

    expect_true(
      all(grepl(
        "run-4",
        result$prev_best_runs_tbl$best_run_name,
        fixed = TRUE
      )),
      info = forecast_approach
    )
  }
})

test_that("initial_checks requires reusable final run metadata", {
  invalid_metadata <- list(
    missing_combo = within(make_update_run_metadata("run-4"), rm(combo)),
    missing_agent_run_id = within(make_update_run_metadata("run-4"), rm(agent_run_id)),
    missing_best_run_name = within(make_update_run_metadata("run-4"), rm(best_run_name)),
    missing_model_type = within(make_update_run_metadata("run-4"), rm(model_type)),
    missing_weighted_mape = within(make_update_run_metadata("run-4"), rm(weighted_mape)),
    blank_combo = transform(make_update_run_metadata("run-4"), combo = c("", "combo-b")),
    blank_agent_run_id = transform(make_update_run_metadata("run-4"), agent_run_id = ""),
    wrong_agent_run_id = transform(make_update_run_metadata("run-4"), agent_run_id = "run-other"),
    blank_best_run_name = transform(make_update_run_metadata("run-4"), best_run_name = ""),
    invalid_model_type = transform(make_update_run_metadata("run-4"), model_type = "other"),
    missing_weighted_mape_value = transform(make_update_run_metadata("run-4"), weighted_mape = NA_real_),
    infinite_weighted_mape = transform(make_update_run_metadata("run-4"), weighted_mape = Inf),
    negative_weighted_mape = transform(make_update_run_metadata("run-4"), weighted_mape = -0.1),
    duplicate_combo = transform(make_update_run_metadata("run-4"), combo = "combo-a")
  )

  for (case_name in names(invalid_metadata)) {
    run4_outputs <- make_complete_final_outputs("run-4")
    run4_outputs$run_metadata <- invalid_metadata[[case_name]]

    result <- tryCatch(
      run_initial_checks_case(list(
        "run-4" = run4_outputs,
        "run-3" = make_complete_final_outputs("run-3")
      )),
      error = identity
    )

    expect_false(inherits(result, "error"), info = case_name)
    if (!inherits(result, "error")) {
      expect_true(
        all(grepl(
          "run-3",
          result$prev_best_runs_tbl$best_run_name,
          fixed = TRUE
        )),
        info = case_name
      )
    }
  }
})

test_that("optional remote final artifacts distinguish missing from provider failures", {
  make_remote_agent_info <- function(storage_class) {
    agent_info <- make_update_agent_info()
    agent_info$project_info$storage_object <- structure(list(), class = storage_class)
    agent_info
  }

  load_missing_artifact <- function(storage_class) {
    testthat::local_mocked_bindings(
      list_files = function(...) character(),
      read_file = function(...) stop("missing artifact was read", call. = FALSE),
      .package = "finnts"
    )

    load_final_agent_artifact(
      agent_info = make_remote_agent_info(storage_class),
      suffix = "forecast",
      allow_missing = TRUE
    )
  }

  load_with_provider_failure <- function(storage_class) {
    testthat::local_mocked_bindings(
      list_files = function(...) stop("storage provider unavailable", call. = FALSE),
      read_file = function(...) data.frame(output = "available"),
      .package = "finnts"
    )

    load_final_agent_artifact(
      agent_info = make_remote_agent_info(storage_class),
      suffix = "forecast",
      allow_missing = TRUE
    )
  }

  for (storage_class in c("blob_container", "ms_drive")) {
    result <- load_missing_artifact(storage_class)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0, info = storage_class)
    expect_error(
      load_with_provider_failure(storage_class),
      "storage provider unavailable",
      fixed = TRUE,
      info = storage_class
    )
  }
})

test_that("optional local final artifacts are checked before reading", {
  agent_info <- make_update_agent_info()
  agent_info$project_info$path <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    read_file = function(...) stop("missing artifact was read", call. = FALSE),
    .package = "finnts"
  )

  result <- load_final_agent_artifact(
    agent_info = agent_info,
    suffix = "forecast",
    allow_missing = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("unreadable final artifacts remain hard errors", {
  agent_info <- make_update_agent_info()
  agent_info$project_info$path <- withr::local_tempdir()
  artifact_path <- fs::path(
    agent_info$project_info$path,
    "final_output",
    paste0(
      hash_data(agent_info$project_info$project_name), "-",
      hash_data(agent_info$run_id), "-forecast.csv"
    )
  )
  fs::dir_create(fs::path_dir(artifact_path))
  file.create(artifact_path)

  testthat::local_mocked_bindings(
    read_file = function(...) {
      warning(paste0("Skipping empty or unreadable file: ", artifact_path))
      tibble::tibble()
    },
    .package = "finnts"
  )

  expect_error(
    load_final_agent_artifact(
      agent_info = agent_info,
      suffix = "forecast",
      allow_missing = TRUE
    ),
    "Skipping empty or unreadable file",
    fixed = TRUE
  )
})

test_that("initial_checks propagates required final-output read failures", {
  for (case in list(
    list(artifact = "forecast", message = "storage provider unavailable"),
    list(artifact = "model_summary", message = "cannot parse model summary")
  )) {
    expect_error(
      run_initial_checks_case(
        final_outputs = list(
          "run-4" = make_complete_final_outputs("run-4"),
          "run-3" = make_complete_final_outputs("run-3")
        ),
        read_error = list(
          run_id = "run-4",
          artifact = case$artifact,
          message = case$message
        )
      ),
      case$message,
      fixed = TRUE
    )
  }
})

test_that("analyze_results uses finalized previous versions", {
  agent_info <- make_update_agent_info()
  agent_runs <- make_update_agent_run_table()
  run_ids <- agent_runs$run_id
  run_files <- paste0(run_ids, "-agent_run.csv")
  combos_by_run <- stats::setNames(lapply(run_ids, function(...) "combo-a"), run_ids)
  intermediate_tables <- stats::setNames(
    lapply(run_ids, function(run_id) {
      make_update_run_metadata(run_id, combos = "combo-a", weighted_mape = 0.1)
    }),
    run_ids
  )
  intermediate_tables[["run-4"]] <- make_update_run_metadata(
    "run-4",
    combos = "combo-a",
    weighted_mape = 1
  )
  final_outputs <- list(
    "run-5" = make_complete_final_outputs("run-5", combos = "combo-a", weighted_mape = 0.15),
    "run-4" = make_complete_final_outputs("run-4", combos = "combo-a", weighted_mape = 1),
    "run-3" = make_complete_final_outputs("run-3", combos = "combo-a", weighted_mape = 0.1),
    "run-2" = make_complete_final_outputs("run-2", combos = "combo-a", weighted_mape = 0.1),
    "run-1" = make_complete_final_outputs("run-1", combos = "combo-a", weighted_mape = 0.1)
  )
  final_outputs[["run-4"]]$forecast <- NULL

  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    final_agent_artifact_exists = function(...) TRUE,
    hash_data = function(x) x,
    list_files = function(...) run_files,
    read_file = function(run_info,
                         path = NULL,
                         file_list = NULL,
                         return_type = "df",
                         schema = NULL,
                         allow_missing = FALSE,
                         character_columns = NULL) {
      if (!is.null(file_list)) {
        return(agent_runs)
      }

      matched_run <- run_ids[vapply(
        run_ids,
        function(run_id) grepl(run_id, path, fixed = TRUE),
        logical(1)
      )]
      if (length(matched_run) == 0) {
        return(tibble::tibble())
      }
      artifacts <- c(
        "run_metadata", "forecast", "model_summary", "eda",
        "hierarchy_summary"
      )
      matched_artifact <- artifacts[vapply(
        artifacts,
        function(artifact) grepl(paste0("-", artifact, "."), path, fixed = TRUE),
        logical(1)
      )]
      if (length(matched_artifact) == 0) {
        return(tibble::tibble())
      }

      result <- final_outputs[[matched_run[[1]]]][[matched_artifact[[1]]]]
      if (is.null(result)) {
        return(tibble::tibble())
      }
      result
    },
    load_best_agent_run = function(agent_info) intermediate_tables[[agent_info$run_id]],
    get_total_combos = function(agent_info) combos_by_run[[agent_info$run_id]],
    .package = "finnts"
  )

  expect_equal(analyze_results(agent_info), 100)
})