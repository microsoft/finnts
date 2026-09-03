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

run_initial_checks_case <- function(final_outputs,
                                    intermediate_tables = NULL,
                                    read_error = NULL,
                                    read_counts = NULL,
                                    forecast_approach = "bottoms_up") {
  agent_info <- make_update_agent_info(forecast_approach)
  agent_runs <- make_update_agent_run_table(forecast_approach)
  run_ids <- agent_runs$run_id
  run_files <- paste0(run_ids, "-agent_run.csv")
  combos_by_run <- stats::setNames(
    lapply(run_ids, function(...) c("combo-a", "combo-b")),
    run_ids
  )

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
                         allow_missing = FALSE) {
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
    load_best_agent_run = function(agent_info) {
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

test_that("initial_checks skips a canceled immediate predecessor", {
  result <- run_initial_checks_case(list(
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-3", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks selects a finalized immediate predecessor", {
  result <- run_initial_checks_case(list(
    "run-4" = make_complete_final_outputs("run-4"),
    "run-3" = make_complete_final_outputs("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
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
                         allow_missing = FALSE) {
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