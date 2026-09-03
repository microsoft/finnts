make_update_agent_run_table <- function() {
  data.frame(
    agent_version = 5:1,
    run_id = paste0("run-", 5:1),
    forecast_approach = "bottoms_up",
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

make_update_agent_info <- function() {
  list(
    agent_version = 5,
    run_id = "run-5",
    overwrite = TRUE,
    forecast_approach = "bottoms_up",
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
    combo = combos,
    model_type = "local",
    best_run_name = paste0("agent_", run_id, "_", combos),
    weighted_mape = weighted_mape,
    stringsAsFactors = FALSE
  )
}

run_initial_checks_case <- function(final_tables,
                                    intermediate_tables = NULL,
                                    provider_error_run = NULL,
                                    read_counts = NULL) {
  agent_info <- make_update_agent_info()
  agent_runs <- make_update_agent_run_table()
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
      if (!is.null(read_counts)) {
        read_counts[[run_id]] <- (read_counts[[run_id]] %||% 0L) + 1L
      }
      if (identical(run_id, provider_error_run)) {
        stop("storage provider unavailable", call. = FALSE)
      }

      result <- final_tables[[run_id]]
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
    "run-3" = make_update_run_metadata("run-3")
  ))

  expect_true(all(grepl("run-3", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks selects a finalized immediate predecessor", {
  result <- run_initial_checks_case(list(
    "run-4" = make_update_run_metadata("run-4"),
    "run-3" = make_update_run_metadata("run-3")
  ))

  expect_true(all(grepl("run-4", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
})

test_that("initial_checks skips incomplete final metadata", {
  result <- run_initial_checks_case(list(
    "run-4" = make_update_run_metadata("run-4", combos = "combo-a"),
    "run-3" = make_update_run_metadata("run-3")
  ))

  expect_true(all(grepl("run-3", result$prev_best_runs_tbl$best_run_name, fixed = TRUE)))
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
      final_tables = list(
        "run-3" = make_update_run_metadata("run-3")
      ),
      provider_error_run = "run-4"
    ),
    "storage provider unavailable"
  )
})

test_that("initial_checks reads selected final metadata once", {
  read_counts <- new.env(parent = emptyenv())

  result <- run_initial_checks_case(
    final_tables = list(
      "run-4" = make_update_run_metadata("run-4")
    ),
    read_counts = read_counts
  )

  expect_s3_class(result$prev_best_runs_tbl, "data.frame")
  expect_identical(read_counts[["run-4"]], 1L)
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
  final_tables <- list(
    "run-5" = make_update_run_metadata("run-5", combos = "combo-a", weighted_mape = 0.15),
    "run-3" = make_update_run_metadata("run-3", combos = "combo-a", weighted_mape = 0.1),
    "run-2" = make_update_run_metadata("run-2", combos = "combo-a", weighted_mape = 0.1),
    "run-1" = make_update_run_metadata("run-1", combos = "combo-a", weighted_mape = 0.1)
  )

  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
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
      if (length(matched_run) == 0 || is.null(final_tables[[matched_run[[1]]]])) {
        return(tibble::tibble())
      }
      final_tables[[matched_run[[1]]]]
    },
    load_best_agent_run = function(agent_info) intermediate_tables[[agent_info$run_id]],
    get_total_combos = function(agent_info) combos_by_run[[agent_info$run_id]],
    .package = "finnts"
  )

  expect_equal(analyze_results(agent_info), 100)
})