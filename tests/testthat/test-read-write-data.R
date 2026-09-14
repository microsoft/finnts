test_that("read_file reports a missing artifact instead of subscripting an empty list", {
  run_info <- list(
    storage_object = NULL,
    path = withr::local_tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  expect_error(
    read_file(
      run_info = run_info,
      file_list = character(0),
      return_type = "df"
    ),
    paste0(
      "No files matched the requested Finn artifact.*",
      "missing, incomplete, or stored under an inconsistent cached identifier"
    )
  )
})

test_that("read_file retains matched empty CSV behavior", {
  output_path <- withr::local_tempfile(fileext = ".csv")
  file.create(output_path)
  run_info <- list(
    storage_object = NULL,
    path = fs::path_dir(output_path),
    data_output = "csv",
    object_output = "rds"
  )

  expect_no_error(
    result <- suppressWarnings(read_file(
      run_info = run_info,
      file_list = output_path,
      return_type = "df"
    ))
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("read_file permits an explicitly optional missing table", {
  run_info <- list(
    storage_object = NULL,
    path = withr::local_tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  result <- read_file(
    run_info = run_info,
    file_list = character(0),
    return_type = "df",
    allow_missing = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("selection reads accept a confirmed missing optional artifact on", provider), {
    transport <- local_artifact_provider(provider)
    run_info <- artifact_test_run("remote-only-artifacts")
    run_info$storage_object <- transport$storage_object

    result <- tryCatch(read_selection_file(run_info, "forecasts", "-average_models", "A",
      optional = TRUE), error = identity)

    expect_s3_class(result, "data.frame")
    if (is.data.frame(result)) expect_equal(nrow(result), 0L)
    expect_equal(transport$root_checks, 1L)
  })
}

for (provider in c("blob_container", "ms_drive")) {
  for (format in c("csv", "rds", "parquet")) {
    test_that(paste("exact", provider, "reads download and parse", format, "once"), {
      if (format == "parquet") skip_if_not_installed("arrow")
      source <- artifact_test_run(withr::local_tempdir(), format)
      expected <- artifact_test_recipe(source, "A", "R1")
      remote <- source
      remote$path <- "provider-artifacts"
      transport <- local_artifact_provider(provider)
      remote$storage_object <- transport$storage_object
      path <- artifact_test_path(remote, "forecasts", "A", "-average_models")
      transport$files[[as.character(path)]] <- artifact_test_path(source, "prep_data", "A", "-R1")
      cache <- new.env(parent = emptyenv())

      result <- read_selection_file(remote, "forecasts", "-average_models", "A", cache = cache)
      cached <- read_selection_file(remote, "forecasts", "-average_models", "A", cache = cache)

      expect_equal(result, expected)
      expect_equal(cached, expected)
      expect_identical(transport$lookups, as.character(path))
      expect_identical(transport$downloads, as.character(path))
      expect_equal(transport$root_checks, 0L)
      transport$files[[as.character(path)]] <- NULL
      expect_equal(nrow(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE)), 0L)
      expect_error(read_selection_file(remote, "forecasts", "-average_models", "A"), class = "http_404")
      transport$files[[as.character(path)]] <- artifact_test_path(source, "prep_data", "A", "-R1")
      expect_equal(read_selection_file(remote, "forecasts", "-average_models", "A"), expected)
      expect_length(unique(transport$destinations), 2L)
    })
  }

  test_that(paste("optional", provider, "reads propagate provider and parsing failures"), {
    source <- artifact_test_run(withr::local_tempdir(), "rds")
    artifact_test_recipe(source, "A", "R1")
    remote <- source
    remote$path <- "provider-artifacts"
    transport <- local_artifact_provider(provider)
    remote$storage_object <- transport$storage_object
    path <- artifact_test_path(remote, "forecasts", "A", "-average_models")
    for (status in c(401L, 403L, 429L, 500L)) {
      condition <- artifact_http_error(status, "provider failure")
      transport$lookup_error <- condition
      observed <- tryCatch(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
        error = identity)
      expect_identical(observed, condition)
    }
    transport$lookup_error <- simpleError("unclassified provider failure")
    expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
      "unclassified provider failure", fixed = TRUE)
    transport$lookup_error <- NULL
    for (status in c(403L, 404L)) {
      transport$root_error <- artifact_http_error(status, "storage root unavailable")
      observed <- tryCatch(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
        error = identity)
      expect_identical(observed, transport$root_error)
    }
    transport$root_error <- NULL
    payload <- artifact_test_path(source, "prep_data", "A", "-R1")
    transport$files[[as.character(path)]] <- payload
    transport$download_error <- artifact_http_error(500L, "download failed")
    expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE), class = "http_500")
    if (provider == "ms_drive") {
      transport$download_error <- artifact_http_error(404L, "download URL unavailable")
      expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE), class = "http_404")
      transport$folder <- TRUE
      expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
        "not a regular file", fixed = TRUE)
      transport$folder <- FALSE
    }
    transport$download_error <- NULL
    writeLines("invalid serialized artifact", payload)
    expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
      "unknown input format|error reading from connection")
    remote$data_output <- "csv"
    empty <- withr::local_tempfile(fileext = ".csv")
    writeLines("Forecast", empty)
    transport$files[[as.character(artifact_test_path(remote, "forecasts", "A", "-average_models"))]] <- empty
    expect_error(read_selection_file(remote, "forecasts", "-average_models", "A", optional = TRUE),
      "exact forecast artifact is empty or unreadable", fixed = TRUE)
  })
}

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("standard hierarchical retrieval reads all provider models on", provider), {
    source <- artifact_test_run(withr::local_tempdir(), "rds")
    artifact_test_log(source, combo_variables = "ID", forecast_approach = "grouped_hierarchy")
    artifact_test_splits(source)
    models <- c("meanf--local--R1", "snaive--local--R1", "Best-Model")
    for (model in models) {
      rows <- artifact_test_forecast(source, write_output = FALSE)
      rows$Model_ID <- model
      rows$Best_Model <- if (model == "Best-Model") "Yes" else "No"
      write_data(rows, model, source, "data", "forecasts", "-reconciled")
    }
    remote <- source
    remote$path <- "provider-artifacts"
    transport <- local_artifact_provider(provider)
    remote$storage_object <- transport$storage_object
    files <- c(artifact_test_path(source, "logs", extension = "csv"),
      artifact_test_path(source, "prep_models", suffix = "-train_test_split"),
      vapply(models, function(model) as.character(artifact_test_path(source, "forecasts", model, "-reconciled")), character(1)))
    remote_files <- fs::path(remote$path, c("logs", "prep_models", rep("forecasts", 3)), fs::path_file(files))
    transport$files <- stats::setNames(as.list(files), as.character(remote_files))
    listings <- 0L
    local_mocked_bindings(list_files = function(storage_object, path, fail_on_error = FALSE) {
      expect_match(path, "-reconciled[.]rds$")
      expect_true(fail_on_error)
      listings <<- listings + 1L
      files <- remote_files[3:5]
      if (provider == "ms_drive") fs::path_file(files) else files
    })

    result <- get_forecast_data(remote)

    expect_setequal(result$Model_ID, models)
    expect_identical(result$Model_ID[result$Best_Model == "Yes"], "Best-Model")
    expect_equal(listings, 1L)
    expect_setequal(transport$downloads, as.character(remote_files))
    expect_length(transport$downloads, 5L)
  })

  test_that(paste("selection history and hierarchy metadata use exact provider transfer on", provider), {
    source <- artifact_test_run(withr::local_tempdir(), "rds")
    history <- artifact_test_recipe(source, "A", "R1")
    hierarchy <- list(original_combos = "A", hts_combos = "A")
    write_data(hierarchy, NULL, source, "object", "prep_data", "-hts_info")
    remote <- source
    remote$path <- "provider-artifacts"
    transport <- local_artifact_provider(provider)
    remote$storage_object <- transport$storage_object
    history_path <- artifact_test_path(remote, "prep_data", "A", "-R1")
    hierarchy_path <- artifact_test_path(remote, "prep_data", suffix = "-hts_info", extension = "rds")
    transport$files[[as.character(history_path)]] <- artifact_test_path(source, "prep_data", "A", "-R1")
    transport$files[[as.character(hierarchy_path)]] <- artifact_test_path(source, "prep_data", suffix = "-hts_info", extension = "rds")
    log <- tibble::tibble(hist_end_date = max(history$Date), recipes_to_run = "R1", date_type = "day")
    cache <- new.env(parent = emptyenv())

    expect_equal(read_series_history(remote, "A", log, cache)$history,
      as.data.frame(history[, c("Date", "Target")]))
    expect_equal(read_selection_hierarchy(remote, cache), hierarchy)
    expect_equal(read_selection_hierarchy(remote, cache), hierarchy)
    expect_setequal(transport$downloads, as.character(c(history_path, hierarchy_path)))
    expect_length(transport$downloads, 2L)
  })
}

test_that("strict CSV fallback preserves valid empty and populated files", {
  run_info <- artifact_test_run(withr::local_tempdir())
  paths <- file.path(run_info$path, paste0("fallback-", seq_len(3), ".csv"))
  file.create(paths[[1]])
  writeLines("value", paths[[2]])
  utils::write.csv(data.frame(value = c(2, 7)), paths[[3]], row.names = FALSE)
  testthat::local_mocked_bindings(
    vroom = function(...) stop("exercise compatible CSV fallback", call. = FALSE),
    .package = "vroom"
  )

  strict_result <- read_file(run_info, file_list = paths, strict = TRUE)
  legacy_result <- read_file(run_info, file_list = paths)

  expect_equal(strict_result, tibble::tibble(value = c(2L, 7L)))
  expect_equal(strict_result, legacy_result)
  expect_equal(nrow(read_file(run_info, file_list = paths[1:2], strict = TRUE)), 0L)
})

test_that("strict CSV fallback keeps metadata exceptions as errors", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  original_info <- file.info
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected CSV read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(files, ...) {
      if (any(as.character(files) == as.character(path))) {
        stop("injected metadata I/O failure", call. = FALSE)
      }
      original_info(files, ...)
    },
    .package = "base"
  )

  expect_error(read_local_artifacts(run_info, path), "injected metadata I/O failure")
  expect_error(read_local_artifacts(run_info, path, allow_missing = TRUE),
    "injected metadata I/O failure")
  expect_warning(legacy <- read_file(run_info, file_list = path),
    "Skipping empty or unreadable file")
  expect_equal(nrow(legacy), 0L)
})
