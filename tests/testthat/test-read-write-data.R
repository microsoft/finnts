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

test_that("a parquet artifact can be rewritten while its read result is still in use", {
  # final_models() reads a series' forecasts and rewrites the same file; a
  # memory-mapped read kept the file open and Windows refused the rewrite.
  skip_if_not_installed("arrow")
  run_info <- artifact_test_run(withr::local_tempdir(), "parquet")
  artifact_test_forecast(run_info)
  path <- artifact_test_path(run_info, "forecasts", "A", "-single_models")
  forecast <- read_file(run_info, file_list = path)
  forecast$Best_Model <- "No"

  expect_no_error(write_data(forecast, "A", run_info, "data", "forecasts", "-single_models"))
  expect_identical(read_file(run_info, file_list = path)$Best_Model, "No")
})

test_that("parquet forecast reads combine model and simple-average files like CSV", {
  skip_if_not_installed("arrow")
  ids <- list()
  for (format in c("csv", "parquet")) {
    run_info <- artifact_test_run(withr::local_tempdir(), format)
    artifact_test_forecast(run_info)
    # simple-average forecasts save Hyperparameter_ID as the text "NA"
    average <- artifact_test_forecast(run_info, write_output = FALSE)
    average$Model_ID <- "meanf--local--R1_naive--local--R1"
    average$Hyperparameter_ID <- "NA"
    write_data(average, "A", run_info, "data", "forecasts", "-average_models")
    forecasts <- read_file(run_info, path = paste0("/forecasts/*", hash_data(run_info$project_name), "-",
      hash_data(run_info$run_name), "*models.", format))
    ids[[format]] <- forecasts$Hyperparameter_ID[order(forecasts$Model_ID)]
  }

  expect_type(ids$parquet, "double")
  expect_identical(ids$parquet, c(1, NA))
  expect_identical(ids$parquet, ids$csv)
})

test_that("settled reads wait only on mounted Spark storage", {
  expect_length(settle_waits(tempdir()), 0L)
  expect_length(settle_waits(NULL), 0L)
  for (path in c("/synfs/notebook/1/mount/project", "/dbfs/mnt/project")) {
    # must outlast Synapse's default 120-second mount file cache
    expect_gt(sum(settle_waits(path)), 120)
  }
  calls <- 0L
  expect_silent(result <- read_settled(function() {
    calls <<- calls + 1L
    "stale"
  }, function(value) FALSE, tempdir(), "forecasts"))
  expect_identical(result, "stale")
  expect_identical(calls, 1L)
})

test_that("settled reads retry stale values and read errors, then keep the caller's error", {
  testthat::local_mocked_bindings(settle_waits = function(path) c(0, 0, 0))
  values <- list(simpleError("not visible yet"), "stale", "fresh")
  calls <- 0L
  read <- function() {
    calls <<- calls + 1L
    value <- values[[calls]]
    if (inherits(value, "error")) stop(value)
    value
  }
  expect_message(result <- read_settled(read, function(value) identical(value, "fresh"),
    "/synfs/mount/project", "forecasts"), "Re-reading forecasts")
  expect_identical(result, "fresh")
  expect_identical(calls, 3L)

  calls <- 0L
  result <- suppressMessages(read_settled(function() {
    calls <<- calls + 1L
    "stale"
  }, function(value) FALSE, "/synfs/mount/project", "forecasts"))
  expect_identical(result, "stale")
  expect_identical(calls, 4L)

  failure <- structure(class = c("finnts_test_storage_error", "error", "condition"),
    list(message = "still missing", call = NULL))
  expect_error(suppressMessages(read_settled(function() stop(failure), function(value) TRUE,
    "/synfs/mount/project", "forecasts")), class = "finnts_test_storage_error")

  # a nested settled read runs once; the enclosing one owns the waits
  inner_calls <- 0L
  outer_calls <- 0L
  expect_error(suppressMessages(read_settled(function() {
    outer_calls <<- outer_calls + 1L
    read_settled(function() {
      inner_calls <<- inner_calls + 1L
      stop("still missing")
    }, function(value) TRUE, "/synfs/mount/project", "inner forecasts")
  }, function(value) TRUE, "/synfs/mount/project", "outer forecasts")), "still missing")
  expect_identical(c(outer_calls, inner_calls), c(4L, 4L))
  expect_silent(read_settled(function() "fresh", function(value) TRUE, "/synfs/mount/project", "forecasts"))
})

test_that("get_forecast_data re-reads forecasts that are stale on mounted storage", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info, "A")
  artifact_test_forecast(run_info, "B")
  original_read <- read_file
  original_dir_ls <- safe_dir_ls
  stale_reads <- 0L
  reads <- 0L
  listings <- 0L
  # The first reads return the copy saved before final selection set Best_Model.
  testthat::local_mocked_bindings(
    settle_waits = function(path) c(0, 0),
    safe_dir_ls = function(...) {
      listings <<- listings + 1L
      original_dir_ls(...)
    },
    read_file = function(run_info, path = NULL, ...) {
      data <- original_read(run_info, path = path, ...)
      if (!is.null(path) && grepl("models.", path, fixed = TRUE)) {
        reads <<- reads + 1L
        if (reads <= stale_reads) data$Best_Model <- "No"
      }
      data
    }
  )
  get_forecast_data(run_info)
  fresh_listings <- listings

  stale_reads <- 1L
  reads <- 0L
  listings <- 0L
  expect_message(result <- get_forecast_data(run_info), "Re-reading")
  expect_identical(reads, 2L)
  expect_setequal(result$Combo[result$Best_Model == "Yes"], c("A", "B"))
  # re-reads reuse the forecast files listed before the first read
  expect_identical(listings, fresh_listings)

  stale_reads <- Inf
  reads <- 0L
  listings <- 0L
  expect_error(suppressMessages(get_forecast_data(run_info)), "missing a best model")
  expect_identical(reads, 3L)
  expect_identical(listings, fresh_listings)
})
