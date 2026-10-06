# Return a passive named definition with deliberately unusable source bodies.
# Name and fixed value vary semantic identity; no source may run in registry tests.
registry_definition <- function(name = "business-growth", value = 1) {
  finnts:::new_custom_model_definition(
    name = name, instructions = "Use the supplied business growth rule.", interpretation = "Apply the fixed growth parameter to historical values.",
    model_type = c("local", "global"), source = c(fit = "function(...) stop('Registry executed fit source')", predict = "function(...) stop('Registry executed predict source')"),
    requirements = list(
      predictors = character(), recipes = "R1", target_scale = "original", date_types = "month", forecast_horizon = 1:12,
      missing_data = "error"
    ), fixed_parameters = list(growth = value)
  )
}

test_that("explicit pools resolve only requested built-in and custom candidates", {
  definition <- registry_definition()
  supplied <- list(`business-growth` = definition)
  mixed <- finnts:::resolve_custom_model_pool(c("business-growth", "arima"), custom_models = supplied)
  expect_identical(mixed$selected, c("business-growth", "arima"))
  expect_identical(mixed$built_in, "arima")
  expect_identical(mixed$custom, supplied)
  custom_only <- finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied)
  expect_identical(custom_only$selected, "business-growth")
  expect_identical(custom_only$built_in, character())
  expect_identical(custom_only$custom, supplied)
})

test_that("defaults never enroll supplied custom models and explicit inclusions win", {
  supplied <- list(`business-growth` = registry_definition())
  catalog <- list_models()
  defaults <- finnts:::resolve_custom_model_pool(custom_models = supplied)
  expect_identical(defaults$selected, catalog)
  expect_length(defaults$custom, 0L)
  excluded <- finnts:::resolve_custom_model_pool(models_not_to_run = list("arima", "snaive"), custom_models = supplied)
  expect_identical(excluded$selected, setdiff(catalog, c("arima", "snaive")))
  explicit <- finnts:::resolve_custom_model_pool(list("arima", "business-growth"), list("arima", "business-growth"), supplied)
  expect_identical(explicit$selected, c("arima", "business-growth"))
  expect_identical(list_models(), catalog)
  expect_identical(supplied[[1]], registry_definition())
  expect_identical(explicit$capabilities[["business-growth"]], c(list(model_type = supplied[[1]]$model_type), supplied[[1]]$requirements))
})

test_that("pool identity ignores display ordering and unselected versions", {
  supplied <- list(`business-growth` = registry_definition(), unused = registry_definition("unused"))
  first <- finnts:::resolve_custom_model_pool(c("business-growth", "arima"), custom_models = supplied)
  supplied$unused <- registry_definition("unused", 2)
  second <- finnts:::resolve_custom_model_pool(list("arima", "business-growth"), custom_models = rev(supplied))
  expect_identical(first$pool_id, second$pool_id)
  expect_match(first$pool_id, "^[a-f0-9]{64}$")
  expect_identical(second$selected, rev(first$selected))
  supplied[["business-growth"]] <- registry_definition(value = 2)
  changed <- finnts:::resolve_custom_model_pool(first$selected, custom_models = supplied)
  expect_false(identical(first$pool_id, changed$pool_id))
  expect_false(identical(first$pool_id, finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied)$pool_id))
})

test_that("malformed pools and unused stale definitions fail without execution", {
  for (selection in list(character(), list(), "", NA_character_, c("arima", "arima"), list(list("arima")), list(
    "arima",
    1
  ), 1, factor("arima"), matrix("arima"))) {
    expect_error(finnts:::resolve_custom_model_pool(selection), "models_to_run")
  }
  expect_error(finnts:::resolve_custom_model_pool(models_not_to_run = list_models()), "empty")
  expect_error(finnts:::resolve_custom_model_pool("unknown"), "Unknown selected")
  definition <- registry_definition()
  expect_error(finnts:::resolve_custom_model_pool(custom_models = list(definition)), "named list")
  expect_error(finnts:::resolve_custom_model_pool(custom_models = list(arima = definition)), "collide")
  expect_error(finnts:::resolve_custom_model_pool(custom_models = list(other = definition)), "alias")
  expect_error(finnts:::resolve_custom_model_pool(custom_models = setNames(list(definition, definition), rep(
    definition$name,
    2
  ))), "unique")
  definition$fixed_parameters$growth <- 2
  expect_error(finnts:::resolve_custom_model_pool("arima", custom_models = list(`business-growth` = definition)), "version_id")
  definition$version_id <- NULL
  expect_error(finnts:::resolve_custom_model_pool("arima", custom_models = list(`business-growth` = definition)), "version_id")
})

# Return the minimal caller-owned storage settings; non-RDS formats deliberately
# prove the registry's fixed format does not mutate ordinary run configuration.
registry_run <- function(path, project = "registry-tests", run = "first-run") {
  list(project_name = project, run_name = run, path = path, storage_object = NULL, object_output = "qs2", data_output = "parquet")
}

# Install call-scoped spies delegating record reads/writes to real package I/O.
# The callbacks count calls and paths, and fail any directory enumeration.
# Returns a mutable counter, with mocks restored in the calling test environment.
registry_spies <- function(.local_envir = parent.frame()) {
  tracker <- new.env(parent = emptyenv())
  tracker$reads <- character()
  tracker$optional <- logical()
  tracker$writes <- 0L
  reader <- finnts:::custom_model_read_record
  writer <- finnts:::write_data
  testthat::local_mocked_bindings(list_files = function(...) stop("Unexpected registry directory listing"), custom_model_read_record = function(reference,
                                                                                                                                                location, allow_missing = FALSE) {
    tracker$reads <- c(tracker$reads, as.character(location$path))
    tracker$optional <- c(tracker$optional, allow_missing)
    reader(reference, location, allow_missing)
  }, write_data = function(...) {
    tracker$writes <- tracker$writes + 1L
    writer(...)
  }, .package = "finnts", .env = .local_envir)
  tracker
}

test_that("passive definitions persist once and reload exact versions across runs", {
  run_info <- registry_run(withr::local_tempdir())
  original_info <- run_info
  definition <- registry_definition()
  tracker <- registry_spies()
  reference <- finnts:::write_custom_model_definition(definition, run_info)
  expect_identical(names(reference), c("registry_schema_version", "name", "version_id"))
  expect_identical(reference$version_id, definition$version_id)
  expect_identical(tracker$optional, c(TRUE, FALSE))
  expect_identical(tracker$writes, 1L)
  expect_match(tracker$reads[[1]], paste0(definition$version_id, "\\.rds$"))
  expect_identical(finnts:::write_custom_model_definition(definition, run_info), reference)
  expect_identical(tracker$writes, 1L)
  expect_identical(tracker$optional, c(TRUE, FALSE, TRUE))
  run_info$run_name <- "second-run"
  expect_identical(finnts:::read_custom_model_definition(reference, run_info), definition)
  expect_length(unique(tracker$reads), 1L)
  expect_identical(original_info, registry_run(original_info$path))
  expect_identical(run_info$object_output, "qs2")
  expect_identical(run_info$data_output, "parquet")
  expect_identical(definition, registry_definition())
})

test_that("only selected references are read and no cache outlives resolution", {
  run_info <- registry_run(withr::local_tempdir())
  definition <- registry_definition()
  reference <- finnts:::write_custom_model_definition(definition, run_info)
  unused <- reference
  unused$name <- "unused"
  unused$version_id <- paste(rep("a", 64), collapse = "")
  supplied <- list(`business-growth` = reference, unused = unused)
  tracker <- registry_spies()
  defaults <- finnts:::resolve_custom_model_pool(custom_models = supplied)
  expect_identical(defaults$pool_id, finnts:::resolve_custom_model_pool()$pool_id)
  expect_length(tracker$reads, 0L)
  expect_error(
    finnts:::resolve_custom_model_pool(c("business-growth", "unknown"), custom_models = supplied, run_info = run_info),
    "Unknown selected"
  )
  expect_length(tracker$reads, 0L)
  expect_error(finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied), "run_info")
  resolved <- finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied, run_info = run_info)
  expect_identical(resolved$custom, list(`business-growth` = definition))
  expect_identical(resolved$pool_id, finnts:::resolve_custom_model_pool("business-growth", custom_models = list(`business-growth` = definition))$pool_id)
  expect_length(tracker$reads, 1L)
  expect_identical(tracker$writes, 0L)
  finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied, run_info = run_info)
  expect_length(tracker$reads, 2L)
  saveRDS(NULL, tracker$reads[[1]])
  expect_error(finnts:::resolve_custom_model_pool("business-growth", custom_models = supplied, run_info = run_info), "Malformed")
})

test_that("references and caller settings are validated before storage access", {
  definition <- registry_definition()
  reference <- list(registry_schema_version = 1L, name = definition$name, version_id = definition$version_id)
  run_info <- registry_run(withr::local_tempdir())
  tracker <- registry_spies()
  malformed <- list(
    modifyList(reference, list(registry_schema_version = 2L)), modifyList(reference, list(name = "../unsafe")),
    modifyList(reference, list(name = "arima")), modifyList(reference, list(name = "bad--name")), modifyList(
      reference,
      list(version_id = "latest")
    ), modifyList(reference, list(version_id = toupper(reference$version_id))), c(
      reference,
      list(path = "elsewhere")
    ), c(reference, list(approved = TRUE)), c(reference, list(allow_code = TRUE)), structure(reference,
      class = "untrusted"
    ), modifyList(reference, list(name = NA_character_))
  )
  for (invalid in malformed) {
    expect_error(finnts:::read_custom_model_definition(invalid, run_info), "reference")
  }
  expect_error(finnts:::write_custom_model_definition(
    modifyList(definition, list(fixed_parameters = list(growth = 5))),
    run_info
  ), "version_id")
  expect_error(finnts:::read_custom_model_definition(reference, NULL), "run_info")
  expect_error(finnts:::read_custom_model_definition(reference, modifyList(run_info, list(project_name = ""))), "project_name")
  expect_error(finnts:::read_custom_model_definition(reference, modifyList(run_info, list(storage_object = list()))), "Unsupported storage")
  expect_length(tracker$reads, 0L)
  expect_identical(tracker$writes, 0L)
})

test_that("corrupt and conflicting existing records cannot be overwritten", {
  run_info <- registry_run(withr::local_tempdir())
  definition <- registry_definition()
  reference <- finnts:::write_custom_model_definition(definition, run_info)
  location <- finnts:::custom_model_registry_location(reference, run_info)
  valid <- readRDS(location$path)
  stale <- valid
  stale$definition$fixed_parameters$growth <- 99
  wrong_definition <- valid
  wrong_definition$definition <- registry_definition("different")
  bad_records <- list(
    NULL, list(), "not a record", stale, wrong_definition, modifyList(valid, list(registry_schema_version = 2L)),
    modifyList(valid, list(name = "different")), modifyList(valid, list(version_id = paste(rep("a", 64), collapse = ""))),
    c(valid, list(approved = TRUE))
  )
  tracker <- registry_spies()
  for (record in bad_records) {
    saveRDS(record, location$path)
    before <- digest::digest(file = location$path, algo = "sha256")
    expect_error(finnts:::read_custom_model_definition(reference, run_info), "Malformed|reference|version_id")
    expect_error(finnts:::write_custom_model_definition(definition, run_info), "Malformed|reference|version_id")
    expect_identical(digest::digest(file = location$path, algo = "sha256"), before)
  }
  for (bytes in list(raw(), charToRaw("not RDS"))) {
    writeBin(bytes, location$path)
    before <- digest::digest(file = location$path, algo = "sha256")
    expect_error(finnts:::write_custom_model_definition(definition, run_info))
    expect_identical(digest::digest(file = location$path, algo = "sha256"), before)
  }
  expect_identical(tracker$writes, 0L)
})

test_that("versions coexist while roots and projects remain isolated", {
  run_info <- registry_run(withr::local_tempdir())
  first <- registry_definition()
  second <- registry_definition(value = 2)
  reference <- finnts:::write_custom_model_definition(first, run_info)
  newer <- finnts:::write_custom_model_definition(second, run_info)
  expect_false(identical(reference$version_id, newer$version_id))
  expect_identical(finnts:::read_custom_model_definition(reference, run_info), first)
  expect_identical(finnts:::read_custom_model_definition(newer, run_info), second)
  expect_error(
    finnts:::read_custom_model_definition(reference, registry_run(run_info$path, project = "other-project")),
    "Missing required"
  )
  expect_error(finnts:::read_custom_model_definition(reference, registry_run(withr::local_tempdir())), "Missing required")
  second$created_at <- "different display timestamp"
  expect_identical(finnts:::write_custom_model_definition(second, run_info), newer)
  expect_null(finnts:::read_custom_model_definition(newer, run_info)$created_at)
  temporary <- registry_run(NULL, project = basename(tempfile("registry-test-")))
  expect_identical(
    finnts:::read_custom_model_definition(finnts:::write_custom_model_definition(first, temporary), temporary),
    first
  )
  expect_null(temporary$path)
})

# Supply synthetic blob/drive transports backed by a test-owned local directory.
# Callbacks record exact requests, copy payloads, and simulate provider failures;
# no callback contacts a service. Mutable failure flags exercise missing-container,
# permission, transient, folder and post-upload corruption paths. Returns tracker
# and storage_object; Azure mocks are restored in the calling test environment.
registry_transport <- function(backend, root, .local_envir = parent.frame()) {
  tracker <- new.env(parent = emptyenv())
  tracker$downloads <- character()
  tracker$uploads <- character()
  tracker$payload_downloads <- 0L
  tracker$root_checks <- 0L
  tracker$failure <- NULL
  tracker$corrupt_upload <- FALSE
  tracker$folder <- FALSE
  fail <- function(message, class) {
    stop(structure(list(message = message, call = NULL), class = c(class, "error", "condition")))
  }
  check_access <- function() {
    if (identical(tracker$failure, "denied"))
      fail("permission denied", "http_403")
    if (identical(tracker$failure, "transient"))
      fail("temporary transport failure", "http_503")
  }
  physical <- function(path) fs::path(root, fs::path_file(path))
  lookup <- function(path) {
    check_access()
    tracker$downloads <- c(tracker$downloads, as.character(path))
    if (identical(tracker$failure, "missing-container") || !file.exists(physical(path))) {
      fail("artifact not found", "http_404")
    }
    physical(path)
  }
  root_access <- function(...) {
    tracker$root_checks <- tracker$root_checks + 1L
    if (identical(tracker$failure, "missing-container"))
      fail("container unavailable", "http_403")
    TRUE
  }
  download <- function(source, destination) {
    force(source)
    tracker$payload_downloads <- tracker$payload_downloads + 1L
    if (!file.copy(source, destination, overwrite = TRUE))
      stop("Fixture download failed")
    invisible(NULL)
  }
  upload <- function(src, dest, ...) {
    check_access()
    tracker$uploads <- c(tracker$uploads, as.character(dest))
    if (!file.copy(src, physical(dest), overwrite = TRUE))
      stop("Fixture upload failed")
    if (tracker$corrupt_upload)
      saveRDS(NULL, physical(dest))
    invisible(NULL)
  }
  if (backend == "blob_container") {
    skip_if_not_installed("AzureStor")
    testthat::local_mocked_bindings(
      storage_download = function(container, src, dest, ...) download(lookup(src), dest),
      storage_upload = function(container, src, dest, ...) upload(src, dest), get_storage_properties = root_access,
      .package = "AzureStor", .env = .local_envir
    )
    storage <- structure(list(), class = "blob_container")
  } else {
    storage <- structure(list(upload_file = upload, get_item = function(path) {
      if (identical(path, "/")) return(root_access())
      source <- lookup(path)
      list(is_folder = function() tracker$folder, download = function(dest, ...) download(source, dest))
    }), class = "ms_drive")
  }
  list(tracker = tracker, storage_object = storage)
}

for (backend in c("blob_container", "ms_drive")) {
  test_that(paste("exact registry transport preserves errors for", backend), {
    root <- withr::local_tempdir()
    transport <- registry_transport(backend, root)
    tracker <- transport$tracker
    run_info <- registry_run("registry-root")
    run_info$storage_object <- transport$storage_object
    definition <- registry_definition()
    spies <- registry_spies()
    reference <- finnts:::write_custom_model_definition(definition, run_info)
    expect_length(tracker$downloads, 2L)
    expect_length(tracker$uploads, 1L)
    expect_identical(tracker$payload_downloads, 1L)
    expect_identical(tracker$root_checks, 1L)
    expect_identical(finnts:::write_custom_model_definition(definition, run_info), reference)
    expect_identical(finnts:::read_custom_model_definition(reference, run_info), definition)
    expect_length(tracker$downloads, 4L)
    expect_length(tracker$uploads, 1L)
    expect_identical(tracker$payload_downloads, 3L)
    expect_identical(unique(tracker$downloads), unique(tracker$uploads))
    expect_false(any(grepl("*", tracker$downloads, fixed = TRUE)))
    for (failure in c("denied", "transient", "missing-container")) {
      tracker$failure <- failure
      error_class <- if (failure == "transient")
        "http_503"
      else "http_403"
      expect_error(finnts:::write_custom_model_definition(definition, run_info), class = error_class)
      expect_length(tracker$uploads, 1L)
    }
    tracker$failure <- NULL
    absent <- reference
    absent$version_id <- paste(rep("a", 64), collapse = "")
    expect_error(finnts:::read_custom_model_definition(absent, run_info), class = "http_404")
    expect_length(tracker$uploads, 1L)
    physical <- fs::path(root, fs::path_file(tracker$uploads[[1]]))
    saveRDS(NULL, physical)
    before <- digest::digest(file = physical, algo = "sha256")
    expect_error(finnts:::write_custom_model_definition(definition, run_info), "Malformed")
    expect_identical(digest::digest(file = physical, algo = "sha256"), before)
    writeBin(charToRaw("not RDS"), physical)
    expect_error(finnts:::read_custom_model_definition(reference, run_info))
    expect_length(tracker$uploads, 1L)
    tracker$corrupt_upload <- TRUE
    expect_error(finnts:::write_custom_model_definition(registry_definition(value = 2), run_info), "Malformed")
    expect_length(tracker$uploads, 2L)
    if (backend == "ms_drive") {
      tracker$folder <- TRUE
      expect_error(finnts:::read_custom_model_definition(reference, run_info), "not a regular file")
    }
    expect_identical(run_info$object_output, "qs2")
  })
}
