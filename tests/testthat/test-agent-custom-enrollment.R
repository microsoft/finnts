test_that("Agent enrollment uses objects alone and returns lightweight references", {
  args <- custom_agent_inputs(withr::local_tempdir())
  args$models_to_run <- NULL
  info <- do.call(set_agent_info, args)
  expect_false("models_to_run" %in% names(formals(set_agent_info)))
  expect_null(info$custom_agent_contract)
  expect_identical(info$custom_models, "business_mean")
  parent <- agent_custom_parent_info(info)
  log <- read_exact_artifact(parent,
    local_artifact_path(parent, "logs", "-agent_run", extension = "csv"))
  expect_identical(log$custom_models, "business_mean")
  expect_true(file.exists(local_artifact_path(parent, "prep_models",
    "-custom-models", extension = "rds")))
  expect_false(file.exists(local_artifact_path(parent, "logs",
    "-agent-custom-models", extension = "rds")))
  ordinary <- custom_agent_inputs(withr::local_tempdir())
  ordinary$models_to_run <- ordinary$custom_models <- NULL
  plain <- do.call(set_agent_info, ordinary)
  log <- read_exact_artifact(agent_custom_parent_info(plain),
    local_artifact_path(agent_custom_parent_info(plain), "logs", "-agent_run", extension = "csv"))
  expect_true("custom_models" %in% names(log))
  expect_true(is.na(log$custom_models))
})

test_that("public Agent setup pins approved custom enrollment without executing source", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  expect_match(info$custom_agent_contract_id, "^custom-agent-[a-f0-9]{64}$")
  expect_setequal(info$custom_agent_contract$selected, c(list_models(), "business_mean"))
  expect_false(info$allow_hierarchical_forecast)
  expect_identical(validate_agent_custom_models(info$custom_agent_contract), info$custom_agent_contract)
})

test_that("custom setup preserves exact enrollment and rejects omission or legacy adoption", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  repeated <- load_agent_custom_state(do.call(set_agent_info, args))
  expect_identical(repeated$run_id, info$run_id)
  expect_identical(repeated$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_identical(tools::md5sum(files), hashes)
  omitted <- args
  omitted$custom_models <- omitted$models_to_run <- NULL
  expect_error(do.call(set_agent_info, omitted), "omitted")
  changed <- args
  changed$custom_models$business_mean$validation$checks$arithmetic <- "Changed evidence"
  expect_error(do.call(set_agent_info, changed), "enrollment changed")
  changed <- args
  changed$forecast_horizon <- 2
  expect_error(do.call(set_agent_info, changed), "enrollment changed")
  expect_identical(tools::md5sum(files), hashes)
  legacy <- custom_agent_inputs(withr::local_tempdir())
  ordinary <- legacy
  ordinary$models_to_run <- ordinary$custom_models <- NULL
  do.call(set_agent_info, ordinary)
  expect_error(do.call(set_agent_info, legacy), "enrollment changed")
})

test_that("parent records are exact-path immutable and malformed content stays untouched", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  parent <- agent_custom_parent_info(info)
  path <- local_artifact_path(parent, "prep_models", "-custom-models", extension = "rds")
  testthat::local_mocked_bindings(list_files = function(...) stop("Unexpected listing"), .package = "finnts")
  expect_identical(read_agent_custom_record(info, TRUE), info$custom_agent_contract)
  original <- tools::md5sum(path)
  write_agent_custom_record(info, info$custom_agent_contract)
  expect_identical(tools::md5sum(path), original)
  saveRDS(NULL, path)
  corrupt <- tools::md5sum(path)
  expect_error(read_agent_custom_record(info), "record identity")
  expect_error(write_agent_custom_record(info, info$custom_agent_contract), "record identity")
  expect_identical(tools::md5sum(path), corrupt)
  other <- info
  other$run_id <- "not-yet-logged"
  write_agent_custom_record(other, info$custom_agent_contract)
  expect_identical(read_agent_custom_record(other, TRUE), info$custom_agent_contract)
})

test_that("execution entry restores saved identity and rejects conflicting context", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  stripped <- info
  stripped$custom_agent_contract <- stripped$custom_agent_contract_id <- NULL
  restored <- load_agent_custom_state(stripped)
  expect_identical(restored$custom_agent_contract_id, info$custom_agent_contract_id)
  changed <- info
  changed$negative_forecast <- FALSE
  expect_error(load_agent_custom_state(changed), "negative_forecast|context")
  record_only <- info
  record_only$run_id <- "record-only"
  write_agent_custom_record(record_only, info$custom_agent_contract)
  expect_error(load_agent_custom_state(record_only), "parent log")
  expect_no_error(agent_custom_preflight(info, "spark", FALSE))
  expect_error(agent_custom_preflight(info, NULL, TRUE), "inner_parallel")
})

test_that("worker references omit nested enrollment and coordinating caches", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  info$selection_cache <- new.env(parent = emptyenv())
  info$selection_cache$contract <- info$custom_agent_contract
  info$custom_agent_update <- list(source = args$custom_models)
  worker <- agent_custom_worker_info(info)
  expect_null(worker$llm)
  expect_null(worker$custom_agent_contract)
  expect_null(worker$custom_agent_update)
  expect_null(worker$selection_cache)
  expect_identical(worker$custom_models, info$custom_models)
  expect_identical(worker$custom_agent_contract_id, info$custom_agent_contract_id)
  payload <- unserialize(serialize(worker, NULL))
  expect_identical(payload, worker)
  expect_false(any(grepl("function\\(data|technical_passed|intent_confirmed",
    capture.output(str(payload)), fixed = FALSE)))
  expect_identical(load_agent_custom_state(payload)$custom_agent_contract,
    info$custom_agent_contract)
  reasoning <- agent_custom_worker_info(info, keep_llm = TRUE)
  expect_identical(reasoning$llm, info$llm)
  expect_null(reasoning$custom_agent_contract)
  expect_false(is.null(info$selection_cache$contract))
})

test_that("worker enrollment reuse is invocation-local and exact-owner bound", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  foreign <- do.call(set_agent_info, custom_agent_inputs(withr::local_tempdir()))
  reads <- 0L
  original <- read_agent_custom_record
  testthat::local_mocked_bindings(
    read_agent_custom_record = function(...) {
      reads <<- reads + 1L
      original(...)
    },
    list_files = function(...) stop("Unexpected enrollment discovery"),
    .package = "finnts")
  cache <- new.env(parent = emptyenv())
  first <- load_agent_custom_state(info, cache = cache)
  second <- load_agent_custom_state(info, cache = cache)
  expect_identical(second$custom_agent_contract, first$custom_agent_contract)
  expect_equal(reads, 1L)
  changed <- info
  changed$negative_forecast <- FALSE
  expect_error(load_agent_custom_state(changed, cache = cache), "negative_forecast|context")
  expect_identical(load_agent_custom_state(foreign, cache = cache)$custom_agent_contract,
    first$custom_agent_contract)
  expect_equal(reads, 2L)
  first$custom_enrollment_cache <- cache
  expect_null(agent_custom_worker_info(first)$custom_enrollment_cache)
})
