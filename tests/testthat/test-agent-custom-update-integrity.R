test_that("custom update execution controls and provenance cannot change on restart", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  expect_error(update_forecast(current), "allow_iterate_forecast")
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE, inner_parallel = TRUE), "inner_parallel")
  changed <- current
  changed$hist_start_date <- as.Date("2020-02-01")
  before <- tools::md5sum(list.files(fixture$args$project_info$path, recursive = TRUE, full.names = TRUE))
  expect_error(update_forecast(changed, allow_iterate_forecast = FALSE), "provenance|inputs changed")
  expect_identical(tools::md5sum(names(before)), before)
})

test_that("custom update detects changed input bytes without modifying artifacts", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  input_path <- local_artifact_path(agent_custom_parent_info(current), "input_data", combo = hash_data("first"))
  altered <- read_file(current$project_info, file_list = input_path)
  altered$Target[[1]] <- 101
  write_data(altered, "first", agent_custom_parent_info(current), "data", "input_data")
  before <- tools::md5sum(list.files(fixture$args$project_info$path, recursive = TRUE, full.names = TRUE))
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "inputs changed")
  expect_identical(tools::md5sum(names(before)), before)
})

test_that("custom update rejects corrupt fits and altered workflow approval", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  best <- get_best_agent_run(current)
  child <- current$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("first"))
  child$run_name <- best$best_run_name[[1]]
  fit_path <- local_artifact_path(child, "models", "-single_models", hash_data("first"), "rds")
  altered <- readRDS(fit_path)
  spec <- workflows::extract_spec_parsnip(altered$Model_Fit[[1]])
  spec$eng_args$allow_code <- rlang::new_quosure(FALSE, emptyenv())
  altered$Model_Fit[[1]]$fit$actions$model$spec <- spec
  expect_error(validate_agent_custom_update_workflows(altered,
    custom_run_load(child, read_selection_file(child, "logs"))), "specification changed")
  saveRDS(NULL, fit_path)
  before <- tools::md5sum(list.files(fixture$args$project_info$path, recursive = TRUE, full.names = TRUE))
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "damaged|fitted outputs")
  expect_identical(tools::md5sum(names(before)), before)
})

test_that("changed custom evidence and invariant context fail before update writes", {
  for (change in c("evidence", "mode", "source")) local({
    fixture <- custom_update_fixture()
    local_custom_update_runtime()
    other <- fixture$args
    if (change == "evidence")
      other$custom_models$replay_mean$validation$checks$arithmetic <- "Different approved evidence"
    if (change == "mode") other$run_global_models <- TRUE
    if (change == "source") other$custom_models <- list(other_mean = custom_update_model("other_mean"))
    changed <- do.call(set_agent_info, other)
    before <- tools::md5sum(list.files(other$project_info$path, recursive = TRUE, full.names = TRUE))
    expect_error(update_forecast(changed, allow_iterate_forecast = FALSE), "same enrollment|series membership")
    expect_identical(tools::md5sum(names(before)), before)
  })
})

test_that("corrupt custom update provenance remains a hard immutable error", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  path <- local_artifact_path(agent_custom_parent_info(current), "logs", "-agent-custom-update", extension = "rds")
  saveRDS(NULL, path)
  before <- tools::md5sum(path)
  expect_error(update_forecast(current, allow_iterate_forecast = FALSE), "provenance identity")
  expect_identical(tools::md5sum(path), before)
})
