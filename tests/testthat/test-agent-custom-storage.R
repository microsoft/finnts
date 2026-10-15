# Map offline exact downloads to genuine artifacts in one isolated local project.
# Supported backend classes are exercised without credentials or network calls.
local_custom_storage_download <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    download_exact_artifact = function(storage, path, destination, allow_missing) {
      source <- fs::path(storage$root, fs::path_file(fs::path_dir(path)), fs::path_file(path))
      if (!file.exists(source)) {
        if (allow_missing) return(FALSE)
        stop("Missing required offline backend artifact.", call. = FALSE)
      }
      fs::file_copy(source, destination)
      TRUE
    },
    list_files = function(...) stop("Unexpected backend enumeration"),
    .package = "finnts", .env = .local_envir)
}

test_that("approved standard workflows load by exact path on supported backends", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    back_test_scenarios = 1, num_hyperparameters = 1, run_ensemble_models = FALSE)
  expected <- custom_run_read(info, TRUE)
  local_custom_storage_download()
  for (backend in c("blob_container", "ms_drive")) {
    remote <- info
    remote$storage_object <- structure(list(root = info$path), class = backend)
    remote$path <- fs::path(withr::local_tempdir(), "remote")
    expect_identical(custom_run_context(remote, expected$manifest$context, TRUE),
      custom_run_context(info, expected$manifest$context, TRUE))
    expect_identical(custom_run_read(remote, TRUE)$manifest, expected$manifest)
    local_mocked_bindings(download_exact_artifact = function(...)
      rlang::abort("Offline provider access denied", class = "offline_provider_error"), .package = "finnts")
    expect_error(custom_run_read(remote, TRUE), class = "offline_provider_error")
    local_custom_storage_download()
  }
})

test_that("update workers restore enrollment and replay without local backend probes", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  prepared <- prepare_agent_custom_update(fixture$current, 123)
  expected <- read_agent_custom_update(prepared, TRUE)
  local_custom_storage_download()
  for (backend in c("blob_container", "ms_drive")) {
    remote <- agent_custom_worker_info(prepared)
    remote$project_info$storage_object <- structure(list(root = prepared$project_info$path), class = backend)
    remote$project_info$path <- fs::path(withr::local_tempdir(), "remote")
    loaded <- load_agent_custom_update_state(remote)
    expect_identical(loaded$custom_agent_contract_id, prepared$custom_agent_contract_id)
    expect_identical(loaded$custom_agent_update, expected)
  }
})
