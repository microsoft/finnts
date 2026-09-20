# Write 1000 deterministic result sets using a few genuine fitted lm templates.
# ownership selects local/shared/mixed models; state selects complete, partial or
# unstarted records. Returns agent identity, expected work, file sizes and setup
# time. Artifacts use ordinary package names; temporary lifetime is caller-owned.
make_completion_benchmark <- function(ownership, state, count = 1000L) {
  started <- proc.time()[["elapsed"]]
  path <- withr::local_tempdir(.local_envir = parent.frame())
  fs::dir_create(fs::path(path, c("logs", "models", "forecasts")))
  combos <- sprintf("series-%04d", seq_len(count))
  global <- switch(ownership, local = rep(FALSE, count), global = rep(TRUE, count),
    mixed = seq_len(count) <= count / 2)
  status <- if (state == "complete") rep(0L, count) else if (state == "unstarted")
    rep(1L, count) else seq_len(count) %% 7L
  agent <- list(run_id = "current", agent_version = 2, forecast_horizon = 6,
    project_info = list(project_name = "completion-benchmark", path = path,
      storage_object = NULL, data_output = "csv", object_output = "rds", date_type = "month"))
  fit <- stats::lm(stats::reformulate("wt", "mpg", env = baseenv()), data = mtcars)
  written_models <- character()
  expected_complete <- character()
  files <- character()
  for (index in seq_along(combos)) {
    combo <- combos[[index]]
    if (status[[index]] == 1L) next
    model_type <- if (global[[index]]) "global" else "local"
    info <- agent$project_info
    info$project_name <- paste0(info$project_name, "_", hash_data(if (global[[index]]) "all" else combo))
    info$run_name <- if (global[[index]]) "global-current" else paste0("current-", combo)
    ids <- paste(c("lm", "meanf", "ets"), model_type, "R1", sep = "--")
    model_path <- local_artifact_path(info, "models", "-single_models",
      hash_data(if (global[[index]]) "All-Data" else combo), "rds")
    if (!model_path %in% written_models) {
      if (global[[index]] || status[[index]] != 2L) {
        if (!global[[index]] && status[[index]] == 3L) {
          writeBin(charToRaw("damaged model"), model_path)
        } else {
          saveRDS(tibble::tibble(Combo_ID = if (global[[index]]) "All-Data" else combo,
            Model_ID = ids, Model_Name = c("lm", "meanf", "ets"),
            Model_Type = model_type, Recipe_ID = "R1", Model_Fit = rep(list(fit), 3)), model_path)
        }
        files <- c(files, model_path)
      }
      written_models <- c(written_models, model_path)
    }
    average <- index %% 2L == 0L
    template <- data.frame(Combo = combo, Train_Test_ID = rep(c(1, 2), each = 6),
      Date = c(seq(as.Date("2026-08-01"), by = "month", length.out = 6),
        seq(as.Date("2026-02-01"), by = "month", length.out = 6)),
      Target = rep(c(NA_real_, 100), each = 6), Forecast = 100)
    rows <- do.call(rbind, lapply(ids, function(model_id) {
      values <- template
      values$Model_ID <- model_id
      values$Best_Model <- if (!average && model_id == ids[[1]]) "Yes" else "No"
      values
    }))
    forecast_path <- local_artifact_path(info, "forecasts",
      if (global[[index]]) "-global_models" else "-single_models", hash_data(combo))
    if (status[[index]] != 4L) {
      if (status[[index]] == 5L) rows <- rows[rows$Date != as.Date("2026-08-01"), ]
      utils::write.csv(rows, forecast_path, row.names = FALSE)
      files <- c(files, forecast_path)
    }
    if (average) {
      values <- template
      values$Model_ID <- paste(sort(ids), collapse = "_")
      values$Best_Model <- "Yes"
      average_path <- local_artifact_path(info, "forecasts", "-average_models", hash_data(combo))
      utils::write.csv(values, average_path, row.names = FALSE)
      files <- c(files, average_path)
    }
    parent <- agent$project_info
    parent$run_name <- agent$run_id
    metadata_path <- local_artifact_path(parent, "logs", "-agent_best_run", hash_data(combo), "csv")
    utils::write.csv(data.frame(combo = combo, agent_run_id = agent$run_id,
      best_run_name = info$run_name, model_type = model_type, weighted_mape = 0.1,
      forecast_approach = "bottoms_up"), metadata_path, row.names = FALSE)
    files <- c(files, metadata_path)
    if (status[[index]] %in% c(0L, 6L) || (global[[index]] && status[[index]] %in% c(2L, 3L))) {
      expected_complete <- c(expected_complete, combo)
    }
  }
  if (any(global & status %in% c(4L, 5L))) expected_complete <- setdiff(expected_complete, combos[global])
  list(agent = agent, combos = combos, complete = expected_complete,
    files = files, bytes = sum(file.info(files)$size),
    setup_seconds = proc.time()[["elapsed"]] - started)
}

# Exercise actual metadata discovery/read, result validation and combo dispatch
# construction. Fits and forecasts are never mocked or loaded into the payload.
run_completion_benchmark <- function(fixture) {
  started <- proc.time()[["elapsed"]]
  metadata <- load_update_runs(fixture$agent)
  completed <- completed_update_runs(fixture$agent, metadata)
  work <- setdiff(fixture$combos, as.character(completed[["combo"]]))
  list(work = work, seconds = proc.time()[["elapsed"]] - started,
    payload_bytes = length(serialize(work, NULL)))
}

test_that("1000-series completion audits use real files and finish in seconds", {
  started <- proc.time()[["elapsed"]]
  fixture <- make_completion_benchmark("mixed", "complete")
  complete <- run_completion_benchmark(fixture)
  expect_identical(complete$work, character())
  expect_lt(complete$seconds, 10)

  local_combo <- fixture$combos[[501]]
  local_info <- fixture$agent$project_info
  local_info$project_name <- paste0(local_info$project_name, "_", hash_data(local_combo))
  local_info$run_name <- paste0("current-", local_combo)
  writeBin(charToRaw("damaged model"), local_artifact_path(local_info, "models",
    "-single_models", hash_data(local_combo), "rds"))
  global_info <- fixture$agent$project_info
  global_info$project_name <- paste0(global_info$project_name, "_", hash_data("all"))
  global_info$run_name <- "global-current"
  forecast_path <- local_artifact_path(global_info, "forecasts", "-global_models",
    hash_data(fixture$combos[[500]]))
  rows <- utils::read.csv(forecast_path)
  utils::write.csv(rows[-1, ], forecast_path, row.names = FALSE)
  damaged <- run_completion_benchmark(fixture)
  expect_identical(damaged$work, fixture$combos[seq_len(501)])
  expect_lt(damaged$seconds, 10)
  expect_equal(damaged$payload_bytes, length(serialize(fixture$combos[seq_len(501)], NULL)))

  empty <- make_completion_benchmark("mixed", "unstarted")
  unstarted <- run_completion_benchmark(empty)
  expect_identical(unstarted$work, empty$combos)
  expect_lt(unstarted$seconds, 10)
  expect_lt(proc.time()[["elapsed"]] - started, 90)
  cat(sprintf("\nCompletion benchmark: complete=%.3fs damaged=%.3fs unstarted=%.3fs setup=%.3fs files=%d bytes=%.0f combo_payload=%d\n",
    complete$seconds, damaged$seconds, unstarted$seconds, fixture$setup_seconds,
    length(fixture$files), fixture$bytes, damaged$payload_bytes))
})