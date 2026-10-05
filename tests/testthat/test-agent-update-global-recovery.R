# Change only one current source's future predictions in the deterministic
# fitted-table fixture. Historical predictions and all artifact identities stay
# intact. Infinite paths exercise hard rejection; finite shifts exercise soft risk.
shift_update_source <- function(current, source, factors) {
  for (index in seq_along(factors)) {
    rows <- current$fitted$Forecast_Tbl[[index]]
    future <- rows$Combo == source & rows$Run_Type == "Future_Forecast"
    rows$Forecast[future] <- current$contexts[[source]]$history$Target[1] * factors[index]
    current$fitted$Forecast_Tbl[[index]] <- rows
  }
  current
}

test_that("hierarchical recovery persists replacement identities through restart and growth", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    path <- withr::local_tempdir()
    previous <- make_update_chain_case(path, approach = approach, uniform = TRUE)
    current <- make_update_chain_case(path, "recovered", approach, legacy = TRUE)
    changed <- current$hierarchy$hts_combos[2]
    current <- shift_update_source(current, changed, c(Inf, 1))
    step <- run_update_chain_step(previous, current)
    expect_identical(step$result$status, "done")
    expect_length(step$result$quality_rejected_combos, 0L)
    if (!identical(step$result$status, "done")) next

    log <- read_selection_file(current$info, "logs")
    mapping <- read_global_update_selection(current$info, log, previous$hierarchy$original_combos)
    expected <- previous$winners
    expected[changed] <- current$ids[2]
    expect_identical(mapping$selected_ids, expected)
    expect_identical(mapping$components[[changed]], current$ids[2])
    expect_false(any(startsWith(names(log), "global_update_")))
    expect_equal(sum(mapping$selected_ids != previous$winners), 1)
    rows <- read_candidate_forecasts(current$info, changed, log, reconciled = FALSE)
    expect_true(all(is.finite(rows$Forecast)))
    expect_true(all(rows$Model_ID[rows$Best_Model == "Yes"] == current$ids[2]))
    expect_false(any(rows$Recipe_ID == "simple_average"))
    restarted <- run_update_chain_step(previous, current)
    expect_identical(restarted$result$status, "done")
    expect_length(restarted$fits, 0L)

    # A different valid winner must reproduce recovery from the predecessor;
    # conflicting winner flags must also fail the source reader's own checks.
    untouched <- setdiff(current$hierarchy$hts_combos, changed)[1]
    single <- read_selection_file(current$info, "forecasts", "-global_models", untouched)
    average <- read_selection_file(current$info, "forecasts", "-average_models", untouched)
    altered <- single
    altered$Best_Model <- ifelse(altered$Model_ID == current$ids[1], "Yes", "No")
    write_data(altered, untouched, current$info, "data", "forecasts", "-global_models")
    altered_average <- average
    altered_average$Best_Model <- "No"
    write_data(altered_average, untouched, current$info, "data", "forecasts", "-average_models")
    expect_null(read_update_result(current$info, previous$hierarchy$original_combos, TRUE, 6,
      approach, current$splits, "R1", previous$winners, previous$hierarchy,
      allow_selection_recovery = TRUE))
    write_data(average, untouched, current$info, "data", "forecasts", "-average_models")
    expect_null(read_update_result(current$info, previous$hierarchy$original_combos, TRUE, 6,
      approach, current$splits, "R1", previous$winners, previous$hierarchy,
      allow_selection_recovery = TRUE))
    expect_error(read_global_update_selection(current$info, log, previous$hierarchy$original_combos),
      "winner is missing or ambiguous", class = "finnts_update_artifact_error")
    write_data(single, untouched, current$info, "data", "forecasts", "-global_models")
    write_data(average, untouched, current$info, "data", "forecasts", "-average_models")

    following <- make_update_chain_case(path, "after-recovery", approach, legacy = TRUE,
      members = rbind(current$members, data.frame(Region = "West", Product = "C")))
    expect_length(changed_update_hierarchy_sources(current$hierarchy, following$hierarchy), 0L)
    next_step <- run_update_chain_step(current, following)
    expect_identical(next_step$result$status, "done")
    expect_length(next_step$result$quality_rejected_combos, 0L)
    next_mapping <- read_global_update_selection(following$info,
      read_selection_file(following$info, "logs"), previous$hierarchy$original_combos)
    expect_setequal(names(next_mapping$selected_ids), following$hierarchy$hts_combos)
    expect_identical(next_mapping$selected_ids[names(expected)], expected)
    expect_setequal(read_selection_file(following$info, "forecasts", "-reconciled", "Best-Model")$Combo,
      previous$hierarchy$original_combos)

    reduced <- make_update_chain_case(path, "removed-after-recovery", approach, legacy = TRUE,
      members = current$members)
    expect_length(changed_update_hierarchy_sources(following$hierarchy, reduced$hierarchy), 0L)
    removed <- run_update_chain_step(following, reduced)
    expect_identical(removed$result$status, "done")
    reduced_mapping <- read_global_update_selection(reduced$info,
      read_selection_file(reduced$info, "logs"), previous$hierarchy$original_combos)
    expect_identical(reduced_mapping$selected_ids, expected)
    expect_setequal(names(reduced_mapping$selected_ids), reduced$hierarchy$hts_combos)
  }
})

test_that("recovered weekly CSV output resumes only with unambiguous source decisions", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE, date_type = "week", data_output = "csv")
  current <- make_update_chain_case(path, "weekly-recovery", legacy = TRUE,
    date_type = "week", data_output = "csv")
  previous$log$weekly_to_daily <- TRUE
  write_data(previous$log, NULL, previous$info, "log", "logs")
  changed <- current$hierarchy$hts_combos[2]
  current <- shift_update_source(current, changed, c(Inf, 1))
  step <- run_update_chain_step(previous, current)
  expect_identical(step$result$status, "done")
  rows <- read_selection_file(current$info, "forecasts", "-global_models", changed)
  expect_true("Date_Day" %in% names(rows))
  expect_equal(sum(rows$Train_Test_ID == 1L & rows$Best_Model == "Yes"), 42L)
  expect_length(run_update_chain_step(previous, current)$fits, 0L)
  log <- read_selection_file(current$info, "logs")
  expect_false(any(startsWith(names(log), "global_update_")))
  untouched <- setdiff(current$hierarchy$hts_combos, changed)[1]
  single <- read_selection_file(current$info, "forecasts", "-global_models", untouched)
  average <- read_selection_file(current$info, "forecasts", "-average_models", untouched)
  for (defect in c("missing_winner", "duplicate_winner")) {
    damaged_single <- single
    damaged_average <- average
    damaged_single$Best_Model <- ifelse(
      defect == "duplicate_winner" & damaged_single$Model_ID == current$ids[1], "Yes", "No")
    damaged_average$Best_Model <- if (defect == "duplicate_winner") "Yes" else "No"
    write_data(damaged_single, untouched, current$info, "data", "forecasts", "-global_models")
    write_data(damaged_average, untouched, current$info, "data", "forecasts", "-average_models")
    expect_null(read_update_result(current$info, previous$hierarchy$original_combos, TRUE, 6,
      "standard_hierarchy", current$splits, "R1", previous$winners, previous$hierarchy,
      allow_selection_recovery = TRUE))
  }
  write_data(single, untouched, current$info, "data", "forecasts", "-global_models")
  write_data(average, untouched, current$info, "data", "forecasts", "-average_models")
  expect_length(run_update_chain_step(previous, current)$fits, 0L)
})

test_that("recovery excludes incomplete components and respects multi-recipe averaging limits", {
  fixture <- make_update_chain_case(withr::local_tempdir(), models = c("xgboost", "xgboost", "chronos2"),
    recipes = c("R1", "R2", "R1"), legacy = TRUE)
  source <- fixture$hierarchy$hts_combos[2]
  rows <- fixture$source[fixture$source$Combo == source & fixture$source$Recipe_ID != "simple_average", ]
  series <- fixture$contexts[[source]]
  for (defect in c("missing_key", "duplicate_key", "missing_component", "nonfinite", "magnitude")) {
    damaged <- rows
    index <- which(damaged$Model_ID == fixture$ids[1] & damaged$Train_Test_ID == 1L)
    if (defect == "missing_key") damaged <- damaged[-index[1], ]
    if (defect == "duplicate_key") damaged <- dplyr::bind_rows(damaged, damaged[index[1], ])
    if (defect == "missing_component") damaged <- damaged[damaged$Model_ID != fixture$ids[1], ]
    if (defect == "nonfinite") damaged$Forecast[index] <- Inf
    if (defect == "magnitude") damaged$Forecast[index] <- 1e10
    choice <- recover_global_update_source(damaged, series, fixture$splits,
      fixture$ids[1], max_model_average = 2L)
    expect_true(choice$recovered, info = defect)
    expect_false(is.na(choice$selection$selected_id), info = defect)
    expect_false(any(grepl(fixture$ids[1], choice$forecasts$Model_ID, fixed = TRUE)), info = defect)
    expect_true(all(lengths(strsplit(choice$selection$rankings$Model_ID, "_", fixed = TRUE)) <= 2L))
  }
  single_only <- recover_global_update_source(rows, series, fixture$splits, NULL, 1L)
  expect_false(any(single_only$forecasts$Recipe_ID == "simple_average"))
  saved_average <- recover_global_update_source(rows, series, fixture$splits, sort(fixture$ids), 1L)
  expect_identical(saved_average$selection$selected_id, paste(sort(fixture$ids), collapse = "_"))
  expect_false(saved_average$recovered)

  hidden <- rows
  future <- hidden$Model_ID == fixture$ids[1] & hidden$Train_Test_ID == 1L
  hidden$Forecast[future] <- series$history$Target[1] * 150
  diluted <- average_update_forecasts(hidden, sort(fixture$ids))
  expect_true(select_series_forecasts(diluted, series, fixture$splits)$rankings$Eligible)
  screened <- recover_global_update_source(hidden, series, fixture$splits, sort(fixture$ids), 3L)
  expect_false(any(grepl(fixture$ids[1], screened$forecasts$Model_ID, fixed = TRUE)))
  expect_false(grepl(fixture$ids[1], screened$selection$selected_id, fixed = TRUE))
})

test_that("hierarchical retuning persists the fresh recovery decision", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE)
  current <- make_update_chain_case(path, "retuned-recovery", legacy = TRUE)
  source <- current$hierarchy$hts_combos[2]
  current <- shift_update_source(current, source, c(Inf, 1))
  retuned <- shift_update_source(current, source, c(1, Inf))$fitted
  # Both initial backtests degrade enough to trigger the existing 10% retune.
  current$fitted$Forecast_Tbl <- lapply(current$fitted$Forecast_Tbl, function(rows) {
    rows$Forecast[rows$Run_Type == "Back_Test"] <- rows$Target[rows$Run_Type == "Back_Test"] * 1.2
    rows
  })
  step <- run_update_chain_step(previous, current, retuned = retuned)
  expect_identical(step$result$status, "done")
  expect_length(step$fits, 2L)
  mapping <- read_global_update_selection(current$info, read_selection_file(current$info, "logs"),
    previous$hierarchy$original_combos)
  expect_identical(mapping$selected_ids[[source]], current$ids[1])
  expect_length(run_update_chain_step(previous, current)$fits, 0L)
})

test_that("interrupted recovered writes refit or resume without publishing partial sources", {
  for (suffix in c("-global_models", "-average_models")) {
    path <- withr::local_tempdir()
    previous <- make_update_chain_case(path, uniform = TRUE)
    current <- make_update_chain_case(path, paste0("interrupted", suffix), legacy = TRUE)
    changed <- current$hierarchy$hts_combos[2]
    current <- shift_update_source(current, changed, c(Inf, 1))
    untouched <- current$hierarchy$hts_combos[1]
    expect_error(run_update_chain_step(previous, current, drop_source = untouched, drop_suffix = suffix),
      "Cannot log an incomplete global update", class = "finnts_update_artifact_error")
    repaired <- run_update_chain_step(previous, current)
    expect_identical(repaired$result$status, "done")
    expect_length(repaired$fits, 1L)
    expect_length(run_update_chain_step(previous, current)$fits, 0L)
  }
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE)
  current <- make_update_chain_case(path, "completion-interruption", legacy = TRUE)
  current <- shift_update_source(current, current$hierarchy$hts_combos[2], c(Inf, 1))
  logger <- log_best_run
  local_mocked_bindings(log_best_run = function(...) stop("completion interrupted"))
  expect_error(run_update_chain_step(previous, current), "completion interrupted")
  local_mocked_bindings(log_best_run = logger)
  resumed <- run_update_chain_step(previous, current)
  expect_identical(resumed$result$status, "done")
  expect_length(resumed$fits, 0L)
})

test_that("global recovery ranks soft concerns without weakening hard eligibility", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE)
  current <- make_update_chain_case(path, "soft-recovery", legacy = TRUE)
  changed <- current$hierarchy$hts_combos[2]
  current <- shift_update_source(current, changed, c(4, 3))
  step <- run_update_chain_step(previous, current)
  expect_identical(step$result$status, "done")
  expect_length(step$result$quality_rejected_combos, 0L)
  if (!identical(step$result$status, "done")) return(invisible(NULL))
  log <- read_selection_file(current$info, "logs")
  expect_false(any(startsWith(names(log), "global_update_")))
  rows <- read_candidate_forecasts(current$info, changed, log, reconciled = FALSE)
  selection <- select_series_forecasts(rows, read_series_history(current$info, changed, log), current$splits)
  chosen <- unique(rows$Model_ID[rows$Best_Model == "Yes"])
  score <- selection$rankings[selection$rankings$Model_ID == chosen, ]
  expect_true(score$Eligible)
  expect_gt(score$Violations, 0)
  expect_identical(chosen, selection$selected_id)
  expect_length(run_update_chain_step(previous, current)$fits, 0L)
})

test_that("hierarchical recovery can replace a failing singleton from the fitted union", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path)
  current <- make_update_chain_case(path, "singleton-recovery", legacy = TRUE)
  changed <- names(previous$winners)[previous$winners == previous$ids[1]][1]
  expect_false(is.na(changed))
  current <- shift_update_source(current, changed, c(1000, 1))
  step <- run_update_chain_step(previous, current)
  expect_identical(step$result$status, "done")
  if (!identical(step$result$status, "done")) return(invisible(NULL))
  mapping <- read_global_update_selection(current$info, read_selection_file(current$info, "logs"),
    previous$hierarchy$original_combos)
  expect_identical(mapping$selected_ids[[changed]], current$ids[2])
  untouched <- setdiff(names(previous$winners), changed)
  expect_identical(mapping$selected_ids[untouched], previous$winners[untouched])
})

test_that("hard-invalid components cannot be rescued by averaging or partial reconciliation", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE)
  current <- make_update_chain_case(path, "no-valid-source", legacy = TRUE)
  changed <- current$hierarchy$hts_combos[2]
  current <- shift_update_source(current, changed, c(Inf, Inf))
  local_mocked_bindings(reconcile = function(...) stop("A partial hierarchy must not be reconciled."))
  step <- run_update_chain_step(previous, current)
  expect_identical(step$result$status, "Global forecast quality rejected")
  expect_setequal(step$result$quality_rejected_combos,
    vapply(previous$hierarchy$original_combos, hash_data, character(1)))
  expect_equal(nrow(read_selection_file(current$info, "forecasts", "-global_models",
    changed, optional = TRUE)), 0L)
})

test_that("the global wrapper never reports completion for a quality-rejected group", {
  hashes <- c("first-hash", "second-hash")
  local_mocked_bindings(
    update_forecast_combo = function(...) list(quality_rejected_combos = hashes)
  )
  best <- data.frame(combo = c("first", "second"), model_type = "global", best_run_name = "previous")
  result <- update_global_models(list(project_info = list()), best, NULL, FALSE, 1, 123)
  expect_identical(result$status, "Global forecast quality rejected")
  expect_identical(result$quality_rejected_combos, hashes)
  expect_length(result$failed_combos, 0L)
})
