test_that("sparse raw hierarchy targets do not suppress accepted global publication", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    path <- withr::local_tempdir()
    previous <- make_update_chain_case(path, approach = approach, uniform = TRUE)
    current <- make_update_chain_case(path, "sparse-targets", approach, legacy = TRUE)
    raw <- read_selection_file(current$info, "prep_data", "-hts_data")
    missing <- current$hierarchy$original_combos[1]
    partial <- current$hierarchy$original_combos[2]
    raw$Target[raw$Combo == missing] <- NA_real_
    last <- which(raw$Combo == partial & raw$Date == max(raw$Date))
    raw$Target[last] <- 77
    write_data(raw, NULL, current$info, "data", "prep_data", "-hts_data")
    step <- run_update_chain_step(previous, current)
    expect_identical(step$result$status, "done")
    forecasts <- read_selection_file(current$info, "forecasts", "-reconciled", "Best-Model")
    expect_true(all(forecasts$Target[forecasts$Combo == missing & forecasts$Train_Test_ID > 1] == 100))
    expect_true(all(is.na(forecasts$Target[forecasts$Train_Test_ID == 1])))
    expect_true(all(forecasts$Target[forecasts$Combo == partial & forecasts$Date == max(raw$Date)] == 77))
    log <- read_selection_file(current$info, "logs")
    expect_true(is.finite(log$weighted_mape))
    parent <- step$agent$project_info
    parent$run_name <- step$agent$run_id
    for (combo in previous$hierarchy$original_combos) {
      best <- read_selection_file(parent, "logs", "-agent_best_run", combo, optional = TRUE)
      expect_equal(nrow(best), 1L)
      if (nrow(best)) expect_identical(best$best_run_name, current$info$run_name)
    }
    expect_length(run_update_chain_step(previous, current)$fits, 0L)
  }
})

test_that("a global update cannot report done when publication retained no series", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE)
  current <- make_update_chain_case(path, "unpublished", legacy = TRUE)
  local_mocked_bindings(log_best_run = function(...) {
    list(status = "rejected", selected_combos = character())
  })
  expect_error(run_update_chain_step(previous, current),
    "did not publish every requested global series", class = "finnts_update_artifact_error")
})
