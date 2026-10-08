# Build valid setup arguments in a caller-owned temporary project. No provider
# calls are made; a Chat template suffices for setup and metadata-only resumes.
hierarchy_setup_args <- function(path) {
  project <- set_project_info(
    project_name = "hierarchy_compatibility", path = path,
    combo_variables = c("Region", "Site"), target_variable = "Revenue",
    date_type = "month", overwrite = TRUE
  )
  list(project_info = project, llm = structure(list(), class = "Chat"),
    input_data = data.frame(Region = "North", Site = "Seattle",
      Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Revenue = seq_len(12)),
    forecast_horizon = 2, hist_start_date = as.Date("2024-01-01"),
    hist_end_date = as.Date("2024-12-01"), combo_cleanup_date = as.Date("2024-07-01"),
    allow_hierarchical_forecast = TRUE, run_global_models = FALSE,
    run_local_models = TRUE, overwrite = TRUE)
}

test_that("single retained series sets up without HTS and keeps drivers", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  args$input_data$Ramadan <- 0
  args$external_regressors <- "Ramadan"
  inactive <- transform(args$input_data, Site = "Inactive", Revenue = 0)
  args$input_data <- rbind(args$input_data, inactive)
  testthat::local_mocked_bindings(
    prep_hierarchical_data = function(...) stop("Unexpected HTS construction"))
  agent <- do.call(set_agent_info, args)
  expect_identical(agent$forecast_approach, "bottoms_up")
  info <- args$project_info
  info$run_name <- agent$run_id
  saved <- read_file(info,
    file_list = local_artifact_path(info, "input_data", combo = hash_data("North--Seattle")),
    return_type = "df")
  expect_equal(unique(saved$Ramadan), 0)
  expect_identical(unique(saved$Combo), "North--Seattle")
  expect_false("ID" %in% names(saved))
})

test_that("existing versions retain saved contracts without detection or writes", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  original <- do.call(set_agent_info, args)
  logs <- load_agent_runs(args$project_info)
  args$overwrite <- FALSE
  args$input_data$Revenue <- args$input_data$Revenue + 100
  testthat::local_mocked_bindings(
    load_agent_runs = function(...) logs,
    hierarchy_detect = function(...) stop("Unexpected resume detection"),
    prep_hierarchical_data = function(...) stop("Unexpected resume preparation"),
    write_data = function(...) stop("Unexpected resume artifact rewrite"))
  for (approach in c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")) {
    logs$forecast_approach <- approach
    resumed <- do.call(set_agent_info, args)
    expect_identical(resumed$run_id, original$run_id)
    expect_identical(resumed$forecast_approach, approach)
  }
  expect_error(do.call(set_agent_info, modifyList(args, list(forecast_horizon = 3))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(allow_hierarchical_forecast = FALSE))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(hist_end_date = as.Date("2024-11-01")))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(combo_cleanup_date = as.Date("2024-08-01")))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(negative_forecast = TRUE))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(run_global_models = TRUE))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(back_test_scenarios = 2))),
    "Inputs have recently changed")
  expect_error(do.call(set_agent_info, modifyList(args, list(back_test_spacing = 2))),
    "Inputs have recently changed")
  logs$forecast_approach <- "bottoms_up"
  logs$allow_hierarchical_forecast <- TRUE
  expect_error(do.call(set_agent_info, modifyList(args, list(allow_hierarchical_forecast = FALSE))),
    "Inputs have recently changed")
  logs$forecast_approach <- NA_character_
  expect_error(do.call(set_agent_info, args), "saved forecast_approach")
})

test_that("resumes keep uploaded values rather than accepting replacement history", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  original <- do.call(set_agent_info, args)
  info <- args$project_info
  info$run_name <- original$run_id
  input_path <- local_artifact_path(info, "input_data", combo = hash_data("North--Seattle"))
  before <- readBin(input_path, "raw", n = 100000)
  args$overwrite <- FALSE
  args$input_data$Revenue <- 1000
  resumed <- do.call(set_agent_info, args)
  expect_identical(resumed$run_id, original$run_id)
  expect_identical(readBin(input_path, "raw", n = 100000), before)
  expect_equal(read_file(info, file_list = input_path)$Target, seq_len(12))
})

test_that("new versions use corrected detection rather than predecessor approach", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  first <- do.call(set_agent_info, args)
  logs <- load_agent_runs(args$project_info)
  logs$forecast_approach <- "standard_hierarchy"
  testthat::local_mocked_bindings(load_agent_runs = function(...) logs,
    prep_hierarchical_data = function(...) stop("Unexpected single-series HTS"))
  next_version <- do.call(set_agent_info, args)
  expect_equal(next_version$agent_version, first$agent_version + 1)
  expect_identical(next_version$forecast_approach, "bottoms_up")
})

test_that("empty new populations fail before saving inputs including one-column policy", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  args$input_data$Revenue <- 0
  testthat::local_mocked_bindings(write_data = function(...) stop("Unexpected empty write"))
  expect_error(do.call(set_agent_info, args), "No retained time series")
  args$project_info$combo_variables <- "Site"
  expect_error(do.call(set_agent_info, args), "No retained time series")
})

test_that("new setup versions prepare standard and grouped populations independently", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  timestamp <- as.POSIXct("2026-01-01", tz = "UTC")
  # Separate saved versions must not depend on the wall-clock setup duration.
  testthat::local_mocked_bindings(get_timestamp = function() {
    timestamp <<- timestamp + 1
    timestamp
  })
  history <- args$input_data
  args$input_data <- dplyr::bind_rows(
    transform(history, Region = "North", Site = "a"),
    transform(history, Region = "North", Site = "b"),
    transform(history, Region = "South", Site = "c"),
    transform(history, Region = "South", Site = "d"))
  standard <- do.call(set_agent_info, args)
  expect_identical(standard$forecast_approach, "standard_hierarchy")
  args$input_data$Site <- rep(c("a", "b", "a", "b"), each = nrow(history))
  grouped <- do.call(set_agent_info, args)
  expect_identical(grouped$forecast_approach, "grouped_hierarchy")
  expect_equal(grouped$agent_version, standard$agent_version + 1)
  expect_false(identical(grouped$run_id, standard$run_id))
  logs <- load_agent_runs(args$project_info)
  expect_setequal(logs$forecast_approach, c("standard_hierarchy", "grouped_hierarchy"))
})

test_that("repeated request IDs resume saved versions for both API actions", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  args$request_id <- "request-one"
  args$agent_action <- "iterate_forecast"
  first <- do.call(set_agent_info_custom, args)
  testthat::local_mocked_bindings(
    hierarchy_detect = function(...) stop("Unexpected retry detection"),
    write_data = function(...) stop("Unexpected retry artifact rewrite"))
  for (action in c("iterate_forecast", "update_forecast")) {
    args$agent_action <- action
    retry <- do.call(set_agent_info_custom, args)
    expect_identical(retry$run_id, first$run_id)
    expect_identical(retry$agent_version, first$agent_version)
    expect_identical(retry$overwrite, action == "update_forecast")
  }
})

test_that("request retries select their own contract after another version is created", {
  args <- hierarchy_setup_args(withr::local_tempdir())
  timestamp <- as.POSIXct("2026-01-01", tz = "UTC")
  # Distinct creation times keep the fixture independent of setup execution speed.
  testthat::local_mocked_bindings(get_timestamp = function() {
    timestamp <<- timestamp + 1
    timestamp
  })
  args$request_id <- "request-A"
  args$agent_action <- "iterate_forecast"
  first <- do.call(set_agent_info_custom, args)
  args$request_id <- "request-B"
  args$forecast_horizon <- 3
  second <- do.call(set_agent_info_custom, args)
  expect_equal(second$agent_version, first$agent_version + 1)
  expect_false(identical(first$run_id, second$run_id))
  logs <- load_agent_runs(args$project_info)
  logs$forecast_approach[logs$run_id == first$run_id] <- "standard_hierarchy"
  logs$forecast_approach[logs$run_id == second$run_id] <- "grouped_hierarchy"
  testthat::local_mocked_bindings(
    load_agent_runs = function(...) logs,
    hierarchy_detect = function(...) stop("Unexpected retry detection"),
    prep_hierarchical_data = function(...) stop("Unexpected retry preparation"),
    write_data = function(...) stop("Unexpected retry artifact rewrite"))
  for (action in c("iterate_forecast", "update_forecast")) {
    args$agent_action <- action
    args$request_id <- "request-A"
    args$forecast_horizon <- 2
    retry <- do.call(set_agent_info_custom, args)
    expect_identical(retry$run_id, first$run_id)
    expect_identical(retry$agent_version, first$agent_version)
    expect_identical(retry$forecast_approach, "standard_hierarchy")
    expect_identical(retry$overwrite, action == "update_forecast")
    args$forecast_horizon <- 3
    expect_error(do.call(set_agent_info_custom, args), "Inputs have recently changed")
    args$request_id <- "request-B"
    retry <- do.call(set_agent_info_custom, args)
    expect_identical(retry$run_id, second$run_id)
    expect_identical(retry$forecast_approach, "grouped_hierarchy")
  }
  public_args <- args[setdiff(names(args), c("request_id", "agent_action"))]
  public_args$overwrite <- FALSE
  expect_identical(do.call(set_agent_info, public_args)$run_id, second$run_id)
  logs <- dplyr::bind_rows(logs, logs[logs$run_id == first$run_id, ])
  args$request_id <- "request-A"
  expect_error(do.call(set_agent_info_custom, args), "multiple saved Agent runs")
  public_args$resume_run_id <- "absent-run"
  expect_error(do.call(set_agent_info_impl, public_args), "exactly one saved run")
  logs <- logs[!duplicated(logs$run_id), ]
  logs$run_id[logs$request_id == "request-A"] <- NA_character_
  expect_error(do.call(set_agent_info_custom, args), "saved run_id is missing or invalid")
})

test_that("Agent pair diagnostics and saved schema preserve legacy contracts", {
  info <- list(project_name = "pair_tests", path = withr::local_tempdir(),
    storage_object = NULL, data_output = "csv", object_output = "rds",
    combo_variables = c("A", "B", "C"))
  agent <- list(project_info = info, run_id = "pairs")
  populations <- list(
    data.frame(A = c("n", "n", "s"), B = c("x", "y", "z"), C = letters[1:3]),
    data.frame(A = c("n", "n", "s"), B = c("x", "y", "x"), C = letters[1:3]),
    data.frame(A = "n", B = "x", C = "a"))
  for (data in populations) {
    text <- hierarchy_detect(agent, data)
    saved_info <- info
    saved_info$run_name <- agent$run_id
    entry <- read_file(saved_info,
      file_list = local_artifact_path(saved_info, "eda", "-hierarchy", extension = "rds"),
      return_type = "object")
    expect_identical(names(entry), c("timestamp", "type", "hierarchy", "pair_tests"))
    pairs <- expand.grid(from = names(data), to = names(data), stringsAsFactors = FALSE)
    pairs <- pairs[pairs$from != pairs$to, ]
    expect_identical(names(entry$pair_tests), paste0(pairs$from, "->", pairs$to))
    for (i in seq_len(nrow(pairs))) {
      labels <- unique(data[, c(pairs$from[i], pairs$to[i])])
      expect_identical(unname(entry$pair_tests[i]),
        if (any(table(labels[[pairs$to[i]]]) > 1L)) "many-to-many" else "one-to-many")
    }
    expect_identical(entry$hierarchy, switch(detect_hierarchy(data, names(data))$forecast_approach,
      bottoms_up = "none", standard_hierarchy = "standard", grouped_hierarchy = "grouped"))
    expect_match(text, "Hierarchy detection:", fixed = TRUE)
    before <- readBin(local_artifact_path(saved_info, "eda", "-hierarchy", extension = "rds"),
      "raw", n = 100000)
    expect_match(hierarchy_detect(agent), "already exists", fixed = TRUE)
    expect_identical(readBin(local_artifact_path(saved_info, "eda", "-hierarchy", extension = "rds"),
      "raw", n = 100000), before)
  }
  expect_error(hierarchy_detect(agent, populations[[1]][0, ], write_data = FALSE), "No retained")
  expect_identical(hierarchy_detect(agent, data.frame(A = 1), write_data = FALSE),
    "FAIL: combo column(s) missing in df.")
})

test_that("legacy labels remain structural labels in Agent analysis", {
  agent <- list(project_info = list(combo_variables = c("A", "B")), run_id = "legacy")
  data <- data.frame(A = c(NA, "n"), B = c("a", "b"))
  expect_identical(hierarchy_detect(agent, data, write_data = FALSE), "standard_hierarchy")
  expect_error(detect_hierarchy(data, c("A", "B")), "Missing, blank")
})

test_that("every outer approach transition rejects predecessor reuse before fitting", {
  approaches <- c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")
  current <- list(project_info = list(path = "unused", project_name = "transition"),
    run_id = "current", agent_version = 3, overwrite = TRUE)
  previous <- list(agent_info = list(forecast_approach = "standard_hierarchy"),
    best_runs_tbl = data.frame(), hierarchy_summary_tbl = data.frame())
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    load_update_runs = function(...) data.frame(),
    list_files = function(...) c("previous", "current"),
    read_file = function(...) data.frame(agent_version = c(3, 2, 1)),
    find_completed_previous_agent_runs = function(...) list(previous),
    get_total_combos = function(...) stop("Reached reuse after incompatible transition"),
    train_models = function(...) stop("Unexpected fit"))
  for (from in approaches) {
    for (to in setdiff(approaches, from)) {
      previous$agent_info$forecast_approach <- from
      current$forecast_approach <- to
      expect_error(initial_checks(current),
        paste0("Current forecast approach is '", to, "' but the previous agent run used '", from, "'"),
        fixed = TRUE)
    }
  }
})
