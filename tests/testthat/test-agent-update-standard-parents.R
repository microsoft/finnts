# Build real hierarchy metadata for the given member rows through
# prep_hierarchical_data() in a caller-owned temporary project. Combo names
# join the member values with "--"; no models or forecasts are created.
# return_info also returns storage settings for checking saved parent values.
parent_hierarchy <- function(members, approach = "standard_hierarchy",
                             .local_envir = parent.frame(), return_info = FALSE,
                             data_output = "csv") {
  info <- list(project_name = "parents", run_name = "version",
    path = withr::local_tempdir(.local_envir = .local_envir), storage_object = NULL,
    data_output = data_output, object_output = "rds")
  calendar <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  input <- do.call(rbind, lapply(seq_len(nrow(members)), function(index) {
    data.frame(members[index, , drop = FALSE],
      Combo = paste(members[index, ], collapse = "--"),
      Date = calendar, Target = 100, row.names = NULL)
  }))
  prep_hierarchical_data(input, info, names(members), NULL, approach, 12)
  hierarchy <- read_selection_hierarchy(info)
  if (return_info) list(hierarchy = hierarchy, info = info) else hierarchy
}

# Two regions with two products each: North is "A" and South is "B".
parent_base <- function() {
  data.frame(Region = c("North", "North", "South", "South"), Product = c("A", "B", "A", "B"))
}

# Return the parent-check error or NULL, forwarding optional saved-data context.
parent_check_error <- function(previous, current, previous_version = NULL, ...) {
  tryCatch({
    check_standard_hierarchy_parents(previous, current, previous_version, ...)
    NULL
  }, error = identity)
}

test_that("standard parent letters that name a different group are detected", {
  base <- parent_base()
  previous <- parent_hierarchy(base)
  cases <- list(
    add_first = list(rbind(base, data.frame(Region = "East", Product = "C")), c("A", "B")),
    add_last = list(rbind(base, data.frame(Region = "West", Product = "C")), character()),
    remove_first = list(base[3:4, ], "A"),
    remove_last = list(base[1:2, ], character()),
    replace_all_members = list(rbind(base[3:4, ],
      data.frame(Region = "Alpha", Product = c("A", "B"))), "A"),
    within_parent = list(rbind(base[1:3, ], data.frame(Region = "North", Product = "C")), character())
  )
  for (case in names(cases)) {
    current <- parent_hierarchy(cases[[case]][[1]])
    expect_identical(sort(standard_hierarchy_reassigned_parents(previous, current)),
      sort(cases[[case]][[2]]), info = case)
    error <- parent_check_error(previous, current, 3)
    if (length(cases[[case]][[2]])) {
      expect_s3_class(error, "finnts_hierarchy_change_error")
    } else {
      expect_null(error, info = case)
    }
  }
})

test_that("grouped hierarchies keep value-named nodes without a parent check", {
  base <- parent_base()
  previous <- parent_hierarchy(base, "grouped_hierarchy")
  current <- parent_hierarchy(rbind(base, data.frame(Region = "East", Product = "C")),
    "grouped_hierarchy")
  expect_length(standard_hierarchy_reassigned_parents(previous, current), 0L)
  expect_null(parent_check_error(previous, current, 3))
})

test_that("shifted middle-level parents are detected in deeper hierarchies", {
  members <- data.frame(Region = c("North", "North", "South", "South"),
    Country = c("X", "Y", "Z", "Z"), City = c("1", "2", "3", "4"))
  previous <- parent_hierarchy(members)
  current <- parent_hierarchy(rbind(members,
    data.frame(Region = "North", Country = "W", City = "5")))
  expect_identical(sort(standard_hierarchy_reassigned_parents(previous, current)), c("AA", "AB"))
})

for (format in c("csv", "parquet")) {
  test_that(paste("replacing every leaf preserves the same saved", format, "parent identity"), {
    if (format == "parquet") skip_if_not_installed("arrow")
    cases <- list(
      two_levels = data.frame(Region = c("North--America", "North--America", "South", "South"),
        Product = c("001", "002", "003", "004")),
      three_levels = data.frame(Region = c("North", "North", "South", "South"),
        Country = c("X", "Y", "Z", "Z"), City = c("001", "002", "003", "004"))
    )
    for (members in cases) local({
      previous <- parent_hierarchy(members, return_info = TRUE, data_output = format)
      replacement <- members
      last <- names(members)[ncol(members)]
      replacement[[last]][1:2] <- c("005", "006")
      current <- parent_hierarchy(replacement, return_info = TRUE, data_output = format)
      reads <- character()
      original_reader <- read_exact_artifact
      local_mocked_bindings(
        read_exact_artifact = function(run_info, file_list, ...) {
          reads <<- c(reads, file_list)
          original_reader(run_info, file_list, ...)
        },
        list_files = function(...) stop("Unexpected hierarchy directory listing")
      )
      expect_null(parent_check_error(previous$hierarchy, current$hierarchy, 3,
        previous_info = previous$info, current_info = current$info,
        combo_variables = names(members)))
      expect_length(reads, 2L)
      expect_true(all(grepl(paste0("-hts_data.", format), reads, fixed = TRUE)))

      replacement[[1]][1:2] <- "Alpha"
      changed <- parent_hierarchy(replacement, return_info = TRUE, data_output = format)
      expect_s3_class(parent_check_error(previous$hierarchy, changed$hierarchy, 3,
        previous_info = previous$info, current_info = changed$info,
        combo_variables = names(members)), "finnts_hierarchy_change_error")
    })
  })
}

test_that("parent identity checks preserve zero-padded group values and storage errors", {
  members <- data.frame(Region = c("001", "001", "002", "002"),
    Product = c("A", "B", "C", "D"))
  previous <- parent_hierarchy(members, return_info = TRUE)
  replacement <- members
  replacement$Product[1:2] <- c("E", "F")
  current <- parent_hierarchy(replacement, return_info = TRUE)
  expect_null(parent_check_error(previous$hierarchy, current$hierarchy,
    previous_info = previous$info, current_info = current$info,
    combo_variables = names(members)))
  replacement$Region[1:2] <- "1"
  changed <- parent_hierarchy(replacement, return_info = TRUE)
  expect_s3_class(parent_check_error(previous$hierarchy, changed$hierarchy,
    previous_info = previous$info, current_info = changed$info,
    combo_variables = names(members)), "finnts_hierarchy_change_error")

  local_mocked_bindings(read_exact_artifact = function(...) stop("Parent data access denied"))
  expect_error(check_standard_hierarchy_parents(previous$hierarchy, current$hierarchy,
    previous_info = previous$info, current_info = current$info,
    combo_variables = names(members)), "Parent data access denied", fixed = TRUE,
    class = "finnts_update_artifact_error")
})

test_that("outer updates keep a parent whose entire bottom population changed", {
  path <- withr::local_tempdir()
  members <- data.frame(Region = c("North", "North", "South", "South"),
    Product = c("001", "002", "003", "004"))
  calendar <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  input <- tidyr::expand_grid(members, Date = calendar)
  input$Value <- 100
  project <- set_project_info(project_name = "parent-replacement", path = path,
    combo_variables = names(members), target_variable = "Value", date_type = "month")
  args <- list(project_info = project, llm = structure(list(), class = "Chat"),
    input_data = input, forecast_horizon = 2, allow_hierarchical_forecast = TRUE,
    run_global_models = FALSE, run_local_models = TRUE, overwrite = TRUE)
  time <- as.POSIXct("2033-01-01", tz = "UTC")
  local_mocked_bindings(get_timestamp = function() {
    time <<- time + 1
    time
  })
  previous <- do.call(set_agent_info, args)
  info <- project
  info$run_name <- previous$run_id
  hierarchy <- read_selection_hierarchy(info)
  metadata <- data.frame(combo = hierarchy$hts_combos,
    agent_run_id = previous$run_id, best_run_name = "saved-local",
    model_type = "local", weighted_mape = 0.1)
  for (suffix in c("run_metadata", "forecast", "model_summary", "eda")) {
    write_data(if (suffix == "run_metadata") metadata else data.frame(complete = TRUE),
      NULL, info, "data", "final_output", paste0("-", suffix))
  }
  summarize_hierarchy(previous)
  args$input_data$Product[args$input_data$Region == "North"] <- rep(c("005", "006"), each = 24)
  current <- do.call(set_agent_info, args)
  current$project_info$combo_variables <- "ID"
  result <- initial_checks(current, hierarchy_combo_variables = names(members))
  expect_true(all(c("Total", "A", "B") %in% result$prev_best_runs_tbl$combo))
  expect_setequal(result$new_combos, vapply(c("North_005", "North_006"),
    hash_data, character(1), USE.NAMES = FALSE))
})

test_that("parent change errors show the groups and the next step", {
  base <- parent_base()
  previous <- parent_hierarchy(base)
  current <- parent_hierarchy(rbind(base[3:4, ],
    data.frame(Region = "Alpha", Product = c("A", "B"))))
  message <- conditionMessage(parent_check_error(previous, current, 3))
  expect_match(message, "Error in update_forecast().", fixed = TRUE)
  expect_match(message, "parent groups changed since Agent version 3", fixed = TRUE)
  expect_match(message, "name parent nodes by position (A, B, AA, ...)", fixed = TRUE)
  expect_match(message, "'A' was North--A, North--B; now Alpha--A, Alpha--B", fixed = TRUE)
  expect_match(message, "Keep the same parent groups as Agent version 3", fixed = TRUE)
  expect_match(message, "start a new history with iterate_forecast()", fixed = TRUE)
  message <- conditionMessage(parent_check_error(previous, current))
  expect_match(message, "since the previous Agent version", fixed = TRUE)
})

# Capture the real parent-change condition for a group whose every series was
# replaced, so callers see the production message and explanation fields.
parent_change_condition <- function(.local_envir = parent.frame()) {
  base <- parent_base()
  previous <- parent_hierarchy(base, .local_envir = .local_envir)
  current <- parent_hierarchy(rbind(base[3:4, ],
    data.frame(Region = "Alpha", Product = c("A", "B"))), .local_envir = .local_envir)
  parent_check_error(previous, current)
}

test_that("global updates count parent changes as failed series", {
  agent_info <- list(project_info = list(project_name = "parents", path = withr::local_tempdir(),
    storage_object = NULL, data_output = "csv", object_output = "rds"),
    run_id = "current", agent_version = 2, llm = NULL)
  best <- data.frame(combo = c("North--A", "South--A"), best_run_name = "global-run",
    model_type = "global", weighted_mape = 0.1)
  condition <- parent_change_condition()
  expect_s3_class(condition, "finnts_hierarchy_change_error")
  wrappers <- list(
    direct = function(...) stop(condition),
    isolated = function(...) stop(structure(class = c("callr_status_error", "error", "condition"),
      list(message = "Child R process failed.", call = NULL, parent = condition)))
  )
  for (mode in names(wrappers)) {
    testthat::local_mocked_bindings(
      load_agent_custom_update_state = function(agent_info) agent_info,
      update_forecast_combo = wrappers[[mode]]
    )
    expect_message(result <- update_global_models(agent_info, best, NULL, FALSE, 1, 123),
      "parent groups changed")
    expect_identical(result$status, "Global model update failed", info = mode)
    expect_setequal(result$failed_combos, vapply(best$combo, hash_data, character(1), USE.NAMES = FALSE))
    expect_length(result$quality_rejected_combos, 0L)
    expect_match(result$failure_reason, "'A' was North--A, North--B; now Alpha--A, Alpha--B", fixed = TRUE)
    expect_false(grepl("Error in update_forecast()", result$failure_reason, fixed = TRUE))
  }

  artifact_condition <- rlang::error_cnd(
    "finnts_update_artifact_error",
    message = "Cannot read saved hierarchy parent identities: Parent data access denied"
  )
  artifact_wrappers <- list(
    direct = function(...) stop(artifact_condition),
    isolated = function(...) stop(structure(class = c("callr_status_error", "error", "condition"),
      list(message = "Child R process failed.", call = NULL, parent = artifact_condition)))
  )
  for (mode in names(artifact_wrappers)) {
    testthat::local_mocked_bindings(
      load_agent_custom_update_state = function(agent_info) agent_info,
      update_forecast_combo = artifact_wrappers[[mode]]
    )
    expect_error(
      update_global_models(agent_info, best, NULL, FALSE, 1, 123),
      "Parent data access denied",
      fixed = TRUE,
      class = "finnts_update_artifact_error",
      info = mode
    )
  }
})

test_that("parent changes fall back within the failure limit and explain the limit error", {
  agent_info <- list(project_info = list(project_name = "parents"))
  testthat::local_mocked_bindings(
    resolve_combo_hashes = function(agent_info, combo_hashes) combo_hashes
  )
  reason <- parent_change_condition()$explanation
  series <- paste0("series-", seq_len(20))
  within <- check_update_failures(agent_info, data.frame(combo = series), series,
    global_failed_combos = series[1:10], local_failed_combos = character(),
    global_failure_reason = reason)
  expect_identical(within, series[1:10])
  message <- tryCatch({
    check_update_failures(agent_info, data.frame(combo = series), series,
      global_failed_combos = series[1:11], local_failed_combos = character(),
      global_failure_reason = reason)
    ""
  }, error = conditionMessage)
  expect_match(message, "11 time series failed to update, which exceeds the limit of 10", fixed = TRUE)
  expect_match(message, "parent groups changed since the previous Agent version", fixed = TRUE)
  expect_match(message, "'A' was North--A, North--B; now Alpha--A, Alpha--B", fixed = TRUE)
  expect_match(message, "start a new history with iterate_forecast()", fixed = TRUE)
})

test_that("the update workflow passes the global failure cause to the limit check", {
  project_info <- list(project_name = "parents", combo_variables = c("Region", "Product"))
  agent_info <- list(project_info = project_info, forecast_approach = "standard_hierarchy")
  agent_info$project_info$combo_variables <- "ID"
  testthat::local_mocked_bindings(
    load_agent_custom_state = function(agent_info, ...) agent_info,
    new_llm_session = function(...) NULL,
    run_graph = function(chat, workflow, ctx) workflow
  )
  workflow <- update_fcst_agent_workflow(agent_info, project_info, NULL, FALSE, 1)
  expect_identical(workflow$start$args$hierarchy_combo_variables, c("Region", "Product"))
  expect_identical(workflow$check_update_failures$args$global_failure_reason,
    "{results$update_global_models$failure_reason}")
})
