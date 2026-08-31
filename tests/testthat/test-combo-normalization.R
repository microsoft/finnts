test_that("combo normalization trims only character-like identifier boundaries", {
  input_data <- tibble::tibble(
    Region = c(" North America ", NA_character_),
    Segment = factor(c(" Enterprise Sales ", " Consumer Products ")),
    Numeric_ID = c(101, 102),
    Date = as.Date(c("2024-01-01", "2024-02-01")),
    Target = c(10, 20)
  )

  normalized <- normalize_combo_values(
    input_data = input_data,
    combo_variables = c("Region", "Segment", "Numeric_ID")
  )

  expect_equal(normalized$Region, c("North America", NA_character_))
  expect_equal(normalized$Segment, c("Enterprise Sales", "Consumer Products"))
  expect_equal(normalized$Numeric_ID, c(101, 102))
  expect_type(normalized$Numeric_ID, "double")
})

test_that("combo validation rejects values that become blank", {
  input_data <- tibble::tibble(
    ID = c("A", "   "),
    Date = as.Date(c("2024-01-01", "2024-02-01")),
    Target = c(10, 20)
  )

  expect_error(
    check_input_data(
      input_data = input_data,
      combo_variables = "ID",
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "cannot be blank after trimming.*Affected columns: ID.*2024-02-01"
  )
})

test_that("combo validation rejects collisions created by normalization", {
  input_data <- tibble::tibble(
    ID = c("A", " A "),
    Date = as.Date(c("2024-01-01", "2024-01-01")),
    Target = c(10, 20)
  )

  expect_error(
    check_input_data(
      input_data = input_data,
      combo_variables = "ID",
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Found 2 duplicate rows.*surrounding whitespace identify the same series"
  )
})

test_that("set_agent_info writes one canonical combo identity", {
  output_path <- withr::local_tempdir()
  project <- set_project_info(
    project_name = "agent_combo_whitespace",
    path = output_path,
    combo_variables = c("Exec_Org", "Leader", "Group"),
    target_variable = "Target_Value",
    date_type = "month",
    overwrite = TRUE
  )
  input_data <- tibble::tibble(
    Exec_Org = rep(" AI Experiences ", 6),
    Leader = factor(rep(" MSA COS ", 6)),
    Group = rep(" Contractor Resource - CONTR ", 6),
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 6),
    Target_Value = seq_len(6)
  )

  agent_info <- set_agent_info(
    project_info = project,
    llm = structure(list(), class = "Chat"),
    input_data = input_data,
    forecast_horizon = 1,
    allow_hierarchical_forecast = FALSE,
    run_global_models = FALSE,
    overwrite = TRUE
  )

  expected_combo <- "AI Experiences--MSA COS--Contractor Resource - CONTR"
  project_for_read <- project
  project_for_read$run_name <- agent_info$run_id
  input_files <- list_files(
    project_for_read$storage_object,
    paste0(
      project_for_read$path, "/input_data/*",
      hash_data(project_for_read$project_name), "-",
      hash_data(agent_info$run_id), "-*.",
      project_for_read$data_output
    )
  )
  saved_input <- read_file(
    run_info = project_for_read,
    file_list = input_files,
    return_type = "df"
  )

  expect_length(input_files, 1)
  expect_equal(unique(saved_input$Exec_Org), "AI Experiences")
  expect_equal(unique(saved_input$Leader), "MSA COS")
  expect_equal(unique(saved_input$Group), "Contractor Resource - CONTR")
  expect_equal(unique(saved_input$Combo), expected_combo)
  expect_match(
    fs::path_file(input_files),
    paste0("-", hash_data(expected_combo), "\\.csv$")
  )

  reconstructed_combo <- saved_input %>%
    tidyr::unite(
      "Combo",
      tidyselect::all_of(project$combo_variables),
      sep = "--",
      remove = FALSE
    ) %>%
    dplyr::pull(Combo) %>%
    unique()

  expect_equal(hash_data(reconstructed_combo), hash_data(expected_combo))
})

test_that("prep_data uses the same canonical combo identity", {
  output_path <- withr::local_tempdir()
  run_info <- set_run_info(
    project_name = "standard_combo_whitespace",
    run_name = "standard_run",
    path = output_path,
    add_unique_id = FALSE
  )
  history_dates <- seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24)
  future_dates <- seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3)
  input_data <- tibble::tibble(
    Region = rep(" North America ", 27),
    Segment = rep(" Enterprise Sales ", 27),
    Date = c(history_dates, future_dates),
    Revenue = c(seq_len(24) + 100, rep(NA_real_, 3))
  )

  suppressWarnings(prep_data(
    run_info = run_info,
    input_data = input_data,
    combo_variables = c("Region", "Segment"),
    target_variable = "Revenue",
    date_type = "month",
    forecast_horizon = 3,
    hist_end_date = max(history_dates),
    box_cox = FALSE,
    stationary = FALSE,
    lag_periods = 3,
    rolling_window_periods = 3,
    recipes_to_run = "R1"
  ))

  prepped <- get_prepped_data(run_info, recipe = "R1")
  expected_combo <- "North America--Enterprise Sales"

  expect_equal(unique(prepped$Combo), expected_combo)
  expect_equal(unique(prepped$Region), "North America")
  expect_equal(unique(prepped$Segment), "Enterprise Sales")
  expect_equal(hash_data(unique(prepped$Combo)), hash_data(expected_combo))
})
