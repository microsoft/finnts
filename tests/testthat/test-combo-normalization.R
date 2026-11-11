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

test_that("combo normalization treats values left blank by trimming as missing", {
  input_data <- tibble::tibble(
    Region = c("", "   ", " North  America ", NA_character_),
    Segment = factor(c(" ", "Enterprise", "Consumer", "Consumer")),
    Numeric_ID = c(1, NA, NaN, 4),
    Driver = c("", "x", "y", "z"),
    Target = c(NA, 2, 3, 4)
  )

  normalized <- normalize_combo_values(
    input_data = input_data,
    combo_variables = c("Region", "Segment", "Numeric_ID")
  )

  expect_identical(normalized$Region, c(NA, NA, "North  America", NA))
  expect_identical(normalized$Segment, c(NA, "Enterprise", "Consumer", "Consumer"))
  expect_identical(normalized$Numeric_ID, input_data$Numeric_ID)
  expect_identical(normalized$Driver, input_data$Driver)
  expect_identical(normalized$Target, input_data$Target)
})

# Run the shared input checks on monthly rows with a numeric `Target` column.
# Returns what check_input_data() returns; errors and warnings propagate. No I/O.
check_combo_input <- function(input_data, combo_variables) {
  check_input_data(
    input_data = input_data,
    combo_variables = combo_variables,
    target_variable = "Target",
    external_regressors = NULL,
    date_type = "month",
    fiscal_year_start = 1,
    parallel_processing = NULL
  )
}

test_that("combo validation accepts values that become blank as missing values", {
  input_data <- tibble::tibble(
    ID = c("A", "   ", ""),
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    Target = c(10, 20, 30)
  )

  expect_silent(check_combo_input(input_data, "ID"))
})

test_that("combo validation warns when a column mixes missing values and NA text", {
  mixed <- tibble::tibble(
    Leader = c(NA, "NA", " ", "North"),
    Product = c("X", "Y", "Z", "X"),
    Date = as.Date("2024-01-01"),
    Target = c(1, 2, 3, 4)
  )
  expect_warning(
    check_combo_input(mixed, c("Leader", "Product")),
    "Combo column 'Leader' contains both missing values and the text \"NA\"",
    fixed = TRUE
  )

  blank_and_text <- tibble::tibble(
    ID = c("", " NA "),
    Date = as.Date(c("2024-01-01", "2024-02-01")),
    Target = c(1, 2)
  )
  expect_warning(check_combo_input(blank_and_text, "ID"), "Combo column 'ID'", fixed = TRUE)

  # Either form alone, and numeric NA with NaN, keep their existing behavior.
  missing_only <- dplyr::mutate(mixed, Leader = c(NA, NA, " ", "North"))
  expect_silent(check_combo_input(missing_only, c("Leader", "Product")))
  text_only <- dplyr::mutate(mixed, Leader = c("NA", "NA", "NA", "North"))
  expect_silent(check_combo_input(text_only, c("Leader", "Product")))
  numeric <- tibble::tibble(ID = c(NA, NaN, 1), Date = as.Date("2024-01-01"), Target = c(1, 2, 3))
  expect_silent(check_combo_input(numeric, "ID"))
})

test_that("duplicate errors explain that blank, missing and NA text values share one label", {
  input_data <- tibble::tibble(
    ID = c("", "NA"),
    Date = as.Date(c("2024-01-01", "2024-01-01")),
    Target = c(10, 20)
  )

  expect_error(
    suppressWarnings(check_combo_input(input_data, "ID")),
    "Found 2 duplicate rows.*Blank values, missing values and the text \"NA\" also identify the same series"
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

test_that("prep_data gives blank combo values the NA label in every forecast approach", {
  history_dates <- seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24)
  combos <- tibble::tibble(Region = c("North", "North", "South"), Site = c("A", "  ", ""))
  input_data <- tidyr::expand_grid(combos, Date = history_dates) %>%
    dplyr::mutate(Revenue = as.numeric(dplyr::row_number()) + 100)
  expected <- list(
    bottoms_up = c("North--A", "North--NA", "South--NA"),
    standard_hierarchy = c("North_A", "North_NA", "South_NA"),
    grouped_hierarchy = c("North_A", "North_NA", "South_NA", "Site_NA")
  )

  for (approach in names(expected)) {
    run_info <- set_run_info(
      project_name = paste0("blank_combo_", approach),
      run_name = "blank_run",
      path = withr::local_tempdir(),
      add_unique_id = FALSE
    )
    suppressWarnings(prep_data(
      run_info = run_info,
      input_data = input_data,
      combo_variables = c("Region", "Site"),
      target_variable = "Revenue",
      date_type = "month",
      forecast_horizon = 3,
      forecast_approach = approach,
      box_cox = FALSE,
      stationary = FALSE,
      lag_periods = 3,
      rolling_window_periods = 3,
      recipes_to_run = "R1"
    ))

    prepped_combos <- unique(get_prepped_data(run_info, recipe = "R1")$Combo)
    expect_true(all(expected[[approach]] %in% prepped_combos), info = approach)
  }
})
