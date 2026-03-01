# helper to build a minimal agent_info for hierarchy_detect(write_data = FALSE)
make_agent_info <- function(combo_vars) {
  list(
    project_info = list(combo_variables = combo_vars),
    run_id = 1
  )
}

# --- hierarchy_detect tests ---

test_that("hierarchy_detect returns bottoms_up for single combo variable", {
  df <- tibble::tibble(
    Region = c("US", "US", "UK", "UK"),
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01")),
    Target = c(1, 2, 3, 4)
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info("Region"),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "bottoms_up")
})

test_that("hierarchy_detect identifies standard hierarchy (2 combo vars)", {
  # Area -> Country is a strict nesting: each Country belongs to exactly one Area
  df <- tibble::tibble(
    Area = c("EMEA", "EMEA", "EMEA", "EMEA", "AMER", "AMER"),
    Country = c("UK", "UK", "Germany", "Germany", "US", "US"),
    Date = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01",
      "2020-01-01", "2020-02-01"
    )),
    Target = 1:6
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Area", "Country")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "standard_hierarchy")
})

test_that("hierarchy_detect identifies standard hierarchy (3 combo vars)", {
  # Region -> Country -> City: strict 3-level nesting
  df <- tibble::tibble(
    Region = rep(c("EMEA", "EMEA", "AMER"), each = 2),
    Country = rep(c("UK", "Germany", "US"), each = 2),
    City = rep(c("London", "Berlin", "NYC"), each = 2),
    Date = rep(as.Date(c("2020-01-01", "2020-02-01")), 3),
    Target = 1:6
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Region", "Country", "City")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "standard_hierarchy")
})

test_that("hierarchy_detect identifies grouped hierarchy (2 crossed vars)", {
  # Segment x Product: no nesting (each Segment has all Products)
  df <- tibble::tibble(
    Segment = c("Commercial", "Commercial", "Consumer", "Consumer"),
    Product = c("Office", "Xbox", "Office", "Xbox"),
    Date = as.Date("2020-01-01"),
    Target = c(10, 20, 30, 40)
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Segment", "Product")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "grouped_hierarchy")
})

test_that("hierarchy_detect identifies grouped hierarchy (3 crossed vars)", {
  # All three variables are crossed (many-to-many)
  df <- tidyr::expand_grid(
    Channel = c("Online", "Retail"),
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  ) %>%
    dplyr::mutate(
      Date = as.Date("2020-01-01"),
      Target = seq_len(dplyr::n())
    )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Channel", "Segment", "Product")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "grouped_hierarchy")
})

test_that("hierarchy_detect handles 11 combo vars (grouped) without hanging", {
  # 11 combo vars with a genuinely crossed (grouped) structure.
  # V1 and V2 each vary independently (many-to-many), so no single
  # nesting chain exists. V3-V11 are constant.
  combo_vars <- paste0("V", seq_len(11))

  df <- tibble::tibble(
    V1 = c("A", "A", "B", "B"),
    V2 = c("X", "Y", "X", "Y"),
    V3 = "C", V4 = "C", V5 = "C", V6 = "C",
    V7 = "C", V8 = "C", V9 = "C", V10 = "C", V11 = "C",
    Date = as.Date("2020-01-01"),
    Target = 1:4
  )

  elapsed <- system.time({
    result <- hierarchy_detect(
      agent_info = make_agent_info(combo_vars),
      input_data = df,
      write_data = FALSE
    )
  })[["elapsed"]]

  expect_equal(result, "grouped_hierarchy")
  expect_lt(elapsed, 30)
})

test_that("hierarchy_detect handles 15 combo vars (standard) without hanging", {
  # 15-level strict nesting: V1 is constant root, V15 varies at the leaf
  combo_vars <- paste0("V", seq_len(15))

  # V1..V14 = "X" (constant), V15 varies => standard hierarchy chain
  df <- tibble::tibble(
    V1 = "X", V2 = "X", V3 = "X", V4 = "X", V5 = "X",
    V6 = "X", V7 = "X", V8 = "X", V9 = "X", V10 = "X",
    V11 = "X", V12 = "X", V13 = "X", V14 = "X",
    V15 = c("A", "B"),
    Date = as.Date("2020-01-01"),
    Target = c(1, 2)
  )

  elapsed <- system.time({
    result <- hierarchy_detect(
      agent_info = make_agent_info(combo_vars),
      input_data = df,
      write_data = FALSE
    )
  })[["elapsed"]]

  expect_equal(result, "standard_hierarchy")
  expect_lt(elapsed, 30)
})

# --- prep_hierarchical_data tests ---

test_that("prep_hierarchical_data returns correct grouped hierarchies", {
  # Mock data setup
  data <- tibble::tibble(
    Segment = as.character(c(
      "Commercial", "Commercial", "Commercial", "Commercial", "Commercial", "Commercial",
      "Commercial", "Commercial", "Commercial", "Commercial", "Commercial", "Commercial",
      "Consumer", "Consumer", "Consumer", "Consumer", "Consumer", "Consumer",
      "Consumer", "Consumer", "Consumer", "Consumer", "Consumer", "Consumer"
    )),
    Country = as.character(c(
      "United (States)", "United (States)", "United (States)", "United (States)", "United (States)", "United (States)",
      "UK", "UK", "UK", "UK", "UK", "UK",
      "United (States)", "United (States)", "United (States)", "United (States)", "United (States)", "United (States)",
      "UK", "UK", "UK", "UK", "UK", "UK"
    )),
    Product = as.character(c(
      "Office", "Office", "Office", "Excel", "Excel", "Excel",
      "Office", "Office", "Office", "Excel", "Excel", "Excel",
      "Office", "Office", "Office", "Excel", "Excel", "Excel",
      "Office", "Office", "Office", "Excel", "Excel", "Excel"
    )),
    Date = as.Date(c(
      "1/1/2020", "2/1/2020", "3/1/2020", "1/1/2020", "2/1/2020", "3/1/2020",
      "1/1/2020", "2/1/2020", "3/1/2020", "1/1/2020", "2/1/2020", "3/1/2020",
      "1/1/2020", "2/1/2020", "3/1/2020", "1/1/2020", "2/1/2020", "3/1/2020",
      "1/1/2020", "2/1/2020", "3/1/2020", "1/1/2020", "2/1/2020", "3/1/2020"
    ), format = "%m/%d/%Y"),
    Target = c(1, 2, 3, 13, 14, 15, 25, 26, 27, 37, 38, 39, 1, 2, 3, 13, 14, 15, 25, 26, 27, 37, 38, 39),
    Value_Country = c(1, 2, 3, 1, 2, 3, 10, 20, 30, 10, 20, 30, 1, 2, 3, 1, 2, 3, 10, 20, 30, 10, 20, 30),
    Value_All = c(1, 2, 3, 37, 38, 39, 73, 74, 75, 109, 110, 111, 145, 146, 147, 181, 182, 183, 217, 218, 219, 253, 254, 255),
    Value_Product = c(1, 2, 3, 10, 11, 12, 1, 2, 3, 10, 11, 12, 1, 2, 3, 10, 11, 12, 1, 2, 3, 10, 11, 12),
    Value_Global = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    Value_Segment_Product = c(100, 101, 102, 200, 201, 202, 100, 101, 102, 200, 201, 202, 300, 301, 302, 400, 401, 402, 300, 301, 302, 400, 401, 402)
  ) %>%
    tidyr::unite("Combo",
      c("Segment", "Country", "Product"),
      sep = "--",
      remove = F
    )

  # run prep hts function
  result_data <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Segment", "Country", "Product"),
    external_regressors = c("Value_Country", "Value_All", "Value_Product", "Value_Global", "Value_Segment_Product"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  ) %>%
    dplyr::filter(Date == "2020-01-01")

  # Expected output setup
  expected_data <- tibble::tibble(
    Combo = as.character(c(
      "Total", "Segment_Commercial", "Segment_Consumer", "Country_United_States", "Country_UK",
      "Product_Office", "Product_Excel", "Commercial_United_States_Office", "Commercial_United_States_Excel",
      "Commercial_UK_Office", "Commercial_UK_Excel", "Consumer_United_States_Office", "Consumer_United_States_Excel",
      "Consumer_UK_Office", "Consumer_UK_Excel"
    )),
    Date = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01",
      "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01",
      "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01"
    )),
    Target = c(152, 76, 76, 28, 124, 52, 100, 1, 13, 25, 37, 1, 13, 25, 37),
    Value_Country = c(11, 11, 11, 1, 10, 11, 11, 1, 1, 10, 10, 1, 1, 10, 10),
    Value_All = c(1016, 220, 796, 364, 652, 436, 580, 1, 37, 73, 109, 145, 181, 217, 253),
    Value_Product = c(11, 11, 11, 11, 11, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10),
    Value_Global = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Value_Segment_Product = c(1000, 300, 700, 1000, 1000, 400, 600, 100, 200, 100, 200, 300, 400, 300, 400)
  )

  # Assertions
  expect_equal(result_data, expected_data)
})

test_that("prep_hierarchical_data returns correct standard hierarchies", {
  # Mock data setup
  data <- tibble::tibble(
    Area = as.character(c("EMEA", "EMEA", "EMEA", "EMEA", "EMEA", "EMEA", "EMEA", "EMEA", "United States", "United States", "United States", "United States")),
    Country = as.character(c("Croatia", "Croatia", "Croatia", "Croatia", "Greece", "Greece", "Greece", "Greece", "United States", "United States", "United States", "United States")),
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    Target = c(1, 2, 3, 4, 100, 101, 102, 103, 1000, 1001, 1002, 1003),
    Value_All = c(10, 11, 12, 13, 46, 47, 48, 49, 82, 83, 84, 85),
    Value_Global = c(50, 51, 52, 53, 50, 51, 52, 53, 50, 51, 52, 53),
    Value_Area = c(20, 21, 22, 23, 20, 21, 22, 23, 70, 71, 72, 73)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Country"),
      sep = "--",
      remove = F
    )

  # run prep hts function for standard hierarchy
  result_data <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country"),
    external_regressors = c("Value_All", "Value_Global", "Value_Area"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  ) %>%
    dplyr::filter(Date == "2020-01-01")

  # Expected output setup for a standard hierarchical forecast
  expected_data <- tibble::tibble(
    Combo = as.character(c("Total", "A", "B", "EMEA_Croatia", "EMEA_Greece", "United_States_United_States")),
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
    Target = c(1101, 101, 1000, 1, 100, 1000),
    Value_All = c(138, 56, 82, 10, 46, 82),
    Value_Global = c(50, 50, 50, 50, 50, 50),
    Value_Area = c(90, 90, 90, 20, 20, 70)
  )

  # Assertions
  expect_equal(result_data, expected_data)
})

test_that("prep_hierarchical_data works with more than 10 combo variables", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # 2 bottom-level combos x 3 dates = 6 rows
  data <- tibble::tibble(
    V1 = rep(c("A", "B"), each = n_dates),
    V2 = rep("X", 2 * n_dates),
    V3 = rep("X", 2 * n_dates),
    V4 = rep("X", 2 * n_dates),
    V5 = rep("X", 2 * n_dates),
    V6 = rep("X", 2 * n_dates),
    V7 = rep("X", 2 * n_dates),
    V8 = rep("X", 2 * n_dates),
    V9 = rep("X", 2 * n_dates),
    V10 = rep("X", 2 * n_dates),
    V11 = rep("X", 2 * n_dates),
    Date = rep(dates, 2),
    Target = c(1, 2, 3, 10, 20, 30),
    Regressor1 = c(100, 101, 102, 100, 101, 102)
  )

  combo_variables <- paste0("V", 1:11)

  data <- data %>%
    tidyr::unite("Combo",
      tidyselect::all_of(combo_variables),
      sep = "--",
      remove = FALSE
    )

  result_data <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_variables,
    external_regressors = c("Regressor1"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # verify "Total" aggregation exists
  expect_true("Total" %in% result_data$Combo)

  # verify bottom-level combos are present
  expect_true(any(grepl("^A_", result_data$Combo)))
  expect_true(any(grepl("^B_", result_data$Combo)))

  # verify row count is reasonable (at least bottom + total)
  expect_gt(nrow(result_data), 2 * n_dates)
})

# --- external_regressor_mapping tests ---

test_that("external_regressor_mapping is fast with many combo variables", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("P", "Q"),
                      stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i], V2 = grid$V2[i],
      V3 = grid$V2[i], V4 = grid$V2[i], V5 = grid$V2[i],
      V6 = grid$V2[i], V7 = grid$V2[i], V8 = grid$V2[i],
      V9 = grid$V2[i], V10 = grid$V2[i], V11 = grid$V2[i],
      V12 = grid$V2[i], V13 = grid$V2[i], V14 = grid$V2[i],
      V15 = grid$V2[i],
      Date = dates,
      # Reg_V1 varies by V1 and Date (maps to "V1")
      # Same Reg_V1 value for both V2 levels within same V1+Date
      Reg_V1 = switch(grid$V1[i], A = 10, B = 20, C = 30) + seq_len(n_dates),
      # Reg_Global is the same value per date across all combos (maps to "Global")
      Reg_Global = c(1, 2, 3, 4),
      # Reg_All varies per combo x date (maps to "All")
      Reg_All = (i - 1) * n_dates + seq_len(n_dates)
    )
  }))

  combo_variables <- paste0("V", 1:15)

  elapsed <- system.time({
    result <- external_regressor_mapping(
      data = data,
      combo_variables = combo_variables,
      external_regressors = c("Reg_V1", "Reg_Global", "Reg_All")
    )
  })["elapsed"]

  expect_lt(as.numeric(elapsed), 30)

  # Verify correctness of mappings
  expect_equal(nrow(result), 3)

  v1_var <- result$Var[result$Regressor == "Reg_V1"]
  global_var <- result$Var[result$Regressor == "Reg_Global"]
  all_var <- result$Var[result$Regressor == "Reg_All"]

  expect_equal(v1_var, "V1")
  expect_equal(global_var, "Global")
  expect_equal(all_var, "All")
})
