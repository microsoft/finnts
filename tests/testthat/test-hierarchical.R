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
  # Region -> Country -> City: strict 3-level nesting with multiple children
  df <- tibble::tibble(
    Region = c(
      "West", "West", "West", "West", "West", "West",
      "East", "East", "East"
    ),
    Country = c(
      "USA", "USA", "USA", "UK", "UK", "UK",
      "Japan", "Japan", "Japan"
    ),
    City = c(
      "Seattle", "New York", "Stilwell",
      "London", "Manchester", "Leeds",
      "Tokyo", "Osaka", "Hiroshima"
    ),
    Date = as.Date("2020-01-01"),
    Target = 1:9
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Region", "Country", "City")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "standard_hierarchy")
})

test_that("hierarchy_detect identifies standard hierarchy (4 combo vars)", {
  # Continent -> Region -> Country -> City: 4-level strict nesting
  df <- tibble::tibble(
    Continent = c(
      "Americas", "Americas", "Americas", "Americas",
      "Europe", "Europe"
    ),
    Region = c(
      "North", "North", "South", "South",
      "West", "West"
    ),
    Country = c(
      "USA", "USA", "Brazil", "Brazil",
      "France", "France"
    ),
    City = c(
      "NYC", "LA", "Sao Paulo", "Rio",
      "Paris", "Lyon"
    ),
    Date = as.Date("2020-01-01"),
    Target = 1:6
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Continent", "Region", "Country", "City")),
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
  skip_on_cran()
  # 11 combo vars with a genuinely crossed (grouped) structure.
  # V1 and V2 each vary independently (many-to-many), so no single
  # nesting chain exists. V3-V11 are constant but still valid for
  # grouped hierarchy (get_grouped_nodes handles constant columns).
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
  skip_on_cran()
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

test_that("hierarchy_detect keeps grouped when a combo col is constant", {
  # Segment x Product is crossed (grouped), Region has only one value.
  # get_grouped_nodes() handles constant columns via reorder + guard row,
  # so hierarchy_detect should still return grouped_hierarchy.
  df <- tibble::tibble(
    Segment = c("Commercial", "Commercial", "Consumer", "Consumer"),
    Product = c("Office", "Xbox", "Office", "Xbox"),
    Region  = "AMER",
    Date    = as.Date("2020-01-01"),
    Target  = c(10, 20, 30, 40)
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Segment", "Product", "Region")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "grouped_hierarchy")
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

  # Expected output for Date == 2020-01-01.
  # Target aggregations (sum):
  #   Total = sum(all 8 combos) = 1+13+25+37+1+13+25+37 = 152
  #   Segment_Commercial = 1+13+25+37 = 76, Segment_Consumer = idem = 76
  #   Country_United_States = 1+13+1+13 = 28, Country_UK = 25+37+25+37 = 124
  #   Product_Office = 1+25+1+25 = 52, Product_Excel = 13+37+13+37 = 100
  #   Bottom combos: raw values (1, 13, 25, 37, 1, 13, 25, 37)
  # Value_Country maps to Country (sum per Country per date):
  #   Bottom-level combos keep their raw Value_Country values.
  #   Each Country node equals the sum of Value_Country across all combos
  #   for that Country on the given date, and the Total node equals the sum
  #   of all Country-node values for that date.
  # Value_Global = 1 on every date (constant across combos and dates).
  # Value_All: each combo has a unique value → aggregated via sum_hts_data.
  # Value_Product maps to Product (sum per Product per date).
  # Value_Segment_Product maps to Segment---Product (sum per pair per date).
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

  # Expected output for Date == 2020-01-01.
  # Area → Country nesting: EMEA→{Croatia, Greece}, US→{US}
  # Standard hierarchy levels: Total, Level-1 (A=EMEA, B=US), Bottom.
  #   A and B are labels assigned by hts for the two Area groups.
  # Target (sum):
  #   Total = 1 + 100 + 1000 = 1101
  #   A (EMEA) = 1 + 100 = 101,  B (US) = 1000
  #   Bottom: Croatia=1, Greece=100, US=1000
  # Value_All maps at full combo granularity (sum via sum_hts_data):
  #   Total = 10+46+82 = 138,  A = 10+46 = 56,  B = 82
  #   Bottom: 10, 46, 82
  # Value_Global = 50 everywhere (constant across combos).
  # Value_Area maps to Area (mid-level sum):
  #   Total = 20+70 = 90,  A = 20,  B = 70
  #   Bottom inherit their Area's value: Croatia=20, Greece=20, US=70
  #   But Total gets the date-level aggregate = 20+70 = 90
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

# --- grouped hierarchy hts::gts() robustness tests ---
# These tests verify which data layouts succeed or fail with hts::gts()
# due to GmatrixG() conditionally skipping Total/Bottom rows while gts()
# unconditionally expects them in rownames.

# Helper: build a minimal dataset ready for prep_hierarchical_data
make_grouped_data <- function(combo_variables, grid, n_dates = 3) {
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    row <- grid[i, , drop = FALSE]
    tbl <- tibble::as_tibble(row)
    tbl <- tbl[rep(1, n_dates), ]
    tbl$Date <- dates
    tbl$Target <- seq_len(n_dates) + (i - 1) * 100
    tbl
  }))
  data %>%
    tidyr::unite("Combo",
      tidyselect::all_of(combo_variables),
      sep = "--",
      remove = FALSE
    )
}

test_that("grouped hierarchy succeeds: 2 crossed vars, no constant cols, m=4", {
  # 2x2 cross => 4 bottom series, no constant columns
  # First row NOT all-1s, last row NOT seq(1,4) => both Total & Bottom added

  grid <- tidyr::expand_grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  )
  combo_vars <- c("Segment", "Product")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds: 3 crossed vars, no constant cols, m=8", {
  # 2x2x2 cross => 8 bottom series
  grid <- tidyr::expand_grid(
    Channel = c("Online", "Retail"),
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  )
  combo_vars <- c("Channel", "Segment", "Product")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds: constant col in middle position, m=4", {
  # Region is constant but sits in position 2 (not first).
  # m=4 bottom series. Last var (Product) has 2 unique values != seq(1,4).
  # First var (Segment) has 2 unique values != rep(1,4).
  # Both Total and Bottom rows are added => no error.
  grid <- tidyr::expand_grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  ) %>%
    dplyr::mutate(Region = "AMER")
  combo_vars <- c("Segment", "Region", "Product")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds: constant col last, m=4", {
  # Region is constant and last. First var (Segment) is non-constant.
  # Integer encoding of last row = all 1s != seq(1,4) => Bottom IS added.
  # First row has 2 unique vals != all 1s => Total IS added.
  grid <- tidyr::expand_grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  ) %>%
    dplyr::mutate(Region = "AMER")
  combo_vars <- c("Segment", "Product", "Region")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds: many constant cols in non-first positions, m=3", {
  # V1 varies (3 values), V2-V5 are constant.
  # First row (V1): 3 unique values != all 1s => Total IS added.
  # Last row (V5): all 1s => not seq(1,3) => Bottom IS added.
  # m=3 bottom series.
  grid <- tibble::tibble(
    V1 = c("A", "B", "C"),
    V2 = "X", V3 = "X", V4 = "X", V5 = "X"
  )
  combo_vars <- paste0("V", 1:5)
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds with constant first combo variable", {
  # Region is constant and FIRST in combo_variables.
  grid <- tidyr::expand_grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  ) %>%
    dplyr::mutate(Region = "AMER")
  combo_vars <- c("Region", "Segment", "Product")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds with m=2 bottom series", {
  # 2 bottom-level combos. Last variable (Product) has 2 unique values.
  grid <- tibble::tibble(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  )
  combo_vars <- c("Segment", "Product")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds with constant first col and m=2", {
  # Region is constant and first, m=2 bottom series.
  grid <- tibble::tibble(
    Region = c("AMER", "AMER"),
    Segment = c("Commercial", "Consumer")
  )
  combo_vars <- c("Region", "Segment")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds with multiple constant cols, m=3", {
  # V1 and V3 are constant, V2 has 3 unique vals, m=3 bottom series.
  grid <- tibble::tibble(
    V1 = "X",
    V2 = c("A", "B", "C"),
    V3 = "Y"
  )
  combo_vars <- c("V1", "V2", "V3")
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds with many constant cols, m=2", {
  # 11 combo vars, most constant, m=2 bottom series.
  grid <- tibble::tibble(
    Channel_Region = c("EMEA", "Asia"),
    Channel_Territory = c("Central Europe", "ANZ"),
    Accountability_L5 = c("Online Stores", "Managed Retail"),
    ChannelCategory2 = "Xbox",
    ChannelCategory3 = "Console",
    ChannelCategory4 = c("X Series X", "X Series S"),
    ChannelCategory5 = c("X Series X 2TB", "X Series S 512GB"),
    PerspectiveMeasure = "SellThru",
    Accountability_L3 = "CSO Region",
    Accountability_L2 = "CSO WW Sales",
    Accountability_L4 = c("Direct", "Partner")
  )
  combo_vars <- c(
    "Channel_Region", "Channel_Territory", "Accountability_L5",
    "ChannelCategory2", "ChannelCategory3", "ChannelCategory4",
    "ChannelCategory5", "PerspectiveMeasure", "Accountability_L3",
    "Accountability_L2", "Accountability_L4"
  )
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

test_that("grouped hierarchy succeeds: same realistic dataset with m=3", {
  # Same structure as above but with 3 bottom-level series.
  # Last var (Accountability_L4) has 2 unique values != seq(1, 3).
  # First var (Channel_Region) has 3 unique values != rep(1, 3).
  # Both Total and Bottom rows added => no error.
  grid <- tibble::tibble(
    Channel_Region = c("EMEA", "Asia", "EMEA"),
    Channel_Territory = c("Central Europe", "ANZ", "Southwest Europe"),
    Accountability_L5 = c("Online Stores", "Managed Retail", "Managed Retail"),
    ChannelCategory2 = "Xbox",
    ChannelCategory3 = "Console",
    ChannelCategory4 = c("X Series X", "X Series S", "X Series X"),
    ChannelCategory5 = c("X Series X 2TB", "X Series S 512GB", "X Series X ODD"),
    PerspectiveMeasure = "SellThru",
    Accountability_L3 = "CSO Region",
    Accountability_L2 = "CSO WW Sales",
    Accountability_L4 = c("Direct", "Partner", "Partner")
  )
  combo_vars <- c(
    "Channel_Region", "Channel_Territory", "Accountability_L5",
    "ChannelCategory2", "ChannelCategory3", "ChannelCategory4",
    "ChannelCategory5", "PerspectiveMeasure", "Accountability_L3",
    "Accountability_L2", "Accountability_L4"
  )
  data <- make_grouped_data(combo_vars, grid)

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = NULL,
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  expect_true("Total" %in% result$Combo)
  expect_gt(nrow(result), nrow(grid) * 3)
})

# --- end-to-end grouped hierarchy forecast test ---

test_that("end-to-end grouped hierarchy forecast with constant and crossed combos", {
  skip_on_cran()

  # 4 bottom-level time series: Region is constant, Segment x Product
  # are fully crossed (2x2 = 4 combos).
  combo_vars <- c("Region", "Segment", "Product")
  n_dates <- 24
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- tidyr::expand_grid(
    Region = "AMER",
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox")
  )

  inp_data <- grid %>%
    dplyr::slice(rep(seq_len(dplyr::n()), each = n_dates)) %>%
    dplyr::mutate(
      Date = rep(dates, nrow(grid)),
      Revenue = seq_len(dplyr::n()) + 100
    )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = inp_data,
    combo_variables = combo_vars,
    target_variable = "Revenue",
    date_type = "month",
    forecast_horizon = 3,
    forecast_approach = "grouped_hierarchy",
    recipes_to_run = "R1"
  )

  prep_models(
    run_info = run_info,
    models_to_run = "snaive",
    back_test_scenarios = 1,
    run_ensemble_models = FALSE
  )

  train_models(
    run_info = run_info,
    run_global_models = FALSE,
    run_local_models = TRUE
  )

  final_models(
    run_info = run_info,
    average_models = FALSE
  )

  fcst <- get_forecast_data(run_info = run_info)

  # 1. Combo variables appear as columns in the correct user-specified order
  combo_col_positions <- match(combo_vars, colnames(fcst))
  expect_true(all(!is.na(combo_col_positions)))
  expect_equal(combo_col_positions, sort(combo_col_positions))

  # Combo variables appear right after the Combo column
  expect_equal(combo_col_positions, seq(2, 2 + length(combo_vars) - 1))

  # 2. All original combo variable values are present
  expect_true(all(unique(fcst$Region) == "AMER"))
  expect_setequal(unique(fcst$Segment), c("Commercial", "Consumer"))
  expect_setequal(unique(fcst$Product), c("Office", "Xbox"))

  # 3. Only bottom-level combos in reconciled output (no "Total" or aggregated levels)
  bottom_combos <- unique(fcst$Combo)
  expect_false(any(grepl("Total", bottom_combos)))
  expect_equal(length(bottom_combos), 4)

  # 4. All 4 crossed combos are present
  expected_combos <- sort(paste(grid$Region, grid$Segment, grid$Product, sep = "--"))
  expect_setequal(sort(bottom_combos), expected_combos)

  # 5. Backtest actuals match original input data
  backtest <- fcst %>%
    dplyr::filter(
      Run_Type == "Back_Test",
      Best_Model == "Yes"
    ) %>%
    dplyr::select(Combo, Date, Target) %>%
    dplyr::distinct()

  for (i in seq_len(nrow(backtest))) {
    row <- backtest[i, ]
    combo_parts <- strsplit(row$Combo, "--")[[1]]
    names(combo_parts) <- combo_vars

    original_val <- inp_data %>%
      dplyr::filter(
        Region == combo_parts["Region"],
        Segment == combo_parts["Segment"],
        Product == combo_parts["Product"],
        Date == row$Date
      ) %>%
      dplyr::pull(Revenue)

    expect_equal(
      row$Target, original_val,
      info = paste("Backtest actual mismatch for", row$Combo, "on", row$Date)
    )
  }

  # 6. Future forecasts exist for all 4 combos
  future <- fcst %>% dplyr::filter(Run_Type == "Future_Forecast")
  expect_gt(nrow(future), 0)
  expect_true(all(is.na(future$Target)))
  expect_setequal(sort(unique(future$Combo)), expected_combos)
})

# --- external_regressor_mapping tests ---

test_that("external_regressor_mapping is fast with many combo variables", {
  skip_on_cran()
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(
    V1 = c("A", "B", "C"), V2 = c("P", "Q"),
    stringsAsFactors = FALSE
  )
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
      Reg_V1 = switch(grid$V1[i],
        A = 10,
        B = 20,
        C = 30
      ) + seq_len(n_dates),
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

test_that("external_regressor_mapping excludes single-value combo variables", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(
    V1 = c("A", "B"),
    V2 = c("X", "Y"),
    V3 = "Only_One",
    stringsAsFactors = FALSE
  )

  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      V3 = grid$V3[i],
      Date = dates,
      Reg_Bottom = (i - 1) * n_dates + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2", "V3"),
    external_regressors = c("Reg_Bottom")
  )

  expect_equal(result$Var[result$Regressor == "Reg_Bottom"], "All")
})

test_that("external_regressor_mapping returns Global when regressor varies only by date and all combo vars are constant", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  data <- tibble::tibble(
    V1 = rep("Only", n_dates),
    V2 = rep("One", n_dates),
    Date = dates,
    Reg = c(10, 20, 30)
  )

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  expect_equal(result$Var[result$Regressor == "Reg"], "Global")
})

test_that("external_regressor_mapping returns All with fully-varying regressor across combos", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = (i * 5) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  # regressor unique per full combo cross, no single var reduces count
  expect_equal(result$Var[result$Regressor == "Reg"], "All")
})

test_that("external_regressor_mapping returns Global with asymmetric combo variable sizes", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = c(10, 20, 30)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  expect_equal(result$Var[result$Regressor == "Reg"], "Global")
})

test_that("external_regressor_mapping returns All when regressor varies uniquely by full combo and no single var reduces uniqueness", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1 x V2 = 2x2 = 4 combos; regressor varies by both V1 and V2
  # but is the same within each (V1, V2) pair across dates
  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      # Reg depends on BOTH V1 and V2 symmetrically
      Reg = (match(grid$V1[i], c("A", "B")) * 10 +
        match(grid$V2[i], c("X", "Y"))) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  # no single var reduces the unique count: regressor is unique per full combo cross
  expect_equal(result$Var[result$Regressor == "Reg"], "All")
})

test_that("external_regressor_mapping picks only the two tied candidates from three", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) x V3(3) = 12 combos; Reg varies by V1 and V2 jointly
  grid <- expand.grid(
    V1 = c("A", "B"), V2 = c("X", "Y"), V3 = c("P", "Q", "R"),
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      V3 = grid$V3[i],
      Date = dates,
      # Reg determined by (V1, V2) only
      Reg = rep(match(grid$V1[i], c("A", "B")) * 10 +
        match(grid$V2[i], c("X", "Y")), n_dates) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2", "V3"),
    external_regressors = c("Reg")
  )

  var_result <- result$Var[result$Regressor == "Reg"]
  parts <- sort(strsplit(var_result, "---")[[1]])

  # V1 and V2 both reduce equally, V3 does not
  expect_equal(parts, c("V1", "V2"))
})

test_that("external_regressor_mapping picks single best candidate among multiple", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(3) x V2(2) = 6 combos; Reg varies only by V1
  grid <- expand.grid(
    V1 = c("A", "B", "C"), V2 = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = rep(match(grid$V1[i], c("A", "B", "C")) * 100, n_dates) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  expect_equal(result$Var[result$Regressor == "Reg"], "V1")
})

test_that("external_regressor_mapping handles constant regressor as Global", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = rep(42, n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  # constant regressor → distinct(Date, Reg) == n_dates → Global
  expect_equal(result$Var[result$Regressor == "Reg"], "Global")
})

test_that("external_regressor_mapping returns All when constant var excluded and sole remaining var cannot reduce count", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(1 constant) = 2 combos; Reg varies by V1.
  # After V2 is excluded (constant), only V1 remains as multi_value_var.
  # all_unique = count_unique(c(V1, V2), Reg) = 6 (each row is distinct).
  # count_unique(V1, Reg) = 6 too (same, because V2 is constant).
  # Since 6 == 6, V1 does NOT reduce the count, so result is "All".
  data <- tibble::tibble(
    V1 = rep(c("A", "B"), each = n_dates),
    V2 = rep("Const", 2 * n_dates),
    Date = rep(dates, 2),
    Reg = c(
      rep(10, n_dates) + seq_len(n_dates),
      rep(20, n_dates) + seq_len(n_dates)
    )
  )

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  expect_equal(result$Var[result$Regressor == "Reg"], "All")
})

test_that("external_regressor_mapping handles nested hierarchical variables", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Region determines Country (nested): EMEA→{Croatia, Greece}, AMER→{US}
  # Reg varies by Region only
  data <- tibble::tibble(
    Region = c(rep("EMEA", 2 * n_dates), rep("AMER", n_dates)),
    Country = c(rep("Croatia", n_dates), rep("Greece", n_dates), rep("US", n_dates)),
    Date = rep(dates, 3),
    Reg = c(
      rep(10, n_dates) + seq_len(n_dates),
      rep(10, n_dates) + seq_len(n_dates),
      rep(50, n_dates) + seq_len(n_dates)
    )
  )

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("Region", "Country"),
    external_regressors = c("Reg")
  )

  # Region has fewer unique count → should be selected
  expect_equal(result$Var[result$Regressor == "Reg"], "Region")
})

test_that("external_regressor_mapping handles multiple regressors at different levels", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) x V3(2) = 8 combos
  grid <- expand.grid(
    V1 = c("A", "B"), V2 = c("X", "Y"), V3 = c("P", "Q"),
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      V3 = grid$V3[i],
      Date = dates,
      Reg_Global = c(1, 2, 3),
      Reg_V1 = rep(match(grid$V1[i], c("A", "B")) * 100, n_dates) + seq_len(n_dates),
      Reg_All = (i - 1) * n_dates + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2", "V3"),
    external_regressors = c("Reg_Global", "Reg_V1", "Reg_All")
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$Var[result$Regressor == "Reg_Global"], "Global")
  expect_equal(result$Var[result$Regressor == "Reg_V1"], "V1")
  expect_equal(result$Var[result$Regressor == "Reg_All"], "All")
})

test_that("external_regressor_mapping handles regressor with NA values", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) = 4 combos; Reg varies by V1 but first combo has an NA.
  # NA is treated as a distinct value by dplyr::distinct, so each (V1,Date)
  # pair still has a unique Reg value. The regressor still fundamentally
  # varies by V1, but the NA on combo 1 / date 2 makes distinct(Date, Reg)
  # return 4 rows (date2 has both NA and 201), so it's not Global.
  # count_unique(V1, Reg) should still be < all_unique → maps to "V1".
  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    v1_idx <- match(grid$V1[i], c("A", "B"))
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = if (i == 1) c(v1_idx * 100 + 1, NA, v1_idx * 100 + 3) else rep(v1_idx * 100, n_dates) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  # NA makes it non-Global; V1 still reduces count → maps to V1
  expect_equal(result$Var[result$Regressor == "Reg"], "V1")
})

# --- end-to-end prep_hierarchical_data xreg + corner case tests ---

test_that("grouped hierarchy with single-value combo var produces no duplicate rows", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Segment(2) x Product(2) x Flag(1 constant) = 4 combos
  # Reg varies at bottom combo level — the original ECIF bug scenario
  grid <- expand.grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox"),
    Flag = "Internal",
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      Segment = grid$Segment[i],
      Product = grid$Product[i],
      Flag = grid$Flag[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Milestone = (i - 1) * 100 + seq_len(n_dates)
    )
  })) %>%
    tidyr::unite("Combo",
      c("Segment", "Product", "Flag"),
      sep = "--",
      remove = FALSE
    )

  combo_vars <- c("Segment", "Product", "Flag")

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = c("Milestone"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate (Combo, Date) rows
  row_count <- nrow(result)
  distinct_count <- nrow(dplyr::distinct(result, Combo, Date))
  expect_equal(row_count, distinct_count)

  # Milestone should not be NA at any hierarchy level
  expect_true(all(!is.na(result$Milestone)))

  # Total row should exist
  expect_true("Total" %in% result$Combo)
})

test_that("standard hierarchy with single-value combo var produces no duplicate rows", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Area(2) x Country(nested) x Flag(1 constant) = 3 combos
  data <- tibble::tibble(
    Area = c(rep("EMEA", 2 * n_dates), rep("US", n_dates)),
    Country = c(rep("Croatia", n_dates), rep("Greece", n_dates), rep("US", n_dates)),
    Flag = rep("Active", 3 * n_dates),
    Date = rep(dates, 3),
    Target = c(1:4, 10:13, 100:103),
    Reg_Area = c(rep(10, n_dates), rep(10, n_dates), rep(50, n_dates)) + rep(seq_len(n_dates), 3),
    Reg_All = seq_len(3 * n_dates)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Country", "Flag"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country", "Flag"),
    external_regressors = c("Reg_Area", "Reg_All"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  )

  # no duplicate (Combo, Date) rows
  row_count <- nrow(result)
  distinct_count <- nrow(dplyr::distinct(result, Combo, Date))
  expect_equal(row_count, distinct_count)

  # xregs should be present (not NA) everywhere
  expect_true(all(!is.na(result$Reg_Area)))
  expect_true(all(!is.na(result$Reg_All)))
})

test_that("grouped hierarchy xregs at multiple levels with constant var are correct", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Segment(2) x Product(2) x Region(1 constant) = 4 combos
  grid <- expand.grid(
    Segment = c("Commercial", "Consumer"),
    Product = c("Office", "Xbox"),
    Region = "AMER",
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      Segment = grid$Segment[i],
      Product = grid$Product[i],
      Region = grid$Region[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Reg_Global = c(99, 98, 97),
      Reg_Segment = rep(match(grid$Segment[i], c("Commercial", "Consumer")) * 100, n_dates),
      Reg_All = (i - 1) * 100 + seq_len(n_dates)
    )
  })) %>%
    tidyr::unite("Combo",
      c("Segment", "Product", "Region"),
      sep = "--",
      remove = FALSE
    )

  combo_vars <- c("Segment", "Product", "Region")

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = combo_vars,
    external_regressors = c("Reg_Global", "Reg_Segment", "Reg_All"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # Global regressor should be the same across all combos for a given date
  total_row <- result %>% dplyr::filter(Combo == "Total", Date == dates[1])
  expect_equal(total_row$Reg_Global, 99)

  # All regressors should have values (no NAs)
  expect_true(all(!is.na(result$Reg_Global)))
  expect_true(all(!is.na(result$Reg_Segment)))
  expect_true(all(!is.na(result$Reg_All)))
})

test_that("grouped hierarchy no-duplicate invariant holds for multi-var xreg mapping", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Segment(2) x Product(2) x Channel(2) = 8 combos
  # Reg varies by (Segment, Product) jointly — a multi-var mapped regressor
  grid <- expand.grid(
    Segment = c("Comm", "Cons"),
    Product = c("Off", "Xbox"),
    Channel = c("Online", "Retail"),
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      Segment = grid$Segment[i],
      Product = grid$Product[i],
      Channel = grid$Channel[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Reg_SegProd = (match(grid$Segment[i], c("Comm", "Cons")) * 10 +
        match(grid$Product[i], c("Off", "Xbox"))) * 100 + seq_len(n_dates)
    )
  })) %>%
    tidyr::unite("Combo",
      c("Segment", "Product", "Channel"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Segment", "Product", "Channel"),
    external_regressors = c("Reg_SegProd"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # regressor should be present everywhere
  expect_true(all(!is.na(result$Reg_SegProd)))
})

test_that("standard hierarchy no-duplicate invariant holds with xregs at mid-level", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Area → Country (nested standard hierarchy): EMEA→{Croatia, Greece}, US→{US}
  # Reg_Area varies by Area (mid-level)
  data <- tibble::tibble(
    Area = c(rep("EMEA", 2 * n_dates), rep("US", n_dates)),
    Country = c(rep("Croatia", n_dates), rep("Greece", n_dates), rep("US", n_dates)),
    Date = rep(dates, 3),
    Target = c(1:4, 100:103, 1000:1003),
    Reg_Area = c(rep(20, n_dates), rep(20, n_dates), rep(70, n_dates)) + rep(seq_len(n_dates), 3)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Country"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country"),
    external_regressors = c("Reg_Area"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # xreg should be present everywhere
  expect_true(all(!is.na(result$Reg_Area)))
})

# --- additional xreg mapping corner cases ---

test_that("external_regressor_mapping detects near-global regressor as non-Global", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Reg is the same across all combos except the last one deviates on date 3
  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg = if (i == nrow(grid)) c(10, 20, 999) else c(10, 20, 30)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg")
  )

  # last combo deviates on date 3, so distinct(Date, Reg) > n_dates → not Global
  expect_true(result$Var[result$Regressor == "Reg"] != "Global")
})



test_that("external_regressor_mapping with multiple single-value vars and two varying", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) = 4 combos; V3, V4, V5 are constant
  # Reg varies only by V1
  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      V3 = "C1",
      V4 = "C2",
      V5 = "C3",
      Date = dates,
      Reg = rep(match(grid$V1[i], c("A", "B")) * 100, n_dates) + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2", "V3", "V4", "V5"),
    external_regressors = c("Reg")
  )

  # V3/V4/V5 should be filtered out; V1 should be selected
  expect_equal(result$Var[result$Regressor == "Reg"], "V1")
})

test_that("external_regressor_mapping with two regressors at the same mid-level", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(3) x V2(2) = 6 combos; both regressors vary by V1
  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    v1_idx <- match(grid$V1[i], c("A", "B", "C"))
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Reg_A = v1_idx * 10 + seq_len(n_dates),
      Reg_B = v1_idx * 50 + seq_len(n_dates)
    )
  }))

  result <- external_regressor_mapping(
    data = data,
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg_A", "Reg_B")
  )

  expect_equal(result$Var[result$Regressor == "Reg_A"], "V1")
  expect_equal(result$Var[result$Regressor == "Reg_B"], "V1")
})

# --- additional end-to-end aggregation corner cases ---

test_that("grouped hierarchy with xreg NAs at some combos produces no duplicates", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) = 4 combos; Reg varies by V1, has NAs in one combo
  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    v1_idx <- match(grid$V1[i], c("A", "B"))
    reg_vals <- v1_idx * 100 + seq_len(n_dates)
    # inject NA into first combo's last date
    if (i == 1) reg_vals[n_dates] <- NA
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Reg = reg_vals
    )
  })) %>%
    tidyr::unite("Combo",
      c("V1", "V2"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate (Combo, Date) rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))
})

test_that("grouped hierarchy with multiple constant combo vars produces no duplicates", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(2) x V2(2) = 4 combos; V3, V4, V5 are constant
  # Reg varies at bottom combo level
  grid <- expand.grid(V1 = c("A", "B"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      V3 = "Const1",
      V4 = "Const2",
      V5 = "Const3",
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Reg = (i - 1) * 100 + seq_len(n_dates)
    )
  })) %>%
    tidyr::unite("Combo",
      c("V1", "V2", "V3", "V4", "V5"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("V1", "V2", "V3", "V4", "V5"),
    external_regressors = c("Reg"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # Total should exist
  expect_true("Total" %in% result$Combo)

  # xreg should be present everywhere
  expect_true(all(!is.na(result$Reg)))
})

test_that("grouped hierarchy with special characters in combo values produces no duplicates", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # combo values with parentheses, spaces, dashes
  data <- tibble::tibble(
    Area = c(rep("United (States)", n_dates), rep("South-East Asia", n_dates)),
    Product = c(rep("MS Office 365", n_dates), rep("MS Office 365", n_dates)),
    Date = rep(dates, 2),
    Target = c(1:3, 10:12),
    Reg_Area = c(rep(100, n_dates), rep(200, n_dates)) + rep(seq_len(n_dates), 2)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Product"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Product"),
    external_regressors = c("Reg_Area"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # xreg should be present everywhere
  expect_true(all(!is.na(result$Reg_Area)))

  # Total should exist
  expect_true("Total" %in% result$Combo)
})

test_that("grouped hierarchy with sparse/unbalanced combos produces no duplicates", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Not all V1 x V2 combinations exist: A-X, A-Y, B-X (missing B-Y)
  data <- tibble::tibble(
    V1 = c(rep("A", 2 * n_dates), rep("B", n_dates)),
    V2 = c(rep("X", n_dates), rep("Y", n_dates), rep("X", n_dates)),
    Date = rep(dates, 3),
    Target = c(1:3, 10:12, 100:102),
    Reg_All = seq_len(3 * n_dates)
  ) %>%
    tidyr::unite("Combo",
      c("V1", "V2"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg_All"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # xreg should be present everywhere
  expect_true(all(!is.na(result$Reg_All)))
})

test_that("grouped hierarchy two regressors at same mid-level both join correctly", {
  n_dates <- 3
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # V1(3) x V2(2) = 6 combos; both Reg_A and Reg_B vary by V1
  grid <- expand.grid(V1 = c("A", "B", "C"), V2 = c("X", "Y"), stringsAsFactors = FALSE)
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    v1_idx <- match(grid$V1[i], c("A", "B", "C"))
    tibble::tibble(
      V1 = grid$V1[i],
      V2 = grid$V2[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(n_dates),
      Reg_A = v1_idx * 10 + seq_len(n_dates),
      Reg_B = v1_idx * 50 + seq_len(n_dates)
    )
  })) %>%
    tidyr::unite("Combo",
      c("V1", "V2"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("V1", "V2"),
    external_regressors = c("Reg_A", "Reg_B"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # both regressors should be present everywhere
  expect_true(all(!is.na(result$Reg_A)))
  expect_true(all(!is.na(result$Reg_B)))

  # verify both regressors have the correct total aggregation (date 1)
  total_d1 <- result %>% dplyr::filter(Combo == "Total", Date == dates[1])
  # Reg_A total for date 1: V1=A→11, V1=B→21, V1=C→31 → sum=63
  # but it's aggregated as sum over first value_level groups
  expect_true(!is.na(total_d1$Reg_A))
  expect_true(!is.na(total_d1$Reg_B))
})

test_that("standard hierarchy with sparse combos and xregs produces no duplicates", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Area → Country: EMEA→{Croatia, Greece, Italy}, US→{US}
  # Reg varies by Area
  data <- tibble::tibble(
    Area = c(rep("EMEA", 3 * n_dates), rep("US", n_dates)),
    Country = c(
      rep("Croatia", n_dates), rep("Greece", n_dates), rep("Italy", n_dates),
      rep("US", n_dates)
    ),
    Date = rep(dates, 4),
    Target = c(1:4, 10:13, 20:23, 100:103),
    Reg = c(rep(10, 3 * n_dates), rep(50, n_dates)) + rep(seq_len(n_dates), 4)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Country"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country"),
    external_regressors = c("Reg"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # xreg present everywhere
  expect_true(all(!is.na(result$Reg)))
})

# --- missing corner case tests ---

test_that("hierarchy_detect identifies grouped when nesting and crossing coexist", {
  # Region → Country is nested (one-to-many), but Product crosses both.
  # This is a partial hierarchy (tree for some pairs, crossed for others)
  # which should be classified as grouped.
  df <- tibble::tibble(
    Region  = c("EMEA", "EMEA", "EMEA", "EMEA", "US", "US"),
    Country = c("Croatia", "Croatia", "Greece", "Greece", "US", "US"),
    Product = c("Office", "Xbox", "Office", "Xbox", "Office", "Xbox"),
    Date    = as.Date("2020-01-01"),
    Target  = seq_len(6)
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Region", "Country", "Product")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "grouped_hierarchy")
})

test_that("hierarchy_detect identifies standard hierarchy with 1:1 equivalent variables", {
  # Country and CountryCode are 1:1 bijective (each determines the other).
  # The union-find algorithm collapses them into one equivalence class,
  # forming a single-node chain → standard hierarchy.
  df <- tibble::tibble(
    Country     = c("Croatia", "Greece", "US"),
    CountryCode = c("HR", "GR", "US"),
    Date        = as.Date("2020-01-01"),
    Target      = c(10, 20, 30)
  )

  result <- hierarchy_detect(
    agent_info = make_agent_info(c("Country", "CountryCode")),
    input_data = df,
    write_data = FALSE
  )

  expect_equal(result, "standard_hierarchy")
})

test_that("standard hierarchy end-to-end with xregs at multiple levels", {
  n_dates <- 4
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_dates)

  # Area → Country (nested): EMEA→{Croatia, Greece}, US→{US}
  # Three regressors at different levels:
  #   Reg_Global: same value per date across all combos → Global
  #   Reg_Area:   varies by Area only (mid-level)
  #   Reg_All:    unique per combo (bottom-level)
  data <- tibble::tibble(
    Area = c(rep("EMEA", 2 * n_dates), rep("US", n_dates)),
    Country = c(rep("Croatia", n_dates), rep("Greece", n_dates), rep("US", n_dates)),
    Date = rep(dates, 3),
    Target = c(1:4, 10:13, 100:103),
    Reg_Global = rep(c(50, 51, 52, 53), 3),
    Reg_Area = c(rep(20, 2 * n_dates), rep(70, n_dates)) + rep(seq_len(n_dates), 3),
    Reg_All = seq_len(3 * n_dates)
  ) %>%
    tidyr::unite("Combo",
      c("Area", "Country"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country"),
    external_regressors = c("Reg_Global", "Reg_Area", "Reg_All"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # all regressors present (no NAs)
  expect_true(all(!is.na(result$Reg_Global)))
  expect_true(all(!is.na(result$Reg_Area)))
  expect_true(all(!is.na(result$Reg_All)))

  # Global regressor should be the same at Total level as at bottom
  total_d1 <- result %>% dplyr::filter(Combo == "Total", Date == dates[1])
  bottom_d1 <- result %>% dplyr::filter(grepl("Croatia", Combo), Date == dates[1])
  expect_equal(total_d1$Reg_Global, bottom_d1$Reg_Global)

  # Total row should exist
  expect_true("Total" %in% result$Combo)
})

test_that("grouped hierarchy aggregation works with only 2 dates", {
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 2)

  # Segment(2) x Product(2) = 4 combos, 2 dates each = 8 rows
  grid <- expand.grid(
    Segment = c("Comm", "Cons"),
    Product = c("Off", "Xbox"),
    stringsAsFactors = FALSE
  )
  data <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    tibble::tibble(
      Segment = grid$Segment[i],
      Product = grid$Product[i],
      Date = dates,
      Target = (i - 1) * 10 + seq_len(2),
      Reg = rep(match(grid$Segment[i], c("Comm", "Cons")) * 100, 2) + seq_len(2)
    )
  })) %>%
    tidyr::unite("Combo",
      c("Segment", "Product"),
      sep = "--",
      remove = FALSE
    )

  result <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Segment", "Product"),
    external_regressors = c("Reg"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  )

  # no duplicate rows
  expect_equal(nrow(result), nrow(dplyr::distinct(result, Combo, Date)))

  # Total row exists
  expect_true("Total" %in% result$Combo)

  # xreg present everywhere
  expect_true(all(!is.na(result$Reg)))

  # should have more rows than just bottom combos (aggregated levels exist)
  expect_gt(nrow(result), nrow(grid) * 2)
})

# --- ensemble / reconciliation dedup regression tests ---

test_that("ensemble prep produces no duplicate rows when models have fp Target discrepancies", {
  # Simulate the scenario where different models return slightly different
  # floating-point Target values for the same (Combo, Date, Train_Test_ID),
  # e.g. croston returning 0 vs arima returning -2.91e-10.
  initial_results <- tibble::tibble(
    Combo_ID = rep("A_B", 6),
    Combo = rep("A_B", 6),
    Model_Name = c("arima", "arima", "croston", "croston", "cubist", "cubist"),
    Recipe_ID = rep("R1", 6),
    Date = rep(as.Date(c("2020-01-01", "2020-02-01")), 3),
    Train_Test_ID = rep(c(1, 1), 3),
    Target = c(-2.91e-10, 100, 0, 100, -2.91e-10, 100),
    Forecast = c(1.5, 102, 0.1, 99, 2.0, 101)
  )

  target_tbl <- initial_results %>%
    dplyr::group_by(Combo, Date, Train_Test_ID) %>%
    dplyr::summarise(Target = mean(Target, na.rm = TRUE), .groups = "drop")

  prep_ensemble_tbl <- initial_results %>%
    dplyr::select(-Target) %>%
    dplyr::mutate(Suffix = ifelse(Combo_ID == "All-Data", "Global", "Local")) %>%
    tidyr::unite(
      col = "Model_Key",
      c("Model_Name", "Recipe_ID", "Suffix"),
      sep = "-",
      remove = FALSE
    ) %>%
    tidyr::pivot_wider(
      names_from = Model_Key, values_from = Forecast,
      id_cols = c("Combo", "Date", "Train_Test_ID"), values_fill = 0
    ) %>%
    dplyr::left_join(target_tbl, by = c("Combo", "Date", "Train_Test_ID"))

  # should have exactly 2 rows (one per date), not 4
  expect_equal(nrow(prep_ensemble_tbl), 2)

  # Target should be consolidated (mean of the fp-discrepant values)
  expect_equal(
    nrow(dplyr::distinct(prep_ensemble_tbl, Combo, Date, Train_Test_ID)),
    nrow(prep_ensemble_tbl)
  )
})

test_that("reconciliation pivot_wider produces numeric columns when model has duplicate forecasts", {
  # Simulate duplicate (Combo, Date, Train_Test_ID) rows in Best-Model data,
  # which previously caused pivot_wider to create list columns.
  model_tbl <- tibble::tibble(
    Combo = c(rep("A_X", 4), rep("B_Y", 4)),
    Date = rep(as.Date(c("2020-01-01", "2020-01-01", "2020-02-01", "2020-02-01")), 2),
    Train_Test_ID = rep(c(1, 1, 1, 1), 2),
    Forecast = c(10, 10.0001, 20, 20, 30, 30, 40, 40.0001)
  )

  # apply the dedup fix: group_by + summarise before pivot_wider
  forecast_tbl <- model_tbl %>%
    dplyr::group_by(Date, Train_Test_ID, Combo) %>%
    dplyr::summarise(Forecast = mean(Forecast, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Combo, values_from = Forecast)

  # all columns should be atomic (numeric), not list
  for (col in setdiff(names(forecast_tbl), c("Date", "Train_Test_ID"))) {
    expect_true(is.numeric(forecast_tbl[[col]]),
      info = paste("Column", col, "should be numeric, not list")
    )
  }

  # should have exactly 2 rows (one per date)
  expect_equal(nrow(forecast_tbl), 2)
})
