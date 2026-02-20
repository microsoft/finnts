# tests/testthat/test-hierarchical.R
# Tests for hierarchy.R functions

# -- prep_hierarchical_data tests --

test_that("prep_hierarchical_data returns correct grouped hierarchies", {
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

  result_data <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Segment", "Country", "Product"),
    external_regressors = c("Value_Country", "Value_All", "Value_Product", "Value_Global", "Value_Segment_Product"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  ) %>%
    dplyr::filter(Date == "2020-01-01")

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

  expect_equal(result_data, expected_data)
})

test_that("prep_hierarchical_data returns correct standard hierarchies", {
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

  result_data <- prep_hierarchical_data(
    input_data = data,
    run_info = set_run_info(),
    combo_variables = c("Area", "Country"),
    external_regressors = c("Value_All", "Value_Global", "Value_Area"),
    forecast_approach = "standard_hierarchy",
    frequency_number = 12
  ) %>%
    dplyr::filter(Date == "2020-01-01")

  expected_data <- tibble::tibble(
    Combo = as.character(c("Total", "A", "B", "EMEA_Croatia", "EMEA_Greece", "United_States_United_States")),
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
    Target = c(1101, 101, 1000, 1, 100, 1000),
    Value_All = c(138, 56, 82, 10, 46, 82),
    Value_Global = c(50, 50, 50, 50, 50, 50),
    Value_Area = c(90, 90, 90, 20, 20, 70)
  )

  expect_equal(result_data, expected_data)
})

# -- summarize_standard_hierarchy tests --

test_that("summarize_standard_hierarchy creates correct summary", {
  skip_if_not_installed("hts")

  original_combos <- c("A", "B", "C", "D")
  nodes <- list(2, c(2, 2))

  dummy_data <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3, ncol = 4)
  colnames(dummy_data) <- original_combos
  ts_data <- stats::ts(dummy_data, frequency = 12)
  hts_obj <- hts::hts(ts_data, nodes = nodes) %>% suppressMessages()
  S <- hts::smatrix(hts_obj)
  hts_combos <- paste0("Level_", seq_len(nrow(S)))

  result <- summarize_standard_hierarchy(original_combos, hts_combos, nodes)

  expect_s3_class(result, "data.frame")
  expect_true("Hierarchy_Level" %in% names(result))
  expect_true("Level_Type" %in% names(result))
  expect_true("Original_Combos" %in% names(result))
  expect_true("Num_Bottom_Series" %in% names(result))
  expect_true("Total" %in% result$Level_Type)
  expect_true("Bottom" %in% result$Level_Type)
  total_row <- result[result$Level_Type == "Total", ]
  expect_equal(total_row$Num_Bottom_Series, 4)
  bottom_rows <- result[result$Level_Type == "Bottom", ]
  expect_true(all(bottom_rows$Num_Bottom_Series == 1))
})

test_that("summarize_standard_hierarchy orders by level type", {
  skip_if_not_installed("hts")

  original_combos <- c("A", "B", "C", "D")
  nodes <- list(2, c(2, 2))

  dummy_data <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3, ncol = 4)
  colnames(dummy_data) <- original_combos
  ts_data <- stats::ts(dummy_data, frequency = 12)
  hts_obj <- hts::hts(ts_data, nodes = nodes) %>% suppressMessages()
  S <- hts::smatrix(hts_obj)
  hts_combos <- paste0("Node_", seq_len(nrow(S)))

  result <- summarize_standard_hierarchy(original_combos, hts_combos, nodes)

  expect_equal(result$Level_Type[1], "Total")
  expect_true(all(which(result$Level_Type == "Bottom") > which(result$Level_Type == "Total")))
})

# -- summarize_grouped_hierarchy tests --

test_that("summarize_grouped_hierarchy creates correct summary", {
  skip_if_not_installed("hts")

  original_combos <- c("A_X", "A_Y", "B_X", "B_Y")

  nodes <- matrix(
    c("A", "A", "B", "B",
      "X", "Y", "X", "Y"),
    nrow = 2, byrow = TRUE
  )
  rownames(nodes) <- c("Group1", "Group2")

  hts_combos <- c(
    "Total",
    "A", "B",
    "X", "Y",
    "A_X", "A_Y", "B_X", "B_Y"
  )

  result <- summarize_grouped_hierarchy(original_combos, hts_combos, nodes)

  expect_s3_class(result, "data.frame")
  expect_true("Hierarchy_Level" %in% names(result))
  expect_true("Level_Type" %in% names(result))
  expect_true("Total" %in% result$Level_Type)
  expect_true("Group1" %in% result$Level_Type)
  expect_true("Group2" %in% result$Level_Type)
  expect_true("Bottom" %in% result$Level_Type)

  total_row <- result[result$Level_Type == "Total", ]
  expect_equal(total_row$Num_Bottom_Series, 4)

  bottom_rows <- result[result$Level_Type == "Bottom", ]
  expect_equal(nrow(bottom_rows), 4)
  expect_true(all(bottom_rows$Num_Bottom_Series == 1))
})

test_that("summarize_grouped_hierarchy matches combos to groups", {
  skip_if_not_installed("hts")

  original_combos <- c("X1", "X2", "Y1")

  nodes <- matrix(
    c("X", "X", "Y"),
    nrow = 1, byrow = TRUE
  )
  rownames(nodes) <- c("Category")

  hts_combos <- c("Total", "X", "Y", "X1", "X2", "Y1")

  result <- summarize_grouped_hierarchy(original_combos, hts_combos, nodes)

  x_row <- result[result$Hierarchy_Level == "X", ]
  expect_equal(x_row$Num_Bottom_Series, 2)

  y_row <- result[result$Hierarchy_Level == "Y", ]
  expect_equal(y_row$Num_Bottom_Series, 1)
})

# -- adjust_df tests --

test_that("adjust_df returns data frame as-is for df return_type", {
  df <- tibble::tibble(
    Combo = rep("A", 5),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 5),
    Target = c(1, 2, 3, 4, 5)
  )

  result <- adjust_df(df, return_type = "df")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_equal(result$Target, df$Target)
})

# -- get_hts tests --

test_that("get_hts creates hts object for standard hierarchy", {
  skip_if_not_installed("hts")

  mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), nrow = 5, ncol = 2)
  colnames(mat) <- c("A", "B")
  ts_data <- stats::ts(mat, start = c(2020, 1), frequency = 12)

  result <- get_hts(ts_data, nodes = list(2), forecast_approach = "standard_hierarchy")
  expect_true(inherits(result, "hts"))
})

test_that("get_hts creates gts object for grouped hierarchy", {
  skip_if_not_installed("hts")

  mat <- matrix(1:15, nrow = 5, ncol = 3)
  colnames(mat) <- c("A_X", "A_Y", "B_X")
  ts_data <- stats::ts(mat, start = c(2020, 1), frequency = 12)

  groups <- matrix(
    c("A", "A", "B",
      "X", "Y", "X"),
    nrow = 2, byrow = TRUE
  )
  rownames(groups) <- c("Group1", "Group2")

  result <- get_hts(ts_data, nodes = groups, forecast_approach = "grouped_hierarchy")
  expect_true(inherits(result, "gts"))
})

# -- get_hts_nodes tests --

test_that("get_hts_nodes returns nodes for standard hierarchy", {
  skip_if_not_installed("hts")

  mat <- matrix(1:10, nrow = 5, ncol = 2)
  colnames(mat) <- c("A", "B")
  ts_data <- stats::ts(mat, start = c(2020, 1), frequency = 12)
  hts_obj <- hts::hts(ts_data, nodes = list(2)) %>% suppressMessages()

  result <- get_hts_nodes(hts_obj, forecast_approach = "standard_hierarchy")
  expect_true(is.list(result))
})
