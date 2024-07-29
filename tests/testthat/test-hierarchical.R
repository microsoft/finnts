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
