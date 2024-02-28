
test_that("prep_hierarchical_data returns correct grouped hierarchies", {
  # Mock data setup
  # Sample data creation with 2 regions and 4 products
  set.seed(123) # For reproducibility
  data <- tibble::tibble(
    Region = rep(c("North", "South"), each = 50), # 2 regions
    Product = rep(c("A", "B", "C", "D"), times = 25), # 4 products repeated across regions
    Date = rep(seq(as.Date("2023-01-01"), length = 25, by = "month"), 4),
    Target = round(runif(100, 100, 500)), # Random target data
    All_Driver = round(runif(100, 100, 500)) # Random driver data for every time series
  )

  # Generate unique combinations of Region and Date with Region_Driver
  region_data <- data %>%
    dplyr::select(Region, Date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Region_Driver = round(runif(dplyr::n(), 500000, 2500000))) # Random region-specific driver

  # Generate global driver data unique for each Date
  global_driver_data <- data %>%
    dplyr::select(Date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Global_Driver = round(runif(dplyr::n(), 100, 500))) # Random global driver

  # Join the region driver data back to the main dataset
  data_with_region_driver <- data %>%
    dplyr::left_join(region_data, by = c("Region", "Date"))

  # Join the global driver data back to the dataset
  final_data <- data_with_region_driver %>%
    dplyr::left_join(global_driver_data, by = "Date") %>%
    tidyr::unite("Combo",
      c("Region", "Product"),
      sep = "--",
      remove = F
    )

  # run prep hts function
  result_data <- prep_hierarchical_data(
    input_data = final_data,
    run_info = set_run_info(),
    combo_variables = c("Region", "Product"),
    external_regressors = c("All_Driver", "Region_Driver", "Global_Driver"),
    forecast_approach = "grouped_hierarchy",
    frequency_number = 12
  ) %>%
    dplyr::filter(Date == "2023-01-01")

  # Expected output setup
  expected_data <- tibble::tibble(
    Combo = c(
      "Total", "Region_North", "Region_South", "Product_A", "Product_B", "Product_C", "Product_D",
      "North_A", "North_B", "North_C", "North_D", "South_C", "South_D", "South_A", "South_B"
    ),
    Date = as.Date(rep("2023-01-01", 15)),
    Target = c(904, 598, 306, 215, 383, 118, 188, 215, 383, 0, 0, 118, 188, 0, 0),
    All_Driver = c(1620, 834, 786, 340, 494, 439, 347, 340, 494, 0, 0, 439, 347, 0, 0),
    Region_Driver = c(2267892, 977452, 1290440, 2267892, 2267892, 2267892, 2267892, 977452, 977452, NA, NA, 1290440, 1290440, NA, NA),
    Global_Driver = rep(203, 15)
  )

  # Assertions
  expect_equal(result_data, expected_data)
})
