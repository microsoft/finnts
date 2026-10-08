# Build a crossed source-grain regression with repeated equal area values.
# Each region/date owns its SqFt; leaves repeat it across functions. No I/O.
hierarchy_xreg_input <- function() {
  data <- expand.grid(ExecFunction = c("A", "B"),
    SubRegion = c("x", "y", "z"), Date = as.Date(c("2026-07-01", "2026-08-01")),
    stringsAsFactors = FALSE)
  data$Site <- paste0(data$ExecFunction, data$SubRegion)
  data$SqFt <- c(x = 10, y = 10, z = 20)[data$SubRegion]
  data$Target <- 1
  data$Ramadan <- as.numeric(data$Date == as.Date("2026-07-01"))
  tidyr::unite(data, "Combo", ExecFunction, SubRegion, Site, sep = "--", remove = FALSE)
}

test_that("mapping requires a functional dependency, not count reduction", {
  data <- hierarchy_xreg_input()
  mapping <- external_regressor_mapping(data, c("ExecFunction", "SubRegion", "Site"),
    c("SqFt", "Ramadan"))
  expect_identical(mapping$Var[mapping$Regressor == "SqFt"], "SubRegion")
  expect_identical(mapping$Var[mapping$Regressor == "Ramadan"], "Global")
  conflicting <- rbind(data, transform(data[1, ], SqFt = 999))
  expect_error(external_regressor_mapping(conflicting,
    c("ExecFunction", "SubRegion", "Site"), "SqFt"), "bottom-series/date")
  data$SqFt[1] <- Inf
  expect_error(external_regressor_mapping(data,
    c("ExecFunction", "SubRegion", "Site"), "SqFt"), "nonfinite")
  data$SqFt[1] <- NaN
  expect_error(external_regressor_mapping(data,
    c("ExecFunction", "SubRegion", "Site"), "SqFt"), "nonfinite")
})

test_that("equivalent ties are deterministic and incompatible ties fail", {
  data <- hierarchy_xreg_input()
  data$Alias <- paste0("region_", data$SubRegion)
  vars <- c("ExecFunction", "SubRegion", "Alias", "Site")
  expect_identical(external_regressor_mapping(data, vars, "SqFt")$Var, "SubRegion")
  expect_identical(external_regressor_mapping(data, rev(vars), "SqFt")$Var, "Alias")
  ambiguous <- data.frame(A = c("a", "b", "c", "d"),
    B = c("w", "x", "y", "z"), Leaf = letters[1:4],
    Date = as.Date("2026-08-01"), Reg = c(10, 10, 20, 20))
  ambiguous <- rbind(ambiguous, transform(ambiguous, Leaf = LETTERS[1:4],
    B = c("x", "w", "z", "y")))
  expect_error(external_regressor_mapping(ambiguous, c("A", "B", "Leaf"), "Reg"),
    "ambiguous source levels")
})

test_that("grouped node drivers count source entities, not distinct values", {
  data <- hierarchy_xreg_input()
  data <- data[!(data$ExecFunction == "B" & data$SubRegion == "z"), ]
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("ExecFunction", "SubRegion", "Site"), c("SqFt", "Ramadan"), "grouped_hierarchy", 12)
  expect_false(anyDuplicated(result[c("Combo", "Date")]) > 0)
  expect_equal(result$SqFt[result$Combo == "Total"], c(40, 40))
  expect_equal(result$SqFt[result$Combo == "ExecFunction_A"], c(40, 40))
  expect_equal(result$SqFt[result$Combo == "ExecFunction_B"], c(20, 20))
  for (region in c("x", "y", "z")) {
    expect_equal(result$SqFt[result$Combo == paste0("SubRegion_", region)],
      rep(c(x = 10, y = 10, z = 20)[[region]], 2))
  }
  expect_equal(result$Ramadan, as.numeric(result$Date == as.Date("2026-07-01")))
  expected_bottoms <- data
  expected_bottoms$Combo <- gsub("--", "_", expected_bottoms$Combo, fixed = TRUE)
  checked <- dplyr::inner_join(expected_bottoms[c("Combo", "Date", "SqFt")],
    result, by = c("Combo", "Date"), suffix = c("_input", "_output"))
  expect_equal(nrow(checked), nrow(data))
  expect_equal(checked$SqFt_input, checked$SqFt_output)
})

test_that("standard parent nodes aggregate only their source children", {
  data <- data.frame(Region = c("N", "N", "S", "S"),
    Country = c("a", "b", "c", "d"), Leaf = letters[1:4],
    Date = as.Date("2026-08-01"), Target = 1, SqFt = c(10, 10, 30, 40))
  data <- rbind(data, transform(data, Leaf = LETTERS[1:4]))
  data <- tidyr::unite(data, "Combo", Region, Country, Leaf, sep = "--", remove = FALSE)
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("Region", "Country", "Leaf"), "SqFt", "standard_hierarchy", 12)
  expect_equal(result$SqFt[result$Combo == "Total"], 90)
  expect_equal(result$SqFt[result$Combo == "A"], 20)
  expect_equal(result$SqFt[result$Combo == "B"], 70)
  expect_equal(result$SqFt[result$Combo == "AA"], 10)
  expect_equal(result$SqFt[result$Combo == "AB"], 10)
})

test_that("composite sources preserve tuple identity and node membership", {
  data <- expand.grid(A = c("a--b", "a"), B = c("c", "b--c"),
    Leaf = c("one", "two"), stringsAsFactors = FALSE)
  data$Date <- as.Date("2026-08-01")
  data$Target <- 1
  data$Reg <- c(10, 20, 30, 10)[match(paste(data$A, data$B, sep = "|"),
    c("a--b|c", "a|c", "a--b|b--c", "a|b--c"))]
  data$Combo <- paste0("leaf", seq_len(nrow(data)))
  expect_identical(external_regressor_mapping(data, c("A", "B", "Leaf"), "Reg")$Var,
    "A---B")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("A", "B", "Leaf"), "Reg", "grouped_hierarchy", 12)
  expect_equal(result$Reg[result$Combo == "Total"], 70)
  expect_equal(result$Reg[result$Combo == "Leaf_one"], 70)
  expect_equal(result$Reg[result$Combo == "Leaf_two"], 70)
  expect_equal(result$Reg[result$Combo == "A_a"], 30)
})

test_that("incompatible composite source partitions fail explicitly", {
  data <- expand.grid(A = 0:1, B = 0:1, C = 0:1, D = 0:1, Leaf = 0:1)
  data <- data[(data$A + data$B) %% 2 == (data$C + data$D) %% 2, ]
  data$Date <- as.Date("2026-08-01")
  data$Reg <- 10 + (data$A + data$B) %% 2
  expect_error(external_regressor_mapping(data, c("A", "B", "C", "D", "Leaf"), "Reg"),
    "ambiguous source levels")
})

test_that("source identity does not overwrite a user regressor", {
  data <- hierarchy_xreg_input()
  names(data)[names(data) == "SqFt"] <- ".source_id"
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("ExecFunction", "SubRegion", "Site"), ".source_id", "grouped_hierarchy", 12)
  expect_equal(result$.source_id[result$Combo == "Total"], c(40, 40))
})

test_that("missing bottom observations are preserved without false conflicts", {
  data <- hierarchy_xreg_input()
  data$SqFt[1] <- NA_real_
  future <- transform(data[data$Date == as.Date("2026-08-01"), ],
    Date = as.Date("2026-09-01"), Target = NA_real_, SqFt = NA_real_)
  data <- rbind(data, future)
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("ExecFunction", "SubRegion", "Site"), "SqFt", "grouped_hierarchy", 12)
  expect_true(all(is.na(result$SqFt[result$Date == as.Date("2026-09-01")])))
  expect_true(is.na(result$SqFt[result$Combo == "A_x_Ax" &
    result$Date == as.Date("2026-07-01")]))
  expect_equal(result$SqFt[result$Combo == "Total" &
    result$Date == as.Date("2026-07-01")], 40)
})

test_that("zero-heavy source drivers retain their grain across all-zero dates", {
  data <- hierarchy_xreg_input()
  data$SqFt <- 0
  data$SqFt[data$SubRegion == "z" & data$Date == as.Date("2026-08-01")] <- 20
  vars <- c("ExecFunction", "SubRegion", "Site")
  expect_identical(external_regressor_mapping(data, vars, "SqFt")$Var, "SubRegion")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "SqFt", "grouped_hierarchy", 12)
  expect_equal(result$SqFt[result$Combo == "Total"], c(0, 20))
  expect_equal(result$SqFt[result$Combo == "ExecFunction_A"], c(0, 20))
  expect_equal(result$SqFt[result$Combo == "ExecFunction_B"], c(0, 20))
  expect_equal(result$SqFt[result$Combo == "SubRegion_x"], c(0, 0))
  expect_false(anyNA(result$SqFt))
})

test_that("zero-heavy composite drivers count each source tuple once", {
  data <- expand.grid(A = c("a", "b"), B = c("x", "y"),
    Leaf = c("one", "two"), Date = as.Date(c("2026-07-01", "2026-08-01")),
    stringsAsFactors = FALSE)
  data$Target <- 1
  data$Reg <- 0
  data$Reg[data$A == "b" & data$B == "y" &
    data$Date == as.Date("2026-08-01")] <- 10
  data <- tidyr::unite(data, "Combo", A, B, Leaf, sep = "--", remove = FALSE)
  vars <- c("A", "B", "Leaf")
  expect_identical(external_regressor_mapping(data, vars, "Reg")$Var, "A---B")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "Reg", "grouped_hierarchy", 12)
  expect_equal(result$Reg[result$Combo == "Total"], c(0, 10))
  expect_equal(result$Reg[result$Combo == "A_a"], c(0, 0))
  expect_equal(result$Reg[result$Combo == "A_b"], c(0, 10))
  expect_equal(result$Reg[result$Combo == "Leaf_one"], c(0, 10))
  expect_equal(result$Reg[result$Combo == "Leaf_two"], c(0, 10))
})

test_that("entirely zero drivers remain Global without missing aggregates", {
  data <- hierarchy_xreg_input()
  data$SqFt <- 0
  vars <- c("ExecFunction", "SubRegion", "Site")
  expect_identical(external_regressor_mapping(data, vars, "SqFt")$Var, "Global")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "SqFt", "grouped_hierarchy", 12)
  expect_true(all(result$SqFt == 0))
  expect_false(anyDuplicated(result[c("Combo", "Date")]) > 0)
})

test_that("observed zeros differ from missing-only level aggregates", {
  data <- hierarchy_xreg_input()
  data$SqFt[data$SubRegion == "x"] <- 0
  data$SqFt[data$SubRegion == "y"] <- NA_real_
  missing_bottom <- data$ExecFunction == "A" & data$SubRegion == "x"
  data$SqFt[missing_bottom] <- NA_real_
  vars <- c("ExecFunction", "SubRegion", "Site")
  expect_identical(external_regressor_mapping(data, vars, "SqFt")$Var, "SubRegion")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "SqFt", "grouped_hierarchy", 12)
  expect_equal(result$SqFt[result$Combo == "SubRegion_x"], c(0, 0))
  expect_true(all(is.na(result$SqFt[result$Combo == "SubRegion_y"])))
  expect_true(all(is.na(result$SqFt[result$Combo == "A_x_Ax"])))
  expect_equal(result$SqFt[result$Combo == "B_x_Bx"], c(0, 0))
  expect_equal(result$SqFt[result$Combo == "Total"], c(20, 20))
})

test_that("sparse nonzero observations invalidate otherwise zero source candidates", {
  data <- hierarchy_xreg_input()
  data$SqFt <- 0
  data$SqFt[data$ExecFunction == "A" & data$SubRegion == "z" &
    data$Date == as.Date("2026-08-01")] <- 20
  vars <- c("ExecFunction", "SubRegion", "Site")
  expect_identical(external_regressor_mapping(data, vars, "SqFt")$Var, "All")
  conflict <- rbind(data, transform(data[1, ], SqFt = 1))
  expect_error(external_regressor_mapping(conflict, vars, "SqFt"), "bottom-series/date")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "SqFt", "grouped_hierarchy", 12)
  expect_equal(result$SqFt[result$Combo == "Total"], c(0, 20))
  expect_equal(result$SqFt[result$Combo == "ExecFunction_B"], c(0, 0))
})

test_that("three-column source grains remain supported within the search budget", {
  data <- expand.grid(A = 0:1, B = 0:1, C = 0:1, Leaf = 0:1)
  data$Date <- as.Date("2026-08-01")
  data$Target <- 1
  data$Reg <- 10 + (data$A + data$B + data$C) %% 2
  data[c("A", "B", "C", "Leaf")] <- lapply(data[c("A", "B", "C", "Leaf")], as.character)
  data <- tidyr::unite(data, "Combo", A, B, C, Leaf, sep = "--", remove = FALSE)
  expect_identical(external_regressor_mapping(data, c("A", "B", "C", "Leaf"), "Reg")$Var,
    "A---B---C")
  testthat::local_mocked_bindings(write_data = function(...) invisible(NULL))
  result <- prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    c("A", "B", "C", "Leaf"), "Reg", "grouped_hierarchy", 12)
  expect_equal(result$Reg[result$Combo == "Total"], 84)
  expect_equal(result$Reg[result$Combo == "Leaf_0"], 84)
  expect_equal(result$Reg[result$Combo == "A_0"], 42)
})

test_that("adversarial inference stops before a partial width or artifact writes", {
  vars <- paste0("V", seq_len(11))
  data <- expand.grid(stats::setNames(rep(list(0:1), 11), vars))
  data$Reg <- rowSums(data) %% 2
  data$Date <- as.Date("2026-08-01")
  data$Target <- 1
  data$Combo <- paste0("bottom", seq_len(nrow(data)))
  scans <- 0L
  original_distinct <- dplyr::distinct
  # Count the actual table scans; the parity fixture keeps every partial grain
  # invalid and repeated. Widths 1:5 contain exactly 1,023 candidates.
  testthat::local_mocked_bindings(.package = "dplyr", distinct = function(...) {
    scans <<- scans + 1L
    original_distinct(...)
  })
  testthat::local_mocked_bindings(
    sum_hts_data = function(...) tibble::tibble(),
    write_data = function(...) stop("Unexpected inference-failure write"))
  expect_error(prep_hierarchical_data(data, set_run_info(path = withr::local_tempdir()),
    vars, "Reg", "grouped_hierarchy", 12), "1,024-candidate search budget")
  # Four setup distinct calls plus one prep combo-table call, before candidates.
  expect_equal(scans, 5L + 3L * 1023L)
})

test_that("the candidate budget does not impose a three-column width limit", {
  vars <- c("A", "B", "C", "D", "Leaf")
  data <- expand.grid(stats::setNames(rep(list(0:1), 5), vars))
  data$Reg <- rowSums(data[c("A", "B", "C", "D")]) %% 2
  data$Date <- as.Date("2026-08-01")
  expect_identical(external_regressor_mapping(data, vars, "Reg")$Var, "A---B---C---D")
})
