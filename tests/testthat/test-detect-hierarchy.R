# Check the selected route and HTS membership using independent bottom impulses.
# Every aggregation must correspond to Total, one supplied level, or one leaf.
# No artifacts are saved, and one-series results bypass multivariate HTS.
expect_detected_membership <- function(data, expected) {
  variables <- names(data)
  result <- detect_hierarchy(data, variables)
  expect_identical(result$forecast_approach, expected)
  combos <- dplyr::distinct(normalize_combo_values(data, variables))
  agent <- list(project_info = list(combo_variables = variables), run_id = "test")
  expect_identical(hierarchy_detect(agent, combos, write_data = FALSE),
    if (length(variables) == 1L) "bottoms_up" else expected)
  expect_identical(analyze_hierarchy(combos, variables), result)
  if (expected == "bottoms_up") {
    expect_equal(nrow(combos), 1L)
    return(invisible(result))
  }
  if (expected == "standard_hierarchy") {
    combos <- dplyr::arrange(combos, dplyr::across(tidyselect::all_of(result$hierarchy_order)))
  }
  n <- nrow(combos)
  nodes <- pick_right_hierarchy(combos, variables, expected)
  object <- get_hts(stats::ts(diag(n), frequency = 12), nodes, expected)
  membership <- t(as.matrix(hts::allts(object)))
  oracle <- rbind(rep(1, n), diag(n))
  for (variable in variables) {
    for (label in unique(combos[[variable]])) {
      oracle <- rbind(oracle, as.integer(combos[[variable]] == label))
    }
  }
  actual_keys <- apply(membership, 1, paste, collapse = ",")
  expected_keys <- apply(oracle, 1, paste, collapse = ",")
  expect_true(all(actual_keys %in% expected_keys))
  expect_true(all(expected_keys %in% actual_keys))
  expect_equal(as.numeric(tail(membership, n)), as.numeric(diag(n)))
  invisible(result)
}

test_that("nested and crossed routes match actual HTS membership", {
  nested <- data.frame(
    Region = c("N", "N", "N", "S"), Country = c("N1", "N1", "N2", "S1"),
    Site = c("a", "b", "c", "d")
  )
  expect_detected_membership(nested, "standard_hierarchy")
  expect_detected_membership(nested[, 3:1], "standard_hierarchy")
  expect_detected_membership(nested[c(4, 2, 1, 3), ], "standard_hierarchy")
  expect_detected_membership(rbind(nested, nested), "standard_hierarchy")
  reused <- nested
  reused$Country[4] <- "N1"
  result <- expect_detected_membership(reused, "grouped_hierarchy")
  expect_equal(result$conflicts, data.frame(
    parent = "Region", child = "Country", child_label = "N1", parent_count = 2L
  ))
  crossed <- expand.grid(A = c("a", "b"), B = c("x", "y"), stringsAsFactors = FALSE)
  expect_detected_membership(crossed, "grouped_hierarchy")
  crossed$Leaf <- letters[1:4]
  expect_detected_membership(crossed, "grouped_hierarchy")
  expect_detected_membership(transform(crossed, Root = "All"), "grouped_hierarchy")
  expect_detected_membership(transform(nested, Root = "All"), "standard_hierarchy")
  expect_detected_membership(data.frame(A = c("a", "b"), B = c("x", "y")), "standard_hierarchy")
  expect_detected_membership(data.frame(A = c("a", "a", "b"), B = c("x", "y", "y")), "grouped_hierarchy")
  expect_detected_membership(data.frame(Site = c("a", "b")), "standard_hierarchy")
  expect_detected_membership(data.frame(A = "a", B = "b"), "bottoms_up")
  expect_detected_membership(data.frame(A = c("a-b", "a"), B = c("c", "b-c")), "standard_hierarchy")
  expect_detected_membership(data.frame(A = c(1, 1, 2), B = c(10, 20, 30)), "standard_hierarchy")
  expect_detected_membership(transform(nested, Country = factor(Country)), "standard_hierarchy")
  expect_detected_membership(setNames(nested, c("Top level", "Middle level", "Leaf level")), "standard_hierarchy")
  equivalent <- as.data.frame(setNames(rep(list(c("a", "b")), 15), paste0("V", 1:15)))
  expect_detected_membership(equivalent, "standard_hierarchy")
})

test_that("normalization matches preprocessing without mutating input", {
  data <- data.frame(A = factor(c(" a ", "a", " b ")), B = c("x", "y ", "z"))
  original <- data
  result <- detect_hierarchy(data, c("A", "B"))
  expect_equal(result$counts, c(A = 2L, B = 3L))
  expect_identical(data, original)
  expect_identical(result, detect_hierarchy(normalize_combo_values(data, c("A", "B")), c("A", "B")))
})

test_that("detection never writes artifacts or overrides explicit preprocessing choices", {
  testthat::local_mocked_bindings(
    write_data = function(...) stop("Unexpected artifact write"),
    list_files = function(...) stop("Unexpected artifact listing")
  )
  data <- data.frame(
    A = c("a", "a", "b"), B = c("x", "y", "z"),
    Date = as.Date("2026-08-01"), Target = 1
  )
  expect_identical(detect_hierarchy(data, c("A", "B"), "Target",
    as.Date("2025-08-01"), as.Date("2026-08-01"))$forecast_approach, "standard_hierarchy")
  expect_identical(formals(prep_data)$forecast_approach, "bottoms_up")
})

test_that("cleanup uses engine activity and excludes future targets", {
  data <- data.frame(
    Region = c("N", "N", "S", "S"), Product = c("P", "Q", "P", "Q"),
    Date = as.Date("2026-08-01"), Revenue = c(10, 0, 0, 20)
  )
  original <- data
  result <- detect_hierarchy(data, c("Region", "Product"), "Revenue",
    as.Date("2025-08-01"), as.Date("2026-08-01"))
  expect_identical(result$forecast_approach, "standard_hierarchy")
  expect_equal(result$total_ts, 2L)
  expect_identical(data, original)
  future <- transform(data, Date = as.Date("2026-09-01"), Revenue = 100)
  expect_identical(result, detect_hierarchy(rbind(data, future), c("Region", "Product"),
    "Revenue", as.Date("2025-08-01"), as.Date("2026-08-01")))
  expect_identical(detect_hierarchy(data, c("Region", "Product"))$forecast_approach, "grouped_hierarchy")
  negative <- transform(data, Date = as.Date("2026-07-01"), Revenue = -Revenue)
  expect_error(detect_hierarchy(rbind(data, negative), c("Region", "Product"), "Revenue",
    as.Date("2025-08-01"), as.Date("2026-08-01")), "No retained")
  data$Revenue <- c(10, NA, NA, NA)
  expect_identical(detect_hierarchy(data, c("Region", "Product"), "Revenue",
    as.Date("2025-08-01"), as.Date("2026-08-01"))$forecast_approach, "bottoms_up")
})

test_that("invalid detector inputs fail explicitly", {
  data <- data.frame(A = c("a", "b"), B = c("x", "y"))
  expect_error(detect_hierarchy(list(), "A"), "local data frame")
  expect_error(detect_hierarchy(setNames(data, c("A", "A")), "A"), "unique column names")
  for (variables in list(character(), c("A", "A"), NA_character_, "")) {
    expect_error(detect_hierarchy(data, variables), "nonempty, unique")
  }
  expect_error(detect_hierarchy(data, "missing"), "Missing combo columns")
  expect_error(detect_hierarchy(data[0, ], c("A", "B")), "No retained")
  expect_error(detect_hierarchy(transform(data, A = c(NA, "a")), "A"), "Missing, blank")
  expect_error(detect_hierarchy(transform(data, A = c(" ", "a")), "A"), "Missing, blank")
  expect_error(detect_hierarchy(data.frame(A = c(1, Inf)), "A"), "nonfinite")
  expect_error(detect_hierarchy(data.frame(A = I(list("a", "b"))), "A"), "Unsupported")
  matrix_column <- data
  matrix_column$A <- matrix(1:4, nrow = 2)
  expect_error(detect_hierarchy(matrix_column, "A"), "Unsupported")
  expect_error(detect_hierarchy(data.frame(A = as.Date("2026-01-01")), "A"), "Unsupported")
  expect_error(detect_hierarchy(data.frame(Date = "x"), "Date"), "reserved")
  expect_error(detect_hierarchy(data.frame(A = c("a--b", "a"), B = c("c", "b--c")), c("A", "B")),
    "same '--'-joined", fixed = TRUE)
  data$Date <- as.Date("2026-08-01")
  data$Target <- 1
  expect_error(detect_hierarchy(data, "A", "Target", as.Date("2025-08-01")), "Date scalars")
  expect_error(detect_hierarchy(data, "A", "Target", as.Date("2027-08-01"), as.Date("2026-08-01")), "Date scalars")
  expect_error(detect_hierarchy(data, "A", "missing", as.Date("2025-08-01"), as.Date("2026-08-01")), "target_variable")
  expect_error(detect_hierarchy(transform(data, Target = "1"), "A", "Target",
    as.Date("2025-08-01"), as.Date("2026-08-01")), "numeric")
  expect_error(detect_hierarchy(transform(data, Target = Inf), "A", "Target",
    as.Date("2025-08-01"), as.Date("2026-08-01")), "infinite")
  expect_error(detect_hierarchy(transform(data, Date = "2026-08-01"), "A", "Target",
    as.Date("2025-08-01"), as.Date("2026-08-01")), "'Date' column")
})

test_that("seeded structures agree with exhaustive nesting oracle", {
  withr::local_seed(69431)
  orders <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  for (iteration in seq_len(100)) {
    data <- if (iteration %% 4L == 0L) {
      n <- sample(4:12, 1)
      data.frame(A = ceiling(seq_len(n) / 4), B = ceiling(seq_len(n) / 2), C = seq_len(n))
    } else {
      as.data.frame(matrix(sample(letters[1:4], 36, replace = TRUE), ncol = 3))
    }
    # Independent oracle checks all parent prefixes in every possible order.
    standard <- any(vapply(orders, function(order) {
      all(vapply(1:2, function(i) {
        prefix <- data[, order[seq_len(i + 1L)], drop = FALSE]
        nrow(unique(prefix)) == length(unique(prefix[[i + 1L]]))
      }, logical(1)))
    }, logical(1)))
    expect_detected_membership(data, if (standard) "standard_hierarchy" else "grouped_hierarchy")
  }
})
