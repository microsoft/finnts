test_that("clamp_negative_target clamps negatives for croston", {
  df <- data.frame(Target = c(-5, 0, 3, -1, 10))

  expect_warning(
    result <- clamp_negative_target(df, "croston"),
    "requires non-negative Target values.*2 negative values clamped"
  )

  expect_equal(result$Target, c(0, 0, 3, 0, 10))
})

test_that("clamp_negative_target is a no-op for non-croston models", {
  df <- data.frame(Target = c(-5, 0, 3, -1, 10))

  result <- clamp_negative_target(df, "arima")
  expect_equal(result$Target, c(-5, 0, 3, -1, 10))
})

test_that("clamp_negative_target is a no-op when no negatives exist", {
  df <- data.frame(Target = c(0, 3, 10))

  # no warning when there are no negatives
  expect_silent(clamp_negative_target(df, "croston"))
  result <- clamp_negative_target(df, "croston")
  expect_equal(result$Target, c(0, 3, 10))
})

test_that("clamp_negative_target handles NA values without error", {
  df <- data.frame(Target = c(-2, NA, 5, -1))

  expect_warning(
    result <- clamp_negative_target(df, "croston"),
    "2 negative values clamped"
  )

  expect_equal(result$Target, c(0, NA, 5, 0))
})
