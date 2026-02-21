# Tests for helper functions in agent_summarize_models.R

test_that(".chr1 handles NULL", {
  expect_equal(.chr1(NULL), "auto")
})

test_that(".chr1 handles missing quosure", {
  q <- rlang::quo()
  expect_equal(.chr1(q), "auto")
})

test_that(".chr1 handles non-missing quosure", {
  q <- rlang::quo(my_var)
  expect_equal(.chr1(q), "my_var")
})

test_that(".chr1 handles language object", {
  expr <- quote(x + y)
  expect_equal(.chr1(expr), "x + y")
})

test_that(".chr1 handles zero-length vector", {
  expect_equal(.chr1(character(0)), "auto")
})

test_that(".chr1 handles single atomic value", {
  expect_equal(.chr1(42), "42")
  expect_equal(.chr1("hello"), "hello")
  expect_equal(.chr1(TRUE), "TRUE")
})

test_that(".chr1 handles multi-element atomic vector", {
  expect_equal(.chr1(c(1, 2, 3)), "1,2,3")
  expect_equal(.chr1(c("a", "b")), "a,b")
})

test_that(".chr1 handles rlang missing", {
  expect_equal(.chr1(rlang::missing_arg()), "auto")
})

test_that(".kv creates proper tibble", {
  result <- .kv("sec", "nm", "val")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$section, "sec")
  expect_equal(result$name, "nm")
  expect_equal(result$value, "val")
})

test_that(".kv coerces types to character", {
  result <- .kv(1, 2, 3)
  expect_equal(result$section, "1")
  expect_equal(result$name, "2")
  expect_equal(result$value, "3")
})

test_that(".unquote strips double quotes", {
  expect_equal(.unquote('"hello"'), "hello")
})

test_that(".unquote strips single quotes", {
  expect_equal(.unquote("'hello'"), "hello")
})

test_that(".unquote leaves unquoted strings alone", {
  expect_equal(.unquote("hello"), "hello")
})

test_that(".unquote handles non-character input", {
  expect_equal(.unquote(42), "42")
})

test_that(".signif_chr formats numeric values", {
  expect_equal(.signif_chr("3.14159", digits = 3), "3.14")
  expect_equal(.signif_chr("1234567", digits = 4), "1235000")
})

test_that(".signif_chr passes through non-numeric strings", {
  expect_equal(.signif_chr("hello"), "hello")
  expect_equal(.signif_chr("NA"), "NA")
})

test_that(".signif_chr handles Inf and NaN", {
  expect_equal(.signif_chr("Inf"), "Inf")
  expect_equal(.signif_chr("NaN"), "NaN")
})

test_that(".extract_predictors handles try-error", {
  result <- .extract_predictors(structure("error", class = "try-error"))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("section", "name", "value"))
})

test_that(".extract_predictors handles NULL predictors", {
  mold <- list(predictors = NULL)
  result <- .extract_predictors(mold)
  expect_equal(nrow(result), 0)
})

test_that(".extract_predictors handles empty predictors", {
  mold <- list(predictors = data.frame())
  result <- .extract_predictors(mold)
  expect_equal(nrow(result), 0)
})

test_that(".extract_predictors extracts predictor info", {
  mold <- list(predictors = data.frame(x1 = 1:3, x2 = c("a", "b", "c"), stringsAsFactors = FALSE))
  result <- .extract_predictors(mold)
  expect_equal(nrow(result), 2)
  expect_true(all(result$section == "predictor"))
  expect_true("x1" %in% result$name)
  expect_true("x2" %in% result$name)
})

test_that(".extract_outcomes handles try-error", {
  result <- .extract_outcomes(structure("error", class = "try-error"))
  expect_equal(nrow(result), 0)
})

test_that(".extract_outcomes handles NULL outcomes", {
  mold <- list(outcomes = NULL)
  result <- .extract_outcomes(mold)
  expect_equal(nrow(result), 0)
})

test_that(".extract_outcomes extracts outcome info", {
  mold <- list(outcomes = data.frame(Target = c(1.5, 2.5, 3.5)))
  result <- .extract_outcomes(mold)
  expect_equal(nrow(result), 1)
  expect_equal(result$section, "outcome")
  expect_equal(result$name, "Target")
})

test_that(".extract_recipe_steps handles non-recipe object", {
  result <- .extract_recipe_steps("not_a_recipe")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("section", "name", "value"))
})

test_that(".extract_recipe_steps handles recipe with steps", {
  rec <- recipes::recipe(Target ~ ., data = data.frame(Target = 1:5, x1 = 1:5)) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors())
  result <- .extract_recipe_steps(rec)
  expect_equal(nrow(result), 2)
  expect_true(all(result$section == "recipe_step"))
  expect_true(any(grepl("zero variance", result$value, ignore.case = TRUE) |
    grepl("step_zv", result$value, ignore.case = TRUE)))
})

test_that(".infer_period_from_dates handles try-error", {
  result <- .infer_period_from_dates(structure("error", class = "try-error"))
  expect_true(is.na(result))
})

test_that(".infer_period_from_dates handles no date columns", {
  mold <- list(predictors = data.frame(x = 1:10))
  result <- .infer_period_from_dates(mold)
  expect_true(is.na(result))
})

test_that(".infer_period_from_dates infers monthly frequency", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  mold <- list(predictors = data.frame(Date = dates, x = 1:24))
  result <- .infer_period_from_dates(mold)
  expect_equal(result, "12")
})

test_that(".infer_period_from_dates infers daily frequency", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  mold <- list(predictors = data.frame(Date = dates, x = 1:100))
  result <- .infer_period_from_dates(mold)
  expect_equal(result, "7")
})

test_that(".infer_period_from_dates infers yearly frequency", {
  dates <- seq(as.Date("2000-01-01"), by = "year", length.out = 20)
  mold <- list(predictors = data.frame(Date = dates, x = 1:20))
  result <- .infer_period_from_dates(mold)
  expect_equal(result, "1")
})

test_that(".infer_period_from_dates infers weekly frequency", {
  dates <- seq(as.Date("2020-01-01"), by = "week", length.out = 52)
  mold <- list(predictors = data.frame(Date = dates, x = 1:52))
  result <- .infer_period_from_dates(mold)
  expect_equal(result, "52")
})

test_that(".infer_period_from_dates infers quarterly frequency", {
  dates <- seq(as.Date("2010-01-01"), by = "quarter", length.out = 20)
  mold <- list(predictors = data.frame(Date = dates, x = 1:20))
  result <- .infer_period_from_dates(mold)
  expect_equal(result, "4")
})

test_that(".find_obj returns NULL when depth exhausted", {
  result <- .find_obj(list(a = 1), function(x) FALSE, depth = 0)
  expect_null(result)
})

test_that(".find_obj returns NULL for NULL input", {
  result <- .find_obj(NULL, function(x) TRUE)
  expect_null(result)
})

test_that(".find_obj finds top-level match", {
  result <- .find_obj(42, function(x) is.numeric(x) && x == 42)
  expect_equal(result, 42)
})

test_that(".find_obj finds nested list element", {
  obj <- list(a = list(b = list(c = "found_it")))
  result <- .find_obj(obj, function(x) identical(x, "found_it"))
  expect_equal(result, "found_it")
})

test_that(".find_obj finds in environment", {
  e <- new.env(parent = emptyenv())
  e$target <- "found"
  result <- .find_obj(e, function(x) identical(x, "found"))
  expect_equal(result, "found")
})

test_that(".find_obj returns NULL when not found", {
  obj <- list(a = 1, b = 2, c = 3)
  result <- .find_obj(obj, function(x) identical(x, "missing"))
  expect_null(result)
})

test_that(".assemble_output builds proper tibble", {
  preds <- tibble::tibble(section = "predictor", name = "x1", value = "numeric")
  outs <- tibble::tibble(section = "outcome", name = "Target", value = "numeric")
  steps <- tibble::tibble(section = "recipe_step", name = "1", value = "Normalize")
  args <- tibble::tibble(section = "model_arg", name = "trees", value = "100")
  eng <- tibble::tibble(section = "engine_param", name = "nthread", value = "1")

  result <- .assemble_output(preds, outs, steps, args, eng,
    model_class = "boost_tree", engine = "xgboost"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("model_class" %in% names(result))
  expect_true("engine" %in% names(result))
  expect_true(all(result$model_class == "boost_tree"))
  expect_true(all(result$engine == "xgboost"))
  # verify ordering: predictor < outcome < recipe_step < model_arg < engine_param
  section_order <- result$section
  pred_idx <- which(section_order == "predictor")
  out_idx <- which(section_order == "outcome")
  step_idx <- which(section_order == "recipe_step")
  arg_idx <- which(section_order == "model_arg")
  eng_idx <- which(section_order == "engine_param")
  if (length(pred_idx) > 0 && length(out_idx) > 0) {
    expect_true(max(pred_idx) < min(out_idx))
  }
  if (length(out_idx) > 0 && length(step_idx) > 0) {
    expect_true(max(out_idx) < min(step_idx))
  }
})

test_that(".assemble_output unquotes values when requested", {
  args <- tibble::tibble(section = "model_arg", name = "penalty", value = '"0.01"')
  result <- .assemble_output(
    tibble::tibble(section = character(), name = character(), value = character()),
    tibble::tibble(section = character(), name = character(), value = character()),
    tibble::tibble(section = character(), name = character(), value = character()),
    args,
    tibble::tibble(section = character(), name = character(), value = character()),
    model_class = "linear_reg", engine = "glmnet",
    unquote_values = TRUE
  )
  expect_equal(unname(result$value[result$name == "penalty"]), "0.01")
})

test_that(".assemble_output deduplicates rows", {
  args1 <- tibble::tibble(section = "model_arg", name = "trees", value = "100")
  args2 <- tibble::tibble(section = "model_arg", name = "trees", value = "100")
  result <- .assemble_output(
    tibble::tibble(section = character(), name = character(), value = character()),
    tibble::tibble(section = character(), name = character(), value = character()),
    tibble::tibble(section = character(), name = character(), value = character()),
    dplyr::bind_rows(args1, args2),
    tibble::tibble(section = character(), name = character(), value = character()),
    model_class = "boost_tree", engine = "xgboost"
  )
  expect_equal(sum(result$name == "trees"), 1)
})
