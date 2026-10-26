# Offline skill/package integration. Run from the repository root with Rscript.
# Generated source and author responses are fixed test fixtures; no provider runs.
Sys.setenv(NOT_CRAN = "false")
pkgload::load_all(".", quiet = TRUE)
library(testthat)
source("tests/testthat/helper-custom-model-authoring.R")
source("tests/testthat/helper-custom-model-historical-drivers.R")
common <- new.env(parent = globalenv())
sys.source("skills/finn/scripts/finn_common.R", common)

# Return script functions in an isolated environment without CLI dispatch.
# Only function assignments are evaluated; no script actions are executed.
skill_functions <- function(file) {
  env <- new.env(parent = common)
  for (expr in parse(file, keep.source = FALSE)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
  }
  env
}
cm <- skill_functions("skills/finn/scripts/custom_model.R")
review <- skill_functions("skills/finn/scripts/validate_config.R")

test_that("skill authoring panels require historical drivers, not premature future inputs", {
  case <- historical_finance_case("customer_growth")
  panel <- cm$custom_model_panel(case$history, "Combo", "Target", case$predictors,
    "month", 2)
  expect_identical(panel$data, case$history)
  case <- historical_finance_case("unit_trend")
  future <- case$future
  future$Target <- NA_real_
  future$Units_Sold <- NA_real_
  raw <- dplyr::bind_rows(case$history, future)[names(case$history)]
  panel <- cm$custom_model_panel(raw, "Combo", "Target", case$predictors, "month", 2)
  expect_equal(panel$data, raw)
  expect_equal(panel$periods, 12L)
})

test_that("skill metadata exposes approved availability without changing old versions", {
  model <- historical_driver_model()
  summary <- common$finn_custom_summary(model)
  expect_identical(summary$historical_only_drivers, list("Units"))
  expect_identical(summary$future_required_drivers, list("Price"))
  legacy <- model
  legacy$definition$requirements$historical_only_predictors <- NULL
  expect_identical(common$finn_custom_summary(legacy)$historical_only_drivers, list())
  expect_identical(common$finn_custom_summary(legacy)$future_required_drivers, list("Units", "Price"))
  paths <- common$finn_project_paths(withr::local_tempdir(), "History")
  common$finn_ensure_project_dirs(paths)
  common$finn_write_json(list(combo_variables = "Combo", date_type = "month",
    forecast_horizon = 2, external_regressors = "Units"), file.path(paths$dir, "config.json"))
  meta <- common$finn_custom_metadata(model)
  expect_identical(meta$historical_only_drivers, list("Units"))
  saved <- common$finn_custom_save(paths, model, meta)
  old <- meta
  old$historical_only_drivers <- old$future_required_drivers <- NULL
  common$finn_write_json(old, saved$json)
  before <- readBin(saved$json, "raw", n = file.info(saved$json)$size)
  model_before <- readBin(saved$rds, "raw", n = file.info(saved$rds)$size)
  versions <- cm$custom_model_versions(paths, model$definition$name)
  expect_identical(unlist(versions[[1]]$historical_only_drivers), "Units")
  expect_identical(readBin(saved$json, "raw", n = file.info(saved$json)$size), before)
  listed <- cm$custom_model_list(list(), paths)$data$models[[1]]$versions[[1]]
  shown <- cm$custom_model_show(list(name = model$definition$name), paths)$data$model
  choices <- cm$custom_model_version_choices(versions)[[1]]
  for (view in list(listed, shown, choices)) {
    expect_identical(view$historical_only_drivers, list("Units"))
    expect_identical(view$future_required_drivers, list("Price"))
  }
  expect_identical(readBin(saved$json, "raw", n = file.info(saved$json)$size), before)
  expect_identical(readBin(saved$rds, "raw", n = file.info(saved$rds)$size), model_before)
  text <- review$validate_custom_models_section(list(summary))
  expect_match(text, "Historical-only", fixed = TRUE)
  expect_match(text, "Future-required", fixed = TRUE)
  template <- cm$custom_model_template(list(name = "history_template", drivers = "Units"), paths)
  expect_true(template$ok)
  expect_match(paste(template$next_actions, collapse = " "), "reviewed contract", fixed = TRUE)
  expect_true(all(is.na(utils::read.csv(template$data$examples_file)$Units)))
})

for (family in c("customer_growth", "unit_trend")) test_that(
  paste("real skill creation validates historical availability for", family), {
  paths <- common$finn_project_paths(withr::local_tempdir(), "CreateHistory")
  common$finn_ensure_project_dirs(paths)
  case <- historical_finance_case(family)
  cfg <- common$finn_merge_config(list(project_name = "CreateHistory",
    combo_variables = "Combo", target_variable = "Target", date_type = "month",
    forecast_horizon = 2, external_regressors = case$predictors,
    agent = list(llm = list(provider = "openai", model = "offline"))))
  common$finn_write_json(cfg, file.path(paths$dir, "config.json"))
  examples <- file.path(paths$dir, "examples.csv")
  utils::write.csv(custom_examples_table(historical_finance_examples(case)),
    examples, row.names = FALSE, na = "")
  stages <- character()
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      stages <<- c(stages, stage)
      if (stage == "source") return(list(status = "candidate", conflict = "",
        fit_body = case$fit, predict_body = case$predict, helpers = list()))
      proposal <- author_proposal()
      proposal$name <- NULL
      proposal$interpretation <- case$instructions
      proposal$defaults_used <- character()
      proposal$historical_only_predictors <- case$historical
      proposal
    }, .package = "finnts")
  cm$finn_load_checked <- function(config, root) list(config = cfg, errors = character(),
    data = if (family == "customer_growth") case$history else
      dplyr::bind_rows(case$history, transform(case$future, Target = NA_real_)))
  calls <- 0L
  cm$finn_build_llm <- function(config) {
    calls <<- calls + 1L
    list()
  }
  args <- list(name = "customer_growth", instructions = case$instructions,
    drivers = paste(case$predictors, collapse = ","), examples = examples)
  preview <- cm$custom_model_create(args, paths)
  expect_identical(preview$status, "needs_confirmation")
  expect_identical(calls, 0L)
  cm$finn_custom_historical_supported <- function() FALSE
  unsupported <- cm$custom_model_create(c(args, list(confirm = "true")), paths)
  expect_identical(unsupported$status, "blocked")
  expect_identical(calls, 0L)
  rm("finn_custom_historical_supported", envir = cm)
  created <- suppressMessages(cm$custom_model_create(c(args, list(confirm = "true")), paths))
  expect_true(created$ok, info = created$message)
  expect_identical(created$data$model$historical_only_drivers, as.list(case$historical))
  expect_identical(created$data$model$future_required_drivers,
    as.list(setdiff(case$predictors, case$historical)))
  model <- readRDS(created$data$saved$rds)
  fit <- finnts:::custom_model_fit_impl(case$history[setdiff(names(case$history), "Target")],
    case$history$Target, model$definition, list(model_type = "local", recipe_id = "C1",
      date_type = "month", forecast_horizon = 2L, target_scale = "original"), TRUE)
  expect_equal(finnts:::custom_model_predict_impl(fit, case$future), case$expected)
  if (family == "unit_trend") {
    stages <- character()
    cm$finn_load_checked <- function(config, root) list(config = cfg, errors = character(),
      data = case$history)
    rejected <- suppressMessages(cm$custom_model_create(c(args, list(confirm = "true")), paths))
    expect_identical(rejected$status, "not_created")
    expect_true(length(stages) > 0L)
    expect_false("source" %in% stages)
    expect_length(cm$custom_model_versions(paths, "customer_growth"), 1L)
  }
  model <- historical_driver_model()
  panel <- historical_driver_panel()
  fit <- finnts:::custom_model_fit_impl(panel$history[setdiff(names(panel$history), "Target")],
    panel$history$Target, model$definition, fit$context[c("model_type", "recipe_id",
      "date_type", "forecast_horizon", "target_scale")], TRUE)
  expect_equal(finnts:::custom_model_predict_impl(fit, panel$future), c(1452, 1730.3))
  expect_error(finnts:::custom_model_predict_impl(fit, panel$future[c("Date", "Combo")]),
    "required predictors")
  bad <- panel$history
  bad$Units[[1]] <- Inf
  expect_error(finnts:::custom_model_fit_impl(bad[setdiff(names(bad), "Target")],
    bad$Target, model$definition, fit$context[c("model_type", "recipe_id", "date_type",
      "forecast_horizon", "target_scale")], TRUE), "finite")
})

test_that("ambiguous driver instructions return questions without saving or generating source", {
  paths <- common$finn_project_paths(withr::local_tempdir(), "Ambiguous")
  common$finn_ensure_project_dirs(paths)
  case <- historical_finance_case("customer_growth")
  cfg <- common$finn_merge_config(list(combo_variables = "Combo", target_variable = "Target",
    date_type = "month", forecast_horizon = 2, external_regressors = case$predictors,
    agent = list(llm = list(provider = "openai", model = "offline"))))
  common$finn_write_json(cfg, file.path(paths$dir, "config.json"))
  cm$finn_load_checked <- function(config, root) list(config = cfg, errors = character(),
    data = case$history)
  cm$finn_build_llm <- function(config) list()
  examples <- file.path(paths$dir, "examples.csv")
  utils::write.csv(custom_examples_table(historical_finance_examples(case)),
    examples, row.names = FALSE, na = "")
  stages <- character()
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      stages <<- c(stages, stage)
      if (stage != "contract") stop("Ambiguity must prevent source generation")
      proposal <- author_proposal()
      proposal$name <- NULL
      proposal$defaults_used <- character()
      proposal$historical_only_predictors <- character()
      proposal$questions <- list(list(kind = "business", topic = "business_context",
        question = "Should future customers be supplied or grown from history?"))
      proposal
    }, .package = "finnts")
  result <- suppressMessages(cm$custom_model_create(list(name = "ambiguous",
    instructions = "Use customers.", drivers = "Active_Customers",
    examples = examples, confirm = "true"), paths))
  expect_identical(result$status, "needs_input")
  expect_true(length(result$data$questions) > 0L)
  expect_true(length(stages) > 0L)
  expect_true(all(stages == "contract"))
  expect_length(list.files(common$finn_custom_models_dir(paths),
    recursive = TRUE, pattern = "\\.(rds|json)$"), 0L)
})
