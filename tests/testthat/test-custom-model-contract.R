# Return inert monthly model arguments for contract tests. Source strings must
# never run; callers may alter fields without sharing mutable environments.
custom_model_contract_args <- function() {
  list(
    name = "seasonal_growth",
    instructions = "Apply the prescribed seasonal growth rule.",
    interpretation = "Use prior-season values with fixed historical growth.",
    model_type = "local",
    source = c(
      fit = "function(data, context, parameters) { stop('must not execute') }",
      predict = "function(object, new_data, context) { stop('must not execute') }"
    ),
    requirements = list(
      predictors = c("Date", "Combo"),
      recipes = "R1",
      target_scale = "original",
      date_types = "month",
      forecast_horizon = 1:12,
      missing_data = "Reject missing required history."
    ),
    fixed_parameters = list(window = 3L),
    packages = "stats"
  )
}

test_that("custom definitions describe local and global logic without execution", {
  for (model_type in list("local", "global", c("local", "global"))) {
    args <- custom_model_contract_args()
    args$model_type <- model_type
    definition <- do.call(new_custom_model_definition, args)

    expect_s3_class(definition, "finnts_custom_model_definition")
    expect_setequal(definition$model_type, model_type)
    expect_identical(definition$source, args$source)
    expect_match(definition$version_id, "^[a-f0-9]{64}$")
  }
})

test_that("custom definitions reject invalid schema and authoring metadata", {
  definition <- do.call(new_custom_model_definition, custom_model_contract_args())
  for (field in c("schema_version", "name", "source", "requirements")) {
    invalid <- definition
    invalid[[field]] <- NULL
    expect_error(validate_custom_model_definition(invalid), "Custom model", info = field)
  }
  for (field in c("scope", "status", "approved", "tuning", "refinement", "llm")) {
    invalid <- definition
    invalid[[field]] <- "unsupported"
    expect_error(validate_custom_model_definition(invalid), "unsupported fields", info = field)
  }
  for (version in list(2L, NA_integer_, "1", c(1L, 1L))) {
    invalid <- definition
    invalid$schema_version <- version
    expect_error(validate_custom_model_definition(invalid), "schema_version")
  }
  for (origin in c("ml_defined", "agent_refined")) {
    invalid <- definition
    invalid$origin <- origin
    expect_error(validate_custom_model_definition(invalid), "origin")
  }
  for (model_type in list(NULL, character(), "both", "single", c("local", "local"), NA_character_)) {
    invalid <- definition
    invalid["model_type"] <- list(model_type)
    expect_error(validate_custom_model_definition(invalid), "model_type")
  }
  invalid <- definition
  names(invalid)[[2]] <- names(invalid)[[1]]
  expect_error(validate_custom_model_definition(invalid), "unique names")
  invalid <- definition
  invalid$interpretation <- " "
  expect_error(validate_custom_model_definition(invalid), "interpretation")
  invalid$interpretation <- NA_character_
  expect_error(validate_custom_model_definition(invalid), "interpretation")
})

test_that("custom names and lineage cannot collide with built-ins or artifact identities", {
  args <- custom_model_contract_args()
  for (name in c(list_models(), "All-Data", "Best-Model", "all", "local",
    "global", "bad--name", "bad---name", "../model", "bad/name", "bad name", "")) {
    args$name <- name
    expect_error(do.call(new_custom_model_definition, args), "name", info = name)
  }
  args <- custom_model_contract_args()
  args$parent_version <- paste(rep("a", 64), collapse = "")
  expect_identical(do.call(new_custom_model_definition, args)$parent_version, args$parent_version)
  for (parent in list("latest", "", NA_character_, rep("a", 64))) {
    args$parent_version <- parent
    expect_error(do.call(new_custom_model_definition, args), "parent_version")
  }
})

test_that("data requirements are explicit and finite", {
  args <- custom_model_contract_args()
  cases <- list(
    predictors = list(c("Date", "Date"), c("Date", NA_character_)),
    recipes = list(character(), "R3", c("R1", "R1")),
    target_scale = list("unknown", c("original", "prepared")),
    date_types = list("hour", character()),
    forecast_horizon = list(0, -1, 1.5, Inf, NA_real_, c(1, 1)),
    missing_data = list("", NULL)
  )
  for (field in names(cases)) {
    for (value in cases[[field]]) {
      invalid <- args
      invalid$requirements[field] <- list(value)
      expect_error(do.call(new_custom_model_definition, invalid), field, info = field)
    }
  }
  args$requirements$predictors <- character()
  args$requirements$recipes <- c("R1", "R2")
  args$requirements$date_types <- c("year", "quarter", "month", "week", "day")
  expect_no_error(do.call(new_custom_model_definition, args))
  args$requirements$extra <- TRUE
  expect_error(do.call(new_custom_model_definition, args), "requirements")
})

test_that("source accepts function text only and never top-level execution", {
  args <- custom_model_contract_args()
  for (source in c("stop('top-level')", "fit <- function() 1", "function() 1; stop('top-level')",
    "{ function() 1 }", "function(", "")) {
    args$source[["fit"]] <- source
    expect_error(do.call(new_custom_model_definition, args), "source")
  }
  args <- custom_model_contract_args()
  args$source <- args$source["fit"]
  expect_error(do.call(new_custom_model_definition, args), "source")
  args$source <- c(fit = "function() 1", fit = "function() 2", predict = "function() 1")
  expect_error(do.call(new_custom_model_definition, args), "unique names")
  args$source <- c(fit = "function() 1", predict = "function() 1", "bad--helper" = "function() 1")
  expect_error(do.call(new_custom_model_definition, args), "source")
})

test_that("executable objects and attributes are rejected without dispatch", {
  args <- custom_model_contract_args()
  for (value in list(base::identity, new.env(parent = emptyenv()), quote(stop("never")),
    expression(stop("never")), structure(1, class = "untrusted"),
    structure(1, payload = new.env(parent = emptyenv())))) {
    args$fixed_parameters <- list(nested = list(value))
    expect_error(do.call(new_custom_model_definition, args), "passive")
  }
  args$fixed_parameters <- list(window = 3, window = 4)
  expect_error(do.call(new_custom_model_definition, args), "unique names")
  args$fixed_parameters <- list(window = Inf)
  expect_error(do.call(new_custom_model_definition, args), "finite")
  args$fixed_parameters <- list(window = NA_real_)
  expect_error(do.call(new_custom_model_definition, args), "finite")
})

test_that("only existing package declarations are accepted without installation", {
  args <- custom_model_contract_args()
  args$packages <- c("base", "stats", "MASS", "finnts", "dplyr", "ellmer")
  expect_no_error(do.call(new_custom_model_definition, args))
  metadata <- utils::packageDescription("finnts", fields = c("Depends", "Imports", "Suggests"))
  declared <- trimws(unlist(strsplit(paste(unlist(metadata), collapse = ","), ",")))
  declared <- trimws(sub("\\s*\\(.*\\)$", "", declared))
  args$packages <- setdiff(unique(declared), "R")
  expect_no_error(do.call(new_custom_model_definition, args))
  for (packages in list("not_a_finnts_dependency", "", NA_character_, c("stats", "stats"))) {
    args$packages <- packages
    expect_error(do.call(new_custom_model_definition, args), "packages")
  }
})

test_that("named metadata and declared sets have canonical version identities", {
  args <- custom_model_contract_args()
  args$model_type <- c("local", "global")
  args$packages <- c("stats", "dplyr")
  args$fixed_parameters <- list(window = 3L, settings = list(baseline = 12L, weight = 1))
  definition <- do.call(new_custom_model_definition, args)
  reordered <- definition[rev(seq_along(definition))]
  reordered$model_type <- rev(reordered$model_type)
  reordered$packages <- rev(reordered$packages)
  reordered$source <- rev(reordered$source)
  reordered$requirements <- rev(reordered$requirements)
  reordered$requirements$predictors <- rev(reordered$requirements$predictors)
  reordered$requirements$forecast_horizon <- rev(reordered$requirements$forecast_horizon)
  reordered$fixed_parameters <- rev(reordered$fixed_parameters)
  reordered$fixed_parameters$settings <- rev(reordered$fixed_parameters$settings)
  expect_identical(custom_model_definition_digest(reordered), definition$version_id)
  expect_identical(custom_model_definition_digest(unclass(definition)), definition$version_id)

  reordered <- definition
  reordered$created_at <- "2026-09-21T00:00:00Z"
  expect_identical(custom_model_definition_digest(reordered), definition$version_id)
})

test_that("semantic changes and ordered sequences produce distinct identities", {
  args <- custom_model_contract_args()
  definition <- do.call(new_custom_model_definition, args)
  changes <- list(
    instructions = "A different human instruction.",
    interpretation = "A different confirmed interpretation.",
    model_type = "global",
    packages = c("stats", "dplyr"),
    fixed_parameters = list(window = 4L),
    source = c(args$source, helper = "function(value) value"),
    parent_version = paste(rep("b", 64), collapse = "")
  )
  for (field in names(changes)) {
    changed <- definition
    changed[[field]] <- changes[[field]]
    expect_false(identical(custom_model_definition_digest(changed), definition$version_id), info = field)
  }
  changed <- definition
  changed$requirements$target_scale <- "prepared"
  expect_false(identical(custom_model_definition_digest(changed), definition$version_id))
  changed <- definition
  changed$source[["fit"]] <- paste0(changed$source[["fit"]], " ")
  expect_false(identical(custom_model_definition_digest(changed), definition$version_id))

  args$fixed_parameters <- list(weights = c(first = 1, second = 2), steps = list("lag", "mean"))
  ordered <- do.call(new_custom_model_definition, args)
  reversed <- ordered
  reversed$fixed_parameters$weights <- rev(reversed$fixed_parameters$weights)
  expect_false(identical(custom_model_definition_digest(reversed), ordered$version_id))
  reversed <- ordered
  reversed$fixed_parameters$steps <- rev(reversed$fixed_parameters$steps)
  expect_false(identical(custom_model_definition_digest(reversed), ordered$version_id))
})

test_that("UTF-8 normalization preserves text semantics across serialization", {
  args <- custom_model_contract_args()
  args$interpretation <- "Prescribed caf\u00e9 growth."
  utf8 <- do.call(new_custom_model_definition, args)
  args$interpretation <- iconv(args$interpretation, from = "UTF-8", to = "latin1")
  latin1 <- do.call(new_custom_model_definition, args)
  expect_identical(latin1$version_id, utf8$version_id)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(utf8, path)
  restored <- readRDS(path)
  expect_identical(restored, utf8)
  expect_identical(custom_model_definition_digest(restored), utf8$version_id)
})

test_that("contract operations neither execute source nor mutate their inputs", {
  args <- custom_model_contract_args()
  args$source <- c(
    fit = "function(data, context, parameters) { options(finnts_contract_executed = TRUE); stop('never') }",
    predict = "function(object, new_data, context) { stop('never') }"
  )
  original <- serialize(args, NULL)
  withr::local_options(list(finnts_contract_executed = FALSE))
  definition <- do.call(new_custom_model_definition, args)
  expect_true(validate_custom_model_definition(definition))
  expect_identical(custom_model_definition_digest(definition), definition$version_id)
  expect_identical(serialize(args, NULL), original)
  expect_false(getOption("finnts_contract_executed"))
  expect_false(any(c("approved", "status", "workflows", "llm") %in% names(definition)))
})

test_that("stored hashes are data and cannot confer approval", {
  definition <- do.call(new_custom_model_definition, custom_model_contract_args())
  invalid <- definition
  invalid$version_id <- "latest"
  expect_error(validate_custom_model_definition(invalid), "version_id")
  modified <- definition
  modified$version_id <- paste(rep("0", 64), collapse = "")
  expect_true(validate_custom_model_definition(modified))
  expect_identical(custom_model_definition_digest(modified), definition$version_id)
  expect_false(identical(custom_model_definition_digest(modified), modified$version_id))
  expect_false(inherits(definition, "finnts_custom_model"))
})