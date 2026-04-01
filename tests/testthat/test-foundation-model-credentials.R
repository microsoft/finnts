# tests/testthat/test-foundation-model-credentials.R
#
# Security tests: ensure no API URLs or keys are stored in foundation model
# fit objects or workflows that get saved to disk.

skip_on_cran()

# Fake credentials used to detect leaks in serialized objects
fake_chronos_url <- "https://fake-chronos-test-endpoint.example.com/api/v1"
fake_chronos_token <- "fake_chronos_token_abc123xyz789_DONOTLEAK"
fake_nixtla_url <- "https://fake-nixtla-test-endpoint.example.com/"
fake_nixtla_key <- "fake_nixtla_key_def456uvw012_DONOTLEAK"

# Helper: recursively search all character values in a nested list/object
contains_credential <- function(obj, patterns) {
  obj_str <- paste(utils::capture.output(dput(obj)), collapse = "\n")
  any(vapply(patterns, function(p) grepl(p, obj_str, fixed = TRUE), logical(1)))
}

# Helper: check serialized bytes for credential strings
serialized_contains_credential <- function(obj, patterns) {
  raw_bytes <- serialize(obj, connection = NULL)
  any(vapply(patterns, function(p) {
    p_bytes <- charToRaw(p)
    p_len <- length(p_bytes)
    if (p_len == 0L || p_len > length(raw_bytes)) return(FALSE)
    for (i in seq_len(length(raw_bytes) - p_len + 1L)) {
      if (all(raw_bytes[i:(i + p_len - 1L)] == p_bytes)) return(TRUE)
    }
    FALSE
  }, logical(1)))
}

# Chronos credential patterns
chronos_patterns <- c(
  fake_chronos_url, fake_chronos_token,
  "CHRONOS_API_URL", "CHRONOS_API_TOKEN"
)

# TimeGPT credential patterns
timegpt_patterns <- c(
  fake_nixtla_url, fake_nixtla_key,
  "NIXTLA_BASE_URL", "NIXTLA_API_KEY"
)

all_patterns <- c(chronos_patterns, timegpt_patterns)

# Sample training data used across tests
sample_x <- data.frame(
  Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
  Combo = "1"
)
sample_y <- seq_len(24) * 1.5

# Sanity check: withr::local_envvar actually makes credentials visible

test_that("local_envvar makes fake credentials visible via Sys.getenv", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token,
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  expect_equal(Sys.getenv("CHRONOS_API_URL"), fake_chronos_url)
  expect_equal(Sys.getenv("CHRONOS_API_TOKEN"), fake_chronos_token)
  expect_equal(Sys.getenv("NIXTLA_BASE_URL"), fake_nixtla_url)
  expect_equal(Sys.getenv("NIXTLA_API_KEY"), fake_nixtla_key)
})

# Chronos 2

test_that("Chronos 2 fit object does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  fit <- chronos2_model_fit_impl(
    sample_x, sample_y,
    forecast_horizon = 3, frequency = 12
  )

  expect_false(
    contains_credential(fit, chronos_patterns),
    info = "Chronos 2 fit object must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(fit, chronos_patterns),
    info = "Serialized Chronos 2 fit object must not contain API credentials"
  )
})

test_that("Chronos 2 workflow fit does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  expect_false(
    contains_credential(wf_fit, chronos_patterns),
    info = "Chronos 2 workflow fit must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(wf_fit, chronos_patterns),
    info = "Serialized Chronos 2 workflow fit must not contain API credentials"
  )
})

# Chronos Bolt Base

test_that("Chronos Bolt Base fit object does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  fit <- chronos_bolt_base_model_fit_impl(
    sample_x, sample_y,
    forecast_horizon = 3, frequency = 12
  )

  expect_false(
    contains_credential(fit, chronos_patterns),
    info = "Chronos Bolt Base fit object must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(fit, chronos_patterns),
    info = "Serialized Chronos Bolt Base fit object must not contain API credentials"
  )
})

test_that("Chronos Bolt Base workflow fit does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos_bolt_base_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos_bolt_base_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  expect_false(
    contains_credential(wf_fit, chronos_patterns),
    info = "Chronos Bolt Base workflow fit must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(wf_fit, chronos_patterns),
    info = "Serialized Chronos Bolt Base workflow fit must not contain API credentials"
  )
})

# Chronos Bolt Tiny

test_that("Chronos Bolt Tiny fit object does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  fit <- chronos_bolt_tiny_model_fit_impl(
    sample_x, sample_y,
    forecast_horizon = 3, frequency = 12
  )

  expect_false(
    contains_credential(fit, chronos_patterns),
    info = "Chronos Bolt Tiny fit object must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(fit, chronos_patterns),
    info = "Serialized Chronos Bolt Tiny fit object must not contain API credentials"
  )
})

test_that("Chronos Bolt Tiny workflow fit does not contain API credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos_bolt_tiny_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos_bolt_tiny_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  expect_false(
    contains_credential(wf_fit, chronos_patterns),
    info = "Chronos Bolt Tiny workflow fit must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(wf_fit, chronos_patterns),
    info = "Serialized Chronos Bolt Tiny workflow fit must not contain API credentials"
  )
})

# TimeGPT

test_that("TimeGPT fit object does not contain API credentials", {
  withr::local_envvar(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  fit <- timegpt_model_fit_impl(
    sample_x, sample_y,
    forecast_horizon = 3, frequency = 12,
    finetune_steps = 0, finetune_depth = 1
  )

  expect_false(
    contains_credential(fit, timegpt_patterns),
    info = "TimeGPT fit object must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(fit, timegpt_patterns),
    info = "Serialized TimeGPT fit object must not contain API credentials"
  )
})

test_that("TimeGPT workflow fit does not contain API credentials", {
  withr::local_envvar(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- timegpt_model(
    forecast_horizon = 3, frequency = 12,
    finetune_steps = 0, finetune_depth = 1
  ) %>%
    parsnip::set_engine("timegpt_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  expect_false(
    contains_credential(wf_fit, timegpt_patterns),
    info = "TimeGPT workflow fit must not contain API credentials"
  )
  expect_false(
    serialized_contains_credential(wf_fit, timegpt_patterns),
    info = "Serialized TimeGPT workflow fit must not contain API credentials"
  )
})

# Cross-model: no credential from ANY foundation model leaks

test_that("No foundation model fit object contains credentials from any provider", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token,
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  fits <- list(
    chronos2 = chronos2_model_fit_impl(
      sample_x, sample_y,
      forecast_horizon = 3, frequency = 12
    ),
    chronos_bolt_base = chronos_bolt_base_model_fit_impl(
      sample_x, sample_y,
      forecast_horizon = 3, frequency = 12
    ),
    chronos_bolt_tiny = chronos_bolt_tiny_model_fit_impl(
      sample_x, sample_y,
      forecast_horizon = 3, frequency = 12
    ),
    timegpt = timegpt_model_fit_impl(
      sample_x, sample_y,
      forecast_horizon = 3, frequency = 12,
      finetune_steps = 0, finetune_depth = 1
    )
  )

  for (model_name in names(fits)) {
    expect_false(
      contains_credential(fits[[model_name]], all_patterns),
      info = paste(model_name, "fit object must not contain any provider credentials")
    )
    expect_false(
      serialized_contains_credential(fits[[model_name]], all_patterns),
      info = paste("Serialized", model_name, "fit must not contain any provider credentials")
    )
  }
})

# Helper: scan raw file bytes on disk for credential strings
file_contains_credential <- function(path, patterns) {
  raw_bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  any(vapply(patterns, function(p) {
    p_bytes <- charToRaw(p)
    p_len <- length(p_bytes)
    if (p_len == 0L || p_len > length(raw_bytes)) return(FALSE)
    for (i in seq_len(length(raw_bytes) - p_len + 1L)) {
      if (all(raw_bytes[i:(i + p_len - 1L)] == p_bytes)) return(TRUE)
    }
    FALSE
  }, logical(1)))
}

# Save-to-disk round-trip: saveRDS → readRDS → scan file bytes

test_that("Chronos 2 workflow fit saved to disk does not leak credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wf_fit, tmp)

  expect_false(
    file_contains_credential(tmp, chronos_patterns),
    info = "Chronos 2 RDS file on disk must not contain API credentials"
  )

  reloaded <- readRDS(tmp)
  expect_false(
    contains_credential(reloaded, chronos_patterns),
    info = "Reloaded Chronos 2 workflow must not contain API credentials"
  )
})

test_that("Chronos Bolt Base workflow fit saved to disk does not leak credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos_bolt_base_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos_bolt_base_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wf_fit, tmp)

  expect_false(
    file_contains_credential(tmp, chronos_patterns),
    info = "Chronos Bolt Base RDS file on disk must not contain API credentials"
  )

  reloaded <- readRDS(tmp)
  expect_false(
    contains_credential(reloaded, chronos_patterns),
    info = "Reloaded Chronos Bolt Base workflow must not contain API credentials"
  )
})

test_that("Chronos Bolt Tiny workflow fit saved to disk does not leak credentials", {
  withr::local_envvar(
    CHRONOS_API_URL = fake_chronos_url,
    CHRONOS_API_TOKEN = fake_chronos_token
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos_bolt_tiny_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos_bolt_tiny_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wf_fit, tmp)

  expect_false(
    file_contains_credential(tmp, chronos_patterns),
    info = "Chronos Bolt Tiny RDS file on disk must not contain API credentials"
  )

  reloaded <- readRDS(tmp)
  expect_false(
    contains_credential(reloaded, chronos_patterns),
    info = "Reloaded Chronos Bolt Tiny workflow must not contain API credentials"
  )
})

test_that("TimeGPT workflow fit saved to disk does not leak credentials", {
  withr::local_envvar(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5,
    temperature_original = rnorm(24, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- timegpt_model(
    forecast_horizon = 3, frequency = 12,
    finetune_steps = 0, finetune_depth = 1
  ) %>%
    parsnip::set_engine("timegpt_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wf_fit, tmp)

  expect_false(
    file_contains_credential(tmp, timegpt_patterns),
    info = "TimeGPT RDS file on disk must not contain API credentials"
  )

  reloaded <- readRDS(tmp)
  expect_false(
    contains_credential(reloaded, timegpt_patterns),
    info = "Reloaded TimeGPT workflow must not contain API credentials"
  )
})

# R options: credentials set via options() instead of env vars

test_that("TimeGPT fit does not leak credentials set via R options", {
  withr::local_options(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )
  withr::local_envvar(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  fit <- timegpt_model_fit_impl(
    sample_x, sample_y,
    forecast_horizon = 3, frequency = 12,
    finetune_steps = 0, finetune_depth = 1
  )

  expect_false(
    contains_credential(fit, timegpt_patterns),
    info = "TimeGPT fit must not leak credentials set via R options"
  )
  expect_false(
    serialized_contains_credential(fit, timegpt_patterns),
    info = "Serialized TimeGPT fit must not leak credentials set via R options"
  )
})

test_that("TimeGPT workflow fit does not leak credentials set via R options", {
  withr::local_options(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )
  withr::local_envvar(
    NIXTLA_BASE_URL = fake_nixtla_url,
    NIXTLA_API_KEY = fake_nixtla_key
  )

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1",
    Target = seq_len(24) * 1.5
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- timegpt_model(
    forecast_horizon = 3, frequency = 12,
    finetune_steps = 0, finetune_depth = 1
  ) %>%
    parsnip::set_engine("timegpt_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wf_fit, tmp)

  expect_false(
    file_contains_credential(tmp, timegpt_patterns),
    info = "TimeGPT RDS file must not leak credentials set via R options"
  )
})
