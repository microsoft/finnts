# Small provider-visible examples have independently specified expected values.
# The second is a zero-valued edge case; neither oracle executes model source.
author_examples <- function() {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")), Combo = "example", Target = c(10, 20))
  request <- data.frame(Date = as.Date("2020-03-01"), Combo = "example")
  expected <- request
  expected$.pred <- 15
  first <- list(
    name = "ordinary", model_type = "local", history = history, new_data = request, expected = expected, tolerance = 1e-08,
    edge_case = FALSE, rationale = "(10 + 20) / 2 = 15", outcome = "predictions", expected_error = NULL
  )
  second <- first
  second$name <- "zero"
  second$history$Target <- c(0, 0)
  second$expected$.pred <- 0
  second$edge_case <- TRUE
  second$rationale <- "The mean of zeros is zero."
  list(first, second)
}

# Return a scripted proposal and source with no provider, session or credentials.
author_proposal <- function() {
  list(
    questions = list(), name = "human_mean", interpretation = "Use the mean of historical targets.", parameters_json = "{}",
    parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
    error_policy = list(), validation_properties = character()
  )
}

# Return fixed source text; tests deliberately replace entries for repair cases.
author_source <- function() {
  list(
    status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))",
    helpers = list()
  )
}

# Convert internal validation-example lists into the caller table accepted by
# create_custom_model(). Each element needs name, history, new_data and either
# expected or expected_error; a Combo series column is kept only when an
# example spans two or more series, so local examples stay series-free.
custom_examples_table <- function(examples, series = "Combo") {
  global <- any(vapply(examples, function(example) length(unique(example$history$Combo)) > 1L, logical(1)))
  rows <- lapply(examples, function(example) {
    history <- example$history
    future <- example$new_data
    drivers <- setdiff(names(history), c("Date", "Combo", "Target"))
    expected <- rep(NA_real_, nrow(future))
    if (!is.null(example$expected)) {
      key <- function(table) paste(format(table$Date), table$Combo)
      expected <- example$expected$.pred[match(key(future), key(example$expected))]
    }
    table <- rbind(
      data.frame(example = example$name, Combo = history$Combo, Date = history$Date, Target = history$Target,
        expected = NA_real_, stringsAsFactors = FALSE),
      data.frame(example = example$name, Combo = future$Combo, Date = future$Date, Target = NA_real_,
        expected = expected, stringsAsFactors = FALSE)
    )
    for (driver in drivers) table[[driver]] <- c(history[[driver]], future[[driver]])
    table$edge_case <- isTRUE(example$edge_case)
    table$expected_error <- if (is.null(example$expected_error)) NA_character_ else
      paste0(example$expected_error$phase, ":", example$expected_error$code)
    table$how <- example$rationale %||% "Independent arithmetic."
    table$tolerance <- example$tolerance %||% 1e-08
    table
  })
  table <- dplyr::bind_rows(rows)
  if (all(is.na(table$expected_error))) table$expected_error <- NULL
  if (global) names(table)[names(table) == "Combo"] <- series else table$Combo <- NULL
  table
}

# Simulate an interactive console for manual approval. `respond` receives the
# review or question request and returns the typed answer ("yes", "no", an
# edit list or named question answers); responses use the production parser.
local_custom_console <- function(respond = function(request) "yes", .env = parent.frame()) {
  withr::local_options(rlang_interactive = TRUE, .local_envir = .env)
  testthat::local_mocked_bindings(
    custom_author_interact = function(stage, prompt, identity = NULL, review = list(), questions = NULL) {
      request <- list(stage = stage, prompt = prompt, identity = identity, review = review, questions = questions)
      custom_author_response(request, respond(request))
    },
    .package = "finnts", .env = .env
  )
}
