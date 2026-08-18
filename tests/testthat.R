library(testthat)
library(finnts)

started <- proc.time()[["elapsed"]]
results <- test_check("finnts")
elapsed <- proc.time()[["elapsed"]] - started
message(sprintf("finnts test suite completed in %.2f seconds", elapsed))

result_files <- vapply(results, function(result) result$file, character(1))
result_times <- vapply(results, function(result) result$real, numeric(1))
file_timings <- sort(tapply(result_times, result_files, sum), decreasing = TRUE)
message("finnts test file timings:")
for (file in names(file_timings)) {
	message(sprintf("  %s: %.2f seconds", file, file_timings[[file]]))
}

limit_text <- Sys.getenv("FINNTS_TEST_TIME_LIMIT_SECONDS")
if (nzchar(limit_text)) {
	limit <- suppressWarnings(as.numeric(limit_text))
	if (is.na(limit) || limit <= 0) {
		stop("FINNTS_TEST_TIME_LIMIT_SECONDS must be a positive number.", call. = FALSE)
	}
	if (elapsed > limit) {
		stop(
			sprintf(
				"finnts test suite exceeded its %.0f-second limit (%.2f seconds).",
				limit,
				elapsed
			),
			call. = FALSE
		)
	}
}

file_limit_text <- Sys.getenv("FINNTS_TEST_FILE_TIME_LIMIT_SECONDS")
if (nzchar(file_limit_text)) {
	file_limit <- suppressWarnings(as.numeric(file_limit_text))
	if (is.na(file_limit) || file_limit <= 0) {
		stop("FINNTS_TEST_FILE_TIME_LIMIT_SECONDS must be a positive number.", call. = FALSE)
	}
	slow_files <- file_timings[file_timings > file_limit]
	if (length(slow_files) > 0) {
		details <- paste(
			sprintf("%s (%.2f seconds)", names(slow_files), slow_files),
			collapse = ", "
		)
		stop(
			sprintf(
				"finnts test files exceeded the %.0f-second limit: %s.",
				file_limit,
				details
			),
			call. = FALSE
		)
	}
}
