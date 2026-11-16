# R CMD check --as-cran sets _R_CHECK_LIMIT_CORES_, which caps
# parallelly::availableCores() (and so finnts' pools) at one worker. Non-CRAN
# runs verify real multi-worker pools, so they lift the cap for the suite; the
# CRAN profile keeps it. Restored when the test run ends.
if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "false"), .local_envir = testthat::teardown_env())
}
