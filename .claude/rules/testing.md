---
paths:
  - "tests/**/*.R"
  - ".github/workflows/*.yaml"
---

# Testing Rules

- Keep the CRAN profile deterministic. It must never call live LLM or foundation-model endpoints, even when credentials are present.
- Preserve every existing `testthat::skip_on_cran()` marker, including deterministic, integration, performance, and credential-security tests. Do not remove one solely because a test was optimized or does not use a network connection.
- Live tests must skip on CRAN before checking credentials or invoking a provider. Credentials must never override CRAN skipping.
- Non-CRAN runs automatically execute every live Agent, Chronos, TimeGPT/Nixtla, and TimesFM test whose provider credentials are available. Do not add separate opt-in flags.
- The R CMD check matrix supplies configured Agent, Nixtla, and Chronos credentials. Other provider tests run when their credentials are present; fork pull requests skip live tests because repository secrets are unavailable.
- Simulate CRAN skips with `R -q -e 'withr::with_envvar(c(NOT_CRAN = "false"), devtools::test())'`.
- Use `FINNTS_TEST_TIME_LIMIT_SECONDS` for a full-suite limit and `FINNTS_TEST_FILE_TIME_LIMIT_SECONDS` for a per-file limit. Keep the CRAN profile below ten minutes and use 90 seconds as the per-file budget for the restored `fit_resamples()` path.
- Do not repeat full `prep_data()` / `prep_models()` / `train_models()` pipelines only to manufacture downstream test input. Prefer deterministic artifacts, one directly finalized fit, or an immutable prepared template copied into an isolated test directory.
- `devtools::test()` uses `pkgload::load_all()`, which may not expose non-exported package helpers to PSOCK `foreach` workers as an installed package namespace does. Before adding a production `.export` for a missing helper, install the current source in a temporary library and reproduce the same call from the installed namespace. A test-only `.export` may be appropriate.
- Add or extend focused tests for the happy path, one or two edge cases, and the regression being fixed. Do not weaken or delete a failing test to make a change pass.