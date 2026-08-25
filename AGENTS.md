# AGENTS.md — Instructions for coding agents working on this R package

## Project overview
- Package name: finnts
- Purpose: The Microsoft Finance Time Series Forecasting Framework, aka finnts or Finn, is an automated forecasting framework for producing financial forecasts.
- Primary users / workflows: Low code agent based solution for financial forecasting, with a focus on time series data. The package provides tools for data preprocessing, model training, and forecast generation, all designed to be user-friendly and accessible to both beginners and experienced practitioners in the field of financial forecasting.
- Non-goals: The package is not intended for high-frequency trading applications, and it does not include tools for data visualization or advanced statistical analysis beyond what is necessary for forecasting.

## Golden rule
Make changes that are **testable, documented, and R CMD check–clean**.
If you can't run tests/checks, say so and explain what would be run.

## Quickstart commands (copy/paste)

### Install dependencies
Prefer the repo's existing dependency workflow:
- If `renv.lock` exists:
  - `R -q -e 'install.packages("renv"); renv::restore()'`
- Otherwise:
  - `R -q -e 'install.packages("devtools"); devtools::install_deps(dependencies = TRUE)'`

### Optional feature selection dependencies
- `vip (>= 0.5.0)` is a suggested package from `https://bgreenwell.r-universe.dev` and requires R 4.1 or newer.
- Feature selection also requires the suggested `Boruta`, `corrr`, and `ranger` packages. Keep `ranger` declared directly because Boruta 10.0 no longer installs it as a strong dependency.
- Use Boruta's `getImpRfZ` ranger adapter to preserve FinnTS behavior across Boruta 10.0 and later; do not rely on Boruta's changing default importance provider.
- FinnTS must install, load, and pass its CRAN-style check without `vip`; never move it to `Imports`, `Depends`, or `LinkingTo` without an explicit packaging decision.
- Feature selection may require `vip` and must fail early with actionable installation guidance when it is unavailable.
- Model summaries must retain non-importance sections without `vip`; only variable-importance output may be omitted.
- Keep both maintained-`vip` compatibility coverage and the explicit CI check without any feature-selection packages.

### Dev loop
- Document (roxygen): `R -q -e 'devtools::document()'`
- Run tests: `R -q -e 'devtools::test()'`
- Full check: `R -q -e 'devtools::check()'`

### Parallel test caveat
- `devtools::test()` runs against `pkgload::load_all()`, which does not always expose non-exported package helpers to PSOCK `foreach` workers like an installed package namespace does.
- A test-only `.export` may therefore be needed even when production code does not need one. Before adding `.export` to production for a missing-helper error, install the current source into a temporary library and reproduce the same `foreach` call from the installed namespace.

### Test profiles and runtime
- The CRAN profile is deterministic and must never call live LLM or foundation-model endpoints, even when credentials are present.
- Simulate CRAN skips with `R -q -e 'withr::with_envvar(c(NOT_CRAN = "false"), devtools::test())'`.
- Optional runtime gates are `FINNTS_TEST_TIME_LIMIT_SECONDS` for the full suite and `FINNTS_TEST_FILE_TIME_LIMIT_SECONDS` per file; keep the CRAN profile below ten minutes and use 90 seconds as the per-file budget for the restored `fit_resamples()` path.
- Non-CRAN runs automatically execute every live Agent, Chronos, TimeGPT/Nixtla, and TimesFM test when that provider's credentials are available; there are no additional opt-in flags.
- Preserve every existing `testthat::skip_on_cran()` marker, including deterministic, integration, performance, and credential-security tests. Do not remove one solely because a test was optimized or does not use a network connection.
- Live tests must skip on CRAN before checking credentials or invoking a provider. Credentials must never override CRAN skipping.
- The existing R CMD check matrix supplies its configured Agent, Nixtla, and Chronos credentials, so those live tests run automatically. Other provider tests run when their credentials are present in the environment. Fork pull requests cannot receive repository secrets and therefore skip live tests.
- Do not repeat full `prep_data()` / `prep_models()` / `train_models()` pipelines only to manufacture downstream test input. Prefer deterministic artifacts, one directly finalized fit, or an immutable prepared template copied into an isolated test directory.

### Multistep validation
- Automatic MARS tuning must exclude `prune_method = "cv"`; explicit multistep CV pruning must supply a bounded `nfold` value.
- Cover all supported `date_type` values and assert exact horizon-to-submodel routing, training rows, lag-feature eligibility, and forecast row identity.
- The daily multistep matrix is part of the standard test suite and must pass after changing lag generation, feature selection, multistep fitting, or prediction.
- Preserve both daily paths: all six adapters with explicit lags/outlier cleaning and GLMnet with automatic lags/raw data.

### Style / lint
- Format: `R -q -e 'if (requireNamespace("styler", quietly=TRUE)) styler::style_pkg()'`
- Lint:   `R -q -e 'if (requireNamespace("lintr", quietly=TRUE)) lintr::lint_package()'`

### Optional docs site (if pkgdown is used)
- `R -q -e 'if (requireNamespace("pkgdown", quietly=TRUE)) pkgdown::build_site()'`

## Repo layout (R package conventions)
- `R/` — implementation (functions, classes, methods)
- `man/` — generated `.Rd` docs (DO NOT hand-edit; generated by roxygen2)
- `tests/testthat/` — unit tests
- `vignettes/` — long-form docs, tutorials, examples
- `DESCRIPTION` — dependencies, metadata, version
- `NAMESPACE` — generated (DO NOT hand-edit if using roxygen2)
- `NEWS.md` — user-facing change log

## Feature implementation workflow (definition of done)
When adding or changing user-facing behavior:
1. **Design**: state what changes for the user + any API changes (args, return types, errors).
2. **Implement**: update code in `R/` (keep diff minimal, avoid unrelated refactors).
3. **Tests**: add/extend tests in `tests/testthat/` for:
   - happy path
   - 1–2 edge cases
   - regression test for the bug/feature request (if applicable)
4. **Docs**:
   - update roxygen comments (`@param`, `@return`, `@examples`)
   - run `devtools::document()` so `man/*.Rd` stays in sync
   - update vignettes if the change affects them or if you added a significant new feature that users should know about
5. **Quality gates**:
   - `devtools::test()`
   - `devtools::check()`
   - style/lint (if configured for the repo)
6. **Changelog**:
   - bump version in `DESCRIPTION` and `NEWS.md`
   - if behavior changed, add a bullet to `NEWS.md`

## Coding conventions
- Prefer small, composable functions.
- Keep exported functions stable; introduce breaking changes only as a last resort, **ask before adding**, with an explicit note in NEWS.
- Agent workflows accept one `llm` Chat template and create a fresh, empty-history session for every graph that can access the LLM, including EDA, forecast update, each time-series combo, and Q&A. Never mutate the template. For parallel forecasts, let `foreach` serialize the template and use the public `Chat$clone(deep = TRUE)` method inside each series workflow after it reaches the worker. A series session may persist across its own iterations but must never be shared across series. Parallel workflows require ellmer 0.4.0 or later in the main process and every worker; do not call internal ellmer constructors or mutate provider credential fields.
- Prefer base R and packages already declared in `Depends`, `Imports`, or `Suggests` before adding a dependency.
- A new package dependency is allowed only for a concrete new feature when existing dependencies and a small, maintainable custom implementation are inadequate, unsafe, or would recreate substantial mature functionality.
- Do not add dependencies for bug fixes, documentation, formatting, developer convenience, or trivial helpers. Do not vendor third-party source code.
- A dependency-changing pull request must document the alternatives considered, why the package is necessary, maintenance/security/license implications, why it belongs in `Imports` or `Suggests`, and any required version constraint. It must also pass a clean-library installation and `devtools::check()`.
- Comments:
  - Use short `# description` style comments. Do **not** use decorative separator lines (e.g. `# ----------`, `# =========`, `# ****`).
  - Keep comments concise and descriptive, matching the existing style throughout the package.
- Errors:
  - be consistent (class/messages), include actionable messages.
- Use `stop()` for user-facing errors, `warning()` for recoverable issues, and `message()` for informational output.
- Performance:
  - avoid unnecessary copies in tight loops; vectorize where it improves clarity/perf.

## Boundaries / safety rails
- Do **not** commit secrets (tokens, keys), local paths, or machine-specific config.
- Do **not** edit `NAMESPACE` or `man/*.Rd` by hand if roxygen2 is in use.
- Do **not** add package dependencies outside the feature-only dependency policy above.
- Do **not** silence failing tests by deleting or weakening them; fix the underlying issue.
- Do **not** create code/functions that delete files.

## When requirements are ambiguous
Before coding, propose:
- 1–2 plausible interpretations,
- the tradeoffs,
- and which files/functions you expect to touch.
Then proceed with the most conservative/backward-compatible option unless told otherwise.
