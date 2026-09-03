---
paths:
  - "DESCRIPTION"
  - "R/optional_dependencies.R"
  - "R/feature_selection.R"
  - "R/agent_summarize_models.R"
  - "R/forecast_time_series.R"
  - "R/train_models.R"
  - "R/timegpt_model.R"
  - "tests/testthat/test-vip-optional.R"
  - "tests/testthat/test-summarize-models-feature-selection.R"
  - "tests/testthat/test-timegpt.R"
  - ".github/workflows/R-CMD-check.yaml"
---

# Optional Dependency Rules

- Keep `vip (>= 0.5.0)` in `Suggests`. It comes from `https://bgreenwell.r-universe.dev` and requires R 4.1 or newer; FinnTS core must install, load, and pass a CRAN-style check without it.
- Feature selection may require `vip` and must fail early with actionable installation guidance when it is unavailable. Model summaries must retain non-importance sections without `vip`; only variable-importance output may be omitted.
- Keep `Boruta`, `corrr`, and `ranger` in `Suggests`. Declare `ranger` directly because Boruta 10.0 no longer installs it as a strong dependency.
- Use Boruta's exported `getImpRfZ` ranger adapter. Do not rely on Boruta's changing default importance provider.
- Preserve maintained-`vip` compatibility coverage and the CI check that runs without any feature-selection package.
- Keep `nixtlar` in `Suggests`. TimeGPT requires R 4.1 or newer, while FinnTS core must remain installable and loadable on R 4.0 without it.
- Selecting TimeGPT without `nixtlar` must report the missing engine dependency while leaving other models available.
- Do not move an optional engine or feature package into `Imports`, `Depends`, or `LinkingTo` without an explicit packaging decision and clean-library validation.
- When optional-package behavior changes, verify that `DESCRIPTION`, package checks, missing-package tests, and `.github/workflows/R-CMD-check.yaml` still describe the same support boundary.