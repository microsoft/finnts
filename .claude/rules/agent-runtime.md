---
paths:
  - "R/agent_*.R"
  - "R/run_info.R"
  - "R/input_checks.R"
  - "R/prep_data.R"
  - "R/prep_models.R"
  - "R/read_write_data.R"
  - "tests/testthat/test-agent*.R"
  - "tests/testthat/test-finalize_run.R"
  - "tests/testthat/test-combo-normalization.R"
  - "tests/testthat/test-prep_models.R"
---

# Agent Runtime Rules

- Treat `agent_info$llm` as an immutable Chat template. Create a fresh, empty-history session for every LLM-capable graph: EDA, forecast update, each time-series combo, and Q&A.
- For parallel forecasts, let `foreach` serialize the template and call the public `Chat$clone(deep = TRUE)` method inside each series workflow after dispatch. A session may persist within one series but must never be shared across series. Require ellmer 0.4.0 or later in the main process and workers; do not call internal ellmer constructors or mutate provider credential fields.
- Compare duplicate runs by effective settings, not raw text. Resolve `NULL` defaults and canonicalize order-insensitive multipart models, regressors, recipes, lags, rolling windows, and seasonal periods while preserving raw run-log values.
- Supply custom `seasonal_period` values through `prep_models()` only. Accept one to three unique finite numeric values greater than 1, pass them to `stlm-arima`, `stlm-ets`, and `tbats`, and log a default `NULL` as `NA`, not as resolved periods. Every cadence default must exceed 1; yearly defaults are `c(2, 3)`.
- Validate `reason_inputs()` seasonal-period proposals before submission. Invalid recommendations use the typed LLM retry path; after retries are exhausted, gracefully abort optimization and preserve the existing best forecast. A legacy invalid period replay warns once and falls back to cadence defaults without weakening validation or silently dropping values.
- Validate every LLM-controlled run setting before submission. Expected proposal and search-exhaustion failures use typed conditions, retry from the same in-memory run-history snapshot, and may gracefully finalize only after retries are exhausted. Provider, storage, data, artifact, training, and unexpected failures remain hard errors.
- Keep current-version optimization history separate from earlier-version replay context. Only current-version rows control run counts, best metrics, duplicate checks, and change budgets. Do not impose a separate cap on external-regressor configurations; their exploration is bounded by the workflow's overall `max_iter`. Canonicalize order-insensitive lag, rolling-window, and seasonal-period configurations before counting their budgets; defaults and previously tested configurations remain allowed after the new-configuration budget is exhausted. Reload both history views exactly once after each successfully logged iteration.
- Keep `execute_node()` storage-free. `finalize_run()` owns exhausted-reasoning artifact decisions. Local workflows use one exact best-run file read without wildcard listing. The single global workflow may list best-run files once, preserve global results when present, continue to enabled local models when no global result exists, and otherwise fail.
- Propagate every best-run blob-listing provider error, including post-write verification errors. Never reinterpret listing failures as no files, and do not add per-retry or per-worker wildcard listings to large ADLS folders.
- Use `normalize_combo_values()` before validation, `Combo` construction, hierarchy processing, or artifact writes in both `set_agent_info()` and `prep_data()`. Trim character boundaries only; preserve internal spaces, missing values, and numeric identifiers. Fail before writing when normalization creates a blank value or duplicate combo/date. Do not trim generic `hash_data()` inputs.
- Never send `Inf`, `-Inf`, or `NaN` in EDA summaries to an LLM. Represent absent outlier dates explicitly and omit unavailable regressor-lag rankings while preserving raw EDA artifacts.
