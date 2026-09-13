---
paths:
  - "R/agent_*.R"
  - "R/final_models.R"
  - "R/forecast_selection.R"
  - "R/run_info.R"
  - "R/input_checks.R"
  - "R/prep_data.R"
  - "R/prep_models.R"
  - "R/read_write_data.R"
  - "tests/testthat/helper-forecast-selection.R"
  - "tests/testthat/test-agent*.R"
  - "tests/testthat/test-final-models-restart.R"
  - "tests/testthat/test-forecast-selection*.R"
  - "tests/testthat/test-reconciled-forecast-selection.R"
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
- Resolve global `forecast_approach` choices once from the outer Agent scope and consolidated hierarchy EDA before starting reasoning. Bottom-level Agent input may compare `bottoms_up` with only the exact detected standard or grouped hierarchy after bottom-level reconciliation. Input pre-expanded to hierarchy-level `ID` series must use `bottoms_up` for every inner global and local iteration, followed by one outer reconciliation. Keep retries storage-free and local proposals `bottoms_up`.
- Keep current-version optimization history separate from earlier-version replay context. Only current-version rows control run counts, best metrics, duplicate checks, and change budgets. Do not impose a separate cap on external-regressor configurations; their exploration is bounded by the workflow's overall `max_iter`. Canonicalize order-insensitive lag, rolling-window, and seasonal-period configurations before counting their budgets; defaults and previously tested configurations remain allowed after the new-configuration budget is exhausted. Reload both history views exactly once after each successfully logged iteration.
- On same-run `iterate_forecast()` restarts, load existing best-run completion metadata before global routing. Any global or local row finalized at the requested iteration target proves the global phase already returned; skip repeated global optimization and reuse that metadata for per-combo local routing. Incomplete, unusable, or lower-target metadata retains global run-count recovery. Do not add a separate phase-checkpoint artifact.
- Keep `execute_node()` storage-free. `finalize_run()` owns exhausted-reasoning artifact decisions. Local workflows use one exact best-run file read without wildcard listing. The single global workflow may list best-run files once, preserve global results when present, continue to enabled local models when no global result exists, and otherwise fail.
- Propagate every best-run blob-listing provider error, including post-write verification errors. Never reinterpret listing failures as no files, and do not add per-retry or per-worker wildcard listings to large ADLS folders.
- Use `normalize_combo_values()` before validation, `Combo` construction, hierarchy processing, or artifact writes in both `set_agent_info()` and `prep_data()`. Trim character boundaries only; preserve internal spaces, missing values, and numeric identifiers. Fail before writing when normalization creates a blank value or duplicate combo/date. Do not trim generic `hash_data()` inputs.
- Never send `Inf`, `-Inf`, or `NaN` in EDA summaries to an LLM. Represent absent outlier dates explicitly and omit unavailable regressor-lag rankings while preserving raw EDA artifacts.

## Iteration Selection Policy

- Average model accuracy is a critical search-direction signal, not redundant logging. The best individual model can stay unchanged while an input or setting change improves other models that may become the winners after further iterations.
- For example, ARIMA may remain the most accurate model after adding an external regressor (`xreg`), while the regressor improves MAPE across the multivariate models. Preserve that evidence when choosing the iteration context for continued optimization; do not discard the change solely because ARIMA did not improve. This is a useful signal, not a guarantee of future accuracy.
- Preserve the original near-best iteration-ranking rule: start from the earliest minimum-WMAPE iteration, then consider later eligible iterations within 10% relative of that WMAPE and prefer a strictly lower `model_avg_wmape`. Do not replace this with strict minimum-WMAPE-only ranking. Keep the existing current-version eligibility and distinct best-forecast persistence safeguards.
- `model_avg_wmape` is the mean of the individual model candidates' WMAPEs, not the WMAPE of an averaged forecast. Preserve the existing local/global meanings of `model_avg_wmape`, `model_median_wmape`, and `model_std_wmape`. Do not replace genuine model-pool statistics with copies of the winning model's WMAPE or a fabricated zero spread.
- Future-plausibility checks select models within each iteration. Avoiding repeated future-quality assessments across iterations must not remove these existing backtest-accuracy signals. Reuse available predictions and recorded metrics without introducing repeated artifact discovery or rereading past forecasts.
- All globally selected series must reference one winning global iteration so forecast updates reuse one global run. Promote global winners as one run-level decision, not per-series improvements across iterations. Preserve superior local winners and allow different model IDs or component subsets within that one global iteration. Reject mixed saved global iteration metadata before publication or update, including after interrupted writes; never silently split it into several global updates.
- Changes to this heuristic, its tolerance, metric meanings, rounding, or run-level versus per-series publishing rules require separate explicit user approval. Tests and documentation must protect the intended policy, not silently redefine it to match a refactor.
