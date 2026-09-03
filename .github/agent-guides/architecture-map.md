# FinnTS Architecture Map

Use this map to find the code that owns behavior. Confirm details in the implementation and tests before editing; this guide is navigation, not a substitute for source code.

## Standard Forecasting Flow

1. `R/run_info.R` creates and validates run configuration through `set_run_info()`.
2. `R/prep_data.R` prepares time-series data; shared input and combo validation lives in `R/input_checks.R`.
3. `R/prep_models.R` resolves model definitions, recipes, lags, and seasonal periods.
4. `R/train_models.R` fits enabled model workflows and coordinates feature selection.
5. `R/forecast_time_series.R` orchestrates the end-to-end standard forecast workflow.

Start with `tests/testthat/test-prep_data.R`, `tests/testthat/test-prep_models.R`, and `tests/testthat/test-forecast_time_series.R` for regression coverage near this flow.

`R/run_info.R` is also consumed by Agent workflows. Follow the Agent runtime rule when changing fields or defaults used by Agent reasoning, replay, or run comparison.

## Agent Forecasting Flow

1. `R/agent_info.R` creates Agent configuration and canonicalizes input identity through `set_agent_info()`.
2. `R/agent_iterate_forecast.R` defines forecast iteration, reasoning inputs, run comparison, graph construction, and finalization.
3. `R/agent_run.R` executes graph nodes and classifies retryable reasoning failures versus hard operational failures.
4. `R/agent_update_forecast.R` replays previous settings and handles compatibility with older runs.
5. `R/agent_eda.R`, `R/agent_summarize_models.R`, and `R/agent_ask.R` own their respective LLM-facing workflows.
6. `R/read_write_data.R` owns storage access used by the workflows; graph execution should not acquire new storage responsibilities.

High-value tests are `test-agent-chat-serialization.R`, `test-agent-duplicate-runs.R`, `test-agent-graceful-abort.R`, `test-agent-eda-summaries.R`, `test-finalize_run.R`, and `test-combo-normalization.R` under `tests/testthat/`.

## Multistep Models

`R/multistep_helper.R` contains shared horizon and training-data behavior. Model adapters live in `R/multistep_*.R`; their cross-frequency and daily regression coverage lives in `tests/testthat/test-multistep*.R`.

## Optional Features

- `R/optional_dependencies.R` centralizes optional-package checks and actionable errors.
- `R/feature_selection.R` owns `vip`, Boruta, ranger, and corrr feature-selection behavior.
- `R/agent_summarize_models.R` must degrade only variable-importance output when `vip` is unavailable.
- `R/timegpt_model.R` owns the optional `nixtlar` TimeGPT integration.
- `DESCRIPTION` and `.github/workflows/R-CMD-check.yaml` define supported package and CI dependency boundaries.

## Generated And Published Files

Roxygen comments in `R/` generate `NAMESPACE` and `man/*.Rd`. Pkgdown generates `docs/`. Edit sources and regenerate outputs; never hand-edit generated files.
