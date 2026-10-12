# Adding A New Model

Use this checklist when adding, renaming, or removing a forecasting model. A model name is registered in several independent lists; missing one usually fails silently (the model is never trained, never summarized, or never offered to the Agent) rather than erroring. Search for an existing model with similar behavior (for example `rg -n '"snaive"' R tests vignettes`) and mirror every hit.

Locate code by function name; line numbers drift.

## 1. Model Registry And Spec (`R/models.R`)

- [ ] Add the name to `list_models()`. This exported list is the standard-run default when `models_to_run` is `NULL` and the source of the Agent's available models.
- [ ] Add the name to every category list that applies: `list_hyperparmater_models()` (tuned grid), `list_ensemble_models()` (feeds simple averages and ensembles), `list_r2_models()` (also trains on the R2 recipe), `list_global_models()`, `list_multivariate_models()` (uses external regressors), `list_foundation_models()`, and `list_multistep_models()`.
- [ ] Write a documented spec function named after the model with `-` replaced by `_` (for example `stlm-arima` → `stlm_arima()`). `prep_models()` resolves it with `get(gsub("-", "_", model))` and passes only the formal arguments it declares, chosen from `train_data`, `frequency`, `horizon`, `seasonal_period`, `model_type`, `pca`, `multistep`, `external_regressors`, and `lag_periods`.
- [ ] Prefer an existing modeltime or parsnip engine over new code. If the engine needs a new package, follow the [optional-dependency rule](../../.claude/rules/optional-dependencies.md) (`Suggests`, `R/optional_dependencies.R`, actionable error) before adding it.

The consumers of these lists usually need no edits, but confirm the model flows through them: `prep_models()` in `R/prep_models.R` (default list, the automatic `snaive` addition, R2 filtering, hyperparameter and ensemble lists), `train_models()` in `R/train_models.R` (global, multivariate, foundation, multistep routing), and `R/ensemble_models.R`.

## 2. Agent Model Summaries (`R/agent_summarize_models.R`)

- [ ] Add the model to the name-to-summarizer map inside `summarize_models()`.
- [ ] Write a documented `summarize_model_<name>(wf)` that validates the workflow class and engine and returns the standard summary shape. Without it, Agent summaries and LLM context omit the model.

## 3. Agent Iteration (`R/agent_iterate_forecast.R`)

- [ ] `get_available_agent_models()` derives from `list_models()` minus foundation models, intersected with `list_global_models()` for global runs. Confirm the model appears where intended.
- [ ] Decide whether the model belongs in the first-iteration rule lists `models_rule_10a`, `models_rule_10b`, and `models_rule_10c`, and update the matching prompt text for rules 9-A/9-B/9-C and the example JSON.
- [ ] If the model consumes `seasonal_period`, add it to prompt rule 8-H and to the custom-period validation that currently requires `stlm-arima`, `stlm-ets`, or `tbats`.
- [ ] Preserve the [iteration selection policy](../../.claude/rules/agent-runtime.md#iteration-selection-policy); adding a model must not change how iterations are compared.

## 4. Agent Update (`R/agent_update_forecast.R`)

- [ ] Decide whether the model belongs in the fixed default `models_to_run` inside `forecast_new_combos()`, used for new and failed series. This list is maintained separately from the first-iteration rules; keep them deliberately aligned.
- [ ] Global, multistep, multivariate, and foundation handling in update replay reads the category lists from `R/models.R`; confirm replay of saved runs that do not contain the new model still works ([version compatibility](../../.claude/rules/agent-runtime.md#existing-project-and-version-compatibility)).

## 5. Special Roles

- [ ] `snaive` is the fallback that fills missing combos during reconciliation in `R/hierarchy.R` and is added automatically for non-bottoms-up runs in `prep_models()`. Only touch these if the new model replaces that role.
- [ ] Multistep models also need an adapter under `R/multistep_*.R`; follow the [multistep rule](../../.claude/rules/multistep.md).

## 6. Documentation

- [ ] Add a row to the model table in `vignettes/models-used-in-finnts.Rmd`.
- [ ] Add a `NEWS.md` bullet under the current development version.
- [ ] Run `devtools::document()`; never hand-edit `NAMESPACE` or `man/`.

## 7. Tests

- [ ] `tests/testthat/test-forecast-selection.R`: the benchmark catalogue row count and the `choose(n, 2) + choose(n, 3)` combination count change with every model (and twice for R2 models).
- [ ] `tests/testthat/test-summarize_models.R`: add the model to `models_to_run` and add a focused summarizer test.
- [ ] `tests/testthat/test-forecast_time_series.R`: an end-to-end standard forecast that trains the model.
- [ ] `tests/testthat/test-agent-graceful-abort.R`: available-model and first-iteration prompt rule assertions.
- [ ] `tests/testthat/test-agent-update-selection.R`: update default model assertions, if the default list changed.
- [ ] Multistep, global, or optional-dependency tests when the model is in those categories.

Select final checks with the [validation matrix](validation-matrix.md); a new model touches standard, Agent, and update workflows, so run the full check before release.
