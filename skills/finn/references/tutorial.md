# First-time tutorial

Offer this when the user is new (for example `setup_project.R --action=list` shows no
projects) or asks how Finn works. It takes about 10-20 minutes and uses the bundled
`examples/sample_monthly.csv`: 4 monthly revenue series (2 regions x 2 products, 48
months). Go one step at a time, explain in plain finance language, and pause for
questions. The user can stop at any point and switch to their own data.

Prerequisites: `check_env.R` is ready and the storage root is chosen (SKILL.md
first-time flow steps 1-2).

## 1. Projects

Explain: each forecast lives in its own project folder under the Finn folder, holding a
copy of the data, settings, run history, and results. Finn never deletes files.

Run `setup_project.R --action=create --project=finn_tutorial --data=<skill>/examples/sample_monthly.csv`.
If `finn_tutorial` already exists, ask whether to reuse it or pick a new name such as
`finn_tutorial_2`.

## 2. The data

Run `profile_data.R --project=finn_tutorial`. Explain:

- a **series** is one line to forecast, identified here by Region and Product;
- the **target** is Revenue, the **date** is monthly;
- Finn needs one row per series per date, in a "long" layout (it can reshape wide files).

## 3. Settings card

Build the tutorial project's config using the normal configuration template, but
recommend these settings for the first run:

- `mode`: `"standard"`
- `models_to_run`: `["arima", "ets", "glmnet"]`
- `run_ensemble_models`: `false`

These are tutorial recommendations, not changes to the general defaults. Explain
them and get the user's agreement before applying them to the project config.
If reusing a tutorial project, review its saved settings first; do not silently
overwrite them. Keep other settings unchanged unless the user agrees.

Run `validate_config.R --project=finn_tutorial` and walk through the card:

- **Forecast horizon:** how many months ahead (suggest 12 here).
- **Back testing:** Finn hides recent months, forecasts them, and compares with actuals to
  score each model. More back tests give a more reliable score but take longer.
- **Models:** keep the first run small with ARIMA, ETS, and GLMNET only. ARIMA
  models patterns over time, ETS models level/trend/seasonality, and GLMNET is
  regularized regression. Do not leave `models_to_run` unset, which uses Finn's
  full default model list.
- **Ensembles:** keep `run_ensemble_models = false` so Finn does not train extra
  ensemble models. Simple averaging (`average_models`) is a separate setting;
  turning off ensemble training does not turn off averaging.
- **Fiscal year start:** ask which month; it helps with seasonal features.

Check that the validated config contains exactly `arima`, `ets`, and `glmnet`
and has `run_ensemble_models = false`, unless the user explicitly chose otherwise.
Save with `--save=true` after the user agrees.

## 4. Run a standard forecast

Explain the run types briefly (choosing-a-run.md) and that the tutorial uses standard
because it avoids Agent iterations and needs no AI provider. The restricted model
list and disabled ensemble training keep this first run simpler; actual runtime
depends on the machine. Ask for approval to start, then run
`run_forecast.R --project=finn_tutorial --mode=standard`. It runs in the background.

## 5. Status

While it runs, show `run_status.R --project=finn_tutorial`: the current stage and %
complete. Mention that the user can ask for status any time, cancel a run, or close the
chat and resume later.

## 6. Results

When finished:

- **Accuracy:** explain the back-test error (for example weighted MAPE: on average, how
  far off the forecast was in past tests) per series and overall.
- **Forecast:** show the next 12 months per series with the 80% and 95% ranges, in the
  input's units.
- **Best model:** which model won for each series, in plain words.

Answer one or two follow-up questions with free-form analysis (analysis.md) to show what
is possible, for example "which product grows fastest next year?".

## 7. Export

Run `export_results.R --project=finn_tutorial` and show where the spreadsheet landed.

## 8. Next steps

- **Your own data:** create a new project from their file; the same steps apply. Use
  one run type for all of a data set's series (choosing-a-run.md). The tutorial's
  three-model shortlist is a starting point, not a requirement for future projects;
  offer broader model comparisons after explaining them and getting approval.
  Keep ensemble training off for all standard runs unless the user explicitly
  asks to enable it; do not proactively offer it.
- **The Agent:** if Copilot is set up and the user agrees to share data with it, a
  `--mode=iterate` run on the tutorial project shows how the AI search improves
  accuracy. Mention it takes longer and uses Copilot requests.
- **Large data:** hundreds or thousands of series get size-based suggestions and a
  sample test first (choosing-a-run.md).
- **New actuals later:** refresh with `--mode=update` after an Agent run, or a new
  standard run.

The tutorial project can be kept as a reference; Finn never deletes it.
