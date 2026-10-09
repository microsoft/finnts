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

Run `validate_config.R --project=finn_tutorial` and walk through the card:

- **Forecast horizon:** how many months ahead (suggest 12 here).
- **Back testing:** Finn hides recent months, forecasts them, and compares with actuals to
  score each model. More back tests give a more reliable score but take longer.
- **Models:** Finn tries simple statistical models (ARIMA, ETS, ...) per series and
  machine-learning models across series, then can average the best ones.
- **Fiscal year start:** ask which month; it helps with seasonal features.

Save with `--save=true` after the user agrees.

## 4. Run a standard forecast

Explain the run types briefly (choosing-a-run.md) and that the tutorial uses standard
because it is fast and needs no AI model. Run
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
  one run type for all of a data set's series (choosing-a-run.md).
- **The Agent:** if Copilot is set up and the user agrees to share data with it, a
  `--mode=iterate` run on the tutorial project shows how the AI search improves
  accuracy. Mention it takes longer and uses Copilot requests.
- **Large data:** hundreds or thousands of series get size-based suggestions and a
  sample test first (choosing-a-run.md).
- **New actuals later:** refresh with `--mode=update` after an Agent run, or a new
  standard run.

The tutorial project can be kept as a reference; Finn never deletes it.
