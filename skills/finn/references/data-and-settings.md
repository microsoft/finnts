# Data, settings, and common requests

For the required data layout, history length, and template, see
[data-requirements.md](data-requirements.md).

## Settings (`config.json`)

Change settings only through `validate_config.R` and the user's approval. Key settings:

| setting | meaning |
|---|---|
| `combo_variables` | columns that identify a series (region, product, ...) |
| `target_variable` | the value to forecast |
| `date_column`, `date_type` | date column and `day`, `week`, `month`, `quarter`, or `year` |
| `date_format` | R date format when dates are ambiguous or unusual, for example `"%d/%m/%Y"`; null = auto-detect |
| `decimal_mark` | `"."` or `","` for text numbers like `1.234,56`; null = auto-detect |
| `fiscal_year_start` | month number (1-12) the fiscal year starts; asked for monthly, quarterly, and yearly data |
| `forecast_horizon` | periods to forecast |
| `hist_start_date`, `hist_end_date` | history window; later rows are treated as future driver values |
| `external_regressors` | driver columns; need values for every forecast period |
| `back_test_scenarios`, `back_test_spacing` | how many past periods are used to score models |
| `models_to_run`, `models_not_to_run` | model choices; null = Finn's defaults |
| `run_global_models`, `run_local_models`, `run_ensemble_models` | model families |
| `forecast_approach` | `bottoms_up`, `standard_hierarchy`, or `grouped_hierarchy` |
| `clean_outliers`, `clean_missing_values`, `negative_forecast` | data cleaning and sign rules |
| `parallel` | `auto` (skill picks local or inner parallelism) or `none` |
| `agent.llm` | AI model for Agent runs (setup.md) |
| `agent.max_iter`, `agent.weighted_mape_goal` | how long the Agent searches and its accuracy target |

## Choosing settings by size

`validate_config.R` suggests faster settings for more than 100 series: spaced back tests
that still cover the last year, global models, and for over 1,000 series a single global
xgboost model and a standard run before the Agent. Suggestions are advice only; apply
them after the user agrees. Details and the sample-first workflow are in
[choosing-a-run.md](choosing-a-run.md).

## International data

- **Dates:** when dates could be read as month/day or day/month, profiling picks one and
  adds a note. Confirm with the user and, if wrong, save `date_format`. Two-digit years
  are read as 2000-2068 and 1969-1999 (R's rule); confirm for old data.
- **Numbers:** text values such as `1.234,56` are detected as decimal comma and noted.
  If wrong, save `decimal_mark`.
- **Encoding and delimiters:** semicolon and tab separated files are detected. Files
  that are not UTF-8 (for example older Excel "CSV" exports) are read as Latin-1 with a
  note; if accented names look wrong, ask the user to save the file as "CSV UTF-8".
- **Fiscal year:** for monthly or higher data, `validate_config.R` warns when
  `fiscal_year_start` is unset. Ask which month the fiscal year starts and save it.

## Common requests

Changing data or settings on a project with an earlier run can trigger
`needs_confirmation` or `needs_new_version` from `run_forecast.R`. Follow
runs-and-restarts.md and confirm with the user before any fresh start or new Agent
version.

- **Exclude or add series:** do not edit the copy in `input/`. Write a short R script that
  filters the user's original file into a new file next to it (never overwrite), show
  which series change, then `setup_project.R --action=create --project=<name>
  --data=<new file> --replace_data=true` after the user agrees.
- **Combine a driver file with the main file:** join them by date (and series columns
  when drivers differ by series) in a short R script, write a new file, check there are
  driver values for every forecast period, then replace the project data as above and add
  the columns to `external_regressors`.
- **Hierarchical forecasts** (totals that must add up): set `forecast_approach` to
  `standard_hierarchy` (nested columns, for example region > country) or
  `grouped_hierarchy` (independent groupings). For the Agent, also set
  `agent.allow_hierarchical_forecast`. Input must be the lowest level only, without
  total rows (`prepare_data.R` removes them).
- **Units and scaling:** forecasts come back in the same units as the input. Do not
  rescale input data; convert to thousands or millions only in analysis or exports.
- **Prediction intervals:** `lo_80`/`hi_80` and `lo_95`/`hi_95` columns are ranges built
  from back-test errors. Explain them as "about 80% (or 95%) of past errors fell inside
  this range", not as guarantees.
- **Why the Agent chose an iteration:** read `get_best_agent_run()` through
  `finn_agent_handle()` and explain the accuracy of each iteration (analysis.md). Do not
  change how the Agent picks iterations.
- **Compare runs or versions:** `finn_compare_runs()` (analysis.md).
- **Switch the AI model or provider:** edit `agent.llm` with the user's OK (setup.md),
  then run `check_env.R` again. A changed setting on an unfinished run asks for
  confirmation like any other setting change.
- **List projects:** `setup_project.R --action=list`.
- **Rename or archive a project:** finnts stores the project name inside its working
  files, so do not rename a project folder. Create a new project with the new name
  instead. To archive, the user moves the whole project folder somewhere else themselves
  while no run is active.
- **Uninstall:** Finn never deletes files. Tell the user they can remove the skill folder,
  the `~/.finn` settings folder, and the finnts package (`remove.packages("finnts")`)
  themselves; projects stay in their Finn folder until they remove them.
