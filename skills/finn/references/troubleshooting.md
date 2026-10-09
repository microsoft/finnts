# Troubleshooting

`diagnose_run.R` returns one of these categories. Always explain the cause in plain
language and ask before changing settings, data, packages, or Agent versions. After any
fix, resume with the same `run_forecast.R --mode=...` command; saved progress is kept.

| category | likely cause | fix |
|---|---|---|
| `memory` | out of memory while training in parallel | resume (it uses fewer workers automatically); ask the user to close large apps; if it repeats, set `"parallel": "none"` with their OK |
| `missing_package` | an R package is missing or old | `check_env.R`; with approval `install_finnts.R --confirm=true` |
| `agent_inputs_changed` | Agent saw different settings/data than its saved version | restore the original settings/data and resume, or confirm a new Agent version |
| `agent_history` | saved Agent history did not match cleanly | delete nothing; report run and request ids; offer a confirmed new Agent version |
| `agent_approach_changed` | forecast approach differs from the saved Agent version | restore the approach, or confirm a new version |
| `update_needs_agent` | update has no finished Agent forecast | run `--mode=iterate` or resume the unfinished Agent run |
| `update_too_many_new_series` | too many new series for a quick update | with approval, a new iterate version |
| `update_model_mix` | update turns off local/global models the last Agent used | restore `run_local_models` / `run_global_models`, or a new version |
| `missing_regressors` | a driver column is missing or lacks future values | `profile_data.R`; add future driver values for the full horizon or drop the driver |
| `no_series` | nothing left after cleanup | check `combo_cleanup_date`, `hist_start_date`, and recent non-zero targets |
| `llm_access` | Agent could not reach the AI model | Copilot: run `check_env.R` and follow `copilot.problems` (install GitHub CLI, user runs `gh auth login` with a Copilot-enabled account, or `GH_TOKEN`; CLI 1.0.93+, standalone `copilot.exe`); API keys: check env vars (setup.md); rate limit: wait and resume |
| `copilot_policy` | the account has no Copilot license, or an organization policy blocks the CLI or model | user checks `gh auth status`, `gh auth switch` to a licensed account, or asks their admin; or use Azure OpenAI/OpenAI (setup.md) |
| `copilot_premium_limit` | the monthly premium-request allowance is used up | wait for the reset, get more allowance, pick a model included in the plan, or switch provider; then resume |
| `copilot_account_host` | wrong account or GitHub host (stale `GH_TOKEN`/`GITHUB_TOKEN`, `GH_HOST`) | `gh auth status`; sign in to the right account; clear the stale variable with the user's OK; resume |
| `file_locked` | OneDrive sync or an open Excel file locked a file | close the file, pause sync, resume; suggest `move_root` if it repeats |
| `disk_space` | the drive is (nearly) full | free space or `move_root` to a drive with room (storage.md); delete nothing yourself; resume |
| `date_format` | dates unreadable or wrong `date_type` | `profile_data.R`; confirm date column and type; set `date_format` for day/month dates (data-and-settings.md) |
| `config_value` | a setting has the wrong type or value | `validate_config.R`; fix `config.json` with approval; resume rereads it |
| `interrupted` | stopped without error (sleep, restart, closed terminal) | keep the computer awake and plugged in; resume |
| `unknown` | not a known pattern | read `error_lines` and the log; explain and propose a fix |

## Common questions

- **"It says busy."** A run is active. Show its status. Cancel only on request.
- **"It says machine_busy."** Other projects' forecasts are using the computer. Show
  `run_status.R --all=true` and offer to start this one when one finishes.
- **"The forecast looks wrong."** Do not rerun first. Analyze: compare to history, check the
  back-test accuracy per series, check outliers and recent level shifts (analysis.md).
- **"Excel export failed."** `writexl` may be missing; offer to install it or use CSV.
  Sheets over Excel's 1,048,575-row limit cannot be written as xlsx; use CSV (written
  UTF-8 with a BOM so Excel shows accented names correctly).
- **Numbers or accented names look wrong.** Check how dates, decimals, and encoding were
  read ("International data" in data-and-settings.md).
- **"R is not found."** See setup.md; installing R is the user's decision.
- **Slow OneDrive.** See storage.md.
- **"My spreadsheet has months as columns / totals / a title row."** `profile_data.R`
  returns `needs_reshape` with `shape_issues` and `suggested_prepare_args`. Explain each
  issue, confirm, run `prepare_data.R` (it writes a `_finn_ready.csv` copy and never
  changes the original), then profile the copy.
- **Data warnings** (all-zero series, values after `hist_end_date`, drivers without
  future values) never block a run. Explain them; the user decides whether to fix the
  data first. Duplicate series/date rows are errors and must be fixed.
- **"Running out of disk space."** `setup_project.R --action=disk_usage --project=<name>`;
  the user can archive old `runs/` and `output/` folders by hand. Keep `finn_artifacts/`.
- **Work or managed computer** (no admin rights, proxy, OneDrive placeholders, org
  Copilot policy): see "Corporate or managed computers" in setup.md.
- **"Something broke after an update."** Run `diagnose_run.R` first. If the problem started
  with the update, offer `update_skill.R --action=rollback --confirm=true`
  (`--package_only=true` if only finnts changed). See updates.md.
