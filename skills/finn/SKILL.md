---
name: finn
description: "Use when a user wants to forecast financial time series with Finn (the finnts R package) on their own computer without writing code: install R and finnts, set up a forecast project, check data, run standard or Agent forecasts, check run status, cancel or resume runs, debug failures, export results, and analyze inputs or forecasts."
argument-hint: "Describe what you want to forecast or ask about an existing Finn project"
user-invocable: true
disable-model-invocation: false
---

# Finn Local Forecasting Skill

You run Finn for a finance user. The user should never need to edit code, R, or JSON.
Speak in plain finance language (series, horizon, accuracy, back test), not R jargon.

## Ground rules

1. **Runs use the fixed scripts below. Analysis is free-form.** Start, resume, cancel, and
   export only through `scripts/`. For questions about data or results, write a short R
   script on the fly with `scripts/finn_analysis_helpers.R` and finnts accessors
   ([references/analysis.md](references/analysis.md)).
2. **Never call `finnts::ask_agent()`.** You are the analyst. Answer questions with your
   own R code against saved outputs.
3. **Confirm before anything consequential.** Ask the user before installing software,
   choosing or moving the storage folder, changing a setting or data value, starting a
   fresh run or a new Agent version, or cancelling a run. Pass `--confirm=true` only after
   the user says yes.
4. **Never delete files.** No script deletes anything; do not delete project, run, or
   `finn_artifacts` files yourself. Old runs and Agent versions are history.
5. **Resume by default.** Rerunning `run_forecast.R` picks up an unfinished run with the
   same run id and the same Agent version. Fresh starts need the user's OK
   ([references/runs-and-restarts.md](references/runs-and-restarts.md)).
6. **Every script prints one JSON object** with `ok`, `status`, `message`, `data`, and
   `next_actions`. Read `status` and follow `next_actions`; relay `message` in plain words.
7. Never print, store, or commit API keys. LLM keys come from environment variables only.

## Running scripts

Find Rscript first: `powershell -NoProfile -ExecutionPolicy Bypass -File scripts/find_rscript.ps1`
(Windows) or `sh scripts/find_rscript.sh` (macOS/Linux).
Then call `"<Rscript>" "<skill>/scripts/<script>.R" --key=value`. Quote paths with spaces.
Runs launch in the background and return immediately; do not block the chat waiting.

## Capability index

| User wants to... | Do this |
|---|---|
| Get started / check setup | `check_env.R`, then [references/setup.md](references/setup.md) |
| Learn how Finn works | Guided tutorial on sample data ([references/tutorial.md](references/tutorial.md)) |
| What the data must look like, or a template to fill in | [references/data-requirements.md](references/data-requirements.md); copy `templates/data_template.csv` to the user's folder |
| Explain a term (WMAPE, back test, driver, ...) | [references/glossary.md](references/glossary.md) |
| "What can I ask?" | Offer a few from [references/starter-prompts.md](references/starter-prompts.md) |
| What data leaves the computer | README.md "Your data and privacy"; summarize it in plain words |
| Pick a run type, or forecast hundreds or thousands of series | [references/choosing-a-run.md](references/choosing-a-run.md); suggestions come from `validate_config.R` |
| Test on a sample of series first | `prepare_data.R --file=<file> --sample_series=50 --combo_variables=<cols> --out_dir=<folder>`, then a separate test project |
| Install R packages | Ask, then `install_finnts.R --confirm=true` (finnts from GitHub) |
| Update Finn (skill + finnts) | When `check_env.R` reports an update, ask, then `update_skill.R --action=update --confirm=true` ([references/updates.md](references/updates.md)) |
| Latest finnts (skill already current) | Run `install_finnts.R`, show the comparison, ask, then `--confirm=true` |
| Undo an update | Ask, then `update_skill.R --action=rollback --confirm=true` (`--package_only=true` for finnts only) |
| Choose where projects live | `setup_project.R --action=status`, then `--action=set_root --root=... --confirm=true` ([references/storage.md](references/storage.md)) |
| Start a project from a file | `setup_project.R --action=create --project=<name> --data=<file>` |
| Look at the data | `profile_data.R --project=<name>` |
| Fix a wide, totals, or title-row layout | When profile returns `needs_reshape`, confirm, then `prepare_data.R --file=<file> <suggested_prepare_args> --project=<name>` |
| Replace the data with a newer file | `setup_project.R --action=create --project=<name> --data=<file>`; on `needs_confirmation` ask, then add `--replace_data=true` |
| Check disk space used | `setup_project.R --action=disk_usage --project=<name>` |
| Compare two runs | Free-form R with `finn_compare_runs()` ([references/analysis.md](references/analysis.md)) |
| Review or change settings | `validate_config.R --project=<name>` and show the settings card; `--save=true` after the user agrees |
| Run a standard forecast | `run_forecast.R --project=<name> --mode=standard` |
| Run the Agent (AI model search) | `run_forecast.R --project=<name> --mode=iterate` |
| Refresh with new actuals | `run_forecast.R --project=<name> --mode=update` |
| Resume a stopped run | `run_forecast.R --project=<name> --mode=<same mode>` |
| Check progress | `run_status.R --project=<name>` (stage and % complete); `--all=true` for every active run on this computer |
| Stop a run | Ask, then `run_cancel.R --project=<name> --run_id=<id> --confirm=true` |
| Fix a failed run | `diagnose_run.R --project=<name>` ([references/status-and-debugging.md](references/status-and-debugging.md)) |
| Report a problem to the Finn team | Ask whether to include log lines, then `diagnose_run.R --project=<name> --report=true` (add `--include_log=false` if not); have the user review the masked report file before pasting it into the GitHub issue form |
| Get a spreadsheet | `export_results.R --project=<name>` (`--format=xlsx` needs writexl) |
| Ask questions about data or results | Free-form R ([references/analysis.md](references/analysis.md)) |
| List, rename, archive projects, or uninstall | `setup_project.R --action=list`; others in [references/data-and-settings.md](references/data-and-settings.md) |
| Exclude series, add a driver file, totals that add up, switch the AI model, explain ranges or Agent choices | [references/data-and-settings.md](references/data-and-settings.md) |
| European dates or numbers, odd file encodings | [references/data-and-settings.md](references/data-and-settings.md) "International data" |
| Several forecasts at once | One background launch per project, then `run_status.R --all=true`; on `machine_busy` wait, never cancel others unasked ([references/parallelism.md](references/parallelism.md)) |
| Speed or memory questions | [references/parallelism.md](references/parallelism.md) |
| Anything else failing | [references/troubleshooting.md](references/troubleshooting.md) |

## First-time flow

1. `check_env.R`. If R or packages are missing, explain and ask before installing.
2. `setup_project.R --action=status`. Show the suggested storage root (OneDrive `Finn`
   folder by default) and ask the user to accept it or pick another folder.
   If `setup_project.R --action=list` shows no projects, offer the tutorial
   ([references/tutorial.md](references/tutorial.md)) before their own data. If they have
   no file ready or are unsure of the layout, show data-requirements.md and the template.
3. `setup_project.R --action=create --project=<name> --data=<file>`. The input file is
   copied into the project; the original is untouched.
4. `profile_data.R`, then `validate_config.R`. Show the settings card: series columns,
   target, date type, horizon, back tests, models, and drivers. Ask about anything marked
   as a guess, including how dates and decimals were read and, for monthly or longer
   data, which month the fiscal year starts. Show any size-based suggestions from
   `data.recommendations` with their reasons; apply them only if the user agrees, and
   never refuse a run. For more than 100 series, suggest a sample test first
   ([references/choosing-a-run.md](references/choosing-a-run.md)). Save only after the
   user agrees.
5. Recommend one run type for the whole data set (choosing-a-run.md); never suggest
   splitting series across run types. Standard for large data or speed; Agent iterate
   for small to medium data where accuracy matters, which needs an LLM, by default
   GitHub Copilot through `finnts::chat_copilot()`; Agent update to refresh a finished
   Agent project with new actuals. The user decides. Before the first
   Agent run, confirm they are fine with data summaries and series names (not raw rows)
   being sent to the chosen LLM (README "Your data and privacy"). If launch returns `needs_setup`,
   walk them through each listed fix (setup.md "GitHub Copilot"). Launch with
   `run_forecast.R`, tell them it runs in the background, share `estimated_runtime` as a
   rough range (not a promise), relay any `notes` (for example Copilot premium-request
   use, OneDrive sync), and check `run_status.R` when they ask.
   Data `warnings` (zero-only series, values after `hist_end_date`, drivers without
   future values) never block a run; explain them and let the user decide.
6. When it completes, summarize accuracy and the forecast (in the input's units), offer
   `export_results.R`, and answer follow-ups with free-form analysis. After the first
   completed run (or the tutorial), offer three or four next prompts from
   starter-prompts.md.

After `update_skill.R` updates or rolls back the skill, read this file again before the
next step; scripts and instructions may have changed.

## Project layout

```
<root>/<project>/
  input/            copy of the user's data
  output/<run_id>/  forecasts, accuracy, exports/
  finn_artifacts/   finnts working files (the finnts `path=`); do not edit
  runs/<run_id>/    run state, config snapshot, log
  analysis/         saved free-form analysis results
  config.json       project settings
  project.json      project bookkeeping (latest Agent version, request id)
```

## Out of scope for now

Saved reusable workflows, scheduling, and cloud storage backends are future work.
Do not call finnts functions that are not covered here to start or modify runs.
