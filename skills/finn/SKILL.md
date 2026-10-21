---
name: finn
description: "Use when a user wants to forecast financial time series with Finn (the finnts R package) on their own computer without writing code: install R and finnts, set up a forecast project, check data, run standard or Agent forecasts, check run status, cancel or resume runs, debug failures, create support reports or submit GitHub issues, export results, and analyze inputs or forecasts."
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
   same run id and the same Agent version. A saved higher `agent.max_iter` or stricter
   `agent.weighted_mape_goal` continues the last Agent version. Fresh starts need the
   user's OK ([references/runs-and-restarts.md](references/runs-and-restarts.md)).
6. **Every script prints one JSON object** with `ok`, `status`, `message`, `data`, and
   `next_actions`. Read `status` and follow `next_actions`; relay `message` in plain words.
7. Never print, store, or commit API keys. LLM keys come from environment variables only.
8. **Standard runs keep ensemble training off.** For every new standard run,
   explicitly propose `run_ensemble_models = false`; never leave it null or enable
   it unless the user explicitly asks for ensemble models. Include this in the
   settings approval and verify it before launch. This applies to tutorials,
   sample tests, first forecasts, and later standard refreshes. Do not proactively
   offer ensemble training. Simple averaging (`average_models`) is separate and
   unchanged. Do not rewrite an unfinished run's saved settings on resume; explain
   any conflict and ask before a fresh run. Agent-run settings are unaffected.
9. **Ask before publishing a support issue.** A local support report is not permission
   to share it. Draft a sanitized title and body, show the exact preview, warn that
   `microsoft/finnts` issues are public, and obtain explicit approval before submission.
   Use the user's authenticated GitHub tooling, not R scripts or stored credentials.
   Follow [references/status-and-debugging.md](references/status-and-debugging.md#reporting-a-problem-on-github).

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
| Update Finn (skill + finnts) | `update_skill.R --action=check`, show versions and changes, ask, then `update_skill.R --action=update --confirm=true` ([references/updates.md](references/updates.md)) |
| What changed between Finn skill versions | `update_skill.R --action=changes` for offline installed history; `--remote=true` for the latest GitHub history. Summarize [version.json](version.json) in plain finance language |
| Latest finnts (skill already current) | Run `install_finnts.R`, show the comparison, ask, then `--confirm=true` |
| Undo an update | Ask, then `update_skill.R --action=rollback --confirm=true` (`--package_only=true` for finnts only) |
| Choose where projects live | `setup_project.R --action=status`, then `--action=set_root --root=... --confirm=true` ([references/storage.md](references/storage.md)) |
| Start a project from a file | `setup_project.R --action=create --project=<name> --data=<file>` |
| Look at the data | `profile_data.R --project=<name>` |
| Fix a wide, totals, or title-row layout | When profile returns `needs_reshape` with `suggested_prepare_args`, confirm, then `prepare_data.R --file=<file> <suggested_prepare_args> --project=<name>`. If it returns `needs_transform` instead, those flags would misread the file; use a transform spec |
| Fix any other messy layout (pivots, merged headers, footnotes, text numbers, mixed dates, duplicates, unit changes) | Diagnose, propose a transform spec on stdin with `--transform=- --preview=true`, show `data.preview_markdown` exactly, get one approval, then `--approve=<token>` ([references/data-preparation.md](references/data-preparation.md)). Never save the spec or skip its checks |
| Replace the data with a newer file | `setup_project.R --action=create --project=<name> --data=<file>`; on `needs_confirmation` ask, then add `--replace_data=true` |
| Check disk space used | `setup_project.R --action=disk_usage --project=<name>` |
| Compare two runs | Free-form R with `finn_compare_runs()` ([references/analysis.md](references/analysis.md)) |
| Review or change settings | `validate_config.R --project=<name>` (add `--run_type=standard\|iterate\|update` once the user picks), show `data.settings_review` exactly as written; for a change, validate a draft with `--config=<draft>` and show `data.settings_changes`; `--save=true` after the user replies `run` |
| Run a standard forecast | `run_forecast.R --project=<name> --mode=standard` |
| Run the Agent (AI model search) | `run_forecast.R --project=<name> --mode=iterate` |
| Refresh with new actuals | Same run type as the project's last forecast: `data.run.launch` from `validate_config.R` (`--mode=update` for Agent projects, `--mode=standard` for standard ones) |
| Resume a stopped run | `run_forecast.R --project=<name> --mode=<same mode>` |
| Check progress | `run_status.R --project=<name>` (stage and % complete); `--all=true` for every active run on this computer |
| Stop a run | Ask, then `run_cancel.R --project=<name> --run_id=<id> --confirm=true` |
| Fix a failed run | `diagnose_run.R --project=<name>` ([references/status-and-debugging.md](references/status-and-debugging.md)) |
| Create a support report / submit a problem to the Finn team | `diagnose_run.R --project=<name> --report=true` for local evidence; draft a sanitized issue, show the public-sharing warning and exact preview, ask, then submit with authenticated GitHub tooling and return the URL; otherwise offer a prefilled issue link ([references/status-and-debugging.md](references/status-and-debugging.md#reporting-a-problem-on-github)) |
| Get a spreadsheet | `export_results.R --project=<name>` (`--format=xlsx` needs writexl) |
| Ask questions about data or results | Free-form R ([references/analysis.md](references/analysis.md)) |
| List, rename, archive projects, or uninstall | `setup_project.R --action=list`; others in [references/data-and-settings.md](references/data-and-settings.md) |
| Exclude series, add a driver file, totals that add up, switch the AI model, explain ranges or Agent choices | [references/data-and-settings.md](references/data-and-settings.md) |
| European dates or numbers, odd file encodings | [references/data-and-settings.md](references/data-and-settings.md) "International data" |
| Several forecasts at once | For N planned concurrent projects, preview and launch each once with `--batch_size=N`, then `run_status.R --all=true`; on `machine_busy` wait, never cancel others unasked ([references/parallelism.md](references/parallelism.md)) |
| Speed or memory questions | [references/parallelism.md](references/parallelism.md) |
| Anything else failing | [references/troubleshooting.md](references/troubleshooting.md) |

## Updating and explaining versions

When users ask to update Finn, run `update_skill.R --action=check` first. Show
the installed and available skill and finnts versions, and summarize
`data.release_notes.changes_since_installed` when present. If release notes are
unavailable, say so; do not invent changes or treat a network failure as proof
that everything is current.

Ask for approval before installing. If the skill has an update, use
`update_skill.R --action=update --confirm=true`; it also installs finnts from
the compared commit when its version is newer. If only finnts is newer, use
`install_finnts.R` to preview its comparison, ask, then add `--confirm=true`.
Offer the same package-only path when explicitly asked for the latest finnts
build even if version numbers are unchanged. Never overwrite a Git worktree
with the downloaded updater; explain its Git-checkout guidance instead.

Explain that active runs must finish first and backups allow rollback. After a
skill update, ask the user to start a new chat and run `check_env.R`. For
"what changed over time", show the full `--action=changes` history; `--action=history`
is instead this machine's installation/rollback log and saved backups.

When maintaining this skill, bump `finn_skill_version` and add a matching newest
entry to [version.json](version.json) for each user-visible skill version change.
Keep `version` and `released` aligned with the newest `changelog` entry's
`version` and `date`; each entry has a `changes` array. Keep older entries below
the newest one. This branch's initial release is a single 0.1.0 entry.
Write NEWS-style summaries of user-visible changes, not setup instructions or
implementation details. Each version allows at most 5 change bullets, 200
characters per bullet, and 800 characters total across its bullets; offline
tests enforce these limits for every entry. Put details in the reference guides.
Do not change the finnts package version for a skill-only change.

## First-time flow

1. `check_env.R`. If R or packages are missing, explain and ask before installing.
2. `setup_project.R --action=status`. Show the suggested storage root (OneDrive `Finn`
   folder by default) and ask the user to accept it or pick another folder.
   If `setup_project.R --action=list` shows no projects, offer the tutorial
   ([references/tutorial.md](references/tutorial.md)) before their own data. If they have
   no file ready or are unsure of the layout, show data-requirements.md and the template.
3. `setup_project.R --action=create --project=<name> --data=<file>`. The input file is
   copied into the project; the original is untouched.
4. `profile_data.R`, then agree on one run type for the whole data set before showing
   settings (choosing-a-run.md); never suggest splitting series across run types.
   Standard for large data or speed; Agent iterate for small to medium data where
   accuracy matters, which needs an LLM, by default GitHub Copilot through
   `finnts::chat_copilot()`; Agent update to refresh a finished Agent project with new
   actuals. For existing projects, `validate_config.R` recommends the next run from the
   project's history (`data.run`); explain its `why` and other option. The user decides;
   pass their choice with `--run_type=<standard|iterate|update>`.
5. `validate_config.R --project=<name> --run_type=<choice>`. Show `data.settings_review`
   exactly as written; do not reorder, reformat, or paraphrase it. It includes an intro
   naming the run type with a plain-language hint, why it fits, and
   the other option; a data check; a forecast configuration table with each input's value
   and meaning; warnings; suggestions; and run time on this computer. If the user now
   prefers the other option, rerun with its `--run_type` and show the new review. Ask
   about anything marked as a guess, including how dates and decimals were read and, for
   monthly or longer data, which month the fiscal year starts. Apply suggestions from
   `data.recommendations` only if the user agrees, and never refuse a run. For more than
   100 series, suggest a sample test first
   ([references/choosing-a-run.md](references/choosing-a-run.md)). When the user changes
   an input, write the full proposed config to a draft file outside the project, run
   `validate_config.R --project=<name> --config=<draft> --run_type=<choice>`, and show
   `data.settings_changes` (only the changed inputs; show `data.settings_review` if they
   ask for the full configuration). Save with `--save=true --run_type=<choice>` only
   after the user replies `run` or otherwise agrees.
6. Launch with `run_forecast.R --project=<name>` plus the arguments in `data.run.launch`,
   so the run matches the reviewed run type; if launch returns `needs_review`, the saved
   settings were reviewed for another run type, so redo step 5. Before the first
   Agent run, confirm they are fine with data summaries and series names (not raw rows)
   being sent to the chosen LLM (README "Your data and privacy"). If launch returns `needs_setup`,
   walk them through each listed fix (setup.md "GitHub Copilot"). Tell them it runs in
   the background, share `estimated_runtime` as a
   rough range (not a promise), relay any `notes` (for example Copilot premium-request
   use, OneDrive sync), and check `run_status.R` when they ask.
   Data `warnings` (zero-only series, values after `hist_end_date`, a latest period that
   has not ended, drivers without future values) never block a run; explain them and let
   the user decide.
7. When it completes, summarize accuracy and the forecast (in the input's units), offer
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
