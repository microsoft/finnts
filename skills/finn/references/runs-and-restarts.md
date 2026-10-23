# Runs and restarts

All runs go through `scripts/run_forecast.R --project=<name>`. Never call finnts forecasting
functions directly for a run; the script records state so runs can be checked, cancelled,
and resumed.

## Modes

| `--mode` | finnts calls | use when |
|---|---|---|
| `standard` (default unless config `mode` is `agent`) | `set_run_info`, `prep_data`, `prep_models`, `train_models`, `ensemble_models`, `final_models` | a normal forecast from `config.json` settings |
| `iterate` | `set_agent_info_custom` + `iterate_forecast` | Agent searches settings per series to reduce error (needs an LLM) |
| `update` | `set_agent_info_custom` + `update_forecast` | refresh a finished Agent forecast with newer actuals, reusing what the Agent learned |

Add `--wait=true` for short runs (small data) so the command returns the final result.
Otherwise the run starts in the background and returns `started` with a `run_id`.

## Resume by default

Each launch first looks at the latest run of the same mode. If it was `interrupted`,
`failed`, or `cancelled`, the script **resumes it**: same `run_id`, same finnts run name or
Agent `request_id`, same saved `runs/<run_id>/config.json`. finnts skips work already saved
in `finn_artifacts/`, so resuming picks up where it stopped. Nothing is wiped and no new
Agent version is created. After memory failures, a resume uses fewer workers automatically.

If the settings or input data changed since that run started, the script returns
`needs_confirmation` with `changed`. Ask the user:

- resume with the original settings and data: rerun with `--confirm=true`, or
- start fresh with the new ones: rerun with `--new=true` (Agent modes also `--confirm=true`).

Agent LLM changes (provider, model, command, timeout, or credential-variable names)
also count as settings drift. Confirmed resumes keep the original LLM snapshot;
choose a fresh run to apply those changes. Environment-variable values are not
saved or fingerprinted, so repairing credentials for the same provider can still
resume. Older runs with saved configuration are compared using that configuration,
not just their older fingerprint. Parallel settings remain adjustable on resume.

The same question comes up when the run started with a different finnts or R version
(`version_changes`): finishing it with the installed software can mix results from two
versions. Run state and `project.json` record `finnts_version` and `r_version` for this
check; runs from older skill releases without them are not flagged.

## Agent versions

`request_id` is stored in each run's state and passed to `set_agent_info_custom()`, so
the same Agent run is reopened on resume instead of creating a new version. A new
version (`overwrite = TRUE`) is only created when:

- the user confirms a fresh iterate (`--mode=iterate --new=true --confirm=true`), or
- an update runs (it builds on the latest finished Agent forecast).

Earlier versions and their history stay in `finn_artifacts/`.

Enrolled custom models ([custom-models.md](custom-models.md)) are part of an Agent
version's settings. Changing the enrollment makes the next iterate a new Agent version,
and an update must enroll the same custom models as the last Agent version
(`needs_new_version`, `reason: custom_models_changed`).

### Continuing an Agent version

`agent.max_iter` is the **total** number of iterations per search (the global search and
each series' own search) for one Agent version, not a number of extra iterations. To
continue a finished iterate run, show the review with the revised values, save it, and
launch `--mode=iterate` again. No extra flag is needed. With the same input file and
every other setting unchanged:

| change from the last iterate run | result |
|---|---|
| higher `agent.max_iter` (with or without a goal change) | `started`: same Agent version and `request_id`, nothing overwritten; each search runs only the iterations it has left |
| stricter (lower) `agent.weighted_mape_goal`, same `max_iter` | `started`: same version; only series whose best model is global and miss the new goal reopen |
| looser goal only | `already_complete` (`reason: looser_goal_only`) |
| lower `agent.max_iter` (even with a goal change) | `already_complete` (`reason: lower_max_iter`); iterations can't be undone, so keep it at or above the last value |
| nothing | `already_complete` (`reason: unchanged`) |
| input data, forecast design, LLM, seed, or any other setting | `needs_new_version` (`reason: data_changed` or `settings_changed`, with `changed`); a new version needs `--new=true --confirm=true` |

A started continuation returns `data.continuation` with `agent_version`, `request_id`,
`from_run_id`, `from_status`, `partial`, and `max_iter` / `weighted_mape_goal` (each
`previous` and `new`). Stop results add `data.reason`. If the installed finnts or R
differs from the last Agent forecast, the launch returns `needs_confirmation` (`reason:
software_changed`); after the user agrees, `--confirm=true` continues the same version.
A newer unfinished update returns `needs_new_version` (`reason: unfinished_update`):
resume it with `--mode=update` first. If the last Agent forecast was an update, only a new
version can search again (`reason: last_agent_not_iterate`).

A stopped iterate run whose only changes are a higher or equal `agent.max_iter` and/or a
different goal continues the same way: a new skill run reuses its `request_id`, so its
finished iterations are kept and the new values apply. Any other change still asks
whether to resume with the original settings or start fresh.

## Updates

`--mode=update` needs a finished Agent forecast and data that ends later than it did
(`--force=true` if only past values were revised). If `hist_end_date` is set to an
earlier date than the newest actuals, it returns `no_new_data` and says so; move
`hist_end_date` with the user's approval. If the only newer period is a month, quarter,
or year that has not ended yet, `no_new_data` says so and suggests waiting until it ends
rather than moving `hist_end_date` into partial actuals. If the series count changed,
the file changed without newer dates, or settings differ from the last Agent forecast,
it returns `needs_confirmation` with `changes`; confirm with the user and rerun with
`--confirm=true`, or suggest a fresh iterate if the series or settings changed a lot.
Projects whose last forecast was standard are refreshed with another standard run, not
an update. A different finnts or R version than the last Agent forecast only adds a
note that results may shift slightly.

## Result statuses to handle

| status | meaning | what to do |
|---|---|---|
| `started` / `completed` | run launched / finished (with `--wait`) | report `estimated_runtime` and `notes`; offer export and analysis |
| `busy` | another run in this project is active, or another launch is starting on this computer | check `run_status.R`; cancel only if the user asks |
| `machine_busy` | other projects' runs use the cores or memory this run needs; nothing started | tell the user what is running; relaunch after one finishes (parallelism.md) |
| `needs_setup` | Agent run but Copilot is not ready | walk through each listed fix (setup.md) |
| `invalid` | config errors | run `validate_config.R`; fix with the user |
| `needs_review` | the saved settings were reviewed for another run type | rerun `validate_config.R --run_type=<mode>`, show the review, save, then launch |
| `needs_confirmation` | resume with changed inputs, update with changed settings, a new Agent version, or different software (`reason: software_changed`) | ask the user, then rerun with the flags given |
| `already_complete` | iterate would run nothing new: same settings and data, a lower `agent.max_iter`, or only a looser goal (`reason`) | show results; raise `agent.max_iter` or tighten the goal to continue, or confirm a new version |
| `needs_new_version` | data or other settings changed, a newer update is unfinished, an update enrolls different custom models, or the earlier Agent run cannot be resumed (`reason`) | suggest `update` if data is newer; resume an unfinished update; restore the custom models of the last Agent version; otherwise confirm a new version |
| `needs_agent_run` | update has no finished Agent forecast | run `--mode=iterate` first or resume the unfinished one; standard projects refresh with `--mode=standard` |
| `no_new_data` | update data ends on the same date as last time, `hist_end_date` holds it back, or the only newer period has not ended | add newer actuals, move `hist_end_date`, wait for the open period to end, or `--force=true` if past values were revised |

## Corner cases

- **Machine restarted or terminal closed:** status shows `interrupted`; rerun the same command.
- **Run on another machine:** state records the host. A run started on another computer
  keeps showing as running here (so launches return `busy` with `other_host`), and cancel
  returns `other_machine`; check or cancel it on that computer. If the user confirms that
  computer is off or the run is dead, rerun with `--takeover=true`: it marks that run
  `interrupted` here so it can be resumed. Avoid sharing one project between machines at
  the same time.
- **User edited `config.json` mid-run:** the running job uses its saved copy; the change applies next run.
- **User replaced the input file:** detected by checksum; resume asks for confirmation.
  To point the project at a newer file, rerun `setup_project.R --action=create` with the
  new `--data`; if a different file with the same name exists it asks first, then
  `--replace_data=true` keeps the old copy and saves the new one with a timestamp.
- **Older finnts without `set_agent_info_custom`:** Agent runs cannot resume by request id.
  `check_env.R` reports this; offer the GitHub install (see setup.md).
- **Standard run after a finished one:** always a new `run_id`; old outputs stay in `output/`.
