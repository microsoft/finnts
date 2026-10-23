# Custom models

A custom model is a forecasting rule the user describes in plain words, for example
"next year's value is this year's value grown by the last three months' growth rate".
finnts asks an LLM to write the R code, checks it on the project's history and the
user's worked examples, and approves it. The skill saves the approved model in the
project. Once enrolled, it competes with Finn's own models in every standard, iterate,
and update run of that project. The user creates it once; later runs reuse the saved
file without contacting the LLM again.

All steps go through `scripts/custom_model.R --project=<name> --action=<action>`.
Never create, edit, copy, or delete files under `custom_models/` yourself.

## Before you start

- `check_env.R` reports `custom_models_supported` (finnts can create and run custom
  models, including in Agent runs) and `custom_models_copilot` (it can create them with
  GitHub Copilot). If either is false and needed, offer `install_finnts.R --confirm=true`.
- Creation uses the project's `agent.llm` (setup.md), even for standard projects:
  GitHub Copilot through `finnts::chat_copilot()` by default, or an `azure_openai` or
  `openai` provider through ellmer. Copilot creation waits at least 600 seconds; pass
  `--timeout=<seconds>` to wait longer.
- The project needs a saved config (`validate_config.R --save=true`) whose data passes
  its checks.

## Workflow

1. **Understand the rule.** Ask what the model should do, whether it forecasts each
   series on its own (`local`, the default) or learns across series (`global`), which
   drivers it may use (they must already be in `external_regressors`), and what it
   should do for edge cases such as zeros, missing values, or short history. Agree on a
   name: it starts with a lowercase letter, uses only lowercase letters, digits, `_`, or
   `-` (no `--`), and differs from Finn's model names and the words `all`, `all-data`,
   `best-model`, `local`, `global`, `ensemble`, and `average`.
2. **Worked examples.** `--action=template --name=<model>` writes
   `custom_models/<model>-examples.csv` and never replaces an existing file (`exists`).
   Help the user replace the illustrative rows with 2 to 8 examples calculated by hand
   from their rule, including at least one edge case (example names starting with
   `edge_`). History rows fill the target and leave `expected` blank; future rows leave
   the target blank and fill `expected`. Examples are sent to the LLM, so use made-up
   values. Skipping them (`--skip_examples=true`) means finnts can't confirm the model
   matches the rule; recommend examples.
3. **Preview.** `--action=create --name=<model> --instructions="<rule>"
   --examples=<csv>` (or `--instructions_file=<path>` for longer rules; add `--drivers`,
   `--model_type`, `--horizon`, `--hist_start_date`, or `--forecast_approach` when
   needed). Without `--confirm=true` it checks every input and returns
   `needs_confirmation` with the request in `data`, including `check_window`.
4. **Consent.** Explain in plain words: creation sends the instructions, column names,
   and worked examples (not the project's series values) to the LLM, then runs the R code
   the LLM wrote on this computer to check it. That code is **not sandboxed**. Get a clear
   yes, then rerun the same command with `--confirm=true`. Add `--enroll=true` only if
   the user also wants this project's runs to use the model.
5. **Explain the result.** On `ok`, describe `data.model.interpretation` in plain words
   and say that passing its checks does not guarantee forecast accuracy.
6. **Enroll.** `--action=enroll --name=<model> [--version=<v>]` returns
   `needs_confirmation`; after the user agrees, rerun with `--confirm=true`. When the
   model has several saved versions and no `--version`, it returns `needs_input` with
   `data.versions`; ask which one. A model has at most one enrolled version.
7. **Review before running.** Rerun `validate_config.R --project=<name>` and show
   `data.settings_review`; its "Custom models" section lists each enrolled model and
   `data.settings_changes` shows enrollment changes.

`--action=list` shows saved models, their versions, and which are enrolled.
`--action=show --name=<model>` checks the saved file and explains the enrolled version
(or the newest, or `--version`). `--action=unenroll --name=<model>` returns
`needs_confirmation`, then with `--confirm=true` stops runs from using it; the saved
files stay, so it can be enrolled again. It returns `not_enrolled` if nothing changed.

The check window is the history used to test the model during creation. By default it is
the latest stretch of complete actuals that keeps the most series and periods, using up
to 3 series (100 for a global model) and at most 10,000 rows. `--hist_start_date`
chooses its start. `data.check_window` reports the series and dates used.

## Create statuses

| status | meaning | what to do |
|---|---|---|
| `needs_confirmation` | inputs checked; nothing sent yet | explain step 4 and ask; rerun with `--confirm=true` |
| `ok` | created, checked, approved, and saved (enrolled with `--enroll=true`); "already saved" means this exact version existed and nothing was replaced | explain the interpretation; offer to enroll |
| `needs_input` | the LLM needs business answers (`data.questions`) | ask the user, add the answers to the instructions, and run create again |
| `not_created` | the LLM failed or the model failed its checks (`data.feedback`); nothing was saved | explain in plain words; adjust the instructions or examples with the user and try again |
| `blocked` | the installed finnts can't create custom models, or can't with Copilot | offer `install_finnts.R --confirm=true`, or switch `agent.llm` to another provider |
| `invalid` | the project config has errors | fix them with `validate_config.R`, then try again |

## Saved files and settings

```
<project>/custom_models/
  <model>-examples.csv          worked examples
  <model>/<version>.rds         approved model (one file per version)
  <model>/<version>.json        readable details: type, drivers, LLM, checks, created_at
```

`<version>` is the first 12 characters of the model's 64-character `version_id`. Saved
versions are never replaced or deleted; changing the rule creates a new version.
Enrollment is saved in `config.json` as
`custom_models: [{"name": "<model>", "version_id": "<64 characters>"}]`; change it only
with `--action=enroll` or `--action=unenroll`. Runs check each enrolled file again
before using it; a missing or changed file is a config error.

## How runs use them

- **Standard:** enrolled models train alongside Finn's models and compete in model
  selection. When `models_to_run` lists models, the enrolled ones are added to it; a
  model can't also be in `models_not_to_run`.
- **Agent iterate:** every iteration of the Agent version includes the enrolled models.
  Agent runs with custom models train series in parallel workers rather than models in
  parallel within a series, so the review's run-time estimate may change.
- **Agent update:** must enroll the same custom models as the last Agent version.
  Otherwise launch returns `needs_new_version` (`reason: custom_models_changed`, with
  `previous` and `new`): restore the earlier enrollment, or start a new Agent version
  after the user agrees.
- **Changing enrollment on an Agent project:** the enrollment is part of the Agent
  version's settings, so the next iterate needs a new Agent version (`needs_new_version`,
  `reason: settings_changed`). With newer actuals, `validate_config.R` recommends
  `--mode=iterate --new=true --confirm=true` and offers update as the other option.
  Earlier versions stay as history (runs-and-restarts.md).

## Limits

`validate_config.R` reports each of these as a config error:

- The model's date type must match `date_type`.
- A `global` model needs `run_global_models` on; a `local` model needs
  `run_local_models` on.
- Drivers the model uses must be in `external_regressors`.
- R packages the model needs must be installed.
- **Hierarchies:** create the model with `--forecast_approach=standard_hierarchy` or
  `grouped_hierarchy` (local, no drivers) and use it only with that approach. The config
  needs `run_local_models` true, `run_global_models` false, no drivers, data that is not
  weekly, and at least one of Finn's models, without foundation models.
- **Agent:** Agent runs can't use hierarchy models, and `agent.allow_hierarchical_forecast`
  and `agent.allow_iterate_forecast` must stay false.
