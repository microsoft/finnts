# Setup

## 1. Find R

Run `powershell -NoProfile -ExecutionPolicy Bypass -File scripts/find_rscript.ps1` (Windows)
or `sh scripts/find_rscript.sh` (macOS/Linux).
It prints the Rscript path. If none is found, tell the user R 4.1 or newer is needed and
point them to https://cloud.r-project.org. Installing R itself is the user's choice; do
not run an installer without explicit approval.

## 2. Check the environment

`Rscript scripts/check_env.R` reports R version, package versions, missing packages,
free memory and cores, and two Agent-related flags:

- `agent_resume_supported`: this finnts version can resume Agent runs by request id.
  If false, standard forecasts still work; offer the GitHub version for Agent mode.
- `copilot`: whether Agent runs can use GitHub Copilot through `finnts::chat_copilot()`.
  `ready` is true or `problems` lists each fix. `auth_source` names where sign-in comes
  from (never the token itself). `run_forecast.R` repeats this check before any Agent
  run and stops with `needs_setup` instead of starting a run that would fail.

## 3. Install packages (after the user agrees)

```
Rscript scripts/install_finnts.R                        # dry run: shows the plan
Rscript scripts/install_finnts.R --confirm=true         # finnts from GitHub + helpers
Rscript scripts/install_finnts.R --ref=<commit or tag> --confirm=true   # a specific build
Rscript scripts/install_finnts.R --force=true --confirm=true   # reinstall even if up to date
Rscript scripts/install_finnts.R --source=cran --confirm=true   # fallback when GitHub is blocked
```

finnts installs from the GitHub repository by default so it matches the skill version;
the CRAN release can lag behind and may lack `chat_copilot()`. GitHub installs use the
`remotes` package (installed automatically).

Helpers: jsonlite, ps, processx, digest, readxl, and ellmer (skip with
`--include_llm=false`). If the default library is not writable, the script installs to
the user library. Installs can take 10-30 minutes; tell the user before starting.
Optional: `writexl` for Excel exports, `ggplot2` for charts in analysis.

## 4. LLM access (Agent mode only)

Standard forecasts need no LLM. Agent mode (`--mode=iterate` or `update`) needs one,
set in `config.json` under `agent.llm`:

| provider | needs |
|---|---|
| `copilot` (default) | See "GitHub Copilot" below |
| `azure_openai` | `AZURE_OPENAI_ENDPOINT`; `AZURE_OPENAI_API_KEY` or Azure sign-in; `model` = deployment name |
| `openai` | `OPENAI_API_KEY`; `model` |

`endpoint_env` and `api_key_env` in `agent.llm` rename the variables to read. Keys are
read from the environment only; never write them into config, logs, or chat.
Ask the user to set variables themselves in their shell or user environment.

### GitHub Copilot (default)

Agent runs call `finnts::chat_copilot()`, which needs:

1. finnts with `chat_copilot()`. The default GitHub install has it; CRAN releases may not.
2. GitHub Copilot CLI 1.0.93 or newer. On Windows use the standalone `copilot.exe`
   (for example `winget install GitHub.Copilot`); the npm `copilot.cmd` shim is rejected.
   If it is not on PATH, set `agent.llm.command` to its full path.
3. The `processx` package 3.9.0 or newer (installed by `install_finnts.R`).
4. A GitHub account with Copilot access, signed in through `gh auth login`, or a token
   the user sets in `COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, or `GITHUB_TOKEN`. Each request
   runs the CLI in an isolated home, so signing in only inside `copilot` is not enough.
   If no sign-in is found, walk the user through:
   1. Install the GitHub CLI from https://cli.github.com/ (Windows: `winget install GitHub.cli`).
   2. Run `gh auth login` in their own terminal (it is interactive; do not run it for them).
   3. Sign in with an account that has Copilot access.
   4. Rerun `check_env.R`, then retry the Finn request.

   Guide: https://docs.github.com/en/copilot/how-tos/copilot-cli/set-up-copilot-cli/authenticate-copilot-cli
5. Data the user's organization allows to be sent to GitHub Copilot. Agent prompts carry
   series summaries and accuracy results; confirm this with the user before the first
   Agent run on business data.

Optional `agent.llm` keys: `model` (default `auto`), `command`, `timeout` (seconds, default 120).

Agent runs send many model requests (often hundreds per run). On Copilot plans with a
premium-request allowance this can use a noticeable share; `run_forecast.R` adds a note.
Tell the user before the first Agent run, and suggest a model included in their plan if
they are close to the limit.

## 5. Corporate or managed computers

- **No admin rights:** R installs per user on Windows (choose "Install just for me"), and
  `install_finnts.R` falls back to the user library. Never ask for elevation.
- **Proxy or firewall:** installs need `github.com` and `api.github.com` (finnts, update
  checks) and a CRAN mirror (`cloud.r-project.org`, dependencies). If GitHub is blocked,
  use `--source=cran`. If downloads fail, ask the user (or their IT team)
  for the proxy address and have them set `HTTPS_PROXY`/`HTTP_PROXY` in their user
  environment. Copilot Agent runs also need the Copilot endpoints to be reachable.
- **Organization Copilot policy:** an organization can turn off the Copilot CLI or certain
  models. `diagnose_run.R` reports `copilot_policy`; the user checks with their admin or
  switches to Azure OpenAI. Confirm data-sharing rules (Copilot requirement 5) first.
- **Several GitHub accounts:** sign in with the one that has Copilot (`gh auth status`,
  `gh auth switch`). Stale `GH_TOKEN`/`GITHUB_TOKEN` variables override `gh` sign-in.
- **OneDrive:** business and personal OneDrive can both exist, and Files On-Demand can
  leave project files as placeholders. See storage.md.
- **Sleep and restarts:** managed devices may sleep or restart for updates mid-run. The
  run shows `interrupted`; resume it. Suggest keeping the device plugged in for long runs.
