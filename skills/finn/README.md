# Finn skill for coding agents

This folder is an agent skill that lets a coding agent (GitHub Copilot, Claude Code,
Codex, Cursor, and others that support `SKILL.md` skills) run Finn forecasts on your own
computer. You describe what you want in plain language; the agent installs R and finnts,
sets up a project, runs forecasts, checks progress, and answers questions about results.
You do not need to write or edit code.

## Install

The quickest way is one line in a terminal. It downloads only this folder from GitHub into
your personal GitHub Copilot skills folder and never overwrites an existing install.

Windows (PowerShell):

```powershell
irm https://raw.githubusercontent.com/microsoft/finnts/main/skills/finn/install_skill.ps1 | iex
```

macOS or Linux:

```sh
curl -fsSL https://raw.githubusercontent.com/microsoft/finnts/main/skills/finn/install_skill.sh | sh
```

For Claude Code, set `FINN_SKILL_AGENT=claude` first (`agents` installs to
`~/.agents/skills/finn`); `FINN_SKILL_DEST` picks any other folder. In PowerShell, for
example: `$env:FINN_SKILL_AGENT = "claude"`, then run the line above.

To install by hand instead, copy (or symlink) this `finn` folder into a skills folder
your agent reads:

| Agent | Personal (all projects) | One workspace |
|---|---|---|
| GitHub Copilot | `~/.copilot/skills/finn` | `.github/skills/finn` |
| Claude Code | `~/.claude/skills/finn` | `.claude/skills/finn` |
| Codex and others | see your agent's docs | `.agents/skills/finn` |

For example, on Windows PowerShell after cloning the repository:

```powershell
git clone https://github.com/microsoft/finnts.git
New-Item -ItemType Directory -Force "$HOME\.copilot\skills" | Out-Null
Copy-Item -Recurse finnts\skills\finn "$HOME\.copilot\skills\finn"
```

Or ask your agent: "Install the Finn skill from the `skills/finn` folder of
github.com/microsoft/finnts into my personal skills folder."

To download only the skill instead of the whole repository:

```powershell
git clone --depth 1 --filter=blob:none --sparse https://github.com/microsoft/finnts.git
git -C finnts sparse-checkout set skills/finn
```

Agents find the skill by its folder name and the `description` in `SKILL.md`. After
copying, start a new chat (or reload the agent) so it is picked up. The repository's
`AGENTS.md` and main `README.md` also point to this folder.

## Update

Your projects, settings, and forecasts live outside this folder, so updating is safe.
The skill and finnts ship together from this repository:

- About once a week `check_env.R` checks GitHub for a newer skill version and the agent
  offers to update. Turn this off with `update_skill.R --action=settings --auto_check=false`
  or the `FINN_SKILL_NO_UPDATE_CHECK=true` environment variable.
- `update_skill.R --action=update --confirm=true` backs up the current skill, copies in the
  new one, and reinstalls finnts from the same commit when its version is newer. If any
  step fails, the previous skill and finnts are restored.
- `update_skill.R --action=rollback --confirm=true` restores the most recent backup (or
  `--to=<backup id>`); `--package_only=true` rolls back only finnts.
- Ask for "the latest finnts" any time: `install_finnts.R --confirm=true` installs the
  newest GitHub build even when the skill is current (`--force=true` reinstalls the same
  build).

Updates wait until no forecast is running. If you use the skill straight from a git clone,
update with `git pull` instead. Unfinished runs resume normally after an update; if finnts
changed, the skill asks before resuming a run that started on the older version.
Details: [references/updates.md](references/updates.md).

## Get started

Open a chat with your agent and say something like:

> Use the Finn skill to forecast the next 12 months of revenue in `C:\data\revenue.xlsx`.

The agent will check your setup, ask before installing anything, ask where to keep
projects (a `Finn` folder in OneDrive by default), confirm the forecast settings with
you, and start the run in the background. Ask "how is my forecast doing?" at any time.

New to Finn? Say "Use the Finn skill to give me the Finn tutorial." Not sure how to lay
out your data? Ask for the data template ([templates/data_template.csv](templates/data_template.csv))
and see [references/data-requirements.md](references/data-requirements.md). More ideas
are in [references/starter-prompts.md](references/starter-prompts.md), and finance-friendly
definitions of terms like WMAPE and back test are in
[references/glossary.md](references/glossary.md).

## Your data and privacy

- **Standard runs stay on your computer.** Your data, settings, and forecasts are saved
  only in the project folder you chose (OneDrive by default, so your organization's
  OneDrive policies apply).
- **Agent runs send summaries, not your data rows, to the AI model you choose** (GitHub
  Copilot by default, or Azure OpenAI or OpenAI). These include data profile statistics
  (row and series counts, date range, negatives), trend, seasonality, outlier,
  missing-value, and driver checks, per-series summaries that name each series (for
  example `East--Laptops`), the settings and accuracy of each attempt, and error text.
  The skill asks before your first Agent run.
- **Your coding agent sees what it reads.** When you ask questions about your data or
  results, the agent reads the relevant files and sends them to its own model, as with
  any file it opens for you.
- **Update checks contact GitHub only**, and send no data. Turn them off with
  `FINN_SKILL_NO_UPDATE_CHECK=true`.
- **API keys** are read from environment variables, never saved, and hidden in logs and
  support reports.

## Report a problem

Ask the agent: "Create a support report so I can report a problem." It runs
`diagnose_run.R --report=true`, which writes a Markdown file to the project's `support/`
folder with versions, the diagnosis, settings, and the last log lines (you can leave
the log out). Home folders, user and computer names, e-mail addresses, organization
names, and keys are masked, but read the file before sharing. Then open a
[Finn skill problem](https://github.com/microsoft/finnts/issues/new?template=finn-skill.yml) issue and
paste it in. Do not include confidential data.

## Requirements

- R 4.1 or newer (the agent can guide installation).
- finnts and a few helper packages, installed by the skill after you agree.
- For Agent runs (AI-guided model search): access to an LLM through GitHub Copilot,
  Azure OpenAI, or OpenAI. Keys are read from environment variables and never saved.

## What is inside

- `SKILL.md`: instructions the agent follows.
- `install_skill.ps1`, `install_skill.sh`: the one-line installers.
- `references/`: detail on setup, storage, data requirements, runs and restarts, status
  and debugging, parallel processing, analysis, troubleshooting, a glossary, and
  starter prompts.
- `scripts/`: R scripts the agent runs; each prints one JSON result.
- `templates/` and `examples/`: a config template, a data template to fill in, and a
  small sample data set.
- `tests/`: offline checks for the scripts. Run `Rscript skills/finn/tests/test_skill.R`
  from the repository root; it needs only `jsonlite` and `digest` and trains no models.

## Future work

Saved reusable forecast workflows and scheduled runs are not part of this version.
