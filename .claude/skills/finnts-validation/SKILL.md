---
name: finnts-validation
description: "Use when validating a FinnTS code, test, documentation, dependency, Agent workflow, multistep model, or release change. Selects focused tests, CRAN profiles, documentation generation, and R CMD checks based on changed files and risk."
argument-hint: "Describe the change or files to validate"
user-invocable: true
disable-model-invocation: false
---

# FinnTS Validation

Use this workflow after making changes or when asked what checks a FinnTS pull request needs.

## Procedure

1. Inspect the changed files without reverting unrelated work.
2. Read the matching `.claude/rules/*.md` files and `.github/agent-guides/validation-matrix.md`.
3. Identify the smallest executable check that can disprove the implementation hypothesis.
4. Run that focused check before making adjacent edits. If it fails, repair the same behavioral slice and rerun it.
5. Regenerate roxygen output only when roxygen source changed. Inspect generated `NAMESPACE` and `man/*.Rd` changes; never edit them directly.
6. Run the deterministic CRAN profile for changes to skips, credentials, Agent reasoning, or provider integrations.
7. Run full tests when shared behavior or multiple modules changed. Run `devtools::check()` for public API, dependency, metadata, release, or broad package changes.
8. Report commands run, outcomes, skipped live integrations, and checks that remain outstanding.

After changing canonical rules or this skill, run `Rscript tools/generate-agent-adapters.R` before validation. After changing instruction discovery, run the representative cases in `.github/agent-guides/evaluation-cases.md` in GitHub Copilot, Claude Code, Codex, and Cursor as applicable.

## Guardrails

- Never expose credentials or print secret environment variables.
- Never remove `skip_on_cran()` to force a local integration test.
- Never treat a credential-based skip as live-provider validation.
- Do not run the full suite repeatedly when a focused test can distinguish the current failure.
- Do not modify package behavior merely to make environmental or optional-package checks pass.