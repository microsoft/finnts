# Agent Guidance Evaluation Cases

Use these cases after changing agent instructions, rules, skills, or discovery settings. They test whether the correct context loads and whether it changes the proposed workflow.

| Representative request | Expected scoped context | Expected response behavior |
| --- | --- | --- |
| Add a documented argument to an exported function in `R/` | `r-package.md` | Preserve compatibility, update roxygen, run a focused test, regenerate documentation, and inspect generated files |
| Change `reason_inputs()` or `finalize_run()` | `agent-runtime.md` and `r-package.md`; `testing.md` when tests are edited | Preserve typed graceful failures, hard operational errors, history separation, and bounded storage access; run Agent-focused tests |
| Change a fit or predict method in `R/multistep_*.R` | `multistep.md` and `r-package.md` | Check routing, training rows, lag eligibility, forecast identity, all date frequencies, and the daily matrix |
| Change `vip`, Boruta, ranger, or `nixtlar` handling | `optional-dependencies.md` and `r-package.md`; `testing.md` for CI/tests | Preserve `Suggests`, actionable missing-package behavior, core installation without the package, and CI isolation coverage |
| Add a provider integration test | `testing.md` | Put `skip_on_cran()` before credentials or provider calls, avoid printing secrets, test the no-credential path, and distinguish skips from live success |
| Change only agent-guidance files | Root instructions plus the relevant guidance file | Regenerate adapters, run `Rscript tools/validate-agent-guidance.R`, and do not run package tests solely for documentation maintenance |

## GitHub Copilot Check

1. Start a new chat for each case and mention the representative file.
2. Open Chat Customization Diagnostics and inspect the response references.
3. Confirm the expected `.github/instructions` adapter loaded and led to the canonical rule; unrelated scoped rules must remain absent.
4. Confirm the proposed checks match `.github/agent-guides/validation-matrix.md`.
5. For GitHub.com cloud agent or code review, confirm repository custom instructions are enabled and the adapter appears in response references.

## Claude Code Check

1. Start a new session and run `/context` to confirm `CLAUDE.md` and `AGENTS.md` loaded.
2. Open or mention the representative file for each case.
3. Use the `InstructionsLoaded` diagnostic hook when path-scoped behavior is unclear.
4. Confirm the matching `.claude/rules` file loads and unrelated rules remain absent.

## Codex Check

1. Start Codex from the repository root and ask it to list active instruction sources.
2. Confirm root `AGENTS.md` loads and the instruction map leads to the matching canonical rule.
3. Run `/skills` or mention `$finnts-validation` and confirm the generated `.agents/skills` mirror is available.
4. For GitHub review, confirm the `## Code Review Rules` section influences findings.

## Cursor Check

1. Open Customize > Rules and confirm the generated `.cursor/rules/*.mdc` files are recognized.
2. Mention a representative matched file and confirm the expected MDC adapter activates.
3. Confirm its `@.claude/rules/...` reference supplies the canonical rule and unrelated MDC rules remain absent.

Record a failed case as an instruction-discovery defect. Fix the path pattern, instruction specificity, or conflicting guidance before adding more always-on text.