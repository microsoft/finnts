# CLAUDE.md — Claude Code project memory for finnts

@AGENTS.md

## Claude-specific working agreements
- Start by summarizing the plan (bullets) and the files you’ll touch.
- Prefer running `devtools::test()` after code changes, and `devtools::check()` before finalizing.
- If you change roxygen comments, run `devtools::document()` and include generated diffs.
- Keep changes minimal: avoid drive-by refactors unless explicitly requested.
- For multistep changes, test all supported date frequencies plus exact submodel routing, training rows, and lag-feature eligibility; the required daily multistep matrix runs in the standard suite.
- Keep CRAN test runs deterministic and under the documented runtime budget. Preserve all existing `skip_on_cran()` markers, including non-network tests. Non-CRAN runs automatically execute all credentialed provider and agent stress tests.
- Reuse directly fitted workflows or isolated artifact fixtures in downstream tests instead of repeating full forecast pipelines.
- If a request is ambiguous, ask the smallest number of clarifying questions needed, then proceed conservatively.

## Notes
- Agent code must treat `agent_info$llm` as an immutable Chat template and create a fresh, empty-history session for every graph that can access the LLM, including EDA, forecast update, each time-series combo, and Q&A. Parallel code must clone inside the series workflow after `foreach` dispatch, preserve the ellmer 0.4+ serialization path, and avoid internal ellmer APIs or provider credential mutation.
- PSOCK tests under `devtools::test()` may need a test-only `.export` for internal helpers because `pkgload::load_all()` differs from an installed namespace. Verify the installed package in a temporary library before adding `.export` to production code.
- If this repo grows, move large topic rules into `.claude/rules/*.md` (testing, style, release).
