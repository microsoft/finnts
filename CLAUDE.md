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
- Agent duplicate-run checks normalize effective `NULL` defaults and sort order-insensitive multipart settings before comparison while preserving raw run-log values.
- Keep custom `seasonal_period` handling in `prep_models()`: validate one to three unique finite values above 1, forward them to the STLM/TBATS workflows, and leave default `NULL` values as `NA` in raw run logs.
- Keep combo identity canonicalization shared between `set_agent_info()` and `prep_data()`: trim only the boundaries of character combo values before validation and artifact naming, preserve internal spaces/NA/numeric values, reject blank or colliding normalized combo/date values, and never implement this by trimming generic `hash_data()` inputs.
- Keep agent EDA prompts finite and explicit: all-missing outlier dates render as absent, and all-missing/non-finite distance-correlation groups do not enter regressor rankings.
- Keep `nixtlar` optional in `Suggests`: TimeGPT requires R 4.1 or newer, while core FinnTS workflows must remain installable and loadable on R 4.0 without it.
- PSOCK tests under `devtools::test()` may need a test-only `.export` for internal helpers because `pkgload::load_all()` differs from an installed namespace. Verify the installed package in a temporary library before adding `.export` to production code.
- If this repo grows, move large topic rules into `.claude/rules/*.md` (testing, style, release).
