# FinnTS Validation Matrix

Choose the smallest check that can falsify the current change, run it immediately after the first edit, then broaden validation according to risk.

When a file matches multiple scoped rules, follow all matching rules. No rule takes precedence by file order.

| Changed area | First focused check | Required follow-up |
| --- | --- | --- |
| Combo identity or input validation | `R -q -e 'devtools::test(filter = "combo-normalization|prep_data")'` | Full tests when artifact naming or hierarchy behavior changes |
| Agent sessions or parallel serialization | `R -q -e 'devtools::test(filter = "agent-chat-serialization")'` | Agent-focused tests, then full tests |
| Agent reasoning, duplicate runs, retries, or finalization | `R -q -e 'devtools::test(filter = "agent-duplicate-runs|agent-graceful-abort|finalize_run")'` | CRAN profile, then full tests |
| Agent EDA prompt summaries | `R -q -e 'devtools::test(filter = "agent-eda-summaries")'` | Agent-focused tests |
| Seasonal-period behavior | `R -q -e 'devtools::test(filter = "prep_models|agent-duplicate-runs|agent-graceful-abort")'` | Multistep tests if model construction changes, then full tests |
| Multistep fitting, prediction, lags, or feature selection | `R -q -e 'devtools::test(filter = "multistep")'` | Full tests; retain the daily matrix and all date frequencies |
| Feature-selection optional packages | `R -q -e 'devtools::test(filter = "vip-optional|summarize-models-feature-selection")'` | CRAN-style check without feature-selection packages |
| TimeGPT or `nixtlar` | `R -q -e 'devtools::test(filter = "timegpt")'` | Core check without `nixtlar`; run live tests only when credentials are available and not on CRAN |
| New or changed provider integration test | Inspect test setup for an early `skip_on_cran()` and non-printing credential checks | Run with `NOT_CRAN=false`, then without credentials; run live only when credentials are available |
| Public API or roxygen | Test the changed exported function and its direct callers | `R -q -e 'devtools::document()'`, inspect generated changes, then full tests |
| Dependencies or package metadata | Relevant focused test | Clean-library install and `R -q -e 'devtools::check()'` |
| Instruction or Agent-guidance files | `Rscript tools/generate-agent-adapters.R`, then `Rscript tools/validate-agent-guidance.R` | Inspect the guidance-only diff; package tests are unnecessary unless package code also changed |

## Standard Profiles

- Full tests: `R -q -e 'devtools::test()'`
- Deterministic CRAN profile: `R -q -e 'withr::with_envvar(c(NOT_CRAN = "false"), devtools::test())'`
- CRAN-style check without suggested packages: `R -q -e 'withr::with_envvar(c(`_R_CHECK_FORCE_SUGGESTS_` = "false", NOT_CRAN = "false"), devtools::check())'`
- Full package check: `R -q -e 'devtools::check()'`

On Windows without R on `PATH`, run expressions with `./tools/run-r.ps1 -Expression '<R expression>'` and scripts with `./tools/run-r.ps1 -File '<script path>'`.

Do not run live provider tests on the CRAN profile. Do not report a skipped credentialed test as a successful live integration test.
