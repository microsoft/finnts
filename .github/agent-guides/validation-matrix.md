# FinnTS Validation Matrix

Choose the smallest check that can falsify the current change, run it immediately after the first edit, then complete the checks in the selected scope.

Follow [AGENTS.md's validation scope](../../AGENTS.md#validation-scope). Interactive development uses targeted checks by default. The full-scope follow-up column below is not an instruction to override a targeted-only request: select affected tests and necessary documentation checks, and record full-suite/package checks as not run by approved scope. Seek approval before expanding coverage. Release qualification and explicit full-validation requests still require full checks.

When a file matches multiple scoped rules, follow all matching rules. No rule takes precedence by file order.

| Changed area | First focused check | Full-scope follow-up |
| --- | --- | --- |
| Combo identity or input validation | `R -q -e 'devtools::test(filter = "combo-normalization|prep_data")'` | Full tests when artifact naming or hierarchy behavior changes |
| Agent sessions or parallel serialization | `R -q -e 'devtools::test(filter = "agent-chat-serialization")'` | Agent-focused tests, then full tests |
| Agent reasoning, duplicate runs, retries, or finalization | `R -q -e 'devtools::test(filter = "agent-duplicate-runs|agent-global-resume|agent-graceful-abort|finalize_run")'` | CRAN profile, then full tests |
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

## Global Custom-Model Regressions

`tests/testthat/helper-custom-model-global.R` supplies synthetic fixed-seed panels,
test-only source counterparts and independent arithmetic, explicit-matrix and
root-solver references. These helpers are not a production model catalog and
must never replace generated source or supply expectations from candidate output.

- `test-custom-model-global.R`: ten pooled rule families; series counts and
	cutoffs; peer sensitivity; calendar/fiscal and category features; exact errors;
	ordering, leakage, RDS, cohort scope, resampling and bounded sampling.
- `test-custom-model-authoring.R`: provider diagnostic privacy, prompt/schema
	contracts and pooled demonstrations.
- `test-custom-model-drafts.R`: fixed caller examples, bounded repair attempts,
	manual consent, exact saved identity and current-format continuation.
- `test-custom-model-runtime.R`: adapter contracts and executable demonstrations
	under the current contract, including explicit rejection of obsolete formats.

Focused offline entrypoint:

```r
withr::with_envvar(c(NOT_CRAN = "false"),
	devtools::test(filter = "^custom-model-global$"))
```

After changing authoring, cohort or runtime contracts, also run the affected
`custom-model-(authoring|drafts|validation|runtime|contract|standard)` files with
the unchanged 90-second file and 600-second aggregate limits. Run the global
worker replay test from a temporary installed namespace: its normal development
skip is not worker validation. Existing release/full-check requirements remain.

Peer tests must use a perturbation capable of changing shared coefficients:
uniform target scaling can be absorbed by a regional intercept in a log model.
Allocation checks compare every result to the reference and preserve totals/caps;
they do not require a binding-cap row to move. Preserve literal oracle checks,
negative controls, old assertions and CRAN skips when extending this library.

Live low/medium LLM benchmarks remain separate, explicitly authorized evidence.
Freeze inputs, expected values, code and source identities before each benchmark;
retain every failed attempt without replacement. Deterministic source-counterpart
tests establish engine capability, not live LLM success or arbitrary correctness.
