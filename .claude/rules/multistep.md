---
paths:
  - "R/multistep_*.R"
  - "tests/testthat/test-multistep*.R"
---

# Multistep Model Rules

- Automatic MARS tuning must exclude `prune_method = "cv"`. Explicit multistep CV pruning must supply a bounded `nfold` value.
- Cover every supported `date_type` and assert exact horizon-to-submodel routing, training rows, lag-feature eligibility, and forecast-row identity.
- Run the daily multistep matrix after changing lag generation, feature selection, multistep fitting, or prediction. It is part of the standard test suite.
- Preserve both daily paths: all six adapters with explicit lags and outlier cleaning, plus GLMnet with automatic lags and raw data.
- Keep model-specific fit and predict implementations aligned with the shared routing behavior in `R/multistep_helper.R`.
