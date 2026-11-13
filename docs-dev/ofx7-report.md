# ofx7 - olsrr fix round 7 (Claude Sonnet)

Base `a36c0b3`. Finding M01 from `ofxx6-reaudit.md` (High, inherited).

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| M01 - call data re-evaluated without identity check (standardized betas, six selection routines) | `test-regress.R` "auxiliary refits reject call data that differs from the fit (M01)": audit's `local()` doubled-response case, response changed after fitting, predictor reversed after fitting, all six selection functions (forward/backward/both x AIC/p), include-all identity, hand-computed sbetas | `R/utils.R` `ols_model_data()` now calls `model_data_matches()`: every term variable, the `weights` argument and the `offset` argument evaluated in the found data must equal the fit's stored model frame exactly (no tolerance, no row-name heuristic), else error "the data used to fit this model has changed or cannot be found; refit the model." | Doubled/changed/reversed data error; unchanged fit gives sbetas -0.6707611747757308, -0.3470564211217647, 0.2431089991525895 (= hand computation from `model.frame()`); the full existing suite (selection, sbetas, offsets, weights, contrasts) passes unchanged, so ordinary fits and package examples are unaffected | Fixed |

Notes: when the call data cannot be found at all, `ols_model_data()` still falls back to the stored `model$model` (the fit's own observations), as before. Sbetas are not computed from `model.frame()` directly; the guard was chosen to keep calling-style and transformed-term handling unchanged.

## Checks (after the last code commit e8b91cd)

- `devtools::test()`: FAIL 0 | WARN 0 | SKIP 0 | PASS 2462.
- Clean `git archive` build and `R CMD check --as-cran --no-manual`: 0 errors | 0 warnings | 1 NOTE (CRAN incoming: maintainer address change, pre-existing and intended).
