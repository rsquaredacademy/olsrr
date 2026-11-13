# ofx8 - olsrr fix round 8 (Claude Sonnet)

Base `bd0fc88`. Findings from `ofxx7-reaudit.md`: N01, N02 (High), N03, N04 (Medium).

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| N01 fallback bypasses guard | `test-regress.R` "changed data, levels and unrelated bindings are rejected (N01, N02)" | `R/utils.R` `ols_model_data()`: when call data cannot be found the stored frame is returned only if every term variable is a plain symbol; transformed terms/offsets stop with the refit message | `log(mpg)` fit, data removed, unrelated `mpg` bound: error. Plain-variable fit with data removed: same sbetas as before | Fixed |
| N02 factor coding unchecked | same test | `model_data_valid()` rebuilds with `model.frame()` (no `xlev`) and `model.matrix(contrasts.arg = model$contrasts)` and requires `identical()` model matrix (values and column names) | releveled `g` errors for `ols_regress()` and `ols_step_both_p()`; stored fit gives sbetas 0.0081, -0.2885, -1.2829, 0.2582, 0.7461 | Fixed |
| N03 false rejections | `test-regress.R` "the data guard accepts ordinary unchanged fits (N03)" | Expression-by-expression guard replaced by `model_data_valid()`: response, model matrix, `model.weights`, `model.offset` of the rebuilt frame vs the fit's own, only evaluated objects compared | NULL weights/offset symbols, subset with external vectors, poly/scale with subset and na.exclude, three formula offsets plus call offset (-0.823439068414872, ...), factors, transformed terms all succeed with the audit's base values | Fixed |
| N04 both-p include-all crash | "both-p selection with every term included returns the model (N04)" | `R/ols-stepwise-regression.R`: returns the included model with empty metrics (same as the other routines) | returns class `ols_step_both_p`, coefficients and response equal the input fit | Fixed |

Compatibility: `ols_regress()$sbetas` and all six selection routines (coefficients and metrics) are `identical()` to a36c0b3 on eight fits (interaction, `.`, surgical, hsb, no intercept, log terms, factor, offset). The earlier-test expectation "could not be evaluated" for a fit whose data is gone is now the refit message.

User-visible: fits whose call data is missing and whose formula has transformed terms or offsets now stop with the refit message in `ols_regress()` for interaction/standardized betas and in selection (previously they re-evaluated the terms in the formula environment). In NEWS.

## Checks (after the last code commit 190f36f)

- `devtools::test()`: FAIL 0 | WARN 0 | SKIP 0 | PASS 2483.
- Clean `git archive` build and `R CMD check --as-cran --no-manual`: 0 errors | 0 warnings | 1 NOTE (CRAN incoming: maintainer address change, pre-existing and intended).
- `190f36f` only restores `ignore_attr = TRUE` arguments that a sed in `c1d4d68` removed from three existing assertions.
