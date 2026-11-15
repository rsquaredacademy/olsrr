# ofx5 - olsrr fix round 5 (Claude Sonnet)

Worktree `task/olsrr-fix-071`, base `1ac47a9`. Findings from `ofxx4-reaudit.md`
(K01 High, K02 Medium, K03 High), applied with the convergence rule: errors
over heuristics, no tolerance tuning. One commit (`6e750e4`) covers all three.

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| K01 - tolerance re-evaluated in the wrong environment | `test-weighted-diagnostics.R` "fits without a QR are rejected, never reconstructed (K01)" (audit's separate-caller `tv` case; replaces the old J01 fallback tests) | `R/utils.R` `model_hatvalues_frame()`: the design-matrix fallback and `model_fit_tol()` are removed. A fit without `qr` stops with "refit with qr = TRUE" (rank-0 fits give zero leverage). | Stored-QR PRESS 246.357079950998 equals explicit leave-one-out fits with `tol = .01`; the `qr = FALSE` twin (plain and weighted) errors. | Fixed |
| K02 - Cook/DFFITS thresholds count aliases | `test-plot-data.R` "Cook's D and DFFITS thresholds count rank (K02)" | `R/ols-plots-data.R` `ols_cooks_ts()` uses `model$rank - 1` (n - rank = residual df); `R/ols-dffits-plot.R` uses `model$rank` | DFFITS threshold 2*sqrt(2/20), outlier obs 20 retained for the aliased model, both options; Cook types 1-5 equal for estimable and aliased fits (type 2 = 4/29, type 4 = 1/29); seed-714 case stays at 4/18 | Fixed |
| K03 - unit-leverage rows get finite statistics | `test-plot-data.R` "unit-leverage rows keep undefined influence statistics as in base R (K03)" | `R/utils.R` new `model_influence_basis()` applies base R's `lm.influence()` conventions (h > 1 - 10 eps set to 1; rounding-error residuals set to 0); all used-row statistics, `model_sigma_loocv_used()` and DFBETAS use it. `R/ols-plots-data.R`: `na.rm = TRUE` in range calculations | Cook's D, rstandard, rstudent, DFFITS equal base R (NaN at obs 7), DFBETAS 0, for the singleton factor level, weighted fit and isolated indicator column | Fixed |

## Checks

- `devtools::test()`: FAIL 0 | WARN 0 | SKIP 0 | PASS 2418 (2387 before).
- Clean `git archive` build, `R CMD check --as-cran --no-manual`: 0 errors | 0 warnings | 2 NOTEs. NOTE 1 (CRAN incoming): maintainer address change (old gmail to rsquaredacademy.com), pre-existing and intended. NOTE 2 was `no visible global function definition for 'median'` from the new helper; fixed afterwards with `stats::median` (one-line change, commit `(ofx5)` follow-up; plot-data tests re-run green, full check not repeated).

## Deferred / notes

- The standard DFFITS cutoff formula `2*sqrt((k+1)/(n-k-1))` is unchanged apart from using the rank for k; its form was not part of the finding.
- Other `length(coefficients(model))` uses (`check_npredictors`, `check_lfit`, `ols_pure_error_anova` guard, `ols_prep_rvsrplot_data` predictor count) are model-size gates or plot labels, not diagnostic thresholds; left alone.
- User-visible change: `qr = FALSE` fits now error in PRESS/predicted R-squared/leverage helpers (`ols_regress()` itself refits with a QR first). NEWS updated.
- On a unit-leverage row the plot colour is `NA` (undefined), not "normal".
