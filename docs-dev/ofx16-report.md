# pkg-olsrr-fx16 — olsrr 0.7.1 re-audit leftovers

2026-11-16. Worktree `F:/R/Packages/statistics/olsrr-wt/fx15`, branch `task/olsrr-fx15`, commits `ea5ddc2` + `e1ff5f9` (round 15). Spec: `docs-dev/pkg-olsrr-rx15-audit.md`; common rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`. Fix commit `eb33fd5` on top of `e1ff5f9`. R 4.6.1, Windows 11, ggplot2 4.0.3. Latest `R CMD check` result: 0 errors, 0 warnings, 1 NOTE (CRAN incoming feasibility / maintainer change only).

## Findings fixed

| id | test added | fix (file:line) | verified how | status |
|---|---|---|---|---|
| **M1** | `tests/testthat/test-bugfix-ofx16.R`: "residual vs regressor uses the used rows of an na.exclude fit (M1)" | `R/ols-residual-vs-regressor-plot.R:31` takes `model_residuals_used(model)` instead of `residuals(model)` | Audit reprex (`d$hp[c(3,9)] <- NA`, `na.action = na.exclude`) now returns 30 points; x equals the used `wt`; y equals both the used-row residuals of the equivalent `lm()` and `model$residuals` (max difference 0). `residuals(model)` is verified to be padded to 32. Test errors on `e1ff5f9` and on `46e3255`. | FIXED |
| **L1** | Same file: "residual histogram…", "the diagnostics panel…", "residual plus component…" (three tests) | `R/ols-residual-histogram.R:46`; diagnostics data helpers `R/ols-residual-vs-predicted-plot.R:47-48`, `R/ols-observed-vs-predicted-plot.R:55-57`, `R/ols-residual-qqplot.R:22`, `R/ols-residual-boxplot.R:23`, `R/ols-plots-data.R:284,299`; comp-plus-resid `R/ols-residual-plus-component-plot.R:73` | Audit fixture: histogram returns 30 residuals and its normal curve still equals `dnorm * n * bar width`; diagnostics returns all 10 panels with 30 rows each; QQ/fitted/observed/fit-mean/spread/histogram/box data all equal the refit's used-row values; comp-plus-resid returns 3 plots with 30 aligned pairs, each `y = used residual + beta * used x`. No error and no recycling warning (`expect_no_warning`). Fails on `e1ff5f9` and `46e3255`. | FIXED |
| **L2** | `tests/testthat/test-step-forward-saturated.R`: asserts `k$metrics$variable` and `a$metrics$variable` equal `x13, x23, x2, x16, x18, x22, x27, x29` | test only | The sequence passes on `e1ff5f9` (13/13) and the file has 7 failures on `46e3255`, so the assertion is tied to the B10 fix. | FIXED |
| **L4** | `tests/testthat/test-bugfix-ofx16.R`: "aliased predictors are not counted as estimated terms (L4)" | `R/ols-stepwise-utils.R:891` (`model_estimable_terms()`); used at `R/ols-all-possible-regression.R:408` and `R/ols-best-subsets-regression.R:166` | `wt2 = wt`: row `wt wt2 hp` now has `n = 2` (was 3), with unchanged R² 0.8267855 and AIC 156.6523 (equal to `lm(mpg ~ wt + hp)`); row `wt wt2` has `n = 1`; ordinary rows unchanged (`wt hp` = 2, `wt` = 1); multi-column `factor(cyl)` still counts once; no-intercept `0 + wt + wt2 + hp` reports 2. Best subsets now has 2 rows (max `n` 2) instead of 3. Fails on `e1ff5f9` and `46e3255`. | FIXED |
| **L3** | `tests/testthat/test-bugfix-ofx16.R`: "VIF warns for a model without an intercept (L3)"; existing M4 test in `tests/testthat/test-collindiag.R` now expects the warning | `R/ols-collinearity-diagnostics.R:127` | `lm(mpg ~ 0 + wt + hp)`: warning "VIFs may not be sensible in a model without an intercept."; tolerance and VIF are still the unchanged centred-auxiliary values (0.5660512, VIF 1.766625 each). Fails on `e1ff5f9` and `46e3255` before the change. | FIXED |

## Audit reprex output after the fix

```text
M1 rows: 30  max|y - used resid|: 0
padded residuals length: 32
hist rows: 30
diag plots: 10  rows: 30,30,30,30,30,30,30,30,30,30
cpr plots: 3  rows: 30,30,30
L3 warning: VIFs may not be sensible in a model without an intercept.
  Variables Tolerance      VIF
1        wt 0.5660512 1.766625
2        hp 0.5660512 1.766625

  n predictors   rsquare      aic
1 1         wt 0.7528328 166.0294
2 1        wt2 0.7528328 166.0294
5 2      wt hp 0.8267855 156.6523
4 1     wt wt2 0.7528328 166.0294
7 2  wt wt2 hp 0.8267855 156.6523
best subset:
  n predictors   rsquare      aic
1 1         wt 0.7528328 166.0294
2 2      wt hp 0.8267855 156.6523
```

## Checks

- `devtools::test()` with `NOT_CRAN=true`: all green. No failures, errors or warnings in `.ofx16-tmp/test-full.log`.
- New/edited tests against the installed `e1ff5f9` library (`.pkg-olsrr-rx15-tmp/lib`): `test-bugfix-ofx16.R` reaches the 10-failure cap (M1, L1, L3, L4 all fail); `test-step-forward-saturated.R` passes 13/13.
- Same tests against the `46e3255` library (`.pkg-olsrr-rx15-tmp/lib0`): `test-bugfix-ofx16.R` reaches the 10-failure cap (M1 error `0, 32`, L1 errors/failures, L3 no warning, L4 counts); `test-step-forward-saturated.R` has 7 failures.
- Clean archive build: `git archive HEAD | tar -x -C .ofx16-tmp/build`, `R CMD build .`, then `R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz` through `heavy-slot.sh pkg-olsrr-fx16`.
  - Final run (`check2.log`): `Status: 1 NOTE`; examples 48 s, donttest 147 s, tests OK, vignettes rebuilt OK. The only NOTE is CRAN incoming feasibility (maintainer e-mail change) — the same NOTE as round 15.
  - An earlier run of the same tarball had a second NOTE from a loaded machine (`ols_step_forward_sbic` 5.72 s > 5 s). The clean rerun had all examples under the threshold, so this was queue load, not a package change.

## Commands that changed something

```text
git add <the 15 edited files + tests/testthat/test-bugfix-ofx16.R>
git commit -m "fix: na.exclude diagnostics, aliased subset count and no-intercept VIF warning (ofx16)"
```

`R/ols-observed-vs-predicted-plot.R`, `R/ols-residual-vs-predicted-plot.R`, `R/ols-residual-qqplot.R`, `R/ols-residual-boxplot.R` and `R/ols-plots-data.R` were changed only in the data helpers used by the diagnostics panel; all ordinary fits use the same rows as before, which the unchanged snapshot/visual tests confirm.

## Deferred

- **L5** (pre-existing, not in this job brief): the R-pre-4.6 variants of `tests/testthat/_snaps/bp-output.md` and `tests/testthat/_snaps/visual/diag-9.svg` are still not regenerated, and the inactive `ggplot2-histogram.svg` harness issue from the ofx15 report remains open. No code is affected on R 4.6.1.
- Nothing else from the re-audit is deferred: the report's only other items were the B1–B10 checks, which stayed fixed and are covered by the existing tests.

## Not done

- Nothing in the brief was left undone. No API change and no new dependency. `ols_vif_tol()` now emits a warning for no-intercept models, matching `car::vif()`; that is the only intentional user-visible behaviour change beyond the crash fixes and the label count.
