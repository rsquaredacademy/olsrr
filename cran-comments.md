# olsrr 0.7.1

This is a bug-fix release. It corrects residual mean square error and root mean
square error in `ols_regress()` and `ols_model_performance()` (now
`deviance(model) / df.residual(model)` and its square root), and fixes
information criteria (`ols_msep()`, `ols_fpe()`, `ols_apc()`, `ols_hsp()`,
`ols_mallows_cp()`, `ols_sbic()`, `ols_aic()`, `ols_sbc()`) to count parameters
with `model$rank`, to use the residual mean square error in `ols_msep()`, to
implement the Hurvich-Tsai correction for `ols_aic(corrected = TRUE)`, and to
return `NA` with a warning at boundary conditions. It also hardens stepwise
selection (intercept-only fallbacks, near-perfect fits, deterministic tie
breaking, no duplicated `include` variables, correct refits of transformed
terms and row names of filtered data), accepts factor terms in the `vars`
argument of `ols_test_f()`, `ols_test_score()` and `ols_test_breusch_pagan()`,
makes `xplorerr` a suggested package so the package no longer accesses the
network when attached, and fixes several audit findings in
`ols_test_bartlett()`, `ols_pure_error_anova()`, `ols_prep_avplot_data()`,
`ols_correlations()`, `ols_test_outlier()`, `ols_get_predictors()` and
`ols_test_breusch_pagan()`. See `NEWS.md` for the full list. No exported
function is removed and no default changes.

## Test environments

* local Windows 11 x64 (build 26300), R 4.6.1 (2026-06-24 ucrt)
* TODO: win-builder (R-devel and R-release)
* TODO: R-hub (Windows, macOS, Ubuntu)
* TODO: macOS (R-release)

## R CMD check results

`R CMD check --as-cran --run-dontrun` on the source tarball built from
`git archive HEAD`:

0 errors | 0 warnings | 3 notes

The notes are:

* `checking CRAN incoming feasibility ... NOTE`: `Version contains large
  components (0.7.0.9000)`. This is the development version; the released
  tarball is 0.7.1.
* `checking examples ... NOTE`: five `ols_step_both_*()` examples need
  6-10 seconds each (`ols_step_both_r2`, `ols_step_both_adj_r2`,
  `ols_step_both_aic`, `ols_step_both_sbic`, `ols_step_both_sbc`). The
  examples are unchanged in this release and the algorithm is unchanged.
* `checking HTML version of manual ... NOTE`: `Skipping checking math
  rendering: package 'V8' unavailable`. `V8` is not installed in this check
  environment; it is not a package dependency.

The `ols_launch_app()` example now runs only in interactive sessions, since it
launches a Shiny app.

## Reverse dependencies

TODO: revdepcheck
