# olsrr 0.7.1

This is a bug-fix release. It adds support for weighted linear models in
`ols_regress()` and guards unsupported diagnostics against weighted models, and
corrects predicted R-squared and PRESS for weighted models. It also aligns the
residual mean square error (MSE/RMSE) with ANOVA residual mean square, corrects
parameter counts and Hurvich-Tsai AICc in information criteria, hardens stepwise
selection, fixes the Cramér-von Mises normality test (it compared with the
uniform distribution), and skips normality tests outside their sample-size range
instead of failing. The package no longer accesses the network when attached:
`xplorerr` moves to Suggests, and `goftest` is no longer imported. No exported
function is removed and no default changes. See `NEWS.md` for the full list.

## Test environments

* local Windows 11 x64 (build 26300), R 4.6.1 (2026-06-24 ucrt)
* win-builder (R-devel and R-release), 2026-10-27: 0 errors | 0 warnings | 0 notes
* R-hub (Windows, macOS, Ubuntu): to run
* macOS (R-release): to run

## R CMD check results

`R CMD check --as-cran --no-manual` on the source tarball (2026-11-01):

0 errors | 0 warnings | 1 note

The note is the CRAN incoming feasibility check recording the maintainer email
change from `hebbali.aravind@gmail.com` to
`aravindhebbali@rsquaredacademy.com`. This is the same maintainer, Aravind
Hebbali; the address in `DESCRIPTION` is the one the maintainer asked us to use.

The `ols_launch_app()` example runs only in interactive sessions, since it
launches a Shiny app; the other examples, including those in `\donttest{}`, run
in the check.

## Reverse dependencies

We checked 6 reverse dependencies (AFR, HanStat, NumericEnsembles, pprof, SQI,
xplorerr), comparing R CMD check results across CRAN and dev versions of this
package.

* We saw 0 new problems
* We failed to check 0 packages
