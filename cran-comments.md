# olsrr 0.7.1

This is a bug-fix release. It corrects the residual mean square error and the
information criteria (parameter counts, the corrected AIC), hardens stepwise
selection, fixes the Cramér-von Mises normality test (it compared with the
uniform distribution), and skips normality tests outside their sample-size range
instead of failing. The package no longer accesses the network when attached:
`xplorerr` moves to Suggests, and `goftest` is no longer imported. No exported
function is removed and no default changes. See `NEWS.md` for the full list.

## Test environments

* local Windows 11 x64 (build 26300), R 4.6.1 (2026-06-24 ucrt)
* TODO: win-builder (R-devel and R-release)
* TODO: R-hub (Windows, macOS, Ubuntu)
* TODO: macOS (R-release)

## R CMD check results

`R CMD check --as-cran` on the source tarball:

0 errors | 0 warnings | 1 note

The note is local: math rendering in the HTML manual was not checked because
the optional `V8` package is not installed here.

The `ols_launch_app()` example now runs only in interactive sessions, since it
launches a Shiny app.

## Reverse dependencies

TODO: revdepcheck
