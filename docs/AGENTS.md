# AGENTS.md — olsrr

R package `olsrr`: tools for building OLS regression models
(rsquaredacademy). Standard R package layout. Development happens on the
`develop` branch (CI runs on pushes/PRs to `develop` and `master`).

## Commands

Run from the package root in R:

`devtools``::`[`load_all`](https://devtools.r-lib.org/reference/load_all.html)`(``)`` ``# fast iteration`` ``devtools``::`[`test`](https://devtools.r-lib.org/reference/test.html)`(``)`` ``# all tests (testthat ed. 3)`` ``devtools``::`[`test`](https://devtools.r-lib.org/reference/test.html)`(``filter ``=`` ``"regress"``)`` ``# single file: matches test-{filter}.R`` ``devtools``::`[`document`](https://devtools.r-lib.org/reference/document.html)`(``)`` ``# regenerate NAMESPACE + man/`` ``devtools``::`[`check`](https://devtools.r-lib.org/reference/check.html)`(``)`` ``# full R CMD check before PR/release`

## Generated files — always edit the source

| Do NOT edit | Edit instead | Regenerate with |
|----|----|----|
| `NAMESPACE`, `man/*.Rd` | roxygen2 comments in `R/*.R` | [`devtools::document()`](https://devtools.r-lib.org/reference/document.html) |
| `README.md` | `README.Rmd` | knit README.Rmd |
| `docs/` | `_pkgdown.yml`, `vignettes/`, `man/` | pkgdown build site |

Roxygen uses Markdown syntax (`Roxygen: list(markdown = TRUE)`).

## Testing quirks

- Two test families per feature:
  - `test-<feature>.R` — behavior/equality tests.
  - `test-<feature>-output.R` — console-output tests using
    `expect_snapshot()`; snapshots live in `tests/testthat/_snaps/*.md`.
    If output intentionally changed, update with
    [`testthat::snapshot_accept()`](https://testthat.r-lib.org/reference/snapshot_accept.html)
    (or delete the `.md` entry and re-run).
- Visual regression: `test-visual.R` uses vdiffr with SVG doppelgangers
  in `tests/testthat/_snaps/visual/`. Plot functions take
  `print_plot = TRUE/FALSE`; tests capture the object via `p$plot` (or
  `plot(x, print_plot = FALSE)`). Accept changes with
  `vdiffr::accept_snapshots()` or delete the specific SVG and re-run to
  regenerate it.
- Most plot/snapshot tests use `skip_on_cran()`; they still run locally.
- Tests rely on built-in datasets (`surgical`, `stepdata`, `auto`, plus
  `mtcars`); don’t rename or reshape `data/*.rda`.

## Code conventions

- Every exported function is prefixed `ols_`. Internal helpers:
  `utils.R` (general), `output.R` (hand-rolled console tables built with
  [`cat()`](https://rdrr.io/r/base/cat.html) + `fc/fl/fg/fs` width
  helpers — not knitr/kable), `ols-error-messages.R`.
- Follow the tidyverse style guide; do not restyle code outside your
  change (per CONTRIBUTING.MD).
- User-facing changes need a bullet at the top of `NEWS.md` under
  `# olsrr (development version)` with your GitHub handle and issue/PR
  links.

## Dependencies & gotchas

- `Imports:` includes **xplorerr** (sibling rsquaredacademy package). It
  must be installed or `load_all()` fails; it powers the
  [`ols_launch_app()`](https://olsrr.rsquaredacademy.com/reference/ols_launch_app.md)
  Shiny app.
- Dev versions use four-component numbers (e.g., `0.7.0.9000`); bump on
  release.
- `revdep/`, `revdep_results/`, `revdep_sources/` hold committed
  revdepcheck artifacts (excluded from the R build via `.Rbuildignore`).
  Re-run
  [`revdepcheck::revdep_check()`](https://revdepcheck.r-lib.org/reference/revdep_check.html)
  before release and summarize in `cran-comments.md`; CRAN target is
  `0 errors | 0 warnings | 0 notes`.

## CI

GitHub Actions (`.github/workflows/`): R-CMD-check matrix
(macOS/Windows/Ubuntu, R devel/release/oldrel-1), test-coverage
(codecov), revdepcheck, rhub. A green local
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
is the bar before opening a PR.
