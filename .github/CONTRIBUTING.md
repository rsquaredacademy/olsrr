# Contributing to olsrr

Thanks for helping improve olsrr. This guide covers bug reports, fixes, and
pull requests.

## Reporting a bug

- Search [existing issues](https://github.com/rsquaredacademy/olsrr/issues)
  before opening a new one.
- Every issue must include a reproducible example (a "reprex"). Use the bug
  report form and keep the example minimal. Build it with
  [reprex](https://reprex.tidyverse.org): `library(olsrr)`, a small inline data
  frame or a built-in dataset, the call, and its output.
- Include the full error or warning text, your `sessionInfo()` output, and the
  package version and install source (CRAN or a GitHub commit).
- Confirm the example fails in a fresh R session.

## How fixes are verified

Every fixing pull request adds a test built from the issue's example. Before
the issue is closed, the maintainer re-runs that example on the fix and posts
the output in the closing comment, together with the commit or pull request.

## Proposing a change

Open an issue first for anything beyond a typo or a small documentation fix.
Explain the problem and, for a new feature, show the proposed interface with an
example call and the alternatives you considered.

## Pull requests

- Branch from `develop`. `master` is only for releases.
- Add a bullet to `NEWS.md` describing the change.
- Add tests with [testthat](https://testthat.r-lib.org).
- Run `devtools::check()`; it should finish with no errors or warnings.
- Follow the [tidyverse style guide](https://style.tidyverse.org).

## Code of conduct

This project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating, you agree to abide by its terms.
