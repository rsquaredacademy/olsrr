# pkg-olsrr-c25-fu — follow-up to the C25 audit

2026-11-19. Worktree `F:/R/Packages/_worktrees/olsrr-c25`, branch `task/olsrr-c25`.
Parent: `docs-dev/pkg-olsrr-c25-aud-report.md` (audit of commit `355c9bf`) and the
package audit stop rule `F:/pm/notes/package-audit-stop-rule.md`.
Scope: only the two Low findings, L1 (NEWS bullet) and L2 (constant-sample error).
No version bump, no branch switch, nothing pushed, no GitHub used. One commit.

Baseline: `HEAD` = `0e16ef5` ("Audit pkg-olsrr-c25 build"), which is `355c9bf`
plus the committed audit report. Working tree clean before the change.

## Files changed

| File | Change |
|---|---|
| `NEWS.md` | L1: rewrote the `#253` bullet to name `ols_plot_fm()`, `ols_plot_resid_spread()`, both panels of `ols_plot_resid_fit_spread()` and panels 7/8 of `ols_plot_diagnostics()`. L2: new bullet for the constant-sample error. Dev section only; version stays 0.7.1. |
| `R/ols-normality-tests.R` | L2: guard in `ols_test_normality.default()` after the "too small for every test" error and before the skip warning: `if (n > 0L && all(y == y[[1L]])) stop("y has no variation: ...")`. |
| `tests/testthat/test-normality.R` | L2: new `test_that("a constant sample errors with a message naming the input")`, plus a near-constant regression check. |
| `docs-dev/pkg-olsrr-c25-fu-report.md` | This report. |

`git diff --stat` for the code and NEWS changes (the report is a new file):

```
 NEWS.md                         |  3 ++-
 R/ols-normality-tests.R         | 13 +++++++++++++
 tests/testthat/test-normality.R |  9 +++++++++
```

No generated file (`NAMESPACE`, `man/`, `docs/`) was touched; no test snapshot
changed. The full-suite run left `git status` clean apart from these four files.

## L1 — NEWS bullet names every function whose plot title changed

**Before** (`NEWS.md`, dev section):

> - `ols_plot_diagnostics()` titles panels 7 and 8 "Fit - Mean" and "Residual"
>   instead of "Residual Fit Spread Plot" for both, matching the two panels of
>   `ols_plot_resid_fit_spread()` ([#253](...)).

This names only `ols_plot_diagnostics()` although the shared helper
`ols_plot_rfs()` changes the title in four user-facing functions.

**After** (`NEWS.md`, `# olsrr (development version)` → `## Bug Fixes`):

> - `ols_plot_fm()`, `ols_plot_resid_spread()`, both panels of
>   `ols_plot_resid_fit_spread()` and panels 7 and 8 of
>   `ols_plot_diagnostics()` now title the fit-mean panel "Fit - Mean" and the
>   residual panel "Residual" instead of "Residual Fit Spread Plot" for both
>   ([#253](https://github.com/rsquaredacademy/olsrr/issues/253)).

Behaviour confirmation (unchanged by this follow-up; ran on the fixed tree):

```
ols_plot_fm:               Fit - Mean
ols_plot_resid_spread:     Residual
resid_fit_spread fm:       Fit - Mean
resid_fit_spread rsd:      Residual
```

For reference, the audit recorded `origin/develop` titles as
"Residual Fit Spread Plot" for both, and HEAD as "Fit - Mean" / "Residual", so
the NEWS text now matches the four changed functions. No code or test changed
for L1: the titles are already asserted in
`tests/testthat/test-diagnostics-panel-titles.R` (both tests, 5 + 4
expectations), which passes on HEAD.

## L2 — constant sample in `ols_test_normality()`

**Before** (`ols_test_normality(rep(3, 10))` on HEAD):

```
ERROR: missing value where TRUE/FALSE needed
```

The error comes from `nortest::lillie.test()`'s internal
`if (pvalue > 0.1)` with an `NA` comparison — it names neither the input nor the
problem.

**After** (same call on the fixed tree):

```
ERROR: y has no variation: all 10 non-missing values are identical, so the normality tests are undefined.
```

Details:
- The guard is placed after the existing "at least 3 observations are required"
  error (so `n = 0/1/2` still gets the sample-size error) and before the
  skipped-tests warning (so a constant sample of 5–7 values does not emit a
  misleading "test skipped" warning before the error).
- Exact equality (`all(y == y[[1L]])`) is used, not a tolerance, so ordinary
  small-scale data are never rejected. Regression check:
  `ols_test_normality(c(rep(3, 9), 4))` still runs all four tests silently.
- The same guard covers `ols_test_normality.lm()` (it passes residuals to the
  default method).

**Test added** (`tests/testthat/test-normality.R`):

```r
test_that("a constant sample errors with a message naming the input", {
  # nortest::lillie.test() fails inside its own `if (NA)` on zero variance, so
  # the input is rejected before any test runs
  expect_error(ols_test_normality(rep(3, 10)), "y has no variation")

  # a sample that varies, even by one value, still runs every test it can
  expect_silent(ols_test_normality(c(rep(3, 9), 4)))
})
```

**Proof the test fails on HEAD.** With only `R/ols-normality-tests.R` stashed
(`git stash push -- R/ols-normality-tests.R`; test and NEWS kept), running
`devtools::test(filter = "normality")`:

```
Error ('test-normality.R:176:3'): a constant sample errors with a message naming the input
Error in `if (pvalue > 0.1) { ... }`: missing value where TRUE/FALSE needed
[ FAIL 1 | WARN 0 | SKIP 0 | PASS 64 ]
```

Then `git stash pop` restored the fix. With the fix in place the same command
gives:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 66 ]
```

## Test totals

`devtools::test()` on the fixed tree (progress reporter, R 4.6.1, Windows,
`NOT_CRAN` set by devtools), script file `run_tests.R`:

```
Duration: 334.9 s
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3615 ]
```

- 57 files, 436 tests (the audit's 435 tests plus the new one), 3615 passing
  expectations (audit: 3613; the new test adds 2 expectations).
- No warnings, no skips, no snapshot changes (`git status` clean afterwards).
- Filtered normality-file runs: `FAIL 1 | WARN 0 | SKIP 0 | PASS 64` without the
  fix, `FAIL 0 | WARN 0 | SKIP 0 | PASS 66` with it.

## Commands run

```powershell
git status; git branch --show-current; git log --oneline -3
git show --stat HEAD; git show --stat HEAD~1
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/reprex.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/run_tests.R normality
git stash push -m "olsrr-c25-fu L2 fix (proof run)" -- R/ols-normality-tests.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/run_tests.R normality   # FAIL 1, PASS 64
git stash pop
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/run_tests.R normality   # FAIL 0, PASS 66
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/run_tests.R             # full suite
git status --short; git diff
```

Scratch files (outside the repository) are in
`C:/Users/DELL/AppData/Local/Temp/opencode/olsrr-c25-fu/`:
`reprex.R` (before/after output), `run_tests.R` (devtools::test() wrapper),
`fail_before_fix.txt` (the stashed-fix run), `full_tests.txt` (the full suite).
`pass_after_fix.txt` is an intermediate run from before the final test file was
settled; the final filtered result is the one quoted above and was re-run after
the last edit.

One harness note: a run through `testthat::test_dir()` without `NOT_CRAN=true`
skips the vdiffr tests and then deletes the unmatched `_snaps/visual/*.svg`
files as "unused snapshots". That happened once here; `git checkout --
tests/testthat/_snaps/visual` restored them and the logged runs use
`devtools::test()`, which sets `NOT_CRAN=true`. Worth avoiding `test_dir()`
directly in this package.

## Follow-ups (not fixed here)

1. **`ols_test_normality.lm()` on a model with (near-)zero residuals still
   reaches `lillie.test()`.** The exact-equality guard does not fire when the
   residuals differ in the last bits: `lm(rep(3, 10) ~ 1)` gives residuals
   `-1.263900e-15` and `1.404333e-16`, so the call still fails with
   `missing value where TRUE/FALSE needed`. Decide whether a perfectly fitting
   or saturated model should get a clearer message; a tolerance-based guard was
   rejected here because it could reject legitimate tiny-scale data. Backlog
   item, with a reprex.
2. **The L2 message always says `y`, including through the `lm` method.** That
   names the argument, as the brief asks, but a model user sees "y has no
   variation" for residuals. Using the deparsed input (`dname`) for the vector
   method would read better; left as-is to keep the message stable and short.
3. **`?ols_test_normality` does not yet state the constant-sample restriction.**
   The audit stop rule asks each package to state its supported domain in the
   help pages. `man/` was not regenerated here because `DESCRIPTION` has no
   `RoxygenNote` and roxygen2 8.1.1 would add one (plus possibly restyle other
   `man/` files) — a docs pass should sweep the whole domain wording at once.
4. **42 zero-byte vdiffr SVGs (audit I4) still leave the stand-alone fit spread
   plots unchecked visually** (e.g. `fm-plot.svg`, `rsd-plot.svg`,
   `residual-fit-spread-plot*.svg`). The title change is covered by
   `test-diagnostics-panel-titles.R`, the rendered figures are not. Needs a
   snapshot regeneration pass.
5. **The generated `docs/` pkgdown site is still stale** (audit follow-up 3);
   rebuild with the pkgdown workflow before release.
6. **`docs-dev/` naming:** this round used the brief's `<id>`
   (`pkg-olsrr-c25-fu`); the parent builder report remains
   `docs-dev/olsrr-c25-report.md` (audit I5). Rename only if the team wants
   consistent ids.

Any wrong result left inside the documented domain: no.
