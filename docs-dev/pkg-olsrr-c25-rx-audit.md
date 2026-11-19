# pkg-olsrr-c25-rx — independent re-check

2026-11-19. Audited HEAD `72b062fb5c4ce3accda9169a14edaa0a07225604`
(`olsrr: follow-up to C25 audit (pkg-olsrr-c25-fu)`), against parent
`0e16ef5`. Branch remained `task/olsrr-c25`. Read the package audit stop rule
at `F:/pm/notes/package-audit-stop-rule.md`, the C25 audit, the follow-up
report, and the actual commit diff. The reports were treated as claims, not
test evidence. No package code, tests, snapshots, generated documentation,
version, or branch was changed. This report is the only tracked repository change.
No push or GitHub operation was performed.

Both claimed Low fixes are **FIXED**. There were no Medium findings claimed
fixed by the follow-up. Ordinary-input comparisons found no regression.
An independently reproduced, pre-existing Medium display issue remains open
under the supplied stop rule; it determines the merge verdict below.

## Claimed fixes

| Parent audit finding | Verdict | Independent evidence |
|---|---|---|
| L1: NEWS omits the stand-alone plot title changes | **FIXED** | `NEWS.md:6` now names all four exported functions and diagnostics panels 7/8. Runtime titles agree with the bullet. |
| L2: constant vector errors inside `lillie.test()` | **FIXED** | The exact-equality guard at `R/ols-normality-tests.R:176` produces the intended error before test execution or skip warnings. Reverting only that source file in scratch restores the old error and breaks the newly added test. |

L1 runtime output for `m <- lm(mpg ~ disp + hp + wt, mtcars)`:

```text
ols_plot_fm(m, FALSE)                    "Fit - Mean"
ols_plot_resid_spread(m, FALSE)          "Residual"
ols_plot_resid_fit_spread(m, FALSE):
  fm_plot                              "Fit - Mean"
  rsd_plot                             "Residual"
ols_plot_diagnostics(m, FALSE):
  panel 7                              "Fit - Mean"
  panel 8                              "Residual"
```

The existing title tests pass: 5 and 6 expectations respectively (11 total,
not the follow-up report's stated 5 + 4). These tests predate this follow-up;
L1 changed NEWS only, so no new test was expected to fail on its parent.

L2 output:

```r
ols_test_normality(rep(3, 10))
# HEAD:
# ERROR: y has no variation: all 10 non-missing values are identical, so the normality tests are undefined.
# Fix reverted in scratch:
# ERROR: missing value where TRUE/FALSE needed
```

The HEAD error also names the correct count for constant samples of sizes
3, 4, 5, and 7, a constant vector with NA/NaN entries, zeros, negative
constants, and `lm(rep(0, 10) ~ 1)` with exactly zero residuals. None emits a
skip warning before the error. Empty/all-missing input and sizes 1/2 retain
their original "at least 3 observations are required" errors.

## New-test sensitivity, using scratch copies only

Two package source copies were extracted from HEAD outside the repository.
Both retained HEAD's tests and NEWS. In `reverted/`, only
`R/ols-normality-tests.R` was replaced by the parent commit's version.
No stash, checkout, branch switch, or repository-source edit was used.
Each copy was loaded in a separate Rscript process.

The only new follow-up test is
`test-normality.R:173`, "a constant sample errors with a message naming the
input". Its two expectations require the helpful constant-sample error and
silent completion for `c(rep(3, 9), 4)`.

```text
Fix reverted, normality file:
Error ('test-normality.R:176:3'):
  missing value where TRUE/FALSE needed
  ols_test_normality.default() -> nortest::lillie.test(y)
[ FAIL 1 | WARN 0 | SKIP 0 | PASS 64 ]

HEAD, normality file:
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 66 ]
New test alone: 2 passing expectations, 0 errors.

Combined normality + existing diagnostics title tests:
Fix reverted: [ FAIL 1 | WARN 0 | SKIP 0 | PASS 75 ]
HEAD:         [ FAIL 0 | WARN 0 | SKIP 0 | PASS 77 ]
```

The reverted run has one unexpected error, rather than a failed value
comparison; testthat counts that as FAIL 1. The error interrupts the new
test before its second expectation, explaining the two-expectation
difference. The nonconstant input was also exercised independently in the
regression grid on both copies and completed without warnings.

For completeness, the original C25 regression tests were also run against a
third HEAD scratch copy with all C25 runtime changes reverted: the three R
files (`ols-normality-tests.R`, `ols-residual-fit-spread-plot.R`, `output.R`)
and `NAMESPACE` were restored from pre-C25 commit `4283adb`. HEAD tests and
snapshots were retained. The filter was
`normality|diagnostics-panel-titles|norm-output` (15 test cases, 3 files).

```text
HEAD:          79 passing expectations; 0 failures/errors/warnings/skips.
Pre-C25 code:  23 failed expectations, 1 unexpected test error,
               1 warning, 54 passing expectations, 0 skips.
```

Failures comprise 9 title expectations, the normality output snapshot, and
13 normality expectations covering corrected p-values, method/printed
labels, and sample-size skipping. The additional error is the already
described new constant-input test; the warning is old `ks.test()`'s ties
warning. For example, the snapshot's KS p-value changes from HEAD's 0.3446
back to 0.7464, and both title tests see "Residual Fit Spread Plot" again.
The summary reporter displayed only its first 10 failure details; the
saved result data frame contains all 15 test cases and the totals above.

## Ordinary-input regression and reference checks

The documented normality input is a numeric vector or `lm` object, with
sample-size ranges 3–5000 (Shapiro-Wilk), >=5 (Lilliefors), and >=8 (AD/CvM).
Tests outside a sample-size range are skipped with a warning. Plot inputs
are `lm` objects. The help still lacks an explicit constant-sample exclusion
and a general output-precision statement; see the open items below.

Using seed 251119, the same script was run separately against HEAD and the
scratch copy with the guard reverted. Its 49 nonconstant inputs were:

- Normal and exponential vectors at n = 3:10, 20, 50, 100, 500, 5000, 5001
  (28 vectors).
- Rounded data, integers, NA/NaN omission, four remaining observations after
  omission, a location shift, scales 1e-8 and 1e8, `c(rep(3, 9), 4)`, and
  `c(rep(3, 9), 3 + 1e-10)` (9 vectors).
- Twelve fits: five mtcars variants (simple, multiple, factor, weighted,
  and `na.exclude`), surgical, stepdata (n = 20000), auto, and mtcars subsets
  of sizes 4/5/6/9.

```text
ORDINARY_INPUTS 49 ERRORS 0                 # both copies
REFERENCE_STATISTIC_AND_PVALUE_PAIRS 159 PASS # each copy
Ordinary normality results including all htest fields and warnings:
  IDENTICAL for 49 inputs
Plot labels and built layer data:
  IDENTICAL for 56 plots across 4 models
Sample-size errors, warnings, and constant-response model result:
  IDENTICAL
```

All available statistics and p-values matched direct calls to
`stats::shapiro.test()` and the respective `nortest` functions exactly.
The complete normality result objects and warning strings were identical
between copies, including documented skips and CvM's lower-bound warning.

Plot comparisons covered all 10 diagnostic panels, both stand-alone spread
plots, and both combined spread panels for four mtcars fits (simple,
multiple, factor, and missing-data models). Actual label values and every
built layer's data were identical. Process-local S7 class metadata on
ggplot2's labels was removed by copying the label values into ordinary named
lists; layer data and numerical values were not normalized or rounded.

## Full test and check runs

`devtools::test()` on the actual worktree, in a clean Rscript process:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3615 ]
TOTAL_FILES 57 TOTAL_TESTS 436
failed 0; warning 0; skipped 0; passed 3615; error 0
Elapsed: 734.5 seconds; Rscript exit status: 0.
```

`devtools::check(document = FALSE, args = "--no-manual")`, directing its build
and check output outside the repository:

```text
0 errors | 0 warnings | 1 note
Status: 1 NOTE
Rscript exit status: 0.
R CMD check phase duration: 26m 34.5s.
```

The single NOTE is the pre-existing root `Rplots.pdf` described below.
Both example phases, code/documentation checks, vignette checks and
vignette rebuilding passed. Check's own `testthat.R` run also reports
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3615 ]` (805.6 seconds elapsed).
The saved result data frames from the two direct full-suite runs have
identical test names and expectation counts, independently verified by
`summarize.R`.

Devtools also supplies its default `--as-cran`; the log records
`_R_CHECK_CRAN_INCOMING_=FALSE`, `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`,
`_R_CHECK_FORCE_SUGGESTS_=FALSE`, and `NOT_CRAN=true`. No additional check
exclusion was introduced by this audit.

The first full-suite invocation already completed with
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3615 ]` (57 files, 436 tests) and saved
its result data frame. Its wrapper then encountered a trailing parser error
after the END marker because I had edited that shared script during its
execution. This was a harness error after the tests returned, not a package
test failure. A separate, immutable `run-tests-clean.R` script reran the
entire suite with the corrected Windows locale for the final result above.

The first requested check attempt, with the inherited environment, stopped
at DESCRIPTION meta-information with **1 ERROR, 1 WARNING, 0 NOTES**. Both
messages consisted of R startup failures to set `C.UTF-8`, an invalid Windows
locale. Installation itself succeeded. Its original logs were retained.
The full check was then rerun with process-local `LC_ALL`, `LC_CTYPE`, and
`LANG` set to `English_United States.utf8`; no machine configuration or
repository file was changed for this correction.

The root `Rplots.pdf` is ignored by Git but not by `.Rbuildignore`, so it is
included in the check tarball and causes a non-standard-top-level-file NOTE.
Its creation and last-write timestamps are 2026-11-18 (before this audit).
It was left untouched. Test execution also writes the usual ignored
`tests/testthat/Rplots.pdf`; no tracked test or snapshot changed.

## Still-open findings and report corrections

**M1 — STILL OPEN: small positive normality p-values print as zero.**
`R/output.R:737` rounds p-values to four decimal places without a bound.
This is pre-existing in the follow-up's parent, not a regression introduced
by the constant-sample guard. It nevertheless conflicts with stop-rule
item 3: "Output below the display resolution prints as a bound". A routine
exponential sample, also used by the package's own normality tests, exposes
it inside the documented domain:

```r
set.seed(2)
x <- rexp(200)
z <- suppressWarnings(ols_test_normality(x))
vapply(z, function(test) test$p.value, numeric(1))
# kolmogorv  4.637178212510176e-12
# shapiro   5.453474192726589e-13
# cramer    7.370000000000000e-10  # nortest's documented floor
# anderson  2.441176651324525e-20
print(z)
```

Actual table, identical on HEAD and with the follow-up fix reverted:

```text
             Test                    Statistic       pvalue
Shapiro-Wilk                           0.8521         0.0000
Kolmogorov-Smirnov (Lilliefors)        0.1513         0.0000
Cramer-von Mises                       1.4037         0.0000
Anderson-Darling                       8.333          0.0000
```

Expected under the supplied rule: a bound such as `< 0.0001`, preserving
the documented CvM floor/caveat. Returned numerical values agree with the
reference; the finding concerns the user-facing table. Because ordinary
non-normal samples trigger it, this is a Medium display issue, not an
exotic floating-point backlog case. It blocks a positive merge verdict
under that rule even though both requested Low fixes are correct.

**Low/documentation — STILL OPEN (already acknowledged, not claimed fixed):**
the normality help omits the constant-sample restriction and a general
precision contract. NEWS explains the constant restriction. This re-check
does not edit or regenerate documentation.

**Follow-up report correction:** its specific claim that
`ols_test_normality(lm(rep(3, 10) ~ 1))` raises the internal `if (NA)` error
did not reproduce. The residuals are the quoted approximately -1.2639e-15
and 1.4043e-16, but both copies complete and agree with the tests applied
directly to those residuals. Lilliefors gives D = 0.52409 and p approximately
7.959e-09. This does not establish meaningful normality inference for a
perfect-fit model; handling numerical residual artifacts remains a separate
degenerate-model/domain-documentation follow-up. Exact zero residuals are
covered by the new guard.

Other acknowledged housekeeping remains outside this two-fix re-check:
42 zero-byte visual SVGs (count independently confirmed), stale generated
pkgdown content, and the original builder-report naming. Existing title
assertions and the built-layer comparison above supply evidence despite
the incomplete visual snapshot coverage. No additional extreme-value
search was pursued.

## Reproduction and retained evidence

R 4.6.1 on Windows 11; devtools 2.5.2, testthat 3.3.2, pkgload 1.5.3,
ggplot2 4.0.3, nortest 1.0.4, vdiffr 1.0.9.
All R commands used script files, never `Rscript -e`.
Scratch directory (outside the repository):
`C:/Users/DELL/AppData/Local/Temp/olsrr-c25-rx-20261119/`.

```powershell
$rxScratch = 'C:/Users/DELL/AppData/Local/Temp/olsrr-c25-rx-20261119'
# From package root; anchor revisions so the audit commit does not change them:
git archive --format=tar -o (Join-Path $rxScratch 'head.tar') 72b062f DESCRIPTION NAMESPACE R data man tests NEWS.md LICENSE
# Extract head.tar into head/ and reverted/ from within the scratch directory.
# Replace only reverted/R/ols-normality-tests.R with git show 0e16ef5:R/ols-normality-tests.R.
Rscript (Join-Path $rxScratch 'run.R') full-test
Rscript (Join-Path $rxScratch 'run.R') full-check
$env:LC_ALL = 'English_United States.utf8'; $env:LC_CTYPE = 'English_United States.utf8'; $env:LANG = 'English_United States.utf8'
Rscript (Join-Path $rxScratch 'verify.R') head
Rscript (Join-Path $rxScratch 'verify.R') reverted
Rscript (Join-Path $rxScratch 'compare.R')
Rscript (Join-Path $rxScratch 'evidence.R')
Rscript (Join-Path $rxScratch 'precision.R') head
Rscript (Join-Path $rxScratch 'precision.R') reverted
# c25-reverted/ retains HEAD tests; restore its three R files + NAMESPACE from 4283adb.
Rscript (Join-Path $rxScratch 'c25-sensitivity.R') c25-reverted
Rscript (Join-Path $rxScratch 'c25-sensitivity.R') head
Rscript (Join-Path $rxScratch 'run-tests-clean.R')
Rscript (Join-Path $rxScratch 'run.R') full-check-clean
Rscript (Join-Path $rxScratch 'summarize.R')
```

`verify-*.log`, `*-grid.rds`, and `*-filtered-tests.rds` retain the differential
evidence; `compare.log` retains the assertions; `evidence.log` retains the
direct HEAD examples; `precision-*.log` retains M1 output.
`c25-sensitivity-*.log` / `.rds` retain the additional pre-C25 comparison.
`full-test.log` / `full-test-results.rds` retain the first complete suite result;
`full-test-clean.log` / `full-test-clean-results.rds` retain the clean rerun.
`full-check.log` and `check/olsrr.Rcheck/00check.log` retain the failed locale
attempt; `full-check-clean.log` and `check-clean/olsrr.Rcheck/` retain the
completed rerun, including installation, examples, tests, and vignette logs.
The scripts are retained beside those logs. The only repository file to be
staged and committed is this report, with message `re-check olsrr-c25`.

No High finding or ordinary-input regression was found in the re-checked
scope. Both claimed Low fixes pass. M1 remains a Medium for an ordinary
documented input, so the supplied stop rule prevents a positive merge
verdict despite the passing numerical and package checks.

Ready to merge into develop: no
