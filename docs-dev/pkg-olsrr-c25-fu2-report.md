# pkg-olsrr-c25-fu2: p-value display bounds

2026-11-19. Starting HEAD: `293cca4` (`re-check olsrr-c25`). Branch:
`task/olsrr-c25`, retained throughout. Read item 3 of
`F:/pm/notes/package-audit-stop-rule.md` and M1 of
`docs-dev/pkg-olsrr-c25-rx-audit.md` before editing.

## Change and scope

All package-owned p-value printers now use four decimals, with `< 0.0001`
for values below `0.0001`, including numerical zero. A single internal
`format_pvalue()` formats the display; no statistical computation or
returned numeric value changes. This also replaces the old three-decimal
coefficient and five-decimal stepwise p-value displays, and makes the
adaptive test-summary displays consistent. Other statistics keep their
existing formats. The bound is selected using the original p-value before
rounding, so a value just below `0.0001` is still shown as a bound.

The sweep covered all of `R/output.R`, the other S3 print methods and
stepwise output helpers. Stepwise final model output already delegates to
`ols_regress()` and therefore receives the same fix. `ols_test_outlier()`
returns an ordinary numeric data frame with no package print method or
fixed-decimal p-value formatter; its return value was left alone.

Column widths use the formatted values. The normality p-value column grows
from seven to eight characters when a bound occurs; its ordinary-input
snapshot is unchanged. Regression coefficient rows retain matching column
positions, and normality rules/header/rows retain the existing width and
trailing-space convention, including skipped tests.

The Cramer-von Mises documentation, numerical floor (`7.37e-10`) and
`nortest` warning are unchanged. The exponential fixture still emits
`p-value is smaller than 7.37e-10, cannot be computed more accurately`.

The ignored, untracked root `Rplots.pdf` and the existing ignored
`tests/testthat/Rplots.pdf` were removed. Neither was tracked, so there is
no Git deletion for them. Test setup now opens a null PDF device and sets
the fallback device to another null PDF device, restoring the option and
closing its device at teardown. Tests that explicitly open their own
devices retain their existing behavior.

DESCRIPTION currently records version `0.7.1`; it was not changed. No
version bump, generated documentation edit, branch switch, push or
GitHub/gh operation was performed.

## Files

- `R/output.R`: internal formatter and all eight p-value print helpers
  (`print_reg`, `print_anova`, `print_norm_test`, `print_ftest`,
  `print_bp_test`, `print_bartlett_test`, `print_score_test`,
  `print_pure_error_anova`).
- `R/ols-stepwise-utils.R`: `ols_stepwise_table` and
  `ols_stepwise_table_p`, including width calculations.
- `tests/testthat/test-pvalue-display.R`: eight regression cases covering
  every affected printer/helper, all 13 existing Breusch-Pagan output
  configurations, numeric-result preservation, alignment and resolution
  boundaries.
- `tests/testthat/test-breusch-pagan.R`: update the existing Holm-output
  assertion to four decimals and assert the bound for its genuinely tiny
  adjusted p-value, while retaining the numeric `p.adjust()` comparisons.
- `tests/testthat/setup-graphics.R`: suite-wide null graphics device.
- `DESCRIPTION`: declare `withr` as a suggested test dependency for device
  teardown; the version is unchanged.
- `NEWS.md`: development bug-fix bullet with author and issue reference.
- Fourteen text snapshots in `tests/testthat/_snaps/`: `anova.md`,
  `bartlett-output.md`, `bp-output.md`, `ftest-output.md`,
  `loftest-output.md`, `regress-output.md`, `score-output.md`,
  `step-backward-output.md`, `step-forward-output.md`,
  `step-rsquared-output.md`, `stepaic-b-output.md`,
  `stepaic-both-output.md`, `stepaic-f-output.md`, `stepwise-output.md`.
- This report.

Snapshots were regenerated and accepted with `testthat::snapshot_accept()`.
Changes are p-value text, column padding, and corresponding table rules
and title centering. Existing snapshot trailing spaces are intentional.
An automated comparison verifies 588 changed p-value cells across these
14 files, with no other numeric-cell changes. No visual snapshot was updated.

## Before/after output per method

For a deterministic display-only comparison, the fixture uses normal
package results from `mtcars` and replaces only their p-value fields with
`1e-12` before printing. Normality instead uses the actual tiny p-values
from `set.seed(2); x <- rexp(200)`. The two stepwise fixtures contain
`p = c(1e-12, 0.1234)` and otherwise ordinary metrics.

| Public print method / helper | Before HEAD p-value text | After p-value text |
|---|---|---|
| `print.ols_regress` / `print_reg`, ANOVA row | `0.0000` | `< 0.0001` |
| `print.ols_regress` / `print_reg`, each coefficient row | `0.000` | `< 0.0001` |
| `print.ols_anova` / `print_anova` | `0.0000` | `< 0.0001` |
| `print.ols_test_normality` / `print_norm_test`, all four tests | `0.0000` | `< 0.0001` |
| `print.ols_test_f` / `print_ftest` | `Prob > F   =    1e-12` | `Prob > F   =    < 0.0001` |
| `print.ols_test_score` / `print_score_test` | `Prob > Chi2   =    1e-12` | `Prob > Chi2   =    < 0.0001` |
| `print.ols_test_bartlett` / `print_bartlett_test` | `Prob > Chi2   =    1e-12` | `Prob > Chi2   =    < 0.0001` |
| `print.ols_test_breusch_pagan` / `print_bp_test`, scalar summaries | `Prob > Chi2   =    1e-12` | `Prob > Chi2   =    < 0.0001` |
| `print.ols_test_breusch_pagan` / `print_bp_test`, variable and simultaneous rows | `1e-12` | `< 0.0001` |
| `print.ols_pure_error_anova` / `print_pure_error_anova`, regression and lack-of-fit rows | `1e-12` | `< 0.0001` |
| `ols_stepwise_table`, tiny predictor row | `tiny          0.00000` | `tiny         < 0.0001` |
| `ols_stepwise_table_p`, tiny predictor row | `tiny          0.00000` | `tiny         < 0.0001` |

The previously adaptive summaries already printed a scientific value for
the artificial `1e-12` fixture. They are included to provide the same bound
and precision across all package-owned p-value printers.

Actual normality data rows before:

```text
Shapiro-Wilk                           0.8521         0.0000
Kolmogorov-Smirnov (Lilliefors)        0.1513         0.0000
Cramer-von Mises                       1.4037         0.0000
Anderson-Darling                       8.333          0.0000
```

After:

```text
Shapiro-Wilk                           0.8521        < 0.0001
Kolmogorov-Smirnov (Lilliefors)        0.1513        < 0.0001
Cramer-von Mises                       1.4037        < 0.0001
Anderson-Darling                       8.333         < 0.0001
```

All 20 saved result objects (seven non-BP methods and 13 BP configurations)
are `identical()` between the HEAD and fixed processes. Each printer's
regression test also checks that printing leaves its result unchanged.
Normality p-values are compared directly with Shapiro-Wilk, Lilliefors
and Anderson-Darling references, and the CvM floor is asserted exactly.

## Tests and sensitivity on HEAD

The new print-method tests were first run against the worktree's unchanged
HEAD runtime: 27 failed expectations, 48 passing expectations, no warnings,
skips or errors. The final test file was also copied to a source archive of
HEAD outside the repository and rerun in a separate R process. This avoids
stashing, checking out files, or switching branches.

Final-file sensitivity against HEAD:

| Case | Failed assertions | Passing assertions | Errors |
|---|---:|---:|---:|
| Resolution/zero/missing-value helper | 0 | 0 | 1 (formatter absent on HEAD) |
| Normality | 2 | 7 | 0 |
| Regression | 2 | 2 | 0 |
| ANOVA | 2 | 1 | 0 |
| F, score, Bartlett summaries | 3 | 6 | 0 |
| All 13 Breusch-Pagan configurations | 13 | 26 | 0 |
| Pure-error ANOVA | 1 | 2 | 0 |
| Both stepwise table helpers | 4 | 4 | 0 |
| Total | 27 | 48 | 1 |

The summary reporter counts the unexpected missing-helper error as an
additional failure: `[ FAIL 28 | WARN 0 | SKIP 0 | PASS 48 ]`.
Every affected method/helper fails the bound assertion on HEAD. The
methods that print fixed zeros also fail the no-`0.0000` assertion;
already-scientific summaries fail the requested bound assertion alone.

Fixed, final new test file:
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 77 ]` (8 test cases).
Boundary coverage includes zero, `1e-300`, `0.000049`, `0.00005`,
`0.0000999`, `0.0001`, `0.00015`, `0.123456`, one, NA and NaN.

The initial full-suite run exposed two remaining old-output expectations:
the ANOVA snapshot outside the `*-output.R` files and a Holm BP assertion
expecting scientific notation. Both were updated to the intentional new
format; the focused ANOVA/BP run then passed all 173 expectations. The
known-failing initial run was stopped and the completed suite was rerun.

Final `devtools::test()` run:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3694 ]
TOTAL_FILES 58 TOTAL_TESTS 444
failed 0; warning 0; skipped 0; passed 3694; error 0
Elapsed: 529.5709 seconds; Rscript exit status: 0.
```

Final `devtools::check(document = FALSE, args = "--no-manual")`:

```text
Status: OK
0 errors | 0 warnings | 0 notes
CHECK_ERRORS 0 CHECK_WARNINGS 0 CHECK_NOTES 0
Elapsed including build: 1122.284 seconds; Rscript exit status: 0.
```

Check's own test run also passed
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3694 ]` (454.64 seconds).
Both example phases passed (90 seconds and 296 seconds with
`--run-donttest`), as did vignette checking/rebuilding (49 seconds for
rebuilding). Top-level files and leftover-file checks passed. The built
tarball contains the new tests/device setup and no `Rplots.pdf` anywhere;
neither the worktree root nor `tests/testthat/` PDF was recreated by the
completed tests/check. The previous root-PDF NOTE is gone without adding
a build exclusion.

## Commands and retained evidence

R 4.6.1, Windows 11. R commands ran from the package root, using script
files. The inherited `C.UTF-8` locale is invalid on Windows; as in the
re-check, only each command's process environment was set to the installed
`English_United States.utf8` locale. No machine setting changed.

Scratch scripts, complete printed outputs, result RDS files and logs are
retained outside the repository at
`C:/Users/DELL/AppData/Local/Temp/olsrr-c25-fu2-20261119/`.

```powershell
$fu2Scratch = Join-Path $env:TEMP 'olsrr-c25-fu2-20261119'
$env:LC_ALL = 'English_United States.utf8'; $env:LC_CTYPE = 'English_United States.utf8'; $env:LANG = 'English_United States.utf8'
Rscript (Join-Path $fu2Scratch 'run.R') head
Rscript (Join-Path $fu2Scratch 'evidence.R') head
git archive --format=tar -o (Join-Path $fu2Scratch 'head.tar') 293cca4
& C:/Windows/System32/tar.exe -xf (Join-Path $fu2Scratch 'head.tar') -C (Join-Path $fu2Scratch 'head')
Copy-Item -LiteralPath tests/testthat/test-pvalue-display.R,tests/testthat/setup-graphics.R -Destination (Join-Path $fu2Scratch 'head/tests/testthat')
Rscript (Join-Path $fu2Scratch 'head-final-tests.R')
Rscript (Join-Path $fu2Scratch 'evidence.R') fixed
Rscript (Join-Path $fu2Scratch 'compare.R')
Rscript (Join-Path $fu2Scratch 'run.R') fixed
Rscript (Join-Path $fu2Scratch 'run.R') snapshots
Rscript (Join-Path $fu2Scratch 'refresh.R')
Rscript (Join-Path $fu2Scratch 'accept-snapshots.R')
Rscript (Join-Path $fu2Scratch 'update-existing.R')
Rscript (Join-Path $fu2Scratch 'run.R') full-test
Rscript (Join-Path $fu2Scratch 'run.R') check
```

The wrapper invokes the requested commands:

```r
devtools::test()
devtools::check(document = FALSE, args = "--no-manual",
                check_dir = file.path(scratch, "check"), quiet = FALSE)
```

Check output is directed outside the worktree. Devtools' default check
environment and arguments are retained; no new exclusions are introduced.
An initial build was stopped before checking so the finalized build would
include the explicit `withr` test dependency; its log is retained as
`check-initial-build.log`.
Evidence files include `head.log`, `head-final-tests.log` and its RDS,
`fixed.log`, `output-head.log`/`output-fixed.log` and corresponding RDS,
`snapshot-review.log`, `verify-snapshots.log`, `update-existing.log`,
`full-test-initial.log`, `full-test.log`/results RDS,
`check.log`/results RDS and `check/olsrr.Rcheck/`.

Final whitespace verification uses
`git -c core.whitespace=-blank-at-eol diff --check`, allowing the intentional
trailing spaces in captured console tables. All 22 changed/new files are
included in the single requested commit:

```text
olsrr: print tiny p-values as a bound (pkg-olsrr-c25-fu2)
```
