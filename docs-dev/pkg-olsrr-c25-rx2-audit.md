# pkg-olsrr-c25-rx2 — re-check 2 of olsrr follow-up 2

2026-11-19. Audited HEAD `4c49aef` (`olsrr: print tiny p-values as a bound
(pkg-olsrr-c25-fu2)`) against its parent `293cca4` (`re-check olsrr-c25`).
Branch `task/olsrr-c25` remained checked out throughout. Read the package
audit stop rule at `F:/pm/notes/package-audit-stop-rule.md`, the M1 re-check
`docs-dev/pkg-olsrr-c25-rx-audit.md`, the follow-up report
`docs-dev/pkg-olsrr-c25-fu2-report.md`, and the actual commit diff. The
reports were treated as claims, not as test evidence. Audit only: no package
code, test, snapshot, generated documentation, DESCRIPTION or branch was
changed, and this report is the only tracked change. No push, `gh` or GitHub
operation was performed.

M1 is **FIXED**. Returned values are unchanged on 30 models, the changed
snapshots differ only in p-value text (and the padding/rules that follow from
it), and the requested test and check runs pass. One pre-existing, unrelated
defect was observed in the regression grid and is recorded separately below;
it is identical on the parent and is not a regression from this change.

## 1. M1 — FIXED

The exact fixture from the M1 finding, in a clean Rscript process with the
HEAD worktree loaded by `pkgload::load_all()`:

```r
set.seed(2)
x <- rexp(200)
print(ols_test_normality(x))
```

```text
-------------------------------------------------------------
             Test                    Statistic        pvalue
-------------------------------------------------------------
Shapiro-Wilk                           0.8521        < 0.0001
Kolmogorov-Smirnov (Lilliefors)        0.1513        < 0.0001
Cramer-von Mises                       1.4037        < 0.0001
Anderson-Darling                       8.333         < 0.0001
-------------------------------------------------------------
```

Four bounds, no `0.0000`, and the Cramer-von Mises warning
`p-value is smaller than 7.37e-10, cannot be computed more accurately` is
still emitted. The returned values are identical to the direct references and
to the documented CvM floor:

```text
kolmogorv  4.6371782125101762e-12   == nortest::lillie.test(x)$p.value
shapiro    5.4534741927265891e-13   == shapiro.test(x)$p.value
cramer     7.3700000000000004e-10   == 7.37e-10 (nortest's documented floor)
anderson   2.4411766513245247e-20   == nortest::ad.test(x)$p.value
```

The values are unchanged from the M1 report, so the fix is display-only.

### Per-printer rounding and bound grid

Every printer in the follow-up report's table was exercised through its own
data field, not through `format_pvalue()` directly. For each printer, 32
inputs were used: 20 pseudo-random p values from `10^(-8 * runif(20))` with
seed 20261119 (9 below `1e-4`, 11 at or above it; range
7.758836e-08–0.3625789), plus 0, 1e-300, 1e-12, 0.000049, 0.0000999,
`1e-4 - 1e-12`, `1e-4`, `1.000001e-4`, 0.00015, 0.123456, 0.5 and 1.

For every input the check asserted: the printed token equals
`format(round(p, 4), nsmall = 4, scientific = FALSE)` when `p >= 1e-4`; prints
as `< 0.0001` when `p < 1e-4` (including 0); appears exactly in the printer's
p-value slots; no `0.0000`, `0.00000` or scientific p-value remains; and the
printed object is unchanged. 1110 assertions in 320 printer/input pairs, 0
failures.

| Printer | p-value slots | Inputs | Failures |
|---|---:|---:|---:|
| `print.ols_regress` (ANOVA + coefficients) | 4 | 32 | 0 |
| `print.ols_anova` | 1 | 32 | 0 |
| `print.ols_test_normality` | 4 | 32 | 0 |
| `print.ols_test_f` | 1 | 32 | 0 |
| `print.ols_test_score` | 1 | 32 | 0 |
| `print.ols_test_bartlett` | 1 | 32 | 0 |
| `print.ols_test_breusch_pagan` (multiple) | 6 | 32 | 0 |
| `print.ols_pure_error_anova` | 2 | 32 | 0 |
| `ols_stepwise_table` | 2 rows | 32 | 0 |
| `ols_stepwise_table_p` | 2 rows | 32 | 0 |

The resolution boundary behaves as requested: `0.0000999` and `1e-4 - 1e-12`
print `< 0.0001`; `1e-4`, `1.000001e-4` and `0.00015` print `0.0001`; values
just above print their four-decimal rounding (for example 0.01331 prints
0.0133 and 0.01248 prints 0.0125). Zero, 1e-300, 1e-12 and 0.000049 print the
bound; `NA`/`NaN` keep `NA`/`NaN`; 1 prints `1.0000`.

All 13 Breusch-Pagan print configurations were also checked independently,
each with a bound (`p = 1e-12`) and an ordinary value (0.123456): bound count
equals the number of p slots, `0.1235` appears the same number of times, no
false zero, and the printed object is unchanged (52 assertions, 0 failures).
The normality skip case (n = 6, CvM/AD unavailable) is byte-identical to the
parent, and a mixed skip+tiny case (n = 7, two bounds and two `NA` rows) is
aligned.

## 2. No regression

Two source trees were compared in separate Rscript processes: the HEAD
worktree and a `git archive` of parent `293cca4` extracted outside the
repository. The same grid script ran against each; results were stored as
RDS and compared with `identical()` after a normalization that drops
process-local environments and turns formulas/calls into text. Numeric and
character leaves were not rounded, and every list name and class was kept.

The grid contains 30 ordinary models: 15 `mtcars` fits (simple, all
predictors, factor, interaction, log, polynomial, weights, `na.exclude`,
subsets), 10 `surgical` fits, 4 `auto` fits and 1 `stepdata` subset. For each
model it recorded `ols_regress`, `ols_anova`, `ols_test_f`, `ols_test_score`,
three `ols_test_breusch_pagan` configurations, `ols_test_normality` on the
residuals, and `ols_test_outlier`: 270 model/call pairs. It added three
Bartlett tests, four pure-error ANOVAs, and eight stepwise objects
(backward/both/forward p and AIC families, best subset, and a surgical
backward-p fit).

```text
MODEL/CALL PAIRS                 270
ADDITIONAL OBJECTS                15  (3 Bartlett, 4 pure error, 8 stepwise)
NORMALIZED SCALAR LEAVES      69,566
IDENTICAL DIFFERENCES              0
WARNING STRINGS                    0
ERROR CALLS                       11  (identical text on parent and HEAD)
```

The 11 error calls are the documented weighted-model restrictions of
`ols_test_f()`, `ols_test_score()` and `ols_test_breusch_pagan()` on the
weighted fit, plus the pre-existing Breusch-Pagan limitation described in the
last section. Their error text is identical between the two trees; no numeric
result differs.

Printed tables keep their columns. Checks covered normal and tiny-p
normality tables (including skipped rows), `ols_regress` and
`ols_test_breusch_pagan` in ordinary, bound, and mixed bound/ordinary forms,
`ols_anova`, the stepwise summary and its final model, and the two stepwise
table helpers. Every segment had one row width, every rule width was the row
width or one less (the legacy trailing-space convention), and no ragged row
was found.

```text
normality tiny            rule 61  rows 62      (4 bounds)
normality ordinary        rule 60  rows 61
ols_regress ANOVA         rule 70  rows 70
ols_regress coefficients  rule 91  rows 91
ols_regress, all bounds   rule 70 / 91, rows 70 / 91
stepwise summary          rule 72  rows 73
stepwise final model      rule 70 / 89, rows 70 / 89
bp multiple, all bounds   rule 46  rows 47
pure error, all bounds    rule 66  rows 67
ols_stepwise_table        row width 63
ols_stepwise_table_p      row width 22
```

## 3. Snapshot changes

All 14 changed text snapshots were compared line by line against the parent
commit. Each file kept its line count. A changed line was accepted only if it
was (a) identical, (b) whitespace-only (title centering and padding),
(c) a rule line of `-` or `=` whose width changed, or (d) a p-value cell whose
new text is exactly the parent value at four decimals (within the parent
token's display resolution) or `< 0.0001` when the parent value is below
`1e-4`. Any other token difference, or a different token count, counted as a
failure.

```text
CHANGED FILES        14
TOTAL LINES       7,553
IDENTICAL LINES   6,002
WHITESPACE-ONLY     508
RULE-WIDTH LINES    455
P-VALUE CELLS       588   (325 became `< 0.0001`, 263 became 4 decimals)
OTHER CHANGES         0
```

| Snapshot | p-value cells |
|---|---:|
| `anova.md` | 1 |
| `bartlett-output.md` | 2 |
| `bp-output.md` | 46 |
| `ftest-output.md` | 3 |
| `loftest-output.md` | 2 |
| `regress-output.md` | 10 |
| `score-output.md` | 4 |
| `step-backward-output.md` | 92 |
| `step-forward-output.md` | 114 |
| `step-rsquared-output.md` | 105 |
| `stepaic-b-output.md` | 57 |
| `stepaic-both-output.md` | 34 |
| `stepaic-f-output.md` | 82 |
| `stepwise-output.md` | 36 |

This reproduces the follow-up report's claim of 588 changed p-value cells
exactly. The distinct transformations are all p-value shapes (69 `0.0000`,
238 `0.000`, 10 `0.00000` and eight scientific tokens became `< 0.0001`; the
remainder are 3-, 5- or 7-decimal values and `1e-04`/`2e-04`, moved to four
decimals). No non-bound parent four-decimal p-value changed, and no
non-p-value numeric cell changed.

One detail not shown in the follow-up report's before/after table: ordinary
p-values in the F, score, Bartlett and Breusch-Pagan summaries previously
used up to seven significant digits and now print four decimals (for example
`0.4884154` to `0.4884` in `ftest-output.md`). That is the intended
"all printed p-values now 4 decimals" scope, not a regression.

## 4. Full test and check

`devtools::test()` on the actual worktree, in a clean Rscript process:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3694 ]
TOTAL_FILES 58 TOTAL_TESTS 444
failed 0; warning 0; skipped 0; passed 3694; error 0
Elapsed: 513.3 seconds; Rscript exit status: 0.
```

`devtools::check(document = FALSE, args = "--no-manual")`, with its build and
check output directed outside the repository (`check_dir` in the scratch
directory):

```text
Status: OK
0 errors | 0 warnings | 0 notes
CHECK_ERRORS 0 CHECK_WARNINGS 0 CHECK_NOTES 0
Check's own testthat run: [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3694 ] (13m)
Elapsed including build: 1537.5 seconds (25m 37s); Rscript exit status: 0.
```

Both example phases passed (110 s, and 257 s with `--run-donttest`), as did
the vignette checks and re-building (100 s). The previous root-`Rplots.pdf`
NOTE does not recur, and the built tarball produces no non-standard-file
finding.

Devtools' default check environment was used. The log records
`_R_CHECK_CRAN_INCOMING_=FALSE`, `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`,
`_R_CHECK_FORCE_SUGGESTS_=FALSE`, `_R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_=FALSE`
and `NOT_CRAN=true`; no additional exclusion was introduced. Neither the
worktree root nor `tests/testthat/` contains an `Rplots.pdf` after the runs,
and `git status` shows no tracked change other than this report.

## Out-of-scope pre-existing observation

`ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)` fails with a
cryptic error when the model contains a transformed term or a factor:

```r
ols_test_breusch_pagan(lm(mpg ~ log(wt) + qsec, mtcars),
                       rhs = TRUE, multiple = TRUE)
# Error: object 'wt' not found
ols_test_breusch_pagan(lm(mpg ~ factor(cyl) + hp, mtcars),
                       rhs = TRUE, multiple = TRUE)
# Error in <text>:1:18: unexpected numeric constant
# 1: ind ~ factor(cyl)6
```

Polynomial and interaction terms (`poly(wt, 2)`, `hp:wt`) fail the same way;
plain predictors work. `bp_case_loop()` builds `as.formula(paste("ind ~",
nam[i]))` from model-matrix column names, which are not parseable expressions.
The same five calls were run against parent `293cca4`: the error text is
identical, so this predates follow-up 2 and is not caused by the p-value
change. It belongs in a separate issue; it is noted here because the
regression grid exposed it.

Other acknowledged items from the earlier audits (stale pkgdown content,
zero-byte visual SVGs and documentation of the normality domain) are outside
this display-only re-check and were not re-examined.

## Reproduction and retained evidence

R 4.6.1 on Windows 11; devtools 2.5.2, testthat 3.3.2, pkgload 1.5.3,
ggplot2 4.0.3, nortest 1.0.4, vdiffr 1.0.9, withr 3.0.3. Audit analyses ran
from script files rather than shell pipelines. Scratch
directory (outside the repository):
`C:/Users/DELL/AppData/Local/Temp/olsrr-c25-rx2-20261119/`.

```powershell
$rx2 = 'C:/Users/DELL/AppData/Local/Temp/olsrr-c25-rx2-20261119'
# Parent tree: git archive --format=tar -o "$rx2/parent.tar" 293cca4, extracted to $rx2/parent.
Rscript (Join-Path $rx2 'run.R') test
Rscript (Join-Path $rx2 'run.R') check
Rscript (Join-Path $rx2 'precision.R')
Rscript (Join-Path $rx2 'model-grid.R') (Join-Path $rx2 'parent') (Join-Path $rx2 'grid-parent.rds')
Rscript (Join-Path $rx2 'model-grid.R') 'F:/R/Packages/_worktrees/olsrr-c25' (Join-Path $rx2 'grid-head.rds')
Rscript (Join-Path $rx2 'compare-grid.R')
Rscript (Join-Path $rx2 'diff-step.R')
Rscript (Join-Path $rx2 'snapshot-verify.R')
Rscript (Join-Path $rx2 'transform-table.R')
Rscript (Join-Path $rx2 'align-check.R')
Rscript (Join-Path $rx2 'extra-check.R')
Rscript (Join-Path $rx2 'bp-configs.R')
Rscript (Join-Path $rx2 'norm-skip.R') (Join-Path $rx2 'parent')
Rscript (Join-Path $rx2 'norm-skip.R') 'F:/R/Packages/_worktrees/olsrr-c25'
Rscript (Join-Path $rx2 'bp-cases.R') (Join-Path $rx2 'parent')
```

Logs retain the evidence: `full-test.log`, `full-check.log`, `precision.log`,
`grid-parent.log`, `grid-head.log`, `compare-grid.log`, `diff-step.log`,
`snapshot-verify.log`, `transform-table.log`, `align-check.log`,
`extra-check.log`, `bp-configs.log`, `bp-cases-*.log`, `norm-skip-*.log`.
`grid-*.rds` and `full-test-results.rds` retain the compared structures.

No High finding and no ordinary-input regression was found. M1 is fixed,
without changing any returned numeric value, and the changed snapshots contain
only the expected p-value formatting.

## Full R CMD check (completed after the agent exited; recorded by Claude 2026-11-19)

```text
Status: OK
0 errors | 0 warnings | 0 notes (elapsed 1537 s)
tests: [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3694 ]
```

Ready to merge into develop: yes
