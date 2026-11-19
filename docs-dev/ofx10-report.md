# pkg-ofx10 — olsrr 0.7.1 fix round 10

Date: 2026-11-14. Worktree: `F:/R/Packages/statistics/olsrr-wt/fix-071`.
Branch: `task/olsrr-fix-071`. Baseline: `d95c76ddae3dd8747a16db87595568ef556aa389`.
Implementation: `5964c54` (`fix: restore predictive offset calls for stepwise models (P01) (ofx10)`).
Instructions read: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`,
AGENTS.md, and P01 and its reproduction in
`F:/pm/notes/2026-11-14-pkgs/ofxx9-reaudit.md`.

## Finding and implementation

| Finding | Regression coverage | Fix | Verification | Status |
|---|---|---|---|---|
| P01 (High): returned offset models retain training vectors/data/closures and predict incorrectly | `tests/testthat/test-step-offset-prediction.R`: all six selectors, call/formula/mixed offsets, seven-row and changed-offset same-size newdata, compact calls, subset/NA/weights/contrasts preservation, and exact-convergence rejection | `R/ols-stepwise-utils.R:780`, `ols_step_final_model()` | Direct `stats::lm()` predictions and exact frozen-fit identity; baseline failures reproduced; full tests and clean archive check below | Fixed |

Selection candidate fits, standardized-beta refits, and their frozen total
offset calculations are unchanged. For a returned offset model, the final
helper starts with the original model call, uses `stats::lm` as its head,
replaces the predictors in its formula with the selected terms, and restores
the formula offset expressions in their original addition order. It retains
the formula environment and the original `data`, `offset`, `subset`, `weights`,
`na.action`, and other call arguments. Contrasts are resolved through
`model_contrasts(model, form)` and supplied as matrices for the retained factors.
The resulting call is evaluated in the formula environment.

Before returning, the helper requires `identical()` coefficients and unpadded
fitted values against the frozen-offset selection refit of the same terms,
and an `identical()` total model-frame offset against the original fit. There
is no tolerance or numerical recovery fallback. A mismatch stops with:

```text
the final model cannot reproduce the frozen-offset selection fit exactly; prediction with newdata is not supported for this model.
```

This is the chosen error policy permitted by the job brief. For example,
dropping a predictor that contained an omitted value can make the original
call fit an additional row; the new test requires this error in all six APIs.
The O02 cancellation witness itself passes the exact checks when its formula
offset addition order is preserved, so it continues to return the correct
model. Offset models no longer take an early return around call construction.
The existing construction path for models without offsets is unchanged.

## Files changed

- `R/ols-stepwise-utils.R`: original-call final offset refit and exact identity guard.
- `R/ols-regression.R`: shared roxygen details distinguish frozen computation
  refits from returned prediction models and document the error policy.
- `NEWS.md`: P01 bullet under the 0.7.1 Bug Fixes heading.
- `tests/testthat/test-step-offset-prediction.R`: new P01 regression file.
- `tests/testthat/test-refit-identity.R`: retain O01/O02 tests, add exact O02
  fitted-value/call/formula checks, and require the returned formula offset.
- `tests/testthat/test-step-backward.R`: direct reference now uses the returned
  formula's restored offset instead of adding that offset a second time.
- `docs-dev/ofx10-report.md`: this report.

The following 21 manuals were regenerated with `devtools::document()` from
the shared roxygen details; none was edited directly:

```text
man/ols_regress.Rd
man/ols_step_all_possible.Rd
man/ols_step_backward_adj_r2.Rd
man/ols_step_backward_aic.Rd
man/ols_step_backward_p.Rd
man/ols_step_backward_r2.Rd
man/ols_step_backward_sbc.Rd
man/ols_step_backward_sbic.Rd
man/ols_step_best_subset.Rd
man/ols_step_both_adj_r2.Rd
man/ols_step_both_aic.Rd
man/ols_step_both_p.Rd
man/ols_step_both_r2.Rd
man/ols_step_both_sbc.Rd
man/ols_step_both_sbic.Rd
man/ols_step_forward_adj_r2.Rd
man/ols_step_forward_aic.Rd
man/ols_step_forward_p.Rd
man/ols_step_forward_r2.Rd
man/ols_step_forward_sbc.Rd
man/ols_step_forward_sbic.Rd
```

## Before and after: P01 reproduction

The audit case was run against clean baseline and implementation archives in
separate R processes (`.ofx10-tmp/witnesses-base.log`, `witnesses-head.log`):

```r
d_train <- mtcars[1:25, ]
d_test <- mtcars[26:32, ]
m <- lm(mpg ~ wt + hp + qsec, data = d_train, offset = disp * 0.01)
fm <- ols_step_forward_p(m)$model
predict(fm, newdata = d_test)
d_same <- d_train
d_same$disp <- d_same$disp + 1000
ref <- lm(mpg ~ wt + hp, data = d_train, offset = disp * 0.01)
max(abs(predict(fm, newdata = d_same) - predict(ref, newdata = d_same)))
print(fm)
```

The same-size control deliberately has 25 rows, matching the training data,
so the baseline's wrong numbers occur silently, without recycling warnings.
All six selectors select `wt + hp` and have the following results:

| Evidence | Baseline d95c76d | Fixed |
|---|---|---|
| Seven-row newdata prediction length | 25 | 7 |
| Recycling warnings for seven-row prediction | 1 | 0 |
| Maximum prediction error, changed-offset same-size data | 10.000000000000004 | 0 |
| Printed model lines (`width = 110`, `digits = 17`) | 111 | 9 |
| Call head | anonymous closure | `stats::lm` |
| Call data | embedded training frame | `d_train` |
| Call offset | embedded training vector | `disp * 0.01` |

Fixed seven-row predictions, exactly identical to the independent direct fit:

```text
Fiat X1-9         27.5605787272649501
Porsche 914-2     25.7926772375821258
Lotus Europa      26.6200921044351517
Ford Pantera L    14.4510630284108945
Ferrari Dino      18.9302259395219359
Maserati Bora      8.4339765777344713
Volvo 142E        22.4007005619192476
```

The fixed printed call is:

```r
stats::lm(formula = mpg ~ wt + hp, data = d_train, offset = disp *
    0.01)
```

The new regression file was first run before the source fix. The final file
was also replayed against clean d95c76d with a silent testthat reporter to
retain complete counts (`.ofx10-tmp/baseline-regressions.log`):

```text
 failed skipped error warning passed
    198       0     0      42     84
```

The warnings are the baseline's recycled training offsets. Formula-only
`offset(log(wt))` and mixed formula/call offsets are also tested for both newdata
sizes; the same-size data changes both `disp` and `wt`. The retained-factor
case uses `g` and drops `h`, asserting only the resolved `g` contrast matrix is
in the call. It also checks the original subset, unit-weight expression and
`na.exclude` expression, fitted values, coefficients and newdata predictions.

## O01/O02 and compatibility evidence

The O01 custom-name rejection and positive resolved-contrast tests remain
intact. The O02 independent standardized-beta reference remains intact.
For the exact cancellation reproduction:

```r
d <- mtcars
d$a <- d$b <- d$c <- d$o <- 0
d$a[17] <- -1e15
d$b[17] <- 1e15
d$c[17] <- -4.687491
d$o[17] <- 16.088774
m <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d, offset = o)
```

All six include-all selectors retain row 17 total offset **11.5**, coefficients
identical to `coef(m)`, and fitted values identical to `m$fitted.values`.
The call offset remains `o`, and all three formula offsets remain in the
returned terms. The nearby formula-only cancellation case and the original
subset/`na.exclude` round trip also pass.

Documentation extraction scans all `man/*.Rd`, all `vignettes/*.Rmd`, and
README.Rmd (113 Rd files and seven vignette Rmd files on this HEAD). Both
baseline and implementation manifests contain **189
occurrences across 109 source documents, representing 19 distinct fitting
calls**. Separate clean R sessions compare baseline and fixed implementations.
The full-object comparison canonicalizes environment identity only; it does
not round numbers or remove result components or attributes.

```text
examples MODELS 19 RESULTS 133 IDENTICAL TRUE
examples-full MODELS 19 RESULTS 133 IDENTICAL TRUE
controls MODELS 43 RESULTS 559 IDENTICAL TRUE
EXAMPLE ERRORS 0 WARNINGS 0
```

The 133 comparisons comprise 19 standardized-beta vectors and 114 selector
results (six selectors per model). The 559 controls retain base named and
explicit contrasts, saved and unsaved x, missing call-data fallback, changed
contrast options, and ordinary call/formula offsets. Numerical control results
remain identical; returned offset call/formula representation intentionally
changes as described above.

An additional check compares ordinary returned-model coefficients and both
training predictions and predictions on reversed newdata, plus exact
`all.equal()` comparisons of coefficients, calls and terms:

```text
ORDINARY COEFFICIENTS AND TRAINING/REVERSED-NEWDATA PREDICTIONS IDENTICAL: 114 selector results
ORDINARY COEFFICIENT/CALL/TERMS all.equal(tolerance = 0): 114 selector results
```

## Commands and validation

All new scratch is inside `.ofx10-tmp/`. Existing audit/fix scratch and
`docs-dev/ofxx9-reaudit.md` were read only and left unchanged. Existing scripts
from `.ofx9-tmp/` were copied into new scratch to repeat the established
133-comparison and 559-control methodology. Edits were made with `apply_patch`.
No dependency, data set, version or maintainer metadata was changed.

Heavy commands ran through `bash ~/.claude/bin/heavy-slot.sh pkg-ofx10`.
The wrapper reports that `AGENT_QUEUE_JOB=pkg-ofx10` already holds the queue
slot. R commands used `LC_ALL=C LANG=C`; full tests/checks set `NOT_CRAN=true`.
Validation used R 4.6.1 (2026-06-24 ucrt), x86_64-w64-mingw32, on Windows 11
x64 build 26300, with olsrr 0.7.1 for both archives.
Principal commands, from the package root unless indicated:

```text
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 Rscript -e 'devtools::document()'
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 Rscript .ofx10-tmp/run-targeted.R
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 Rscript .ofx10-tmp/run-tests.R
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 Rscript .ofx10-tmp/baseline-regressions.R
git archive --format=tar -o .ofx10-tmp/base.tar d95c76d
tar -xf .ofx10-tmp/base.tar -C .ofx10-tmp/base
git archive --format=tar -o .ofx10-tmp/head.tar HEAD
tar -xf .ofx10-tmp/head.tar -C .ofx10-tmp/head
bash .ofx10-tmp/run-comparisons.sh
bash .ofx10-tmp/run-predictions.sh
bash .ofx10-tmp/check-implementation.sh
bash .ofx10-tmp/check-fixed.sh
bash .ofx10-tmp/check-final.sh
git add R/ols-stepwise-utils.R R/ols-regression.R man NEWS.md tests/testthat/test-refit-identity.R tests/testthat/test-step-backward.R tests/testthat/test-step-offset-prediction.R
git commit -m 'fix: restore predictive offset calls for stepwise models (P01) (ofx10)'
git commit --amend --no-edit
git add docs-dev/ofx10-report.md
git commit -m 'docs: record P01 evidence and validation (ofx10)'
```

The comparison scripts run the following through the heavy-slot wrapper from
`.ofx10-tmp/`, using both `base` and `head` for `<version>`:

```text
Rscript examples.R <version>
Rscript examples-full.R <version>
Rscript controls.R <version>
Rscript extract-manifest.R <version>
Rscript predictions.R <version>
Rscript witnesses.R <version>
Rscript compare-all.R
Rscript compare-predictions.R
```

The initial new-test run used `devtools::test(filter =
"step-offset-prediction", reporter = "summary", stop_on_failure = FALSE)`.
An intermediate targeted invocation passed an R expression containing `|`
through the Windows launcher, which parsed it as a shell pipe. It ran no tests;
the invocation was replaced with the R script above. Early targeted tests
also found the old references that stripped/double-added formula offsets;
those references were updated to the new returned formula. No snapshot was
accepted or regenerated.

Final targeted result (`.ofx10-tmp/final-targeted.log`):

```text
 failed skipped error warning passed
      0       0     0       0    544
```

Full development suite: `run-tests.R` calls
`devtools::test(reporter = "summary", stop_on_failure = TRUE)` with
`NOT_CRAN=true`, then prints/saves the result counts. Result
(`.ofx10-tmp/full-tests.log`, repeated after the namespace correction in
`final-full-tests.log`; `tests.rds`):

```text
 failed skipped error warning passed
      0       0     0       0   2982
```

### Clean archive build and CRAN-style check

Implementation check results and final-commit verification are recorded below.
The final verification script archives HEAD only after the report commit and
records the exact checked hash in `.ofx10-tmp/final/commit.txt`.

The first implementation check reached examples but reported an additional
code-analysis NOTE for an unqualified `coef()` in the new guard. That attempt
was stopped; the guard now uses `stats::coef()`. The full development suite
was repeated and a fresh clean archive was built for the corrected check.
The initial attempt's logs are retained under `.ofx10-tmp/implementation/`;
the corrected implementation's logs are under `.ofx10-tmp/verified/`.

Corrected implementation archive `5964c54`: clean source build passed,
including vignette creation. `R CMD check --as-cran --no-manual` completed
with **0 errors, 0 warnings, 1 NOTE**. Its sole NOTE is the expected maintainer
address change:

```text
* checking CRAN incoming feasibility ... [106s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Relevant completed check output:

```text
* checking R code for possible problems ... [27s] OK
* checking examples ... [62s] OK
* checking examples with --run-donttest ... [197s] OK
* checking tests ...
  Running 'testthat.R' [318s]
 [318s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [42s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 NOTE
```

The installed-package test output independently records
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2982 ]`. Logs are
`.ofx10-tmp/verified/build.log`, `check.log`,
`olsrr.Rcheck/00check.log`, and `olsrr.Rcheck/tests/testthat.Rout`.

The report commit changes only `docs-dev/`, which is excluded from the R
build. The required verification after that final commit is run with
`bash .ofx10-tmp/check-final.sh`, whose complete sequence is:

```sh
export LC_ALL=C LANG=C NOT_CRAN=true
mkdir -p .ofx10-tmp/final/source
git rev-parse HEAD > .ofx10-tmp/final/commit.txt
git archive --format=tar -o .ofx10-tmp/final/source.tar HEAD
tar -xf .ofx10-tmp/final/source.tar -C .ofx10-tmp/final/source
cd .ofx10-tmp/final
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 R CMD build --no-manual source > build.log 2>&1
bash ~/.claude/bin/heavy-slot.sh pkg-ofx10 R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz > check.log 2>&1
```

Final-commit confirmation logs are `.ofx10-tmp/final/build.log`,
`.ofx10-tmp/final/check.log`, `.ofx10-tmp/final/olsrr.Rcheck/00check.log`, and
`.ofx10-tmp/final/olsrr.Rcheck/tests/testthat.Rout`. The final outcome is
reported to the user after this confirmation finishes; no package source,
test, or manual changes follow the corrected implementation check.

## Deferred work and follow-ups

No P01 work is deferred. The intentional behavior change is a hard error when
the original-call final fit cannot reproduce selection coefficients, fitted
values and total offset exactly. It prevents returning a model whose
prediction behavior represents a different fit. Models without offsets retain
their previous behavior.

The maintainer NOTE is explained in full: the maintainer remains Aravind
Hebbali; the email address changed intentionally from
`hebbali.aravind@gmail.com` to `aravindhebbali@rsquaredacademy.com`. This round
does not modify that metadata. Explain the address change in future CRAN
submission correspondence. CRAN submission itself is outside this task.

No push, gh/GitHub action, branch switch, or modification of another worktree
was performed. All commits end in `(ofx10)`.
