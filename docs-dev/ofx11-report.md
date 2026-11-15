# pkg-ofx11 — olsrr 0.7.1 fix round 11

Date: 2026-11-14. Worktree: `F:/R/Packages/statistics/olsrr-wt/fix-071`.
Branch: `task/olsrr-fix-071`. Baseline: `4017adc7ebbf33ba9f49c0d12e6945a0e964ca6e`.
Implementation: `fc97dc8`; explicit tie assertions: `f25a417`.
Instructions read: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`,
root AGENTS.md, `F:/pm/notes/2026-11-14-pkgs/ofxx10-reaudit.md`, the previous
fix report, and relevant sources/tests. No pushes, GitHub operations, branch
switches, or changes to other worktrees were made. Existing `.ofx*` scratch
and untracked `docs-dev/*reaudit.md` were left unchanged. New scratch is
confined to `.ofx11-tmp/`.

## Findings, tests and fixes

| Finding | Regression test | Fix | Independent verification | Status |
|---|---|---|---|---|
| P02 High: attributes falsely fail exact offset identity | `tests/testthat/test-step-selection-round11.R`: built-in auto and haven-style response, all six selectors, changed-offset seven-row newdata; cancellation mismatch | `R/ols-stepwise-utils.R:780`, `ols_step_final_model()` | Coefficients/fitted values as plain numeric vectors and newdata predictions against direct `lm()`; preserved O02 and P01 tests | Fixed |
| P03 High: more than one tied term selected and incorrect histories | Same test file: exact audit witness, exact backward F/AIC ties, both-p entry ties, all criterion families, forced terms, removals after locked terms, beta/p-value history | `R/ols-stepwise-utils.R:830`/`:840`; forward `R/ols-stepwise-forward-regression.R:207`/`:308`, backward `R/ols-stepwise-backward-regression.R:175`, both `R/ols-stepwise-regression.R:196`/`:257`/`:295`; `R/ols-step.R:435` backward SBIC | Each recorded transition changes exactly one term; every row's R2, adjusted R2, AIC, SBC, SBIC, Cp, RMSE and available coefficient/p-value history compared with independently reconstructed `lm()` | Fixed |
| P04 Medium: empty index from all-NA candidate F tests crashes | Same test file: exact x1=wt/x2=-wt witness, all-constant candidates, forward/backward/both p and hierarchical paths | Shared F/p ranking `R/ols-stepwise-utils.R:830` filters missing tests; callers handle no valid index; hierarchical conditions `R/ols-stepwise-hierarchical.R:127`/`:249` use `isTRUE()` | Exact reference coefficients for `mpg ~ x1 + hp`; no warning/empty-index crash; clear existing entry error when no initial candidate qualifies | Fixed |

`ols_step_final_model()` compares coefficients, fitted values and total offsets
using `identical(unname(as.numeric(...)), unname(as.numeric(...)))`. No rounding,
tolerance, coefficient substitution or fallback is used. Ancillary column
attributes are ignored; any numeric mismatch still raises the existing clear
frozen-offset convergence error.

P-value ranking picks largest F for entry, smallest F for removal, then smallest
p, then original candidate order. It returns exactly one integer or NA when no
eligible candidate remains. Infinite F is valid, as a perfect candidate fit can
have F=Inf and p=0. Missing F or p tests are skipped. Entry at the first step
uses the selected candidate's p value. Subsequent exhausted searches end
normally; forward/both searches with no initial eligible term retain their
existing entry-criteria error. Criterion ties use `which.min()`/`which.max()`
and retain candidate order; the existing backward criterion sort already
returns a single, stable first candidate. Forced forward predictors retain the
existing one-term-at-a-time initialization history.

Two additional incorrect history values were exposed by the required independent
fit assertions and corrected under P03: both-p removal labels now account for
locked predictors, and generic backward selectors calculate history SBIC against
the original full model, consistently with their candidate SBIC calculations.
Previously `mi$sbic` compared each step to itself. Leaving that number unchanged
would violate the instruction to fix every known wrong result.

## Files changed

- `R/ols-stepwise-utils.R`: numeric identity checks, shared F/p ranking, scalar criterion index.
- `R/ols-stepwise-forward-regression.R`: select and test one candidate, including at the initial step; handle missing candidate tests.
- `R/ols-stepwise-backward-regression.R`: scalar F/p removal ranking and exhausted-search handling.
- `R/ols-stepwise-regression.R`: scalar entry/removal ranking, missing candidate handling, correct removal variable after included terms.
- `R/ols-stepwise-hierarchical.R`: missing p tests terminate selection normally.
- `R/ols-step.R`: backward history SBIC uses the original full model.
- `NEWS.md`: separate P02, P03 and P04 bullets in 0.7.1 Bug Fixes.
- `tests/testthat/test-step-selection-round11.R`: numeric regression coverage for all three findings and related histories.
- `tests/testthat/_snaps/stepaic-b-output.md` and `step-rsquared-output.md`: accepted only corrected backward SBIC display cells.
- `docs-dev/ofx11-report.md`: this report.

No exported signature, dependency, package data, version or maintainer metadata
changed. No generated manual or other documentation source needed changing.
Output snapshots retain the existing table whitespace; `git -c
core.whitespace=-blank-at-eol diff --check` passes.

## Before/after reproductions

The audit cases were replayed in separate baseline/current R sessions using
`.ofx11-tmp/witnesses.R`. Full outputs are in `witnesses-base.log` and
`witnesses-head.log` in that directory.

P02 exact audit model:

```r
d <- auto
m <- lm(mpg ~ weight + length + displacement, d, offset = displacement / 10)
```

Baseline: all six selectors stop with `the final model cannot reproduce the
frozen-offset selection fit exactly; prediction with newdata is not supported
for this model.` Fixed: all six return; predictions on `d[1:7, ]` after adding
100 to displacement are exactly identical to a direct `lm()` of the selected
terms with `offset = displacement / 10`. The selected terms are:

| Selector | Returned terms | Steps |
|---|---|---:|
| forward_aic | displacement, weight, length | 3 |
| backward_aic | weight, length, displacement | 0 |
| both_aic | displacement, weight, length | 3 |
| forward_p | displacement, weight, length | 3 |
| backward_p | weight, length, displacement | 0 |
| both_p | displacement, weight | 2 |

The nearby response is explicitly constructed without adding a dependency:

```r
d$mpg <- structure(as.numeric(d$mpg), label = "Mileage", labels = c(low = 12),
                   class = c("haven_labelled", "numeric"))
```

All six failed before and succeed after, with the same exact prediction checks.
This tests haven-style class/label/value-label metadata; the haven package is
not installed in this environment, so no claim is made about a separate live
haven/vctrs package test.

P03 exact audit construction:

```r
set.seed(123)
n <- 100
y <- scale(rnorm(n))
e1 <- scale(resid(lm(rnorm(n) ~ y)))
e2 <- scale(resid(lm(rnorm(n) ~ y + e1)))
r <- 0.35
x1 <- as.numeric(r * y + sqrt(1 - r^2) * e1)
a <- 0.9 * sqrt(1 - r^2)
b <- sqrt(1 - r^2 - a^2)
x2 <- as.numeric(r * y + a * e1 + b * e2)
d <- data.frame(y = as.numeric(y), x1 = x1, x2 = x2, x3 = rnorm(n))
m <- lm(y ~ x1 + x2 + x3, d)
ols_step_forward_p(m, p_val = 0.001)
```

Both initial F values are exactly `13.680911680911679`. The conditional x2 p
value is `0.43097892082625766`. Baseline returns `x1 + x2`, two steps, AIC
`275.71484069137273` then `277.07217338662844`. Fixed returns only `x1`, one
step, AIC `275.71484069137273`, R2 `0.12249999999999998`, adjusted R2
`0.11354591836734695`. Both-p also retains only x1 under the same entry threshold.

The constructed backward tie uses all combinations of four -1/+1 Walsh columns,
with `y = 10*z + 3*e + (x1+x2)/4`. Both conditional F tests and the AICs from
removing x1 or x2 are asserted `identical()`; backward-p removes x1 then x2.
Identical predictor columns provide exact entry ties for AIC. Every recorded
row is rebuilt independently. The locked-removal example uses
`ols_step_both_p(lm(y ~ ., stepdata), include = "x1")`: the removal is correctly
labelled x6 and its metrics/coefficient history match the independently fitted
remaining terms.

P04 exact audit construction:

```r
d <- mtcars
d$x1 <- d$wt
d$x2 <- -d$wt
m <- lm(mpg ~ x1 + x2 + hp, d)
ols_step_both_p(m)
```

Baseline emits `no non-missing arguments to max; returning -Inf` and errors
`argument is of length zero`. Fixed returns x1 then hp in two addition steps;
coefficients are exactly identical to `lm(mpg ~ x1 + hp, d)`. The two rows have
R2 `0.75283279365826439`, `0.82678545188279118` and AIC
`166.02942899187622`, `156.65233882564132`. Forward-p also skips the aliased x2.
All-constant forward/both candidates give the clear existing entry-criteria
error; backward and hierarchical searches terminate with zero step rows.

O02 control, retained from the previous round:

```r
d <- mtcars
d$a <- d$b <- d$c <- d$o <- 0
d$a[17] <- -1e15
d$b[17] <- 1e15
d$c[17] <- -4.687491
d$o[17] <- 16.088774
m <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d, offset = o)
```

All six include-all selectors retain total offset 11.5 at row 17 and exact
original coefficients. This original addition order is reproducible and was
already accepted at 4017adc; the audit explicitly records that fact. A nearby
reordered-offset mismatch is still rejected with the exact-convergence error.
The existing P01 tests also verify all six selectors reject a final refit that
would expand the fitted row set after dropping a predictor with a missing value.
Thus reproducible cancellation passes and numeric cancellation mismatches stop.

## Examples/vignettes compatibility

Extraction scans all `man/*.Rd`, all `vignettes/*.Rmd`, and README.Rmd. It finds
189 fitting-call occurrences, 19 distinct model calls, and the expected **133
comparisons** (19 standardized-beta vectors plus 114 results from six selectors).
Separate clean R processes run `examples.R` and `examples-full.R`; the latter
compares every object component and attribute, canonicalizing environment
identity only. No rounding or component stripping is used in the initial exact
comparison. Computation errors: 0; warnings: 0.

**119/133 full results are exactly identical to 4017adc. Fourteen differ solely
in the corrected backward-AIC history SBIC cells.** All 133 have identical
selected terms and all other components. An explicit second comparison removes
only `metrics$sbic` from the 14 differing results and requires the entire
remaining objects to be identical. No documentation selection history changes
because of a real tie.

The requested 133-identical target therefore has this additional, necessary
exception: preserving the old backward SBIC numbers would return a known wrong
metric, contrary to both the independent-step-metric requirement and the common
fix-round correctness rule. The 14 affected calls (all `ols_step_backward_aic`)
are fully listed below:

- `lm(mpg ~ disp + hp + wt + qsec, data = mtcars)`
- `lm(mpg ~ disp + hp + wt, data = mtcars)`
- `lm(mpg ~ disp + hp + wt + drat, data = mtcars)`
- `lm(mpg ~ disp + hp + wt + qsec + +drat, data = mtcars)`
- `lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)`
- `lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)`
- `lm(mpg ~ 0 + wt + cyl + hp + disp, data = mtcars)`
- `lm(mpg ~ 0 + wt + cyl + hp + disp + gear + drat, data = mtcars)`
- `lm(mpg ~ ., data = mtcars)`
- `lm(mpg ~ disp + hp + drat + wt + qsec, data = mtcars)`
- `lm(y ~ ., data = surgical)`
- `lm(y ~ bcs + alc_heavy + pindex + age + alc_mod, data = surgical)`
- `lm(y ~ ., data = stepdata)`
- `lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)`

For example, surgical backward history previously printed SBIC 583.884,
581.290, 578.844; the independent-full-model values are 584.276, 581.938,
579.638. Selection paths, coefficient estimates and final models are unchanged.
Full precision before/after values and exact difference output are retained in
`.ofx11-tmp/comparisons.log`, with call lists in `examples-differences.csv` and
`examples-full-differences.csv`.

## Commands and checks

All R/heavy work runs via `bash ~/.claude/bin/heavy-slot.sh pkg-ofx11`; the wrapper
reports that the existing pkg-ofx11 queue job already owns its slot. Bash scripts
set `LC_ALL=C LANG=C`; full tests and check scripts also set `NOT_CRAN=true`.
Environment: R 4.6.1 (2026-06-24 ucrt), x86_64-w64-mingw32, Windows 11 build 26300.

Source/test/NEWS edits used `apply_patch`; temporary Python scripts restored the
original forced-predictor initialization and normalized only edited source files'
line endings. No repository-wide restyling was done. The first new-test invocation
found a closing-brace typo in the test helper before any tests ran; it was fixed
and the before-fix tests were then run successfully as a reproduction. The first
probe tried `library(olsrr)` although this worktree package is not installed in the
shared library; the replacement probe uses `devtools::load_all()`.

Principal reproducible commands (from the worktree unless specified):

```text
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 Rscript .ofx11-tmp/targeted.R
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 Rscript .ofx11-tmp/baseline-tests.R
bash .ofx11-tmp/run-tests.sh
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 Rscript .ofx11-tmp/accept-snapshots.R
bash .ofx11-tmp/final-tests.sh
git archive --format=tar -o .ofx11-tmp/base.tar 4017adc
tar -xf .ofx11-tmp/base.tar -C .ofx11-tmp/base
bash .ofx11-tmp/compare-base.sh
git archive --format=tar -o .ofx11-tmp/current.tar HEAD
tar -xf .ofx11-tmp/current.tar -C .ofx11-tmp/head
bash .ofx11-tmp/compare-head.sh
bash .ofx11-tmp/compare.sh
bash .ofx11-tmp/witnesses.sh
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 Rscript .ofx11-tmp/tie-probe.R
bash .ofx11-tmp/check-implementation.sh
git -c core.whitespace=-blank-at-eol diff --check
git add NEWS.md R/ols-step.R R/ols-stepwise-backward-regression.R R/ols-stepwise-forward-regression.R R/ols-stepwise-hierarchical.R R/ols-stepwise-regression.R R/ols-stepwise-utils.R tests/testthat/test-step-selection-round11.R tests/testthat/_snaps/step-rsquared-output.md tests/testthat/_snaps/stepaic-b-output.md
git commit -m 'fix: correct stepwise identity, ties and aliased candidates (P02/P03/P04) (ofx11)'
git add tests/testthat/test-step-selection-round11.R
git commit -m 'test: verify constructed F and AIC ties are exact (P03) (ofx11)'
```

`targeted.R` calls `devtools::test(filter = "step-selection-round11")`. The first
valid baseline run recorded 5 failed assertions, 2 errors and 108 passes,
including the exact P02/P03/P04 witnesses and the independent SBIC failures.
The expanded baseline replay recorded 27 failures, 3 errors and 287 passes;
errors include the missing new shared helper. The corrected targeted run before
the final three exact-tie assertions recorded 361 passes, 0 failures/errors,
0 warnings/skips. The final full suite includes the additional three assertions.

The first full development run found exactly 12 snapshot failures, 0 errors,
0 warnings/skips and 3309 passes. The failures were the corrected backward SBIC
cells. The two specific snapshot files were reviewed and updated using:

```r
testthat::snapshot_accept("stepaic-b-output", path = "tests/testthat")
testthat::snapshot_accept("step-rsquared-output", path = "tests/testthat")
```

No plot/visual snapshot was accepted or changed.

Final full development suite (`devtools::test(reporter = "summary",
stop_on_failure = TRUE)`, NOT_CRAN=true), `.ofx11-tmp/final-full-tests.log`:

```text
 failed skipped   error warning  passed
      0       0       0       0    3346
```

The new regression file contributes 364 assertions. The full test result is
also saved as `.ofx11-tmp/final-tests.rds`. All visual tests passed without
changing visual snapshots.

Clean archive build/check of implementation `fc97dc8` completed successfully.
`R CMD build --no-manual source` created all vignettes and the source tarball.
`R CMD check --as-cran --no-manual` returned **0 errors, 0 warnings, 1 NOTE**.
The only NOTE is the expected maintainer address change:

```text
* checking CRAN incoming feasibility ... [97s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Relevant completed output (`.ofx11-tmp/verified/check.log`):

```text
* checking examples ... [66s] OK
* checking examples with --run-donttest ... [188s] OK
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
  Running 'testthat.R' [317s]
 [317s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [39s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 NOTE
```

The installed-package test output records `[ FAIL 0 | WARN 0 | SKIP 0 | PASS
3343 ]`. This archive precedes the three explicit exact-tie assertions in
`f25a417`; the full development suite above includes them and passes 3346.
Package source and accepted snapshots are identical between these commits.
The final verification below includes all 3346 assertions. Build/check logs,
`00check.log`, and installed test output are under `.ofx11-tmp/verified/`.

The final report commit changes only build-excluded `docs-dev/`. After that
last commit, `.ofx11-tmp/check-final.sh` archives HEAD and runs the entire clean
build/check again. It records the exact hash in `.ofx11-tmp/final/commit.txt`;
its final result is confirmed in the completion message. The full command is:

```sh
export LC_ALL=C LANG=C NOT_CRAN=true
mkdir -p .ofx11-tmp/final/source
git rev-parse HEAD > .ofx11-tmp/final/commit.txt
git archive --format=tar -o .ofx11-tmp/final/source.tar HEAD
tar -xf .ofx11-tmp/final/source.tar -C .ofx11-tmp/final/source
cd .ofx11-tmp/final
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 R CMD build --no-manual source > build.log 2>&1
bash ~/.claude/bin/heavy-slot.sh pkg-ofx11 R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz > check.log 2>&1
```

Report/final verification commands:

```text
git add docs-dev/ofx11-report.md
git commit -m 'docs: record P02/P03/P04 evidence and validation (ofx11)'
bash .ofx11-tmp/check-final.sh
```

## Deferred work and follow-ups

No P02/P03/P04 defect is deferred. The exact-convergence error policy remains
in force, including numeric cancellation mismatches and final fits that expand
the observation set. Ordinary attributed-response models now pass.

The compatibility target has the fully enumerated SBIC correction exception
above; no selected documentation model changes. Re-audit should independently
verify those corrected numbers against `ols_sbic(step_fit, original_full_fit)`
and confirm F/p entry and removal and all criterion selectors under ties.

The haven package itself is unavailable locally; regression tests exercise
haven-style labels/classes without introducing a dependency. A re-audit with a
live haven-imported response is a useful additional environment check.

The maintainer-address-change NOTE is expected and is not changed in this task.
No CRAN submission or GitHub operation is authorized or performed here.
