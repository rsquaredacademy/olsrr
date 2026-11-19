# pkg-ofx9 — olsrr 0.7.1 fix round 9

Date: 2026-11-14. Worktree: `F:/R/Packages/statistics/olsrr-wt/fix-071`.
Branch: `task/olsrr-fix-071`. Baseline: `2d8b8300d02c944a79ddc4bea78f04430e7ee9fc`.
Implementation commit: `6f1a591` (`fix: freeze refit coding and offsets and correct NEWS (O01-O03) (ofx9)`).
Instructions: `common-pkgfix.md`, the supplied AGENTS.md, and O01–O03 in
`F:/pm/notes/2026-11-14-pkgs/ofxx8-reaudit.md`.

## Findings

| Finding | Tests | Source fix | Verification | Status |
|---|---|---|---|---|
| O01: mutable named contrasts change coding | `tests/testthat/test-refit-identity.R:6` and `:54` | `R/utils.R:102`, `:154`, `:227`, `:268`; `R/ols-regress_compute.R` | Same-column-name witness rejects in all seven APIs; 8 crossed custom-name variants; base names, explicit matrices, stored x, missing data, changed options and search-path shadows checked | Fixed |
| O02: mixed-offset subtraction changes the frozen total | `tests/testthat/test-refit-identity.R:92`; updated offset reference in `tests/testthat/test-step-backward.R:90` | `R/ols-regress_compute.R:185`; `R/ols-stepwise-utils.R:719`, `:747`, `:780`; removed offset recovery helpers from `R/utils.R` | Exact independent beta reference, exact offset and coefficients in all six selectors; formula-only cancellation and subset/na.exclude round trip | Fixed |
| O03: N04 NEWS bullet under 0.1.0 | Release/documentation assertion in `.ofx9-tmp/verify-release.R` | `NEWS.md:5`–`:7` | Bullet occurs once at line 7, inside 0.7.1 Bug Fixes; O01/O02 bullets added above it | Fixed |

Contrast resolution accepts matrices unchanged. Character entries must name
`contr.treatment`, `contr.sum`, `contr.helmert`, `contr.poly` or `contr.SAS`;
the functions are obtained with `getExportedValue("stats", name)`. Generated
matrices receive the same factor-level row names that R's `contrasts<-` installs,
including the otherwise unnamed rows of `contr.poly`. The plain-symbol fallback
checks this restriction before returning data. Matrix reconstruction and beta
calculation also use the resolved specification, so they do not invoke named
contrast functions on the search path. A stored x retains its numerical entries;
only its contrast specification attribute is expressed as the resolved matrices
for comparison. Design, response, weight and offset comparisons remain exact
`identical()` checks. No tolerances, QR reconstructions or name-only design
checks were added.

Offset refits use `stats::model.offset(model$model)`, the total retained by the
fitted model frame. Both candidate and standardized-beta formulas omit every
formula offset term. There is no formula-offset summation, subtraction or call
offset recovery. An offset whose length cannot match the fitted rows errors
instead of being dropped. Returned offset models retain their frozen refit data
and total in the call, without restoring the original subset or only the original
call contribution. This avoids subsetting an already shortened total twice.
Their formula/call representation intentionally changes; the numerical results
for the ordinary controls below remain identical. Custom named contrasts now
require an explicit matrix even if their function has not changed or x was saved.

## O01 before and after

The exact audit witness was run in separate R processes against clean baseline
and fixed archives (`.ofx9-tmp/witnesses-base.log`, `witnesses-head.log`):

```r
mycon <- function(n, contrasts = TRUE, sparse = FALSE) {
  contr.treatment(n, contrasts = contrasts, sparse = sparse)
}
d <- transform(mtcars, g = factor(cyl))
m <- lm(mpg ~ g * hp, d, contrasts = list(g = "mycon"))
original_X <- model.matrix(m)
mycon <- function(n, contrasts = TRUE, sparse = FALSE) {
  z <- contr.sum(n, contrasts = contrasts, sparse = sparse)
  colnames(z) <- c("6", "8")
  z
}
identical(colnames(original_X), colnames(model.matrix(m))) # TRUE
identical(original_X, model.matrix(m))                    # FALSE
ols_regress(m)$sbetas
```

Baseline beta output in order `g6`, `g8`, `hp`, `g6:hp`, `g8:hp`:

```text
 0.16459276470786177  0.16143864512916753 -0.51053167925596843
-0.49571388004706318  0.27654001033347902
```

The original explicit-matrix coding instead gives:

```text
 0.008144386521189928 -0.288477791207574441 -1.282944252796579310
 0.258194497698283176  0.746130640996480854
```

Before: all six include-all selectors return intercept `24.91231682623225`
instead of `35.98302563889810`; maximum coefficient error is
`26.379883318877908`.

After: `ols_regress()` and each of forward/backward/both AIC and p-value
selection stop with:

```text
olsrr can't verify the custom contrast 'mycon' used to fit this model. Refit it with an explicit contrast matrix (contrasts = list(g = <matrix>)).
```

The committed tests cross explicit named contrast versus fitting through
`options(contrasts=)`, found versus missing call data, and x FALSE versus TRUE:
8 variants, 56 API rejection assertions, with same-column-name checks and
serialized-input invariance. Positive tests shadow all five allowed names with
functions that error if called and change call-time options to
`c("contr.sum", "contr.poly")`. Named and matrix beta vectors remain identical;
include-all coefficients remain identical, including stored-x fits.

## O02 before and after

The exact audit reproduction and independent reference were run unchanged:

```r
d <- mtcars
d$a <- d$b <- d$c <- d$o <- 0
d$a[17] <- -1e15
d$b[17] <- 1e15
d$c[17] <- -4.687491
d$o[17] <- 16.088774
m <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d, offset = o)
ds <- transform(d, wt = as.numeric(scale(wt)), hp = as.numeric(scale(hp)))
ref <- lm(mpg ~ wt * hp, ds, offset = model.offset(model.frame(m)))
expected <- coef(ref)[-1] * apply(model.matrix(ref)[, -1, drop = FALSE], 2, sd) /
  sd(m$model$mpg)
```

Input total at observation 17: `11.5`. Stored call contribution:
`16.088774000000001`.

| Term | Baseline beta | Independent reference and fixed beta |
|---|---:|---:|
| wt | -0.797522022640441253 | -0.79615900277092810 |
| hp | -0.331644409807831442 | -0.33181013035959417 |
| wt:hp | 0.099836296395248172 | 0.10137686309156362 |

Before: beta maximum error `0.001540566696315443`; all six selectors return
offset `11.625` and maximum coefficient error `0.054759440754580169`.

After: `identical(actual, expected)` is TRUE, maximum beta error is 0;
all six selectors return offset `11.5`, `identical(coef(result$model), coef(m))`
is TRUE and maximum coefficient error is 0. Input coefficients, reproduced
exactly by all six selectors, are:

```text
(Intercept) 44.770554879339933052
wt          -6.607496410390782948
hp          -0.066528465449868721
wt:hp        0.011612724865403306
```

The nearby formula-only cancellation case also has exact beta, offset and
coefficient assertions. A mixed-offset subset/na.exclude case checks exact
offsets and coefficients, absence of offset terms, and a subsequent
`ols_regress(result$model, iterm = TRUE)` call against the input beta vector.
The explicit `iterm = TRUE` selects the same scaling convention for the returned
formula, which expresses interactions with colons.

## Compatibility comparisons

Both archives independently extract 189 model-call occurrences in 109 source
documents, covering every man example, all six vignettes and README.Rmd:
19 distinct fitting calls. The manifests are `identical()`.

```text
examples MODELS 19 RESULTS 133 IDENTICAL TRUE
examples-full MODELS 19 RESULTS 133 IDENTICAL TRUE
controls MODELS 43 RESULTS 559 IDENTICAL TRUE
EXAMPLE ERRORS 0 WARNINGS 0
```

The 133 comparisons are 19 standardized-beta vectors plus 114 selection results
(six APIs per fit). The full-component comparison retains all selection and lm
components and attributes, including model frames, calls, formulas, QR, weights,
offsets and metrics; only environment identity is normalized across processes.
No values are rounded and no tolerance is used.

The additional controls cross all five allowed base names with explicit-matrix
versus named fitting, x FALSE/TRUE and found/missing call data (40 models), then
add single formula offset, single call offset and no offset (3 models). Every
call uses options `c("contr.sum", "contr.poly")`. Each model supplies one beta
vector, six default selections and six include-all selections: 43 × 13 = 559.
These compare complete selection metadata outside lm objects, and lm numerical
components including coefficients, design, response, residuals, fitted values,
rank, degrees of freedom, weights, offset, AIC and deviance. The matrix's contrast
specification attribute is omitted in this numerical control comparison because
refits now intentionally retain resolved matrices. Offset formula/call metadata
is likewise represented by its actual fitted values, rather than the obsolete
offset expression. The separate 133 complete-component comparisons omit neither.

## Files changed

Implementation and documentation sources (12): `R/utils.R`,
`R/ols-regress_compute.R`, `R/ols-stepwise-utils.R`, `R/ols-regression.R`,
`R/ols-all-possible-regression.R`, `R/ols-best-subsets-regression.R`,
`R/ols-stepaic-forward-regression.R`, `R/ols-stepaic-backward-regression.R`,
`R/ols-stepaic-both-regression.R`, `R/ols-stepwise-forward-regression.R`,
`R/ols-stepwise-backward-regression.R`, `R/ols-stepwise-regression.R`.

Generated documentation (22): `man/ols_regress.Rd`,
`man/ols_step_all_possible.Rd`, `man/ols_step_all_possible_betas.Rd`,
`man/ols_step_best_subset.Rd`, and `man/ols_step_{forward,backward,both}_{aic,p,sbc,sbic,r2,adj_r2}.Rd`.
All Rd edits were generated with `devtools::document()` from roxygen comments.
The base contrast and explicit-matrix restriction is in the requested APIs'
details and inherited by the related selection wrappers.

Tests: new `tests/testthat/test-refit-identity.R`, and the existing offset
reference in `tests/testthat/test-step-backward.R`.
Release record: `NEWS.md`. Report: `docs-dev/ofx9-report.md`.
No dependencies, snapshots, datasets, version numbers or NAMESPACE changed.

## Commands and checks

All heavy R commands use `bash ~/.claude/bin/heavy-slot.sh pkg-ofx9`.
Scratch files and clean archive copies are confined to `.ofx9-tmp/`.
R 4.6.1 (2026-06-24 ucrt), Windows 11 x64 build 26300.

The command sequence, with scratch scripts providing the R entrypoints, was:

```text
git archive --format=tar -o .ofx9-tmp/base.tar 2d8b830
tar -xf .ofx9-tmp/base.tar -C .ofx9-tmp/base
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 Rscript -e "devtools::test(filter=\"refit-identity\", stop_on_failure=TRUE)"
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 Rscript -e "devtools::document(); Sys.setenv(NOT_CRAN=\"true\"); devtools::test(stop_on_failure=TRUE)"
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 Rscript -e "devtools::document()"
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 Rscript .ofx9-tmp/run-targeted.R
git add R man NEWS.md tests/testthat/test-refit-identity.R tests/testthat/test-step-backward.R
git commit -m "fix: freeze refit coding and offsets and correct NEWS (O01-O03) (ofx9)"
git commit --amend --no-edit
git archive --format=tar -o .ofx9-tmp/head.tar HEAD
tar -xf .ofx9-tmp/head.tar -C .ofx9-tmp/head
```

The amend incorporated the stored-x contr.poly row-name fix and its test, and
removed a trailing blank line. From `.ofx9-tmp/`, each of the following Rscript
commands also ran through the heavy-slot wrapper, for both `base` and `head`
where shown:

```text
Rscript examples.R base
Rscript examples.R head
Rscript examples-full.R base
Rscript examples-full.R head
Rscript controls.R base
Rscript controls.R head
Rscript witnesses.R base
Rscript witnesses.R head
Rscript extract-manifest.R base
Rscript extract-manifest.R head
Rscript compare-all.R
Rscript verify-release.R
Rscript baseline-regressions.R
R CMD build head
R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
```

Final full development suite (from package root):

```text
LC_ALL=C LANG=C bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 Rscript .ofx9-tmp/run-tests.R
```

`run-tests.R` sets `NOT_CRAN=true`, calls
`devtools::test(reporter = "summary", stop_on_failure = TRUE)`, and prints/saves
the complete test result counts. Final targeted run: FAIL 0 | WARN 0 | SKIP 0 |
PASS 244. The complete development suite passes:

```text
 failed skipped   error warning  passed
      0       0       0       0    2682
```

The clean `git archive` build of implementation commit `6f1a591` passed, including
vignette creation. `R CMD check --as-cran --no-manual` then completed with
**0 errors, 0 warnings, 1 NOTE**, the expected maintainer-address change:

```text
* checking CRAN incoming feasibility ... [120s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>

* checking examples ... [65s] OK
* checking examples with --run-donttest ... [194s] OK
* checking tests ...
  Running 'testthat.R' [382s]
 [383s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [56s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 NOTE
```

The installed-package test output independently records
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2682 ]`. Logs:
`.ofx9-tmp/build.log`, `.ofx9-tmp/check.log`,
`.ofx9-tmp/olsrr.Rcheck/00check.log`,
`.ofx9-tmp/olsrr.Rcheck/tests/testthat.Rout`, and `.ofx9-tmp/final-tests.log`.

Initial regression runs failed against the unfixed implementation as required.
The final committed regression file was also replayed against the clean
2d8b830 archive: the custom-name test records 28 failed assertions and one error;
the offset test records 19 failed assertions. Its positive contrast test passes
all 75 assertions against the baseline. The first full development run had
exactly two failures in the old backward-selection test, which reconstructed an offset model from its
formula without supplying the now-explicit offset. Its independent reference
was corrected to include the frozen offset. An intermediate suite was stopped
when stored-x contrast coverage was expanded, then restarted with the final
sources and tests. Early shell/R entrypoint setup errors were corrected;
tests use base R and testthat without adding withr or rlang dependencies.
Later runs set `LC_ALL=C LANG=C` to avoid the host bash C.UTF-8 startup warnings.

The final report commit is also verified using `.ofx9-tmp/check-final.sh`: a new
archive of that final HEAD is extracted in `.ofx9-tmp/final/source`, its exact
commit is recorded in `.ofx9-tmp/final/commit.txt`, and the clean build and same
CRAN-style check are repeated with `NOT_CRAN=true`. Final confirmation logs are
`.ofx9-tmp/final/build.log` and `.ofx9-tmp/final/check.log`.
The final confirmation command is `bash .ofx9-tmp/check-final.sh`; its complete
build/check sequence is:

```sh
export LC_ALL=C LANG=C NOT_CRAN=true
mkdir -p .ofx9-tmp/final/source
git rev-parse HEAD > .ofx9-tmp/final/commit.txt
git archive --format=tar -o .ofx9-tmp/final/source.tar HEAD
tar -xf .ofx9-tmp/final/source.tar -C .ofx9-tmp/final/source
cd .ofx9-tmp/final
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 R CMD build source > build.log 2>&1
bash ~/.claude/bin/heavy-slot.sh pkg-ofx9 R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz > check.log 2>&1
```

## Deferred work and follow-ups

No O01, O02 or O03 work is deferred. The custom-name restriction and frozen
offset formula/call representation are intentional compatibility changes and
are documented. The prior missing-data restriction for transformed formulas
remains in force; this round does not relax it. No new tolerance or recovery
fallback was introduced.

CRAN submission is not part of this task. The confirmed maintainer-address-change
NOTE must be explained in submission correspondence;
the full explanation is: the maintainer remains Aravind Hebbali and the
maintainer email changed intentionally from `hebbali.aravind@gmail.com` to
`aravindhebbali@rsquaredacademy.com`. This round does not change maintainer
metadata. No push, gh/GitHub operation,
branch switch, other-worktree change or pre-existing .ofxx*-tmp edit was made.
