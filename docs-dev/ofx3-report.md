# ofx3 — olsrr fix round 3 report

Date: 2026-11-01. Worktree/branch: `task/olsrr-fix-071`. Base audited HEAD:
`245d6bbdb252bd5cdf606363ca7d82daf56aeda8` (`docs: ofx2 report...`).

Scope: the five findings in `F:/pm/notes/2026-11-01-pkg-audits/ofxx2-reaudit.md`
(G01–G05; 3 High, 2 Medium) and the seven "Partly" statuses that depend on them.
Each fix has an exact-case value test that failed before the implementation and
was checked against base R (`hatvalues()`, `rstudent()`, `summary()`, `anova()`)
or an independent weighted leave-one-out / refit calculation.

## Findings

| ID | Severity | Test added | Fix | Verified how | Status |
|---|---|---|---|---|---|
| G01 | High | `tests/testthat/test-weighted-diagnostics.R`: "leverages and PRESS ignore non-estimable QR directions (G01)"; "rank-deficient weighted PRESS keeps row alignment (G01)" | `R/utils.R:280` (`model_hatvalues_frame()` now maps positive-weight rows), `R/utils.R:321` (new `qr_hatvalues()`, first `rank` Q columns, rank 0 explicit) | Aliased `mpg ~ wt + I(2*wt) + hp` now has `rank = sum(h) = 3`, PRESS `246.5062590` and predicted R² `0.7810871`, exactly the equivalent `mpg ~ wt + hp` fit and `sum((resid/(1-hatvalues))^2)`. Weighted zero/NA/repeated-name case: PRESS `589.8720` on the QR path and on the `m$qr <- NULL` fallback, matching the fit on the positive-weight/non-omitted rows. | Fixed |
| G02 | High | `tests/testthat/test-loftest.R`: "no-intercept lack-of-fit degrees of freedom come from the model rank (G02)"; "constant or intercept-only models have no regression test (G02)" | `R/ols-pure-error-anova.R:93` (reject no estimated direction), `:139` (regression SS from total minus residual), `:141`–`:143` (df from rank, `df.residual`, unique predictor values) | `mpg ~ 0 + disp`: `df_ess = 31`, `df_lof = 26`, overall F `36.56991`, lack-of-fit F `95.99720 / p 3.695322e-05`, matching `summary()` and `anova(m, lm(mpg ~ 0 + factor(disp)))`. No-intercept offset case matches the equivalent offset-adjusted nested ANOVA. Constant `x = 1` and `mpg ~ 1` now error with "no tested direction beyond the intercept" instead of a false F `30 / p 6.055176e-06`. | Fixed |
| G03 | High | `tests/testthat/test-regress.R`: "a response component used as a predictor is still standardised (G03)" | `R/ols-regress_compute.R:214`–`:225` (removed the response-variable skip; the response expression still evaluates on unchanged original columns) | `I(mpg / wt) ~ wt * hp` gives `-0.8195021, -0.1546689, 0.4008095` for `wt`, `hp`, `wt:hp`, matching an independent scaled-predictor refit; `log(mpg) ~ mpg * hp` now gives `hp = 0.1079692` (was `-0.6296300`) and the full named vector matches the refit. | Fixed |
| G04 | Medium | `tests/testthat/test-regress.R`: "factor levels that look like placeholders keep their names (G04)" | `R/ols-regress_compute.R:225` (reserve data names, factor levels and original model-matrix columns), `:270` (skip any placeholder that occurs as a substring of a reserved label) | `mpg ~ hp * g` with level `.ols_sb_0001` returns names `hp`, `g.ols_sb_0001`, `hp:g.ols_sb_0001` and values `-1.2829443, -0.1350772, 0.6757784`, matching a refit on scaled `hp`. Printed Std. Beta cells are non-blank on both rows and match the returned values. | Fixed |
| G05 | Medium | `tests/testthat/test-utility.R`: "studentized residual vs leverage data handles zero weights and omissions (G05)" | `R/utils.R:339` (new `model_rstudent_used()`), `R/utils.R:369` (new `model_obs_ids()`), `R/ols-plots-data.R:351` (`ols_prep_rstudlev_data()` uses used-row leverage/studentized residuals and maps original IDs) | Zero weight at row 1 and `NA` at row 32 (endpoint), and zero weights 1/16 with `NA` 2/31: no warnings or errors; returned IDs are the remaining original rows, leverages/studentized residuals equal `hatvalues()`/`rstudent()` of the same model fitted after removing those rows. `na.exclude` without zero weights also returns IDs `1:31` with matching values. | Fixed |

The seven "Partly" statuses in the re-audit are covered by these fixes: H1/H2/F08
by G01, H4/F06 by G02, H5 by G03 and G04, and F04 by G04.

## Deferred (not in the ofxx2 findings; no wrong-number result observed)

- `ols_prep_dsrvf_data()`, `ols_prep_srchart_data()`, `ols_prep_srplot_data()` and
  `ols_prep_cdplot_data()` still call base `rstudent()`/`rstandard()`/
  `cooks.distance()`, so the same zero-weight + `na.exclude` combination that
  G05 fixed for `ols_prep_rstudlev_data()` can still error in those separate
  helpers. They were not part of G05, no incorrect finite statistic was
  reported for them, and fixing them requires used-row versions of the other
  influence measures. Not changed in this round.
- `ols_plot_dffits()`/`ols_plot_dfbetas()` likewise use base `dffits()`/
  `dfbetas()` and share that latent limitation.
- No unreported wrong numerical result is known for an accepted input after
  G01–G05; the audit's earlier "Deferred" items remain deferred as recorded in
  `docs-dev/ofx2-report.md`.

## Checks

- `devtools::test()`: **FAIL 0 | WARN 0 | SKIP 0 | PASS 2328**.
- Clean `git archive HEAD` copy in an external temporary directory:
  `R CMD build --no-manual` passed, including vignette construction and
  `olsrr_0.7.1.tar.gz`.
- `R CMD check --as-cran --no-manual` on that tarball:
  **0 errors, 0 warnings, 1 NOTE**.
  - NOTE: CRAN incoming feasibility, the same maintainer-email change recorded
    in the re-audit (`hebbali.aravind@gmail.com` →
    `aravindhebbali@rsquaredacademy.com`); not a code or documentation defect.
- Check-mode tests: **FAIL 0 | WARN 0 | SKIP 83 | PASS 2118**. Examples,
  `--run-donttest` examples and vignette rebuilding all passed.
- The built archive contains no `.git`, `docs-dev` or other audit artefacts
  (hidden-files, package-subdirectory, non-standard-contents and detritus checks
  OK).

## Commands that changed the worktree

- Added tests and ran them before each fix with
  `Rscript -e "devtools::test(filter = '<file>')"`; each new test failed on the
  audited HEAD behavior and passed after the fix.
- `Rscript -e "devtools::test()"` for the full suite.
- `git archive --format=tar HEAD | tar -xf - -C <temp>/src`; `R CMD build
  --no-manual .`; `R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz`.
- `rm -rf .ofxx-tmp` removed the untracked audit scratch directory. The
  pre-existing untracked audit reports `docs-dev/ofxx-reaudit.md` and
  `docs-dev/ofxx2-reaudit.md` were left untouched (not committed).

## Commits (all end with `(ofx3)`)

- `7360c76` fix: use only estimable QR directions for leverage and PRESS (G01)
- `5061553` fix: derive lack-of-fit df and regression SS from model rank (G02)
- `02dfb55` fix: standardise response components used as predictors (G03)
- `9c9b43a` fix: avoid placeholder collisions with factor-level names (G04)
- `72da581` fix: map leverage and studentized residuals for zero-weight and omitted rows (G05)
- This report commit (`docs: ofx3 report ... (ofx3)`).

## Not done

- No API was added or removed; no new dependency was added; no generated
  `NAMESPACE`/`man/` files needed regeneration because no roxygen documentation
  text changed.
- No win-builder/R-hub/macOS/revdep run was performed in this round.
