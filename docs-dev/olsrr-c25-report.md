# pkg-olsrr-c25 — olsrr #254 + #253 (patch round, build only)

2026-11-18. Worktree `F:/R/Packages/_worktrees/olsrr-c25`, branch `task/olsrr-c25`
from `origin/develop` (`4283adb`). R 4.6.1, Windows 11, nortest 1.0.4,
ggplot2 4.x, testthat 3e. Single local commit, message:
`Lilliefors KS p-value and distinct diagnostic panel titles (#254, #253)`.
Not pushed; no version bump (0.7.1 stays as in `DESCRIPTION`).

## Findings fixed

| issue | change | files | verification | status |
|---|---|---|---|---|
| **#254** | `ols_test_normality()` runs the Lilliefors-corrected KS test (`nortest::lillie.test()`) instead of `ks.test(y, "pnorm", mean(y), sd(y))`. The mean and sd are estimated from the same residuals, so the uncorrected p-value was too large. The statistic is unchanged (same KS distance); only the p-value moves. The test is now skipped (with the existing warning mechanism) below 5 observations, the minimum `nortest::lillie.test()` supports, and is labelled `Kolmogorov-Smirnov (Lilliefors)` in the printed table and in the skip warning. `nortest` was already in `Imports:` (used for AD/CvM); `lillie.test` is now imported too and `ks.test` is no longer imported. | `R/ols-normality-tests.R`, `R/output.R` (table label and column width), `NAMESPACE` + `man/` (regenerated), `vignettes/residual_diagnostics.Rmd`, `NEWS.md` | New tests in `tests/testthat/test-normality.R`: p-value equals `nortest::lillie.test(residuals(m))$p.value` and the statistic matches for `lm(mpg ~ disp + hp + wt, mtcars)`; p-value is below the uncorrected `ks.test()` p-value; table label; n = 4 skip. `test-golden.R` and the `norm-output` snapshot updated. All fail before the change (26 failures in the targeted run). | FIXED |
| **#253** | `ols_plot_diagnostics()` panels 7 and 8 no longer share the title "Residual Fit Spread Plot": `ols_plot_rfs()` titles each panel after its y variable, `Fit - Mean` and `Residual` — the exact titles the two panels of `ols_plot_resid_fit_spread()` had before the shared-title refactor and the values on their y axes now. This also gives the two `ols_plot_resid_fit_spread()` panels distinct titles; the combined figure keeps its "Residual Fit Spread Plot" top banner. | `R/ols-residual-fit-spread-plot.R`, `man/ols_plot_resid_fit_spread.Rd` (regenerated), `NEWS.md`, `tests/testthat/_snaps/visual/diag-7.svg`, `diag-8.svg` | New `tests/testthat/test-diagnostics-panel-titles.R`: titles of panels 7 and 8 differ, equal `Fit - Mean` and `Residual`, and are not "Residual Fit Spread Plot"; the same assertions on `ols_plot_resid_fit_spread()`/`ols_plot_fm()`/`ols_plot_resid_spread()`. Fails before the change. | FIXED |

Decisions taken (not asked back):

- Panel titles use the y-variable names `Fit - Mean` / `Residual` (the exact
  strings of the two pre-refactor panels of `ols_plot_resid_fit_spread()`);
  the y-axis labels are unchanged, so the title repeats the axis label.
- `NEWS.md` had no development heading, so `# olsrr (development version)` was
  created at the top with the two bullets; the existing `# olsrr 0.7.1`
  section is untouched. `DESCRIPTION` is not bumped.
- The normality table's test column is widened from 18 to 31 characters so the
  longer label does not shift the statistic and p-value columns.
- Visual snapshots were changed only for `diag-7`/`diag-8` (the panels whose
  titles changed). The `fm-plot`/`rsd-plot`/`residual-fit-spread-plot`
  snapshots are pre-existing empty files and are not exercised (see
  follow-ups).

## Reprex

```r
library(olsrr)
m <- lm(mpg ~ disp + hp + wt, mtcars)
ols_test_normality(m)
p <- ols_plot_diagnostics(m, print_plot = FALSE)
p[[7]]$labels$title
p[[8]]$labels$title
```

Before (HEAD `4283adb`):

```text
-----------------------------------------------
       Test             Statistic       pvalue
-----------------------------------------------
Shapiro-Wilk              0.9273         0.0330
Kolmogorov-Smirnov        0.1083         0.8089
Cramer-von Mises          0.0831         0.1820
Anderson-Darling          0.666          0.0745
-----------------------------------------------

nortest::lillie.test(r)$statistic : 0.108265447892069
nortest::lillie.test(r)$p.value   : 0.443473239076533
ks.test(r, 'pnorm', mean(r), sd(r))$p.value: 0.808868677941475
ols_test_normality(m)$kolmogorv$p.value   : 0.808868677941475

diagnostic panel 7: Residual Fit Spread Plot
diagnostic panel 8: Residual Fit Spread Plot
titles differ      : FALSE
ols_plot_resid_fit_spread(): fm "Residual Fit Spread Plot", rsd "Residual Fit Spread Plot"
```

After:

```text
------------------------------------------------------------
             Test                    Statistic       pvalue
------------------------------------------------------------
Shapiro-Wilk                           0.9273         0.0330
Kolmogorov-Smirnov (Lilliefors)        0.1083         0.4435
Cramer-von Mises                       0.0831         0.1820
Anderson-Darling                       0.666          0.0745
------------------------------------------------------------

nortest::lillie.test(r)$statistic : 0.108265447892069
nortest::lillie.test(r)$p.value   : 0.443473239076533
ks.test(r, 'pnorm', mean(r), sd(r))$p.value: 0.808868677941475
ols_test_normality(m)$kolmogorv$p.value   : 0.443473239076533

diagnostic panel 7: Fit - Mean
diagnostic panel 8: Residual
titles differ      : TRUE
ols_plot_resid_fit_spread(): fm "Fit - Mean", rsd "Residual"
```

## Tests

- Before the fix, the new/edited tests fail: targeted
  `devtools::test(filter = "normality|norm-output|golden|diagnostics-panel-titles")`
  reports 26 failures (10 displayed), including
  `Kolmogorov-Smirnov p-value uses the Lilliefors correction (#254)` (p-value
  0.8089 vs 0.4435, and the "less than the naive p" comparison fails),
  `Kolmogorov-Smirnov test is skipped below 5 observations`, both panel-title
  tests and the golden reference values.
- After the fix: `devtools::test()` — 0 failures, 0 errors, 0 skips.
- Updated expected values: `tests/testthat/_snaps/norm-output.md`
  (p 0.7464 → 0.3446 for `mpg ~ disp + hp + wt + qsec`, new label),
  `test-golden.R` (mtcars p 0.746376839491 → 0.344566747029; fitness p added
  0.147787675781), `diag-7.svg` / `diag-8.svg` title text only.

## Checks

`devtools::document()` regenerated `NAMESPACE` and the two `man/*.Rd` files.
`R CMD build` produced `olsrr_0.7.1.tar.gz`; `R CMD check --as-cran
--no-manual` on the tarball with `_R_CHECK_FORCE_SUGGESTS_=false`:

- 0 errors, 0 NOTEs.
- 1 WARNING: `checking CRAN incoming feasibility ... Insufficient package
  version (submitted: 0.7.1, existing: 0.7.1)`. 0.7.1 is already the CRAN
  version; the version was intentionally not bumped (patch release is after
  2027-01-13), so this warning is the un-bumped version, not the patch.
- Supplementary run of the same tarball with `_R_CHECK_CRAN_INCOMING_=false`:
  `Status: OK` (0 errors, 0 warnings, 0 notes). Examples 49 s, donttest
  169 s, tests 234 s, vignettes rebuilt OK.

## Commands that changed something

```text
git checkout -- R NAMESPACE man          # temporary revert to capture before/after
git apply fix.patch                      # restore the fix
Rscript <tmp>/olsrr-c25-doc.R            # devtools::document()
Rscript <tmp>/olsrr-c25-test.R           # devtools::test()
Rscript <tmp>/olsrr-c25-check.R          # R CMD build + R CMD check --as-cran --no-manual
Rscript <tmp>/olsrr-c25-check3.R         # same check with _R_CHECK_CRAN_INCOMING_=false
git add -- R/ols-normality-tests.R R/ols-residual-fit-spread-plot.R R/output.R \
  NAMESPACE man NEWS.md vignettes/residual_diagnostics.Rmd \
  tests/testthat/test-normality.R tests/testthat/test-golden.R \
  tests/testthat/test-diagnostics-panel-titles.R \
  tests/testthat/_snaps/norm-output.md \
  tests/testthat/_snaps/visual/diag-7.svg tests/testthat/_snaps/visual/diag-8.svg \
  docs-dev/olsrr-c25-report.md
git commit -m "Lilliefors KS p-value and distinct diagnostic panel titles (#254, #253)"
```

Helper scripts and the build/check tree live under
`C:/Users/DELL/AppData/Local/Temp/opencode/` (not in the repository).

## Follow-ups

1. `tests/testthat/test-visual.R:124` compares the wrong objects: it calls
   `ols_plot_resid_fit_spread(model, print_plot = TRUE)` (which returns a
   gtable) and then `p$fm_plot` / `p$rsd_plot`, both `NULL`, so the
   `fm-plot.svg`, `rsd-plot.svg` and `residual-fit-spread-plot.svg` snapshots
   are empty (0 bytes) and the two panels are not covered visually. Change the
   call to `print_plot = FALSE` and regenerate those three snapshots;
   `test-diagnostics-panel-titles.R` covers their titles until then.
2. `tests/figs/visual-test/residual-fit-spread-plot*.svg` still contain the old
   shared title; they are not used by vdiffr and should be deleted when the
   visual-test fixtures are next cleaned up.
3. The committed pkgdown site still shows the old normality table and label
   (`docs/reference/ols_test_normality.html`, `docs/search.json`) and the old
   plot titles; rebuild the site (`pkgdown::build_site()`) at release. Generated
   `docs/` was not edited by hand.
4. `DESCRIPTION` says `Version: 0.7.1`, which CRAN already has, so every
   `--as-cran` check warns about an insufficient version. When the patch release
   window opens (after 2027-01-13), bump to the next version (or follow the
   repository convention of a four-component dev version such as
   `0.7.1.9000`) so the incoming check is clean.
5. The normality table is now 60 characters wide (was 47) because of the longer
   label. If the width matters for downstream console alignment, a shorter
   label such as `Lilliefors (K-S)` could be used; snapshots and the
   `test-normality.R` label assertions would need the same change.
