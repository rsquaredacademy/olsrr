# ofx15 — olsrr 0.7.1 known-bug fix round

2026-11-16. Worktree `F:/R/Packages/statistics/olsrr-wt/fx15`, branch `task/olsrr-fx15`, based on
`46e3255` (the 0.7.1 candidate). Spec: `docs-dev/pkg-olsrr-inv-audit.md` section 1, B1–B10.
R 4.6.1 on Windows 11, ggplot2 4.0.3, vdiffr 1.x. Heavy commands through
`heavy-slot.sh pkg-olsrr-fx15`. Scratch files only in `.ofx15-tmp/`.

**Result: all ten findings fixed, each with a regression test that failed on `46e3255`.
`devtools::test()` green. Clean `git archive` build + `R CMD check --as-cran --no-manual`:
0 errors, 0 warnings, 1 NOTE (maintainer change only).**

Commit: `ea5ddc2` — `fix: B1-B10 known bugs from the 0.7.1 inventory (ofx15)`.

## B1 — Holm adjusted p values (H)

- **Fix:** `R/ols-breusch-pagan-test.R:430`, `bp_case_adj()` Holm branch now
  `p <- c(stats::p.adjust(pvals, "holm"), ps)`. Simultaneous p (`ps`) is appended unchanged.
  Bonferroni and Sidak branches were already exact and are now pinned against
  `p.adjust(..., "bonferroni")` and `1 - (1 - p)^n`.
- **Test:** `tests/testthat/test-breusch-pagan.R`,
  “Holm adjusted p values equal p.adjust and the printed decision follows (B1)” and
  “Bonferroni and Sidak branches still agree with p.adjust and the Sidak formula (B1)”.
- **Before (46e3255):** aud2 fixture (`set.seed(15)`, n = 60, `y ~ x1+x2+x3`,
  `rhs = TRUE, multiple = TRUE`): Holm p = 0.04651371, 3.211077e-05, 0.08192284; x1 printed
  at 4.651371e-02 and was wrongly rejected at .05.
- **After:** 0.08192284, 3.211077e-05, 0.08192284, exactly `p.adjust(raw, "holm")`;
  x1 prints 8.192284e-02 and is not rejected. Simultaneous statistic unchanged
  (21.479644, df 3, p 8.369002e-05). mtcars `vars = c("wt","hp","disp")` cells are now
  0.891, 0.891, 0.891 (were 0.516, 0.891, 0.870).
- Two older assertions in the same file encoded the wrong Holm values
  (0.3817/0.7574) and were corrected to the `p.adjust` values (1, 1, 1, 1, 1, 0.7781 and
  0.6730, 0.6730, 0.6192). Snapshot `bp-output.md` accepted.

## B2 — residual + component plot rows and no-intercept columns (H)

- **Fix:** `R/ols-residual-plus-component-plot.R:71-79`, `cpout()` builds
  `X <- model.matrix(model)`, `keep <- setdiff(colnames(X), "(Intercept)")`, uses
  `X[, keep, drop = FALSE]` and `stats::coef(model)[keep]`; `cpdata()` uses
  `y <- mc[i] * data[[i]] + e`. No position-based `[-1]`.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “the residual plus component plot keeps
  every model-matrix column (B2)”.
- **Before:** `lm(mpg ~ wt)`: 1 plot, 1 row (expected 32); `lm(mpg ~ 0 + wt + hp)`:
  1 plot, 1 row, only one predictor.
- **After:** `lm(mpg ~ wt)`: 1 plot, 32 rows; `lm(mpg ~ 0 + wt + hp)`: 2 plots × 32 rows;
  max abs difference from `residuals + coef × x` is 0.

## B3 — residual histogram curve/bar alignment (H)

- **Fix:** `R/ols-residual-histogram.R:24,31`, `hist(..., breaks = 6)` and
  `geom_histogram(breaks = h$breaks)`, so the normal curve uses the drawn bin width.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “the residual histogram bars and normal
  curve share one bin width (B3)”, asserting
  `curve / (dnorm × n × drawn bar width) == 1` from `ggplot_build()`.
- **Before:** ratio 0.5417551 (`hwy ~ displ + cty + year`, bars 1.845853 wide) and
  1.025427 (`mpg ~ disp + hp + wt`, bars 1.950408 wide).
- **After:** ratio 1.000000 in both; drawn bar width 2 in both (same as the `hist()` width).

## B4 — residual vs regressor with NA rows (M)

- **Fix:** `R/ols-residual-vs-regressor-plot.R:29` uses `ols_model_data(model)`, the
  data restricted to the rows the fit used; `R/ols-resid-vs-regressor-plot-shiny.R:26-28`
  drops `model$na.action` rows from the supplied data only when its row count is not
  already the used count.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “residual vs regressor plots align rows
  dropped by the fit (B4)”.
- **Before:** `d$hp[3] <- NA; lm(mpg ~ wt + hp, d)`:
  `rvsr_plot_shiny()` and `ols_plot_resid_regressor()` (both `drat` and the model variable
  `wt`) errored with “arguments imply differing number of rows: 32, 31”.
- **After:** 31 rows in all three; x values are the 31 used rows and y equals the 31
  residuals.

## B5 — diagnostics panel `print_plot = FALSE` (M)

- **Fix:** `R/ols-diagnostics-panel.R:21-40`, every sub-plot is built with
  `print_plot = FALSE`; list-returning sub-plots contribute `$plot`. The returned list now
  holds ten actual plot objects instead of ten `NULL`s.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “ols_plot_diagnostics does not draw when
  print_plot is FALSE (B5)”, using `pdf(NULL)` + `dev.control("enable")` + `recordPlot()`.
- **Before:** display-list length after the call 3 (plots drawn), returned list of `NULL`s.
- **After:** display-list length 0; ten elements, the checked elements are `ggplot`
  objects.

## B6 — `ols_prep_rvsrplot_data()` names (L)

- **Fix:** `R/ols-plots-data.R:319-328`, uses `model.matrix(model)`, removes
  `(Intercept)`, `(weights)` and `(offset)`, keeps `drop = FALSE`, and sets
  `np <- length(pnames)`.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “ols_prep_rvsrplot_data names every
  non-intercept model matrix column (B6)”.
- **Before:** `lm(mpg ~ 0 + wt + hp)`: `np = 1`, `pnames = "hp"`, data had 2 columns;
  weighted `lm(mpg ~ wt + hp, weights = cyl)`: `np = 2` but 3 columns including
  `(weights)`; one predictor: a bare numeric vector.
- **After:** no-intercept `np = 2`, names `wt, hp`, 2 columns; weighted `np = 2`, names
  `wt, hp`, 2 columns; single predictor `np = 1`, one-column `data.frame`.

## B7 — Bonferroni p capped at 1 (L)

- **Fix:** `R/ols-outlier-test.R:42`, `p_bon <- pmin(1, n * p_val)`.
- **Test:** `tests/testthat/test-influence.R` is not used; the test is
  `tests/testthat/test-bugfix-ofx15.R`, “Bonferroni p values from ols_test_outlier are
  probabilities (B7)”. (Kept in the ofx15 file with the other B-item tests.)
- **Before:** `lm(mpg ~ wt + hp + disp, mtcars)` internal `p_bon` up to 31.76883; with
  `cut_off = 40` the printed column could exceed 1.
- **After:** maximum 1; every reported value equals `pmin(1, n × raw p)`.

## B8 — grob-returning plots draw on `print_plot = TRUE` (L)

- **Fix:** `R/ols-added-variable-plot.R:97-100`,
  `R/ols-residual-plus-component-plot.R:52-55`, `R/ols-dfbetas-panel.R:83-86`,
  `R/ols-diagnostics-panel.R:38-41`: `g <- marrangeGrob(...); grid::grid.draw(g);
  invisible(g)`. `grid` (base R) added to `Imports` in `DESCRIPTION`; no external
  dependency added.
- **Test:** `tests/testthat/test-bugfix-ofx15.R`, “the grob-returning plots draw when
  print_plot is TRUE (B8)”, one `recordPlot()` delta per function.
- **Before:** draw delta 0 for all four (the grob was returned but nothing was drawn).
- **After:** draw delta 2 for all four (grob drawn), object returned invisibly.

## B9 — #232 message wording (L)

- **Fix:** `R/ols-correlations.R:78` message and the roxygen details at line 22 now say
  “more than one predictor coefficient”.
- **Test:** `tests/testthat/test-correlations.R`, updated regex on the existing
  multi-predictor test plus “the no-intercept message counts predictor coefficients (B9)”
  for `lm(mpg ~ 0 + factor(cyl))`.
- **Before:** “ols_correlations() is not supported for models without an intercept and
  more than one predictor.”
- **After:** “... more than one predictor coefficient.” Guard unchanged.
  `man/ols_correlations.Rd` regenerated with `devtools::document()`.

## B10 — forward selection from a saturated model (L)

- **Fix:** `R/ols-stepwise-forward-regression.R:122,138,196-218,321-357` and
  `R/ols-step.R:38,51,103-125,206-243`.
  A zero-RSS full model with positive residual df keeps the old clear “no steps” result
  (so near-perfect deterministic models behave as before). A saturated full model
  (`df.residual(model) == 0`, the `n <= p` case) now runs forward selection from the null
  model; candidates with `df.residual <= 0`, zero RSS or an `Anova()` error are treated as
  unavailable and selection stops, returning the steps recorded so far with the existing
  near-perfect-fit message. Applied to `ols_step_forward_p()` and the shared
  `ols_step_forward()` used by `_aic`, `_sbc`, `_sbic`, `_rsq`, `_adj_r2`.
- **Test:** `tests/testthat/test-step-forward-saturated.R`, “forward selection keeps the
  steps recorded before a saturated model stops it (B10)”.
- **Before:** n = 10, 29 noise predictors, `p_val = 0.5`: message printed but
  `nrow(metrics) = 0` (variables `character(0)`); `ols_step_forward_aic()` likewise 0.
- **After:** `ols_step_forward_p()` and `ols_step_forward_aic()` both record 8 steps:
  `x13, x23, x2, x16, x18, x22, x27, x29`; final model has the same 8 terms, near-perfect
  fit (deviance 2.5e-08), step numbers 1..8 and metrics rows match variables.

## Snapshots

Regenerated on R 4.6.1 (both files generated by the test tools, not hand-edited):

- `tests/testthat/_snaps/bp-output.md` — B1 Holm cells (two tests, six lines).
- `tests/testthat/_snaps/visual/diag-9.svg` — B3 histogram inside the diagnostics panel
  (`diag_9`). The other nine `diag-*.svg` files compare unchanged.
- No other visual snapshot changed. `B2`, `B4`, `B6` and `B8` do not alter the plotted
  content of the snapshotted examples; `B5` makes the returned diagnostics objects real,
  and their existing SVG snapshots already matched the new objects except `diag-9`.

R-pre-4.6 variant: **not regenerated here** (only R 4.6.1 is available on this box).
Claude should regenerate the R-pre-4.6 variants of `bp-output.md` and
`tests/testthat/_snaps/visual/diag-9.svg` on the Linux box.

## Checks

- `NOT_CRAN=true devtools::test()` — green, 0 failures, 0 errors (log
  `.ofx15-tmp/test-full2.log`). Every new test was run on `46e3255` before the fix and
  failed (B1 fixture values; B2 length/rows; B3 ratio; B4 row-count error; B5 display
  list; B6 names/columns; B7 p > 1; B8 draw delta; B9 message regex; B10 zero steps).
- Clean build: `git archive HEAD | tar -x` into `.ofx15-tmp/build`, `R CMD build .`,
  then `R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz`
  (`.ofx15-tmp/check.log`).
- Result: **Status: 1 NOTE** — the maintainer note only:
  `Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'`,
  `New maintainer: ... Old maintainer(s): ... <hebbali.aravind@gmail.com>`.
  Tests in check: `[179s] OK`; examples `[41s] OK`; donttest `[140s] OK`.
- `devtools::document()` regenerated `man/ols_correlations.Rd` only.

## Commands that changed files

- `devtools::document()` → `man/ols_correlations.Rd`.
- `testthat::snapshot_accept("bp-output")` → `tests/testthat/_snaps/bp-output.md`.
- `testthat::snapshot_accept("visual")` wrote `diag-9.new.svg`; renamed to `diag-9.svg`
  (the standard vdiffr acceptance step; the file was generated by vdiffr, not edited).
- `R CMD build` / `R CMD check` wrote only under `.ofx15-tmp/`.

## Follow-ups (not fixed in this round)

1. **Inactive visual snapshot.** `tests/testthat/test-visual.R:6` calls
   `ols_plot_resid_hist(..., print_plot = FALSE)$plot`, but that function returns the
   `ggplot` object directly, so `$plot` is `NULL`; vdiffr skips `NULL` and
   `tests/testthat/_snaps/visual/ggplot2-histogram.svg` is a 0-byte file. The same pattern
   appears for other single-plot functions in that file. Fixing the harness would activate
   several snapshots at once and should be a separate test-only change.
2. **R-pre-4.6 visual snapshots.** Regenerate `bp-output.md` and `visual/diag-9.svg` on the
   Linux box with the older R as requested; this worktree only has R 4.6.1.
3. **B10 scope.** `ols_step_both_p()`, `ols_step_backward_p()` and the hierarchical forward
   selector still return the previous “no steps” result for a near-perfect full model.
   Extending the from-null saturated-selection rule to those selectors is a behaviour
   decision for Aravind; only `ols_step_forward_p()` and the shared `ols_step_forward()`
   were in the B10 finding.
4. **Selection warnings at saturation.** Saturated / near-perfect candidate fits still
   emit base R and package warnings (`ANOVA F-tests ... unreliable`,
   `Residual sum of squares < 1e-10; SBIC returned NA.`, `NaNs produced`) before the
   candidate is skipped. The new B10 test suppresses them; a user still sees them.
   Silencing them would change warning behaviour inside selection and was left alone.
5. **`sbic` / `mallows_cp` columns** in a saturated selection result are `NaN` (the
   existing information-criterion guards), while other steps are recorded. Reporting `NA`
   consistently, or omitting the columns, is cosmetic and deferred.
6. **`ols_plot_resid_regressor()` variable lookup.** A `variable` that is not a column of
   the data found through the model call now fails with the standard “undefined columns
   selected” subscript error; a named error (“`variable` is not a column of the model
   data”) would be friendlier.
7. **Factor names in `ols_prep_rvsrplot_data()`.** Names are model-matrix column names
   (`factor(cyl)6`, ...), as requested by the inventory fix (“non-intercept columns of
   `model.matrix`”), not term labels. This is currently unused downstream; if a caller
   wants term labels, a separate accessor is needed.
8. **Inventory sections 3 and 4** (not bugs/enhancements and cannot-reproduce items) were
   not touched; all are listed in `docs-dev/pkg-olsrr-inv-audit.md`.
