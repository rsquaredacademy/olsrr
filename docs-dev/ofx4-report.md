# ofx4 — olsrr fix round 4 (DeepSeek Pro)

Date: 2026-11-02. Worktree: `task/olsrr-fix-071` (head `ba3455e` before the
round, `df24544` after). Three commits, one per finding from
`ofxx3-reaudit.md`: J01 (High), J02 (Medium), J03 (Medium).

## Findings fixed

| Finding | Test added | Fix (file:line) | Verified how | Status |
|---|---|---|---|---|
| J01 — High: QR fallback re-estimates rank with the default tolerance | `tests/testthat/test-weighted-diagnostics.R:236` (`the QR fallback honours the fitted tolerance and rank`) | `R/utils.R:277` (`model_fit_tol`), `R/utils.R:346` (`qr(xw, tol = model_fit_tol(model))` + rank-mismatch rejection at `:349`) | `lm(..., tol = .1, qr = FALSE)` on `mpg ~ wt + (wt + .001*hp)` now returns leverage sum 2 (= `model$rank`) and PRESS `328.022775371`, equal to the stored-QR fit and to `sum((m$residuals/(1-hatvalues(m)))^2)`; weighted (`w = 1:32/8`) returns `773.676634658` on both paths. Test fails before the fix (fallback gave `368.3035…` / `937.7401…`) and passes after. | Fixed |
| J02 — Medium: aliases raise the leverage threshold | `tests/testthat/test-utility.R:183` (`the leverage threshold counts rank, not coefficient slots`) | `R/ols-plots-data.R:363` (`k <- model$rank`) | `ols_prep_rstudlev_data(lm(mpg ~ wt + I(2*wt) + hp))` now has `lev_thrsh == 2*rank/nobs == 0.1875`, equal to the estimable `wt + hp` fit; observations 16 and 29 stay `"leverage"`; classification tables are identical, including a zero-weight + omission case. Test fails before (threshold `0.25`, obs 16/29 reclassified `"normal"`) and passes after. | Fixed |
| J03 — Medium: six influence/plotting helpers fail on zero weights + omissions | `tests/testthat/test-plot-data.R:83` (`plot helpers align influence measures to the used rows`) | `R/utils.R:381–559` (new `model_residual_attrs`, `restore_residual_attrs`, `model_sigma_loocv_used`, `model_rstandard_used`, `model_cooksd_used`, `model_dffits_used`, `model_dfbetas_used`; `model_rstudent_used` wraps output with `restore_residual_attrs`; `model_fitted_used` at `:596` subsets to used rows), `R/ols-plots-data.R:95,113,237,421,456`, `R/ols-dffits-plot.R:94`, `R/ols-dfbetas-panel.R:42` | `ols_prep_dsrvf_data()`, `ols_prep_srchart_data()`, `ols_prep_srplot_data()`, `ols_prep_cdplot_data()`, `ols_plot_dffits()` and `ols_plot_dfbetas()` on `mpg ~ wt + hp` with `w[1] = 0`, `mpg[32] = NA`, `na.exclude` no longer error; obs IDs, residuals, studentized residuals, Cook's D, DFFITS and DFBETAS all equal the deleted-row fit (`d[-c(1,32),]`), asserted against base `rstudent`/`rstandard`/`cooks.distance`/`dffits`/`dfbetas`. Test fails before (`'names' attribute [32] must be the same length as the vector [31]`) and passes after. | Fixed |

### J01 detail

The fallback path in `model_hatvalues_frame()` called `qr(sqrt(w) * x)` with the
default tolerance and used that QR's rank, so a column that
`lm(..., tol = .1)` declared aliased was re-promoted to a third fitted
direction. The fix recovers the fit tolerance from `model$call$tol`
(defaulting to `1e-7`, matching `lm.fit()`/`qr()`), forms the QR with that
tolerance, and rejects with a clear error if the reconstructed rank differs
from `model$rank` (an unreconstructable fit is never guessed). The stored-QR
path is unchanged. `ols_pred_rsq()` is intentionally not tested on `qr = FALSE`
fits: its `anova()` path independently requires the QR, as the re-audit already
notes ("Predicted R² … its `anova()` path independently requires QR").

### J02 detail

`ols_prep_rstudlev_data()` computed `lev_thrsh <- 2 * length(coef(model)) / n`,
counting the aliased coefficient slot. It now uses `model$rank`, so the
threshold is `2 * rank / n` and the fitted projection's trace (the leverage
sum) is preserved. `n` remains `length(obs)` — the effective number of used
observations, matching `nobs(model)`.

### J03 detail

Base R's influence functions (`rstudent`, `rstandard`, `cooks.distance`,
`dffits`, `dfbetas`) all go through `lm.influence()`, whose `naresid` padding
fails when zero prior weights and `na.exclude` omissions occur together. The
six helpers now compute their statistic on the positive-weight fit rows via
used-row helpers and map `model_obs_ids()` back to the original observation
IDs, so the returned rows match the equivalent deleted-row fit. DFBETAS are
reconstructed from the fit QR (`invRQtt = t(backsolve(R, t(Q)))`, the
leave-one-out sigma and `chol2inv`) exactly as base R's `dfbeta`/`dfbetas`
compute them, and the columns carry the estimable coefficient names. The
response metadata attributes (`label`, `format.stata`) that base R's influence
functions carry through their arithmetic are restored (`restore_residual_attrs`)
so the golden `auto`-dataset assertions still compare attribute-for-attribute.

## Tests and check

- Local suite: `devtools::test()` → **FAIL 0 | WARN 0 | SKIP 0 | PASS 2387**
  (was 2328 at the round-3 base; the three new tests add 59 assertions).
- Clean-archive build/check, from `git archive HEAD` extracted outside the repo
  (no untracked/`docs-dev` content copied in):

  ```text
  git archive --format=tar --output=…/src.tar HEAD
  tar -xf …/src.tar -C …/src
  R CMD build --no-manual .
  R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
  ```

  **Status: OK — 0 errors, 0 warnings, 0 notes.** Examples passed in ~35 s,
  `--run-donttest` examples ~96 s, check-mode tests ~111 s, vignette rebuild
  ~27 s. The built tarball has **464 entries** and contains no `docs-dev`,
  `README.Rmd`, `cran-comments.md` or `.git`.

  The CRAN-incoming remote lookup was disabled with
  `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE` because `archive.rds` timed out on this
  machine (the first un-flagged run hit "Timeout of 60 seconds" downloading from
  CRAN and halted); the local incoming-feasibility line then reports `OK`. The
  round-3 report's lone NOTE (maintainer-email change) is that same incoming
  check and is not a warning/error.

## Commands that changed something

- `R/utils.R` — `model_fit_tol()`; `model_hatvalues_frame()` fallback honours
  tolerance + rank and rejects a rank mismatch; new used-row influence helpers
  and `restore_residual_attrs()`; `model_rstudent_used()` restores residual
  attributes; `model_fitted_used()` subsets to used rows.
- `R/ols-plots-data.R` — `ols_prep_rstudlev_data()` threshold uses rank;
  `ols_prep_cdplot_data()`, `ols_cooks_ts()`, `ols_prep_dsrvf_data()`,
  `ols_prep_srplot_data()`, `ols_prep_srchart_data()` use used-row helpers and
  mapped IDs.
- `R/ols-dffits-plot.R` — `ols_prep_dffits_plot_data()` uses used-row DFFITS and
  mapped IDs.
- `R/ols-dfbetas-panel.R` — `ols_plot_dfbetas()` uses used-row DFBETAS and
  mapped IDs.
- Tests: `test-weighted-diagnostics.R`, `test-utility.R`, `test-plot-data.R`.
- `NEWS.md` — one bullet per fix at the top of `## Bug Fixes`.

## Deferred

- `ols_pred_rsq()` on a `qr = FALSE` fit errors because `anova()` requires the
  QR; the re-audit documents this as an accepted limitation, not a J01–J03
  finding.
- The remote CRAN-incoming feasibility check was not run (network timeout); it
  is an environmental check and the package check itself is clean.

## Anything not done

- No API changes, no new dependencies, no exports added/removed (NAMESPACE still
  110 exports).
- The untracked `docs-dev/ofxx*-reaudit.md` and `.ofxx3-tmp/` were left alone.
