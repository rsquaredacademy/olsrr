# olsrr 0.7.1 fix round — report

Job id: `olsrr-fix-071`
Worktree: `task/olsrr-fix-071` (branched from `release/v0.7.1`, `26e91e1`)
Date: 2026-11-01
Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt)

Inputs: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`,
`F:/pm/notes/2026-11-01-pkg-audits/cx-olsrr-audit.md` (Codex audit),
`F:/pm/notes/2026-10-26-lms-reviews/ols-sweep-071.md` (Gemini sweep, items 1–6).

## Wrong results (Codex findings)

Every finding was reproduced with the audit's own probe before changing any
source (see "Commands run"). The new tests were then run against an untouched
`git archive HEAD` copy of the package: **78 failures and 7 errors** there, and
all of them pass on the fixed tree.

| Finding | Test added | Fix (file:line) | Verified how | Status |
|---|---|---|---|---|
| H1 weighted predicted R² depends on weight scale | `test-weighted-diagnostics.R`: "weighted PRESS and predicted R-squared use the model weights (H1)" | `R/ols-influence-measures.R:126` (`ols_press()` now sums `w_i (e_i/(1-h_i))^2`); docs at `:107`; predicted R² already divides by the weighted ANOVA TSS | equals the weighted reference `1 - sum(w*(r/(1-h))^2)/sum(anova[,2])` = 0.7859224; invariant to `w -> 10w` (`press` scales by 10, R² unchanged); `ols_regress()$prsq` agrees | Fixed |
| H2 zero weights recycle and wrong criterion sample size | `test-weighted-diagnostics.R`: "zero prior weights are dropped consistently (H2)" | `R/utils.R:157` `model_used_rows()`, `:173` `model_residuals_used()`, `:179` `model_hatvalues_used()` (aligned by row name), `:219` `model_weights_used()`, `:232` `model_fitted_used()`; `R/ols-information-criteria.R:632` `model_rows()` now `nobs()` | zero weight at first/middle/last position: PRESS, predicted R², AIC, AICc, SBC and MAE equal the fit on the remaining rows; SBC equals base `BIC()`; zero weight + `na.exclude` equals `na.omit` and the reduced fit | Fixed |
| H3 aliased coefficients shift inference | `test-regress.R`: "aliased coefficients are aligned by name (H3)" | `R/ols-regress_compute.R:107` coefficient table aligned by name, NA for aliases; standardised betas restricted to estimable columns (`:41`–`:104`) | `wt + I(2*wt) + hp`: SE/t/p/CI of `wt` and `hp` equal `summary()`/`confint()`; alias row all NA in the printout; zero-variance and aliased-interaction cases assert too | Fixed |
| H4 unit-weight lack-of-fit ANOVA wrong | `test-loftest.R`: "unit weights give the same lack-of-fit ANOVA as no weights (H4)" | `R/ols-pure-error-anova.R:161` `pea_frame()` drops synthetic `(weights)`/offset columns; `:198` uses unpadded fitted values | full equality of unit-weight and unweighted `rf`, lack of fit, pure error, `ess`, `lf`, `pl` (F = 76.51, not 1.90); `ess == deviance(model)`; `na.exclude` equals `na.omit` | Fixed |
| H5 interaction standardization corrupts transformed/categorical terms | `test-regress.R`: "standardised betas respect transformed terms in interactions (H5)" | `R/ols-regress_compute.R:181` `ols_scale_terms()` standardises each term component after its transformation and rewrites the formula; `:268` `ols_term_names()` restores coefficient names | `log(wt)*hp` matches an independent refit on scaled `log(wt)`/`hp` (-0.6318, -0.3617, 0.1296) with all 32 rows; `factor(cyl)*hp` keeps level names and printed cells; `poly()` inside an interaction now errors clearly | Fixed |
| H6 offsets lost/rescaled in interaction refit | `test-regress.R`: "standardised betas carry model offsets through the refit (H6)" | `R/ols-regress_compute.R:181` (offset variables excluded from scaling; offset terms re-added to the refit formula); `:66` call offsets passed as `offset =` | calling-style and inline offsets give identical betas matching the offset-preserving refit, with weights and with `na.exclude` rows | Fixed |
| H7 selection adds an intercept and drops offsets | `test-step-forward.R`: "selection preserves the intercept and model offsets (H7)"; `test-step-backward.R`: backward/both case; `test-all-possible.R`: "all-possible routines respect a no-intercept model (H7)" | `R/ols-stepwise-utils.R:718` `ols_step_formula()` keeps intercept, offsets and environment; `:754` `ols_step_lm()` carries call offsets; `:769` `ols_step_fit()`; `R/ols-step.R`, `ols-stepaic-*.R`, `ols-stepwise-*-regression.R`, `ols-stepwise-hierarchical.R`, `ols-all-possible-regression.R:388`, `ols-best-subsets-regression.R:165` use them | include-all forward AIC returns the input coefficients for a no-intercept model and for inline/call offsets (coefs identical, `length(model$offset) == 32`); backward and both-direction keep intercept 0 and the offset; predictions usable | Fixed |
| H8 chi-squared tests overcount aliased df | `test-breusch-pagan.R` and `test-score.R`: "aliased predictors do not inflate the auxiliary degrees of freedom (H8)" | `R/ols-breusch-pagan-test.R:314` `bp_model()` returns `rank - 1`, used for `df`, p values and the printed simultaneous row (`R/output.R:248`, `:367`, `:428`); `R/ols-score-test.R:141`/`:171` auxiliary rank; `R/ols-error-messages.R:113` de-duplicates matched columns | `hp` and `2*hp`: BP df 1, p 0.3115 (= `pchisq(bp, 1)`); score df 1, p 0.3486; duplicate `vars` and overlapping factor/model-matrix selections give one df | Fixed |
| H9 SAS criteria use unweighted, NA-padded RSS | `test-weighted-diagnostics.R`: "SAS information criteria use deviance and the effective sample size (H9)" | `R/ols-information-criteria.R:658` `model_rss()` now `deviance(model)` | SAS AIC 89.564 for the weighted fit (was 64.197), SAS SBC matches the formula; `na.exclude` and `na.omit` agree for all three methods | Fixed |

### Medium findings that returned a wrong number/label/row

| Finding | Test added | Fix (file:line) | Verified how | Status |
|---|---|---|---|---|
| M1 local formula environments lost | `test-step-forward.R`: "selection keeps the formula environment of the input model (M1)" | `R/ols-stepwise-utils.R:718` sets the formula environment from the input model for every candidate and final fit | a locally defined `kk()` and an external predictor vector both work and match the direct `lm()` fit | Fixed |
| M4 VIF fails for one predictor / removes a predictor without intercept | `test-collindiag.R`: "VIF and tolerance work for one predictor and without an intercept (M4)" | `R/ols-collinearity-diagnostics.R:194` `viftol()` drops the intercept only when it exists, uses `drop = FALSE`, returns tolerance/VIF 1 for a lone predictor | one-predictor VIF = 1; no-intercept VIF equals `1/(1 - R²)` of each predictor on the others; multi-predictor values unchanged; convention documented | Fixed |
| M5 `na.exclude` breaks heteroskedasticity diagnostics | `test-f.R`, `test-score.R`, `test-breusch-pagan.R`: "na.exclude and na.omit fits give the same … (M5)" and `test-weighted-diagnostics.R`: "ols_test_correlation() works for na.exclude fits (M5)" | `R/ols-score-test.R:130`, `:134`, `:154`; `R/ols-breusch-pagan-test.R:328`, `:253`; `R/ols-f-test.R:107`, `:118`, `:132` use the used-rows residuals/fitted values; `R/ols-normality-tests.R:29` for `ols_test_correlation()` | `ols_test_score`/`ols_test_f`/`ols_test_breusch_pagan` (fitted, `rhs`, `vars`, `multiple`) give identical values for `na.exclude` and `na.omit`; no more "0 (non-NA) cases" / length errors; `ols_test_correlation()` agrees too | Fixed |
| L1 Bartlett returns a result for invalid group sizes | `test-bartlett.R`: "invalid group sizes give the same errors as bartlett.test() (L1)" | `R/ols-bartlett-test.R:88` validates ≥ 2 non-empty groups and ≥ 2 complete observations per group after NA filtering | same errors as `stats::bartlett.test()` for one group and for a singleton group; valid two-group case still matches base R exactly | Fixed |

## CRAN must-fix items (Gemini sweep, items 1–6)

| Item | Change | Status |
|---|---|---|
| 1. Maintainer email | `DESCRIPTION:5` now `aravindhebbali@rsquaredacademy.com` | Done |
| 2. `cran-comments.md` environments | win-builder (R-devel, R-release), 2026-10-27 recorded as `0 errors | 0 warnings | 0 notes`; R-hub and macOS marked "to run" (no results invented); headline summary mentions weighted support and the weighted-statistics fixes; the one local note (new maintainer email) is explained | Done |
| 3. README re-knit | `README.Rmd` suppresses build warnings in the setup chunk; `README.md` re-rendered (MSE 5.801 → 6.875, RMSE 2.409 → 2.622, stale warnings gone) | Done |
| 4. Four documentation typos | `exmine`→`examine` (`R/ols-cooks-d-chart.R:23`), `resiudual`→`residual` (`R/ols-plots-data.R:328`), `statistis`→`statistics` (`R/ols-anova.R:20`, `R/ols-regression.R:42`); `man/` regenerated | Done |
| 5. Fast `\dontrun{}` examples | `ols_step_all_possible_betas()` example now runs; the six `ols_step_both_*()` examples are `\donttest{}`; `R CMD check --as-cran` runs them (`--run-donttest`, 99 s) | Done |
| 6. `docs-dev/` out of the build | `.Rbuildignore` gains `^docs-dev$`; also `^\.git$` for M2 | Done |

## Deferred (not fixed, one line each)

- M3 `max_order` still builds every `combn()` subset before fitting — exponential allocation remains; would need a rewrite of the subset enumeration.
- M6 remaining test-coverage wishes (direct assertions for the five `ols_prep_*` helpers, more plot-data value tests, `expect_silent()` wrapping) — partially covered by the new value tests; the rest is test-only work.
- Gemini nice-to-have: `Nachtscheim` → `Nachtsheim` in 8 files, 5 vignette typos, 2 historic `NEWS.md` typos, `Date:` field, `cran-comments` PR links, `AGENTS.md` (`xplorerr` is in Suggests) — documentation polish outside the six must-fix items.
- `ols_step_all_possible_betas()` still attributes coefficients to models by their intercept rows, so a no-intercept model is now rejected explicitly ("requires a model with an intercept") instead of returning a table for a different model definition; the other two subset routines support no-intercept models and are tested.
- Weighted models with zero weights remain rejected by the diagnostics guarded by `check_weights_diagnostic()` (the guard tests `weights != 1`); this is the documented "weighted diagnostics not available" policy, not a wrong result.

## Checks

Full suite (`NOT_CRAN=true`, visual and snapshot tests enabled):

```
 failed   error warning skipped  passed
      0       0       0       0    2140
```

Build and check from a clean `git archive HEAD` copy in a temporary directory:

```
R CMD build --no-manual src
R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
  New maintainer:
    Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
  Old maintainer(s):
    Aravind Hebbali <hebbali.aravind@gmail.com>
```

0 errors, 0 warnings, 1 note. The note is the incoming-feasibility record of
the requested maintainer-email change (same maintainer); it is explained in
`cran-comments.md` for the CRAN submission. everything else passed, including
examples (38 s), examples with `--run-donttest` (99 s), the test suite
(118 s) and vignette rebuilding (30 s). The tarball contains no `.git`,
`docs-dev/`, `README.Rmd` or `cran-comments.md`.

## Commands run that changed something

```text
Rscript -e 'devtools::load_all(); ...'                 # reproductions for H1-H9, M1, M4, M5, L1
Rscript -e 'devtools::test(filter = ...)'              # per-finding runs while fixing
Rscript -e 'devtools::test()'                          # full suite, 2134 passes
Rscript -e 'rmarkdown::render("README.Rmd")'           # README.md re-knit, then CRLF -> LF
Rscript -e 'devtools::document()'                      # NAMESPACE + man/, roxygen2 8.1.1
git archive HEAD | tar -x -C ...; R CMD build --no-manual src
R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
```

Note: the installed roxygen2 is 8.1.1 (the package was last documented with
7.3.3), so `devtools::document()` also reformatted family cross-references in
`man/` and replaced `RoxygenNote` with `Config/roxygen2/version`. Only generated
files changed.

## Anything not done

- M3 and the documentation-polish items above are deferred deliberately (see the
  Deferred list).
- No API was changed. `ols_press()` keeps its name and now documents the
  weighted convention; `ols_test_breusch_pagan()` gains a `simdf` component
  (in addition to the documented `df`) so the printed simultaneous row can show
  the tested dimension; no exported function was added or removed.
- No new dependencies.
