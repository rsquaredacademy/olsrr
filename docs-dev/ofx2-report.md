# ofx2 — olsrr fix round 2 report

Worktree: `task/olsrr-fix-071` (`F:/R/Packages/statistics/olsrr-wt/fix-071`).
Base of this round: `c1d4e7a`. Round 2 start: `26e91e1..c1d4e7a` already in
history. Input: `F:/pm/notes/2026-11-01-pkg-audits/ofxx-reaudit.md`
(13 findings: 7 High, 5 Medium, 1 Low) and `common-pkgfix.md`.
Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt), testthat 3, vdiffr.

Every finding in the re-audit was reproduced at `c1d4e7a` before being fixed,
each fix is preceded by a value-asserting test that fails on the old code, and
the re-audit's own reproduction is re-run at the end (see "Re-audit
reproductions" below). H7's explicit rejection of no-intercept beta tables is
kept, as the re-audit recommended.

## Findings

| ID | Sev | Test added (file: line of the test title) | Fix (file: line) | Verified how | Status |
|---|---|---|---|---|---|
| **F01** mixed formula/call offsets dropped in refits | High | `tests/testthat/test-regress.R`: "mixed formula and call offsets are carried through the refit (H6)"; `tests/testthat/test-step-forward.R`: "selection keeps mixed formula and call offsets (H6)" | `R/utils.R:178` `model_formula_offset()`, `R/utils.R:221` `model_call_offset()`; used at `R/ols-regress_compute.R:64` and `R/ols-stepwise-utils.R:764` | mixed offsets now equal the equivalent `lm(mpg ~ wt*hp, offset = qsec + drat)` model: betas `-0.75369001, -0.04120972, 0.21597235`; include-all selection returns `24.72888949, -8.27145748, -0.08321612, 0.02473964` (was `30.93181914, …`); the all-possible full-model row equals `coef(m)`; candidate metrics of both offset styles are identical | Fixed |
| **F02** offset variable excluded from scaling | High | `tests/testthat/test-regress.R`: "an offset variable is still standardised as a predictor (H6)" | `R/ols-regress_compute.R:214` (the skip list is now the response only) | `mpg ~ wt*hp + offset(wt)` and the call-style equivalent both return `-0.8331083, -0.3470564, 0.2431090` (was `-0.8331083, -1.3662875, 1.0601535`), matching an independent refit with both predictors scaled and the unscaled `wt` as offset; repeated for `log(wt)` and prior weights | Fixed |
| **F03** explicit factor contrasts discarded | High | `tests/testthat/test-regress.R`: "standardised betas keep the model's contrast coding (H5)"; `tests/testthat/test-step-forward.R`: "selection keeps the model's contrast coding (H5)" | `R/utils.R:157` `model_contrasts()`; passed at `R/ols-regress_compute.R:68` and `R/ols-stepwise-utils.R:772` | `contr.sum` model returns `0.1645928, 0.1614386, -0.5105317, -0.4957139, 0.2765400` with names `factor(cyl)1, factor(cyl)2, …` that match `coef(m)`; include-all selection gives intercept `23.82061622` and the same factor names (was `28.65011816`) | Fixed |
| **F04** term text split at every colon | High | `tests/testthat/test-regress.R`: "term structure comes from the terms object, not from splitting on colons (H5)" | `R/ols-regress_compute.R:187`–`:301` (`ols_scale_terms()`) and `R/ols-regress_compute.R:304` (`ols_term_names()`) | term components come from the row/column structure of `attr(terms, "factors")`; `` `a:b` * hp `` returns `-0.6707612, -0.3470564, 0.2431090` (was `-0.6707612, -1.3662875, 1.0601535`); placeholder names are fixed-width and translated by literal replacement, so a colon in a name is never an interaction; an unevaluable term now errors ("could not be evaluated") instead of staying unscaled | Fixed |
| **F05** rank-zero auxiliary gives a false BP rejection | High | `tests/testthat/test-breusch-pagan.R` and `tests/testthat/test-score.R`: "an auxiliary design with no tested direction is rejected (H8)" | `R/ols-error-messages.R:38` `check_aux_rank()`; called at `R/ols-breusch-pagan-test.R:331` (`bp_model()`), `:267` (`bp_case_3()`), `:386` (`bp_case_loop()`), and `R/ols-score-test.R:153` (`rhs_score()`), `:169` (`fit_score()`), `:188` (`var_score()`) | `vars = "z"` with `z = 1` now errors with "no tested direction" instead of `bp = 6.9e-32, df = 0, p = 0` / `score df = 0, p = 1`; the multiple/one-at-a-time path and an intercept-only model (which reported `bp = 28.79, p = 8.1e-8` from `anova()`'s residual row) hit the same guard; `rhs = TRUE` neighbours keep `df = 2, bp = 1.0267659` and `df = 2, score = 0.8807225` | Fixed |
| **F06** lack-of-fit ANOVA not offset-safe | High | `tests/testthat/test-loftest.R`: "offsets are removed from the response before the decomposition (H4)" | `R/ols-pure-error-anova.R:171` `pea_frame()` (adjusted response) and `:215` `pea_data_comp()` (adjusted fitted values) | `lm(mpg ~ disp + offset(qsec))` now gives `ess = 363.40498 = deviance(m)`, `lackoffit = 352.73732`, `pure_error = 10.66767`, `rf = 48.05078 = summary(m)$fstatistic[1]` (was `366.83698 / 47.60123`), equal to `lm(I(mpg - qsec) ~ disp)`; both offset styles, unit weights and `na.exclude`/`na.omit` agree | Fixed |
| **F07** all-possible beta table drops weights | High | `tests/testthat/test-all-possible.R`: "the beta table rejects weighted models instead of dropping weights (H7)" | `R/ols-all-possible-regression.R:303` | weighted input now stops with "weighted models are not supported by ols_step_all_possible_betas"; unit weights and unweighted input still return the input fit's coefficients (was `37.22727012, -3.87783074, -0.03177295` instead of `37.93826738, -4.10173904, -0.03034698`) | Fixed |
| **F08** positional leverage fallback can misalign | Medium | `tests/testthat/test-weighted-diagnostics.R`: "PRESS aligns leverages by model-frame row, not by row name (H2)" | `R/utils.R:268` `model_hatvalues_used()`, `R/utils.R:280` `model_hatvalues_frame()` | leverages are read from the fit QR, which has one row per observation used in the fit, and are aligned by row index of the model frame; unnamed and repeated residual/row names now give `236.5615444` for PRESS and `0.7895890` for predicted R² in both cases (was `235.5134826` / `0.7905212` for the unnamed case); no name matching and no length inference remains | Fixed |
| **F09** ordinary zero weights + `na.exclude` fail at end rows | Medium | `tests/testthat/test-weighted-diagnostics.R`: "zero-weight rows and end-of-frame omissions keep PRESS correct (H2)" | same code path as F08 (leverages no longer come from `hatvalues()`, so `lm.influence()`'s padded `naresid()` call is never reached) | zero weight at row 1 with the missing row at 32 now returns `236.5579`, matching the equivalent fit on rows 3–32, with no warnings and no `'names' attribute [32] …` error; second-to-last and multiple-omission layouts also agree | Fixed |
| **F10** bounded searches allocate all subsets | Medium | `tests/testthat/test-all-possible.R`: "a bounded search enumerates only the requested subset orders (M3)", "bounded searches stay cheap for many predictors (M3)" | `R/ols-all-possible-regression.R:344`–`:359` (`allpos_helper()`) and `R/ols-best-subsets-regression.R:98`–`:113` | 16 predictors at `max_order = 1` now makes 1 `combn()` call and allocates 280 bytes (was 16 calls and 2,100,608 bytes); 16/22 predictors at order 2 make 2 calls and stay under 100 kB (the 22-predictor case used ≥ 184 MB); an impossible `max_order` is rejected before any allocation; fitted model counts and R² values are unchanged | Fixed |
| **F11** win-builder evidence predates the fixes | Medium | n/a (release record) | `cran-comments.md:17`–`:20`; superseded status for Gemini item 2 in `docs-dev/olsrr-fix-071-report.md` | the 2026-10-27 run is described as the pre-fix code and win-builder is "to re-run"; no result is invented and R-hub/macOS stay "to run" | Fixed |
| **F12** earlier M6 coverage debt | Medium | `tests/testthat/test-plot-data.R` (4 tests, 24 assertions) covering `ols_prep_regress_y`, `ols_prep_outlier_obs`, `ols_prep_cdplot_outliers`, `ols_prep_dfbeta_data`, `ols_prep_dfbeta_outliers`; `tests/testthat/test-regress.R:222`–`224` replaces the last `expect_silent()` | tests only | selected observation ids (`17, 20, 31` cooks-d outliers; `9, 20, 21` dfbeta outliers), the cooks distances, the residual vector of `ols_prep_regress_y()` and the dfbeta selection are asserted against the model's own `cooks.distance()`/`dfbetas()` and against written-out values; a parsed census now finds ≥ 2 direct references for each of the five exports | Fixed |
| **F13** release prose and API documentation | Low | `tests/testthat/test-all-possible.R` (in the H7 test block): exact-two-predictor no-intercept message and the 1-predictor count | `R/ols-influence-measures.R:107` and `:148` (PRESS scales with the weights; only predicted R² is invariant), `NEWS.md:5`, `R/ols-all-possible-regression.R:308` (term count), `R/ols-all-possible-regression.R:285` (documented restrictions), `R/ols-collinearity-diagnostics.R:44` (stray `#'` removed, `man/ols_coll_diag.Rd` regenerated), `docs-dev/olsrr-fix-071-report.md` ("no API was changed" corrected) | PRESS is documented as scaling with the weights, matching the existing test `ols_press(10w) = 10 * ols_press(w)`; `lm(mpg ~ 0 + wt + hp)` now reports "requires a model with an intercept" instead of "at least 2 predictors"; the intercept and weighted restrictions are in the Rd; `man/ols_coll_diag.Rd` no longer contains `#'` | Fixed |

### H2 detail (explicit request)

The positional fallback added in `106feee` is gone. `model_hatvalues_frame()`
does not look at row names at all:

* `qr.Q(model$qr)` has exactly one row per observation used in the fit, in
  model-frame order (rows with a zero prior weight are not in the QR), so the
  leveraged rows are written into a zero vector at `model_used_rows(model)`
  positions — alignment is by row index of the model frame;
* if the QR row count does not match, the leverages are recomputed from
  `model.matrix()` and `model$weights`, again in model-frame row order;
* `model_hatvalues_used()` then selects the used rows. Nothing infers row
  identity from lengths and nothing matches by name.

`tests/testthat/test-weighted-diagnostics.R` covers the named case, unnamed
residuals, and **repeated row names** (`names(m$residuals)` and
`attr(m$model, "row.names")` both set to `a, b, a, b, …`), each asserting the
PRESS and predicted-R² values.

### H7

Unchanged: `ols_step_all_possible_betas()` still rejects models without an
intercept, and the rejection is now documented (see F13). F07 closes the
remaining weighted-input path through the same function; the other two subset
APIs keep supporting no-intercept models.

## Commits (one per finding, `(ofx2)` suffix)

```text
bd20b9c fix: align leverages by model-frame row instead of row name (H2) (ofx2)          F08
9b10066 test: zero-weight rows with end-of-frame omissions (H2) (ofx2)                   F09
9226d14 fix: reject heteroskedasticity tests with no tested direction (H8) (ofx2)        F05
cf48a86 fix: remove fixed offsets from the response before the lack-of-fit split (H4)    F06
d27d321 fix: reject weighted models in the all-possible beta table (H7) (ofx2)           F07
3706d5a perf: enumerate only the requested subset orders (M3) (ofx2)                     F10
4fbf543 fix: carry formula and call offsets together in refits (H6) (ofx2)               F01
071c041 fix: standardise a predictor that is also an offset (H6) (ofx2)                  F02
7f90d03 fix: keep the model's contrast coding in standardised and candidate refits (H5)  F03
85771cb fix: read model term structure from the terms object, not from term text (H5)    F04
dcb79b1 docs: label the 2026-10-27 win-builder run as pre-fix (F11) (ofx2)               F11
4eb3579 test: add direct value tests for the plot-data helpers (F12) (ofx2)              F12
f2bcff5 docs: correct the PRESS scaling claim, document the beta-table restrictions      F13
f996ec3 docs: NEWS entries for the re-audit fixes (ofx2)                                 NEWS
d0adfb4 test: assert standardised beta values instead of only silence (F12) (ofx2)       F12
57ef037 test: fix a typo in a test title (ofx2)                                          tidy-up
b77a9eb refactor: drop a shadowed variable in ols_scale_terms() (ofx2)                   tidy-up
```

F08 and F09 share one code path, so the functional change is in the F08 commit;
the F09 commit adds the ordinary-input assertions that failed before it (the
test was written and run red before the F08 fix landed).

## Re-audit reproductions after the fixes

Run with `devtools::load_all()` on the final tree (`.ofx2-final.R`, outside the
worktree):

```text
F01 sbetas   mixed = equivalent = -0.75369001, -0.04120972, 0.21597235
F01 select   input = selected  = 24.72888949, -8.27145748, -0.08321612, 0.02473964
F01 betas    full-model row = coef(m)
F02          inline = call = -0.8331083, -0.3470564, 0.2431090
F03 sbetas   0.1645928, 0.1614386, -0.5105317, -0.4957139, 0.2765400
F03 select   intercept 23.82062, same factor names as coef(m)
F04          -0.6707612, -0.3470564, 0.2431090
F05          errors: "no tested direction beyond the intercept" (bp, score, intercept-only)
F06          ess 363.40498 = deviance; f 48.05078 = summary; lof 352.73732; pe 10.66767
F07          error: weighted models are not supported by ols_step_all_possible_betas
F08          named = unnamed = reference = 236.561544; pred rsq 0.789589
F09          got = ref = 236.5579 (no warnings, no error)
F10          16 models, 1 combn call, 280 bytes (was 16 calls / 2,100,608 bytes)
F13          "ols_step_all_possible_betas() requires a model with an intercept." (0 + wt + hp)
```

## Checks

Full local suite (`NOT_CRAN=true`, snapshots and visual tests enabled):

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2266 ]
```

Clean archive build and CRAN check, from `git archive` of the final code state
(no working-tree copy, no reports; `docs-dev/` is excluded by `.Rbuildignore`, so
this report's own commit cannot change the checked tarball):

```text
R CMD build --no-manual .
R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
Status: 1 NOTE
```

* 0 errors, 0 warnings, 1 NOTE.
* The NOTE is the CRAN incoming feasibility check recording the maintainer
  e-mail change (`hebbali.aravind@gmail.com` → `aravindhebbali@rsquaredacademy.com`),
  which is the same maintainer; it is pre-existing for this release and cannot
  be removed by a local check.
* Examples 35 s, `--run-donttest` examples 100 s, tests 113 s, vignette rebuild
  28 s. Check-mode tests: `FAIL 0 | WARN 0 | SKIP 83 | PASS 2056` (the 83 skips
  are the `skip_on_cran()` snapshot/visual tests, run in the full local suite).
* The tarball has 464 entries and contains no `.git`, `docs-dev/`, `README.Rmd`
  or `cran-comments.md`. `docs-dev/` is excluded by `.Rbuildignore`, so
  committing this report does not change the checked tarball.

`LC_ALL=C` and `LANG=C` were used, as in the re-audit: a `C.UTF-8` locale is not
available to this Windows R and produces spurious DESCRIPTION subprocess noise.

## Commands run that changed something

```text
Rscript run-tests.R <filter>      # NOT_CRAN=true + devtools::load_all(); run red before each fix, green after
Rscript ofx2-final.R              # re-ran every audit reproduction against the fixed tree
git stash push/pop R/...          # re-ran the new tests against the pre-fix code for F05 and F10
Rscript -e "devtools::document('.')"   # regenerated 6 Rd files (PRESS/PRED R2 prose, beta-table details,
                                        # BP/score notes, pure-error offset note, stray "#'" removal)
git archive HEAD | tar -x -C <tmp>      # clean copy for build/check
R CMD build --no-manual . ; R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz
```

## Deferred

- `ols_prep_rstudlev_data()` still calls `hatvalues()`/`rstudent()` directly and
  so still recycles (or fails) for `na.exclude` fits with zero weights; it is a
  plotting helper, not one of the reported wrong values, and switching it to the
  new leverage helper would change plot data.
- `check_npredictors()` still counts coefficients rather than terms for the two
  subset entry points; the re-audit only asked for the beta-table message, and
  changing the shared guard would alter which models the subset routines accept.
- Gemini documentation polish (typos in old NEWS/vignettes, `Date:` field,
  `AGENTS.md` wording) remains untouched.
- `ols_step_final_model()` still writes `final_model$call$offset` from the input
  call although `predict.lm()` reads offsets from the terms; harmless, but it
  makes `update()` semantics on the returned model unclear.
- The committed `revdep/` and `revdep_results/` artifacts were not re-run; they
  are not evidence for this HEAD.

## Anything not done

- No push, no GitHub operations, no branch changes, no other worktrees touched.
- No new dependency and no exported function added or removed.
- Accepted-input restrictions added in this round (all documented in NEWS and
  Rd): `ols_test_breusch_pagan()`/`ols_test_score()` error when the auxiliary
  regression has no tested direction (constant, fully aliased or intercept-only
  design); `ols_step_all_possible_betas()` errors for weighted models (in
  addition to the no-intercept rejection kept from the previous round);
  standardised betas error when a model term cannot be evaluated in the model
  data.
- No remote check (win-builder, R-hub, macOS) was submitted. `cran-comments.md`
  says so and marks them "to re-run"/"to run"; the release record still needs a
  real run after this HEAD.
- The uncommitted re-audit report `docs-dev/ofxx-reaudit.md` and the
  `.ofxx-tmp/` audit scratch directory were left in place, untracked, as found.
