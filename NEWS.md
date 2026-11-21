# olsrr (development version)

- Rewrote the getting-started, regression output, residual diagnostics and variance/normality vignettes

## Bug Fixes

- Printed p-values use four decimals and show `< 0.0001` below that resolution across regression, ANOVA, normality, heteroskedasticity and stepwise tables; returned numeric values and the documented Cramer-von Mises floor are unchanged (@aravindhebbali, pkg-olsrr-c25-fu2; follow-up to [#254](https://github.com/rsquaredacademy/olsrr/issues/254)).
- `ols_test_normality()` uses the Lilliefors-corrected Kolmogorov-Smirnov test (`nortest::lillie.test()`) and labels it "Kolmogorov-Smirnov (Lilliefors)". The test now accounts for the mean and standard deviation being estimated from the same residuals, so its p-value is smaller than the uncorrected `ks.test()` p-value it used to report; the statistic is unchanged. The test is skipped, with a warning, for fewer than 5 observations, the smallest sample `nortest::lillie.test()` supports ([#254](https://github.com/rsquaredacademy/olsrr/issues/254)).
- `ols_plot_fm()`, `ols_plot_resid_spread()`, both panels of `ols_plot_resid_fit_spread()` and panels 7 and 8 of `ols_plot_diagnostics()` now title the fit-mean panel "Fit - Mean" and the residual panel "Residual" instead of "Residual Fit Spread Plot" for both ([#253](https://github.com/rsquaredacademy/olsrr/issues/253)).
- `ols_test_normality()` stops with "y has no variation" when every non-missing value is identical, instead of failing inside `nortest::lillie.test()`; a constant sample has no defined normality test.

# olsrr 0.7.1

## Bug Fixes

- `ols_plot_resid_regressor()`, `ols_plot_resid_hist()`, `ols_plot_diagnostics()` and `ols_plot_comp_plus_resid()` take the residuals and fitted values of the rows the fit used, so models fitted with `na.action = na.exclude` plot the used observations instead of failing with a differing-row-count error or pairing padded residuals with used rows (M1, L1).
- `ols_step_all_possible()` and `ols_step_best_subset()` report the number of estimable terms rather than the number of candidate columns, so an exactly collinear predictor is not counted in `n`; the fit statistics are unchanged (L4).
- `ols_vif_tol()` warns that VIFs may not be sensible for a model without an intercept, as `car::vif()` does (also through `ols_coll_diag()`, which calls it); the tolerance and VIF values are unchanged (L3).
- `ols_test_breusch_pagan()` now computes Holm adjusted p values with `stats::p.adjust(p, "holm")`. Earlier versions sorted the raw p values, multiplied by a reversed rank and mapped them back, which dropped the cumulative maximum: the smallest raw p was returned unchanged, so the first predictor could be rejected at .05 when its Holm p was above the cutoff. The simultaneous test is unchanged; Bonferroni and Sidak were already correct (B1).
- `ols_plot_comp_plus_resid()` keeps every non-intercept column of the model matrix when it is built, so single-predictor and no-intercept models return one plot per predictor with one point per observation. Earlier versions dropped the only column (`[, -1]`) for a single predictor and the intercept handling silently reduced no-intercept models to one column, giving a one-row plot and losing predictors (B2).
- `ols_plot_resid_hist()` draws the histogram with the breaks used for the normal curve, so the curve and the bars share one bin width. Earlier versions scaled the curve to the `hist()` bin width while `geom_histogram(bins = 6)` drew different-width bars, so the fitted normal density was drawn at the wrong height (0.54x or 1.03x) (B3).
- `ols_plot_resid_regressor()` and `rvsr_plot_shiny()` align the regressor with the rows the fit used, so a model fitted with an omitted `NA` row plots instead of failing with a differing-row-count error (B4).
- `ols_plot_diagnostics(print_plot = FALSE)` no longer draws the ten sub-plots before returning them; each sub-plot is built with `print_plot = FALSE` (B5).
- `ols_prep_rvsrplot_data()` reports every non-intercept model-matrix column, so no-intercept and weighted models lose no predictor names and a single predictor returns a one-column data frame instead of a bare vector (B6).
- `ols_test_outlier()` caps the Bonferroni p values at 1, so they remain probabilities (earlier versions could report values above 1 for large samples) (B7).
- The grob-returning plots (`ols_plot_added_variable()`, `ols_plot_comp_plus_resid()`, `ols_plot_dfbetas()` and `ols_plot_diagnostics()`) draw the grob and return it invisibly when `print_plot = TRUE`, so they print in scripts, functions and loops instead of only at top level ([#161](https://github.com/rsquaredacademy/olsrr/issues/161)) (B8).
- `ols_correlations()` reports that models without an intercept are not supported with "more than one predictor coefficient", matching the guard that counts coefficients rather than term labels ([#232](https://github.com/rsquaredacademy/olsrr/issues/232)) (B9).
- Forward selection on a saturated model (`n <= p`, residual degrees of freedom 0) now starts from the null model and records the predictors selected before the next candidate saturates, instead of returning an empty result and losing the selection ([#199](https://github.com/rsquaredacademy/olsrr/issues/199)) (B10).

- Stepwise selection plots no longer cut off the highest value when the plotted metric (e.g. AIC) has both negative and positive values (F13).
- Stepwise offset identity checks compare exact numeric values, allowing labelled or attributed responses without weakening the rejection of a different fit (P02).
- Stepwise selectors choose one term per step. Selection rankings and criterion improvement tests use 10 significant digits: differences below that precision are treated as ties, equal criterion keys mean no improvement, and any remaining tie goes to the first candidate in formula order, after F ties are broken by p value (smaller for entry, larger for removal). P-value entry/removal cutoffs are compared without rounding. Histories identify the term actually removed with forced predictors and report SBIC against the original full model (P03, M1-M4, L1-L2).
- P-value selection ignores candidates with missing F tests and ends cleanly when none remain; an initial search with no eligible candidate gives the existing entry-criteria error (P04).
- Stepwise selectors return offset models fitted from the original call, preserving offset expressions for correct predictions on new data and a compact printed call (P01). If that fit cannot exactly reproduce the frozen-offset selection fit, selection stops with an error.
- `ols_regress()` and selection refits reject custom named contrast functions; fit with an explicit contrast matrix instead. Base contrast names are resolved from the stats namespace and refits use matrices, so changes to search-path functions or contrast options cannot change the fitted coding.
- Standardized-beta and selection refits carry the fit's frozen total offset with formula offset terms removed, preserving cancelling formula and call offsets exactly instead of recovering a contribution by subtraction.
- `ols_step_both_p()` with every term in `include` returns the included model with empty step metrics instead of failing.
- The deleted-studentized-residual, standardized-residual, studentized-residual, Cook's D, DFFITS and DFBETAS plot data/plot functions now compute their influence measure on the positive-weight fit rows and map the original observation IDs back. Fits that combine zero prior weights with `na.action = na.exclude` no longer fail with a differing-row-count error, and their values and observation IDs agree with the same model fitted after removing the unused rows.
- `ols_prep_rstudlev_data()` and `ols_plot_resid_lev()` compute the leverage threshold from the model rank rather than the number of coefficient slots, so adding an aliased predictor no longer raises the threshold and reclassifies genuine leverage points as "normal".
- `ols_press()`, `ols_pred_rsq()` and the other leverage-based diagnostics stop with an error ("refit with qr = TRUE") for a model fitted with `qr = FALSE`, instead of rebuilding the projection from the design matrix. The fitted tolerance and pivot cannot be recovered from the call, and a rebuilt decomposition can span different columns than the fit and give a wrong PRESS; only the fit's own QR is used.
- Standardized betas and every stepwise selection routine now rebuild the fit from the data found through the model call with `model.frame()` and `model.matrix()` (the call's subset, weights, offset, na.action and the fit's contrasts) and require the response, model matrix (values and column names, so factor coding), weights and offset to be identical to the fit's. If the data have changed since fitting, a factor was releveled, or the call symbol is bound to different data, they stop with "the data used to fit this model has changed or cannot be found; refit the model". When the call data cannot be found, the stored model frame is used only for formulas made of plain variables; transformed terms and offsets require the data. Unchanged fits with NULL weights/offset symbols, subsets, `na.exclude`, `poly()`/`scale()` terms and multiple offsets are accepted.
- `ols_regress()` no longer silently refits a model fitted with `qr = FALSE` or `model = FALSE`: re-evaluating the call could use another environment, tolerance or data and report a different model than the one supplied. It stops with "refit with lm(..., qr = TRUE, model = TRUE)".
- `ols_press()` and `ols_pred_rsq()` return `NA` with a warning naming the observations when a used row has leverage 1 (for example a singleton factor level or an isolated indicator), because its deleted residual cannot be estimated; previously rounding error gave a finite value. `ols_regress()` and `ols_model_performance()` show `NA` for predicted R-squared in that case.
- Cook's D (types 2 and 4) and DFFITS thresholds use the model rank and residual degrees of freedom rather than the number of coefficient slots, so an aliased predictor no longer changes the cutoff or hides influential observations.
- Observations with leverage 1 (for example the only member of a factor level) keep undefined (`NaN`) standardized/studentized residuals, Cook's D and DFFITS in the plot data and plots, as in base R, and a DFBETAS of 0, instead of arbitrary finite values flagged as normal. Plot ranges ignore the undefined values.
- `ols_prep_rstudlev_data()` computes leverages and studentized residuals for the positive-weight fit rows and maps the original observation IDs back, instead of calling the NA-padded `hatvalues()`/`rstudent()` path. Fits that combine zero prior weights with `na.action = na.exclude`, or that omit rows at the end of the data, no longer fail with a differing-row-count error; the returned leverages, studentized residuals and IDs agree with the same model fitted after removing the unused rows.
- Standardised betas keep the original coefficient names when a factor level contains text that looks like one of the internal placeholder names. The placeholder is now chosen so that it does not occur anywhere in the data names, factor levels or model-matrix columns, so translating the refitted names back no longer rewrites the factor level itself. The affected Std. Beta cells are printed against their own coefficient rows again.
- Standardised betas standardise a predictor even when it is a raw variable that also appears inside a transformed response, such as `wt` in `I(mpg / wt) ~ wt * hp` or `mpg` in `log(mpg) ~ mpg * hp`. The response itself is still carried through unchanged, but its separate predictor occurrence now uses its own scale instead of being left unstandardised.
- `ols_pure_error_anova()` derives the regression, error and lack-of-fit degrees of freedom from the model rank and the number of distinct predictor values, and obtains the regression sum of squares as the part of the total not explained by the residual sum of squares. No-intercept fits now report the same error degrees of freedom, overall F and nested lack-of-fit F as `summary()` and `anova()`. A model with no estimated direction beyond the intercept — an intercept-only fit or a constant/fully aliased predictor — is rejected instead of reporting a significant regression for an intercept-only fit.
- PRESS, predicted R-squared and the internal leverage calculation use only the first `rank` columns of the fit QR decomposition. Rank-deficient models (for example a predictor that is an exact multiple of another) previously included the non-estimable orthonormal directions, so `ols_press()` and the predicted R-squared printed by `ols_regress()` disagreed with the equivalent estimable fit.
- `ols_press()`, `ols_pred_rsq()` and `ols_regress()` take the leverage of each observation from the fit itself by row position in the model frame instead of matching row names. Fits that combine zero prior weights with `na.action = na.exclude` no longer pair residuals with another observation's leverage, and no longer fail when the omitted rows are at the end of the data.
- `ols_test_breusch_pagan()` and `ols_test_score()` stop with an informative error when the auxiliary regression has no direction beyond the intercept — a constant or fully aliased regressor, or an intercept-only model — instead of reporting a statistic with zero degrees of freedom, which gave a false `p = 0` rejection for `ols_test_breusch_pagan()` and a degenerate `p = 1` for `ols_test_score()`.
- `ols_pure_error_anova()` removes a fixed offset from the response before splitting the sums of squares, so the decomposition adds up for offset models: the error sum of squares equals `deviance(model)` and the overall regression F equals `summary()`. It previously grouped the raw response and reported an error sum of squares larger than the model's.
- `ols_regress()` standardised betas and the selection routines carry both offset contributions of a model that has `offset()` terms in its formula *and* an `offset` argument to `lm()`, instead of silently dropping the argument offset and standardising or selecting on a different model.
- Standardised betas read the term structure from the model terms rather than splitting deparsed term text, so a predictor whose name contains a colon is standardised correctly, and a term that cannot be evaluated in the model data raises an error instead of leaving that predictor unscaled.
- Standardised betas keep the model's contrast coding, so the names of a fit made with `contrasts = list(...)` match `coef()` and its values come from the same parameterisation. Selection refits keep the contrasts too, so an include-all selection returns the input fit instead of re-parameterising its factors.
- A predictor that is also used as an offset is standardised on its own scale; the offset itself stays a fixed, unstandardised vector.
- `ols_step_all_possible_betas()` stops with a clear error for weighted models instead of returning the unweighted coefficients of every subset. It now also documents that it requires a model with an intercept, and counts predictor terms rather than coefficients when checking the model size.
- `ols_step_all_possible()` and `ols_step_best_subset()` enumerate only the requested subset orders, so a bounded search with many predictors no longer builds the combination matrices for every order up to the number of predictors. A `max_order` larger than the model is rejected before any allocation.
- Predicted R-squared and PRESS now use the model weights for weighted models. PRESS is the weighted sum of squared deleted residuals, so it scales with the weights instead of moving towards one as the weights grow; predicted R-squared, the ratio of PRESS to the weighted total sum of squares, is invariant to a constant rescaling of the weights.
- Observations with zero prior weights are excluded consistently from PRESS, predicted R-squared, MAE and the information criteria, which now use the effective sample size and the fitted model's deviance. Zero-weight fits now agree with the same model fitted after removing those rows, and `na.exclude` and `na.omit` fits agree with each other.
- `ols_aic()` and `ols_sbc()` with `method = "SAS"` now use the model's residual sum of squares (honouring weights and `na.exclude`) instead of the unweighted, NA-padded residuals, so the criteria no longer depend on the missing-value action.
- `ols_regress()` aligns the coefficient table by name when the model has aliased coefficients, so standard errors, t statistics, p values and confidence limits stay with their own terms instead of shifting onto the next one; standardized betas are computed only for estimable coefficients.
- Standardized betas of interaction models now standardize transformed predictors after the transformation (`log(wt)`, `I(x^2)`), keep the levels and names of categorical terms, and preserve model offsets, both calling styles giving the same values. Matrix-valued terms such as `poly()` inside an interaction now give a clear error instead of a value from a different model.
- `ols_pure_error_anova()` gives the same lack-of-fit and overall regression statistics for a unit-weight model as for the unweighted model, and works for `na.exclude` fits.
- Stepwise and variable selection routines preserve the intercept and any fixed offset of the input model, so selecting with `include` set to every predictor returns the input fit, and models without an intercept are not refitted with one.
- Selection routines keep the formula environment of the input model for every candidate and final fit, so locally defined transformations and predictors outside the data frame work.
- `ols_test_breusch_pagan()` and `ols_test_score()` report the rank of the auxiliary regression (independent tested directions) as degrees of freedom, so aliased predictors no longer inflate the degrees of freedom and p values. Overlapping or duplicated `vars` selections are de-duplicated.
- `ols_test_f()`, `ols_test_score()` and `ols_test_breusch_pagan()` work for models fitted with `na.action = na.exclude`, giving the same results as the `na.omit` fit.
- `ols_vif_tol()` works for single-predictor models and for models without an intercept, returning tolerance and VIF for every model-matrix column instead of failing.
- `ols_test_bartlett()` stops with the same informative errors as `stats::bartlett.test()` when all observations are in one group or a group has fewer than two observations, instead of printing a result with `NaN` or `NA` statistics.
- `ols_regress()` now supports weighted linear models (`lm(..., weights = ...)`), matching `summary()`, `anova()`, and `confint()` for coefficient estimates, standard errors, test statistics, confidence intervals, R-squared, residual standard error, and the ANOVA decomposition, including models with interaction terms, whose standardised betas come from a weighted refit of the standardised predictors. Passing `weights` through the formula interface and passing a `glm` object now give clear errors, and stepwise and variable selection routines stop with a clear error message when passed a weighted model instead of selecting on unweighted refits ([#236](https://github.com/rsquaredacademy/olsrr/issues/236)).
- Diagnostics that mix weighted and unweighted sums of squares or refit auxiliary regressions without the model weights now stop with a clear error for weighted models: `ols_pure_error_anova()`, `ols_plot_added_variable()`, `ols_plot_comp_plus_resid()`, `ols_plot_resid_regressor()`, `ols_test_breusch_pagan()`, `ols_test_score()`, `ols_test_f()`, `ols_correlations()`, `ols_test_correlation()` and `ols_prep_avplot_data()`. Collinearity diagnostics (`ols_vif_tol()`, `ols_eigen_cindex()`, `ols_coll_diag()`) remain available because they are computed from the design matrix ([#236](https://github.com/rsquaredacademy/olsrr/issues/236)).
- `plot()` on a 0-step stepwise selection result (such as models with near-perfect fit) returns invisibly with an informative message instead of failing with a differing row count error ([#234](https://github.com/rsquaredacademy/olsrr/issues/234)).
- `ols_step_forward_aic()` (and related forward selection routines) handles models where all candidate predictors are forced in `include`, returning a valid result object with the included predictors that can be printed and plotted alongside an informative message that no candidate predictors were left ([#235](https://github.com/rsquaredacademy/olsrr/issues/235)).
- Stepwise and selection routines now preserve the model formula environment and call so the returned `$model` can be used directly with `ols_regress()` and `predict()` when transformed terms (`log()`, `poly()`, `factor()`, `I()`) are present ([#246](https://github.com/rsquaredacademy/olsrr/issues/246)).
- `ols_test_normality()`'s Cramér–von Mises test now compares the data with a normal distribution whose mean and standard deviation are estimated from the data (`nortest::cvm.test()`); it previously compared with the uniform distribution and rejected normality for almost any data.
- `ols_test_normality()` no longer fails for samples larger than 5000 or smaller than 8; tests outside their sample-size range are skipped with a warning.
- The package no longer accesses the network or offers to update itself when attached, and `xplorerr` is now a suggested package; `ols_launch_app()` explains how to install it when it is missing.
- `ols_step_all_possible()` now counts the models it will actually fit when warning about a large search or refusing one without `force = TRUE`, taking `max_order` into account instead of the number of predictors. It warns above `2^15 - 1` models and refuses more than `2^20 - 1` models unless `force = TRUE`.
- `ols_test_f()`, `ols_test_score()` and `ols_test_breusch_pagan()` accept factor terms in `vars`. `ols_test_breusch_pagan()` prints the degrees of freedom of the tested term(s) instead of the number of `vars`, and rejects factor terms with `multiple = TRUE` with an informative error instead of failing with `object 'cyl' not found`. `vars` are now validated against the model matrix, so model-matrix column names keep working and components of interaction terms that are not in the model get the standard model-variable error.
- Stepwise selection is hardened against edge cases: the step counter is always initialized, backward elimination falls back to an intercept-only model when every predictor is dropped (#186), selection stops with an informative message when the residual sum of squares is below `1e-10` (#199), tied criteria are broken deterministically (#206), and forced `include` variables are no longer duplicated or removed by position (#172). `ols_regress()` also handles intercept-only models.
- `ols_regress()`, `ols_step_forward_p()`, `ols_step_backward_p()`, `ols_step_best_subset()`, `ols_step_all_possible()` and the AIC, SBC, R2 and adjusted R2 stepwise routines now refit models that contain transformed terms (`log()`, `factor()`, `poly()`, `I()`) correctly, returning the same estimates as the original `lm()` fit, and printing a stepwise result no longer fails in the final model output for such terms ([#244](https://github.com/rsquaredacademy/olsrr/issues/244)).
- The final model from `ols_step_backward_p()` no longer repeats variables passed through `include`.
- Refits in `ols_regress()` and the selection routines now take the model's observations by row name, so models fitted on filtered data use the right rows and report the correct number of observations instead of shifted rows and a reduced sample size.
- Standardised betas of models with interaction terms no longer standardise the response variable, so interaction models with a transformed response (`log()`, `sqrt()`) return the correct values instead of a `NaN` warning and estimates from a subset of the data.
- `ols_step_both_p()` and the hierarchical selection routines stop with the near-perfect fit message instead of failing inside `Anova()`, and `ols_step_both_p()` ignores duplicated `include`/`exclude` variables.
- `ols_regress()` and `ols_model_performance()` now compute residual Mean Square Error (MSE) as `deviance(model) / model$df.residual`, aligning with the ANOVA residual mean square, and Root Mean Square Error (RMSE) as `sqrt(mse)` rather than using the sample variance denominator `mean(residuals^2)` ([#221](https://github.com/rsquaredacademy/olsrr/issues/221)).
- Information criteria (`ols_msep()`, `ols_fpe()`, `ols_apc()`, `ols_hsp()`, `ols_mallows_cp()`, `ols_sbic()`, `ols_aic()`, `ols_sbc()`) now evaluate model complexity using `model$rank`, fixing parameter counting for factor variables and rank-deficient models; `ols_msep()` now uses the residual mean square error rather than sum of squares; `ols_aic(corrected = TRUE)` now implements the Hurvich-Tsai correction for R and STATA methods; and `ols_sbic()` and `ols_aic()` include guards returning NA with warnings on boundary conditions ([#224](https://github.com/rsquaredacademy/olsrr/issues/224)).
- `ols_test_bartlett()` coerces `group_var` to a factor, so numeric grouping variables return the correct degrees of freedom, statistic and p-value (audit F4).
- `ols_pure_error_anova()` reports the overall regression F using the error mean square, matching `summary.lm()`, and returns `NA` for the lack of fit statistics when the model has no replicates (audit F5).
- `ols_prep_avplot_data()` drops the intercept only for models that have one, preserves `drop = FALSE`, and keeps the real predictor names (audit F7).
- `ols_correlations()` works for single-predictor models, returning the zero-order correlation with `NA` partial and part correlations, and gives a clear error for multi-predictor models without an intercept (audit F8).
- `ols_test_outlier()` reports the most extreme significant outliers when `n_max` truncates the result (audit F11).
- `ols_get_predictors()` always returns a `data.frame`, including for single-predictor models (audit F14).
- `ols_test_breusch_pagan()` accepts `vars = NULL` and falls back to the fitted values test; calling with `fitted.values = FALSE` without `vars` or `rhs` now errors (audit F15).
- `ols_regress()` forwards `conf.level`, `iterm` and `title` to the formula method when it is called with a model, so `conf_lm` uses the requested confidence level instead of always 95% and the title is printed in the parameter estimates table (audit F2, [#244](https://github.com/rsquaredacademy/olsrr/issues/244)).
- Standardised betas of models without an intercept now include every coefficient and are aligned with the coefficients by name, instead of dropping the first predictor and shifting the printed `Std. Beta` column (audit F3, [#244](https://github.com/rsquaredacademy/olsrr/issues/244)).
- `ols_plot_added_variable()` stops with a clear message when the model has fewer than two predictors, instead of failing while preparing the plot data (audit F10, [#244](https://github.com/rsquaredacademy/olsrr/issues/244)).

## Other changes

- `goftest` is no longer a dependency: the Cramér–von Mises test for normality is now `nortest::cvm.test()`.

# olsrr 0.7.0

This is a minor release for bug fixes and other enhancements.

## New Features

- compare performance of regression models using multiple metrics
- ANOVA for regression models
- evaluate performance of regression models
- comprehensive information from model objects

## Enhancements

- Fixed number of steps in stepwise regression ([#214](https://github.com/rsquaredacademy/olsrr/issues/214))

## Bug Fixes

- `ols_step_best_subset()` unable to force more than one variable in selection process ([#210](https://github.com/rsquaredacademy/olsrr/issues/210))
- `plot()` with `ols_step_both_aic()` doesn't show anything when AIC values are negative ([#212](https://github.com/rsquaredacademy/olsrr/issues/212))

# olsrr 0.6.1

This is a patch release for urgent bug fixes.

## Bug Fixes

- Limit maximum subset order in `ols_step_all_possible()` ([#202](https://github.com/rsquaredacademy/olsrr/issues/202))
- Check model type ([#204](https://github.com/rsquaredacademy/olsrr/issues/204))
- Mismatch in column names in `ols_step_all_possible()` ([#211](https://github.com/rsquaredacademy/olsrr/issues/211))
- RMSE is not square root of MSE in `ols_regress()` ([#213](https://github.com/rsquaredacademy/olsrr/issues/213))
- `geom_segment()` warning in `ols_plot_obs_fit()` ([#217](https://github.com/rsquaredacademy/olsrr/issues/217))
- New snapshot added every time tests are run ([#218](https://github.com/rsquaredacademy/olsrr/issues/218))

# olsrr 0.6.0

This is a minor release for bug fixes and other enhancements.

## New Features

- hierarchical selection can be enables when using `p` values as variable selection metric

## Enhancements

- force variables to be included or excluded from the model at all stages of variable selection
- Variable selection methods allow use of the following metrics:
    - p value
    - akaike information criterion (aic)
    - schwarz bayesian criterion (sbc)
    - sawa bayesian criterion (sbic)
    - r-square
    - adjusted r-square
- Choose threshold for determining influential observations in `ols_plot_dffits()`

## Bug Fixes

- Allow users to specify threshold for detecting outliers ([#178](https://github.com/rsquaredacademy/olsrr/issues/178))
- If `ols_test_outlier()` does not find any outliers, it returns largest positive residual instead of largest absolute residual ([#177](https://github.com/rsquaredacademy/olsrr/issues/177))
- using `ols_step_all_possible()` with Model created from dynamic function leads to `"Error in eval(model$call$data) . . . not found"` ([#176](https://github.com/rsquaredacademy/olsrr/issues/176))
- `ols_step_both_p(): Error in if (pvals[minp] <= pent) {: argument is of length zero` ([#175](https://github.com/rsquaredacademy/olsrr/issues/175))
- Handle extremely significant variables ([#173](https://github.com/rsquaredacademy/olsrr/issues/173))
- `ols_correlations()` returns error for models with 2 predictors ([#168](https://github.com/rsquaredacademy/olsrr/issues/168))
- `ols_step_both_aic()` doesn't return model ([#167](https://github.com/rsquaredacademy/olsrr/issues/167))
- `ols_regress()` returned residual standard error instead of RMSE (@jens-daniel-mueller, [#165](https://github.com/rsquaredacademy/olsrr/issues/165))
- Extracting model data ([#159](https://github.com/rsquaredacademy/olsrr/issues/159))
- ols_plot_resid_stud() fails to plot outliers due to y-axis range ([#155](https://github.com/rsquaredacademy/olsrr/issues/155))
- ols_correlations error ([#191](https://github.com/rsquaredacademy/olsrr/issues/191))
- Mallow's Cp behaves inconsistently depending on model specification ([#196](https://github.com/rsquaredacademy/olsrr/issues/196))
- ols_step_forward_p(...) problem using the funtion ols_step_forward_p ([#200](https://github.com/rsquaredacademy/olsrr/issues/200))
- Output of the command "ols_step_both_aic" doesn't contain final model ([#201](https://github.com/rsquaredacademy/olsrr/issues/201))

# olsrr 0.5.3

This is a patch release to reduce the number of packages imported and
fix other CRAN errors.

## New Features

- Bonferroni outlier test ([#129](https://github.com/rsquaredacademy/olsrr/issues/129))

## Breaking Changes

The following functions will now require the variable names to be enclosed within quotes

- `ols_test_bartlett()`
- `ols_plot_resid_regressor()`

# olsrr 0.5.2

This is a minor release to fix bugs from breaking changes in recipes package
and other enhancements.

## Enhancements

- variable selection procedures now return the final model as an object of
  class `lm` ([#81](https://github.com/rsquaredacademy/olsrr/issues/81))
- data preparation functions of selected plots are now exported to enable end
  users to create customized plots and to use plotting library of their
  choice ([#86](https://github.com/rsquaredacademy/olsrr/issues/81))

# olsrr 0.5.1

This is a patch release to fix minor bugs and improve error messages.

## Enhancements

olsrr now throws better error messages keeping in mind beginner and intermediate R users. It is
a work in progress and should get better in future releases.

## Bug Fixes

Variable selection procedures based on p values now handle categorical variables in the
same way as the procedures based on AIC values.

# olsrr 0.5.0

This is a minor release for bug fixes and API changes.

## API Changes

We have made some changes to the API to make it more user friendly:

- all the variable selection procedures start with `ols_step_*`
- all the test start with `ols_test_*`
- all the plots start with `ols_plot_*`

## Bug Fixes

- ols_regress returns error in the presence of interaction terms in the formula ([#49](https://github.com/rsquaredacademy/olsrr/issues/49))

- ols_regress returns error in the presence of interaction terms in the formula ([#47](https://github.com/rsquaredacademy/olsrr/issues/47))

- return current version ([#48](https://github.com/rsquaredacademy/olsrr/issues/48))

# olsrr 0.4.0

## Enhancements

- use `ols_launch_app()` to launch a shiny app for building models
- save beta coefficients for each independent variable in `ols_all_subset()` ([#41](https://github.com/rsquaredacademy/olsrr/issues/41))

## Bug Fixes

- mismatch in sign of partial and semi partial correlations ([#44](https://github.com/rsquaredacademy/olsrr/issues/44))
- error in diagnostic panel ([#45](https://github.com/rsquaredacademy/olsrr/issues/45))
- standardized betas in the presence of interaction terms ([#46](https://github.com/rsquaredacademy/olsrr/issues/46))

A big thanks goes to ([Dr. Kimberly Henry](https://kimberlyhenry.github.io/my_website/)) for
identifying bugs and other valuable feedback that helped improve the package.

# olsrr 0.3.0

This is a minor release containing bug fixes.

## Bug Fixes

- output from reg_compute rounded up to 3 decimal points ([#24](https://github.com/rsquaredacademy/olsrr/issues/24))
- added variable plot fails when model includes categorical variables ([#25](https://github.com/rsquaredacademy/olsrr/issues/25))
- all possible regression fails when model includes categorical predictors ([#26](https://github.com/rsquaredacademy/olsrr/issues/26))
- output from bartlett test rounded to 3 decimal points ([#27](https://github.com/rsquaredacademy/olsrr/issues/27))
- best subsets regression fails when model includes categorical predictors ([#28](https://github.com/rsquaredacademy/olsrr/issues/28))
- output from breusch pagan test rounded to 4 decimal points ([#29](https://github.com/rsquaredacademy/olsrr/issues/29))
- output from collinearity diagnostics rounded to 3 decimal points ([#30](https://github.com/rsquaredacademy/olsrr/issues/30))
- cook's d bar plot threshold rounded to 3 decimal points ([#31](https://github.com/rsquaredacademy/olsrr/issues/31))
- cook's d chart threshold rounded to 3 decimal points ([#32](https://github.com/rsquaredacademy/olsrr/issues/32))
- output from f test rounded to 3 decimal points ([#33](https://github.com/rsquaredacademy/olsrr/issues/33))
- output from measures of influence rounded to 4 decimal points ([#34](https://github.com/rsquaredacademy/olsrr/issues/34))
- output from information criteria rounded to 4 decimal points ([#35](https://github.com/rsquaredacademy/olsrr/issues/35))
- studentized residuals vs leverage plot threshold rounded to 3 decimal points ([#36](https://github.com/rsquaredacademy/olsrr/issues/36))
- output from score test rounded to 3 decimal points ([#37](https://github.com/rsquaredacademy/olsrr/issues/37))
- step AIC backward method AIC value rounded to 3 decimal points ([#38](https://github.com/rsquaredacademy/olsrr/issues/38))
- step AIC backward method AIC value rounded to 3 decimal points ([#39](https://github.com/rsquaredacademy/olsrr/issues/39))
- step AIC both direction method AIC value rounded to 3 decimal points ([#40](https://github.com/rsquaredacademy/olsrr/issues/40))

# olsrr 0.2.0

This is a minor release containing bug fixes and minor improvements.

## Bug Fixes

- inline functions in model formula caused errors in stepwise regression ([#2](https://github.com/rsquaredacademy/olsrr/issues/2))
- added variable plots (`ols_avplots`) returns error when model formula contains inline functions ([#3](https://github.com/rsquaredacademy/olsrr/issues/3))
- all possible regression (`ols_all_subset`) returns an error when the model formula contains inline functions or interaction variables ([#4](https://github.com/rsquaredacademy/olsrr/issues/4))
- best subset regression (`ols_best_subset`) returns an error when the model formula contains inline functions or interaction variables ([#5](https://github.com/rsquaredacademy/olsrr/issues/5))
- studentized residual plot (`ols_srsd_plot`) returns an error when the model formula contains inline functions ([#6](https://github.com/rsquaredacademy/olsrr/issues/6))
- stepwise backward regression (`ols_step_backward`) returns an error when the model formula contains inline functions or interaction variables ([#7](https://github.com/rsquaredacademy/olsrr/issues/7))
- stepwise forward regression (`ols_step_backward`) returns an error when the model formula contains inline functions ([#8](https://github.com/rsquaredacademy/olsrr/issues/8))
- stepAIC backward regression (`ols_stepaic_backward`) returns an error when the model formula contains inline functions ([#9](https://github.com/rsquaredacademy/olsrr/issues/9))
- stepAIC forward regression (`ols_stepaic_forward`) returns an error when the model formula contains inline functions ([#10](https://github.com/rsquaredacademy/olsrr/issues/10))
- stepAIC regression (`ols_stepaic_both`) returns an error when the model formula contains inline functions ([#11](https://github.com/rsquaredacademy/olsrr/issues/11))
- outliers incorrectly plotted in (`ols_cooksd_barplot`) cook's d bar plot ([#12](https://github.com/rsquaredacademy/olsrr/issues/12))
- regression (`ols_regress`) returns an error when the model formula contains inline functions ([#21](https://github.com/rsquaredacademy/olsrr/issues/21))
- output from step AIC backward regression (`ols_stepaic_backward`) is not properly formatted ([#22](https://github.com/rsquaredacademy/olsrr/issues/22))
- output from step AIC regression (`ols_stepaic_both`) is not properly formatted ([#23](https://github.com/rsquaredacademy/olsrr/issues/23))

## Enhancements

- cook's d bar plot (`ols_cooksd_barplot`) returns the threshold value used to classify the observations as outliers ([#13](https://github.com/rsquaredacademy/olsrr/issues/13))
- cook's d chart (`ols_cooksd_chart`) returns the threshold value used to classify the observations as outliers ([#14](https://github.com/rsquaredacademy/olsrr/issues/14))
- DFFITs plot (`ols_dffits_plot`) returns the threshold value used to classify the observations as outliers ([#15](https://github.com/rsquaredacademy/olsrr/issues/15))
- deleted studentized residuals vs fitted values plot (`ols_dsrvsp_plot`) returns the threshold value used to classify the observations as outliers ([#16](https://github.com/rsquaredacademy/olsrr/issues/16))
- studentized residuals vs leverage plot (`ols_rsdlev_plot`) returns the threshold value used to detect outliers/high leverage observations ([#17](https://github.com/rsquaredacademy/olsrr/issues/17))
- standarized residuals chart (`ols_srsd_chart`) returns the threshold value used to classify the observations as outliers ([#18](https://github.com/rsquaredacademy/olsrr/issues/18))
- studentized residuals plot (`ols_srsd_plot`) returns the threshold value used to classify the observations as outliers ([#19](https://github.com/rsquaredacademy/olsrr/issues/19))

## Documentation

There were errors in the description of the values returned by some functions. The documentation has been thoroughly revised and improved in this release.

# olsrr 0.1.0

First release.
