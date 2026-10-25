# olsrr (development version)

## Bug Fixes

- `ols_test_normality()` no longer fails for samples larger than 5000 or smaller than 8; tests outside their sample-size range are skipped with a warning.
- The package no longer accesses the network or offers to update itself when attached, and `xplorerr` is now a suggested package; `ols_launch_app()` explains how to install it when it is missing.
- `ols_step_all_possible()` warns when a model has more than 15 predictors and refuses to run combinatorial selection with more than 20 predictors unless `force = TRUE`.
- `ols_test_f()`, `ols_test_score()` and `ols_test_breusch_pagan()` accept factor terms in `vars`. `ols_test_breusch_pagan()` prints the degrees of freedom of the tested term(s) instead of the number of `vars`, and rejects factor terms with `multiple = TRUE` with an informative error instead of failing with `object 'cyl' not found`. `vars` are now validated against the model matrix, so model-matrix column names keep working and components of interaction terms that are not in the model get the standard model-variable error.
- Stepwise selection is hardened against edge cases: the step counter is always initialized, backward elimination falls back to an intercept-only model when every predictor is dropped (#186), selection stops with an informative message when the residual sum of squares is below `1e-10` (#199), tied criteria are broken deterministically (#206), and forced `include` variables are no longer duplicated or removed by position (#172). `ols_regress()` also handles intercept-only models.
- `ols_regress()`, `ols_step_forward_p()`, `ols_step_backward_p()`, `ols_step_best_subset()`, `ols_step_all_possible()` and the AIC, SBC and R2 stepwise routines now refit models that contain transformed terms (`log()`, `factor()`, `poly()`, `I()`) correctly, returning the same estimates as the original `lm()` fit.
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
