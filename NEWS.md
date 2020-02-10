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

- mismatch in sign of partial and semi partial correlations  ([#44](https://github.com/rsquaredacademy/olsrr/issues/44))
- error in diagnostic panel ([#45](https://github.com/rsquaredacademy/olsrr/issues/45))
- standardized betas in the presence of interaction terms  ([#46](https://github.com/rsquaredacademy/olsrr/issues/46))

A big thanks goes to ([Dr. Kimberly Henry](http://psy.psych.colostate.edu/psylist/detail.asp?Num=163)) for
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
