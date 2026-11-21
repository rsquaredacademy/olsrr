# `Ordinary least squares regression`

# `Ordinary least squares regression`

Source:
[`R/ols-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-regression.R)

`ols_regress.Rd`

Ordinary least squares regression.

## Usage

``` r
ols_regress(object, ...)

# S3 method for class 'lm'
ols_regress(object, conf.level = 0.95, iterm = FALSE, title = "model", ...)
```

## Arguments

- object:

  An object of class "formula" (or one that can be coerced to that
  class): a symbolic description of the model to be fitted or class
  `lm`.

- ...:

  Other inputs.

- conf.level:

  Confidence level for the confidence intervals of the coefficients.

- iterm:

  Logical; if `TRUE`, the predictors are scaled and centered before
  computing the standardized betas. See *Interaction Terms* below.

- title:

  Character; title of the model, printed as the header of the parameter
  estimates table.

## Value

`ols_regress` returns an object of class `"ols_regress"`. An object of
class `"ols_regress"` is a list containing the following components:

- r:

  square root of rsquare, correlation between observed and predicted
  values of dependent variable

- rsq:

  coefficient of determination or r-square

- adjr:

  adjusted rsquare

- rmse:

  root mean squared error

- cv:

  coefficient of variation

- mse:

  mean squared error

- mae:

  mean absolute error

- aic:

  akaike information criteria

- sbc:

  bayesian information criteria

- sbic:

  sawa bayesian information criteria

- prsq:

  predicted rsquare

- error_df:

  residual degrees of freedom

- model_df:

  regression degrees of freedom

- total_df:

  total degrees of freedom

- ess:

  error sum of squares

- rss:

  regression sum of squares

- tss:

  total sum of squares

- rms:

  regression mean square

- ems:

  error mean square

- f:

  f statistics

- p:

  p-value for `f`

- n:

  number of predictors including intercept

- betas:

  betas; estimated coefficients

- sbetas:

  standardized betas

- std_errors:

  standard errors

- tvalues:

  t values

- pvalues:

  p-value of `tvalues`

- df:

  degrees of freedom of `betas`

- conf_lm:

  confidence intervals for coefficients

- title:

  title for the model

- dependent:

  character vector; name of the dependent variable

- predictors:

  character vector; name of the predictor variables

- mvars:

  character vector; name of the predictor variables including intercept

- model:

  input model for `ols_regress`

## Details

Refits accept explicit contrast matrices or the base contrast names
`contr.treatment`, `contr.sum`, `contr.helmert`, `contr.poly` and
`contr.SAS` (resolved from the stats namespace); models fitted with
custom named contrast functions must be refitted with an explicit
matrix, for example `contrasts = list(g = my_matrix)`. Refits carry the
fitted model's frozen total offset as an argument with all formula
[`offset()`](https://rdrr.io/r/stats/offset.html) terms removed for
standardized-beta and selection computations. Returned stepwise models
are fitted from the original call, preserving formula and argument
offsets for prediction on new data. Their coefficients, fitted values
and total offset must be identical to the frozen-offset selection fit;
otherwise selection stops with an error because prediction with new data
is not supported for that model.

For non-hierarchical stepwise selection, candidate rankings and
criterion improvement tests use 10 significant digits: values that agree
to 10 significant digits are ties, equal criterion values mean no
improvement, and a remaining tie goes to the first candidate in formula
order after any F tie is broken by p value (smaller for entry, larger
for removal). P-value entry and removal cutoffs are compared without
rounding.

## Interaction Terms

If the model includes interaction terms, the standardized betas are
computed after scaling and centering the predictors (with the model's
prior weights when the model is weighted) and refitting the model.

## Weighted Models

Models fitted with `lm(..., weights = ...)` are supported. The reported
estimates, standard errors, test statistics, confidence intervals and
R-squared match [`summary()`](https://rdrr.io/r/base/summary.html),
[`anova()`](https://rdrr.io/r/stats/anova.html) and
[`confint()`](https://rdrr.io/r/stats/confint.html). Weights cannot be
supplied through the formula interface; fit the model with
[`lm()`](https://rdrr.io/r/stats/lm.html) and pass the fitted model
instead. Objects of class `glm` are not supported; use
[`lm()`](https://rdrr.io/r/stats/lm.html) or the formula interface.

## References

https://www.ssc.wisc.edu/~hemken/Stataworkshops/stdBeta/Getting%20Standardized%20Coefficients%20Right.pdf

Kutner, M. H., Nachtsheim, C. J., Neter, J. and Li, W. (2005). *Applied
Linear Statistical Models*, 5th ed. New York: McGraw-Hill/Irwin.

Aiken, L. S. and West, S. G. (1991). *Multiple Regression: Testing and
Interpreting Interactions*. Newbury Park, CA: Sage.

## Examples

``` r
ols_regress(mpg ~ disp + hp + wt, data = mtcars)
#>                          Model Summary                          
#> ---------------------------------------------------------------
#> R                       0.909       RMSE                 2.639 
#> R-Squared               0.827       MSE                  6.964 
#> Adj. R-Squared          0.808       Coef. Var           13.135 
#> Pred R-Squared          0.768       AIC                158.643 
#> MAE                     1.907       SBC                165.972 
#> ---------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                 ANOVA                                  
#> ----------------------------------------------------------------------
#>                 Sum of                                                
#>                Squares        DF    Mean Square      F           Sig. 
#> ----------------------------------------------------------------------
#> Regression     931.057         3        310.352    44.566    < 0.0001 
#> Residual       194.991        28          6.964                       
#> Total         1126.047        31                                      
#> ----------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    37.106         2.111                 17.579    < 0.0001    32.782    41.429 
#>        disp    -0.001         0.010       -0.019    -0.091      0.9285    -0.022     0.020 
#>          hp    -0.031         0.011       -0.354    -2.724      0.0110    -0.055    -0.008 
#>          wt    -3.801         1.066       -0.617    -3.565      0.0013    -5.985    -1.617 
#> -------------------------------------------------------------------------------------------

# if model includes interaction terms set iterm to TRUE
ols_regress(mpg ~ disp * wt, data = mtcars, iterm = TRUE)
#>                          Model Summary                          
#> ---------------------------------------------------------------
#> R                       0.922       RMSE                 2.455 
#> R-Squared               0.850       MSE                  6.027 
#> Adj. R-Squared          0.834       Coef. Var           12.219 
#> Pred R-Squared          0.801       AIC                154.018 
#> MAE                     1.897       SBC                161.346 
#> ---------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                 ANOVA                                  
#> ----------------------------------------------------------------------
#>                 Sum of                                                
#>                Squares        DF    Mean Square      F           Sig. 
#> ----------------------------------------------------------------------
#> Regression     957.299         3        319.100    52.947    < 0.0001 
#> Residual       168.749        28          6.027                       
#> Total         1126.047        31                                      
#> ----------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    44.082         3.123                 14.115    < 0.0001    37.685    50.479 
#>        disp    -0.056         0.013       -0.385    -4.257      0.0002    -0.083    -0.029 
#>          wt    -6.496         1.313       -0.616    -4.946    < 0.0001    -9.186    -3.805 
#>     disp:wt     0.012         0.003        0.278     3.596      0.0012     0.005     0.018 
#> -------------------------------------------------------------------------------------------
```
