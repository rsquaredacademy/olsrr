# Ordinary least squares regression

Ordinary least squares regression.

## Usage

``` r
ols_regress(object, ...)

# S3 method for class 'lm'
ols_regress(object, ...)
```

## Arguments

- object:

  An object of class "formula" (or one that can be coerced to that
  class): a symbolic description of the model to be fitted or class
  `lm`.

- ...:

  Other inputs.

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

  f statistis

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

## Interaction Terms

If the model includes interaction terms, the standardized betas are
computed after scaling and centering the predictors.

## References

https://www.ssc.wisc.edu/~hemken/Stataworkshops/stdBeta/Getting%20Standardized%20Coefficients%20Right.pdf

## Examples

``` r
ols_regress(mpg ~ disp + hp + wt, data = mtcars)
#>                          Model Summary                          
#> ---------------------------------------------------------------
#> R                       0.909       RMSE                 2.468 
#> R-Squared               0.827       MSE                  6.093 
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
#>                                ANOVA                                 
#> --------------------------------------------------------------------
#>                 Sum of                                              
#>                Squares        DF    Mean Square      F         Sig. 
#> --------------------------------------------------------------------
#> Regression     931.057         3        310.352    44.566    0.0000 
#> Residual       194.991        28          6.964                     
#> Total         1126.047        31                                    
#> --------------------------------------------------------------------
#> 
#>                                   Parameter Estimates                                    
#> ----------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t        Sig      lower     upper 
#> ----------------------------------------------------------------------------------------
#> (Intercept)    37.106         2.111                 17.579    0.000    32.782    41.429 
#>        disp    -0.001         0.010       -0.019    -0.091    0.929    -0.022     0.020 
#>          hp    -0.031         0.011       -0.354    -2.724    0.011    -0.055    -0.008 
#>          wt    -3.801         1.066       -0.617    -3.565    0.001    -5.985    -1.617 
#> ----------------------------------------------------------------------------------------

# if model includes interaction terms set iterm to TRUE
ols_regress(mpg ~ disp * wt, data = mtcars, iterm = TRUE)
#>                          Model Summary                          
#> ---------------------------------------------------------------
#> R                       0.922       RMSE                 2.296 
#> R-Squared               0.850       MSE                  5.273 
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
#>                                ANOVA                                 
#> --------------------------------------------------------------------
#>                 Sum of                                              
#>                Squares        DF    Mean Square      F         Sig. 
#> --------------------------------------------------------------------
#> Regression     957.299         3        319.100    52.947    0.0000 
#> Residual       168.749        28          6.027                     
#> Total         1126.047        31                                    
#> --------------------------------------------------------------------
#> 
#>                                   Parameter Estimates                                    
#> ----------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t        Sig      lower     upper 
#> ----------------------------------------------------------------------------------------
#> (Intercept)    44.082         3.123                 14.115    0.000    37.685    50.479 
#>        disp    -0.056         0.013       -0.385    -4.257    0.000    -0.083    -0.029 
#>          wt    -6.496         1.313       -0.616    -4.946    0.000    -9.186    -3.805 
#>     disp:wt     0.012         0.003        0.278     3.596    0.001     0.005     0.018 
#> ----------------------------------------------------------------------------------------
```
