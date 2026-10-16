# Score test

Test for heteroskedasticity under the assumption that the errors are
independent and identically distributed (i.i.d.).

## Usage

``` r
ols_test_score(model, fitted_values = TRUE, rhs = FALSE, vars = NULL)
```

## Arguments

- model:

  An object of class `lm`.

- fitted_values:

  Logical; if TRUE, use fitted values of regression model.

- rhs:

  Logical; if TRUE, specifies that tests for heteroskedasticity be
  performed for the right-hand-side (explanatory) variables of the
  fitted regression model.

- vars:

  Variables to be used for for heteroskedasticity test.

## Value

`ols_test_score` returns an object of class `"ols_test_score"`. An
object of class `"ols_test_score"` is a list containing the following
components:

- score:

  f statistic

- p:

  p value of `score`

- df:

  degrees of freedom

- fv:

  fitted values of the regression model

- rhs:

  names of explanatory variables of fitted regression model

- resp:

  response variable

- preds:

  predictors

## References

Breusch, T. S. and Pagan, A. R. (1979) A simple test for
heteroscedasticity and random coefficient variation. Econometrica 47,
1287–1294.

Cook, R. D. and Weisberg, S. (1983) Diagnostics for heteroscedasticity
in regression. Biometrika 70, 1–10.

Koenker, R. 1981. A note on studentizing a test for heteroskedasticity.
Journal of Econometrics 17: 107–112.

## See also

Other heteroskedasticity tests:
[`ols_test_bartlett()`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.md),
[`ols_test_breusch_pagan()`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md),
[`ols_test_f()`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md)

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# using fitted values of the model
ols_test_score(model)
#> 
#>  Score Test for Heteroskedasticity
#>  ---------------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: fitted values of mpg 
#> 
#>         Test Summary         
#>  ----------------------------
#>  DF            =    1 
#>  Chi2          =    0.6452907 
#>  Prob > Chi2   =    0.4218014 

# using predictors from the model
ols_test_score(model, rhs = TRUE)
#> 
#>  Score Test for Heteroskedasticity
#>  ---------------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: disp hp wt 
#> 
#>         Test Summary         
#>  ----------------------------
#>  DF            =    3 
#>  Chi2          =    0.945898 
#>  Prob > Chi2   =    0.8143398 

# specify predictors from the model
ols_test_score(model, vars = c('disp', 'wt'))
#> 
#>  Score Test for Heteroskedasticity
#>  ---------------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: disp wt 
#> 
#>         Test Summary         
#>  ----------------------------
#>  DF            =    2 
#>  Chi2          =    0.5349726 
#>  Prob > Chi2   =    0.7653008 
```
