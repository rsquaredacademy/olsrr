# F test

Test for heteroskedasticity under the assumption that the errors are
independent and identically distributed (i.i.d.).

## Usage

``` r
ols_test_f(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...)
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

- ...:

  Other arguments.

## Value

`ols_test_f` returns an object of class `"ols_test_f"`. An object of
class `"ols_test_f"` is a list containing the following components:

- f:

  f statistic

- p:

  p-value of `f`

- fv:

  fitted values of the regression model

- rhs:

  names of explanatory variables of fitted regression model

- numdf:

  numerator degrees of freedom

- dendf:

  denominator degrees of freedom

- vars:

  variables to be used for heteroskedasticity test

- resp:

  response variable

- preds:

  predictors

## References

Wooldridge, J. M. 2013. Introductory Econometrics: A Modern Approach.
5th ed. Mason, OH: South-Western.

## See also

Other heteroskedasticity tests:
[`ols_test_bartlett()`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.md),
[`ols_test_breusch_pagan()`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md),
[`ols_test_score()`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)

# using fitted values
ols_test_f(model)
#> 
#>  F Test for Heteroskedasticity
#>  -----------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: fitted values of mpg 
#> 
#>       Test Summary        
#>  -------------------------
#>  Num DF     =    1 
#>  Den DF     =    30 
#>  F          =    0.4920617 
#>  Prob > F   =    0.4884154 

# using all predictors of the model
ols_test_f(model, rhs = TRUE)
#> 
#>  F Test for Heteroskedasticity
#>  -----------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: disp hp wt qsec 
#> 
#>       Test Summary        
#>  -------------------------
#>  Num DF     =    4 
#>  Den DF     =    27 
#>  F          =    0.4594694 
#>  Prob > F   =    0.7647271 

# using fitted values
ols_test_f(model, vars = c('disp', 'hp'))
#> 
#>  F Test for Heteroskedasticity
#>  -----------------------------
#>  Ho: Variance is homogenous
#>  Ha: Variance is not homogenous
#> 
#>  Variables: disp hp 
#> 
#>       Test Summary        
#>  -------------------------
#>  Num DF     =    2 
#>  Den DF     =    29 
#>  F          =    0.4669306 
#>  Prob > F   =    0.631555 
```
