# Breusch pagan test

Test for constant variance. It assumes that the error terms are normally
distributed.

## Usage

``` r
ols_test_breusch_pagan(
  model,
  fitted.values = TRUE,
  rhs = FALSE,
  multiple = FALSE,
  p.adj = c("none", "bonferroni", "sidak", "holm"),
  vars = NA
)
```

## Arguments

- model:

  An object of class `lm`.

- fitted.values:

  Logical; if TRUE, use fitted values of regression model.

- rhs:

  Logical; if TRUE, specifies that tests for heteroskedasticity be
  performed for the right-hand-side (explanatory) variables of the
  fitted regression model.

- multiple:

  Logical; if TRUE, specifies that multiple testing be performed.

- p.adj:

  Adjustment for p value, the following options are available:
  bonferroni, holm, sidak and none.

- vars:

  Variables to be used for heteroskedasticity test.

## Value

`ols_test_breusch_pagan` returns an object of class
`"ols_test_breusch_pagan"`. An object of class
`"ols_test_breusch_pagan"` is a list containing the following
components:

- bp:

  breusch pagan statistic

- p:

  p-value of `bp`

- fv:

  fitted values of the regression model

- rhs:

  names of explanatory variables of fitted regression model

- multiple:

  logical value indicating if multiple tests should be performed

- padj:

  adjusted p values

- vars:

  variables to be used for heteroskedasticity test

- resp:

  response variable

- preds:

  predictors

## Details

Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in
1979. It is used to test for heteroskedasticity in a linear regression
model. It test whether variance of errors from a regression is dependent
on the values of a independent variable.

- Null Hypothesis: Equal/constant variances

- Alternative Hypothesis: Unequal/non-constant variances

Computation

- Fit a regression model

- Regress the squared residuals from the above model on the independent
  variables

- Compute \\nR^2\\. It follows a chi square distribution with p -1
  degrees of freedom, where p is the number of independent variables, n
  is the sample size and \\R^2\\ is the coefficient of determination
  from the regression in step 2.

## References

T.S. Breusch & A.R. Pagan (1979), A Simple Test for Heteroscedasticity
and Random Coefficient Variation. Econometrica 47, 1287–1294

Cook, R. D.; Weisberg, S. (1983). "Diagnostics for Heteroskedasticity in
Regression". Biometrika. 70 (1): 1–10.

## See also

Other heteroskedasticity tests:
[`ols_test_bartlett()`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.md),
[`ols_test_f()`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md),
[`ols_test_score()`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

# use fitted values of the model
ols_test_breusch_pagan(model)
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>              Data               
#>  -------------------------------
#>  Response : mpg 
#>  Variables: fitted values of mpg 
#> 
#>        Test Summary         
#>  ---------------------------
#>  DF            =    1 
#>  Chi2          =    1.429672 
#>  Prob > Chi2   =    0.231818 

# use independent variables of the model
ols_test_breusch_pagan(model, rhs = TRUE)
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>            Data            
#>  --------------------------
#>  Response : mpg 
#>  Variables: disp hp wt drat 
#> 
#>         Test Summary         
#>  ----------------------------
#>  DF            =    4 
#>  Chi2          =    1.513808 
#>  Prob > Chi2   =    0.8241927 

# use independent variables of the model and perform multiple tests
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>            Data            
#>  --------------------------
#>  Response : mpg 
#>  Variables: disp hp wt drat 
#> 
#>         Test Summary (Unadjusted p values)       
#>  ----------------------------------------------
#>   Variable           chi2       df        p     
#>  ----------------------------------------------
#>   disp             1.2355345     1    0.2663334 
#>   hp               0.9209878     1    0.3372157 
#>   wt               1.2529988     1    0.2629805 
#>   drat             1.1668486     1    0.2800497 
#>  ----------------------------------------------
#>   simultaneous     1.5138083     4    0.8241927 
#>  ----------------------------------------------

# bonferroni p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>            Data            
#>  --------------------------
#>  Response : mpg 
#>  Variables: disp hp wt drat 
#> 
#>         Test Summary (Bonferroni p values)       
#>  ----------------------------------------------
#>   Variable           chi2       df        p     
#>  ----------------------------------------------
#>   disp             1.2355345     1    1.0000000 
#>   hp               0.9209878     1    1.0000000 
#>   wt               1.2529988     1    1.0000000 
#>   drat             1.1668486     1    1.0000000 
#>  ----------------------------------------------
#>   simultaneous     1.5138083     4    0.8241927 
#>  ----------------------------------------------

# sidak p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>            Data            
#>  --------------------------
#>  Response : mpg 
#>  Variables: disp hp wt drat 
#> 
#>           Test Summary (Sidak p values)          
#>  ----------------------------------------------
#>   Variable           chi2       df        p     
#>  ----------------------------------------------
#>   disp             1.2355345     1    0.7102690 
#>   hp               0.9209878     1    0.8070305 
#>   wt               1.2529988     1    0.7049362 
#>   drat             1.1668486     1    0.7313356 
#>  ----------------------------------------------
#>   simultaneous     1.5138083     4    0.8241927 
#>  ----------------------------------------------

# holm's p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>            Data            
#>  --------------------------
#>  Response : mpg 
#>  Variables: disp hp wt drat 
#> 
#>           Test Summary (Holm's p values)         
#>  ----------------------------------------------
#>   Variable           chi2       df        p     
#>  ----------------------------------------------
#>   disp             1.2355345     1    0.7990002 
#>   hp               0.9209878     1    0.3372157 
#>   wt               1.2529988     1    1.0000000 
#>   drat             1.1668486     1    0.5600994 
#>  ----------------------------------------------
#>   simultaneous     1.5138083     4    0.8241927 
#>  ----------------------------------------------
```
