# Lack of fit F test

Assess how much of the error in prediction is due to lack of model fit.

## Usage

``` r
ols_pure_error_anova(model, ...)
```

## Arguments

- model:

  An object of class `lm`.

- ...:

  Other parameters.

## Value

`ols_pure_error_anova` returns an object of class
`"ols_pure_error_anova"`. An object of class `"ols_pure_error_anova"` is
a list containing the following components:

- lackoffit:

  lack of fit sum of squares

- pure_error:

  pure error sum of squares

- rss:

  regression sum of squares

- ess:

  error sum of squares

- total:

  total sum of squares

- rms:

  regression mean square

- ems:

  error mean square

- lms:

  lack of fit mean square

- pms:

  pure error mean square

- rf:

  f statistic

- lf:

  lack of fit f statistic

- pr:

  p-value of f statistic

- pl:

  p-value pf lack of fit f statistic

- mpred:

  `data.frame` containing data for the response and predictor of the
  `model`

- df_rss:

  regression sum of squares degrees of freedom

- df_ess:

  error sum of squares degrees of freedom

- df_lof:

  lack of fit degrees of freedom

- df_error:

  pure error degrees of freedom

- final:

  data.frame; contains computed values used for the lack of fit f test

- resp:

  character vector; name of `response variable`

- preds:

  character vector; name of `predictor variable`

## Details

The residual sum of squares resulting from a regression can be
decomposed into 2 components:

- Due to lack of fit

- Due to random variation

If most of the error is due to lack of fit and not just random error,
the model should be discarded and a new model must be built.

## Note

The lack of fit F test works only with simple linear regression.
Moreover, it is important that the data contains repeat observations
i.e. replicates for at least one of the values of the predictor x. This
test generally only applies to datasets with plenty of replicates.

## References

Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear
Statistical Models (5th edition). Chicago, IL., McGraw Hill/Irwin.

## Examples

``` r
model <- lm(mpg ~ disp, data = mtcars)
ols_pure_error_anova(model)
#> Lack of Fit F Test 
#> -----------------
#> Response :   mpg 
#> Predictor:   disp 
#> 
#>                       Analysis of Variance Table                       
#> ----------------------------------------------------------------------
#>                 DF     Sum Sq     Mean Sq     F Value        Pr(>F)    
#> ----------------------------------------------------------------------
#> disp             1    808.8885    808.8885    314.0095    1.934413e-17 
#> Residual        30    317.1587    10.57196                             
#>  Lack of fit    25    304.2787    12.17115    4.724824      0.04563623 
#>  Pure Error      5       12.88       2.576                             
#> ----------------------------------------------------------------------
```
