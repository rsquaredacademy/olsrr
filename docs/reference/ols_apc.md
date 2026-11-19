# Amemiya's prediction criterion

Amemiya's prediction error.

## Usage

``` r
ols_apc(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Amemiya's prediction error of the model.

## Details

Amemiya's Prediction Criterion penalizes R-squared more heavily than
does adjusted R-squared for each addition degree of freedom used on the
right-hand-side of the equation. The lower the better for this
criterion.

\$\$((n + p) / (n - p))(1 - (R^2))\$\$

where *n* is the sample size, *p* is the number of predictors including
the intercept and *R^2* is the coefficient of determination.

## References

Amemiya, T. (1976). Selection of Regressors. Technical Report 225,
Stanford University, Stanford, CA.

Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The
Theory and Practice of Econometrics. New York: John Wiley & Sons.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_apc(model)
#> [1] 0.2259134
```
