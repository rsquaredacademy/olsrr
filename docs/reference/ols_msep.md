# MSEP

Estimated error of prediction, assuming multivariate normality.

## Usage

``` r
ols_msep(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Estimated error of prediction of the model.

## Details

Computes the estimated mean square error of prediction assuming that
both independent and dependent variables are multivariate normal.

\$\$MSE(n + 1)(n - 2) / n(n - p - 1)\$\$

where \\MSE = SSE / (n - p)\\, n is the sample size and p is the number
of predictors including the intercept

## References

Stein, C. (1960). “Multiple Regression.” In Contributions to Probability
and Statistics: Essays in Honor of Harold Hotelling, edited by I. Olkin,
S. G. Ghurye, W. Hoeffding, W. G. Madow, and H. B. Mann, 264–305.
Stanford, CA: Stanford University Press.

Darlington, R. B. (1968). “Multiple Regression in Psychological Research
and Practice.” Psychological Bulletin 69:161–182.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_msep(model)
#> [1] 220.8882
```
