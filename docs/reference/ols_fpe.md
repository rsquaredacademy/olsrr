# Final prediction error

Estimated mean square error of prediction.

## Usage

``` r
ols_fpe(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Final prediction error of the model.

## Details

Computes the estimated mean square error of prediction for each model
selected assuming that the values of the regressors are fixed and that
the model is correct.

\$\$MSE((n + p) / n)\$\$

where \\MSE = SSE / (n - p)\\, n is the sample size and p is the number
of predictors including the intercept

## References

Akaike, H. (1969). “Fitting Autoregressive Models for Prediction.”
Annals of the Institute of Statistical Mathematics 21:243–247.

Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The
Theory and Practice of Econometrics. New York: John Wiley & Sons.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_fpe(model)
#> [1] 7.949661
```
