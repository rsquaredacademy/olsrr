# Hocking's Sp

Average prediction mean squared error.

## Usage

``` r
ols_hsp(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Hocking's Sp of the model.

## Details

Hocking's Sp criterion is an adjustment of the residual sum of Squares.
Minimize this criterion.

\$\$MSE / (n - p - 1)\$\$

where \\MSE = SSE / (n - p)\\, n is the sample size and p is the number
of predictors including the intercept

## References

Hocking, R. R. (1976). “The Analysis and Selection of Variables in a
Linear Regression.” Biometrics 32:1–50.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_hsp(model)
#> [1] 0.2644378
```
