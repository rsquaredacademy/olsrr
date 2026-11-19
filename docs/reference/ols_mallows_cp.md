# Mallow's Cp

Mallow's Cp.

## Usage

``` r
ols_mallows_cp(model, fullmodel)
```

## Arguments

- model:

  An object of class `lm`.

- fullmodel:

  An object of class `lm`.

## Value

Mallow's Cp of the model.

## Details

Mallows' Cp statistic estimates the size of the bias that is introduced
into the predicted responses by having an underspecified model. Use
Mallows' Cp to choose between multiple regression models. Look for
models where Mallows' Cp is small and close to the number of predictors
in the model plus the constant (p).

## References

Hocking, R. R. (1976). “The Analysis and Selection of Variables in a
Linear Regression.” Biometrics 32:1–50.

Mallows, C. L. (1973). “Some Comments on Cp.” Technometrics 15:661–675.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
full_model <- lm(mpg ~ ., data = mtcars)
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_mallows_cp(model, full_model)
#> [1] 4.430434
```
