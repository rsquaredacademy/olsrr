# Leverage

The leverage of an observation is based on how much the observation's
value on the predictor variable differs from the mean of the predictor
variable. The greater an observation's leverage, the more potential it
has to be an influential observation.

## Usage

``` r
ols_leverage(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Leverage of the model.

## References

Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear
Statistical Models (5th edition). Chicago, IL., McGraw Hill/Irwin.

## See also

Other influence measures:
[`ols_hadi()`](https://olsrr.rsquaredacademy.com/reference/ols_hadi.md),
[`ols_pred_rsq()`](https://olsrr.rsquaredacademy.com/reference/ols_pred_rsq.md),
[`ols_press()`](https://olsrr.rsquaredacademy.com/reference/ols_press.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_leverage(model)
#>  [1] 0.14830256 0.12992351 0.06317600 0.11598460 0.17187869 0.09986745
#>  [7] 0.14720085 0.12912656 0.45839085 0.13597113 0.11541159 0.10343044
#> [13] 0.04684167 0.05527722 0.20578392 0.21066328 0.19546860 0.08593044
#> [19] 0.14685385 0.14389010 0.11957800 0.11051613 0.08002229 0.10635592
#> [25] 0.19566514 0.09324142 0.15093650 0.16467565 0.23978580 0.21863752
#> [31] 0.53168134 0.07953098
```
