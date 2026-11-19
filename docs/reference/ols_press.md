# PRESS

PRESS (prediction sum of squares) tells you how well the model will
predict new data.

## Usage

``` r
ols_press(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Predicted sum of squares of the model.

## Details

The prediction sum of squares (PRESS) is the sum of squares of the
prediction error. Each fitted to obtain the predicted value for the ith
observation. Use PRESS to assess your model's predictive ability.
Usually, the smaller the PRESS value, the better the model's predictive
ability.

## References

Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear
Statistical Models (5th edition). Chicago, IL., McGraw Hill/Irwin.

## See also

Other influence measures:
[`ols_hadi()`](https://olsrr.rsquaredacademy.com/reference/ols_hadi.md),
[`ols_leverage()`](https://olsrr.rsquaredacademy.com/reference/ols_leverage.md),
[`ols_pred_rsq()`](https://olsrr.rsquaredacademy.com/reference/ols_pred_rsq.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_press(model)
#> [1] 257.8314
```
