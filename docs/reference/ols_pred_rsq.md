# Predicted rsquare

Use predicted rsquared to determine how well the model predicts
responses for new observations. Larger values of predicted R2 indicate
models of greater predictive ability.

## Usage

``` r
ols_pred_rsq(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Predicted rsquare of the model.

## See also

Other influence measures:
[`ols_hadi()`](https://olsrr.rsquaredacademy.com/reference/ols_hadi.md),
[`ols_leverage()`](https://olsrr.rsquaredacademy.com/reference/ols_leverage.md),
[`ols_press()`](https://olsrr.rsquaredacademy.com/reference/ols_press.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_pred_rsq(model)
#> [1] 0.7710297
```
