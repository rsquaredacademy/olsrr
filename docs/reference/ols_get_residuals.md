# Model residuals

Returns the residuals from the model.

## Usage

``` r
ols_get_residuals(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Residuals from the regression model.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_residuals(model)
#>  [1] -1.88348251 -0.96087442 -2.95801095  0.39521516  0.96031301 -1.47766882
#>  [7] -1.02351638  0.49136410 -0.39927411 -0.46613351 -1.86613351  2.29672498
#> [13]  1.96658087  0.04748441 -1.32117579 -0.29262151  4.25811051  5.67899631
#> [19]  0.59763045  5.83429787 -3.19218209 -1.80392810 -2.49806721 -1.28127180
#> [25]  2.42573959 -0.38391738 -1.53686642  2.03845392 -2.00316616 -0.87403374
#> [31]  1.83534925 -2.60393602
```
