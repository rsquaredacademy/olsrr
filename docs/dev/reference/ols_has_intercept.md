# `Model intercept check`

# `Model intercept check`

Source:
[`R/ols-model-info.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-model-info.R)

`ols_has_intercept.Rd`

Checks if model has an intercept.

## Usage

``` r
ols_has_intercept(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

`TRUE` if `model` has an intercept, `FALSE` otherwise.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_has_intercept(model)
#> [1] TRUE

model <- lm(mpg ~ 0 + wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_has_intercept(model)
#> [1] FALSE
```
