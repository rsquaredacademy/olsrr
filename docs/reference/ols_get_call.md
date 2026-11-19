# `Model function call`

# `Model function call`

Source:
[`R/ols-model-info.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-model-info.R)

`ols_get_call.Rd`

Returns the model's function call.

## Usage

``` r
ols_get_call(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Model's function call.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_call(model)
#> lm(formula = mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
```
