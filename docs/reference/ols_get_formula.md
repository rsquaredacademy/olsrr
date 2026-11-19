# `Model formula`

# `Model formula`

Source:
[`R/ols-model-info.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-model-info.R)

`ols_get_formula.Rd`

Returns the model formula.

## Usage

``` r
ols_get_formula(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

An object of class `formula`.

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_get_formula(model)
#> mpg ~ disp + hp + wt + qsec
#> <environment: 0x000001e2563b0a18>
```
