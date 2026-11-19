# `Observations`

# `Observations`

Source:
[`R/ols-model-info.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-model-info.R)

`ols_count_obs.Rd`

Returns the number of observation from a model.

## Usage

``` r
ols_count_obs(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Number of observations.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_count_obs(model)
#> [1] 32
```
