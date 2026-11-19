# Deviance

Returns the model deviance.

## Usage

``` r
ols_get_deviance(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Model deviance.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_deviance(model)
#> [1] 166.1858
```
