# Sigma

Returns the residual standard deviation.

## Usage

``` r
ols_get_sigma(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Residual standard deviation.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_sigma(model)
#> [1] 2.578261
```
