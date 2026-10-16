# Degrees of freedom

Returns residual degrees of freedom.

## Usage

``` r
ols_get_df(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Residual degrees of freedom.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
ols_get_df(model)
#> [1] 20
```
