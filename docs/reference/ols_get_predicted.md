# Fitted values

Returns the fitted values.

## Usage

``` r
ols_get_predicted(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Fitted values.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_predicted(model)
#>  [1] 22.88348 21.96087 25.75801 21.00478 17.73969 19.57767 15.32352 23.90864
#>  [9] 23.19927 19.66613 19.66613 14.10328 15.33342 15.15252 11.72118 10.69262
#> [17] 10.44189 26.72100 29.80237 28.06570 24.69218 17.30393 17.69807 14.58127
#> [25] 16.77426 27.68392 27.53687 28.36155 17.80317 20.57403 13.16465 24.00394
```
