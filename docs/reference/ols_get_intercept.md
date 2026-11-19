# Model intercept

Returns the value of the model intercept.

## Usage

``` r
ols_get_intercept(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

The value of the intercept.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_intercept(model)
#> [1] 34.72139

model <- lm(mpg ~ 0 + wt + cyl + hp + disp, data = mtcars)
ols_get_intercept(model)
#> NULL
```
