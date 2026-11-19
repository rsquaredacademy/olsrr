# Response variable data

Returns the values of the response variable of a model.

## Usage

``` r
ols_get_response(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Values of the response variable.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_response(model)
#>  [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4
#> [16] 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7
#> [31] 15.0 21.4
```
