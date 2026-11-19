# Parameters count

Returns the number of parameters from a model.

## Usage

``` r
ols_count_parameters(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Number of parameters.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_count_parameters(model)
#> [1] 7
```
