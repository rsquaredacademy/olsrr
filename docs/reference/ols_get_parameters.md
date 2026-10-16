# Model parameters

Returns the coefficients.

## Usage

``` r
ols_get_parameters(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

A `data.frame` with model coefficients.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_parameters(model)
#>     Parameter    Estimate
#> 1 (Intercept) 34.72139013
#> 2          wt -3.61807091
#> 3         cyl -1.02844365
#> 4          hp -0.02841085
#> 5        disp  0.01374967
#> 6        gear  0.48659061
#> 7        drat  0.71563702
```
