# Model variables

Return model variables.

## Usage

``` r
ols_get_variables(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

List with response and predictors.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
ols_get_variables(model)
#> $response
#> [1] "mpg"
#> 
#> $predictors
#> [1] "wt"   "cyl"  "hp"   "disp" "gear" "drat"
#> 
```
