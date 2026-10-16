# Interaction terms

Returns interaction terms present in the model.

## Usage

``` r
ols_get_interaction_terms(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

`Character` vector or `NULL`.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
ols_get_interaction_terms(model)
#> [1] "cyl:hp"      "cyl:disp"    "hp:disp"     "gear:drat"   "cyl:hp:disp"
```
