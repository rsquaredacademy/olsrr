# Model formula

Returns the model formula.

## Usage

``` r
ols_get_formula(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

An object of class `formula`.

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_get_formula(model)
#> mpg ~ disp + hp + wt + qsec
#> <environment: 0x0000025d84b84d18>
```
