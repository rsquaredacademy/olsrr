# Model object info

Comprehensive information from model objects.

## Usage

``` r
ols_model_info(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

A list with information about model objects.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
ols_model_info(model)
#> $call
#> lm(formula = mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
#> 
#> $degrees_of_freedom
#> [1] 20
#> 
#> $deviance
#> [1] 97.96674
#> 
#> $formula
#> mpg ~ wt + cyl * hp * disp + gear * drat
#> <environment: 0x0000025d8c72fb18>
#> 
#> $has_intercept
#> [1] TRUE
#> 
#> $interaction_terms
#> [1] "cyl:hp"      "cyl:disp"    "hp:disp"     "gear:drat"   "cyl:hp:disp"
#> 
#> $intercept_value
#> [1] 66.41937
#> 
#> $obs
#> [1] 32
#> 
#> $parameters
#>      Parameter      Estimate
#> 1  (Intercept) 66.4193746020
#> 2           wt -2.7886831751
#> 3          cyl -7.4130004777
#> 4           hp -0.4685755622
#> 5         disp -0.2998519166
#> 6         gear  7.6287335428
#> 7         drat  3.4733008926
#> 8       cyl:hp  0.0556005661
#> 9     cyl:disp  0.0396323518
#> 10     hp:disp  0.0025781441
#> 11   gear:drat -1.5128828165
#> 12 cyl:hp:disp -0.0003254985
#> 
#> $sigma
#> [1] 2.213219
#> 
#> $variables
#> $variables$response
#> [1] "mpg"
#> 
#> $variables$predictors
#> [1] "wt"   "cyl"  "hp"   "disp" "gear" "drat"
#> 
#> 
```
