# `All possible regression variable coefficients`

# `All possible regression variable coefficients`

Source:
[`R/ols-all-possible-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-all-possible-regression.R)

`ols_step_all_possible_betas.Rd`

Returns the coefficients for each variable from each model.

## Usage

``` r
ols_step_all_possible_betas(object, ...)
```

## Arguments

- object:

  An object of class `lm`.

- ...:

  Other arguments.

## Value

`ols_step_all_possible_betas` returns a `data.frame` containing:

- model_index:

  model number

- predictor:

  predictor

- beta_coef:

  coefficient for the predictor

## Details

The table is built by refitting every subset of the model terms, so it
is not available for weighted models (the subset fits would silently
drop the weights) or for models without an intercept. Both cases stop
with an informative error. Custom named contrast functions must be
supplied as explicit contrast matrices when fitting the input model;
base contrast names are resolved from the stats namespace before
refitting.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_step_all_possible_betas(model)
#>    model   predictor          beta
#> 1      1 (Intercept) 29.5998547562
#> 2      1        disp -0.0412151200
#> 3      2 (Intercept) 30.0988605396
#> 4      2          hp -0.0682282781
#> 5      3 (Intercept) 37.2851261673
#> 6      3          wt -5.3444715727
#> 7      4 (Intercept) 30.7359042483
#> 8      4        disp -0.0303462766
#> 9      4          hp -0.0248400812
#> 10     5 (Intercept) 34.9605540362
#> 11     5        disp -0.0177247443
#> 12     5          wt -3.3508253333
#> 13     6 (Intercept) 37.2272701164
#> 14     6          hp -0.0317729470
#> 15     6          wt -3.8778307424
#> 16     7 (Intercept) 37.1055052690
#> 17     7        disp -0.0009370091
#> 18     7          hp -0.0311565508
#> 19     7          wt -3.8008905826
```
