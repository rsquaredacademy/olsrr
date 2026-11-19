# All possible regression variable coefficients

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

## Examples

``` r
if (FALSE) { # \dontrun{
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_step_all_possible_betas(model)
} # }
```
