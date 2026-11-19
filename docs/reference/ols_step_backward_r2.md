# Stepwise R-Squared backward regression

Build regression model from a set of candidate predictor variables by
removing predictors based on r-squared, in a stepwise manner until there
is no variable left to remove any more.

## Usage

``` r
ols_step_backward_r2(model, ...)

# Default S3 method
ols_step_backward_r2(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_backward_r2'
plot(x, print_plot = TRUE, details = TRUE, digits = 3, ...)
```

## Arguments

- model:

  An object of class `lm`; the model should include all candidate
  predictor variables.

- ...:

  Other arguments.

- include:

  Character or numeric vector; variables to be included in selection
  process.

- exclude:

  Character or numeric vector; variables to be excluded from selection
  process.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- steps:

  Number of steps after which the stepwise procedures should stop.

- x:

  An object of class `ols_step_backward_*`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- digits:

  Number of decimal places to display.

## Value

List containing the following components:

- model:

  final model; an object of class `lm`

- metrics:

  selection metrics

- others:

  list; info used for plotting and printing

## References

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with
S. Fourth edition. Springer.

## See also

Other backward selection procedures:
[`ols_step_backward_adj_r2()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_adj_r2.md),
[`ols_step_backward_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_aic.md),
[`ols_step_backward_p()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_p.md),
[`ols_step_backward_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbc.md),
[`ols_step_backward_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbic.md)

## Examples

``` r
# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_r2(model)
#> [1] "No variables have been removed from the model."

# final model and selection metrics
k <- ols_step_backward_aic(model)
k$metrics
#>   step variable        r2    adj_r2      aic      sbc     sbic
#> 1    1  alc_mod 0.7817703 0.7485615 734.4068 752.3077 583.8836
#> 2    2   gender 0.7814169 0.7535127 732.4942 748.4061 581.2896
#> 3    3      age 0.7809054 0.7580831 730.6204 744.5433 578.8438
k$model
#> 
#> Call:
#> lm(formula = paste(response, "~", paste(preds, collapse = " + ")), 
#>     data = l)
#> 
#> Coefficients:
#> (Intercept)          bcs       pindex  enzyme_test   liver_test    alc_heavy  
#>   -1178.330       59.864        8.924        9.748       58.064      317.848  
#> 

# include or exclude variable
# force variables to be included in the selection process
ols_step_backward_r2(model, include = c("alc_mod", "gender"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_r2(model, include = c(7, 6))
#> [1] "No variables have been removed from the model."

# force variable to be excluded from selection process
ols_step_backward_r2(model, exclude = c("alc_heavy", "bcs"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_r2(model, exclude = c(8, 1))
#> [1] "No variables have been removed from the model."

# steps
ols_step_backward_r2(model, steps = 2)
#> [1] "No variables have been removed from the model."
```
