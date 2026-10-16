# Residual vs regressor plot

Graph to determine whether we should add a new predictor to the model
already containing other predictors. The residuals from the model is
regressed on the new predictor and if the plot shows non random pattern,
you should consider adding the new predictor to the model.

## Usage

``` r
ols_plot_resid_regressor(model, variable, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- variable:

  New predictor to be added to the `model`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## See also

[`ols_plot_added_variable()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_added_variable.md),
[`ols_plot_comp_plus_resid()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_comp_plus_resid.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_regressor(model, 'drat')


```
