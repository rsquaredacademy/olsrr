# Diagnostics panel

Panel of plots for regression diagnostics.

## Usage

``` r
ols_plot_diagnostics(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_diagnostics(model)













```
