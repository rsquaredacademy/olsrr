# `Simple linear regression line`

# `Simple linear regression line`

Source:
[`R/ols-fitted-line-properties.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-fitted-line-properties.R)

`ols_plot_reg_line.Rd`

Plot to demonstrate that the regression line always passes through mean
of the response and predictor variables.

## Usage

``` r
ols_plot_reg_line(response, predictor, print_plot = TRUE)
```

## Arguments

- response:

  Response variable.

- predictor:

  Predictor variable.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
ols_plot_reg_line(mtcars$mpg, mtcars$disp)
#> `geom_smooth()` using formula = 'y ~ x'

```
