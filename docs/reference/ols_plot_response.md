# Response variable profile

Panel of plots to explore and visualize the response variable.

## Usage

``` r
ols_plot_response(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_response(model)

```
