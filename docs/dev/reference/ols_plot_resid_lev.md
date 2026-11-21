# `Studentized residuals vs leverage plot`

# `Studentized residuals vs leverage plot`

Source:
[`R/ols-rstud-vs-lev-plot.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-rstud-vs-lev-plot.R)

`ols_plot_resid_lev.Rd`

Graph for detecting outliers and/or observations with high leverage.

## Usage

``` r
ols_plot_resid_lev(model, threshold = NULL, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- threshold:

  Threshold for detecting outliers. Default is 2.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## See also

[`ols_plot_resid_stud_fit()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_stud_fit.md),
`ols_plot_resid_lev()`

## Examples

``` r
model <- lm(read ~ write + math + science, data = hsb)
ols_plot_resid_lev(model)

ols_plot_resid_lev(model, threshold = 3)

```
