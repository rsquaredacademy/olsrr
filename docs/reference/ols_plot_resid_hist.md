# Residual histogram

Histogram of residuals for detecting violation of normality assumption.

## Usage

``` r
ols_plot_resid_hist(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## See also

Other residual diagnostics:
[`ols_plot_resid_box()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_box.md),
[`ols_plot_resid_fit()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_fit.md),
[`ols_plot_resid_qq()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_qq.md),
[`ols_test_correlation()`](https://olsrr.rsquaredacademy.com/reference/ols_test_correlation.md),
[`ols_test_normality()`](https://olsrr.rsquaredacademy.com/reference/ols_test_normality.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_hist(model)

```
