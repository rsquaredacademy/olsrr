# Residual box plot

Box plot of residuals to examine if residuals are normally distributed.

## Usage

``` r
ols_plot_resid_box(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## See also

Other residual diagnostics:
[`ols_plot_resid_fit()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_fit.md),
[`ols_plot_resid_hist()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_hist.md),
[`ols_plot_resid_qq()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_qq.md),
[`ols_test_correlation()`](https://olsrr.rsquaredacademy.com/reference/ols_test_correlation.md),
[`ols_test_normality()`](https://olsrr.rsquaredacademy.com/reference/ols_test_normality.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_box(model)

```
