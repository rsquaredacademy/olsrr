# Correlation test for normality

Correlation between observed residuals and expected residuals under
normality.

## Usage

``` r
ols_test_correlation(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Correlation between fitted regression model residuals and expected
values of residuals.

## See also

Other residual diagnostics:
[`ols_plot_resid_box()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_box.md),
[`ols_plot_resid_fit()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_fit.md),
[`ols_plot_resid_hist()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_hist.md),
[`ols_plot_resid_qq()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_qq.md),
[`ols_test_normality()`](https://olsrr.rsquaredacademy.com/reference/ols_test_normality.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_correlation(model)
#> [1] 0.970066
```
