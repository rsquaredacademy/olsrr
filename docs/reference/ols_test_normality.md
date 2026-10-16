# Test for normality

Test for detecting violation of normality assumption.

## Usage

``` r
ols_test_normality(y, ...)

# S3 method for class 'lm'
ols_test_normality(y, ...)
```

## Arguments

- y:

  A numeric vector or an object of class `lm`.

- ...:

  Other arguments.

## Value

`ols_test_normality` returns an object of class `"ols_test_normality"`.
An object of class `"ols_test_normality"` is a list containing the
following components:

- kolmogorv:

  kolmogorv smirnov statistic

- shapiro:

  shapiro wilk statistic

- cramer:

  cramer von mises statistic

- anderson:

  anderson darling statistic

## See also

Other residual diagnostics:
[`ols_plot_resid_box()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_box.md),
[`ols_plot_resid_fit()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_fit.md),
[`ols_plot_resid_hist()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_hist.md),
[`ols_plot_resid_qq()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_qq.md),
[`ols_test_correlation()`](https://olsrr.rsquaredacademy.com/reference/ols_test_correlation.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_normality(model)
#> -----------------------------------------------
#>        Test             Statistic       pvalue  
#> -----------------------------------------------
#> Shapiro-Wilk              0.9366         0.0600 
#> Kolmogorov-Smirnov        0.1152         0.7464 
#> Cramer-von Mises          2.8122         0.0000 
#> Anderson-Darling          0.5859         0.1188 
#> -----------------------------------------------
```
