# `Test for normality`

# `Test for normality`

Source:
[`R/ols-normality-tests.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-normality-tests.R)

`ols_test_normality.Rd`

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

  Lilliefors (Kolmogorov-Smirnov) statistic

- shapiro:

  shapiro wilk statistic

- cramer:

  cramer von mises statistic

- anderson:

  anderson darling statistic

## Details

The tests differ in the sample sizes they support.
[`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) requires
between 3 and 5000 observations,
[`nortest::lillie.test()`](https://rdrr.io/pkg/nortest/man/lillie.test.html)
requires more than 4, and
[`nortest::ad.test()`](https://rdrr.io/pkg/nortest/man/ad.test.html) and
[`nortest::cvm.test()`](https://rdrr.io/pkg/nortest/man/cvm.test.html)
require at least 8. Tests whose sample-size requirement is not met are
skipped: their statistic and p-value are returned as `NA` and a single
warning lists the skipped tests and the reason. If the sample is too
small for every test, the function stops with an error.

The Kolmogorov-Smirnov test is the Lilliefors test,
[`nortest::lillie.test()`](https://rdrr.io/pkg/nortest/man/lillie.test.html).
A plain [`ks.test()`](https://rdrr.io/r/stats/ks.test.html) against a
normal distribution whose mean and standard deviation are estimated from
the same data is conservative: the estimation is not accounted for in
the null distribution, so the p-value is too large. The Lilliefors
correction uses the null distribution of the statistic with estimated
parameters.

The Cramér–von Mises test is
[`nortest::cvm.test()`](https://rdrr.io/pkg/nortest/man/cvm.test.html),
the test of composite normality, which compares the data with a normal
distribution whose mean and standard deviation are estimated from the
data. It is deterministic. For very small p-values the test reports
`7.37e-10` with a warning instead of a more precise value.

## References

Stephens, M. A. (1986). Tests based on EDF statistics. In R. B.
D'Agostino and M. A. Stephens (Eds.), *Goodness-of-Fit Techniques* (pp.
97–193). New York: Marcel Dekker.

Thode, H. C. (2002). *Testing for Normality*. New York: Marcel Dekker.

## See also

Other residual diagnostics:
[`ols_plot_resid_box()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_box.md),
[`ols_plot_resid_fit()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_fit.md),
[`ols_plot_resid_hist()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_hist.md),
[`ols_plot_resid_qq()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_qq.md),
[`ols_test_correlation()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_test_correlation.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_normality(model)
#> ------------------------------------------------------------
#>              Test                    Statistic       pvalue  
#> ------------------------------------------------------------
#> Shapiro-Wilk                           0.9366         0.0600 
#> Kolmogorov-Smirnov (Lilliefors)        0.1152         0.3446 
#> Cramer-von Mises                       0.0764         0.2230 
#> Anderson-Darling                       0.5859         0.1188 
#> ------------------------------------------------------------
```
