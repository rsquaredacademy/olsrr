# Bonferroni Outlier Test

Detect outliers using Bonferroni p values.

## Usage

``` r
ols_test_outlier(model, cut_off = 0.05, n_max = 10, ...)
```

## Arguments

- model:

  An object of class `lm`.

- cut_off:

  Bonferroni p-values cut off for reporting observations.

- n_max:

  Maximum number of observations to report, default is 10.

- ...:

  Other arguments.

## Examples

``` r
# model
model <- lm(y ~ ., data = surgical)
ols_test_outlier(model)
#>   studentized_residual unadjusted_p_val bonferroni_p_val
#> 5             5.713932     8.870345e-07     4.789987e-05
```
