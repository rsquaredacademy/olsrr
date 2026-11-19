# DFBETAs panel

Panel of plots to detect influential observations using DFBETAs.

## Usage

``` r
ols_plot_dfbetas(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

list; `ols_plot_dfbetas` returns a list of `data.frame` (for intercept
and each predictor) with the observation number and DFBETA of
observations that exceed the threshold for classifying an observation as
an outlier/influential observation.

## Details

DFBETA measures the difference in each parameter estimate with and
without the influential point. There is a DFBETA for each data point i.e
if there are n observations and k variables, there will be \\n \* k\\
DFBETAs. In general, large values of DFBETAS indicate observations that
are influential in estimating a given parameter. Belsley, Kuh, and
Welsch recommend 2 as a general cutoff value to indicate influential
observations and \\2/\sqrt(n)\\ as a size-adjusted cutoff.

## References

Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity.

Wiley Series in Probability and Mathematical Statistics. New York: John
Wiley & Sons. pp. ISBN 0-471-05856-4.

## See also

[`ols_plot_dffits()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_dffits.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_dfbetas(model)


```
