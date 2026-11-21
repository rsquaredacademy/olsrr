# `Hadi plot`

# `Hadi plot`

Source:
[`R/ols-hadi-plot.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-hadi-plot.R)

`ols_plot_hadi.Rd`

Hadi's measure of influence based on the fact that influential
observations can be present in either the response variable or in the
predictors or both. The plot is used to detect influential observations
based on Hadi's measure.

## Usage

``` r
ols_plot_hadi(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## See also

[`ols_plot_resid_pot()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_pot.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_hadi(model)

```
