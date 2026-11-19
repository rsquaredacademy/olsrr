# Potential residual plot

Plot to aid in classifying unusual observations as high-leverage points,
outliers, or a combination of both.

## Usage

``` r
ols_plot_resid_pot(model, print_plot = TRUE)
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

[`ols_plot_hadi()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_hadi.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_pot(model)

```
