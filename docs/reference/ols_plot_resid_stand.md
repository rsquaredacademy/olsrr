# Standardized residual chart

Chart for identifying outliers.

## Usage

``` r
ols_plot_resid_stand(model, threshold = NULL, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `lm`.

- threshold:

  Threshold for detecting outliers. Default is 2.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`ols_plot_resid_stand` returns a list containing the following
components:

- outliers:

  a `data.frame` with observation number and `standardized resiudals`
  that exceed `threshold`

for classifying an observation as an outlier

- threshold:

  `threshold` for classifying an observation as an outlier

## Details

Standardized residual (internally studentized) is the residual divided
by estimated standard deviation.

## See also

[`ols_plot_resid_stud()`](https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_stud.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_stand(model)

ols_plot_resid_stand(model, threshold = 3)

```
