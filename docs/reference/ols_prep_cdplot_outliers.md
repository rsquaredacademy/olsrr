# Cooks' d outlier data

Outlier data for cook's d bar plot.

## Usage

``` r
ols_prep_cdplot_outliers(k)
```

## Arguments

- k:

  Cooks' d bar plot data.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
k <- ols_prep_cdplot_data(model)
ols_prep_cdplot_outliers(k)
#>                   observation cooks_distance
#> Chrysler Imperial          17      0.3199707
#> Toyota Corolla             20      0.1529771
#> Maserati Bora              31      0.3402911
```
