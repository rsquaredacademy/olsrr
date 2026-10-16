# Regress predictor on other predictors

Regress a predictor in the model on all the other predictors.

## Usage

``` r
ols_prep_regress_x(data, i)
```

## Arguments

- data:

  A `data.frame`.

- i:

  A numeric vector (indicates the predictor in the model).

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
data <- ols_prep_avplot_data(model)
ols_prep_regress_x(data, 1)
#>  [1] -2.5700299 -1.6008028 -2.4886829  0.1833269  0.4592780 -2.3721590
#>  [7] -1.2656477  1.4885011  0.7591033 -0.8411432 -2.2411432  0.6307257
#> [13]  0.2384229 -1.6715326  0.0785315  1.0402079  5.4885456  5.7865290
#> [19]  1.1240053  5.8609261 -3.1015898 -3.2549189 -3.8911127 -1.2487773
#> [25]  2.5361190 -0.3204259 -0.0236311  2.6550420 -0.7024625 -1.2887756
#> [31]  2.1831584 -1.6295873
```
