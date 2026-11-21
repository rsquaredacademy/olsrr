# `Compare model performance`

# `Compare model performance`

Source:
[`R/ols-model-performance.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-model-performance.R)

`ols_compare_model_performance.Rd`

Compare performance of regression models using multiple metrics.

## Usage

``` r
ols_compare_model_performance(...)
```

## Arguments

- ...:

  Multiple objects of class `lm`.

## Examples

``` r
model_1 <- lm(mpg ~ disp + hp, data = mtcars)
model_2 <- lm(mpg ~ disp + hp + wt, data = mtcars)
model_3 <- lm(mpg ~ disp + hp + wt + qsec + + drat, data = mtcars)
ols_compare_model_performance(model_1, model_2, model_3)
#>      name        r2    r2_adj   r2_pred      aic      sbc     sbic     rmse
#> 1 model_1 0.7482402 0.7308774 0.6945438 168.6186 174.4815 78.40578 3.126601
#> 2 model_2 0.8268361 0.8082829 0.7678953 158.6430 165.9717 68.93295 2.638930
#> 3 model_3 0.8489147 0.8198599 0.7666213 158.2784 168.5385 70.12902 2.558013
#>      sigma
#> 1 3.126601
#> 2 2.638930
#> 3 2.558013
```
