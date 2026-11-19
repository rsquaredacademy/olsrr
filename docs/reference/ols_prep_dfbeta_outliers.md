# DFBETAs plot outliers

Data for identifying outliers in dfbetas plot.

## Usage

``` r
ols_prep_dfbeta_outliers(d)
```

## Arguments

- d:

  A `tibble` or `data.frame`.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
dfb <- dfbetas(model)
n <- nrow(dfb)
threshold <- 2 / sqrt(n)
dbetas  <- dfb[, 1]
df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
d <- ols_prep_dfbeta_data(df_data, threshold)
ols_prep_dfbeta_outliers(d)
#>                obs     dbetas
#> Merc 230         9  0.4484991
#> Toyota Corolla  20 -0.3583659
#> Toyota Corona   21  0.3763120
```
