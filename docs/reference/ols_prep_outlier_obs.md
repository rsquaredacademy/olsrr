# Cooks' D outlier observations

Identify outliers in cook's d plot.

## Usage

``` r
ols_prep_outlier_obs(k)
```

## Arguments

- k:

  Cooks' d bar plot data.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
k <- ols_prep_cdplot_data(model)
ols_prep_outlier_obs(k)
#>                     obs           cd   color fct_color txt
#> Mazda RX4             1 1.152035e-02  normal    normal  NA
#> Mazda RX4 Wag         2 4.621112e-03  normal    normal  NA
#> Datsun 710            3 1.598334e-02  normal    normal  NA
#> Hornet 4 Drive        4 1.283888e-04  normal    normal  NA
#> Hornet Sportabout     5 1.839055e-03  normal    normal  NA
#> Valiant               6 1.560119e-02  normal    normal  NA
#> Duster 360            7 1.053270e-02  normal    normal  NA
#> Merc 240D             8 1.313511e-02  normal    normal  NA
#> Merc 230              9 2.525382e-03  normal    normal  NA
#> Merc 280             10 3.671067e-03  normal    normal  NA
#> Merc 280C            11 2.606104e-02  normal    normal  NA
#> Merc 450SE           12 1.551454e-03  normal    normal  NA
#> Merc 450SL           13 1.049983e-04  normal    normal  NA
#> Merc 450SLC          14 5.648180e-03  normal    normal  NA
#> Cadillac Fleetwood   15 7.218880e-05  normal    normal  NA
#> Lincoln Continental  16 1.298764e-02  normal    normal  NA
#> Chrysler Imperial    17 3.199707e-01 outlier   outlier  17
#> Fiat 128             18 1.196019e-01  normal    normal  NA
#> Honda Civic          19 9.092102e-03  normal    normal  NA
#> Toyota Corolla       20 1.529771e-01 outlier   outlier  20
#> Toyota Corona        21 2.215865e-02  normal    normal  NA
#> Dodge Challenger     22 4.218196e-02  normal    normal  NA
#> AMC Javelin          23 4.909944e-02  normal    normal  NA
#> Camaro Z28           24 7.181085e-03  normal    normal  NA
#> Pontiac Firebird     25 6.980693e-02  normal    normal  NA
#> Fiat X1-9            26 4.163138e-04  normal    normal  NA
#> Porsche 914-2        27 1.732523e-06  normal    normal  NA
#> Lotus Europa         28 5.959750e-02  normal    normal  NA
#> Ford Pantera L       29 7.279943e-03  normal    normal  NA
#> Ferrari Dino         30 1.100867e-02  normal    normal  NA
#> Maserati Bora        31 3.402911e-01 outlier   outlier  31
#> Volvo 142E           32 8.796726e-03  normal    normal  NA
```
