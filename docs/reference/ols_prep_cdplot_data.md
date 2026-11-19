# Cooks' D plot data

Prepare data for cook's d bar plot.

## Usage

``` r
ols_prep_cdplot_data(model, type = 1)
```

## Arguments

- model:

  An object of class `lm`.

- type:

  An integer between 1 and 5 selecting one of the 6 methods for
  computing the threshold.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_prep_cdplot_data(model)
#> $ckd
#>                     obs           cd   color fct_color
#> Mazda RX4             1 1.152035e-02  normal    normal
#> Mazda RX4 Wag         2 4.621112e-03  normal    normal
#> Datsun 710            3 1.598334e-02  normal    normal
#> Hornet 4 Drive        4 1.283888e-04  normal    normal
#> Hornet Sportabout     5 1.839055e-03  normal    normal
#> Valiant               6 1.560119e-02  normal    normal
#> Duster 360            7 1.053270e-02  normal    normal
#> Merc 240D             8 1.313511e-02  normal    normal
#> Merc 230              9 2.525382e-03  normal    normal
#> Merc 280             10 3.671067e-03  normal    normal
#> Merc 280C            11 2.606104e-02  normal    normal
#> Merc 450SE           12 1.551454e-03  normal    normal
#> Merc 450SL           13 1.049983e-04  normal    normal
#> Merc 450SLC          14 5.648180e-03  normal    normal
#> Cadillac Fleetwood   15 7.218880e-05  normal    normal
#> Lincoln Continental  16 1.298764e-02  normal    normal
#> Chrysler Imperial    17 3.199707e-01 outlier   outlier
#> Fiat 128             18 1.196019e-01  normal    normal
#> Honda Civic          19 9.092102e-03  normal    normal
#> Toyota Corolla       20 1.529771e-01 outlier   outlier
#> Toyota Corona        21 2.215865e-02  normal    normal
#> Dodge Challenger     22 4.218196e-02  normal    normal
#> AMC Javelin          23 4.909944e-02  normal    normal
#> Camaro Z28           24 7.181085e-03  normal    normal
#> Pontiac Firebird     25 6.980693e-02  normal    normal
#> Fiat X1-9            26 4.163138e-04  normal    normal
#> Porsche 914-2        27 1.732523e-06  normal    normal
#> Lotus Europa         28 5.959750e-02  normal    normal
#> Ford Pantera L       29 7.279943e-03  normal    normal
#> Ferrari Dino         30 1.100867e-02  normal    normal
#> Maserati Bora        31 3.402911e-01 outlier   outlier
#> Volvo 142E           32 8.796726e-03  normal    normal
#> 
#> $maxx
#> [1] 0.3436941
#> 
#> $ts
#> [1] 0.125
#> 
```
