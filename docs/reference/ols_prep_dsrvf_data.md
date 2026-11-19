# Deleted studentized residual plot data

Generates data for deleted studentized residual vs fitted plot.

## Usage

``` r
ols_prep_dsrvf_data(model, threshold = NULL)
```

## Arguments

- model:

  An object of class `lm`.

- threshold:

  Threshold for detecting outliers. Default is 2.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_prep_dsrvf_data(model)
#> $ds
#>                     obs      pred         dsr   color fct_color
#> Mazda RX4             1 22.583962 -0.64748839  normal    normal
#> Mazda RX4 Wag         2 21.713366 -0.28666527  normal    normal
#> Datsun 710            3 25.315315 -0.99075628  normal    normal
#> Hornet 4 Drive        4 21.724442 -0.12918232  normal    normal
#> Hornet Sportabout     5 18.429194  0.11139625  normal    normal
#> Valiant               6 21.024991 -1.18451521  normal    normal
#> Duster 360            7 15.881265 -0.64594383  normal    normal
#> Merc 240D             8 22.743604  0.66997702  normal    normal
#> Merc 230              9 23.874317 -0.54948140  normal    normal
#> Merc 280             10 19.583341 -0.15441033  normal    normal
#> Merc 280C            11 19.909837 -0.85114256  normal    normal
#> Merc 450SE           12 15.414384  0.39070003  normal    normal
#> Merc 450SL           13 17.090317  0.08038783  normal    normal
#> Merc 450SLC          14 17.077525 -0.73029648  normal    normal
#> Cadillac Fleetwood   15 10.347731  0.02195020  normal    normal
#> Lincoln Continental  16  9.240018  0.49088476  normal    normal
#> Chrysler Imperial    17  9.053153  2.65670246 outlier   outlier
#> Fiat 128             18 26.762248  2.44800052 outlier   outlier
#> Honda Civic          19 29.194960  0.49050814  normal    normal
#> Toyota Corolla       20 28.676968  2.32117834 outlier   outlier
#> Toyota Corona        21 25.366415 -1.61789048  normal    normal
#> Dodge Challenger     22 18.333506 -1.15275035  normal    normal
#> AMC Javelin          23 18.921940 -1.51497848  normal    normal
#> Camaro Z28           24 14.376149 -0.42753101  normal    normal
#> Pontiac Firebird     25 16.685482  1.07223104  normal    normal
#> Fiat X1-9            26 27.674294 -0.14716500  normal    normal
#> Porsche 914-2        27 25.175740  0.33549692  normal    normal
#> Lotus Europa         28 27.696641  1.13400631  normal    normal
#> Ford Pantera L       29 16.617084 -0.35154799  normal    normal
#> Ferrari Dino         30 20.116900 -0.17661296  normal    normal
#> Maserati Bora        31 13.369229  0.90578477  normal    normal
#> Volvo 142E           32 22.925681 -0.59923054  normal    normal
#> 
#> $cminx
#> [1] -2.61789
#> 
#> $cmaxx
#> [1] 3.656702
#> 
ols_prep_dsrvf_data(model, threshold = 3)
#> $ds
#>                     obs      pred         dsr  color fct_color
#> Mazda RX4             1 22.583962 -0.64748839 normal    normal
#> Mazda RX4 Wag         2 21.713366 -0.28666527 normal    normal
#> Datsun 710            3 25.315315 -0.99075628 normal    normal
#> Hornet 4 Drive        4 21.724442 -0.12918232 normal    normal
#> Hornet Sportabout     5 18.429194  0.11139625 normal    normal
#> Valiant               6 21.024991 -1.18451521 normal    normal
#> Duster 360            7 15.881265 -0.64594383 normal    normal
#> Merc 240D             8 22.743604  0.66997702 normal    normal
#> Merc 230              9 23.874317 -0.54948140 normal    normal
#> Merc 280             10 19.583341 -0.15441033 normal    normal
#> Merc 280C            11 19.909837 -0.85114256 normal    normal
#> Merc 450SE           12 15.414384  0.39070003 normal    normal
#> Merc 450SL           13 17.090317  0.08038783 normal    normal
#> Merc 450SLC          14 17.077525 -0.73029648 normal    normal
#> Cadillac Fleetwood   15 10.347731  0.02195020 normal    normal
#> Lincoln Continental  16  9.240018  0.49088476 normal    normal
#> Chrysler Imperial    17  9.053153  2.65670246 normal    normal
#> Fiat 128             18 26.762248  2.44800052 normal    normal
#> Honda Civic          19 29.194960  0.49050814 normal    normal
#> Toyota Corolla       20 28.676968  2.32117834 normal    normal
#> Toyota Corona        21 25.366415 -1.61789048 normal    normal
#> Dodge Challenger     22 18.333506 -1.15275035 normal    normal
#> AMC Javelin          23 18.921940 -1.51497848 normal    normal
#> Camaro Z28           24 14.376149 -0.42753101 normal    normal
#> Pontiac Firebird     25 16.685482  1.07223104 normal    normal
#> Fiat X1-9            26 27.674294 -0.14716500 normal    normal
#> Porsche 914-2        27 25.175740  0.33549692 normal    normal
#> Lotus Europa         28 27.696641  1.13400631 normal    normal
#> Ford Pantera L       29 16.617084 -0.35154799 normal    normal
#> Ferrari Dino         30 20.116900 -0.17661296 normal    normal
#> Maserati Bora        31 13.369229  0.90578477 normal    normal
#> Volvo 142E           32 22.925681 -0.59923054 normal    normal
#> 
#> $cminx
#> [1] -3.5
#> 
#> $cmaxx
#> [1] 3.656702
#> 
```
