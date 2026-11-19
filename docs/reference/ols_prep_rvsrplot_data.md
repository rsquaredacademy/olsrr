# Residual vs regressor plot data

Data for generating residual vs regressor plot.

## Usage

``` r
ols_prep_rvsrplot_data(model)
```

## Arguments

- model:

  An object of class `lm`.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_prep_rvsrplot_data(model)
#> $np
#> [1] 4
#> 
#> $dat
#>                      disp  hp    wt  qsec
#> Mazda RX4           160.0 110 2.620 16.46
#> Mazda RX4 Wag       160.0 110 2.875 17.02
#> Datsun 710          108.0  93 2.320 18.61
#> Hornet 4 Drive      258.0 110 3.215 19.44
#> Hornet Sportabout   360.0 175 3.440 17.02
#> Valiant             225.0 105 3.460 20.22
#> Duster 360          360.0 245 3.570 15.84
#> Merc 240D           146.7  62 3.190 20.00
#> Merc 230            140.8  95 3.150 22.90
#> Merc 280            167.6 123 3.440 18.30
#> Merc 280C           167.6 123 3.440 18.90
#> Merc 450SE          275.8 180 4.070 17.40
#> Merc 450SL          275.8 180 3.730 17.60
#> Merc 450SLC         275.8 180 3.780 18.00
#> Cadillac Fleetwood  472.0 205 5.250 17.98
#> Lincoln Continental 460.0 215 5.424 17.82
#> Chrysler Imperial   440.0 230 5.345 17.42
#> Fiat 128             78.7  66 2.200 19.47
#> Honda Civic          75.7  52 1.615 18.52
#> Toyota Corolla       71.1  65 1.835 19.90
#> Toyota Corona       120.1  97 2.465 20.01
#> Dodge Challenger    318.0 150 3.520 16.87
#> AMC Javelin         304.0 150 3.435 17.30
#> Camaro Z28          350.0 245 3.840 15.41
#> Pontiac Firebird    400.0 175 3.845 17.05
#> Fiat X1-9            79.0  66 1.935 18.90
#> Porsche 914-2       120.3  91 2.140 16.70
#> Lotus Europa         95.1 113 1.513 16.90
#> Ford Pantera L      351.0 264 3.170 14.50
#> Ferrari Dino        145.0 175 2.770 15.50
#> Maserati Bora       301.0 335 3.570 14.60
#> Volvo 142E          121.0 109 2.780 18.60
#> 
#> $pnames
#> [1] "disp" "hp"   "wt"   "qsec"
#> 
```
