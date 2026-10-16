# Predictor variable data

Returns the data of all model predictors.

## Usage

``` r
ols_get_predictors(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

A `data.frame` with model predictors.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_predictors(model)
#>                        wt cyl  hp  disp gear drat
#> Mazda RX4           2.620   6 110 160.0    4 3.90
#> Mazda RX4 Wag       2.875   6 110 160.0    4 3.90
#> Datsun 710          2.320   4  93 108.0    4 3.85
#> Hornet 4 Drive      3.215   6 110 258.0    3 3.08
#> Hornet Sportabout   3.440   8 175 360.0    3 3.15
#> Valiant             3.460   6 105 225.0    3 2.76
#> Duster 360          3.570   8 245 360.0    3 3.21
#> Merc 240D           3.190   4  62 146.7    4 3.69
#> Merc 230            3.150   4  95 140.8    4 3.92
#> Merc 280            3.440   6 123 167.6    4 3.92
#> Merc 280C           3.440   6 123 167.6    4 3.92
#> Merc 450SE          4.070   8 180 275.8    3 3.07
#> Merc 450SL          3.730   8 180 275.8    3 3.07
#> Merc 450SLC         3.780   8 180 275.8    3 3.07
#> Cadillac Fleetwood  5.250   8 205 472.0    3 2.93
#> Lincoln Continental 5.424   8 215 460.0    3 3.00
#> Chrysler Imperial   5.345   8 230 440.0    3 3.23
#> Fiat 128            2.200   4  66  78.7    4 4.08
#> Honda Civic         1.615   4  52  75.7    4 4.93
#> Toyota Corolla      1.835   4  65  71.1    4 4.22
#> Toyota Corona       2.465   4  97 120.1    3 3.70
#> Dodge Challenger    3.520   8 150 318.0    3 2.76
#> AMC Javelin         3.435   8 150 304.0    3 3.15
#> Camaro Z28          3.840   8 245 350.0    3 3.73
#> Pontiac Firebird    3.845   8 175 400.0    3 3.08
#> Fiat X1-9           1.935   4  66  79.0    4 4.08
#> Porsche 914-2       2.140   4  91 120.3    5 4.43
#> Lotus Europa        1.513   4 113  95.1    5 3.77
#> Ford Pantera L      3.170   8 264 351.0    5 4.22
#> Ferrari Dino        2.770   6 175 145.0    5 3.62
#> Maserati Bora       3.570   8 335 301.0    5 3.54
#> Volvo 142E          2.780   4 109 121.0    4 4.11
```
