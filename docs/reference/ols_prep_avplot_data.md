# Added variable plot data

Data for generating the added variable plots.

## Usage

``` r
ols_prep_avplot_data(model)
```

## Arguments

- model:

  An object of class `lm`.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_prep_avplot_data(model)
#>                      mpg  disp  hp    wt
#> Mazda RX4           21.0 160.0 110 2.620
#> Mazda RX4 Wag       21.0 160.0 110 2.875
#> Datsun 710          22.8 108.0  93 2.320
#> Hornet 4 Drive      21.4 258.0 110 3.215
#> Hornet Sportabout   18.7 360.0 175 3.440
#> Valiant             18.1 225.0 105 3.460
#> Duster 360          14.3 360.0 245 3.570
#> Merc 240D           24.4 146.7  62 3.190
#> Merc 230            22.8 140.8  95 3.150
#> Merc 280            19.2 167.6 123 3.440
#> Merc 280C           17.8 167.6 123 3.440
#> Merc 450SE          16.4 275.8 180 4.070
#> Merc 450SL          17.3 275.8 180 3.730
#> Merc 450SLC         15.2 275.8 180 3.780
#> Cadillac Fleetwood  10.4 472.0 205 5.250
#> Lincoln Continental 10.4 460.0 215 5.424
#> Chrysler Imperial   14.7 440.0 230 5.345
#> Fiat 128            32.4  78.7  66 2.200
#> Honda Civic         30.4  75.7  52 1.615
#> Toyota Corolla      33.9  71.1  65 1.835
#> Toyota Corona       21.5 120.1  97 2.465
#> Dodge Challenger    15.5 318.0 150 3.520
#> AMC Javelin         15.2 304.0 150 3.435
#> Camaro Z28          13.3 350.0 245 3.840
#> Pontiac Firebird    19.2 400.0 175 3.845
#> Fiat X1-9           27.3  79.0  66 1.935
#> Porsche 914-2       26.0 120.3  91 2.140
#> Lotus Europa        30.4  95.1 113 1.513
#> Ford Pantera L      15.8 351.0 264 3.170
#> Ferrari Dino        19.7 145.0 175 2.770
#> Maserati Bora       15.0 301.0 335 3.570
#> Volvo 142E          21.4 121.0 109 2.780
```
