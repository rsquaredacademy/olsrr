# Residual fit spread plot data

Data for generating residual fit spread plot.

## Usage

``` r
ols_prep_rfsplot_fmdata(model)

ols_prep_rfsplot_rsdata(model)
```

## Arguments

- model:

  An object of class `lm`.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_prep_rfsplot_fmdata(model)
#>                           x            y
#> Mazda RX4           0.65625   2.49333715
#> Mazda RX4 Wag       0.59375   1.62274065
#> Datsun 710          0.81250   5.22468963
#> Hornet 4 Drive      0.62500   1.63381717
#> Hornet Sportabout   0.40625  -1.66143057
#> Valiant             0.56250   0.93436596
#> Duster 360          0.21875  -4.20935985
#> Merc 240D           0.68750   2.65297895
#> Merc 230            0.75000   3.78369213
#> Merc 280            0.46875  -0.50728418
#> Merc 280C           0.50000  -0.18078799
#> Merc 450SE          0.18750  -4.67624141
#> Merc 450SL          0.34375  -3.00030766
#> Merc 450SLC         0.31250  -3.01309966
#> Cadillac Fleetwood  0.09375  -9.74289440
#> Lincoln Continental 0.06250 -10.85060658
#> Chrysler Imperial   0.03125 -11.03747168
#> Fiat 128            0.87500   6.67162325
#> Honda Civic         1.00000   9.10433522
#> Toyota Corolla      0.96875   8.58634326
#> Toyota Corona       0.84375   5.27579029
#> Dodge Challenger    0.37500  -1.75711947
#> AMC Javelin         0.43750  -1.16868515
#> Camaro Z28          0.15625  -5.71447620
#> Pontiac Firebird    0.28125  -3.40514317
#> Fiat X1-9           0.90625   7.58366929
#> Porsche 914-2       0.78125   5.08511501
#> Lotus Europa        0.93750   7.60601644
#> Ford Pantera L      0.25000  -3.47354135
#> Ferrari Dino        0.53125   0.02627523
#> Maserati Bora       0.12500  -6.72139630
#> Volvo 142E          0.71875   2.83505599
ols_prep_rfsplot_rsdata(model)
#>                           x          y
#> Mazda RX4           0.25000 -1.5839621
#> Mazda RX4 Wag       0.43750 -0.7133657
#> Datsun 710          0.15625 -2.5153146
#> Hornet 4 Drive      0.56250 -0.3244422
#> Hornet Sportabout   0.65625  0.2708056
#> Valiant             0.09375 -2.9249910
#> Duster 360          0.28125 -1.5812652
#> Merc 240D           0.84375  1.6563961
#> Merc 230            0.37500 -1.0743171
#> Merc 280            0.50000 -0.3833408
#> Merc 280C           0.18750 -2.1098370
#> Merc 450SE          0.71875  0.9856164
#> Merc 450SL          0.62500  0.2096827
#> Merc 450SLC         0.21875 -1.8775253
#> Cadillac Fleetwood  0.59375  0.0522694
#> Lincoln Continental 0.75000  1.1599816
#> Chrysler Imperial   1.00000  5.6468467
#> Fiat 128            0.96875  5.6377518
#> Honda Civic         0.78125  1.2050398
#> Toyota Corolla      0.93750  5.2230317
#> Toyota Corona       0.03125 -3.8664153
#> Dodge Challenger    0.12500 -2.8335055
#> AMC Javelin         0.06250 -3.7219399
#> Camaro Z28          0.34375 -1.0761488
#> Pontiac Firebird    0.87500  2.5145182
#> Fiat X1-9           0.53125 -0.3742943
#> Porsche 914-2       0.68750  0.8242600
#> Lotus Europa        0.90625  2.7033586
#> Ford Pantera L      0.40625 -0.8170837
#> Ferrari Dino        0.46875 -0.4169002
#> Maserati Bora       0.81250  1.6307713
#> Volvo 142E          0.31250 -1.5256810
```
