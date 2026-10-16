# Hadi's influence measure

Measure of influence based on the fact that influential observations in
either the response variable or in the predictors or both.

## Usage

``` r
ols_hadi(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Hadi's measure of the model.

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## See also

Other influence measures:
[`ols_leverage()`](https://olsrr.rsquaredacademy.com/reference/ols_leverage.md),
[`ols_pred_rsq()`](https://olsrr.rsquaredacademy.com/reference/ols_pred_rsq.md),
[`ols_press()`](https://olsrr.rsquaredacademy.com/reference/ols_press.md)

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_hadi(model)
#> $hadi
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>          0.19318654          0.10375645          0.20740884          0.09775893 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>          0.20724815          0.19946745          0.19652103          0.19692137 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>          0.12314263          0.14455206          0.24742810          0.10784806 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>          0.05027013          0.11471138          0.25914561          0.29265274 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>          1.14426624          0.99609055          0.20171955          1.06239341 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>          0.28064340          0.35373215          0.44820010          0.15099700 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>          0.41281074          0.10477548          0.08003177          0.37633639 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>          0.32631790          0.19910073          1.19634000          0.14496664 
#> 
#> $potential
#>  [1] 0.04642961 0.04793496 0.06734998 0.09700248 0.20204116 0.07204021
#>  [7] 0.15814866 0.14431329 0.10998283 0.12811895 0.12811895 0.09886225
#> [13] 0.04904646 0.05345386 0.25898633 0.26443008 0.23883499 0.09118421
#> [19] 0.17116856 0.11159933 0.06050321 0.10075604 0.08338035 0.11503917
#> [25] 0.24318619 0.10245226 0.08001939 0.19678226 0.31299327 0.15926270
#> [31] 0.99626944 0.08504229
#> 
#> $residual
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>        0.1467569282        0.0558214978        0.1400588567        0.0007564513 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>        0.0052069911        0.1274272438        0.0383723684        0.0526080784 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>        0.0131598054        0.0164331043        0.1193091514        0.0089858122 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>        0.0012236671        0.0612575182        0.0001592827        0.0282226574 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>        0.9054312452        0.9049063421        0.0305509895        0.9507940779 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>        0.2201401850        0.2529761045        0.3648197429        0.0359578331 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>        0.1696245441        0.0023232180        0.0000123722        0.1795541315 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>        0.0133246374        0.0398380292        0.2000705631        0.0599243499 
#> 
```
