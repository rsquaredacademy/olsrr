# DFBETAs plot data

Prepares the data for dfbetas plot.

## Usage

``` r
ols_prep_dfbeta_data(d, threshold)
```

## Arguments

- d:

  A `tibble` or `data.frame` with dfbetas.

- threshold:

  The threshold for outliers.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
dfb <- dfbetas(model)
n <- nrow(dfb)
threshold <- 2 / sqrt(n)
dbetas  <- dfb[, 1]
df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
ols_prep_dfbeta_data(df_data, threshold)
#>                     obs        dbetas   color fct_color txt
#> Mazda RX4             1 -0.2408625040  normal    normal  NA
#> Mazda RX4 Wag         2 -0.0893854682  normal    normal  NA
#> Datsun 710            3 -0.0226714112  normal    normal  NA
#> Hornet 4 Drive        4  0.0165461439  normal    normal  NA
#> Hornet Sportabout     5 -0.0001791988  normal    normal  NA
#> Valiant               6  0.2161392861  normal    normal  NA
#> Duster 360            7  0.0559934442  normal    normal  NA
#> Merc 240D             8  0.0339246041  normal    normal  NA
#> Merc 230              9  0.4484991305 outlier   outlier   9
#> Merc 280             10 -0.0178054777  normal    normal  NA
#> Merc 280C            11 -0.0032897727  normal    normal  NA
#> Merc 450SE           12  0.0270335275  normal    normal  NA
#> Merc 450SL           13 -0.0022994410  normal    normal  NA
#> Merc 450SLC          14  0.0663413205  normal    normal  NA
#> Cadillac Fleetwood   15 -0.0010867064  normal    normal  NA
#> Lincoln Continental  16 -0.0129594354  normal    normal  NA
#> Chrysler Imperial    17 -0.0672243472  normal    normal  NA
#> Fiat 128             18 -0.0353581676  normal    normal  NA
#> Honda Civic          19  0.0591178524  normal    normal  NA
#> Toyota Corolla       20 -0.3583658816 outlier   outlier  20
#> Toyota Corona        21  0.3763120390 outlier   outlier  21
#> Dodge Challenger     22 -0.2073631012  normal    normal  NA
#> AMC Javelin          23 -0.1393390661  normal    normal  NA
#> Camaro Z28           24 -0.0209471589  normal    normal  NA
#> Pontiac Firebird     25  0.0503356537  normal    normal  NA
#> Fiat X1-9            26 -0.0059464066  normal    normal  NA
#> Porsche 914-2        27  0.1174959855  normal    normal  NA
#> Lotus Europa         28  0.1206617098  normal    normal  NA
#> Ford Pantera L       29 -0.0016098012  normal    normal  NA
#> Ferrari Dino         30 -0.0519535108  normal    normal  NA
#> Maserati Bora        31 -0.2994696671  normal    normal  NA
#> Volvo 142E           32 -0.0168421538  normal    normal  NA
```
