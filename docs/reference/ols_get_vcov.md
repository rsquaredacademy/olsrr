# Variance covariance matrix

Returns the variance covariance matrix from the model.

## Usage

``` r
ols_get_vcov(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

Variance covariance matrix.

## Examples

``` r
model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
ols_get_vcov(model)
#>             (Intercept)           wt          cyl            hp          disp
#> (Intercept) 68.05480702 -4.170155897 -3.821986835  6.934879e-02  8.275310e-03
#> wt          -4.17015590  1.174044257  0.115881777 -1.033268e-03 -7.862270e-03
#> cyl         -3.82198683  0.115881777  0.562523439 -7.004062e-03 -3.490671e-03
#> hp           0.06934879 -0.001033268 -0.007004062  2.854728e-04 -6.980737e-05
#> disp         0.00827531 -0.007862270 -0.003490671 -6.980737e-05  1.564643e-04
#> gear        -3.35622849  0.143996009  0.206132638 -1.145742e-02  3.631012e-03
#> drat        -8.47695567  0.308773551  0.289504420 -1.725772e-03 -1.754776e-04
#>                     gear          drat
#> (Intercept) -3.356228486 -8.4769556746
#> wt           0.143996009  0.3087735505
#> cyl          0.206132638  0.2895044201
#> hp          -0.011457419 -0.0017257724
#> disp         0.003631012 -0.0001754776
#> gear         1.268959944 -0.6169423590
#> drat        -0.616942359  2.2968754445
```
