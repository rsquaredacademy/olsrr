# Akaike information criterion

Akaike information criterion for model selection.

## Usage

``` r
ols_aic(model, method = c("R", "STATA", "SAS"), corrected = FALSE)
```

## Arguments

- model:

  An object of class `lm`.

- method:

  A character vector; specify the method to compute AIC. Valid options
  include R, STATA and SAS.

- corrected:

  Logical; if `TRUE`, returns corrected akaike information criterion for
  SAS method.

## Value

Akaike information criterion of the model.

## Details

AIC provides a means for model selection. Given a collection of models
for the data, AIC estimates the quality of each model, relative to each
of the other models. R and STATA use loglikelihood to compute AIC. SAS
uses residual sum of squares. Below is the formula in each case:

*R & STATA* \$\$AIC = -2(loglikelihood) + 2p\$\$

*SAS* \$\$AIC = n \* ln(SSE / n) + 2p\$\$

*corrected* \$\$AIC = n \* ln(SSE / n) + ((n \* (n + p)) / (n - p -
2))\$\$

where *n* is the sample size and *p* is the number of model parameters
including intercept.

## References

Akaike, H. (1969). “Fitting Autoregressive Models for Prediction.”
Annals of the Institute of Statistical Mathematics 21:243–247.

Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The
Theory and Practice of Econometrics. New York: John Wiley & Sons.

## See also

Other model selection criteria:
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
# using R computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_aic(model)
#> [1] 159.0696

# using STATA computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_aic(model, method = 'STATA')
#> [1] 157.0696

# using SAS computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_aic(model, method = 'SAS')
#> [1] 66.25754

# corrected akaike information criterion
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_aic(model, method = 'SAS', corrected = TRUE)
#> [1] 103.6175
```
