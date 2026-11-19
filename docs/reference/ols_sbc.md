# Bayesian information criterion

Bayesian information criterion for model selection.

## Usage

``` r
ols_sbc(model, method = c("R", "STATA", "SAS"))
```

## Arguments

- model:

  An object of class `lm`.

- method:

  A character vector; specify the method to compute BIC. Valid options
  include R, STATA and SAS.

## Value

The bayesian information criterion of the model.

## Details

SBC provides a means for model selection. Given a collection of models
for the data, SBC estimates the quality of each model, relative to each
of the other models. R and STATA use loglikelihood to compute SBC. SAS
uses residual sum of squares. Below is the formula in each case:

*R & STATA* \$\$AIC = -2(loglikelihood) + ln(n) \* 2p\$\$

*SAS* \$\$AIC = n \* ln(SSE / n) + p \* ln(n)\$\$

where *n* is the sample size and *p* is the number of model parameters
including intercept.

## References

Schwarz, G. (1978). “Estimating the Dimension of a Model.” Annals of
Statistics 6:461–464.

Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The
Theory and Practice of Econometrics. New York: John Wiley & Sons.

## See also

Other model selection criteria:
[`ols_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_aic.md),
[`ols_apc()`](https://olsrr.rsquaredacademy.com/reference/ols_apc.md),
[`ols_fpe()`](https://olsrr.rsquaredacademy.com/reference/ols_fpe.md),
[`ols_hsp()`](https://olsrr.rsquaredacademy.com/reference/ols_hsp.md),
[`ols_mallows_cp()`](https://olsrr.rsquaredacademy.com/reference/ols_mallows_cp.md),
[`ols_msep()`](https://olsrr.rsquaredacademy.com/reference/ols_msep.md),
[`ols_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_sbic.md)

## Examples

``` r
# using R computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_sbc(model)
#> [1] 167.864

# using STATA computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_sbc(model, method = 'STATA')
#> [1] 164.3983

# using SAS computation method
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_sbc(model, method = 'SAS')
#> [1] 73.58622
```
