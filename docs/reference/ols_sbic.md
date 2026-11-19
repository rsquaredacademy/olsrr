# Sawa's bayesian information criterion

Sawa's bayesian information criterion for model selection.

## Usage

``` r
ols_sbic(model, full_model)
```

## Arguments

- model:

  An object of class `lm`.

- full_model:

  An object of class `lm`.

## Value

Sawa's Bayesian Information Criterion

## Details

Sawa (1978) developed a model selection criterion that was derived from
a Bayesian modification of the AIC criterion. Sawa's Bayesian
Information Criterion (BIC) is a function of the number of observations
n, the SSE, the pure error variance fitting the full model, and the
number of independent variables including the intercept.

\$\$SBIC = n \* ln(SSE / n) + 2(p + 2)q - 2(q^2)\$\$

where \\q = n(\sigma^2)/SSE\\, *n* is the sample size, *p* is the number
of model parameters including intercept *SSE* is the residual sum of
squares.

## References

Sawa, T. (1978). “Information Criteria for Discriminating among
Alternative Regression Models.” Econometrica 46:1273–1282.

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
[`ols_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_sbc.md)

## Examples

``` r
full_model <- lm(mpg ~ ., data = mtcars)
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_sbic(model, full_model)
#> [1] 70.27599
```
