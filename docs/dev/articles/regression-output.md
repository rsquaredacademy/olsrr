# Read a customer-spending regression model

# Read a customer-spending regression model

Source:
[`vignettes/regression-output.Rmd`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/vignettes/regression-output.Rmd)

`regression-output.Rmd`

How does milk spending vary with grocery spending, after accounting for
detergent and paper spending? We read one regression result and connect
its coefficients, uncertainty and residual error.

## Understand the measurements

The `wholesale_customers` data contain annual spending by 440 wholesale
customers in monetary units; the currency is unspecified. Each row is a
customer. See
[`?wholesale_customers`](https://olsrr.rsquaredacademy.com/dev/reference/wholesale_customers.md)
for the source and licence.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`olsrr`](https://olsrr.rsquaredacademy.com/)`)`\
`customer_data`` ``<-`` ``wholesale_customers``[`[`c`](https://rdrr.io/r/base/c.html)`(``"milk"``, ``"grocery"``, ``"detergents_paper"``)``]`\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``customer_data``)`` ``==`` ``440L``, ``!`[`anyNA`](https://rdrr.io/r/base/NA.html)`(``customer_data``)``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``customer_data``)`\
`#>   milk grocery detergents_paper`\
`#> 1 9656    7561             2674`\
`#> 2 9810    9568             3293`\
`#> 3 8808    7684             3516`\
`#> 4 1196    4221              507`\
`#> 5 5410    7198             1777`\
`#> 6 8259    5126             1795`

All three measurements are present. The model describes observed
associations, not the effect of an intervention on purchases.

## Fit one model

\
`spending_model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``milk`` ``~`` ``grocery`` ``+`` ``detergents_paper``, data ``=`` ``customer_data``)`

Each additive slope compares customers holding the other predictor
fixed. The model does not include interactions or curved relationships.

## Read the regression table

\
[`ols_regress`](https://olsrr.rsquaredacademy.com/dev/reference/ols_regress.md)`(``spending_model``)`\
`#>                              Model Summary                              `\
`#> -----------------------------------------------------------------------`\
`#> R                          0.729       RMSE                   5063.711 `\
`#> R-Squared                  0.531       MSE                25641169.793 `\
`#> Adj. R-Squared             0.529       Coef. Var                87.362 `\
`#> Pred R-Squared             0.490       AIC                    8759.928 `\
`#> MAE                     2563.777       SBC                    8776.275 `\
`#> -----------------------------------------------------------------------`\
`#>  RMSE: Root Mean Square Error `\
`#>  MSE: Mean Square Error `\
`#>  MAE: Mean Absolute Error `\
`#>  AIC: Akaike Information Criteria `\
`#>  SBC: Schwarz Bayesian Criteria `\
`#> `\
`#>                                       ANOVA                                        `\
`#> ----------------------------------------------------------------------------------`\
`#>                        Sum of                                                     `\
`#>                       Squares         DF       Mean Square       F           Sig. `\
`#> ----------------------------------------------------------------------------------`\
`#> Regression    12707124418.370          2    6353562209.185    247.788    < 0.0001 `\
`#> Residual      11205191199.519        437      25641169.793                        `\
`#> Total         23912315617.889        439                                          `\
`#> ----------------------------------------------------------------------------------`\
`#> `\
`#>                                          Parameter Estimates                                          `\
`#> -----------------------------------------------------------------------------------------------------`\
`#>            model        Beta    Std. Error    Std. Beta      t         Sig         lower       upper `\
`#> -----------------------------------------------------------------------------------------------------`\
`#>      (Intercept)    1198.484       332.724                  3.602      0.0004    544.545    1852.423 `\
`#>          grocery       0.623         0.067        0.802     9.333    < 0.0001      0.492       0.754 `\
`#> detergents_paper      -0.124         0.133       -0.080    -0.933      0.3515     -0.386       0.137 `\
`#> -----------------------------------------------------------------------------------------------------`

The **Beta** estimate for `grocery`, 0.623, means about 0.623 additional
monetary units of milk spending per additional unit of grocery spending,
holding detergent and paper spending fixed. The corresponding detergent
slope is -0.124. Standardized betas use a different scale.

The **lower** and **upper** columns give 95% confidence intervals under
the model assumptions: 0.492 to 0.754 for grocery and -0.386 to 0.137
for detergents and paper. These describe uncertainty in conditional mean
slopes, not individual customer spending. The intercept is 1198.484
units when both predictors are zero; extrapolation requires care.

## Connect the output to base R

\
`base_summary`` ``<-`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``spending_model``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``base_summary``$``coefficients``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Estimate"``, ``"Std. Error"``)``]``,`\
`             digits ``=`` ``3``, caption ``=`` ``"The same coefficients and standard errors from summary(lm(...))."``)`

|                  | Estimate | Std. Error |
|:-----------------|---------:|-----------:|
| (Intercept)      | 1198.484 |    332.724 |
| grocery          |    0.623 |      0.067 |
| detergents_paper |   -0.124 |      0.133 |

The same coefficients and standard errors from summary(lm(…)). {.table}

The estimates and standard errors agree. R’s residual standard error is
the RMSE, 5063.711, calculated from the residual sum of squares and 437
residual degrees of freedom. It describes scatter rather than a
prediction error bound.

## Understand the ANOVA decomposition

\
`spending_anova`` ``<-`` `[`ols_anova`](https://olsrr.rsquaredacademy.com/dev/reference/ols_anova.md)`(``spending_model``)`\
`spending_anova`\
`#>                                       ANOVA                                        `\
`#> ----------------------------------------------------------------------------------`\
`#>                        Sum of                                                     `\
`#>                       Squares         DF       Mean Square       F           Sig. `\
`#> ----------------------------------------------------------------------------------`\
`#> Regression    12707124418.370          2    6353562209.185    247.788    < 0.0001 `\
`#> Residual      11205191199.519        437      25641169.793                        `\
`#> Total         23912315617.889        439                                          `\
`#> ----------------------------------------------------------------------------------`

The total sum of squares is 23912315617.889, comprising 12707124418.370
for regression and 11205191199.519 for residuals. Their ratio gives
R-squared of 0.531. There are two regression and 437 residual degrees of
freedom. The overall F statistic is 247.788 and tests whether both
slopes are zero; it does not validate the model’s shape or error
assumptions.

Continue with [residual
diagnostics](https://olsrr.rsquaredacademy.com/dev/articles/residual-diagnostics.md),
or revisit [getting
started](https://olsrr.rsquaredacademy.com/dev/articles/getting-started.md).
See
[`?ols_regress`](https://olsrr.rsquaredacademy.com/dev/reference/ols_regress.md)
and
[`?ols_anova`](https://olsrr.rsquaredacademy.com/dev/reference/ols_anova.md)
for the remaining fields.
