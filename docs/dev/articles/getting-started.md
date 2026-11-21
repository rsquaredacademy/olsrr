# Can product spending explain milk spending?

# Can product spending explain milk spending?

Source:
[`vignettes/getting-started.Rmd`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/vignettes/getting-started.Rmd)

`getting-started.Rmd`

Can grocery and detergent spending help describe milk spending? We fit
one model, read its coefficients and inspect what it misses. The model
accounts for about 53% of observed variation, while substantial
residuals remain.

## Understand the data

The `wholesale_customers` data contain annual spending for 440 wholesale
customers. Each row is a customer; spending is in monetary units, with
currency unspecified. These observational associations do not establish
the effect of changing a customer’s purchases. See
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

All three columns are complete. When adapting the example, decide how to
handle missing values before comparing models.

## Fit once and read the result

Use [`lm()`](https://rdrr.io/r/stats/lm.html) with the response on the
left of `~` and predictors on the right. The additive model has no
interaction.

\
`spending_model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``milk`` ``~`` ``grocery`` ``+`` ``detergents_paper``, data ``=`` ``customer_data``)`\
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

Holding detergent spending fixed, one additional monetary unit of
grocery spending is associated with about 0.623 units more milk
spending. Holding grocery spending fixed, the detergent slope is -0.124,
with a 95% confidence interval from -0.386 to 0.137. These conditional
comparisons do not imply an intervention effect. The intercept refers to
zero spending on both predictors, so extrapolation needs care.

## Inspect what remains unexplained

A residual is observed minus fitted milk spending. A positive residual
identifies a customer spending more on milk than fitted.

\
`residual_plot`` ``<-`` `[`ols_plot_resid_fit`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_fit.md)`(``spending_model``, print_plot ``=`` ``FALSE``)`\
`residual_plot`

![Residual milk spending against fitted milk spending, with several
large positive and negative
residuals.](getting-started_files/figure-html/residual-plot-1.png)

Milk spending residuals against fitted spending.

The large residuals warrant investigation of individual customers and
the mean relationship. R-squared does not establish that the
straight-line mean or uncertainty calculations are adequate. Here
`print_plot = FALSE` returns a ggplot directly; other plotting functions
can return lists.

## Put the fit in perspective

\
`performance`` ``<-`` `[`ols_model_performance`](https://olsrr.rsquaredacademy.com/dev/reference/ols_model_performance.md)`(``spending_model``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  Measure ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"R-squared"``, ``"Adjusted R-squared"``, ``"Predicted R-squared"``, ``"RMSE"``)``,`\
`  Value ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``performance``$``rsq``, ``performance``$``adjr``, ``performance``$``prsq``, ``performance``$``rmse``)`\
`)``, digits ``=`` ``3``, caption ``=`` ``"Performance for the same milk-spending model."``)`

| Measure             |    Value |
|:--------------------|---------:|
| R-squared           |    0.531 |
| Adjusted R-squared  |    0.529 |
| Predicted R-squared |    0.490 |
| RMSE                | 5063.711 |

Performance for the same milk-spending model. {.table}

R-squared is 0.531, and RMSE is 5063.711 monetary units. Predicted
R-squared is 0.490 and uses leave-one-out prediction errors; it is not
independent validation on new customers. RMSE describes residual
scatter, not an error bound for an individual prediction.

Next, learn to [read a regression
table](https://olsrr.rsquaredacademy.com/dev/articles/regression-output.md),
then investigate [residual
diagnostics](https://olsrr.rsquaredacademy.com/dev/articles/residual-diagnostics.md).
See
[`?ols_regress`](https://olsrr.rsquaredacademy.com/dev/reference/ols_regress.md),
[`?ols_plot_resid_fit`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_fit.md)
and
[`?ols_model_performance`](https://olsrr.rsquaredacademy.com/dev/reference/ols_model_performance.md)
for their interfaces.
