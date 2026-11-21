# What does a customer-spending model miss?

# What does a customer-spending model miss?

Source:
[`vignettes/residual-diagnostics.Rmd`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/vignettes/residual-diagnostics.Rmd)

`residual-diagnostics.Rmd`

Does a straight line capture how milk spending changes with grocery
spending? We fit one model and inspect two residual plots. Large
residuals and an uneven spread raise questions about the mean and
variance.

## Understand the data and fit

The `wholesale_customers` data contain annual spending by 440 customers
in monetary units, with currency unspecified. All observations used here
are complete. See
[`?wholesale_customers`](https://olsrr.rsquaredacademy.com/dev/reference/wholesale_customers.md)
for the source and licence.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`olsrr`](https://olsrr.rsquaredacademy.com/)`)`\
`customer_data`` ``<-`` ``wholesale_customers``[`[`c`](https://rdrr.io/r/base/c.html)`(``"milk"``, ``"grocery"``)``]`\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``customer_data``)`` ``==`` ``440L``, ``!`[`anyNA`](https://rdrr.io/r/base/NA.html)`(``customer_data``)``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``customer_data``)`\
`#>   milk grocery`\
`#> 1 9656    7561`\
`#> 2 9810    9568`\
`#> 3 8808    7684`\
`#> 4 1196    4221`\
`#> 5 5410    7198`\
`#> 6 8259    5126`

\
`spending_model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``milk`` ``~`` ``grocery``, data ``=`` ``customer_data``)`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``spending_model``)`\
`#> (Intercept)     grocery `\
`#> 1298.689653    0.565642`

The fitted slope is about 0.566 units of milk spending per unit of
grocery spending. A positive residual means observed milk spending
exceeds the fitted value. R-squared is about 0.531, but it does not
establish whether the residual pattern is acceptable.

The assumptions concern unobserved errors conditional on predictors.
Fitted residuals estimate those errors; estimating coefficients makes
residuals dependent and gives them different variances even under
constant error variance.

## Look for shape and changing spread

\
[`ols_plot_resid_fit`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_fit.md)`(``spending_model``)`

![Residuals against fitted milk spending; residuals vary substantially,
with extreme positive and negative observations at high fitted
values.](residual-diagnostics_files/figure-html/residual-fit-1.png)

Milk spending residuals against fitted spending, in monetary units.

The residual spread is much larger among some customers with high fitted
spending. Investigate both the variance and whether the straight-line
mean misses structure. A large residual calls for checking the
observation and its context; it is not a reason by itself to delete a
row.

## Compare the residual distribution with normality

\
[`ols_plot_resid_qq`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_qq.md)`(``spending_model``)`

![Normal Q-Q plot of residuals with substantial departures from the
reference line in both
tails.](residual-diagnostics_files/figure-html/residual-qq-1.png)

Normal Q-Q plot of milk spending residuals.

The tails depart substantially from the reference line. A missed mean
relationship, changing variance or unusual observations can affect this
plot. Normal errors are relevant to exact small-sample inference under
the usual assumptions, but are not required merely to calculate OLS
coefficients. Neither plot checks independence.

## Choose the next investigation

Continue with [variance and normality
tests](https://olsrr.rsquaredacademy.com/dev/articles/variance-normality-tests.md)
on this same model. Tests cannot choose a transformation or certify the
entire model.

| Question | Function |
|----|----|
| View several diagnostic summaries together | [`ols_plot_diagnostics()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_diagnostics.md) |
| Inspect residuals as bars or a box plot | [`ols_plot_resid_hist()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_hist.md), [`ols_plot_resid_box()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_resid_box.md) |
| Inspect the observed response before fitting | [`ols_plot_response()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_plot_response.md) |
| Draw a simple regression line from vectors | `ols_plot_reg_line(customer_data$milk, customer_data$grocery)` |

Return to [regression
output](https://olsrr.rsquaredacademy.com/dev/articles/regression-output.md)
for coefficient and RMSE interpretation. Keep the same observations when
comparing tables and plots.
