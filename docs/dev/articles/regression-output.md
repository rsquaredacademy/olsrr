# Read a tree-volume regression model

# Read a tree-volume regression model

Source:
[`vignettes/regression-output.Rmd`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/vignettes/regression-output.Rmd)

`regression-output.Rmd`

How much does a tree’s estimated timber volume change with trunk
diameter, after accounting for height? We will read one regression
result and connect its coefficients, uncertainty and residual error. In
this sample, an extra inch of diameter is associated with about 4.71
extra cubic feet of volume at a fixed height. The remaining residual
scatter is about 3.88 cubic feet.

## Understand the measurements

**Example data: R’s built-in trees dataset**

The 31 observations are felled black cherry trees. `Volume` is timber
volume in cubic feet and `Height` is height in feet. Despite its name,
`Girth` is trunk **diameter**, measured in inches at 4 feet 6 inches
above the ground. Reading the data documentation matters: interpreting
this column as circumference would give the slope the wrong physical
meaning.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`olsrr`](https://olsrr.rsquaredacademy.com/)`)`\
`tree_data`` ``<-`` ``datasets``::`[`trees`](https://rdrr.io/r/datasets/trees.html)\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``tree_data``)`` ``==`` ``31L``, ``!`[`anyNA`](https://rdrr.io/r/base/NA.html)`(``tree_data``)``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``tree_data``)`\
`#>   Girth Height Volume`\
`#> 1   8.3     70   10.3`\
`#> 2   8.6     65   10.3`\
`#> 3   8.8     63   10.2`\
`#> 4  10.5     72   16.4`\
`#> 5  10.7     81   18.8`\
`#> 6  10.8     83   19.7`

Every row has all three measurements, so no tree is dropped. This
example describes associations in the observed trees. It does not
establish a growth law or show what an intervention on height would
cause. Keeping the units beside the data helps distinguish a coefficient
from a measure without units, such as R-squared.

## Fit one model

\
`volume_model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``Volume`` ``~`` ``Girth`` ``+`` ``Height``, data ``=`` ``tree_data``)`

This additive model lets volume change linearly with diameter and
height. Each slope describes a comparison holding the other predictor
fixed. The formula does not encode the geometric relationship between
diameter and volume, which might motivate a curved or transformed model
later. For now the aim is to understand exactly what this fitted summary
says.

## Read the regression table

\
[`ols_regress`](https://olsrr.rsquaredacademy.com/dev/reference/ols_regress.md)`(``volume_model``)`\
`#>                          Model Summary                          `\
`#> ---------------------------------------------------------------`\
`#> R                       0.974       RMSE                 3.882 `\
`#> R-Squared               0.948       MSE                 15.069 `\
`#> Adj. R-Squared          0.944       Coef. Var           12.866 `\
`#> Pred R-Squared          0.931       AIC                176.910 `\
`#> MAE                     2.956       SBC                182.646 `\
`#> ---------------------------------------------------------------`\
`#>  RMSE: Root Mean Square Error `\
`#>  MSE: Mean Square Error `\
`#>  MAE: Mean Absolute Error `\
`#>  AIC: Akaike Information Criteria `\
`#>  SBC: Schwarz Bayesian Criteria `\
`#> `\
`#>                                  ANOVA                                  `\
`#> -----------------------------------------------------------------------`\
`#>                 Sum of                                                 `\
`#>                Squares        DF    Mean Square       F           Sig. `\
`#> -----------------------------------------------------------------------`\
`#> Regression    7684.163         2       3842.081    254.972    < 0.0001 `\
`#> Residual       421.921        28         15.069                        `\
`#> Total         8106.084        30                                       `\
`#> -----------------------------------------------------------------------`\
`#> `\
`#>                                      Parameter Estimates                                       `\
`#> ----------------------------------------------------------------------------------------------`\
`#>       model       Beta    Std. Error    Std. Beta      t         Sig         lower      upper `\
`#> ----------------------------------------------------------------------------------------------`\
`#> (Intercept)    -57.988         8.638                 -6.713    < 0.0001    -75.682    -40.293 `\
`#>       Girth      4.708         0.264        0.899    17.816    < 0.0001      4.167      5.249 `\
`#>      Height      0.339         0.130        0.132     2.607      0.0145      0.073      0.606 `\
`#> ----------------------------------------------------------------------------------------------`

The **Beta** estimate for `Girth`, 4.708, means about 4.71 additional
cubic feet per additional inch of diameter, comparing trees of the same
height. For `Height`, 0.339 means about 0.34 additional cubic feet per
extra foot of height at a fixed diameter. Standardized betas express
effects on a different scale; use the unstandardized Beta column for
these statements in inches, feet and cubic feet.

The **Std. Error** column measures uncertainty in each estimated slope.
The **lower** and **upper** columns give its 95% confidence interval
under the model assumptions: about 4.17 to 5.25 for diameter and 0.073
to 0.606 for height. The height estimate is less precise relative to its
size. These intervals concern conditional mean slopes, not the volumes
of individual trees. The p-values test zero coefficients conditional on
the other term; they do not measure the probability that a predictor
matters.

The intercept is about -57.99 cubic feet at zero diameter and zero
height. Neither value describes a tree in this sample. This is a warning
against extrapolation, rather than evidence of negative timber volume.

## Connect the output to base R

\
`base_summary`` ``<-`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``volume_model``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``base_summary``$``coefficients``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Estimate"``, ``"Std. Error"``)``]``,`\
`             digits ``=`` ``3``, caption ``=`` ``"The same coefficients and standard errors from summary(lm(...))."``)`

|             | Estimate | Std. Error |
|:------------|---------:|-----------:|
| (Intercept) |  -57.988 |      8.638 |
| Girth       |    4.708 |      0.264 |
| Height      |    0.339 |      0.130 |

The same coefficients and standard errors from summary(lm(…)). {.table}

The estimates and standard errors agree with the parameter table above:
olsrr presents information from the same fitted model. R’s residual
standard error, `base_summary$sigma`, is the RMSE reported here. Its
value of 3.882 cubic feet is the square root of residual sum of squares
divided by 28 residual degrees of freedom. It describes remaining
scatter, not a fixed error allowance for each new prediction.

## Understand the ANOVA decomposition

The regression output already includes an ANOVA table.
[`ols_anova()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_anova.md)
lets you request just that part when the overall fit is your question.

\
`volume_anova`` ``<-`` `[`ols_anova`](https://olsrr.rsquaredacademy.com/dev/reference/ols_anova.md)`(``volume_model``)`\
`volume_anova`

In that table, the total sum of squares, 8106.084, divides into 7684.163
attributed to the regression and 421.921 left in the residuals. Dividing
7684.163 by 8106.084 gives R-squared, 0.948. This is variation explained
in these trees, not a percentage accuracy guarantee for future volumes.

There are two regression degrees of freedom, one for each slope, and 28
residual degrees of freedom: 31 observations minus three estimated
coefficients, including the intercept. Dividing each sum of squares by
its degrees of freedom gives the mean squares. Their ratio is the
overall F statistic, 254.972. Its null is that both slopes are zero.
This joint test does not identify which model shape is adequate or
validate the error assumptions.

The concrete next task is to inspect residual shape and spread before
relying on the confidence intervals. Continue with [residual
diagnostics](https://olsrr.rsquaredacademy.com/dev/articles/residual-diagnostics.md),
or revisit [getting
started](https://olsrr.rsquaredacademy.com/dev/articles/getting-started.md).
The help for
[`ols_regress()`](https://olsrr.rsquaredacademy.com/reference/ols_regress.html)
and
[`ols_anova()`](https://olsrr.rsquaredacademy.com/reference/ols_anova.html)
covers the remaining table fields and supported interfaces.
