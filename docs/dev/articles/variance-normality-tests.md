# What do variance and normality tests establish?

# What do variance and normality tests establish?

Source:
[`vignettes/variance-normality-tests.Rmd`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/vignettes/variance-normality-tests.Rmd)

`variance-normality-tests.Rmd`

Do formal tests support the concerns in a customer-spending residual
plot? We will use the same model as the [residual diagnostics
guide](https://olsrr.rsquaredacademy.com/dev/articles/residual-diagnostics.md),
distinguish two null hypotheses and interpret the results together. Here
both the variance test and normality checks give evidence against their
nulls. They identify questions for further investigation, without
choosing a remedy.

## Keep the model and observations fixed

**Example data: wholesale customer spending**

The 440 observations record annual spending in monetary units, with
currency unspecified. The earlier plots showed uneven residual spread
and departures from normality. Fit the same milk-on-grocery model so
that the tests address those findings. See
[`?wholesale_customers`](https://olsrr.rsquaredacademy.com/dev/reference/wholesale_customers.md)
for the source and licence.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`olsrr`](https://olsrr.rsquaredacademy.com/)`)`\
`customer_data`` ``<-`` ``wholesale_customers``[`[`c`](https://rdrr.io/r/base/c.html)`(``"milk"``, ``"grocery"``)``]`\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``customer_data``)`` ``==`` ``440L``, ``!`[`anyNA`](https://rdrr.io/r/base/NA.html)`(``customer_data``)``)`\
`spending_model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``milk`` ``~`` ``grocery``, data ``=`` ``customer_data``)`

The assumptions concern unobserved model errors conditional on grocery
spending. The functions use fitted residuals to investigate them.
Residuals inherit constraints and dependence from fitting, so formal
distribution tests applied to them are diagnostic approximations, not a
complete validation of the error model.

## Test a specified variance pattern

The Breusch-Pagan null is **constant conditional error variance**. The
default call looks for variance associated with the model’s fitted
values. Its alternative concerns that specified direction; it does not
cover every possible pattern of changing variance. This implementation
assumes normal errors for its reference calibration, as well as an
appropriate mean model and independent observations.

\
`variance_test`` ``<-`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/dev/reference/ols_test_breusch_pagan.md)`(``spending_model``)`\
`variance_test`\
`#> `\
`#>  Breusch Pagan Test for Heteroskedasticity`\
`#>  -----------------------------------------`\
`#>  Ho: the variance is constant            `\
`#>  Ha: the variance is not constant        `\
`#> `\
`#>               Data               `\
`#>  --------------------------------`\
`#>  Response : milk `\
`#>  Variables: fitted values of milk `\
`#> `\
`#>        Test Summary         `\
`#>  ---------------------------`\
`#>  DF            =    1 `\
`#>  Chi2          =    936.0886 `\
`#>  Prob > Chi2   =    < 0.0001`

The chi-square statistic is 936.089 with one degree of freedom and p \<
0.0001. At a prespecified 5% significance level, that is evidence
against the constant-variance null in the tested direction. It agrees
with the visual concern about widening spread, but does not prove which
variance function is correct. An incorrect mean model can also create a
residual pattern.

Changing variance does not by itself make OLS coefficients biased:
unbiasedness requires a correctly specified conditional mean with zero
conditional-mean errors, and consistency needs corresponding exogeneity
and regularity conditions. The usual constant-variance standard errors
can nevertheless be inappropriate, affecting confidence intervals and
tests. A rejected variance test does not invalidate every fitted value
or automatically select weighted fitting, a transformation or another
remedy.

## Ask a separate normality question

Each normality check has a **normal-distribution null**, with location
and scale estimated from the data. They emphasize different kinds of
departure and inspect the same residuals, so their results are not four
independent confirmations. The Kolmogorov-Smirnov row uses the
Lilliefors correction for estimating the normal mean and standard
deviation.

\
`normality_tests`` ``<-`` `[`ols_test_normality`](https://olsrr.rsquaredacademy.com/dev/reference/ols_test_normality.md)`(``spending_model``)`\
`#> Warning in nortest::cvm.test(y): p-value is smaller than 7.37e-10, cannot be`\
`#> computed more accurately`\
`normality_tests`\
`#> -------------------------------------------------------------`\
`#>              Test                    Statistic        pvalue  `\
`#> -------------------------------------------------------------`\
`#> Shapiro-Wilk                           0.6334        < 0.0001 `\
`#> Kolmogorov-Smirnov (Lilliefors)        0.2084        < 0.0001 `\
`#> Cramer-von Mises                       7.4861        < 0.0001 `\
`#> Anderson-Darling                      39.4027        < 0.0001 `\
`#> -------------------------------------------------------------`

The Shapiro-Wilk, Lilliefors, Cramer-von Mises and Anderson-Darling
p-values are all below 0.0001. These results indicate departures from a
normal shape at a 5% threshold, consistent with the Q-Q plot’s tails.
They do not distinguish skewness caused by changing variance from a
missed mean relationship or unusual observations.

A large p-value would mean insufficient evidence against the particular
null, not proof that an assumption holds. Small samples can miss
relevant departures and large samples can detect minor ones. Normality
is useful for exact small-sample inference under the model; it is not
necessary just to compute least-squares coefficients. No test here
establishes independence or the suitability of the model for new
customers.

## Keep other test options in context

| Question or option | Function and scope |
|----|----|
| Test variance against model predictors or selected predictors | [`ols_test_breusch_pagan()`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.html) with `rhs = TRUE` or `vars` |
| Test several variance directions | `multiple = TRUE` with `rhs = TRUE`; `p.adj` offers Holm, Bonferroni and Sidak adjustments |
| Explore another variance test | [`ols_test_score()`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.html), [`ols_test_f()`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.html); read their assumptions before use |
| Summarize alignment with normal scores | [`ols_test_correlation()`](https://olsrr.rsquaredacademy.com/reference/ols_test_correlation.html); this is not a serial-correlation diagnostic |

These options change the question or calibration. Do not search among
them for a favorable p-value. The variance and residual-normality
diagnostics used here are not supported for weighted models; consult the
individual function help before extending the example.

### Separate sidebar: compare genuinely grouped variances

**Example data: wholesale customer regions**

Bartlett’s test asks whether population variances are equal across
groups. Here the question concerns milk spending in Lisbon, Porto and
the other regions. This is a separate grouped question, not a
replacement test of regression residual variance.

\
[`ols_test_bartlett`](https://olsrr.rsquaredacademy.com/dev/reference/ols_test_bartlett.md)`(``wholesale_customers``, ``"milk"``, group_var ``=`` ``"region"``)`\
`#> `\
`#>     Bartlett's Test of Homogenity of Variances    `\
`#> ------------------------------------------------`\
`#> Ho: Variances are equal across groups`\
`#> Ha: Variances are unequal for atleast two groups`\
`#> `\
`#>        Test Summary         `\
`#>  ---------------------------`\
`#>  DF            =    2 `\
`#>  Chi2          =    16.35007 `\
`#>  Prob > Chi2   =    0.0003`

The statistic is 16.350 on two degrees of freedom, with p = 0.0003. This
is evidence against equal group variances. Bartlett’s calibration is
sensitive to departures from normality and assumes independent
observations within and between groups. Inspect the group distributions
before relying on it.

Next, revisit the [two residual
plots](https://olsrr.rsquaredacademy.com/dev/articles/residual-diagnostics.md)
and investigate the mean shape and spread together. The help pages for
[`ols_test_breusch_pagan()`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.html),
[`ols_test_normality()`](https://olsrr.rsquaredacademy.com/reference/ols_test_normality.html)
and
[`ols_test_bartlett()`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.html)
describe their interfaces and limitations. Choose any follow-up model
using the data context and compare its diagnostics on the same rows.
