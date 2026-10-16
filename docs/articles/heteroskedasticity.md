# Heteroscedasticity

## Introduction

One of the assumptions made about residuals/errors in OLS regression is
that the errors have the same but unknown variance. This is known as
constant variance or homoscedasticity. When this assumption is violated,
the problem is known as heteroscedasticity.

###### Consequences of Heteroscedasticity

- The OLS estimators and regression predictions based on them remains
  unbiased and consistent.
- The OLS estimators are no longer the BLUE (Best Linear Unbiased
  Estimators) because they are no longer efficient, so the regression
  predictions will be inefficient too.
- Because of the inconsistency of the covariance matrix of the estimated
  regression coefficients, the tests of hypotheses, (t-test, F-test) are
  no longer valid.

**olsrr** provides the following 4 tests for detecting
heteroscedasticity:

- Bartlett Test
- Breusch Pagan Test
- Score Test
- F Test

### Bartlett Test

Bartlett’s test is used to test if variances across samples is equal. It
is sensitive to departures from normality. The Levene test is an
alternative test that is less sensitive to departures from normality.

You can perform the test using 2 continuous variables, one continuous
and one grouping variable, a formula or a linear model.

##### Use grouping variable

[`ols_test_bartlett`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.md)`(``hsb``, ``'read'``, group_var ``=`` ``'female'``)`

    ## 
    ##     Bartlett's Test of Homogenity of Variances    
    ## ------------------------------------------------
    ## Ho: Variances are equal across groups
    ## Ha: Variances are unequal for atleast two groups
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    1 
    ##  Chi2          =    0.1866579 
    ##  Prob > Chi2   =    0.6657129

##### Using variables

[`ols_test_bartlett`](https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.md)`(``hsb``, ``'read'``, ``'write'``)`

    ## 
    ##     Bartlett's Test of Homogenity of Variances    
    ## ------------------------------------------------
    ## Ho: Variances are equal across groups
    ## Ha: Variances are unequal for atleast two groups
    ## 
    ##         Data          
    ##  ---------------------
    ##  Variables: read write 
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    1 
    ##  Chi2          =    1.222871 
    ##  Prob > Chi2   =    0.2687979

### Breusch Pagan Test

Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in
1979. It is used to test for heteroskedasticity in a linear regression
model and assumes that the error terms are normally distributed. It
tests whether the variance of the errors from a regression is dependent
on the values of the independent variables. It is a $`\chi^{2}`$ test.

You can perform the test using the fitted values of the model, the
predictors in the model and a subset of the independent variables. It
includes options to perform multiple tests and p value adjustments. The
options for p value adjustments include Bonferroni, Sidak and Holm’s
method.

##### Use fitted values of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##              Data               
    ##  -------------------------------
    ##  Response : mpg 
    ##  Variables: fitted values of mpg 
    ## 
    ##        Test Summary         
    ##  ---------------------------
    ##  DF            =    1 
    ##  Chi2          =    1.429672 
    ##  Prob > Chi2   =    0.231818

##### Use independent variables of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``, rhs ``=`` ``TRUE``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##            Data            
    ##  --------------------------
    ##  Response : mpg 
    ##  Variables: disp hp wt drat 
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    4 
    ##  Chi2          =    1.513808 
    ##  Prob > Chi2   =    0.8241927

##### Use independent variables of the model and perform multiple tests

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``, rhs ``=`` ``TRUE``, multiple ``=`` ``TRUE``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##            Data            
    ##  --------------------------
    ##  Response : mpg 
    ##  Variables: disp hp wt drat 
    ## 
    ##         Test Summary (Unadjusted p values)       
    ##  ----------------------------------------------
    ##   Variable           chi2       df        p     
    ##  ----------------------------------------------
    ##   disp             1.2355345     1    0.2663334 
    ##   hp               0.9209878     1    0.3372157 
    ##   wt               1.2529988     1    0.2629805 
    ##   drat             1.1668486     1    0.2800497 
    ##  ----------------------------------------------
    ##   simultaneous     1.5138083     4    0.8241927 
    ##  ----------------------------------------------

##### Bonferroni p value Adjustment

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``, rhs ``=`` ``TRUE``, multiple ``=`` ``TRUE``, p.adj ``=`` ``'bonferroni'``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##            Data            
    ##  --------------------------
    ##  Response : mpg 
    ##  Variables: disp hp wt drat 
    ## 
    ##         Test Summary (Bonferroni p values)       
    ##  ----------------------------------------------
    ##   Variable           chi2       df        p     
    ##  ----------------------------------------------
    ##   disp             1.2355345     1    1.0000000 
    ##   hp               0.9209878     1    1.0000000 
    ##   wt               1.2529988     1    1.0000000 
    ##   drat             1.1668486     1    1.0000000 
    ##  ----------------------------------------------
    ##   simultaneous     1.5138083     4    0.8241927 
    ##  ----------------------------------------------

##### Sidak p value Adjustment

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``, rhs ``=`` ``TRUE``, multiple ``=`` ``TRUE``, p.adj ``=`` ``'sidak'``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##            Data            
    ##  --------------------------
    ##  Response : mpg 
    ##  Variables: disp hp wt drat 
    ## 
    ##           Test Summary (Sidak p values)          
    ##  ----------------------------------------------
    ##   Variable           chi2       df        p     
    ##  ----------------------------------------------
    ##   disp             1.2355345     1    0.7102690 
    ##   hp               0.9209878     1    0.8070305 
    ##   wt               1.2529988     1    0.7049362 
    ##   drat             1.1668486     1    0.7313356 
    ##  ----------------------------------------------
    ##   simultaneous     1.5138083     4    0.8241927 
    ##  ----------------------------------------------

##### Holm’s p value Adjustment

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``drat``, data ``=`` ``mtcars``)`` `[`ols_test_breusch_pagan`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md)`(``model``, rhs ``=`` ``TRUE``, multiple ``=`` ``TRUE``, p.adj ``=`` ``'holm'``)`

    ## 
    ##  Breusch Pagan Test for Heteroskedasticity
    ##  -----------------------------------------
    ##  Ho: the variance is constant            
    ##  Ha: the variance is not constant        
    ## 
    ##            Data            
    ##  --------------------------
    ##  Response : mpg 
    ##  Variables: disp hp wt drat 
    ## 
    ##           Test Summary (Holm's p values)         
    ##  ----------------------------------------------
    ##   Variable           chi2       df        p     
    ##  ----------------------------------------------
    ##   disp             1.2355345     1    0.7990002 
    ##   hp               0.9209878     1    0.3372157 
    ##   wt               1.2529988     1    1.0000000 
    ##   drat             1.1668486     1    0.5600994 
    ##  ----------------------------------------------
    ##   simultaneous     1.5138083     4    0.8241927 
    ##  ----------------------------------------------

### Score Test

Test for heteroskedasticity under the assumption that the errors are
independent and identically distributed (i.i.d.). You can perform the
test using the fitted values of the model, the predictors in the model
and a subset of the independent variables.

##### Use fitted values of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_score`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)`(``model``)`

    ## 
    ##  Score Test for Heteroskedasticity
    ##  ---------------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: fitted values of mpg 
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    1 
    ##  Chi2          =    0.5163959 
    ##  Prob > Chi2   =    0.4723832

##### Use independent variables of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_score`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)`(``model``, rhs ``=`` ``TRUE``)`

    ## 
    ##  Score Test for Heteroskedasticity
    ##  ---------------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: disp hp wt qsec 
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    4 
    ##  Chi2          =    2.039404 
    ##  Prob > Chi2   =    0.7285114

##### Specify variables

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_score`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)`(``model``, vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``'disp'``, ``'hp'``)``)`

    ## 
    ##  Score Test for Heteroskedasticity
    ##  ---------------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: disp hp 
    ## 
    ##         Test Summary         
    ##  ----------------------------
    ##  DF            =    2 
    ##  Chi2          =    0.9983196 
    ##  Prob > Chi2   =    0.6070405

### F Test

F Test for heteroskedasticity under the assumption that the errors are
independent and identically distributed (i.i.d.). You can perform the
test using the fitted values of the model, the predictors in the model
and a subset of the independent variables.

##### Use fitted values of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_f`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md)`(``model``)`

    ## 
    ##  F Test for Heteroskedasticity
    ##  -----------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: fitted values of mpg 
    ## 
    ##       Test Summary        
    ##  -------------------------
    ##  Num DF     =    1 
    ##  Den DF     =    30 
    ##  F          =    0.4920617 
    ##  Prob > F   =    0.4884154

##### Use independent variables of the model

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_f`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md)`(``model``, rhs ``=`` ``TRUE``)`

    ## 
    ##  F Test for Heteroskedasticity
    ##  -----------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: disp hp wt qsec 
    ## 
    ##       Test Summary        
    ##  -------------------------
    ##  Num DF     =    4 
    ##  Den DF     =    27 
    ##  F          =    0.4594694 
    ##  Prob > F   =    0.7647271

##### Specify variables

`model`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``disp`` ``+`` ``hp`` ``+`` ``wt`` ``+`` ``qsec``, data ``=`` ``mtcars``)`` `[`ols_test_f`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md)`(``model``, vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``'disp'``, ``'hp'``)``)`

    ## 
    ##  F Test for Heteroskedasticity
    ##  -----------------------------
    ##  Ho: Variance is homogenous
    ##  Ha: Variance is not homogenous
    ## 
    ##  Variables: disp hp 
    ## 
    ##       Test Summary        
    ##  -------------------------
    ##  Num DF     =    2 
    ##  Den DF     =    29 
    ##  F          =    0.4669306 
    ##  Prob > F   =    0.631555
