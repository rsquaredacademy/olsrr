
<!-- README.md is generated from README.Rmd. Please edit that file -->

# olsrr

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/olsrr)](https://cran.r-project.org/package=olsrr)
[![cran
checks](https://cranchecks.info/badges/summary/olsrr)](https://cran.r-project.org/web/checks/check_results_olsrr.html)
[![R build
status](https://github.com/rsquaredacademy/olsrr/workflows/R-CMD-check/badge.svg)](https://github.com/rsquaredacademy/olsrr/actions)
[![Coverage
status](https://codecov.io/gh/rsquaredacademy/olsrr/branch/master/graph/badge.svg)](https://codecov.io/github/rsquaredacademy/olsrr?branch=master)
[![status](https://tinyverse.netlify.com/badge/olsrr)](https://CRAN.R-project.org/package=olsrr)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/olsrr)](https://cran.r-project.org/package=olsrr)
<!-- badges: end -->

## Overview

The olsrr package provides following tools for building OLS regression
models using R:

-   Comprehensive Regression Output
-   Variable Selection Procedures
-   Heteroskedasticity Tests
-   Collinearity Diagnostics
-   Model Fit Assessment
-   Measures of Influence
-   Residual Diagnostics
-   Variable Contribution Assessment

## Installation

``` r
# Install release version from CRAN
install.packages("olsrr")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/olsrr")
```

## Articles

-   [Quick
    Overview](https://olsrr.rsquaredacademy.com/articles/intro.html)
-   [Variable Selection
    Methods](https://olsrr.rsquaredacademy.com/articles/variable_selection.html)
-   [Residual
    Diagnostics](https://olsrr.rsquaredacademy.com/articles/residual_diagnostics.html)
-   [Heteroskedasticity](https://olsrr.rsquaredacademy.com/articles/heteroskedasticity.html)
-   [Measures of
    Influence](https://olsrr.rsquaredacademy.com/articles/influence_measures.html)
-   [Collinearity
    Diagnostics](https://olsrr.rsquaredacademy.com/articles/regression_diagnostics.html)

## Usage

olsrr uses consistent prefix `ols_` for easy tab completion.

olsrr is built with the aim of helping those users who are new to the R
language. If you know how to write a `formula` or build models using
`lm`, you will find olsrr very useful. Most of the functions use an
object of class `lm` as input. So you just need to build a model using
`lm` and then pass it onto the functions in olsrr. Below is a quick
demo:

#### Regression

``` r
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)
#>                         Model Summary                          
#> --------------------------------------------------------------
#> R                       0.914       RMSE                2.409 
#> R-Squared               0.835       Coef. Var          13.051 
#> Adj. R-Squared          0.811       MSE                 6.875 
#> Pred R-Squared          0.771       MAE                 1.858 
#> --------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#> 
#>                                ANOVA                                 
#> --------------------------------------------------------------------
#>                 Sum of                                              
#>                Squares        DF    Mean Square      F         Sig. 
#> --------------------------------------------------------------------
#> Regression     940.412         4        235.103    34.195    0.0000 
#> Residual       185.635        27          6.875                     
#> Total         1126.047        31                                    
#> --------------------------------------------------------------------
#> 
#>                                   Parameter Estimates                                    
#> ----------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t        Sig      lower     upper 
#> ----------------------------------------------------------------------------------------
#> (Intercept)    27.330         8.639                  3.164    0.004     9.604    45.055 
#>        disp     0.003         0.011        0.055     0.248    0.806    -0.019     0.025 
#>          hp    -0.019         0.016       -0.212    -1.196    0.242    -0.051     0.013 
#>          wt    -4.609         1.266       -0.748    -3.641    0.001    -7.206    -2.012 
#>        qsec     0.544         0.466        0.161     1.166    0.254    -0.413     1.501 
#> ----------------------------------------------------------------------------------------
```

#### Stepwise Regression

Build regression model from a set of candidate predictor variables by
entering and removing predictors based on p values, in a stepwise manner
until there is no variable left to enter or remove any more.

#### Variable Selection

``` r
# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)
#> 
#>                                 Stepwise Selection Summary                                 
#> ------------------------------------------------------------------------------------------
#>                         Added/                   Adj.                                         
#> Step     Variable      Removed     R-Square    R-Square     C(p)        AIC         RMSE      
#> ------------------------------------------------------------------------------------------
#>    1    liver_test     addition       0.455       0.444    62.5120    771.8753    290.7604    
#>    2     alc_heavy     addition       0.567       0.550    41.3680    761.4394    259.1357    
#>    3    enzyme_test    addition       0.659       0.639    24.3380    750.5089    229.8956    
#>    4      pindex       addition       0.750       0.730     7.5370    735.7146    196.7872    
#>    5        bcs        addition       0.781       0.758     3.1920    730.6204    184.2762    
#> ------------------------------------------------------------------------------------------
```

#### Stepwise AIC Backward Regression

Build regression model from a set of candidate predictor variables by
removing predictors based on Akaike Information Criteria, in a stepwise
manner until there is no variable left to remove any more.

##### Variable Selection

``` r
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k
#> 
#> 
#>                         Backward Elimination Summary                         
#> ---------------------------------------------------------------------------
#> Variable        AIC          RSS          Sum Sq        R-Sq      Adj. R-Sq 
#> ---------------------------------------------------------------------------
#> Full Model    736.390    1825905.713    6543614.824    0.78184      0.74305 
#> alc_mod       734.407    1826477.828    6543042.709    0.78177      0.74856 
#> gender        732.494    1829435.617    6540084.920    0.78142      0.75351 
#> age           730.620    1833716.447    6535804.090    0.78091      0.75808 
#> ---------------------------------------------------------------------------
```

#### Breusch Pagan Test

Breusch Pagan test is used to test for herteroskedasticity (non-constant
error variance). It tests whether the variance of the errors from a
regression is dependent on the values of the independent variables. It
is a *χ*<sup>2</sup> test.

``` r
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model)
#> 
#>  Breusch Pagan Test for Heteroskedasticity
#>  -----------------------------------------
#>  Ho: the variance is constant            
#>  Ha: the variance is not constant        
#> 
#>              Data               
#>  -------------------------------
#>  Response : mpg 
#>  Variables: fitted values of mpg 
#> 
#>        Test Summary         
#>  ---------------------------
#>  DF            =    1 
#>  Chi2          =    1.429672 
#>  Prob > Chi2   =    0.231818
```

#### Collinearity Diagnostics

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)
#> Tolerance and Variance Inflation Factor
#> ---------------------------------------
#>   Variables Tolerance      VIF
#> 1      disp 0.1252279 7.985439
#> 2        hp 0.1935450 5.166758
#> 3        wt 0.1445726 6.916942
#> 4      qsec 0.3191708 3.133119
#> 
#> 
#> Eigenvalue and Condition Index
#> ------------------------------
#>    Eigenvalue Condition Index   intercept        disp          hp           wt
#> 1 4.721487187        1.000000 0.000123237 0.001132468 0.001413094 0.0005253393
#> 2 0.216562203        4.669260 0.002617424 0.036811051 0.027751289 0.0002096014
#> 3 0.050416837        9.677242 0.001656551 0.120881424 0.392366164 0.0377028008
#> 4 0.010104757       21.616057 0.025805998 0.777260487 0.059594623 0.7017528428
#> 5 0.001429017       57.480524 0.969796790 0.063914571 0.518874831 0.2598094157
#>           qsec
#> 1 0.0001277169
#> 2 0.0046789491
#> 3 0.0001952599
#> 4 0.0024577686
#> 5 0.9925403056
```

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, use
[StackOverflow](https://stackoverflow.com/).

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
