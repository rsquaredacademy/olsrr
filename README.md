
<!-- README.md is generated from README.Rmd. Please edit that file -->
olsrr: Tools for building OLS Regression models <img src="olsrr.jpg" align="right" />
-------------------------------------------------------------------------------------

**Author:** [Aravind Hebbali]()<br/> **License:** [MIT](https://opensource.org/licenses/MIT)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/olsrr)](https://cran.r-project.org/package=olsrr) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/olsrr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/olsrr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/olsrr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/olsrr) [![Coverage Status](https://img.shields.io/codecov/c/github/rsquaredacademy/olsrr/master.svg)](https://codecov.io/github/rsquaredacademy/olsrr?branch=master) [![](https://cranlogs.r-pkg.org/badges/grand-total/olsrr)](https://cran.r-project.org/package=olsrr) ![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

Overview
--------

The olsrr package provides following tools for teaching and learning OLS regression using R:

-   Comprehensive Regression Output
-   Variable Selection Procedures
-   Heteroskedasticity Tests
-   Collinearity Diagnostics
-   Model Fit Assessment
-   Measures of Influence
-   Residual Diagnostics
-   Variable Contribution Assessment

Installation
------------

You can install olsrr from github with:

``` r
# install olsrr from CRAN
install.packages("olsrr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/olsrr")
```

Shiny App
---------

Use `ols_launch_app()` to explore the package using a shiny app.

Vignettes
---------

-   [Quick Overview](http://www.rsquaredacademy.com/olsrr/articles/intro.html)
-   [Variable Selection Methods](http://www.rsquaredacademy.com/olsrr/articles/variable_selection.html)
-   [Residual Diagnostics](http://www.rsquaredacademy.com/olsrr/articles/residual_diagnostics.html)
-   [Heteroskedasticity](http://www.rsquaredacademy.com/olsrr/articles/heteroskedasticity.html)
-   [Measures of Influence](http://www.rsquaredacademy.com/olsrr/articles/influence_measures.html)
-   [Collinearity Diagnostics](http://www.rsquaredacademy.com/olsrr/articles/regression_diagnostics.html)

Consistent Prefix
-----------------

olsrr uses consistent prefix `ols_` for easy tab completion.

Quick Demo
----------

olsrr is built with the aim of helping those users who are new to the R language. If you know how to write a `formula` or build models using `lm`, you will find olsrr very useful. Most of the functions use an object of class `lm` as input. So you just need to build a model using `lm` and then pass it onto the functions in olsrr. Below is a quick demo:

##### Regression

``` r
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)
#>                         Model Summary                          
#> --------------------------------------------------------------
#> R                       0.914       RMSE                2.622 
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

##### Residual vs Fitted Values Plot

Plot to detect non-linearity, unequal error variances, and outliers.

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rvsp_plot(model)
```

<img src="README-rvsfplot-1.png" style="display: block; margin: auto;" />

##### DFBETAs Panel

DFBETAs measure the difference in each parameter estimate with and without the influential observation. `dfbetas_panel` creates plots to detect influential observations using DFBETAs.

``` r
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_dfbetas_panel(model)
```

<img src="README-dfbpanel-1.png" style="display: block; margin: auto;" />

##### Residual Fit Spread Plot

Plot to detect non-linearity, influential observations and outliers.

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rfs_plot(model)
```

<img src="README-rfsplot-1.png" style="display: block; margin: auto;" />

##### Breusch Pagan Test

Breusch Pagan test is used to test for herteroskedasticity (non-constant error variance). It tests whether the variance of the errors from a regression is dependent on the values of the independent variables. It is a *χ*<sup>2</sup> test.

``` r
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model)
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

##### Collinearity Diagnostics

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)
#> Tolerance and Variance Inflation Factor
#> ---------------------------------------
#> # A tibble: 4 x 3
#>   Variables Tolerance      VIF
#>       <chr>     <dbl>    <dbl>
#> 1      disp 0.1252279 7.985439
#> 2        hp 0.1935450 5.166758
#> 3        wt 0.1445726 6.916942
#> 4      qsec 0.3191708 3.133119
#> 
#> 
#> Eigenvalue and Condition Index
#> ------------------------------
#>    Eigenvalue Condition Index   intercept        disp          hp
#> 1 4.721487187        1.000000 0.000123237 0.001132468 0.001413094
#> 2 0.216562203        4.669260 0.002617424 0.036811051 0.027751289
#> 3 0.050416837        9.677242 0.001656551 0.120881424 0.392366164
#> 4 0.010104757       21.616057 0.025805998 0.777260487 0.059594623
#> 5 0.001429017       57.480524 0.969796790 0.063914571 0.518874831
#>             wt         qsec
#> 1 0.0005253393 0.0001277169
#> 2 0.0002096014 0.0046789491
#> 3 0.0377028008 0.0001952599
#> 4 0.7017528428 0.0024577686
#> 5 0.2598094157 0.9925403056
```

##### Stepwise Regression

Build regression model from a set of candidate predictor variables by entering and removing predictors based on p values, in a stepwise manner until there is no variable left to enter or remove any more.

###### Variable Selection

``` r
# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_stepwise(model)
#> We are selecting variables based on p value...
#> 1 variable(s) added....
#> 1 variable(s) added...
#> 1 variable(s) added...
#> 1 variable(s) added...
#> 1 variable(s) added...
#> No more variables to be added or removed.
#> Stepwise Selection Method                                                                  
#> 
#> Candidate Terms:                                                                           
#> 
#> 1 . bcs                                                                                    
#> 2 . pindex                                                                                 
#> 3 . enzyme_test                                                                            
#> 4 . liver_test                                                                             
#> 5 . age                                                                                    
#> 6 . gender                                                                                 
#> 7 . alc_mod                                                                                
#> 8 . alc_heavy                                                                              
#> 
#> ------------------------------------------------------------------------------------------
#>                                 Stepwise Selection Summary                                 
#> ------------------------------------------------------------------------------------------
#>                         Added/                   Adj.                                         
#> Step     Variable      Removed     R-Square    R-Square     C(p)        AIC         RMSE      
#> ------------------------------------------------------------------------------------------
#>    1    liver_test     addition       0.455       0.444    62.5120    771.8753    296.2992    
#>    2     alc_heavy     addition       0.567       0.550    41.3680    761.4394    266.6484    
#>    3    enzyme_test    addition       0.659       0.639    24.3380    750.5089    238.9145    
#>    4      pindex       addition       0.750       0.730     7.5370    735.7146    206.5835    
#>    5        bcs        addition       0.781       0.758     3.1920    730.6204    195.4544    
#> ------------------------------------------------------------------------------------------
```

###### Plot

``` r
model <- lm(y ~ ., data = surgical)
k <- ols_stepwise(model)
#> We are selecting variables based on p value...
#> 1 variable(s) added....
#> 1 variable(s) added...
#> 1 variable(s) added...
#> 1 variable(s) added...
#> 1 variable(s) added...
#> No more variables to be added or removed.
plot(k)
```

<img src="README-stepwise2-1.png" style="display: block; margin: auto;" />

##### Stepwise AIC Backward Regression

Build regression model from a set of candidate predictor variables by removing predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to remove any more.

###### Variable Selection

``` r
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_stepaic_backward(model)
k
#> 
#> 
#>                        Backward Elimination Summary                        
#> -------------------------------------------------------------------------
#> Variable        AIC          RSS          Sum Sq       R-Sq     Adj. R-Sq 
#> -------------------------------------------------------------------------
#> Full Model    736.390    1825905.713    6543614.824    0.782        0.743 
#> alc_mod       734.407    1826477.828    6543042.709    0.782        0.749 
#> gender        732.494    1829435.617    6540084.920    0.781        0.754 
#> age           730.620    1833716.447    6535804.090    0.781        0.758 
#> -------------------------------------------------------------------------

###### Plot
```

``` r
model <- lm(y ~ ., data = surgical)
k <- ols_stepaic_backward(model)
plot(k)
```

<img src="README-stepaicb2-1.png" style="display: block; margin: auto;" />

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
