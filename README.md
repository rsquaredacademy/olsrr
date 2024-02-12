
<!-- README.md is generated from README.Rmd. Please edit that file -->

# olsrr

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/olsrr)](https://cran.r-project.org/package=olsrr)
[![R build
status](https://github.com/rsquaredacademy/olsrr/workflows/R-CMD-check/badge.svg)](https://github.com/rsquaredacademy/olsrr/actions)
[![Coverage
status](https://codecov.io/gh/rsquaredacademy/olsrr/branch/master/graph/badge.svg)](https://app.codecov.io/github/rsquaredacademy/olsrr?branch=master)
<!-- badges: end -->

## Overview

The olsrr package provides following tools for building OLS regression
models using R:

- Comprehensive Regression Output
- Variable Selection Procedures
- Heteroskedasticity Tests
- Collinearity Diagnostics
- Model Fit Assessment
- Measures of Influence
- Residual Diagnostics
- Variable Contribution Assessment

## Installation

``` r
# Install release version from CRAN
install.packages("olsrr")

# Install development version from GitHub
# install.packages("pak")
pak::pak("rsquaredacademy/olsrr")
```

## Articles

- [Quick
  Overview](https://olsrr.rsquaredacademy.com/articles/intro.html)
- [Variable Selection
  Methods](https://olsrr.rsquaredacademy.com/articles/variable_selection.html)
- [Residual
  Diagnostics](https://olsrr.rsquaredacademy.com/articles/residual_diagnostics.html)
- [Heteroskedasticity](https://olsrr.rsquaredacademy.com/articles/heteroskedasticity.html)
- [Measures of
  Influence](https://olsrr.rsquaredacademy.com/articles/influence_measures.html)
- [Collinearity
  Diagnostics](https://olsrr.rsquaredacademy.com/articles/regression_diagnostics.html)

## Usage

olsrr uses consistent prefix `ols_` for easy tab completion. If you know
how to write a `formula` or build models using `lm`, you will find olsrr
very useful. Most of the functions use an object of class `lm` as input.
So you just need to build a model using `lm` and then pass it onto the
functions in olsrr. Below is a quick demo:

#### Regression

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_regress(model)
#>                          Model Summary                          
#> ---------------------------------------------------------------
#> R                       0.914       RMSE                 2.409 
#> R-Squared               0.835       MSE                  6.875 
#> Adj. R-Squared          0.811       Coef. Var           13.051 
#> Pred R-Squared          0.771       AIC                159.070 
#> MAE                     1.858       SBC                167.864 
#> ---------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
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

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, use
[StackOverflow](https://stackoverflow.com/).

## Code of Conduct

Please note that the olsrr project is released with a [Contributor Code
of Conduct](https://olsrr.rsquaredacademy.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
