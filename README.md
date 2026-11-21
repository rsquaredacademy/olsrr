
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
# annual spend of 440 wholesale customers (UCI, CC BY 4.0; see ?wholesale_customers)
model <- lm(milk ~ grocery + detergents_paper, data = wholesale_customers)
ols_regress(model)
#>                              Model Summary                              
#> -----------------------------------------------------------------------
#> R                          0.729       RMSE                   5063.711 
#> R-Squared                  0.531       MSE                25641169.793 
#> Adj. R-Squared             0.529       Coef. Var                87.362 
#> Pred R-Squared             0.490       AIC                    8759.928 
#> MAE                     2563.777       SBC                    8776.275 
#> -----------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                       ANOVA                                        
#> ----------------------------------------------------------------------------------
#>                        Sum of                                                     
#>                       Squares         DF       Mean Square       F           Sig. 
#> ----------------------------------------------------------------------------------
#> Regression    12707124418.370          2    6353562209.185    247.788    < 0.0001 
#> Residual      11205191199.519        437      25641169.793                        
#> Total         23912315617.889        439                                          
#> ----------------------------------------------------------------------------------
#> 
#>                                          Parameter Estimates                                          
#> -----------------------------------------------------------------------------------------------------
#>            model        Beta    Std. Error    Std. Beta      t         Sig         lower       upper 
#> -----------------------------------------------------------------------------------------------------
#>      (Intercept)    1198.484       332.724                  3.602      0.0004    544.545    1852.423 
#>          grocery       0.623         0.067        0.802     9.333    < 0.0001      0.492       0.754 
#> detergents_paper      -0.124         0.133       -0.080    -0.933      0.3515     -0.386       0.137 
#> -----------------------------------------------------------------------------------------------------
```

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, ask on Stack Overflow with the `r` tag.

## Code of Conduct

Please note that the olsrr project is released with a [Contributor Code
of Conduct](https://olsrr.rsquaredacademy.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
