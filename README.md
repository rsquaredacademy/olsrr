
<!-- README.md is generated from README.Rmd. Please edit that file -->

# olsrr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/olsrr)](https://CRAN.R-project.org/package=olsrr)
[![R-CMD-check](https://github.com/rsquaredacademy/olsrr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rsquaredacademy/olsrr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rsquaredacademy/olsrr/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/rsquaredacademy/olsrr?branch=develop)
<!-- badges: end -->

olsrr helps analysts build and check linear regression models, with
tools for diagnostics, variable selection and clear reports.

## Installation

``` r
# Release version from CRAN
install.packages("olsrr")

# Development version from GitHub
# install.packages("pak")
pak::pak("rsquaredacademy/olsrr")
```

## Quick example

### Read the regression output

Can grocery and household paper spending explain milk spending?

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

The two spend variables explain 53.1% of the variation in milk spend.
Grocery spend has a positive coefficient of 0.62; household paper spend
is not significant (p = 0.35).

### Check the residuals

Is the error pattern random, or does it hide structure?

``` r
ols_plot_resid_fit(model)
```

![](README-resid-fit-1.png)<!-- -->

Residuals fan out as fitted milk spend rises, which casts doubt on the
constant variance assumption.

### Select variables by AIC

Which spend variables belong in the model?

``` r
ols_step_both_aic(lm(milk ~ fresh + grocery + frozen + detergents_paper + delicassen, data = wholesale_customers))
#> 
#> 
#>                                     Stepwise Summary                                    
#> --------------------------------------------------------------------------------------
#> Step    Variable                  AIC         SBC         SBIC        R2       Adj. R2 
#> --------------------------------------------------------------------------------------
#>  0      Base Model              9089.455    9097.629    7838.859    0.00000    0.00000 
#>  1      grocery (+)             8758.803    8771.063    7509.478    0.53047    0.52940 
#>  2      delicassen (+)          8691.088    8707.435    7442.376    0.59927    0.59744 
#>  3      detergents_paper (+)    8689.441    8709.875    7440.780    0.60258    0.59984 
#>  4      frozen (+)              8687.574    8712.095    7439.003    0.60606    0.60243 
#> --------------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                              Model Summary                              
#> -----------------------------------------------------------------------
#> R                          0.778       RMSE                   4653.538 
#> R-Squared                  0.606       MSE                21655415.516 
#> Adj. R-Squared             0.602       Coef. Var                80.285 
#> Pred R-Squared             0.566       AIC                    8687.574 
#> MAE                     2367.923       SBC                    8712.095 
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
#> Regression    14492209868.247          4    3623052467.062    167.305    < 0.0001 
#> Residual       9420105749.641        435      21655415.516                        
#> Total         23912315617.889        439                                          
#> ----------------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                          
#> ----------------------------------------------------------------------------------------------------
#>            model       Beta    Std. Error    Std. Beta      t        Sig          lower       upper 
#> ----------------------------------------------------------------------------------------------------
#>      (Intercept)    487.099       331.482                 1.469      0.1424    -164.407    1138.605 
#>          grocery      0.395         0.066        0.509    5.958    < 0.0001       0.265       0.525 
#>       delicassen      0.690         0.091        0.264    7.577    < 0.0001       0.511       0.869 
#> detergents_paper      0.281         0.130        0.182    2.156      0.0316       0.025       0.538 
#>           frozen      0.099         0.051        0.065    1.960      0.0507       0.000       0.199 
#> ----------------------------------------------------------------------------------------------------
```

The search adds four variables and lowers AIC from 9089.5 to 8687.6,
with an adjusted R-squared of 0.60.

## Learn more

- [Documentation site](https://olsrr.rsquaredacademy.com)
- Vignettes:
  - [Can product spending explain milk
    spending?](https://olsrr.rsquaredacademy.com/articles/getting-started.html)
  - [Read a customer-spending regression
    model](https://olsrr.rsquaredacademy.com/articles/regression-output.html)
  - [What does a customer-spending model
    miss?](https://olsrr.rsquaredacademy.com/articles/residual-diagnostics.html)
  - [What do variance and normality tests
    establish?](https://olsrr.rsquaredacademy.com/articles/variance-normality-tests.html)
  - [Collinearity Diagnostics, Model Fit & Variable
    Contribution](https://olsrr.rsquaredacademy.com/articles/regression_diagnostics.html)
  - [Measures of
    Influence](https://olsrr.rsquaredacademy.com/articles/influence_measures.html)
  - [Variable Selection
    Methods](https://olsrr.rsquaredacademy.com/articles/variable_selection.html)
  - [Media](https://olsrr.rsquaredacademy.com/articles/reference-media.html)
- [NEWS](https://olsrr.rsquaredacademy.com/news/index.html)

## Getting help and contributing

**Help:** open an
[issue](https://github.com/rsquaredacademy/olsrr/issues) with a minimal
reproducible example, built with
[reprex](https://reprex.tidyverse.org/).

**Contributing:** read the [contributing
guide](https://github.com/rsquaredacademy/olsrr/blob/develop/.github/CONTRIBUTING.md).

**Code of Conduct:** Please note that the olsrr project is released with
a [Contributor Code of
Conduct](https://olsrr.rsquaredacademy.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

**Citation:** run `citation("olsrr")`. The [authors
page](https://olsrr.rsquaredacademy.com/authors.html) lists the package
authors.
