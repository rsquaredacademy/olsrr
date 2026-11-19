# Model Performance

Evaluate performance of regression models.

## Usage

``` r
ols_model_performance(model, ...)
```

## Arguments

- model:

  An object of class `lm`.

- ...:

  Other arguments.

## Value

`ols_model_performance` returns an object of class
`"ols_model_performance"`. An object of class `"ols_regress"` is a list
containing the following components:

- r:

  square root of rsquare, correlation between observed and predicted
  values of dependent variable

- rsq:

  coefficient of determination or r-square

- adjr:

  adjusted rsquare

- prsq:

  predicted rsquare

- aic:

  akaike information criteria

- sbc:

  bayesian information criteria

- sbic:

  sawa bayesian information criteria

- rmse:

  root mean squared error

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# model performance
ols_model_performance(model)
#>               Model Performance               
#> ---------------------------------------------
#> R                 0.909       AIC     158.643 
#> R-Squared         0.827       SBC     165.972 
#> Adj. R-Squared    0.808       SBIC     68.933 
#> Pred R-Squared    0.768       RMSE      2.468 
#> ---------------------------------------------
#>   AIC: Akaike Information Criteria 
#>   SBC: Schwarz Bayesian Criteria 
#>  SBIC: Sawa's Bayesian Criteria 
#>  RMSE: Root Mean Square Error 
#> 
```
