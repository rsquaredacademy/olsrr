# Stepwise AIC backward regression

Build regression model from a set of candidate predictor variables by
removing predictors based on akaike information criterion, in a stepwise
manner until there is no variable left to remove any more.

## Usage

``` r
ols_step_backward_aic(model, ...)

# Default S3 method
ols_step_backward_aic(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_backward_aic'
plot(x, print_plot = TRUE, details = TRUE, digits = 3, ...)
```

## Arguments

- model:

  An object of class `lm`; the model should include all candidate
  predictor variables.

- ...:

  Other arguments.

- include:

  Character or numeric vector; variables to be included in selection
  process.

- exclude:

  Character or numeric vector; variables to be excluded from selection
  process.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- steps:

  Number of steps after which the stepwise procedures should stop.

- x:

  An object of class `ols_step_backward_*`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- digits:

  Number of decimal places to display.

## Value

List containing the following components:

- model:

  final model; an object of class `lm`

- metrics:

  selection metrics

- others:

  list; info used for plotting and printing

## References

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with
S. Fourth edition. Springer.

## See also

Other backward selection procedures:
[`ols_step_backward_adj_r2()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_adj_r2.md),
[`ols_step_backward_p()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_p.md),
[`ols_step_backward_r2()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_r2.md),
[`ols_step_backward_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbc.md),
[`ols_step_backward_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbic.md)

## Examples

``` r
# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_aic(model)
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      alc_mod       734.407    752.308    583.884    0.78177    0.74856 
#>  2      gender        732.494    748.406    581.290    0.78142    0.75351 
#>  3      age           730.620    744.543    578.844    0.78091    0.75808 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.884       RMSE                 184.276 
#> R-Squared                 0.781       MSE                33957.712 
#> Adj. R-Squared            0.758       Coef. Var             27.839 
#> Pred R-Squared            0.700       AIC                  730.620 
#> MAE                     137.656       SBC                  744.543 
#> -------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                  ANOVA                                  
#> -----------------------------------------------------------------------
#>                    Sum of                                              
#>                   Squares        DF    Mean Square      F         Sig. 
#> -----------------------------------------------------------------------
#> Regression    6535804.090         5    1307160.818    34.217    0.0000 
#> Residual      1833716.447        48      38202.426                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1178.330       208.682                 -5.647    0.000    -1597.914    -758.746 
#>         bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
#>      pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
#> enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
#>  liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
#>   alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
#> ------------------------------------------------------------------------------------------------
#> 

# stepwise backward regression plot
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
plot(k)


# selection metrics
k$metrics
#>   step variable        r2    adj_r2      aic      sbc     sbic
#> 1    1  alc_mod 0.7817703 0.7485615 734.4068 752.3077 583.8836
#> 2    2   gender 0.7814169 0.7535127 732.4942 748.4061 581.2896
#> 3    3      age 0.7809054 0.7580831 730.6204 744.5433 578.8438

# final model
k$model
#> 
#> Call:
#> lm(formula = paste(response, "~", paste(preds, collapse = " + ")), 
#>     data = l)
#> 
#> Coefficients:
#> (Intercept)          bcs       pindex  enzyme_test   liver_test    alc_heavy  
#>   -1178.330       59.864        8.924        9.748       58.064      317.848  
#> 

# include or exclude variable
# force variables to be included in the selection process
ols_step_backward_aic(model, include = c("alc_mod", "gender"))
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      age           734.544    752.445    584.021    0.78121    0.74792 
#>  2      liver_test    734.502    750.413    583.297    0.77314    0.74418 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.879       RMSE                 187.514 
#> R-Squared                 0.773       MSE                35161.506 
#> Adj. R-Squared            0.744       Coef. Var             28.628 
#> Pred R-Squared            0.672       AIC                  734.502 
#> MAE                     138.127       SBC                  750.413 
#> -------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                  ANOVA                                  
#> -----------------------------------------------------------------------
#>                    Sum of                                              
#>                   Squares        DF    Mean Square      F         Sig. 
#> -----------------------------------------------------------------------
#> Regression    6470799.239         6    1078466.540    26.696    0.0000 
#> Residual      1898721.298        47      40398.325                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1332.543       187.069                 -7.123    0.000    -1708.878    -956.208 
#>         bcs       80.623        18.135        0.325     4.446    0.000       44.140     117.105 
#>      pindex       10.000         1.668        0.425     5.995    0.000        6.644      13.355 
#> enzyme_test       11.116         1.349        0.595     8.243    0.000        8.403      13.829 
#>      gender       33.509        56.149        0.042     0.597    0.554      -79.448     146.465 
#>     alc_mod        7.110        64.258        0.009     0.111    0.912     -122.161     136.381 
#>   alc_heavy      321.131        84.770        0.317     3.788    0.000      150.596     491.666 
#> ------------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_backward_aic(model, include = c(7, 6))
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      age           734.544    752.445    584.021    0.78121    0.74792 
#>  2      liver_test    734.502    750.413    583.297    0.77314    0.74418 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.879       RMSE                 187.514 
#> R-Squared                 0.773       MSE                35161.506 
#> Adj. R-Squared            0.744       Coef. Var             28.628 
#> Pred R-Squared            0.672       AIC                  734.502 
#> MAE                     138.127       SBC                  750.413 
#> -------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                  ANOVA                                  
#> -----------------------------------------------------------------------
#>                    Sum of                                              
#>                   Squares        DF    Mean Square      F         Sig. 
#> -----------------------------------------------------------------------
#> Regression    6470799.239         6    1078466.540    26.696    0.0000 
#> Residual      1898721.298        47      40398.325                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1332.543       187.069                 -7.123    0.000    -1708.878    -956.208 
#>         bcs       80.623        18.135        0.325     4.446    0.000       44.140     117.105 
#>      pindex       10.000         1.668        0.425     5.995    0.000        6.644      13.355 
#> enzyme_test       11.116         1.349        0.595     8.243    0.000        8.403      13.829 
#>      gender       33.509        56.149        0.042     0.597    0.554      -79.448     146.465 
#>     alc_mod        7.110        64.258        0.009     0.111    0.912     -122.161     136.381 
#>   alc_heavy      321.131        84.770        0.317     3.788    0.000      150.596     491.666 
#> ------------------------------------------------------------------------------------------------
#> 

# force variable to be excluded from selection process
ols_step_backward_aic(model, exclude = c("alc_heavy", "bcs"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_aic(model, exclude = c(8, 1))
#> [1] "No variables have been removed from the model."

# steps
ols_step_backward_aic(model, steps = 2)
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      alc_mod       734.407    752.308    583.884    0.78177    0.74856 
#>  2      gender        732.494    748.406    581.290    0.78142    0.75351 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.884       RMSE                 184.061 
#> R-Squared                 0.781       MSE                33878.437 
#> Adj. R-Squared            0.754       Coef. Var             28.101 
#> Pred R-Squared            0.692       AIC                  732.494 
#> MAE                     138.160       SBC                  748.406 
#> -------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                  ANOVA                                  
#> -----------------------------------------------------------------------
#>                    Sum of                                              
#>                   Squares        DF    Mean Square      F         Sig. 
#> -----------------------------------------------------------------------
#> Regression    6540084.920         6    1090014.153    28.004    0.0000 
#> Residual      1829435.617        47      38924.162                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1143.080       235.943                 -4.845    0.000    -1617.737    -668.424 
#>         bcs       61.424        23.748        0.248     2.586    0.013       13.649     109.199 
#>      pindex        8.974         1.832        0.382     4.900    0.000        5.290      12.659 
#> enzyme_test        9.852         1.700        0.527     5.794    0.000        6.431      13.273 
#>  liver_test       54.053        42.288        0.146     1.278    0.207      -31.019     139.125 
#>         age       -0.850         2.563       -0.024    -0.332    0.742       -6.007       4.307 
#>   alc_heavy      314.585        72.974        0.310     4.311    0.000      167.781     461.390 
#> ------------------------------------------------------------------------------------------------
#> 
```
