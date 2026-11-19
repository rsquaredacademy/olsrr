# Stepwise Adjusted R-Squared forward regression

Build regression model from a set of candidate predictor variables by
entering predictors based on adjusted r-squared, in a stepwise manner
until there is no variable left to enter any more.

## Usage

``` r
ols_step_forward_adj_r2(model, ...)

# Default S3 method
ols_step_forward_adj_r2(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_forward_adj_r2'
plot(x, print_plot = TRUE, details = TRUE, digits = 3, ...)
```

## Arguments

- model:

  An object of class `lm`.

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

  An object of class `ols_step_forward_*`.

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

Other forward selection procedures:
[`ols_step_forward_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_forward_aic.md),
[`ols_step_forward_p()`](https://olsrr.rsquaredacademy.com/reference/ols_step_forward_p.md),
[`ols_step_forward_r2()`](https://olsrr.rsquaredacademy.com/reference/ols_step_forward_r2.md),
[`ols_step_forward_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_step_forward_sbc.md),
[`ols_step_forward_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_forward_sbic.md)

## Examples

``` r
# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_adj_r2(model)
#> 
#> 
#>                               Stepwise Summary                              
#> --------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2 
#> --------------------------------------------------------------------------
#>  0      Base Model     802.606    806.584    646.794    0.00000    0.00000 
#>  1      liver_test     771.875    777.842    616.009    0.45454    0.44405 
#>  2      alc_heavy      761.439    769.395    605.506    0.56674    0.54975 
#>  3      enzyme_test    750.509    760.454    595.297    0.65900    0.63854 
#>  4      pindex         735.715    747.649    582.943    0.75015    0.72975 
#>  5      bcs            730.620    744.543    579.638    0.78091    0.75808 
#> --------------------------------------------------------------------------
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
#>  liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
#>   alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
#> enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
#>      pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
#>         bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
#> ------------------------------------------------------------------------------------------------
#> 

# stepwise forward regression plot
k <- ols_step_forward_adj_r2(model)
plot(k)


# selection metrics
k$metrics
#>   step    variable        r2    adj_r2      aic      sbc     sbic
#> 1    1  liver_test 0.4545389 0.4440492 771.8753 777.8423 616.0089
#> 2    2   alc_heavy 0.5667409 0.5497504 761.4394 769.3953 605.5062
#> 3    3 enzyme_test 0.6590000 0.6385400 750.5089 760.4538 595.2974
#> 4    4      pindex 0.7501457 0.7297495 735.7146 747.6485 582.9426
#> 5    5         bcs 0.7809054 0.7580831 730.6204 744.5433 579.6377

# extract final model
k$model
#> 
#> Call:
#> lm(formula = paste(response, "~", paste(preds, collapse = " + ")), 
#>     data = l)
#> 
#> Coefficients:
#> (Intercept)   liver_test    alc_heavy  enzyme_test       pindex          bcs  
#>   -1178.330       58.064      317.848        9.748        8.924       59.864  
#> 

# include or exclude variables
# force variable to be included in selection process
ols_step_forward_adj_r2(model, include = c("age"))
#> 
#> 
#>                               Stepwise Summary                               
#> ---------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2  
#> ---------------------------------------------------------------------------
#>  0      Base Model     803.834    809.801    646.572    0.01420    -0.00476 
#>  1      liver_test     773.831    781.787    616.928    0.45498     0.43361 
#>  2      alc_heavy      763.110    773.055    606.421    0.56938     0.54354 
#>  3      enzyme_test    752.416    764.350    596.755    0.65959     0.63180 
#>  4      pindex         737.680    751.603    585.012    0.75030     0.72429 
#>  5      bcs            732.494    748.406    581.938    0.78142     0.75351 
#> ---------------------------------------------------------------------------
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
#>         age       -0.850         2.563       -0.024    -0.332    0.742       -6.007       4.307 
#>  liver_test       54.053        42.288        0.146     1.278    0.207      -31.019     139.125 
#>   alc_heavy      314.585        72.974        0.310     4.311    0.000      167.781     461.390 
#> enzyme_test        9.852         1.700        0.527     5.794    0.000        6.431      13.273 
#>      pindex        8.974         1.832        0.382     4.900    0.000        5.290      12.659 
#>         bcs       61.424        23.748        0.248     2.586    0.013       13.649     109.199 
#> ------------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_forward_adj_r2(model, include = c(5))
#> 
#> 
#>                               Stepwise Summary                               
#> ---------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2  
#> ---------------------------------------------------------------------------
#>  0      Base Model     803.834    809.801    646.572    0.01420    -0.00476 
#>  1      liver_test     773.831    781.787    616.928    0.45498     0.43361 
#>  2      alc_heavy      763.110    773.055    606.421    0.56938     0.54354 
#>  3      enzyme_test    752.416    764.350    596.755    0.65959     0.63180 
#>  4      pindex         737.680    751.603    585.012    0.75030     0.72429 
#>  5      bcs            732.494    748.406    581.938    0.78142     0.75351 
#> ---------------------------------------------------------------------------
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
#>         age       -0.850         2.563       -0.024    -0.332    0.742       -6.007       4.307 
#>  liver_test       54.053        42.288        0.146     1.278    0.207      -31.019     139.125 
#>   alc_heavy      314.585        72.974        0.310     4.311    0.000      167.781     461.390 
#> enzyme_test        9.852         1.700        0.527     5.794    0.000        6.431      13.273 
#>      pindex        8.974         1.832        0.382     4.900    0.000        5.290      12.659 
#>         bcs       61.424        23.748        0.248     2.586    0.013       13.649     109.199 
#> ------------------------------------------------------------------------------------------------
#> 

# force variable to be excluded from selection process
ols_step_forward_adj_r2(model, exclude = c("liver_test"))
#> 
#> 
#>                               Stepwise Summary                              
#> --------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2 
#> --------------------------------------------------------------------------
#>  0      Base Model     802.606    806.584    646.794    0.00000    0.00000 
#>  1      enzyme_test    782.629    788.596    626.220    0.33435    0.32154 
#>  2      bcs            766.271    774.226    609.940    0.52619    0.50761 
#>  3      pindex         746.376    756.320    591.702    0.68413    0.66518 
#>  4      alc_heavy      730.924    742.858    579.087    0.77136    0.75269 
#> --------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.878       RMSE                 188.249 
#> R-Squared                 0.771       MSE                35437.709 
#> Adj. R-Squared            0.753       Coef. Var             28.147 
#> Pred R-Squared            0.695       AIC                  730.924 
#> MAE                     140.619       SBC                  742.858 
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
#> Regression    6455884.265         4    1613971.066    41.327    0.0000 
#> Residual      1913636.272        49      39053.801                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1334.424       180.589                 -7.389    0.000    -1697.332    -971.516 
#> enzyme_test       11.243         1.308        0.601     8.596    0.000        8.614      13.871 
#>         bcs       81.439        17.781        0.329     4.580    0.000       45.706     117.171 
#>      pindex       10.131         1.622        0.431     6.246    0.000        6.871      13.390 
#>   alc_heavy      312.777        72.341        0.309     4.324    0.000      167.402     458.152 
#> ------------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_forward_adj_r2(model, exclude = c(4))
#> 
#> 
#>                               Stepwise Summary                              
#> --------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2 
#> --------------------------------------------------------------------------
#>  0      Base Model     802.606    806.584    646.794    0.00000    0.00000 
#>  1      enzyme_test    782.629    788.596    626.220    0.33435    0.32154 
#>  2      bcs            766.271    774.226    609.940    0.52619    0.50761 
#>  3      pindex         746.376    756.320    591.702    0.68413    0.66518 
#>  4      alc_heavy      730.924    742.858    579.087    0.77136    0.75269 
#> --------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.878       RMSE                 188.249 
#> R-Squared                 0.771       MSE                35437.709 
#> Adj. R-Squared            0.753       Coef. Var             28.147 
#> Pred R-Squared            0.695       AIC                  730.924 
#> MAE                     140.619       SBC                  742.858 
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
#> Regression    6455884.265         4    1613971.066    41.327    0.0000 
#> Residual      1913636.272        49      39053.801                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1334.424       180.589                 -7.389    0.000    -1697.332    -971.516 
#> enzyme_test       11.243         1.308        0.601     8.596    0.000        8.614      13.871 
#>         bcs       81.439        17.781        0.329     4.580    0.000       45.706     117.171 
#>      pindex       10.131         1.622        0.431     6.246    0.000        6.871      13.390 
#>   alc_heavy      312.777        72.341        0.309     4.324    0.000      167.402     458.152 
#> ------------------------------------------------------------------------------------------------
#> 

# include & exclude variables in the selection process
ols_step_forward_adj_r2(model, include = c("age"), exclude = c("liver_test"))
#> 
#> 
#>                               Stepwise Summary                               
#> ---------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2  
#> ---------------------------------------------------------------------------
#>  0      Base Model     803.834    809.801    646.572    0.01420    -0.00476 
#>  1      enzyme_test    783.607    791.563    626.048    0.34683     0.32121 
#>  2      bcs            767.078    777.023    609.973    0.53654     0.50873 
#>  3      pindex         747.171    759.105    592.354    0.69109     0.66588 
#>  4      alc_heavy      732.339    746.262    580.934    0.77382     0.75026 
#> ---------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.880       RMSE                 187.233 
#> R-Squared                 0.774       MSE                35056.145 
#> Adj. R-Squared            0.750       Coef. Var             28.286 
#> Pred R-Squared            0.688       AIC                  732.339 
#> MAE                     140.528       SBC                  746.262 
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
#> Regression    6476488.730         5    1295297.746    32.844    0.0000 
#> Residual      1893031.807        48      39438.163                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1237.653       225.517                 -5.488    0.000    -1691.085    -784.221 
#>         age       -1.787         2.473       -0.050    -0.723    0.473       -6.759       3.184 
#> enzyme_test       11.244         1.314        0.601     8.555    0.000        8.601      13.887 
#>         bcs       81.587        17.870        0.329     4.566    0.000       45.657     117.516 
#>      pindex       10.062         1.633        0.428     6.163    0.000        6.779      13.344 
#>   alc_heavy      306.655        73.188        0.303     4.190    0.000      159.500     453.809 
#> ------------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_forward_adj_r2(model, include = c(5), exclude = c(4))
#> 
#> 
#>                               Stepwise Summary                               
#> ---------------------------------------------------------------------------
#> Step    Variable         AIC        SBC       SBIC        R2       Adj. R2  
#> ---------------------------------------------------------------------------
#>  0      Base Model     803.834    809.801    646.572    0.01420    -0.00476 
#>  1      enzyme_test    783.607    791.563    626.048    0.34683     0.32121 
#>  2      bcs            767.078    777.023    609.973    0.53654     0.50873 
#>  3      pindex         747.171    759.105    592.354    0.69109     0.66588 
#>  4      alc_heavy      732.339    746.262    580.934    0.77382     0.75026 
#> ---------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.880       RMSE                 187.233 
#> R-Squared                 0.774       MSE                35056.145 
#> Adj. R-Squared            0.750       Coef. Var             28.286 
#> Pred R-Squared            0.688       AIC                  732.339 
#> MAE                     140.528       SBC                  746.262 
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
#> Regression    6476488.730         5    1295297.746    32.844    0.0000 
#> Residual      1893031.807        48      39438.163                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                       Parameter Estimates                                        
#> ------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
#> ------------------------------------------------------------------------------------------------
#> (Intercept)    -1237.653       225.517                 -5.488    0.000    -1691.085    -784.221 
#>         age       -1.787         2.473       -0.050    -0.723    0.473       -6.759       3.184 
#> enzyme_test       11.244         1.314        0.601     8.555    0.000        8.601      13.887 
#>         bcs       81.587        17.870        0.329     4.566    0.000       45.657     117.516 
#>      pindex       10.062         1.633        0.428     6.163    0.000        6.779      13.344 
#>   alc_heavy      306.655        73.188        0.303     4.190    0.000      159.500     453.809 
#> ------------------------------------------------------------------------------------------------
#> 

# steps
ols_step_forward_adj_r2(model, steps = 2)
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Base Model    802.606    806.584    646.794    0.00000    0.00000 
#>  1      liver_test    771.875    777.842    616.009    0.45454    0.44405 
#>  2      alc_heavy     761.439    769.395    605.506    0.56674    0.54975 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.753       RMSE                 259.136 
#> R-Squared                 0.567       MSE                67151.310 
#> Adj. R-Squared            0.550       Coef. Var             37.979 
#> Pred R-Squared            0.487       AIC                  761.439 
#> MAE                     187.393       SBC                  769.395 
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
#> Regression    4743349.776         2    2371674.888    33.356    0.0000 
#> Residual      3626170.761        51      71101.387                     
#> Total         8369520.537        53                                    
#> -----------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                      
#> --------------------------------------------------------------------------------------------
#>       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
#> --------------------------------------------------------------------------------------------
#> (Intercept)     -5.069       100.828                 -0.050    0.960    -207.490    197.352 
#>  liver_test    234.597        34.491        0.632     6.802    0.000     165.353    303.841 
#>   alc_heavy    342.183        94.156        0.338     3.634    0.001     153.157    531.208 
#> --------------------------------------------------------------------------------------------
#> 
```
