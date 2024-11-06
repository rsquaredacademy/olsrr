# output from stepwise regression is as expected

    Code
      ols_step_both_p(model)
    Output
      
      
                                      Stepwise Summary                                
      ------------------------------------------------------------------------------
      Step    Variable             AIC        SBC       SBIC        R2       Adj. R2 
      ------------------------------------------------------------------------------
       0      Base Model         802.606    806.584    646.794    0.00000    0.00000 
       1      liver_test (+)     771.875    777.842    616.009    0.45454    0.44405 
       2      alc_heavy (+)      761.439    769.395    605.506    0.56674    0.54975 
       3      enzyme_test (+)    750.509    760.454    595.297    0.65900    0.63854 
       4      pindex (+)         735.715    747.649    582.943    0.75015    0.72975 
       5      bcs (+)            730.620    744.543    579.638    0.78091    0.75808 
      ------------------------------------------------------------------------------
      
      Final Model Output 
      ------------------
      
                                 Model Summary                            
      -------------------------------------------------------------------
      R                         0.884       RMSE                 184.276 
      R-Squared                 0.781       MSE                33957.712 
      Adj. R-Squared            0.758       Coef. Var             27.839 
      Pred R-Squared            0.700       AIC                  730.620 
      MAE                     137.656       SBC                  744.543 
      -------------------------------------------------------------------
       RMSE: Root Mean Square Error 
       MSE: Mean Square Error 
       MAE: Mean Absolute Error 
       AIC: Akaike Information Criteria 
       SBC: Schwarz Bayesian Criteria 
      
                                       ANOVA                                  
      -----------------------------------------------------------------------
                         Sum of                                              
                        Squares        DF    Mean Square      F         Sig. 
      -----------------------------------------------------------------------
      Regression    6535804.090         5    1307160.818    34.217    0.0000 
      Residual      1833716.447        48      38202.426                     
      Total         8369520.537        53                                    
      -----------------------------------------------------------------------
      
                                            Parameter Estimates                                        
      ------------------------------------------------------------------------------------------------
            model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
      ------------------------------------------------------------------------------------------------
      (Intercept)    -1178.330       208.682                 -5.647    0.000    -1597.914    -758.746 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_both_p(model, progress = TRUE)
    Output
      Stepwise Selection Method 
      -------------------------
      
      Candidate Terms: 
      
      1. bcs 
      2. pindex 
      3. enzyme_test 
      4. liver_test 
      5. age 
      6. gender 
      7. alc_mod 
      8. alc_heavy 
      
      
      Variables Added/Removed: 
      
      => liver_test added 
      => alc_heavy added 
      => enzyme_test added 
      => pindex added 
      => bcs added 
      
      No more variables to be added or removed.
      
      
                                      Stepwise Summary                                
      ------------------------------------------------------------------------------
      Step    Variable             AIC        SBC       SBIC        R2       Adj. R2 
      ------------------------------------------------------------------------------
       0      Base Model         802.606    806.584    646.794    0.00000    0.00000 
       1      liver_test (+)     771.875    777.842    616.009    0.45454    0.44405 
       2      alc_heavy (+)      761.439    769.395    605.506    0.56674    0.54975 
       3      enzyme_test (+)    750.509    760.454    595.297    0.65900    0.63854 
       4      pindex (+)         735.715    747.649    582.943    0.75015    0.72975 
       5      bcs (+)            730.620    744.543    579.638    0.78091    0.75808 
      ------------------------------------------------------------------------------
      
      Final Model Output 
      ------------------
      
                                 Model Summary                            
      -------------------------------------------------------------------
      R                         0.884       RMSE                 184.276 
      R-Squared                 0.781       MSE                33957.712 
      Adj. R-Squared            0.758       Coef. Var             27.839 
      Pred R-Squared            0.700       AIC                  730.620 
      MAE                     137.656       SBC                  744.543 
      -------------------------------------------------------------------
       RMSE: Root Mean Square Error 
       MSE: Mean Square Error 
       MAE: Mean Absolute Error 
       AIC: Akaike Information Criteria 
       SBC: Schwarz Bayesian Criteria 
      
                                       ANOVA                                  
      -----------------------------------------------------------------------
                         Sum of                                              
                        Squares        DF    Mean Square      F         Sig. 
      -----------------------------------------------------------------------
      Regression    6535804.090         5    1307160.818    34.217    0.0000 
      Residual      1833716.447        48      38202.426                     
      Total         8369520.537        53                                    
      -----------------------------------------------------------------------
      
                                            Parameter Estimates                                        
      ------------------------------------------------------------------------------------------------
            model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
      ------------------------------------------------------------------------------------------------
      (Intercept)    -1178.330       208.682                 -5.647    0.000    -1597.914    -758.746 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_both_p(model, details = TRUE)
    Output
      Stepwise Selection Method 
      -------------------------
      
      Candidate Terms: 
      
      1. bcs 
      2. pindex 
      3. enzyme_test 
      4. liver_test 
      5. age 
      6. gender 
      7. alc_mod 
      8. alc_heavy 
      
      
      Step   => 0 
      Model  => y ~ 1 
      R2     => 0 
      
      Initiating stepwise selection... 
      
      Step      => 1 
      Selected  => liver_test 
      Model     => y ~ liver_test 
      R2        => 0.455 
      
      Step      => 2 
      Selected  => alc_heavy 
      Model     => y ~ liver_test + alc_heavy 
      R2        => 0.567 
      
      Step      => 3 
      Selected  => enzyme_test 
      Model     => y ~ liver_test + alc_heavy + enzyme_test 
      R2        => 0.659 
      
      Step      => 4 
      Selected  => pindex 
      Model     => y ~ liver_test + alc_heavy + enzyme_test + pindex 
      R2        => 0.75 
      
      Step      => 5 
      Selected  => bcs 
      Model     => y ~ liver_test + alc_heavy + enzyme_test + pindex + bcs 
      R2        => 0.781 
      
      
      No more variables to be added or removed.
      
      
                                      Stepwise Summary                                
      ------------------------------------------------------------------------------
      Step    Variable             AIC        SBC       SBIC        R2       Adj. R2 
      ------------------------------------------------------------------------------
       0      Base Model         802.606    806.584    646.794    0.00000    0.00000 
       1      liver_test (+)     771.875    777.842    616.009    0.45454    0.44405 
       2      alc_heavy (+)      761.439    769.395    605.506    0.56674    0.54975 
       3      enzyme_test (+)    750.509    760.454    595.297    0.65900    0.63854 
       4      pindex (+)         735.715    747.649    582.943    0.75015    0.72975 
       5      bcs (+)            730.620    744.543    579.638    0.78091    0.75808 
      ------------------------------------------------------------------------------
      
      Final Model Output 
      ------------------
      
                                 Model Summary                            
      -------------------------------------------------------------------
      R                         0.884       RMSE                 184.276 
      R-Squared                 0.781       MSE                33957.712 
      Adj. R-Squared            0.758       Coef. Var             27.839 
      Pred R-Squared            0.700       AIC                  730.620 
      MAE                     137.656       SBC                  744.543 
      -------------------------------------------------------------------
       RMSE: Root Mean Square Error 
       MSE: Mean Square Error 
       MAE: Mean Absolute Error 
       AIC: Akaike Information Criteria 
       SBC: Schwarz Bayesian Criteria 
      
                                       ANOVA                                  
      -----------------------------------------------------------------------
                         Sum of                                              
                        Squares        DF    Mean Square      F         Sig. 
      -----------------------------------------------------------------------
      Regression    6535804.090         5    1307160.818    34.217    0.0000 
      Residual      1833716.447        48      38202.426                     
      Total         8369520.537        53                                    
      -----------------------------------------------------------------------
      
                                            Parameter Estimates                                        
      ------------------------------------------------------------------------------------------------
            model         Beta    Std. Error    Std. Beta      t        Sig         lower       upper 
      ------------------------------------------------------------------------------------------------
      (Intercept)    -1178.330       208.682                 -5.647    0.000    -1597.914    -758.746 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
      ------------------------------------------------------------------------------------------------
      

