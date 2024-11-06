# output from stepwise backward regression is as expected

    Code
      ols_step_backward_p(model)
    Output
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_backward_p(model, progress = TRUE)
    Output
      Backward Elimination Method 
      ---------------------------
      
      Candidate Terms: 
      
      1. bcs 
      2. pindex 
      3. enzyme_test 
      4. liver_test 
      5. age 
      6. gender 
      7. alc_mod 
      8. alc_heavy 
      
      
      Variables Removed: 
      
      => alc_mod 
      => gender 
      => age 
      
      No more variables to be removed.
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_backward_p(model, details = TRUE)
    Output
      Backward Elimination Method 
      ---------------------------
      
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
      Model  => y ~ bcs + pindex + enzyme_test + liver_test + age + gender + alc_mod + alc_heavy 
      R2     => 0.782 
      
      Initiating stepwise selection... 
      
      Step     => 1 
      Removed  => alc_mod 
      Model    => y ~ bcs + pindex + enzyme_test + liver_test + age + gender + alc_heavy 
      R2       => 0.78177 
      
      Step     => 2 
      Removed  => gender 
      Model    => y ~ bcs + pindex + enzyme_test + liver_test + age + alc_heavy 
      R2       => 0.78142 
      
      Step     => 3 
      Removed  => age 
      Model    => y ~ bcs + pindex + enzyme_test + liver_test + alc_heavy 
      R2       => 0.78091 
      
      
      No more variables to be removed.
      
      Variables Removed: 
      
      => alc_mod 
      => gender 
      => age 
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
      ------------------------------------------------------------------------------------------------
      

# output from stepwise backward hierarchical regression

    Code
      ols_step_backward_p(model, hierarchical = TRUE)
    Output
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_backward_p(model, hierarchical = TRUE, progress = TRUE)
    Output
      Backward Elimination Method 
      ---------------------------
      
      Candidate Terms: 
      
      1. bcs 
      2. alc_heavy 
      3. pindex 
      4. enzyme_test 
      5. liver_test 
      6. age 
      7. gender 
      8. alc_mod 
      
      
      Variables Removed: 
      
      => alc_mod 
      => gender 
      => age 
      
      No more variables to be removed.
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
      ------------------------------------------------------------------------------------------------
      

---

    Code
      ols_step_backward_p(model, hierarchical = TRUE, details = TRUE)
    Output
      Backward Elimination Method 
      ---------------------------
      
      Candidate Terms: 
      
      1. bcs 
      2. alc_heavy 
      3. pindex 
      4. enzyme_test 
      5. liver_test 
      6. age 
      7. gender 
      8. alc_mod 
      
      
      Step   => 0 
      Model  => y ~ 1 
      R2     => 0.782 
      
      Initiating stepwise selection... 
      
        Significance Table    
      -----------------------
      Predictor      Pr(>|t|) 
      -----------------------
      bcs             0.01426 
      alc_heavy       0.00047 
      pindex            2e-05 
      enzyme_test     0.00000 
      liver_test      0.26811 
      age             0.72123 
      gender          0.78727 
      alc_mod         0.90601 
      -----------------------
      
      Step     => 1 
      Removed  => alc_mod 
      Model    => y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender 
      R2       => 0.78177 
      
        Significance Table    
      -----------------------
      Predictor      Pr(>|t|) 
      -----------------------
      bcs             0.01331 
      alc_heavy         9e-05 
      pindex            1e-05 
      enzyme_test     0.00000 
      liver_test      0.25860 
      age             0.72755 
      gender          0.78613 
      -----------------------
      
      Step     => 2 
      Removed  => gender 
      Model    => y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age 
      R2       => 0.78142 
      
        Significance Table    
      -----------------------
      Predictor      Pr(>|t|) 
      -----------------------
      bcs             0.01285 
      alc_heavy         8e-05 
      pindex            1e-05 
      enzyme_test     0.00000 
      liver_test      0.20745 
      age             0.74164 
      -----------------------
      
      Step     => 3 
      Removed  => age 
      Model    => y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test 
      R2       => 0.78091 
      
        Significance Table    
      -----------------------
      Predictor      Pr(>|t|) 
      -----------------------
      bcs             0.01248 
      alc_heavy         5e-05 
      pindex            1e-05 
      enzyme_test     0.00000 
      liver_test      0.15457 
      -----------------------
      
      
      No more variables to be removed.
      
      Variables Removed: 
      
      => alc_mod 
      => gender 
      => age 
      
      
                                   Stepwise Summary                              
      -------------------------------------------------------------------------
      Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
      -------------------------------------------------------------------------
       0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
       1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
       2      gender        732.494    748.406    581.938    0.78142    0.75351 
       3      age           730.620    744.543    579.638    0.78091    0.75808 
      -------------------------------------------------------------------------
      
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
              bcs       59.864        23.060        0.241     2.596    0.012       13.498     106.230 
        alc_heavy      317.848        71.634        0.314     4.437    0.000      173.818     461.878 
           pindex        8.924         1.808        0.380     4.935    0.000        5.288      12.559 
      enzyme_test        9.748         1.656        0.521     5.887    0.000        6.419      13.077 
       liver_test       58.064        40.144        0.156     1.446    0.155      -22.652     138.779 
      ------------------------------------------------------------------------------------------------
      

