# output from pure error anova is as expected

    Code
      ols_pure_error_anova(model)
    Output
      Lack of Fit F Test 
      -----------------
      Response :   mpg 
      Predictor:   disp 
      
                            Analysis of Variance Table                       
      ----------------------------------------------------------------------
                      DF     Sum Sq     Mean Sq     F Value        Pr(>F)    
      ----------------------------------------------------------------------
      disp             1    808.8885    808.8885    76.51266    9.380327e-10 
      Residual        30    317.1587    10.57196                             
       Lack of fit    25    304.2787    12.17115    4.724824      0.04563623 
       Pure Error      5       12.88       2.576                             
      ----------------------------------------------------------------------

