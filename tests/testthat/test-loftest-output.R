context('pure error anova output')

test_that('output from pure error anova is as expected', {

  x <- cat("Lack of Fit F Test 
-----------------
Response :   mpg 
Predictor:   disp 

                      Analysis of Variance Table                       
----------------------------------------------------------------------
                DF     Sum Sq     Mean Sq     F Value        Pr(>F)    
----------------------------------------------------------------------
disp             1    808.8885    808.8885    314.0095    1.934413e-17 
Residual        30    317.1587    10.57196                             
 Lack of fit    25    304.2787    12.17115    4.724824      0.04563623 
 Pure Error      5       12.88       2.576                             
----------------------------------------------------------------------")

  model <- lm(mpg ~ disp, data = mtcars)	
  expect_equivalent(print(ols_pure_error_anova(model)), x)

})