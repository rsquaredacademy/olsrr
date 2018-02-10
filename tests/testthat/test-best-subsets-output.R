context("best subsets regression output")

test_that("output from best subsets regression is as expected", {
  x <- cat("   Best Subsets Regression    
------------------------------
Model Index    Predictors
------------------------------
     1         wt              
     2         hp wt           
     3         hp wt qsec      
     4         disp hp wt qsec 
------------------------------

                                                  Subsets Regression Summary                                                   
-------------------------------------------------------------------------------------------------------------------------------
                       Adj.        Pred                                                                                         
Model    R-Square    R-Square    R-Square     C(p)        AIC        SBIC        SBC        MSEP      FPE       HSP       APC  
-------------------------------------------------------------------------------------------------------------------------------
  1        0.7528      0.7446      0.7087    12.4809    166.0294    74.2916    170.4266    9.8972    9.8572    0.3199    0.2801 
  2        0.8268      0.8148      0.7811     2.3690    156.6523    66.5755    162.5153    7.4314    7.3563    0.2402    0.2091 
  3        0.8348      0.8171       0.782     3.0617    157.1426    67.7238    164.4713    7.6140    7.4756    0.2461    0.2124 
  4        0.8351      0.8107       0.771     5.0000    159.0696    70.0408    167.8640    8.1810    7.9497    0.2644    0.2259 
-------------------------------------------------------------------------------------------------------------------------------
AIC: Akaike Information Criteria 
 SBIC: Sawa's Bayesian Information Criteria 
 SBC: Schwarz Bayesian Criteria 
 MSEP: Estimated error of prediction, assuming multivariate normality 
 FPE: Final Prediction Error 
 HSP: Hocking's Sp 
 APC: Amemiya Prediction Criteria")

  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_equivalent(print(ols_best_subset(model)), x)
})
