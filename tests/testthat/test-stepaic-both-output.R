model <- lm(y ~ ., data = surgical)

test_that("output from stepAIC both direction regression is as expected", {
  x <- cat("

                                    Stepwise Summary
--------------------------------------------------------------------------------------
Variable        Method       AIC          ESS          Sum Sq       R-Sq     Adj. R-Sq
--------------------------------------------------------------------------------------
liver_test     addition    771.875    4565248.060    3804272.477    0.455        0.444
alc_heavy      addition    761.439    3626170.761    4743349.776    0.567        0.550
enzyme_test    addition    750.509    2854006.401    5515514.136    0.659        0.639
pindex         addition    735.715    2091160.477    6278360.060    0.750        0.730
bcs            addition    730.620    1833716.447    6535804.090    0.781        0.758
--------------------------------------------------------------------------------------")


  expect_output(print(ols_step_both_aic(model)), x)
})


test_that("output from stepAIC both direction regression is as expected when details == TRUE", {
  x <- cat(" y ~ liver_test + alc_heavy



 Step 3 : AIC = 750.5089
 y ~ liver_test + alc_heavy + enzyme_test



 Step 4 : AIC = 735.7146
 y ~ liver_test + alc_heavy + enzyme_test + pindex



 Step 5 : AIC = 730.6204
 y ~ liver_test + alc_heavy + enzyme_test + pindex + bcs

No more variables to be added or removed.


                                    Stepwise Summary
--------------------------------------------------------------------------------------
Variable        Method       AIC          ESS          Sum Sq       R-Sq     Adj. R-Sq
--------------------------------------------------------------------------------------
liver_test     addition    771.875    4565248.060    3804272.477    0.455        0.444
alc_heavy      addition    761.439    3626170.761    4743349.776    0.567        0.550
enzyme_test    addition    750.509    2854006.401    5515514.136    0.659        0.639
pindex         addition    735.715    2091160.477    6278360.060    0.750        0.730
bcs            addition    730.620    1833716.447    6535804.090    0.781        0.758
--------------------------------------------------------------------------------------")


  expect_output(print(ols_step_both_aic(model, details = TRUE)), x)
})
