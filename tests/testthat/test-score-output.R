model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("when fitted.values == TRUE, fitted values from the regression\n\tare used for the test", {
  x <- cat("
 Score Test for Heteroskedasticity
 ---------------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: fitted values of mpg

        Test Summary
 ----------------------------
 DF            =    1
 Chi2          =    1.268973
 Prob > Chi2   =    0.2599594")

  expect_equal(print(ols_test_score(model)), x, ignore_attr = TRUE)
})

test_that("when fitted.values == TRUE and rhs == TRUE, predictors from the\n\tmodel are used for the test", {
  x <- cat("
 Score Test for Heteroskedasticity
 ---------------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: disp hp wt drat qsec

       Test Summary
 ---------------------------
 DF            =    5
 Chi2          =    2.515705
 Prob > Chi2   =    0.774128")

  expect_output(print(ols_test_score(model, rhs = TRUE)), x)
})


test_that("when vars != NULL, variables specified from the are\n\tused for the test", {
  x <- cat("
 Score Test for Heteroskedasticity
 ---------------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: disp hp

        Test Summary
 ----------------------------
 DF            =    2
 Chi2          =    0.9690651
 Prob > Chi2   =    0.6159851")

  expect_output(print(ols_test_score(model, vars = c("disp", "hp"))), x)
})


test_that("when vars != NULL and rhs == TRUE, predictors in the model are\n\tused for the test", {
  x <- cat("
 Score Test for Heteroskedasticity
 ---------------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: disp hp wt drat qsec

       Test Summary
 ---------------------------
 DF            =    5
 Chi2          =    2.515705
 Prob > Chi2   =    0.774128")

  expect_output(print(ols_test_score(model, rhs = TRUE, vars = c("disp", "hp"))), x)
})
