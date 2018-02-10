context("ftest output")

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)

test_that("output from ftest is as expected", {
  x <- cat("\n\t F Test for Heteroskedasticity\n\t -----------------------------\n\t Ho: Variance is homogenous\n\t Ha: Variance is not homogenous\n\n\t Variables: fitted values of mpg\n\n\t      Test Summary\n\t -------------------------\n\t Num DF     =    1\n\t Den DF     =    30\n\t F          =    0.4920617\n\t Prob > F   =    0.4884154")

  expect_equivalent(print(olsrr::ols_f_test(model)), x)
})

test_that("output from ftest is as expected when rhs = TRUE", {
  x <- cat("\n F Test for Heteroskedasticity\n -----------------------------\n Ho: Variance is homogenous\n Ha: Variance is not homogenous\n\n Variables: disp hp wt qsec \n\n      Test Summary        \n -------------------------\n Num DF     =    4 \n Den DF     =    27 \n F          =    0.4594694 \n Prob > F   =    0.7647271")

  expect_equivalent(print(olsrr::ols_f_test(model, rhs = TRUE)), x)
})

test_that("output from ftest is as expected when variables are specified", {
  x <- cat("\n F Test for Heteroskedasticity\n -----------------------------\n Ho: Variance is homogenous\n Ha: Variance is not homogenous\n\n Variables: disp hp \n\n      Test Summary        \n -------------------------\n Num DF     =    2 \n Den DF     =    29 \n F          =    0.4669306 \n Prob > F   =    0.631555")

  expect_equivalent(print(olsrr::ols_f_test(model, vars = c("disp", "hp"))), x)
})
