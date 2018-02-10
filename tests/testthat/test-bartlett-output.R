context("bartlett test output")

test_that("output from bartlett test is as expected", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
------------------------------------------------
Ho: Variances are equal across groups
Ha: Variances are unequal for atleast two groups

        Test Summary
 ----------------------------
 DF            =    2
 Chi2          =    3.647834
 Prob > Chi2   =    0.1613923")


  model <- lm(mpg ~ disp + hp, data = mtcars)
  resid <- residuals(model)
  cyl <- as.factor(mtcars$cyl)
  expect_equivalent(print(ols_bartlett_test(resid, group_var = cyl)), x)
})


test_that("output from bartlett test is as expected when using variables", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
------------------------------------------------
Ho: Variances are equal across groups
Ha: Variances are unequal for atleast two groups

        Data
 ---------------------
 Variables: read write

        Test Summary
 ----------------------------
 DF            =    1
 Chi2          =    1.222871
 Prob > Chi2   =    0.2687979")

  expect_equivalent(print(ols_bartlett_test(hsb$read, hsb$write)), x)
})


test_that("output from bartlett test is as expected when using formula", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
------------------------------------------------
Ho: Variances are equal across groups
Ha: Variances are unequal for atleast two groups

        Test Summary
 -----------------------------
 DF            =    2
 Chi2          =    8.39345
 Prob > Chi2   =    0.01504477")

  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)
  expect_equivalent(print(ols_bartlett_test(mpg ~ cyl, data = mt)), x)
})

test_that("output from bartlett test is as expected when using model", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
------------------------------------------------
Ho: Variances are equal across groups
Ha: Variances are unequal for atleast two groups

        Test Summary
 -----------------------------
 DF            =    2
 Chi2          =    8.39345
 Prob > Chi2   =    0.01504477")

  model <- lm(mpg ~ cyl, data = mtcars)
  expect_equivalent(print(ols_bartlett_test(model)), x)
})
