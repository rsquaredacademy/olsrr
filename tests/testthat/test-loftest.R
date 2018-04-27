context("pure_error_anova")

test_that("output from pure_error_anova matches expected result", {
  model <- lm(mpg ~ hp, data = mtcars)
  k <- ols_pure_error_anova(model)
  act <- k$lackoffit
  exp <- 430.32
  expect_equal(round(act, 2), exp)

  act <- k$pure_error
  exp <- 17.36
  expect_equal(round(act, 2), exp)

  model <- lm(mpg ~ disp, data = mtcars)
  k <- ols_pure_error_anova(model)
  act <- k$lackoffit
  exp <- 304.28
  expect_equal(round(act, 2), exp)

  act <- k$pure_error
  exp <- 12.88
  expect_equal(round(act, 2), exp)
})


