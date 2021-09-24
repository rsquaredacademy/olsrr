test_that("output from pure error anova is as expected", {
  model <- lm(mpg ~ disp, data = mtcars)
  expect_snapshot(ols_pure_error_anova(model))
})
