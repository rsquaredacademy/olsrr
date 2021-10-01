test_that("output from normality test is as expected", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_snapshot(ols_test_normality(model))
})

test_that("output from ols_corr_test is as expected", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_equal(round(ols_test_correlation(model), 3), 0.97)
})

test_that("ols_test_normality returns error messages", {
  model <- glm(prog ~ female + read + science, data = hsb, family = binomial(link = 'logit'))
  expect_error(ols_test_normality(hsb$female), "y must be numeric")
  expect_error(ols_test_normality(model), "Please specify a OLS linear regression model.")
})