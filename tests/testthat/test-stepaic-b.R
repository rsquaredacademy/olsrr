context("stepaic_backward")

test_that("output from stepaic_backward matches the expected outptu", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_step_backward_aic(model)
  expect_equal(k$steps, 2)
  expect_equivalent(k$predictors, c("disp", "drat"))
  expect_equivalent(round(k$aics, 3), c(158.584, 156.731, 156.652))
  expect_equivalent(round(k$ess, 3), c(182.838, 183.682, 195.048))
  expect_equivalent(round(k$rss, 3), c(943.210, 942.365, 930.999))
  expect_equivalent(round(k$rsq, 3), c(0.838, 0.837, 0.827))
  expect_equivalent(round(k$arsq, 3), c(0.814, 0.819, 0.815))
})

test_that("backward selection fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_step_backward_aic(m), "Please specify a OLS linear regression model.")
})

test_that("backward selection fails when model contains less than 2 predictors", {
  model <- lm(y ~ x1, data = cement)
  expect_error(ols_step_backward_aic(model), "Please specify a model with at least 2 predictors.")
})
