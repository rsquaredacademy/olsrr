context("step_backward")

test_that("backward elimination output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_backward_p(model)
  expect_equal(k$steps, 1)
  expect_equal(k$removed, c("x3"))
})

test_that("backward selection fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_step_backward_p(m), "Please specify a OLS regression model.")
})

test_that("backward selection fails when penter is not between 0 and 1", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  expect_error(ols_step_backward_p(model, prem = -0.5), "p value for removing variables from the model must be between 0 and 1.")
  expect_error(ols_step_backward_p(model, prem = 1.5), "p value for removing variables from the model must be between 0 and 1.")
})

test_that("backward selection fails when model contains less than 2 predictors", {
  model <- lm(y ~ x1, data = cement)
  expect_error(ols_step_backward_p(model), "Please specify a model with at least 2 predictors.")
})
