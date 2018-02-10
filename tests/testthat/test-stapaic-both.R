context("stepwise")

test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_stepaic_both(model)
  expect_equal(k$steps, 3)
  expect_equivalent(k$method, c("addition", "addition", "addition"))
  expect_equivalent(k$predictors, c("x4", "x1", "x2"))
})

test_that("stepwise selection fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_stepaic_both(m), "Please specify a OLS linear regression model.")
})

test_that("stepwise selection fails when model contains less than 2 predictors", {
  model <- lm(y ~ x1, data = cement)
  expect_error(ols_stepaic_both(model), "Please specify a model with at least 2 predictors.")
})
