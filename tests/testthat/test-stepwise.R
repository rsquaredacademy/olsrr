context("stepwise")

test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_p(model, pent = 0.15, prem = 0.155)
  expect_equivalent(k$orders, c("x4", "x1", "x2", "x4"))
  expect_equivalent(k$method, c("addition", "addition", "addition", "removal"))
  expect_equal(k$steps, 4)
  expect_equivalent(k$predictors, c("x1", "x2"))
})

