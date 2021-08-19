context("stepwise")

test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_aic(model)
  expect_equal(k$metrics$step, 1:3)
  expect_equivalent(k$metrics$method, c("addition", "addition", "addition"))
  expect_equivalent(k$metrics$variable, c("x4", "x1", "x2"))
})


