test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_p(model, pent = 0.15, prem = 0.155)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2", "x4"), ignore_attr = TRUE)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "removal"), ignore_attr = TRUE)
  expect_equal(k$metrics$step, 1:4)
})

