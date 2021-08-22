test_that("output from stepAIC backward regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_backward_aic(model))
  expect_snapshot(ols_step_backward_aic(model, details = TRUE))
})
