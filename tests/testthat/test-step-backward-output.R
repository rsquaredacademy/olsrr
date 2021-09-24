test_that("output from stepwise backward regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_backward_p(model))
  expect_snapshot(ols_step_backward_p(model, details = TRUE))
})
