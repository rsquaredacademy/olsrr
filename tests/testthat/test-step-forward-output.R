test_that("output from stepwise forward regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_forward_p(model))
  expect_snapshot(ols_step_forward_p(model, details = TRUE))
})
