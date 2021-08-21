test_that("output from stepAIC forward regression is as expected", {
  local_edition(3)
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_forward_aic(model))
  expect_snapshot(ols_step_forward_aic(model, progress = TRUE))
  expect_snapshot(ols_step_forward_aic(model, details = TRUE))
})