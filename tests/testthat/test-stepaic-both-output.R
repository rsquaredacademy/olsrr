test_that("output from stepAIC both direction regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_both_aic(model))
  expect_snapshot(ols_step_both_aic(model, details = TRUE))
})
