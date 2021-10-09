test_that("output from stepwise regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_both_p(model))
  expect_snapshot(ols_step_both_p(model, progress = TRUE))
  expect_snapshot(ols_step_both_p(model, details = TRUE))
})
