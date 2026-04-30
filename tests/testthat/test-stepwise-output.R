test_that("output from stepwise regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_both_p(model))
  expect_snapshot(ols_step_both_p(model, progress = TRUE))
  expect_snapshot(ols_step_both_p(model, details = TRUE))
})

test_that("output from stepwise regression is as expected when steps is specified", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_both_p(model, steps = 3))
  expect_snapshot(ols_step_both_p(model, progress = TRUE, steps = 3))
  expect_snapshot(ols_step_both_p(model, details = TRUE, steps = 3))
})