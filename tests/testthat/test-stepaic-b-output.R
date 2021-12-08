model <- lm(y ~ ., data = surgical)

test_that("output from stepAIC backward regression is as expected", {
  expect_snapshot(ols_step_backward_aic(model))
  expect_snapshot(ols_step_backward_aic(model, progress = TRUE))
  expect_snapshot(ols_step_backward_aic(model, details = TRUE))
})

test_that("output from stepSBC backward regression is as expected", {
  expect_snapshot(ols_step_backward_sbc(model))
  expect_snapshot(ols_step_backward_sbc(model, progress = TRUE))
  expect_snapshot(ols_step_backward_sbc(model, details = TRUE))
})

test_that("output from stepSBIC backward regression is as expected", {
  expect_snapshot(ols_step_backward_sbic(model))
  expect_snapshot(ols_step_backward_sbic(model, progress = TRUE))
  expect_snapshot(ols_step_backward_sbic(model, details = TRUE))
})
