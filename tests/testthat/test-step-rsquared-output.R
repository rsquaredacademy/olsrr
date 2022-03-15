model <- lm(y ~ ., data = stepdata)

test_that("output from rsquared forward regression is as expected", {
  expect_snapshot(ols_step_forward_r2(model))
  expect_snapshot(ols_step_forward_r2(model, progress = TRUE))
  expect_snapshot(ols_step_forward_r2(model, details = TRUE))
})

test_that("output from adjusted rsquared forward regression is as expected", {
  expect_snapshot(ols_step_forward_adj_r2(model))
  expect_snapshot(ols_step_forward_adj_r2(model, progress = TRUE))
  expect_snapshot(ols_step_forward_adj_r2(model, details = TRUE))
})

test_that("output from rsquared backward regression is as expected", {
  expect_snapshot(ols_step_backward_r2(model))
  expect_snapshot(ols_step_backward_r2(model, progress = TRUE))
  expect_snapshot(ols_step_backward_r2(model, details = TRUE))
})

test_that("output from adjusted rsquared backward regression is as expected", {
  expect_snapshot(ols_step_backward_adj_r2(model))
  expect_snapshot(ols_step_backward_adj_r2(model, progress = TRUE))
  expect_snapshot(ols_step_backward_adj_r2(model, details = TRUE))
})

test_that("output from rsquared both direction regression is as expected", {
  expect_snapshot(ols_step_both_r2(model))
  expect_snapshot(ols_step_both_r2(model, progress = TRUE))
  expect_snapshot(ols_step_both_r2(model, details = TRUE))
})

test_that("output from adjusted rsquared both direction regression is as expected", {
  expect_snapshot(ols_step_both_adj_r2(model))
  expect_snapshot(ols_step_both_adj_r2(model, progress = TRUE))
  expect_snapshot(ols_step_both_adj_r2(model, details = TRUE))
})