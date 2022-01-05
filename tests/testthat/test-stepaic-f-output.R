model <- lm(y ~ ., data = surgical)

test_that("output from stepAIC forward regression is as expected", {
  expect_snapshot(ols_step_forward_aic(model))
  expect_snapshot(ols_step_forward_aic(model, progress = TRUE))
  expect_snapshot(ols_step_forward_aic(model, details = TRUE))
  expect_snapshot(ols_step_forward_aic(model, include = "pindex", details = TRUE))
})

test_that("output from stepSBC forward regression is as expected", {
  expect_snapshot(ols_step_forward_sbc(model))
  expect_snapshot(ols_step_forward_sbc(model, progress = TRUE))
  expect_snapshot(ols_step_forward_sbc(model, details = TRUE))
  expect_snapshot(ols_step_forward_sbc(model, include = "pindex", details = TRUE))
})

test_that("output from stepSBIC forward regression is as expected", {
  expect_snapshot(ols_step_forward_sbic(model))
  expect_snapshot(ols_step_forward_sbic(model, progress = TRUE))
  expect_snapshot(ols_step_forward_sbic(model, details = TRUE))
  expect_snapshot(ols_step_forward_sbic(model, include = "pindex", details = TRUE))
})