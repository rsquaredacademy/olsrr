model <- lm(y ~ ., data = stepdata)

test_that("output from rsquared forward regression is as expected", {
  expect_snapshot(ols_step_rsquared(model))
  expect_snapshot(ols_step_rsquared(model, exlude = "x5"))
  expect_snapshot(ols_step_rsquared(model, include = "x6", details = TRUE))
  expect_snapshot(ols_step_rsquared(model, progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, details = TRUE))
})

test_that("output from adjusted rsquared forward regression is as expected", {
  expect_snapshot(ols_step_rsquared(model, "adj_r2"))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", include = "x6", details = TRUE))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", details = TRUE))
})

test_that("output from rsquared backward regression is as expected", {
  expect_snapshot(ols_step_rsquared(model, direction = "backward"))
  expect_snapshot(ols_step_rsquared(model, direction = "backward", exlude = "x5"))
  expect_snapshot(ols_step_rsquared(model, direction = "backward", include = "x6", details = TRUE))
  expect_snapshot(ols_step_rsquared(model, direction = "backward", progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, direction = "backward", details = TRUE))
})

test_that("output from adjusted rsquared backward regression is as expected", {
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "backward"))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "backward", exlude = "x5"))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "backward", include = "x6", details = TRUE))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "backward", progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "backward", details = TRUE))
})

test_that("output from rsquared both direction regression is as expected", {
  expect_snapshot(ols_step_rsquared(model, direction = "both"))
  expect_snapshot(ols_step_rsquared(model, direction = "both", exlude = "x5"))
  expect_snapshot(ols_step_rsquared(model, direction = "both", include = "x6", details = TRUE))
  expect_snapshot(ols_step_rsquared(model, direction = "both", progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, direction = "both", details = TRUE))
})

test_that("output from adjusted rsquared both direction regression is as expected", {
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "both"))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "both", progress = TRUE))
  expect_snapshot(ols_step_rsquared(model, "adj_r2", "both", details = TRUE))
})