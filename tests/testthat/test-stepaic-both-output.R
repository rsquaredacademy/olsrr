test_that("output from stepAIC both direction regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_both_aic(model))
  expect_snapshot(ols_step_both_aic(model, progress = TRUE))
  expect_snapshot(ols_step_both_aic(model, details = TRUE))
  expect_snapshot(ols_step_both_aic(model, include = "pindex", details = TRUE))
})

test_that("output from stepAIC both direction regression is as expected when adding and removing variables", {
  model <- lm(y ~ ., data = stepdata)
  expect_snapshot(ols_step_both_aic(model, progress = TRUE))
  expect_snapshot(ols_step_both_aic(model, details = TRUE))
})
