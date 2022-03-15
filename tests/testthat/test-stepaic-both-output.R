test_that("output from stepAIC both direction regression is as expected when adding and removing variables", {
  model <- lm(y ~ ., data = stepdata)
  expect_snapshot(ols_step_both_aic(model, progress = TRUE))
  expect_snapshot(ols_step_both_aic(model, details = TRUE))
})

test_that("output from stepSBC both direction regression is as expected when adding and removing variables", {
  model <- lm(y ~ ., data = stepdata)
  expect_snapshot(ols_step_both_sbc(model, progress = TRUE))
  expect_snapshot(ols_step_both_sbc(model, details = TRUE))
})

test_that("output from stepSBIC both direction regression is as expected when adding and removing variables", {
  model <- lm(y ~ ., data = stepdata)
  expect_snapshot(ols_step_both_sbic(model, progress = TRUE))
  expect_snapshot(ols_step_both_sbic(model, details = TRUE))
})
