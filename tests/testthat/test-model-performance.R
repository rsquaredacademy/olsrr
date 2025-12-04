test_that("model performance works as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  perf  <- ols_model_performance(model)
  expect_equal(round(perf$r, 3), 0.909)
  expect_equal(round(perf$rsq, 3), 0.827)
  expect_equal(round(perf$adjr, 3), 0.808)
  expect_equal(round(perf$prsq, 3), 0.768)
  expect_equal(round(perf$aic, 3), 158.643)
  expect_equal(round(perf$sbc, 3),165.972 )
  expect_equal(round(perf$sbic, 3), 68.933)
  expect_equal(round(perf$rmse, 3), 2.468)
})

test_that("output from model performance is as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  expect_snapshot(ols_model_performance(model))
})
