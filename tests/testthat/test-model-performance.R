test_that("model performance works as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  perf  <- ols_model_performance(model)
  expect_equal(round(perf$r, 3), 0.909)
  expect_equal(round(perf$rsq, 3), 0.827)
  expect_equal(round(perf$adjr, 3), 0.808)
  expect_equal(round(perf$prsq, 3), 0.768)
  expect_equal(round(perf$aic, 3), 158.643)
  expect_equal(round(perf$sbc, 3), 165.972)
  expect_equal(round(perf$sbic, 3), 68.933)
  expect_equal(round(perf$rmse, 3), 2.468)
})

test_that("output from model performance is as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  expect_snapshot(ols_model_performance(model))
})

test_that("model comparison works as expected", {
  model_1 <- lm(mpg ~ disp + hp, data = mtcars)
  model_2 <- lm(mpg ~ disp + hp + wt, data = mtcars)
  model_3 <- lm(mpg ~ disp + hp + wt + qsec + + drat, data = mtcars)
  result <- ols_compare_model_performance(model_1, model_2, model_3)
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 3)
  expect_equal(result[, 1], c("model_1", "model_2", "model_3"))
  expect_equal(round(result$sigma, 3), c(3.127, 2.639, 2.558))
})
