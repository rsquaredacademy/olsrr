test_that("output from best subsets regression is as expected", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_snapshot(ols_step_best_subset(model))
})

test_that("output from best subsets regression is as expected when using different metric", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_snapshot(ols_step_best_subset(model, metric = "aic"))
})
