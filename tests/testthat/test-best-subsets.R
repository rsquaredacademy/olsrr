model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)

test_that("best subsets selection output matches the expected result", {
  k <- ols_step_best_subset(model)
  pred_exp <- c("x4", "x1 x2", "x1 x2 x4", "x1 x2 x3 x4")
  expect_equal(k$metrics$mindex, c(1, 2, 3, 4))
  expect_equal(k$metrics$predictors, pred_exp, ignore_attr = TRUE)
})


test_that("best subsets regression returns the appropriate error", {
  expect_error(ols_step_best_subset(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_best_subset(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_best_subset(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_best_subset(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

test_that("output from best subsets regression matches the expected output when variables are locked in", {

  k <- ols_step_best_subset(model, include = c("x3"))
  pred_exp <- c("x3", "x3 x4", "x1 x2 x3", "x1 x2 x3 x4")
  expect_equal(k$metrics$mindex, c(1, 2, 3, 4))
  expect_equal(k$metrics$predictors, pred_exp, ignore_attr = TRUE)

  k <- ols_step_best_subset(model, include = c(3))
  pred_exp <- c("x3", "x3 x4", "x1 x2 x3", "x1 x2 x3 x4")
  expect_equal(k$metrics$mindex, c(1, 2, 3, 4))
  expect_equal(k$metrics$predictors, pred_exp, ignore_attr = TRUE)
})

test_that("output from best subsets regression matches the expected output when variables are locked out", {

  k <- ols_step_best_subset(model, exclude = c("x1"))
  pred_exp <- c("x4", "x3 x4", "x2 x3 x4")
  expect_equal(k$metrics$mindex, c(1, 2, 3))
  expect_equal(k$metrics$predictors, pred_exp, ignore_attr = TRUE)

  k <- ols_step_best_subset(model, exclude = c(1))
  pred_exp <- c("x4", "x3 x4", "x2 x3 x4")
  expect_equal(k$metrics$mindex, c(1, 2, 3))
  expect_equal(k$metrics$predictors, pred_exp, ignore_attr = TRUE)
})