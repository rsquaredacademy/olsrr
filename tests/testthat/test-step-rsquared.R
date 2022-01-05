model <- lm(y ~ ., data = stepdata)

test_that("output from forward regression matches the expected output", {  
  k <- ols_step_forward_r2(model)
  expect_equal(k$metrics$step, 1:6)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4", "x5"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 2), c(33473.30, 32931.76, 31912.72, 29304.30, 29302.80, 29304.73), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.691, 0.700, 0.715, 0.750, 0.750, 0.750), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.691, 0.700, 0.715, 0.750, 0.750, 0.750), ignore_attr = TRUE)
})

test_that("output from forward regression matches the expected output when variables are locked in", {

  k <- ols_step_forward_adj_r2(model, include = "x5")
  expect_equal(k$metrics$step, 1:5)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  k <- ols_step_forward_adj_r2(model, include = c(5))
  expect_equal(k$metrics$step, 1:5)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from forward regression matches the expected output when variables are locked out", {

  k <- ols_step_forward_adj_r2(model, include = "x6")
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  k <- ols_step_forward_adj_r2(model, include = c(6))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from backward regression matches the expected output", {  
  k <- ols_step_backward_adj_r2(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("x6", "x5"), ignore_attr = TRUE)
})

test_that("output from stepwise regression matches the expected output", {  
  k <- ols_step_both_adj_r2(model)
  expect_equal(k$metrics$step, 1:6)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x6", "x4"), ignore_attr = TRUE)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "addition", "removal", "addition"), ignore_attr = TRUE)
})


test_that("step rsquared returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_forward_r2(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_forward_r2(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_forward_r2(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_forward_r2(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
  expect_error(ols_step_backward_r2(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_backward_r2(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_backward_r2(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_backward_r2(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
  expect_error(ols_step_both_r2(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_r2(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_r2(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_r2(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})