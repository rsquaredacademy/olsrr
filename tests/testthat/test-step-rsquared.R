model <- lm(y ~ ., data = stepdata)

test_that("output from forward regression matches the expected output", {  
  k <- ols_step_rsquared(model)
  expect_equal(k$metrics$step, 1:6)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4", "x5"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 2), c(33473.30, 32931.76, 31912.72, 29304.30, 29302.80, 29304.73), ignore_attr = TRUE)
  expect_equal(round(k$metrics$ess, 3), c(6241.497, 6074.156, 5771.842, 5065.587, 5064.701, 5064.685), ignore_attr = TRUE)
  expect_equal(round(k$metrics$rss, 2), c(13986.74, 14154.08, 14456.39, 15162.65, 15163.53, 15163.55), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.691, 0.700, 0.715, 0.750, 0.750, 0.750), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.691, 0.700, 0.715, 0.750, 0.750, 0.750), ignore_attr = TRUE)
})

test_that("output from forward regression matches the expected output when variables are locked in", {

  k <- ols_step_rsquared(model, "adj_r2", include = "x5")
  expect_equal(k$metrics$step, 1:5)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  k <- ols_step_rsquared(model, "adj_r2", include = c(5))
  expect_equal(k$metrics$step, 1:5)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from forward regression matches the expected output when variables are locked out", {

  k <- ols_step_rsquared(model, "adj_r2", include = "x6")
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  k <- ols_step_rsquared(model, "adj_r2", include = c(6))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from backward regression matches the expected output", {  
  k <- ols_step_rsquared(model, "adj_r2", "backward")
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("x6", "x5"), ignore_attr = TRUE)
})

test_that("output from stepwise regression matches the expected output", {  
  k <- ols_step_rsquared(model, "adj_r2", "both")
  expect_equal(k$metrics$step, 1:6)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x2", "x6", "x4"), ignore_attr = TRUE)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "addition", "removal", "addition"), ignore_attr = TRUE)
})


test_that("stepaic_forward returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_rsquared(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_rsquared(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_rsquared(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_rsquared(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})