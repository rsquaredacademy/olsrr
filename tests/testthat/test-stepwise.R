test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_p(model, p_enter = 0.15, p_remove = 0.155)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2", "x4"), ignore_attr = TRUE)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "removal"), ignore_attr = TRUE)
  expect_equal(k$metrics$step, 1:4)
})

test_that("output from stepwise selection matches the expected output when variables are locked in", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_p(model, include = c("x6"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_p(model, include = c(6))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepwise selection matches the expected output when variables are locked out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_p(model, exclude = c("x1"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x6", "x3", "x2", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_p(model, exclude = c(1))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x6", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepwise selection matches the expected output when variables are locked in and out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_p(model, include = c("x6"), exclude = c("x1"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x3", "x2", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_p(model, include = c(6), exclude = c(1))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("x3", "x2", "x4"), ignore_attr = TRUE)
})


test_that("stepaic_forward returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_both_p(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_p(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_p(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_p(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

