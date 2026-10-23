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


test_that("output from stepwise selection matches the expected output when steps are specified", {
  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_p(model, steps = 3)
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3"), ignore_attr = TRUE)
})

test_that("stepaic_forward returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_both_p(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_p(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_p(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_p(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})


test_that("stepwise forward AIC does not crash when no variable improves the model (#186)", {
  set.seed(20261023)
  noise <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
  k <- ols_step_forward_aic(lm(y ~ ., data = noise))

  expect_equal(deparse(formula(k$model)), "y ~ 1")
  expect_equal(nrow(k$metrics), 0)
})

test_that("stepwise backward AIC falls back to an intercept-only model (#186)", {
  set.seed(20261023)
  noise <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
  k <- ols_step_backward_aic(lm(y ~ ., data = noise))

  expect_equal(deparse(formula(k$model)), "y ~ 1")
  expect_equal(nrow(k$metrics), 3)
})

test_that("stepwise selection stops on a near-perfect fit (#199)", {
  perfect <- data.frame(y = 1:10, x = 1:10)
  model   <- lm(y ~ x, data = perfect)

  expect_message(k <- ols_step_forward_p(model), "near-perfect fit")
  expect_equal(deparse(formula(k$model)), "y ~ x")
  expect_equal(coef(k$model), coef(model))
})

test_that("stepwise selection breaks ties deterministically (#206)", {
  set.seed(20261023)
  x1  <- rnorm(30)
  dup <- data.frame(y = x1 + rnorm(30, sd = 0.1), x1 = x1)
  dup$x2 <- dup$x1

  k <- ols_step_forward_aic(lm(y ~ x1 + x2, data = dup))
  expect_equal(attr(terms(k$model), "term.labels"), "x1")
  expect_equal(coef(k$model), coef(lm(y ~ x1, data = dup)))
})

test_that("stepwise backward AIC never drops a forced include variable (#172)", {
  a <- ols_step_backward_aic(lm(mpg ~ disp + hp + wt, data = mtcars), include = "disp")
  expect_equal(attr(terms(a$model), "term.labels")[1], "disp")
})
