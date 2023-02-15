test_that("all possible regression output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_all_possible(model)
  pred_exp <- c(
    "x4", "x2", "x1", "x3", "x1 x2",
    "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",
    "x1 x2 x4", "x1 x2 x3", "x1 x3 x4", "x2 x3 x4",
    "x1 x2 x3 x4"
  )
  expect_equal(k$result$mindex, c(1:15))
  expect_equal(k$result$predictors, pred_exp, ignore_attr = TRUE)

  k2 <- ols_step_all_possible(model, max_order = 3)
  preds_selected <- c(
    "x4", "x2", "x1", "x3", "x1 x2",
    "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",
    "x1 x2 x4", "x1 x2 x3", "x1 x3 x4", "x2 x3 x4"
  )
  expect_equal(k2$result$mindex, c(1:14))
  expect_equal(k2$result$predictors, preds_selected, ignore_attr = TRUE)
})

test_that("all possible regression throws error messages", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  error_msg <- "Maximum subset order should be less than or equal to the number of predictors in the specified model."
  expect_error(ols_step_all_possible(model, max_order = 5), error_msg)
})


test_that("output from all subsets regression is as expected", {
  model <- lm(mpg ~ disp + hp, data = mtcars)
  expect_snapshot(ols_step_all_possible(model))
})

test_that("all possible regression betas are as expected", {
  
  model   <- lm(mpg ~ disp + hp + wt, data = mtcars)
  k       <- ols_step_all_possible_betas(model)
  out     <- lapply(split(k$beta, k$predictor), mean)
  actual  <- data.frame(predictor = names(out), beta = as.numeric(out))

  predictor <- c("(Intercept)", "disp", "hp", "wt")
  beta <- c(33.85901073, -0.02255579, -0.03899945, -4.09350456)

  expected <- data.frame(predictor, beta)
  expect_equal(actual$predictor, expected$predictor, ignore_attr = TRUE)
  expect_equal(actual$beta, expected$beta, ignore_attr = TRUE)
})

