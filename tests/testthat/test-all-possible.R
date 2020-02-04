context("all_subsets")

test_that("all subsets selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_all_possible(model)
  pred_exp <- c(
    "x4", "x2", "x1", "x3", "x1 x2",
    "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",
    "x1 x2 x4", "x1 x2 x3", "x1 x3 x4", "x2 x3 x4",
    "x1 x2 x3 x4"
  )
  expect_equal(k$mindex, c(1:15))
  expect_equivalent(k$predictors, pred_exp)
})


test_that("output from all subsets regression is as expected", {
  x <- cat("Index N Predictors  R-Square Adj. R-Square Mallow's Cp
1     1 1       disp 0.7183433     0.7089548    4.443792
2     2 1         hp 0.6024373     0.5891853   17.794906
3     3 2    disp hp 0.7482402     0.7308774    3.000000")

  model <- lm(mpg ~ disp + hp, data = mtcars)
  expect_output(print(ols_step_all_possible(model)), x)
})

test_that("all possible regression betas are as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  k <- ols_step_all_possible_betas(model)
  actual <- k %>%
    select(predictor, beta) %>%
    group_by(predictor) %>%
    summarise_all(mean)

  predictor <- c("(Intercept)", "disp", "hp", "wt")
  beta <- c(33.85901073, -0.02255579, -0.03899945, -4.09350456)

  expected <- data.frame(predictor, beta)
  expect_equivalent(actual$predictor, expected$predictor)
  expect_equivalent(actual$beta, expected$beta)
})
