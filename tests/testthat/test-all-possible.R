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

test_that("all_subsets fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_step_all_possible(m), "Please specify a OLS linear regression model.")
})

test_that("all_subsets returns an error when number of predictors < 2", {
  model <- lm(y ~ x1, data = cement)
  expect_error(ols_step_all_possible(model), "Please specify a model with at least 2 predictors.")
})

test_that("output from all subsets regression is as expected", {
  x <- cat("# A tibble: 3 x 6
  Index     N Predictors `R-Square` `Adj. R-Square` `Mallow's Cp`
  <int> <int>      <chr>      <chr>           <chr>         <chr>
1     1     1       disp    0.71834         0.70895       4.44379
2     2     1         hp    0.60244         0.58919      17.79491
3     3     2    disp hp    0.74824         0.73088       3.00000")

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

  expected <- tibble(predictor, beta)
  expect_equivalent(actual$predictor, expected$predictor)
  expect_equivalent(actual$beta, expected$beta)
})
