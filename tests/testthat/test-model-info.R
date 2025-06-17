test_that("the model forumula is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- as.formula(mpg ~ disp + hp + wt)
  got   <- ols_get_formula(model)
  expect_equal(want, got)
})

test_that("interaction terms are returned", {
  model <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
  want  <- c("wt:cyl", "vs:hp", "vs:gear",  "hp:gear", "vs:hp:gear")
  got   <- ols_get_interaction_terms(model)
  expect_equal(want, got)
})

test_that("NULL is returned in absence of interaction terms", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- NULL
  got   <- ols_get_interaction_terms(model)
  expect_null(want)
})

test_that("predictors does not return null when model has at least one predictor", {
  model <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
  want  <- c("wt", "cyl", "vs", "hp", "gear", "carb")
  got   <- ols_get_variables(model)$predictors
  expect_equal(want, got)
})

test_that("predictors returns null in case of intercept only model", {
  model <- lm(mpg ~ 1, data = mtcars)
  want  <- NULL
  got   <- ols_get_variables(model)
  expect_null(want)
})

test_that("response is not null", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- c("mpg")
  got   <- ols_get_variables(model)$response
  expect_equal(want, got)
})

test_that("model data is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- c("mpg", "disp", "hp", "wt")
  got   <- names(ols_get_data(model))
  expect_equal(want, got)
})

test_that("residual degrees of freedom is returned", {
  model <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
  want  <- 20
  got   <- ols_get_df(model)
  expect_equal(want, got)
})

test_that("intercept is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- 37.11
  got   <- round(ols_get_intercept(model), 2)
  expect_equal(want, got)
})


test_that("NULL is returned in the absence of intercept", {
  model <- lm(mpg ~ 0 + disp + hp + wt, data = mtcars)
  want  <- NULL
  got   <- ols_get_intercept(model)
  expect_null(want)
})

test_that("model matrix is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- c("(Intercept)", "disp", "hp", "wt")
  got   <- colnames(ols_get_model_matrix(model))
  expect_equal(want, got)
})

test_that("fitted values are returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- 32
  got   <- length(ols_get_predicted(model))
  expect_equal(want, got)
})

test_that("residuals are returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- 32
  got   <- length(ols_get_residuals(model))
  expect_equal(want, got)
})

test_that("sigma is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- 2.64
  got   <- round(ols_get_sigma(model), 2)
  expect_equal(want, got)
})

test_that("variance covariance matrix is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- c("(Intercept)", "disp", "hp", "wt")
  got   <- colnames(ols_get_vcov(model))
  expect_equal(want, got)
})

test_that("deviance is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- 194.99
  got   <- round(ols_get_deviance(model), 2)
  expect_equal(want, got)
})

test_that("parameters and estimates are returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want  <- c("(Intercept)", "disp", "hp", "wt")
  got   <- ols_get_parameters(model)[, 1]
  expect_equal(want, got)
})
