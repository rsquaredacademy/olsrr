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
