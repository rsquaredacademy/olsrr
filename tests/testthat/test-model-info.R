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
