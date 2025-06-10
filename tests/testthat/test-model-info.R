test_that("the model forumula is returned", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  want <- as.formula(mpg ~ disp + hp + wt)
  got <- ols_get_formula(model)
  expect_equal(want, got)
})
