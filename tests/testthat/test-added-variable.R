test_that("added variable plot requires at least two predictors (audit F10)", {
  expect_error(
    ols_plot_added_variable(lm(mpg ~ disp, data = mtcars), print_plot = FALSE),
    "at least two predictors"
  )

  expect_error(
    ols_plot_added_variable(lm(mpg ~ 1, data = mtcars), print_plot = FALSE),
    "at least two predictors"
  )
})

test_that("added variable plot returns one plot per predictor", {
  p <- ols_plot_added_variable(lm(mpg ~ disp + hp + wt, data = mtcars), print_plot = FALSE)

  expect_length(p, 3)
  expect_s3_class(p[[1]], "ggplot")
})
