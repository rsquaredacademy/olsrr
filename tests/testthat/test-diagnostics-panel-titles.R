test_that("diagnostics panels 7 and 8 have distinct titles (#253)", {
  model <- lm(mpg ~ disp + hp + wt, mtcars)
  p     <- ols_plot_diagnostics(model, print_plot = FALSE)

  # the titles of the fit mean and residual spread panels match the two panels
  # of ols_plot_resid_fit_spread() instead of sharing one title
  expect_equal(p[[7]]$labels$title, "Fit - Mean")
  expect_equal(p[[8]]$labels$title, "Residual")
  expect_false(identical(p[[7]]$labels$title, p[[8]]$labels$title))
  expect_false(identical(p[[7]]$labels$title, "Residual Fit Spread Plot"))
  expect_false(identical(p[[8]]$labels$title, "Residual Fit Spread Plot"))
})

test_that("the residual fit spread panels are titled after their variables (#253)", {
  model <- lm(mpg ~ disp + hp + wt, mtcars)

  p <- ols_plot_resid_fit_spread(model, print_plot = FALSE)
  expect_equal(p$fm_plot$labels$title, "Fit - Mean")
  expect_equal(p$rsd_plot$labels$title, "Residual")
  expect_equal(p$fm_plot$labels$y, "Fit - Mean")
  expect_equal(p$rsd_plot$labels$y, "Residual")

  expect_equal(ols_plot_fm(model, print_plot = FALSE)$labels$title, "Fit - Mean")
  expect_equal(
    ols_plot_resid_spread(model, print_plot = FALSE)$labels$title, "Residual"
  )
})
