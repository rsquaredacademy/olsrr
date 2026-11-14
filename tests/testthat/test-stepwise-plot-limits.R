test_that("stepwise plot limits contain every metric value (F13)", {
  lims <- function(v) {
    x <- list(metrics = data.frame(step = seq_along(v), variable = paste0("x", seq_along(v)), aic = v))
    d <- ols_stepwise_plot_data(x, paste0("x", seq_along(v)), "aic")
    c(d$ymin, d$ymax)
  }
  expect_equal(lims(c(-10, 100)), c(-10.5, 105))
  # same limits as before for one-signed values
  expect_equal(lims(c(10, 100)), c(9.5, 105))
  expect_equal(lims(c(-100, -10)), c(-105, -9.5))
  for (v in list(c(-10, 100), c(10, 100), c(-100, -10), c(-3, 0, 2))) {
    l <- lims(v)
    expect_true(l[1] <= min(v) && l[2] >= max(v))
  }
})
