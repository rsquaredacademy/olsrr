# Regression tests for the 0.7.1 known-bug fix round (ofx15).

test_that("the residual plus component plot keeps every model-matrix column (B2)", {
  m1 <- lm(mpg ~ wt, mtcars)
  p1 <- ols_plot_comp_plus_resid(m1, print_plot = FALSE)

  expect_length(p1, 1)
  expect_equal(nrow(p1[[1]]$data), 32)
  expect_equal(p1[[1]]$data$x, mtcars$wt)
  expect_equal(p1[[1]]$data$y, unname(residuals(m1) + coef(m1)[["wt"]] * mtcars$wt))

  m2 <- lm(mpg ~ 0 + wt + hp, mtcars)
  p2 <- ols_plot_comp_plus_resid(m2, print_plot = FALSE)

  expect_length(p2, 2)
  expect_equal(vapply(p2, function(p) nrow(p$data), integer(1)), c(32L, 32L))
  expect_equal(p2[[1]]$data$x, mtcars$wt)
  expect_equal(p2[[2]]$data$x, mtcars$hp)
  expect_equal(p2[[1]]$data$y, unname(residuals(m2) + coef(m2)[["wt"]] * mtcars$wt))
  expect_equal(p2[[2]]$data$y, unname(residuals(m2) + coef(m2)[["hp"]] * mtcars$hp))
})

test_that("the residual histogram bars and normal curve share one bin width (B3)", {
  library(ggplot2)

  check_hist <- function(model) {
    p <- ols_plot_resid_hist(model, print_plot = FALSE)
    b <- ggplot_build(p)
    r <- residuals(model)

    bar_width <- b$data[[1]]$xmax - b$data[[1]]$xmin
    expect_lt(diff(range(bar_width)), 1e-8)
    bar_width <- mean(bar_width)

    curve <- b$data[[2]]
    ratio <- curve$y / (dnorm(curve$x, mean = mean(r), sd = sd(r)) *
                          length(r) * bar_width)
    expect_equal(ratio, rep(1, length(ratio)), tolerance = 1e-8)
  }

  check_hist(lm(hwy ~ displ + cty + year, data = mpg))
  check_hist(lm(mpg ~ disp + hp + wt, data = mtcars))
})

test_that("residual vs regressor plots align rows dropped by the fit (B4)", {
  d <- mtcars
  d$hp[3] <- NA
  m <- lm(mpg ~ wt + hp, d)
  used <- mtcars[-3, ]

  expect_equal(length(residuals(m)), 31)

  p <- ols_plot_resid_regressor(m, "drat", print_plot = FALSE)
  expect_equal(nrow(p$data), 31)
  expect_equal(p$data$x, used$drat)
  expect_equal(p$data$y, unname(residuals(m)))

  pw <- ols_plot_resid_regressor(m, "wt", print_plot = FALSE)
  expect_equal(nrow(pw$data), 31)
  expect_equal(pw$data$x, used$wt)
  expect_equal(pw$data$y, unname(residuals(m)))

  ps <- rvsr_plot_shiny(m, d, "drat", print_plot = FALSE)
  expect_equal(nrow(ps$data), 31)
  expect_equal(ps$data$x, used$drat)
  expect_equal(ps$data$y, unname(residuals(m)))
})

test_that("ols_plot_diagnostics does not draw when print_plot is FALSE (B5)", {
  m <- lm(mpg ~ disp + hp + wt + qsec, mtcars)

  grDevices::pdf(NULL)
  grDevices::dev.control("enable")
  on.exit(grDevices::dev.off(), add = TRUE)

  before <- length(grDevices::recordPlot()[[1]])
  p      <- ols_plot_diagnostics(m, print_plot = FALSE)
  after  <- length(grDevices::recordPlot()[[1]])

  expect_equal(before, 0)
  expect_equal(after, 0)
  expect_length(p, 10)
  expect_s3_class(p[[1]], "ggplot")
  expect_s3_class(p[[4]], "ggplot")
  expect_s3_class(p[[10]], "ggplot")
})

test_that("ols_prep_rvsrplot_data names every non-intercept model matrix column (B6)", {
  m0 <- lm(mpg ~ 0 + wt + hp, mtcars)
  k0 <- ols_prep_rvsrplot_data(m0)
  expect_equal(k0$np, 2)
  expect_equal(k0$pnames, c("wt", "hp"))
  expect_equal(ncol(k0$dat), 2)

  mw <- lm(mpg ~ wt + hp, weights = cyl, mtcars)
  kw <- ols_prep_rvsrplot_data(mw)
  expect_equal(kw$np, 2)
  expect_equal(kw$pnames, c("wt", "hp"))
  expect_equal(ncol(kw$dat), 2)
  expect_false("(weights)" %in% colnames(kw$dat))

  ms <- lm(mpg ~ wt, mtcars)
  ks <- ols_prep_rvsrplot_data(ms)
  expect_equal(ks$np, 1)
  expect_s3_class(ks$dat, "data.frame")
  expect_equal(ncol(ks$dat), 1)
  expect_equal(ks$pnames, "wt")
})

test_that("Bonferroni p values from ols_test_outlier are probabilities (B7)", {
  m <- lm(mpg ~ wt + hp + disp, mtcars)
  k <- ols_test_outlier(m, cut_off = 40, n_max = 32)

  expect_true(all(k$bonferroni_p_val <= 1))
  expect_equal(k$bonferroni_p_val,
               pmin(1, 32 * k$unadjusted_p_val))
})

test_that("the grob-returning plots draw when print_plot is TRUE (B8)", {
  draw_delta <- function(expr) {
    grDevices::pdf(NULL)
    grDevices::dev.control("enable")
    on.exit(grDevices::dev.off(), add = TRUE)
    before <- length(grDevices::recordPlot()[[1]])
    g <- force(expr)
    after <- length(grDevices::recordPlot()[[1]])
    list(delta = after - before, object = g)
  }

  m <- lm(mpg ~ disp + hp + wt, mtcars)
  for (f in list(
    function() ols_plot_added_variable(m, print_plot = TRUE),
    function() ols_plot_comp_plus_resid(m, print_plot = TRUE),
    function() ols_plot_dfbetas(m, print_plot = TRUE),
    function() ols_plot_diagnostics(lm(mpg ~ disp + hp + wt + qsec, mtcars),
                                    print_plot = TRUE)
  )) {
    got <- draw_delta(f())
    expect_gt(got$delta, 0)
    expect_s3_class(got$object, "arrangelist")
  }
})
