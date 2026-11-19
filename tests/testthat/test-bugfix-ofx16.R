# Regression tests for the olsrr 0.7.1 re-audit leftovers (ofx16).

na_exclude_fixture <- function() {
  d <- mtcars
  d$hp[c(3, 9)] <- NA
  list(
    data = d,
    used = mtcars[-c(3, 9), ],
    fit  = lm(mpg ~ wt + hp + disp, d, na.action = na.exclude)
  )
}

test_that("residual vs regressor uses the used rows of an na.exclude fit (M1)", {
  f    <- na_exclude_fixture()
  ref  <- lm(mpg ~ wt + hp + disp, f$used)

  # residuals() pads this fit to the original 32 rows
  expect_length(residuals(f$fit), 32)

  p <- ols_plot_resid_regressor(f$fit, "wt", print_plot = FALSE)

  expect_equal(nrow(p$data), 30)
  expect_equal(unname(p$data$x), f$used$wt)
  expect_equal(unname(p$data$y), unname(ref$residuals))
  expect_equal(unname(p$data$y), unname(f$fit$residuals))
})

test_that("residual histogram uses the used rows of an na.exclude fit (L1)", {
  f   <- na_exclude_fixture()
  ref <- lm(mpg ~ wt + hp + disp, f$used)

  expect_no_warning(p <- ols_plot_resid_hist(f$fit, print_plot = FALSE))

  b <- ggplot2::ggplot_build(p)
  expect_equal(unname(p$data$x), unname(ref$residuals))

  bar_width <- mean(b$data[[1]]$xmax - b$data[[1]]$xmin)
  r         <- ref$residuals
  curve     <- b$data[[2]]
  ratio     <- curve$y / (dnorm(curve$x, mean = mean(r), sd = sd(r)) *
                            length(r) * bar_width)
  expect_equal(ratio, rep(1, length(ratio)), tolerance = 1e-8)
})

test_that("the diagnostics panel uses the used rows of an na.exclude fit (L1)", {
  f   <- na_exclude_fixture()
  ref <- lm(mpg ~ wt + hp + disp, f$used)

  expect_no_warning(p <- ols_plot_diagnostics(f$fit, print_plot = FALSE))

  expect_length(p, 10)
  expect_equal(unname(vapply(p, function(x) nrow(x$data), integer(1))),
               rep(30L, 10))

  # residual vs fitted
  expect_equal(unname(p[[1]]$data$predicted), unname(ref$fitted.values))
  expect_equal(unname(p[[1]]$data$resid), unname(ref$residuals))

  # QQ, observed vs fitted, fit mean and residual spread, histogram, box plot
  expect_equal(unname(p[[4]]$data$x), unname(ref$residuals))
  expect_equal(unname(p[[5]]$data$x), f$used$mpg)
  expect_equal(unname(p[[5]]$data$y), unname(ref$fitted.values))
  expect_equal(unname(p[[7]]$data$y),
               unname(ref$fitted.values - mean(ref$fitted.values)))
  expect_equal(unname(p[[8]]$data$y), unname(ref$residuals))
  expect_equal(unname(p[[9]]$data$x), unname(ref$residuals))
  expect_equal(unname(p[[10]]$data$resid), unname(ref$residuals))
})

test_that("residual plus component uses the used rows of an na.exclude fit (L1)", {
  f   <- na_exclude_fixture()
  ref <- lm(mpg ~ wt + hp + disp, f$used)
  xm  <- model.matrix(ref)[, -1, drop = FALSE]

  expect_no_warning(p <- ols_plot_comp_plus_resid(f$fit, print_plot = FALSE))

  expect_length(p, 3)
  expect_equal(vapply(p, function(x) nrow(x$data), integer(1)), rep(30L, 3))

  for (i in seq_len(ncol(xm))) {
    v <- colnames(xm)[i]
    expect_equal(unname(p[[i]]$data$x), unname(xm[, i]))
    expect_equal(unname(p[[i]]$data$y),
                 unname(ref$residuals + coef(ref)[[v]] * xm[, i]))
  }
})

test_that("VIF warns for a model without an intercept (L3)", {
  m <- lm(mpg ~ 0 + wt + hp, mtcars)

  expect_warning(
    v <- ols_vif_tol(m),
    "VIFs may not be sensible in a model without an intercept"
  )

  tol <- c(1 - summary(lm(wt ~ hp, mtcars))$r.squared,
           1 - summary(lm(hp ~ wt, mtcars))$r.squared)

  expect_equal(v$Variables, c("wt", "hp"))
  expect_equal(v$Tolerance, tol)
  expect_equal(v$VIF, 1 / tol)
})

test_that("aliased predictors are not counted as estimated terms (L4)", {
  d    <- mtcars
  d$wt2 <- d$wt
  m    <- lm(mpg ~ wt + wt2 + hp, d)

  ap  <- ols_step_all_possible(m)$result
  row <- ap[ap$predictors == "wt wt2 hp", ]

  expect_equal(row$n, 2)
  expect_equal(row$rsquare, summary(lm(mpg ~ wt + hp, d))$r.squared)
  expect_equal(row$aic, AIC(lm(mpg ~ wt + hp, d)))

  # an exactly collinear pair estimates one term
  alias <- ap[ap$predictors == "wt wt2", ]
  expect_equal(alias$n, 1)
  expect_equal(alias$rsquare, summary(lm(mpg ~ wt, d))$r.squared)

  # non-aliased counts are unchanged
  expect_equal(ap$n[ap$predictors == "wt hp"], 2)
  expect_equal(ap$n[ap$predictors == "wt"], 1)

  # best subsets groups the full aliased fit with the two-term fits
  bs <- ols_step_best_subset(m)$metrics
  expect_equal(nrow(bs), 2)
  expect_equal(max(bs$n), 2)

  # a multi-column factor term still counts once
  mf <- lm(mpg ~ factor(cyl) + wt, mtcars)
  af <- ols_step_all_possible(mf)$result
  expect_equal(af$n[af$predictors == "factor(cyl)"], 1)
  expect_equal(af$n[af$predictors == "factor(cyl) wt"], 2)

  # no-intercept fits count the estimated directions
  m0 <- lm(mpg ~ 0 + wt + wt2 + hp, d)
  a0 <- ols_step_all_possible(m0)$result
  expect_equal(a0$n[a0$predictors == "wt wt2 hp"], 2)
})
