# Direct value assertions for the plot-data helpers. They were only exercised
# indirectly through the plotting tests, which do not check which observations
# are selected or which residuals are returned (re-audit F12).

library(testthat)

test_that("ols_prep_regress_y() returns the residual of y on the other predictors", {
  m <- lm(mpg ~ disp + hp + wt, mtcars)
  d <- ols_prep_avplot_data(m)

  # `i` indexes the columns of the prepared data, whose first column is the
  # response: i = 2 removes `disp` from the predictor set
  got <- ols_prep_regress_y(d, 2)
  ref <- lsfit(as.matrix(d[, c(3, 4)]), d[[1]])$residuals

  expect_equal(unname(got), unname(ref))
  expect_equal(unname(head(got, 4)),
               c(-2.5723294, -1.5834826, -2.4758187, 0.1349799),
               tolerance = 1e-6)
})

test_that("ols_prep_outlier_obs() flags exactly the cooks d outliers", {
  m <- lm(mpg ~ disp + hp + wt, mtcars)
  k <- ols_prep_cdplot_data(m)

  got <- ols_prep_outlier_obs(k)
  cd  <- cooks.distance(m)
  ts  <- 4 / length(cd)

  expect_equal(nrow(got), 32L)
  expect_equal(got$obs[!is.na(got$txt)], unname(which(cd >= ts)))
  expect_true(all(is.na(got$txt[cd < ts])))
  expect_equal(got$txt[!is.na(got$txt)], unname(which(cd >= ts)))
  expect_equal(got$cd, unname(cd))

  # the threshold and the value columns are the model's own statistics
  expect_equal(k$ts, ts)
  expect_equal(k$maxx, max(cd) * 1.01)
})

test_that("ols_prep_cdplot_outliers() returns the flagged observations and distances", {
  m <- lm(mpg ~ disp + hp + wt, mtcars)
  k <- ols_prep_cdplot_data(m)

  got <- ols_prep_cdplot_outliers(k)
  cd  <- cooks.distance(m)
  idx <- which(cd >= 4 / length(cd))

  expect_equal(names(got), c("observation", "cooks_distance"))
  expect_equal(got$observation, unname(idx))
  expect_equal(got$cooks_distance, unname(cd[idx]))
  expect_equal(nrow(got), 3L)

  # named observations are kept and the distances are the model's own
  expect_equal(unname(got$observation), c(17L, 20L, 31L))
  expect_equal(unname(got$cooks_distance),
               c(0.3199707, 0.1529771, 0.3402911), tolerance = 1e-6)
})

test_that("ols_prep_dfbeta_data() and ols_prep_dfbeta_outliers() select the same rows", {
  m <- lm(mpg ~ disp + hp + wt + qsec, mtcars)
  dfb <- dfbetas(m)
  th  <- 2 / sqrt(nrow(dfb))
  raw <- data.frame(obs = seq_len(nrow(dfb)), dbetas = dfb[, 1])

  d <- ols_prep_dfbeta_data(raw, th)

  expect_equal(nrow(d), 32L)
  expect_equal(d$color, ifelse(abs(raw$dbetas) >= th, "outlier", "normal"))
  expect_equal(d$txt[!is.na(d$txt)], raw$obs[abs(raw$dbetas) >= th])
  expect_equal(levels(d$fct_color), c("normal", "outlier"))

  out <- ols_prep_dfbeta_outliers(d)
  expect_equal(out$obs, raw$obs[abs(raw$dbetas) >= th])
  expect_equal(out$dbetas, raw$dbetas[abs(raw$dbetas) >= th])
  expect_equal(names(out), c("obs", "dbetas"))

  expect_equal(unname(out$obs), c(9L, 20L, 21L))
  expect_equal(unname(out$dbetas), c(0.4484991, -0.3583659, 0.3763120),
               tolerance = 1e-6)
})

test_that("plot helpers align influence measures to the used rows (J03)", {
  cases <- list(
    list(zero = 1, missing = 32),
    list(zero = c(1, 16), missing = c(2, 31))
  )

  for (z in cases) {
    d <- mtcars
    d$w <- 1
    d$w[z$zero] <- 0
    d$mpg[z$missing] <- NA
    m   <- lm(mpg ~ wt + hp, d, weights = w, na.action = na.exclude)
    ref <- lm(mpg ~ wt + hp, d[-c(z$zero, z$missing), ])

    ids <- setdiff(seq_len(32), c(z$zero, z$missing))

    # deleted studentized residuals vs fitted
    ds <- ols_prep_dsrvf_data(m)
    expect_equal(ds$ds$obs, ids)
    expect_equal(ds$ds$dsr, unname(rstudent(ref)))
    expect_equal(ds$ds$pred, unname(fitted(ref)))

    # standardized residual chart
    sc <- ols_prep_srchart_data(m)
    expect_equal(sc$obs, ids)
    expect_equal(sc$sdres, unname(rstandard(ref)))

    # studentized residual plot
    sp <- ols_prep_srplot_data(m)
    expect_equal(sp$dsr$obs, ids)
    expect_equal(sp$dsr$dsr, unname(rstudent(ref)))

    # cook's d plot
    cd <- ols_prep_cdplot_data(m)
    expect_equal(cd$ckd$obs, ids)
    expect_equal(cd$ckd$cd, unname(cooks.distance(ref)))

    # dffits, flagged at the size-adjusted threshold
    k  <- length(coef(m))
    nn <- nobs(m)
    dff <- ols_plot_dffits(m, print_plot = FALSE)
    keep <- abs(dffits(ref)) > 2 * sqrt(k / nn)
    expect_equal(dff$outliers$observation, ids[keep])
    expect_equal(dff$outliers$dffits, unname(dffits(ref))[keep])

    # dfbetas, flagged at 2 / sqrt(n)
    dfb <- ols_plot_dfbetas(m, print_plot = FALSE)
    thr <- 2 / sqrt(nn)
    for (j in seq_len(ncol(dfbetas(ref)))) {
      keep <- abs(dfbetas(ref)[, j]) >= thr
      expect_equal(dfb$outliers[[j]]$obs, ids[keep])
      expect_equal(dfb$outliers[[j]]$dbetas, unname(dfbetas(ref)[, j])[keep])
    }
  }
})
