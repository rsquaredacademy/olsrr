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

test_that("Cook's D and DFFITS thresholds count rank, not aliased slots (K02)", {
  d <- data.frame(x = 1:20, y = sin(1:20))
  d$y[6] <- 1
  m  <- lm(y ~ x, d)
  ma <- lm(y ~ x + I(2 * x), d)

  # DFFITS: size-adjusted and standard cutoffs, outliers and labels
  for (adj in c(TRUE, FALSE)) {
    a <- ols_plot_dffits(m, size_adj_threshold = adj, print_plot = FALSE)
    b <- ols_plot_dffits(ma, size_adj_threshold = adj, print_plot = FALSE)
    expect_equal(b$threshold, a$threshold)
    expect_equal(b$outliers, a$outliers)
  }
  a <- ols_plot_dffits(m, print_plot = FALSE)
  expect_equal(a$threshold, round(2 * sqrt(2 / 20), 2))
  expect_equal(a$outliers$observation, 20)
  expect_equal(ols_plot_dffits(ma, print_plot = FALSE)$outliers$observation, 20)

  # Cook's D types 2 and 4 use the residual degrees of freedom
  e  <- lm(mpg ~ wt + hp, mtcars)
  ea <- lm(mpg ~ wt + I(2 * wt) + hp, mtcars)
  expect_equal(ols_prep_cdplot_data(ea, type = 2)$ts, 4 / 29)
  expect_equal(ols_prep_cdplot_data(ea, type = 4)$ts, 1 / 29)
  for (ty in 1:5) {
    expect_equal(ols_prep_cdplot_data(ea, type = ty)$ts,
                 ols_prep_cdplot_data(e, type = ty)$ts)
    expect_equal(ols_prep_cdplot_data(ea, type = ty)$ckd$color,
                 ols_prep_cdplot_data(e, type = ty)$ckd$color)
  }
  expect_equal(ols_prep_cdplot_data(ea, type = 2)$ts, 4 / df.residual(ea))

  set.seed(714)
  z  <- replicate(22, rnorm(20))[, 22]
  dz <- data.frame(x = 1:20, y = z)
  s  <- lm(y ~ x, dz)
  sa <- lm(y ~ x + I(2 * x), dz)
  expect_equal(ols_prep_cdplot_data(sa, type = 2)$ts, 4 / 18)
  expect_equal(ols_prep_cdplot_data(sa, type = 2)$ckd$color,
               ols_prep_cdplot_data(s, type = 2)$ckd$color)
})

test_that("unit-leverage rows keep undefined influence statistics as in base R (K03)", {
  d <- data.frame(y = c(2, 5, 7, 3, 8, 1, 4),
                  g = factor(c(rep("a", 6), "b")))
  m <- lm(y ~ g, d)

  expect_true(is.nan(rstandard(m)[7]))
  expect_true(is.nan(cooks.distance(m)[7]))

  cd <- ols_prep_cdplot_data(m)$ckd
  expect_equal(cd$cd, unname(cooks.distance(m)))
  expect_true(is.nan(cd$cd[7]))
  expect_false(identical(cd$color[7], "normal"))

  expect_equal(ols_prep_srchart_data(m)$sdres, unname(rstandard(m)))
  expect_equal(ols_prep_srplot_data(m)$dsr$dsr, unname(rstudent(m)))
  expect_equal(ols_prep_dsrvf_data(m)$ds$dsr, unname(rstudent(m)))

  df <- ols_plot_dffits(m, print_plot = FALSE)$plot$data
  expect_equal(df$dbetas, unname(dffits(m)))
  expect_true(is.nan(df$dbetas[7]))
  expect_false(identical(df$color[7], "normal"))

  # DFBETAS: base R's unit-leverage convention (0)
  db <- ols_plot_dfbetas(m, print_plot = FALSE)$plots
  expect_equal(db[[2]]$data$dbetas, unname(dfbetas(m)[, 2]))
  expect_equal(db[[2]]$data$dbetas[7], 0)

  # weights and omissions: the same boundary
  d$w <- c(1, 2, 1, 2, 1, 2, 1)
  mw <- lm(y ~ g, d, weights = w)
  expect_equal(ols_prep_cdplot_data(mw)$ckd$cd, unname(cooks.distance(mw)))
  expect_equal(ols_prep_srchart_data(mw)$sdres, unname(rstandard(mw)))

  # an isolated indicator column
  d2 <- data.frame(y = c(2, 5, 7, 3, 8, 1, 4, 6), x = c(1, 3, 2, 5, 4, 6, 8, 7),
                   i = c(rep(0, 7), 1))
  m2 <- lm(y ~ x + i, d2)
  expect_equal(ols_prep_cdplot_data(m2)$ckd$cd, unname(cooks.distance(m2)))
  expect_equal(ols_prep_srchart_data(m2)$sdres, unname(rstandard(m2)))
  expect_equal(ols_plot_dffits(m2, print_plot = FALSE)$plot$data$dbetas,
               unname(dffits(m2)))
})
