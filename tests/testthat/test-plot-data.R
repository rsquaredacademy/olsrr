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
