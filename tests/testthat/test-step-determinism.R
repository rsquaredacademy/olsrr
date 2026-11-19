# Determinism of stepwise selection on exactly tied candidates.
# Each data set is built so two candidates tie in exact arithmetic. Rescaling y
# by 1 +/- a few ulps gives an equivalent model, but floating-point noise lands
# differently on each copy. Selection must give the same model in all 100 runs.

nudge_y <- function(d, rel) {
  d$y <- d$y * (1 + rel * sample(c(-1, 1), nrow(d), replace = TRUE))
  d
}

selections <- function(dat, fit, runs = 100, rel = 3e-16) {
  set.seed(2026)
  vapply(seq_len(runs), function(i) {
    r <- fit(nudge_y(dat, rel))
    if (nrow(r$metrics) == 0) "none" else paste(r$metrics$variable, collapse = ",")
  }, character(1))
}

test_that("forward p-value selection is deterministic on an exact F tie", {
  skip_on_cran()
  set.seed(123)
  n <- 100
  y <- scale(rnorm(n))
  e1 <- scale(resid(lm(rnorm(n) ~ y)))
  e2 <- scale(resid(lm(rnorm(n) ~ y + e1)))
  r <- 0.35
  x1 <- as.numeric(r * y + sqrt(1 - r^2) * e1)
  a <- 0.9 * sqrt(1 - r^2)
  b <- sqrt(1 - r^2 - a^2)
  x2 <- as.numeric(r * y + a * e1 + b * e2)
  dat <- data.frame(y = as.numeric(y), x1 = x1, x2 = x2, x3 = rnorm(n))
  out <- selections(dat, function(d) {
    ols_step_forward_p(lm(y ~ x1 + x2 + x3, data = d), p_val = 0.001)
  })
  expect_identical(unique(out), "x1")
})

test_that("backward AIC removal is deterministic on an exact criterion tie", {
  skip_on_cran()
  set.seed(21)
  n <- 50
  u1 <- rnorm(n); u2 <- rnorm(n); z <- rnorm(n)
  y0 <- 1.5 * z + rnorm(n)
  gap <- function(b2) {
    y <- y0 + 0.02 * u1 + b2 * u2
    ols_aic(lm(y ~ u2 + z)) - ols_aic(lm(y ~ u1 + z))
  }
  b2 <- uniroot(gap, c(0, 1), tol = 1e-15)$root
  dat <- data.frame(y = y0 + 0.02 * u1 + b2 * u2, u1 = u1, u2 = u2, z = z)
  out <- selections(dat, function(d) {
    ols_step_backward_aic(lm(y ~ u1 + u2 + z, data = d))
  })
  expect_length(unique(out), 1L)
  expect_identical(strsplit(unique(out), ",")[[1]][1], "u1")
})

test_that("forward R-squared selection is deterministic when a term adds exactly nothing", {
  skip_on_cran()
  set.seed(3)
  n <- 30
  Q <- qr.Q(qr(cbind(1, matrix(rnorm(n * 3), n, 3))))
  e <- resid(lm(rnorm(n) ~ Q))
  dat <- data.frame(y = 2 * Q[, 2] + Q[, 3] + e, x1 = Q[, 2], x2 = Q[, 3], x4 = Q[, 4])
  out <- selections(dat, function(d) {
    ols_step_forward_r2(lm(y ~ x1 + x2 + x4, data = d))
  }, rel = 5e-16)
  expect_identical(unique(out), "x1,x2")
})
