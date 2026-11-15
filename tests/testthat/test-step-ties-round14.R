test_that("rounded F ties remove the less significant term first (L1)", {
  f <- c(13.680911680911679, 13.680911680911702)
  expect_identical(ols_step_p_index(f, c(.1, .2), entry = FALSE), 2L)
  expect_identical(ols_step_p_index(f, c(.2, .2), entry = FALSE), 1L)

  # Audit p15-factor3: tied F tests with two versus one numerator df.
  set.seed(42)
  n <- 60
  f <- factor(rep(c("a", "b", "c"), length.out = n))
  x <- rnorm(n)
  z <- rnorm(n)
  y <- 0.24058843812794375 * (as.numeric(f) - 1) +
    0.17362307970490476 * x + 1.5 * z + rnorm(n, sd = 0.8)
  d <- data.frame(y, f, x, z)
  m <- lm(y ~ f + x + z, d)
  a <- car::Anova(m)
  expect_equal(a$`F value`[1], a$`F value`[2], tolerance = 1e-10)
  expect_gt(a$`Pr(>F)`[2], a$`Pr(>F)`[1])
  k <- ols_step_backward_p(m, p_val = 0.05)
  expect_identical(k$metrics$variable, c("x", "f"))
  expect_equal(coef(k$model), coef(lm(y ~ z, d)))
})

test_that("backward criteria rank eligible terms by rounded values (M1/M4)", {
  m <- lm(mpg ~ wt + hp + qsec + disp, mtcars)
  # Synthetic metrics isolate rounding from platform-specific QR arithmetic.
  # wt is locked, hp is the clear first removal, then qsec/disp nearly tie.
  ic <- function(model, ...) {
    p <- attr(terms(model), "term.labels")
    60 + 20 * length(p) - 5 * (!"wt" %in% p) - 2 * (!"hp" %in% p) -
      3e-14 * ("qsec" %in% p)
  }
  r2 <- function(model) {
    p <- attr(terms(model), "term.labels")
    (5 - length(p)) / 10 + .02 * (!"wt" %in% p) + .01 * (!"hp" %in% p) +
      2^-53 * ("qsec" %in% p)
  }
  fit <- ols_step_fit
  local_mocked_bindings(
    ols_aic = ic, ols_sbc = ic, ols_sbic = ic,
    ols_base_criteria = function(model, criteria) {
      if (criteria %in% c("rsq", "adjrsq")) r2(model) else ic(model)
    },
    ols_step_fit = function(...) {
      k <- fit(...)
      k$rsq <- k$adjr <- r2(k$model)
      k$aic <- k$sbc <- ic(k$model)
      k
    }
  )
  for (criterion in c("aic", "sbc", "sbic", "r2", "adj_r2")) {
    selector <- get(paste0("ols_step_backward_", criterion))
    for (include in list("wt", 1L)) {
      k <- selector(m, include = include, steps = 2)
      expect_identical(k$metrics$variable, c("hp", "qsec"), info = criterion)
      expect_identical(attr(terms(k$model), "term.labels"), c("wt", "disp"))
      k <- selector(lm(mpg ~ wt + qsec + disp, mtcars), include = include, steps = 1)
      expect_identical(k$metrics$variable, "qsec", info = criterion)
      expect_identical(attr(terms(k$model), "term.labels"), c("wt", "disp"))
    }
  }
})

test_that("selection keys merge last-bit differences on every platform (M4)", {
  f <- c(13.680911680911679, 13.680911680911702)
  expect_gt(f[2], f[1])
  expect_identical(ols_step_p_index(f, c(.1, .1)), 1L)
  expect_identical(ols_step_p_index(rev(f), c(.1, .1), entry = FALSE), 1L)
  expect_identical(ols_step_p_index(f, c(.2, .1)), 2L)
  expect_identical(ols_step_p_index(f, c(.1, .1 - 3e-17)), 1L)
  expect_identical(ols_step_p_index(f, c(.1, .1 + 3e-17), entry = FALSE), 1L)
  for (criterion in c("aic", "sbc", "sbic")) {
    expect_identical(ols_threshold(c(100, 100 - 3e-14), criterion), 1L)
    expect_identical(ols_threshold(c(100, 99), criterion), 2L)
  }
  for (criterion in c("rsq", "adjrsq")) {
    expect_identical(ols_threshold(c(.5, .5 + 2^-52), criterion), 1L)
    expect_identical(ols_threshold(c(.5, .51), criterion), 2L)
  }
  expect_identical(ols_step_p_index(c(10, 11), c(.1, .1)), 2L)
})

test_that("criterion improvements require distinct ten-digit keys (M2/M4)", {
  # Audit p10-rsq3: two ulps must not count as an R-squared improvement.
  expect_false(ols_f_criteria("rsq", 0.2475087654506713252811, 1L,
                              0.2475087654506712975255))
  expect_false(ols_next_criteria("rsq", 0.2475087654506713252811, 1L,
                                 0.2475087654506712975255, 1L))
  for (criterion in c("aic", "sbc", "sbic", "rsq", "adjrsq")) {
    minimize <- criterion %in% c("aic", "sbc", "sbic")
    baseline <- if (minimize) 100 else .5
    noise <- if (minimize) -3e-14 else 2^-52
    better <- if (minimize) 99 else .51
    worse <- if (minimize) 101 else .49
    for (candidate in c(baseline, baseline + noise, worse)) {
      expect_false(ols_f_criteria(criterion, candidate, 1L, baseline))
      expect_false(ols_next_criteria(criterion, candidate, 1L, baseline, 1L))
    }
    expect_true(ols_f_criteria(criterion, better, 1L, baseline))
    expect_true(ols_next_criteria(criterion, better, 1L, baseline, 1L))
  }
})
