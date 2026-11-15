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
