test_that("forward selection keeps the steps recorded before a saturated model stops it (B10)", {
  set.seed(199)
  d <- data.frame(y = rnorm(10), matrix(rnorm(10 * 29), 10, 29))
  names(d)[-1] <- paste0("x", 1:29)
  m <- lm(y ~ ., d)

  expect_message(
    k <- suppressWarnings(ols_step_forward_p(m, p_val = 0.5)),
    "Residual sum of squares < 1e-10"
  )
  expect_gt(nrow(k$metrics), 0)
  expect_equal(nrow(k$metrics), length(k$metrics$variable))
  expect_equal(k$metrics$step, seq_len(nrow(k$metrics)))
  expect_equal(attr(terms(k$model), "term.labels"), k$metrics$variable)
  expect_lt(deviance(k$model), 1e-6)
  expect_equal(length(coef(k$model)), nrow(k$metrics) + 1L)
  expect_equal(k$metrics$variable,
               c("x13", "x23", "x2", "x16", "x18", "x22", "x27", "x29"))

  expect_message(
    a <- suppressWarnings(ols_step_forward_aic(m)),
    "Residual sum of squares < 1e-10"
  )
  expect_gt(nrow(a$metrics), 0)
  expect_equal(nrow(a$metrics), length(a$metrics$variable))
  expect_equal(attr(terms(a$model), "term.labels"), a$metrics$variable)
  expect_equal(a$metrics$variable,
               c("x13", "x23", "x2", "x16", "x18", "x22", "x27", "x29"))
})
