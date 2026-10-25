factor_model <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  list(data = d, model = lm(mpg ~ cyl + hp, d))
}

test_that("ols_test_f accepts a factor term in vars", {
  fm <- factor_model()
  d  <- fm$data
  m  <- fm$model

  k <- ols_test_f(m, vars = "cyl")

  aux <- summary(lm(I(residuals(m)^2) ~ cyl, data = d))$fstatistic
  expect_equal(k$f, unname(aux[["value"]]))
  expect_equal(k$numdf, unname(aux[["numdf"]]))
  expect_equal(k$dendf, unname(aux[["dendf"]]))
  expect_equal(k$p, unname(pf(k$f, k$numdf, k$dendf, lower.tail = FALSE)))
  expect_equal(k$vars, "cyl", ignore_attr = TRUE)
})

test_that("ols_test_score accepts a factor term in vars", {
  fm <- factor_model()
  d  <- fm$data
  m  <- fm$model

  k <- ols_test_score(m, vars = "cyl")

  g   <- residuals(m)^2
  ind <- g / mean(g) - 1
  score <- nrow(d) * summary(lm(ind ~ cyl, data = d))$r.squared

  expect_equal(k$score, score)
  expect_equal(k$df, 2)
  expect_equal(k$p, pchisq(score, 2, lower.tail = FALSE))
})

test_that("ols_test_breusch_pagan accepts a factor term in vars", {
  fm <- factor_model()
  d  <- fm$data
  m  <- fm$model

  k <- ols_test_breusch_pagan(m, vars = "cyl")

  g  <- residuals(m)^2
  u  <- g / mean(g) - 1
  z  <- model.matrix(~ cyl, data = d)
  bp <- 0.5 * as.numeric(t(u) %*% z %*% solve(crossprod(z)) %*% t(z) %*% u)

  expect_equal(k$bp, bp)
  expect_equal(k$p, pchisq(bp, 2, lower.tail = FALSE))

  if (requireNamespace("lmtest", quietly = TRUE)) {
    expect_equal(k$bp, unname(lmtest::bptest(m, ~ cyl, data = d, studentize = FALSE)$statistic))
  }
})

test_that("numeric vars give the same results as before", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)

  f <- ols_test_f(model, vars = "hp")
  expect_equal(f$f, 3.320145896183725, tolerance = 1e-12)
  expect_equal(f$p, 0.078419117380157, tolerance = 1e-12)
  expect_equal(f$numdf, 1)
  expect_equal(f$dendf, 30)

  s <- ols_test_score(model, vars = "hp")
  expect_equal(s$score, 3.188601544810396, tolerance = 1e-12)
  expect_equal(s$p, 0.074153422473379, tolerance = 1e-12)
  expect_equal(s$df, 1)

  b <- ols_test_breusch_pagan(model, vars = "hp")
  expect_equal(b$bp, 2.587322117382067, tolerance = 1e-12)
  expect_equal(b$p, 0.107722324021430, tolerance = 1e-12)
})
