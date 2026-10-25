factor_model <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  list(data = d, model = lm(mpg ~ cyl + hp, d))
}

interaction_model <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  list(data = d, model = lm(mpg ~ cyl:hp, d))
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
  expect_equal(b$df, 1)
})

test_that("vars of interaction models are validated against the model matrix", {
  im <- interaction_model()
  d  <- im$data
  m  <- im$model

  expect_error(
    ols_test_f(m, vars = "hp"),
    "Model `(mpg ~ cyl:hp)` should include the predictor(s) `hp`.",
    fixed = TRUE
  )
  expect_error(
    ols_test_score(m, vars = "cyl"),
    "Model `(mpg ~ cyl:hp)` should include the predictor(s) `cyl`.",
    fixed = TRUE
  )

  k <- ols_test_f(m, vars = "cyl6:hp")

  aux <- summary(lm(I(residuals(m)^2) ~ I((cyl == "6") * hp), data = d))$fstatistic
  expect_equal(k$f, unname(aux[["value"]]))
  expect_equal(k$numdf, 1)
  expect_equal(k$dendf, 30)
})

test_that("ols_test_breusch_pagan multiple = TRUE rejects factor terms in vars", {
  fm <- factor_model()

  expect_error(
    ols_test_breusch_pagan(fm$model, vars = c("cyl", "hp"), multiple = TRUE),
    "`multiple = TRUE` does not support factor terms in `vars`.",
    fixed = TRUE
  )

  # a single factor term is not looped over and still works
  k <- ols_test_breusch_pagan(fm$model, vars = "cyl", multiple = TRUE)
  expect_equal(k$bp, 7.444201, tolerance = 1e-6)
  expect_equal(k$df, 2)

  # model-matrix column names are single columns and still work
  mm <- ols_test_breusch_pagan(fm$model, vars = c("cyl6", "hp"), multiple = TRUE)
  expect_length(mm$bp, 3)
})

test_that("breusch pagan degrees of freedom reflect the tested term", {
  fm <- factor_model()
  k  <- ols_test_breusch_pagan(fm$model, vars = "cyl")

  expect_equal(k$df, 2)
  expect_equal(k$p, pchisq(k$bp, k$df, lower.tail = FALSE))

  out <- paste(capture.output(print(k)), collapse = "\n")
  expect_match(out, "DF += +2")

  multi <- ols_test_breusch_pagan(fm$model, vars = "cyl", multiple = TRUE)
  expect_match(paste(capture.output(print(multi)), collapse = "\n"), "DF += +2")

  n <- ols_test_breusch_pagan(lm(mpg ~ hp, data = mtcars), vars = "hp")
  expect_equal(n$df, 1)
  expect_equal(n$p, pchisq(n$bp, 1, lower.tail = FALSE))
})
