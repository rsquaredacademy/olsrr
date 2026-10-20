test_that("output from pure_error_anova matches expected result", {
  model <- lm(mpg ~ hp, data = mtcars)
  k <- ols_pure_error_anova(model)
  act <- k$lackoffit
  exp <- 430.32
  expect_equal(round(act, 2), exp)

  act <- k$pure_error
  exp <- 17.36
  expect_equal(round(act, 2), exp)

  model <- lm(mpg ~ disp, data = mtcars)
  k <- ols_pure_error_anova(model)
  act <- k$lackoffit
  exp <- 304.28
  expect_equal(round(act, 2), exp)

  act <- k$pure_error
  exp <- 12.88
  expect_equal(round(act, 2), exp)
})

test_that("overall regression F matches summary.lm", {
  model <- lm(mpg ~ disp, data = mtcars)
  k     <- ols_pure_error_anova(model)
  fs    <- summary(model)$fstatistic

  expect_equal(k$rf, unname(fs[1]))
  expect_equal(k$pr, unname(pf(fs[1], fs[2], fs[3], lower.tail = FALSE)))
})

test_that("models without replicates are handled explicitly", {
  d     <- data.frame(x = 1:10, y = c(1, 3, 2, 5, 4, 7, 6, 9, 8, 11))
  model <- lm(y ~ x, d)
  k     <- ols_pure_error_anova(model)

  expect_equal(k$df_error, 0)
  expect_true(is.na(k$pms))
  expect_true(is.na(k$lf))
  expect_true(is.na(k$pl))
  expect_equal(k$rf, unname(summary(model)$fstatistic[1]))
})


