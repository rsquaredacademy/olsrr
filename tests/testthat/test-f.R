model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("when fitted.values == TRUE, fitted values from the regression\n\tare used for the test", {
  k <- ols_test_f(model)

  expect_equal(round(k$f, 3), 1.239)
  expect_equal(round(k$p, 3), 0.275)
  expect_equal(k$numdf, 1)
  expect_equal(k$dendf, 30)

  expect_true(k$fv)
  expect_false(k$rhs)

  expect_null(k$vars)

  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when fitted_values == TRUE and rhs == TRUE, predictors from the\n\tmodel are used for the test", {
  k <- ols_test_f(model, fitted_values = TRUE, rhs = TRUE)

  expect_equal(round(k$f, 3), 0.444)
  expect_equal(round(k$p, 3), 0.814)
  expect_equal(k$numdf, 5)
  expect_equal(k$dendf, 26)

  expect_false(k$fv)
  expect_true(k$rhs)

  expect_null(k$vars)

  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when vars != NULL, variables specified from the are\n\tused for the test", {
  k <- ols_test_f(model, vars = c("disp", "hp"))

  expect_equal(round(k$f, 3), 0.453)
  expect_equal(round(k$p, 3), 0.64)
  expect_equal(k$numdf, 2)
  expect_equal(k$dendf, 29)

  expect_false(k$fv)
  expect_false(k$rhs)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when vars != NULL and rhs == TRUE, predictors in the model are\n\tused for the test", {
  k <- ols_test_f(model, rhs = TRUE, vars = c("disp", "hp"))

  expect_equal(round(k$f, 3), 0.444)
  expect_equal(round(k$p, 3), 0.814)
  expect_equal(k$numdf, 5)
  expect_equal(k$dendf, 26)

  expect_false(k$fv)
  expect_true(k$rhs)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})



test_that("na.exclude and na.omit fits give the same F test (M5)", {
  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp + hp + wt, d, na.action = na.exclude)
  mo <- lm(mpg ~ disp + hp + wt, d, na.action = na.omit)

  k_me <- ols_test_f(me)
  k_mo <- ols_test_f(mo)

  expect_equal(k_me$f, k_mo$f)
  expect_equal(k_me$p, k_mo$p)
  expect_equal(k_me$numdf, k_mo$numdf)
  expect_equal(k_me$dendf, k_mo$dendf)
  expect_equal(k_me$f, 0.4624112, tolerance = 1e-6)

  expect_equal(ols_test_f(me, rhs = TRUE)$f, ols_test_f(mo, rhs = TRUE)$f)
  expect_equal(
    ols_test_f(me, vars = c("disp", "hp"))$f,
    ols_test_f(mo, vars = c("disp", "hp"))$f
  )
})
