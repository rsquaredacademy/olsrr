model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("when fitted.values == TRUE, fitted values from the regression\n\tare used for the test", {
  b <- ols_test_score(model)

  expect_equal(round(b$score, 3), 1.269)
  expect_equal(round(b$p, 3), 0.26)
  expect_equal(b$df, 1)
  expect_true(b$fv)
  expect_false(b$rhs)
  expect_match(b$preds, "fitted values of mpg")
  expect_match(b$resp, "mpg")
})

test_that("when fitted.values == TRUE and rhs == TRUE, predictors from the\n\tmodel are used for the test", {
  b <- ols_test_score(model, rhs = TRUE)

  expect_equal(round(b$score, 3), 2.516)
  expect_equal(round(b$p, 3), 0.774)
  expect_equal(b$df, 5)
  expect_false(b$fv)
  expect_true(b$rhs)
  expect_equal(b$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})


test_that("when vars != NULL, variables specified from the are\n\tused for the test", {
  b <- ols_test_score(model, vars = c("disp", "hp"))

  expect_equal(round(b$score, 3), 0.969)
  expect_equal(round(b$p, 3), 0.616)
  expect_equal(b$df, 2)
  expect_false(b$fv)
  expect_false(b$rhs)
  expect_equal(b$preds, c("disp", "hp"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})


test_that("when vars != NULL and rhs == TRUE, predictors in the model are\n\tused for the test", {
  b <- ols_test_score(model, rhs = TRUE, vars = c("disp", "hp"))

  expect_equal(round(b$score, 3), 2.516)
  expect_equal(round(b$p, 3), 0.774)
  expect_equal(b$df, 5)
  expect_false(b$fv)
  expect_true(b$rhs)
  expect_equal(b$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})



test_that("na.exclude and na.omit fits give the same score test (M5)", {
  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp + hp + wt, d, na.action = na.exclude)
  mo <- lm(mpg ~ disp + hp + wt, d, na.action = na.omit)

  expect_equal(ols_test_score(me)$score, ols_test_score(mo)$score)
  expect_equal(ols_test_score(me)$p, ols_test_score(mo)$p)
  expect_equal(ols_test_score(me)$score, 0.4865436, tolerance = 1e-6)
  expect_equal(ols_test_score(me, rhs = TRUE)$score, ols_test_score(mo, rhs = TRUE)$score)
  expect_equal(
    ols_test_score(me, vars = c("disp", "hp"))$score,
    ols_test_score(mo, vars = c("disp", "hp"))$score
  )
  expect_equal(
    ols_test_score(me, vars = c("disp", "hp"))$df,
    ols_test_score(mo, vars = c("disp", "hp"))$df
  )
})

test_that("aliased predictors do not inflate the auxiliary degrees of freedom (H8)", {
  d <- mtcars
  d$hp2 <- 2 * d$hp
  m <- lm(mpg ~ wt + hp + hp2, d)

  s <- ols_test_score(m, vars = c("hp", "hp2"))
  expect_equal(s$df, 1)
  expect_equal(s$p, pchisq(s$score, 1, lower.tail = FALSE))
  expect_equal(s$p, 0.3486117, tolerance = 1e-6)

  s1 <- ols_test_score(m, vars = "hp")
  s2 <- ols_test_score(m, vars = c("hp", "hp"))
  expect_equal(s2$score, s1$score)
  expect_equal(s2$df, 1)

  # rhs test on a rank-deficient design
  rhs <- ols_test_score(m, rhs = TRUE)
  expect_equal(rhs$df, 2)
  expect_equal(rhs$p, pchisq(rhs$score, 2, lower.tail = FALSE))

  mf <- lm(mpg ~ factor(cyl) + hp, mtcars)
  sf <- ols_test_score(mf, vars = c("factor(cyl)", "factor(cyl)6"))
  expect_equal(sf$df, 2)
  expect_equal(sf$p, pchisq(sf$score, 2, lower.tail = FALSE))
})

test_that("an auxiliary design with no tested direction is rejected (H8)", {
  d <- transform(mtcars, z = 1)
  m <- lm(mpg ~ wt + hp + z, d)

  expect_error(
    ols_test_score(m, vars = "z"),
    "no tested direction"
  )

  # neighbours still return the chi-squared reference values
  s <- ols_test_score(m, rhs = TRUE)
  expect_equal(s$df, 2)
  expect_equal(s$p, pchisq(s$score, 2, lower.tail = FALSE))
  expect_equal(s$score, 0.8807225, tolerance = 1e-6)

  sz <- ols_test_score(m, vars = "wt")
  expect_equal(sz$df, 1)
  expect_equal(sz$p, pchisq(sz$score, 1, lower.tail = FALSE))

  expect_error(
    ols_test_score(lm(mpg ~ 1, mtcars)),
    "no tested direction"
  )
})

