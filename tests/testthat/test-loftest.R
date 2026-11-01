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

test_that("unit weights give the same lack-of-fit ANOVA as no weights (H4)", {
  m  <- lm(mpg ~ disp, mtcars)
  mu <- lm(mpg ~ disp, mtcars, weights = rep(1, 32))

  r  <- ols_pure_error_anova(m)
  ru <- ols_pure_error_anova(mu)

  expect_equal(
    unlist(ru[c("rf", "lackoffit", "pure_error", "ess", "lf", "pl")]),
    unlist(r[c("rf", "lackoffit", "pure_error", "ess", "lf", "pl")])
  )
  expect_equal(ru$ess, deviance(mu))
  expect_equal(ru$ess, deviance(m))
  expect_equal(ru$rf, unname(summary(mu)$fstatistic[1]))
  expect_equal(ru$resp, "mpg")
  expect_equal(ru$preds, "disp")

  # na.exclude fits use the unpadded fitted values
  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp, d, na.action = na.exclude)
  mo <- lm(mpg ~ disp, d, na.action = na.omit)

  expect_equal(
    unlist(ols_pure_error_anova(me)[c("rf", "lackoffit", "pure_error", "ess")]),
    unlist(ols_pure_error_anova(mo)[c("rf", "lackoffit", "pure_error", "ess")])
  )
  expect_equal(ols_pure_error_anova(me)$ess, deviance(me))
})


