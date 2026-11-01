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

test_that("no-intercept lack-of-fit degrees of freedom come from the model rank (G02)", {
  m <- lm(mpg ~ 0 + disp, mtcars)
  o <- ols_pure_error_anova(m)
  nd <- length(unique(mtcars$disp))

  nested <- anova(m, lm(mpg ~ 0 + factor(disp), mtcars))

  expect_equal(o$df_rss, 1)
  expect_equal(o$df_ess, df.residual(m))
  expect_equal(o$df_ess, 31)
  expect_equal(o$df_lof, nd - m$rank)
  expect_equal(o$df_lof, nested$Df[2])
  expect_equal(o$df_error, nrow(mtcars) - nd)
  expect_equal(o$rss, sum(mtcars$mpg ^ 2) - deviance(m))
  expect_equal(o$rss, unname(anova(m)[1, 2]))

  # the overall regression F agrees with summary() and the lack-of-fit F with
  # the nested group-means ANOVA
  expect_equal(o$rf, unname(summary(m)$fstatistic[1]))
  expect_equal(o$rf, 36.56991, tolerance = 1e-5)
  expect_equal(o$pr, unname(pf(summary(m)$fstatistic[1], o$df_rss, o$df_ess,
                                lower.tail = FALSE)))
  expect_equal(o$lf, nested$F[2])
  expect_equal(o$pl, nested$`Pr(>F)`[2])

  # the same decomposition after removing a fixed offset
  mo   <- lm(mpg ~ 0 + disp + offset(qsec), mtcars)
  oo   <- ols_pure_error_anova(mo)
  adj  <- lm(I(mpg - qsec) ~ 0 + disp, mtcars)
  full <- lm(I(mpg - qsec) ~ 0 + factor(disp), mtcars)
  ref  <- anova(adj, full)

  expect_equal(oo$df_lof, ref$Df[2])
  expect_equal(oo$ess, deviance(mo))
  expect_equal(oo$lf, ref$F[2])
  expect_equal(oo$pl, ref$`Pr(>F)`[2])
  expect_equal(oo$rf, unname(summary(mo)$fstatistic[1]))
})

test_that("constant or intercept-only models have no regression test (G02)", {
  d <- transform(mtcars, x = 1)
  m <- lm(mpg ~ x, d)

  expect_error(
    ols_pure_error_anova(m),
    "no tested direction beyond the intercept"
  )

  expect_error(
    ols_pure_error_anova(lm(mpg ~ 1, mtcars)),
    "no tested direction beyond the intercept"
  )
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

test_that("offsets are removed from the response before the decomposition (H4)", {
  m <- lm(mpg ~ disp + offset(qsec), mtcars)
  r <- ols_pure_error_anova(m)

  # the same decomposition as an explicitly offset-adjusted response
  adj <- lm(I(mpg - qsec) ~ disp, mtcars)

  expect_equal(r$ess, deviance(m))
  expect_equal(r$ess, 363.40498, tolerance = 1e-6)
  expect_equal(r$lackoffit, 352.73732, tolerance = 1e-6)
  expect_equal(r$pure_error, 10.66767, tolerance = 1e-6)
  expect_equal(r$lackoffit + r$pure_error, deviance(m))

  # the regression row is the offset-adjusted fit, so the overall F agrees
  expect_equal(r$rss, unname(anova(adj)[1, 2]))
  expect_equal(r$rf, unname(summary(m)$fstatistic[1]))
  expect_equal(r$rf, 48.05078, tolerance = 1e-6)
  expect_equal(r$lf, (r$lackoffit / r$df_lof) / (r$pure_error / r$df_error))

  # the same offset supplied through the model call
  mc <- lm(mpg ~ disp, mtcars, offset = qsec)
  expect_equal(
    unlist(ols_pure_error_anova(mc)[c("rss", "ess", "lackoffit", "pure_error", "rf", "lf")]),
    unlist(r[c("rss", "ess", "lackoffit", "pure_error", "rf", "lf")])
  )

  # unit weights and missing-value actions
  mu <- lm(mpg ~ disp + offset(qsec), mtcars, weights = rep(1, 32))
  expect_equal(ols_pure_error_anova(mu)$ess, deviance(mu))
  expect_equal(ols_pure_error_anova(mu)$pure_error, r$pure_error)

  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp + offset(qsec), d, na.action = na.exclude)
  mo <- lm(mpg ~ disp + offset(qsec), d, na.action = na.omit)
  expect_equal(ols_pure_error_anova(me)$ess, deviance(me))
  expect_equal(
    unlist(ols_pure_error_anova(me)[c("rss", "ess", "lackoffit", "pure_error", "rf", "lf")]),
    unlist(ols_pure_error_anova(mo)[c("rss", "ess", "lackoffit", "pure_error", "rf", "lf")])
  )
})


