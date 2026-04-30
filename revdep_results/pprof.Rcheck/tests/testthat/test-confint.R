test_that("test.linear_fe function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_fe <- linear_fe(Y = Y, Z = Z, ProvID = ProvID)

  confint_gamma <- confint(fit_fe, parm = c(1,31:35, 99:101), option = "gamma", stdz = c("indirect", "direct"))
  expect_true(is.data.frame(confint_gamma))
  expect_true(isTRUE(all.equal(colnames(confint_gamma), c("gamma", "gamma.Lower", "gamma.Upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_gamma), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_fe, option = "gamma", alternative = "greater"), "Provider effect (option = 'gamma') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_df <- confint(fit_fe, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect", "CI.direct"  ))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_fe, option = "alpha"), "Argument 'option' should be 'gamma' or 'SM'")
})


test_that("test.linear_re function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_re <- linear_re(Y = Y, Z = Z, ProvID = ProvID)

  confint_alpha <- confint(fit_re, parm = c(1,31:35, 99:101), option = "alpha", stdz = c("indirect", "direct"))
  expect_true(is.data.frame(confint_alpha))
  expect_true(isTRUE(all.equal(colnames(confint_alpha), c("Estimate", "alpha.Lower", "alpha.Upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_alpha), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_re, option = "alpha", alternative = "greater"), "Provider effect (option = 'alpha') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_df <- confint(fit_re, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect", "CI.direct"  ))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_re, option = "gamma"), "Argument 'option' should be 'alpha' or 'SM'")
})


test_that("test.linear_cre function behaves correctly", {
  data(ExampleDataLinear)
  outcome <- ExampleDataLinear$Y
  covar <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID
  data <- data.frame(outcome, ProvID, covar)
  outcome.char <- colnames(data)[1]
  ProvID.char <- colnames(data)[2]
  wb.char <- c("z1", "z2")
  other.char <- c("z3", "z4", "z5")

  fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char, wb.char = wb.char, other.char = other.char)

  confint_alpha <- confint(fit_cre, parm = c(1,31:35, 99:101), option = "alpha", stdz = c("indirect", "direct"))
  expect_true(is.data.frame(confint_alpha))
  expect_true(isTRUE(all.equal(colnames(confint_alpha), c("Estimate", "alpha.Lower", "alpha.Upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_alpha), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_cre, option = "alpha", alternative = "greater"), "Provider effect (option = 'alpha') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_df <- confint(fit_cre, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect", "CI.direct"  ))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_cre, option = "gamma"), "Argument 'option' should be 'alpha' or 'SM'")
})


test_that("test.logis_fe function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_fe <- suppressWarnings(logis_fe(Y = Y, Z = Z, ProvID = ProvID))

  confint_gamma1 <- suppressWarnings(confint(fit_fe, parm = c(1,31:35, 99:101), option = "gamma", stdz = c("indirect", "direct"), test = "wald"))
  expect_true(is.data.frame(confint_gamma1))
  expect_true(isTRUE(all.equal(colnames(confint_gamma1), c("gamma", "gamma.lower", "gamma.upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_gamma1), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_fe, option = "gamma", alternative = "greater"), "Provider effect (option = 'gamma') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_gamma2<- confint(fit_fe, parm = c(1,31:35, 99:101), option = "gamma", stdz = c("indirect", "direct"), test = "exact")
  expect_true(is.data.frame(confint_gamma2))
  expect_true(isTRUE(all.equal(colnames(confint_gamma2), c("gamma", "gamma.lower", "gamma.upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_gamma2), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")

  confint_gamma3<- confint(fit_fe, parm = c(1,31:35, 99:101), option = "gamma", stdz = c("indirect", "direct"), test = "score")
  expect_true(is.data.frame(confint_gamma3))
  expect_true(isTRUE(all.equal(colnames(confint_gamma3), c("gamma", "gamma.lower", "gamma.upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_gamma3), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")

  confint_df <- confint(fit_fe, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect_ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct_rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect_ratio", "CI.indirect_rate", "CI.direct_ratio", "CI.direct_rate"))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_fe, option = "alpha"), "Argument 'option' should be 'gamma' or 'SM'")
})


test_that("test.logis_re function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_re <- logis_re(Y = Y, Z = Z, ProvID = ProvID)

  confint_alpha <- confint(fit_re, parm = c(1,31:35, 99:101), option = "alpha", stdz = c("indirect", "direct"))
  expect_true(is.data.frame(confint_alpha))
  expect_true(isTRUE(all.equal(colnames(confint_alpha), c("Estimate", "alpha.Lower", "alpha.Upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_alpha), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_re, option = "alpha", alternative = "greater"), "Provider effect (option = 'alpha') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_df <- confint(fit_re, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect_ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct_rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect_ratio", "CI.indirect_rate", "CI.direct_ratio", "CI.direct_rate"))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_re, option = "gamma"), "Argument 'option' should be 'alpha' or 'SM'")
})



test_that("test.logis_cre function behaves correctly", {
  data(ExampleDataBinary)
  outcome <- ExampleDataBinary$Y
  covar <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID
  data <- data.frame(outcome, ProvID, covar)
  outcome.char <- colnames(data)[1]
  ProvID.char <- colnames(data)[2]
  wb.char <- c("z1", "z2")
  other.char <- c("z3", "z4", "z5")

  fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char, wb.char = wb.char, other.char = other.char)

  confint_alpha <- confint(fit_cre, parm = c(1,31:35, 99:101), option = "alpha", stdz = c("indirect", "direct"))
  expect_true(is.data.frame(confint_alpha))
  expect_true(isTRUE(all.equal(colnames(confint_alpha), c("Estimate", "alpha.Lower", "alpha.Upper"))))
  expect_true(isTRUE(all.equal(rownames(confint_alpha), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_error(confint(fit_cre, option = "alpha", alternative = "greater"), "Provider effect (option = 'alpha') only supports two-sided confidence intervals.", fixed = TRUE)

  confint_df <- confint(fit_cre, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.indirect_ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(confint_df$CI.direct_rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(names(confint_df), c("CI.indirect_ratio", "CI.indirect_rate", "CI.direct_ratio", "CI.direct_rate"))))
  expect_true(is.list(confint_df))

  expect_error(confint(fit_cre, option = "gamma"), "Argument 'option' should be 'alpha' or 'SM'")
})

