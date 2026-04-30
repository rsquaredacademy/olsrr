test_that("test.linear_fe function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_fe <- linear_fe(Y = Y, Z = Z, ProvID = ProvID)

  test_df <- test(fit_fe, parm = c(1,31:35, 99:101))
  expect_true(isTRUE(all.equal(rownames(test_df), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df))

  expect_error(test(fit_fe, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
  expect_error(test(fit_fe, null = "abc"), "Argument 'null' NOT as required!")
})


test_that("test.linear_re function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_re <- linear_re(Y = Y, Z = Z, ProvID = ProvID)

  test_df <- test(fit_re, parm = c(1,31:35, 99:101))
  expect_true(isTRUE(all.equal(rownames(test_df), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df))

  expect_error(test(fit_re, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
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

  test_df <- test(fit_cre, parm = c(1,31:35, 99:101))

  expect_true(isTRUE(all.equal(rownames(test_df), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df))

  expect_error(test(fit_cre, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
})


test_that("test.logis_fe function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_fe <- suppressWarnings(logis_fe(Y = Y, Z = Z, ProvID = ProvID))

  test_df1 <- test(fit_fe, parm = c(1,31:35, 99:101), test = "exact.poisbinom")
  expect_true(isTRUE(all.equal(rownames(test_df1), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df1), c("flag", "p value", "stat"))))
  expect_true(is.data.frame(test_df1))

  test_df2 <- test(fit_fe, parm = c(1,31:35, 99:101), test = "exact.bootstrap")
  expect_true(isTRUE(all.equal(rownames(test_df2), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df2), c("flag", "p value", "stat"))))
  expect_true(is.data.frame(test_df2))

  test_df3 <- test(fit_fe, parm = c(1,31:35, 99:101), test = "score", score_modified = TRUE)
  expect_true(isTRUE(all.equal(rownames(test_df3), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df3), c("flag", "p value", "stat"))))
  expect_true(is.data.frame(test_df3))

  test_df4 <- suppressWarnings(test(fit_fe, parm = c(1,31:35, 99:101), test = "wald"))
  expect_warning(test(fit_fe, test = "wald"), "Wald test fails for datasets with providers having all or no events. Score test or exact test are recommended.")
  expect_true(isTRUE(all.equal(rownames(test_df4), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df4), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df4))

  expect_error(test(fit_fe, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'greater', or 'less'")
  expect_error(test(fit_fe, null = "abc"), "Argument 'null' NOT as required!")
})


test_that("test.logis_re function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_re <- logis_re(Y = Y, Z = Z, ProvID = ProvID)

  test_df <- test(fit_re, parm = c(1,31:35, 99:101))
  expect_true(isTRUE(all.equal(rownames(test_df), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df))

  expect_error(test(fit_re, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
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

  test_df <- test(fit_cre, parm = c(1,31:35, 99:101))
  expect_true(isTRUE(all.equal(rownames(test_df), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(colnames(test_df), c("flag", "p value", "stat", "Std.Error"))))
  expect_true(is.data.frame(test_df))

  expect_error(test(fit_cre, alternative = "xyz"), "Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
})

