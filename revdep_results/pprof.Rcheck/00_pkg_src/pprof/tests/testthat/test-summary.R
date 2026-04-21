test_that("summary.linear_fe function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_fe <- linear_fe(Y = Y, Z = Z, ProvID = ProvID)

  summary_df <- summary(fit_fe, parm = c("z1", "z4", "Z5"))
  expect_true(isTRUE(all.equal(rownames(summary_df), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df))
  expect_equal(colnames(summary_df), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))
})


test_that("summary.linear_re function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_re <- linear_re(Y = Y, Z = Z, ProvID = ProvID)

  summary_df <- summary(fit_re, parm = c("z1", "z4", "Z5"))
  expect_true(isTRUE(all.equal(rownames(summary_df), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df))
  expect_equal(colnames(summary_df), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))
})


test_that("summary.linear_cre function behaves correctly", {
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

  summary_df <- summary(fit_cre, parm = c("z1_within", "z2_within", "z4", "Z5"))
  expect_true(isTRUE(all.equal(rownames(summary_df), c("z1_within", "z2_within", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df))
  expect_equal(colnames(summary_df), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))
})


test_that("summary.logis_fe function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_fe <- suppressWarnings(logis_fe(Y = Y, Z = Z, ProvID = ProvID))

  summary_df1 <- summary(fit_fe, parm = c("z1", "z4", "Z5"), test = "wald")
  expect_true(isTRUE(all.equal(rownames(summary_df1), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df1))
  expect_equal(colnames(summary_df1), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))

  summary_df2 <- summary(fit_fe, parm = c("z1", "z4", "Z5"), test = "lr")
  expect_true(isTRUE(all.equal(rownames(summary_df2), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df2))
  expect_equal(colnames(summary_df2), c("Estimate", "stat", "p value"))

  summary_df3 <- summary(fit_fe, parm = c("z1", "z4", "Z5"), test = "score")
  expect_true(isTRUE(all.equal(rownames(summary_df3), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df3))
  expect_equal(colnames(summary_df3), c("Estimate", "stat", "p value"))

  expect_error(summary(fit_fe, test = "exact"), "Argument 'test' NOT as required!")
})


test_that("summary.logis_re function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_re <- logis_re(Y = Y, Z = Z, ProvID = ProvID)

  summary_df <- summary(fit_re, parm = c("z1", "z4", "Z5"))
  expect_true(isTRUE(all.equal(rownames(summary_df), c("z1", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df))
  expect_equal(colnames(summary_df), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))
})


test_that("summary.logis_cre function behaves correctly", {
  data(ExampleDataBinary)
  outcome <- ExampleDataBinary$Y
  covar <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID
  data <- data.frame(outcome, ProvID, covar)
  outcome.char <- colnames(data)[1]
  ProvID.char <- colnames(data)[2]
  wb.char <- c("z1", "z2")
  other.char <- c("z3", "z4", "z5")

  fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char, wb.char = wb.char, other.char = other.char)

  summary_df <- summary(fit_cre, parm = c("z1_within", "z2_within", "z4", "Z5"))
  expect_true(isTRUE(all.equal(rownames(summary_df), c("z1_within", "z2_within", "z4"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(is.data.frame(summary_df))
  expect_equal(colnames(summary_df), c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper" ))
})
