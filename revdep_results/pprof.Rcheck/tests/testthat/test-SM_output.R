test_that("SM_output.linear_fe function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_fe <- linear_fe(Y = Y, Z = Z, ProvID = ProvID)
  SM_median <- SM_output(fit_fe, stdz = c("indirect", "direct"))
  expect_true(all.equal(SM_median$indirect.difference, SM_median$direct.difference, check.attributes = FALSE),
              info = "ISD and DSD are equal.")

  SM_mean <- SM_output(fit_fe, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"), null = "mean")
  expect_true(isTRUE(all.equal(rownames(SM_mean$indirect.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM_mean$direct.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM_mean, "list")
  expect_equal(names(SM_mean), c("indirect.difference", "direct.difference", "OE"))

  expect_error(SM_output(fit_fe, stdz = "xyz"), "Argument 'stdz' NOT as required!")
  expect_error(SM_output(fit_fe, null = "abc"), "Argument 'null' NOT as required!")
})


test_that("SM_output.linear_re function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID

  fit_re <- linear_re(Y = Y, Z = Z, ProvID = ProvID)
  SM <- SM_output(fit_re, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))

  expect_true(isTRUE(all.equal(rownames(SM$indirect.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM$direct.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM, "list")
  expect_equal(names(SM), c("indirect.difference", "direct.difference", "OE"))

  expect_error(SM_output(fit_re, stdz = "xyz"), "Argument 'stdz' NOT as required!")
})


test_that("SM_output.linear_cre function behaves correctly", {
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

  SM <- SM_output(fit_cre, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))

  expect_true(isTRUE(all.equal(rownames(SM$indirect.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM$direct.difference), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM, "list")
  expect_equal(names(SM), c("indirect.difference", "direct.difference", "OE"))

  expect_error(SM_output(fit_cre, stdz = "xyz"), "Argument 'stdz' NOT as required!")
})


test_that("SM_output.logis_fe function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_fe <- suppressWarnings(logis_fe(Y = Y, Z = Z, ProvID = ProvID))

  SM_mean <- SM_output(fit_fe, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(SM_mean$indirect.ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM_mean$direct.rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM_mean, "list")
  expect_equal(names(SM_mean), c("indirect.ratio", "indirect.rate", "direct.ratio", "direct.rate", "OE"))

  expect_error(SM_output(fit_fe, stdz = "xyz"), "Argument 'stdz' NOT as required!")
  expect_error(SM_output(fit_fe, null = "abc"), "Argument 'null' NOT as required!")
  expect_error(SM_output(fit_fe, measure = "mn"), "Argument 'measure' NOT as required!")
})


test_that("SM_output.logis_re function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID

  fit_re <- logis_re(Y = Y, Z = Z, ProvID = ProvID)

  SM <- SM_output(fit_re, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(SM$indirect.ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM$direct.rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM, "list")
  expect_equal(names(SM), c("indirect.ratio", "indirect.rate", "direct.ratio", "direct.rate", "OE"))

  expect_error(SM_output(fit_re, stdz = "xyz"), "Argument 'stdz' NOT as required!")
  expect_error(SM_output(fit_re, measure = "mn"), "Argument 'measure' NOT as required!")
})


test_that("SM_output.logis_cre function behaves correctly", {
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

  SM <- SM_output(fit_cre, parm = c(1,31:35, 99:101), stdz = c("indirect", "direct"))
  expect_true(isTRUE(all.equal(rownames(SM$indirect.ratio), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_true(isTRUE(all.equal(rownames(SM$direct.rate), c("1","31","32","33","34","35","99","100"))),
              info = "Argument 'parm' performs correctly.")
  expect_type(SM, "list")
  expect_equal(names(SM), c("indirect.ratio", "indirect.rate", "direct.ratio", "direct.rate", "OE"))

  expect_error(SM_output(fit_cre, stdz = "xyz"), "Argument 'stdz' NOT as required!")
  expect_error(SM_output(fit_cre, measure = "mn"), "Argument 'measure' NOT as required!")
})
