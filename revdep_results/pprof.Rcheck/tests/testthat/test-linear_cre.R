test_that("linear_cre function behaves correctly", {
  data(ExampleDataLinear)
  outcome <- ExampleDataLinear$Y
  covar   <- ExampleDataLinear$Z
  ProvID  <- ExampleDataLinear$ProvID
  data    <- data.frame(outcome, ProvID, covar)

  outcome.char <- colnames(data)[1]
  ProvID.char  <- colnames(data)[2]
  wb.char      <- c("z1", "z2")
  other.char   <- c("z3", "z4", "z5")

  fit_cre <- linear_cre(data = data, Y.char = outcome.char, wb.char = wb.char, other.char = other.char, ProvID.char = ProvID.char)

  expect_s3_class(fit_cre, "linear_cre")

  fe_names <- rownames(fit_cre$coefficient$FE)
  expect_true(all(c("z1_within","z2_within","z1_bar","z2_bar") %in% fe_names))
  expect_true(all(other.char %in% fe_names))

  expect_equal(nrow(fit_cre$fitted), nrow(fit_cre$observation))
})
