test_that("logis_re function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID
  data <- data.frame(Y, ProvID, Z)
  Z.char <- colnames(Z)
  Y.char <- "Y"
  ProvID.char <- "ProvID"
  formula <- as.formula(paste("Y ~", paste(Z.char, collapse = " + "), "+ (1 | ProvID)"))

  # Fit random effect linear model using three input formats
  fit_re1 <- logis_re(Y = Y, Z = Z, ProvID = ProvID)
  fit_re2 <- logis_re(data = data, Y.char = Y.char, Z.char = Z.char, ProvID.char = ProvID.char)
  fit_re3 <- logis_re(formula, data)

  # Check if the three models the correct class "logis_re"
  expect_true(all(class(fit_re1) == "logis_re", class(fit_re2) == "logis_re", class(fit_re3) == "logis_re"),
              info = "All models should be of class 'logis_re'.")

  # Check if the three models have the same result
  expect_true(all(all.equal(fit_re1$fitted, fit_re2$fitted),
                  all.equal(fit_re1$fitted, fit_re3$fitted)),
              info = "All models have the same result.")
})
