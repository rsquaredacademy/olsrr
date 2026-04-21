test_that("linear_re function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID
  data <- data.frame(Y, ProvID, Z)
  Z.char <- colnames(Z)
  Y.char <- "Y"
  ProvID.char <- "ProvID"
  formula <- as.formula(paste("Y ~", paste(Z.char, collapse = " + "), "+ (1 | ProvID)"))

  # Fit random effect linear model using three input formats
  fit_re1 <- linear_re(Y = Y, Z = Z, ProvID = ProvID)
  fit_re2 <- linear_re(data = data, Y.char = Y.char, Z.char = Z.char, ProvID.char = ProvID.char)
  fit_re3 <- linear_re(formula, data)


  # Check if the three models the correct class "linear_re"
  expect_true(all(class(fit_re1) == "linear_re", class(fit_re2) == "linear_re", class(fit_re3) == "linear_re"),
              info = "All models should be of class 'linear_re'.")

  # Check if the three models have the same result
  expect_true(all(all.equal(fit_re1$fitted, fit_re2$fitted),
                  all.equal(fit_re1$fitted, fit_re3$fitted)),
              info = "All models have the same result.")
})
