test_that("logis_fe function behaves correctly", {
  data(ExampleDataBinary)
  Y <- ExampleDataBinary$Y
  Z <- ExampleDataBinary$Z
  ProvID <- ExampleDataBinary$ProvID
  data <- data.frame(Y, ProvID, Z)
  Z.char <- colnames(Z)
  Y.char <- "Y"
  ProvID.char <- "ProvID"
  formula <- as.formula(paste("Y ~", paste(Z.char, collapse = " + "), "+ id(ProvID)"))

  # Fit random effect linear model using three input formats
  fit_fe1 <- suppressWarnings(logis_fe(Y = Y, Z = Z, ProvID = ProvID))
  fit_fe2 <- suppressWarnings(logis_fe(data = data, Y.char = Y.char, Z.char = Z.char, ProvID.char = ProvID.char))
  fit_fe3 <- suppressWarnings(logis_fe(formula, data))

  # Check if the three models the correct class "logis_fe"
  expect_true(all(class(fit_fe1) == "logis_fe", class(fit_fe2) == "logis_fe", class(fit_fe3) == "logis_fe"),
              info = "All models should be of class 'logis_fe'.")

  # Check if the three models have the same result
  expect_true(all(all.equal(fit_fe1$fitted, fit_fe2$fitted),
                  all.equal(fit_fe1$fitted, fit_fe3$fitted)),
              info = "All models have the same result.")
})
