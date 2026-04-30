test_that("linear_fe function behaves correctly", {
  data(ExampleDataLinear)
  Y <- ExampleDataLinear$Y
  Z <- ExampleDataLinear$Z
  ProvID <- ExampleDataLinear$ProvID
  data <- data.frame(Y, ProvID, Z)
  Z.char <- colnames(Z)
  Y.char <- "Y"
  ProvID.char <- "ProvID"
  formula <- as.formula(paste("Y ~", paste(Z.char, collapse = " + "), "+ id(ProvID)"))

  # Fit random effect linear model using three input formats
  fit_fe1 <- linear_fe(Y = Y, Z = Z, ProvID = ProvID)
  fit_fe2 <- linear_fe(data = data, Y.char = Y.char, Z.char = Z.char, ProvID.char = ProvID.char)
  fit_fe3 <- linear_fe(formula, data)

  # Check if the three models the correct class "linear_fe"
  expect_true(all(class(fit_fe1) == "linear_fe", class(fit_fe2) == "linear_fe", class(fit_fe3) == "linear_fe"),
              info = "All models should be of class 'linear_fe'.")

  # Check if the three models have the same result
  expect_true(all(all.equal(fit_fe1$fitted, fit_fe2$fitted),
                  all.equal(fit_fe1$fitted, fit_fe3$fitted)),
              info = "All models have the same result.")
})
