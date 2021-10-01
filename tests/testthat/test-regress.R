model <- ols_regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("regress returns all the model validation metrics", {
  sb  <- c(disp = 0.179, hp = -0.234, wt = -0.712, drat = 0.179, qsec = 0.190)
  lm1 <- c("(Intercept)" = -6.00372864, disp = -0.01427933, hp = -0.05201291, wt = -6.94137515, drat = -0.67585793, qsec = -0.30404593)
  lm2 <- c("(Intercept)" = 39.07086783, disp = 0.03171968, hp = 0.01081675, wt = -1.82955263, drat = 4.70740704, qsec = 1.58434573)

  expect_equal(model$r, 0.9213657)
  expect_equal(round(model$cv, 3), 12.732)
  expect_equal(round(model$mae, 3), 1.84)
  expect_equal(round(model$prsq, 4), 0.7666)
  expect_equal(round(model$sbetas, 3), sb)
  expect_equal(model$conf_lm[, 1], lm1)
  expect_equal(model$conf_lm[, 2], lm2)
})

test_that("If model includes interaction terms, ols_regress scales and centers\n          predictors before computing standardized betas", {
  model_inter <- ols_regress(price ~ weight * displacement, data = auto, iterm = TRUE)
  actual   <- round(model_inter$sbetas, 2)
  expected <- c(weight = 0.57, displacement = -0.15, `weight:displacement` = 0.38)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("ols_regress returns error messages", {
  expect_error(ols_regress(mpg ~ disp + hp + wt), "data missing")
  expect_error(ols_regress(mpg ~ disp + hp + wt, data = mtcars, conf.level = "0.95"), "conf.level must be numeric")
  expect_error(ols_regress(mpg ~ disp + hp + wt, data = mtcars, conf.level = 1.95), "conf.level must be between 0 and 1")
  expect_error(ols_regress(mpg ~ disp + hp + wt, data = mtcars, title = 1), "1 is not a string, Please specify a string as title.")
})

