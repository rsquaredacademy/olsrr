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

test_that("If model includes interaction terms, ols_regress scales and centers
          predictors before computing standardized betas", {
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

test_that("MSE and RMSE use the ANOVA residual mean square (#221)", {
  # 1. Verification on mtcars simple regression
  m_mtcars_simple <- lm(mpg ~ disp, data = mtcars)
  res_simple <- ols_regress(m_mtcars_simple)
  expect_equal(res_simple$mse, 10.57196, tolerance = 1e-4)
  expect_equal(res_simple$rmse, 3.251454, tolerance = 1e-4)
  expect_equal(res_simple$mse, anova(m_mtcars_simple)["Residuals", "Mean Sq"])
  expect_equal(res_simple$rmse, sqrt(deviance(m_mtcars_simple) / m_mtcars_simple$df.residual))

  # 2. Verification on surgical data (multiple regression)
  m_surgical <- lm(y ~ bcs + pindex + enzyme_test + liver_test, data = surgical)
  res_surgical <- ols_regress(m_surgical)
  expected_mse_surg <- deviance(m_surgical) / m_surgical$df.residual
  expect_equal(res_surgical$mse, expected_mse_surg)
  expect_equal(res_surgical$rmse, sqrt(expected_mse_surg))
  expect_equal(res_surgical$mse, anova(m_surgical)["Residuals", "Mean Sq"])

  # 3. Verification on cement data
  m_cement <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  res_cement <- ols_regress(m_cement)
  expected_mse_cement <- deviance(m_cement) / m_cement$df.residual
  expect_equal(res_cement$mse, expected_mse_cement)
  expect_equal(res_cement$rmse, sqrt(expected_mse_cement))
  expect_equal(res_cement$mse, anova(m_cement)["Residuals", "Mean Sq"])

  # 4. Verification on factor model (auto)
  d_auto <- na.omit(auto)
  d_auto$rep78 <- as.factor(d_auto$rep78)
  m_auto <- lm(price ~ mpg + weight + rep78, data = d_auto)
  res_auto <- ols_regress(m_auto)
  expected_mse_auto <- deviance(m_auto) / m_auto$df.residual
  expect_equal(res_auto$mse, expected_mse_auto)
  expect_equal(res_auto$rmse, sqrt(expected_mse_auto))
  expect_equal(res_auto$mse, anova(m_auto)["Residuals", "Mean Sq"])
})

test_that("ols_regress refits transformed and factor terms safely (audit F1)", {
  models <- list(
    log(mpg) ~ disp + hp,
    mpg ~ factor(cyl) + hp,
    mpg ~ poly(disp, 2) + hp,
    mpg ~ I(disp^2) + hp
  )

  for (f in models) {
    m   <- lm(f, data = mtcars)
    got <- ols_regress(m)
    ref <- lm(f, data = mtcars)

    expect_equal(coef(got$model), coef(ref))
    expect_equal(got$rsq, summary(ref)$r.squared)
    expect_equal(got$aic, ols_aic(ref))
    expect_equal(got$rmse, sqrt(deviance(ref) / ref$df.residual))
  }
})

test_that("refits use the model's rows, matched by name (audit F1)", {
  d <- mtcars
  rownames(d) <- NULL
  d <- d[-c(1, 2), ]
  d$mpg[29:30] <- NA
  m <- lm(log(mpg) ~ disp + hp, data = d)

  expect_equal(unname(ols_regress(m)$betas), unname(coef(m)))
  expect_equal(nobs(ols_step_backward_aic(lm(mpg ~ disp + hp, data = d))$model), 28L)
})

test_that("standardised betas of interaction models keep the response untransformed", {
  m <- lm(log(mpg) ~ disp * hp, data = mtcars)

  expect_silent(got <- ols_regress(m)$sbetas)

  ref <- lm(log(mpg) ~ scale(disp) * scale(hp), data = mtcars)
  expected <- coef(ref)[-1] * apply(model.matrix(ref)[, -1, drop = FALSE], 2, sd) / sd(log(mtcars$mpg))

  expect_equal(unname(got), unname(expected))
})

test_that("ols_regress.lm() forwards conf.level, iterm and title (audit F2)", {
  m <- lm(mpg ~ disp + hp, data = mtcars)

  expect_equal(ols_regress(m, conf.level = 0.90)$conf_lm, confint(m, level = 0.90))
  expect_equal(
    ols_regress(m, conf.level = 0.90)$conf_lm["disp", 1],
    confint(m, level = 0.90)["disp", 1]
  )

  out <- capture.output(print(ols_regress(m, title = "my model")))
  expect_true(any(grepl("my model", out, fixed = TRUE)))

  m_inter <- lm(mpg ~ disp * wt, data = mtcars)
  expect_equal(
    ols_regress(m_inter, iterm = TRUE)$sbetas,
    ols_regress(mpg ~ disp * wt, data = mtcars, iterm = TRUE)$sbetas
  )
  expect_error(ols_regress(m_inter, iterm = "yes"), "must be a flag")
})

test_that("standardised betas keep every coefficient of no-intercept models (audit F3)", {
  m        <- lm(mpg ~ 0 + disp + hp, data = mtcars)
  res      <- ols_regress(m)
  expected <- coef(m) * c(sd(mtcars$disp), sd(mtcars$hp)) / sd(mtcars$mpg)

  expect_length(res$betas, 2)
  expect_length(res$sbetas, 2)
  expect_equal(names(res$sbetas), names(coef(m)))
  expect_equal(unname(res$sbetas), unname(expected))

  m1 <- lm(mpg ~ 0 + disp, data = mtcars)
  expect_length(ols_regress(m1)$sbetas, 1)
  expect_equal(
    unname(ols_regress(m1)$sbetas),
    unname(coef(m1) * sd(mtcars$disp) / sd(mtcars$mpg))
  )

  # the printed Std. Beta column is aligned with the coefficients by name
  out <- capture.output(print(res))
  std_beta <- function(term) {
    row <- Filter(function(r) identical(r[1], term), lapply(out, function(l) {
      strsplit(trimws(l), "[[:space:]]+")[[1]]
    }))
    as.numeric(row[[1]][4])
  }
  expect_equal(std_beta("disp"), unname(round(expected[["disp"]], 3)))
  expect_equal(std_beta("hp"), unname(round(expected[["hp"]], 3)))
})
