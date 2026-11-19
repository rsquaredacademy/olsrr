fullmodel <- lm(y ~ x1 + x2 + x3 + x4, data = cement)

test_that("aic output matches expected result", {
  expect_equal(ols_aic(fullmodel, method = "R"), AIC(fullmodel))
  expect_equal(ols_aic(fullmodel, method = "STATA"), 63.83669)
  expect_equal(round(ols_aic(fullmodel, method = "SAS"), 3), 26.944)
  expect_equal(round(ols_aic(fullmodel, method = "SAS", TRUE), 3), 55.944)

  model <- lm(y ~ x1 + x2 + x4, data = cement)
  expect_equal(ols_aic(model, method = "R"), AIC(model))
  expect_equal(round(ols_aic(model, method = "STATA"), 4), 61.8663)

  model <- lm(y ~ x2 + x4, data = cement)
  expect_equal(ols_aic(model, method = "R"), AIC(model))
  expect_equal(round(ols_aic(model, method = "STATA"), 4), 97.5217)
})


test_that("sbc output matches expected result", {
  expect_equal(ols_sbc(fullmodel, method = "R"), BIC(fullmodel))
  expect_equal(round(ols_sbc(fullmodel, method = "STATA"), 4), 66.6614)
  expect_equal(round(ols_sbc(fullmodel, method = "SAS"), 4), 29.769)

  model <- lm(y ~ x1 + x2 + x4, data = cement)
  expect_equal(ols_sbc(model, method = "R"), BIC(model))
  expect_equal(round(ols_sbc(model, method = "STATA"), 4), 64.1261)

  model <- lm(y ~ x2 + x4, data = cement)
  expect_equal(ols_sbc(model, method = "R"), BIC(model))
  expect_equal(round(ols_sbc(model, method = "STATA"), 4), 99.2166)
})

test_that("apc output matches expected result", {
  expect_equal(round(ols_apc(fullmodel), 3), 0.04)

  model <- lm(y ~ x1, data = cement)
  expect_equal(round(ols_apc(model), 3), 0.636)

  model <- lm(y ~ x4, data = cement)
  expect_equal(round(ols_apc(model), 3), 0.444)

  model <- lm(y ~ x1 + x2 + x4, data = cement)
  expect_equal(round(ols_apc(model), 3), 0.033)

  model <- lm(y ~ x1 + x3 + x4, data = cement)
  expect_equal(round(ols_apc(model), 3), 0.035)

  model <- lm(y ~ x2 + x4, data = cement)
  expect_equal(round(ols_apc(model), 3), 0.512)
})


test_that("mallow's cp output matches expected result", {
  model <- lm(y ~ x1, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 202.55
  expect_equal(act, exp)

  model <- lm(y ~ x4, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 138.73
  expect_equal(act, exp)

  model <- lm(y ~ x1 + x2, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 2.68
  expect_equal(act, exp)

  model <- lm(y ~ x2 + x4, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 138.23
  expect_equal(act, exp)

  model <- lm(y ~ x1 + x2 + x4, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 3.02
  expect_equal(act, exp)

  model <- lm(y ~ x1 + x3 + x4, data = cement)
  act <- round(ols_mallows_cp(model, fullmodel), 2)
  exp <- 3.50
  expect_equal(act, exp)
})

test_that("ols_msep matches theoretical formula across models (#224)", {
  # 1. mtcars simple regression
  m_mtcars <- lm(mpg ~ disp, data = mtcars)
  n_mtcars <- nrow(mtcars)
  p_mtcars <- m_mtcars$rank
  mse_mtcars <- deviance(m_mtcars) / m_mtcars$df.residual
  expected_msep_mtcars <- ((n_mtcars + 1) * (n_mtcars - 2) * mse_mtcars) / (n_mtcars * (n_mtcars - p_mtcars - 1))
  expect_equal(ols_msep(m_mtcars), expected_msep_mtcars)
  expect_equal(ols_msep(m_mtcars), 11.27827, tolerance = 1e-4)

  # 2. cement data
  expected_msep_cement <- ((13 + 1) * (13 - 2) * (deviance(fullmodel) / fullmodel$df.residual)) /
    (13 * (13 - fullmodel$rank - 1))
  expect_equal(ols_msep(fullmodel), expected_msep_cement)

  # 3. surgical data
  m_surg <- lm(y ~ bcs + pindex + enzyme_test + liver_test, data = surgical)
  n_surg <- nrow(surgical)
  p_surg <- m_surg$rank
  mse_surg <- deviance(m_surg) / m_surg$df.residual
  expected_msep_surg <- ((n_surg + 1) * (n_surg - 2) * mse_surg) / (n_surg * (n_surg - p_surg - 1))
  expect_equal(ols_msep(m_surg), expected_msep_surg)

  # 4. factor model (auto)
  d_auto <- na.omit(auto)
  d_auto$rep78 <- as.factor(d_auto$rep78)
  m_fac <- lm(price ~ mpg + rep78, data = d_auto)
  n_fac <- nrow(d_auto)
  p_fac <- m_fac$rank
  mse_fac <- deviance(m_fac) / m_fac$df.residual
  expected_msep_fac <- ((n_fac + 1) * (n_fac - 2) * mse_fac) / (n_fac * (n_fac - p_fac - 1))
  expect_equal(ols_msep(m_fac), expected_msep_fac)
})

test_that("information criteria count parameters by rank for factors and collinear models (#224)", {
  # Factor model
  d_auto <- na.omit(auto)
  d_auto$rep78 <- as.factor(d_auto$rep78)
  m_fac <- lm(price ~ mpg + rep78, data = d_auto)
  n_fac <- nrow(d_auto)
  p_fac <- m_fac$rank
  mse_fac <- deviance(m_fac) / m_fac$df.residual
  r2_fac <- summary(m_fac)$r.squared

  expect_equal(p_fac, 6) # Intercept + mpg + 4 factor dummies
  expect_equal(ols_fpe(m_fac), ((n_fac + p_fac) / n_fac) * mse_fac)
  expect_equal(ols_apc(m_fac), ((n_fac + p_fac) / (n_fac - p_fac)) * (1 - r2_fac))
  expect_equal(ols_hsp(m_fac), mse_fac / (n_fac - p_fac - 1))

  # Surgical model
  m_surg <- lm(y ~ bcs + pindex + enzyme_test + liver_test, data = surgical)
  n_surg <- nrow(surgical)
  p_surg <- m_surg$rank
  mse_surg <- deviance(m_surg) / m_surg$df.residual
  r2_surg <- summary(m_surg)$r.squared

  expect_equal(ols_fpe(m_surg), ((n_surg + p_surg) / n_surg) * mse_surg)
  expect_equal(ols_apc(m_surg), ((n_surg + p_surg) / (n_surg - p_surg)) * (1 - r2_surg))
  expect_equal(ols_hsp(m_surg), mse_surg / (n_surg - p_surg - 1))

  # Collinear model in Mallows' Cp
  d_collin <- mtcars
  d_collin$hp2 <- d_collin$hp * 2
  m_collin <- lm(mpg ~ hp + hp2, data = d_collin)
  m_full_collin <- lm(mpg ~ ., data = mtcars)

  p_collin <- m_collin$rank
  expect_equal(p_collin, 2)
  expect_equal(length(m_collin$coefficients), 3) # aliased coef present
  expected_cp <- (deviance(m_collin) / (deviance(m_full_collin) / m_full_collin$df.residual)) -
    (nrow(mtcars) - 2 * p_collin)
  expect_equal(ols_mallows_cp(m_collin, m_full_collin), expected_cp)
})

test_that("ols_sbic uses deviance extracts and respects near-perfect fit guard (#224)", {
  m <- lm(mpg ~ disp, data = mtcars)
  m_full <- lm(mpg ~ ., data = mtcars)
  n <- nrow(mtcars)
  p <- m$rank
  sse <- deviance(m)
  mse_full <- deviance(m_full) / m_full$df.residual
  q <- (n * mse_full) / sse
  expected_sbic <- (log(sse / n) * n) + 2 * (p + 2) * q - 2 * (q ^ 2)
  expect_equal(ols_sbic(m, m_full), expected_sbic)

  # Near-perfect fit guard
  df_zero <- data.frame(y = 1:10, x = 1:10)
  m_zero <- lm(y ~ x, data = df_zero)
  expect_warning(
    res_zero <- ols_sbic(m_zero, m_zero),
    "Residual sum of squares < 1e-10; SBIC returned NA."
  )
  expect_true(is.na(res_zero))
})

test_that("ols_aic computes corrected AIC for R and STATA, and triggers denominator guards (#224)", {
  m <- lm(mpg ~ disp, data = mtcars)
  n <- nrow(mtcars)
  p <- m$rank
  k <- p + 1

  # R method AICc (Hurvich-Tsai)
  expected_aicc_r <- AIC(m) + (2 * k * (k + 1)) / (n - k - 1)
  expect_equal(ols_aic(m, method = "R", corrected = TRUE), expected_aicc_r)
  expect_equal(round(ols_aic(m, method = "R", corrected = TRUE), 4), 171.0665)

  # STATA method AICc
  aic_stata <- ols_aic(m, method = "STATA")
  expected_aicc_stata <- aic_stata + (2 * p * (p + 1)) / (n - p - 1)
  expect_equal(ols_aic(m, method = "STATA", corrected = TRUE), expected_aicc_stata)
  expect_equal(round(ols_aic(m, method = "STATA", corrected = TRUE), 4), 168.6232)

  # SAS method AICc on cement pinned
  expect_equal(round(ols_aic(fullmodel, method = "SAS", corrected = TRUE), 3), 55.944)

  # Denominator guards:
  # R method guard: n - k - 1 <= 0
  df_small_r <- data.frame(y = 1:4, x1 = c(1, 2, 3, 5), x2 = c(2, 4, 1, 3))
  m_small_r <- lm(y ~ x1 + x2, data = df_small_r) # n = 4, p = 3, k = 4, 4 - 4 - 1 = -1 <= 0
  expect_warning(
    res_r <- ols_aic(m_small_r, method = "R", corrected = TRUE),
    "Sample size n <= k \\+ 1; corrected AIC cannot be computed."
  )
  expect_true(is.na(res_r))

  # STATA method guard: n - p - 1 <= 0
  df_small_stata <- data.frame(y = 1:3, x1 = c(1, 2, 3), x2 = c(2, 1, 4))
  m_small_stata <- lm(y ~ x1 + x2, data = df_small_stata) # n = 3, p = 3, 3 - 3 - 1 = -1 <= 0
  expect_warning(
    res_stata <- ols_aic(m_small_stata, method = "STATA", corrected = TRUE),
    "Sample size n <= p \\+ 1; corrected AIC cannot be computed."
  )
  expect_true(is.na(res_stata))

  # SAS method guard: n - p - 2 <= 0
  df_small_sas <- data.frame(y = 1:4, x1 = c(1, 2, 3, 5), x2 = c(2, 4, 1, 3))
  m_small_sas <- lm(y ~ x1 + x2, data = df_small_sas) # n = 4, p = 3, 4 - 3 - 2 = -1 <= 0
  expect_warning(
    res_sas <- ols_aic(m_small_sas, method = "SAS", corrected = TRUE),
    "Sample size n <= p \\+ 2; corrected AIC cannot be computed."
  )
  expect_true(is.na(res_sas))
})
