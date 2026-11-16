hsb$race_1 <- ifelse(hsb$race == 1, 1, 0)
hsb$race_2 <- ifelse(hsb$race == 2, 1, 0)
hsb$race_3 <- ifelse(hsb$race == 3, 1, 0)
hsb$race_4 <- ifelse(hsb$race == 4, 1, 0)

model <- lm(
  write ~ read + math + science + race_2 + race_3 + race_4,
  data = hsb
)


test_that("output from vif_tol matches expected result", {
  act <- ols_vif_tol(model)
  Variables <- c("read", "math", "science", "race_2", "race_3", "race_4")
  Tolerance <- c(0.482, 0.469, 0.475, 0.692, 0.602, 0.467)
  VIF <- c(2.074, 2.132, 2.104, 1.446, 1.662, 2.141)
  exp <- data.frame(Variables, Tolerance, VIF)
  expect_equal(round(act$Tolerance, 3), exp$Tolerance, ignore_attr = TRUE)
  expect_equal(round(act$VIF, 3), exp$VIF, ignore_attr = TRUE)
})


test_that("output from eigen_cindex matches expected result", {
  act <- ols_eigen_cindex(model)
  col1 <- c(4.865, 1.002, 1.000, 0.091, 0.018, 0.013, 0.011)
  col2 <- c(1.000, 2.203, 2.205, 7.298, 16.263, 19.583, 21.447)
  col3 <- c(0.001, 0.000, 0.000, 0.009, 0.874, 0.049, 0.067)
  col4 <- c(0.001, 0.000, 0.000, 0.012, 0.240, 0.375, 0.373)
  col5 <- c(0.001, 0.000, 0.000, 0.009, 0.016, 0.017, 0.957)
  col6 <- c(0.001, 0.000, 0.000, 0.007, 0.024, 0.904, 0.064)
  col7 <- c(0.002, 0.003, 0.608, 0.367, 0.003, 0.000, 0.017)
  col8 <- c(0.002, 0.479, 0.012, 0.431, 0.061, 0.013, 0.002)
  col9 <- c(0.004, 0.014, 0.006, 0.962, 0.002, 0.011, 0.001)
  exp <- data.frame(col1, col2, col3, col4, col5, col6, col7, col8, col9)
  names(exp) <- c("Eigenvalue", "Condition Index", "intercept", "read", "math", "science", "race_2", "race_3", "race_4")
  expect_equal(round(act, 3), exp, ignore_attr = TRUE)
})


test_that("output from ols_coll_diag is as expected", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_snapshot(ols_coll_diag(model))
})

test_that("VIF and tolerance work for one predictor and without an intercept (M4)", {
  m1 <- lm(mpg ~ wt, mtcars)
  v1 <- ols_vif_tol(m1)

  expect_equal(v1$Variables, "wt")
  expect_equal(v1$Tolerance, 1)
  expect_equal(v1$VIF, 1)

  m2 <- lm(mpg ~ 0 + wt + hp, mtcars)
  expect_warning(
    v2 <- ols_vif_tol(m2),
    "VIFs may not be sensible in a model without an intercept"
  )

  expect_equal(v2$Variables, c("wt", "hp"))
  expect_equal(
    v2$Tolerance,
    c(1 - summary(lm(wt ~ hp, mtcars))$r.squared,
      1 - summary(lm(hp ~ wt, mtcars))$r.squared)
  )
  expect_equal(v2$VIF, 1 / v2$Tolerance)

  # multi-predictor model with an intercept is unchanged
  m3 <- lm(mpg ~ disp + hp + wt, mtcars)
  v3 <- ols_vif_tol(m3)
  expect_equal(
    v3$VIF,
    c(1 / (1 - summary(lm(disp ~ hp + wt, mtcars))$r.squared),
      1 / (1 - summary(lm(hp ~ disp + wt, mtcars))$r.squared),
      1 / (1 - summary(lm(wt ~ disp + hp, mtcars))$r.squared))
  )
})
