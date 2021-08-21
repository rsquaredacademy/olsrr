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
  x <- cat("Tolerance and Variance Inflation Factor
---------------------------------------
# A tibble: 4 x 3
  Variables Tolerance      VIF
      <chr>     <dbl>    <dbl>
1      disp 0.1218116 8.209402
2        hp 0.3454979 2.894373
3        wt 0.1962092 5.096601
4      drat 0.4386836 2.279547


Eigenvalue and Condition Index
------------------------------
   Eigenvalue Condition Index    intercept        disp          hp
1 4.692806914        1.000000 0.0002323252 0.001106455 0.002566185
2 0.240308641        4.419078 0.0036813894 0.034132904 0.031334562
3 0.052153430        9.485821 0.0009192095 0.058394262 0.735003722
4 0.011406889       20.283026 0.0014476535 0.885725642 0.207337511
5 0.003324127       37.573144 0.9937194224 0.020640737 0.023758021
            wt         drat
1 0.0007172086 0.0003775503
2 0.0009394254 0.0148250672
3 0.0700789813 0.0026259361
4 0.7179834661 0.0568226912
5 0.2102809185 0.9253487552")

  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_output(print(ols_coll_diag(model)), x)
})
