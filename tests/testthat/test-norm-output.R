context("normality test output")

test_that("output from normality test is as expected", {
  x <- cat("-----------------------------------------------
       Test             Statistic       pvalue
-----------------------------------------------
Shapiro-Wilk              0.9366         0.0600
Kolmogorov-Smirnov        0.1152         0.7464
Cramer-von Mises          2.8122         0.0000
Anderson-Darling          0.5859         0.1188
-----------------------------------------------")


  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_output(print(ols_test_normality(model)), x)
})

test_that("output from ols_corr_test is as expected", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_equal(round(ols_test_correlation(model), 3), 0.97)
})
