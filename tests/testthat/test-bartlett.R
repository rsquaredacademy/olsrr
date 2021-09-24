test_that("all output from the test match the result", {

  b <- ols_test_bartlett(mtcars, 'mpg', 'disp')

  expect_equal(round(b$fstat, 3), 142.336)
  expect_equal(b$pval, 0)
  expect_equal(b$df, 1)
  expect_equal(b$var_c, c("mpg", "disp"), ignore_attr = TRUE)
  expect_null(b$g_var)

  b <- ols_test_bartlett(descriptr::mtcarz, 'mpg', group_var = 'vs')

  expect_equal(round(b$fstat, 3), 1.585)
  expect_equal(round(b$pval, 3), 0.208)
  expect_equal(b$df, 1)
  expect_equal(b$var_c, "mpg")
  expect_equal(b$g_var, "vs")

})



