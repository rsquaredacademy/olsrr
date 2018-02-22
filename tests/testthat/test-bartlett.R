context("bartlett_test")

test_that("all output from the test match the result", {

  b <- ols_bartlett_test(mtcars, mpg, disp)

  expect_equal(round(b$fstat, 3), 142.336)
  expect_equal(b$pval, 0)
  expect_equal(b$df, 1)
  expect_equivalent(b$var_c, c("mpg", "disp"))
  expect_null(b$g_var)

  b <- ols_bartlett_test(descriptr::mtcarz, mpg, group_var = vs)

  expect_equal(round(b$fstat, 3), 1.585)
  expect_equal(round(b$pval, 3), 0.208)
  expect_equal(b$df, 1)
  expect_equal(b$var_c, "mpg")
  expect_equal(b$g_var, "vs")

})


test_that("when group_var = NA, at least two variables must be specified", {
  expect_error(
    ols_bartlett_test(mtcars, mpg),
    "Please specify at least two variables."
  )
})
