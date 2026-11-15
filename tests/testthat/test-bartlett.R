test_that("output from the test match the result when using variables", {

  b <- ols_test_bartlett(mtcars, 'mpg', 'disp')

  expect_equal(round(b$fstat, 3), 142.335)
  expect_equal(b$pval, 0)
  expect_equal(b$df, 1)
  expect_equal(b$var_c, c("mpg", "disp"), ignore_attr = TRUE)
  expect_null(b$g_var)

})

test_that("output from test match the result when using grouping variables", {
  if (requireNamespace("descriptr", quietly = TRUE)) {
    b <- ols_test_bartlett(descriptr::mtcarz, 'mpg', group_var = 'vs')

    expect_equal(round(b$fstat, 3), 1.585)
    expect_equal(round(b$pval, 3), 0.208)
    expect_equal(b$df, 1)
    expect_equal(b$var_c, "mpg")
    expect_equal(b$g_var, "vs")
  }
})

test_that("bartlett test throws error messages", {
  expect_error(ols_test_bartlett(mtcars, 'mpg'), "Please specify at least two variables.")
})

test_that("bartlett test matches stats::bartlett.test for a numeric group variable", {
  b    <- ols_test_bartlett(mtcars, "mpg", group_var = "cyl")
  base <- bartlett.test(mpg ~ cyl, mtcars)

  expect_equal(b$df, unname(base$parameter))
  expect_equal(b$fstat, unname(base$statistic))
  expect_equal(b$pval, base$p.value)
})

test_that("bartlett test matches stats::bartlett.test for a factor group variable", {
  d      <- mtcars
  d$cyl  <- factor(d$cyl)
  b      <- ols_test_bartlett(d, "mpg", group_var = "cyl")
  base   <- bartlett.test(mpg ~ cyl, d)

  expect_equal(b$df, unname(base$parameter))
  expect_equal(b$fstat, unname(base$statistic))
  expect_equal(b$pval, base$p.value)
})

test_that("bartlett test matches stats::bartlett.test when the variable has missing values", {
  d       <- mtcars
  d$mpg[3] <- NA
  b       <- ols_test_bartlett(d, "mpg", group_var = "cyl")
  base    <- bartlett.test(mpg ~ cyl, d)

  expect_equal(b$df, unname(base$parameter))
  expect_equal(b$fstat, unname(base$statistic))
  expect_equal(b$pval, base$p.value)
})




test_that("invalid group sizes give the same errors as bartlett.test() (L1)", {
  expect_error(
    ols_test_bartlett(data.frame(y = 1:3, g = "a"), "y", group_var = "g"),
    "all observations are in the same group"
  )
  expect_error(
    ols_test_bartlett(data.frame(y = 1:3, g = c("a", "a", "b")), "y", group_var = "g"),
    "there must be at least 2 observations in each group"
  )

  # NA filtering happens before the check
  expect_error(
    ols_test_bartlett(
      data.frame(y = c(1, 2, NA, 3), g = c("a", "a", "a", "b")),
      "y",
      group_var = "g"
    ),
    "there must be at least 2 observations in each group"
  )

  # a valid two-group comparison still matches base R
  d  <- data.frame(y = c(1, 2, 3, 4, 8, 9), g = rep(c("a", "b"), each = 3))
  bt <- stats::bartlett.test(y ~ g, d)
  k  <- ols_test_bartlett(d, "y", group_var = "g")

  expect_equal(k$df, unname(bt$parameter))
  expect_equal(k$fstat, unname(bt$statistic))
  expect_equal(k$pval, bt$p.value)
})
