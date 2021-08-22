test_that("output from ftest is as expected", {
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  expect_snapshot(ols_test_f(model))
  expect_snapshot(ols_test_f(model, rhs = TRUE))
  expect_snapshot(ols_test_f(model, vars = c("disp", "hp")))
})

