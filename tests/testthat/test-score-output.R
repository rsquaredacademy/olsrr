test_that("when fitted.values == TRUE, fitted vals from the reg are used", {
  model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)
  expect_snapshot(ols_test_score(model))
  expect_snapshot(ols_test_score(model, rhs = TRUE))
  expect_snapshot(ols_test_score(model, vars = c("disp", "hp")))
  expect_snapshot(ols_test_score(model, rhs = TRUE, vars = c("disp", "hp")))
})
