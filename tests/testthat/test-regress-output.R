test_that("output from regress is as expected", {
  expect_snapshot(ols_regress(mpg ~ disp + hp + wt, data = mtcars))
  expect_snapshot(ols_regress(lm(mpg ~ disp + hp + wt, data = mtcars)))
})

