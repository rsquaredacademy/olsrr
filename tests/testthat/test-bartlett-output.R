test_that("output from bartlett test is as expected when using variables", {
  expect_snapshot(ols_test_bartlett(hsb, 'read', 'write'))
  expect_snapshot(ols_test_bartlett(descriptr::mtcarz, 'mpg', group_var = 'vs'))
})

