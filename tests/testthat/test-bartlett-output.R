test_that("output from bartlett test is as expected when using variables", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
------------------------------------------------
Ho: Variances are equal across groups
Ha: Variances are unequal for atleast two groups

        Data
 ---------------------
 Variables: read write

        Test Summary
 ----------------------------
 DF            =    1
 Chi2          =    1.222871
 Prob > Chi2   =    0.2687979")

  expect_output(print(ols_test_bartlett(hsb, 'read', 'write')), x)
})


test_that("output from bartlett test is as expected when using grouping variable", {
  x <- cat("
    Bartlett's Test of Homogenity of Variances
           ------------------------------------------------
           Ho: Variances are equal across groups
           Ha: Variances are unequal for atleast two groups

           Test Summary
           ----------------------------
           DF            =    1
           Chi2          =    1.584876
           Prob > Chi2   =    0.2080597")

  expect_output(print(ols_test_bartlett(descriptr::mtcarz, 'mpg', group_var = 'vs')), x)
})


