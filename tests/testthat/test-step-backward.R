test_that("backward elimination output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_backward_p(model)
  expect_equal(k$metrics$step, 1)
  expect_equal(k$metrics$variable, c("x3"))
})

test_that("backward hierarchical selection output matches the expected result", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_backward_p(model, 0.1, TRUE)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("alc_mod", "gender", "age", "liver_test"), ignore_attr = TRUE)
})
