test_that("backward elimination output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_backward_p(model)
  expect_equal(k$metrics$step, 1)
  expect_equal(k$metrics$variable, c("x3"))

  k1 <- ols_step_backward_p(model, include = "x3")
  expect_equal(k1$metrics$step, 1)
  expect_equal(k1$metrics$variable, c("x4"))

  k2 <- ols_step_backward_p(model, p_remove = 0.1, exclude = "x4")
  expect_equal(k2$metrics$step, 1)
  expect_equal(k2$metrics$variable, c("x3"))

})

test_that("backward hierarchical selection output matches the expected result", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_backward_p(model, 0.1, hierarchical = TRUE)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("alc_mod", "gender", "age", "liver_test"), ignore_attr = TRUE)
})

test_that("step_backward_p returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_backward_p(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_backward_p(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_backward_p(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_backward_p(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})