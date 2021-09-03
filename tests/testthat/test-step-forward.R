test_that("forward selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_forward_p(model)
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2"), ignore_attr = TRUE)

  k1 <- ols_step_forward_p(model, include = c("x3"))
  expect_equal(k1$metrics$step, 1:3)
  expect_equal(k1$metrics$variable, c("x3", "x4", "x1"), ignore_attr = TRUE)

  k2 <- ols_step_forward_p(model, exclude = c("x4"))
  expect_equal(k2$metrics$step, 1:3)
  expect_equal(k2$metrics$variable, c("x2", "x1", "x3"), ignore_attr = TRUE)
})

test_that("forward hierarchical selection output matches the expected result", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_forward_p(model, 0.1, hierarchical = TRUE)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("bcs", "alc_heavy", "pindex", "enzyme_test"), ignore_attr = TRUE)
})


test_that("step_forward_p returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_forward_p(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_forward_p(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_forward_p(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_forward_p(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})