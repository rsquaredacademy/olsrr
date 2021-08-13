context("step_forward")

test_that("forward selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_forward_p(model)
  expect_equal(k$metrics$step, 1:3)
  expect_equivalent(k$metrics$variable, c("x4", "x1", "x2"))
})

test_that("forward hierarchical selection output matches the expected result", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_forward_p(model, 0.1, TRUE)
  expect_equal(k$metrics$step, 1:4)
  expect_equivalent(k$metrics$variable, c("bcs", "alc_heavy", "pindex", "enzyme_test"))
})

