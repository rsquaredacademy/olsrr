test_that("output from stepwise backward regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_backward_p(model))
  expect_snapshot(ols_step_backward_p(model, progress = TRUE))
  expect_snapshot(ols_step_backward_p(model, details = TRUE))
})

test_that("output from stepwise backward hierarchical regression", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  expect_snapshot(ols_step_backward_p(model, hierarchical = TRUE))
  expect_snapshot(ols_step_backward_p(model, hierarchical = TRUE, progress = TRUE))
  expect_snapshot(ols_step_backward_p(model, hierarchical = TRUE, details = TRUE))
})
