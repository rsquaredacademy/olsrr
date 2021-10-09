test_that("output from stepwise forward regression is as expected", {
  model <- lm(y ~ ., data = surgical)
  expect_snapshot(ols_step_forward_p(model))
  expect_snapshot(ols_step_forward_p(model, progress = TRUE))
  expect_snapshot(ols_step_forward_p(model, details = TRUE))
})

test_that("output from stepwise forward hierarchical regression", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  expect_snapshot(ols_step_forward_p(model, 0.1, hierarchical = TRUE))
  expect_snapshot(ols_step_forward_p(model, 0.1, hierarchical = TRUE, progress = TRUE))
  expect_snapshot(ols_step_forward_p(model, 0.1, hierarchical = TRUE, details = TRUE))
})
