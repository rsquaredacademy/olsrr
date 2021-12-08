model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

test_that("output from stepaic_backward matches the expected output", {
  k <- ols_step_backward_aic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("disp", "drat"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 3), c(156.731, 156.652), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.837, 0.827), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.819, 0.815), ignore_attr = TRUE)

  k <- ols_step_backward_aic(model, include = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

  k <- ols_step_backward_aic(model, exclude = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

})

test_that("stepaic_backward returns the appropriate error", {
  expect_error(ols_step_backward_aic(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_backward_aic(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_backward_aic(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_backward_aic(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

test_that("output from stepsbc_backward matches the expected output", {
  k <- ols_step_backward_sbc(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("disp", "drat"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 3), c(156.731, 156.652), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.837, 0.827), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.819, 0.815), ignore_attr = TRUE)

  k <- ols_step_backward_sbc(model, include = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

  k <- ols_step_backward_sbc(model, exclude = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

})

test_that("stepsbc_backward returns the appropriate error", {
  expect_error(ols_step_backward_sbc(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_backward_sbc(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_backward_sbc(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_backward_sbc(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

test_that("output from stepsbic_backward matches the expected output", {
  k <- ols_step_backward_sbic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("disp", "drat"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 3), c(156.731, 156.652), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.837, 0.827), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.819, 0.815), ignore_attr = TRUE)

  k <- ols_step_backward_sbic(model, include = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

  k <- ols_step_backward_sbic(model, exclude = c("drat"))
  expect_equal(k$metrics$variable, c("disp"), ignore_attr = TRUE)

})

test_that("stepsbic_backward returns the appropriate error", {
  expect_error(ols_step_backward_sbic(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_backward_sbic(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_backward_sbic(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_backward_sbic(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

