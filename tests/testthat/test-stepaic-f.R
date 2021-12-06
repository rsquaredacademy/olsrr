model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

test_that("output from stepaic_forward matches the expected output", {  
  k <- ols_step_forward_aic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("wt", "hp"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 3), c(166.029, 156.652), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.753, 0.827), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.745, 0.815), ignore_attr = TRUE)
})

test_that("output from stepaic_forward matches the expected output when variables are locked in", {

  k <- ols_step_forward_aic(model, include = c("disp"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("wt", "hp", "drat"), ignore_attr = TRUE)

  h <- ols_step_forward_aic(model, include = c(1))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("wt", "hp", "drat"), ignore_attr = TRUE)
})

test_that("output from stepaic_forward matches the expected output when variables are locked out", {

  k <- ols_step_forward_aic(model, exclude = c("hp"))
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("wt", "disp"), ignore_attr = TRUE)

  h <- ols_step_forward_aic(model, exclude = c(2))
  expect_equal(h$metrics$step, 1:2)
  expect_equal(h$metrics$variable, c("wt", "disp"), ignore_attr = TRUE)
})

test_that("output from stepaic_forward matches the expected output when variables are locked in and out", {

  k <- ols_step_forward_aic(model, include = c("disp"), exclude = c("hp"))
  expect_equal(k$metrics$step, 1)
  expect_equal(k$metrics$variable, c("wt"), ignore_attr = TRUE)

  h <- ols_step_forward_aic(model, include = c(1), exclude = c(2))
  expect_equal(h$metrics$step, 1)
  expect_equal(h$metrics$variable, c("wt"), ignore_attr = TRUE)
})


test_that("stepaic_forward returns the appropriate error", {
  expect_error(ols_step_forward_aic(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_forward_aic(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_forward_aic(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_forward_aic(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})