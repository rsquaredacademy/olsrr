test_that("stepwise selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_aic(model)
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$method, c("addition", "addition", "addition"), ignore_attr = TRUE)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2"), ignore_attr = TRUE)
})

test_that("output from stepaic_both matches the expected output when variables are locked in", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_aic(model, include = c("x6"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_aic(model, include = c(6))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepaic_both matches the expected output when variables are locked out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_aic(model, exclude = c("x2"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_aic(model, exclude = c(2))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x6", "x1", "x3", "x4"), ignore_attr = TRUE)
})

test_that("output from stepaic_both matches the expected output when variables are locked in and out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_aic(model, include = c("x6"), exclude = c("x2"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x1", "x3", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_aic(model, include = c(6), exclude = c(2))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("x1", "x3", "x4"), ignore_attr = TRUE)
})

test_that("stepaic_both returns the appropriate error", {

  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  expect_error(ols_step_both_aic(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_aic(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_aic(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_aic(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

test_that("stepwise sbc selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_sbc(model)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "removal"), ignore_attr = TRUE)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepsbc_both matches the expected output when variables are locked in", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbc(model, include = c("x6"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2"), ignore_attr = TRUE)

  h <- ols_step_both_sbc(model, include = c(6))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("x1", "x3", "x2"), ignore_attr = TRUE)
})

test_that("output from stepsbc_both matches the expected output when variables are locked out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbc(model, exclude = c("x2"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3"), ignore_attr = TRUE)

  h <- ols_step_both_sbc(model, exclude = c(2))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("x6", "x1", "x3"), ignore_attr = TRUE)
})

test_that("output from stepsbc_both matches the expected output when variables are locked in and out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbc(model, include = c("x6"), exclude = c("x2"))
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("x1", "x3"), ignore_attr = TRUE)

  h <- ols_step_both_sbc(model, include = c(6), exclude = c(2))
  expect_equal(h$metrics$step, 1:2)
  expect_equal(h$metrics$variable, c("x1", "x3"), ignore_attr = TRUE)
})

test_that("stepsbc_both returns the appropriate error", {

  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  expect_error(ols_step_both_sbc(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_sbc(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_sbc(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_sbc(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})

test_that("stepwise sbic selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_both_sbic(model)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$method, c("addition", "addition", "addition", "removal"), ignore_attr = TRUE)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepsbic_both matches the expected output when variables are locked in", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbic(model, include = c("x6"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_sbic(model, include = c(6))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x1", "x3", "x2", "x4"), ignore_attr = TRUE)
})

test_that("output from stepsbic_both matches the expected output when variables are locked out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbic(model, exclude = c("x2"))
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("x6", "x1", "x3", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_sbic(model, exclude = c(2))
  expect_equal(h$metrics$step, 1:4)
  expect_equal(h$metrics$variable, c("x6", "x1", "x3", "x4"), ignore_attr = TRUE)
})

test_that("output from stepsbic_both matches the expected output when variables are locked in and out", {

  model <- lm(y ~ ., data = stepdata)
  k <- ols_step_both_sbic(model, include = c("x6"), exclude = c("x2"))
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x1", "x3", "x4"), ignore_attr = TRUE)

  h <- ols_step_both_sbic(model, include = c(6), exclude = c(2))
  expect_equal(h$metrics$step, 1:3)
  expect_equal(h$metrics$variable, c("x1", "x3", "x4"), ignore_attr = TRUE)
})

test_that("stepsbic_both returns the appropriate error", {

  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  expect_error(ols_step_both_sbic(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_both_sbic(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_both_sbic(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_both_sbic(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})