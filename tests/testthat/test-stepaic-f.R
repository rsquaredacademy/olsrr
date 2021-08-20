context("stepaic_forward")

test_that("output from stepaic_forward matches the expected outptu", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_step_forward_aic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equivalent(k$metrics$variable, c("wt", "hp"))
  expect_equivalent(round(k$metrics$aic, 3), c(166.029, 156.652))
  expect_equivalent(round(k$metrics$ess, 3), c(278.322, 195.048))
  expect_equivalent(round(k$metrics$rss, 3), c(847.725, 930.999))
  expect_equivalent(round(k$metrics$r2, 3), c(0.753, 0.827))
  expect_equivalent(round(k$metrics$adj_r2, 3), c(0.745, 0.815))
})

test_that("output from stepaic_forward matches the expected output when variables are locked in", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_step_forward_aic(model, include = c("disp"))
  expect_equal(k$metrics$step, 1:3)
  expect_equivalent(k$metrics$variable, c("wt", "hp", "drat"))
})


