context("stepaic_backward")

test_that("output from stepaic_backward matches the expected outptu", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_step_backward_aic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equivalent(k$metrics$variable, c("disp", "drat"))
  expect_equivalent(round(k$metrics$aic, 3), c(156.731, 156.652))
  expect_equivalent(round(k$metrics$ess, 3), c(183.682, 195.048))
  expect_equivalent(round(k$metrics$rss, 3), c(942.365, 930.999))
  expect_equivalent(round(k$metrics$r2, 3), c(0.837, 0.827))
  expect_equivalent(round(k$metrics$adj_r2, 3), c(0.819, 0.815))
})

