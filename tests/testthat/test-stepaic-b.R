test_that("output from stepaic_backward matches the expected outptu", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_step_backward_aic(model)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("disp", "drat"), ignore_attr = TRUE)
  expect_equal(round(k$metrics$aic, 3), c(156.731, 156.652), ignore_attr = TRUE)
  expect_equal(round(k$metrics$ess, 3), c(183.682, 195.048), ignore_attr = TRUE)
  expect_equal(round(k$metrics$rss, 3), c(942.365, 930.999), ignore_attr = TRUE)
  expect_equal(round(k$metrics$r2, 3), c(0.837, 0.827), ignore_attr = TRUE)
  expect_equal(round(k$metrics$adj_r2, 3), c(0.819, 0.815), ignore_attr = TRUE)
})

