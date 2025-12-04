test_that("output from anova is as expected", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  result <- ols_anova(model)
  expect_equal(result$error_df, 28)
  expect_equal(result$model_df, 3)
  expect_equal(result$total_df, 31)
  expect_equal(round(result$ess, 3), 194.991)
  expect_equal(round(result$tss, 3), 1126.047)
  expect_equal(round(result$rss, 3), 931.057)
  expect_equal(round(result$rms, 3), 310.352)
  expect_equal(round(result$ems, 3), 6.964)
  expect_equal(round(result$f, 3), 44.566)
  expect_equal(round(result$p, 3), 0)
})

test_that("ols_anova prints the expected output", {
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  expect_snapshot(ols_anova(model))
})
