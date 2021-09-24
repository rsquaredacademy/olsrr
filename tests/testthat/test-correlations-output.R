test_that("output from ols_correlations is as expected", {
	model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
	expect_snapshot(ols_correlations(model))
})

