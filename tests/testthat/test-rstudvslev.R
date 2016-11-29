context("studvslev_plot")

test_that('observations are correctly classified as normal, outlier and/or both', {

	model <- lm(formula = Nitrogen ~ ComIndl + Agr + Forest + Rsdntial, data = rivers)
	k <- studvslev_plot(model)
	expect_equal(k$lev_threshold, 0.6)
	expect_equal(k$rstud_threshold, 2)
	expect_equivalent(k$normal, c(1, 2, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20))
	expect_equivalent(k$outliers, c(7, 19))
	expect_equivalent(k$out_lev, c(4, 5))
	
})


test_that("studvslev_plot fails when model inherits other than 'lm'", {
	  y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(studvslev_plot(m), 'Please specify a OLS linear regression model.')
})
