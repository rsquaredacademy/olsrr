context("dsrvsp_plot")

test_that('observations are correctly classified as normal and outlier', {

	model <- lm(formula = Nitrogen ~ ComIndl + Agr + Forest + Rsdntial, data = rivers)
	k <- dsrvsp_plot(model)
	expect_equal(k$threshold, 2)
	expect_equivalent(k$normal, c(1, 2, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20))
	expect_equivalent(k$outlier, c(4, 5, 7, 19))
	
})


test_that("dsrvsp_plot fails when model inherits other than 'lm'", {
	  y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(dsrvsp_plot(m), 'Please specify a OLS linear regression model.')
})
