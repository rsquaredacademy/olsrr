context('dfbetas_panel')

test_that('observations are correctly classified as normal and outlier', {

	model <- lm(formula = y ~ x2 + x4, data = cement)
	k <- dfbetas_panel(model)
	expect_equal(k$threshold, 0.5547002)
	expect_equivalent(unlist(k$outliers), c(8, 10, 8, 10, 10))
	
})


test_that("srplot fails when model inherits other than 'lm'", {
	  y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(dfbetas_panel(m), 'Please specify a OLS linear regression model.')
})
