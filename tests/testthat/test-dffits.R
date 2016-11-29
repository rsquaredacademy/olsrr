context("dffits_plot")

test_that('observations are correctly classified as normal and outlier', {

	model <- lm(formula = y ~ x2 + x4, data = cement)
	k <- dffits_plot(model)
	expect_equal(k$threshold, 0.9608)
	expect_equal(k$outliers, 10)
	
})


test_that("srplot fails when model inherits other than 'lm'", {
	  y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(dffits_plot(m), 'Please specify a OLS linear regression model.')
})
