context("studres_chart")

test_that('observations are correctly classified as normal and outlier', {

	model <- lm(formula = y ~ x2 + x4, data = cement)
	k <- studres_chart(model)
	expect_equal(k$outlier, 10)
	
})


test_that("studres_chart fails when model inherits other than 'lm'", {
	  y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(studres_chart(m), 'Please specify a OLS linear regression model.')
})
