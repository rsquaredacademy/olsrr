context("pure_error_anova")

test_that("output from pure_error_anova matches expected result", {

	model <- lm(mpg ~ hp, data = mtcars)
	k <- ols_pure_error_anova(model)
	act <- k$lackoffit
	exp <- 430.32
	expect_equal(round(act, 2), exp)

	act <- k$pure_error
	exp <- 17.36
	expect_equal(round(act, 2), exp)

	model <- lm(mpg ~ disp, data = mtcars)
	k <- ols_pure_error_anova(model)
	act <- k$lackoffit
	exp <- 304.28
	expect_equal(round(act, 2), exp)

	act <- k$pure_error
	exp <- 12.88
	expect_equal(round(act, 2), exp)

})


test_that("pure_error_anova fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_pure_error_anova(m), 'Please specify a OLS linear regression model.')
})


test_that("when object is not a simple linear regression, error is thrown", {
	model <- lm(mpg ~ cyl + disp, data = mtcars)
	expect_error(ols_pure_error_anova(model),
		"Lack of fit F test is available only for simple linear regression.")
})
