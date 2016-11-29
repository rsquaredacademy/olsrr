context("pure_error_anova")

test_that("output from pure_error_anova matches expected result", {

	model <- lm(mpg ~ cyl, data = mtcars)
	k <- pure_error_anova(model)
	act <- k$lackoffit
	exp <- 7.07
	expect_equal(act, exp)
	
	act <- k$pure_error
	exp <- 301.26
	expect_equal(act, exp)

	model <- lm(mpg ~ disp, data = mtcars)
	k <- pure_error_anova(model)
	act <- k$lackoffit
	exp <- 304.28
	expect_equal(act, exp)
	
	act <- k$pure_error
	exp <- 12.88
	expect_equal(act, exp)

})


test_that("pure_error_anova fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(pure_error_anova(m), 'Please specify a OLS linear regression model.')
})


test_that("when object is not a simple linear regression, error is thrown", {
	model <- lm(mpg ~ cyl + disp, data = mtcars)
	expect_error(pure_error_anova(model), 
		"Lack of fit F test is available only for simple linear regression.")
})
