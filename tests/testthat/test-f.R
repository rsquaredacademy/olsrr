context('f_test')

model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('when fitted.values == TRUE, fitted values from the regression
	are used for the test', {

		k <- f_test(model)

		expect_equal(k$f, 1.239)
		expect_equal(k$p, 0.275)
		expect_equal(k$numdf, 1)
		expect_equal(k$dendf, 30)

		expect_true(k$fv)
		expect_false(k$rhs)

		expect_null(k$vars)

		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that('when fitted_values == TRUE and rhs == TRUE, predictors from the
	model are used for the test', {

		k <- f_test(model, fitted_values = TRUE, rhs = TRUE)

		expect_equal(k$f, 0.444)
		expect_equal(k$p, 0.814)
		expect_equal(k$numdf, 5)
		expect_equal(k$dendf, 26)

		expect_false(k$fv)
		expect_true(k$rhs)

		expect_null(k$vars)

		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that('when vars != NULL, variables specified from the are
	used for the test', {

		k <- f_test(model, vars = c("disp", "hp"))

		expect_equal(k$f, 0.453)
		expect_equal(k$p, 0.64)
		expect_equal(k$numdf, 2)
		expect_equal(k$dendf, 29)

		expect_false(k$fv)
		expect_false(k$rhs)

		expect_equivalent(k$vars, c("disp", "hp"))
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that('when vars != NULL and rhs == TRUE, predictors in the model are
	used for the test', {

		k <- f_test(model, rhs = TRUE, vars = c("disp", "hp"))

		expect_equal(k$f, 0.444)
		expect_equal(k$p, 0.814)
		expect_equal(k$numdf, 5)
		expect_equal(k$dendf, 26)

		expect_false(k$fv)
		expect_true(k$rhs)

		expect_equivalent(k$vars, c("disp", "hp"))
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("f_test fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(f_test(m), 'Please specify a OLS linear regression model.')
})


test_that("f_test fails when input for fitted.values is not logical", {
    expect_error(f_test(model, fitted_values = 'TRUE'),
    	'fitted.values must be either TRUE or FALSE')
    expect_error(f_test(model, fitted_values = 0),
    	'fitted.values must be either TRUE or FALSE')
})


test_that("f_test fails when input for rhs is not logical", {
    expect_error(f_test(model, rhs = 'TRUE'),
    	'rhs must be either TRUE or FALSE')
    expect_error(f_test(model, rhs = 0),
    	'rhs must be either TRUE or FALSE')
})


test_that("f_test fails when input for vars are not a subset of predictors", {
    expect_error(f_test(model, vars = c("gear", "carb")),
    	'vars must be a subset of the predictors in the model')
})
