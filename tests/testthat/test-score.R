context('score_test')

model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('when fitted.values == TRUE, fitted values from the regression
	are used for the test', {

		b <- score_test(model)

		expect_equal(b$score, 1.269)
		expect_equal(b$p, 0.26)
		expect_equal(b$df, 1)
		expect_true(b$fv)
		expect_false(b$rhs)
		expect_match(b$preds, 'fitted values of mpg')
		expect_match(b$resp, "mpg")

})

test_that('when fitted.values == TRUE and rhs == TRUE, predictors from the
	model are used for the test', {

		b <- score_test(model, rhs = TRUE)

		expect_equal(b$score, 2.516)
		expect_equal(b$p, 0.774)
		expect_equal(b$df, 5)
		expect_false(b$fv)
		expect_true(b$rhs)
		expect_equivalent(b$preds, c("disp", "hp", "wt", "drat", "qsec"))
		expect_match(b$resp, "mpg")

})


test_that('when vars != NULL, variables specified from the are
	used for the test', {

		b <- score_test(model, vars = c("disp", "hp"))

		expect_equal(b$score, 0.969)
		expect_equal(b$p, 0.616)
		expect_equal(b$df, 2)
		expect_false(b$fv)
		expect_false(b$rhs)
		expect_equivalent(b$preds, c("disp", "hp"))
		expect_match(b$resp, "mpg")

})


test_that('when vars != NULL and rhs == TRUE, predictors in the model are
	used for the test', {

		b <- score_test(model, rhs = TRUE, vars = c("disp", "hp"))

		expect_equal(b$score, 2.516)
		expect_equal(b$p, 0.774)
		expect_equal(b$df, 5)
		expect_false(b$fv)
		expect_true(b$rhs)
		expect_equivalent(b$preds, c("disp", "hp", "wt", "drat", "qsec"))
		expect_match(b$resp, "mpg")

})


test_that("score_test fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(score_test(m), 'Please specify a OLS regression model.')
})


test_that("score_test fails when input for fitted.values is not logical", {
    expect_error(score_test(model, fitted_values = 'TRUE'),
    	'fitted_values must be either TRUE or FALSE')
    expect_error(score_test(model, fitted_values = 0),
    	'fitted_values must be either TRUE or FALSE')
})


test_that("score_test fails when input for rhs is not logical", {
    expect_error(score_test(model, rhs = 'TRUE'),
    	'rhs must be either TRUE or FALSE')
    expect_error(score_test(model, rhs = 0),
    	'rhs must be either TRUE or FALSE')
})


test_that("score_test fails when input for vars are not a subset of predictors", {
    expect_error(score_test(model, vars = c("gear", "carb")),
    	'vars must be a subset of the predictors in the model')
})
