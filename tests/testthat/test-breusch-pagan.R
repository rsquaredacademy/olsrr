context('bp_test')

model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('when fitted.values == TRUE, fitted values from the regression
	are used for the test', {

		k <- ols_bp_test(model)

		expect_equal(k$bp, 1.2555)
		expect_equal(k$p, 0.2625)

		expect_true(k$fv)
		expect_false(k$rhs)
		expect_false(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))


})

test_that('when rhs == TRUE, predictors from the regression
	are used for the test', {

		k <- ols_bp_test(model, rhs = TRUE)

		expect_equal(k$bp, 2.489)
		expect_equal(k$p, 0.7781)

		expect_true(k$fv)
		expect_true(k$rhs)
		expect_false(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))


})

test_that('when rhs == TRUE and multiple == TRUE, multiple p values are
	returned', {

		k <- ols_bp_test(model, rhs = TRUE, multiple = TRUE)

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
		expect_equivalent(k$p, c(0.3365, 0.3817, 0.3787, 0.3786, 0.2560, 0.7781))

		expect_true(k$fv)
		expect_true(k$rhs)
		expect_true(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'bonferroni'
	bonferroni adjusted p values are returned", {

		k <- ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
		expect_equivalent(k$p, c(1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.7781))

		expect_true(k$fv)
		expect_true(k$rhs)
		expect_true(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "bonferroni")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))


})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'holm',
	bonferroni adjusted p values are returned", {

		k <- ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "holm")

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
		expect_equivalent(k$p, c(1.0000, 0.3817, 0.7574, 1.0000, 1.0000, 0.7781))

		expect_true(k$fv)
		expect_true(k$rhs)
		expect_true(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "holm")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'sidak',
	bonferroni adjusted p values are returned", {

		k <- ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "sidak")

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
		expect_equivalent(k$p, c(0.8714, 0.9096, 0.9074, 0.9074, 0.7720, 0.7781))

		expect_true(k$fv)
		expect_true(k$rhs)
		expect_true(k$multiple)

		expect_output(k$vars, NA)

		expect_match(k$padj, "sidak")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))


})

test_that('when vars != NA, variables specified are used for the test', {

		k <- ols_bp_test(model, vars = c("disp"))

		expect_equal(k$bp, 0.9237)
		expect_equal(k$p, 0.3365)

		expect_false(k$fv)
		expect_false(k$rhs)
		expect_false(k$multiple)

		expect_match(k$vars, "disp")
		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})

test_that('when rhs == TRUE and vars != NA, variables specified
	used for the test', {

		k <- ols_bp_test(model, vars = c("disp", "hp"), rhs = TRUE)

		expect_equal(k$bp, 2.489)
		expect_equal(k$p, 0.7781)

		expect_false(k$fv)
		expect_true(k$rhs)
		expect_false(k$multiple)

		expect_equivalent(k$vars, c( "disp", "hp"))

		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that('when rhs == FALSE, multiple == TRUE and vars != NA,
	variables specified are used for the test', {

		k <- ols_bp_test(model, multiple = TRUE, rhs = FALSE, vars = c("disp", "hp"))

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.9588))
		expect_equivalent(k$p, c(0.3365, 0.3817, 0.6192))

		expect_false(k$fv)
		expect_false(k$rhs)
		expect_true(k$multiple)

		expect_equivalent(k$vars, c( "disp", "hp"))

		expect_match(k$padj, "none")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("when multiple == TRUE and vars != NA and p.adj == 'bonferroni',
	variables specified are used for the test", {

		k <- ols_bp_test(model, multiple = TRUE, vars = c("disp", "hp"),
			p.adj = "bonferroni")

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.9588))
		expect_equivalent(k$p, c(0.6730, 0.7634, 0.6192))

		expect_false(k$fv)
		expect_false(k$rhs)
		expect_true(k$multiple)

		expect_equivalent(k$vars, c( "disp", "hp"))

		expect_match(k$padj, "bonferroni")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("when multiple == TRUE and vars != NA and p.adj == 'sidak',
	variables specified are used for the test", {

		k <- ols_bp_test(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = 'sidak')


		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.9588))
		expect_equivalent(k$p, c(0.5598, 0.6177, 0.6192))

		expect_false(k$fv)
		expect_false(k$rhs)
		expect_true(k$multiple)

		expect_equivalent(k$vars, c( "disp", "hp"))

		expect_match(k$padj, "sidak")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("when multiple == TRUE and vars != NA and p.adj == 'holm',
	variables specified are used for the test", {

		k <- ols_bp_test(model, multiple = TRUE,
       vars = c("disp", "hp"), p.adj = "holm")

		expect_equivalent(k$bp, c(0.9237, 0.7652, 0.9588))
		expect_equivalent(k$p, c(0.6730, 0.3817, 0.6192))

		expect_false(k$fv)
		expect_false(k$rhs)
		expect_true(k$multiple)

		expect_equivalent(k$vars, c( "disp", "hp"))

		expect_match(k$padj, "holm")
		expect_match(k$resp, "mpg")
		expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))

})


test_that("bp_test fails when model inherits other than 'lm'", {
	y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_bp_test(m), 'Please specify a OLS linear regression model.')
})


test_that("bp_test fails when input for fitted.values is not logical", {
    expect_error(ols_bp_test(model, fitted.values = 'TRUE'),
    	'fitted.values must be either TRUE or FALSE')
    expect_error(ols_bp_test(model, fitted.values = 0),
    	'fitted.values must be either TRUE or FALSE')
})


test_that("bp_test fails when input for rhs is not logical", {
    expect_error(ols_bp_test(model, rhs = 'TRUE'),
    	'rhs must be either TRUE or FALSE')
    expect_error(ols_bp_test(model, rhs = 0),
    	'rhs must be either TRUE or FALSE')
})

test_that("bp_test fails when input for multiple is not logical", {
    expect_error(ols_bp_test(model, rhs = TRUE, multiple = 'TRUE'),
    	'multiple must be either TRUE or FALSE')
    expect_error(ols_bp_test(model, rhs = TRUE, multiple = 1),
    	'multiple must be either TRUE or FALSE')
})


test_that("bp_test fails when input for vars are not a subset of predictors", {
    expect_error(ols_bp_test(model, vars = c("gear", "carb")),
    	'vars must be a subset of the predictors in the model')
})
