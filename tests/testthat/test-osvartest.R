context('osvar_test')

test_that('all outputs from the test match the result', {

	b <- osvar_test(mtcars$mpg)

	expect_equal(b$n, 32)
	expect_equal(b$sd, 6.027)
	expect_equal(b$df, 31)
	expect_equal(b$t, 4504.27)
	expect_equal(b$lchi, 1)
	expect_equal(b$uchi, 0)
	expect_equal(b$tchi, 0)
	expect_equivalent(b$confint, c(18.004, 22.178))
	expect_equal(b$se, 1.065)

	b <- osvar_test(mtcars$mpg, hyp_sd = 2.36)

	expect_equal(b$n, 32)
	expect_equal(b$sd, 6.027)
	expect_equal(b$df, 31)
	expect_equal(b$t, 202.18)
	expect_equal(b$lchi, 1)
	expect_equal(b$uchi, 0)
	expect_equal(b$tchi, 0)
	expect_equivalent(b$confint, c(18.004, 22.178))
	expect_equal(b$se, 1.065)

})

test_that('input for data is numeric', {
	expect_error(osvar_test(as.factor(mtcars$gear)),
		'data must be numeric')
})

test_that('input for hyp_sd is numeric', {
	expect_error(osvar_test(mtcars$mpg, hyp_sd = '0.5'),
		'standard deviation must be numeric')
})

test_that('input for hyp_sd is positive', {
	expect_error(osvar_test(mtcars$mpg, hyp_sd = -0.5),
		'standard deviation must be positive')
})

test_that('input for conf.level is numeric', {
	expect_error(osvar_test(mtcars$mpg, conf.level = '0.95'),
		'conf.level must be numeric')
})

test_that('input for conf.level must be between 0 and 1', {
	expect_error(osvar_test(mtcars$mpg, conf.level = 1.95),
		'conf.level must be between 0 and 1')
	expect_error(osvar_test(mtcars$mpg, conf.level = -0.95),
		'conf.level must be between 0 and 1')
})
