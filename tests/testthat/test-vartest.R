context('var_test')

m <- split(mtcars$mpg, mtcars$vs)

test_that('all output from the test match the result', {

	b <- var_test(m$`0`, m$`1`, group_var = NA)

	expect_equal(b$f, 0.5151)
	expect_equal(b$lower, 0.0999)
	expect_equal(b$upper, 0.9001)
	expect_equal(b$two_tail, 0.1997)
	expect_equal(unname(b$n1), 17)
	expect_equal(unname(b$n2), 13)

	b <- var_test(mtcars$mpg, mtcars$disp)

	expect_equal(b$f, 0.0024)
	expect_equal(b$lower, 0)
	expect_equal(b$upper, 1)
	expect_equal(b$two_tail, 0)
	expect_equal(unname(b$n1), 31)
	expect_equal(unname(b$n2), 31)

	b <- var_test(mtcars$mpg, group_var =  mtcars$vs)	

	expect_equal(b$f, 0.5151)
	expect_equal(b$lower, 0.0999)
	expect_equal(b$upper, 0.9001)
	expect_equal(b$two_tail, 0.1997)
	expect_equal(unname(b$n1), 17)
	expect_equal(unname(b$n2), 13)

})


test_that('when group_var != NA, the length of variable and group_var match', {
	expect_error(var_test(mtcars$mpg, group_var = mtcars$vs[-1]), 
		'Length of variable and group_var do not match.')
})


test_that('when group_var = NA, at least two variables must be specified', {
	expect_error(var_test(mtcars$mpg), 
		'Please specify at least two variables.')
})
