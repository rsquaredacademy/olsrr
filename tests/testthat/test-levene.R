context('levene_test')

m <- split(mtcars$mpg, mtcars$vs)

test_that('all output from the test match the result', {
	
	b <- levene_test(m$`0`, m$`1`)

	expect_equal(b$bf, 3.0862)
	expect_equal(b$p_bf, 0.0892)
	expect_equal(b$lev, 1.5922)
	expect_equal(b$p_lev, 0.2167)
	expect_equal(b$bft, 2.8559)
	expect_equal(b$p_bft, 0.1014)

})


test_that('when group_var != NA, the length of variable and group_var match', {
	expect_error(levene_test(mtcars$mpg, group_var = mtcars$vs[-1]), 
		'Length of variable and group_var do not match.')
})


test_that('when group_var = NA, at least two variables must be specified', {
	expect_error(levene_test(mtcars$mpg), 
		'Please specify at least two variables.')
})
