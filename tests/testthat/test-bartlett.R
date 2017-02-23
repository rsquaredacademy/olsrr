context('bartlett_test')

m <- split(mtcars$mpg, mtcars$vs)

test_that('all output from the test match the result', {
	
	b <- ols_bartlett_test(m$`0`, m$`1`)

	expect_equal(b$fstat, 1.585)	
	expect_equal(b$pval, 0.208)
	expect_equal(b$df, 1)
	expect_equivalent(b$var_c, c("m$`0`", "m$`1`"))
	expect_output(b$g_var, NA)

	b <- ols_bartlett_test(mtcars$mpg, mtcars$disp)

	expect_equal(b$fstat, 142.336)	
	expect_equal(b$pval, 0)
	expect_equal(b$df, 1)
	expect_equivalent(b$var_c, c( "mtcars$mpg", "mtcars$disp"))
	expect_output(b$g_var, NA)

	b <- ols_bartlett_test(mtcars$mpg, group_var =  mtcars$vs)	

	expect_equal(b$fstat, 1.585)	
	expect_equal(b$pval, 0.208)
	expect_equal(b$df, 1)
	expect_equal(b$var_c, "mtcars$mpg")
	expect_equal(b$g_var, "mtcars$vs")

})


test_that('when group_var != NA, the length of variable and group_var match', {
	expect_error(ols_bartlett_test(mtcars$mpg, group_var = mtcars$vs[-1]), 
		'Length of variable and group_var do not match.')
})


test_that('when group_var = NA, at least two variables must be specified', {
	expect_error(ols_bartlett_test(mtcars$mpg), 
		'Please specify at least two variables.')
})



