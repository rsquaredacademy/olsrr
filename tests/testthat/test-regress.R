context('regress')

model <- regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('regress returns all the model validation metrics', {

	sb <- c(disp = 0.179, hp = -0.234, wt = -0.712, drat = 0.179, qsec = 0.190)
	lm1 <- c('(Intercept)' = -6.004, disp = -0.014, hp = -0.052, wt = -6.941, drat = -0.676, qsec = -0.304)
	lm2 <- c('(Intercept)' = 39.071, disp = 0.032, hp = 0.011, wt = -1.830, drat = 4.707, qsec = 1.584)

	expect_equal(model$r, 0.921)
	expect_equal(model$cv, 12.732)
	expect_equal(model$mae, 1.84)
	expect_equal(model$prsq, 0.767)
	expect_equal(model$sbetas, sb)
	expect_equal(model$conf_lm[, 1], lm1)
	expect_equal(model$conf_lm[, 2], lm2)

})

test_that('regress fails when input for formula is other than formula', {
	expect_error(regress('mpg ~ disp + hp', data = mtcars),
		'Please specify a valid formula.')
})

test_that('regress fails when input for data is missing', {
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec),
		'data missing')
})

test_that('regress fails when input for data is other than data frame', {
	mat_mt <- as.matrix(mtcars)
	expect_error(regress(mtcars$mpg ~ mtcars$hp + mtcars$disp, data = mat_mt),
		'data must be a data frame')
})

test_that('regress fails when input for conf.level is non-numeric', {
	cl <- '1'
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars,
	 conf.level = cl), 'conf.level must be numeric')
})

test_that('regress fails when input for conf.level is negative', {
	cl <- -0.95
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars, 
		conf.level = cl), 'conf.level must be between 0 and 1')
})

test_that('regress fails when input for conf.level is negative', {
	cl <- 1.95
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars, 
		conf.level = cl), 'conf.level must be between 0 and 1')
})

test_that('regress fails when input for title is not a string/character', {
	title <- 0.95
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars,
	 title = title), '0.95 is not a string, Please specify a string as title.')

	title <- TRUE
	expect_error(regress(mpg ~ disp + hp + wt + drat + qsec, data = mtcars, 
		title = title), 'TRUE is not a string, Please specify a string as title.')

})




