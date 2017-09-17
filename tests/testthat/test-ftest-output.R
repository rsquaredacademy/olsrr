context('ftest output')

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)

test_that('output from ftest is as expected', {

	x <-cat('
	 F Test for Heteroskedasticity
	 -----------------------------
	 Ho: Variance is homogenous
	 Ha: Variance is not homogenous

	 Variables: fitted values of mpg

	      Test Summary
	 -------------------------
	 Num DF     =    1
	 Den DF     =    30
	 F          =    0.4920617
	 Prob > F   =    0.4884154')

	expect_equivalent(print(olsrr::ols_f_test(model)), x)

})

test_that('output from ftest is as expected when rhs = TRUE', {

	x <- cat('
 F Test for Heteroskedasticity
 -----------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: disp hp wt qsec 

      Test Summary        
 -------------------------
 Num DF     =    4 
 Den DF     =    27 
 F          =    0.4594694 
 Prob > F   =    0.7647271') 

	expect_equivalent(print(olsrr::ols_f_test(model, rhs = TRUE)), x)

})

test_that('output from ftest is as expected when variables are specified', {

	x <- cat('
 F Test for Heteroskedasticity
 -----------------------------
 Ho: Variance is homogenous
 Ha: Variance is not homogenous

 Variables: disp hp 

      Test Summary        
 -------------------------
 Num DF     =    2 
 Den DF     =    29 
 F          =    0.4669306 
 Prob > F   =    0.631555')

	expect_equivalent(print(olsrr::ols_f_test(model, vars = c('disp', 'hp'))), x)

})