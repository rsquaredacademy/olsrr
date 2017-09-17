context('breusch pagan test output')

model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('when fitted.values == TRUE, fitted values from the regression
	are used for the test', {

	x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: fitted values of mpg 

        Test Summary         
 ----------------------------
 DF            =    1 
 Chi2          =    1.255517 
 Prob > Chi2   =    0.2625014 ')

 expect_equivalent(print(ols_bp_test(model)), x)		

})

test_that('when rhs == TRUE, predictors from the regression
	are used for the test', {

	x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: disp hp wt drat qsec 

        Test Summary         
 ----------------------------
 DF            =    5 
 Chi2          =    2.489028 
 Prob > Chi2   =    0.7781466')

 	expect_equivalent(print(ols_bp_test(model, rhs = TRUE)), x)	

})

test_that('when rhs == TRUE and multiple == TRUE, multiple p values are
	returned', {

  x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: disp hp wt drat qsec 

        Test Summary (Unadjusted p values)       
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.3364977 
  hp               0.7652006     1    0.3817059 
  wt               0.7748714     1    0.3787143 
  drat             0.7751270     1    0.3786356 
  qsec             1.2902861     1    0.2559952 
 ----------------------------------------------
  simultaneous     2.4890277     5    0.7781466 
 ----------------------------------------------')

  expect_equivalent(print(ols_bp_test(model, rhs = TRUE, multiple = TRUE)), x)	
		
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'bonferroni'
	bonferroni adjusted p values are returned", {

	x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: disp hp wt drat qsec 

        Test Summary (Bonferroni p values)       
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    1.0000000 
  hp               0.7652006     1    1.0000000 
  wt               0.7748714     1    1.0000000 
  drat             0.7751270     1    1.0000000 
  qsec             1.2902861     1    1.0000000 
 ----------------------------------------------
  simultaneous     2.4890277     5    0.7781466 
 ----------------------------------------------')

 	expect_equivalent(print(ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")), x)	

})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'holm',
	bonferroni adjusted p values are returned", {

	x <- cat("
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: disp hp wt drat qsec 

          Test Summary (Holm's p values)         
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    1.0000000 
  hp               0.7652006     1    0.3817059 
  wt               0.7748714     1    0.7574285 
  drat             0.7751270     1    1.0000000 
  qsec             1.2902861     1    1.0000000 
 ----------------------------------------------
  simultaneous     2.4890277     5    0.7781466 
 ----------------------------------------------")

  expect_equivalent(print(ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "holm")), x)	
		
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'sidak',
	bonferroni adjusted p values are returned", {

  x <- cat("
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

             Data               
 -------------------------------
 Response : mpg 
 Variables: disp hp wt drat qsec 

          Test Summary (Sidak p values)          
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.8714086 
  hp               0.7652006     1    0.9096401 
  wt               0.7748714     1    0.9074328 
  drat             0.7751270     1    0.9073743 
  qsec             1.2902861     1    0.7720295 
 ----------------------------------------------
  simultaneous     2.4890277     5    0.7781466 
 ----------------------------------------------")

 	expect_equivalent(print(ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = "sidak")), x)		

})

test_that('when vars != NA, variables specified are used for the test', {

  x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

     Data       
 ---------------
 Response : mpg 
 Variables: disp 

        Test Summary         
 ----------------------------
 DF            =    1 
 Chi2          =    0.9237291 
 Prob > Chi2   =    0.3364977')

  expect_equivalent(print(ols_bp_test(model, vars = c("disp"))), x)		
		
})


test_that('when rhs == FALSE, multiple == TRUE and vars != NA,
	variables specified are used for the test', {

	x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

       Data        
 ------------------
 Response : mpg 
 Variables: disp hp 

        Test Summary (Unadjusted p values)       
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.3364977 
  hp               0.7652006     1    0.3817059 
 ----------------------------------------------
  simultaneous     0.9587887     2    0.6191583 
 ----------------------------------------------')	

	expect_equivalent(print(ols_bp_test(model, multiple = TRUE, rhs = FALSE, vars = c("disp", "hp"))), x)	

})


test_that("when multiple == TRUE and vars != NA and p.adj == 'bonferroni',
	variables specified are used for the test", {

	x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

       Data        
 ------------------
 Response : mpg 
 Variables: disp hp 

        Test Summary (Bonferroni p values)       
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.6729955 
  hp               0.7652006     1    0.7634118 
 ----------------------------------------------
  simultaneous     0.9587887     2    0.6191583 
 ----------------------------------------------')

 expect_equivalent(print(ols_bp_test(model, multiple = TRUE, vars = c("disp", "hp"),
            p.adj = "bonferroni")), x)		
		
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'sidak',
	variables specified are used for the test", {

  x <- cat('
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

       Data        
 ------------------
 Response : mpg 
 Variables: disp hp 

          Test Summary (Sidak p values)          
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.5597648 
  hp               0.7652006     1    0.6177124 
 ----------------------------------------------
  simultaneous     0.9587887     2    0.6191583 
 ----------------------------------------------')

	expect_equivalent(print(ols_bp_test(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = 'sidak')), x)	  
		
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'holm',
	variables specified are used for the test", {

	x <- cat("
 Breusch Pagan Test for Heteroskedasticity
 -----------------------------------------
 Ho: the variance is constant            
 Ha: the variance is not constant        

       Data        
 ------------------
 Response : mpg 
 Variables: disp hp 

          Test Summary (Holm's p values)         
 ----------------------------------------------
  Variable           chi2       df        p     
 ----------------------------------------------
  disp             0.9237291     1    0.6729955 
  hp               0.7652006     1    0.3817059 
 ----------------------------------------------
  simultaneous     0.9587887     2    0.6191583 
 ----------------------------------------------")	

	expect_equivalent(print(ols_bp_test(model, multiple = TRUE,
            vars = c("disp", "hp"), p.adj = "holm")), x)	

})