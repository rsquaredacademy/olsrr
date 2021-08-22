# when fitted.values == TRUE, fitted values from the regression
	are used for the test

    Code
      ols_test_score(model)
    Output
      
       Score Test for Heteroskedasticity
       ---------------------------------
       Ho: Variance is homogenous
       Ha: Variance is not homogenous
      
       Variables: fitted values of mpg 
      
              Test Summary         
       ----------------------------
       DF            =    1 
       Chi2          =    1.268973 
       Prob > Chi2   =    0.2599594 

---

    Code
      ols_test_score(model, rhs = TRUE)
    Output
      
       Score Test for Heteroskedasticity
       ---------------------------------
       Ho: Variance is homogenous
       Ha: Variance is not homogenous
      
       Variables: disp hp wt drat qsec 
      
             Test Summary         
       ---------------------------
       DF            =    5 
       Chi2          =    2.515705 
       Prob > Chi2   =    0.774128 

---

    Code
      ols_test_score(model, vars = c("disp", "hp"))
    Output
      
       Score Test for Heteroskedasticity
       ---------------------------------
       Ho: Variance is homogenous
       Ha: Variance is not homogenous
      
       Variables: disp hp 
      
              Test Summary         
       ----------------------------
       DF            =    2 
       Chi2          =    0.9690651 
       Prob > Chi2   =    0.6159851 

---

    Code
      ols_test_score(model, rhs = TRUE, vars = c("disp", "hp"))
    Output
      
       Score Test for Heteroskedasticity
       ---------------------------------
       Ho: Variance is homogenous
       Ha: Variance is not homogenous
      
       Variables: disp hp wt drat qsec 
      
             Test Summary         
       ---------------------------
       DF            =    5 
       Chi2          =    2.515705 
       Prob > Chi2   =    0.774128 

