# output from ftest is as expected

    Code
      ols_test_f(model)
    Output
      
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
       Prob > F   =    0.4884154 

---

    Code
      ols_test_f(model, rhs = TRUE)
    Output
      
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
       Prob > F   =    0.7647271 

---

    Code
      ols_test_f(model, vars = c("disp", "hp"))
    Output
      
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
       Prob > F   =    0.631555 

