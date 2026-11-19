# when fitted.values == TRUE, fitted vals from reg are used

    Code
      ols_test_breusch_pagan(model)
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: fitted values of mpg 
      
             Test Summary         
       ---------------------------
       DF            =    1 
       Chi2          =    1.255517 
       Prob > Chi2   =    0.2625 

---

    Code
      ols_test_breusch_pagan(model, rhs = TRUE)
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
             Test Summary         
       ---------------------------
       DF            =    5 
       Chi2          =    2.489028 
       Prob > Chi2   =    0.7781 

---

    Code
      ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
            Test Summary (Unadjusted p values)      
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.3365 
        hp               0.7652006     1    0.3817 
        wt               0.7748714     1    0.3787 
        drat             0.7751270     1    0.3786 
        qsec             1.2902861     1    0.2560 
       -------------------------------------------
        simultaneous     2.4890277     5    0.7781 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, fitted.values = FALSE, rhs = TRUE, multiple = TRUE)
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
            Test Summary (Unadjusted p values)      
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.3365 
        hp               0.7652006     1    0.3817 
        wt               0.7748714     1    0.3787 
        drat             0.7751270     1    0.3786 
        qsec             1.2902861     1    0.2560 
       -------------------------------------------
        simultaneous     2.4890277     5    0.7781 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
            Test Summary (Bonferroni p values)      
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    1.0000 
        hp               0.7652006     1    1.0000 
        wt               0.7748714     1    1.0000 
        drat             0.7751270     1    1.0000 
        qsec             1.2902861     1    1.0000 
       -------------------------------------------
        simultaneous     2.4890277     5    0.7781 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "holm")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
              Test Summary (Holm's p values)        
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    1.0000 
        hp               0.7652006     1    1.0000 
        wt               0.7748714     1    1.0000 
        drat             0.7751270     1    1.0000 
        qsec             1.2902861     1    1.0000 
       -------------------------------------------
        simultaneous     2.4890277     5    0.7781 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "sidak")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
                   Data               
       -------------------------------
       Response : mpg 
       Variables: disp hp wt drat qsec 
      
               Test Summary (Sidak p values)        
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.8714 
        hp               0.7652006     1    0.9096 
        wt               0.7748714     1    0.9074 
        drat             0.7751270     1    0.9074 
        qsec             1.2902861     1    0.7720 
       -------------------------------------------
        simultaneous     2.4890277     5    0.7781 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, vars = c("disp"))
    Output
      
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
       Prob > Chi2   =    0.3365 

---

    Code
      ols_test_breusch_pagan(model, multiple = TRUE, rhs = FALSE, vars = c("disp",
        "hp"))
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
             Data        
       ------------------
       Response : mpg 
       Variables: disp hp 
      
            Test Summary (Unadjusted p values)      
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.3365 
        hp               0.7652006     1    0.3817 
       -------------------------------------------
        simultaneous     0.9587887     2    0.6192 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = "bonferroni")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
             Data        
       ------------------
       Response : mpg 
       Variables: disp hp 
      
            Test Summary (Bonferroni p values)      
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.6730 
        hp               0.7652006     1    0.7634 
       -------------------------------------------
        simultaneous     0.9587887     2    0.6192 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = "sidak")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
             Data        
       ------------------
       Response : mpg 
       Variables: disp hp 
      
               Test Summary (Sidak p values)        
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.5598 
        hp               0.7652006     1    0.6177 
       -------------------------------------------
        simultaneous     0.9587887     2    0.6192 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = "holm")
    Output
      
       Breusch Pagan Test for Heteroskedasticity
       -----------------------------------------
       Ho: the variance is constant            
       Ha: the variance is not constant        
      
             Data        
       ------------------
       Response : mpg 
       Variables: disp hp 
      
              Test Summary (Holm's p values)        
       -------------------------------------------
        Variable           chi2       df      p    
       -------------------------------------------
        disp             0.9237291     1    0.6730 
        hp               0.7652006     1    0.6730 
       -------------------------------------------
        simultaneous     0.9587887     2    0.6192 
       -------------------------------------------

---

    Code
      ols_test_breusch_pagan(model, multiple = TRUE, rhs = FALSE, vars = c("disp"))
    Output
      
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
       Prob > Chi2   =    0.3365 

