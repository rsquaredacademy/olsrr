# output from bartlett test is as expected when using variables

    Code
      ols_test_bartlett(hsb, "read", "write")
    Output
      
          Bartlett's Test of Homogenity of Variances    
      ------------------------------------------------
      Ho: Variances are equal across groups
      Ha: Variances are unequal for atleast two groups
      
              Data          
       ---------------------
       Variables: read write 
      
              Test Summary         
       ----------------------------
       DF            =    1 
       Chi2          =    1.222863 
       Prob > Chi2   =    0.2687995 

---

    Code
      ols_test_bartlett(descriptr::mtcarz, "mpg", group_var = "vs")
    Output
      
          Bartlett's Test of Homogenity of Variances    
      ------------------------------------------------
      Ho: Variances are equal across groups
      Ha: Variances are unequal for atleast two groups
      
              Test Summary         
       ----------------------------
       DF            =    1 
       Chi2          =    1.584866 
       Prob > Chi2   =    0.2080612 

