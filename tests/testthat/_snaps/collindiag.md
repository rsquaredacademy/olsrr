# output from ols_coll_diag is as expected

    Code
      ols_coll_diag(model)
    Output
      Tolerance and Variance Inflation Factor
      ---------------------------------------
        Variables Tolerance      VIF
      1      disp 0.1218116 8.209402
      2        hp 0.3454979 2.894373
      3        wt 0.1962092 5.096601
      4      drat 0.4386836 2.279547
      
      
      Eigenvalue and Condition Index
      ------------------------------
         Eigenvalue Condition Index    intercept        disp          hp           wt
      1 4.692806914        1.000000 0.0002323252 0.001106455 0.002566185 0.0007172086
      2 0.240308641        4.419078 0.0036813894 0.034132904 0.031334562 0.0009394254
      3 0.052153430        9.485821 0.0009192095 0.058394262 0.735003722 0.0700789813
      4 0.011406889       20.283026 0.0014476535 0.885725642 0.207337511 0.7179834661
      5 0.003324127       37.573144 0.9937194224 0.020640737 0.023758021 0.2102809185
                drat
      1 0.0003775503
      2 0.0148250672
      3 0.0026259361
      4 0.0568226912
      5 0.9253487552

