context('correlations output')

x <- cat('               Correlations                 
-------------------------------------------
Variable    Zero Order    Partial     Part  
-------------------------------------------
disp            -0.848     -0.048    -0.019 
hp              -0.776     -0.224    -0.093 
wt              -0.868     -0.574    -0.285 
qsec             0.419      0.219     0.091 
-------------------------------------------')

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
expect_equivalent(print(olsrr::ols_correlations(model)), x)