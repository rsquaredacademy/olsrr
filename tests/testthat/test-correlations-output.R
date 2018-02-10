context("correlations output")

x <- cat("               Correlations                 \n-------------------------------------------\nVariable    Zero Order    Partial     Part  \n-------------------------------------------\ndisp            -0.848     -0.048    -0.019 \nhp              -0.776     -0.224    -0.093 \nwt              -0.868     -0.574    -0.285 \nqsec             0.419      0.219     0.091 \n-------------------------------------------")

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
expect_equivalent(print(olsrr::ols_correlations(model)), x)
