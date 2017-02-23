context("all_subsets")

test_that('all subsets selection output matches the expected result', {
    
    model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
    k <- ols_all_subset(model)
    pred_exp <- c("x4", "x2", "x1", "x3", "x1 x2",      
                  "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",      
                  "x1 x2 x3", "x1 x2 x4", "x1 x3 x4", "x2 x3 x4", 
                  "x1 x2 x3 x4")
    expect_equal(k$mindex, c(1:15))
    expect_equivalent(k$predictors, pred_exp)
    
})