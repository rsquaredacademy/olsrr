context("best_subsets")

test_that('best subsets selection output matches the expected result', {

    model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
    k <- best_subset(model)
    pred_exp <- c("x4", "x1 x2", "x1 x2 x3", "x1 x2 x3 x4")
    expect_equal(k$mindex, c(1, 2, 3, 4))
    expect_equivalent(k$predictors, pred_exp)

})
