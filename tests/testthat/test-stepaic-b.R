context('stepaic_backward')

test_that('output from stepaic_backward matches the expected outptu', {
    model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
    k <- ols_stepaic_backward(model)
    expect_equal(k$steps, 2)
    expect_equivalent(k$predictors, c("disp", "drat"))
    expect_equivalent(round(k$aics, 3), c(158.584, 156.731, 156.652))
    expect_equivalent(round(k$ess, 3), c(182.838, 183.682, 195.048))
    expect_equivalent(round(k$rss, 3), c(943.209, 942.365, 930.999))
    expect_equivalent(round(k$rsq, 3), c(0.838, 0.837, 0.827))
    expect_equivalent(round(k$arsq, 3), c(0.814, 0.819, 0.815))
})
