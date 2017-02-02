context('stepaic_forward')

test_that('output from stepaic_forward matches the expected outptu', {
    model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
    k <- stepaic_forward(model)
    expect_equal(k$steps, 2)
    expect_equivalent(k$predictors, c("wt", "hp"))
    expect_equivalent(round(k$aics, 3), c(166.029, 156.652))
    expect_equivalent(round(k$ess, 3), c(278.322, 195.048))
    expect_equivalent(round(k$rss, 3), c(847.725, 930.999))
    expect_equivalent(round(k$rsq, 3), c(0.753, 0.827))
    expect_equivalent(round(k$arsq, 3), c(0.745, 0.815))
})
