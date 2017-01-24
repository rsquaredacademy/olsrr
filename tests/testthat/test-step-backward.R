context("step_backward")

test_that('backward elimination output matches the expected result', {

    model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
    k <- step_backward(model)
    expect_equal(k$steps, 1)
    expect_equal(k$removed, c("x3"))

})




